#' Template management UI, decoupled from git deploys.
#'
#' Lets someone add a new think-cell `.pptx` template without a developer
#' committing it to the repo. Uploads are written to `templates/custom/`,
#' which the deploy workflow never syncs (see CLAUDE.md), so they survive a
#' redeploy automatically. Reuses the existing template resolution logic in
#' `utils/slide_download.R` rather than duplicating it — this module only
#' widens where those functions look.

#' Sanitize an uploaded file name to a safe, flat `.pptx` basename.
#'
#' Strips any directory components (defends against path traversal via a
#' crafted upload name) and replaces anything but alphanumerics/`-`/`_` with
#' `_`, keeping the extension.
#' @param name Original file name from `fileInput`.
#' @return Sanitized base name, always ending in `.pptx`.
tmpl_sanitize_filename <- function(name) {
  name <- basename(as.character(name))
  ext  <- tools::file_ext(name)
  stem <- tools::file_path_sans_ext(name)
  stem <- gsub("[^A-Za-z0-9_-]+", "_", stem)
  if (!nzchar(stem)) stem <- "template"
  paste0(stem, ".pptx")
}

#' Whether a file looks like a real `.pptx` (a zip archive) rather than
#' something merely renamed to end in `.pptx`.
#' @param path Path to the uploaded temp file.
tmpl_looks_like_pptx <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con))
  magic <- tryCatch(readBin(con, "raw", n = 2), error = function(e) raw(0))
  length(magic) == 2 && magic[1] == as.raw(0x50) && magic[2] == as.raw(0x4B) # "PK"
}

#' Copy a validated upload into `templates/custom/`, avoiding name collisions.
#' @param tmp_path Path to the uploaded temp file (from `fileInput`).
#' @param original_name Original file name as selected by the user.
#' @param templates_dir Optional base templates directory override.
#' @return list(ok, message, filename).
tmpl_save_upload <- function(tmp_path, original_name, templates_dir = NULL) {
  ext <- tolower(tools::file_ext(original_name))
  if (!identical(ext, "pptx")) {
    return(list(ok = FALSE, message = "Only .pptx files are accepted.", filename = NA_character_))
  }
  if (!tmpl_looks_like_pptx(tmp_path)) {
    return(list(ok = FALSE, message = "File does not look like a valid .pptx (not a zip archive).",
                filename = NA_character_))
  }

  custom_dir <- tc_custom_templates_dir(templates_dir)
  if (is.null(custom_dir) || is.na(custom_dir)) {
    return(list(ok = FALSE, message = "Could not resolve a templates/ directory to upload into.",
                filename = NA_character_))
  }
  if (!dir.exists(custom_dir)) {
    dir.create(custom_dir, recursive = TRUE, showWarnings = FALSE)
  }
  # The uploads directory is created at runtime under templates/, which on a
  # deployed server may not be writable by the Shiny process. Detect that here
  # instead of reporting a false "Uploaded" (the copy below would silently fail
  # and the template would never appear in the list).
  if (!dir.exists(custom_dir)) {
    return(list(ok = FALSE, filename = NA_character_, message = sprintf(
      "Could not create the uploads folder '%s'. The app may not have write access there.",
      custom_dir)))
  }

  filename <- tmpl_sanitize_filename(original_name)
  dest <- file.path(custom_dir, filename)
  copied <- file.copy(tmp_path, dest, overwrite = TRUE)
  if (!isTRUE(copied) || !file.exists(dest)) {
    return(list(ok = FALSE, filename = NA_character_, message = sprintf(
      "Could not save the upload to '%s'. The folder may not be writable by the app.",
      dest)))
  }

  list(ok = TRUE, message = sprintf("Uploaded '%s'.", filename), filename = filename)
}

#' Bundle every currently available template into one `.zip` so someone can
#' download a base template, edit it in PowerPoint, and re-upload it.
#'
#' Includes the built-in `templates/` set plus any uploaded overrides in
#' `templates/custom/` (deduplicated by name, the custom copy winning — the
#' same effective set [tc_list_templates()] shows), so what you download is
#' exactly what the dashboard would use. Always produces a valid `.zip`; if no
#' templates are found it contains a short README instead.
#' @param zip_path Output `.zip` path (the `file` from a downloadHandler).
#' @param templates_dir Optional base templates directory override.
#' @return `zip_path` (invisibly).
tmpl_build_templates_zip <- function(zip_path, templates_dir = NULL) {
  work <- tempfile("templates_dl_")
  dir.create(work)
  old_wd <- getwd()
  on.exit({
    setwd(old_wd)
    unlink(work, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  files <- tc_list_templates(templates_dir)
  copied <- character(0)
  for (f in files) {
    src <- tc_resolve_template_path(f, templates_dir)
    if (!is.na(src)) {
      file.copy(src, file.path(work, f), overwrite = TRUE)
      copied <- c(copied, f)
    }
  }
  if (length(copied) == 0) {
    writeLines(
      "No slide templates were found to download.",
      file.path(work, "README.txt")
    )
  }

  zip_files <- basename(list.files(work, full.names = TRUE))
  zip_path_abs <- normalizePath(zip_path, winslash = "/", mustWork = FALSE)
  setwd(work)
  utils::zip(zipfile = zip_path_abs, files = zip_files, flags = "-q -X")
  invisible(zip_path_abs)
}

#' UI for the template management panel.
#' @param id Module id.
template_admin_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Slide templates"),
    shiny::p(
      class = "text-muted",
      "Upload a new think-cell .pptx template here — no git commit needed. ",
      "Uploaded templates take precedence over a built-in template of the same name."
    ),
    shiny::fileInput(ns("upload"), "Upload a .pptx template", accept = ".pptx"),
    shiny::uiOutput(ns("status")),
    shiny::h5("Available templates"),
    shiny::p(
      class = "text-muted",
      "Download the current templates to edit a base template, then upload your ",
      "edited copy above (same file name to replace it, a new name to add it)."
    ),
    shiny::downloadButton(ns("download_templates"), "Download templates (.zip)",
                          class = "btn-default"),
    shiny::tableOutput(ns("template_list"))
  )
}

#' Server logic for the template management panel.
#' @param id Module id.
#' @param templates_dir Optional base templates directory override.
template_admin_server <- function(id, templates_dir = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    status_rv <- shiny::reactiveValues(message = NULL, ok = NA)
    refresh_trigger <- shiny::reactiveVal(0)

    shiny::observeEvent(input$upload, {
      req_file <- input$upload
      shiny::req(req_file)

      result <- tmpl_save_upload(req_file$datapath, req_file$name, templates_dir)
      status_rv$message <- result$message
      status_rv$ok <- result$ok
      if (isTRUE(result$ok)) {
        refresh_trigger(shiny::isolate(refresh_trigger()) + 1)
      }
    })

    output$status <- shiny::renderUI({
      shiny::req(status_rv$message)
      cls <- if (isTRUE(status_rv$ok)) "text-success" else "text-danger"
      shiny::tags$p(class = cls, status_rv$message)
    })

    output$download_templates <- shiny::downloadHandler(
      filename = function() paste0("slide_templates_", Sys.Date(), ".zip"),
      content = function(file) {
        tmpl_build_templates_zip(file, templates_dir)
      }
    )

    output$template_list <- shiny::renderTable({
      refresh_trigger()
      files <- tc_list_templates(templates_dir)
      custom_dir <- tc_custom_templates_dir(templates_dir)
      is_custom <- !is.na(custom_dir) & file.exists(file.path(custom_dir, files))
      data.frame(
        Template = files,
        Source   = ifelse(is_custom, "Uploaded", "Built-in"),
        stringsAsFactors = FALSE
      )
    })

    invisible(refresh_trigger)
  })
}
