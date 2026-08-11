library(testthat)

source(file.path("..", "..", "utils", "slide_download.R"))
source(file.path("..", "..", "utils", "dictionary.R"))
source(file.path("..", "..", "utils", "dictionary_admin.R"))

with_dictionary_path <- function(code) {
  path <- tempfile("dictionary_", fileext = ".json")
  old <- Sys.getenv("SHINY_DICTIONARY_PATH", unset = NA)
  Sys.setenv(SHINY_DICTIONARY_PATH = path)
  on.exit({
    if (is.na(old)) Sys.unsetenv("SHINY_DICTIONARY_PATH") else Sys.setenv(SHINY_DICTIONARY_PATH = old)
    unlink(path)
  }, add = TRUE)
  force(code)
}

test_that("dictionary_scope_label uses a curated label when one exists, else a generic prettified fallback", {
  expect_equal(dictionary_scope_label(""), "Overig (geen scope)")
  expect_equal(dictionary_scope_label("some_new_scope"), "Some New Scope")
})

test_that("dictionary_scope_order puts curated scopes first, in their own order, then unknown ones alphabetically", {
  old <- DICTIONARY_SCOPE_LABELS
  DICTIONARY_SCOPE_LABELS <<- c(stats::setNames("Overig", ""), sheet = "Sheets", age_cat = "Leeftijd")
  on.exit(DICTIONARY_SCOPE_LABELS <<- old, add = TRUE)

  ordered <- dictionary_scope_order(c("zeta", "age_cat", "alpha", "sheet"))
  expect_equal(ordered, c("sheet", "age_cat", "alpha", "zeta"))
})

test_that("adding a new entry through the module persists it to the dictionary", {
  with_dictionary_path({
    shiny::testServer(dictionary_admin_server, args = list(id = "test_dict"), {
      session$setInputs(add = 1)
      session$setInputs(edit_raw_key = "wlz", edit_scope = "sheet", edit_pretty_label = "WLZ")
      session$setInputs(save = 1)

      expect_equal(dictionary_lookup("wlz", "sheet"), "WLZ")
      expect_true(isTRUE(status_rv$ok))
      expect_match(status_rv$message, "Saved")
    })
  })
})

test_that("saving with a missing raw name or pretty label is rejected without writing anything", {
  with_dictionary_path({
    shiny::testServer(dictionary_admin_server, args = list(id = "test_dict"), {
      session$setInputs(add = 1)
      session$setInputs(edit_raw_key = "", edit_scope = "", edit_pretty_label = "Something")
      session$setInputs(save = 1)

      expect_false(isTRUE(status_rv$ok))
      expect_equal(dictionary_list(), list())
    })
  })
})

test_that("clicking a row's Edit button opens the editor for that entry, and Save updates it in place", {
  with_dictionary_path({
    dictionary_set_entry("wlz", "sheet", "WLZ")

    shiny::testServer(dictionary_admin_server, args = list(id = "test_dict"), {
      # Force the entries list (and its dynamic per-row Edit-button observers,
      # registered by a separate shiny::observe() block) to materialize before
      # simulating a click on one of those buttons -- session$flushReact()
      # runs the whole reactive graph, not just the one reactive referenced
      # directly below.
      entries <- filtered_entries()
      expect_length(entries, 1)
      session$flushReact()
      btn_id <- paste0("edit_", dict_entry_ui_id("wlz", "sheet"))

      do.call(session$setInputs, setNames(list(1), btn_id))
      expect_equal(editing()$raw_key, "wlz")

      session$setInputs(edit_pretty_label = "WLZ (bijgewerkt)")
      session$setInputs(save = 1)

      expect_equal(dictionary_lookup("wlz", "sheet"), "WLZ (bijgewerkt)")
    })
  })
})

test_that("deleting an entry via the editor's Delete button removes it", {
  with_dictionary_path({
    dictionary_set_entry("wlz", "sheet", "WLZ")

    shiny::testServer(dictionary_admin_server, args = list(id = "test_dict"), {
      entries <- filtered_entries()
      session$flushReact()
      btn_id <- paste0("edit_", dict_entry_ui_id("wlz", "sheet"))
      do.call(session$setInputs, setNames(list(1), btn_id))
      expect_equal(editing()$raw_key, "wlz")

      session$setInputs(delete = 1)

      expect_null(dictionary_lookup("wlz", "sheet"))
      expect_true(isTRUE(status_rv$ok))
      expect_match(status_rv$message, "Removed")
    })
  })
})

test_that("the search box filters the entries list by raw name, scope, or label", {
  with_dictionary_path({
    dictionary_set_entry("wlz", "sheet", "WLZ")
    dictionary_set_entry("zvw", "sheet", "ZVW")

    shiny::testServer(dictionary_admin_server, args = list(id = "test_dict"), {
      expect_length(filtered_entries(), 2)

      session$setInputs(search = "zvw")
      expect_length(filtered_entries(), 1)
      expect_equal(filtered_entries()[[1]]$raw_key, "zvw")

      session$setInputs(search = "")
      expect_length(filtered_entries(), 2)
    })
  })
})
