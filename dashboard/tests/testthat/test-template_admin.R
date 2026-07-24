library(testthat)

source(file.path("..", "..", "utils", "format_thinkcell_download.R"))
source(file.path("..", "..", "utils", "slide_download.R"))
source(file.path("..", "..", "utils", "template_admin.R"))

make_fake_pptx <- function(path) {
  con <- file(path, "wb")
  writeBin(as.raw(c(0x50, 0x4B, 0x03, 0x04)), con)
  close(con)
}

test_that("filenames are sanitized to a flat, safe .pptx basename", {
  expect_equal(tmpl_sanitize_filename("My Template.pptx"), "My_Template.pptx")
  expect_equal(tmpl_sanitize_filename("../../etc/passwd.pptx"), "passwd.pptx")
  expect_equal(tmpl_sanitize_filename("weird!@#chars.pptx"), "weird_chars.pptx")
})

test_that("only zip-signed files are accepted as .pptx", {
  tmp_ok <- tempfile(fileext = ".pptx")
  make_fake_pptx(tmp_ok)
  expect_true(tmpl_looks_like_pptx(tmp_ok))

  tmp_bad <- tempfile(fileext = ".pptx")
  writeLines("not a zip", tmp_bad)
  expect_false(tmpl_looks_like_pptx(tmp_bad))
})

test_that("uploads are rejected when not a .pptx or not zip-signed", {
  templates_dir <- tempfile("templates_")
  dir.create(templates_dir)

  tmp_txt <- tempfile(fileext = ".txt")
  writeLines("hello", tmp_txt)
  res <- tmpl_save_upload(tmp_txt, "notes.txt", templates_dir)
  expect_false(res$ok)

  tmp_fake <- tempfile(fileext = ".pptx")
  writeLines("not a zip", tmp_fake)
  res2 <- tmpl_save_upload(tmp_fake, "fake.pptx", templates_dir)
  expect_false(res2$ok)
})

test_that("a valid upload lands in templates/custom/ and is listed with precedence", {
  templates_dir <- tempfile("templates_")
  dir.create(templates_dir)
  # a built-in template with the same name as the upload, to prove precedence
  writeLines("builtin", file.path(templates_dir, "team_deck.pptx"))

  tmp_upload <- tempfile(fileext = ".pptx")
  make_fake_pptx(tmp_upload)
  res <- tmpl_save_upload(tmp_upload, "team_deck.pptx", templates_dir)

  expect_true(res$ok)
  expect_true(file.exists(file.path(templates_dir, "custom", "team_deck.pptx")))

  # listing shows one entry (deduplicated by name)
  files <- tc_list_templates(templates_dir)
  expect_equal(sum(files == "team_deck.pptx"), 1)

  # resolution prefers the uploaded (custom) copy
  resolved <- tc_resolve_template_path("team_deck.pptx", templates_dir)
  expect_true(grepl("custom", resolved, fixed = TRUE))
})

test_that("uploads survive being listed even when no custom dir exists yet", {
  templates_dir <- tempfile("templates_")
  dir.create(templates_dir)
  writeLines("builtin", file.path(templates_dir, "only_builtin.pptx"))
  expect_equal(tc_list_templates(templates_dir), "only_builtin.pptx")
})
