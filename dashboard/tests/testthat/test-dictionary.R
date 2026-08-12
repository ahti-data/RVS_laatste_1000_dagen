library(testthat)

source(file.path("..", "..", "utils", "slide_download.R"))
source(file.path("..", "..", "utils", "dictionary.R"))

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

test_that("dictionary_path honors SHINY_DICTIONARY_PATH and defaults to state/dictionary.json", {
  with_dictionary_path({
    expect_equal(dictionary_path(), Sys.getenv("SHINY_DICTIONARY_PATH"))
  })
  old <- Sys.getenv("SHINY_DICTIONARY_PATH", unset = NA)
  Sys.unsetenv("SHINY_DICTIONARY_PATH")
  on.exit(if (!is.na(old)) Sys.setenv(SHINY_DICTIONARY_PATH = old), add = TRUE)
  expect_equal(dictionary_path(), file.path("state", "dictionary.json"))
})

with_seed_entries <- function(seed_fn, code) {
  old <- dictionary_seed_entries
  dictionary_seed_entries <<- seed_fn
  on.exit(dictionary_seed_entries <<- old, add = TRUE)
  force(code)
}

test_that("dictionary_list seeds from dictionary_seed_entries on first read and persists it", {
  with_dictionary_path({
    with_seed_entries(
      function() list(list(raw_key = "zvwktotaal", scope = "zvw_metric", pretty_label = "Totaal")),
      {
        expect_false(file.exists(dictionary_path()))
        entries <- dictionary_list()
        expect_length(entries, 1)
        expect_equal(entries[[1]]$pretty_label, "Totaal")
        expect_true(file.exists(dictionary_path()))
      }
    )
  })
})

test_that("dictionary_list fills in a seed entry added after the file already existed", {
  with_dictionary_path({
    with_seed_entries(
      function() list(list(raw_key = "bedragwlzzin", scope = "metric_generic", pretty_label = "WLZ kosten")),
      {
        entries <- dictionary_list()
        expect_length(entries, 1)
      }
    )
    # Simulates a later code change adding a new raw name to
    # dictionary_seed_entries() -- the file above already exists (created
    # by the first with_seed_entries() block), so a plain "seed once, at
    # creation" design would never pick this up.
    with_seed_entries(
      function() list(
        list(raw_key = "bedragwlzzin", scope = "metric_generic", pretty_label = "WLZ kosten"),
        list(raw_key = "zvwktotaal", scope = "metric_generic", pretty_label = "ZVW kosten totaal")
      ),
      {
        entries <- dictionary_list()
        expect_length(entries, 2)
        expect_equal(dictionary_lookup("zvwktotaal", "metric_generic"), "ZVW kosten totaal")
        # And it's actually persisted, not just returned in-memory.
        on_disk <- jsonlite::fromJSON(dictionary_path(), simplifyVector = FALSE)
        expect_length(on_disk, 2)
      }
    )
  })
})

test_that("dictionary_fill_missing_seed never overwrites a user's existing edit", {
  with_dictionary_path({
    dictionary_set_entry("bedragwlzzin", "metric_generic", "WLZ kosten (aangepast door gebruiker)")
    with_seed_entries(
      function() list(list(raw_key = "bedragwlzzin", scope = "metric_generic", pretty_label = "WLZ kosten")),
      {
        # The seed's own value must NOT clobber the user's edit -- only
        # entries genuinely missing (by raw_key+scope) get added.
        expect_equal(dictionary_lookup("bedragwlzzin", "metric_generic"), "WLZ kosten (aangepast door gebruiker)")
      }
    )
  })
})

test_that("dictionary_set_entry still works correctly once a dictionary.json already exists (no nested-lock hang)", {
  with_dictionary_path({
    with_seed_entries(
      function() list(list(raw_key = "bedragwlzzin", scope = "metric_generic", pretty_label = "WLZ kosten")),
      {
        dictionary_list() # materializes the file
        # dictionary_set_entry() reads with tc_json_list_read(), not
        # dictionary_list(), specifically so this doesn't try to acquire
        # the same file lock twice in one call stack.
        dictionary_set_entry("zvwktotaal", "metric_generic", "ZVW kosten totaal")
        expect_equal(dictionary_lookup("bedragwlzzin", "metric_generic"), "WLZ kosten")
        expect_equal(dictionary_lookup("zvwktotaal", "metric_generic"), "ZVW kosten totaal")
      }
    )
  })
})

test_that("dictionary_set_entry adds a new entry findable by dictionary_lookup", {
  with_dictionary_path({
    dictionary_set_entry("zvwktotaal", "zvw_metric", "Totale ZVW kosten")
    expect_equal(dictionary_lookup("zvwktotaal", "zvw_metric"), "Totale ZVW kosten")
  })
})

test_that("dictionary_set_entry upserts an existing (raw_key, scope) pair instead of duplicating", {
  with_dictionary_path({
    dictionary_set_entry("zvwktotaal", "zvw_metric", "Totale ZVW kosten")
    dictionary_set_entry("zvwktotaal", "zvw_metric", "ZVW totaal (bijgewerkt)")
    entries <- dictionary_list()
    matching <- Filter(function(e) identical(e$raw_key, "zvwktotaal"), entries)
    expect_length(matching, 1)
    expect_equal(matching[[1]]$pretty_label, "ZVW totaal (bijgewerkt)")
  })
})

test_that("scope disambiguates identical raw keys instead of colliding (age_cat vs inkomen_klasse)", {
  with_dictionary_path({
    dictionary_set_entry("2", "age_cat", "18-29 jaar")
    dictionary_set_entry("2", "inkomen_klasse", "Zeer laag")
    expect_equal(dictionary_lookup("2", "age_cat"), "18-29 jaar")
    expect_equal(dictionary_lookup("2", "inkomen_klasse"), "Zeer laag")
    expect_null(dictionary_lookup("2", ""))
  })
})

test_that("dictionary_remove_entry removes only the matching (raw_key, scope) pair", {
  with_dictionary_path({
    dictionary_set_entry("2", "age_cat", "18-29 jaar")
    dictionary_set_entry("2", "inkomen_klasse", "Zeer laag")
    dictionary_remove_entry("2", "age_cat")
    expect_null(dictionary_lookup("2", "age_cat"))
    expect_equal(dictionary_lookup("2", "inkomen_klasse"), "Zeer laag")
  })
})

test_that("a stored dictionary entry wins over a caller-supplied fallback", {
  with_dictionary_path({
    dictionary_set_entry("wlz", "sheet", "WLZ")
    fallback <- function(x) paste0("fallback:", x)
    expect_equal(dictionary_pretty("wlz", scope = "sheet", fallback = fallback), "WLZ")
  })
})

test_that("dictionary_pretty calls the caller-supplied fallback when no entry exists", {
  with_dictionary_path({
    fallback <- function(x) paste0("fallback:", x)
    expect_equal(dictionary_pretty("unmapped", scope = "sheet", fallback = fallback), "fallback:unmapped")
  })
})

test_that("dictionary_relabel vectorizes over a column and passes NA through unchanged", {
  with_dictionary_path({
    dictionary_set_entry("wlz", "sheet", "WLZ")
    result <- dictionary_relabel(c("wlz", "zvw", NA), scope = "sheet")
    expect_equal(result, c("WLZ", "Zvw", NA_character_))
  })
})
