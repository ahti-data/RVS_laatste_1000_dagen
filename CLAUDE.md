# RVS laatste 1000 dagen

Shiny dashboard analysing healthcare costs and utilisation in the last 1000 days of life,
plus the R pipeline that produces its aggregated outputs.

## Structure

- `dashboard/app.R` — the dashboard UI and server logic; sources the helpers below.
- `dashboard/utils/` — reusable functions shared across the app
  (`format_thinkcell_download.R` for think-cell exports, `chart_downloads.R` for the
  download-button wiring).
- `dashboard/data/` — dashboard input files (including per-iteration outputs under
  `data/data_iteration_3/iteration_<id>/`).
- `dashboard/data/metadata/` — shared metadata and branding helpers (e.g. `brand_colors.R`).
- `dashboard/templates/` — built-in think-cell `.pptx` slide templates for the "Download slide"
  export (committed to git, shipped by the deploy). `templates/previews/<name>.png` is an
  optional, curated-by-hand screenshot shown as a thumbnail on the "Manage templates" tab.
- `dashboard/state/` — runtime state, never committed or synced by the deploy:
  - `favorites.json` (shared favorites) and `favorite_assets/` (client-captured PNG snapshots of
    starred charts, see `TC_FAVORITE_CAPTURE_JS` in `utils/chart_downloads.R`).
  - `export_history/<id>.json` — one file per "Download slide" click, logged automatically (see
    `utils/export_history.R`) so it can be redownloaded exactly later from the **Export history**
    tab, however long ago it was created. Distinct from favorites: history is automatic and
    complete, favorites are manually curated.
  - The same selection log (plus a generation timestamp and, when wired, `source_output`/
    `source_sheet` identifiers -- see Conventions below) also gets written into every exported
    chart's own think-cell datasheet (row 1, column 1 — a cell think-cell never renders or reads
    as data), via `tc_build_datasheet_log()` in `utils/slide_download.R`. Unlike `log.txt`, this
    rides inside the chart element itself, so it survives the chart being copied to a new slide or
    deck. "Download data (think-cell)" (the plain `.xlsx` a PM links into their own chart) carries
    the same log stamped onto its corner header cell instead, via `tc_stamp_tc_matrix_corner()`
    in `utils/format_thinkcell_download.R`, since it never goes through a chart datasheet.
  - `template_uploads/` (templates uploaded via the "Manage templates" tab, plus
    `template_uploads/previews/<name>.png`). Uploads live here, not under `templates/`, because
    the Shiny process must be able to write them whereas the deploy-owned `templates/` is
    read-only on the server; an upload overrides a built-in of the same name. Override with the
    `SHINY_TEMPLATE_UPLOADS_DIR` env var.
- `dashboard/tests/testthat/` — testthat tests.
- `output_src/` — the R pipeline that generates the aggregated `.xlsx` outputs consumed by the dashboard.

## Conventions

- Add reusable logic to `dashboard/utils/`, not inline in `app.R`.
- New chart types for think-cell export go in `dashboard/utils/format_thinkcell_download.R`
  with tests before being exposed in the UI (see `dashboard/utils/chart_downloads.R` for the
  wiring pattern, and `.cursor/rules/thinkcell-export.mdc` / `.claude/skills/thinkcell-export`
  for the full export convention).
- This dashboard's chart data comes from named `output_src` pipeline outputs (e.g.
  `data/data_iteration_3/iteration_<id>/...`, read by sheet). Pass `source_output`/`source_sheet`
  when wiring `chart_data_downloads_server()` (and `favorites_capture()`, if the chart supports
  favoriting) identifying which output/sheet backs that chart -- they ride along in every
  export's embedded provenance log (see below) so a chart found later can be traced back to the
  exact source file/sheet, not just the dashboard tab.

## Running

```r
shiny::runApp("dashboard/app.R")
```

## Tests

```r
testthat::test_dir("dashboard/tests")
```

Run the test suite after changing anything in `dashboard/utils/`.
