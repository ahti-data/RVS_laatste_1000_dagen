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
- `dashboard/tests/testthat/` — testthat tests.
- `output_src/` — the R pipeline that generates the aggregated `.xlsx` outputs consumed by the dashboard.

## Conventions

- Add reusable logic to `dashboard/utils/`, not inline in `app.R`.
- New chart types for think-cell export go in `dashboard/utils/format_thinkcell_download.R`
  with tests before being exposed in the UI (see `dashboard/utils/chart_downloads.R` for the
  wiring pattern, and `.cursor/rules/thinkcell-export.mdc` / `.claude/skills/thinkcell-export`
  for the full export convention).

## Running

```r
shiny::runApp("dashboard/app.R")
```

## Tests

```r
testthat::test_dir("dashboard/tests")
```

Run the test suite after changing anything in `dashboard/utils/`.
