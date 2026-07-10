---
name: thinkcell-export
description: Standardize think-cell data exports from Shiny dashboard charts. Use when adding or editing a chart (in dashboard/app.R or dashboard/utils/**/*.R) that has, or should have, download buttons.
---

# Think-cell chart data exports

When adding or editing charts with download buttons, follow this pattern.

## Dual download buttons

Every chart that supports export must offer:

1. **Download data (raw)** — the exact data frame used to build the ggplot, written as `.xlsx` without reshaping.
2. **Download data (think-cell)** — the same data passed through `format_tc_data()`, written as `.xlsx`.

Use `chart_data_downloads_ui()` and `chart_data_downloads_server()` from `dashboard/utils/chart_downloads.R`.

The think-cell button is shown only when `chart_type` is in `TC_SUPPORTED_CHART_TYPES`:

- `line`
- `bar`
- `stacked_bar`
- `grouped_bar`
- `waterfall`

If a chart does not match one of these types, wire only the raw download (or omit think-cell export until the chart type is added to `dashboard/utils/format_thinkcell_download.R`).

## ggplot to export column mapping

| ggplot aesthetic | `format_tc_data()` argument |
|------------------|----------------------------|
| `aes(x = ...)`   | `category_col`               |
| `aes(fill = ...)` or `aes(color = ...)` or `aes(group = ...)` | `series_col` |
| `aes(y = ...)`   | `value_col`                  |
| `facet_wrap(~ ...)` / `facet_grid(...)` | `facet_col` (optional; one Excel sheet per facet level) |

Set `chart_type` explicitly to match the think-cell chart the PM will build in PowerPoint.

- `line` → column/line orientation (series in rows, categories in columns)
- `bar`, `stacked_bar`, `grouped_bar` → bar orientation (transposed matrix)
- `stacked_bar` vs `grouped_bar` → same Excel layout; stacking is configured in think-cell

## Rules

- Never hand-roll `pivot_wider()` for think-cell exports in `dashboard/app.R`.
- Pass the same reactive/filtered data to both the plot and the download handlers.
- Prefer `agg_fun = NULL` when the plot data is already aggregated.
- Add new chart types in `dashboard/utils/format_thinkcell_download.R` with tests before exposing the think-cell button.

## Example wiring

```r
plot_data <- reactive({ filtered_chart_data() })

output$my_chart <- renderPlot({
  plot_data() %>%
    ggplot(aes(x = quarter, y = revenue, fill = product)) +
    geom_col(position = "stack")
})

chart_data_downloads_server(
  id = "my_chart_downloads",
  data = plot_data,
  chart_type = "stacked_bar",
  category_col = "quarter",
  series_col = "product",
  value_col = "revenue",
  filename_prefix = "revenue_chart",
  agg_fun = NULL
)
```
