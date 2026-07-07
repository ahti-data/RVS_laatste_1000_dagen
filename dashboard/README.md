# RVS Tool

Korte reminder om de app lokaal te starten.

## Lokaal runnen (aanrader: R Interactive)

1. Open een **R Interactive** terminal in VS Code.
2. Run:

```r
setwd("dashboard")
shiny::runApp("app.R")
```


asdfkjhasdf

## Alternatief (als je al in de projectmap zit)

```r
shiny::runApp("app.R")
```

## Stoppen

- Druk `Esc` in de R-console, of gebruik `Ctrl + C` in de terminal.

## Opmerking

- Benodigde packages worden in `app.R` automatisch geïnstalleerd en geladen bij opstarten.
- Think-cell export utilities staan in `utils/`; tests draaien met `testthat::test_dir("tests")` vanuit de `dashboard/` map.
