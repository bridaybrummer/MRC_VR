# Poisoning deaths project

This project contains a starter workflow to analyze poisoning-related deaths from the MRC vital registration dataset.

## Run

```bash
Rscript projects/poisoning_deaths/poisoning_wrangling.R
quarto render projects/poisoning_deaths/index.qmd
```

## Outputs

- `poisoning_results.rda`: weekly series, annual summary table, and plot objects used by `index.qmd`.
