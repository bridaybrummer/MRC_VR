# District QA Project

This project runs simple district-level QA checks:

- district death counts,
- district coverage (which districts have zero deaths),
- crude death rates using MYPES district population,
- district maps of counts and crude rates.

## Files

- `wrangling.R`: builds counts, crude rates, and map-ready tables.
- `index.qmd`: renders the QA report.
- `district_veracity_results.rda`: generated output used by `index.qmd`.

## Run

```bash
Rscript projects/district_variable_veracity/wrangling.R
quarto render projects/district_variable_veracity/index.qmd
```

Optional analysis year override:

```bash
ANALYSIS_YEAR=2022 Rscript projects/district_variable_veracity/wrangling.R
```

## Data sources

- `Deaths2022_MRCversionFINAL.feather` for death counts.
- `projects/data_explorer/population_data.rda` for MYPES district population denominators.
- `projects/data_explorer/shape_files.rda` for district map geometry.

## Interpretation

This is a descriptive triage view. Districts with zero deaths or very high/low crude rates are candidates for deeper review of reporting completeness, coding, and denominator alignment.
