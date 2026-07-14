"""Write the expanded poisoning index.qmd."""
import pathlib

QMD = r"""---
title: "Poisoning Deaths in South Africa"
description: "Epidemiology of poisoning-related mortality from the national vital registration system, 1997–2022"
date: "2026-03-09"
categories: [poisoning, injury-mortality, vital-registration, icd10, epidemiology]
execute:
  echo: false
  warning: false
  message: false
---

```{r}
#| label: setup
#| include: false

library(data.table)
library(ggplot2)
library(flextable)
library(scales)

if (file.exists("poisoning_results.rda")) {
  load("poisoning_results.rda")
} else {
  stop("Run poisoning_wrangling.R first:\n  Rscript projects/poisoning_deaths/poisoning_wrangling.R")
}
```

## Overview

Poisoning is a significant cause of injury death in South Africa. This page describes the epidemiology of poisoning deaths recorded in the national vital registration (VR) system, using underlying-cause ICD-10 codes. Four intent categories are distinguished:

| ICD-10 block | Category | Description |
|:---|:---|:---|
| X40–X49 | Accidental | Unintentional exposure to noxious substances |
| X60–X69 | Self-harm | Intentional self-poisoning |
| X85 | Assault | Poisoning inflicted by another person |
| Y10–Y19 | Undetermined | Intent unknown or not specified |

### Summary statistics

```{r}
#| label: tbl-overall
#| tbl-cap: "Table 1. Overall summary — poisoning deaths, vital registration"

if (exists("tbl_overall")) {
  flextable(as.data.frame(tbl_overall)) |>
    bold(part = "header") |>
    theme_booktabs() |>
    width(j = 1, width = 2.8) |>
    width(j = 2, width = 2.5)
} else {
  message("Run poisoning_wrangling.R")
}
```

---

## Annual trends

### Total poisoning deaths

Annual count of poisoning deaths with a linear trend line.

```{r}
#| label: fig-annual-trend
#| fig-cap: "Fig. 1. Annual poisoning deaths. Bars = observed count; dashed red line = OLS trend with 95% CI."
#| fig-width: 9
#| fig-height: 4.5

if (exists("fig_annual_trend")) fig_annual_trend else message("Run poisoning_wrangling.R")
```

### Trends by intent

The stacked area chart shows each intent category over time. Changes in proportional composition can indicate genuine shifts in poisoning patterns or changes in coding completeness.

```{r}
#| label: fig-intent-trend
#| fig-cap: "Fig. 2. Annual poisoning deaths by intent (stacked). Blue = accidental; red = self-harm; orange = assault; green = undetermined."
#| fig-width: 9
#| fig-height: 5

if (exists("fig_intent_trend")) fig_intent_trend else message("Run poisoning_wrangling.R")
```

### Annual summary table

```{r}
#| label: tbl-year-summary
#| tbl-cap: "Table 2. Annual poisoning deaths by intent, % male, and median age"

if (exists("tbl_year_summary")) {
  flextable(as.data.frame(tbl_year_summary)) |>
    bold(part = "header") |>
    theme_booktabs() |>
    fontsize(size = 9, part = "all") |>
    autofit()
} else {
  message("Run poisoning_wrangling.R")
}
```

---

## Epicurve

### Weekly time series

The weekly epicurve shows the raw week-by-week count of poisoning deaths from 1997 to 2022. The red LOESS smoother highlights the long-term trajectory.

```{r}
#| label: fig-weekly-epicurve
#| fig-cap: "Fig. 3. Weekly poisoning deaths — epicurve. Blue bars = weekly counts; red line = LOESS smoother."
#| fig-width: 11
#| fig-height: 4.5

if (exists("fig_weekly_epicurve")) fig_weekly_epicurve else message("Run poisoning_wrangling.R")
```

### Monthly seasonality

Average deaths per calendar month (averaged across all years), with ±1 SD error bars. A consistent seasonal pattern would suggest environmental or behavioural drivers (e.g. agricultural pesticide exposure in summer, indoor heating–related gas poisoning in winter).

```{r}
#| label: fig-seasonality
#| fig-cap: "Fig. 4. Mean poisoning deaths per calendar month (+/-1 SD across years)."
#| fig-width: 9
#| fig-height: 4

if (exists("fig_seasonality")) fig_seasonality else message("Run poisoning_wrangling.R")
```

---

## Age and sex

### Age-sex pyramid

The population pyramid shows the sex- and age-distribution of all poisoning deaths. Males to the left, females to the right.

```{r}
#| label: fig-pyramid
#| fig-cap: "Fig. 5. Age-sex pyramid — all poisoning deaths. Blue = male; red = female."
#| fig-width: 7
#| fig-height: 5.5

if (exists("fig_pyramid")) fig_pyramid else message("Run poisoning_wrangling.R")
```

### Age-sex pyramid by intent

```{r}
#| label: fig-pyramid-intent
#| fig-cap: "Fig. 6. Age-sex pyramids by intent category (accidental, self-harm, undetermined). X-axis scales differ by panel."
#| fig-width: 11
#| fig-height: 5.5

if (exists("fig_pyramid_intent")) fig_pyramid_intent else message("Run poisoning_wrangling.R")
```

### Age × sex table

```{r}
#| label: tbl-age-sex
#| tbl-cap: "Table 3. Poisoning deaths by age group and sex"

if (exists("tbl_age_sex")) {
  flextable(as.data.frame(tbl_age_sex)) |>
    bold(part = "header") |>
    theme_booktabs() |>
    autofit()
} else {
  message("Run poisoning_wrangling.R")
}
```

---

## Geographic distribution

Counts by province of **usual residence** (`ResProvince`), excluding records coded as "Outside SA" or "Unspecified".

```{r}
#| label: fig-provincial
#| fig-cap: "Fig. 7. Poisoning deaths by province of usual residence. Provinces ordered by count."
#| fig-width: 8
#| fig-height: 5

if (exists("fig_provincial")) fig_provincial else message("Run poisoning_wrangling.R")
```

---

## Cause breakdown — ICD subcodes

The top 15 three-character ICD-10 subcodes within the poisoning block, ranked by total deaths across the study period.

```{r}
#| label: fig-top-icd
#| fig-cap: "Fig. 8. Top 15 ICD-10 subcodes for poisoning deaths."
#| fig-width: 10
#| fig-height: 5.5

if (exists("fig_top_icd")) fig_top_icd else message("Run poisoning_wrangling.R")
```

---

## Next steps

- Add population denominators to compute age-standardised mortality rates.
- Stratify provincial trends by intent.
- Link to external toxicology data (e.g. NPIS calls, emergency department presentations) for triangulation.
- Investigate the spike/dip years visible in the epicurve.
"""

dest = pathlib.Path("/Users/briday/Desktop/study_stats/MRC_VR/projects/poisoning_deaths/index.qmd")
dest.write_text(QMD)
print(f"Written: {dest} ({dest.stat().st_size} bytes, {QMD.count(chr(10))} lines)")
