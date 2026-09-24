# Uses of mortality data figure

This project recreates Figure 1 from `Poster contents (003).docx` using base R.
The eight pictograms are drawn programmatically, so the output has no external
image, font, or package dependencies.

## Render

Open `uses_of_mortality_figure.Rproj`, then run:

```r
source("render.R")
```

Or render from a terminal:

```sh
Rscript render.R
```

The 300-DPI PNG is written to `output/uses_of_mortality_data.png`.
