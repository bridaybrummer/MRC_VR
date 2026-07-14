# MRC Vital Registration Analysis

Analysis of South African vital registration data quality and mortality estimates.

🌐 **Live Site**: [https://bridaybrummer.github.io/MRC_VR](https://bridaybrummer.github.io/MRC_VR)

## Project Structure

```
MRC_VR/
├── _quarto.yml              # Main Quarto website configuration
├── index.qmd                # Homepage with project listing
├── about.qmd                # About page
├── styles.css               # Custom CSS styles
│
├── projects/                # Individual analysis projects
│   ├── _metadata.yml        # Shared settings for all projects
│   ├── mortality_estimates/ # HIV/AIDS, TB mortality comparisons
│   ├── dha_access/          # DHA office accessibility analysis
│   ├── icc_analysis/        # ICC for eMCCD pilot
│   └── j_code_modelling/    # Respiratory mortality modelling
│
├── outputs/                 # Generated data objects (.RData)
├── docs/                    # Rendered website (auto-generated)
│
├── ME_wrangling.r           # Monitoring and evaluation data wrangling
├── plotting_DHA_offices.r   # DHA access data wrangling
├── ICC_wrangling_v2.R       # ICC analysis wrangling
├── J_code_modelling.r       # J-code modelling script
│
└── data/                    # Raw data files (not in git)
```

## Quick Start

### Prerequisites

- R (≥ 4.2.0)
- Quarto CLI (≥ 1.3.0)
- RStudio (recommended)

### Required R Packages

```r
install.packages(c(
  "dplyr", "data.table", "ggplot2", "plotly", 
  "sf", "arrow", "haven", "readxl",
  "scales", "glue", "tidyr", "flextable",
  "htmlwidgets", "lubridate", "stringr"
))

# For NMCleaner (if available)
# remotes::install_github("bridaybrummer/NMCleaner")
```

### Running Locally

1. **Clone the repository**
   ```bash
   git clone https://github.com/bridaybrummer/MRC_VR.git
   cd MRC_VR
   ```

2. **Run wrangling scripts** (generates outputs/*.RData)
   ```r
   source("ME_wrangling.r")
   source("plotting_DHA_offices.r")
   ```

3. **Preview the website**
   ```bash
   quarto preview
   ```

4. **Render the full website**
   ```bash
   quarto render
   ```

## Development Workflow

### Working on a Single Project

Each project can be developed independently:

```r
# 1. Open the wrangling script and run interactively
# e.g., ME_wrangling.r

# 2. When outputs are ready, preview the project page
quarto preview projects/mortality_estimates/index.qmd
```

### Rendering the Full Site

```bash
# Render all pages
quarto render

# The output will be in docs/
```

## Deployment to GitHub Pages

### Option 1: Manual Deployment

1. Render the site locally:
   ```bash
   quarto render
   ```

2. Commit and push the `docs/` folder:
   ```bash
   git add docs/
   git commit -m "Update website"
   git push
   ```

3. Enable GitHub Pages:
   - Go to repository Settings → Pages
   - Source: "Deploy from a branch"
   - Branch: `main` (or `master`)
   - Folder: `/docs`

### Option 2: GitHub Actions (Automated)

The repository includes a GitHub Actions workflow (`.github/workflows/publish.yml`) that automatically rebuilds the site when you push changes.

1. Enable GitHub Actions in your repository
2. Push changes to `main` branch
3. The site will be automatically deployed

### Option 3: Quarto Publish

```bash
# First time setup
quarto publish gh-pages

# Subsequent updates
quarto publish gh-pages
```

## Data Files

Large data files are not included in the repository. Place these files in the project root:

| File | Description | Source |
|------|-------------|--------|
| `LGH_MasterFile_preCollapsedAll.feather` | Death records | LGH project |
| `DHA_offices_2024.dta` | DHA office locations | Tom Moultrie |
| `ME_dashboard/*.xlsx` | Tembisa model outputs | Thembisa project |

## Contributing

1. Create a feature branch
2. Make your changes
3. Run `quarto render` to test
4. Submit a pull request

## License

MIT License - see [LICENSE](LICENSE) for details.

## Contact

- **Author**: [Your Name]
- **Email**: [your.email@example.com]
- **Institution**: Medical Research Council, South Africa
