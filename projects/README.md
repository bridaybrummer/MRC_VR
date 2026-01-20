# Projects Directory

This directory contains individual analysis projects. Each project follows a consistent structure:

```
project_name/
├── index.qmd          # Main output document (appears on website)
├── _wrangling.R       # Data wrangling script (optional, can use root scripts)
├── outputs/           # Project-specific outputs (optional)
└── data/              # Project-specific data (optional)
```

## Available Projects

| Project | Description | Status |
|---------|-------------|--------|
| `mortality_estimates/` | HIV/AIDS and TB mortality M&E | ✅ Active |
| `national_overview/` | National excess mortality analysis | ✅ Active |
| `cause_codes/` | ICD code-specific analyses | ✅ Active |
| `dha_access/` | DHA office accessibility analysis | ✅ Active |
| `icc_analysis/` | ICC for eMCCD pilot study | ✅ Active |
| `j_code_modelling/` | Respiratory mortality modelling | ✅ Active |
| `ci_modelling/` | CI methods documentation | ✅ Active |

## Adding a New Project

1. Create a new folder: `projects/new_project/`

2. Create `index.qmd` with the required frontmatter:
   ```yaml
   ---
   title: "Project Title"
   description: "Brief description for the listing"
   date: "YYYY-MM-DD"
   categories: [tag1, tag2]
   ---
   ```

3. Add your wrangling script either:
   - In the project folder as `_wrangling.R`
   - In the root directory (existing pattern)

4. The project will automatically appear on the homepage listing.

## Shared Settings

The `_metadata.yml` file in this directory provides default settings for all projects:

- Table of contents enabled
- Code folding enabled
- Warnings/messages suppressed
- Echo disabled (code hidden by default)

Individual projects can override these settings in their frontmatter.
