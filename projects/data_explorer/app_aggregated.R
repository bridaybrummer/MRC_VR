# =============================================================================
# MRC VR Data Explorer Shiny App - AGGREGATED DATA VERSION
# Interactive exploration of South African mortality data (1997-2022)
# =============================================================================
# This version uses pre-aggregated CSV files instead of raw data:
# - Deployable to shinyapps.io (~375MB vs 1.8GB)
# - No individual-level data exposure
# - Same functionality minus: Raw Data tab, Registration Delay chart
# =============================================================================

library(shiny)
library(bslib)
library(data.table)
library(plotly)
library(DT)
library(waiter)           # For startup loading screen
library(shinycssloaders)  # For plot spinners
library(sf)               # For choropleth maps
library(dplyr)            # For left_join in choropleth maps

# Resolve conflicted package issues (NMCleaner loads conflicted)
if (requireNamespace("conflicted", quietly = TRUE)) {
  conflicted::conflicts_prefer(plotly::layout)
  conflicted::conflicts_prefer(dplyr::filter)
}

# =============================================================================
# HELPER: Get app directory (works locally and on shinyapps.io)
# =============================================================================
get_app_dir <- function() {
  # Try different methods to find app directory
  if (file.exists("aggregated_data")) {
    return(".")
  }
  if (file.exists("projects/data_explorer/aggregated_data")) {
    return("projects/data_explorer")
  }
  # For shinyapps.io deployment
  app_dir <- getwd()
  if (file.exists(file.path(app_dir, "aggregated_data"))) {
    return(app_dir)
  }
  stop("Cannot find aggregated_data directory")
}

# =============================================================================
# DATA LOADING - FROM AGGREGATED CSVs
# =============================================================================

load_aggregated_data <- function() {
  message("Loading aggregated data...")
  start_time <- Sys.time()
  
  app_dir <- get_app_dir()
  data_dir <- file.path(app_dir, "aggregated_data")
  
  agg <- list()
  
  # Province name mapping (DeathProvince code → full name)
  province_map <- c(
    "1" = "Western Cape", "2" = "Eastern Cape", "3" = "Northern Cape",
    "4" = "Free State", "5" = "KwaZulu-Natal", "6" = "North West",
    "7" = "Gauteng", "8" = "Mpumalanga", "9" = "Limpopo"
  )
  
  # Sex name mapping
  sex_map <- c("1" = "Male", "2" = "Female", "9" = "Unknown")
  
  # Age group label mapping (agegroup10 → standard labels)
  agegroup_map <- c(
    "<1" = "0", "1-4" = "1-4", "5-14" = "5-14", "15-24" = "15-24",
    "25-34" = "25-34", "35-44" = "35-44", "45-54" = "45-54",
    "55-64" = "55-64", "65-74" = "65-74", "75-84" = "75-84", "85+" = "85+"
  )
  
  # Load full aggregation (main data source)
  message("  Loading full_aggregation.csv...")
  full <- fread(file.path(data_dir, "full_aggregation.csv"))
  
  # Convert codes to names
  full[, DeathProvinceName := province_map[as.character(DeathProvince)]]
  full[, SexName := sex_map[as.character(Sex)]]
  full[, agegroup := factor(agegroup10, levels = names(agegroup_map), labels = agegroup_map)]
  full[, count := deaths]
  
  # Convert types
  full[, epi_year := as.integer(epi_year)]
  full[, epi_week := as.integer(epi_week)]
  full[, NaturalUnnatural := as.integer(NaturalUnnatural)]
  
  setkey(full, epi_year)
  
  # Create weekly aggregation
  agg$weekly <- full[, .(count = sum(count)), 
                     by = .(epi_year, epi_week, DeathProvinceName, NaturalUnnatural)]
  setkey(agg$weekly, epi_year)
  message("  ✓ weekly: ", format(nrow(agg$weekly), big.mark = ","), " rows")
  
  # Create yearly province aggregation
  agg$yearly_province <- full[, .(count = sum(count)), 
                              by = .(epi_year, DeathProvinceName, agegroup, SexName, NaturalUnnatural)]
  setkey(agg$yearly_province, epi_year)
  message("  ✓ yearly_province: ", format(nrow(agg$yearly_province), big.mark = ","), " rows")
  
  # Create cause aggregation (without mean_age since not available in aggregated data)
  agg$causes <- full[!is.na(UnderlyingCause) & UnderlyingCause != "",
                     .(count = sum(count)),
                     by = .(epi_year, UnderlyingCause, DeathProvinceName, agegroup, SexName)]
  setkey(agg$causes, epi_year)
  message("  ✓ causes: ", format(nrow(agg$causes), big.mark = ","), " rows")
  
  # Load district data
  message("  Loading district_year.csv...")
  districts <- fread(file.path(data_dir, "district_year.csv"))
  districts[, epi_year := as.integer(epi_year)]
  districts[, count := deaths]
  agg$districts <- districts
  setkey(agg$districts, epi_year)
  message("  ✓ districts: ", format(nrow(agg$districts), big.mark = ","), " rows")
  
  # Load pyramid data for detailed age-sex breakdowns
  message("  Loading pyramid_data.csv...")
  pyramid <- fread(file.path(data_dir, "pyramid_data.csv"))
  pyramid[, DeathProvinceName := province_map[as.character(DeathProvince)]]
  pyramid[, SexName := sex_map[as.character(Sex)]]
  pyramid[, epi_year := as.integer(epi_year)]
  pyramid[, count := deaths]
  agg$pyramid <- pyramid
  message("  ✓ pyramid: ", format(nrow(agg$pyramid), big.mark = ","), " rows")
  
  elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
  message(sprintf("Aggregated data loaded in %s seconds", elapsed))
  
  return(agg)
}

# =============================================================================
# LOAD SHAPE FILES FOR CHOROPLETH MAPS
# =============================================================================

load_shape_files <- function() {
  message("Loading shape files...")
  
  # Try relative path first (for deployment)
  shape_paths <- c(
    "shape_files.rda"
  )
  
  shape_path <- NULL
  for (p in shape_paths) {
    if (file.exists(p)) {
      shape_path <- p
      break
    }
  }
  
  if (is.null(shape_path)) {
    warning("Shape files not found")
    return(NULL)
  }
  
  load(shape_path)
  
  # Province name mapping
  province_name_map <- c(
    "WC" = "Western Cape", "EC" = "Eastern Cape", "NC" = "Northern Cape",
    "FS" = "Free State", "KZN" = "KwaZulu-Natal", "NW" = "North West",
    "GP" = "Gauteng", "MP" = "Mpumalanga", "LP" = "Limpopo"
  )
  
  # Add full province names to shape files
  if (exists("province_sf")) {
    province_sf$province_full <- province_name_map[province_sf$prov]
  }
  
  if (exists("district_sf")) {
    district_sf$province_full <- province_name_map[district_sf$prov]
  }
  
  message("  Shape files loaded successfully")
  
  return(list(
    provinces = if(exists("province_sf")) province_sf else NULL,
    districts = if(exists("district_sf")) district_sf else NULL
  ))
}

# =============================================================================
# LOAD POPULATION DATA
# =============================================================================

load_population_data <- function() {
  message("Loading population data...")
  
  pop_paths <- c(
    "population_data.rda"
  )
  
  pop_path <- NULL
  for (p in pop_paths) {
    if (file.exists(p)) {
      pop_path <- p
      break
    }
  }
  
  if (is.null(pop_path)) {
    warning("Population data not found")
    return(NULL)
  }
  
  load(pop_path)
  
  result <- list()
  
  # Handle the pop.rda format from NMCleaner
  if (exists("pop")) {
    pop <- as.data.table(pop)
    pop[, Year := as.integer(Year)]
    
    # Aggregate to province level (sum all ages and sexes)
    province_pop <- pop[, .(population = sum(Population, na.rm = TRUE)), 
                        by = .(Year, province_standard)]
    setnames(province_pop, "province_standard", "province_full")
    result$province <- province_pop
    
    # Aggregate to district level
    district_pop <- pop[, .(population = sum(Population, na.rm = TRUE)),
                        by = .(Year, district_standard)]
    result$district <- district_pop
    
    message("  Population data loaded: ", uniqueN(province_pop$Year), " years, ",
            uniqueN(province_pop$province_full), " provinces, ",
            uniqueN(district_pop$district_standard), " districts")
  }
  
  return(result)
}

# =============================================================================
# LOAD LGH LOOKUP
# =============================================================================

load_lgh_lookup <- function() {
  message("Loading LGH ICD-10 cause lookup...")
  
  lookup_paths <- c(
    here::here("LGH_ICD10_Cause_Lookup.rda"),
    "../../LGH_ICD10_Cause_Lookup.rda",
    "LGH_ICD10_Cause_Lookup.rda"
  )
  
  lookup_path <- NULL
  for (p in lookup_paths) {
    if (file.exists(p)) {
      lookup_path <- p
      break
    }
  }
  
  if (is.null(lookup_path)) {
    message("  LGH lookup not found, creating default")
    icd_lookup <- data.table(
      LGH_Cause = c("B24", "B33", "E10-E14", "I10-I15", "I20-I25", "I26-I28", "I42",
                   "I49-I51", "I60-I69", "J09-J18", "J20-J22", "J45", "J80", "J96-J98",
                   "N17-N19", "R00-R99+I46", "U07", "A00-B99", "C00-D48", "D50-D99",
                   "E00-E99*", "G00-G99", "I00-I99*", "J00-J99*", "N00-N99*", "P00-P99",
                   "ZZOthers (F/H/K-M/O/Q)"),
      description = c("HIV disease", "Other viral diseases", "Diabetes mellitus",
                     "Hypertensive diseases", "Ischaemic heart diseases",
                     "Pulmonary heart disease", "Cardiomyopathy",
                     "Cardiac arrhythmias/heart disease", "Cerebrovascular (stroke)",
                     "Influenza and pneumonia", "Acute lower respiratory infections",
                     "Asthma", "ARDS", "Respiratory failure", "Renal failure",
                     "Symptoms/signs + cardiac arrest", "COVID-19",
                     "Infectious/parasitic diseases", "Neoplasms", "Blood diseases",
                     "Endocrine/metabolic", "Nervous system", "Circulatory system",
                     "Respiratory system", "Genitourinary system", "Perinatal conditions",
                     "Other chapters")
    )
  } else {
    load(lookup_path)
    icd_lookup <- as.data.table(icd_lookup)
  }
  
  icd_lookup <- icd_lookup[!is.na(LGH_Cause)]
  icd_lookup[, display_label := paste0(LGH_Cause, " - ", description)]
  
  message("  LGH lookup loaded: ", nrow(icd_lookup), " cause groups")
  return(icd_lookup)
}

# =============================================================================
# UI
# =============================================================================

ui <- page_navbar(
  title = "MRC VR Data Explorer",
  theme = bs_theme(
    bootswatch = "cosmo",
    primary = "#0d6efd",
    font_scale = 0.9
  ),
  header = tagList(
    useWaiter(),
    waiterShowOnLoad(html = tagList(
      spin_fading_circles(),
      h4("Loading mortality data...", style = "color: white; margin-top: 20px;"),
      p("Using pre-aggregated data for fast performance", style = "color: #aaa;")
    ), color = "#0d6efd")
  ),
  
  # Overview Tab
  nav_panel(
    title = "Overview",
    icon = icon("chart-line"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Filters",
        width = 300,
        sliderInput("overview_years", "Year Range:",
                    min = 1997, max = 2022,
                    value = c(1997, 2022),
                    step = 1, sep = ""),
        selectInput("overview_province", "Province:",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE),
        radioButtons("overview_nat_unnat", "Death Type:",
                     choices = c("All" = "all",
                                 "Natural" = "1",
                                 "Unnatural" = "2"),
                     selected = "all")
      ),
      layout_columns(
        col_widths = c(8, 4),
        card(
          card_header("Deaths Over Time (Weekly)"),
          withSpinner(plotlyOutput("overview_time_plot", height = "350px"), type = 4, color = "#0d6efd")
        ),
        card(
          card_header("Summary"),
          verbatimTextOutput("overview_summary")
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Deaths by Province"),
          withSpinner(plotlyOutput("overview_province_plot", height = "350px"), type = 4, color = "#0d6efd")
        ),
        card(
          card_header("Deaths by Age Group"),
          withSpinner(plotlyOutput("overview_age_plot", height = "350px"), type = 4, color = "#198754")
        )
      )
    )
  ),
  
  # Temporal Trends Tab
  nav_panel(
    title = "Temporal Trends",
    icon = icon("calendar-alt"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Filters",
        width = 300,
        sliderInput("temporal_years", "Year Range:",
                    min = 1997, max = 2022,
                    value = c(2010, 2022),
                    step = 1, sep = ""),
        selectInput("temporal_province", "Province:",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE),
        radioButtons("temporal_nat_unnat", "Death Type:",
                     choices = c("All" = "all", "Natural" = "1", "Unnatural" = "2"),
                     selected = "all")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Annual Deaths Trend"),
          withSpinner(plotlyOutput("temporal_annual", height = "400px"), type = 4, color = "#0d6efd")
        ),
        card(
          card_header("Seasonal Pattern (Average by Week)"),
          withSpinner(plotlyOutput("temporal_seasonal", height = "400px"), type = 4, color = "#198754")
        )
      ),
      card(
        card_header("Heatmap: Deaths by Year and Week"),
        withSpinner(plotlyOutput("temporal_heatmap", height = "500px"), type = 4, color = "#6c757d")
      )
    )
  ),
  
  # Cause Codes Tab
  nav_panel(
    title = "Cause Codes",
    icon = icon("file-medical"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Filters",
        width = 300,
        sliderInput("cause_years", "Year Range:",
                    min = 1997, max = 2022,
                    value = c(2015, 2022),
                    step = 1, sep = ""),
        selectInput("cause_province", "Province:",
                    choices = NULL, selected = NULL, multiple = TRUE),
        selectInput("cause_agegroup", "Age Group:",
                    choices = NULL, selected = NULL, multiple = TRUE),
        selectInput("cause_sex", "Sex:",
                    choices = c("All" = "all", "Male" = "Male", "Female" = "Female"),
                    selected = "all"),
        selectInput("cause_code", "ICD-10 Code (prefix):",
                    choices = NULL, selected = NULL, multiple = TRUE),
        helpText("Select codes like 'B24' (HIV), 'E10' (Diabetes), 'I' (Circulatory)")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Top 20 Cause Codes"),
          withSpinner(plotlyOutput("cause_top20", height = "450px"), type = 4, color = "#0d6efd")
        ),
        card(
          card_header("Cause Code Trends Over Time"),
          withSpinner(plotlyOutput("cause_trends", height = "450px"), type = 4, color = "#198754")
        )
      ),
      card(
        card_header("Detailed Cause Code Table"),
        withSpinner(DTOutput("cause_table"), type = 4, color = "#6c757d")
      )
    )
  ),
  
  # Demographics Tab
  nav_panel(
    title = "Demographics",
    icon = icon("users"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Filters",
        width = 300,
        sliderInput("demo_years", "Year Range:",
                    min = 1997, max = 2022,
                    value = c(2015, 2022),
                    step = 1, sep = ""),
        selectInput("demo_province", "Province:",
                    choices = NULL, selected = NULL, multiple = TRUE),
        radioButtons("demo_nat_unnat", "Death Type:",
                     choices = c("All" = "all", "Natural" = "1", "Unnatural" = "2"),
                     selected = "all")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Population Pyramid"),
          withSpinner(plotlyOutput("demo_pyramid", height = "450px"), type = 4, color = "#0d6efd")
        ),
        card(
          card_header("Age Distribution Over Time"),
          withSpinner(plotlyOutput("demo_age_time", height = "450px"), type = 4, color = "#198754")
        )
      ),
      card(
        card_header("Sex Ratio by Age Group"),
        withSpinner(plotlyOutput("demo_sex_ratio", height = "350px"), type = 4, color = "#6c757d")
      )
    )
  ),
  
  # Geographic Tab
  nav_panel(
    title = "Geographic",
    icon = icon("map"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Filters",
        width = 300,
        sliderInput("geo_years", "Year Range:",
                    min = 1997, max = 2022,
                    value = c(2015, 2022),
                    step = 1, sep = ""),
        selectInput("geo_agegroup", "Age Group:",
                    choices = NULL, selected = NULL, multiple = TRUE),
        radioButtons("geo_map_metric", "Map Display:",
                     choices = c("Total Deaths" = "count", "Rate per 100k" = "rate"),
                     selected = "count"),
        hr(),
        radioButtons("geo_cause_type", "Cause Grouping:",
                     choices = c("All Causes" = "all",
                                 "Individual ICD-10 Codes" = "icd10",
                                 "LGH Cause Groups" = "lgh"),
                     selected = "all"),
        conditionalPanel(
          condition = "input.geo_cause_type == 'icd10'",
          selectInput("geo_cause", "ICD-10 Code (prefix):",
                      choices = NULL, selected = NULL, multiple = TRUE),
          helpText("e.g., 'B24' for HIV, 'I' for all circulatory")
        ),
        conditionalPanel(
          condition = "input.geo_cause_type == 'lgh'",
          selectInput("geo_lgh_cause", "LGH Cause Group:",
                      choices = NULL, selected = NULL, multiple = TRUE),
          helpText("Pre-defined cause groupings from LGH classification")
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Deaths by Province (Choropleth)"),
          withSpinner(plotlyOutput("geo_province_map", height = "450px"), type = 4, color = "#0d6efd")
        ),
        card(
          card_header("Deaths by District (Choropleth)"),
          withSpinner(plotlyOutput("geo_district_map", height = "450px"), type = 4, color = "#198754")
        )
      ),
      card(
        card_header("Deaths by Province (Bar Chart)"),
        withSpinner(plotlyOutput("geo_province_bar", height = "350px"), type = 4, color = "#0d6efd")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Top 20 Districts"),
          withSpinner(plotlyOutput("geo_district_plot", height = "400px"), type = 4, color = "#198754")
        ),
        card(
          card_header("Province Trends Over Time"),
          withSpinner(plotlyOutput("geo_province_time", height = "400px"), type = 4, color = "#0d6efd")
        )
      )
    )
  ),
  
  # Code Comparison Tab
  nav_panel(
    title = "Code Comparison",
    icon = icon("balance-scale"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Compare ICD-10 Code Groups",
        width = 350,
        sliderInput("compare_years", "Year Range:",
                    min = 1997, max = 2022,
                    value = c(2015, 2022),
                    step = 1, sep = ""),
        selectInput("compare_province", "Province:",
                    choices = NULL, selected = NULL, multiple = TRUE),
        selectInput("compare_agegroup", "Age Group:",
                    choices = NULL, selected = NULL, multiple = TRUE),
        selectInput("compare_sex", "Sex:",
                    choices = c("All" = "all", "Male" = "Male", "Female" = "Female"),
                    selected = "all"),
        hr(),
        h6("Group A"),
        selectInput("compare_group_a", "ICD-10 Codes:",
                    choices = NULL, selected = NULL, multiple = TRUE),
        textInput("compare_group_a_name", "Label:", value = "Group A"),
        hr(),
        h6("Group B"),
        selectInput("compare_group_b", "ICD-10 Codes:",
                    choices = NULL, selected = NULL, multiple = TRUE),
        textInput("compare_group_b_name", "Label:", value = "Group B"),
        helpText("Compare any two groups of ICD-10 codes. Select code prefixes to include.")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Trends Over Time"),
          withSpinner(plotlyOutput("compare_time_plot", height = "350px"), type = 4, color = "#0d6efd")
        ),
        card(
          card_header("Total Deaths"),
          withSpinner(plotlyOutput("compare_totals_plot", height = "350px"), type = 4, color = "#dc3545")
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Age Distribution (%)"),
          withSpinner(plotlyOutput("compare_age_plot", height = "350px"), type = 4, color = "#198754")
        ),
        card(
          card_header("Sex Distribution (%)"),
          withSpinner(plotlyOutput("compare_sex_plot", height = "350px"), type = 4, color = "#6c757d")
        )
      ),
      card(
        card_header("Province Distribution (%)"),
        withSpinner(plotlyOutput("compare_province_plot", height = "350px"), type = 4, color = "#0d6efd")
      ),
      card(
        card_header("Code Details"),
        withSpinner(DTOutput("compare_table"), type = 4, color = "#6c757d")
      )
    )
  ),
  
  # LGH Causes Tab
  nav_panel(
    title = "LGH Causes",
    icon = icon("heartbeat"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Compare LGH Cause Groups",
        width = 350,
        sliderInput("lgh_years", "Year Range:",
                    min = 1997, max = 2022,
                    value = c(2015, 2022),
                    step = 1, sep = ""),
        selectInput("lgh_province", "Province:",
                    choices = NULL, selected = NULL, multiple = TRUE),
        selectInput("lgh_agegroup", "Age Group:",
                    choices = NULL, selected = NULL, multiple = TRUE),
        selectInput("lgh_sex", "Sex:",
                    choices = c("All" = "all", "Male" = "Male", "Female" = "Female"),
                    selected = "all"),
        hr(),
        h6("Group A - LGH Causes"),
        selectInput("lgh_group_a", "Select Cause Group(s):",
                    choices = NULL, selected = NULL, multiple = TRUE),
        hr(),
        h6("Group B - LGH Causes"),
        selectInput("lgh_group_b", "Select Cause Group(s):",
                    choices = NULL, selected = NULL, multiple = TRUE),
        helpText("Compare pre-defined LGH cause categories")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Trends Over Time"),
          withSpinner(plotlyOutput("lgh_time_plot", height = "350px"), type = 4, color = "#0d6efd")
        ),
        card(
          card_header("Total Deaths"),
          withSpinner(plotlyOutput("lgh_totals_plot", height = "350px"), type = 4, color = "#dc3545")
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Age Distribution (%)"),
          withSpinner(plotlyOutput("lgh_age_plot", height = "350px"), type = 4, color = "#198754")
        ),
        card(
          card_header("Sex Distribution (%)"),
          withSpinner(plotlyOutput("lgh_sex_plot", height = "350px"), type = 4, color = "#6c757d")
        )
      ),
      card(
        card_header("Province Distribution (%)"),
        withSpinner(plotlyOutput("lgh_province_plot", height = "350px"), type = 4, color = "#0d6efd")
      ),
      card(
        card_header("Summary Statistics"),
        withSpinner(DTOutput("lgh_table"), type = 4, color = "#6c757d")
      )
    )
  ),
  
  # About Tab
  nav_panel(
    title = "About",
    icon = icon("info-circle"),
    card(
      card_header("About This App"),
      markdown("
## MRC VR Data Explorer (Aggregated Version)

This Shiny application provides interactive exploration of South African vital registration mortality data from 1997-2022.

### Features
- **Overview**: Summary statistics and trends
- **Temporal Trends**: Weekly/annual patterns, seasonality
- **Cause Codes**: ICD-10 cause of death analysis
- **Demographics**: Age/sex distributions
- **Geographic**: Province and district maps
- **Code Comparison**: Compare any ICD-10 code groups
- **LGH Causes**: Pre-defined cause category comparisons

### Data
- ~13 million death records (1997-2022)
- Pre-aggregated for performance and privacy
- No individual-level data exposed

### Deployment
This version uses pre-aggregated CSV files (~375MB) suitable for shinyapps.io deployment.
      ")
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
  
  # Load aggregated data at startup
  agg <- load_aggregated_data()
  shapes <- load_shape_files()
  lgh_lookup <- load_lgh_lookup()
  pop_data <- load_population_data()
  
  # Hide the loading screen

  waiter_hide()
  
  # Get unique values for filters
  provinces <- unique(agg$yearly_province$DeathProvinceName)
  provinces <- provinces[!is.na(provinces)]
  provinces <- sort(provinces)
  
  agegroups <- c("0", "1-4", "5-14", "15-24", "25-34", "35-44", 
                 "45-54", "55-64", "65-74", "75-84", "85+")
  
  # Get unique cause codes from data
  cause_codes <- sort(unique(agg$causes$UnderlyingCause))
  cause_prefixes <- unique(substr(cause_codes, 1, 3))
  cause_prefixes <- sort(cause_prefixes[nchar(cause_prefixes) > 0])
  
  # Create LGH choices
  lgh_choices <- setNames(lgh_lookup$LGH_Cause, lgh_lookup$display_label)
  
  # Update all selectInputs with data-driven choices
  observe({
    updateSelectInput(session, "overview_province", choices = c("All" = "", provinces), selected = "")
    updateSelectInput(session, "temporal_province", choices = c("All" = "", provinces), selected = "")
    updateSelectInput(session, "cause_province", choices = c("All" = "", provinces), selected = "")
    updateSelectInput(session, "cause_agegroup", choices = c("All" = "", agegroups), selected = "")
    updateSelectInput(session, "cause_code", choices = c("All" = "", cause_prefixes), selected = "")
    updateSelectInput(session, "demo_province", choices = c("All" = "", provinces), selected = "")
    updateSelectInput(session, "geo_agegroup", choices = c("All" = "", agegroups), selected = "")
    updateSelectInput(session, "geo_cause", choices = c("All" = "", cause_prefixes), selected = "")
    updateSelectInput(session, "geo_lgh_cause", choices = lgh_choices, selected = NULL)
    updateSelectInput(session, "compare_province", choices = c("All" = "", provinces), selected = "")
    updateSelectInput(session, "compare_agegroup", choices = c("All" = "", agegroups), selected = "")
    updateSelectInput(session, "compare_group_a", choices = cause_prefixes, selected = NULL)
    updateSelectInput(session, "compare_group_b", choices = cause_prefixes, selected = NULL)
    updateSelectInput(session, "lgh_province", choices = c("All" = "", provinces), selected = "")
    updateSelectInput(session, "lgh_agegroup", choices = c("All" = "", agegroups), selected = "")
    updateSelectInput(session, "lgh_group_a", choices = lgh_choices, selected = NULL)
    updateSelectInput(session, "lgh_group_b", choices = lgh_choices, selected = NULL)
  })
  
  # ==========================================================================
  # OVERVIEW TAB
  # ==========================================================================
  
  overview_weekly <- reactive({
    d <- agg$weekly[epi_year >= input$overview_years[1] & epi_year <= input$overview_years[2]]
    
    if (length(input$overview_province) > 0 && !("" %in% input$overview_province)) {
      d <- d[DeathProvinceName %in% input$overview_province]
    }
    
    if (input$overview_nat_unnat != "all") {
      d <- d[NaturalUnnatural == as.integer(input$overview_nat_unnat)]
    }
    
    d[, .(count = sum(count)), by = .(epi_year, epi_week)]
  }) |> bindCache(input$overview_years, input$overview_province, input$overview_nat_unnat)
  
  overview_by_province <- reactive({
    d <- agg$yearly_province[epi_year >= input$overview_years[1] & epi_year <= input$overview_years[2]]
    
    if (length(input$overview_province) > 0 && !("" %in% input$overview_province)) {
      d <- d[DeathProvinceName %in% input$overview_province]
    }
    
    if (input$overview_nat_unnat != "all") {
      d <- d[NaturalUnnatural == as.integer(input$overview_nat_unnat)]
    }
    
    d
  }) |> bindCache(input$overview_years, input$overview_province, input$overview_nat_unnat)
  
  output$overview_time_plot <- renderPlotly({
    d <- overview_weekly()[order(epi_year, epi_week)]
    d[, date_approx := as.Date(paste0(epi_year, "-01-01")) + (epi_week - 1L) * 7L]
    
    plot_ly(d, x = ~date_approx, y = ~count, type = 'scatter', mode = 'lines',
            line = list(color = '#0d6efd', width = 1),
            hovertemplate = "Week: %{x}<br>Deaths: %{y:,}<extra></extra>") %>%
      layout(xaxis = list(title = "Date"),
             yaxis = list(title = "Deaths per Week"),
             hovermode = "x unified")
  })
  
  output$overview_province_plot <- renderPlotly({
    d <- overview_by_province()[, .(count = sum(count)), by = .(DeathProvinceName)]
    d <- d[!is.na(DeathProvinceName)][order(-count)]
    
    plot_ly(d, x = ~reorder(DeathProvinceName, count), y = ~count, type = 'bar',
            marker = list(color = '#0d6efd'),
            hovertemplate = "%{x}<br>Deaths: %{y:,}<extra></extra>") %>%
      layout(xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = "Total Deaths"))
  })
  
  output$overview_age_plot <- renderPlotly({
    d <- overview_by_province()[, .(count = sum(count)), by = .(agegroup)]
    d <- d[!is.na(agegroup)]
    
    plot_ly(d, x = ~agegroup, y = ~count, type = 'bar',
            marker = list(color = '#198754'),
            hovertemplate = "Age: %{x}<br>Deaths: %{y:,}<extra></extra>") %>%
      layout(xaxis = list(title = "Age Group"),
             yaxis = list(title = "Total Deaths"))
  })
  
  output$overview_summary <- renderPrint({
    d <- overview_by_province()
    total <- sum(d$count)
    cat("Data Summary\n")
    cat("============\n\n")
    cat("Total deaths:", format(total, big.mark = ","), "\n")
    cat("Year range:", input$overview_years[1], "-", input$overview_years[2], "\n")
    cat("Provinces:", length(unique(d$DeathProvinceName[!is.na(d$DeathProvinceName)])), "\n")
    cat("\nBy Sex:\n")
    sex_summary <- d[, .(count = sum(count)), by = SexName][order(-count)]
    print(sex_summary)
  })
  
  # ==========================================================================
  # TEMPORAL TRENDS TAB
  # ==========================================================================
  
  temporal_data <- reactive({
    d <- agg$weekly[epi_year >= input$temporal_years[1] & epi_year <= input$temporal_years[2]]
    
    if (length(input$temporal_province) > 0 && !("" %in% input$temporal_province)) {
      d <- d[DeathProvinceName %in% input$temporal_province]
    }
    
    if (input$temporal_nat_unnat != "all") {
      d <- d[NaturalUnnatural == as.integer(input$temporal_nat_unnat)]
    }
    
    d
  }) |> bindCache(input$temporal_years, input$temporal_province, input$temporal_nat_unnat)
  
  output$temporal_annual <- renderPlotly({
    d <- temporal_data()[, .(count = sum(count)), by = .(epi_year)]
    
    plot_ly(d, x = ~epi_year, y = ~count, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#0d6efd'),
            marker = list(color = '#0d6efd'),
            hovertemplate = "Year: %{x}<br>Deaths: %{y:,}<extra></extra>") %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "Total Deaths"))
  })
  
  output$temporal_seasonal <- renderPlotly({
    d <- temporal_data()[, .(avg_count = mean(count)), by = .(epi_week)]
    
    plot_ly(d, x = ~epi_week, y = ~avg_count, type = 'scatter', mode = 'lines',
            line = list(color = '#198754'),
            fill = 'tozeroy',
            fillcolor = 'rgba(25, 135, 84, 0.2)',
            hovertemplate = "Week: %{x}<br>Avg Deaths: %{y:,.0f}<extra></extra>") %>%
      layout(xaxis = list(title = "Week of Year", range = c(1, 53)),
             yaxis = list(title = "Average Deaths"))
  })
  
  output$temporal_heatmap <- renderPlotly({
    d <- temporal_data()[, .(count = sum(count)), by = .(epi_year, epi_week)]
    
    plot_ly(d, x = ~epi_week, y = ~epi_year, z = ~count, type = 'heatmap',
            colors = colorRamp(c("#f7fbff", "#08306b")),
            hovertemplate = "Week: %{x}<br>Year: %{y}<br>Deaths: %{z:,}<extra></extra>") %>%
      layout(xaxis = list(title = "Week of Year"),
             yaxis = list(title = "Year", autorange = "reversed"))
  })
  
  # ==========================================================================
  # CAUSE CODES TAB
  # ==========================================================================
  
  cause_data <- reactive({
    d <- agg$causes[epi_year >= input$cause_years[1] & epi_year <= input$cause_years[2]]
    
    if (length(input$cause_province) > 0 && !("" %in% input$cause_province)) {
      d <- d[DeathProvinceName %in% input$cause_province]
    }
    
    if (length(input$cause_agegroup) > 0 && !("" %in% input$cause_agegroup)) {
      d <- d[agegroup %in% input$cause_agegroup]
    }
    
    if (input$cause_sex != "all") {
      d <- d[SexName == input$cause_sex]
    }
    
    if (length(input$cause_code) > 0 && !("" %in% input$cause_code)) {
      patterns <- paste0("^(", paste(input$cause_code, collapse = "|"), ")")
      d <- d[grepl(patterns, UnderlyingCause, ignore.case = TRUE)]
    }
    
    d
  }) |> bindCache(input$cause_years, input$cause_province, input$cause_agegroup, 
                  input$cause_sex, input$cause_code)
  
  output$cause_top20 <- renderPlotly({
    d <- cause_data()[, .(count = sum(count)), by = .(UnderlyingCause)]
    d <- d[order(-count)][1:20]
    
    plot_ly(d, y = ~reorder(UnderlyingCause, count), x = ~count, type = 'bar',
            orientation = 'h',
            marker = list(color = '#0d6efd'),
            hovertemplate = "%{y}<br>Deaths: %{x:,}<extra></extra>") %>%
      layout(xaxis = list(title = "Deaths"),
             yaxis = list(title = ""))
  })
  
  output$cause_trends <- renderPlotly({
    d <- cause_data()
    top_causes <- d[, .(count = sum(count)), by = .(UnderlyingCause)][order(-count)][1:5]$UnderlyingCause
    d_trend <- d[UnderlyingCause %in% top_causes, .(count = sum(count)), by = .(epi_year, UnderlyingCause)]
    
    plot_ly(d_trend, x = ~epi_year, y = ~count, color = ~UnderlyingCause,
            type = 'scatter', mode = 'lines+markers',
            hovertemplate = "%{fullData.name}<br>Year: %{x}<br>Deaths: %{y:,}<extra></extra>") %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "Deaths"),
             legend = list(orientation = "h", y = -0.2))
  })
  
  output$cause_table <- renderDT({
    d <- cause_data()[, .(
      Deaths = sum(count)
    ), by = .(UnderlyingCause)][order(-Deaths)]
    
    d[, Percentage := round(100 * Deaths / sum(Deaths), 2)]
    
    datatable(d, options = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE)
  })
  
  # ==========================================================================
  # DEMOGRAPHICS TAB
  # ==========================================================================
  
  demo_data <- reactive({
    d <- agg$yearly_province[epi_year >= input$demo_years[1] & epi_year <= input$demo_years[2]]
    
    if (length(input$demo_province) > 0 && !("" %in% input$demo_province)) {
      d <- d[DeathProvinceName %in% input$demo_province]
    }
    
    if (input$demo_nat_unnat != "all") {
      d <- d[NaturalUnnatural == as.integer(input$demo_nat_unnat)]
    }
    
    d
  }) |> bindCache(input$demo_years, input$demo_province, input$demo_nat_unnat)
  
  output$demo_pyramid <- renderPlotly({
    d <- demo_data()[, .(count = sum(count)), by = .(agegroup, SexName)]
    d <- d[SexName %in% c("Male", "Female")]
    d[SexName == "Male", count := -count]
    
    plot_ly() %>%
      add_bars(data = d[SexName == "Male"], y = ~agegroup, x = ~count, 
               name = "Male", orientation = 'h',
               marker = list(color = '#0d6efd')) %>%
      add_bars(data = d[SexName == "Female"], y = ~agegroup, x = ~count,
               name = "Female", orientation = 'h',
               marker = list(color = '#dc3545')) %>%
      layout(barmode = 'overlay',
             xaxis = list(title = "Deaths", 
                          tickformat = ",d",
                          tickvals = seq(-max(abs(d$count)), max(abs(d$count)), length.out = 5),
                          ticktext = format(abs(seq(-max(abs(d$count)), max(abs(d$count)), length.out = 5)), big.mark = ",")),
             yaxis = list(title = "Age Group"),
             legend = list(orientation = "h", y = -0.15))
  })
  
  output$demo_age_time <- renderPlotly({
    d <- demo_data()[, .(count = sum(count)), by = .(epi_year, agegroup)]
    
    plot_ly(d, x = ~epi_year, y = ~count, color = ~agegroup,
            type = 'scatter', mode = 'lines',
            hovertemplate = "Year: %{x}<br>Deaths: %{y:,}<extra>%{fullData.name}</extra>") %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "Deaths"),
             legend = list(title = list(text = "Age Group")))
  })
  
  output$demo_sex_ratio <- renderPlotly({
    d <- demo_data()[SexName %in% c("Male", "Female"), .(count = sum(count)), by = .(agegroup, SexName)]
    d_wide <- dcast(d, agegroup ~ SexName, value.var = "count")
    d_wide[, ratio := Male / Female]
    
    plot_ly(d_wide, x = ~agegroup, y = ~ratio, type = 'bar',
            marker = list(color = '#6c757d'),
            hovertemplate = "Age: %{x}<br>M:F Ratio: %{y:.2f}<extra></extra>") %>%
      layout(xaxis = list(title = "Age Group"),
             yaxis = list(title = "Male:Female Ratio"),
             shapes = list(list(type = "line", x0 = -0.5, x1 = 10.5, y0 = 1, y1 = 1,
                               line = list(color = "red", dash = "dash"))))
  })
  
  # ==========================================================================
  # GEOGRAPHIC TAB
  # ==========================================================================
  
  # Helper function: expand LGH cause range to individual codes
  expand_lgh_cause <- function(lgh_cause, all_codes) {
    if (grepl("-", lgh_cause) && !grepl("\\+", lgh_cause)) {
      parts <- strsplit(lgh_cause, "-")[[1]]
      if (length(parts) == 2) {
        prefix <- gsub("[0-9].*", "", parts[1])
        start_num <- as.integer(gsub("[^0-9]", "", parts[1]))
        end_num <- as.integer(gsub("[^0-9]", "", parts[2]))
        
        if (!is.na(start_num) && !is.na(end_num)) {
          expanded <- paste0(prefix, sprintf("%02d", start_num:end_num))
          expanded <- c(expanded, paste0(prefix, start_num:end_num))
          return(all_codes[all_codes %in% expanded | 
                          substr(all_codes, 1, 3) %in% expanded])
        }
      }
    }
    
    if (grepl("\\+", lgh_cause)) {
      parts <- strsplit(lgh_cause, "\\+")[[1]]
      result <- character(0)
      for (p in parts) {
        result <- c(result, expand_lgh_cause(trimws(p), all_codes))
      }
      return(unique(result))
    }
    
    if (grepl("\\*$", lgh_cause)) {
      base <- gsub("\\*$", "", lgh_cause)
      return(expand_lgh_cause(base, all_codes))
    }
    
    matched <- all_codes[all_codes == lgh_cause | startsWith(all_codes, lgh_cause)]
    return(matched)
  }
  
  geo_data <- reactive({
    cause_type <- input$geo_cause_type
    
    if (cause_type == "all") {
      d <- agg$yearly_province[epi_year >= input$geo_years[1] & epi_year <= input$geo_years[2]]
      
      if (length(input$geo_agegroup) > 0 && !("" %in% input$geo_agegroup)) {
        d <- d[agegroup %in% input$geo_agegroup]
      }
    } else if (cause_type == "icd10") {
      d <- agg$causes[epi_year >= input$geo_years[1] & epi_year <= input$geo_years[2]]
      
      if (length(input$geo_agegroup) > 0 && !("" %in% input$geo_agegroup)) {
        d <- d[agegroup %in% input$geo_agegroup]
      }
      
      if (length(input$geo_cause) > 0) {
        all_causes <- unique(d$UnderlyingCause)
        matched_codes <- character(0)
        for (pattern in input$geo_cause) {
          matched_codes <- c(matched_codes, all_causes[grepl(paste0("^", pattern), all_causes, ignore.case = TRUE)])
        }
        d <- d[UnderlyingCause %in% matched_codes]
      }
    } else if (cause_type == "lgh") {
      d <- agg$causes[epi_year >= input$geo_years[1] & epi_year <= input$geo_years[2]]
      
      if (length(input$geo_agegroup) > 0 && !("" %in% input$geo_agegroup)) {
        d <- d[agegroup %in% input$geo_agegroup]
      }
      
      if (length(input$geo_lgh_cause) > 0) {
        all_causes <- unique(d$UnderlyingCause)
        matched_codes <- unique(unlist(lapply(input$geo_lgh_cause, expand_lgh_cause, all_codes = all_causes)))
        d <- d[UnderlyingCause %in% matched_codes]
      }
    }
    
    d
  }) |> bindCache(input$geo_years, input$geo_agegroup, input$geo_cause_type, input$geo_cause, input$geo_lgh_cause)
  
  geo_district <- reactive({
    d <- agg$districts[epi_year >= input$geo_years[1] & epi_year <= input$geo_years[2]]
    d[, .(count = sum(count)), by = .(deathdistrictname)]
  }) |> bindCache(input$geo_years)
  
  output$geo_province_map <- renderPlotly({
    if (is.null(shapes)) {
      return(plotly_empty() %>% layout(title = "Shape files not available"))
    }
    
    d <- geo_data()[!is.na(DeathProvinceName), .(count = sum(count)), by = .(DeathProvinceName)]
    
    show_rate <- input$geo_map_metric == "rate" && !is.null(pop_data)
    
    if (show_rate) {
      years_selected <- seq(input$geo_years[1], input$geo_years[2])
      pop_avg <- pop_data$province[Year %in% years_selected, 
                                   .(avg_pop = mean(population, na.rm = TRUE)), 
                                   by = .(province_full)]
      d <- merge(d, pop_avg, by.x = "DeathProvinceName", by.y = "province_full", all.x = TRUE)
      d[, rate := count / avg_pop * 100000]
    }
    
    prov_sf <- shapes$provinces %>%
      left_join(d, by = c("province_full" = "DeathProvinceName"))
    
    if (show_rate) {
      prov_sf$hover_text <- paste0(
        "<b>", prov_sf$province_full, "</b><br>",
        "Deaths: ", format(prov_sf$count, big.mark = ",", na.rm = TRUE), "<br>",
        "Rate: ", round(prov_sf$rate, 1), " per 100k"
      )
      fill_var <- prov_sf$rate
      legend_title <- "Deaths per 100k"
    } else {
      prov_sf$hover_text <- paste0(
        "<b>", prov_sf$province_full, "</b><br>",
        "Deaths: ", format(prov_sf$count, big.mark = ",", na.rm = TRUE)
      )
      fill_var <- prov_sf$count
      legend_title <- "Deaths"
    }
    
    p <- ggplot(prov_sf) +
      geom_sf(aes(fill = fill_var, text = hover_text), color = "white", linewidth = 0.3) +
      scale_fill_gradient(low = "#fff7bc", high = "#d95f0e", na.value = "grey80",
                          name = legend_title) +
      theme_void() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$geo_district_map <- renderPlotly({
    if (is.null(shapes)) {
      return(plotly_empty() %>% layout(title = "Shape files not available"))
    }
    
    d <- geo_district()
    
    if (nrow(d) == 0) {
      return(plotly_empty() %>% layout(title = "District data not available for cause filtering"))
    }
    
    show_rate <- input$geo_map_metric == "rate" && !is.null(pop_data)
    
    if (show_rate && !is.null(pop_data$district)) {
      years_selected <- seq(input$geo_years[1], input$geo_years[2])
      pop_avg <- pop_data$district[Year %in% years_selected, 
                                   .(avg_pop = mean(population, na.rm = TRUE)), 
                                   by = .(district_standard)]
      d <- merge(d, pop_avg, by.x = "deathdistrictname", by.y = "district_standard", all.x = TRUE)
      d[, rate := count / avg_pop * 100000]
    }
    
    dist_sf <- shapes$districts %>%
      left_join(d, by = c("district_standard" = "deathdistrictname"))
    
    if (show_rate && "rate" %in% names(d)) {
      dist_sf$hover_text <- paste0(
        "<b>", dist_sf$district_standard, "</b><br>",
        "Deaths: ", format(dist_sf$count, big.mark = ",", na.rm = TRUE), "<br>",
        "Rate: ", round(dist_sf$rate, 1), " per 100k"
      )
      fill_var <- dist_sf$rate
      legend_title <- "Deaths per 100k"
    } else {
      dist_sf$hover_text <- paste0(
        "<b>", dist_sf$district_standard, "</b><br>",
        "Deaths: ", format(dist_sf$count, big.mark = ",", na.rm = TRUE)
      )
      fill_var <- dist_sf$count
      legend_title <- "Deaths"
    }
    
    p <- ggplot(dist_sf) +
      geom_sf(aes(fill = fill_var, text = hover_text), color = "white", linewidth = 0.1) +
      scale_fill_gradient(low = "#e5f5e0", high = "#31a354", na.value = "grey80",
                          name = legend_title) +
      theme_void() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$geo_province_bar <- renderPlotly({
    d <- geo_data()[!is.na(DeathProvinceName), .(count = sum(count)), by = .(DeathProvinceName)]
    d <- d[order(-count)]
    
    plot_ly(d, x = ~reorder(DeathProvinceName, count), y = ~count, type = 'bar',
            marker = list(color = '#0d6efd'),
            hovertemplate = "%{x}<br>Deaths: %{y:,}<extra></extra>") %>%
      layout(xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = "Total Deaths"))
  })
  
  output$geo_district_plot <- renderPlotly({
    d <- geo_district()[order(-count)][1:20]
    
    if (nrow(d) == 0) {
      return(plotly_empty() %>% layout(title = "No district data"))
    }
    
    plot_ly(d, y = ~reorder(deathdistrictname, count), x = ~count, type = 'bar',
            orientation = 'h',
            marker = list(color = '#198754'),
            hovertemplate = "%{y}<br>Deaths: %{x:,}<extra></extra>") %>%
      layout(xaxis = list(title = "Deaths"),
             yaxis = list(title = ""))
  })
  
  output$geo_province_time <- renderPlotly({
    d <- geo_data()[!is.na(DeathProvinceName), .(count = sum(count)), by = .(epi_year, DeathProvinceName)]
    
    plot_ly(d, x = ~epi_year, y = ~count, color = ~DeathProvinceName, 
            type = 'scatter', mode = 'lines+markers') %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "Deaths"),
             legend = list(orientation = "h", y = -0.2))
  })
  
  # ==========================================================================
  # CODE COMPARISON TAB
  # ==========================================================================
  
  match_code_group <- function(codes, patterns) {
    if (is.null(patterns) || length(patterns) == 0) return(character(0))
    regex_patterns <- paste0("^(", paste(patterns, collapse = "|"), ")")
    codes[grepl(regex_patterns, codes, ignore.case = TRUE)]
  }
  
  compare_data <- reactive({
    req(input$compare_group_a, input$compare_group_b)
    
    all_causes <- unique(agg$causes$UnderlyingCause)
    group_a_codes <- match_code_group(all_causes, input$compare_group_a)
    group_b_codes <- match_code_group(all_causes, input$compare_group_b)
    
    if (length(group_a_codes) == 0 || length(group_b_codes) == 0) {
      return(NULL)
    }
    
    d <- agg$causes[epi_year >= input$compare_years[1] & epi_year <= input$compare_years[2]]
    
    if (length(input$compare_province) > 0 && !("" %in% input$compare_province)) {
      d <- d[DeathProvinceName %in% input$compare_province]
    }
    
    if (length(input$compare_agegroup) > 0 && !("" %in% input$compare_agegroup)) {
      d <- d[agegroup %in% input$compare_agegroup]
    }
    
    if (input$compare_sex != "all") {
      d <- d[SexName == input$compare_sex]
    }
    
    d[, code_group := fcase(
      UnderlyingCause %in% group_a_codes, input$compare_group_a_name,
      UnderlyingCause %in% group_b_codes, input$compare_group_b_name,
      default = NA_character_
    )]
    
    d <- d[!is.na(code_group)]
    d
  }) |> bindCache(input$compare_years, input$compare_group_a, input$compare_group_b,
                  input$compare_province, input$compare_agegroup, input$compare_sex,
                  input$compare_group_a_name, input$compare_group_b_name)
  
  output$compare_time_plot <- renderPlotly({
    d <- compare_data()
    if (is.null(d) || nrow(d) == 0) {
      return(plotly_empty() %>% layout(title = "Select codes for both groups to compare"))
    }
    
    d_time <- d[, .(count = sum(count)), by = .(epi_year, code_group)]
    
    plot_ly(d_time, x = ~epi_year, y = ~count, color = ~code_group,
            type = 'scatter', mode = 'lines+markers',
            colors = c('#0d6efd', '#dc3545')) %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "Number of Deaths"),
             legend = list(orientation = "h", y = -0.15))
  })
  
  output$compare_totals_plot <- renderPlotly({
    d <- compare_data()
    if (is.null(d) || nrow(d) == 0) {
      return(plotly_empty() %>% layout(title = "Select codes for both groups"))
    }
    
    d_totals <- d[, .(count = sum(count)), by = .(code_group)]
    
    plot_ly(d_totals, x = ~code_group, y = ~count, type = 'bar',
            marker = list(color = c('#0d6efd', '#dc3545')),
            text = ~format(count, big.mark = ","),
            textposition = 'outside') %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Total Deaths"))
  })
  
  output$compare_age_plot <- renderPlotly({
    d <- compare_data()
    if (is.null(d) || nrow(d) == 0) {
      return(plotly_empty() %>% layout(title = "Select codes for both groups"))
    }
    
    d_age <- d[!is.na(agegroup), .(count = sum(count)), by = .(agegroup, code_group)]
    d_age[, pct := count / sum(count) * 100, by = code_group]
    
    plot_ly(d_age, x = ~agegroup, y = ~pct, color = ~code_group,
            type = 'bar', colors = c('#0d6efd', '#dc3545')) %>%
      layout(xaxis = list(title = "Age Group"),
             yaxis = list(title = "Percentage within Group"),
             barmode = 'group',
             legend = list(orientation = "h", y = -0.2))
  })
  
  output$compare_sex_plot <- renderPlotly({
    d <- compare_data()
    if (is.null(d) || nrow(d) == 0) {
      return(plotly_empty() %>% layout(title = "Select codes for both groups"))
    }
    
    d_sex <- d[SexName %in% c("Male", "Female"), .(count = sum(count)), by = .(SexName, code_group)]
    d_sex[, pct := count / sum(count) * 100, by = code_group]
    
    plot_ly(d_sex, x = ~code_group, y = ~pct, color = ~SexName,
            type = 'bar', colors = c('#0d6efd', '#dc3545')) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Percentage"),
             barmode = 'group')
  })
  
  output$compare_province_plot <- renderPlotly({
    d <- compare_data()
    if (is.null(d) || nrow(d) == 0) {
      return(plotly_empty() %>% layout(title = "Select codes for both groups"))
    }
    
    d_prov <- d[!is.na(DeathProvinceName), .(count = sum(count)), by = .(DeathProvinceName, code_group)]
    d_prov[, pct := count / sum(count) * 100, by = code_group]
    
    plot_ly(d_prov, x = ~DeathProvinceName, y = ~pct, color = ~code_group,
            type = 'bar', colors = c('#0d6efd', '#dc3545')) %>%
      layout(xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = "Percentage within Group"),
             barmode = 'group',
             legend = list(orientation = "h", y = -0.2))
  })
  
  output$compare_table <- renderDT({
    d <- compare_data()
    if (is.null(d) || nrow(d) == 0) {
      return(datatable(data.frame(Message = "Select codes for both groups")))
    }
    
    d_summary <- d[, .(Deaths = sum(count), N_Codes = uniqueN(UnderlyingCause)), 
                   by = .(code_group, UnderlyingCause)]
    d_summary <- d_summary[order(code_group, -Deaths)]
    
    datatable(d_summary, options = list(pageLength = 20, scrollX = TRUE),
              rownames = FALSE, filter = 'top')
  })
  
  # ==========================================================================
  # LGH CAUSE COMPARISON TAB
  # ==========================================================================
  
  lgh_data <- reactive({
    req(input$lgh_group_a, input$lgh_group_b)
    
    all_causes <- unique(agg$causes$UnderlyingCause)
    
    group_a_codes <- unique(unlist(lapply(input$lgh_group_a, expand_lgh_cause, all_codes = all_causes)))
    group_b_codes <- unique(unlist(lapply(input$lgh_group_b, expand_lgh_cause, all_codes = all_causes)))
    
    if (length(group_a_codes) == 0 || length(group_b_codes) == 0) {
      return(NULL)
    }
    
    group_a_label <- paste(lgh_lookup[LGH_Cause %in% input$lgh_group_a, description], collapse = " + ")
    group_b_label <- paste(lgh_lookup[LGH_Cause %in% input$lgh_group_b, description], collapse = " + ")
    
    if (nchar(group_a_label) > 40) group_a_label <- paste0(substr(group_a_label, 1, 37), "...")
    if (nchar(group_b_label) > 40) group_b_label <- paste0(substr(group_b_label, 1, 37), "...")
    
    d <- agg$causes[epi_year >= input$lgh_years[1] & epi_year <= input$lgh_years[2]]
    
    if (length(input$lgh_province) > 0 && !("" %in% input$lgh_province)) {
      d <- d[DeathProvinceName %in% input$lgh_province]
    }
    
    if (length(input$lgh_agegroup) > 0 && !("" %in% input$lgh_agegroup)) {
      d <- d[agegroup %in% input$lgh_agegroup]
    }
    
    if (input$lgh_sex != "all") {
      d <- d[SexName == input$lgh_sex]
    }
    
    d[, cause_group := fcase(
      UnderlyingCause %in% group_a_codes, group_a_label,
      UnderlyingCause %in% group_b_codes, group_b_label,
      default = NA_character_
    )]
    
    attr(d, "group_a_label") <- group_a_label
    attr(d, "group_b_label") <- group_b_label
    
    d <- d[!is.na(cause_group)]
    d
  }) |> bindCache(input$lgh_years, input$lgh_group_a, input$lgh_group_b,
                  input$lgh_province, input$lgh_agegroup, input$lgh_sex)
  
  output$lgh_time_plot <- renderPlotly({
    d <- lgh_data()
    if (is.null(d) || nrow(d) == 0) {
      return(plotly_empty() %>% layout(title = "Select cause groups to compare"))
    }
    
    d_time <- d[, .(count = sum(count)), by = .(epi_year, cause_group)]
    
    plot_ly(d_time, x = ~epi_year, y = ~count, color = ~cause_group,
            type = 'scatter', mode = 'lines+markers',
            colors = c('#0d6efd', '#dc3545')) %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "Number of Deaths"),
             legend = list(orientation = "h", y = -0.15))
  })
  
  output$lgh_totals_plot <- renderPlotly({
    d <- lgh_data()
    if (is.null(d) || nrow(d) == 0) {
      return(plotly_empty() %>% layout(title = "Select cause groups"))
    }
    
    d_totals <- d[, .(count = sum(count)), by = .(cause_group)]
    
    plot_ly(d_totals, x = ~cause_group, y = ~count, type = 'bar',
            marker = list(color = c('#0d6efd', '#dc3545')),
            text = ~format(count, big.mark = ","),
            textposition = 'outside') %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Total Deaths"))
  })
  
  output$lgh_age_plot <- renderPlotly({
    d <- lgh_data()
    if (is.null(d) || nrow(d) == 0) {
      return(plotly_empty() %>% layout(title = "Select cause groups"))
    }
    
    d_age <- d[!is.na(agegroup), .(count = sum(count)), by = .(agegroup, cause_group)]
    d_age[, pct := count / sum(count) * 100, by = cause_group]
    
    plot_ly(d_age, x = ~agegroup, y = ~pct, color = ~cause_group,
            type = 'bar', colors = c('#0d6efd', '#dc3545')) %>%
      layout(xaxis = list(title = "Age Group"),
             yaxis = list(title = "Percentage within Group"),
             barmode = 'group',
             legend = list(orientation = "h", y = -0.2))
  })
  
  output$lgh_sex_plot <- renderPlotly({
    d <- lgh_data()
    if (is.null(d) || nrow(d) == 0) {
      return(plotly_empty() %>% layout(title = "Select cause groups"))
    }
    
    d_sex <- d[SexName %in% c("Male", "Female"), .(count = sum(count)), by = .(SexName, cause_group)]
    d_sex[, pct := count / sum(count) * 100, by = cause_group]
    
    plot_ly(d_sex, x = ~cause_group, y = ~pct, color = ~SexName,
            type = 'bar', colors = c('#0d6efd', '#dc3545')) %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Percentage"),
             barmode = 'group')
  })
  
  output$lgh_province_plot <- renderPlotly({
    d <- lgh_data()
    if (is.null(d) || nrow(d) == 0) {
      return(plotly_empty() %>% layout(title = "Select cause groups"))
    }
    
    d_prov <- d[!is.na(DeathProvinceName), .(count = sum(count)), by = .(DeathProvinceName, cause_group)]
    d_prov[, pct := count / sum(count) * 100, by = cause_group]
    
    plot_ly(d_prov, x = ~DeathProvinceName, y = ~pct, color = ~cause_group,
            type = 'bar', colors = c('#0d6efd', '#dc3545')) %>%
      layout(xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = "Percentage within Group"),
             barmode = 'group',
             legend = list(orientation = "h", y = -0.2))
  })
  
  output$lgh_table <- renderDT({
    d <- lgh_data()
    if (is.null(d) || nrow(d) == 0) {
      return(datatable(data.frame(Message = "Select cause groups to see comparison")))
    }
    
    d_summary <- d[, .(Deaths = sum(count), N_Codes = uniqueN(UnderlyingCause)), 
                   by = .(cause_group)]
    d_summary[, Pct_Total := round(100 * Deaths / sum(Deaths), 1)]
    
    datatable(d_summary, options = list(pageLength = 10, scrollX = TRUE, dom = 't'),
              rownames = FALSE)
  })
  
}

# =============================================================================
# RUN APP
# =============================================================================

shinyApp(ui, server)
