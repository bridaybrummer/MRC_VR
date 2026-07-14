# =============================================================================
# MRC VR Data Explorer Shiny App - OPTIMIZED VERSION
# Interactive exploration of South African mortality data (1997-2022)
# =============================================================================
# OPTIMIZATIONS APPLIED:
# 1. Pre-aggregated summary tables at startup (avoids repeated aggregation)
# 2. data.table keys for O(log n) filtering instead of O(n)
# 3. Reactive caching with bindCache() for repeated queries
# 4. Load only required columns to reduce memory footprint
# 5. Integer types where possible (smaller memory, faster ops)
# 6. Debounced inputs to reduce recalculation frequency
# =============================================================================

library(shiny)
library(bslib)
library(arrow)
library(data.table)
library(plotly)
library(DT)
library(labelled)
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
# DATA LOADING - OPTIMIZED
# =============================================================================

load_data <- function() {
  message("Loading data...")
  start_time <- Sys.time()
  
  data_path <- here::here("Deaths2022_MRCversionFINAL.feather")
  if (!file.exists(data_path)) {
    data_path <- "../../Deaths2022_MRCversionFINAL.feather"
  }
  
  # Read only required columns (major memory savings)
  required_cols <- c(
    "serialno", "DeathYear", "DeathMonth", "DeathDay",
    "epi_year", "epi_week", "age", "Sex", 
    "DeathProvince", "deathdistrictname",
    "UnderlyingCause", "CauseA", "CauseB",
    "DeathType", "NaturalUnnatural",
    "RegistrationYear", "RegistrationMonth"
  )
  
  dt <- as.data.table(read_feather(data_path, col_select = all_of(required_cols)))
  
  message(sprintf("  Loaded %s rows, %s columns", 
                  format(nrow(dt), big.mark = ","), ncol(dt)))
  
  # Extract labels for provinces before conversion
  province_labels <- val_labels(dt$DeathProvince)
  sex_labels <- val_labels(dt$Sex)
  
  # Convert to factors (more memory efficient than character)
  dt[, DeathProvinceName := factor(
    names(province_labels)[match(DeathProvince, province_labels)],
    levels = names(province_labels)
  )]
  
  dt[, SexName := factor(
    names(sex_labels)[match(Sex, sex_labels)],
    levels = names(sex_labels)
  )]
  
  # Create age groups as factor
  dt[, agegroup := cut(age, 
                       breaks = c(0, 1, 5, 15, 25, 35, 45, 55, 65, 75, 85, Inf),
                       right = FALSE,
                       labels = c("0", "1-4", "5-14", "15-24", "25-34", 
                                  "35-44", "45-54", "55-64", "65-74", "75-84", "85+"))]
  
  # Convert to integer for speed (smaller, faster comparisons)
  dt[, epi_year := as.integer(epi_year)]
  dt[, epi_week := as.integer(epi_week)]
  dt[, DeathType := as.integer(DeathType)]
  dt[, NaturalUnnatural := as.integer(NaturalUnnatural)]
  dt[, DeathYear := as.integer(DeathYear)]
  dt[, DeathMonth := as.integer(DeathMonth)]
  dt[, RegistrationYear := as.integer(RegistrationYear)]
  
  # Set keys for fast filtering (critical optimization!)
  setkey(dt, epi_year, DeathProvinceName)
  
  # Create secondary index for other common queries
  setindex(dt, UnderlyingCause)
  setindex(dt, agegroup)
  
  elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
  message(sprintf("  Data loaded in %s seconds", elapsed))
  
  return(dt)
}

# =============================================================================
# LOAD SHAPE FILES FOR CHOROPLETH MAPS
# =============================================================================

load_shape_files <- function() {
  message("Loading shape files...")
  
  shape_path <- "/Users/briday/Desktop/SAFETP/CLA/NMCleaner/data/shape_files.rda"
  
  if (!file.exists(shape_path)) {
    warning("Shape files not found at: ", shape_path)
    return(NULL)
  }
  
  load(shape_path)
  
  # Province name mapping: mortality data full names -> shape file abbreviations
  prov_map <- c(
    "Western Cape" = "WC",
    "Eastern Cape" = "EC",
    "Northern Cape" = "NC",
    "Free State" = "FS",
    "KwaZulu-Natal" = "KZN",
    "North West" = "NW",
    "Gauteng" = "GT",
    "Mpumalanga" = "MP",
    "Limpopo" = "LIM"
  )
  
  # Add full province names to shape files for easier joining
  shape_files$provinces$province_full <- names(prov_map)[match(shape_files$provinces$prov, prov_map)]
  
  message("  Shape files loaded: provinces, districts, sub_districts")
  
  return(list(
    provinces = shape_files$provinces,
    districts = shape_files$districts,
    sub_districts = shape_files$sub_districts,
    prov_map = prov_map
  ))
}

# =============================================================================
# LOAD POPULATION DATA
# =============================================================================

load_population_data <- function() {
  message("Loading population data...")
  
  tryCatch({
    pop <- as.data.table(NMCleaner::pop)
    
    # Province mapping for joining with mortality data
    prov_name_map <- c(
      "WC" = "Western Cape",
      "EC" = "Eastern Cape",
      "NC" = "Northern Cape",
      "FS" = "Free State",
      "KZN" = "KwaZulu-Natal",
      "NW" = "North West",
      "GT" = "Gauteng",
      "MP" = "Mpumalanga",
      "LIM" = "Limpopo"
    )
    
    # Create yearly population by province (sum across all ages and sexes)
    pop_province <- pop[, .(
      population = sum(Population, na.rm = TRUE)
    ), by = .(Year, prov)]
    pop_province[, province_full := prov_name_map[prov]]
    pop_province[, Year := as.integer(Year)]
    
    # Create yearly population by district
    pop_district <- pop[, .(
      population = sum(Population, na.rm = TRUE)
    ), by = .(Year, district_standard)]
    pop_district[, Year := as.integer(Year)]
    
    message("  Population data loaded: ", nrow(pop_province), " province-years, ", 
            nrow(pop_district), " district-years")
    
    return(list(
      province = pop_province,
      district = pop_district,
      years_available = sort(unique(as.integer(pop$Year)))
    ))
  }, error = function(e) {
    warning("Could not load NMCleaner::pop: ", e$message)
    return(NULL)
  })
}

# =============================================================================
# LOAD LGH ICD-10 CAUSE LOOKUP
# =============================================================================

load_lgh_lookup <- function() {
  message("Loading LGH ICD-10 cause lookup...")
  
  lookup_path <- here::here("LGH_ICD10_Cause_Lookup.rda")
  if (!file.exists(lookup_path)) {
    lookup_path <- "../../LGH_ICD10_Cause_Lookup.rda"
  }
  
  if (!file.exists(lookup_path)) {
    warning("LGH lookup not found, creating default")
    # Create inline if file not found
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
  
  # Remove NA row if present
  icd_lookup <- icd_lookup[!is.na(LGH_Cause)]
  
  # Create display labels
  icd_lookup[, display_label := paste0(LGH_Cause, " - ", description)]
  
  message("  LGH lookup loaded: ", nrow(icd_lookup), " cause groups")
  
  return(icd_lookup)
}

# =============================================================================
# PRE-AGGREGATION FUNCTIONS
# =============================================================================

# Pre-compute common aggregations at startup to avoid repeated calculations
create_aggregated_tables <- function(dt) {
  message("Creating pre-aggregated summary tables...")
  start_time <- Sys.time()
  
  agg <- list()
  
  # Weekly counts by year (for time series)
  agg$weekly <- dt[, .(count = .N), 
                   by = .(epi_year, epi_week, DeathProvinceName, 
                          DeathType, NaturalUnnatural)]
  setkey(agg$weekly, epi_year)
  
  # Yearly counts by province and demographics
  agg$yearly_province <- dt[, .(count = .N), 
                            by = .(epi_year, DeathProvinceName, agegroup, SexName,
                                   DeathType, NaturalUnnatural)]
  setkey(agg$yearly_province, epi_year)
  
  # Cause code summaries
  agg$causes <- dt[!is.na(UnderlyingCause) & UnderlyingCause != "", 
                   .(count = .N, mean_age = mean(age, na.rm = TRUE)),
                   by = .(epi_year, UnderlyingCause, DeathProvinceName, 
                          agegroup, SexName)]
  setkey(agg$causes, epi_year)
  
  # District summaries
  agg$districts <- dt[!is.na(deathdistrictname), .(count = .N),
                      by = .(epi_year, deathdistrictname, DeathProvinceName)]
  setkey(agg$districts, epi_year)
  
  # Registration delay pre-calculation
  agg$reg_delay <- dt[!is.na(RegistrationYear) & !is.na(DeathYear), 
                      .(count = .N),
                      by = .(epi_year, DeathProvinceName,
                             reg_delay = pmin(as.integer(RegistrationYear) - as.integer(DeathYear), 10L))]
  agg$reg_delay <- agg$reg_delay[reg_delay >= 0]
  setkey(agg$reg_delay, epi_year)
  
  elapsed <- round(difftime(Sys.time(), start_time, units = "secs"), 1)
  message(sprintf("  Aggregations complete in %s seconds", elapsed))
  
  return(agg)
}

# =============================================================================
# UI (unchanged structure, but with debounced inputs)
# =============================================================================

ui <- page_navbar(
  title = "MRC VR Data Explorer",
  theme = bs_theme(
    bootswatch = "cosmo",
    primary = "#0d6efd",
    font_scale = 0.9
  ),
  header = tagList(
    useWaiter(),  # Enable waiter loading screen
    waiterShowOnLoad(html = tagList(
      spin_fading_circles(),
      h4("Loading 13 million records...", style = "color: white; margin-top: 20px;"),
      p("This may take 20-30 seconds on first load", style = "color: #aaa;")
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
                    min = 1997L, max = 2022L,
                    value = c(1997L, 2022L),
                    step = 1L, sep = ""),
        selectInput("overview_province", "Province:",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE),
        radioButtons("overview_death_type", "Death Type:",
                     choices = c("All" = "all", 
                                 "Individual deaths" = "1",
                                 "Stillbirths" = "2"),
                     selected = "all"),
        radioButtons("overview_nat_unnat", "Cause Type:",
                     choices = c("All" = "all",
                                 "Natural" = "1",
                                 "Non-natural" = "2"),
                     selected = "all")
      ),
      card(
        card_header("Deaths Over Time"),
        withSpinner(plotlyOutput("overview_time_plot", height = "400px"), type = 4, color = "#0d6efd")
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
      ),
      card(
        card_header("Summary Statistics"),
        verbatimTextOutput("overview_summary")
      )
    )
  ),
  
  # Temporal Trends Tab
  nav_panel(
    title = "Temporal Trends",
    icon = icon("calendar"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Filters",
        width = 300,
        sliderInput("temporal_years", "Year Range:",
                    min = 1997L, max = 2022L,
                    value = c(2014L, 2022L),
                    step = 1L, sep = ""),
        selectInput("temporal_province", "Province:",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE),
        selectInput("temporal_agegroup", "Age Group:",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE),
        radioButtons("temporal_aggregation", "Aggregation:",
                     choices = c("Weekly" = "week",
                                 "Monthly" = "month",
                                 "Yearly" = "year"),
                     selected = "week"),
        checkboxInput("temporal_compare_years", "Compare years overlay", FALSE)
      ),
      card(
        card_header("Time Series"),
        withSpinner(plotlyOutput("temporal_plot", height = "500px"), type = 4, color = "#0d6efd")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Seasonality Heatmap"),
          withSpinner(plotlyOutput("temporal_heatmap", height = "400px"), type = 4, color = "#0d6efd")
        ),
        card(
          card_header("Year-over-Year Comparison"),
          withSpinner(plotlyOutput("temporal_yoy", height = "400px"), type = 4, color = "#dc3545")
        )
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
                    min = 1997L, max = 2022L,
                    value = c(2020L, 2022L),
                    step = 1L, sep = ""),
        selectInput("cause_province", "Province:",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE),
        selectInput("cause_agegroup", "Age Group:",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE),
        selectInput("cause_sex", "Sex:",
                    choices = c("All" = "all", "Male" = "Male", "Female" = "Female"),
                    selected = "all"),
        numericInput("cause_top_n", "Top N causes:", value = 20, min = 5, max = 50)
      ),
      card(
        card_header("Top Underlying Causes"),
        withSpinner(plotlyOutput("cause_top_plot", height = "500px"), type = 4, color = "#0d6efd")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Causes Over Time"),
          withSpinner(plotlyOutput("cause_time_plot", height = "400px"), type = 4, color = "#0d6efd")
        ),
        card(
          card_header("Cause Distribution by Province"),
          withSpinner(plotlyOutput("cause_province_plot", height = "400px"), type = 4, color = "#0d6efd")
        )
      ),
      card(
        card_header("Cause Code Details"),
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
                    min = 1997L, max = 2022L,
                    value = c(2020L, 2022L),
                    step = 1L, sep = ""),
        selectInput("demo_province", "Province:",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE),
        radioButtons("demo_nat_unnat", "Cause Type:",
                     choices = c("All" = "all",
                                 "Natural" = "1",
                                 "Non-natural" = "2"),
                     selected = "all")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Age Distribution"),
          withSpinner(plotlyOutput("demo_age_hist", height = "350px"), type = 4, color = "#0d6efd")
        ),
        card(
          card_header("Sex Distribution"),
          withSpinner(plotlyOutput("demo_sex_plot", height = "350px"), type = 4, color = "#dc3545")
        )
      ),
      card(
        card_header("Age-Sex Pyramid"),
        withSpinner(plotlyOutput("demo_pyramid", height = "500px"), type = 4, color = "#6f42c1")
      ),
      card(
        card_header("Registration Delay Analysis"),
        withSpinner(plotlyOutput("demo_reg_delay", height = "400px"), type = 4, color = "#198754")
      )
    )
  ),
  
  # Code Comparison Tab
  nav_panel(
    title = "Code Comparison",
    icon = icon("code-compare"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Compare Cause Codes",
        width = 350,
        sliderInput("compare_years", "Year Range:",
                    min = 1997L, max = 2022L,
                    value = c(2015L, 2022L),
                    step = 1L, sep = ""),
        hr(),
        h5("Group A (e.g., HIV)"),
        textInput("compare_group_a_name", "Group A Label:", value = "HIV"),
        selectizeInput("compare_group_a", "ICD-10 Codes:",
                       choices = NULL,
                       selected = NULL,
                       multiple = TRUE,
                       options = list(placeholder = "Type codes (e.g., B20, B21...)")),
        helpText("Enter codes like B20, B21, B22, B23, B24 for HIV"),
        hr(),
        h5("Group B (e.g., Asthma)"),
        textInput("compare_group_b_name", "Group B Label:", value = "Asthma"),
        selectizeInput("compare_group_b", "ICD-10 Codes:",
                       choices = NULL,
                       selected = NULL,
                       multiple = TRUE,
                       options = list(placeholder = "Type codes (e.g., J45, J46...)")),
        helpText("Enter codes like J45, J46 for Asthma"),
        hr(),
        selectInput("compare_province", "Province:",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE),
        selectInput("compare_agegroup", "Age Group:",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE),
        selectInput("compare_sex", "Sex:",
                    choices = c("All" = "all", "Male" = "Male", "Female" = "Female"),
                    selected = "all")
      ),
      card(
        card_header("Code Group Comparison Over Time"),
        withSpinner(plotlyOutput("compare_time_plot", height = "400px"), type = 4, color = "#0d6efd")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Group A vs Group B Totals"),
          withSpinner(plotlyOutput("compare_totals_plot", height = "350px"), type = 4, color = "#6f42c1")
        ),
        card(
          card_header("Age Distribution Comparison"),
          withSpinner(plotlyOutput("compare_age_plot", height = "350px"), type = 4, color = "#dc3545")
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Sex Distribution Comparison"),
          withSpinner(plotlyOutput("compare_sex_plot", height = "350px"), type = 4, color = "#198754")
        ),
        card(
          card_header("Province Distribution Comparison"),
          withSpinner(plotlyOutput("compare_province_plot", height = "350px"), type = 4, color = "#fd7e14")
        )
      ),
      card(
        card_header("Detailed Comparison Table"),
        withSpinner(DTOutput("compare_table"), type = 4, color = "#6c757d")
      )
    )
  ),
  
  # LGH Cause Comparison Tab (using pre-defined cause groups)
  nav_panel(
    title = "LGH Causes",
    icon = icon("disease"),
    layout_sidebar(
      sidebar = sidebar(
        title = "LGH Cause Comparison",
        width = 380,
        p("Compare pre-defined LGH cause groups (no manual code entry needed)"),
        hr(),
        sliderInput("lgh_years", "Year Range:",
                    min = 1997L, max = 2022L,
                    value = c(2015L, 2022L),
                    step = 1L, sep = ""),
        hr(),
        h5("Group A", style = "color: #0d6efd;"),
        selectInput("lgh_group_a", "Select Cause:",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE),
        hr(),
        h5("Group B", style = "color: #dc3545;"),
        selectInput("lgh_group_b", "Select Cause:",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE),
        hr(),
        selectInput("lgh_province", "Province:",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE),
        selectInput("lgh_agegroup", "Age Group:",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE),
        selectInput("lgh_sex", "Sex:",
                    choices = c("All" = "all", "Male" = "Male", "Female" = "Female"),
                    selected = "all")
      ),
      card(
        card_header("LGH Cause Group Comparison Over Time"),
        withSpinner(plotlyOutput("lgh_time_plot", height = "400px"), type = 4, color = "#0d6efd")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Group Totals"),
          withSpinner(plotlyOutput("lgh_totals_plot", height = "350px"), type = 4, color = "#6f42c1")
        ),
        card(
          card_header("Age Distribution"),
          withSpinner(plotlyOutput("lgh_age_plot", height = "350px"), type = 4, color = "#dc3545")
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Sex Distribution"),
          withSpinner(plotlyOutput("lgh_sex_plot", height = "350px"), type = 4, color = "#198754")
        ),
        card(
          card_header("Province Distribution"),
          withSpinner(plotlyOutput("lgh_province_plot", height = "350px"), type = 4, color = "#fd7e14")
        )
      ),
      card(
        card_header("Detailed Summary Table"),
        withSpinner(DTOutput("lgh_table"), type = 4, color = "#6c757d")
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
        width = 320,
        sliderInput("geo_years", "Year Range:",
                    min = 1997L, max = 2022L,
                    value = c(2020L, 2022L),
                    step = 1L, sep = ""),
        selectInput("geo_agegroup", "Age Group:",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE),
        hr(),
        radioButtons("geo_cause_type", "Cause Grouping:",
                     choices = c("All Causes" = "all",
                                 "Individual ICD-10 Codes" = "icd10",
                                 "LGH Cause Groups" = "lgh"),
                     selected = "all"),
        conditionalPanel(
          condition = "input.geo_cause_type == 'icd10'",
          selectInput("geo_cause", "Underlying Cause (ICD-10):",
                      choices = NULL,
                      selected = NULL,
                      multiple = TRUE)
        ),
        conditionalPanel(
          condition = "input.geo_cause_type == 'lgh'",
          selectInput("geo_lgh_cause", "LGH Cause Group:",
                      choices = NULL,
                      selected = NULL,
                      multiple = TRUE),
          helpText("Pre-defined cause categories from LGH classification")
        ),
        hr(),
        radioButtons("geo_map_metric", "Map Metric:",
                     choices = c("Total Deaths" = "count",
                                 "Deaths per 100k (requires pop)" = "rate"),
                     selected = "count")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Province Choropleth Map"),
          withSpinner(plotlyOutput("geo_province_map", height = "450px"), type = 4, color = "#0d6efd")
        ),
        card(
          card_header("District Choropleth Map"),
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
  
  # Raw Data Tab
  nav_panel(
    title = "Raw Data",
    icon = icon("table"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Filters",
        width = 300,
        selectInput("raw_years", "Year:",
                    choices = 1997:2022,
                    selected = 2022,
                    multiple = FALSE),
        selectInput("raw_province", "Province:",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE),
        numericInput("raw_sample", "Sample Size:", value = 1000, min = 100, max = 10000),
        downloadButton("download_filtered", "Download Filtered Data")
      ),
      card(
        card_header("Raw Data Sample"),
        withSpinner(DTOutput("raw_data_table"), type = 4, color = "#6c757d")
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
## MRC VR Data Explorer (Optimized)

This Shiny application provides interactive exploration of South African vital registration mortality data from 1997-2022.

### Optimizations Applied
- **Pre-aggregated tables**: Common aggregations computed at startup
- **data.table keys**: O(log n) filtering instead of O(n) scans
- **Reactive caching**: Repeated queries served from cache
- **Integer types**: Smaller memory footprint, faster comparisons
- **Column selection**: Only required columns loaded

### Data
- ~13 million death records (1997-2022)
- Key variables: demographics, cause codes, geography, timing
      ")
    )
  )
)

# =============================================================================
# SERVER - OPTIMIZED
# =============================================================================

server <- function(input, output, session) {
  
  # Load data and pre-aggregate ONCE at startup
  dt <- load_data()
  agg <- create_aggregated_tables(dt)
  shapes <- load_shape_files()
  lgh_lookup <- load_lgh_lookup()
  pop_data <- load_population_data()
  
  # Hide the loading screen now that data is ready
  waiter_hide()
  
  # Get unique values for filters
  provinces <- levels(dt$DeathProvinceName)
  provinces <- provinces[!is.na(provinces)]
  agegroups <- levels(dt$agegroup)
  causes <- sort(unique(dt$UnderlyingCause[!is.na(dt$UnderlyingCause) & dt$UnderlyingCause != ""]))
  
  # Create LGH cause choices with descriptions
  lgh_choices <- setNames(lgh_lookup$LGH_Cause, lgh_lookup$display_label)
  
  # Update selectInputs with data values
  updateSelectInput(session, "overview_province", choices = c("All" = "", provinces), selected = "")
  updateSelectInput(session, "temporal_province", choices = c("All" = "", provinces), selected = "")
  updateSelectInput(session, "temporal_agegroup", choices = c("All" = "", agegroups), selected = "")
  updateSelectInput(session, "cause_province", choices = c("All" = "", provinces), selected = "")
  updateSelectInput(session, "cause_agegroup", choices = c("All" = "", agegroups), selected = "")
  updateSelectInput(session, "demo_province", choices = c("All" = "", provinces), selected = "")
  updateSelectInput(session, "geo_agegroup", choices = c("All" = "", agegroups), selected = "")
  updateSelectInput(session, "geo_cause", choices = c("All" = "", causes[1:100]), selected = "")
  updateSelectInput(session, "geo_lgh_cause", choices = lgh_choices, selected = NULL)
  updateSelectInput(session, "raw_province", choices = c("All" = "", provinces), selected = "")
  
  # Code Comparison tab - use selectize for searchable dropdowns
  updateSelectizeInput(session, "compare_group_a", choices = causes, selected = NULL, server = TRUE)
  updateSelectizeInput(session, "compare_group_b", choices = causes, selected = NULL, server = TRUE)
  updateSelectInput(session, "compare_province", choices = c("All" = "", provinces), selected = "")
  updateSelectInput(session, "compare_agegroup", choices = c("All" = "", agegroups), selected = "")
  
  # LGH Cause Comparison tab - use pre-defined cause groups
  updateSelectInput(session, "lgh_group_a", choices = lgh_choices, selected = NULL)
  updateSelectInput(session, "lgh_group_b", choices = lgh_choices, selected = NULL)
  updateSelectInput(session, "lgh_province", choices = c("All" = "", provinces), selected = "")
  updateSelectInput(session, "lgh_agegroup", choices = c("All" = "", agegroups), selected = "")
  
  # ==========================================================================
  # OVERVIEW TAB - Using pre-aggregated data
  # ==========================================================================
  
  # Use the pre-aggregated weekly table instead of raw data
  overview_weekly <- reactive({
    d <- agg$weekly[epi_year >= input$overview_years[1] & epi_year <= input$overview_years[2]]
    
    if (length(input$overview_province) > 0 && !("" %in% input$overview_province)) {
      d <- d[DeathProvinceName %in% input$overview_province]
    }
    
    if (input$overview_death_type != "all") {
      d <- d[DeathType == as.integer(input$overview_death_type)]
    }
    
    if (input$overview_nat_unnat != "all") {
      d <- d[NaturalUnnatural == as.integer(input$overview_nat_unnat)]
    }
    
    # Re-aggregate after filtering
    d[, .(count = sum(count)), by = .(epi_year, epi_week)]
  }) |> bindCache(input$overview_years, input$overview_province, 
                  input$overview_death_type, input$overview_nat_unnat)
  
  overview_by_province <- reactive({
    d <- agg$yearly_province[epi_year >= input$overview_years[1] & epi_year <= input$overview_years[2]]
    
    if (length(input$overview_province) > 0 && !("" %in% input$overview_province)) {
      d <- d[DeathProvinceName %in% input$overview_province]
    }
    
    if (input$overview_death_type != "all") {
      d <- d[DeathType == as.integer(input$overview_death_type)]
    }
    
    if (input$overview_nat_unnat != "all") {
      d <- d[NaturalUnnatural == as.integer(input$overview_nat_unnat)]
    }
    
    d
  }) |> bindCache(input$overview_years, input$overview_province,
                  input$overview_death_type, input$overview_nat_unnat)
  
  output$overview_time_plot <- renderPlotly({
    d <- overview_weekly()[order(epi_year, epi_week)]
    d[, date_approx := as.Date(paste0(epi_year, "-01-01")) + (epi_week - 1L) * 7L]
    
    plot_ly(d, x = ~date_approx, y = ~count, type = 'scatter', mode = 'lines',
            line = list(color = '#0d6efd', width = 1),
            hovertemplate = "Week: %{x}<br>Deaths: %{y:,}<extra></extra>") %>%
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Deaths per Week"),
        hovermode = "x unified"
      )
  })
  
  output$overview_province_plot <- renderPlotly({
    d <- overview_by_province()[, .(count = sum(count)), by = .(DeathProvinceName)]
    d <- d[!is.na(DeathProvinceName)][order(-count)]
    
    plot_ly(d, x = ~reorder(DeathProvinceName, count), y = ~count, type = 'bar',
            marker = list(color = '#0d6efd'),
            hovertemplate = "%{x}<br>Deaths: %{y:,}<extra></extra>") %>%
      layout(
        xaxis = list(title = "", tickangle = -45),
        yaxis = list(title = "Total Deaths")
      )
  })
  
  output$overview_age_plot <- renderPlotly({
    d <- overview_by_province()[, .(count = sum(count)), by = .(agegroup)]
    d <- d[!is.na(agegroup)]
    
    plot_ly(d, x = ~agegroup, y = ~count, type = 'bar',
            marker = list(color = '#198754'),
            hovertemplate = "Age: %{x}<br>Deaths: %{y:,}<extra></extra>") %>%
      layout(
        xaxis = list(title = "Age Group"),
        yaxis = list(title = "Total Deaths")
      )
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
  # TEMPORAL TRENDS TAB - Using pre-aggregated data
  # ==========================================================================
  
  temporal_data <- reactive({
    d <- agg$yearly_province[epi_year >= input$temporal_years[1] & epi_year <= input$temporal_years[2]]
    
    if (length(input$temporal_province) > 0 && !("" %in% input$temporal_province)) {
      d <- d[DeathProvinceName %in% input$temporal_province]
    }
    
    if (length(input$temporal_agegroup) > 0 && !("" %in% input$temporal_agegroup)) {
      d <- d[agegroup %in% input$temporal_agegroup]
    }
    
    d
  }) |> bindCache(input$temporal_years, input$temporal_province, input$temporal_agegroup)
  
  temporal_weekly <- reactive({
    d <- agg$weekly[epi_year >= input$temporal_years[1] & epi_year <= input$temporal_years[2]]
    
    if (length(input$temporal_province) > 0 && !("" %in% input$temporal_province)) {
      d <- d[DeathProvinceName %in% input$temporal_province]
    }
    
    d[, .(count = sum(count)), by = .(epi_year, epi_week)]
  }) |> bindCache(input$temporal_years, input$temporal_province)
  
  output$temporal_plot <- renderPlotly({
    if (input$temporal_aggregation == "week") {
      d <- temporal_weekly()
      d[, date_approx := as.Date(paste0(epi_year, "-01-01")) + (epi_week - 1L) * 7L]
      
      if (input$temporal_compare_years) {
        p <- plot_ly()
        for (yr in unique(d$epi_year)) {
          yr_data <- d[epi_year == yr]
          p <- add_trace(p, data = yr_data, x = ~epi_week, y = ~count, 
                         type = 'scatter', mode = 'lines', name = as.character(yr))
        }
        p <- p %>% layout(xaxis = list(title = "Epidemiological Week"),
                          yaxis = list(title = "Deaths"))
      } else {
        d <- d[order(date_approx)]
        p <- plot_ly(d, x = ~date_approx, y = ~count, type = 'scatter', mode = 'lines',
                     line = list(color = '#0d6efd')) %>%
          layout(xaxis = list(title = "Date"), yaxis = list(title = "Deaths per Week"))
      }
      
    } else if (input$temporal_aggregation == "month") {
      # Need raw data for monthly aggregation by DeathMonth
      d <- dt[epi_year >= input$temporal_years[1] & epi_year <= input$temporal_years[2]]
      if (length(input$temporal_province) > 0 && !("" %in% input$temporal_province)) {
        d <- d[DeathProvinceName %in% input$temporal_province]
      }
      d <- d[, .(count = .N), by = .(epi_year, DeathMonth)]
      d[, date_approx := as.Date(paste0(epi_year, "-", DeathMonth, "-01"))]
      d <- d[order(date_approx)]
      
      p <- plot_ly(d, x = ~date_approx, y = ~count, type = 'scatter', mode = 'lines+markers',
                   line = list(color = '#0d6efd')) %>%
        layout(xaxis = list(title = "Date"), yaxis = list(title = "Deaths per Month"))
      
    } else {
      d <- temporal_data()[, .(count = sum(count)), by = .(epi_year)][order(epi_year)]
      
      p <- plot_ly(d, x = ~epi_year, y = ~count, type = 'bar',
                   marker = list(color = '#0d6efd')) %>%
        layout(xaxis = list(title = "Year"), yaxis = list(title = "Deaths per Year"))
    }
    
    p
  })
  
  output$temporal_heatmap <- renderPlotly({
    d <- temporal_weekly()
    d_wide <- dcast(d, epi_week ~ epi_year, value.var = "count", fill = 0)
    
    plot_ly(z = as.matrix(d_wide[, -1]), 
            x = names(d_wide)[-1],
            y = d_wide$epi_week,
            type = "heatmap",
            colorscale = "Viridis") %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "Epidemiological Week"))
  })
  
  output$temporal_yoy <- renderPlotly({
    d <- temporal_data()[, .(count = sum(count)), by = .(epi_year)][order(epi_year)]
    d[, pct_change := (count - shift(count)) / shift(count) * 100]
    
    plot_ly(d[!is.na(pct_change)], x = ~epi_year, y = ~pct_change, type = 'bar',
            marker = list(color = ~ifelse(pct_change >= 0, '#dc3545', '#198754')),
            hovertemplate = "Year: %{x}<br>Change: %{y:.1f}%<extra></extra>") %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "% Change from Previous Year"))
  })
  
  # ==========================================================================
  # CAUSE CODES TAB - Using pre-aggregated data
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
    
    d
  }) |> bindCache(input$cause_years, input$cause_province, 
                  input$cause_agegroup, input$cause_sex)
  
  output$cause_top_plot <- renderPlotly({
    d <- cause_data()[, .(count = sum(count)), by = .(UnderlyingCause)]
    d <- d[order(-count)][1:min(input$cause_top_n, nrow(d))]
    
    plot_ly(d, y = ~reorder(UnderlyingCause, count), x = ~count, type = 'bar',
            orientation = 'h',
            marker = list(color = '#0d6efd'),
            hovertemplate = "Code: %{y}<br>Deaths: %{x:,}<extra></extra>") %>%
      layout(xaxis = list(title = "Number of Deaths"),
             yaxis = list(title = ""))
  })
  
  output$cause_time_plot <- renderPlotly({
    # Get top 10 causes
    top_causes <- cause_data()[, .(count = sum(count)), by = .(UnderlyingCause)][order(-count)][1:10, UnderlyingCause]
    
    d <- cause_data()[UnderlyingCause %in% top_causes, 
                      .(count = sum(count)), by = .(epi_year, UnderlyingCause)]
    
    plot_ly(d, x = ~epi_year, y = ~count, color = ~UnderlyingCause, 
            type = 'scatter', mode = 'lines+markers') %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "Deaths"),
             legend = list(orientation = "h", y = -0.2))
  })
  
  output$cause_province_plot <- renderPlotly({
    top_causes <- cause_data()[, .(count = sum(count)), by = .(UnderlyingCause)][order(-count)][1:5, UnderlyingCause]
    
    d <- cause_data()[UnderlyingCause %in% top_causes & !is.na(DeathProvinceName), 
                      .(count = sum(count)), by = .(DeathProvinceName, UnderlyingCause)]
    
    plot_ly(d, x = ~DeathProvinceName, y = ~count, color = ~UnderlyingCause, type = 'bar') %>%
      layout(xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = "Deaths"),
             barmode = 'stack')
  })
  
  output$cause_table <- renderDT({
    d <- cause_data()[, .(Deaths = sum(count),
                          Mean_Age = weighted.mean(mean_age, count, na.rm = TRUE)), 
                      by = .(UnderlyingCause)]
    d <- d[order(-Deaths)]
    d[, Percentage := round(100 * Deaths / sum(Deaths), 2)]
    d[, Mean_Age := round(Mean_Age, 1)]
    
    datatable(d, options = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE)
  })
  
  # ==========================================================================
  # DEMOGRAPHICS TAB - Using pre-aggregated data
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
  
  output$demo_age_hist <- renderPlotly({
    d <- demo_data()[, .(count = sum(count)), by = .(agegroup)]
    d <- d[!is.na(agegroup)]
    
    plot_ly(d, x = ~agegroup, y = ~count, type = 'bar',
            marker = list(color = '#0d6efd'),
            hovertemplate = "Age: %{x}<br>Deaths: %{y:,}<extra></extra>") %>%
      layout(xaxis = list(title = "Age Group"),
             yaxis = list(title = "Count"))
  })
  
  output$demo_sex_plot <- renderPlotly({
    d <- demo_data()[, .(count = sum(count)), by = .(SexName)]
    d <- d[!is.na(SexName)]
    
    plot_ly(d, labels = ~SexName, values = ~count, type = 'pie',
            marker = list(colors = c('#0d6efd', '#dc3545', '#6c757d', '#ffc107'))) %>%
      layout(title = "")
  })
  
  output$demo_pyramid <- renderPlotly({
    d <- demo_data()[SexName %in% c("Male", "Female") & !is.na(agegroup), 
                     .(count = sum(count)), by = .(agegroup, SexName)]
    
    d_male <- d[SexName == "Male"][, .(agegroup, count_male = -count)]
    d_female <- d[SexName == "Female"][, .(agegroup, count_female = count)]
    
    d_merged <- merge(d_male, d_female, by = "agegroup", all = TRUE)
    d_merged[is.na(count_male), count_male := 0]
    d_merged[is.na(count_female), count_female := 0]
    
    plot_ly() %>%
      add_bars(data = d_merged, y = ~agegroup, x = ~count_male, orientation = 'h',
               name = 'Male', marker = list(color = '#0d6efd')) %>%
      add_bars(data = d_merged, y = ~agegroup, x = ~count_female, orientation = 'h',
               name = 'Female', marker = list(color = '#dc3545')) %>%
      layout(barmode = 'relative',
             xaxis = list(title = "Deaths"),
             yaxis = list(title = "", categoryorder = "array", 
                          categoryarray = agegroups))
  })
  
  output$demo_reg_delay <- renderPlotly({
    d <- agg$reg_delay[epi_year >= input$demo_years[1] & epi_year <= input$demo_years[2]]
    
    if (length(input$demo_province) > 0 && !("" %in% input$demo_province)) {
      d <- d[DeathProvinceName %in% input$demo_province]
    }
    
    delay_summary <- d[, .(count = sum(count)), by = .(reg_delay)]
    
    plot_ly(delay_summary, x = ~reg_delay, y = ~count, type = 'bar',
            marker = list(color = '#198754'),
            hovertemplate = "Delay: %{x} years<br>Deaths: %{y:,}<extra></extra>") %>%
      layout(xaxis = list(title = "Registration Delay (Years)", dtick = 1),
             yaxis = list(title = "Number of Deaths"))
  })
  
  # ==========================================================================
  # CODE COMPARISON TAB
  # ==========================================================================
  
  # Helper: Match codes with prefix pattern (e.g., "B20" matches "B200", "B201", etc.)
  match_code_group <- function(codes, patterns) {
    if (is.null(patterns) || length(patterns) == 0) return(character(0))
    # Create regex pattern to match codes that start with any of the patterns
    regex_patterns <- paste0("^(", paste(patterns, collapse = "|"), ")")
    codes[grepl(regex_patterns, codes, ignore.case = TRUE)]
  }
  
  compare_data <- reactive({
    req(input$compare_group_a, input$compare_group_b)
    
    # Get all codes that match the patterns
    all_causes <- unique(agg$causes$UnderlyingCause)
    group_a_codes <- match_code_group(all_causes, input$compare_group_a)
    group_b_codes <- match_code_group(all_causes, input$compare_group_b)
    
    if (length(group_a_codes) == 0 || length(group_b_codes) == 0) {
      return(NULL)
    }
    
    # Filter base data
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
    
    # Assign group labels
    d[, code_group := fcase(
      UnderlyingCause %in% group_a_codes, input$compare_group_a_name,
      UnderlyingCause %in% group_b_codes, input$compare_group_b_name,
      default = NA_character_
    )]
    
    # Filter to only matched codes
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
            colors = c('#0d6efd', '#dc3545'),
            hovertemplate = "Year: %{x}<br>Deaths: %{y:,}<extra>%{fullData.name}</extra>") %>%
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
            textposition = 'outside',
            hovertemplate = "%{x}<br>Deaths: %{y:,}<extra></extra>") %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "Total Deaths"))
  })
  
  output$compare_age_plot <- renderPlotly({
    d <- compare_data()
    if (is.null(d) || nrow(d) == 0) {
      return(plotly_empty() %>% layout(title = "Select codes for both groups"))
    }
    
    d_age <- d[!is.na(agegroup), .(count = sum(count)), by = .(agegroup, code_group)]
    
    # Calculate percentages within each group for comparison
    d_age[, pct := count / sum(count) * 100, by = code_group]
    
    plot_ly(d_age, x = ~agegroup, y = ~pct, color = ~code_group,
            type = 'bar',
            colors = c('#0d6efd', '#dc3545'),
            hovertemplate = "Age: %{x}<br>%{y:.1f}%<extra>%{fullData.name}</extra>") %>%
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
            type = 'bar',
            colors = c('#0d6efd', '#dc3545'),
            hovertemplate = "%{x}<br>%{y:.1f}%<extra>%{fullData.name}</extra>") %>%
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
            type = 'bar',
            colors = c('#0d6efd', '#dc3545'),
            hovertemplate = "%{x}<br>%{y:.1f}%<extra>%{fullData.name}</extra>") %>%
      layout(xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = "Percentage within Group"),
             barmode = 'group',
             legend = list(orientation = "h", y = -0.3))
  })
  
  output$compare_table <- renderDT({
    d <- compare_data()
    if (is.null(d) || nrow(d) == 0) {
      return(datatable(data.frame(Message = "Select codes for both groups to see comparison")))
    }
    
    # Create summary by code
    d_summary <- d[, .(
      Deaths = sum(count),
      Mean_Age = weighted.mean(mean_age, count, na.rm = TRUE)
    ), by = .(code_group, UnderlyingCause)]
    
    d_summary <- d_summary[order(code_group, -Deaths)]
    d_summary[, Mean_Age := round(Mean_Age, 1)]
    d_summary[, Pct_of_Group := round(100 * Deaths / sum(Deaths), 2), by = code_group]
    
    setnames(d_summary, c("code_group", "UnderlyingCause"), c("Group", "ICD-10 Code"))
    
    datatable(d_summary,
              options = list(pageLength = 20, scrollX = TRUE,
                             order = list(list(0, 'asc'), list(2, 'desc'))),
              rownames = FALSE,
              filter = 'top')
  })
  
  # ==========================================================================
  # LGH CAUSE COMPARISON TAB
  # ==========================================================================
  
  # Helper function: expand LGH cause range to individual codes
  expand_lgh_cause <- function(lgh_cause, all_codes) {
    # Handle ranges like "E10-E14", "I60-I69", etc.
    if (grepl("-", lgh_cause) && !grepl("\\+", lgh_cause)) {
      # Extract prefix and range
      parts <- strsplit(lgh_cause, "-")[[1]]
      if (length(parts) == 2) {
        prefix <- gsub("[0-9].*", "", parts[1])
        start_num <- as.integer(gsub("[^0-9]", "", parts[1]))
        end_num <- as.integer(gsub("[^0-9]", "", parts[2]))
        
        if (!is.na(start_num) && !is.na(end_num)) {
          # Generate all codes in range
          expanded <- paste0(prefix, sprintf("%02d", start_num:end_num))
          # Also include 3-character versions
          expanded <- c(expanded, paste0(prefix, start_num:end_num))
          # Match against actual codes
          return(all_codes[all_codes %in% expanded | 
                          substr(all_codes, 1, 3) %in% expanded])
        }
      }
    }
    
    # Handle special cases like "R00-R99+I46"
    if (grepl("\\+", lgh_cause)) {
      parts <- strsplit(lgh_cause, "\\+")[[1]]
      result <- character(0)
      for (p in parts) {
        result <- c(result, expand_lgh_cause(trimws(p), all_codes))
      }
      return(unique(result))
    }
    
    # Handle broad categories with asterisk like "I00-I99*"
    if (grepl("\\*$", lgh_cause)) {
      base <- gsub("\\*$", "", lgh_cause)
      return(expand_lgh_cause(base, all_codes))
    }
    
    # Single code - match exact or prefix
    matched <- all_codes[all_codes == lgh_cause | 
                        startsWith(all_codes, lgh_cause)]
    return(matched)
  }
  
  lgh_data <- reactive({
    req(input$lgh_group_a, input$lgh_group_b)
    
    # Get all unique underlying causes
    all_causes <- unique(agg$causes$UnderlyingCause)
    
    # Expand selected LGH causes to actual ICD codes
    group_a_codes <- unique(unlist(lapply(input$lgh_group_a, expand_lgh_cause, all_codes = all_causes)))
    group_b_codes <- unique(unlist(lapply(input$lgh_group_b, expand_lgh_cause, all_codes = all_causes)))
    
    if (length(group_a_codes) == 0 || length(group_b_codes) == 0) {
      return(NULL)
    }
    
    # Get labels for display
    group_a_label <- paste(lgh_lookup[LGH_Cause %in% input$lgh_group_a, description], collapse = " + ")
    group_b_label <- paste(lgh_lookup[LGH_Cause %in% input$lgh_group_b, description], collapse = " + ")
    
    # Shorten if too long
    if (nchar(group_a_label) > 40) group_a_label <- paste0(substr(group_a_label, 1, 37), "...")
    if (nchar(group_b_label) > 40) group_b_label <- paste0(substr(group_b_label, 1, 37), "...")
    
    # Filter base data
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
    
    # Assign group labels
    d[, cause_group := fcase(
      UnderlyingCause %in% group_a_codes, group_a_label,
      UnderlyingCause %in% group_b_codes, group_b_label,
      default = NA_character_
    )]
    
    # Keep group labels as attributes for plotting
    attr(d, "group_a_label") <- group_a_label
    attr(d, "group_b_label") <- group_b_label
    
    # Filter to only matched codes
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
            colors = c('#0d6efd', '#dc3545'),
            hovertemplate = "Year: %{x}<br>Deaths: %{y:,}<extra>%{fullData.name}</extra>") %>%
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
            textposition = 'outside',
            hovertemplate = "%{x}<br>Deaths: %{y:,}<extra></extra>") %>%
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
            type = 'bar',
            colors = c('#0d6efd', '#dc3545'),
            hovertemplate = "Age: %{x}<br>%{y:.1f}%<extra>%{fullData.name}</extra>") %>%
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
            type = 'bar',
            colors = c('#0d6efd', '#dc3545'),
            hovertemplate = "%{x}<br>%{y:.1f}%<extra>%{fullData.name}</extra>") %>%
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
            type = 'bar',
            colors = c('#0d6efd', '#dc3545'),
            hovertemplate = "%{x}<br>%{y:.1f}%<extra>%{fullData.name}</extra>") %>%
      layout(xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = "Percentage within Group"),
             barmode = 'group',
             legend = list(orientation = "h", y = -0.3))
  })
  
  output$lgh_table <- renderDT({
    d <- lgh_data()
    if (is.null(d) || nrow(d) == 0) {
      return(datatable(data.frame(Message = "Select cause groups to see comparison")))
    }
    
    # Summary by cause group
    d_summary <- d[, .(
      Deaths = sum(count),
      Mean_Age = weighted.mean(mean_age, count, na.rm = TRUE),
      N_Codes = uniqueN(UnderlyingCause)
    ), by = .(cause_group)]
    
    d_summary[, Mean_Age := round(Mean_Age, 1)]
    d_summary[, Pct_Total := round(100 * Deaths / sum(Deaths), 1)]
    
    setnames(d_summary, "cause_group", "Cause Group")
    
    datatable(d_summary,
              options = list(pageLength = 10, scrollX = TRUE, dom = 't'),
              rownames = FALSE)
  })
  
  # ==========================================================================
  # GEOGRAPHIC TAB - Using pre-aggregated data
  # ==========================================================================
  
  geo_data <- reactive({
    cause_type <- input$geo_cause_type
    
    if (cause_type == "all") {
      # All causes - use yearly_province aggregation
      d <- agg$yearly_province[epi_year >= input$geo_years[1] & epi_year <= input$geo_years[2]]
      
      if (length(input$geo_agegroup) > 0 && !("" %in% input$geo_agegroup)) {
        d <- d[agegroup %in% input$geo_agegroup]
      }
    } else if (cause_type == "icd10") {
      # Individual ICD-10 codes - use causes aggregation
      d <- agg$causes[epi_year >= input$geo_years[1] & epi_year <= input$geo_years[2]]
      
      if (length(input$geo_agegroup) > 0 && !("" %in% input$geo_agegroup)) {
        d <- d[agegroup %in% input$geo_agegroup]
      }
      
      if (length(input$geo_cause) > 0) {
        # Use prefix matching like in Cause Codes tab
        all_causes <- unique(d$UnderlyingCause)
        matched_codes <- character(0)
        for (pattern in input$geo_cause) {
          matched_codes <- c(matched_codes, all_causes[grepl(paste0("^", pattern), all_causes, ignore.case = TRUE)])
        }
        d <- d[UnderlyingCause %in% matched_codes]
      }
    } else if (cause_type == "lgh") {
      # LGH cause groups - use causes aggregation and expand LGH codes
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
    cause_type <- input$geo_cause_type
    
    if (cause_type == "all") {
      # All causes
      d <- agg$districts[epi_year >= input$geo_years[1] & epi_year <= input$geo_years[2]]
    } else if (cause_type == "icd10") {
      # Individual ICD-10 codes - need to use causes data and aggregate by district
      d <- agg$causes[epi_year >= input$geo_years[1] & epi_year <= input$geo_years[2]]
      
      if (length(input$geo_cause) > 0) {
        all_causes <- unique(d$UnderlyingCause)
        matched_codes <- character(0)
        for (pattern in input$geo_cause) {
          matched_codes <- c(matched_codes, all_causes[grepl(paste0("^", pattern), all_causes, ignore.case = TRUE)])
        }
        d <- d[UnderlyingCause %in% matched_codes]
      }
    } else if (cause_type == "lgh") {
      # LGH cause groups
      d <- agg$causes[epi_year >= input$geo_years[1] & epi_year <= input$geo_years[2]]
      
      if (length(input$geo_lgh_cause) > 0) {
        all_causes <- unique(d$UnderlyingCause)
        matched_codes <- unique(unlist(lapply(input$geo_lgh_cause, expand_lgh_cause, all_codes = all_causes)))
        d <- d[UnderlyingCause %in% matched_codes]
      }
    }
    
    # For district-level aggregation from causes data, we need to handle differently
    # The districts aggregation already has deathdistrictname, but causes has DeathProvinceName
    # We'll need to aggregate appropriately
    if ("deathdistrictname" %in% names(d)) {
      d[, .(count = sum(count)), by = .(deathdistrictname)]
    } else {
      # If using causes data which doesn't have district, return empty with message
      # District-level cause filtering requires district info in causes data
      data.table(deathdistrictname = character(0), count = numeric(0))
    }
  }) |> bindCache(input$geo_years, input$geo_cause_type, input$geo_cause, input$geo_lgh_cause)
  
  # Province choropleth map
  output$geo_province_map <- renderPlotly({
    if (is.null(shapes)) {
      return(plotly_empty() %>% layout(title = "Shape files not available"))
    }
    
    # Get province counts
    d <- geo_data()[!is.na(DeathProvinceName), .(count = sum(count)), by = .(DeathProvinceName)]
    
    # Check if no data after filtering
    if (nrow(d) == 0 || sum(d$count, na.rm = TRUE) == 0) {
      return(plotly_empty() %>% layout(title = "No data for selected cause filter"))
    }
    
    # Calculate rate if population data available and rate selected
    show_rate <- input$geo_map_metric == "rate" && !is.null(pop_data)
    
    if (show_rate) {
      # Get average population for selected years
      years_selected <- seq(input$geo_years[1], input$geo_years[2])
      pop_avg <- pop_data$province[Year %in% years_selected, 
                                   .(avg_pop = mean(population, na.rm = TRUE)), 
                                   by = .(province_full)]
      d <- merge(d, pop_avg, by.x = "DeathProvinceName", by.y = "province_full", all.x = TRUE)
      d[, rate := count / avg_pop * 100000]
    }
    
    # Join with shape file
    prov_sf <- shapes$provinces %>%
      left_join(d, by = c("province_full" = "DeathProvinceName"))
    
    # Build title with cause filter info
    cause_label <- ""
    if (input$geo_cause_type == "icd10" && length(input$geo_cause) > 0) {
      cause_label <- paste0(" - ", paste(input$geo_cause, collapse = ", "))
      if (nchar(cause_label) > 30) cause_label <- paste0(substr(cause_label, 1, 27), "...")
    } else if (input$geo_cause_type == "lgh" && length(input$geo_lgh_cause) > 0) {
      lgh_descs <- lgh_lookup[LGH_Cause %in% input$geo_lgh_cause, description]
      cause_label <- paste0(" - ", paste(lgh_descs, collapse = ", "))
      if (nchar(cause_label) > 50) cause_label <- paste0(substr(cause_label, 1, 47), "...")
    }
    
    # Create hover text
    if (show_rate) {
      prov_sf$hover_text <- paste0(
        "<b>", prov_sf$province_full, "</b><br>",
        "Deaths: ", format(prov_sf$count, big.mark = ",", na.rm = TRUE), "<br>",
        "Population: ", format(round(prov_sf$avg_pop), big.mark = ",", na.rm = TRUE), "<br>",
        "Rate: ", round(prov_sf$rate, 1), " per 100k"
      )
      fill_var <- prov_sf$rate
      legend_title <- "Deaths per 100k"
      title_suffix <- " (Rate per 100k)"
    } else {
      prov_sf$hover_text <- paste0(
        "<b>", prov_sf$province_full, "</b><br>",
        "Deaths: ", format(prov_sf$count, big.mark = ",", na.rm = TRUE)
      )
      fill_var <- prov_sf$count
      legend_title <- "Deaths"
      title_suffix <- ""
    }
    
    # Create ggplot choropleth
    p <- ggplot(prov_sf) +
      geom_sf(aes(fill = fill_var, text = hover_text), color = "white", linewidth = 0.3) +
      scale_fill_gradient(
        low = "#fff7bc",
        high = "#d95f0e",
        na.value = "grey80",
        name = legend_title,
        labels = if(show_rate) scales::number_format(accuracy = 1) else scales::comma
      ) +
      theme_void() +
      theme(
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 12)
      ) +
      labs(title = paste0("Deaths by Province (", input$geo_years[1], "-", input$geo_years[2], ")", 
                          cause_label, title_suffix))
    
    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(align = "left"))
  })
  
  # District choropleth map
  output$geo_district_map <- renderPlotly({
    if (is.null(shapes)) {
      return(plotly_empty() %>% layout(title = "Shape files not available"))
    }
    
    # Get district counts
    d <- geo_district()
    
    # Check if we have district data (not available when filtering by cause)
    if (nrow(d) == 0) {
      return(plotly_empty() %>% layout(
        title = list(
          text = "District-level cause filtering not available<br><sub>Use 'All Causes' for district map</sub>",
          font = list(size = 14)
        )
      ))
    }
    
    # Calculate rate if population data available and rate selected
    show_rate <- input$geo_map_metric == "rate" && !is.null(pop_data)
    
    if (show_rate) {
      # Get average population for selected years
      years_selected <- seq(input$geo_years[1], input$geo_years[2])
      pop_avg <- pop_data$district[Year %in% years_selected, 
                                   .(avg_pop = mean(population, na.rm = TRUE)), 
                                   by = .(district_standard)]
      d <- merge(d, pop_avg, by.x = "deathdistrictname", by.y = "district_standard", all.x = TRUE)
      d[, rate := count / avg_pop * 100000]
    }
    
    # Join with shape file - handle slight name differences
    dist_sf <- shapes$districts %>%
      left_join(d, by = c("district_standard" = "deathdistrictname"))
    
    # Create hover text
    if (show_rate) {
      dist_sf$hover_text <- paste0(
        "<b>", dist_sf$district_standard, "</b><br>",
        "Province: ", dist_sf$prov, "<br>",
        "Deaths: ", format(dist_sf$count, big.mark = ",", na.rm = TRUE), "<br>",
        "Population: ", format(round(dist_sf$avg_pop), big.mark = ",", na.rm = TRUE), "<br>",
        "Rate: ", round(dist_sf$rate, 1), " per 100k"
      )
      fill_var <- dist_sf$rate
      legend_title <- "Deaths per 100k"
      title_suffix <- " (Rate per 100k)"
    } else {
      dist_sf$hover_text <- paste0(
        "<b>", dist_sf$district_standard, "</b><br>",
        "Province: ", dist_sf$prov, "<br>",
        "Deaths: ", format(dist_sf$count, big.mark = ",", na.rm = TRUE)
      )
      fill_var <- dist_sf$count
      legend_title <- "Deaths"
      title_suffix <- ""
    }
    
    # Create ggplot choropleth
    p <- ggplot(dist_sf) +
      geom_sf(aes(fill = fill_var, text = hover_text), color = "white", linewidth = 0.1) +
      scale_fill_gradient(
        low = "#e5f5e0",
        high = "#31a354",
        na.value = "grey80",
        name = legend_title,
        labels = if(show_rate) scales::number_format(accuracy = 1) else scales::comma
      ) +
      theme_void() +
      theme(
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 12)
      ) +
      labs(title = paste0("Deaths by District (", input$geo_years[1], "-", input$geo_years[2], ")", title_suffix))
    
    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(align = "left"))
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
  # RAW DATA TAB - Uses raw data but with efficient sampling
  # ==========================================================================
  
  raw_data <- reactive({
    # Use key for fast year filtering
    d <- dt[.(as.integer(input$raw_years)), nomatch = NULL]
    
    if (length(input$raw_province) > 0 && !("" %in% input$raw_province)) {
      d <- d[DeathProvinceName %in% input$raw_province]
    }
    
    # Sample for performance
    if (nrow(d) > input$raw_sample) {
      d <- d[sample(.N, input$raw_sample)]
    }
    
    # Select key columns for display
    d[, .(serialno, DeathYear, DeathMonth, DeathDay, 
          age, agegroup, SexName, 
          DeathProvinceName, deathdistrictname,
          UnderlyingCause, CauseA, CauseB,
          RegistrationYear, RegistrationMonth)]
  }) |> bindCache(input$raw_years, input$raw_province, input$raw_sample)
  
  output$raw_data_table <- renderDT({
    datatable(raw_data(), 
              options = list(pageLength = 25, scrollX = TRUE),
              rownames = FALSE,
              filter = 'top')
  })
  
  output$download_filtered <- downloadHandler(
    filename = function() {
      paste0("mortality_data_", input$raw_years, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      fwrite(raw_data(), file)
    }
  )
  
}

# =============================================================================
# RUN APP
# =============================================================================

shinyApp(ui, server)
