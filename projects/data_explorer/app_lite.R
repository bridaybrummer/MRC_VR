################################################################################
# SA Mortality Data Explorer - LITE VERSION
# 
# This lightweight version uses pre-aggregated data for:
# - Fast loading (seconds vs minutes)
# - Small file size (~50MB vs ~3GB)
# - Safe for deployment to shinyapps.io
# - No individual-level data exposed
#
# Run: shiny::runApp("projects/data_explorer/app_lite.R")
################################################################################

library(shiny)
library(bslib)
library(data.table)
library(plotly)
library(DT)
library(sf)
library(dplyr)

# Resolve function conflicts
if (requireNamespace("conflicted", quietly = TRUE)) {
  conflicted::conflicts_prefer(plotly::layout)
  conflicted::conflicts_prefer(dplyr::filter)
}

# ============================================================================
# DATA LOADING
# ============================================================================

# Set paths - handle both sourcing and runApp scenarios
get_app_dir <- function() {
  # Try multiple methods to find the app directory
  
  # Method 1: If run via runApp or source
  if (!is.null(sys.frame(1)$ofile)) {
    return(dirname(sys.frame(1)$ofile))
  }
  
  # Method 2: Check for shiny's app directory
  if (exists("getCurrentAppDir", envir = asNamespace("shiny"))) {
    d <- try(shiny:::getCurrentAppDir(), silent = TRUE)
    if (!inherits(d, "try-error") && !is.null(d)) {
      return(d)
    }
  }
  
  # Method 3: Working directory
  wd <- getwd()
  if (file.exists(file.path(wd, "aggregated_data"))) {
    return(wd)
  }
  
  # Method 4: Look for the data_explorer folder
  if (file.exists(file.path(wd, "projects/data_explorer/aggregated_data"))) {
    return(file.path(wd, "projects/data_explorer"))
  }
  
  # Fallback
  return(wd)
}

app_dir <- get_app_dir()
data_dir <- file.path(app_dir, "aggregated_data")
root_dir <- dirname(dirname(app_dir))

load_aggregated_data <- function() {
  cat("Loading aggregated data...\n")
  
  data <- list()
  
  # Load CSV files (skip full_aggregation as it's large)
  files <- c(
    "weekly_province", "weekly_cause", "monthly",
    "age_sex_year", "pyramid_data",
    "province_year", "district_year",
    "cause_year", "cause_province_year"
  )
  
  for (f in files) {
    path <- file.path(data_dir, paste0(f, ".csv"))
    if (file.exists(path)) {
      data[[f]] <- fread(path)
      cat("  ✓", f, ":", format(nrow(data[[f]]), big.mark = ","), "rows\n")
    } else {
      cat("  ✗", f, ": not found\n")
    }
  }
  
  return(data)
}

load_shape_files <- function() {
  app_dir <- get_app_dir()
  
  # First try bundled shape_files.rda in app directory
  bundled_path <- file.path(app_dir, "shape_files.rda")
  if (file.exists(bundled_path)) {
    env <- new.env()
    load(bundled_path, envir = env)
    cat("  Shape files loaded from bundled file\n")
    return(list(provinces = env$provinces, districts = env$districts))
  }
  
  cat("  Shape files not found\n")
  return(NULL)
}

load_population_data <- function() {
  app_dir <- get_app_dir()
  
  # Try bundled population_data.rda
  bundled_path <- file.path(app_dir, "population_data.rda")
  if (file.exists(bundled_path)) {
    env <- new.env()
    load(bundled_path, envir = env)
    cat("  Population data loaded from bundled file\n")
    # Return whatever structure is in the file
    if (exists("pop_prov", envir = env) && exists("pop_dist", envir = env)) {
      return(list(province = env$pop_prov, district = env$pop_dist))
    }
    if (exists("population_data", envir = env)) {
      return(env$population_data)
    }
  }
  
  cat("  Population data not found\n")
  return(NULL)
}

# Province name mapping
prov_name_map <- c(
  "WC" = "Western Cape", "EC" = "Eastern Cape", "NC" = "Northern Cape",
  "FS" = "Free State", "KZN" = "KwaZulu-Natal", "NW" = "North West",
  "GT" = "Gauteng", "MP" = "Mpumalanga", "LIM" = "Limpopo"
)
prov_code_map <- setNames(names(prov_name_map), prov_name_map)

# Load data at startup
agg_data <- load_aggregated_data()
shapes <- load_shape_files()
pop_data <- load_population_data()

# Get available years
if (!is.null(agg_data$weekly_province)) {
  year_range <- range(agg_data$weekly_province$epi_year, na.rm = TRUE)
} else {
  year_range <- c(1997, 2022)
}

# Get provinces
provinces <- if (!is.null(agg_data$province_year)) {
  unique(agg_data$province_year$DeathProvince)
} else {
  names(prov_name_map)
}

cat("Data loading complete!\n")

# ============================================================================
# UI
# ============================================================================

ui <- page_navbar(
  title = "SA Mortality Explorer (Lite)",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2c3e50"
  ),
  
  # Overview Tab
  nav_panel(
    title = "📊 Overview",
    icon = icon("chart-line"),
    
    layout_sidebar(
      sidebar = sidebar(
        title = "Filters",
        width = 280,
        
        sliderInput("year_range", "Year Range",
                    min = year_range[1], max = year_range[2],
                    value = year_range, step = 1, sep = ""),
        
        selectInput("province_filter", "Province",
                    choices = c("All Provinces" = "all", provinces),
                    selected = "all")
      ),
      
      # Main content
      layout_columns(
        col_widths = c(4, 4, 4),
        
        value_box(
          title = "Total Deaths",
          value = textOutput("total_deaths"),
          showcase = icon("users"),
          theme = "primary"
        ),
        value_box(
          title = "Year Range",
          value = textOutput("year_display"),
          showcase = icon("calendar"),
          theme = "info"
        ),
        value_box(
          title = "Provinces",
          value = textOutput("province_count"),
          showcase = icon("map"),
          theme = "success"
        )
      ),
      
      card(
        card_header("Deaths Over Time"),
        plotlyOutput("overview_trend", height = "400px")
      )
    )
  ),
  
  # Temporal Tab
  nav_panel(
    title = "📅 Temporal",
    icon = icon("calendar-alt"),
    
    layout_sidebar(
      sidebar = sidebar(
        title = "Options",
        width = 280,
        
        sliderInput("temp_years", "Year Range",
                    min = year_range[1], max = year_range[2],
                    value = year_range, step = 1, sep = ""),
        
        selectInput("temp_province", "Province",
                    choices = c("All Provinces" = "all", provinces),
                    selected = "all"),
        
        radioButtons("temp_agg", "Aggregation",
                     choices = c("Weekly" = "week", "Monthly" = "month", "Yearly" = "year"),
                     selected = "week")
      ),
      
      card(
        card_header("Temporal Trends"),
        plotlyOutput("temporal_plot", height = "500px")
      )
    )
  ),
  
  # Cause Codes Tab
  nav_panel(
    title = "🏥 Cause Codes",
    icon = icon("medkit"),
    
    layout_sidebar(
      sidebar = sidebar(
        title = "Options",
        width = 280,
        
        sliderInput("cause_years", "Year Range",
                    min = year_range[1], max = year_range[2],
                    value = year_range, step = 1, sep = ""),
        
        sliderInput("top_n_causes", "Top N Causes",
                    min = 5, max = 50, value = 20, step = 5)
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          card_header("Top Causes (All Years)"),
          plotlyOutput("top_causes_plot", height = "500px")
        ),
        
        card(
          card_header("Cause Trends Over Time"),
          plotlyOutput("cause_trends_plot", height = "500px")
        )
      )
    )
  ),
  
  # Demographics Tab
  nav_panel(
    title = "👥 Demographics",
    icon = icon("users"),
    
    layout_sidebar(
      sidebar = sidebar(
        title = "Options",
        width = 280,
        
        sliderInput("demo_years", "Year Range",
                    min = year_range[1], max = year_range[2],
                    value = year_range, step = 1, sep = ""),
        
        selectInput("demo_province", "Province",
                    choices = c("All Provinces" = "all", provinces),
                    selected = "all")
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          card_header("Age Distribution"),
          plotlyOutput("age_dist_plot", height = "400px")
        ),
        
        card(
          card_header("Sex Distribution"),
          plotlyOutput("sex_dist_plot", height = "400px")
        )
      ),
      
      card(
        card_header("Population Pyramid"),
        plotlyOutput("pyramid_plot", height = "500px")
      )
    )
  ),
  
  # Geographic Tab
  nav_panel(
    title = "🗺️ Geographic",
    icon = icon("globe-africa"),
    
    layout_sidebar(
      sidebar = sidebar(
        title = "Options",
        width = 280,
        
        sliderInput("geo_years", "Year Range",
                    min = year_range[1], max = year_range[2],
                    value = year_range, step = 1, sep = ""),
        
        radioButtons("geo_metric", "Display Metric",
                     choices = c("Deaths (Count)" = "count", 
                                 "Deaths per 100k" = "rate"),
                     selected = "count"),
        
        radioButtons("geo_level", "Geographic Level",
                     choices = c("Province" = "province", "District" = "district"),
                     selected = "province")
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          card_header("Choropleth Map"),
          plotlyOutput("geo_choropleth", height = "500px")
        ),
        
        card(
          card_header("Provincial Trends"),
          plotlyOutput("geo_trends", height = "500px")
        )
      )
    )
  ),
  
  # About Tab
  nav_panel(
    title = "ℹ️ About",
    icon = icon("info-circle"),
    
    card(
      card_header("About This App"),
      card_body(
        h4("SA Mortality Data Explorer - Lite Version"),
        p("This lightweight version of the mortality data explorer uses pre-aggregated 
          summary statistics rather than individual-level records."),
        
        h5("Data Coverage"),
        tags$ul(
          tags$li("Time period: 1997-2022"),
          tags$li("Geographic: 9 provinces, 52 districts"),
          tags$li("Source: South African vital registration system")
        ),
        
        h5("Limitations of Lite Version"),
        tags$ul(
          tags$li("No individual record browsing"),
          tags$li("Fixed age groupings"),
          tags$li("Limited cross-tabulation flexibility"),
          tags$li("No raw data export")
        ),
        
        h5("For Full Access"),
        p("The full version with individual-level data is available for local use only.
          Contact the data custodian for access."),
        
        hr(),
        p(em("Built with R Shiny • Data: MRC Vital Registration"))
      )
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  
  # ---- Overview Tab ----
  
  filtered_weekly <- reactive({
    req(agg_data$weekly_province)
    d <- agg_data$weekly_province[
      epi_year >= input$year_range[1] & epi_year <= input$year_range[2]
    ]
    if (input$province_filter != "all") {
      d <- d[DeathProvince == input$province_filter]
    }
    d
  })
  
  output$total_deaths <- renderText({
    format(sum(filtered_weekly()$deaths, na.rm = TRUE), big.mark = ",")
  })
  
  output$year_display <- renderText({
    paste(input$year_range[1], "-", input$year_range[2])
  })
  
  output$province_count <- renderText({
    if (input$province_filter == "all") "9" else "1"
  })
  
  output$overview_trend <- renderPlotly({
    d <- filtered_weekly()[, .(deaths = sum(deaths, na.rm = TRUE)), 
                           by = .(epi_year, epi_week)]
    d[, date := as.Date(paste0(epi_year, "-01-01")) + (epi_week - 1) * 7]
    
    plot_ly(d, x = ~date, y = ~deaths, type = "scatter", mode = "lines",
            line = list(color = "#2c3e50")) %>%
      plotly::layout(
        title = "Weekly Deaths",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Deaths"),
        hovermode = "x unified"
      )
  })
  
  # ---- Temporal Tab ----
  
  output$temporal_plot <- renderPlotly({
    req(agg_data$weekly_province)
    
    d <- agg_data$weekly_province[
      epi_year >= input$temp_years[1] & epi_year <= input$temp_years[2]
    ]
    
    if (input$temp_province != "all") {
      d <- d[DeathProvince == input$temp_province]
    }
    
    if (input$temp_agg == "week") {
      d <- d[, .(deaths = sum(deaths, na.rm = TRUE)), by = .(epi_year, epi_week)]
      d[, date := as.Date(paste0(epi_year, "-01-01")) + (epi_week - 1) * 7]
      
      plot_ly(d, x = ~date, y = ~deaths, type = "scatter", mode = "lines") %>%
        plotly::layout(title = "Weekly Deaths", xaxis = list(title = ""), yaxis = list(title = "Deaths"))
      
    } else if (input$temp_agg == "month") {
      req(agg_data$monthly)
      m <- agg_data$monthly[DeathYear >= input$temp_years[1] & DeathYear <= input$temp_years[2]]
      m[, date := as.Date(paste0(DeathYear, "-", DeathMonth, "-01"))]
      
      plot_ly(m, x = ~date, y = ~deaths, type = "bar") %>%
        plotly::layout(title = "Monthly Deaths", xaxis = list(title = ""), yaxis = list(title = "Deaths"))
      
    } else {
      y <- d[, .(deaths = sum(deaths, na.rm = TRUE)), by = epi_year]
      
      plot_ly(y, x = ~epi_year, y = ~deaths, type = "bar") %>%
        plotly::layout(title = "Yearly Deaths", xaxis = list(title = "Year"), yaxis = list(title = "Deaths"))
    }
  })
  
  # ---- Cause Codes Tab ----
  
  output$top_causes_plot <- renderPlotly({
    req(agg_data$cause_year)
    
    d <- agg_data$cause_year[
      epi_year >= input$cause_years[1] & epi_year <= input$cause_years[2]
    ][, .(deaths = sum(deaths, na.rm = TRUE)), by = UnderlyingCause]
    
    d <- d[order(-deaths)][1:min(input$top_n_causes, nrow(d))]
    d[, UnderlyingCause := factor(UnderlyingCause, levels = rev(UnderlyingCause))]
    
    plot_ly(d, y = ~UnderlyingCause, x = ~deaths, type = "bar", orientation = "h",
            marker = list(color = "#3498db")) %>%
      plotly::layout(
        title = paste("Top", input$top_n_causes, "Causes"),
        xaxis = list(title = "Deaths"),
        yaxis = list(title = ""),
        margin = list(l = 100)
      )
  })
  
  output$cause_trends_plot <- renderPlotly({
    req(agg_data$cause_year)
    
    # Get top 10 causes
    top_causes <- agg_data$cause_year[
      epi_year >= input$cause_years[1] & epi_year <= input$cause_years[2]
    ][, .(deaths = sum(deaths)), by = UnderlyingCause][order(-deaths)][1:10]$UnderlyingCause
    
    d <- agg_data$cause_year[
      UnderlyingCause %in% top_causes &
      epi_year >= input$cause_years[1] & epi_year <= input$cause_years[2]
    ]
    
    plot_ly(d, x = ~epi_year, y = ~deaths, color = ~UnderlyingCause,
            type = "scatter", mode = "lines") %>%
      plotly::layout(
        title = "Top 10 Cause Trends",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Deaths"),
        legend = list(orientation = "v", x = 1.02, y = 1)
      )
  })
  
  # ---- Demographics Tab ----
  
  output$age_dist_plot <- renderPlotly({
    req(agg_data$age_sex_year)
    
    d <- agg_data$age_sex_year[
      epi_year >= input$demo_years[1] & epi_year <= input$demo_years[2]
    ][, .(deaths = sum(deaths, na.rm = TRUE)), by = age]
    
    plot_ly(d, x = ~age, y = ~deaths, type = "bar",
            marker = list(color = "#9b59b6")) %>%
      plotly::layout(
        title = "Age at Death",
        xaxis = list(title = "Age"),
        yaxis = list(title = "Deaths")
      )
  })
  
  output$sex_dist_plot <- renderPlotly({
    req(agg_data$age_sex_year)
    
    d <- agg_data$age_sex_year[
      epi_year >= input$demo_years[1] & epi_year <= input$demo_years[2]
    ][, .(deaths = sum(deaths, na.rm = TRUE)), by = Sex]
    
    d[, sex_label := ifelse(Sex == 1 | Sex == "Male", "Male", "Female")]
    
    plot_ly(d, labels = ~sex_label, values = ~deaths, type = "pie",
            marker = list(colors = c("#3498db", "#e74c3c"))) %>%
      plotly::layout(title = "Sex Distribution")
  })
  
  output$pyramid_plot <- renderPlotly({
    req(agg_data$pyramid_data)
    
    d <- agg_data$pyramid_data[
      epi_year >= input$demo_years[1] & epi_year <= input$demo_years[2]
    ]
    
    if (input$demo_province != "all") {
      d <- d[DeathProvince == input$demo_province]
    }
    
    d <- d[, .(deaths = sum(deaths, na.rm = TRUE)), by = .(agegroup5, Sex)]
    d[, sex_label := ifelse(Sex == 1 | Sex == "Male", "Male", "Female")]
    
    # Make male negative for pyramid
    d[sex_label == "Male", deaths := -deaths]
    
    plot_ly() %>%
      add_bars(data = d[sex_label == "Male"], 
               y = ~agegroup5, x = ~deaths, name = "Male",
               orientation = "h", marker = list(color = "#3498db")) %>%
      add_bars(data = d[sex_label == "Female"],
               y = ~agegroup5, x = ~deaths, name = "Female", 
               orientation = "h", marker = list(color = "#e74c3c")) %>%
      plotly::layout(
        title = "Population Pyramid",
        barmode = "overlay",
        xaxis = list(title = "Deaths", 
                     tickvals = seq(-max(abs(d$deaths)), max(abs(d$deaths)), length.out = 5),
                     ticktext = abs(seq(-max(abs(d$deaths)), max(abs(d$deaths)), length.out = 5))),
        yaxis = list(title = "Age Group")
      )
  })
  
  # ---- Geographic Tab ----
  
  output$geo_choropleth <- renderPlotly({
    req(shapes)
    req(shapes$provinces)
    
    if (input$geo_level == "province") {
      req(agg_data$province_year)
      
      d <- agg_data$province_year[
        epi_year >= input$geo_years[1] & epi_year <= input$geo_years[2]
      ][, .(deaths = sum(deaths, na.rm = TRUE)), by = DeathProvince]
      
      # Map province codes - handle both full names and codes
      d[, prov_code := prov_code_map[DeathProvince]]
      d[is.na(prov_code), prov_code := DeathProvince]
      
      # Also try matching directly if province names are already codes
      if (all(is.na(d$prov_code))) {
        d$prov_code <- d$DeathProvince
      }
      
      # Join with shapes
      map_data <- shapes$provinces
      map_data <- left_join(map_data, as.data.frame(d), by = c("prov" = "prov_code"))
      
      # Add population for rates
      if (input$geo_metric == "rate" && !is.null(pop_data) && !is.null(pop_data$province)) {
        pop_sum <- pop_data$province[
          epi_year >= input$geo_years[1] & epi_year <= input$geo_years[2]
        ][, .(Population = sum(Population, na.rm = TRUE)), by = DeathProvince]
        pop_sum[, prov_code := prov_code_map[DeathProvince]]
        pop_sum[is.na(prov_code), prov_code := DeathProvince]
        
        map_data <- left_join(map_data, as.data.frame(pop_sum[, .(prov_code, Population)]), 
                              by = c("prov" = "prov_code"))
        map_data$rate <- map_data$deaths / map_data$Population * 100000
        map_data$display <- round(map_data$rate, 1)
        fill_col <- "rate"
        legend_title <- "Deaths/100k"
      } else {
        map_data$display <- map_data$deaths
        fill_col <- "deaths"
        legend_title <- "Deaths"
      }
      
      # Create ggplot then convert to plotly
      p <- ggplot(map_data) +
        geom_sf(aes(fill = .data[[fill_col]], 
                    text = paste(prov, "\n", format(display, big.mark = ","))),
                color = "white", size = 0.5) +
        scale_fill_viridis_c(option = "plasma", name = legend_title) +
        theme_void() +
        labs(title = "Deaths by Province")
      
      ggplotly(p, tooltip = "text")
      
    } else {
      req(agg_data$district_year)
      req(shapes$districts)
      
      d <- agg_data$district_year[
        epi_year >= input$geo_years[1] & epi_year <= input$geo_years[2]
      ][, .(deaths = sum(deaths, na.rm = TRUE)), by = deathdistrictname]
      
      map_data <- shapes$districts
      map_data <- left_join(map_data, as.data.frame(d), by = c("district_standard" = "deathdistrictname"))
      
      if (input$geo_metric == "rate" && !is.null(pop_data) && !is.null(pop_data$district)) {
        pop_sum <- pop_data$district[
          epi_year >= input$geo_years[1] & epi_year <= input$geo_years[2]
        ][, .(Population = sum(Population, na.rm = TRUE)), by = deathdistrictname]
        
        map_data <- left_join(map_data, as.data.frame(pop_sum), 
                              by = c("district_standard" = "deathdistrictname"))
        map_data$rate <- map_data$deaths / map_data$Population * 100000
        map_data$display <- round(map_data$rate, 1)
        fill_col <- "rate"
        legend_title <- "Deaths/100k"
      } else {
        map_data$display <- map_data$deaths
        fill_col <- "deaths"
        legend_title <- "Deaths"
      }
      
      p <- ggplot(map_data) +
        geom_sf(aes(fill = .data[[fill_col]],
                    text = paste(district_standard, "\n", format(display, big.mark = ","))),
                color = "white", size = 0.2) +
        scale_fill_viridis_c(option = "plasma", name = legend_title, na.value = "grey80") +
        theme_void() +
        labs(title = "Deaths by District")
      
      ggplotly(p, tooltip = "text")
    }
  })
  
  output$geo_trends <- renderPlotly({
    req(agg_data$province_year)
    
    d <- agg_data$province_year[
      epi_year >= input$geo_years[1] & epi_year <= input$geo_years[2]
    ]
    
    plot_ly(d, x = ~epi_year, y = ~deaths, color = ~DeathProvince,
            type = "scatter", mode = "lines") %>%
      plotly::layout(
        title = "Provincial Trends",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Deaths"),
        legend = list(orientation = "v", x = 1.02, y = 1)
      )
  })
}

# Run the app
shinyApp(ui, server)
