# =============================================================================
# MRC VR Data Explorer Shiny App
# Interactive exploration of South African mortality data (1997-2022)
# =============================================================================

library(shiny)
library(bslib)
library(arrow)
library(data.table)
library(plotly)
library(DT)
library(labelled)

# =============================================================================
# DATA LOADING
# =============================================================================

# Load the data efficiently using arrow/feather
load_data <- function() {
  data_path <- here::here("Deaths2022_MRCversionFINAL.feather")
  
  if (!file.exists(data_path)) {
    # Try alternative path
    data_path <- "../../Deaths2022_MRCversionFINAL.feather"
  }
  
  dt <- as.data.table(read_feather(data_path))
  
  # Extract labels for provinces before conversion
  province_labels <- val_labels(dt$DeathProvince)
  sex_labels <- val_labels(dt$Sex)
  death_type_labels <- val_labels(dt$DeathType)
  nat_unnat_labels <- val_labels(dt$NaturalUnnatural)
  
  # Convert labelled columns to character for readability
  dt[, DeathProvinceName := fifelse(
    DeathProvince %in% names(province_labels),
    names(province_labels)[match(DeathProvince, province_labels)],
    as.character(DeathProvince)
  )]
  
  dt[, SexName := fifelse(
    Sex %in% names(sex_labels),
    names(sex_labels)[match(Sex, sex_labels)],
    as.character(Sex)
  )]
  
  dt[, DeathTypeName := fifelse(
    DeathType %in% names(death_type_labels),
    names(death_type_labels)[match(DeathType, death_type_labels)],
    as.character(DeathType)
  )]
  
  dt[, NaturalUnnatName := fifelse(
    NaturalUnnatural %in% names(nat_unnat_labels),
    names(nat_unnat_labels)[match(NaturalUnnatural, nat_unnat_labels)],
    as.character(NaturalUnnatural)
  )]
  
  # Create age groups
  dt[, agegroup := cut(age, 
                       breaks = c(0, 1, 5, 15, 25, 35, 45, 55, 65, 75, 85, Inf),
                       right = FALSE,
                       labels = c("0", "1-4", "5-14", "15-24", "25-34", 
                                  "35-44", "45-54", "55-64", "65-74", "75-84", "85+"))]
  
  # Ensure epi_year is numeric
  dt[, epi_year := as.numeric(epi_year)]
  
  return(dt)
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
        plotlyOutput("overview_time_plot", height = "400px")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Deaths by Province"),
          plotlyOutput("overview_province_plot", height = "350px")
        ),
        card(
          card_header("Deaths by Age Group"),
          plotlyOutput("overview_age_plot", height = "350px")
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
                    min = 1997, max = 2022,
                    value = c(2014, 2022),
                    step = 1, sep = ""),
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
        plotlyOutput("temporal_plot", height = "500px")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Seasonality Heatmap"),
          plotlyOutput("temporal_heatmap", height = "400px")
        ),
        card(
          card_header("Year-over-Year Comparison"),
          plotlyOutput("temporal_yoy", height = "400px")
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
                    min = 1997, max = 2022,
                    value = c(2020, 2022),
                    step = 1, sep = ""),
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
        plotlyOutput("cause_top_plot", height = "500px")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Causes Over Time"),
          plotlyOutput("cause_time_plot", height = "400px")
        ),
        card(
          card_header("Cause Distribution by Province"),
          plotlyOutput("cause_province_plot", height = "400px")
        )
      ),
      card(
        card_header("Cause Code Details"),
        DTOutput("cause_table")
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
                    value = c(2020, 2022),
                    step = 1, sep = ""),
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
          plotlyOutput("demo_age_hist", height = "350px")
        ),
        card(
          card_header("Sex Distribution"),
          plotlyOutput("demo_sex_plot", height = "350px")
        )
      ),
      card(
        card_header("Age-Sex Pyramid"),
        plotlyOutput("demo_pyramid", height = "500px")
      ),
      card(
        card_header("Registration Delay Analysis"),
        plotlyOutput("demo_reg_delay", height = "400px")
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
                    value = c(2020, 2022),
                    step = 1, sep = ""),
        selectInput("geo_agegroup", "Age Group:",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE),
        selectInput("geo_cause", "Underlying Cause (ICD-10):",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE)
      ),
      card(
        card_header("Deaths by Province"),
        plotlyOutput("geo_province_bar", height = "400px")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Deaths by District"),
          plotlyOutput("geo_district_plot", height = "400px")
        ),
        card(
          card_header("Province Trends Over Time"),
          plotlyOutput("geo_province_time", height = "400px")
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
        DTOutput("raw_data_table")
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
## MRC VR Data Explorer

This Shiny application provides interactive exploration of South African vital registration mortality data from 1997-2022.

### Data Source
- **Deaths2022_MRCversionFINAL.feather**: Contains over 13 million death records
- Data includes information on demographics, cause of death, geographic location, and registration timing

### Features
- **Overview**: High-level summary statistics and trends
- **Temporal Trends**: Weekly, monthly, and yearly patterns
- **Cause Codes**: Analysis of underlying causes of death (ICD-10)
- **Demographics**: Age and sex distribution analysis
- **Geographic**: Provincial and district-level analysis
- **Raw Data**: Browse and download filtered data samples

### Notes
- Visualizations are designed for rapid data exploration and quality checks
- Large dataset - some operations may take a few seconds
- Use filters to focus on specific subsets of interest
      ")
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
  
  # Load data once at startup
  dt <- load_data()
  
  # Get unique values for filters
  provinces <- sort(unique(dt$DeathProvinceName[!is.na(dt$DeathProvinceName)]))
  agegroups <- levels(dt$agegroup)
  causes <- sort(unique(dt$UnderlyingCause[!is.na(dt$UnderlyingCause) & dt$UnderlyingCause != ""]))
  
  # Update selectInputs with data values
  updateSelectInput(session, "overview_province", choices = c("All" = "", provinces), selected = "")
  updateSelectInput(session, "temporal_province", choices = c("All" = "", provinces), selected = "")
  updateSelectInput(session, "temporal_agegroup", choices = c("All" = "", agegroups), selected = "")
  updateSelectInput(session, "cause_province", choices = c("All" = "", provinces), selected = "")
  updateSelectInput(session, "cause_agegroup", choices = c("All" = "", agegroups), selected = "")
  updateSelectInput(session, "demo_province", choices = c("All" = "", provinces), selected = "")
  updateSelectInput(session, "geo_agegroup", choices = c("All" = "", agegroups), selected = "")
  updateSelectInput(session, "geo_cause", choices = c("All" = "", causes[1:100]), selected = "")  # Limit for performance
  updateSelectInput(session, "raw_province", choices = c("All" = "", provinces), selected = "")
  
  # ==========================================================================
  # OVERVIEW TAB
  # ==========================================================================
  
  overview_data <- reactive({
    d <- dt[epi_year >= input$overview_years[1] & epi_year <= input$overview_years[2]]
    
    if (length(input$overview_province) > 0 && !("" %in% input$overview_province)) {
      d <- d[DeathProvinceName %in% input$overview_province]
    }
    
    if (input$overview_death_type != "all") {
      d <- d[DeathType == as.numeric(input$overview_death_type)]
    }
    
    if (input$overview_nat_unnat != "all") {
      d <- d[NaturalUnnatural == as.numeric(input$overview_nat_unnat)]
    }
    
    d
  })
  
  output$overview_time_plot <- renderPlotly({
    d <- overview_data()[, .(count = .N), by = .(epi_year, epi_week)]
    d <- d[order(epi_year, epi_week)]
    d[, date_approx := as.Date(paste0(epi_year, "-01-01")) + (epi_week - 1) * 7]
    
    p <- plot_ly(d, x = ~date_approx, y = ~count, type = 'scatter', mode = 'lines',
                 line = list(color = '#0d6efd', width = 1),
                 hovertemplate = "Week: %{x}<br>Deaths: %{y}<extra></extra>") %>%
      layout(
        title = "",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Deaths per Week"),
        hovermode = "x unified"
      )
    p
  })
  
  output$overview_province_plot <- renderPlotly({
    d <- overview_data()[, .(count = .N), by = .(DeathProvinceName)]
    d <- d[!is.na(DeathProvinceName)][order(-count)]
    
    plot_ly(d, x = ~reorder(DeathProvinceName, count), y = ~count, type = 'bar',
            marker = list(color = '#0d6efd'),
            hovertemplate = "%{x}<br>Deaths: %{y}<extra></extra>") %>%
      layout(
        title = "",
        xaxis = list(title = "", tickangle = -45),
        yaxis = list(title = "Total Deaths")
      )
  })
  
  output$overview_age_plot <- renderPlotly({
    d <- overview_data()[, .(count = .N), by = .(agegroup)]
    d <- d[!is.na(agegroup)]
    
    plot_ly(d, x = ~agegroup, y = ~count, type = 'bar',
            marker = list(color = '#198754'),
            hovertemplate = "Age: %{x}<br>Deaths: %{y}<extra></extra>") %>%
      layout(
        title = "",
        xaxis = list(title = "Age Group"),
        yaxis = list(title = "Total Deaths")
      )
  })
  
  output$overview_summary <- renderPrint({
    d <- overview_data()
    cat("Data Summary\n")
    cat("============\n\n")
    cat("Total deaths:", format(nrow(d), big.mark = ","), "\n")
    cat("Year range:", min(d$epi_year, na.rm = TRUE), "-", max(d$epi_year, na.rm = TRUE), "\n")
    cat("Provinces:", length(unique(d$DeathProvinceName[!is.na(d$DeathProvinceName)])), "\n")
    cat("Mean age at death:", round(mean(d$age, na.rm = TRUE), 1), "years\n")
    cat("Median age at death:", round(median(d$age, na.rm = TRUE), 1), "years\n")
    cat("\nSex distribution:\n")
    print(d[, .N, by = SexName][order(-N)])
  })
  
  # ==========================================================================
  # TEMPORAL TRENDS TAB
  # ==========================================================================
  
  temporal_data <- reactive({
    d <- dt[epi_year >= input$temporal_years[1] & epi_year <= input$temporal_years[2]]
    
    if (length(input$temporal_province) > 0 && !("" %in% input$temporal_province)) {
      d <- d[DeathProvinceName %in% input$temporal_province]
    }
    
    if (length(input$temporal_agegroup) > 0 && !("" %in% input$temporal_agegroup)) {
      d <- d[agegroup %in% input$temporal_agegroup]
    }
    
    d
  })
  
  output$temporal_plot <- renderPlotly({
    d <- temporal_data()
    
    if (input$temporal_aggregation == "week") {
      agg <- d[, .(count = .N), by = .(epi_year, epi_week)]
      agg[, date_approx := as.Date(paste0(epi_year, "-01-01")) + (epi_week - 1) * 7]
      
      if (input$temporal_compare_years) {
        p <- plot_ly()
        for (yr in unique(agg$epi_year)) {
          yr_data <- agg[epi_year == yr]
          p <- add_trace(p, data = yr_data, x = ~epi_week, y = ~count, 
                         type = 'scatter', mode = 'lines', name = as.character(yr))
        }
        p <- p %>% layout(xaxis = list(title = "Epidemiological Week"),
                          yaxis = list(title = "Deaths"))
      } else {
        agg <- agg[order(date_approx)]
        p <- plot_ly(agg, x = ~date_approx, y = ~count, type = 'scatter', mode = 'lines',
                     line = list(color = '#0d6efd')) %>%
          layout(xaxis = list(title = "Date"), yaxis = list(title = "Deaths per Week"))
      }
      
    } else if (input$temporal_aggregation == "month") {
      agg <- d[, .(count = .N), by = .(epi_year, DeathMonth)]
      agg[, date_approx := as.Date(paste0(epi_year, "-", DeathMonth, "-01"))]
      agg <- agg[order(date_approx)]
      
      p <- plot_ly(agg, x = ~date_approx, y = ~count, type = 'scatter', mode = 'lines+markers',
                   line = list(color = '#0d6efd')) %>%
        layout(xaxis = list(title = "Date"), yaxis = list(title = "Deaths per Month"))
      
    } else {
      agg <- d[, .(count = .N), by = .(epi_year)]
      agg <- agg[order(epi_year)]
      
      p <- plot_ly(agg, x = ~epi_year, y = ~count, type = 'bar',
                   marker = list(color = '#0d6efd')) %>%
        layout(xaxis = list(title = "Year"), yaxis = list(title = "Deaths per Year"))
    }
    
    p
  })
  
  output$temporal_heatmap <- renderPlotly({
    d <- temporal_data()[, .(count = .N), by = .(epi_year, epi_week)]
    
    # Pivot for heatmap
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
    d <- temporal_data()[, .(count = .N), by = .(epi_year)]
    d <- d[order(epi_year)]
    d[, pct_change := (count - shift(count)) / shift(count) * 100]
    
    plot_ly(d[!is.na(pct_change)], x = ~epi_year, y = ~pct_change, type = 'bar',
            marker = list(color = ~ifelse(pct_change >= 0, '#dc3545', '#198754')),
            hovertemplate = "Year: %{x}<br>Change: %{y:.1f}%<extra></extra>") %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "% Change from Previous Year"))
  })
  
  # ==========================================================================
  # CAUSE CODES TAB
  # ==========================================================================
  
  cause_data <- reactive({
    d <- dt[epi_year >= input$cause_years[1] & epi_year <= input$cause_years[2]]
    
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
  })
  
  output$cause_top_plot <- renderPlotly({
    d <- cause_data()[!is.na(UnderlyingCause) & UnderlyingCause != "", 
                      .(count = .N), by = .(UnderlyingCause)]
    d <- d[order(-count)][1:min(input$cause_top_n, nrow(d))]
    
    plot_ly(d, y = ~reorder(UnderlyingCause, count), x = ~count, type = 'bar',
            orientation = 'h',
            marker = list(color = '#0d6efd'),
            hovertemplate = "Code: %{y}<br>Deaths: %{x}<extra></extra>") %>%
      layout(xaxis = list(title = "Number of Deaths"),
             yaxis = list(title = ""))
  })
  
  output$cause_time_plot <- renderPlotly({
    # Get top 10 causes
    top_causes <- cause_data()[!is.na(UnderlyingCause) & UnderlyingCause != "", 
                              .(count = .N), by = .(UnderlyingCause)][order(-count)][1:10, UnderlyingCause]
    
    d <- cause_data()[UnderlyingCause %in% top_causes, .(count = .N), by = .(epi_year, UnderlyingCause)]
    
    plot_ly(d, x = ~epi_year, y = ~count, color = ~UnderlyingCause, type = 'scatter', mode = 'lines+markers') %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "Deaths"),
             legend = list(orientation = "h", y = -0.2))
  })
  
  output$cause_province_plot <- renderPlotly({
    # Get top 5 causes for the selected data
    top_causes <- cause_data()[!is.na(UnderlyingCause) & UnderlyingCause != "", 
                              .(count = .N), by = .(UnderlyingCause)][order(-count)][1:5, UnderlyingCause]
    
    d <- cause_data()[UnderlyingCause %in% top_causes & !is.na(DeathProvinceName), 
                      .(count = .N), by = .(DeathProvinceName, UnderlyingCause)]
    
    plot_ly(d, x = ~DeathProvinceName, y = ~count, color = ~UnderlyingCause, type = 'bar') %>%
      layout(xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = "Deaths"),
             barmode = 'stack')
  })
  
  output$cause_table <- renderDT({
    d <- cause_data()[!is.na(UnderlyingCause) & UnderlyingCause != "", 
                      .(Deaths = .N, 
                        Mean_Age = round(mean(age, na.rm = TRUE), 1),
                        Pct_Male = round(100 * sum(SexName == "Male", na.rm = TRUE) / .N, 1)), 
                      by = .(UnderlyingCause)]
    d <- d[order(-Deaths)]
    d[, Percentage := round(100 * Deaths / sum(Deaths), 2)]
    
    datatable(d, options = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE)
  })
  
  # ==========================================================================
  # DEMOGRAPHICS TAB
  # ==========================================================================
  
  demo_data <- reactive({
    d <- dt[epi_year >= input$demo_years[1] & epi_year <= input$demo_years[2]]
    
    if (length(input$demo_province) > 0 && !("" %in% input$demo_province)) {
      d <- d[DeathProvinceName %in% input$demo_province]
    }
    
    if (input$demo_nat_unnat != "all") {
      d <- d[NaturalUnnatural == as.numeric(input$demo_nat_unnat)]
    }
    
    d
  })
  
  output$demo_age_hist <- renderPlotly({
    d <- demo_data()[!is.na(age)]
    
    plot_ly(d, x = ~age, type = 'histogram',
            marker = list(color = '#0d6efd'),
            nbinsx = 100) %>%
      layout(xaxis = list(title = "Age at Death"),
             yaxis = list(title = "Count"))
  })
  
  output$demo_sex_plot <- renderPlotly({
    d <- demo_data()[, .(count = .N), by = .(SexName)]
    d <- d[!is.na(SexName)]
    
    plot_ly(d, labels = ~SexName, values = ~count, type = 'pie',
            marker = list(colors = c('#0d6efd', '#dc3545', '#6c757d', '#ffc107'))) %>%
      layout(title = "")
  })
  
  output$demo_pyramid <- renderPlotly({
    d <- demo_data()[SexName %in% c("Male", "Female") & !is.na(agegroup), 
                     .(count = .N), by = .(agegroup, SexName)]
    
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
                          categoryarray = levels(dt$agegroup)))
  })
  
  output$demo_reg_delay <- renderPlotly({
    d <- demo_data()[!is.na(RegistrationYear) & !is.na(DeathYear)]
    d[, reg_delay := as.numeric(RegistrationYear) - as.numeric(DeathYear)]
    d <- d[reg_delay >= 0 & reg_delay <= 10]
    
    delay_summary <- d[, .(count = .N), by = .(reg_delay)]
    
    plot_ly(delay_summary, x = ~reg_delay, y = ~count, type = 'bar',
            marker = list(color = '#198754'),
            hovertemplate = "Delay: %{x} years<br>Deaths: %{y}<extra></extra>") %>%
      layout(xaxis = list(title = "Registration Delay (Years)", dtick = 1),
             yaxis = list(title = "Number of Deaths"))
  })
  
  # ==========================================================================
  # GEOGRAPHIC TAB
  # ==========================================================================
  
  geo_data <- reactive({
    d <- dt[epi_year >= input$geo_years[1] & epi_year <= input$geo_years[2]]
    
    if (length(input$geo_agegroup) > 0 && !("" %in% input$geo_agegroup)) {
      d <- d[agegroup %in% input$geo_agegroup]
    }
    
    if (length(input$geo_cause) > 0 && !("" %in% input$geo_cause)) {
      d <- d[UnderlyingCause %in% input$geo_cause]
    }
    
    d
  })
  
  output$geo_province_bar <- renderPlotly({
    d <- geo_data()[!is.na(DeathProvinceName), .(count = .N), by = .(DeathProvinceName)]
    d <- d[order(-count)]
    
    plot_ly(d, x = ~reorder(DeathProvinceName, count), y = ~count, type = 'bar',
            marker = list(color = '#0d6efd'),
            hovertemplate = "%{x}<br>Deaths: %{y}<extra></extra>") %>%
      layout(xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = "Total Deaths"))
  })
  
  output$geo_district_plot <- renderPlotly({
    d <- geo_data()[!is.na(deathdistrictname), .(count = .N), by = .(deathdistrictname)]
    d <- d[order(-count)][1:20]
    
    plot_ly(d, y = ~reorder(deathdistrictname, count), x = ~count, type = 'bar',
            orientation = 'h',
            marker = list(color = '#198754'),
            hovertemplate = "%{y}<br>Deaths: %{x}<extra></extra>") %>%
      layout(xaxis = list(title = "Deaths"),
             yaxis = list(title = ""))
  })
  
  output$geo_province_time <- renderPlotly({
    d <- geo_data()[!is.na(DeathProvinceName), .(count = .N), by = .(epi_year, DeathProvinceName)]
    
    plot_ly(d, x = ~epi_year, y = ~count, color = ~DeathProvinceName, 
            type = 'scatter', mode = 'lines+markers') %>%
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "Deaths"),
             legend = list(orientation = "h", y = -0.2))
  })
  
  # ==========================================================================
  # RAW DATA TAB
  # ==========================================================================
  
  raw_data <- reactive({
    d <- dt[epi_year == input$raw_years]
    
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
          DeathTypeName, NaturalUnnatName,
          RegistrationYear, RegistrationMonth)]
  })
  
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
