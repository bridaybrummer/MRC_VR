# Get an estimate of the number of HIV aids deaths in SA per province acorss ages from TEMBISA model 

#read in excel 
library(NMCleaner)
conflicted::conflicts_prefer(dplyr::select)

readxl::read_excel("ME_dashboard/Age-specificOutputs4.8_final2.xlsx", sheet = "SA") -> tembisa_SA

tembisa_SA %>%rename( 
    "stat"= 1
)-> tembisa_SA

tembisa_SA%>%setDT()

tembisa_SA%>%
filter( 
 grepl("death", stat) 
)-> tembisa_SA_deaths

tembisa_SA_deaths$stat %>%unique

tembisa_SA%>%
    mutate( 
        stat_1 = as.numeric(stat), 
        stat= if_else( !is.na(stat_1), NA_character_, stat  )
        )%>%
        # fill the string down 
        fill( stat, .direction = "down")%>%
        mutate( 
            stat_1 = if_else( grepl("year of", stat), -1, stat_1 )
        )%>%
        filter( !is.na(stat_1))%>%
        select( stat_1, stat, everything() )%>%
        rename( 
            age = stat_1
        ) -> tembisa_SA

tembisa_SA_deaths%>%
select( 
    stat
)%>%unique() %>%pull() -> deaths_rows

which( tembisa_SA$stat %in% deaths_rows)-> rows_of_interest
min(rows_of_interest)-> min
max(rows_of_interest) -> max
max(diff(which( tembisa_SA$stat %in% deaths_rows)))


#tembisa_SA[c(min:max)] -> tembisa_SA_deaths

# pivot longer in data.table
tembisa_SA[rows_of_interest, ]-> 
tembisa_SA_deaths

tembisa_SA_deaths%>%
    melt( 
        id.vars = c("stat", "age"), 
        variable.name = "year", 
        value.name = "deaths"
    )-> tembisa_SA_deaths_long

    tembisa_SA_deaths_long$stat%>%unique

    tembisa_SA_deaths_long


tembisa_SA_deaths_long[, 
.(deaths = sum(deaths, na.rm = TRUE)), by=( year)
    ]-> dt_tembi
    
    ggplot(dt_tembi) + 
    geom_line( 
        aes( 
            x =year, 
            y= deaths,
            group = 1
        )
    )+ scale_y_continuous(labels = scales::comma)

# pull in the SA estimate of B code deaths (HIV)
library(arrow)
dt <- read_feather("LGH_MasterFile_preCollapsedAll.feather") %>% as.data.table()

dt_total_deaths <- dt[ , .(deaths = .N), by = .(epi_year)]
#glimpse(dt)

dt$epi_year%>%unique

dt[, 
    hiv_pos := fifelse( LGH_Cause %in% c("B33"), TRUE, FALSE )
    ][, 
    year := epi_year][
        hiv_pos==TRUE, 
        .(deaths = .N), 
        by = .(year)] -> 
        dt_vr

                ggplot(dt_vr) +
                    geom_line(
                        aes(
                            x = year,
                            y = deaths,
                            group = 1
                        )
                    )

#glimpse(dt)


# NCODV glimpse

NCODV_proportion <- 0.23
N_deaths_2017 <- dt_total_deaths[epi_year == 2017, deaths]
corrected_hiv_2017 <- N_deaths_2017 * NCODV_proportion

N_deaths_2018 <- dt_total_deaths[epi_year == 2018, deaths]
corrected_hiv_2018 <- N_deaths_2018 * NCODV_proportion

tibble( 
    source = c("NCODV","NCODV"),
    year = c( 2017, 2018), 
    deaths = c(corrected_hiv_2017, corrected_hiv_2018)
)-> 
dt_ncodv
    

rbind( 
dt_tembi[, source := "Tembisa"], 
dt_vr[, source := "VR data"]
)%>%
rbind(
    ., 
    dt_ncodv) -> dt_hiv

dt_hiv[year %in% 1997:2025]%>%

    ggplot()+ 
    geom_line( 
        aes(
            x = year, 
            y = deaths, 
            group = source, 
            color = source
        )
    )



# Tuberculosis deaths 

# TB in VR data 
dt[
    ,
    tb_pos := fifelse(UnderlyingCause %in% c("A15", "A16", "A17", "A18", "A19"), TRUE, FALSE)
][
    ,
    year := epi_year
][
    tb_pos == TRUE,
    .(deaths = .N),
    by = .(year)
] -> dt_tb_vr

dt_tb_vr %>%
    ggplot() +
    geom_line(
        aes(
            x = year,
            y = deaths,
            group = 1
        )
    )

# TB in tembisa 
readxl::read_excel("ME_dashboard/TBoutput2.1.xlsx", sheet = "SA") -> tembisa_SA_TB

tembisa_SA_TB%>%
    dplyr::select( - 2) %>%
rename( 
    stat = 1
)%>%
    filter( 
grepl("Total TB deaths in adults", ignore.case = TRUE, stat)
    )%>%
    pivot_longer( 
        cols = -stat, 
        names_to = "year", 
        values_to = "deaths"
    ) %>%dplyr::select( 
        year, deaths)%>%as.data.table() -> tembisa_SA_TB_long

    tembisa_SA_TB_long

# TB from NMC
new_master <- arrow::read_feather("~/Desktop/SAFETP/CLA/NMC_database/master/new_master.feather")
new_master %>% setDT()

new_master$patient_vital_status %>% unique()

new_master[grepl("tuber", condition, ignore.case = TRUE) 
& patient_vital_status %in% "Deceased"
, ][
    ,
    .(deaths = .N),
    by = .(year)
]-> nmc_SA_TB_deaths

new_master[grepl("tuber", condition, ignore.case = TRUE) 
    , ][
    ,
    .(deaths = .N*0.2),
    by = .(year)
] -> nmc_SA_TB_deaths_from_CFR

    rbind( 
        dt_tb_vr[, source := "VR data"], 
        tembisa_SA_TB_long[, source := "Tembisa"], 
        nmc_SA_TB_deaths[, source:= "NMC"], 
        nmc_SA_TB_deaths_from_CFR[, source:= "NMC (from CFR)"]

    )%>% 
    ggplot() + 
    geom_line( 
        aes( 
            x = year, 
            y = deaths, 
            group = source, 
            color = source
        )
    )



# ============================================================================
# POISONING DEATHS
# ============================================================================

# Poisoning deaths from NMC (Agricultural or stock remedy poisoning)
new_master[
    grepl("agric", ignore.case = TRUE, condition), 
][
    ,
    .(cases = .N, 
       deaths = sum( patient_vital_status %in% "Deceased", na.rm=TRUE)),
    by = .(year)
] -> nmc_SA_poisoning_agric

# Poisoning deaths from VR data - limited to pesticide-poisoning codes only
# (X48 accidental, X68 intentional self-poisoning, X87 assault, Y18 undetermined
# intent), so the VR comparison matches NMC's agricultural/pesticide scope.
dt[
    , 
    poison_pos := fifelse(
        UnderlyingCause %in% c("X48", "X68", "X87", "Y18"), 
        TRUE, FALSE
    )
][
    poison_pos == TRUE,
    .(deaths = .N),
    by = .(epi_year)
] -> dt_poisoning_vr

# Rename for consistency
setnames(dt_poisoning_vr, "epi_year", "year")
setnames(nmc_SA_poisoning_agric, "deaths", "nmc_deaths")

# Unnatural deaths aggregate from VR (all external causes: V, W, X, Y codes)
dt[
    grepl("^[VWXY]", UnderlyingCause), 
    .(unnatural_deaths = .N),
    by = .(epi_year)
] -> dt_unnatural_vr

setnames(dt_unnatural_vr, "epi_year", "year")

# make some plotly type of plot to look at the garbage codes over time by province or by facility level 
# WHat kind fo decision do we want to make 
 # - identify facility level/ province with highest proportion of junk codes for targetted training. 


dt$DeathInst%>%unique()-> unique_labels


# label from a stata label object 

stata_labels <- haven::print_labels(dt$DeathInst)

dt[
    ,
    Death_instance := haven::as_factor(DeathInst, levels = "labels")
]


dt$epi_year %>% tabyl()
dt$DeathProvince %>% tabyl()
dt$Death_instance %>% tabyl()
dt$garbage_flag_label %>% tabyl()

dt[
    , 
    .(n = .N), 
    by = .(epi_year, DeathProvince, garbage_flag_label)
][
    , 
    proportion := n / sum(n), 
    by = .(epi_year, DeathProvince)
]-> dt_garbage_summary_by_province




# perc garbage code by province 
dt_garbage_summary_by_province[
    garbage_flag_label == "Not garbage",
    ]%>%
    ggplot()+ 
    geom_line( 
        aes( 
            x = epi_year, 
            y = proportion, 
            group = garbage_flag_label, 
            color = garbage_flag_label
        )
    ) + 
    geom_hline(
        yintercept =  0.8
    )+ 
    scale_y_continuous( 
        labels = scales::percent_format(accuracy = 1), 
        limits = c(0,1)
    ) +
    facet_wrap(~DeathProvince)


# perc garbage code by instance. 
dt[
    ,
    .(n = .N),
    by = .(epi_year, Death_instance, garbage_flag_label)
][
    ,
    proportion := n / sum(n),
    by = .(epi_year, Death_instance)
] -> dt_garbage_summary_by_instance

dt_garbage_summary_by_instance[
    garbage_flag_label == "Not garbage",
    ]%>%
    ggplot()+ 
    geom_line( 
        aes( 
            x = epi_year, 
            y = proportion, 
            group = garbage_flag_label, 
            color = garbage_flag_label
        )
    ) + 
    geom_hline(
        yintercept =  0.8
    )+ 
    scale_y_continuous( 
        labels = scales::percent_format(accuracy = 1), 
        limits = c(0,1)
    ) +
    facet_wrap(~Death_instance)

# ============================================================================
# STYLED PLOTS FOR DASHBOARD
# ============================================================================

library(plotly)

# HIV/AIDS Deaths Plot (including NCODV corrected estimates)
p_hiv <- dt_hiv[year %in% 1997:2025] %>%
  ggplot(aes(x = as.numeric(as.character(year)), y = deaths, 
             color = source, group = source,
             text = paste0("Year: ", year, "<br>",
                          "Deaths: ", scales::comma(round(deaths)), "<br>",
                          "Source: ", source))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c(
    "Tembisa" = "#2E86AB", 
    "VR data" = "#A23B72",
    "NCODV" = "#F77F00"
  )) +
  labs(x = "Year", y = "Deaths", color = "Data Source",
       title = "HIV/AIDS Deaths Comparison",
       subtitle = "Comparing VR, Tembisa model, and NCODV-corrected estimates") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    panel.grid.minor = element_blank()
  )

# TB Deaths Plot (all 4 sources)
dt_tb <- rbind(
  dt_tb_vr[, source := "VR data"],
  tembisa_SA_TB_long[, source := "Tembisa"],
  nmc_SA_TB_deaths[, source := "NMC"],
  nmc_SA_TB_deaths_from_CFR[, source := "NMC (from CFR)"]
)

p_tb <- dt_tb[year %in% 1997:2025] %>%
  ggplot(aes(x = as.numeric(as.character(year)), y = deaths, 
             color = source, group = source,
             text = paste0("Year: ", year, "<br>",
                          "Deaths: ", scales::comma(round(deaths)), "<br>",
                          "Source: ", source))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c(
    "Tembisa" = "#2E86AB", 
    "VR data" = "#A23B72",
    "NMC" = "#F77F00",
    "NMC (from CFR)" = "#06D6A0"
  )) +
  labs(x = "Year", y = "Deaths", color = "Data Source",
       title = "TB Deaths Comparison",
       subtitle = "Comparing VR, Tembisa model, and NMC estimates") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    panel.grid.minor = element_blank()
  )

# Garbage Code Plot by Province
# First, calculate total deaths and proportion of national deaths per province
dt_province_totals <- dt_garbage_summary_by_province[, 
  .(total_n = sum(n)), 
  by = .(epi_year, DeathProvince)
][, 
  national_total := sum(total_n), 
  by = epi_year
][, 
  pct_of_national := total_n / national_total * 100
]

# Join back to get enriched data for plotting
dt_garbage_province_enriched <- dt_garbage_summary_by_province[garbage_flag_label == "Not garbage"] %>%
  merge(dt_province_totals, by = c("epi_year", "DeathProvince"))

p_garbage_province <- dt_garbage_province_enriched %>%
  ggplot(aes(x = epi_year, y = proportion, 
             color = as.factor(DeathProvince), group = DeathProvince,
             text = paste0("Year: ", epi_year, "<br>",
                          "Province: ", DeathProvince, "<br>",
                          "Non-Garbage: ", scales::percent(proportion, accuracy = 0.1), "<br>",
                          "<b>Scale:</b><br>",
                          "Deaths (N): ", scales::comma(total_n), "<br>",
                          "% of National: ", round(pct_of_national, 1), "%"))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  # Standardized data-quality target: 80% non-garbage = 20% garbage upper bound
  # (WHO/GBD usability cut-point; Mathers 2005, Mahapatra 2007, Naghavi 2010)
  geom_hline(yintercept = 0.8, linetype = "dotted", color = "#D32F2F", linewidth = 0.8) +
  annotate("point", x = min(dt_garbage_province_enriched$epi_year),
           y = 0.8, shape = 8, color = "#D32F2F", size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Year", y = "Proportion Non-Garbage", color = "Province",
       title = "Data Quality by Province: Proportion of Non-Garbage Codes",
       subtitle = "Red dotted line / star = 80% target (≤20% garbage; WHO/GBD usability cut-point)") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    panel.grid.minor = element_blank()
  )

# Garbage Code Plot by Death Instance
# First, calculate total deaths and proportion per instance
dt_instance_totals <- dt_garbage_summary_by_instance[, 
  .(total_n = sum(n)), 
  by = .(epi_year, Death_instance)
][, 
  national_total := sum(total_n), 
  by = epi_year
][, 
  pct_of_national := total_n / national_total * 100
]

# Join back to get enriched data for plotting
dt_garbage_instance_enriched <- dt_garbage_summary_by_instance[garbage_flag_label == "Not garbage"] %>%
  merge(dt_instance_totals, by = c("epi_year", "Death_instance"))

p_garbage_instance <- dt_garbage_instance_enriched %>%
    filter( 
    Death_instance %in% c("Hospital", "Home", "Unspecified")
    )%>%
  ggplot(aes(x = epi_year, y = proportion, 
             color = Death_instance, group = Death_instance,
             text = paste0("Year: ", epi_year, "<br>",
                          "Institution: ", Death_instance, "<br>",
                          "Non-Garbage: ", scales::percent(proportion, accuracy = 0.1), "<br>",
                          "<b>Scale:</b><br>",
                          "Deaths (N): ", scales::comma(total_n), "<br>",
                          "% of National: ", round(pct_of_national, 1), "%"))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  # Standardized data-quality target: 80% non-garbage = 20% garbage upper bound
  # (WHO/GBD usability cut-point; Mathers 2005, Mahapatra 2007, Naghavi 2010)
  geom_hline(yintercept = 0.8, linetype = "dotted", color = "#D32F2F", linewidth = 0.8) +
  annotate("point", x = min(dt_garbage_instance_enriched$epi_year),
           y = 0.8, shape = 8, color = "#D32F2F", size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Year", y = "Proportion Non-Garbage", color = "Death Instance",
       title = "Data Quality by Institution: Proportion of Non-Garbage Codes",
       subtitle = "Red dotted line / star = 80% target (≤20% garbage; WHO/GBD usability cut-point)") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    panel.grid.minor = element_blank()
  )

# ============================================================================
# POISONING DEATHS PLOT
# ============================================================================

# Combine poisoning data sources
dt_poisoning <- rbind(
  dt_poisoning_vr[, .(year, deaths, source = "VR data")],
  nmc_SA_poisoning_agric[, .(year, deaths = nmc_deaths, source = "NMC Deaths (Agricultural)")],
  nmc_SA_poisoning_agric[, .(year, deaths = cases, source = "NMC Total Notifications (Agricultural)")],
  fill = TRUE
)

p_poisoning <- dt_poisoning[year %in% 1997:2025] %>%
  ggplot(aes(x = as.numeric(as.character(year)), y = deaths, 
             color = source, group = source,
             text = paste0("Year: ", year, "<br>",
                          "Count: ", scales::comma(round(deaths)), "<br>",
                          "Source: ", source))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c(
    "VR data" = "#A23B72",
    "NMC Deaths (Agricultural)" = "#F77F00",
    "NMC Total Notifications (Agricultural)" = "#06D6A0"
  )) +
  labs(x = "Year", y = "Count", color = "Data Source",
       title = "Poisoning Deaths & Notifications Comparison",
       subtitle = "VR pesticide-poisoning codes (X48, X68, X87, Y18) vs NMC Agricultural poisoning notifications & deaths") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    panel.grid.minor = element_blank()
  )

# Unnatural deaths aggregate plot
p_unnatural <- dt_unnatural_vr[year %in% 1997:2022] %>%
  ggplot(aes(x = as.numeric(as.character(year)), y = unnatural_deaths,
             text = paste0("Year: ", year, "<br>",
                          "Unnatural Deaths: ", scales::comma(unnatural_deaths)))) +
  geom_line(linewidth = 1, color = "#E63946", aes( group = 1)) +
  geom_point(size = 2.5, color = "#E63946") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Year", y = "Deaths",
       title = "Total Unnatural Deaths (External Causes)",
       subtitle = "All deaths coded with external causes of injury (ICD-10: V, W, X, Y codes)") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    panel.grid.minor = element_blank()
  )

# ============================================================================
# TIMELINESS OF REPORTS
# ============================================================================

  library(tibble)
  library(dplyr)
  library(lubridate)
  library(stringr)

  mcod_statssa <- tribble(
      ~year_of_death, ~embargoed_until, ~url,
      2006, "23 October 2008 10:00", "https://www.statssa.gov.za/publications/P03093/P030932006.pdf",
      2007, "2 November 2009 13:00", "https://www.statssa.gov.za/publications/P03093/P030932007.pdf",
      2008, "18 November 2010 11:30", "https://www.statssa.gov.za/publications/P03093/P030932008.pdf",
      2009, "30 November 2011 11:30", "https://www.statssa.gov.za/publications/P03093/P030932009.pdf",
      2010, "11 April 2013 10:00", "https://www.statssa.gov.za/publications/P03093/P030932010.pdf",
      2011, "18 March 2014 10:00", "https://www.statssa.gov.za/publications/P03093/P030932011.pdf",
      2012, "4 September 2014 10:00", "https://www.statssa.gov.za/publications/P03093/P030932012.pdf",
      2013, "02 December 2014 11:00", "https://www.statssa.gov.za/publications/P03093/P030932013.pdf",
      2014, "02 December 2015 11:00", "https://www.statssa.gov.za/publications/P03093/P030932014.pdf",
      2015, "28 February 2017 11:00", "https://www.statssa.gov.za/publications/P03093/P030932015.pdf",
      2016, "27 March 2018 11:30", "https://www.statssa.gov.za/publications/P03093/P030932016.pdf",
      2017, "26 March 2020 11:00", "https://www.statssa.gov.za/publications/P03093/P030932017.pdf",
      2018, "15 June 2021 11:00", "https://www.statssa.gov.za/publications/P03093/P030932018.pdf",
      2019, "13 December 2023 11:00", "https://www.statssa.gov.za/publications/P03093/P030932019.pdf",
      2020, "30 April 2024 11:30", "https://www.statssa.gov.za/publications/P03093/P030932020.pdf",
      2021, "31 March 2025 14:30", "https://www.statssa.gov.za/publications/P03093/P030932021.pdf",
      2022, "28 August 2025 09:00", "https://www.statssa.gov.za/publications/P03093/P030932022.pdf"
  ) %>%
      mutate(
          # parse date robustly (drop time if you prefer)
          release_date = dmy(str_extract(embargoed_until, "\\d{1,2}\\s+\\w+\\s+\\d{4}")),
          publication_year = lubridate::year(release_date),
          product = "P0309.3",
          report_title = paste0(
              "Mortality and causes of death in South Africa, ", year_of_death,
              ": Findings from death notification"
          )
      ) %>%
      dplyr::select(
          publication_year,
          year_of_death,
          release_date,
          product,
          report_title,
          url
      ) %>%
      arrange(year_of_death)

# take each URL and put each PDF into a file 
if(!exists("MCOD_reports")){
    dir.create("MCOD_reports")
}
mcod_statssa$url %>% purrr::walk( ~ {
    url <- .
    destfile <- file.path("MCOD_reports", basename(url))
    if (!file.exists(destfile)) {
        download.file(url, destfile, mode = "wb")
    }
})

list.files("MCOD_reports", full.names = TRUE) -> downloaded_files
downloaded_files

library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(pdftools)

# tabulapdf is usually installed from GitHub; if you use tabulizer, same idea
# install.packages("remotes")
# remotes::install_github("ropensci/tabulizer")   # legacy
# remotes::install_github("f0nzie/tabulapdf")     # recommended fork (if you go this route)
# Optional: only needed for live PDF table extraction (not used in the plot path)
if (requireNamespace("tabulapdf", quietly = TRUE)) {
    library(tabulapdf)
}

find_pages_containing <- function(pdf_file, pattern, ignore_case = TRUE) {
    txt <- pdftools::pdf_text(pdf_file)
    hits <- which(str_detect(txt, regex(pattern, ignore_case = ignore_case)))
    tibble(page = hits)
}

extract_first_table_from_page <- function(pdf_file, page) {
    # tabula pages are 1-indexed
    tabulapdf::extract_tables(pdf_file, pages = page) |>
        pluck(1) |>
        as_tibble(.name_repair = "minimal")
}

clean_num <- function(x) {
    x %>%
        str_replace_all("\\s+", "") %>%
        str_replace_all(",", "") %>%
        str_extract("-?\\d+(\\.\\d+)?") %>%
        as.numeric()
}


library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)

df_plot <- mcod_statssa %>%
  # M+E review (Aug 2026): restrict to the most recent 10 death-year cohorts
  filter(year_of_death %in% ((max(year_of_death) - 9):max(year_of_death))) %>%
  mutate(
    death_year_end = ymd(paste0(year_of_death, "-12-31")),
    delay_days     = as.numeric(release_date - death_year_end),
    delay_months   = delay_days / 30.4375, 
    delay_years = round( delay_days / 365.25, 1),
    # Create hover text with clickable URL
    hover_text = paste0(
      "<b>Deaths in: </b>", year_of_death, "<br>",
      "<b>Released: </b>", format(release_date, "%d %b %Y"), "<br>",
      "<b>Delay: </b>", delay_years, " years<br>",
      "<b>Report: </b><a href='", url, "' target='_blank'>View PDF</a>"
    )
  )

  
df_plot

# Single-dimension vertical bar chart: bars anchored at y=0, height = delay in
# years, with value labels so outlier years (e.g. 2019) jump out immediately.
ggplot(df_plot, aes(x = factor(year_of_death), y = delay_years,
                     text = hover_text)) +
    geom_col(fill = "#2E86AB", width = 0.65) +
    geom_text(
        aes(label = paste0(delay_years, " yr")),
        vjust = -0.4, size = 3.3, colour = "grey20"
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(
        x = "Deaths occurring in year",
        y = "Delay to publication (years)"
    ) +
    theme_minimal(
    
    ) + 
    theme(
     axis.text.x = element_text(angle = 45, hjust = 1),
     panel.grid.minor = element_blank()
    )-> p_timeliness

# stylise for dashboard 
p_timeliness %>%
    plotly::ggplotly(
        tooltip = "text"
    ) %>%
    plotly::style(
        customdata = df_plot$url,
        traces = 1  # The geom_col trace (bars)
    ) %>%
    plotly::layout(
        hoverlabel = list(
            align = "left",
            bgcolor = "white",
            bordercolor = "darkgray"
        ),
        hovermode = "closest"
    ) %>%
    htmlwidgets::onRender("
        function(el, x) {
            el.on('plotly_click', function(d) {
                var url = d.points[0].customdata;
                if(url) window.open(url, '_blank');
            });
        }
    ") -> p_timeliness_interactive

    p_timeliness_interactive 


# ============================================================================
# DATA QUALITY OVERVIEW PLOTS - Total N by Province and Death Instance
# ============================================================================

# Calculate total deaths by province (most recent year for snapshot)
dt_province_overview <- dt[, .(total_deaths = .N), by = .(DeathProvince)][order(-total_deaths)]
dt_province_overview[, pct_of_total := total_deaths / sum(total_deaths) * 100]

# Calculate total deaths by death instance
dt_instance_overview <- dt[, .(total_deaths = .N), by = .(Death_instance)][order(-total_deaths)]
dt_instance_overview[, pct_of_total := total_deaths / sum(total_deaths) * 100]

# Province overview bar chart
p_overview_province <- dt_province_overview %>%
  ggplot(aes(x = reorder(as.factor(DeathProvince), total_deaths), y = total_deaths,
             text = paste0("Province: ", DeathProvince, "<br>",
                          "Total Deaths: ", scales::comma(total_deaths), "<br>",
                          "% of National: ", round(pct_of_total, 1), "%"))) +
  geom_col(fill = "#2E86AB", alpha = 0.8) +
  geom_text(aes(label = paste0(round(pct_of_total, 0), "%")), 
            hjust = -0.2, size = 3, color = "grey30") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = "Total Deaths (1997-2022)",
       title = "Deaths by Province",
       subtitle = "Total registered deaths across all years") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9, color = "grey40"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# Death instance overview bar chart
p_overview_instance <- dt_instance_overview %>%
  ggplot(aes(x = reorder(Death_instance, total_deaths), y = total_deaths,
             text = paste0("Institution: ", Death_instance, "<br>",
                          "Total Deaths: ", scales::comma(total_deaths), "<br>",
                          "% of Total: ", round(pct_of_total, 1), "%"))) +
  geom_col(fill = "#A23B72", alpha = 0.8) +
  geom_text(aes(label = paste0(round(pct_of_total, 0), "%")), 
            hjust = -0.2, size = 3, color = "grey30") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = "Total Deaths (1997-2022)",
       title = "Deaths by Institution Type",
       subtitle = "Total registered deaths across all years") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9, color = "grey40"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# ============================================================================
# Non-hospital deaths by province
# ============================================================================

# Facility-type classification (matches index.qmd's dq-data-load chunk):
# In-hospital = Hospital, Emergency room/Outpatient
# Out-of-hospital = Home, Nursing home, Dead on arrival, Other
# Unspecified/Unknown deaths are excluded from this denominator (not counted
# as non-hospital), consistent with the dashboard's Non-Hospital Deaths section.
dt[, facility_type := fcase(
  Death_instance %in% c("Hospital", "Emergency room/Outpatient"),       "In-hospital",
  Death_instance %in% c("Home", "Nursing home", "Dead on arrival",
                        "Other"),                                       "Out-of-hospital",
  default = "Unspecified/Unknown"
)]

# Calculate non-hospital deaths by province and year
dt_nonhospital_province <- dt[
  facility_type == "Out-of-hospital",
  .(non_hospital_deaths = .N),
  by = .(epi_year, DeathProvince)
]

# Also get total deaths per province per year for percentage, excluding
# Unspecified/Unknown facility type from the denominator
dt_total_by_prov_year <- dt[
  facility_type != "Unspecified/Unknown",
  .(total_deaths = .N),
  by = .(epi_year, DeathProvince)
]

# Merge and calculate percentage
dt_nonhospital_province <- merge(
  dt_nonhospital_province, 
  dt_total_by_prov_year, 
  by = c("epi_year", "DeathProvince")
)
dt_nonhospital_province[, pct_non_hospital := non_hospital_deaths / total_deaths * 100]

# Summary totals across all years
dt_nonhospital_summary <- dt_nonhospital_province[, 
  .(total_non_hospital = sum(non_hospital_deaths),
    total_deaths = sum(total_deaths)), 
  by = .(DeathProvince)
][, pct_non_hospital := total_non_hospital / total_deaths * 100][order(-total_non_hospital)]

# Bar chart of non-hospital deaths by province (totals)
p_nonhospital_province <- dt_nonhospital_summary %>%
  ggplot(aes(x = reorder(as.factor(DeathProvince), total_non_hospital), 
             y = total_non_hospital,
             text = paste0("Province: ", DeathProvince, "<br>",
                          "Non-Hospital Deaths: ", scales::comma(total_non_hospital), "<br>",
                          "Total Deaths: ", scales::comma(total_deaths), "<br>",
                          "% Non-Hospital: ", round(pct_non_hospital, 1), "%"))) +
  geom_col(fill = "#E63946", alpha = 0.8) +
  geom_text(aes(label = paste0(round(pct_non_hospital, 0), "%")), 
            hjust = -0.2, size = 3, color = "grey30") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
  labs(x = NULL, y = "Non-Hospital Deaths (1997-2022)",
       title = "Non-Hospital Deaths by Province",
       subtitle = "Home, Nursing Home, Dead on Arrival, Other; excludes Unspecified/Unknown facility type") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9, color = "grey40"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# Enrich non-hospital data with national totals for hover text
dt_nonhospital_national <- dt_nonhospital_province[, 
  .(national_non_hospital = sum(non_hospital_deaths),
    national_total = sum(total_deaths)),
  by = epi_year
]

dt_nonhospital_enriched <- merge(
  dt_nonhospital_province, 
  dt_nonhospital_national, 
  by = "epi_year"
)[, pct_of_national := non_hospital_deaths / national_non_hospital * 100]

# Time trend of non-hospital deaths by province
p_nonhospital_trend <- dt_nonhospital_enriched %>%
  ggplot(aes(x = epi_year, y = pct_non_hospital / 100, 
             color = as.factor(DeathProvince), group = DeathProvince,
             text = paste0("Year: ", epi_year, "<br>",
                          "Province: ", DeathProvince, "<br>",
                          "Non-Hospital: ", scales::percent(pct_non_hospital / 100, accuracy = 0.1), "<br>",
                          "<b>Scale:</b><br>",
                          "Non-Hospital Deaths (N): ", scales::comma(non_hospital_deaths), "<br>",
                          "Total Deaths: ", scales::comma(total_deaths), "<br>",
                          "% of National Non-Hospital: ", round(pct_of_national, 1), "%"))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  # Descriptive 50% reference line (NOT a literature-based quality target):
  # grey dashed to visually distinguish it from the standardized red CoD target.
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50", linewidth = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Year", y = "Proportion Non-Hospital", color = "Province",
       title = "Non-Hospital Deaths by Province: Proportion Over Time",
       subtitle = "Grey line = 50% descriptive reference (not a quality target)") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    panel.grid.minor = element_blank()
  )


# Save all plot objects
save(
  p_hiv,
  p_tb,
  p_poisoning,
  p_unnatural,
  p_garbage_province,
  p_garbage_instance,
  p_overview_province,
  p_overview_instance,
  p_nonhospital_province,
  p_nonhospital_trend,
  p_timeliness,
  p_timeliness_interactive,
  dt_hiv,
  dt_tb,
  dt_poisoning,
  dt_unnatural_vr,
  dt_garbage_summary_by_province,
  dt_garbage_summary_by_instance,
  dt_province_overview,
  dt_instance_overview,
  dt_nonhospital_summary,
  dt_nonhospital_enriched,
  file = "outputs/me_plots.RData"
)

message("Plots saved to outputs/me_plots.RData")


