# D4H indicators

# ============================================================================
# TIMELINESS OF REPORTS
# ============================================================================

  library(tibble)
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(pdftools)

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


if (!exists("P0309_reports")) {
    dir.create("P0309_reports")
}

mcod_statssa$url %>% purrr::walk( ~ {
    url <- .
    destfile <- file.path("P0309_reports", basename(url))
    if (!file.exists(destfile)) {
        download.file(url, destfile, mode = "wb")
    }
})

list.files("MCOD_reports", full.names = TRUE) -> downloaded_files
downloaded_files


library(tibble)
library(dplyr)

p0305_urls <- tribble(
  ~report_label, ~url,
  "Recorded live births, 2004 (P0305)", "https://www.statssa.gov.za/publications/P0305/P03052004.pdf",
  "Recorded live births, 2005 (P0305)", "https://www.statssa.gov.za/publications/P0305/P03052005.pdf",
  "Recorded live births, 2006 (P0305)", "https://www.statssa.gov.za/publications/P0305/P03052006.pdf",
  "Recorded live births, 2007 (P0305)", "https://www.statssa.gov.za/publications/P0305/P03052007.pdf",
  "Recorded live births, 2008 (P0305)", "https://www.statssa.gov.za/publications/P0305/P03052008.pdf",
  "Recorded live births, 2009 (P0305)", "https://www.statssa.gov.za/publications/P0305/P03052009.pdf",
  "Recorded live births, 2010 (P0305)", "https://www.statssa.gov.za/publications/P0305/P03052010.pdf",
  "Recorded live births, 2011 (P0305)", "https://www.statssa.gov.za/publications/P0305/P03052011.pdf",
  "Recorded live births, 2012 (P0305)", "https://www.statssa.gov.za/publications/P0305/P03052012.pdf",
  "Recorded live births, 2013 (P0305)", "https://www.statssa.gov.za/publications/P0305/P03052013.pdf",
  "Recorded live births, 2014 (P0305)", "https://www.statssa.gov.za/publications/P0305/P03052014.pdf",
  "Recorded live births, 2013–2015 (P0305)", "https://www.statssa.gov.za/publications/P0305/P03052015.pdf",
  "Recorded live births, 2016 (P0305)", "https://www.statssa.gov.za/publications/P0305/P03052016.pdf",
  "Recorded live births, 2017 (P0305)", "https://www.statssa.gov.za/publications/P0305/P03052017.pdf",
  "Recorded live births, 2018 (P0305)", "https://www.statssa.gov.za/publications/P0305/P03052018.pdf",
  "Recorded live births, 2019 (P0305)", "https://www.statssa.gov.za/publications/P0305/P03052019.pdf",
  "Recorded live births, 2020 (P0305)", "https://www.statssa.gov.za/publications/P0305/P03052020.pdf",
  "Recorded live births, 2021 (P0305)", "https://www.statssa.gov.za/publications/P0305/P03052021.pdf",
  "Recorded live births, 2022 (P0305)", "https://www.statssa.gov.za/publications/P0305/P03052022.pdf",
  "Recorded live births, 2023 (P0305)", "https://www.statssa.gov.za/publications/P0305/P03052023.pdf",
  "Recorded live births, 2024 (P0305)", "https://www.statssa.gov.za/publications/P0305/P03052024.pdf"
) %>%
  mutate(product = "P0305") %>%
  select(product, report_label, url)

p0305_urls

if(!exists("P0305_reports")) {
    dir.create("P0305_reports")
}
p0305_urls$url %>% purrr::walk( ~ {
    url <- .
    destfile <- file.path("P0305_reports", basename(url))
    if (!file.exists(destfile)) {
        download.file(url, destfile, mode = "wb")
    }
})

library(dplyr)
library(purrr)
library(stringr)
library(tibble)
library(pdftools)

# tabulapdf is usually installed from GitHub; if you use tabulizer, same idea
# install.packages("remotes")
 #remotes::install_github("ropensci/tabulizer")   # legacy
    pacman::p_load(tabulapdf)

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

extract_tables_from_page <- function(pdf_file, page) {
    tabs <- tabulapdf::extract_tables(pdf_file, pages = page)
    if (length(tabs) == 0) {
        return(list())
    }
    lapply(tabs, function(x) as_tibble(x, .name_repair = "minimal"))
}

find_best_page <- function(pdf_file, include, exclude = NULL) {
  txt <- pdftools::pdf_text(pdf_file)

  score_page <- function(s) {
    s2 <- tolower(s)
    score <- sum(str_detect(s2, tolower(include)))
    if (!is.null(exclude)) score <- score - 2L * sum(str_detect(s2, tolower(exclude)))
    score
  }

  scores <- vapply(txt, score_page, integer(1))
  best <- which.max(scores)

  if (scores[best] <= 0) return(NA_integer_)
  best
}

clean_num <- function(x) {
    x %>%
        str_replace_all("\\s+", "") %>%
        str_replace_all(",", "") %>%
        str_extract("-?\\d+(\\.\\d+)?") %>%
        as.numeric()
}


extract_births_p0305 <- function(year, url) {
    tmp <- tempfile(fileext = ".pdf")
    download.file(url, tmp, mode = "wb", quiet = TRUE)

    # Find best candidate page (much tighter than your regex)
    page <- find_best_page(
        tmp,
        include = c("live births", "occurred", "registered", "male", "female", "total"),
        exclude = c("contents", "glossary")
    )

    if (is.na(page)) {
        return(tibble(year = year, I2a = NA_real_, I15a = NA_real_, I15b = NA_real_, method = "no_page"))
    }

    # 1) Try table extraction
    tabs <- extract_tables_from_page(tmp, page)

    parse_table <- function(tab) {
        tab2 <- tab %>% mutate(across(everything(), ~ str_squish(as.character(.x))))
        # Look for a row containing the year
        r <- which(apply(tab2, 1, \(rr) any(str_detect(rr, paste0("\\b", year, "\\b")))))[1]
        if (is.na(r)) {
            return(NULL)
        }

        # Heuristic: pull numbers from that row (male/female/total) based on presence of headers
        # This still needs a one-time check on a recent PDF to confirm column ordering.
        row <- unlist(tab2[r, ], use.names = FALSE)
        nums <- clean_num(row)
        nums <- nums[!is.na(nums)]

        if (length(nums) < 3) {
            return(NULL)
        }

        # Conservative assumption: first three big numbers correspond to male/female/total (or total/male/female)
        # We'll try to detect header row keywords to map properly; if absent, return NA.
        headers <- tolower(unlist(tab2[1, ], use.names = FALSE))
        colmap <- list(
            male   = which(str_detect(headers, "male"))[1],
            female = which(str_detect(headers, "female"))[1],
            total  = which(str_detect(headers, "total"))[1]
        )

        if (all(!is.na(unlist(colmap)))) {
            return(tibble(
                year = year,
                I15a = clean_num(tab2[r, colmap$male]),
                I15b = clean_num(tab2[r, colmap$female]),
                I2a = clean_num(tab2[r, colmap$total]),
                method = "tabula_table"
            ))
        }

        NULL
    }

    out <- purrr::keep(purrr::map(tabs, parse_table), ~ !is.null(.x))
    if (length(out) > 0) {
        return(out[[1]])
    }

    # 2) Fallback: text extraction from narrative (works when tables fail)
    txt <- paste(pdftools::pdf_text(tmp), collapse = "\n") %>% str_squish()

    # Common stable pattern: "A total of X births that occurred in YEAR were registered ..."
    m_total <- str_match(
        txt,
        paste0("total of\\s+([0-9][0-9\\s,]*)\\s+births\\s+that\\s+occurred\\s+in\\s+", year, "\\s+were\\s+registered")
    )

    I2a <- if (!is.na(m_total[1, 2])) clean_num(m_total[1, 2]) else NA_real_

    tibble(year = year, I2a = I2a, I15a = NA_real_, I15b = NA_real_, method = "text_total_only")
}

extract_deaths_p03093 <- function(year, url) {
    tmp <- tempfile(fileext = ".pdf")
    download.file(url, tmp, mode = "wb", quiet = TRUE)

    # Find a likely page for sex totals
    sex_page <- find_best_page(
        tmp,
        include = c("deaths", "male", "female", "total", "occurred"),
        exclude = c("contents", "annexure")
    )

    # Find a likely page for ill-defined (optional)
    ill_page <- find_best_page(
        tmp,
        include = c("ill-defined", "r00", "r99", "symptoms", "signs"),
        exclude = c("contents")
    )

    out <- tibble(
        year = year,
        I5a = NA_real_,
        I16a1 = NA_real_,
        I16b1 = NA_real_,
        I16c = NA_real_,
        I9 = NA_real_,
        method_sex = NA_character_,
        method_ill = NA_character_
    )

    # ---- SEX TOTALS: try table ----
    if (!is.na(sex_page)) {
        tabs <- extract_tables_from_page(tmp, sex_page)

        # You will likely need a single one-time adjustment here after you inspect one table,
        # but the header-based mapping works surprisingly often.
        parsed <- NULL
        for (tab in tabs) {
            tab2 <- tab %>% mutate(across(everything(), ~ str_squish(as.character(.x))))
            # Find row with year
            r <- which(apply(tab2, 1, \(rr) any(str_detect(rr, paste0("\\b", year, "\\b")))))[1]
            if (is.na(r)) next

            headers <- tolower(unlist(tab2[1, ], use.names = FALSE))
            col_total <- which(str_detect(headers, "total"))[1]
            col_male <- which(str_detect(headers, "male"))[1]
            col_female <- which(str_detect(headers, "female"))[1]

            if (!is.na(col_total) && !is.na(col_male) && !is.na(col_female)) {
                parsed <- tibble(
                    I5a = clean_num(tab2[r, col_total]),
                    I16a1 = clean_num(tab2[r, col_male]),
                    I16b1 = clean_num(tab2[r, col_female]),
                    method_sex = "tabula_table"
                )
                break
            }
        }

        if (!is.null(parsed)) {
            out <- out %>% mutate(
                I5a = parsed$I5a,
                I16a1 = parsed$I16a1,
                I16b1 = parsed$I16b1,
                method_sex = parsed$method_sex
            )
        }
    }

    # ---- SEX TOTALS fallback: text ----
    if (is.na(out$I5a)) {
        txt <- paste(pdftools::pdf_text(tmp), collapse = "\n") %>% str_squish()
        # Look for a stable sentence (you may need to adjust after checking one PDF):
        # "A total of X deaths occurred in YEAR ..."
        m_total <- str_match(
            txt,
            paste0("total of\\s+([0-9][0-9\\s,]*)\\s+deaths\\s+occurred\\s+in\\s+", year)
        )
        if (!is.na(m_total[1, 2])) {
            out <- out %>% mutate(I5a = clean_num(m_total[1, 2]), method_sex = "text_total_only")
        }
    }

    # ---- Ill-defined (optional): leave NA unless you standardise definition and confirm pattern ----
    if (!is.na(ill_page)) {
        # You can add a similar table/text parse here once you decide:
        # - R00–R99 share? or "ill-defined" category used in the report tables?
        out <- out %>% mutate(method_ill = "not_implemented")
    }

    out
}


# 1) Pick one recent PDF and print the best page number + a snippet
tmp <- tempfile(fileext = ".pdf")
download.file("https://www.statssa.gov.za/publications/P0305/P03052024.pdf", tmp, mode = "wb", quiet = TRUE)
p <- find_best_page(tmp, include = c("occurred", "registered", "male", "female", "total", "live births"))
p
cat(substr(pdftools::pdf_text(tmp)[p], 1, 800))

# 2) If tabula tables are coming back empty, you’ll know instantly:
#tabs <- extract_tables_from_page(tmp, p)

#length(tabs)


# Deaths indicator baseline
# from MACOD report publishd 2025 (for 2022 deaths)
 tribble(
    ~year, ~total_deaths, ~male_pct, ~female_pct,
    2001, 456842, 52.2, 47.8,
    2002, 504073, 51.4, 48.6,
    2003, 559108, 51.0, 49.0,
    2004, 579040, 50.6, 49.4,
    2005, 600215, 50.3, 49.7,
    2006, 614764, 50.6, 49.4,
    2007, 606782, 51.0, 49.0,
    2008, 598830, 51.2, 48.9,
    2009, 584625, 51.4, 48.6,
    2010, 553367, 51.5, 48.5,
    2011, 520774, 51.7, 48.3,
    2012, 498217, 52.2, 47.8,
    2013, 479715, 52.4, 47.6,
    2014, 480071, 52.4, 47.6,
    2015, 478098, 52.6, 47.4,
    2016, 474920, 52.8, 47.2,
    2017, 469688, 52.8, 47.2,
    2018, 471388, 52.8, 47.2,
    2019, 467601, 52.7, 47.3,
    2020, 515891, 51.1, 48.9,
    2021, 620394, 51.0, 49.1,
    2022, 486041, 53.5, 46.6
) %>%
    mutate(
        male_n = round(total_deaths * male_pct / 100),
        female_n = total_deaths - male_n, # forces totals to match after rounding
        male_minus_female_n = male_n - female_n,
        male_minus_female_pp = male_pct - female_pct
    )-> 
    deaths_sex_tbl 

deaths_sex_tbl

# from table 5 in 2024 birth registration report

birth_reg_status_tbl <- tribble(
    ~year_registration, ~total, ~current, ~late, ~pct_current, ~pct_late,
    2005, 1380496, 793788, 586708, 57.5, 42.5,
    2006, 1346119, 860263, 485856, 63.9, 36.1,
    2007, 1199712, 858866, 340846, 71.6, 28.4,
    2008, 1277763, 915674, 362089, 71.7, 28.3,
    2009, 1254707, 879707, 375000, 70.1, 29.9,
    2010, 1294694, 889691, 405003, 68.7, 31.3,
    2011, 1202377, 911353, 291024, 75.8, 24.2,
    2012, 1168403, 926726, 241677, 79.3, 20.7,
    2013, 1158622, 939011, 219611, 81.0, 19.0,
    2014, 1142275, 954385, 187890, 83.6, 16.4,
    2015, 1084511, 919562, 164949, 84.8, 15.2,
    2016, 969415, 876435, 92980, 90.4, 9.6,
    2017, 989318, 897750, 91568, 90.7, 9.3,
    2018, 1009065, 927113, 81952, 91.9, 8.1,
    2019, 1051311, 954532, 96779, 90.8, 9.2,
    2020, 1003307, 899303, 104004, 89.6, 10.4,
    2021, 1087526, 949757, 137769, 87.3, 12.7,
    2022, 998362, 911986, 86376, 91.3, 8.7,
    2023, 932138, 848337, 83801, 91.0, 9.0,
    2024, 863858, 787933, 75925, 91.2, 8.8
)

birth_reg_status_tbl

# From table 11 in 2024 birth registration report 
birth_occ_sex_tbl <- tribble(
    ~year_of_birth, ~total, ~male, ~female, ~sex_ratio,
    2005, 1072588, 539572, 532998, 101,
    2006, 1104399, 554723, 549676, 101,
    2007, 1090379, 548944, 541435, 101,
    2008, 1113857, 560212, 553645, 101,
    2009, 1063439, 534836, 527603, 101,
    2010, 1036395, 522815, 513580, 102,
    2011, 1045507, 527329, 518178, 102,
    2012, 1046070, 527667, 518403, 102,
    2013, 1033325, 521481, 511844, 102,
    2014, 1037787, 523593, 514194, 102,
    2015, 989001, 498380, 490621, 102,
    2016, 932186, 470958, 461228, 102,
    2017, 946770, 477626, 469144, 102,
    2018, 974995, 492336, 482659, 102,
    2019, 997486, 503264, 494222, 102,
    2020, 1021664, 515644, 506020, 102,
    2021, 1017807, 512924, 504883, 102,
    2022, 953951, 480704, 472447, 102,
    2023, 872792, 440364, 432428, 102,
    2024, 798581, 402316, 396265, 102
) %>%
    mutate(
        unknown_sex = total - male - female, # if totals include unknown/unspecified sex
        male_pct    = 100 * male / total,
        female_pct  = 100 * female / total
    )

births_deaths_tbl <- tibble(
    year = 2002:2025,
    births = c(
        982627, 969589, 1042137, 1105210, 1137244, 1160263, 1185987, 1171502,
        1156746, 1167060, 1171207, 1153570, 1169536, 1120064, 1055340, 1062148,
        1118566, 1138289, 1150914, 1144526, 1131070, 1119345, 1115478, 1117840
    ),
    deaths = c(
        600819, 627928, 649085, 665558, 673516, 669914, 642486, 621064,
        608810, 579091, 528193, 523445, 506120, 505168, 511007, 514426,
        522182, 523898, 529444, 690939, 610193, 550549, 550474, 557164
    ),
    aids_related_deaths = c(
        216649, 242358, 253597, 272508, 275348, 252392, 215337, 186697,
        169076, 140809, 114252, 104330, 84283, 83672, 84622, 82835,
        81158, 79929, 81186, 80300, 78642, 79627, 79703, 77639
    ),
    pct_aids_related_deaths = c(
        36.1, 38.6, 39.1, 40.9, 40.9, 37.7, 33.5, 30.1,
        27.8, 24.3, 21.6, 19.9, 16.7, 16.6, 16.6, 16.1,
        15.5, 15.3, 15.3, 11.6, 12.9, 14.5, 14.5, 13.9
    )
)

# tables
mcod_statssa

deaths_sex_tbl %>%
select( 
    year, total_deaths, male_n, female_n
)%>%print( n = 25 )


# registered births 
birth_reg_status_tbl
birth_occ_sex_tbl # use this on in d4h report 


# MYPE 
#estimated births and deaths 
births_deaths_tbl%>%print( n = 100)
