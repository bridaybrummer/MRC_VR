# ============================================================================
# Stats SA P0309.3 ("Mortality and causes of death in South Africa: Findings
# from death notification") release tracker.
#
# - Starts from a hard-coded table of known releases (death years 2006-2022).
# - Probes statssa.gov.za for newer reports (P03093<year>.pdf) and stops at the
#   first death year that is not yet published (HTTP 404/403).
# - Downloads new PDFs to MCOD_reports/ and reads the release date from the
#   "Embargoed until: <date>" text on the cover page.
# - Caches results in outputs/mcod_statssa_releases.csv so offline renders
#   still show the most recently discovered reports.
#
# The Stats SA listing page sits behind a bot challenge and R's default
# downloader is refused (403), so the system `curl` with a browser
# User-Agent is used instead.
#
# Paths are relative to the project root; pass `root` when calling from a
# sub-directory (e.g. a Quarto page).
# ============================================================================

# Functions are fully namespace-qualified so they work alongside `conflicted`.
`%>%` <- dplyr::`%>%`

statssa_p03093_url <- function(year) {
    sprintf("https://www.statssa.gov.za/publications/P03093/P03093%d.pdf", year)
}

statssa_known_releases <- function() {
    tibble::tribble(
        ~year_of_death, ~embargoed_until,
        2006, "23 October 2008 10:00",
        2007, "2 November 2009 13:00",
        2008, "18 November 2010 11:30",
        2009, "30 November 2011 11:30",
        2010, "11 April 2013 10:00",
        2011, "18 March 2014 10:00",
        2012, "4 September 2014 10:00",
        2013, "02 December 2014 11:00",
        2014, "02 December 2015 11:00",
        2015, "28 February 2017 11:00",
        2016, "27 March 2018 11:30",
        2017, "26 March 2020 11:00",
        2018, "15 June 2021 11:00",
        2019, "13 December 2023 11:00",
        2020, "30 April 2024 11:30",
        2021, "31 March 2025 14:30",
        2022, "28 August 2025 09:00"
    ) %>%
        dplyr::mutate(
            release_date = lubridate::dmy(stringr::str_extract(embargoed_until, "\\d{1,2}\\s+\\w+\\s+\\d{4}")),
            source = "Known release"
        ) %>%
        dplyr::select(year_of_death, release_date, source)
}

# Download a URL with system curl; returns TRUE when a PDF was saved.
statssa_curl_pdf <- function(url, destfile, timeout = 90) {
    curl_bin <- Sys.which("curl")
    if (!nzchar(curl_bin)) return(FALSE)
    tmp <- tempfile(fileext = ".pdf")
    status <- suppressWarnings(system2(
        curl_bin,
        c("-s", "-L", "-m", timeout,
          "-A", shQuote("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)"),
          "-o", shQuote(tmp), "-w", shQuote("%{http_code}"), shQuote(url)),
        stdout = TRUE, stderr = FALSE
    ))
    ok <- length(status) == 1 && status == "200" && file.exists(tmp) &&
        identical(readBin(tmp, "raw", 4), charToRaw("%PDF"))
    if (ok) file.copy(tmp, destfile, overwrite = TRUE)
    unlink(tmp)
    ok
}

# Read "Embargoed until: 22 September 2026" from the report cover.
statssa_release_date_from_pdf <- function(pdf_file) {
    txt <- tryCatch(paste(pdftools::pdf_text(pdf_file)[1:2], collapse = " "),
                    error = function(e) NA_character_)
    if (is.na(txt)) return(as.Date(NA))
    hit <- stringr::str_match(
        stringr::str_squish(txt),
        stringr::regex("Embargoed until:?\\s*(\\d{1,2}\\s+[A-Za-z]+\\s+\\d{4})", ignore_case = TRUE)
    )[, 2]
    suppressWarnings(lubridate::dmy(hit))
}

# Returns one row per death year with release date, delay, URL and local file.
get_statssa_mcod_releases <- function(root = ".",
                                      check_online = TRUE,
                                      max_year = lubridate::year(Sys.Date())) {
    pdf_dir <- file.path(root, "MCOD_reports")
    cache_file <- file.path(root, "outputs", "mcod_statssa_releases.csv")
    dir.create(pdf_dir, showWarnings = FALSE, recursive = TRUE)

    releases <- statssa_known_releases()
    if (file.exists(cache_file)) {
        cached <- read.csv(cache_file, stringsAsFactors = FALSE) %>%
            dplyr::transmute(year_of_death = as.numeric(year_of_death),
                      release_date = as.Date(release_date),
                      source = "Auto-discovered")
        releases <- dplyr::bind_rows(releases, dplyr::filter(cached, !year_of_death %in% releases$year_of_death))
    }

    # Pick up any PDF already on disk that is newer than the table/cache.
    on_disk <- as.numeric(stringr::str_match(list.files(pdf_dir, "^P03093\\d{4}\\.pdf$"), "(\\d{4})\\.pdf")[, 2])
    for (y in sort(setdiff(on_disk, releases$year_of_death))) {
        rd <- statssa_release_date_from_pdf(file.path(pdf_dir, sprintf("P03093%d.pdf", y)))
        if (!is.na(rd)) releases <- tibble::add_row(releases, year_of_death = y, release_date = rd, source = "Auto-discovered")
    }

    if (check_online) {
        y <- max(releases$year_of_death) + 1
        while (y <= max_year) {
            dest <- file.path(pdf_dir, sprintf("P03093%d.pdf", y))
            if (!statssa_curl_pdf(statssa_p03093_url(y), dest)) break
            rd <- statssa_release_date_from_pdf(dest)
            if (is.na(rd)) {
                warning("Downloaded P03093", y, ".pdf but could not read the embargo date")
                break
            }
            message("Stats SA P0309.3: found new report for death year ", y,
                    " (released ", format(rd, "%d %B %Y"), ")")
            releases <- tibble::add_row(releases, year_of_death = y, release_date = rd, source = "Auto-discovered")
            y <- y + 1
        }
    }

    releases <- releases %>%
        dplyr::arrange(year_of_death) %>%
        dplyr::mutate(
            publication_year = lubridate::year(release_date),
            product = "P0309.3",
            report_title = paste0("Mortality and causes of death in South Africa, ",
                                  year_of_death, ": Findings from death notification"),
            url = statssa_p03093_url(year_of_death),
            local_file = file.path(pdf_dir, basename(url))
        )

    # Download any historical PDFs that are missing locally (best effort).
    if (check_online) {
        for (i in which(!file.exists(releases$local_file))) {
            statssa_curl_pdf(releases$url[i], releases$local_file[i])
        }
    }

    auto <- dplyr::filter(releases, source == "Auto-discovered")
    if (nrow(auto) > 0 && dir.exists(dirname(cache_file))) {
        write.csv(dplyr::transmute(auto, year_of_death, release_date = format(release_date)),
                  cache_file, row.names = FALSE)
    }
    attr(releases, "checked_on") <- Sys.Date()
    releases
}

# WHO (2010), Rapid assessment of national CRVS systems, question 24, p. 14:
# the highest-scoring category for public cause-of-death statistics is <2 years.
# https://www.who.int/publications/i/item/rapid-assessment-of-national-civil-registration-and-vital-statistics-systems
# Measure elapsed time after the reference year, starting on the following 1 January.
build_timeliness_plots <- function(releases, n_years = 10) {
    who_benchmark_years <- 2
    timeframe <- "Measured from 1 January following the death year to the report release date"
    benchmark_note <- "Dashed line: WHO highest-scoring timeliness category is <2 years (2010, question 24)"
    latest_year <- max(releases$year_of_death)
    df_plot <- releases %>%
        dplyr::filter(year_of_death > latest_year - n_years) %>%
        dplyr::mutate(
            death_year_end = lubridate::ymd(paste0(year_of_death, "-12-31")),
            delay_start_date = lubridate::ymd(paste0(year_of_death + 1, "-01-01")),
            delay_days     = as.numeric(release_date - delay_start_date),
            delay_months   = delay_days / 30.4375,
            delay_years    = round(delay_days / 365.25, 1),
            is_latest      = year_of_death == latest_year,
            bar_group      = dplyr::if_else(is_latest, "Most recent report", "Earlier reports"),
            hover_text = paste0(
                "<b>Deaths in: </b>", year_of_death, "<br>",
                "<b>Measured from: </b>", format(delay_start_date, "%d %b %Y"), "<br>",
                "<b>Released: </b>", format(release_date, "%d %b %Y"), "<br>",
                "<b>Elapsed time: </b>", delay_days, " days (", delay_years, " years)<br>",
                "<b>WHO benchmark: </b>less than 2 years<br>",
                "<b>Report: </b><a href='", url, "' target='_blank'>View PDF</a>"
            )
        )

    label_gap <- 0.025 * max(df_plot$delay_years, who_benchmark_years)
    p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = factor(year_of_death), y = delay_years,
                             fill = bar_group, text = hover_text)) +
        ggplot2::geom_col(width = 0.65) +
        ggplot2::geom_hline(yintercept = who_benchmark_years,
                           linetype = "dashed", colour = "#D32F2F") +
        ggplot2::geom_text(ggplot2::aes(y = delay_years + label_gap,
                                      label = paste0(delay_years, " yr")),
                  vjust = 0, size = 3.3, colour = "grey20") +
        ggplot2::scale_fill_manual(values = c("Earlier reports" = "#2E86AB",
                                     "Most recent report" = "#E07A1F"),
                          name = NULL) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
        ggplot2::labs(x = "Year in which deaths occurred", y = "Time after death year (years)",
                      subtitle = paste0("Publication delay after the death year\n", timeframe),
                      caption = benchmark_note) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
              panel.grid.minor = ggplot2::element_blank(),
              legend.position = "bottom",
              plot.subtitle = ggplot2::element_text(size = 10),
              plot.caption = ggplot2::element_text(hjust = 0, size = 9))

    # Map each bar trace back to its URLs so clicking opens the PDF.
    gg <- plotly::ggplotly(p, tooltip = "text")
    year_levels <- levels(factor(df_plot$year_of_death))
    for (i in seq_along(gg$x$data)) {
        if (identical(gg$x$data[[i]]$type, "bar")) {
            yrs <- year_levels[as.integer(round(as.numeric(gg$x$data[[i]]$x)))]
            gg$x$data[[i]]$customdata <- df_plot$url[match(yrs, as.character(df_plot$year_of_death))]
        }
        if (identical(gg$x$data[[i]]$mode, "text")) {
            gg$x$data[[i]]$textposition <- "top center"
            gg$x$data[[i]]$hoverinfo <- "skip"
        }
    }
    p_int <- gg %>%
        plotly::layout(
            hoverlabel = list(align = "left", bgcolor = "white", bordercolor = "darkgray"),
            hovermode = "closest",
            margin = list(t = 105, b = 115),
            annotations = list(
                list(x = 0, y = 1.11, xref = "paper", yref = "paper",
                     xanchor = "left", yanchor = "bottom", showarrow = FALSE,
                     text = paste0("Publication delay after the death year<br>", timeframe),
                     align = "left", font = list(size = 12)),
                list(x = 0, y = 1.01, xref = "paper", yref = "paper",
                     xanchor = "left", yanchor = "bottom", showarrow = FALSE,
                     text = paste0("Dashed line: WHO highest-scoring category is <b>&lt;2 years</b>",
                                   "<br>(Rapid assessment, 2010, question 24)"),
                     align = "left", font = list(size = 11, color = "#D32F2F"))
            ),
            legend = list(orientation = "h", y = -0.30)
        ) %>%
        htmlwidgets::onRender("
            function(el, x) {
                el.on('plotly_click', function(d) {
                    var url = d.points[0].customdata;
                    if (url) window.open(url, '_blank');
                });
            }
        ")

    list(data = df_plot, plot = p, interactive = p_int)
}
