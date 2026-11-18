# =============================================================================
# me_vr_data.r - single loader for the Monitoring & Evaluation analyses
# =============================================================================
# The LGH extract (LGH_MasterFile_preCollapsedAll.feather) stops at death year
# 2022. Deaths for 2023+ exist only in Deaths2022_MRCversionFINAL_2024.feather
# (Isibalo bridge, see data/macod_isibalo/). This loader keeps the LGH rows for
# 1997-2022 untouched and appends the later death years from the bridge file,
# deriving the garbage-code flag from UnderlyingCause with the same crosswalk
# used to build the LGH extract (LGH2025_analysis_FINAL_using2022data.R, S3).
#
# Limits of the appended rows: province-level geography only, and no
# registration dates (so registration-lag analyses cannot include them).
# =============================================================================

suppressPackageStartupMessages({
  library(arrow)
  library(data.table)
  library(haven)
})

me_garbage_labels <- c(
  "0" = "Not garbage",
  "1" = "Symptoms, signs and ill-defined conditions",
  "2" = "Impossible as underlying causes of death",
  "3" = "Intermediate causes of death",
  "4" = "Immediate causes of death",
  "5" = "Insufficiently specified causes within ICD chapters"
)

me_garbage_flag <- function(underlying_cause) {
  uc <- toupper(trimws(as.character(underlying_cause)))
  in_rng <- function(lo, hi) !is.na(uc) & uc >= lo & uc <= hi
  flag <- rep(0L, length(uc))

  flag[in_rng("R00", "R99")] <- 1L
  flag[uc %in% c("D65", "I46", "J96")] <- 4L

  g5 <- c("C80", "C26", "C39", "C57", "C64", "C76", "A49", "B83", "B99",
          "E88", "I51", "I99", "X59")
  flag[uc %in% g5 | in_rng("D00", "D13") | in_rng("D16", "D18") |
         in_rng("D20", "D24") | in_rng("D28", "D48") | in_rng("Y10", "Y34")] <- 5L

  g3 <- c("A48", "I26", "I27", "I74", "I81", "J69", "J86", "J90", "J93", "J94",
          "J98", "K75", "K76", "M86", "N14", "K92")
  flag[in_rng("A40", "A41") | uc %in% g3 | in_rng("E85", "E87") |
         in_rng("G91", "G93") | in_rng("I44", "I45") | in_rng("I49", "I50") |
         in_rng("J80", "J81") | in_rng("K65", "K66") | in_rng("K71", "K72") |
         in_rng("N17", "N19")] <- 3L

  g2 <- c("A31", "A59", "A60", "A63", "B00", "B07", "B08", "B30", "G54", "J30",
          "K14", "L94", "L98", "M03", "M07", "M35", "M40", "M43", "M45", "N39",
          "N40", "N46", "N60", "N97", "Q36", "Q38", "Q54", "B94", "Y86", "Y87",
          "Y89", "I10", "I15", "I70")
  g2_ranges <- list(
    c("A71", "A74"), c("B35", "B36"), c("F32", "F33"), c("F40", "F42"),
    c("F45", "F48"), c("F51", "F53"), c("F60", "F98"), c("G43", "G45"),
    c("G47", "G52"), c("G56", "G58"), c("H00", "H69"), c("H71", "H80"),
    c("H83", "H93"), c("J33", "J35"), c("K00", "K11"), c("L04", "L08"),
    c("L20", "L25"), c("L28", "L87"), c("L90", "L92"), c("M09", "M12"),
    c("M14", "M25"), c("M47", "M60"), c("M63", "M71"), c("M73", "M79"),
    c("M95", "M99"), c("N84", "N93"), c("Q10", "Q18"), c("Q65", "Q74"),
    c("Q82", "Q84"), c("G80", "G83")
  )
  hit2 <- uc %in% g2
  for (r in g2_ranges) hit2 <- hit2 | in_rng(r[1], r[2])
  flag[hit2] <- 2L

  flag
}

me_garbage_factor <- function(flag) {
  factor(flag, levels = as.integer(names(me_garbage_labels)),
         labels = me_garbage_labels)
}

# Returns one data.table across all available death years with:
#   DeathYear, epi_year, UnderlyingCause, CauseA, DeathProvince (character),
#   DeathInst / sex (haven_labelled), NaturalUnnatural, garbage_flag,
#   garbage_flag_label, vr_source.
load_me_vr <- function(root = ".", natural_only = FALSE) {
  base_path <- file.path(root, "LGH_MasterFile_preCollapsedAll.feather")
  ext_path  <- file.path(root, "Deaths2022_MRCversionFINAL_2024.feather")

  base_cols <- c("DeathYear", "epi_year", "UnderlyingCause", "CauseA",
                 "DeathProvince", "DeathInst", "sex", "NaturalUnnatural",
                 "garbage_flag", "garbage_flag_label")
  base <- as.data.table(read_feather(base_path, col_select = all_of(base_cols)))

  inst_labels <- attr(base$DeathInst, "labels")
  sex_labels  <- attr(base$sex, "labels")
  for (v in c("DeathYear", "DeathInst", "sex", "NaturalUnnatural")) {
    set(base, j = v, value = as.integer(unclass(base[[v]])))
  }
  base[, vr_source := "LGH extract"]
  base_max_year <- max(base$DeathYear, na.rm = TRUE)

  if (file.exists(ext_path)) {
    ext <- as.data.table(read_feather(ext_path, col_select = all_of(
      c("DeathYear", "epi_year", "UnderlyingCause", "CauseA", "DeathProvince",
        "DeathInst", "Sex", "NaturalUnnatural"))))
    prov_labels <- attr(ext$DeathProvince, "labels")
    for (v in c("DeathYear", "DeathProvince", "DeathInst", "Sex", "NaturalUnnatural")) {
      set(ext, j = v, value = as.integer(unclass(ext[[v]])))
    }
    ext <- ext[DeathYear > base_max_year]

    # The LGH extract only holds deaths with a South African province of death.
    ext <- ext[DeathProvince %in% 1:9]
    ext[, DeathProvince := names(prov_labels)[match(DeathProvince, prov_labels)]]
    setnames(ext, "Sex", "sex")

    ext[, UnderlyingCause := toupper(trimws(as.character(UnderlyingCause)))]
    ext[, garbage_flag := me_garbage_flag(UnderlyingCause)]
    ext[, garbage_flag_label := me_garbage_factor(garbage_flag)]

    # ISO epi-weeks put the last days of December into the next epi-year; keep
    # them in the final year of data rather than creating a stub year.
    ext[, epi_year := pmin(as.numeric(epi_year), max(DeathYear, na.rm = TRUE))]
    ext[, vr_source := "Isibalo bridge"]

    base[, garbage_flag_label := as.factor(garbage_flag_label)]
    dt <- rbindlist(list(base, ext), use.names = TRUE, fill = TRUE)
    rm(base, ext)
  } else {
    warning("2024 bridge extract not found; M&E analyses stop at death year ",
            base_max_year, call. = FALSE)
    dt <- base
  }

  dt[, DeathInst := haven::labelled(DeathInst, labels = inst_labels)]
  dt[, sex := haven::labelled(sex, labels = sex_labels)]
  if (natural_only) dt <- dt[NaturalUnnatural == 1L]
  invisible(gc(FALSE))
  dt
}
