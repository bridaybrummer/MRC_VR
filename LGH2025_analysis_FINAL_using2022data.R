################################################################################
# South Africa Deaths Analysis 2022 - R Translation
# Converted from Stata .do file to R using data.table
# 
# NOTES FOR TIDYVERSE USERS:
# - data.table uses := for assignment by reference (modifies in place, fast!)
# - [i, j, by] structure: i=row filter, j=column operations, by=grouping
# - No quotes needed for column names in most operations
# - .N is the count of rows (like n() in dplyr)
# - .SD is "Subset of Data" (like cur_data() in dplyr)
################################################################################

# Load required packages
library(data.table)  # Fast data manipulation (replaces dplyr)
library(haven)       # Read Stata files (read_dta)
library(ggplot2)     # For plotting
library(MASS)        # For glm.nb (negative binomial regression)
library(magrittr)
library(gtsummary)

# Set working directory - ADJUST THIS TO YOUR PATH
#setwd("/Users/briday/Desktop/study_stats/MRC_VR")

################################################################################
# SECTION 1: DATA LOADING AND INITIAL PROCESSING
################################################################################
# load up .dta files 

if( FALSE) {



# Load the deaths data
# In tidyverse: read_dta() %>% as_tibble()
# In data.table: read_dta() %>% as.data.table()
start_time <- Sys.time()
dt <- as.data.table(read_dta("Deaths2022_MRCversionFINAL.dta"))
nrow(dt)-> full_dt_n
names(dt)
setnames(dt, "SerialNo", "serialno")

dt_districts1997_2021 <- as.data.table(read_dta("District1997_2021.dta"))
names(dt_districts1997_2021)
nrow(dt_districts1997_2021)-> dt_97_21_n

dt_districts2022 <- as.data.table(read_dta("District2022.dta"))
setnames(dt_districts2022, names(dt_districts2022), tolower(
                                                                                    names(dt_districts2022))
                                                                                    )

nrow(dt_districts2022)-> dt_2022_n
names(dt_districts2022)
rbind( 
    dt_districts1997_2021, 
    dt_districts2022
)-> dt_districts

# join by the serialno
dt[
    dt_districts, on = .(serialno)
    ]-> dt


end_time <- Sys.time()
time_taken <- end_time - start_time
cat("Time taken to load and process data:", time_taken, "\n")

dt$deathdistrictname%>%is.na()%>%table()

names(dt)

arrow::write_feather(dt, "Deaths2022_MRCversionFINAL.feather")



}else{
# save as an feather dataset 
library(arrow)
#write_feather(dt, "Deaths2022_MRCversionFINAL.feather")

#read back in the feather dataset
time_taken_start <- Sys.time()
dt <- read_feather("Deaths2022_MRCversionFINAL.feather") %>% as.data.table()
end_time <- Sys.time()
time_taken <- end_time - time_taken_start
cat("Time taken to read back feather dataset:", time_taken, "\n")

}
names(dt)

# province
# place of death
# district
# deathype 


# Create count variable (equivalent to Stata's gen count=1)
# This is used for aggregation counting later
dt[, count := 1]
dt$epi_year%>%unique()
# Filter years 2014-2022
# Tidyverse equivalent: filter(epi_year >= 2014, epi_year <= 2022)
# data.table: use [row_condition]




table(dt$NaturalUnnatural)
table(dt$DeathType)
# Keep only national natural deaths
# Tidyverse: filter(Nat == 1, DeathType == 1)
dt <- dt[NaturalUnnatural == 1 & DeathType == 1]



# Drop specific underlying causes (unusual codes)
# Tidyverse: filter(!UnderlyingCause %in% c("U51", "U52", "W34"))
dt <- dt[!(UnderlyingCause %in% c("U51", "U52", "W34"))]

# Drop missing weekstart and age values
# Tidyverse: filter(!is.na(weekstart), !is.na(age), age >= 0)
dt <- dt[!is.na(weekstart) & !is.na(age) & age >= 0]

dt$UnderlyingCause
dt$epi_year
dt$epi_week


dt_nat_cause <- dt[!is.na(UnderlyingCause), .(count = .N),
    by = .( epi_year, epi_week)
]


dt_nat_cause
dt_nat_cause%>%
    ggplot() + 
    geom_line( 
        aes( 
            x = epi_week,
            y = count,
        )
    )+ facet_wrap(
        ~ epi_year
    )

# Group by underlying cause year months for a ggplot
dt_by_cause <- dt[!is.na(UnderlyingCause), .(count = .N),
    by = .(UnderlyingCause, epi_year, epi_week)
]

dt_by_cause%>%
    ggplot( )+ 
    geom_line( 
        aes( 
            x = epi_week,
            y = count,
            color = UnderlyingCause
        ), 
        show.legend = FALSE
    ) + facet_wrap(
        ~ epi_year
    )+ theme_minimal() 

dt_by_cause[, .(count = sum(count)) , by=.(UnderlyingCause)][order(-count)]


# Rename Sex to sex (lowercase)
# Tidyverse: rename(sex = Sex)
# data.table: setnames() or direct assignment
setnames(dt, "Sex", "sex")

# Keep only valid sex codes (1=male, 2=female)
dt <- dt[sex <= 2]

# Create age groups using cut
# Tidyverse: mutate(agegroup = cut(age, breaks = c(0,5,40,60,70,80,150), right = FALSE))
# data.table: use := for in-place assignment
dt[, agegroup := cut(age, 
                     breaks = c(0, 5, 40, 60, 70, 80, 150),
                     right = FALSE,
                     labels = c("0-4", "5-39", "40-59", "60-69", "70-79", "80+"))]

# Create LGH_MainGroup variable from Code_Main
dt[, LGH_MainGroup := Code_Main]


# Code death province 
dt$DeathProvince %>% unique() -> province_unique

library(labelled)

var_label(dt$DeathProvince)
val_labels(dt$DeathProvince)
province_labels <- val_labels(dt$DeathProvince)

province_labels %>% as.list()
province_labels[1]

dt[, DeathProvince := names(province_labels[DeathProvince])]
province_labels[1]
dt$DeathProvince %>% unique()

################################################################################
# SECTION 2: RECODE LGH VALUES
################################################################################

# Recode specific LGH values to 21 (Others category)
# Tidyverse: mutate(LGH = case_when(LGH %in% c(5,7,8,11:13,15,17) ~ 21, TRUE ~ LGH))
# data.table: use %in% for multiple values
dt[LGH_MainGroup %in% c(5, 7, 8, 11, 12, 13, 15, 17), LGH_MainGroup := 21]

################################################################################
# SECTION 3: CREATE COVID FLAG AND CAUSE GROUPS
################################################################################

# Initialize covid flag to 0
# Tidyverse: mutate(covid = 0)
dt[, covid := 0]

# Create LGH_CauseGroup from UnderlyingCause
# TODO: Insert garbage-code identification right here so suspect ICD codes
# can be flagged/recoded before any downstream cause grouping.

icd_in_range <- function(code, lower, upper) {
    !is.na(code) & code >= lower & code <= upper
}

dt[, UnderlyingCause := toupper(trimws(as.character(UnderlyingCause)))]
dt[, garbage_flag := 0L]

dt[icd_in_range(UnderlyingCause, "R00", "R99"), garbage_flag := 1L]

dt[UnderlyingCause %in% c("D65", "I46", "J96"), garbage_flag := 4L]

garbage_5_codes <- c(
    "C80", "C26", "C39", "C57", "C64", "C76", "A49", "B83", "B99",
    "E88", "I51", "I99", "X59"
)
dt[UnderlyingCause %in% garbage_5_codes |
         icd_in_range(UnderlyingCause, "D00", "D13") |
         icd_in_range(UnderlyingCause, "D16", "D18") |
         icd_in_range(UnderlyingCause, "D20", "D24") |
         icd_in_range(UnderlyingCause, "D28", "D48") |
         icd_in_range(UnderlyingCause, "Y10", "Y34"),
     garbage_flag := 5L]

garbage_3_codes <- c(
    "A48", "I26", "I27", "I74", "I81", "J69", "J86", "J90", "J93", "J94",
    "J98", "K75", "K76", "M86", "N14", "K92"
)
dt[icd_in_range(UnderlyingCause, "A40", "A41") |
         UnderlyingCause %in% garbage_3_codes |
         icd_in_range(UnderlyingCause, "E85", "E87") |
         icd_in_range(UnderlyingCause, "G91", "G93") |
         icd_in_range(UnderlyingCause, "I44", "I45") |
         icd_in_range(UnderlyingCause, "I49", "I50") |
         icd_in_range(UnderlyingCause, "J80", "J81") |
         icd_in_range(UnderlyingCause, "K65", "K66") |
         icd_in_range(UnderlyingCause, "K71", "K72") |
         icd_in_range(UnderlyingCause, "N17", "N19"),
     garbage_flag := 3L]

garbage_2_codes <- c(
    "A31", "A59", "A60", "A63", "B00", "B07", "B08", "B30", "G54", "J30",
    "K14", "L94", "L98", "M03", "M07", "M35", "M40", "M43", "M45", "N39",
    "N40", "N46", "N60", "N97", "Q36", "Q38", "Q54", "B94", "Y86", "Y87",
    "Y89", "I10", "I15", "I70"
)
dt[UnderlyingCause %in% garbage_2_codes |
         icd_in_range(UnderlyingCause, "A71", "A74") |
         icd_in_range(UnderlyingCause, "B35", "B36") |
         icd_in_range(UnderlyingCause, "F32", "F33") |
         icd_in_range(UnderlyingCause, "F40", "F42") |
         icd_in_range(UnderlyingCause, "F45", "F48") |
         icd_in_range(UnderlyingCause, "F51", "F53") |
         icd_in_range(UnderlyingCause, "F60", "F98") |
         icd_in_range(UnderlyingCause, "G43", "G45") |
         icd_in_range(UnderlyingCause, "G47", "G52") |
         icd_in_range(UnderlyingCause, "G56", "G58") |
         icd_in_range(UnderlyingCause, "H00", "H69") |
         icd_in_range(UnderlyingCause, "H71", "H80") |
         icd_in_range(UnderlyingCause, "H83", "H93") |
         icd_in_range(UnderlyingCause, "J33", "J35") |
         icd_in_range(UnderlyingCause, "K00", "K11") |
         icd_in_range(UnderlyingCause, "L04", "L08") |
         icd_in_range(UnderlyingCause, "L20", "L25") |
         icd_in_range(UnderlyingCause, "L28", "L87") |
         icd_in_range(UnderlyingCause, "L90", "L92") |
         icd_in_range(UnderlyingCause, "M09", "M12") |
         icd_in_range(UnderlyingCause, "M14", "M25") |
         icd_in_range(UnderlyingCause, "M47", "M60") |
         icd_in_range(UnderlyingCause, "M63", "M71") |
         icd_in_range(UnderlyingCause, "M73", "M79") |
         icd_in_range(UnderlyingCause, "M95", "M99") |
         icd_in_range(UnderlyingCause, "N84", "N93") |
         icd_in_range(UnderlyingCause, "Q10", "Q18") |
         icd_in_range(UnderlyingCause, "Q65", "Q74") |
         icd_in_range(UnderlyingCause, "Q82", "Q84") |
         icd_in_range(UnderlyingCause, "G80", "G83"),
     garbage_flag := 2L]

garbage_labels <- c(
    "0" = "Not garbage",
    "1" = "Symptoms, signs and ill-defined conditions",
    "2" = "Impossible as underlying causes of death",
    "3" = "Intermediate causes of death",
    "4" = "Immediate causes of death",
    "5" = "Insufficiently specified causes within ICD chapters"
)
dt[, garbage_flag_label := factor(
    garbage_flag,
    levels = as.integer(names(garbage_labels)),
    labels = garbage_labels
)]









dt[, LGH_CauseGroup := UnderlyingCause]

# HIV (B24, B33)
# Tidyverse: mutate(covid = ifelse(LGH_CauseGroup %in% c("B24", "B33"), 1, covid))
dt[LGH_CauseGroup %in% c("B24", "B33"), covid := 1]

# Diabetes (E10-E14)
dt[LGH_CauseGroup %in% c("E10", "E11", "E12", "E13", "E14"), covid := 1]
dt[LGH_CauseGroup %in% c("E10", "E11", "E12", "E13", "E14"), LGH_CauseGroup := "E10-E14"]

# Hypertension (I10-I15)
dt[LGH_CauseGroup %in% c("I10", "I11", "I12", "I13", "I14", "I15"), covid := 1]
dt[LGH_CauseGroup %in% c("I10", "I11", "I12", "I13", "I14", "I15"), LGH_CauseGroup := "I10-I15"]

# Ischemic heart disease (I20-I25)
dt[LGH_CauseGroup %in% c("I20", "I21", "I22", "I23", "I24", "I25"), covid := 1]
dt[LGH_CauseGroup %in% c("I20", "I21", "I22", "I23", "I24", "I25"), LGH_CauseGroup := "I20-I25"]

# Pulmonary heart disease (I26-I28)
dt[LGH_CauseGroup %in% c("I26", "I27", "I28"), covid := 1]
dt[LGH_CauseGroup %in% c("I26", "I27", "I28"), LGH_CauseGroup := "I26-I28"]

# Cardiomyopathy (I42)
dt[LGH_CauseGroup == "I42", covid := 1]

# Heart failure (I49-I51)
dt[LGH_CauseGroup %in% c("I49", "I50", "I51"), covid := 1]
dt[LGH_CauseGroup %in% c("I49", "I50", "I51"), LGH_CauseGroup := "I49-I51"]

# Cerebrovascular diseases (I60-I69)
dt[LGH_CauseGroup %in% c("I60", "I61", "I62", "I63", "I64", "I65", "I66", "I67", "I68", "I69"), covid := 1]
dt[LGH_CauseGroup %in% c("I60", "I61", "I62", "I63", "I64", "I65", "I66", "I67", "I68", "I69"), LGH_CauseGroup := "I60-I69"]

# Pneumonia and influenza (J09-J18)
dt[LGH_CauseGroup %in% c("J09", "J10", "J11", "J12", "J13", "J14", "J15", "J16", "J17", "J18"), covid := 1]
dt[LGH_CauseGroup %in% c("J09", "J10", "J11", "J12", "J13", "J14", "J15", "J16", "J17", "J18"), LGH_CauseGroup := "J09-J18"]

# Acute bronchitis (J20-J22)
dt[LGH_CauseGroup %in% c("J20", "J21", "J22"), covid := 1]
dt[LGH_CauseGroup %in% c("J20", "J21", "J22"), LGH_CauseGroup := "J20-J22"]

# Asthma (J45)
dt[LGH_CauseGroup == "J45", covid := 1]

# Acute respiratory distress syndrome (J80)
dt[LGH_CauseGroup == "J80", covid := 1]

# Respiratory failure (J96-J98)
dt[LGH_CauseGroup %in% c("J96", "J97", "J98"), covid := 1]
dt[LGH_CauseGroup %in% c("J96", "J97", "J98"), LGH_CauseGroup := "J96-J98"]

# Renal failure (N17-N19)
dt[LGH_CauseGroup %in% c("N17", "N18", "N19"), covid := 1]
dt[LGH_CauseGroup %in% c("N17", "N18", "N19"), LGH_CauseGroup := "N17-N19"]

# Symptoms, signs, ill-defined conditions (R codes)
# strpos(UnderlyingCause,"R")==1 means string starts with "R"
# In R: substr(UnderlyingCause, 1, 1) == "R" or startsWith()
dt[startsWith(UnderlyingCause, "R"), covid := 1]

# Cardiac arrest (I46)
dt[LGH_CauseGroup == "I46", covid := 1]
dt[LGH_CauseGroup == "I46", LGH_CauseGroup := "R00-R99+I46"]

# All LGH_Main==18 should be R codes
dt[LGH_MainGroup == 18, LGH_CauseGroup := "R00-R99+I46"]

# COVID-19 (U07)
dt[LGH_CauseGroup == "U07", covid := 1]

# For non-covid causes, use first character of UnderlyingCause
# Tidyverse: mutate(LGH_CauseGroup = if_else(covid==0, substr(UnderlyingCause,1,1), LGH_CauseGroup))
dt[covid == 0, LGH_CauseGroup := substr(UnderlyingCause, 1, 1)]

dt$LGH_CauseGroup%>%unique()%>%length()
dt$LGH_Cause %>%unique() %>%length()
dt$LGH_Cause_encoded %>%unique()%>%length()


################################################################################
# SECTION 4: CREATE LABELS AND LGH_CAUSE
################################################################################

# Define labels for LGH_MainGroup
# In Stata: label define
# In R: we'll use factor levels
lgh_maingroup_labels <- c(
  "1" = "A00-B99",
  "2" = "C00-D48",
  "3" = "D50-D99",
  "4" = "E00-E90",
  "6" = "G00-G99",
  "9" = "I00-I99",
  "10" = "J00-J99",
  "14" = "N00-N99",
  "16" = "P00-P99",
  "18" = "R00-R99+I46",
  "20" = "Covid",
  "21" = "Others (F/H/K-M/O/Q)"
)

# If LGH_Cause is only 1 character, prefix with Z (to sort it last)
# Tidyverse: mutate(LGH_Cause = if_else(nchar(LGH_Cause)==1, "Z", LGH_Cause))
dt[nchar(LGH_CauseGroup) == 1, LGH_CauseGroup := "Z"]

# Assign broad cause categories based on LGH_Main for non-covid cases
dt[LGH_MainGroup == 1 & covid == 0, LGH_CauseGroup := "A00-B99"] # ICD codes for infectious diseases
dt[LGH_MainGroup == 2 & covid == 0, LGH_CauseGroup := "C00-D48"] # ICD codes for neoplasms
dt[LGH_MainGroup == 3 & covid == 0, LGH_CauseGroup := "D50-D99"] # ICD codes for blood diseases
dt[LGH_MainGroup == 4 & covid == 0, LGH_CauseGroup := "E00-E99*"] # ICD codes for endocrine/metabolic diseases
dt[LGH_MainGroup == 6 & covid == 0, LGH_CauseGroup := "G00-G99"] # ICD codes for nervous system diseases
dt[LGH_MainGroup == 9 & covid == 0, LGH_CauseGroup := "I00-I99*"] # ICD codes for circulatory system diseases
dt[LGH_MainGroup == 10 & covid == 0, LGH_CauseGroup := "J00-J99*"] # ICD codes for respiratory system diseases
dt[LGH_MainGroup == 14 & covid == 0, LGH_CauseGroup := "N00-N99*"] # ICD codes for genitourinary diseases
dt[LGH_MainGroup == 16 & covid == 0, LGH_CauseGroup := "P00-P99"] # ICD codes for perinatal conditions
dt[LGH_MainGroup == 20 & covid == 0, LGH_CauseGroup := "U07"] # ICD code for COVID-19
dt[LGH_MainGroup == 21 & covid == 0, LGH_CauseGroup := "ZZOthers (F/H/K-M/O/Q)"] # Others

# would like to look at Pulmonary embolism as a cause code to include ########################### Brians Flag

# CRITICAL FIX: Create LGH_Cause variable following Stata logic
# In Stata, LGH_Cause starts as a copy of LGH_CauseGroup, then gets replaced
# for covid==0 cases with broad categories based on LGH_MainGroup

# Initialize LGH_Cause as a copy of LGH_CauseGroup (keeps specific categories for covid==1)
dt[, LGH_Cause := LGH_CauseGroup]

# For non-covid-sensitive deaths (covid==0), replace with broad cause categories
# This matches Stata lines 55-66 which use: replace LGH_Cause = "..." if LGH_Main==X & covid==0
dt[LGH_MainGroup == 1 & covid == 0, LGH_Cause := "A00-B99"]
dt[LGH_MainGroup == 2 & covid == 0, LGH_Cause := "C00-D48"]
dt[LGH_MainGroup == 3 & covid == 0, LGH_Cause := "D50-D99"]
dt[LGH_MainGroup == 4 & covid == 0, LGH_Cause := "E00-E99*"]
dt[LGH_MainGroup == 6 & covid == 0, LGH_Cause := "G00-G99"]
dt[LGH_MainGroup == 9 & covid == 0, LGH_Cause := "I00-I99*"]
dt[LGH_MainGroup == 10 & covid == 0, LGH_Cause := "J00-J99*"]
dt[LGH_MainGroup == 14 & covid == 0, LGH_Cause := "N00-N99*"]
dt[LGH_MainGroup == 16 & covid == 0, LGH_Cause := "P00-P99"]
dt[LGH_MainGroup == 20 & covid == 0, LGH_Cause := "U07"]
dt[LGH_MainGroup == 21 & covid == 0, LGH_Cause := "ZZOthers (F/H/K-M/O/Q)"]

# Encode LGH_Cause as factor (equivalent to Stata's: encode LGH_Cause, gen(LGH_Cause_encoded))
# This creates a numeric encoding of the cause categories
dt[, LGH_Cause_encoded := as.numeric(factor(LGH_Cause))]

cat("Unique LGH_Cause_encoded values:", length(unique(dt$LGH_Cause_encoded)), "\n")
cat("Covid-sensitive causes:", dt[covid == 1, .N], "deaths\n")
cat("Non-covid-sensitive causes:", dt[covid == 0, .N], "deaths\n")

# Show a sample of the cause categories
cat("\nSample of LGH_Cause categories:\n")
print(dt[, .N, by = .(LGH_Cause, covid)][order(-N)][1:20])
cat("\n")

dt$LGH_MainGroup%>%unique()%>%length()
dt$LGH_Cause %>%unique() %>%length()
dt$LGH_Cause_encoded %>%unique()%>%length()
dt$LGH_CauseGroup %>%unique()%>%length()


# Create the map of cause categories to their encoded values
cause_map <- dt[, .(LGH_Cause, LGH_Cause_encoded)]%>%unique
cause_map
setkey(cause_map, LGH_Cause)

################################################################################
# SECTION 5: COLLAPSE (AGGREGATE) DATA
################################################################################

# IMPORTANT NOTE ON CAUSE CATEGORIZATION:
# - For covid==1 (covid-sensitive deaths): LGH_Cause retains LGH_CauseGroup values with specific 
#   categories like "E10-E14" (diabetes), "I10-I15" (hypertension), "J09-J18" (pneumonia)
# - For covid==0 (non-sensitive deaths): LGH_Cause is replaced with broad categories 
#   like "A00-B99", "C00-D48", etc. based on LGH_MainGroup
# - LGH_Cause_encoded is the numeric encoding used for modeling

# Collapse = aggregate/summarize by grouping variables
# Tidyverse: group_by(...) %>% summarize(count = sum(count))
# data.table: [, .(count = sum(count)), by = .(var1, var2, ...)]
#save( dt, file = "LGH_MasterFile_preCollapsed2022.RData")

write_feather(dt, "LGH_MasterFile_preCollapsedAll.feather")


dt <- dt[epi_year >= 2014 & epi_year <= 2022]


dt %>% glimpse()
write_feather(dt, "LGH_MasterFile_preCollapsed2022.feather")



dt_collapsed <- dt[, .(count = sum(count)), 
                   by = .(agegroup, sex, LGH_Cause, LGH_Cause_encoded, covid, 
                         weekstart, epi_week, epi_year)]



tidyr::complete( 
    dt_collapsed,
    epi_week, epi_year, sex, agegroup, LGH_Cause_encoded, fill = list(count = 0)
)%>%as.data.table() -> dt_collapsed



# Diagnostic: Check that we have data for both covid==0 and covid==1
cat("\n=== DIAGNOSTIC CHECK ===\n")
cat("Total collapsed rows:", nrow(dt_collapsed), "\n")
cat("Collapsed data with covid==0:", dt_collapsed[covid == 0, .N], "\n")
cat("Collapsed data with covid==1:", dt_collapsed[covid == 1, .N], "\n")
cat("Unique LGH_Cause_encoded in collapsed data:", length(unique(dt_collapsed$LGH_Cause_encoded)), "\n")
cat("========================\n\n")

# Save collapsed data
# haven::write_dta() for Stata format
write_dta(dt_collapsed, "LGH_MasterFile_collapsed2022.dta")
# Also save as R data for faster loading
saveRDS(dt_collapsed, "LGH_MasterFile_collapsed2022.rds")

################################################################################
# SECTION 6: CREATE BASELINE (2015-2019)
################################################################################
dt_collapsed$count%>%sum() 
nrow(dt)
# Note: The original Stata code has some inconsistencies in variable names
# (uses 'age' vs 'agegroup', 'LGH' vs 'LGH_Cause_encoded')
# I'll follow the logic as closely as possible

# Load collapsed data and filter to baseline years
dt_baseline <- copy(dt_collapsed)  # copy() prevents reference issues
dt_baseline <- dt_baseline[epi_year >= 2015 & epi_year <= 2019, ]

dt_baseline$LGH_Cause%>%unique()%>%length()
dt_collapsed$LGH_Cause%>%unique()%>%length()

setdiff(
    dt_collapsed$LGH_Cause %>% unique(),
    dt_baseline$LGH_Cause %>% unique()
)
# U07 is COVID, which shoudl not be in baseline 

# Aggregate for baseline model
# Note: Original uses 'age' and 'LGH' but we have 'agegroup' and 'LGH_Cause_encoded'
dt_for_model <- dt_baseline[, .(count = sum(count)), 
                             by = .(epi_week, epi_year, sex, agegroup, LGH_Cause, LGH_Cause_encoded)]

dt_for_model
dt_for_model$LGH_Cause %>% table()
dt_for_model$LGH_Cause %>% unique() %>% length()


dt_for_model
dt_for_model[agegroup == "40-59" & sex == 1  ]%>%
    ggplot() + 
    geom_line( 
        aes( 
            x = epi_week,
            y = count,
            color = as.factor(LGH_Cause), 
        ), 
        show.legend = FALSE
    )+
    facet_wrap(
        ~ epi_year
    )+ theme_minimal()

################################################################################
# SECTION 7: NEGATIVE BINOMIAL REGRESSION FOR BASELINE
################################################################################

cat("Fitting negative binomial regression model for baseline...\n")
cat("This may take several minutes...\n")

# Filter out week 53 for model fitting
dt_model_train <- dt_for_model[epi_week != 53]


#LGH_Cause_base_line_factor_levels <- dt_model_train$LGH_Cause %>% unique() %>% sort()

# Create interaction terms for the model
# In Stata: i.sex#i.agegroup#i.LGH i.epi_week#i.LGH
# In R: use factor() and * for interactions
dt_model_train[, sex_f := factor(sex)]
dt_model_train[, agegroup_f := factor(agegroup)]
dt_model_train[, LGH_f := factor(LGH_Cause)]
dt_model_train[, epi_week_f := factor(epi_week)]

dt_model_train$LGH_f %>% table()
dt_model_train$count %>% is.na() %>% table()
dt_model_train$sex_f %>% is.na() %>% table()
dt_model_train$agegroup_f %>% is.na() %>% table()
dt_model_train$epi_week_f %>% is.na() %>% table()

# Consider complete() to make sure all combinations exist


# Fit negative binomial regression
# This is computationally intensive!
# Formula: count ~ epi_year + sex:agegroup:LGH + epi_week:LGH

# Look at faster implementations on a sample first, look at two consecutive years

dt_model_train_sample <- dt_model_train[epi_year %in% c(2018, 2019)]

# cause codes
dt_model_train_sample$LGH_f %>% table()
dt_model_train_sample$LGH_f %>%
    unique() %>%
    length()

# sex and agegroup
xtabs( ~ sex_f + agegroup_f, data = dt_model_train_sample)


# ========================================
# MODELLING START 
# ========================================

pacman::p_load(glmmTMB)

# Run the normal glm.nb on the sample to compare time taken

    # first glmmTMB
        start_time <- Sys.time()
            glmmTMB(count ~ epi_year + 
                sex_f:agegroup_f:LGH_f + 
                epi_week_f:LGH_f,
                data = dt_model_train_sample,
                family = nbinom2(link = "log")
            )-> model_sample
        end_time <- Sys.time()
        time_taken_for_model_sample <- difftime(end_time ,  start_time, units = "mins")
        cat("Time taken for sample model fitting: From", start_time, "to", end_time, ", therefore", time_taken_for_model_sample, "\n")

        # test prediction by just modelling on itself 
        predict(model_sample, newdata = dt_model_train_sample, type = "link") -> self_predicted_TMB

    # Now normal glm.ng() 
        start_time <- Sys.time()
            glm.nb(
                count ~ epi_year + sex_f:agegroup_f:LGH_f + epi_week_f:LGH_f,
                data = dt_model_train_sample,
            )-> model_sample_glm
        end_time <- Sys.time()
        time_taken_for_model_sample_glm <- difftime(end_time ,  start_time, units = "mins")
        cat("Time taken for sample glm.nb model fitting: From", start_time, " to", end_time, ", therefore", time_taken_for_model_sample_glm, "\n")

        # test prediction by just modelling on itself
        predict(model_sample_glm, newdata = dt_model_train_sample, type = "link") -> self_predicted_glm



# can you predict anything from the sample model?
exp(predict(model_sample, newdata = .SD, type = "link"))-> prediction

prediction

# Decide whether to run a full model 
if(TRUE){

# Decide which model to run
    cat("Skipping full model fitting as per user setting.\n")

    # load the glmnb or glmmTMB model. 
    load( "LGH_NegativeBinomialModel_2015_2019_glmnb.rda") # this is the standard model 
    load( "LGH_NegativeBinomialModel_2015_2019_glmmTMB.RData") # this is a fast running one but strggling with factors 

}else{


    if(TRUE){
                # then normal glm.nb
                start_time <- Sys.time()
                    model_sample <- glm.nb(
                        count ~ epi_year +
                            sex_f:agegroup_f:LGH_f +
                            epi_week_f:LGH_f,
                        data = dt_model_train_sample,
                    )
                end_time <- Sys.time()
                time_taken_for_model_sample_glm <- difftime(end_time ,  start_time, units = "mins")
                cat("Time taken for sample glm.nb model fitting: From", start_time, " to", end_time, ", therefore", time_taken_for_model_sample_glm, "\n")

                start_time <- Sys.time()

                    model <- glm.nb(count ~ epi_year + 
                                    sex_f:agegroup_f:LGH_f + 
                                    epi_week_f:LGH_f,
                                    data = dt_model_train)

                end_time <- Sys.time()

                time_taken_for_model <- difftime(end_time ,  start_time, units = "mins")
                cat("Time taken for model fitting: From", start_time, "to", end_time, ", therefore", time_taken_for_model, "\n")

                cat("Model fitting complete!\n")

    }else{
        # use the glmmTMB model for the full data
            start_time <- Sys.time()
            model <- glmmTMB(count ~ epi_year +
                                sex_f:agegroup_f:LGH_f +
                                epi_week_f:LGH_f,
                            data = dt_model_train, 
                                family = nbinom2(link = "log"))
            end_time <- Sys.time()
            time_taken_for_model <- difftime(end_time ,  start_time, units = "mins")
            cat("Time taken for glmmTMB model fitting: From", start_time, " to", end_time, ", therefore", time_taken_for_model, "\n")
            cat("Model fitting complete!\n")
    }
}
#save( model, file = "LGH_NegativeBinomialModel_2015_2019_glmmTMB.RData")
#load( "LGH_NegativeBinomialModel_2015_2019_glmmTMB.RData")

#save( model , file = "LGH_NegativeBinomialModel_2015_2019_glmnb.rda")
save( model, file = "LGH_NegativeBinomialModel_2015_2019_glmnb.rda")
load( "LGH_NegativeBinomialModel_2015_2019_glmnb.rda")
# show the modelled results and the actual to get a residual some diagnostic stats

# ========================================
# OTHER MODELS 
# ========================================
# Other models ideas 
# Simple model 

dt_collapsed <- dt[, .(count = sum(count)),
    by = .(
        agegroup, sex, LGH_Cause, LGH_Cause_encoded, covid,
        weekstart, epi_week, epi_year
    )
]

dt_collapsed$epi_year %>% unique()


dt[epi_week != 53 , .(count = sum(count)), 
    by=.( sex, agegroup, LGH_Cause, LGH_Cause_encoded, weekstart, epi_week, epi_year,  DeathProvince)] -> dt_collapse_prov

dt_collapse_prov$week_start <- as.Date(paste0(dt_collapse_prov$epi_year, "-01-01")) + (dt_collapse_prov$epi_week - 1) * 7


# Create factors 
dt_collapse_prov[, sex_f := factor(sex)]
dt_collapse_prov[, agegroup_f := factor(agegroup)]
dt_collapse_prov[, LGH_f := factor(LGH_Cause)]
dt_collapse_prov[, province_f := factor(DeathProvince)]
dt_collapse_prov[, epi_week_f := factor(epi_week)]

# add in the population 
load("pop_collapsed_prov.RData") # loads dt_pop
pop_collapsed_prov

# validation check on the matching levels 
    # Province 
        setdiff(  
            dt_collapse_prov$province_f %>% unique(),
            pop_collapsed_prov$province_f %>% unique()
        )
    # agegroup
        setdiff(
            dt_collapse_prov$agegroup_f %>% unique(),
            pop_collapsed_prov$agegroup_f %>% unique()
        )
    # sex
        setdiff(
            dt_collapse_prov$sex_f %>% unique(),
            pop_collapsed_prov$sex_f %>% unique()
        )

dt_collapse_prov%>%na.omit() 
dt_collapse_prov[pop_collapsed_prov, on = .(epi_year, sex_f, agegroup_f, province_f)] -> dt_collapse_prov
dt_collapse_prov%>%na.omit() -> dt_collapse_prov

# add fourier terms 
wk <- as.integer(dt_collapse_prov$epi_week) # 1..52
dt_collapse_prov$sin1 <- sin(2 * pi * wk / 52)
dt_collapse_prov$cos1 <- cos(2 * pi * wk / 52)
dt_collapse_prov$sin2 <- sin(4 * pi * wk / 52)
dt_collapse_prov$cos2 <- cos(4 * pi * wk / 52) # optional 2nd harmonic

dt_collapse_prov[epi_year %in% 2015:2019] -> dt_model_train_prov

# plot all data 
dt_collapse_prov[, 
.( count = sum(count) ),
by=.( province_f, week_start, epi_year, epi_week )
]%>%
    ggplot() + 
    geom_line( 
        aes( 
            x = week_start,
            y = count,
            color = as.factor(province_f), 
        ), 
        show.legend = FALSE
    )+ 
    facet_wrap(
        ~ province_f
    )+ theme_minimal()

# plot training data only
dt_model_train_prov[,
    .(count = sum(count)),
    by = .(province_f, week_start, epi_year, epi_week)
] %>%
    ggplot() +
    geom_line(
        aes(
            x = week_start,
            y = count,
            color = as.factor(province_f),
        ),
        show.legend = FALSE
    ) +
    facet_wrap(
        ~province_f
    ) +
    theme_minimal()

# plot the population in each province
dt_model_train_prov[,
    .(pop = unique(pop)),
    by = .(province_f, week_start, epi_year, epi_week)
] %>%
    ggplot() +
    geom_line(
        aes(
            x = week_start,
            y = pop,
            color = as.factor(province_f),
        ),
        show.legend = FALSE
    ) +
    facet_wrap(
        ~province_f
    ) +
    theme_minimal()

# plot mortality rates per province 
dt_model_train_prov[,
    .(count = sum(count), pop = unique(pop)),
    by = .(province_f, week_start, epi_year, epi_week)
][,
    mortality_rate := count / pop * 100000
] %>%
    ggplot() +
    geom_line(
        aes(
            x = week_start,
            y = mortality_rate,
            color = as.factor(province_f),
        ),
        show.legend = FALSE
    ) +
    facet_wrap(
        ~province_f
    ) +
    theme_minimal()


if(FALSE){
    print("Starting simple models fitting...")
model_simple<- glm.nb(count ~ province_f + sex_f + agegroup_f + LGH_Cause_encoded + epi_week_f + epi_year,
                     data = dt_model_train_prov)
                     save( model_simple , file = "LGH_NegativeBinomialModel_2015_2019_ProvinceSimple_glmnb.rda")
       
        print("Starting fourier models fitting...")
model_simple_fourier <- glm.nb(count ~  province_f  + sex_f + agegroup_f + LGH_Cause_encoded+ sin1+ sin2 + cos1 + cos2,
                     data = dt_model_train_prov)
                     save( model_simple_fourier , file = "LGH_NegativeBinomialModel_2015_2019_ProvinceSimpleFourier_glmnb.rda")
        
        print("Starting interaction models fitting...")
model_simple_interactions <- glm.nb(count ~ province_f + sex_f:agegroup_f:LGH_Cause_encoded + epi_week_f:LGH_Cause_encoded+ epi_year,
                          data = dt_model_train_prov)
                    save( model_simple_interactions , file = "LGH_NegativeBinomialModel_2015_2019_ProvinceSimpleInteractions_glmnb.rda")
        
        print("Starting population offset models fitting...")
model_simple_pop <- glm.nb(count ~ offset(log(pop)) + province_f + sex_f + agegroup_f + LGH_Cause_encoded + epi_week_f + epi_year,
                     data = dt_model_train_prov)
                     save( model_simple_pop , file = "LGH_NegativeBinomialModel_2015_2019_ProvinceSimplePopOffset_glmnb.rda")

        print("Starting fourier interaction population offset models fitting...")
model_fourier_interactions_pop_offset <- glm.nb(count ~ 
                    offset(log(pop)) + sin1 + cos1 + sin2+ cos2+  province_f:(sin1+ cos1) + sex_f + agegroup_f + LGH_Cause_encoded:province_f ,
                                        data = dt_model_train_prov)
                     save( model_fourier_interactions_pop_offset , file = "LGH_NegativeBinomialModel_2015_2019_ProvinceFourierInteractionsPopOffset_glmnb.rda")
}else{
    load( "LGH_NegativeBinomialModel_2015_2019_ProvinceSimple_glmnb.rda")
    load( "LGH_NegativeBinomialModel_2015_2019_ProvinceSimpleFourier_glmnb.rda")
    load( "LGH_NegativeBinomialModel_2015_2019_ProvinceSimpleInteractions_glmnb.rda")
    load( "LGH_NegativeBinomialModel_2015_2019_ProvinceSimplePopOffset_glmnb.rda")
    load( "LGH_NegativeBinomialModel_2015_2019_ProvinceFourierInteractionsPopOffset_glmnb.rda")
}


# try glmmTMB again 
library(glmmTMB)
fit_tmb <- glmmTMB(
  count ~ province_f + poly(epi_year,2) +
          sin1 + cos1 + sin2 + cos2 +
          province_f:(sin1 + cos1) +
          sex_f + agegroup_f + LGH_f +
          offset(log(pop)),
  family = nbinom2(),                 # try nbinom1 too; sometimes better
  dispformula = ~ province_f,           # start modest; expand if needed
  data = dt_model_train_prov
)


# models 
model
model_simple
model_simple_fourier
model_simple_interactions
model_simple_pop
model_fourier_interactions_pop_offset

AIC(model_simple, model_simple_fourier, model_simple_interactions, model_simple_pop, model_fourier_interactions_pop_offset)
BIC(model_simple, model_simple_fourier, model_simple_interactions, model_simple_pop, model_fourier_interactions_pop_offset)


# Model simple pop diagnsotics 
pacman::p_load(DHARMa)
sim <- simulateResiduals(model_simple_pop) # from MASS::glm.nb
plot(sim)
testDispersion(sim)
testZeroInflation(sim)


predict_glm.nb_with_ci <- function(model, newdata, type = "link", level = 0.95) {
    # Get predictions on the link scale
    link_pred <- predict(model, newdata = newdata, type = type, se.fit = TRUE)
    
    # Calculate the critical value for the normal distribution
    crit_value <- qnorm((1 + level) / 2)
    
    # Calculate the confidence intervals on the link scale
    link_lower <- link_pred$fit - crit_value * link_pred$se.fit
    link_upper <- link_pred$fit + crit_value * link_pred$se.fit
    
    # Transform back to response scale
    response_pred <- exp(link_pred$fit)
    response_lower <- exp(link_lower)
    response_upper <- exp(link_upper)
    
    # Combine into a data.table
    result <- data.table(
        .row_id = 1:nrow(newdata),
        week_start = newdata$week_start,
        fit = response_pred,
        lower = response_lower,
        upper = response_upper
    )
    
    return(result)
}

# use function to predict on province level data
# model_simple 
predict_glm.nb_with_ci(model_simple, newdata = dt_collapse_prov)-> simple_model_preds

dt_collapse_prov$.row_id <- 1:nrow(dt_collapse_prov)

dt_collapse_prov[simple_model_preds, on = .(.row_id)
]-> dt_collapse_prov



dt_collapse_prov[, pred_simple := predict(model_simple, newdata = dt_collapse_prov, type = "response", allow.new.levels = TRUE)]
dt_collapse_prov[, pred_simple_fourier := predict(model_simple_fourier, newdata = dt_collapse_prov, type = "response", allow.new.levels = TRUE)]
dt_collapse_prov[, pred_simple_interactions := predict(model_simple_interactions, newdata = dt_collapse_prov, type = "response", allow.new.levels = TRUE)]
dt_collapse_prov[, pred_simple_pop := predict(model_simple_pop, newdata = dt_collapse_prov, type = "response", allow.new.levels = TRUE)]
dt_collapse_prov[, pred_fourier_interactions_pop_offset := predict(model_fourier_interactions_pop_offset, newdata = dt_collapse_prov, type = "response", allow.new.levels = TRUE)]

write_feather(dt_collapse_prov, "LGH_BaselinePredictions_ProvinceLevel_TempPop.feather")


dt_collapse_prov[, 
    .(count = sum(count), 
      pred_simple = sum(pred_simple), 
      pred_simple_fourier = sum(pred_simple_fourier),
      pred_simple_interactions = sum(pred_simple_interactions),
      pred_simple_pop = sum(pred_simple_pop), 
    pred_fourier_interactions_pop_offset = sum(pred_fourier_interactions_pop_offset)
    ),
    by = .( province_f, week_start )
]%>%
    ggplot() + 
    geom_line( 
        aes( 
            x = week_start,
            y = count,
            color = "Actual"
        )
    ) + 
    geom_line( 
        aes( 
            x = week_start,
            y = pred_simple,
            color = "Simple Model"
        )
    ) + 
    geom_line( 
        aes( 
            x = week_start,
            y = pred_simple_fourier,
            color = "Fourier Model"
        )
    ) + 
    geom_line( 
        aes( 
            x = week_start,
            y = pred_simple_interactions,
            color = "Interactions Model"
        )
    ) +
    geom_line( 
        aes( 
            x = week_start,
            y = pred_simple_pop,
            color = "Population Offset Model"
        )
    ) +
    geom_line( 
        aes( 
            x = week_start,
            y = pred_fourier_interactions_pop_offset,
            color = "Fourier Interactions Population Offset Model"
        )
    ) +
    facet_wrap(
        ~ province_f, 
        scales = "free_y", 
        nrow = 10
    )

# diagnostic between fourier model and simple model
dt_collapse_prov[epi_year <2020,
    .(
        count = sum(count),
        pred_simple = sum(pred_simple),
        pred_simple_fourier = sum(pred_simple_fourier), 
        pred_simple_interactions = sum(pred_simple_interactions), 
        pred_simple_pop = sum(pred_simple_pop)
    ),
    by = .(province_f, week_start)
][,
    .(
        rmse_simple = sqrt(mean((count - pred_simple)^2)),
        rmse_fourier = sqrt(mean((count - pred_simple_fourier)^2)), 
        rmse_interactions = sqrt(mean((count - pred_simple_interactions)^2)), 
        rmse_simple_pop = sqrt(mean((count - pred_simple_pop)^2))
    ),
    by = province_f
]



# Plot the difference from actual to modelled data for each province

dt_collapse_prov[epi_year <2020, 
    .(count = sum(count), 
      pred_simple = sum(pred_simple), 
      pred_simple_fourier = sum(pred_simple_fourier),
      pred_simple_interactions = sum(pred_simple_interactions),
      pred_simple_pop = sum(pred_simple_pop), 
      pred_fourier_interactions_pop_offset = sum(pred_fourier_interactions_pop_offset)
    ),
    by = .( province_f, week_start )
][
    ,
    .(
        diff_simple = count - pred_simple,
        diff_fourier = count - pred_simple_fourier,
        diff_interactions = count - pred_simple_interactions, 
        diff_simple_pop = count - pred_simple_pop, 
        diff_fourier_interactions_pop_offset = count - pred_fourier_interactions_pop_offset
    ),
    by = .(province_f, week_start)
]%>%
melt( 
    id.vars = c("province_f", "week_start"),
    variable.name = "model",
    value.name = "difference"
) %>%
    ggplot() + 
    geom_line( 
        aes( 
            x = week_start,
            y = difference,
            color = model
        )
    ) + 
    facet_grid(
        province_f ~ model, 
        scales = "free_y"
    )


# look at a specific code by provinces to assess model fit
dt_collapse_prov[LGH_Cause == "J09-J18",
    .(
        count = sum(count), 
        baseline = sum(pred_simple)),
    by = .(DeathProvince, week_start, epi_week, epi_year)
] %>%
    ggplot() +
    geom_line(
        aes(
            x = week_start,
            y = count,
            color = "Observed"
        )
    ) +
    geom_line(
        aes(
            x = week_start,
            y = baseline,
            color = "Expected"
        )
    ) +
    scale_color_manual(
        values = c("Expected" = "blue", "Observed" = "red")
    ) +
    facet_wrap(
        ~DeathProvince,
        scales = "free_y",
        nrow = 4
    ) +
    theme_minimal()









# it doesnt appear like the fourier or complex interactiosn model is any better. 
# we can perhaps still ad temperature, and population 
dt_collapse_prov[, baseline := sum(pred_simple)]



dt_template_full <- CJ(
  province_f = unique(dt_model_train_prov$province_f),
  epi_year_f= unique(dt_model_train_prov$epi_year),
  epi_week_f = unique(dt_model_train_prov$epi_week_f),
  week_start = unique(dt_model_train_prov$week_start),
  agegroup_f = unique(dt_model_train_prov$agegroup_f),
  sex_f = unique(dt_model_train_prov$sex_f),
  LGH_f = unique(dt_model_train_prov$LGH_f)
)
# add fourier terms 
wk <- as.integer(dt_template_full$epi_week_f) # 1..52
dt_template_full$sin1 <- sin(2 * pi * wk / 52)
dt_template_full$cos1 <- cos(2 * pi * wk / 52)
dt_template_full$sin2 <- sin(4 * pi * wk / 52)
dt_template_full$cos2 <- cos(4 * pi * wk / 52) #

# predict on full template
dt_template_full$epi_year_f %>% unique()
dt_template_full[, pred_simple := predict(model_simple, newdata = dt_template_full, type = "response")]

dt_template_full[, pred_simple_fourier := predict(model_simple_fourier, newdata = .SD, type = "link")]

write_feather(dt_template_full, "LGH_BaselinePredictions_ProvinceLevel.feather")

# plot actual and two modelled baselines
dt_template_full[, 
    .(count = sum(exp(pred_simple)), 
      count_fourier = sum(exp(pred_simple_fourier)) ),
    by = .( province_f, week_start )
]
    ggplot() + 
    geom_line( 
        aes( 
            x = as.integer(epi_week_f),
            y = exp(pred_simple),
            color = "Simple Model"
        )
    ) + 
    geom_line( 
        aes( 
            x = as.integer(epi_week_f),
            y = exp(pred_simple_fourier),
            color = "Fourier Model"
        )
    ) + 
    facet_wrap(
        ~ province_f
    ) 



################################################################################
# SECTION 8: CREATE BASELINE TEMPLATE AND PREDICT
################################################################################

# Create a template for all combinations
# Note: The Stata code merges with a pre-existing template
# We'll create the template here by expanding the grid

# Load or create template
# If template exists, load it; otherwise create it
template_file <- "LGH_MasterFile_baseline_template_2022.dta"

if (file.exists(template_file)) {
  dt_template <- as.data.table(read_dta(template_file))
} else {
  # Create complete combinations grid
  dt_template <- CJ(
    sex = unique(dt_collapsed$sex),
    epi_week = 1:53,
    epi_year = 2014:2022,
    agegroup = unique(dt_collapsed$agegroup),
    LGH_Cause_encoded = unique(dt_collapsed$LGH_Cause_encoded)
  )
  dt_template[, weekstart := as.Date(paste0(epi_year, "-01-01")) + (epi_week - 1) * 7]
}

dt_template <- as.data.table(read_dta(template_file))

# Drop 2014
dt_template <- dt_template[epi_year != 2014]

dt_template %>%names() 

dt_template$LGH_Cause_encoded %>% unique() 
dt_template <- dt_template[epi_year != 2014]
dt_template$agegroup%>%table
dt_template$epi_week%>%unique
dt_template$epi_year %>% unique()
# CRITICAL: Filter template to only include factor levels that exist in training data
# This prevents "unknown fixed effects" errors during prediction
cat("\n=== Filtering template to match training data ===\n")
cat("Original template rows:", nrow(dt_template), "\n")

# Only keep combinations that exist in the training data
valid_sex <- unique(dt_model_train$sex)
valid_agegroup <- unique(dt_model_train$agegroup)
valid_LGH <- unique(dt_model_train$LGH_Cause) # this may miss the U07 code for COVID 
valid_week <- unique(dt_model_train$epi_week)

cat("Valid sex levels:", paste(valid_sex, collapse=", "), "\n")
cat("Valid agegroup levels:", paste(valid_agegroup, collapse=", "), "\n")
cat("Valid LGH_Cause levels:", length(valid_LGH), "codes\n")
cat("Valid epi_week levels:", paste(valid_week, collapse=", "), "\n")
cat("Filtered template rows:", nrow(dt_template), "\n\n")

# Add factor columns for prediction
dt_template[, sex_f := factor(sex, levels = levels(dt_model_train$sex_f))]

dt_template[, agegroup_f := factor(
        dplyr::case_when( 
            agegroup == 0 ~ "0-4",
            agegroup == 5 ~ "5-39",
            agegroup == 40 ~ "40-59",
            agegroup == 60 ~ "60-69",
            agegroup == 70 ~ "70-79",
            agegroup == 80 ~ "80+"
        ), levels = levels(dt_model_train$agegroup_f)
    )]

dt_template[, agegroup := as.character(agegroup_f)] # will need to be a character that matches the age group selection later on in the merge() 

# map numeric LGH_Cause_encoded back to the factor levels used in the training data
    lvl <- levels(dt_model_train$LGH_f)
    lvl

    # Map numeric encoded back to LGH factor using the previously built cause_map
    cause_map_enc <- unique(cause_map[, .(LGH_Cause, LGH_Cause_encoded)])
    setkey(cause_map_enc, LGH_Cause_encoded)

    # Lookup LGH_Cause by encoded value and create factor with same levels as training
    # Use direct keyed lookup to map encoded values back to names (avoids wrong number of items error)
    dt_template[, LGH_name := cause_map_enc[LGH_Cause_encoded, LGH_Cause]]
    dt_template[, LGH_f := factor(LGH_name, levels = lvl)]

    # remove temporary helper column
    dt_template[, LGH_name := NULL]
    xtabs(~ LGH_f+LGH_Cause_encoded, data = dt_template)

# Add epi_week factor
# NOTE: Week 53 will become NA since it's not in training levels - this is intentional
# We'll predict for weeks 1-52, then interpolate week 53 separately
dt_template[, epi_week_f := factor(epi_week, levels = levels(dt_model_train$epi_week_f))]

# Check how many week 53 rows exist
cat("\nWeek 53 check:\n")
cat("Total rows with epi_week==53:", dt_template[epi_week == 53, .N], "\n")
cat("Rows with NA epi_week_f (should match week 53 count):", dt_template[is.na(epi_week_f), .N], "\n\n")

# Filter template to only valid combinations
dt_template <- dt_template[
    LGH_f %in% valid_LGH
]

# so the df_template has all the cause codes except for COVID-19 (U07), perhaps need to check ICD code for MIS-C ? 
    # COVID = U07
    # MIS-C is coded as M35.8 in ICD-10 but perhaps also need to check for U10.9 (PIMS-TS) in children


# CRITICAL CHECK: Remove any rows with NA factor levels
# These would cause prediction errors
# NOTE: Week 53 will have NA in epi_week_f, but we keep it for interpolation
cat("\n=== Checking for NA factor levels ===\n")
cat("NA in sex_f:", sum(is.na(dt_template$sex_f)), "\n")
cat("NA in agegroup_f:", sum(is.na(dt_template$agegroup_f)), "\n")
cat("NA in LGH_f:", sum(is.na(dt_template$LGH_f)), "\n")
cat("NA in epi_week_f (should be week 53 only):", sum(is.na(dt_template$epi_week_f)), "\n")

# Remove rows with NA in sex, agegroup, or LGH (but keep week 53 with NA epi_week_f)
dt_template <- dt_template[!is.na(sex_f) & !is.na(agegroup_f) & !is.na(LGH_f)]
cat("Rows after removing NA factors (keeping week 53):", nrow(dt_template), "\n")
cat("Week 53 rows remaining:", dt_template[epi_week == 53, .N], "\n\n")

# remove the factor level if it is NA

model
dt_template

# DIAGNOSTIC: Check for mismatches between training and template data
cat("\n=== FACTOR LEVEL COMPARISON ===\n")
cat("Training data factor levels:\n")
cat("  sex:", paste(levels(dt_model_train$sex_f), collapse=", "), "\n")
cat("  agegroup:", paste(levels(dt_model_train$agegroup_f), collapse=", "), "\n")
cat("  LGH:", length(levels(dt_model_train$LGH_f)), "levels\n")
cat("  epi_week:", paste(levels(dt_model_train$epi_week_f), collapse=", "), "\n\n")

cat("Template data factor levels:\n")
cat("  sex:", paste(levels(dt_template$sex_f), collapse=", "), "\n")
cat("  agegroup:", paste(levels(dt_template$agegroup_f), collapse=", "), "\n")
cat("  LGH:", length(levels(dt_template$LGH_f)), "levels\n")
cat("  epi_week:", paste(levels(dt_template$epi_week_f), collapse=", "), "\n\n")

# Check for LGH codes in template but not in training
template_LGH_not_in_train <- setdiff(unique(dt_template$LGH_Cause_encoded), unique(dt_model_train$LGH_Cause_encoded))
if(length(template_LGH_not_in_train) > 0) {
  cat("WARNING: LGH codes in template but NOT in training data:", paste(template_LGH_not_in_train, collapse=", "), "\n")
  cat("These will be removed to avoid prediction errors.\n")
  dt_template <- dt_template[!LGH_Cause_encoded %in% template_LGH_not_in_train]
  cat("Template rows after removal:", nrow(dt_template), "\n\n")
}
cat("===================================\n\n")

# Predict baseline (on log scale, then exponentiate)
# NOTE: glmmTMB predict syntax is different from glm.nb
# For glmmTMB: 
#   - type = "link" gives log scale (eta = X*beta)
#   - type = "response" gives exp(link), i.e., the count scale (mu = exp(eta))
# For glm.nb:
#   - type = "link" gives log scale
#   - Must manually exponentiate: exp(predict(..., type = "link"))
# We need to handle the prediction differently for glmmTMB vs glm.nb

# IMPORTANT: glmmTMB doesn't work well with .SD in data.table
# Must pass the full data.table as newdata parameter
dt_template[epi_year %in% 2020]%>%na.omit() -> test_data_2020

# the test data needs to only have the values that were used in training

predict(model, newdata = test_data_2020, type = "link", allow.new.levels = TRUE) -> test_pred


test_data_2020[sex_f ==2 &agegroup =="60-69" & LGH_f== "P00-P99"]

cat("Generating baseline predictions...\n")
cat("Note: Week 53 will be interpolated after prediction\n")
start_time <- Sys.time()

# Create a dataset for prediction (excluding week 53 which has NA epi_week_f)
dt_for_prediction <- dt_template[epi_week != 53]

cat("Predicting for", nrow(dt_for_prediction), "rows (weeks 1-52 only)\n")

# Check which model type we're using
if(inherits(model, "glmmTMB")) {
  cat("Using glmmTMB prediction method...\n")
  
  # Wrap prediction in tryCatch to handle errors gracefully
  tryCatch({
    # For glmmTMB, predict on weeks 1-52 only
    # type = "response" automatically exponentiates (gives count scale)
    dt_template[epi_week != 53, baseline := predict(model, newdata = dt_for_prediction, type = "response", allow.new.levels = TRUE)]
  }, error = function(e) {
    cat("\n!!! PREDICTION ERROR !!!\n")
    cat("Error message:", conditionMessage(e), "\n")
    cat("\nTrying alternative prediction method...\n")
    
    # Alternative: predict in smaller chunks or use type = "link"
    dt_template[epi_week != 53, baseline := exp(predict(model, newdata = dt_for_prediction, type = "link", allow.new.levels = TRUE))]
  })
  
} else {
  cat("Using glm.nb prediction method...\n")
  # For glm.nb, use the original method (weeks 1-52 only)
  dt_template[epi_week != 53, baseline := exp(predict(model, newdata = dt_for_prediction, type = "link"))]
}

end_time <- Sys.time()

time_taken_for_prediction <- difftime(end_time ,  start_time, units = "mins")
cat("Time taken for prediction: From", start_time, "to", end_time, ", therefore", time_taken_for_prediction, "\n")

# Diagnostic check for predictions
cat("\n=== PREDICTION DIAGNOSTIC (before week 53 interpolation) ===\n")
cat("Total rows in template:", nrow(dt_template), "\n")
cat("Non-NA predictions:", sum(!is.na(dt_template$baseline)), "\n")
cat("NA predictions (should be week 53 rows):", sum(is.na(dt_template$baseline)), "\n")
cat("Week 53 rows:", dt_template[epi_week == 53, .N], "\n")
cat("Range of baseline predictions:", range(dt_template$baseline, na.rm = TRUE), "\n")
cat("Mean baseline:", mean(dt_template$baseline, na.rm = TRUE), "\n")
cat("============================\n\n")

# plot the prediction and actual for a specific group to see how well the model fits
dt_template

dt_template[, .(baseline = sum(baseline, na.rm = TRUE)) , by=.(epi_year, epi_week)][order(epi_year, epi_week)]%>%
    ggplot() + 
    geom_line( 
        aes( 
            x = epi_week,
            y = baseline,
            color = epi_year, 
            group = epi_year
        )
    )+
    scale_color_gradient(low="lightblue", high="darkblue")+
    theme_minimal() 

# Set COVID baseline to 0 (LGH==26 in original, need to identify COVID)
# Since we don't have LGH directly, we'll use a different approach
# For now, we'll keep this as-is and note that COVID baselines should be 0

# Interpolate week 53 baseline
# Week 53 was excluded from model training, so we need to interpolate it
# Strategy: For each sex/agegroup/LGH combination, average week 52 and week 1 of next year
# In Stata: baseline = (baseline[_n-324]+baseline[_n+324])/2 if epi_week==53

cat("\n=== Interpolating week 53 baselines ===\n")

# First, sort the data properly
setorder(dt_template, sex, agegroup, LGH_Cause_encoded, epi_year, epi_week)

# For each group, interpolate week 53 as average of week 52 (same year) and week 1 (next year)
dt_template[, baseline_week52 := shift(baseline, 1, type = "lag"), 
            by = .(sex, agegroup, LGH_Cause_encoded)]
dt_template[, baseline_week1_next := shift(baseline, 1, type = "lead"), 
            by = .(sex, agegroup, LGH_Cause_encoded)]

# Interpolate week 53
dt_template[epi_week == 53, 
            baseline := (baseline_week52 + baseline_week1_next) / 2]

# Clean up temporary columns
dt_template[, c("baseline_week52", "baseline_week1_next") := NULL]

cat("Week 53 interpolations completed\n")
cat("Week 53 rows with non-NA baseline:", dt_template[epi_week == 53, sum(!is.na(baseline))], "\n")
cat("Week 53 rows with NA baseline:", dt_template[epi_week == 53, sum(is.na(baseline))], "\n")
dt_template[epi_week == 53 , epi_week_f:= "53"]
dt_template[epi_week == 53 ,]
cat("=======================================\n\n")

# Diagnostic plot to check week 53 interpolation
# Show total baseline deaths by week for all years
dt_template[, 
    .(baseline = sum(baseline, na.rm = TRUE)) , 
    by=.(epi_year, epi_week)][order(epi_year, epi_week)] %>%
    ggplot() + 
    geom_line(
        aes(
            x = epi_week, 
            y = baseline, 
            color = as.factor(epi_year), 
            group = epi_year)) +
    geom_point(#data = . %>% filter(epi_week == 53), 
               aes(x = epi_week, y = baseline, color = as.factor(epi_year)), 
               size = 3, shape = 21, fill = "white") +
    scale_color_viridis_d(name = "Year") +
    labs(title = "Baseline predictions by week (circles = interpolated week 53)",
         x = "Epi Week", y = "Total Baseline Deaths") +
    theme_minimal()


# plot actual 
dt[!epi_year %in% 2020:2022 , .(count = sum(count, na.rm = TRUE)) , by=.(epi_year, epi_week)][order(epi_year, epi_week)]%>%
    ggplot() + 
    geom_line( 
        aes( 
            x = epi_week,
            y = count,
            color = as.factor(epi_year), 
            group = epi_year
        )
    )+
    scale_color_viridis_d(name = "Year") +
    labs(title = "Actual deaths by week",
         x = "Epi Week", y = "Total Deaths") +
    theme_minimal()

# Save baseline
write_dta(dt_template, "LGH_MasterFile_baseline_2022.dta")
saveRDS(dt_template, "LGH_MasterFile_baseline_2022.rds")

cat("\n")
cat("================================================================================\n")
cat("DATA PROCESSING COMPLETE!\n")
cat("================================================================================\n")
cat("Generated files:\n")
cat("  - LGH_MasterFile_collapsed2022.dta / .rds\n")
cat("  - LGH_MasterFile_baseline_2022.dta / .rds\n")
cat("\n")
cat("You can now proceed with the analysis sections.\n")
cat("To continue, uncomment and run the ANALYSIS sections below.\n")
cat("================================================================================\n")

################################################################################
# SECTION 9: ANALYSIS - ALL NATURAL CAUSES
################################################################################

# Note: The analysis sections below create various plots
# Uncomment and run as needed

# dt_collapsed has these cause codes
unique(dt_collapsed$LGH_Cause_encoded)
table(dt_collapsed$LGH_Cause_encoded)

# dt_template has all cause codes from the model
unique(dt_template$LGH_Cause_encoded)
table(dt_template$LGH_Cause_encoded)

dt_template # the one with predicted baseline 
dt_template$LGH_f

dt_template$LGH_Cause <- as.character(dt_template$LGH_f) 


dt_collapsed$LGH_Cause # the one with actual counts
dt_template$LGH_Cause # the one with predicted baseline

merge( 
dt_collapsed, 
dt_template, 
by = c("sex", "epi_week", "epi_year", "agegroup", "LGH_Cause"), 
all.x = TRUE
)-> dt_analysis


# ICD codes to actual cause names
dt_analysis$LGH_Cause %>% unique() 

source( "lgh_cause_description_map.R") # creates icd_lookup data.frame
dt_analysis <- merge(
    dt_analysis,
    as.data.table(icd_lookup),
    by = "LGH_Cause",
    all.x = TRUE
)

dt_analysis$description %>% unique()

# write feather 
write_feather(dt_analysis, "LGH_AnalysisData_2022.feather")



dt_analysis
dt_analysis%>%na.omit() 

dt_analysis[epi_year %in% 2015:2022]-> dt_analysis

dt_analysis[
    !is.na(LGH_Cause ) ,  # this will ensure it is only for modelled causes
    .(count = sum(count, na.rm = TRUE), 
    baseline = sum(baseline, na.rm = TRUE)), 
    by=.(epi_week, epi_year)
    ]%>%
    ggplot() + 
    geom_line( 
        aes( 
            x = epi_week,
            y = count,
            color = "Reported deaths"
        )
    ) + 
    geom_line( 
        aes( 
            x = epi_week,
            y = baseline,
            color = "Baseline deaths (2015-2019)"
        )
    ) +
    ggh4x::facet_nested( 
        ~ epi_year,
        scales = "free_x",
        space = "free_x"
    )+
    scale_color_manual(values = c("Reported deaths" = "red", "Baseline deaths (2015-2019)" = "blue")) +
    #scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(x = "", y = "Deaths", color = "")



# Order conditions by high proportion of excess deaths to high proportion of deficit deaths
dt_analysis[
    !is.na(LGH_Cause) & epi_year %in% 2020:2022, # this will ensure it is only for modelled causes
    {
        csum <- sum(count, na.rm = TRUE)
        bsum <- sum(baseline, na.rm = TRUE)
        .(
            count = csum,
            baseline = bsum,
            difference = csum - bsum,
            excess_deficit = ifelse(csum - bsum > 0, "Excess", "Deficit")
            #covid_period = ifelse(epi_year < 2020, "Pre-COVID", "COVID")
        )
    },
    by = .( LGH_Cause, epi_year)
][, 
    `:=`(
        prop_diff = (count - baseline) / baseline * 100, 
        factor_diff = count/baseline
    )
] -> dt_analysis_ordered_by_year 


dt_analysis[
    !is.na(LGH_Cause) & epi_year %in% 2020:2022, # this will ensure it is only for modelled causes
    {
        csum <- sum(count, na.rm = TRUE)
        bsum <- sum(baseline, na.rm = TRUE)
        .(
            count = csum,
            baseline = bsum,
            difference = csum - bsum,
            excess_deficit = ifelse(csum - bsum > 0, "Excess", "Deficit")
            # covid_period = ifelse(epi_year < 2020, "Pre-COVID", "COVID")
        )
    },
    by = .(LGH_Cause)
][
    ,
    `:=`(
        prop_diff = (count - baseline) / baseline * 100,
        factor_diff = count / baseline, 
        epi_year = "Overall (2020-2022)"
    )
] -> dt_analysis_ordered_overall

rbind(
    dt_analysis_ordered_by_year[, period := as.character(epi_year)],
    dt_analysis_ordered_overall[, period := "Overall (2020-2022)"]
) -> dt_analysis_ordered_final

dt_analysis_ordered_overall[order(-prop_diff)]$LGH_Cause -> LGH_descending_label

dt_analysis_ordered_final$LGH_Cause <- factor(
    dt_analysis_ordered_final$LGH_Cause,
    levels = LGH_descending_label
)

dt_analysis_ordered_final %>%
    ggplot() +
    geom_tile( 
        aes( 
            x = period,
            y = LGH_Cause,
            fill = prop_diff
        )
    )+ 
    scale_fill_gradient2(
        low = "green",
        mid = "white",
        high = "red",
        midpoint = 0,
        name = "Proportional Difference (%)"
    )



dt_analysis$week_start <- as.Date(paste0(dt_analysis$epi_year, "-01-01")) + (dt_analysis$epi_week - 1) * 7

# This one is faceted by LGH_Cause
dt_analysis[
    !is.na(LGH_Cause) & epi_year %in% 2020:2022, # this will ensure it is only for modelled causes
    .(
        count = sum(count, na.rm = TRUE),
        baseline = sum(baseline, na.rm = TRUE)
    ),
    by = .(week_start, LGH_Cause)
] %>%
    ggplot() +
    geom_line(
        aes(
            x = week_start,
            y = count,
            color = "Reported deaths"
        )
    ) +
        geom_line(
        aes(
            x = week_start,
            y = baseline,
            color = "Baseline deaths (2015-2019)"
        )
    ) +
    facet_wrap(
        LGH_Cause ~ .,
        scales = "free_y", 
        ncol = 3
    ) +
    scale_color_manual(values = c("Reported deaths" = "red", "Baseline deaths (2015-2019)" = "blue")) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
    theme_minimal()

# plot the difference between observed and expected 
dt_analysis[
    !is.na(LGH_Cause) & epi_year %in% 2019:2022, # this will ensure it is only for modelled causes
    {
        csum <- sum(count, na.rm = TRUE)
        bsum <- sum(baseline, na.rm = TRUE)
        .(
            count = csum,
            baseline = bsum,
            difference = csum - bsum,
            excess_deficit = ifelse(csum - bsum > 0, "Excess", "Deficit"), 
            covid_period = ifelse(epi_year <2020, "Pre-COVID", "COVID")
        )
    },
    by = .(week_start, LGH_Cause)
]%>%
    ggplot() +
    geom_col(
        aes(
            x = week_start,
            y = difference,
            fill = excess_deficit
        ),
        stat = "identity",
        position = "dodge"
    )+ 
    scale_fill_manual(
        values = c("Excess" = "orange", "Deficit" = "green")
    )+
    geom_vline(
        xintercept = as.Date("2020-04-01"),
        linetype = "dashed",
        color = "red"
    ) +
    facet_wrap(
        ~ LGH_Cause,
        ncol = 3, 
        scales = "free_y"
    )+
    labs(x = "Cause of Death", y = "Excess Deaths", fill = "Year") +
    theme_minimal()


# LGH cause with find highest to lowest deficit on average
dt_analysis[
    !is.na(LGH_Cause) & epi_year %in% 2020:2022, # this will ensure it is only for modelled causes
    {
        csum <- sum(count, na.rm = TRUE)
        bsum <- sum(baseline, na.rm = TRUE)
        .(
            count = csum,
            baseline = bsum,
            difference = csum - bsum,
            diff_color = ifelse(csum - bsum > 0, "Excess", "Deficit")
        )
    },
    by = .(LGH_Cause)
][order(difference)] %>% print()


# proportion difference of baseline to observed 
dt_analysis$week_start

dt_analysis[, 
prop_diff := (count - baseline) / baseline * 100, 
    by = .(LGH_Cause, week_start)
] -> dt_analysis_prop_diff

dt_analysis_prop_diff%>%na.omit() %>%
    ggplot() +
    geom_line(
        aes(
            x = week_start,
            y = prop_diff,
        ),
        stat = "identity",
        position = "dodge"
    )+ 
    facet_wrap(
        ~ LGH_Cause,
        ncol = 3, 
        scale = "free_y"
    )+
    labs(x = "Cause of Death", y = "Proportional Difference (%)", fill = "Year") +
    theme_minimal()

# make a geomtime plot of the higher vs lower excess deaths as a proportion.


# make a geomtime plot of the higher vs lower excess deaths as a proportion. 

dt_analysis[, prop_diff := (count - baseline) / baseline * 100, 
    by = .(LGH_Cause, week_start)
] %>%
    ggplot() +
    geom_line(
        aes(
            x = week_start,
            y = prop_diff,
        ),
        stat = "identity",
        position = "dodge"
    )+ 
    facet_wrap(
        ~ LGH_Cause,
        ncol = 3
    )+
    labs(x = "Cause of Death", y = "Proportional Difference (%)", fill = "Year") +
    theme_minimal()

write_feather(dt_analysis, "LGH_AnalysisData_2022.feather")


analysis_all_natural <- function() {
  
  # Load data
  dt_collapsed <- readRDS("LGH_MasterFile_collapsed2022.rds")
  dt_baseline <- readRDS("LGH_MasterFile_baseline_2022.rds")
  
  # Merge collapsed with baseline
  # Tidyverse: left_join(dt_collapsed, dt_baseline, by = c("sex", "epi_week", ...))
  # data.table: merge() or using keys
  dt_analysis <- merge(dt_collapsed, 
                       dt_baseline[, .(sex, epi_week, epi_year, agegroup, 
                                      LGH_Cause_encoded, baseline)],
                       by = c("sex", "epi_week", "epi_year", "agegroup", "LGH_Cause_encoded"),
                       all.x = TRUE)
  
  # Aggregate by week
  # Tidyverse: group_by(weekstart) %>% summarize(count = sum(count), baseline = sum(baseline))
  dt_weekly <- dt_analysis[, .(count = sum(count, na.rm = TRUE), 
                               baseline = sum(baseline, na.rm = TRUE)), 
                          by = .(weekstart)]
  
  # Filter to 2017-2022
  dt_weekly <- dt_weekly[year(weekstart) >= 2017 & year(weekstart) <= 2022]
  
  # Sort by date
  setorder(dt_weekly, weekstart)
  
  # Calculate excess deaths
  dt_weekly[, excess := count - baseline]
  
  # Plot 1: Count vs baseline
  p1 <- ggplot(dt_weekly, aes(x = weekstart)) +
    geom_line(aes(y = count, color = "Reported deaths")) +
    geom_line(aes(y = baseline, color = "Baseline deaths (2015-2019)")) +
    scale_color_manual(values = c("Reported deaths" = "red", 
                                  "Baseline deaths (2015-2019)" = "blue")) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(x = "", y = "Deaths", color = "")
  
  ggsave("AllNatural_count.png", p1, width = 10, height = 6)
  
  # Plot 2: Excess deaths
  p2 <- ggplot(dt_weekly, aes(x = weekstart, y = excess)) +
    geom_line(color = "blue") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    theme_minimal() +
    labs(x = "", y = "Excess natural deaths")
  
  ggsave("AllNatural_excess.png", p2, width = 10, height = 6)
  
  cat("All Natural Causes analysis complete!\n")
  cat("Saved: AllNatural_count.png, AllNatural_excess.png\n")
}

# Uncomment to run:
 analysis_all_natural()

################################################################################
# SECTION 10: ANALYSIS BY CAUSE OF DEATH, SEX, AGE
################################################################################

# The following functions follow the same pattern as above
# They are provided as templates - uncomment and modify as needed

analysis_by_cod <- function() {
  cat("Analysis by Cause of Death - implement similarly to analysis_all_natural()\n")
  cat("Group by weekstart and LGH_Cause_encoded, filter to 2020-2022\n")
  cat("Create faceted plots using facet_wrap(~LGH_Cause_encoded)\n")
}

analysis_by_sex <- function() {
  cat("Analysis by Sex - implement similarly to analysis_all_natural()\n")
  cat("Group by weekstart and sex, filter to 2020-2022\n")
  cat("Create faceted plots using facet_wrap(~sex)\n")
}

analysis_by_age <- function() {
  cat("Analysis by Age Group - implement similarly to analysis_all_natural()\n")
  cat("Group by weekstart and agegroup, filter to 2020-2022\n")
  cat("Create faceted plots using facet_wrap(~agegroup)\n")
}

# Uncomment to run:
# analysis_by_cod()
# analysis_by_sex()
# analysis_by_age()

################################################################################
# SECTION 11: TABLES
################################################################################

create_tables <- function() {
  
  cat("Creating summary tables...\n")
  
  # Load data
  dt_collapsed <- readRDS("LGH_MasterFile_collapsed2022.rds")
  dt_baseline <- readRDS("LGH_MasterFile_baseline_2022.rds")
  
  # Merge
  dt_analysis <- merge(dt_collapsed, 
                       dt_baseline[, .(sex, epi_week, epi_year, agegroup, 
                                      LGH_Cause_encoded, baseline)],
                       by = c("sex", "epi_week", "epi_year", "agegroup", "LGH_Cause_encoded"),
                       all.x = TRUE)
  
  # Filter to 2020-2022
  dt_tables <- dt_analysis[epi_year >= 2020 & epi_year <= 2022]
  
  # Calculate excess
  dt_tables[, excess := count - baseline]
  
  # Table 1: Total by year
  # Tidyverse: group_by(epi_year) %>% summarize(total_count = sum(count), total_excess = sum(excess))
  tab1_count <- dt_tables[, .(total = sum(count, na.rm = TRUE)), by = .(epi_year)]
  tab1_excess <- dt_tables[, .(total = sum(excess, na.rm = TRUE)), by = .(epi_year)]
  
  print("Counts by year:")
  print(tab1_count)
  
  print("Excess deaths by year:")
  print(tab1_excess)
  
  # Additional tables can be created similarly
  # Table by age group and year
  tab2 <- dt_tables[, .(count = sum(count, na.rm = TRUE), 
                        excess = sum(excess, na.rm = TRUE)), 
                    by = .(agegroup, epi_year)]
  
  print("Counts and excess by age group and year:")
  print(tab2)
  
  # Table by sex and year
  tab3 <- dt_tables[, .(count = sum(count, na.rm = TRUE), 
                        excess = sum(excess, na.rm = TRUE)), 
                    by = .(sex, epi_year)]
  
  print("Counts and excess by sex and year:")
  print(tab3)
  
  cat("Table creation complete!\n")
}

# Uncomment to run:
# create_tables()

################################################################################
# KEY DIFFERENCES: STATA vs R/data.table
################################################################################

# 1. ASSIGNMENT:
#    Stata: gen/replace variable = value
#    data.table: dt[, variable := value]  (modifies in place!)
#    tidyverse: mutate(variable = value)  (creates copy)

# 2. FILTERING:
#    Stata: keep if condition
#    data.table: dt <- dt[condition]
#    tidyverse: filter(condition)

# 3. GROUPING & AGGREGATION:
#    Stata: collapse (sum) var, by(group_vars)
#    data.table: dt[, .(sum_var = sum(var)), by = .(group_vars)]
#    tidyverse: group_by(group_vars) %>% summarize(sum_var = sum(var))

# 4. CONDITIONAL ASSIGNMENT:
#    Stata: replace var = value if condition
#    data.table: dt[condition, var := value]
#    tidyverse: mutate(var = if_else(condition, value, var))

# 5. MERGING:
#    Stata: merge 1:1 vars using filename
#    data.table: merge(dt1, dt2, by = "vars")
#    tidyverse: left_join(dt1, dt2, by = "vars")

# 6. SORTING:
#    Stata: sort vars
#    data.table: setorder(dt, vars) or setkey(dt, vars)
#    tidyverse: arrange(vars)

################################################################################
# PERFORMANCE TIPS FOR data.table
################################################################################

# 1. Use := for assignment (modifies in place, no copy)
# 2. Use keys for repeated subsetting: setkey(dt, var)
# 3. Chain operations: dt[condition1][condition2]
# 4. Use .N for counting rows
# 5. Use .SD for operations on subset of columns
# 6. Avoid copies with copy() only when necessary

################################################################################
# END OF SCRIPT
################################################################################

cat("\n")
cat("Script loaded successfully!\n")
cat("Run the main sections in order:\n")
cat("  1. Data loading and processing (automatic)\n")
cat("  2. Baseline creation (automatic, may take time)\n")
cat("  3. Analysis functions (uncomment to run)\n")
