suppressPackageStartupMessages({
  library(arrow); library(data.table); library(ggplot2)
  library(lubridate); library(scales); library(sf)
})

project_dir <- "projects/poisoning_deaths"
# Prefer LGH source as requested; fall back to the full pre-collapsed LGH extract.
input_candidates <- c("LGH.feather", "LGH_MasterFile_preCollapsedAll.feather")
input_file <- input_candidates[file.exists(input_candidates)][1]
output_rda  <- file.path(project_dir, "poisoning_results.rda")
if (is.na(input_file)) {
  stop("No LGH feather input found. Expected one of: ",
       paste(input_candidates, collapse = ", "))
}
message("Using input file: ", input_file)

# ── Decode Stata-labelled types ──────────────────────────────────────────────
dt <- as.data.table(read_feather(input_file))
for (v in names(dt)) {
  if (inherits(dt[[v]], "haven_labelled")) {
    raw <- unclass(dt[[v]])
    if (is.numeric(raw)) dt[, (v) := as.integer(raw)]
    else                 dt[, (v) := as.character(raw)]
  }
}

# ── Flag poisoning records ───────────────────────────────────────────────────
dt[, icd_clean := toupper(gsub("[^A-Z0-9]", "", as.character(UnderlyingCause)))]
dt[, is_poisoning := grepl("^(X4[0-9]|X6[0-9]|X85|Y1[0-9])", icd_clean)]
p <- dt[is_poisoning == TRUE & !is.na(DeathYear)]

# ── Intent ───────────────────────────────────────────────────────────────────
p[, intent := fcase(
  grepl("^X4[0-9]", icd_clean), "Accidental/unspec.",
  grepl("^X6[0-9]", icd_clean), "Self-harm",
  grepl("^X85",     icd_clean), "Assault",
  grepl("^Y1[0-9]", icd_clean), "Undetermined",
  default = "Other"
)]
p[, intent := factor(intent, levels = c("Accidental/unspec.","Self-harm","Assault","Undetermined","Other"))]

# ── Sex ──────────────────────────────────────────────────────────────────────
sex_var <- if ("Sex" %in% names(p)) "Sex" else if ("sex" %in% names(p)) "sex" else NA_character_
if (is.na(sex_var)) stop("Missing sex column. Expected one of: Sex, sex")
p[, sex_code := as.character(get(sex_var))]
sex_lu <- c("1"="Male","2"="Female","3"="Unknown","9"="Unknown")
p[, sex_label := factor(
  fifelse(sex_code %in% names(sex_lu), sex_lu[sex_code], "Unknown"),
  levels = c("Male","Female","Unknown")
)]

# ── Province ─────────────────────────────────────────────────────────────────
prov_lu <- c("1"="Western Cape","2"="Eastern Cape","3"="Northern Cape",
             "4"="Free State","5"="KwaZulu-Natal","6"="North West",
             "7"="Gauteng","8"="Mpumalanga","9"="Limpopo",
             "98"="Outside SA","99"="Unspecified")
p[, province := fifelse(
  as.character(ResProvince) %in% names(prov_lu),
  prov_lu[as.character(ResProvince)], "Unspecified"
)]

# ── Death institution (place of death) ───────────────────────────────────────
inst_lu <- c("1"="Hospital","2"="Emergency room/Outpatient",
             "3"="Dead on arrival","4"="Nursing home",
             "5"="Home","6"="Other","8"="Unknown","9"="Unspecified")
p[, death_inst := fifelse(
  as.character(DeathInst) %in% names(inst_lu),
  inst_lu[as.character(DeathInst)], "Unspecified"
)]
INST_LEVELS <- c("Hospital","Emergency room/Outpatient","Dead on arrival",
                 "Nursing home","Home","Other","Unknown","Unspecified")
p[, death_inst := factor(death_inst, levels = INST_LEVELS)]

# Broad grouping: health facility / dead on arrival / out of facility / unknown
p[, facility_grp := fcase(
  DeathInst %in% c(1L,2L,4L), "Health facility",
  DeathInst == 3L,             "Dead on arrival",
  DeathInst %in% c(5L,6L),    "Out of facility",
  default = "Unknown/unspecified"
)]
p[, facility_grp := factor(facility_grp, levels = c(
  "Health facility","Dead on arrival","Out of facility","Unknown/unspecified"))]

# ── Age groups ───────────────────────────────────────────────────────────────
AGE_BREAKS <- c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,Inf)
AGE_LABELS <- c("<1","1-4","5-9","10-14","15-19","20-24","25-29","30-34",
                "35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75+")
p[, age_grp := cut(age, breaks=AGE_BREAKS, labels=AGE_LABELS, right=FALSE)]
p[, year    := as.integer(DeathYear)]
p[, month   := as.integer(DeathMonth)]
p[, date_month := as.Date(sprintf("%d-%02d-01", year, month))]

# ── Monthly epicurve data (complete — all records have year+month) ────────────
monthly_intent <- p[!is.na(date_month) & intent != "Other",
  .(n = .N), by = .(date_month, intent)]
# complete grid so missing intent×month cells become 0
all_months  <- seq(min(monthly_intent$date_month),
                   max(monthly_intent$date_month), by = "month")
all_intents <- c("Accidental/unspec.","Self-harm","Assault","Undetermined")
monthly_intent <- merge(
  CJ(date_month = all_months, intent = all_intents),
  monthly_intent, by = c("date_month","intent"), all.x = TRUE)
monthly_intent[is.na(n), n := 0L]
monthly_intent[, intent := factor(intent, levels = all_intents)]
# monthly totals for the smoother
monthly_total <- monthly_intent[, .(n = sum(n)), by = date_month]

# ── Annual summaries ─────────────────────────────────────────────────────────
annual_total  <- p[,.N,by=year]; setorder(annual_total,year)
annual_intent <- p[,.N,by=.(year,intent)]; setorder(annual_intent,year,intent)

tbl_year_summary <- p[,.(
  Total                = .N,
  `Accidental/unspec.` = sum(intent=="Accidental/unspec."),
  `Self-harm`          = sum(intent=="Self-harm"),
  Assault              = sum(intent=="Assault"),
  Undetermined         = sum(intent=="Undetermined"),
  `% Male`     = round(100*mean(sex_label=="Male",na.rm=TRUE),1),
  `Median age` = round(median(age,na.rm=TRUE),1)
), by=.(Year=year)]
setorder(tbl_year_summary,Year)

# ── Age-sex pyramid data ─────────────────────────────────────────────────────
pyramid_data <- p[sex_label %in% c("Male","Female")&!is.na(age_grp),
  .(n=.N), by=.(age_grp,sex_label)]
pyramid_data[, n_directed := fifelse(sex_label=="Male",-n,n)]
pyramid_data[, age_grp := factor(age_grp,levels=AGE_LABELS)]

tbl_age_sex <- dcast(
  p[sex_label %in% c("Male","Female")&!is.na(age_grp),.(n=.N),by=.(age_grp,sex_label)],
  age_grp~sex_label, value.var="n", fill=0L)
tbl_age_sex[, Total    := Male+Female]
tbl_age_sex[, `% Male` := round(100*Male/Total,1)]
setnames(tbl_age_sex,"age_grp","Age group")

pyramid_intent <- p[sex_label %in% c("Male","Female")&!is.na(age_grp)&
                    intent %in% c("Accidental/unspec.","Self-harm","Undetermined"),
  .(n=.N), by=.(age_grp,sex_label,intent)]
pyramid_intent[, n_directed := fifelse(sex_label=="Male",-n,n)]
pyramid_intent[, age_grp := factor(age_grp,levels=AGE_LABELS)]

# ── Seasonality ──────────────────────────────────────────────────────────────
seasonality_avg <- p[,.(n=.N),by=.(year,month)][,
  .(mean_n=mean(n,na.rm=TRUE),sd_n=sd(n,na.rm=TRUE)),by=month]
setorder(seasonality_avg,month)
seasonality_avg[, month_label := factor(month.abb[month],levels=month.abb)]

# ── Provincial summary ───────────────────────────────────────────────────────
prov_summary <- p[!province %in% c("Outside SA","Unspecified"),.(n=.N),by=province]
setorder(prov_summary,-n)

# ── ANCOD-style crude and age-specific death rates ───────────────────────────
# Population and district geometry are loaded from the shared data explorer
# project so the policy brief can show crude rates and district maps.
load(file.path("projects/data_explorer/population_data.rda"))
load(file.path("projects/data_explorer/shape_files.rda"))
setDT(pop)
pop[, population := as.numeric(Population)]

norm_key <- function(x) {
  x <- as.character(x)
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]", "", x)
  x
}

ref_district_keys <- unique(norm_key(c(
  as.character(pop$district_standard),
  as.character(shape_files$districts$district_standard)
)))

match_key_to_ref <- function(keys, ref_keys) {
  out <- keys
  idx <- match(out, ref_keys)
  missing <- is.na(idx)
  if (any(missing)) {
    best_idx <- apply(adist(out[missing], ref_keys), 1, which.min)
    out[missing] <- ref_keys[best_idx]
  }
  out
}

# Prefer residence district for policy-facing burden estimates.
p[, district_name := as.character(resdistrictname)]
p[is.na(district_name) | trimws(district_name) == "", district_name := as.character(deathdistrictname)]
p[, district_name := trimws(district_name)]
p[, district_key := match_key_to_ref(norm_key(district_name), ref_district_keys)]

analysis_year <- max(p$year, na.rm = TRUE)

annual_pop <- pop[, .(population = sum(population, na.rm = TRUE)), by = .(year = as.integer(as.character(Year)))]
annual_pop <- annual_pop[!is.na(year)]

annual_counts <- p[, .(deaths = .N), by = year]
annual_crude <- merge(annual_counts, annual_pop, by = "year", all.x = TRUE)
annual_crude[, crude_rate_per_100k := fifelse(!is.na(population) & population > 0,
                                             (deaths / population) * 1e5,
                                             NA_real_)]

fig_crude_rate_trend <- ggplot(annual_crude[!is.na(crude_rate_per_100k)], aes(x = year, y = crude_rate_per_100k)) +
  geom_line(linewidth = 0.9, colour = "steelblue") +
  geom_point(size = 1.8, colour = "steelblue") +
  scale_x_continuous(breaks = seq(min(annual_crude$year, na.rm = TRUE), max(annual_crude$year, na.rm = TRUE), 2)) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0.02, 0.05))) +
  labs(x = NULL, y = "Deaths per 100,000 population") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

AGE_RATE_LABELS <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
                     "35-39", "40-44", "45-49", "50-54", "55-59", "60-64",
                     "65-69", "70-74", "75-79", "80+")
p[, age_rate_grp := cut(age,
                        breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45,
                                   50, 55, 60, 65, 70, 75, 80, Inf),
                        labels = AGE_RATE_LABELS, right = FALSE)]

death_age_counts <- p[sex_label %in% c("Male", "Female") & !is.na(age_rate_grp),
  .(deaths = .N), by = .(year, sex_label, age_rate_grp)]
pop_age_counts <- pop[
  Sex %in% c("Male", "Female") & Age %in% AGE_RATE_LABELS,
  .(population = sum(population, na.rm = TRUE)),
  by = .(year = as.integer(as.character(Year)), sex_label = as.character(Sex), age_rate_grp = as.character(Age))
]

age_rate_grid <- merge(
  CJ(year = sort(unique(death_age_counts$year)),
     sex_label = c("Male", "Female"),
     age_rate_grp = AGE_RATE_LABELS),
  death_age_counts,
  by = c("year", "sex_label", "age_rate_grp"),
  all.x = TRUE
)
age_rate_grid <- merge(age_rate_grid, pop_age_counts,
                       by = c("year", "sex_label", "age_rate_grp"), all.x = TRUE)
age_rate_grid[is.na(deaths), deaths := 0L]
age_rate_grid[is.na(population), population := NA_real_]
age_rate_grid[, crude_rate_per_100k := fifelse(!is.na(population) & population > 0,
                                               (deaths / population) * 1e5,
                                               NA_real_)]
age_rate_grid[, age_rate_grp := factor(age_rate_grp, levels = AGE_RATE_LABELS)]
age_rate_grid[, age_rate_grp_rev := factor(age_rate_grp, levels = rev(AGE_RATE_LABELS))]
age_rate_grid[, sex_label := factor(sex_label, levels = c("Male", "Female"))]

fig_age_year_sex_rate <- ggplot(age_rate_grid, aes(x = year, y = age_rate_grp_rev, fill = crude_rate_per_100k)) +
  geom_tile(colour = "white", linewidth = 0.25) +
  facet_wrap(~sex_label, ncol = 2) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1,
                       name = "Deaths\nper 100,000") +
  scale_x_continuous(breaks = seq(min(age_rate_grid$year), max(age_rate_grid$year), 3)) +
  labs(x = NULL, y = "Age group") +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold"))

district_latest_counts <- p[year == analysis_year, .(deaths = .N), by = district_key]
district_pop_latest <- pop[as.integer(as.character(Year)) == analysis_year,
  .(population = sum(population, na.rm = TRUE)), by = .(district_key = norm_key(as.character(district_standard)))]

district_rates_latest <- merge(district_latest_counts, district_pop_latest, by = "district_key", all.x = TRUE)
district_rates_latest[, crude_rate_per_100k := fifelse(!is.na(population) & population > 0,
                                                       (deaths / population) * 1e5,
                                                       NA_real_)]

district_lookup <- unique(data.table(
  district_key = norm_key(as.character(shape_files$districts$district_standard)),
  district_name = as.character(shape_files$districts$district_standard),
  province = as.character(shape_files$districts$province)
))

district_rates_latest <- merge(district_lookup, district_rates_latest, by = "district_key", all.x = TRUE)
district_rates_latest[is.na(deaths), deaths := 0L]
district_rates_latest[is.na(population), population := NA_real_]

district_rates_latest[, rate_flag := fifelse(
  is.na(crude_rate_per_100k), "no population",
  fifelse(crude_rate_per_100k >= quantile(crude_rate_per_100k, 0.9, na.rm = TRUE), "high",
         fifelse(crude_rate_per_100k <= quantile(crude_rate_per_100k, 0.1, na.rm = TRUE), "low", "mid"))
)]

top5_districts <- district_rates_latest[!is.na(crude_rate_per_100k)][order(-crude_rate_per_100k)][1:5,
  .(district_name, province, deaths, population, crude_rate_per_100k)]

district_map_sf <- merge(shape_files$districts, district_rates_latest,
                         by.x = "district_standard", by.y = "district_name", all.x = TRUE)

fig_district_rate_map <- ggplot(district_map_sf) +
  geom_sf(aes(fill = crude_rate_per_100k), colour = "white", linewidth = 0.2) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, na.value = "grey90",
                       name = "Deaths\nper 100,000") +
  labs(x = NULL, y = NULL, title = paste0("District crude death rate map (", analysis_year, ")")) +
  theme_minimal(base_size = 11)

recent_years <- sort(unique(p$year), decreasing = TRUE)[1:min(3L, uniqueN(p$year))]
recent_years <- sort(recent_years)

district_counts_3yr <- p[year %in% recent_years, .(deaths = .N), by = district_key]
district_pop_3yr <- pop[
  as.integer(as.character(Year)) %in% recent_years,
  .(population = sum(population, na.rm = TRUE)),
  by = .(district_key = norm_key(as.character(district_standard)))
]

district_rates_3yr <- merge(district_counts_3yr, district_pop_3yr, by = "district_key", all.x = TRUE)
district_rates_3yr[, crude_rate_per_100k := fifelse(!is.na(population) & population > 0,
                                                    (deaths / population) * 1e5,
                                                    NA_real_)]
district_rates_3yr <- merge(district_lookup, district_rates_3yr, by = "district_key", all.x = TRUE)
district_rates_3yr[is.na(deaths), deaths := 0L]
district_rates_3yr[is.na(population), population := NA_real_]
district_rates_3yr[, hotspot_flag := fifelse(
  is.na(crude_rate_per_100k), "no population",
  fifelse(crude_rate_per_100k >= quantile(crude_rate_per_100k, 0.9, na.rm = TRUE), "hot spot",
          fifelse(crude_rate_per_100k <= quantile(crude_rate_per_100k, 0.1, na.rm = TRUE), "cold spot", "other"))
)]
district_rates_3yr[, hotspot_flag := factor(hotspot_flag,
                                            levels = c("cold spot", "other", "hot spot", "no population"))]

district_hotspot_map_3yr_sf <- merge(shape_files$districts, district_rates_3yr,
                                     by.x = "district_standard", by.y = "district_name", all.x = TRUE)

fig_district_hotspot_map_3yr <- ggplot(district_hotspot_map_3yr_sf) +
  geom_sf(aes(fill = hotspot_flag), colour = "white", linewidth = 0.2) +
  scale_fill_manual(values = c("cold spot" = "#2b8cbe",
                               "other" = "#f0f0f0",
                               "hot spot" = "#d7301f",
                               "no population" = "grey80"),
                    name = NULL, drop = FALSE) +
  labs(x = NULL, y = NULL,
       title = paste0("District hot spots, pooled crude death rate (",
                      min(recent_years), "-", max(recent_years), ")")) +
  theme_minimal(base_size = 11)

# ── Top 15 ICD subcodes ──────────────────────────────────────────────────────
icd_sub <- p[,.(n=.N),by=.(icd=substr(icd_clean,1,3))]
setorder(icd_sub,-n); icd_sub <- head(icd_sub,15)
icd_lu <- c(
  X40="X40  Acc./unspec. - analgesics (non-opioid)",
  X41="X41  Acc./unspec. - antiepileptics/sedatives",
  X42="X42  Acc./unspec. - narcotics/psychodysleptics",
  X44="X44  Acc./unspec. - other/unspecified drugs",
  X45="X45  Acc./unspec. - alcohol",
  X47="X47  Acc./unspec. - gases & vapours",
  X48="X48  Acc./unspec. - pesticides",
  X49="X49  Acc./unspec. - other chemicals/NOS",
  X60="X60  Self-harm - analgesics (non-opioid)",
  X61="X61  Self-harm - antiepileptics/sedatives",
  X62="X62  Self-harm - narcotics",
  X64="X64  Self-harm - other/unspecified drugs",
  X68="X68  Self-harm - pesticides",
  X69="X69  Self-harm - other chemicals/NOS",
  X85="X85  Assault",
  Y10="Y10  Undetermined - analgesics (non-opioid)",
  Y14="Y14  Undetermined - other/unspecified drugs",
  Y19="Y19  Undetermined - other chemicals/NOS"
)
icd_sub[, label := fifelse(icd %in% names(icd_lu), icd_lu[icd], icd)]
icd_sub[, label := factor(label,levels=rev(label))]

# ── Overall summary table ────────────────────────────────────────────────────
tbl_overall <- data.table(
  Metric = c("Total poisoning deaths","Analysis years","% Male",
             "Median age (years)","% Accidental/unspec.","% Self-harm",
             "% Assault","% Undetermined"),
  Value  = c(
    format(nrow(p),big.mark=","),
    paste(min(p$year),"-",max(p$year)),
    paste0(round(100*mean(p$sex_label=="Male",na.rm=TRUE),1),"%"),
    as.character(round(median(p$age,na.rm=TRUE),1)),
    paste0(round(100*mean(p$intent=="Accidental/unspec."),1),"%"),
    paste0(round(100*mean(p$intent=="Self-harm"),1),"%"),
    paste0(round(100*mean(p$intent=="Assault"),1),"%"),
    paste0(round(100*mean(p$intent=="Undetermined"),1),"%")
  )
)

# ── Colour palette ───────────────────────────────────────────────────────────
INTENT_COLS <- c("Accidental/unspec."="steelblue","Self-harm"="firebrick",
                 "Assault"="#FDAE61","Undetermined"="#4DAC26","Other"="#AAAAAA")

# ── FIG 1: Annual trend ──────────────────────────────────────────────────────
fig_annual_trend <- ggplot(annual_total,aes(x=year,y=N)) +
  geom_col(fill="steelblue",alpha=.75,width=.7) +
  geom_smooth(method="lm",se=TRUE,colour="firebrick",linewidth=.9,
              linetype="dashed",fill="firebrick",alpha=.12) +
  scale_x_continuous(breaks=seq(min(annual_total$year),max(annual_total$year),2)) +
  scale_y_continuous(labels=comma,expand=expansion(mult=c(0,.07))) +
  labs(x=NULL,y="Deaths") +
  theme_minimal(base_size=11) + theme(panel.grid.minor=element_blank())

# ── FIG 2: Stacked area by intent ────────────────────────────────────────────
fig_intent_trend <- ggplot(annual_intent,aes(x=year,y=N,fill=intent)) +
  geom_area(position="stack",alpha=.85,colour="white",linewidth=.2) +
  scale_fill_manual(values=INTENT_COLS,name=NULL) +
  scale_x_continuous(breaks=seq(min(annual_intent$year),max(annual_intent$year),2)) +
  scale_y_continuous(labels=comma,expand=expansion(mult=c(0,.04))) +
  labs(x=NULL,y="Deaths") +
  theme_minimal(base_size=11) +
  theme(panel.grid.minor=element_blank(),legend.position="bottom")

# ── FIG 3: Monthly epicurve — stacked by intent ──────────────────────────────
fig_weekly_epicurve <- ggplot() +
  # stacked bars by intent
  geom_col(data = monthly_intent,
           aes(x = date_month, y = n, fill = intent),
           width = 28, alpha = 0.80, position = "stack") +
  # LOESS smoother on total count (span ≈ 5-year window on 26-year series)
  geom_smooth(data = monthly_total,
              aes(x = date_month, y = n),
              method = "loess", span = 0.20, se = TRUE,
              colour  = "grey10", fill = "grey30", alpha = 0.18,
              linewidth = 0.9, linetype = "solid") +
  scale_fill_manual(values = INTENT_COLS, name = NULL) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y",
               expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(labels = comma,
                     expand = expansion(mult = c(0, 0.06))) +
  labs(x = NULL, y = "Deaths per month") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor   = element_blank(),
        legend.position    = "bottom",
        legend.key.size    = unit(0.4, "cm"))

# ── FIG 4: Monthly seasonality ───────────────────────────────────────────────
fig_seasonality <- ggplot(seasonality_avg,aes(x=month_label,y=mean_n)) +
  geom_col(fill="steelblue",alpha=.75,width=.7) +
  geom_errorbar(aes(ymin=pmax(0,mean_n-sd_n),ymax=mean_n+sd_n),width=.3,colour="#444444") +
  labs(x=NULL,y="Mean deaths per month (+/-1 SD)") +
  theme_minimal(base_size=11) + theme(panel.grid.minor=element_blank())

# ── FIG 5: Age-sex pyramid (overall) ────────────────────────────────────────
max_n <- max(abs(pyramid_data$n_directed))
pb    <- pretty(c(-max_n,max_n),n=6)
fig_pyramid <- ggplot(pyramid_data,aes(x=n_directed,y=age_grp,fill=sex_label)) +
  geom_col(alpha=.85,width=.8) +
  geom_vline(xintercept=0,colour="white",linewidth=.6) +
  scale_x_continuous(breaks=pb,labels=comma(abs(pb))) +
  scale_fill_manual(values=c(Male="steelblue",Female="firebrick"),name=NULL) +
  labs(x="Number of deaths",y="Age group") +
  theme_minimal(base_size=11) +
  theme(legend.position="bottom",panel.grid.minor=element_blank())

# ── FIG 6: Age-sex pyramid by intent ────────────────────────────────────────
fig_pyramid_intent <- ggplot(pyramid_intent,aes(x=n_directed,y=age_grp,fill=sex_label)) +
  geom_col(alpha=.85,width=.8) +
  geom_vline(xintercept=0,colour="white",linewidth=.5) +
  facet_wrap(~intent,scales="free_x") +
  scale_x_continuous(labels=function(x) comma(abs(x))) +
  scale_fill_manual(values=c(Male="steelblue",Female="firebrick"),name=NULL) +
  labs(x="Number of deaths",y="Age group") +
  theme_minimal(base_size=10) +
  theme(legend.position="bottom",panel.grid.minor=element_blank(),
        strip.text=element_text(face="bold"))

# ── FIG 7: Provincial bar ────────────────────────────────────────────────────
fig_provincial <- ggplot(prov_summary,aes(x=n,y=reorder(province,n))) +
  geom_col(fill="steelblue",alpha=.8,width=.7) +
  geom_text(aes(label=comma(n)),hjust=-.1,size=3.2) +
  scale_x_continuous(labels=comma,expand=expansion(mult=c(0,.15))) +
  labs(x="Poisoning deaths",y=NULL) +
  theme_minimal(base_size=11) +
  theme(panel.grid.minor=element_blank(),panel.grid.major.y=element_blank())

# ── FIG 8: Top ICD subcodes ──────────────────────────────────────────────────
fig_top_icd <- ggplot(icd_sub,aes(x=n,y=label)) +
  geom_col(fill="steelblue",alpha=.8,width=.7) +
  geom_text(aes(label=comma(n)),hjust=-.1,size=3.2) +
  scale_x_continuous(labels=comma,expand=expansion(mult=c(0,.18))) +
  labs(x="Deaths",y=NULL) +
  theme_minimal(base_size=11) +
  theme(panel.grid.minor=element_blank(),panel.grid.major.y=element_blank())

# ── Age × Year heatmap data ──────────────────────────────────────────────────
age_year_grid <- p[!is.na(age_grp), .(n = .N), by = .(year, age_grp)]
full_grid <- CJ(year      = min(p$year):max(p$year),
                age_grp   = factor(AGE_LABELS, levels = AGE_LABELS))
age_year_grid <- merge(full_grid, age_year_grid, by = c("year","age_grp"), all.x = TRUE)
age_year_grid[is.na(n), n := 0L]
yr_tots       <- age_year_grid[, .(yr_total = sum(n)), by = year]
age_year_grid <- merge(age_year_grid, yr_tots, by = "year")
age_year_grid[, prop        := 100 * n / pmax(yr_total, 1)]
age_year_grid[, age_grp_rev := factor(age_grp, levels = rev(AGE_LABELS))]

# ── FIG 9a: Age × Year count heatmap ─────────────────────────────────────────
fig_age_year_count_heatmap <- ggplot(age_year_grid,
    aes(x = year, y = age_grp_rev, fill = n)) +
  geom_tile(colour = "white", linewidth = 0.3) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1,
                       name = "Deaths\n(count)",
                       labels = comma) +
  scale_x_continuous(breaks = seq(1997, 2022, 3)) +
  labs(x = NULL, y = "Age group") +
  theme_minimal(base_size = 11) +
  theme(panel.grid      = element_blank(),
        axis.text.x     = element_text(angle = 45, hjust = 1),
        legend.key.height = unit(1.2, "cm"))

# ── FIG 9b: Age × Year proportional heatmap ──────────────────────────────────
fig_age_year_heatmap <- ggplot(age_year_grid,
    aes(x = year, y = age_grp_rev, fill = prop)) +
  geom_tile(colour = "white", linewidth = 0.3) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1,
                       name = "% of year's\ndeaths") +
  scale_x_continuous(breaks = seq(1997, 2022, 3)) +
  labs(x = NULL, y = "Age group") +
  theme_minimal(base_size = 11) +
  theme(panel.grid      = element_blank(),
        axis.text.x     = element_text(angle = 45, hjust = 1),
        legend.key.height = unit(1.2, "cm"))

# ── EAPC (Estimated Annual % Change) by age group ────────────────────────────
fit_eapc <- function(d) {
  d2 <- copy(d)
  d2[n == 0, n := 0.5]                       # continuity correction
  m  <- tryCatch(lm(log(n) ~ year, data = d2), error = function(e) NULL)
  if (is.null(m) || nrow(d2) < 5)
    return(data.table(eapc = NA_real_, ci_lo = NA_real_,
                      ci_hi = NA_real_, p_val = NA_real_))
  b  <- coef(m)["year"]
  se <- sqrt(vcov(m)["year","year"])
  data.table(eapc  = (exp(b)            - 1) * 100,
             ci_lo = (exp(b - 1.96*se)  - 1) * 100,
             ci_hi = (exp(b + 1.96*se)  - 1) * 100,
             p_val = summary(m)$coefficients["year","Pr(>|t|)"])
}

age_eapc <- age_year_grid[, fit_eapc(.SD), by = age_grp]
age_eapc[, age_grp   := factor(age_grp, levels = AGE_LABELS)]
age_eapc[, sig       := fifelse(!is.na(p_val) & p_val < 0.05, "p<0.05", "p\u22650.05")]
age_eapc[, direction := fifelse(eapc >= 0, "Increase", "Decrease")]

# ── FIG 10: EAPC lollipop / forest plot ──────────────────────────────────────
fig_age_eapc <- ggplot(age_eapc[!is.na(eapc)],
    aes(y = age_grp, x = eapc,
        colour = direction, alpha = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbar(aes(xmin = ci_lo, xmax = ci_hi),
                width = 0.3, linewidth = 0.6,
                orientation = "y") +
  geom_point(size = 3.2) +
  scale_colour_manual(values = c("Increase" = "firebrick",
                                  "Decrease" = "steelblue"),
                      name = NULL) +
  scale_alpha_manual(values = c("p<0.05" = 1.0, "p\u22650.05" = 0.35),
                     name = "Significance") +
  scale_y_discrete(limits = rev(levels(age_eapc$age_grp))) +
  labs(x = "Estimated annual % change (EAPC, 1997\u20132022)",
       y = "Age group") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        legend.position  = "bottom")

# ── Age × Year × Sex grid ───────────────────────────────────────────────────
age_yr_sex <- p[sex_label %in% c("Male","Female") & !is.na(age_grp),
  .(n = .N), by = .(year, age_grp, sex_label)]
full_sex_grid <- CJ(year      = min(p$year):max(p$year),
                    age_grp   = factor(AGE_LABELS, levels = AGE_LABELS),
                    sex_label = c("Male","Female"))
age_yr_sex <- merge(full_sex_grid, age_yr_sex,
                    by = c("year","age_grp","sex_label"), all.x = TRUE)
age_yr_sex[is.na(n), n := 0L]
# within-sex annual proportions (% of same-sex deaths that year)
age_yr_sex[, yr_sex_tot := sum(n), by = .(year, sex_label)]
age_yr_sex[, prop       := 100 * n / pmax(yr_sex_tot, 1)]
age_yr_sex[, age_grp_rev := factor(age_grp, levels = rev(AGE_LABELS))]
age_yr_sex[, sex_label  := factor(sex_label, levels = c("Male","Female"))]

# ── EAPC by age group × sex ───────────────────────────────────────────────────
age_sex_eapc <- age_yr_sex[, fit_eapc(.SD), by = .(age_grp, sex_label)]
age_sex_eapc[, age_grp   := factor(age_grp,   levels = AGE_LABELS)]
age_sex_eapc[, sex_label := factor(sex_label, levels = c("Male","Female"))]
age_sex_eapc[, sig       := fifelse(!is.na(p_val) & p_val < 0.05, "p<0.05", "p\u22650.05")]
age_sex_eapc[, direction := fifelse(eapc >= 0, "Increase", "Decrease")]

# ── FIG 11a: Age × Year COUNT heatmap (faceted by sex) ───────────────────────
fig_age_year_sex_count <- ggplot(age_yr_sex,
    aes(x = year, y = age_grp_rev, fill = n)) +
  geom_tile(colour = "white", linewidth = 0.25) +
  facet_wrap(~ sex_label, ncol = 2) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1,
                       name = "Deaths\n(count)", labels = comma) +
  scale_x_continuous(breaks = seq(1997, 2022, 5)) +
  labs(x = NULL, y = "Age group") +
  theme_minimal(base_size = 11) +
  theme(panel.grid        = element_blank(),
        axis.text.x       = element_text(angle = 45, hjust = 1),
        strip.text        = element_text(face = "bold"),
        legend.key.height = unit(1.1, "cm"))

# ── FIG 11b: Age × Year PROPORTIONAL heatmap (faceted by sex) ────────────────
fig_age_year_sex_prop <- ggplot(age_yr_sex,
    aes(x = year, y = age_grp_rev, fill = prop)) +
  geom_tile(colour = "white", linewidth = 0.25) +
  facet_wrap(~ sex_label, ncol = 2) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1,
                       name = "% of same-\nsex deaths") +
  scale_x_continuous(breaks = seq(1997, 2022, 5)) +
  labs(x = NULL, y = "Age group") +
  theme_minimal(base_size = 11) +
  theme(panel.grid        = element_blank(),
        axis.text.x       = element_text(angle = 45, hjust = 1),
        strip.text        = element_text(face = "bold"),
        legend.key.height = unit(1.1, "cm"))

# ── FIG 12: EAPC by age group — Male vs Female comparative forest plot ────────
SEX_COLS <- c("Male" = "steelblue", "Female" = "firebrick")
fig_age_sex_eapc <- ggplot(age_sex_eapc[!is.na(eapc)],
    aes(y = age_grp, x = eapc,
        colour = sex_label,
        alpha  = sig,
        shape  = sex_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbar(
    aes(xmin = ci_lo, xmax = ci_hi),
    width = 0, linewidth = 0.6,
    orientation  = "y",
    position     = position_dodge(width = 0.55)) +
  geom_point(
    size         = 3.0,
    position     = position_dodge(width = 0.55)) +
  scale_colour_manual(values = SEX_COLS, name = NULL) +
  scale_shape_manual( values = c("Male" = 16, "Female" = 17), name = NULL) +
  scale_alpha_manual( values = c("p<0.05" = 1.0, "p\u22650.05" = 0.30),
                      name = "Significance") +
  scale_y_discrete(limits = rev(levels(age_sex_eapc$age_grp))) +
  labs(x = "Estimated annual % change (EAPC, 1997\u20132022)",
       y = "Age group") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        legend.position  = "bottom")

# ── YLL — WHO standard life table (Murray 1994 / GBD reference) ─────────────
# Reference: Murray CJL (1994) Quantifying the burden of disease.
# Life expectancy at exact ages for the WHO standard (LE at birth = 82.5 yrs).
yll_lt <- data.table(
  age = c(0,  1,  5,  10, 15, 20, 25, 30, 35, 40,
          45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100),
  ex  = c(82.51, 81.63, 77.69, 72.72, 67.75, 62.79, 57.85, 52.93, 48.02, 43.14,
          38.28, 33.49, 28.82, 24.27, 19.87, 15.68, 11.79,  8.29,  5.31,  3.14,
           1.72,  0.97))

# Linear interpolation to get remaining life expectancy at any given age
get_le <- function(ages) {
  approx(x = yll_lt$age, y = yll_lt$ex, xout = pmin(ages, 100),
         method = "linear", rule = 2)$y
}

# Individual-level YLL — requires non-missing age and known sex
p_yll <- p[sex_label %in% c("Male","Female") & !is.na(age) & !is.na(age_grp)]
p_yll[, yll := get_le(age)]

# ── YLL by age group × sex (aggregate across all years) ──────────────────────
yll_age_sex <- p_yll[, .(
  deaths        = .N,
  yll_total     = sum(yll),
  yll_per_death = mean(yll)
), by = .(age_grp, sex_label)]
yll_age_sex[, age_grp   := factor(age_grp,   levels = AGE_LABELS)]
yll_age_sex[, sex_label := factor(sex_label, levels = c("Male","Female"))]
setorder(yll_age_sex, age_grp, sex_label)

# ── YLL summary table (wide format) ──────────────────────────────────────────
yll_wide <- dcast(yll_age_sex, age_grp ~ sex_label,
  value.var = c("deaths","yll_total","yll_per_death"), fill = 0)
yll_wide[, Total_deaths   := deaths_Male   + deaths_Female]
yll_wide[, Total_YLL      := round(yll_total_Male + yll_total_Female)]
tbl_yll_age_sex <- yll_wide[, .(
  `Age group`      = age_grp,
  `Male deaths`    = deaths_Male,
  `Male YLL`       = round(yll_total_Male),
  `Male YLL/death` = round(yll_per_death_Male, 1),
  `Female deaths`  = deaths_Female,
  `Female YLL`     = round(yll_total_Female),
  `Female YLL/death` = round(yll_per_death_Female, 1),
  `Total deaths`   = Total_deaths,
  `Total YLL`      = Total_YLL
)]

# ── YLL by year × sex (for trend) ────────────────────────────────────────────
yll_year_sex <- p_yll[, .(
  yll_total = sum(yll),
  deaths    = .N
), by = .(year, sex_label)]
yll_year_sex[, sex_label := factor(sex_label, levels = c("Male","Female"))]
setorder(yll_year_sex, year, sex_label)

# ── YLL by age group × year (for heatmap) ────────────────────────────────────
yll_age_year <- p_yll[, .(yll_total = sum(yll), deaths = .N),
                      by = .(year, age_grp)]
yll_full_grid <- CJ(year    = min(p$year):max(p$year),
                    age_grp = factor(AGE_LABELS, levels = AGE_LABELS))
yll_age_year  <- merge(yll_full_grid, yll_age_year,
                       by = c("year","age_grp"), all.x = TRUE)
yll_age_year[is.na(yll_total), yll_total := 0]
yll_age_year[is.na(deaths),    deaths    := 0L]
yll_age_year[, age_grp_rev := factor(age_grp, levels = rev(AGE_LABELS))]

# ── Overall YLL summary ───────────────────────────────────────────────────────
yll_overall <- list(
  total_yll        = round(sum(p_yll$yll)),
  male_yll         = round(sum(p_yll[sex_label=="Male",  yll])),
  female_yll       = round(sum(p_yll[sex_label=="Female",yll])),
  mean_yll_death   = round(mean(p_yll$yll), 1)
)

# ── FIG YLL-1: Total YLL by age group, grouped by sex ────────────────────────
fig_yll_age_sex <- ggplot(yll_age_sex,
    aes(x = age_grp, y = yll_total / 1e3, fill = sex_label)) +
  geom_col(position = position_dodge(width = 0.8),
           alpha = 0.85, width = 0.75) +
  scale_fill_manual(values = c(Male = "steelblue", Female = "firebrick"),
                    name = NULL) +
  scale_y_continuous(labels = comma,
                     expand = expansion(mult = c(0, 0.07))) +
  labs(x = "Age group",
       y = "Total YLL (thousands, 1997\u20132022)") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        axis.text.x      = element_text(angle = 45, hjust = 1),
        legend.position  = "bottom")

# ── FIG YLL-2: Mean YLL per death by age group ───────────────────────────────
fig_yll_per_death <- ggplot(yll_age_sex,
    aes(x = age_grp, y = yll_per_death,
        colour = sex_label, group = sex_label)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_colour_manual(values = c(Male = "steelblue", Female = "firebrick"),
                      name = NULL) +
  scale_y_continuous(limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Age group",
       y = "Mean YLL per death (standard life-years)") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        axis.text.x      = element_text(angle = 45, hjust = 1),
        legend.position  = "bottom")

# ── FIG YLL-3: Annual YLL trend by sex ───────────────────────────────────────
fig_yll_year_sex <- ggplot(yll_year_sex,
    aes(x = year, y = yll_total / 1e3, colour = sex_label)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  scale_colour_manual(values = c(Male = "steelblue", Female = "firebrick"),
                      name = NULL) +
  scale_x_continuous(breaks = seq(min(yll_year_sex$year),
                                  max(yll_year_sex$year), 2)) +
  scale_y_continuous(labels = comma,
                     expand = expansion(mult = c(0.02, 0.07))) +
  labs(x = NULL, y = "Total YLL (thousands)") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        legend.position  = "bottom")

# ── FIG YLL-4: YLL heatmap by age group × year ───────────────────────────────
fig_yll_age_year <- ggplot(yll_age_year,
    aes(x = year, y = age_grp_rev, fill = yll_total / 1e3)) +
  geom_tile(colour = "white", linewidth = 0.3) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1,
                       name = "YLL\n(thousands)",
                       labels = comma) +
  scale_x_continuous(breaks = seq(1997, 2022, 3)) +
  labs(x = NULL, y = "Age group") +
  theme_minimal(base_size = 11) +
  theme(panel.grid        = element_blank(),
        axis.text.x       = element_text(angle = 45, hjust = 1),
        legend.key.height = unit(1.2, "cm"))

# ── Death institution — facility type analysis ───────────────────────────────
# Detailed breakdown: X40–X49 by institution
acc_facility_detail <- p[intent == "Accidental/unspec.",
  .(n = .N), by = .(death_inst, facility_grp)]
setorder(acc_facility_detail, -n)
acc_facility_detail[, pct := round(100 * n / sum(n), 1)]

# All poisoning (excl. "Other"): facility group × intent proportions
facility_intent_ct <- p[intent != "Other",
  .(n = .N), by = .(facility_grp, intent)]
facility_intent_prop <- copy(facility_intent_ct)
facility_intent_prop[, pct := 100 * n / sum(n), by = facility_grp]
facility_intent_prop[, intent := factor(intent,
  levels = c("Accidental/unspec.","Self-harm","Assault","Undetermined"))]

# Table A: all poisoning — intent × facility group (wide)
tbl_facility_intent <- dcast(
  facility_intent_ct, facility_grp ~ intent,
  value.var = "n", fill = 0L)
tbl_facility_intent[, Total := `Accidental/unspec.` + `Self-harm` + Assault + Undetermined]
tbl_facility_intent[, `% Acc./unspec.` := round(100 * `Accidental/unspec.` / Total, 1)]
setnames(tbl_facility_intent, "facility_grp", "Facility setting")

# Table B: X40–X49 by detailed institution (known records only)
tbl_facility_acc <- acc_facility_detail[
  !death_inst %in% c("Unknown","Unspecified"),
  .(`Death institution` = as.character(death_inst),
    `Facility group`    = as.character(facility_grp),
    N                   = n,
    `% of X40-49`       = pct)]

# ── FIG INST-1: X40–X49 by death institution (horizontal bar) ────────────────
INST_COLS <- c(
  "Hospital"                  = "#2166AC",
  "Emergency room/Outpatient" = "#74ADD1",
  "Dead on arrival"           = "#ABD9E9",
  "Nursing home"              = "#E0F3F8",
  "Home"                      = "#FDAE61",
  "Other"                     = "#F46D43",
  "Unknown"                   = "#CCCCCC",
  "Unspecified"               = "#AAAAAA"
)
fig_facility_acc <- ggplot(
    acc_facility_detail[!death_inst %in% c("Unknown","Unspecified")],
    aes(x = pct, y = reorder(death_inst, pct), fill = death_inst)) +
  geom_col(alpha = 0.85, show.legend = FALSE) +
  geom_text(aes(label = paste0(pct, "%  (n=", scales::comma(n), ")")),
    hjust = -0.05, size = 3.2) +
  scale_fill_manual(values = INST_COLS) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.35))) +
  labs(x = "% of X40\u201349 poisoning deaths", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_blank())

# ── FIG INST-2: Intent composition within each facility setting ───────────────
fig_facility_intent <- ggplot(facility_intent_prop,
    aes(x = pct, y = facility_grp, fill = intent)) +
  geom_col(position = "stack", alpha = 0.85) +
  geom_text(aes(label = ifelse(pct >= 3, paste0(round(pct, 0), "%"), "")),
    position = position_stack(vjust = 0.5),
    size = 3, colour = "white", fontface = "bold") +
  scale_fill_manual(values = INTENT_COLS, name = NULL) +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(x = "% of poisoning deaths within setting", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        legend.position  = "bottom")

# ── Metadata & save ──────────────────────────────────────────────────────────
analysis_metadata <- list(
  input_file           = input_file,
  n_records_total      = format(nrow(dt),big.mark=","),
  n_poisoning_records  = format(nrow(p), big.mark=","),
  poisoning_definition = "X40-X49, X60-X69, X85, Y10-Y19",
  year_range           = paste(min(p$year),"-",max(p$year))
)

save(monthly_intent,monthly_total,annual_total,annual_intent,
     tbl_overall,tbl_year_summary,tbl_age_sex,
     pyramid_data,pyramid_intent,prov_summary,icd_sub,seasonality_avg,
     age_year_grid,age_eapc,
     age_yr_sex,age_sex_eapc,
     yll_age_sex,tbl_yll_age_sex,yll_year_sex,yll_age_year,yll_overall,
  annual_pop,annual_counts,annual_crude,fig_crude_rate_trend,
  age_rate_grid,fig_age_year_sex_rate,
  district_rates_latest,top5_districts,district_map_sf,fig_district_rate_map,
  recent_years,district_rates_3yr,district_hotspot_map_3yr_sf,fig_district_hotspot_map_3yr,
     acc_facility_detail,facility_intent_prop,tbl_facility_intent,tbl_facility_acc,
     fig_annual_trend,fig_intent_trend,fig_weekly_epicurve,
     fig_seasonality,fig_pyramid,fig_pyramid_intent,
     fig_provincial,fig_top_icd,
     fig_age_year_count_heatmap,fig_age_year_heatmap,fig_age_eapc,
     fig_age_year_sex_count,fig_age_year_sex_prop,fig_age_sex_eapc,
     fig_yll_age_sex,fig_yll_per_death,fig_yll_year_sex,fig_yll_age_year,
     fig_facility_acc,fig_facility_intent,
     analysis_metadata,
     file=output_rda)
message("Done. Saved to: ", output_rda)
