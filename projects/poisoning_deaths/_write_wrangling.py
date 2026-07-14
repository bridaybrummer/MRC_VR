"""Helper: writes poisoning_wrangling.R from Python to avoid shell-escaping issues."""
import pathlib

R_CODE = r"""suppressPackageStartupMessages({
  library(arrow); library(data.table); library(ggplot2)
  library(lubridate); library(scales)
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
  grepl("^X4[0-9]", icd_clean), "Accidental",
  grepl("^X6[0-9]", icd_clean), "Self-harm",
  grepl("^X85",     icd_clean), "Assault",
  grepl("^Y1[0-9]", icd_clean), "Undetermined",
  default = "Other"
)]
p[, intent := factor(intent, levels = c("Accidental","Self-harm","Assault","Undetermined","Other"))]

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

# ── Age groups ───────────────────────────────────────────────────────────────
AGE_BREAKS <- c(0,5,15,25,35,45,55,65,75,Inf)
AGE_LABELS <- c("0-4","5-14","15-24","25-34","35-44","45-54","55-64","65-74","75+")
p[, age_grp := cut(age, breaks=AGE_BREAKS, labels=AGE_LABELS, right=FALSE)]
p[, year    := as.integer(DeathYear)]
p[, month   := as.integer(DeathMonth)]
p[, date_month := as.Date(sprintf("%d-%02d-01", year, month))]

# ── Weekly epicurve ──────────────────────────────────────────────────────────
weekly_poisoning <- p[!is.na(epi_year)&!is.na(epi_week),
  .(poisoning_deaths=.N), by=.(year=as.integer(epi_year), week=as.integer(epi_week))]
setorder(weekly_poisoning,year,week)
weekly_poisoning[, week_start := as.Date(
  strptime(sprintf("%d-W%02d-1",year,week), format="%G-W%V-%u", tz="UTC"))]

# ── Annual summaries ─────────────────────────────────────────────────────────
annual_total  <- p[,.N,by=year]; setorder(annual_total,year)
annual_intent <- p[,.N,by=.(year,intent)]; setorder(annual_intent,year,intent)

tbl_year_summary <- p[,.(
  Total        = .N,
  Accidental   = sum(intent=="Accidental"),
  `Self-harm`  = sum(intent=="Self-harm"),
  Assault      = sum(intent=="Assault"),
  Undetermined = sum(intent=="Undetermined"),
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
                    intent %in% c("Accidental","Self-harm","Undetermined"),
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

# ── Top 15 ICD subcodes ──────────────────────────────────────────────────────
icd_sub <- p[,.(n=.N),by=.(icd=substr(icd_clean,1,3))]
setorder(icd_sub,-n); icd_sub <- head(icd_sub,15)
icd_lu <- c(
  X40="X40  Accidental - analgesics (non-opioid)",
  X41="X41  Accidental - antiepileptics/sedatives",
  X42="X42  Accidental - narcotics/psychodysleptics",
  X44="X44  Accidental - other/unspecified drugs",
  X45="X45  Accidental - alcohol",
  X47="X47  Accidental - gases & vapours",
  X48="X48  Accidental - pesticides",
  X49="X49  Accidental - other chemicals/NOS",
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
             "Median age (years)","% Accidental","% Self-harm",
             "% Assault","% Undetermined"),
  Value  = c(
    format(nrow(p),big.mark=","),
    paste(min(p$year),"-",max(p$year)),
    paste0(round(100*mean(p$sex_label=="Male",na.rm=TRUE),1),"%"),
    as.character(round(median(p$age,na.rm=TRUE),1)),
    paste0(round(100*mean(p$intent=="Accidental"),1),"%"),
    paste0(round(100*mean(p$intent=="Self-harm"),1),"%"),
    paste0(round(100*mean(p$intent=="Assault"),1),"%"),
    paste0(round(100*mean(p$intent=="Undetermined"),1),"%")
  )
)

# ── Colour palette ───────────────────────────────────────────────────────────
INTENT_COLS <- c("Accidental"="steelblue","Self-harm"="firebrick",
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

# ── FIG 3: Weekly epicurve ───────────────────────────────────────────────────
fig_weekly_epicurve <- ggplot(weekly_poisoning,aes(x=week_start,y=poisoning_deaths)) +
  geom_col(fill="steelblue",alpha=.65,width=5) +
  geom_smooth(method="loess",span=.06,se=FALSE,colour="firebrick",linewidth=.9) +
  scale_x_date(date_breaks="2 years",date_labels="%Y") +
  scale_y_continuous(labels=comma,expand=expansion(mult=c(0,.07))) +
  labs(x=NULL,y="Deaths per week") +
  theme_minimal(base_size=11) + theme(panel.grid.minor=element_blank())

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

# ── Metadata & save ──────────────────────────────────────────────────────────
analysis_metadata <- list(
  input_file           = input_file,
  n_records_total      = format(nrow(dt),big.mark=","),
  n_poisoning_records  = format(nrow(p), big.mark=","),
  poisoning_definition = "X40-X49, X60-X69, X85, Y10-Y19",
  year_range           = paste(min(p$year),"-",max(p$year))
)

save(weekly_poisoning,annual_total,annual_intent,
     tbl_overall,tbl_year_summary,tbl_age_sex,
     pyramid_data,pyramid_intent,prov_summary,icd_sub,seasonality_avg,
     fig_annual_trend,fig_intent_trend,fig_weekly_epicurve,
     fig_seasonality,fig_pyramid,fig_pyramid_intent,
     fig_provincial,fig_top_icd,analysis_metadata,
     file=output_rda)
message("Done. Saved to: ", output_rda)
"""

dest = pathlib.Path("/Users/briday/Desktop/study_stats/MRC_VR/projects/poisoning_deaths/poisoning_wrangling.R")
dest.write_text(R_CODE)
print(f"Written: {dest} ({dest.stat().st_size} bytes)")
