# =============================================================================
# ward_level_dha_access_script.R
# Computes ALL outputs for ward_level_dha_access.qmd and saves them to RData.
# The .qmd only loads and prints these precomputed objects.
# =============================================================================

library(sf)
library(dplyr)
library(readxl)
library(ggplot2)
library(haven)
library(tidyr)
library(stringr)
library(scales)
library(flextable)
library(glue)

sf_use_s2(FALSE)

# =============================================================================
# 1. Load shared data
# =============================================================================

# ---- DHA offices ----
dha_path <- "DHA_offices_2024.dta"
dha_df   <- read_dta(dha_path)
dha_sf   <- dha_df %>%
  filter(!is.na(office_longitude) & !is.na(office_latitude)) %>%
  mutate(office_name = off_new) %>%
  st_as_sf(coords = c("office_longitude", "office_latitude"), crs = 4326)
dha_prj  <- st_transform(dha_sf, 3857)

# ---- Ward-level shapefiles & population ----
shp_path <- "ward_level_shp_pop/MDB_Wards_2020_-7628782095861510160/MDB_Wards_2020.shp"
pop_path <- "ward_level_shp_pop/PP_Sex_27-10-2025.xlsx"

wards_sf <- st_read(shp_path, quiet = TRUE)
pop_df   <- read_excel(pop_path, col_types = "text") %>%
  mutate(
    WARD_CODE = as.numeric(WARD_CODE),
    Total     = as.numeric(Total),
    Male      = as.numeric(Male),
    Female    = as.numeric(Female)
  ) %>%
  filter(!is.na(WARD_CODE))

# ---- District-level objects (from plotting_DHA_offices.r) ----
district_objects_path <- "outputs/dha_me_indicators/dha_me_indicator_objects.RData"
if (file.exists(district_objects_path)) {
  load(district_objects_path)
  # Provides: SA_pop, SA_N_off, p_with_buffers, off_pop_shape,
  #   dha_offices_distance_summary, dha_access_summary_table,
  #   modelled_population_map, etc.
} else {
  stop("District-level objects not found. Run plotting_DHA_offices.r first.")
}

if (file.exists("dha_access_summary_gradient.rda")) {
  load("dha_access_summary_gradient.rda")
}

# =============================================================================
# 2. Method 1: District-Level Office-to-Office Distance
# =============================================================================

if (exists("dha_offices_distance_summary") && exists("off_pop_shape")) {
  m1_summary <- dha_offices_distance_summary %>%
    st_drop_geometry() %>%
    select(district_standard, median_nearest_dha_distance_km, half_median_nearest_dha_distance_km)

  m1_with_pop <- off_pop_shape %>%
    st_drop_geometry() %>%
    select(district_standard, pop) %>%
    left_join(m1_summary, by = "district_standard") %>%
    mutate(
      access_5km  = half_median_nearest_dha_distance_km <= 5,
      access_10km = half_median_nearest_dha_distance_km <= 10,
      access_20km = half_median_nearest_dha_distance_km <= 20
    )

  m1_national <- m1_with_pop %>%
    summarise(
      total_pop = sum(pop, na.rm = TRUE),
      pop_5km   = sum(pop[access_5km  == TRUE], na.rm = TRUE),
      pop_10km  = sum(pop[access_10km == TRUE], na.rm = TRUE),
      pop_20km  = sum(pop[access_20km == TRUE], na.rm = TRUE)
    ) %>%
    mutate(
      pct_5km  = round(pop_5km  / total_pop * 100, 1),
      pct_10km = round(pop_10km / total_pop * 100, 1),
      pct_20km = round(pop_20km / total_pop * 100, 1)
    )

  # Formatted table for display
  m1_table <- m1_with_pop %>%
    arrange(half_median_nearest_dha_distance_km) %>%
    mutate(
      pop = comma(pop),
      median_nearest_dha_distance_km = round(median_nearest_dha_distance_km, 1),
      half_median_nearest_dha_distance_km = round(half_median_nearest_dha_distance_km, 1)
    ) %>%
    flextable() %>%
    set_header_labels(
      district_standard = "District",
      pop = "Population",
      median_nearest_dha_distance_km = "Median Inter-Office Distance (km)",
      half_median_nearest_dha_distance_km = "Half-Median Distance (km)",
      access_5km  = "Within 5km?",
      access_10km = "Within 10km?",
      access_20km = "Within 20km?"
    ) %>%
    bold(part = "header") %>%
    theme_zebra() %>%
    autofit()
}

# =============================================================================
# 3. Method 2: District-Level Modelled Population Grid
# =============================================================================

if (exists("dha_access_summary_table")) {
  m2_pct_5km  <- dha_access_summary_table$body$data[nrow(dha_access_summary_table$body$data), ]["pct_within_5km"]
  m2_pct_10km <- dha_access_summary_table$body$data[nrow(dha_access_summary_table$body$data), ]["pct_within_10km"]
  m2_pct_20km <- dha_access_summary_table$body$data[nrow(dha_access_summary_table$body$data), ]["pct_within_20km"]
} else {
  m2_pct_5km  <- "N/A"
  m2_pct_10km <- "N/A"
  m2_pct_20km <- "N/A"
}

# =============================================================================
# 4. Method 3: Ward-Level Centroid Distance
# =============================================================================

# Join population to wards
wards_sf <- wards_sf %>% mutate(WardID_num = as.numeric(WardID))
ward_pop_sf <- left_join(wards_sf, pop_df, by = c("WardID_num" = "WARD_CODE"))
missing_pop <- sum(is.na(ward_pop_sf$Total))

# Project and compute distances
ward_pop_prj   <- st_transform(ward_pop_sf, 3857)
ward_centroids <- st_centroid(ward_pop_prj)
nearest_idx    <- st_nearest_feature(ward_centroids, dha_prj)
dist_vec       <- st_distance(ward_centroids, dha_prj[nearest_idx, ], by_element = TRUE)

ward_pop_sf$nearest_dha_dist_km <- as.numeric(dist_vec) / 1000
ward_pop_sf$nearest_dha_name    <- dha_prj$office_name[nearest_idx]

# Classify access bands
ward_pop_sf <- ward_pop_sf %>%
  mutate(access_category = case_when(
    nearest_dha_dist_km <= 5  ~ "< 5 km",
    nearest_dha_dist_km <= 10 ~ "5 - 10 km",
    nearest_dha_dist_km <= 20 ~ "10 - 20 km",
    nearest_dha_dist_km <= 50 ~ "20 - 50 km",
    TRUE ~ "> 50 km"
  ))

# Summary table (data frame)
ward_access_summary <- ward_pop_sf %>%
  st_drop_geometry() %>%
  group_by(access_category) %>%
  summarise(
    wards_count      = n(),
    total_population = sum(Total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(percent_pop = round(total_population / sum(total_population) * 100, 1)) %>%
  arrange(factor(access_category, levels = c("< 5 km", "5 - 10 km", "10 - 20 km", "20 - 50 km", "> 50 km")))

# Formatted flextable for display
ward_access_ft <- ward_access_summary %>%
  mutate(total_population = comma(total_population)) %>%
  flextable() %>%
  set_header_labels(
    access_category  = "Distance to Nearest DHA Office",
    wards_count      = "Number of Wards",
    total_population = "Population",
    percent_pop      = "% of Population"
  ) %>%
  bold(part = "header") %>%
  theme_zebra() %>%
  autofit()

# Key percentages
m3_pct_5km <- ward_access_summary %>%
  filter(access_category == "< 5 km") %>%
  pull(percent_pop)

m3_pct_10km <- ward_access_summary %>%
  filter(access_category %in% c("< 5 km", "5 - 10 km")) %>%
  summarise(p = sum(percent_pop)) %>%
  pull(p)

m3_pct_20km <- ward_access_summary %>%
  filter(access_category %in% c("< 5 km", "5 - 10 km", "10 - 20 km")) %>%
  summarise(p = sum(percent_pop)) %>%
  pull(p)

# Distance summary
m3_dist_summary <- summary(ward_pop_sf$nearest_dha_dist_km)

# ---- Method 3 Plots ----

# Ward distance map
ward_distance_map <- ggplot() +
  geom_sf(data = ward_pop_sf, aes(fill = nearest_dha_dist_km), color = NA) +
  geom_sf(data = dha_sf, color = "red", size = 0.5, alpha = 0.5) +
  scale_fill_viridis_c(
    name   = "Distance (km)",
    option = "magma",
    trans  = "log1p",
    breaks = c(0, 5, 10, 20, 50, 100),
    labels = c("0", "5", "10", "20", "50", "100")
  ) +
  theme_minimal() +
  labs(
    title    = "Distance to Nearest DHA Office by Ward",
    subtitle = "Calculated from ward centroids to nearest DHA office"
  )

# Cumulative coverage curve data
coverage_curve <- ward_pop_sf %>%
  st_drop_geometry() %>%
  arrange(nearest_dha_dist_km) %>%
  mutate(
    cum_pop       = cumsum(coalesce(Total, 0)),
    total_pop_all = sum(Total, na.rm = TRUE),
    cum_pct       = cum_pop / total_pop_all * 100
  )

# Cumulative coverage plot
coverage_curve_plot <- ggplot(coverage_curve, aes(x = nearest_dha_dist_km, y = cum_pct)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_vline(xintercept = c(5, 10, 20, 50), linetype = "dashed", color = "gray50") +
  annotate("label", x = 5,  y = 40, label = "5 km",  size = 3, fill = "white") +
  annotate("label", x = 10, y = 50, label = "10 km", size = 3, fill = "white") +
  annotate("label", x = 20, y = 60, label = "20 km", size = 3, fill = "white") +
  annotate("label", x = 50, y = 70, label = "50 km", size = 3, fill = "white") +
  scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  labs(
    title = "Cumulative Population Coverage by Distance",
    x     = "Distance to Nearest DHA Office (km)",
    y     = "Cumulative % of Population"
  ) +
  theme_minimal()

# =============================================================================
# 5. Comparison Tables
# =============================================================================

comparison_df <- tibble::tribble(
  ~Feature, ~`Method 1`, ~`Method 2`, ~`Method 3`,
  "Geographic unit", "District", "1km grid within district", "Ward (~4,400 units)",
  "Population data", "District totals", "District totals (modelled to grid)", "Ward-level counts (observed)",
  "Population distribution", "Assumed uniform across district", "Modelled (inverse power decay near offices)", "Assigned to ward centroid",
  "Distance measure", "Half-median inter-office distance", "Grid point to nearest office", "Ward centroid to nearest office",
  "Key assumption", "People live between offices", "People cluster near offices", "People live near ward centre",
  "Key strength", "Simple; no spatial computation", "Models within-district variation", "Uses actual sub-district population data",
  "Key weakness", "Single estimate per district; ignores clustering", "Population model may not reflect reality", "Centroid may not represent large rural wards",
  "Data granularity", "52 districts", "~1.2 million grid points", "~4,400 wards",
  "Computational cost", "Low", "High (1km grid across SA)", "Moderate"
)

comparison_ft <- comparison_df %>%
  flextable() %>%
  bold(part = "header") %>%
  bg(j = "Method 3", bg = "#E8F5E9") %>%
  theme_zebra() %>%
  autofit() %>%
  set_caption("Comparison of the three DHA access methodologies")

comparison_results <- tibble::tribble(
  ~Metric, ~`Method 1`, ~`Method 2`, ~`Method 3`,
  "% within 5km",
  if (exists("m1_national")) paste0(m1_national$pct_5km,  "%") else "N/A",
  paste0(m2_pct_5km,  "%"),
  paste0(m3_pct_5km,  "%"),
  "% within 10km",
  if (exists("m1_national")) paste0(m1_national$pct_10km, "%") else "N/A",
  paste0(m2_pct_10km, "%"),
  paste0(m3_pct_10km, "%"),
  "% within 20km",
  if (exists("m1_national")) paste0(m1_national$pct_20km, "%") else "N/A",
  paste0(m2_pct_20km, "%"),
  paste0(m3_pct_20km, "%")
)

comparison_results_ft <- comparison_results %>%
  flextable() %>%
  bold(part = "header") %>%
  theme_zebra() %>%
  autofit() %>%
  set_caption("National population coverage estimates by method")

# =============================================================================
# 5b. Sensitivity: population density within 500 m of each DHA node
# =============================================================================
# Approach
# --------
# For each DHA office:
#   1. Draw a 500 m buffer in EPSG:3857 (~ 0.785 km^2 circle).
#   2. Intersect with ward polygons (also projected).
#   3. Estimate population in the buffer assuming population is uniformly
#      distributed within each ward (best available approximation without a
#      raster). For each ward fragment inside the buffer:
#            pop_frag = (area_frag / area_ward) * ward_total_pop
#   4. Sum across fragments -> estimated population within 500 m
#      Density (per km^2) = pop_500m / 0.7854
#
# Realism check: compare the distribution of node-level densities against the
# ward-level density distribution. DHA offices placed in genuinely urban areas
# should have node densities consistent with high-density wards (Hillbrow,
# Khayelitsha, etc.). Nodes returning implausibly high densities (e.g. >
# 99th percentile of ward density) flag locations where the uniform-within-ward
# assumption is breaking down (offices sitting inside a small dense pocket of
# a much larger sparse ward).

# Project wards once for area-weighted intersection
wards_for_sens <- ward_pop_sf %>%
  st_transform(3857) %>%
  mutate(ward_area_m2 = as.numeric(st_area(.))) %>%
  select(WardID, Total, ward_area_m2)

# Ward-level density (people per km^2) — used as the reference distribution
ward_density_ref <- wards_for_sens %>%
  st_drop_geometry() %>%
  mutate(ward_density_km2 = Total / (ward_area_m2 / 1e6)) %>%
  filter(!is.na(ward_density_km2), is.finite(ward_density_km2))

ward_density_quantiles <- quantile(
  ward_density_ref$ward_density_km2,
  probs = c(0.50, 0.75, 0.90, 0.95, 0.99),
  na.rm = TRUE
)

# Build buffers around DHA offices
dha_buffer_500m <- st_buffer(dha_prj, dist = 500)

# Intersect buffers with wards
sens_intersect <- suppressWarnings(
  st_intersection(dha_buffer_500m, wards_for_sens)
)
sens_intersect$frag_area_m2 <- as.numeric(st_area(sens_intersect))
sens_intersect$frag_pop <- with(
  sens_intersect,
  (frag_area_m2 / ward_area_m2) * Total
)

buffer_area_km2 <- pi * (0.5)^2  # ~ 0.7854 km^2

node_density_sens <- sens_intersect %>%
  st_drop_geometry() %>%
  group_by(office_name) %>%
  summarise(
    pop_within_500m = sum(frag_pop, na.rm = TRUE),
    n_wards_touched = dplyr::n(),
    .groups = "drop"
  ) %>%
  mutate(
    density_per_km2 = pop_within_500m / buffer_area_km2
  ) %>%
  arrange(desc(density_per_km2))

# Classify each node against the ward-density reference distribution
node_density_sens <- node_density_sens %>%
  mutate(
    density_band = cut(
      density_per_km2,
      breaks = c(-Inf,
                 ward_density_quantiles["50%"],
                 ward_density_quantiles["75%"],
                 ward_density_quantiles["90%"],
                 ward_density_quantiles["95%"],
                 ward_density_quantiles["99%"],
                 Inf),
      labels = c("< median ward",
                 "median – p75",
                 "p75 – p90",
                 "p90 – p95",
                 "p95 – p99",
                 "> p99 (very dense)"),
      include.lowest = TRUE
    )
  )

# Known high-density reference wards (urban centres). Names matched fuzzily
# against ward attributes where available; otherwise we report the top wards by
# density as the reference set.
top_density_wards <- ward_density_ref %>%
  arrange(desc(ward_density_km2)) %>%
  slice_head(n = 15) %>%
  mutate(ward_density_km2 = round(ward_density_km2))

node_density_summary <- node_density_sens %>%
  count(density_band, name = "n_offices") %>%
  mutate(pct = round(n_offices / sum(n_offices) * 100, 1))

# Flextables
sens_node_summary_ft <- node_density_summary %>%
  flextable() %>%
  set_header_labels(
    density_band = "Node density vs ward density distribution",
    n_offices    = "DHA offices",
    pct          = "% of offices"
  ) %>%
  bold(part = "header") %>%
  theme_zebra() %>%
  autofit() %>%
  set_caption("DHA offices classified by modelled population density within 500 m, benchmarked against the national ward-density distribution")

sens_top_nodes_ft <- node_density_sens %>%
  slice_head(n = 15) %>%
  mutate(
    pop_within_500m  = round(pop_within_500m),
    density_per_km2  = round(density_per_km2)
  ) %>%
  flextable() %>%
  set_header_labels(
    office_name     = "DHA office",
    pop_within_500m = "Modelled pop. within 500 m",
    n_wards_touched = "Wards intersected",
    density_per_km2 = "Density (people / km²)",
    density_band    = "Density band"
  ) %>%
  bold(part = "header") %>%
  theme_zebra() %>%
  autofit() %>%
  set_caption("Top 15 DHA offices by modelled population density within 500 m")

sens_top_wards_ft <- top_density_wards %>%
  select(WardID, Total, ward_area_m2, ward_density_km2) %>%
  mutate(
    ward_area_km2 = round(ward_area_m2 / 1e6, 2),
    Total = comma(Total)
  ) %>%
  select(WardID, Total, ward_area_km2, ward_density_km2) %>%
  flextable() %>%
  set_header_labels(
    WardID            = "Ward ID",
    Total             = "Ward population",
    ward_area_km2     = "Ward area (km²)",
    ward_density_km2  = "Density (people / km²)"
  ) %>%
  bold(part = "header") %>%
  theme_zebra() %>%
  autofit() %>%
  set_caption("Reference: 15 highest-density wards in South Africa (Stats SA 2025, MDB 2020 boundaries)")

# Histogram comparing node and ward density distributions (log scale)
sens_density_compare_plot <- ggplot() +
  geom_histogram(
    data = ward_density_ref,
    aes(x = ward_density_km2, y = after_stat(density), fill = "Wards"),
    bins = 50, alpha = 0.45
  ) +
  geom_histogram(
    data = node_density_sens %>% filter(density_per_km2 > 0),
    aes(x = density_per_km2, y = after_stat(density), fill = "DHA nodes (500 m)"),
    bins = 50, alpha = 0.55
  ) +
  scale_x_log10(labels = comma) +
  scale_fill_manual(values = c("Wards" = "steelblue", "DHA nodes (500 m)" = "firebrick")) +
  labs(
    title    = "Are DHA office locations in plausibly dense areas?",
    subtitle = "Modelled population density within 500 m of each office vs national ward-density distribution",
    x        = "People per km² (log scale)",
    y        = "Density",
    fill     = NULL
  ) +
  theme_minimal()

# Key sensitivity statistics
sens_median_node_density   <- round(median(node_density_sens$density_per_km2, na.rm = TRUE))
sens_p95_node_density      <- round(quantile(node_density_sens$density_per_km2, 0.95, na.rm = TRUE))
sens_median_ward_density   <- round(median(ward_density_ref$ward_density_km2, na.rm = TRUE))
sens_p95_ward_density      <- round(quantile(ward_density_ref$ward_density_km2, 0.95, na.rm = TRUE))
sens_pct_nodes_above_p90   <- round(
  mean(node_density_sens$density_per_km2 >= ward_density_quantiles["90%"], na.rm = TRUE) * 100,
  1
)

# =============================================================================
# 5c. Birth registration: include hospital-based DHA satellite offices
# =============================================================================
# Birth registration is offered at standalone DHA offices AND at a subset of
# public hospitals that host DHA satellite desks. The hospital list (curated by
# Nadine) is linked to MFL coordinates via DHA_access/link_DHA_to_MFL.r.
# For the birth-registration analysis we treat both as valid registration nodes
# and re-run Method 3 (ward centroid -> nearest node).

dha_mfl_matched_path <- "DHA_access/dha_mfl_matched.csv"

if (file.exists(dha_mfl_matched_path)) {
  dha_mfl_matched <- readr::read_csv(dha_mfl_matched_path, show_col_types = FALSE)

  hospital_dha_sf <- dha_mfl_matched %>%
    filter(!is.na(latitude), !is.na(longitude)) %>%
    transmute(
      office_name = paste0(facility_name, " (hospital DHA)"),
      latitude, longitude,
      node_type = "Hospital DHA satellite",
      match_quality
    ) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

  # Rebuild standalone DHA sf from raw df (dha_sf may have been overwritten by
  # the RData load above, which lacks the original off_new name column).
  standalone_dha_sf <- dha_df %>%
    filter(!is.na(office_longitude) & !is.na(office_latitude)) %>%
    mutate(
      office_name   = as.character(off_new),
      node_type     = "Standalone DHA office",
      match_quality = NA_character_
    ) %>%
    st_as_sf(coords = c("office_longitude", "office_latitude"), crs = 4326) %>%
    dplyr::select(office_name, node_type, match_quality)

  birth_nodes_sf <- bind_rows(standalone_dha_sf, hospital_dha_sf)
  birth_nodes_prj <- st_transform(birth_nodes_sf, 3857)

  # Recompute Method 3 against the augmented node set
  nearest_idx_b <- st_nearest_feature(ward_centroids, birth_nodes_prj)
  dist_vec_b    <- st_distance(ward_centroids, birth_nodes_prj[nearest_idx_b, ], by_element = TRUE)

  ward_pop_birth_sf <- ward_pop_sf
  ward_pop_birth_sf$nearest_dist_km   <- as.numeric(dist_vec_b) / 1000
  ward_pop_birth_sf$nearest_node_name <- birth_nodes_prj$office_name[nearest_idx_b]
  ward_pop_birth_sf$nearest_node_type <- birth_nodes_prj$node_type[nearest_idx_b]

  ward_pop_birth_sf <- ward_pop_birth_sf %>%
    mutate(access_category = case_when(
      nearest_dist_km <= 5  ~ "< 5 km",
      nearest_dist_km <= 10 ~ "5 - 10 km",
      nearest_dist_km <= 20 ~ "10 - 20 km",
      nearest_dist_km <= 50 ~ "20 - 50 km",
      TRUE ~ "> 50 km"
    ))

  birth_access_summary <- ward_pop_birth_sf %>%
    st_drop_geometry() %>%
    group_by(access_category) %>%
    summarise(
      wards_count      = dplyr::n(),
      total_population = sum(Total, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(percent_pop = round(total_population / sum(total_population) * 100, 1)) %>%
    arrange(factor(access_category,
                   levels = c("< 5 km", "5 - 10 km", "10 - 20 km", "20 - 50 km", "> 50 km")))

  birth_access_ft <- birth_access_summary %>%
    mutate(total_population = comma(total_population)) %>%
    flextable() %>%
    set_header_labels(
      access_category  = "Distance to nearest registration node",
      wards_count      = "Number of wards",
      total_population = "Population",
      percent_pop      = "% of population"
    ) %>%
    bold(part = "header") %>%
    theme_zebra() %>%
    autofit() %>%
    set_caption("Birth registration: coverage including hospital-based DHA satellite offices")

  birth_pct_5km  <- birth_access_summary %>% filter(access_category == "< 5 km") %>% pull(percent_pop)
  birth_pct_10km <- birth_access_summary %>%
    filter(access_category %in% c("< 5 km","5 - 10 km")) %>%
    summarise(p = sum(percent_pop)) %>% pull(p)
  birth_pct_20km <- birth_access_summary %>%
    filter(access_category %in% c("< 5 km","5 - 10 km","10 - 20 km")) %>%
    summarise(p = sum(percent_pop)) %>% pull(p)

  # Side-by-side comparison: deaths (DHA only) vs births (DHA + hospital)
  birth_vs_death_tbl <- tibble::tribble(
    ~Threshold, ~`Death reg. (DHA only)`, ~`Birth reg. (DHA + hospital)`, ~`Gain (pp)`,
    "< 5 km",  paste0(m3_pct_5km,  "%"), paste0(birth_pct_5km,  "%"), round(birth_pct_5km  - m3_pct_5km,  1),
    "< 10 km", paste0(m3_pct_10km, "%"), paste0(birth_pct_10km, "%"), round(birth_pct_10km - m3_pct_10km, 1),
    "< 20 km", paste0(m3_pct_20km, "%"), paste0(birth_pct_20km, "%"), round(birth_pct_20km - m3_pct_20km, 1)
  )

  birth_vs_death_ft <- birth_vs_death_tbl %>%
    flextable() %>%
    bold(part = "header") %>%
    bg(j = "Birth reg. (DHA + hospital)", bg = "#E8F5E9") %>%
    theme_zebra() %>%
    autofit() %>%
    set_caption("Population coverage: death registration (DHA offices only) vs birth registration (DHA + hospital-based DHA satellites)")

  # Birth registration map
  birth_distance_map <- ggplot() +
    geom_sf(data = ward_pop_birth_sf, aes(fill = nearest_dist_km), color = NA) +
    geom_sf(data = birth_nodes_sf, aes(color = node_type), size = 0.6, alpha = 0.7) +
    scale_fill_viridis_c(
      name   = "Distance (km)",
      option = "magma",
      trans  = "log1p",
      breaks = c(0, 5, 10, 20, 50, 100),
      labels = c("0","5","10","20","50","100")
    ) +
    scale_color_manual(
      name = "Registration node",
      values = c("Standalone DHA office" = "red", "Hospital DHA satellite" = "blue")
    ) +
    theme_minimal() +
    labs(
      title    = "Birth registration access",
      subtitle = "Distance from ward centroid to nearest DHA office OR hospital-based DHA satellite"
    )

  # Cumulative coverage curves overlaid: deaths vs births
  birth_curve <- ward_pop_birth_sf %>%
    st_drop_geometry() %>%
    arrange(nearest_dist_km) %>%
    mutate(
      cum_pop = cumsum(coalesce(Total, 0)),
      cum_pct = cum_pop / sum(Total, na.rm = TRUE) * 100,
      scope = "Birth registration (DHA + hospital)"
    ) %>%
    select(dist = nearest_dist_km, cum_pct, scope)

  death_curve_df <- coverage_curve %>%
    transmute(dist = nearest_dha_dist_km, cum_pct, scope = "Death registration (DHA only)")

  birth_vs_death_curve_plot <- ggplot(
    bind_rows(death_curve_df, birth_curve),
    aes(x = dist, y = cum_pct, color = scope)
  ) +
    geom_line(linewidth = 1) +
    geom_vline(xintercept = c(5, 10, 20, 50), linetype = "dashed", color = "gray60") +
    scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
    scale_y_continuous(breaks = seq(0, 100, 10)) +
    scale_color_manual(values = c(
      "Death registration (DHA only)" = "firebrick",
      "Birth registration (DHA + hospital)" = "steelblue"
    )) +
    labs(
      title = "Cumulative population coverage: deaths vs births",
      x     = "Distance to nearest registration node (km)",
      y     = "Cumulative % of population",
      color = NULL
    ) +
    theme_minimal()

  # Interactive plotly map: standalone vs hospital DHA satellites
  birth_nodes_tbl <- bind_rows(
    dha_df %>%
      filter(!is.na(office_longitude) & !is.na(office_latitude)) %>%
      transmute(
        name      = as.character(off_new),
        lon       = office_longitude,
        lat       = office_latitude,
        node_type = "Standalone DHA office",
        detail    = paste0("Province: ", province_2016)
      ),
    dha_mfl_matched %>%
      filter(!is.na(latitude), !is.na(longitude)) %>%
      transmute(
        name      = facility_name,
        lon       = longitude,
        lat       = latitude,
        node_type = "Hospital DHA satellite",
        detail    = paste0("Match: ", match_quality, " | Province: ", province_code)
      )
  )

  birth_nodes_plotly <- plotly::plot_ly(
    data       = birth_nodes_tbl,
    type       = "scattermapbox",
    mode       = "markers",
    lat        = ~lat,
    lon        = ~lon,
    color      = ~node_type,
    colors     = c("Standalone DHA office" = "#e41a1c",
                   "Hospital DHA satellite" = "#377eb8"),
    text       = ~paste0("<b>", name, "</b><br>", node_type, "<br>", detail),
    hoverinfo  = "text",
    marker     = list(size = 8, opacity = 0.8)
  ) %>%
    plotly::layout(
      mapbox = list(
        style  = "open-street-map",
        center = list(lon = 25, lat = -29),
        zoom   = 4.5
      ),
      legend = list(orientation = "h", y = -0.05),
      margin = list(l = 0, r = 0, t = 0, b = 0)
    )

  n_hospital_dha <- nrow(hospital_dha_sf)
  n_standalone_dha <- nrow(standalone_dha_sf)
} else {
  message("dha_mfl_matched.csv not found — run DHA_access/link_DHA_to_MFL.r first.")
  birth_access_ft <- birth_vs_death_ft <- birth_distance_map <- birth_vs_death_curve_plot <- birth_nodes_plotly <- NULL
  birth_pct_5km <- birth_pct_10km <- birth_pct_20km <- NA
  n_hospital_dha <- n_standalone_dha <- NA
}

# =============================================================================
# 6. Save ALL outputs
# =============================================================================

save(
  # Shared / overview
  SA_pop, SA_N_off, p_with_buffers, modelled_population_map,
  # Method 1
  m1_national, m1_table,
  # Method 2
  dha_access_summary_table, m2_pct_5km, m2_pct_10km, m2_pct_20km,
  # Method 3
  ward_pop_sf, ward_access_summary, ward_access_ft,
  ward_distance_map, coverage_curve, coverage_curve_plot,
  m3_pct_5km, m3_pct_10km, m3_pct_20km, m3_dist_summary, missing_pop,
  # Comparison
  comparison_ft, comparison_results_ft,
  # Sensitivity (500 m density)
  node_density_sens, node_density_summary, top_density_wards,
  ward_density_quantiles,
  sens_node_summary_ft, sens_top_nodes_ft, sens_top_wards_ft,
  sens_density_compare_plot,
  sens_median_node_density, sens_p95_node_density,
  sens_median_ward_density, sens_p95_ward_density,
  sens_pct_nodes_above_p90,
  # Birth registration (DHA + hospital satellites)
  birth_access_summary, birth_access_ft,
  birth_vs_death_ft, birth_distance_map, birth_vs_death_curve_plot,
  birth_nodes_plotly,
  birth_pct_5km, birth_pct_10km, birth_pct_20km,
  n_hospital_dha, n_standalone_dha,
  file = "outputs/ward_level_dha_access.RData"
)

readr::write_csv(
  ward_pop_sf %>% st_drop_geometry(),
  "outputs/ward_level_dha_access_indicators.csv"
)

cat("\n=== Done ===\n")
cat("All outputs saved to outputs/ward_level_dha_access.RData\n")
