# ──────────────────────────────────────────────────────────────────────────────
# Link DHA hospital list (from Nadine) to MFL_Updated.csv coordinates
# Two-pass fuzzy matching + auditable outputs
# ──────────────────────────────────────────────────────────────────────────────

pacman::p_load(
  readr, readxl, dplyr, stringr, janitor,
  fuzzyjoin, sf, leaflet, ggplot2
)


# ── 1. Load & clean MFL ───────────────────────────────────────────────────────

mfl_raw <- read_csv("./DHA_access/MFL_Updated.csv") |> clean_names()

mfl_raw <- mfl_raw |>
  mutate(
    latitude      = as.numeric(gsub(",", ".", latitude)),
    longitude     = as.numeric(gsub(",", ".", longitude)),
    # province 2-letter code lives at start of other_name (e.g. "mp Boekenhouthoek")
    province_code = toupper(trimws(substr(other_name, 1, 2)))
  )

# One row per facility; prefer the most specific type where a facility has many
type_priority <- c(
  "District Hospital", "Regional Hospital", "Tertiary Hospital",
  "Central Hospital", "Specialised TB Hospital",
  "Specialised Psychiatric Hospital", "Community Health Centre",
  "Clinic", "Gateway Clinic"
)

mfl_unique <- mfl_raw |>
  filter(
    !grepl(
      "vaccine|mobile|temporary|distribution|outreach|field hospital|school|pharmacy|satelliet|stock|covid",
      types, ignore.case = TRUE
    ),
    !is.na(latitude), !is.na(longitude)
  ) |>
  mutate(type_rank = match(types, type_priority, nomatch = 99L)) |>
  group_by(id) |>
  slice_min(type_rank, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(id, ndd_code, primary_name, province_code, types, latitude, longitude)


# ── 2. Load DHA hospital list ─────────────────────────────────────────────────

dha_raw <- read_excel("./DHA_access/hospitals_with_dha_offices_from_Nadine.xlsx") |>
  clean_names() |>
  rename(dha_code = hospital_code, facility_name = hospital)


# ── 3. Standardise names for fuzzy matching ───────────────────────────────────

# Words that carry no discriminating information between the two datasets.
# Removing them increases JW similarity between pairs that are otherwise identical.
noise_words <- paste(c(
  # Facility type words
  "community health centre", "community health center",
  "hospital", "clinic", "chc",
  # Qualifiers often present in one list but absent in the other
  "provincial", "memorial", "academic", "partnership",
  "specialised", "specialized", "psychiatric", "tuberculosis",
  "\\bhealth\\b", "\\bcentre\\b", "\\bcenter\\b",
  "\\bwar\\b", "\\bmou\\b",
  # Type modifiers
  "district", "regional", "tertiary", "central",
  # Articles and prepositions
  "\\bthe\\b", "\\band\\b", "\\bof\\b", "\\bat\\b", "\\bfor\\b",
  # Abbreviations stripped inconsistently
  "\\bdr\\b", "\\bst\\b", "\\bno\\b", "\\bnr\\b",
  # Other noise
  "samhs", "private"
), collapse = "|")

clean_name <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("\\s*\\([^)]*\\)", "") |>  # strip parenthetical place names e.g. "(Port St Johns)"
    str_replace_all("[[:punct:]]", " ") |>       # remove all punctuation; turns "/" into " "
    str_replace_all(noise_words, " ") |>
    str_replace_all("\\b[0-9]+\\b", " ") |>    # remove standalone numbers (e.g. "Unit 9")
    str_replace_all("\\b[a-z]\\b", " ") |>     # remove single-letter tokens
    str_squish()
}

# Private facility chains are unlikely to appear in the MFL (public health system)
private_pattern <- paste(c(
  "medi.?clinic", "mediclinic", "netcare", "life clinic", "clinix",
  "lenmed", "linmed", "medforum", "femina", "mulbarton", "durdoc",
  "rosepark", "sunninghill", "sunward", "wilgers", "mercantile",
  "unitas", "park lane", "robinson", "greenacres", "cuyler", "clinton",
  "busamed", "medicross", "garden city", "casternhof",
  "constantiaberg", "vereeniging medi", "limpopo medi", "gatesville medi",
  "east rand", "cape gate medi", "sandton.*medi",
  "\\bprivate\\b"   # catch any name that explicitly says "private" e.g. "BISHOP (PRIVATE)"
), collapse = "|")

# DHA: create primary key (text before slash) and alt key (text after slash).
# Many DHA names use "/" for bilingual/alternate names e.g. DIAMOND/DIAMANT HOSPITAL
dha <- dha_raw |>
  mutate(
    name_primary = trimws(str_replace(facility_name, "\\s*/.*$", "")),
    name_alt     = if_else(
      str_detect(facility_name, "/"),
      trimws(str_extract(facility_name, "(?<=/).*")),
      NA_character_
    ),
    key_primary  = clean_name(name_primary),
    key_alt      = if_else(!is.na(name_alt), clean_name(name_alt), NA_character_),
    is_private   = str_detect(str_to_lower(facility_name), private_pattern)
  )

mfl_unique <- mfl_unique |> mutate(key = clean_name(primary_name))


# ── 4. Two-pass fuzzy matching ────────────────────────────────────────────────
#
# Pass 1 — primary key (full name before any "/")
# Pass 2 — for slash-named facilities still unmatched, try the alt key
#           (text after "/") to catch bilingual names like "DIAMOND/DIAMANT"

# --- Pass 1 ---
p1_join <- stringdist_left_join(
  dha |> mutate(key = key_primary),
  mfl_unique,
  by           = "key",
  method       = "jw",   # Jaro-Winkler: handles transpositions and prefix variation
  max_dist     = 0.15,
  distance_col = "jw_dist"
) |>
  rename(key_used = key.x, key_mfl = key.y) |>
  group_by(dha_code, facility_name) |>
  slice_min(jw_dist, n = 1, with_ties = FALSE) |>
  ungroup()

p1_matched   <- p1_join |> filter(!is.na(primary_name)) |> mutate(match_pass = "primary")
p1_unmatched <- p1_join |> filter( is.na(primary_name))

# --- Pass 2: alt/slash key for unmatched facilities ---
p2_dha <- dha |>
  filter(dha_code %in% p1_unmatched$dha_code, !is.na(key_alt)) |>
  mutate(key = key_alt)

if (nrow(p2_dha) > 0) {
  p2_join <- stringdist_left_join(
    p2_dha,
    mfl_unique,
    by           = "key",
    method       = "jw",
    max_dist     = 0.15,
    distance_col = "jw_dist"
  ) |>
    rename(key_used = key.x, key_mfl = key.y) |>
    group_by(dha_code, facility_name) |>
    slice_min(jw_dist, n = 1, with_ties = FALSE) |>
    ungroup()

  p2_matched <- p2_join |> filter(!is.na(primary_name)) |> mutate(match_pass = "alt_slash")
} else {
  p2_matched <- tibble()
}

all_matched <- bind_rows(p1_matched, p2_matched)


# ── 5. Final unmatched with reason ────────────────────────────────────────────

final_unmatched <- dha |>
  filter(!dha_code %in% all_matched$dha_code) |>
  select(dha_code, facility_name, key_primary, key_alt, is_private) |>
  mutate(
    likely_reason = case_when(
      is_private                                   ~ "Private facility chain — not expected in MFL",
      !is.na(key_alt)                              ~ "Slash name: neither side matched — manual lookup needed",
      is.na(key_primary) | nchar(key_primary) < 3  ~ "Name too short after cleaning — manual lookup needed",
      TRUE                                         ~ "No close match in MFL — possible renamed or absent facility"
    )
  )

cat(sprintf(
  "\nDHA total:       %d\n  Pass 1 matched : %d\n  Pass 2 matched : %d\n  Total matched  : %d (%.0f%%)\n  Unmatched      : %d\n\n",
  nrow(dha), nrow(p1_matched), nrow(p2_matched),
  nrow(all_matched), 100 * nrow(all_matched) / nrow(dha),
  nrow(final_unmatched)
))


# ── 6. Build auditable final dataset ─────────────────────────────────────────
#
# Columns included for auditability:
#   dha_code, facility_name    — original from Nadine's list
#   mfl_id, mfl_name           — matched MFL record
#   province_code, types       — from MFL
#   latitude, longitude        — from MFL (for plotting)
#   jw_dist                    — 0 = identical keys; higher = more different
#   match_pass                 — "primary" (full name) or "alt_slash" (after "/" separator)
#   match_quality              — Exact / Strong / Good / Weak-verify
#   review_flag                — TRUE if human check recommended
#   key_used                   — cleaned DHA key that produced the match
#   key_mfl                    — cleaned MFL key it was matched against
#   is_private                 — TRUE if name contains a known private chain keyword

final_matched <- all_matched |>
  mutate(
    match_quality = case_when(
      jw_dist == 0    ~ "Exact",
      jw_dist <= 0.05 ~ "Strong",
      jw_dist <= 0.10 ~ "Good",
      TRUE            ~ "Weak — verify"
    ),
    review_flag = match_quality == "Weak — verify" | is_private
  ) |>
  select(
    dha_code, facility_name, is_private,
    mfl_id     = id,
    mfl_name   = primary_name,
    province_code, types,
    latitude, longitude,
    jw_dist, match_pass, match_quality, review_flag,
    key_used, key_mfl
  ) |>
  arrange(desc(jw_dist))


# ── 7. Audit / review tables ─────────────────────────────────────────────────

cat("══ UNMATCHED DHA facilities ══════════════════════════════════════════════\n")
final_unmatched |>
  arrange(is_private, facility_name) |>
  print(n = Inf)

cat("\n══ Weak matches (jw_dist > 0.10) — manually verify ══════════════════════\n")
final_matched |>
  filter(match_quality == "Weak — verify") |>
  select(dha_code, facility_name, mfl_name, province_code, types, jw_dist, key_used, key_mfl) |>
  arrange(desc(jw_dist)) |>
  print(n = Inf)

cat("\n══ Pass 2 (alt/slash key) matches — verify these are correct ═════════════\n")
final_matched |>
  filter(match_pass == "alt_slash") |>
  select(dha_code, facility_name, mfl_name, province_code, jw_dist, key_used, key_mfl) |>
  print(n = Inf)

cat("\n══ Match summary ═════════════════════════════════════════════════════════\n")
final_matched |>
  count(match_quality, match_pass, name = "n") |>
  arrange(match_pass, match_quality) |>
  print()


# ── 8. Export CSVs for manual curation ───────────────────────────────────────
# Edit dha_mfl_unmatched.csv externally to add manual matches, then reload and
# bind to final_matched before plotting.

write_csv(final_matched,   "./DHA_access/dha_mfl_matched.csv")
write_csv(final_unmatched, "./DHA_access/dha_mfl_unmatched.csv")
cat("\nExported:\n  DHA_access/dha_mfl_matched.csv\n  DHA_access/dha_mfl_unmatched.csv\n\n")


# ── 9. Convert to sf ──────────────────────────────────────────────────────────

matched_sf <- st_as_sf(
  final_matched,
  coords = c("longitude", "latitude"),
  crs    = 4326
)


# ── 10. Leaflet interactive map ───────────────────────────────────────────────

quality_colours <- c(
  "Exact"          = "#27ae60",
  "Strong"         = "#2980b9",
  "Good"           = "#f39c12",
  "Weak — verify"  = "#e74c3c"
)

pal_quality <- colorFactor(
  palette = unname(quality_colours),
  levels  = names(quality_colours)
)

leaflet(matched_sf) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addCircleMarkers(
    radius      = 7,
    color       = ~pal_quality(match_quality),
    fillOpacity = 0.85,
    stroke      = FALSE,
    popup = ~paste0(
      "<b>", facility_name, "</b><br>",
      "<i>MFL: ", mfl_name, "</i><br>",
      "Province: ", province_code, " | Type: ", types, "<br>",
      "Match: <b>", match_quality, "</b>",
      " (dist=", round(jw_dist, 3), ", pass=", match_pass, ")",
      if_else(review_flag, "<br><span style='color:red'>\u26a0 review flag</span>", "")
    ),
    label = ~facility_name,
    group = "DHA-linked facilities"
  ) |>
  addLegend(
    "bottomright",
    pal    = pal_quality,
    values = ~match_quality,
    title  = "Match quality"
  ) |>
  addLayersControl(
    overlayGroups = "DHA-linked facilities",
    options       = layersControlOptions(collapsed = FALSE)
  )


# ── 11. Static ggplot (SA overview) ───────────────────────────────────────────

ggplot(final_matched, aes(x = longitude, y = latitude, colour = types)) +
  geom_point(alpha = 0.7, size = 2) +
  coord_fixed(ratio = 1.2) +
  scale_colour_brewer(palette = "Set1", name = "Facility type") +
  labs(
    title    = "DHA-linked facilities mapped via MFL coordinates",
    subtitle = sprintf(
      "%d of %d DHA hospitals matched (%d unmatched, %d flagged for review)",
      nrow(final_matched), nrow(dha), nrow(final_unmatched),
      sum(final_matched$review_flag)
    ),
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal()
