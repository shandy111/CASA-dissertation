# =============================================================
# 08_spatial_indicators.R
# =============================================================
# Additional spatial site-context indicators for 59 corpus:
# - Stage 31: IMD 2025 spatial join (via LSOA 2021)
# - Stage 32: Distance to nearest tube/DLR station
#
# Input:
#   output/corpus_59_with_ptal.gpkg      (from 07)
#   data/lsoa_boundaries/Tower Hamlets.shp  (LSOA 2021)
#   data/imd_2025/File_7_IoD2025_...csv
#   data/stations/London stations.csv
#
# Output:
#   output/corpus_59_with_imd.csv
#   output/corpus_59_full_spatial.csv    (final master with all spatial)
# =============================================================

source(here::here("scripts", "00_setup.R"))
library(sf)


# =============================================================
# STAGE 31 — IMD SPATIAL JOIN
# =============================================================

# ---- Load LSOA 2021 boundaries (Tower Hamlets) ----

th_lsoa <- st_read(
  here("data", "lsoa_boundaries", "Tower Hamlets.shp"),
  quiet = TRUE
)

cat("LSOA polygons:", nrow(th_lsoa), "\n")
cat("CRS:", st_crs(th_lsoa)$input, "\n\n")


# ---- Load IMD 2025 ----

imd_file <- list.files(here("data", "imd_2025"), 
                       pattern = "\\.csv$", full.names = TRUE)[1]

imd_raw <- read_csv(imd_file, show_col_types = FALSE)

# 简化 column names
imd_clean <- imd_raw %>%
  select(
    lsoa21cd = `LSOA code (2021)`,
    lsoa_name = `LSOA name (2021)`,
    imd_score = `Index of Multiple Deprivation (IMD) Score`,
    imd_rank = `Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`,
    imd_decile = `Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)`,
    income_score = `Income Score (rate)`,
    income_decile = `Income Decile (where 1 is most deprived 10% of LSOAs)`,
    employment_score = `Employment Score (rate)`,
    barriers_housing_score = `Barriers to Housing and Services Score`,
    barriers_housing_decile = `Barriers to Housing and Services Decile (where 1 is most deprived 10% of LSOAs)`,
    living_env_score = `Living Environment Score`
  )

cat("IMD 2025 clean:", nrow(imd_clean), "rows\n\n")


# ---- Join IMD to LSOA polygons (filter to Tower Hamlets) ----

th_lsoa_imd <- th_lsoa %>%
  left_join(imd_clean, by = "lsoa21cd")

cat("=== LSOA with IMD merge check ===\n")
cat("With IMD score:", sum(!is.na(th_lsoa_imd$imd_score)), "/", 
    nrow(th_lsoa_imd), "\n\n")

# Quick summary of TH IMD distribution
cat("=== Tower Hamlets IMD 2025 summary ===\n")
th_lsoa_imd %>%
  st_drop_geometry() %>%
  summarise(
    median_imd = median(imd_score, na.rm = TRUE),
    median_decile = median(imd_decile, na.rm = TRUE),
    min_imd = min(imd_score, na.rm = TRUE),
    max_imd = max(imd_score, na.rm = TRUE),
    pct_top_20 = round(mean(imd_decile <= 2, na.rm = TRUE) * 100, 1)
  ) %>%
  print()


# ---- Load corpus_59 spatial + spatial join with LSOA/IMD ----

corpus_sf <- st_read(here(output_dir, "corpus_59_with_ptal.gpkg"), 
                     quiet = TRUE)

cat("\nCorpus points:", nrow(corpus_sf), "\n")
cat("Corpus CRS:", st_crs(corpus_sf)$input, "\n\n")

# Ensure same CRS
if (st_crs(corpus_sf) != st_crs(th_lsoa_imd)) {
  corpus_sf <- st_transform(corpus_sf, st_crs(th_lsoa_imd))
}

# Point-in-polygon: find which LSOA each project falls in
corpus_with_imd <- corpus_sf %>%
  st_join(th_lsoa_imd, join = st_within)

cat("=== Corpus IMD join check ===\n")
cat("With IMD score:", sum(!is.na(corpus_with_imd$imd_score)), "/", 
    nrow(corpus_with_imd), "\n\n")

# 看有没有 NA (project 落在 LSOA 外部)
missing_imd <- corpus_with_imd %>% 
  st_drop_geometry() %>%
  filter(is.na(imd_score))

if (nrow(missing_imd) > 0) {
  cat("Missing IMD projects:\n")
  missing_imd %>% 
    select(lpa_number, site_name, precision) %>%
    print()
}


# ---- Corpus IMD distribution ----

cat("\n=== Corpus 59 IMD 2025 distribution ===\n")
corpus_with_imd %>%
  st_drop_geometry() %>%
  count(imd_decile) %>%
  arrange(imd_decile) %>%
  print()

cat("\n=== Corpus IMD summary ===\n")
corpus_with_imd %>%
  st_drop_geometry() %>%
  summarise(
    n = n(),
    median_imd_score = median(imd_score, na.rm = TRUE),
    median_decile = median(imd_decile, na.rm = TRUE),
    pct_top_20_deprived = round(mean(imd_decile <= 2, na.rm = TRUE) * 100, 1),
    pct_bottom_20_deprived = round(mean(imd_decile >= 9, na.rm = TRUE) * 100, 1)
  ) %>%
  print()


# ---- Save ----

corpus_with_imd %>%
  st_drop_geometry() %>%
  write_csv(here(output_dir, "corpus_59_with_imd.csv"))

cat("\nSaved: output/corpus_59_with_imd.csv\n")
# =============================================================
# STAGE 32 — DISTANCE TO NEAREST STATION
# =============================================================

# ---- Load stations ----

stations_raw <- read_csv(here("data", "stations", "London stations.csv"),
                         show_col_types = FALSE)

cat("Stations rows:", nrow(stations_raw), "\n")
cat("Columns:", paste(names(stations_raw), collapse = ", "), "\n\n")
print(head(stations_raw))

# ---- Distance summary ----

cat("=== Distance to nearest station ===\n")
corpus_stations %>%
  summarise(
    n = n(),
    median_m = round(median(dist_to_station_m), 0),
    min_m = round(min(dist_to_station_m), 0),
    max_m = round(max(dist_to_station_m), 0)
  ) %>%
  print()

# ---- IMD data ----
imd_data <- read_csv(here(output_dir, "corpus_59_with_imd.csv"),
                     show_col_types = FALSE) %>%
  select(lpa_number, imd_score, imd_decile,
         income_score, income_decile,
         barriers_housing_score, living_env_score,
         lsoa21cd, lsoa_name)

# ---- Discourse master ----
master_corpus <- read_csv(here(output_dir, "master_pairing_data_corpus.csv"),
                          show_col_types = FALSE)

# ---- Integrate ----
full_master <- master_corpus %>%
  left_join(imd_data, by = "lpa_number") %>%
  left_join(corpus_stations %>% 
              select(lpa_number, nearest_station, dist_to_station_m),
            by = "lpa_number")

cat("\nFull master:", nrow(full_master), "rows,", ncol(full_master), "cols\n")

write_csv(full_master, here(output_dir, "corpus_59_full_spatial.csv"))
cat("Saved: output/corpus_59_full_spatial.csv\n")


# ---- IMD × narrative (substantive analysis) ----

cat("\n=== IMD score × Narrative frequency ===\n")
cat("(negative = deprived areas use more of this narrative)\n\n")

narrative_cols <- c("regeneration", "redevelopment", "community",
                    "sustainability", "connectivity", "public_realm",
                    "affordable", "high_density", "placemaking",
                    "heritage", "displacement")

for (n in narrative_cols) {
  r <- suppressWarnings(cor(full_master[[n]], full_master$imd_score, 
                            use = "pairwise.complete.obs", method = "spearman"))
  cat(sprintf("%-15s  ρ = %+.3f\n", n, r))
}
# =============================================================
# STAGE 33 — GREEN SPACE (OS Open Greenspace, 500m buffer)
# =============================================================

# ---- Load greenspace polygons ----

greenspace <- st_read(
  here("data", "open_space", "TQ_GreenspaceSite.shp"),
  quiet = TRUE
) %>%
  st_transform(27700)

cat("Greenspace polygons:", nrow(greenspace), "\n")
cat("CRS:", st_crs(greenspace)$input, "\n")
cat("Bbox:\n")
print(st_bbox(greenspace))

# 类型分布
cat("\n=== Greenspace types (function) ===\n")
greenspace %>%
  st_drop_geometry() %>%
  count(function.) %>%
  arrange(desc(n)) %>%
  print(n = Inf)

cat("Column names:\n")
print(names(greenspace))

cat("\n=== Function distribution ===\n")
greenspace %>%
  st_drop_geometry() %>%
  as_tibble() %>%
  count(across(any_of("function"))) %>%
  arrange(desc(n)) %>%
  print(n = Inf)

cat("=== Function distribution ===\n")
table(greenspace$function.) %>%
  sort(decreasing = TRUE) %>%
  print()
# ---- Filter to public accessible open space ----

public_types <- c(
  "Play Space",
  "Public Park Or Garden",
  "Playing Field",
  "Allotments Or Community Growing Spaces"
)

greenspace_public <- greenspace %>%
  filter(function. %in% public_types)

cat("Public greenspace polygons:", nrow(greenspace_public), "\n\n")


# ---- Buffer 500m + intersect + area ----

corpus_sf <- st_read(here("output", "corpus_59_with_ptal.gpkg"), quiet = TRUE)
corpus_sf <- st_transform(corpus_sf, 27700)
corpus_buffer <- st_buffer(corpus_sf, dist = 500)

cat("Computing greenspace area in 500m buffer...\n")

# Intersection
gs_areas <- corpus_buffer %>%
  st_intersection(greenspace_public) %>%
  mutate(intersection_area = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>%
  group_by(lpa_number) %>%
  summarise(
    open_space_area_500m_sqm = sum(intersection_area, na.rm = TRUE),
    n_open_spaces_500m = n(),
    .groups = "drop"
  )

corpus_with_os <- corpus_sf %>%
  st_drop_geometry() %>%
  select(lpa_number) %>%
  left_join(gs_areas, by = "lpa_number") %>%
  mutate(
    open_space_area_500m_sqm = coalesce(open_space_area_500m_sqm, 0),
    n_open_spaces_500m = coalesce(n_open_spaces_500m, 0L),
    # buffer area = pi * 500^2 = 785,398 sqm
    open_space_pct_500m = round(open_space_area_500m_sqm / 785398 * 100, 1)
  )

cat("\n=== Open space in 500m buffer ===\n")
corpus_with_os %>%
  summarise(
    median_area_sqm = round(median(open_space_area_500m_sqm), 0),
    median_pct = round(median(open_space_pct_500m), 1),
    max_pct = round(max(open_space_pct_500m), 1),
    projects_with_0_os = sum(n_open_spaces_500m == 0)
  ) %>%
  print()
# =============================================================
# STAGE 34 — TREE CANOPY (LSOA-level join)
# =============================================================

# ---- Load tree canopy ----

canopy <- read_csv(here("data", "tree_canopy", "gla-canopy-lsoas.csv"),
                   show_col_types = FALSE)

cat("\nTree canopy LSOA rows:", nrow(canopy), "\n")
cat("Columns:", paste(names(canopy), collapse = ", "), "\n\n")
print(head(canopy, 3))

# Check CRS 是什么
cat("Open space CRS:\n")
print(st_crs(open_space))

# 直接 transform 到 BNG(如果已经是就不变)
open_space <- st_transform(open_space, 27700)

cat("\nAfter transform CRS:", st_crs(open_space)$input, "\n")
# ---- Tree canopy: join to corpus via LSOA ----

# 从 IMD merged file 拿 corpus LSOA
corpus_imd <- read_csv(here(output_dir, "corpus_59_with_imd.csv"),
                       show_col_types = FALSE)

# LSOA 2021 code
head(corpus_imd$lsoa21cd)

# Direct join (试 LSOA 2021 = LSOA 2011)
corpus_canopy <- corpus_imd %>%
  select(lpa_number, lsoa21cd) %>%
  left_join(canopy %>% 
              select(lsoa_code = lsoa_cd, 
                     canopy_pct = canopy_per,
                     canopy_area_kmsq = canopy_kmsq),
            by = c("lsoa21cd" = "lsoa_code"))

cat("=== Tree canopy join check ===\n")
cat("With canopy %:", sum(!is.na(corpus_canopy$canopy_pct)), "/", 
    nrow(corpus_canopy), "\n\n")

cat("=== Corpus tree canopy distribution ===\n")
summary(corpus_canopy$canopy_pct)

cat("=== Tree canopy join check ===\n")
cat("With canopy %:", sum(!is.na(corpus_canopy$canopy_pct)), "/", 
    nrow(corpus_canopy), "\n\n")

# Distribution
cat("=== Corpus tree canopy distribution ===\n")
summary(corpus_canopy$canopy_pct)
# ---- Load lookup ----

lookup <- read_csv(
  here("data", "lsoa_lookup",
       "LSOA_(2011)_to_LSOA_(2021)_to_Local_Authority_District_(2022)_Exact_Fit_Lookup_for_EW_(V3).csv"),
  show_col_types = FALSE
) %>%
  select(LSOA11CD, LSOA21CD, CHGIND)


# ---- Reload canopy (in case not in memory) ----

canopy <- read_csv(here("data", "tree_canopy", "gla-canopy-lsoas.csv"),
                   show_col_types = FALSE)


# ---- Join canopy to lookup (2011 → 2021 mapping) ----

canopy_v2 <- canopy %>%
  select(LSOA11CD = lsoa_cd, lsoa_kmsq, canopy_kmsq, canopy_per) %>%
  left_join(lookup, by = "LSOA11CD")

cat("Canopy after lookup join:", nrow(canopy_v2), "\n")
cat("With LSOA21CD:", sum(!is.na(canopy_v2$LSOA21CD)), "\n\n")

# CHGIND 分布
cat("=== Change indicator distribution ===\n")
canopy_v2 %>% count(CHGIND) %>% print()


# ---- Aggregate to 2021 LSOA level ----
# 
# CHGIND = "U" (unchanged): 1:1 mapping, 直接用
# CHGIND = "S" (split): 1 x 2011 → many x 2021, canopy % 复制到每个 2021
# CHGIND = "M" (merged): many x 2011 → 1 x 2021, 需要 aggregate canopy area
# CHGIND = "X" (irregular): 复杂,用 sum/mean 处理

canopy_2021 <- canopy_v2 %>%
  filter(!is.na(LSOA21CD)) %>%
  group_by(LSOA21CD) %>%
  summarise(
    canopy_area_kmsq = sum(canopy_kmsq, na.rm = TRUE),
    total_area_kmsq = sum(lsoa_kmsq, na.rm = TRUE),
    canopy_pct = round(canopy_area_kmsq / total_area_kmsq * 100, 2),
    .groups = "drop"
  )

cat("\nCanopy at 2021 level:", nrow(canopy_2021), "rows\n\n")


# ---- Join to corpus ----

corpus_canopy <- corpus_imd %>%
  select(lpa_number, lsoa21cd) %>%
  left_join(canopy_2021, by = c("lsoa21cd" = "LSOA21CD"))

cat("=== Corpus canopy join (via lookup) ===\n")
cat("With canopy %:", sum(!is.na(corpus_canopy$canopy_pct)), "/", 
    nrow(corpus_canopy), "\n\n")

# 仍 missing 的 project
missing_after_lookup <- corpus_canopy %>%
  filter(is.na(canopy_pct)) %>%
  pull(lsoa21cd)

if (length(missing_after_lookup) > 0) {
  cat("Still missing LSOA:", length(missing_after_lookup), "\n")
  print(missing_after_lookup)
} else {
  cat("All corpus projects have canopy data.\n")
}

cat("\n=== Canopy % distribution ===\n")
summary(corpus_canopy$canopy_pct)
# =============================================================
# STAGE 35 — INTEGRATE ALL SPATIAL INDICATORS (FINAL)
# =============================================================

full_master <- read_csv(here("output", "corpus_59_full_spatial.csv"),
                        show_col_types = FALSE)

# Drop 之前的 open space columns (broken 那次的 0 values)
full_master <- full_master %>%
  select(-any_of(c("open_space_area_500m_sqm", "open_space_pct_500m",
                   "n_open_spaces_500m")))

# Merge new open space + canopy (canopy 应该已经在 full_master 里了)
full_master_final <- full_master %>%
  left_join(corpus_with_os %>% 
              select(lpa_number, open_space_area_500m_sqm, 
                     open_space_pct_500m, n_open_spaces_500m),
            by = "lpa_number")

cat("=== Final master ===\n")
cat("Rows:", nrow(full_master_final), "\n")
cat("Cols:", ncol(full_master_final), "\n\n")

write_csv(full_master_final, here("output", "corpus_59_full_spatial.csv"))
cat("Saved: output/corpus_59_full_spatial.csv\n\n")


# ---- Open space × narrative correlations ----

narrative_cols <- c("regeneration", "redevelopment", "community",
                    "sustainability", "connectivity", "public_realm",
                    "affordable", "high_density", "placemaking",
                    "heritage", "displacement")

cat("=== Open space (500m) × Narrative ===\n")
cat("(negative = less green area → more of this narrative)\n\n")

for (n in narrative_cols) {
  r <- suppressWarnings(cor(full_master_final[[n]], 
                            full_master_final$open_space_pct_500m,
                            use = "pairwise.complete.obs", method = "spearman"))
  cat(sprintf("%-15s  ρ = %+.3f\n", n, r))
}
# =============================================================
# STAGE 36 — DISTANCE TO NEAREST SCHOOL + SCHOOL COUNT IN 500m
# =============================================================

# ---- Filter to Tower Hamlets + London open schools ----

schools <- schools_raw %>%
  filter(`EstablishmentStatus (name)` == "Open") %>%
  # 只保留有坐标的
  filter(!is.na(Easting), !is.na(Northing)) %>%
  # Filter London (Tower Hamlets + surrounding boroughs 5-10km 内)
  filter(`LA (name)` %in% c(
    "Tower Hamlets", "City of London", "Hackney", "Newham", 
    "Southwark", "Islington", "Camden", "Westminster",
    "Lewisham", "Waltham Forest", "Greenwich"
  ))

cat("Schools (open, coords, London):", nrow(schools), "\n\n")

# 学校类型分布
cat("=== Phase distribution ===\n")
schools %>% count(`PhaseOfEducation (name)`) %>%
  arrange(desc(n)) %>% print()


# ---- Convert to sf ----

schools_sf <- schools %>%
  st_as_sf(coords = c("Easting", "Northing"), crs = 27700, remove = FALSE)


# ---- Distance to nearest school ----

# Corpus_sf 应该已经在环境里(BNG)
if (!exists("corpus_sf") || nrow(corpus_sf) != 59) {
  corpus_sf <- st_read(here("output", "corpus_59_with_ptal.gpkg"), quiet = TRUE)
  corpus_sf <- st_transform(corpus_sf, 27700)
}

dist_matrix_school <- st_distance(corpus_sf, schools_sf)
nearest_idx <- apply(dist_matrix_school, 1, which.min)
nearest_dist <- apply(dist_matrix_school, 1, min)

corpus_schools <- corpus_sf %>%
  st_drop_geometry() %>%
  select(lpa_number) %>%
  mutate(
    nearest_school = schools_sf$EstablishmentName[nearest_idx],
    nearest_school_phase = schools_sf$`PhaseOfEducation (name)`[nearest_idx],
    dist_to_school_m = as.numeric(nearest_dist)
  )


# ---- School count within 500m ----

corpus_buffer <- st_buffer(corpus_sf, dist = 500)
within_500m <- st_intersects(corpus_buffer, schools_sf, sparse = TRUE)
n_schools_500m <- lengths(within_500m)

corpus_schools <- corpus_schools %>%
  mutate(n_schools_500m = n_schools_500m)


# ---- Summary ----

cat("\n=== Distance to nearest school ===\n")
corpus_schools %>%
  summarise(
    median_m = round(median(dist_to_school_m), 0),
    min_m = round(min(dist_to_school_m), 0),
    max_m = round(max(dist_to_school_m), 0)
  ) %>%
  print()

cat("\n=== Schools within 500m buffer ===\n")
corpus_schools %>%
  summarise(
    median_n = median(n_schools_500m),
    max_n = max(n_schools_500m),
    projects_with_0 = sum(n_schools_500m == 0)
  ) %>%
  print()


# =============================================================
# STAGE 37 — INTEGRATE SCHOOLS INTO FULL MASTER + CORRELATIONS
# =============================================================

full_master <- read_csv(here("output", "corpus_59_full_spatial.csv"),
                        show_col_types = FALSE)

# 加 schools
full_master_v3 <- full_master %>%
  left_join(corpus_schools, by = "lpa_number")

cat("\n=== Full master v3 ===\n")
cat("Rows:", nrow(full_master_v3), "\n")
cat("Cols:", ncol(full_master_v3), "\n\n")

write_csv(full_master_v3, here("output", "corpus_59_full_spatial.csv"))
cat("Saved: output/corpus_59_full_spatial.csv (with schools)\n\n")


# ---- School × narrative correlations ----

narrative_cols <- c("regeneration", "redevelopment", "community",
                    "sustainability", "connectivity", "public_realm",
                    "affordable", "high_density", "placemaking",
                    "heritage", "displacement")

cat("=== School distance × narrative ===\n")
for (n in narrative_cols) {
  r <- suppressWarnings(cor(full_master_v3[[n]], 
                            full_master_v3$dist_to_school_m,
                            use = "pairwise.complete.obs", method = "spearman"))
  cat(sprintf("%-15s  ρ = %+.3f\n", n, r))
}

cat("\n=== N schools 500m × narrative ===\n")
for (n in narrative_cols) {
  r <- suppressWarnings(cor(full_master_v3[[n]], 
                            full_master_v3$n_schools_500m,
                            use = "pairwise.complete.obs", method = "spearman"))
  cat(sprintf("%-15s  ρ = %+.3f\n", n, r))
}
# =============================================================
# STAGE 38 — BUS STOPS: DISTANCE + COUNT IN 500m
# =============================================================
# Source: Naptan (DfT national dataset, filtered to London)
# =============================================================

# ---- Load bus stops ----

bus_stops_raw <- read_csv(here("data", "bus_stops", "Stops.csv"),
                          show_col_types = FALSE)

# Filter: active, bus stop type, London bbox
bus_stops <- bus_stops_raw %>%
  filter(Status == "active",
         StopType == "BCT",  # Bus/Coach Terminus
         !is.na(Easting), !is.na(Northing),
         Easting >= 500000, Easting <= 570000,
         Northing >= 155000, Northing <= 210000)

cat("London bus stops (active BCT):", nrow(bus_stops), "\n")

bus_sf <- bus_stops %>%
  st_as_sf(coords = c("Easting", "Northing"), crs = 27700, remove = FALSE)


# ---- Ensure corpus_sf in BNG ----

if (!exists("corpus_sf") || nrow(corpus_sf) != 59) {
  corpus_sf <- st_read(here("output", "corpus_59_with_ptal.gpkg"), quiet = TRUE)
  corpus_sf <- st_transform(corpus_sf, 27700)
}


# ---- Distance + count ----

dist_bus <- st_distance(corpus_sf, bus_sf)
nearest_bus_idx <- apply(dist_bus, 1, which.min)
nearest_bus_dist <- apply(dist_bus, 1, min)

corpus_bus <- corpus_sf %>%
  st_drop_geometry() %>%
  select(lpa_number) %>%
  mutate(
    nearest_bus_stop = bus_sf$CommonName[nearest_bus_idx],
    dist_to_bus_m = as.numeric(nearest_bus_dist)
  )

corpus_buffer <- st_buffer(corpus_sf, dist = 500)
corpus_bus$n_bus_stops_500m <- lengths(
  st_intersects(corpus_buffer, bus_sf, sparse = TRUE)
)


# ---- Summary ----

cat("\n=== Distance to nearest bus stop ===\n")
corpus_bus %>%
  summarise(median_m = round(median(dist_to_bus_m), 0),
            min_m = round(min(dist_to_bus_m), 0),
            max_m = round(max(dist_to_bus_m), 0)) %>%
  print()

cat("\n=== Bus stops in 500m ===\n")
corpus_bus %>%
  summarise(median_n = median(n_bus_stops_500m),
            max_n = max(n_bus_stops_500m),
            projects_with_0 = sum(n_bus_stops_500m == 0)) %>%
  print()


# =============================================================
# STAGE 39 — CYCLING INFRASTRUCTURE
# =============================================================
# Sources: TfL Cycling Infrastructure Database (CID) 2021
#   - cycle_lane_track.json (lines): dedicated cycle infrastructure
#   - cycle_parking.json (points): bike parking
# =============================================================

# ---- Load cycle lanes ----

cycle_lanes <- st_read(here("data", "cycling", "cycle_lane_track.json"),
                       quiet = TRUE)

cat("Cycle lane features:", nrow(cycle_lanes), "\n")
cat("CRS:", st_crs(cycle_lanes)$input, "\n\n")

cycle_lanes <- st_transform(cycle_lanes, 27700)


# ---- Load cycle parking ----

cycle_parking <- st_read(here("data", "cycling", "cycle_parking.json"),
                         quiet = TRUE)

cat("Cycle parking features:", nrow(cycle_parking), "\n")
cat("CRS:", st_crs(cycle_parking)$input, "\n\n")

cycle_parking <- st_transform(cycle_parking, 27700)


# ---- Distance to nearest cycle lane ----

dist_lane <- st_distance(corpus_sf, cycle_lanes)
nearest_lane_dist <- apply(dist_lane, 1, min)


# ---- Distance to nearest cycle parking + count in 500m ----

dist_parking <- st_distance(corpus_sf, cycle_parking)
nearest_parking_dist <- apply(dist_parking, 1, min)

n_parking_500m <- lengths(
  st_intersects(corpus_buffer, cycle_parking, sparse = TRUE)
)


# ---- Combine ----

corpus_cycling <- corpus_sf %>%
  st_drop_geometry() %>%
  select(lpa_number) %>%
  mutate(
    dist_to_cycle_lane_m = as.numeric(nearest_lane_dist),
    dist_to_cycle_parking_m = as.numeric(nearest_parking_dist),
    n_cycle_parking_500m = n_parking_500m
  )


# ---- Summary ----

cat("=== Distance to nearest cycle lane ===\n")
corpus_cycling %>%
  summarise(median_m = round(median(dist_to_cycle_lane_m), 0),
            min_m = round(min(dist_to_cycle_lane_m), 0),
            max_m = round(max(dist_to_cycle_lane_m), 0)) %>%
  print()

cat("\n=== Distance to nearest cycle parking ===\n")
corpus_cycling %>%
  summarise(median_m = round(median(dist_to_cycle_parking_m), 0),
            min_m = round(min(dist_to_cycle_parking_m), 0),
            max_m = round(max(dist_to_cycle_parking_m), 0)) %>%
  print()

cat("\n=== Cycle parking within 500m ===\n")
corpus_cycling %>%
  summarise(median_n = median(n_cycle_parking_500m),
            max_n = max(n_cycle_parking_500m),
            projects_with_0 = sum(n_cycle_parking_500m == 0)) %>%
  print()


# =============================================================
# STAGE 40 — INTEGRATE BUS + CYCLING INTO FULL MASTER
# =============================================================

full_master <- read_csv(here("output", "corpus_59_full_spatial.csv"),
                        show_col_types = FALSE)

# 加 bus + cycling
full_master_v4 <- full_master %>%
  left_join(corpus_bus, by = "lpa_number") %>%
  left_join(corpus_cycling, by = "lpa_number")

cat("\n=== Full master v4 ===\n")
cat("Rows:", nrow(full_master_v4), "\n")
cat("Cols:", ncol(full_master_v4), "\n\n")

write_csv(full_master_v4, here("output", "corpus_59_full_spatial.csv"))
cat("Saved: output/corpus_59_full_spatial.csv (final with bus + cycling)\n\n")


# ---- Correlations with narratives ----

narrative_cols <- c("regeneration", "redevelopment", "community",
                    "sustainability", "connectivity", "public_realm",
                    "affordable", "high_density", "placemaking",
                    "heritage", "displacement")

cat("=== Bus stops (500m) × Narrative ===\n")
for (n in narrative_cols) {
  r <- suppressWarnings(cor(full_master_v4[[n]], 
                            full_master_v4$n_bus_stops_500m,
                            use = "pairwise.complete.obs", method = "spearman"))
  cat(sprintf("%-15s  ρ = %+.3f\n", n, r))
}

cat("\n=== Distance to cycle lane × Narrative ===\n")
for (n in narrative_cols) {
  r <- suppressWarnings(cor(full_master_v4[[n]], 
                            full_master_v4$dist_to_cycle_lane_m,
                            use = "pairwise.complete.obs", method = "spearman"))
  cat(sprintf("%-15s  ρ = %+.3f\n", n, r))
}

cat("\n=== Cycle parking (500m) × Narrative ===\n")
for (n in narrative_cols) {
  r <- suppressWarnings(cor(full_master_v4[[n]], 
                            full_master_v4$n_cycle_parking_500m,
                            use = "pairwise.complete.obs", method = "spearman"))
  cat(sprintf("%-15s  ρ = %+.3f\n", n, r))
}