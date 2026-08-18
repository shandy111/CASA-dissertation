# =============================================================
# 07_corpus_geocoding.R
# =============================================================
# Geocode all 59 discourse-analyzable corpus projects, 
# integrate PTAL, prepare for spatial analysis (Moran's I / GWR).
#
# Input:  output/master_pairing_data_corpus.csv (has 59 lpa_number)
#         output/th_74_typed.csv (has postcode + site_name for 74)
#         output/sub_sample_with_ptal.csv (18 already geocoded)
# Output: output/corpus_59_geocoded.csv
#         output/corpus_59_with_ptal.csv
#         output/corpus_59_with_ptal.gpkg
# =============================================================

source(here::here("scripts", "00_setup.R"))


# =============================================================
# STAGE 28 — CORPUS PROJECT LIST + GEOCODING
# =============================================================

# ---- Load which projects need geocoding ----

master_corpus <- read_csv(here(output_dir, "master_pairing_data_corpus.csv"),
                          show_col_types = FALSE)

# 59 corpus lpa_numbers (Round 1 LLM 使用的)
corpus_59_lpas <- master_corpus %>%
  select(lpa_number) %>%
  distinct() %>%
  pull(lpa_number)

cat("Corpus size:", length(corpus_59_lpas), "\n\n")


# ---- Get postcode + site info from 74 metadata ----

th_74 <- read_csv(here(output_dir, "th_74_typed.csv"),
                  show_col_types = FALSE) %>%
  mutate(base_lpa = str_replace(lpa_number, "/[A-Z][A-Z0-9]*$", "")) %>%
  distinct(base_lpa, .keep_all = TRUE) %>%
  mutate(join_key = ifelse(str_detect(base_lpa, "^(PA|LL)/"),
                           base_lpa,
                           paste0("LL/", base_lpa)))

# 59 corpus 对应的 metadata
corpus_meta <- tibble(lpa_number = corpus_59_lpas) %>%
  left_join(th_74 %>% select(join_key, site_name, street_name, postcode,
                             units_proposed, units_lost, financial_year),
            by = c("lpa_number" = "join_key"))

cat("Metadata join check:\n")
cat("Successfully joined:", sum(!is.na(corpus_meta$postcode)), "/", 
    nrow(corpus_meta), "\n\n")


# ---- Prepare postcode field ----

corpus_pc <- corpus_meta %>%
  mutate(
    # 从 site_name 提 postcode(有些老 project postcode 字段空)
    postcode_from_site = str_extract(
      site_name,
      "[A-Z]{1,2}[0-9][A-Z0-9]?\\s?[0-9][A-Z]{2}"
    ),
    postcode_final = coalesce(
      str_trim(str_to_upper(postcode)),
      str_trim(str_to_upper(postcode_from_site))
    ),
    is_full_postcode = str_detect(
      postcode_final,
      "^[A-Z]{1,2}[0-9][A-Z0-9]?\\s?[0-9][A-Z]{2}$"
    )
  )

cat("Postcode availability:\n")
corpus_pc %>%
  mutate(has_pc = !is.na(postcode_final)) %>%
  count(has_pc, is_full_postcode) %>%
  print()


# ---- Load previously geocoded 18 from sub-sample ----

prev_geocoded <- read_csv(here(output_dir, "sub_sample_with_ptal.csv"),
                          show_col_types = FALSE) %>%
  select(lpa_number, postcode_final, precision,
         latitude = any_of(c("latitude", "lat")),
         longitude = any_of(c("longitude", "lng", "lon")),
         in_th_bbox, source, PTAL)

# 处理:sub-sample 的 lpa_number 是 base(strip suffix),corpus 也一样
prev_geocoded <- prev_geocoded %>%
  mutate(base_lpa = str_replace(lpa_number, "/[A-Z][A-Z0-9]*$", ""))

# 找出 corpus 里已 geocoded 的
already_geocoded <- corpus_pc %>%
  mutate(base_lpa_corpus = str_replace(lpa_number, "/[A-Z][A-Z0-9]*$", "")) %>%
  filter(base_lpa_corpus %in% prev_geocoded$base_lpa) %>%
  pull(lpa_number)

cat("\nAlready geocoded from sub-sample:", length(already_geocoded), "\n")
cat("Still to geocode:", length(corpus_59_lpas) - length(already_geocoded), "\n")


# ---- Preview only, no API calls yet ----

cat("\n=== Preview complete. Next: run geocoding ===\n")

# 直接读 raw,不 distinct
th_74_raw <- read_csv(here(output_dir, "th_74_typed.csv"),
                      show_col_types = FALSE) %>%
  mutate(base_lpa = str_replace(lpa_number, "/[A-Z][A-Z0-9]*$", ""))

# 看 PA/11/03670 所有行
th_74_raw %>%
  filter(base_lpa == "PA/11/03670") %>%
  select(lpa_number, site_name, postcode) %>%
  print()
# Fix: 同时处理 /suffix 和 直接尾字母
# 如: PA/22/00210/A1 → PA/22/00210
#     PA/11/03670A → PA/11/03670
strip_lpa <- function(x) {
  x %>%
    str_replace("/[A-Z][A-Z0-9]*$", "") %>%  # strip /A1 /NC /S 等
    str_replace("[A-Z]$", "")                 # strip 尾字母 A(无 /)
}
strip_lpa <- function(x) {
  x %>%
    str_replace("/[A-Z][A-Z0-9]*$", "") %>%
    str_replace("[A-Z]$", "")
}

# 重跑 join(用正确 strip)
th_74 <- read_csv(here(output_dir, "th_74_typed.csv"),
                  show_col_types = FALSE) %>%
  mutate(base_lpa = strip_lpa(lpa_number)) %>%
  arrange(base_lpa, is.na(postcode)) %>%
  distinct(base_lpa, .keep_all = TRUE) %>%
  mutate(join_key = ifelse(str_detect(base_lpa, "^(PA|LL)/"),
                           base_lpa,
                           paste0("LL/", base_lpa)))

corpus_meta <- tibble(lpa_number = corpus_59_lpas) %>%
  left_join(th_74 %>% select(join_key, site_name, street_name, postcode,
                             units_proposed, units_lost, financial_year),
            by = c("lpa_number" = "join_key"))

cat("=== After fix ===\n")
cat("Total:", nrow(corpus_meta), "\n")
cat("With site_name:", sum(!is.na(corpus_meta$site_name)), "\n")

# 现在 PA/11/03670 有信息吗?
corpus_meta %>%
  filter(lpa_number == "PA/11/03670") %>%
  select(lpa_number, site_name, postcode) %>%
  print()
corpus_meta <- tibble(lpa_number = corpus_59_lpas) %>%
  left_join(th_74 %>% select(join_key, site_name, street_name, postcode,
                             units_proposed, units_lost, financial_year),
            by = c("lpa_number" = "join_key"))

cat("=== corpus_meta rebuilt ===\n")
cat("With site_name:", sum(!is.na(corpus_meta$site_name)), "/", nrow(corpus_meta), "\n")
cat("With postcode:", sum(!is.na(corpus_meta$postcode)), "/", nrow(corpus_meta), "\n\n")

# 那 2 个现在有 postcode 吗
corpus_meta %>%
  filter(lpa_number %in% c("LL/18/00325", "PA/16/01958")) %>%
  select(lpa_number, site_name, postcode) %>%
  print()
corpus_pc <- corpus_meta %>%
  mutate(
    postcode_full = str_extract(
      site_name,
      "[A-Z]{1,2}[0-9][A-Z0-9]?\\s?[0-9][A-Z]{2}"
    ),
    postcode_outward = str_extract(
      site_name,
      "\\b[A-Z]{1,2}[0-9][A-Z0-9]?\\b(?!\\s?[0-9][A-Z])"
    ),
    postcode_final = coalesce(
      str_trim(str_to_upper(postcode)),
      str_trim(str_to_upper(postcode_full)),
      str_trim(str_to_upper(postcode_outward))
    ),
    is_full_postcode = str_detect(
      postcode_final,
      "^[A-Z]{1,2}[0-9][A-Z0-9]?\\s?[0-9][A-Z]{2}$"
    )
  )

cat("=== Final postcode coverage ===\n")
corpus_pc %>%
  mutate(has_pc = !is.na(postcode_final)) %>%
  count(has_pc, is_full_postcode) %>%
  print()

# 还没 postcode 的 project
cat("\n=== Still no postcode ===\n")
corpus_pc %>%
  filter(is.na(postcode_final)) %>%
  select(lpa_number, site_name) %>%
  print(n = Inf, width = Inf)
# 手动填 3 个 outward postcode
manual_pc <- tribble(
  ~lpa_number,     ~manual_postcode,
  "PA/20/00571",   "E1",
  "PA/24/00733",   "E14",
  "PA/24/00996",   "E3"
)

corpus_pc <- corpus_pc %>%
  left_join(manual_pc, by = "lpa_number") %>%
  mutate(
    postcode_final = coalesce(postcode_final, manual_postcode),
    is_full_postcode = str_detect(
      postcode_final,
      "^[A-Z]{1,2}[0-9][A-Z0-9]?\\s?[0-9][A-Z]{2}$"
    ),
    postcode_source = case_when(
      !is.na(manual_postcode) ~ "manual_from_site_description",
      is_full_postcode ~ "full_from_data",
      !is.na(postcode_final) ~ "outward_from_data",
      TRUE ~ "none"
    )
  ) %>%
  select(-manual_postcode)

cat("=== Final coverage after manual fill ===\n")
corpus_pc %>%
  count(postcode_source, is_full_postcode) %>%
  print()
library(sf)

prev_sf <- st_read(here(output_dir, "sub_sample_with_ptal.gpkg"), quiet = TRUE)

# 提取坐标
prev_coords <- prev_sf %>%
  mutate(
    coords = st_coordinates(st_transform(., 4326))
  )

prev_geocoded_full <- prev_sf %>%
  st_transform(4326) %>%
  mutate(
    latitude = st_coordinates(.)[, "Y"],
    longitude = st_coordinates(.)[, "X"]
  ) %>%
  st_drop_geometry() %>%
  mutate(base_lpa = str_replace(lpa_number, "/[A-Z][A-Z0-9]*$", ""),
         base_lpa = str_replace(base_lpa, "[A-Z]$", ""))

cat("Prev geocoded rows:", nrow(prev_geocoded_full), "\n")
cat("Columns:", paste(names(prev_geocoded_full), collapse = ", "), "\n\n")
# =============================================================
# STAGE 29 — GEOCODE 46 REMAINING PROJECTS
# =============================================================
# 13 sub-sample already geocoded → reuse from gpkg
# Wood Wharf补加 (was appended to CSV but not gpkg)
# 46 need fresh geocoding via postcodes.io (free API)
# =============================================================

library(sf)

# ---- Geocoding functions ----

geocode_full_postcode <- function(pc) {
  pc_clean <- str_replace_all(pc, " ", "")
  url <- paste0("https://api.postcodes.io/postcodes/", pc_clean)
  response <- try(GET(url), silent = TRUE)
  if (inherits(response, "try-error") || status_code(response) != 200) {
    # Try terminated endpoint
    url_term <- paste0("https://api.postcodes.io/terminated_postcodes/", pc_clean)
    response <- try(GET(url_term), silent = TRUE)
    if (inherits(response, "try-error") || status_code(response) != 200) {
      return(tibble(latitude = NA_real_, longitude = NA_real_,
                    precision = "failed", source = "postcodes.io_full"))
    }
    result <- content(response)$result
    return(tibble(latitude = result$latitude, longitude = result$longitude,
                  precision = "unit_postcode_terminated",
                  source = "postcodes.io_terminated"))
  }
  result <- content(response)$result
  tibble(latitude = result$latitude, longitude = result$longitude,
         precision = "unit_postcode", source = "postcodes.io_full")
}

geocode_outward <- function(outward) {
  url <- paste0("https://api.postcodes.io/outcodes/", outward)
  response <- try(GET(url), silent = TRUE)
  if (inherits(response, "try-error") || status_code(response) != 200) {
    return(tibble(latitude = NA_real_, longitude = NA_real_,
                  precision = "failed", source = "postcodes.io_outward"))
  }
  result <- content(response)$result
  tibble(latitude = result$latitude, longitude = result$longitude,
         precision = "outward_centroid", source = "postcodes.io_outward")
}


# ---- Load prev geocoded (from gpkg, WGS84) + append Wood Wharf ----

prev_sf <- st_read(here(output_dir, "sub_sample_with_ptal.gpkg"), quiet = TRUE)

prev_geocoded_full <- prev_sf %>%
  st_transform(4326) %>%
  mutate(
    latitude = st_coordinates(.)[, "Y"],
    longitude = st_coordinates(.)[, "X"]
  ) %>%
  st_drop_geometry() %>%
  mutate(base_lpa = str_replace(lpa_number, "/[A-Z][A-Z0-9]*$", ""),
         base_lpa = str_replace(base_lpa, "[A-Z]$", ""))

# Wood Wharf 补加(gpkg 里没有,但 CSV 里有)
wood_wharf_row <- tibble(
  lpa_number = "PA/13/02966",
  base_lpa = "PA/13/02966",
  latitude = 51.50168,
  longitude = -0.00977,
  precision = "unit_postcode_terminated",
  source = "postcodes.io_terminated",
  PTAL = "3"
)

prev <- bind_rows(prev_geocoded_full, wood_wharf_row)

cat("=== Prev geocoded ready ===\n")
cat("Rows:", nrow(prev), "\n")
cat("has latitude:", "latitude" %in% names(prev), "\n\n")


# ---- Split corpus into reused vs to-geocode ----

corpus_geocoded_reuse <- corpus_pc %>%
  mutate(base_lpa_corpus = str_replace(lpa_number, "/[A-Z][A-Z0-9]*$", ""),
         base_lpa_corpus = str_replace(base_lpa_corpus, "[A-Z]$", "")) %>%
  filter(base_lpa_corpus %in% prev$base_lpa) %>%
  left_join(prev %>% select(base_lpa, latitude, longitude,
                            prev_precision = precision,
                            prev_source = source, PTAL),
            by = c("base_lpa_corpus" = "base_lpa")) %>%
  mutate(precision = prev_precision,
         source = prev_source) %>%
  select(-base_lpa_corpus, -prev_precision, -prev_source)

to_geocode <- corpus_pc %>%
  mutate(base_lpa_corpus = str_replace(lpa_number, "/[A-Z][A-Z0-9]*$", ""),
         base_lpa_corpus = str_replace(base_lpa_corpus, "[A-Z]$", "")) %>%
  filter(!base_lpa_corpus %in% prev$base_lpa) %>%
  select(-base_lpa_corpus)

cat("Reused:", nrow(corpus_geocoded_reuse), "\n")
cat("To geocode:", nrow(to_geocode), "\n\n")


# ---- Geocode 46 remaining via API ----

cat("=== Geocoding via postcodes.io... ===\n")

new_geocoded <- to_geocode %>%
  rowwise() %>%
  mutate(
    outward = str_extract(postcode_final, "^[A-Z]{1,2}[0-9][A-Z0-9]?"),
    result = list(
      if (is_full_postcode) {
        geocode_full_postcode(postcode_final)
      } else if (!is.na(outward)) {
        geocode_outward(outward)
      } else {
        tibble(latitude = NA_real_, longitude = NA_real_,
               precision = "no_postcode", source = "none")
      }
    )
  ) %>%
  unnest(result) %>%
  ungroup() %>%
  select(-outward)

cat("\n=== New geocoded results ===\n")
new_geocoded %>% count(precision) %>% print()


# ---- Combine reused + new ----

corpus_geocoded_all <- bind_rows(
  corpus_geocoded_reuse,
  new_geocoded %>% mutate(PTAL = NA_character_)
) %>%
  mutate(
    in_th_bbox = latitude >= 51.49 & latitude <= 51.55 &
      longitude >= -0.08 & longitude <= 0.00
  )

cat("\n=== Final corpus geocoded ===\n")
cat("Total:", nrow(corpus_geocoded_all), "\n")
cat("With coordinates:", sum(!is.na(corpus_geocoded_all$latitude)), "\n\n")

corpus_geocoded_all %>% count(precision) %>% print()

cat("\n=== Bbox check ===\n")
corpus_geocoded_all %>% count(in_th_bbox) %>% print()


# ---- Save ----

corpus_geocoded_all %>%
  select(lpa_number, site_name, postcode_final, is_full_postcode,
         postcode_source, precision, latitude, longitude,
         in_th_bbox, source, financial_year, units_proposed, units_lost, PTAL) %>%
  write_csv(here(output_dir, "corpus_59_geocoded.csv"))

cat("\nSaved: output/corpus_59_geocoded.csv\n")
corpus_geocoded_all <- corpus_geocoded_all %>%
  mutate(
    in_th_bbox = latitude >= 51.49 & latitude <= 51.55 &
      longitude >= -0.08 & longitude <= 0.03
  )

cat("=== Updated bbox check ===\n")
corpus_geocoded_all %>% count(in_th_bbox) %>% print()

# Save 更新版
write_csv(corpus_geocoded_all %>%
            select(lpa_number, site_name, postcode_final, is_full_postcode,
                   postcode_source, precision, latitude, longitude,
                   in_th_bbox, source, financial_year, units_proposed, units_lost, PTAL),
          here(output_dir, "corpus_59_geocoded.csv"))
# =============================================================
# STAGE 30 — PTAL SPATIAL JOIN ON 59 CORPUS
# =============================================================

library(sf)

# ---- Load PTAL layer ----

ptal_path <- here(ptal_dir, "2015  PTALs Contours 280515.TAB")
ptal <- st_read(ptal_path, quiet = TRUE)
st_crs(ptal) <- 27700

cat("PTAL layers loaded:", nrow(ptal), "\n\n")


# ---- Convert corpus to sf, transform to BNG ----

corpus_sf <- corpus_geocoded_all %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(27700)

cat("Corpus geocoded points:", nrow(corpus_sf), "\n\n")


# ---- Spatial join ----

# 对于 already-PTAL 的 13 sub-sample,保留原 PTAL
# 对于新 geocoded 的 46,做 spatial join
corpus_new_ptal <- corpus_sf %>%
  filter(is.na(PTAL)) %>%
  st_join(ptal, join = st_within)

cat("=== New PTAL join results ===\n")
corpus_new_ptal %>%
  st_drop_geometry() %>%
  count(PTAL.y) %>%
  print()


# ---- Combine reused + new PTAL ----

corpus_with_ptal <- corpus_sf %>%
  st_drop_geometry() %>%
  left_join(
    corpus_new_ptal %>% st_drop_geometry() %>%
      select(lpa_number, PTAL_new = PTAL.y),
    by = "lpa_number"
  ) %>%
  mutate(PTAL = coalesce(PTAL, PTAL_new)) %>%
  select(-PTAL_new)

cat("\n=== Corpus PTAL distribution ===\n")
corpus_with_ptal %>% count(PTAL) %>% print()

cat("\n=== Missing PTAL ===\n")
missing_ptal <- corpus_with_ptal %>% filter(is.na(PTAL))
cat("Rows missing PTAL:", nrow(missing_ptal), "\n")
if (nrow(missing_ptal) > 0) {
  missing_ptal %>%
    select(lpa_number, site_name, precision, latitude, longitude) %>%
    print(width = Inf)
}


# ---- Save ----

write_csv(corpus_with_ptal, here(output_dir, "corpus_59_with_ptal.csv"))

# Save gpkg for later spatial ops
corpus_sf_final <- corpus_sf %>%
  st_drop_geometry() %>%
  left_join(
    corpus_new_ptal %>% st_drop_geometry() %>%
      select(lpa_number, PTAL_new = PTAL.y),
    by = "lpa_number"
  ) %>%
  mutate(PTAL = coalesce(PTAL, PTAL_new)) %>%
  select(-PTAL_new) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(27700)

st_write(corpus_sf_final, 
         here(output_dir, "corpus_59_with_ptal.gpkg"),
         delete_dsn = TRUE, quiet = TRUE)

cat("\nSaved:\n")
cat("- output/corpus_59_with_ptal.csv\n")
cat("- output/corpus_59_with_ptal.gpkg\n")