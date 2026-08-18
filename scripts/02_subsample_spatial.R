# =============================================================
# 02_subsample_spatial.R
# =============================================================
# Sub-sample selection → geocoding → PTAL join → Wood Wharf补加
#
# Input:  output/th_74_typed.csv
#         data/2015  PTALs Grid Values/*.TAB
# Output: output/sub_sample_20.csv
#         output/sub_sample_20_geocoded_final.csv
#         output/sub_sample_with_ptal.csv
#         output/sub_sample_with_ptal.gpkg
# =============================================================

source(here::here("scripts", "00_setup.R"))


# ---- STAGE 8: SUB-SAMPLE SELECTION ----

th_typed <- read_csv(here(output_dir, "th_74_typed.csv"),
                     show_col_types = FALSE)

set.seed(42)

demolition_5 <- th_typed %>%
  filter(units_lost > 0,
         !str_detect(lpa_number, "/A[0-9]"),
         !str_detect(description, "S73|amendment|variation")) %>%
  arrange(desc(units_lost)) %>%
  slice_head(n = 5)

new_build_15 <- th_typed %>%
  filter(units_lost == 0) %>%
  mutate(period = case_when(
    financial_year <= 2017 ~ "early",
    financial_year <= 2021 ~ "mid",
    TRUE                    ~ "recent"
  )) %>%
  group_by(period) %>%
  slice_sample(n = 5) %>%
  ungroup()

sub_sample <- bind_rows(demolition_5, new_build_15)

cat("Sub-sample size:", nrow(sub_sample), "\n")
write_csv(sub_sample, here(output_dir, "sub_sample_20.csv"))


# ---- STAGE 9: GEOCODING via postcodes.io ----

sub_sample_pc <- sub_sample %>%
  mutate(
    postcode_from_site = str_extract(
      site_name, "[A-Z]{1,2}[0-9][A-Z0-9]?\\s?[0-9][A-Z]{2}"
    ),
    postcode_final = coalesce(
      str_trim(str_to_upper(postcode_from_site)),
      str_trim(str_to_upper(postcode))
    ),
    is_full_postcode = str_detect(
      postcode_final, "^[A-Z]{1,2}[0-9][A-Z0-9]?\\s?[0-9][A-Z]{2}$"
    )
  )

geocode_full_postcode <- function(pc) {
  pc_clean <- str_replace_all(pc, " ", "")
  url <- paste0("https://api.postcodes.io/postcodes/", pc_clean)
  response <- try(GET(url), silent = TRUE)
  if (inherits(response, "try-error") || status_code(response) != 200) {
    return(tibble(latitude = NA_real_, longitude = NA_real_,
                  precision = "failed", source = "postcodes.io_full"))
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

geocoded <- sub_sample_pc %>%
  rowwise() %>%
  mutate(
    outward = str_extract(postcode_final, "^[A-Z]{1,2}[0-9][A-Z0-9]?"),
    result = list(
      case_when(
        is_full_postcode ~ list(geocode_full_postcode(postcode_final)),
        !is.na(outward)  ~ list(geocode_outward(outward)),
        TRUE             ~ list(tibble(latitude = NA_real_, longitude = NA_real_,
                                       precision = "no_postcode", source = "none"))
      )[[1]]
    )
  ) %>%
  unnest(result) %>%
  ungroup()

geocoded <- geocoded %>%
  mutate(in_th_bbox = latitude >= 51.49 & latitude <= 51.55 &
           longitude >= -0.08 & longitude <= 0.00)

cat("\n=== Geocoding results ===\n")
geocoded %>% count(precision) %>% print()

geocoded %>%
  select(lpa_number, financial_year, units_proposed, units_lost,
         site_name, postcode_final, precision, latitude, longitude,
         in_th_bbox, source) %>%
  write_csv(here(output_dir, "sub_sample_20_geocoded_final.csv"))


# ---- STAGE 10: PTAL SPATIAL JOIN ----

ptal_path <- here(ptal_dir, "2015  PTALs Contours 280515.TAB")

ptal <- st_read(ptal_path, quiet = TRUE)
st_crs(ptal) <- 27700

sites_with_coords <- geocoded %>%
  filter(!is.na(latitude), !is.na(longitude))

sites_sf <- st_as_sf(sites_with_coords,
                     coords = c("longitude", "latitude"),
                     crs = 4326)
sites_bng <- st_transform(sites_sf, 27700)

sites_with_ptal <- st_join(sites_bng, ptal, join = st_within)

cat("\n=== PTAL join results ===\n")
sites_with_ptal %>%
  st_drop_geometry() %>%
  count(PTAL) %>% print()

st_write(sites_with_ptal,
         here(output_dir, "sub_sample_with_ptal.gpkg"),
         delete_dsn = TRUE, quiet = TRUE)

sites_with_ptal %>%
  st_drop_geometry() %>%
  write_csv(here(output_dir, "sub_sample_with_ptal.csv"))


# ---- STAGE 13: ADD WOOD WHARF TO PTAL DATASET ----
# Wood Wharf (PA/13/02966) was missing from sub-sample due to
# earlier filter. E14 9SF is a terminated postcode; coordinates
# from postcodes.io /terminated endpoint.

response <- GET("https://api.postcodes.io/postcodes/E149SF")
wood_wharf_geo <- content(response)$terminated

wood_wharf <- tibble(
  lpa_number = "PA/13/02966",
  financial_year = 2014,
  units_proposed = 3107,
  units_lost = 29,
  site_name = "Wood Wharf",
  postcode_final = "E14 9SF",
  precision = "unit_postcode_terminated",
  latitude = wood_wharf_geo$latitude,
  longitude = wood_wharf_geo$longitude,
  in_th_bbox = TRUE,
  source = "postcodes.io_terminated"
)

wood_wharf_sf <- st_as_sf(wood_wharf,
                          coords = c("longitude", "latitude"),
                          crs = 4326, remove = FALSE) %>%
  st_transform(27700)

wood_wharf_joined <- st_join(wood_wharf_sf, ptal, join = st_within) %>%
  st_drop_geometry()

cat("\nWood Wharf PTAL:", wood_wharf_joined$PTAL, "\n")

existing_ptal <- read_csv(here(output_dir, "sub_sample_with_ptal.csv"),
                          show_col_types = FALSE)

# 只有 Wood Wharf 还不在数据里时才 append
if (!"PA/13/02966" %in% existing_ptal$lpa_number) {
  new_row <- wood_wharf_joined %>%
    select(any_of(names(existing_ptal)))
  
  ptal_full <- bind_rows(existing_ptal, new_row)
  write_csv(ptal_full, here(output_dir, "sub_sample_with_ptal.csv"))
  cat("Wood Wharf appended. Total rows:", nrow(ptal_full), "\n")
} else {
  cat("Wood Wharf already in PTAL dataset.\n")
}

cat("\n=== 02_subsample_spatial.R DONE ===\n")