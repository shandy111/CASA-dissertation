# =============================================================
# 09_spatial_analysis.R
# =============================================================
# Comprehensive spatial-discourse analysis:
# - Stage 41: 8-indicator × 11-narrative correlation heatmap
# - Stage 42: Moran's I on narrative frequencies (spatial 
#   autocorrelation)
# - Stage 43: GWR on selected narrative-indicator pairings
#
# Input:  output/corpus_59_full_spatial.csv
# Output: output/fig_spatial_discourse_heatmap.png
#         output/moran_results.csv
#         output/gwr_results.csv
# =============================================================


source(here::here("scripts", "00_setup.R"))
library(sf)


# =============================================================
# STAGE 41 — COMPREHENSIVE HEATMAP
# =============================================================

full_master <- read_csv(here(output_dir, "corpus_59_full_spatial.csv"),
                        show_col_types = FALSE)
# ---- Fix: PTAL column missing from full_master, join from PTAL file ----

ptal_data <- read_csv(here(output_dir, "corpus_59_with_ptal.csv"),
                      show_col_types = FALSE) %>%
  select(lpa_number, PTAL)

full_master <- full_master %>%
  left_join(ptal_data, by = "lpa_number") %>%
  mutate(PTAL_numeric = case_when(
    PTAL == "1b" ~ 1.5, PTAL == "2" ~ 2, PTAL == "3" ~ 3,
    PTAL == "4" ~ 4, PTAL == "5" ~ 5, PTAL == "6a" ~ 6,
    TRUE ~ NA_real_
  ))

# 保存回 CSV(以后再打开 09 不用再 fix)
write_csv(full_master, here(output_dir, "corpus_59_full_spatial.csv"))
cat("PTAL joined and saved back to CSV\n")

cat("=== PTAL distribution ===\n")
full_master %>% count(PTAL) %>% print()

# ---- Define narrative × spatial indicator grid ----

narrative_cols <- c("regeneration", "redevelopment", "mixed_use",
                    "community", "sustainability", "connectivity",
                    "public_realm", "affordable", "high_density",
                    "placemaking", "heritage", "displacement")

# Ordered by category
spatial_indicators <- list(
  "Transport" = c("PTAL_numeric" = "PTAL",
                  "dist_to_station_m" = "Dist to station",
                  "n_bus_stops_500m" = "Bus stops (500m)",
                  "dist_to_cycle_lane_m" = "Dist to cycle lane",
                  "n_cycle_parking_500m" = "Cycle parking (500m)"),
  "Deprivation" = c("imd_score" = "IMD Score",
                    "imd_decile" = "IMD Decile",
                    "income_score" = "Income depriv.",
                    "barriers_housing_score" = "Housing barriers"),
  "Environment" = c("canopy_pct" = "Tree canopy %",
                    "open_space_pct_500m" = "Open space % (500m)",
                    "n_open_spaces_500m" = "N open spaces (500m)"),
  "Amenity" = c("dist_to_school_m" = "Dist to school",
                "n_schools_500m" = "N schools (500m)")
)


# ---- Compute all correlations ----

correlation_df <- tibble()

for (category in names(spatial_indicators)) {
  for (indicator_var in names(spatial_indicators[[category]])) {
    indicator_label <- spatial_indicators[[category]][[indicator_var]]
    for (narrative in narrative_cols) {
      r <- suppressWarnings(cor(
        full_master[[narrative]],
        full_master[[indicator_var]],
        use = "pairwise.complete.obs",
        method = "spearman"
      ))
      correlation_df <- correlation_df %>%
        bind_rows(tibble(
          narrative = narrative,
          category = category,
          indicator = indicator_label,
          spearman = r
        ))
    }
  }
}


# ---- Plot heatmap ----

# 排序:narrative 按 category (compensation / factual / mixed)
narrative_order <- c(
  "public_realm", "community", "placemaking", "high_density",     # compensation/scale
  "connectivity", "mixed_use",                                     # transport-related
  "affordable", "redevelopment",                                   # baseline
  "regeneration", "sustainability", "heritage",                    # uniform
  "displacement"                                                    # non-affirmed
)

# 排序 indicators 按 category
indicator_order <- correlation_df %>%
  distinct(category, indicator) %>%
  arrange(match(category, c("Transport", "Deprivation", "Environment", "Amenity")),
          indicator) %>%
  pull(indicator)

fig_heatmap <- correlation_df %>%
  mutate(narrative = factor(narrative, levels = rev(narrative_order)),
         indicator = factor(indicator, levels = indicator_order)) %>%
  ggplot(aes(x = indicator, y = narrative, fill = spearman)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.2f", spearman)),
            size = 2.8, color = "black") +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b",
                       midpoint = 0, limits = c(-0.6, 0.6),
                       na.value = "grey90", name = "Spearman ρ") +
  labs(title = "Narrative × Spatial Indicator Correlation Matrix",
       subtitle = "59 Tower Hamlets major residential schemes",
       x = NULL, y = NULL,
       caption = "Blue = negative (narrative used more in low-value areas). Red = positive.") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 9),
        plot.title = element_text(face = "bold", size = 12),
        legend.position = "right")

ggsave(here(output_dir, "fig_spatial_discourse_heatmap.png"), fig_heatmap,
       width = 11, height = 8, dpi = 300, bg = "white")

cat("=== Heatmap saved ===\n")
cat("output/fig_spatial_discourse_heatmap.png\n\n")


# Also save correlations
write_csv(correlation_df, here(output_dir, "correlation_narrative_spatial.csv"))
# =============================================================
# STAGE 42 — MORAN'S I ON NARRATIVE FREQUENCIES
# =============================================================
# Tests whether each narrative's frequency is spatially clustered
# across the 59-project corpus.
#
# H0: narrative frequency is randomly distributed in space
# HA: narrative frequency is spatially clustered (or dispersed)
#
# Method: Moran's I with row-normalized k-nearest-neighbor weights (k=5)
# Significance: Monte Carlo permutation test (999 permutations)
# =============================================================

library(spdep)  # if not installed: install.packages("spdep")

# ---- Load corpus with spatial data ----

corpus_sf <- st_read(here(output_dir, "corpus_59_with_ptal.gpkg"),
                     quiet = TRUE) %>%
  st_transform(27700)

full_master <- read_csv(here(output_dir, "corpus_59_full_spatial.csv"),
                        show_col_types = FALSE)


# ---- Join master data to sf ----

corpus_analysis <- corpus_sf %>%
  select(lpa_number) %>%
  left_join(full_master, by = "lpa_number")

cat("Corpus for Moran's I:", nrow(corpus_analysis), "projects\n\n")


# ---- Build spatial weights (k-nearest neighbors, k=5) ----

# 用 k-NN 因为 corpus 是 point pattern(不是 polygon)
coords <- st_coordinates(corpus_analysis)

# k = 5 nearest neighbors
knn5 <- knn2nb(knearneigh(coords, k = 5))
listw_knn5 <- nb2listw(knn5, style = "W")  # row-normalized

cat("Spatial weights: k=5 nearest neighbors, row-normalized\n\n")


# ---- Moran's I on each narrative ----

narrative_cols <- c("regeneration", "redevelopment", "mixed_use",
                    "community", "sustainability", "connectivity",
                    "public_realm", "affordable", "high_density",
                    "placemaking", "heritage", "displacement")

moran_results <- tibble()

set.seed(42)

for (n in narrative_cols) {
  values <- corpus_analysis[[n]]
  
  if (all(is.na(values))) next
  
  # Monte Carlo Moran's I (999 permutations)
  mc_result <- moran.mc(values, listw = listw_knn5, nsim = 999)
  
  moran_results <- moran_results %>%
    bind_rows(tibble(
      narrative = n,
      morans_I = as.numeric(mc_result$statistic),
      p_value = mc_result$p.value,
      significant_at_05 = mc_result$p.value < 0.05,
      significant_at_01 = mc_result$p.value < 0.01
    ))
}


# ---- Report ----

cat("=== Moran's I results (k=5 NN weights, 999 permutations) ===\n\n")
moran_results %>%
  arrange(desc(morans_I)) %>%
  mutate(across(c(morans_I, p_value), ~ round(., 4))) %>%
  print(n = Inf, width = Inf)


# ---- Interpretation summary ----

cat("\n=== Significant spatial clustering (p < 0.05) ===\n")
sig_narratives <- moran_results %>%
  filter(significant_at_05) %>%
  arrange(desc(morans_I))

if (nrow(sig_narratives) > 0) {
  cat("These narratives are spatially clustered:\n")
  sig_narratives %>%
    select(narrative, morans_I, p_value) %>%
    print()
} else {
  cat("No narrative shows statistically significant clustering.\n")
  cat("Discourse is spatially random across the 59-project corpus.\n")
}


# ---- Save ----
write_csv(moran_results, here(output_dir, "moran_results.csv"))
cat("\nSaved: output/moran_results.csv\n")
# =============================================================
# STAGE 43 — GEOGRAPHICALLY WEIGHTED REGRESSION (GWR)
# =============================================================
# Tests whether narrative-context relationships vary spatially.
#
# 3 pairings:
#   (1) public_realm ~ imd_score
#   (2) displacement ~ dist_to_school_m
#   (3) high_density ~ building_height_max_storeys
#
# Method: GWR with adaptive bandwidth (k nearest neighbors)
# Significance: z-score and p-value per local coefficient
# =============================================================

library(GWmodel)   # if not installed: install.packages("GWmodel")


# ---- Prepare spatial data ----

corpus_analysis <- corpus_sf %>%
  select(lpa_number) %>%
  left_join(full_master, by = "lpa_number") %>%
  st_transform(27700)

# GWR needs Spatial* (sp) objects, not sf
corpus_sp <- as(corpus_analysis, "Spatial")


# ---- Helper: run GWR for one pairing ----

run_gwr <- function(y_var, x_var, label) {
  
  cat("\n=== GWR:", label, "===\n")
  
  data <- corpus_sp
  data@data <- data@data %>%
    select(y = all_of(y_var), x = all_of(x_var)) %>%
    filter(!is.na(y), !is.na(x))
  
  # Rebuild sp with filtered data
  keep_idx <- !is.na(corpus_sp@data[[y_var]]) & !is.na(corpus_sp@data[[x_var]])
  data_sp <- corpus_sp[keep_idx, ]
  data_sp@data <- data.frame(
    y = corpus_sp@data[[y_var]][keep_idx],
    x = corpus_sp@data[[x_var]][keep_idx]
  )
  
  cat("N observations:", nrow(data_sp), "\n")
  
  # Global OLS for baseline
  ols_model <- lm(y ~ x, data = data_sp@data)
  ols_r2 <- summary(ols_model)$r.squared
  ols_coef <- coef(ols_model)[2]
  ols_p <- summary(ols_model)$coefficients[2, 4]
  
  cat(sprintf("Global OLS:  β = %.4f, p = %.3f, R² = %.3f\n",
              ols_coef, ols_p, ols_r2))
  
  # Adaptive bandwidth via cross-validation
  set.seed(42)
  bw <- bw.gwr(y ~ x, data = data_sp,
               approach = "AICc",
               kernel = "bisquare",
               adaptive = TRUE)
  
  cat(sprintf("Optimal bandwidth: k = %.0f neighbors\n", bw))
  
  # Fit GWR
  gwr_model <- gwr.basic(y ~ x, data = data_sp,
                         bw = bw, kernel = "bisquare",
                         adaptive = TRUE)
  
  # Extract local coefficients + z-scores + p-values
  local_coef <- gwr_model$SDF$x
  local_se <- gwr_model$SDF$x_SE
  local_z <- local_coef / local_se
  local_p <- 2 * (1 - pnorm(abs(local_z)))
  
  cat(sprintf("Local coef range: [%.4f, %.4f]\n",
              min(local_coef), max(local_coef)))
  cat(sprintf("Local coef mean: %.4f\n", mean(local_coef)))
  cat(sprintf("Sig. at p<0.05: %d / %d local points\n",
              sum(local_p < 0.05), length(local_p)))
  cat(sprintf("GWR R²: %.3f\n", 1 - sum(gwr_model$SDF$residual^2) / 
                sum((data_sp@data$y - mean(data_sp@data$y))^2)))
  
  tibble(
    pairing = label,
    n = nrow(data_sp),
    global_beta = ols_coef,
    global_p = ols_p,
    global_r2 = ols_r2,
    bandwidth_k = bw,
    local_beta_min = min(local_coef),
    local_beta_max = max(local_coef),
    local_beta_mean = mean(local_coef),
    n_sig_local = sum(local_p < 0.05),
    gwr_r2 = 1 - sum(gwr_model$SDF$residual^2) / 
      sum((data_sp@data$y - mean(data_sp@data$y))^2)
  )
}


# ---- Run 3 GWR pairings ----

gwr_summary <- bind_rows(
  run_gwr("public_realm", "imd_score", "public_realm ~ IMD"),
  run_gwr("displacement", "dist_to_school_m", "displacement ~ dist_to_school"),
  run_gwr("high_density", "building_height_max_storeys", 
          "high_density ~ height")
)


# ---- Overall summary ----

cat("\n\n=== GWR summary table ===\n")
gwr_summary %>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  print(width = Inf)


# ---- Save ----
write_csv(gwr_summary, here(output_dir, "gwr_summary.csv"))
cat("\nSaved: output/gwr_summary.csv\n")
# =============================================================
# STAGE 44 — VISUALIZE GWR LOCAL COEFFICIENTS
# =============================================================
# Map local GWR coefficients for each of the 3 pairings.
# Show which sub-areas of Tower Hamlets have strong/weak/
# opposite relationships between discourse and outcome.
# =============================================================

library(GWmodel)
library(sf)


# ---- Helper: run GWR and return sf with coefficients ----

run_gwr_map <- function(y_var, x_var, label) {
  
  keep_idx <- !is.na(corpus_sp@data[[y_var]]) & !is.na(corpus_sp@data[[x_var]])
  data_sp <- corpus_sp[keep_idx, ]
  data_sp@data <- data.frame(
    y = corpus_sp@data[[y_var]][keep_idx],
    x = corpus_sp@data[[x_var]][keep_idx],
    lpa_number = corpus_sp@data$lpa_number[keep_idx]
  )
  
  set.seed(42)
  bw <- bw.gwr(y ~ x, data = data_sp,
               approach = "AICc", kernel = "bisquare", adaptive = TRUE)
  
  gwr_model <- gwr.basic(y ~ x, data = data_sp,
                         bw = bw, kernel = "bisquare", adaptive = TRUE)
  
  # Extract sf with local coefficients
  result_sf <- st_as_sf(gwr_model$SDF) %>%
    st_set_crs(27700) %>%
    mutate(
      lpa_number = data_sp@data$lpa_number,
      local_beta = x,
      local_se = x_SE,
      local_z = local_beta / local_se,
      local_p = 2 * (1 - pnorm(abs(local_z))),
      significant = local_p < 0.05,
      pairing = label
    ) %>%
    select(lpa_number, local_beta, local_se, local_z, local_p, 
           significant, pairing, geometry)
  
  result_sf
}


# ---- Run all 3 for mapping ----

cat("Building GWR maps for 3 pairings...\n\n")

gwr_public <- run_gwr_map("public_realm", "imd_score", 
                          "public_realm ~ IMD")
gwr_disp <- run_gwr_map("displacement", "dist_to_school_m", 
                        "displacement ~ dist_to_school")
gwr_dens <- run_gwr_map("high_density", "building_height_max_storeys",
                        "high_density ~ height")


# ---- Load Tower Hamlets boundary for basemap ----

th_lsoa <- st_read(here("data", "lsoa_boundaries", "Tower Hamlets.shp"),
                   quiet = TRUE)

th_boundary <- th_lsoa %>%
  st_union() %>%
  st_as_sf()


# ---- Individual maps ----

make_gwr_map <- function(gwr_sf, title, subtitle) {
  ggplot() +
    geom_sf(data = th_boundary, fill = "grey95", color = "grey50",
            linewidth = 0.3) +
    geom_sf(data = gwr_sf, aes(color = local_beta, shape = significant),
            size = 3, alpha = 0.85) +
    scale_color_gradient2(low = "#2166ac", mid = "white", high = "#b2182b",
                          midpoint = 0, name = "Local β") +
    scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1),
                       name = "p < 0.05",
                       labels = c("TRUE" = "Significant", 
                                  "FALSE" = "Not sig.")) +
    labs(title = title, subtitle = subtitle) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold", size = 11),
          plot.subtitle = element_text(size = 9, color = "grey30"),
          legend.position = "right")
}

# 3 maps
map1 <- make_gwr_map(gwr_public,
                     "Public realm ~ IMD",
                     "Local β range [-0.077, -0.023] · Global β = -0.041")

map2 <- make_gwr_map(gwr_disp,
                     "Displacement ~ Distance to school",
                     "Local β range [0.0006, 0.0012] · Global β = 0.001")

map3 <- make_gwr_map(gwr_dens,
                     "High-density ~ Building height",
                     "Local β range [0.030, 0.157] · Global β = 0.086 · GWR R² = 0.47")


# ---- Combine and save ----

library(patchwork)

combined <- (map1 + map2 + map3) +
  plot_annotation(
    title = "Geographically Weighted Regression: local coefficient maps",
    subtitle = "59 Tower Hamlets major residential schemes · adaptive bisquare kernel",
    theme = theme(plot.title = element_text(size = 14, face = "bold"),
                  plot.subtitle = element_text(size = 10, color = "grey30"))
  )

ggsave(here(output_dir, "fig_gwr_local_coefficients.png"), combined,
       width = 16, height = 6, dpi = 300, bg = "white")

cat("=== GWR map saved ===\n")
cat("output/fig_gwr_local_coefficients.png\n\n")


# Also save individual coefficient data
bind_rows(
  gwr_public %>% st_drop_geometry(),
  gwr_disp %>% st_drop_geometry(),
  gwr_dens %>% st_drop_geometry()
) %>%
  write_csv(here(output_dir, "gwr_local_coefficients.csv"))

cat("Saved: output/gwr_local_coefficients.csv\n")
# =============================================================
# STAGE 45 — LOCAL MORAN'S I (LISA)
# =============================================================
# Local counterpart to Global Moran's I.
# Identifies HH / LL / HL / LH clusters for each narrative.
#
# Focus:
#   - public_realm (only global-significant narrative)
#   - heritage, high_density (near-significant globally)
#   - Also run all 12 for completeness
# =============================================================

library(spdep)


# ---- Load spatial data ----

corpus_sf <- st_read(here(output_dir, "corpus_59_with_ptal.gpkg"),
                     quiet = TRUE) %>%
  st_transform(27700)

full_master <- read_csv(here(output_dir, "corpus_59_full_spatial.csv"),
                        show_col_types = FALSE)

corpus_analysis <- corpus_sf %>%
  select(lpa_number) %>%
  left_join(full_master, by = "lpa_number")


# ---- Build k=5 NN weights ----

coords <- st_coordinates(corpus_analysis)
knn5 <- knn2nb(knearneigh(coords, k = 5))
listw_knn5 <- nb2listw(knn5, style = "W")


# ---- Run LISA on each narrative ----

narrative_cols <- c("regeneration", "redevelopment", "mixed_use",
                    "community", "sustainability", "connectivity",
                    "public_realm", "affordable", "high_density",
                    "placemaking", "heritage", "displacement")

lisa_all <- list()

for (n in narrative_cols) {
  values <- corpus_analysis[[n]]
  
  # localmoran returns matrix with Ii, E.Ii, Var.Ii, Z.Ii, Pr(z)
  lm_result <- localmoran(values, listw = listw_knn5)
  
  # Classify: HH / LL / HL / LH / NS
  # 用 z-scores 判断
  values_std <- scale(values)[, 1]
  lag_values <- lag.listw(listw_knn5, values)
  lag_std <- scale(lag_values)[, 1]
  
  p_value <- lm_result[, "Pr(z != E(Ii))"]
  
  cluster_type <- case_when(
    p_value >= 0.05 ~ "NS",  # Not significant
    values_std > 0 & lag_std > 0 ~ "HH",
    values_std < 0 & lag_std < 0 ~ "LL",
    values_std > 0 & lag_std < 0 ~ "HL",
    values_std < 0 & lag_std > 0 ~ "LH",
    TRUE ~ "NS"
  )
  
  lisa_all[[n]] <- tibble(
    lpa_number = corpus_analysis$lpa_number,
    narrative = n,
    local_I = as.numeric(lm_result[, "Ii"]),
    z_score = as.numeric(lm_result[, "Z.Ii"]),
    p_value = p_value,
    cluster_type = cluster_type
  )
}

lisa_results <- bind_rows(lisa_all)


# ---- Summary: which narratives have local clusters? ----

cat("=== LISA cluster summary by narrative ===\n\n")

lisa_summary <- lisa_results %>%
  filter(cluster_type != "NS") %>%
  count(narrative, cluster_type) %>%
  pivot_wider(names_from = cluster_type, values_from = n, values_fill = 0)

# Ensure all 4 columns exist
for (col in c("HH", "LL", "HL", "LH")) {
  if (!col %in% names(lisa_summary)) lisa_summary[[col]] <- 0
}

lisa_summary <- lisa_summary %>%
  mutate(total_sig = HH + LL + HL + LH) %>%
  arrange(desc(total_sig)) %>%
  select(narrative, HH, LL, HL, LH, total_sig)

print(lisa_summary)


# ---- Save results ----

write_csv(lisa_results, here(output_dir, "lisa_results.csv"))
write_csv(lisa_summary, here(output_dir, "lisa_summary.csv"))
cat("\nSaved:\n")
cat("- output/lisa_results.csv (all narrative × project LISA)\n")
cat("- output/lisa_summary.csv (cluster counts by narrative)\n\n")


# ---- Focus map: public_realm LISA cluster ----

th_boundary <- st_read(
  here("data", "lsoa_boundaries", "Tower Hamlets.shp"),
  quiet = TRUE
) %>%
  st_union() %>%
  st_as_sf()

# Public realm LISA + geometry
lisa_public <- corpus_analysis %>%
  select(lpa_number) %>%
  left_join(
    lisa_results %>% filter(narrative == "public_realm"),
    by = "lpa_number"
  ) %>%
  mutate(cluster_type = factor(cluster_type, 
                               levels = c("HH", "LL", "HL", "LH", "NS")))

# Cluster color palette
cluster_colors <- c(
  "HH" = "#b2182b",  # dark red (high surrounded by high)
  "LL" = "#2166ac",  # dark blue (low surrounded by low)
  "HL" = "#ef8a62",  # light red (outlier: high in low context)
  "LH" = "#67a9cf",  # light blue (outlier: low in high context)
  "NS" = "#d9d9d9"   # grey (not significant)
)

map_lisa_public <- ggplot() +
  geom_sf(data = th_boundary, fill = "grey96", color = "grey60",
          linewidth = 0.3) +
  geom_sf(data = lisa_public, aes(color = cluster_type),
          size = 3, alpha = 0.9) +
  scale_color_manual(values = cluster_colors, name = "LISA cluster") +
  labs(
    title = "Local Moran's I: Public realm narrative",
    subtitle = "HH = high value surrounded by high; LL = low-low; HL/LH = spatial outliers",
    caption = "k=5 nearest-neighbor weights, p < 0.05"
  ) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 9, color = "grey30"))

ggsave(here(output_dir, "fig_lisa_public_realm.png"), map_lisa_public,
       width = 8, height = 6, dpi = 300, bg = "white")

cat("Saved: output/fig_lisa_public_realm.png\n")
# =============================================================
# STAGE 46 — BIVARIATE LOCAL MORAN'S I
# =============================================================
# Tests spatial co-clustering of narrative × context variable.
#
# 3 pairings:
#   (1) public_realm × IMD_score
#       — compensation discourse ↔ deprivation
#   (2) high_density × canopy_pct  
#       — density rhetoric ↔ material environment
#   (3) mixed_use × PTAL_numeric
#       — mixed-use narrative ↔ transport accessibility
#
# HH: high narrative + high context lag
# LL: low narrative + low context lag
# HL: high narrative + low context lag (compensation outlier)
# LH: low narrative + high context lag
# =============================================================


# ---- Reuse spatial weights ----
# listw_knn5 already built in Stage 45


# ---- Helper: Bivariate Local Moran ----

bivariate_lisa <- function(x_var, y_var, listw, alpha = 0.05) {
  # Standardize both
  x_std <- as.numeric(scale(x_var))
  y_std <- as.numeric(scale(y_var))
  
  # Neighbor-lagged Y (context 邻居值)
  y_lag <- lag.listw(listw, y_std)
  
  # Local Moran statistic: x_i * y_lag_i
  local_stat <- x_std * y_lag
  
  # Monte Carlo permutation for significance
  n_sim <- 999
  set.seed(42)
  perm_stats <- matrix(NA, nrow = length(x_var), ncol = n_sim)
  
  for (i in 1:n_sim) {
    y_perm <- sample(y_std)
    y_perm_lag <- lag.listw(listw, y_perm)
    perm_stats[, i] <- x_std * y_perm_lag
  }
  
  # Pseudo p-value (two-sided)
  p_value <- rowMeans(abs(perm_stats) >= abs(local_stat))
  
  # Classify clusters
  cluster_type <- case_when(
    p_value >= alpha ~ "NS",
    x_std > 0 & y_lag > 0 ~ "HH",
    x_std < 0 & y_lag < 0 ~ "LL",
    x_std > 0 & y_lag < 0 ~ "HL",
    x_std < 0 & y_lag > 0 ~ "LH",
    TRUE ~ "NS"
  )
  
  tibble(
    local_stat = local_stat,
    x_std = x_std,
    y_lag = y_lag,
    p_value = p_value,
    cluster_type = cluster_type
  )
}


# ---- Run 3 Bivariate LISA pairings ----

bivar_summary <- tibble()
bivar_all <- list()

pairings <- list(
  list(x = "public_realm", y = "imd_score", 
       label = "public_realm × IMD (compensation)"),
  list(x = "high_density", y = "canopy_pct",
       label = "high_density × canopy"),
  list(x = "mixed_use", y = "PTAL_numeric",
       label = "mixed_use × PTAL")
)

for (p in pairings) {
  cat("Running:", p$label, "\n")
  
  # Handle NA (filter to complete pairs)
  x_vals <- corpus_analysis[[p$x]]
  y_vals <- corpus_analysis[[p$y]]
  complete_idx <- !is.na(x_vals) & !is.na(y_vals)
  
  if (sum(complete_idx) < nrow(corpus_analysis)) {
    cat("  Dropping", sum(!complete_idx), "NA rows\n")
    # Rebuild weights for filtered subset
    coords_sub <- coords[complete_idx, ]
    knn_sub <- knn2nb(knearneigh(coords_sub, k = 5))
    listw_sub <- nb2listw(knn_sub, style = "W")
    
    result <- bivariate_lisa(x_vals[complete_idx], 
                             y_vals[complete_idx], 
                             listw_sub)
    result$lpa_number <- corpus_analysis$lpa_number[complete_idx]
  } else {
    result <- bivariate_lisa(x_vals, y_vals, listw_knn5)
    result$lpa_number <- corpus_analysis$lpa_number
  }
  
  result$pairing <- p$label
  bivar_all[[p$label]] <- result
  
  # Summary counts
  summary_counts <- result %>%
    filter(cluster_type != "NS") %>%
    count(cluster_type) %>%
    pivot_wider(names_from = cluster_type, values_from = n, values_fill = 0)
  
  # Ensure all cluster types present
  for (col in c("HH", "LL", "HL", "LH")) {
    if (!col %in% names(summary_counts)) summary_counts[[col]] <- 0
  }
  
  summary_counts <- summary_counts %>%
    select(HH, LL, HL, LH) %>%
    mutate(pairing = p$label, total_sig = HH + LL + HL + LH) %>%
    select(pairing, HH, LL, HL, LH, total_sig)
  
  bivar_summary <- bind_rows(bivar_summary, summary_counts)
}


# ---- Report summary ----

cat("\n=== Bivariate LISA cluster summary ===\n")
print(bivar_summary)


# ---- Save all bivariate results ----

bivar_results_df <- bind_rows(bivar_all)
write_csv(bivar_results_df, here(output_dir, "bivariate_lisa_results.csv"))
write_csv(bivar_summary, here(output_dir, "bivariate_lisa_summary.csv"))

cat("\nSaved:\n")
cat("- output/bivariate_lisa_results.csv\n")
cat("- output/bivariate_lisa_summary.csv\n\n")


# ---- Bivariate LISA maps ----

make_bivar_map <- function(pairing_label, subtitle) {
  data <- bivar_all[[pairing_label]] %>%
    mutate(cluster_type = factor(cluster_type,
                                 levels = c("HH", "LL", "HL", "LH", "NS")))
  
  map_data <- corpus_analysis %>%
    select(lpa_number) %>%
    left_join(data, by = "lpa_number")
  
  ggplot() +
    geom_sf(data = th_boundary, fill = "grey96", color = "grey60",
            linewidth = 0.3) +
    geom_sf(data = map_data, aes(color = cluster_type),
            size = 3, alpha = 0.9) +
    scale_color_manual(values = cluster_colors, name = "LISA cluster") +
    labs(title = pairing_label, subtitle = subtitle) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold", size = 11),
          plot.subtitle = element_text(size = 9, color = "grey30"))
}

map_bivar1 <- make_bivar_map(
  "public_realm × IMD (compensation)",
  "HH: narrative high + neighbors' IMD high (well-off) | LL: low narrative + deprived neighbors"
)

map_bivar2 <- make_bivar_map(
  "high_density × canopy",
  "HH: narrative high + neighbors' canopy high | LL: low narrative + low canopy neighbors"
)

map_bivar3 <- make_bivar_map(
  "mixed_use × PTAL",
  "HH: narrative high + neighbors' PTAL high | LL: low narrative + low PTAL neighbors"
)


# Combine
library(patchwork)
combined_bivar <- (map_bivar1 + map_bivar2 + map_bivar3) +
  plot_annotation(
    title = "Bivariate Local Moran's I: Discourse × Spatial Context",
    subtitle = "999 Monte Carlo permutations, p < 0.05 significance threshold",
    theme = theme(plot.title = element_text(size = 14, face = "bold"),
                  plot.subtitle = element_text(size = 10, color = "grey30"))
  )

ggsave(here(output_dir, "fig_bivariate_lisa.png"), combined_bivar,
       width = 16, height = 6, dpi = 300, bg = "white")

cat("Saved: output/fig_bivariate_lisa.png\n")
# =============================================================
# STAGE 47 — LSOA-LEVEL AGGREGATION (parallel to project-level)
# =============================================================
# Aggregate 59 project narrative frequencies + indicators to 
# 169 Tower Hamlets LSOA polygons.
# Rerun Univariate + Bivariate LISA on LSOA level for comparison.
#
# Purpose: Chng-style multi-scale analytical unit switching.
# =============================================================

library(spdep)
library(sf)


# ---- Load LSOA boundaries ----

th_lsoa_full <- st_read(
  here("data", "lsoa_boundaries", "Tower Hamlets.shp"),
  quiet = TRUE
) %>%
  st_transform(27700)

cat("LSOA polygons:", nrow(th_lsoa_full), "\n\n")


# ---- Load corpus with spatial data ----

corpus_sf_59 <- st_read(here(output_dir, "corpus_59_with_ptal.gpkg"),
                        quiet = TRUE) %>%
  st_transform(27700)

full_master <- read_csv(here(output_dir, "corpus_59_full_spatial.csv"),
                        show_col_types = FALSE)

corpus_full <- corpus_sf_59 %>%
  select(lpa_number) %>%
  left_join(full_master, by = "lpa_number")


# ---- Spatial join: which LSOA does each project fall in? ----

# Drop existing lsoa21cd first (from full_master), then re-join
project_in_lsoa <- corpus_full %>%
  select(-any_of("lsoa21cd")) %>%   # 避免 column 冲突
  st_join(th_lsoa_full %>% select(lsoa21cd), 
          join = st_within) %>%
  st_drop_geometry()

cat("Projects with LSOA:", sum(!is.na(project_in_lsoa$lsoa21cd)), "\n")
cat("Projects outside TH LSOA:", sum(is.na(project_in_lsoa$lsoa21cd)), "\n\n")


# ---- Aggregate narrative frequency to LSOA level ----

narrative_cols <- c("regeneration", "redevelopment", "mixed_use",
                    "community", "sustainability", "connectivity",
                    "public_realm", "affordable", "high_density",
                    "placemaking", "heritage", "displacement")

lsoa_narratives <- project_in_lsoa %>%
  filter(!is.na(lsoa21cd)) %>%
  group_by(lsoa21cd) %>%
  summarise(
    n_projects = n(),
    across(all_of(narrative_cols), ~ mean(., na.rm = TRUE)),
    imd_score = first(imd_score),      # LSOA level, same across projects
    imd_decile = first(imd_decile),
    canopy_pct = first(canopy_pct),
    PTAL_numeric = mean(PTAL_numeric, na.rm = TRUE),  # 项目平均
    .groups = "drop"
  )

cat("LSOA with at least 1 project:", nrow(lsoa_narratives), "\n\n")


# ---- Join to full LSOA (169 total, ~40 have projects, rest NA) ----

lsoa_analysis <- th_lsoa_full %>%
  select(lsoa21cd, geometry) %>%
  left_join(lsoa_narratives, by = "lsoa21cd")

# 只留有 project 的 LSOA 做 LISA(NA LSOA 不做)
lsoa_with_data <- lsoa_analysis %>%
  filter(!is.na(n_projects))

cat("LSOA for LISA analysis:", nrow(lsoa_with_data), "\n")


# ---- Build spatial weights on LSOA polygons ----
# 用 Queen contiguity(polygon 更 natural,和 Chng 一致)

lsoa_coords <- st_centroid(lsoa_with_data) %>%
  st_coordinates()

# 因为不是所有 LSOA 相邻(有的 LSOA 之间隔了 no-project 的 LSOA),
# 用 k=5 nearest neighbors 保持一致
knn5_lsoa <- knn2nb(knearneigh(lsoa_coords, k = 5))
listw_lsoa <- nb2listw(knn5_lsoa, style = "W")

cat("Spatial weights: k=5 NN, row-normalized\n\n")


# =============================================================
# STAGE 47a — UNIVARIATE LISA ON LSOA
# =============================================================

lisa_lsoa_all <- list()

for (n in narrative_cols) {
  values <- lsoa_with_data[[n]]
  
  if (all(is.na(values))) next
  if (sd(values, na.rm = TRUE) == 0) next  # constant, skip
  
  lm_result <- localmoran(values, listw = listw_lsoa)
  
  values_std <- scale(values)[, 1]
  lag_values <- lag.listw(listw_lsoa, values)
  lag_std <- scale(lag_values)[, 1]
  
  p_value <- lm_result[, "Pr(z != E(Ii))"]
  
  cluster_type <- case_when(
    p_value >= 0.05 ~ "NS",
    values_std > 0 & lag_std > 0 ~ "HH",
    values_std < 0 & lag_std < 0 ~ "LL",
    values_std > 0 & lag_std < 0 ~ "HL",
    values_std < 0 & lag_std > 0 ~ "LH",
    TRUE ~ "NS"
  )
  
  lisa_lsoa_all[[n]] <- tibble(
    lsoa21cd = lsoa_with_data$lsoa21cd,
    narrative = n,
    local_I = as.numeric(lm_result[, "Ii"]),
    z_score = as.numeric(lm_result[, "Z.Ii"]),
    p_value = p_value,
    cluster_type = cluster_type
  )
}

lisa_lsoa_results <- bind_rows(lisa_lsoa_all)

# Summary
lisa_lsoa_summary <- lisa_lsoa_results %>%
  filter(cluster_type != "NS") %>%
  count(narrative, cluster_type) %>%
  pivot_wider(names_from = cluster_type, values_from = n, values_fill = 0)

for (col in c("HH", "LL", "HL", "LH")) {
  if (!col %in% names(lisa_lsoa_summary)) lisa_lsoa_summary[[col]] <- 0
}

lisa_lsoa_summary <- lisa_lsoa_summary %>%
  mutate(total_sig = HH + LL + HL + LH) %>%
  arrange(desc(total_sig)) %>%
  select(narrative, HH, LL, HL, LH, total_sig)

cat("=== LSOA-level LISA summary ===\n")
print(lisa_lsoa_summary)


# Save LSOA-level LISA
write_csv(lisa_lsoa_results, here(output_dir, "lisa_results_lsoa.csv"))
write_csv(lisa_lsoa_summary, here(output_dir, "lisa_summary_lsoa.csv"))
cat("\nSaved: lisa_results_lsoa.csv, lisa_summary_lsoa.csv\n\n")


# ---- LSOA-level map: public_realm ----

lisa_public_lsoa <- lsoa_analysis %>%
  left_join(
    lisa_lsoa_results %>% filter(narrative == "public_realm"),
    by = "lsoa21cd"
  ) %>%
  mutate(cluster_type = factor(
    ifelse(is.na(cluster_type), "No data", cluster_type),
    levels = c("HH", "LL", "HL", "LH", "NS", "No data")
  ))

cluster_colors_full <- c(
  "HH" = "#b2182b", "LL" = "#2166ac",
  "HL" = "#ef8a62", "LH" = "#67a9cf",
  "NS" = "#f0f0f0", "No data" = "#ffffff"
)

map_lisa_public_lsoa <- ggplot() +
  geom_sf(data = lisa_public_lsoa, aes(fill = cluster_type),
          color = "grey60", linewidth = 0.2) +
  scale_fill_manual(values = cluster_colors_full, name = "LISA cluster") +
  labs(
    title = "LSOA-Level LISA: Public realm narrative",
    subtitle = "Aggregated to 169 Tower Hamlets LSOAs · k=5 NN weights",
    caption = "White = LSOA has no project · Grey = not significant"
  ) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 9, color = "grey30"))

ggsave(here(output_dir, "fig_lisa_public_realm_lsoa.png"), 
       map_lisa_public_lsoa,
       width = 8, height = 6, dpi = 300, bg = "white")

cat("Saved: fig_lisa_public_realm_lsoa.png\n\n")


# =============================================================
# STAGE 47b — BIVARIATE LISA ON LSOA
# =============================================================

bivariate_lisa_lsoa <- function(x_var, y_var, listw, alpha = 0.05) {
  x_std <- as.numeric(scale(x_var))
  y_std <- as.numeric(scale(y_var))
  y_lag <- lag.listw(listw, y_std)
  
  local_stat <- x_std * y_lag
  
  n_sim <- 999
  set.seed(42)
  perm_stats <- matrix(NA, nrow = length(x_var), ncol = n_sim)
  
  for (i in 1:n_sim) {
    y_perm <- sample(y_std)
    y_perm_lag <- lag.listw(listw, y_perm)
    perm_stats[, i] <- x_std * y_perm_lag
  }
  
  p_value <- rowMeans(abs(perm_stats) >= abs(local_stat))
  
  cluster_type <- case_when(
    p_value >= alpha ~ "NS",
    x_std > 0 & y_lag > 0 ~ "HH",
    x_std < 0 & y_lag < 0 ~ "LL",
    x_std > 0 & y_lag < 0 ~ "HL",
    x_std < 0 & y_lag > 0 ~ "LH",
    TRUE ~ "NS"
  )
  
  tibble(local_stat, x_std, y_lag, p_value, cluster_type)
}

pairings_lsoa <- list(
  list(x = "public_realm", y = "imd_score",
       label = "public_realm × IMD (LSOA)"),
  list(x = "high_density", y = "canopy_pct",
       label = "high_density × canopy (LSOA)"),
  list(x = "mixed_use", y = "PTAL_numeric",
       label = "mixed_use × PTAL (LSOA)")
)

bivar_lsoa_all <- list()
bivar_lsoa_summary <- tibble()

for (p in pairings_lsoa) {
  cat("Running LSOA:", p$label, "\n")
  
  x_vals <- lsoa_with_data[[p$x]]
  y_vals <- lsoa_with_data[[p$y]]
  complete_idx <- !is.na(x_vals) & !is.na(y_vals)
  
  if (sum(complete_idx) < nrow(lsoa_with_data)) {
    coords_sub <- lsoa_coords[complete_idx, ]
    knn_sub <- knn2nb(knearneigh(coords_sub, k = 5))
    listw_sub <- nb2listw(knn_sub, style = "W")
    
    result <- bivariate_lisa_lsoa(x_vals[complete_idx],
                                  y_vals[complete_idx],
                                  listw_sub)
    result$lsoa21cd <- lsoa_with_data$lsoa21cd[complete_idx]
  } else {
    result <- bivariate_lisa_lsoa(x_vals, y_vals, listw_lsoa)
    result$lsoa21cd <- lsoa_with_data$lsoa21cd
  }
  
  result$pairing <- p$label
  bivar_lsoa_all[[p$label]] <- result
  
  summary_counts <- result %>%
    filter(cluster_type != "NS") %>%
    count(cluster_type) %>%
    pivot_wider(names_from = cluster_type, values_from = n, values_fill = 0)
  
  for (col in c("HH", "LL", "HL", "LH")) {
    if (!col %in% names(summary_counts)) summary_counts[[col]] <- 0
  }
  
  summary_counts <- summary_counts %>%
    select(HH, LL, HL, LH) %>%
    mutate(pairing = p$label, total_sig = HH + LL + HL + LH) %>%
    select(pairing, HH, LL, HL, LH, total_sig)
  
  bivar_lsoa_summary <- bind_rows(bivar_lsoa_summary, summary_counts)
}

cat("\n=== LSOA-level Bivariate LISA summary ===\n")
print(bivar_lsoa_summary)


bivar_lsoa_df <- bind_rows(bivar_lsoa_all)
write_csv(bivar_lsoa_df, here(output_dir, "bivariate_lisa_results_lsoa.csv"))
write_csv(bivar_lsoa_summary, here(output_dir, "bivariate_lisa_summary_lsoa.csv"))
cat("\nSaved: bivariate_lisa_results_lsoa.csv, bivariate_lisa_summary_lsoa.csv\n\n")


# ---- Bivariate LISA maps on LSOA ----

make_bivar_map_lsoa <- function(pairing_label, subtitle) {
  data <- bivar_lsoa_all[[pairing_label]]
  
  map_data <- lsoa_analysis %>%
    left_join(data, by = "lsoa21cd") %>%
    mutate(cluster_type = factor(
      ifelse(is.na(cluster_type), "No data", cluster_type),
      levels = c("HH", "LL", "HL", "LH", "NS", "No data")
    ))
  
  ggplot() +
    geom_sf(data = map_data, aes(fill = cluster_type),
            color = "grey70", linewidth = 0.2) +
    scale_fill_manual(values = cluster_colors_full, name = "LISA cluster") +
    labs(title = pairing_label, subtitle = subtitle) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold", size = 11),
          plot.subtitle = element_text(size = 9, color = "grey30"))
}

map_bivar_lsoa1 <- make_bivar_map_lsoa(
  "public_realm × IMD (LSOA)",
  "Compensation discourse ↔ neighborhood deprivation"
)
map_bivar_lsoa2 <- make_bivar_map_lsoa(
  "high_density × canopy (LSOA)",
  "Density rhetoric ↔ green infrastructure"
)
map_bivar_lsoa3 <- make_bivar_map_lsoa(
  "mixed_use × PTAL (LSOA)",
  "Mixed-use narrative ↔ transport accessibility"
)

library(patchwork)
combined_bivar_lsoa <- (map_bivar_lsoa1 + map_bivar_lsoa2 + map_bivar_lsoa3) +
  plot_annotation(
    title = "LSOA-Level Bivariate LISA: Discourse × Spatial Context",
    subtitle = "Aggregated to 169 Tower Hamlets LSOAs · 999 permutations · p < 0.05",
    theme = theme(plot.title = element_text(size = 14, face = "bold"),
                  plot.subtitle = element_text(size = 10, color = "grey30"))
  )

ggsave(here(output_dir, "fig_bivariate_lisa_lsoa.png"), combined_bivar_lsoa,
       width = 16, height = 6, dpi = 300, bg = "white")

cat("Saved: fig_bivariate_lisa_lsoa.png\n")
# 确保 Thames 是 BNG
cat("Thames CRS:", st_crs(thames)$input, "\n")

# 强制 transform
thames <- st_transform(thames, 27700)

# 用 sf polygon 方式 crop
crop_bbox <- st_bbox(c(
  xmin = as.numeric(th_bbox$xmin) - 500,
  xmax = as.numeric(th_bbox$xmax) + 500,
  ymin = as.numeric(th_bbox$ymin) - 500,
  ymax = as.numeric(th_bbox$ymax) + 500
), crs = 27700)

thames_th <- st_crop(thames, crop_bbox)

cat("Thames features in TH bbox:", nrow(thames_th), "\n")
# =============================================================
# STAGE 48 — POLISHED MAPS (project-level, cosmetic upgrade)
# =============================================================
# Add ward boundaries, Thames river, place labels for
# professional cartographic presentation.
# =============================================================

library(sf)
library(rnaturalearth)


# ---- Load ward boundaries + Thames ----

wards_all <- st_read(here("data", "wards", "London_Ward.shp"),
                     quiet = TRUE) %>%
  st_transform(27700)

# Filter to Tower Hamlets wards
th_wards <- wards_all %>%
  filter(str_detect(DISTRICT, "Tower Hamlets"))

cat("Tower Hamlets wards:", nrow(th_wards), "\n")

# Thames (clip to Tower Hamlets area)
rivers <- ne_download(scale = 10, type = "rivers_lake_centerlines",
                      category = "physical", returnclass = "sf")
thames <- rivers %>% filter(name == "Thames") %>%
  st_transform(27700)


# ---- Tower Hamlets outline + bounding ----

th_boundary <- th_wards %>%
  st_union() %>%
  st_as_sf()

# Thames clipped to TH bbox
th_bbox <- st_bbox(th_boundary)
thames_th <- st_crop(thames, 
                     xmin = th_bbox$xmin - 500, xmax = th_bbox$xmax + 500,
                     ymin = th_bbox$ymin - 500, ymax = th_bbox$ymax + 500)


# ---- Place labels (manual, hand-placed for TH landmarks) ----

place_labels <- tribble(
  ~name, ~easting, ~northing,
  "Canary Wharf", 537300, 180500,
  "Isle of Dogs", 538000, 178800,
  "Poplar", 537500, 181400,
  "Bow", 537000, 183300,
  "Wapping", 534700, 180200,
  "Whitechapel", 534700, 181500,
  "Aldgate", 533500, 181300,
  "Stepney", 535800, 181500
) %>%
  st_as_sf(coords = c("easting", "northing"), crs = 27700, remove = FALSE)


# ---- Cluster color palette (reuse from before) ----

cluster_colors <- c(
  "HH" = "#b2182b",
  "LL" = "#2166ac",
  "HL" = "#ef8a62",
  "LH" = "#67a9cf",
  "NS" = "#d9d9d9"
)


# ---- Base layer function ----

base_layers <- function() {
  list(
    geom_sf(data = th_boundary, fill = "grey98", color = NA),
    geom_sf(data = th_wards, fill = NA, color = "grey70", 
            linewidth = 0.3),
    geom_sf(data = thames_th, color = "#4a90d9", 
            linewidth = 1.2, alpha = 0.5),
    geom_sf(data = th_boundary, fill = NA, color = "grey30",
            linewidth = 0.6),
    geom_sf_text(data = place_labels, aes(label = name),
                 size = 2.4, color = "grey40", 
                 fontface = "italic", alpha = 0.7)
  )
}


# ---- Polished LISA map (public_realm) ----

lisa_public_map <- lisa_public %>%
  st_as_sf()

map_polished_lisa <- ggplot() +
  base_layers() +
  geom_sf(data = lisa_public_map, aes(fill = cluster_type),
          shape = 21, size = 4, color = "white", 
          stroke = 0.8, alpha = 0.9) +
  scale_fill_manual(values = cluster_colors, name = "LISA cluster",
                    breaks = c("HH", "LL", "HL", "LH", "NS")) +
  labs(
    title = "Local Moran's I: Public realm narrative",
    subtitle = "59 major residential schemes · k=5 NN weights · p < 0.05",
    caption = "HH = high-value cluster · LL = low-value cluster\nHL/LH = spatial outliers"
  ) +
  coord_sf(xlim = c(th_bbox$xmin - 200, th_bbox$xmax + 200),
           ylim = c(th_bbox$ymin - 200, th_bbox$ymax + 200)) +
  theme_void(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0),
    plot.subtitle = element_text(size = 10, color = "grey30", hjust = 0),
    plot.caption = element_text(size = 8, color = "grey40", hjust = 0),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 9),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave(here(output_dir, "fig_lisa_public_realm_polished.png"),
       map_polished_lisa, width = 9, height = 7, dpi = 300, bg = "white")

cat("Saved: fig_lisa_public_realm_polished.png\n")


# ---- Polished Bivariate LISA maps ----

make_polished_bivar <- function(data_key, title, subtitle) {
  data <- bivar_all[[data_key]]
  
  map_data <- corpus_analysis %>%
    select(lpa_number) %>%
    left_join(data, by = "lpa_number") %>%
    mutate(cluster_type = factor(cluster_type,
                                 levels = c("HH", "LL", "HL", "LH", "NS")))
  
  ggplot() +
    base_layers() +
    geom_sf(data = map_data, aes(fill = cluster_type),
            shape = 21, size = 4, color = "white",
            stroke = 0.8, alpha = 0.9) +
    scale_fill_manual(values = cluster_colors, name = "LISA cluster") +
    labs(title = title, subtitle = subtitle) +
    coord_sf(xlim = c(th_bbox$xmin - 200, th_bbox$xmax + 200),
             ylim = c(th_bbox$ymin - 200, th_bbox$ymax + 200)) +
    theme_void(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0),
      plot.subtitle = element_text(size = 9, color = "grey30", hjust = 0),
      legend.position = "right",
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

map1 <- make_polished_bivar(
  "public_realm × IMD (compensation)",
  "Public realm × IMD",
  "Compensation discourse ↔ neighborhood deprivation"
)
map2 <- make_polished_bivar(
  "high_density × canopy",
  "High-density × Canopy",
  "Density rhetoric ↔ green infrastructure"
)
map3 <- make_polished_bivar(
  "mixed_use × PTAL",
  "Mixed-use × PTAL",
  "Mixed-use narrative ↔ transport accessibility"
)


library(patchwork)

combined_polished <- (map1 + map2 + map3) +
  plot_annotation(
    title = "Bivariate Local Moran's I: Discourse × Spatial Context",
    subtitle = "59 major residential schemes in Tower Hamlets · 999 permutations · p < 0.05",
    theme = theme(
      plot.title = element_text(size = 15, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "grey30"),
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

ggsave(here(output_dir, "fig_bivariate_lisa_polished.png"),
       combined_polished, width = 18, height = 7, dpi = 300, bg = "white")

cat("Saved: fig_bivariate_lisa_polished.png\n")


# ---- Also polish GWR map ----

make_polished_gwr <- function(gwr_sf, title, subtitle) {
  ggplot() +
    base_layers() +
    geom_sf(data = gwr_sf, aes(fill = local_beta, shape = significant),
            size = 4, color = "white", stroke = 0.6, alpha = 0.9) +
    scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b",
                         midpoint = 0, name = "Local β") +
    scale_shape_manual(values = c("TRUE" = 21, "FALSE" = 22),
                       name = "p < 0.05",
                       labels = c("TRUE" = "Sig.", "FALSE" = "Not sig.")) +
    labs(title = title, subtitle = subtitle) +
    coord_sf(xlim = c(th_bbox$xmin - 200, th_bbox$xmax + 200),
             ylim = c(th_bbox$ymin - 200, th_bbox$ymax + 200)) +
    theme_void(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = 11, hjust = 0),
      plot.subtitle = element_text(size = 9, color = "grey30", hjust = 0),
      legend.position = "right",
      plot.background = element_rect(fill = "white", color = NA)
    )
}

gwr_map1 <- make_polished_gwr(gwr_public,
                              "Public realm ~ IMD",
                              "Global β = -0.041 · GWR R² = 0.20")
gwr_map2 <- make_polished_gwr(gwr_disp,
                              "Displacement ~ Distance to school",
                              "Global β = 0.001 · Spatially uniform")
gwr_map3 <- make_polished_gwr(gwr_dens,
                              "High-density ~ Building height",
                              "Global β = 0.086 · GWR R² = 0.47")

combined_gwr_polished <- (gwr_map1 + gwr_map2 + gwr_map3) +
  plot_annotation(
    title = "Geographically Weighted Regression: local coefficients",
    subtitle = "Adaptive bisquare kernel · 59 Tower Hamlets major residential schemes",
    theme = theme(
      plot.title = element_text(size = 15, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "grey30"),
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

ggsave(here(output_dir, "fig_gwr_polished.png"),
       combined_gwr_polished, width = 18, height = 7, dpi = 300, bg = "white")

cat("Saved: fig_gwr_polished.png\n")

cat("\n=== All 3 polished figures saved ===\n")
cat("- fig_lisa_public_realm_polished.png\n")
cat("- fig_bivariate_lisa_polished.png\n")
cat("- fig_gwr_polished.png\n")