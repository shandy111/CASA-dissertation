# =============================================================
# 04_pairing_analysis.R
# =============================================================
# Narrative × Indicator pairing analysis + visualisations
#
# Input:  output/llm_keyword_matrix.csv
#         output/llm_indicators_v2.csv
#         output/llm_specific_patterns.csv
#         output/sub_sample_with_ptal.csv
# Output: output/master_pairing_data.csv
#         output/ptal_connectivity_pairing.csv
#         output/fig16_1_regen_density.png
#         output/fig16_2_regen_affordable.png
#         output/fig16_3_aligned_pairings.png
#         output/fig16_4_correlation_heatmap.png
# =============================================================

source(here::here("scripts", "00_setup.R"))
library(ggrepel)
library(patchwork)


# ---- STAGE 15: MASTER DATAFRAME + CORE PAIRINGS ----

keyword_matrix <- read_csv(here(output_dir, "llm_keyword_matrix.csv"),
                           show_col_types = FALSE)
indicators <- read_csv(here(output_dir, "llm_indicators_v2.csv"),
                       show_col_types = FALSE)
patterns <- read_csv(here(output_dir, "llm_specific_patterns.csv"),
                     show_col_types = FALSE)

# PTAL: strip suffix for join
ptal <- read_csv(here(output_dir, "sub_sample_with_ptal.csv"),
                 show_col_types = FALSE) %>%
  mutate(base_lpa = str_replace(lpa_number, "/[A-Z][A-Z0-9]*$", "")) %>%
  select(base_lpa, PTAL, precision) %>%
  distinct()

master <- keyword_matrix %>%
  left_join(indicators, by = "lpa_number") %>%
  left_join(ptal, by = c("lpa_number" = "base_lpa")) %>%
  left_join(patterns, by = "lpa_number") %>%
  mutate(
    density_dph = dwellings_proposed_total / site_area_hectares,
    PTAL_numeric = case_when(
      PTAL == "2" ~ 2, PTAL == "3" ~ 3, PTAL == "4" ~ 4,
      PTAL == "5" ~ 5, PTAL == "6a" ~ 6, PTAL == "6b" ~ 6.5,
      TRUE ~ NA_real_
    )
  )

cat("Master dataframe:", nrow(master), "rows,", ncol(master), "cols\n")

# ---- Core pairings ----
compute_cor <- function(x, y, method = "spearman") {
  suppressWarnings(cor(x, y, use = "pairwise.complete.obs", method = method))
}

cat("\n=== Core narrative-indicator pairings ===\n")
cat(sprintf("Regeneration × Density        ρ = %.3f\n",
            compute_cor(master$regeneration, master$density_dph)))
cat(sprintf("Regeneration × Affordable %%   ρ = %.3f\n",
            compute_cor(master$regeneration, master$affordable_percentage)))
cat(sprintf("Community × Public realm      ρ = %.3f\n",
            compute_cor(master$community, master$public_realm_area_sqm)))
cat(sprintf("Connectivity × PTAL           ρ = %.3f\n",
            compute_cor(master$connectivity, master$PTAL_numeric)))
cat(sprintf("High-density × Density        ρ = %.3f\n",
            compute_cor(master$high_density, master$density_dph)))
cat(sprintf("Sustainability × Affordable %% ρ = %.3f\n",
            compute_cor(master$sustainability, master$affordable_percentage)))
cat(sprintf("Heritage × Site area          ρ = %.3f\n",
            compute_cor(master$heritage, master$site_area_hectares)))

write_csv(master, here(output_dir, "master_pairing_data.csv"))


# ---- STAGE 12.6/12.7: PTAL × Connectivity typology ----

pairing_data <- master %>%
  filter(!is.na(PTAL_numeric))

median_ptal <- median(pairing_data$PTAL_numeric, na.rm = TRUE)
median_conn <- median(pairing_data$connectivity, na.rm = TRUE)

pairing_data <- pairing_data %>%
  mutate(
    ptal_high = PTAL_numeric > median_ptal,
    conn_high = connectivity > median_conn,
    quadrant = case_when(
      ptal_high & conn_high    ~ "Q1: Aligned high",
      ptal_high & !conn_high   ~ "Q2: Understated",
      !ptal_high & conn_high   ~ "Q3: Overstated",
      !ptal_high & !conn_high  ~ "Q4: Aligned low"
    )
  )

cat("\n=== PTAL × Connectivity typology ===\n")
pairing_data %>% count(quadrant) %>% print()

pairing_data %>%
  select(lpa_number, PTAL, PTAL_numeric, precision,
         connectivity, community, regeneration, public_realm, quadrant) %>%
  write_csv(here(output_dir, "ptal_connectivity_pairing.csv"))


# ---- STAGE 16: VISUALISATIONS ----

theme_diss <- theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(color = "grey40", size = 10),
        plot.caption = element_text(color = "grey50", size = 8, hjust = 0),
        panel.grid.minor = element_blank())


# Fig 1: Regeneration × Density
fig1 <- master %>%
  filter(!is.na(density_dph), !is.na(regeneration)) %>%
  ggplot(aes(x = density_dph, y = regeneration)) +
  geom_smooth(method = "lm", se = TRUE, color = "grey60",
              linetype = "dashed", linewidth = 0.6) +
  geom_point(size = 3, alpha = 0.7, color = "#c0392b") +
  geom_text_repel(aes(label = str_extract(lpa_number, "\\d{2}/\\d{5}")),
                  size = 3, color = "grey30",
                  box.padding = 0.4, max.overlaps = 15) +
  scale_x_continuous(labels = label_comma()) +
  labs(title = "Regeneration narrative vs proposed density",
       subtitle = "Spearman ρ = -0.375 — higher-density schemes use less 'regeneration' language",
       x = "Proposed density (dwellings per hectare)",
       y = "'Regeneration' keyword frequency") +
  theme_diss

ggsave(here(output_dir, "fig16_1_regen_density.png"), fig1,
       width = 8, height = 5, dpi = 300, bg = "white")


# Fig 2: Regeneration × Affordable
fig2 <- master %>%
  filter(!is.na(affordable_percentage), !is.na(regeneration)) %>%
  ggplot(aes(x = affordable_percentage, y = regeneration)) +
  geom_smooth(method = "lm", se = TRUE, color = "grey60",
              linetype = "dashed", linewidth = 0.6) +
  geom_point(size = 3, alpha = 0.7, color = "#2c7a4b") +
  geom_text_repel(aes(label = str_extract(lpa_number, "\\d{2}/\\d{5}")),
                  size = 3, color = "grey30",
                  box.padding = 0.4, max.overlaps = 15) +
  labs(title = "Regeneration narrative vs affordable housing %",
       subtitle = "Spearman ρ = 0.165 — near-zero (challenges direct application of Watt 2013)",
       x = "Affordable housing (%)",
       y = "'Regeneration' keyword frequency") +
  theme_diss

ggsave(here(output_dir, "fig16_2_regen_affordable.png"), fig2,
       width = 8, height = 5, dpi = 300, bg = "white")


# Fig 3: Community × PR + Connectivity × PTAL side by side
fig3a <- master %>%
  filter(!is.na(public_realm_area_sqm), !is.na(community)) %>%
  ggplot(aes(x = public_realm_area_sqm, y = community)) +
  geom_smooth(method = "lm", se = TRUE, color = "grey60",
              linetype = "dashed", linewidth = 0.6) +
  geom_point(size = 3, alpha = 0.7, color = "#2d5a8f") +
  geom_text_repel(aes(label = str_extract(lpa_number, "\\d{2}/\\d{5}")),
                  size = 3, color = "grey30",
                  box.padding = 0.4, max.overlaps = 15) +
  scale_x_continuous(labels = label_comma()) +
  labs(title = "Community × Public realm",
       subtitle = "Spearman ρ = 0.366",
       x = "Public realm area (sqm)",
       y = "'Community' frequency") +
  theme_diss

fig3b <- master %>%
  filter(!is.na(PTAL_numeric), !is.na(connectivity)) %>%
  ggplot(aes(x = PTAL_numeric, y = connectivity)) +
  geom_smooth(method = "lm", se = TRUE, color = "grey60",
              linetype = "dashed", linewidth = 0.6) +
  geom_point(size = 3, alpha = 0.7, color = "#8b4a99") +
  geom_text_repel(aes(label = str_extract(lpa_number, "\\d{2}/\\d{5}")),
                  size = 3, color = "grey30",
                  box.padding = 0.4, max.overlaps = 15) +
  scale_x_continuous(breaks = 2:6, labels = c("2", "3", "4", "5", "6a")) +
  labs(title = "Connectivity × PTAL",
       subtitle = "Spearman ρ = 0.331",
       x = "PTAL",
       y = "'Connectivity' frequency") +
  theme_diss

fig3_combined <- (fig3a | fig3b) +
  plot_annotation(
    title = "Aligned pairings: community-realm and connectivity-PTAL",
    theme = theme(plot.title = element_text(face = "bold", size = 13))
  )

ggsave(here(output_dir, "fig16_3_aligned_pairings.png"), fig3_combined,
       width = 12, height = 5, dpi = 300, bg = "white")


# Fig 4: Correlation heatmap
narrative_cols <- c("regeneration", "redevelopment", "mixed_use", "community",
                    "sustainability", "connectivity", "public_realm",
                    "affordable", "high_density", "heritage")
indicator_cols <- c("density_dph", "building_height_max_storeys",
                    "affordable_percentage", "public_realm_area_sqm",
                    "PTAL_numeric", "community_facilities_count",
                    "dwellings_proposed_total", "site_area_hectares")

cor_data <- map_dfr(narrative_cols, function(n) {
  map_dfr(indicator_cols, function(i) {
    r <- suppressWarnings(cor(master[[n]], master[[i]],
                              use = "pairwise.complete.obs",
                              method = "spearman"))
    tibble(narrative = n, indicator = i, spearman = r)
  })
})

narrative_order <- c("regeneration", "redevelopment", "community",
                     "public_realm", "connectivity", "affordable",
                     "high_density", "sustainability", "mixed_use", "heritage")
indicator_order <- c("density_dph", "dwellings_proposed_total",
                     "building_height_max_storeys", "site_area_hectares",
                     "affordable_percentage", "public_realm_area_sqm",
                     "PTAL_numeric", "community_facilities_count")

fig4 <- cor_data %>%
  mutate(narrative = factor(narrative, levels = rev(narrative_order)),
         indicator = factor(indicator, levels = indicator_order),
         label = ifelse(is.na(spearman), "", sprintf("%.2f", spearman))) %>%
  ggplot(aes(x = indicator, y = narrative, fill = spearman)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = label), size = 3.2, color = "black") +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b",
                       midpoint = 0, limits = c(-1, 1),
                       na.value = "grey90", name = "Spearman ρ") +
  scale_x_discrete(labels = c(
    density_dph = "Density\n(dph)",
    dwellings_proposed_total = "Dwellings\ntotal",
    building_height_max_storeys = "Height\n(storeys)",
    site_area_hectares = "Site area\n(ha)",
    affordable_percentage = "Affordable\n%",
    public_realm_area_sqm = "Public\nrealm (sqm)",
    PTAL_numeric = "PTAL",
    community_facilities_count = "Community\nfacilities"
  )) +
  labs(title = "Narrative × Indicator correlation matrix",
       subtitle = "Spearman ρ across 14 sub-sample schemes",
       x = NULL, y = NULL,
       caption = "Blue = negative (mismatch). Red = positive (alignment). Grey = insufficient data.") +
  theme_diss +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 10),
        legend.position = "right")

ggsave(here(output_dir, "fig16_4_correlation_heatmap.png"), fig4,
       width = 11, height = 7, dpi = 300, bg = "white")

cat("\n=== 04_pairing_analysis.R DONE ===\n")
cat("4 figures saved to output/\n")