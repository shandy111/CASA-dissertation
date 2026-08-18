# =============================================================
# 01_pld_pipeline.R
# =============================================================
# Input:  data/pld_raw/*.csv
# Output: output/all_data_cleaned.csv
#         output/matrix_100plus_by_borough_year.csv
#         output/all_100plus_schemes.csv
#         output/th_74_typed.csv
#         output/th_74_indicators.csv
#         output/tower_hamlets_narrative_flags.csv
#         output/keyword_frequency_summary.csv
#         output/fig_heatmap_borough_year.png
#         output/fig_borough_totals.png
#         output/fig_yearly_trend.png
#         output/fig_scheme_size_dist.png
# =============================================================

source(here::here("scripts", "00_setup.R"))


# ---- STAGE 1: DATA LOADING ----

csv_files <- dir_ls(data_dir, glob = "*.csv")
cat("Found", length(csv_files), "CSV files\n")

read_pld <- function(file_path) {
  year <- str_extract(basename(file_path), "20\\d{2}") %>% as.integer()
  read_csv(file_path,
           col_types = cols(.default = "c"),
           show_col_types = FALSE) %>%
    clean_names() %>%
    mutate(financial_year = year, .before = 1)
}

all_data <- map_dfr(csv_files, read_pld)

all_data <- all_data %>%
  mutate(
    units_proposed = as.numeric(units_proposed),
    units_lost     = as.numeric(units_lost),
    net_units      = as.numeric(net_units),
    total_number_of_proposed_residential_units =
      as.numeric(total_number_of_proposed_residential_units),
    total_number_of_existing_residential_units =
      as.numeric(total_number_of_existing_residential_units)
  )

cat("Merged rows:", nrow(all_data), "\n")


# ---- STAGE 2: BOROUGH STANDARDISATION ----

borough_map <- c(
  "London Borough of Barnet"                 = "Barnet",
  "London Borough of Bexley"                 = "Bexley",
  "London Borough of Barking and Dagenham"   = "Barking and Dagenham",
  "Barking & Dagenham"                       = "Barking and Dagenham",
  "LB Bromley"                               = "Bromley",
  "Bromley Custodian Code"                   = "Bromley",
  "London Borough of Croydon"                = "Croydon",
  "Croydon SLA Code"                         = "Croydon",
  "Enfield Council"                          = "Enfield",
  "London Borough of Hammersmith and Fulham" = "Hammersmith and Fulham",
  "Hammersmith & Fulham"                     = "Hammersmith and Fulham",
  "Kensington & Chelsea"                     = "Kensington and Chelsea",
  "Royal Borough of Kingston (LA Code)"      = "Kingston upon Thames",
  "Kingston"                                 = "Kingston upon Thames",
  "London Borough of Lambeth"                = "Lambeth",
  "London Borough of Newham"                 = "Newham",
  "London Borough of Southwark"              = "Southwark",
  "London Borough of Sutton"                 = "Sutton",
  "Richmond"                                 = "Richmond upon Thames"
)

invalid_boroughs <- c("Out of Borough", "Default LA Code", "Custodian code", NA)

all_data <- all_data %>%
  mutate(borough = recode(borough, !!!borough_map)) %>%
  filter(!borough %in% invalid_boroughs)

cat("Cleaned boroughs:", n_distinct(all_data$borough), "\n")

write_csv(all_data, here(output_dir, "all_data_cleaned.csv"))


# ---- STAGE 3: LONDON-WIDE CORPUS ----

large_schemes <- all_data %>% filter(units_proposed >= 100)

cat("100+ unit schemes London-wide:", nrow(large_schemes), "\n")

matrix_100plus <- large_schemes %>%
  count(borough, financial_year) %>%
  pivot_wider(names_from = financial_year, values_from = n, values_fill = 0) %>%
  mutate(total = rowSums(across(where(is.numeric)))) %>%
  arrange(desc(total))

write_csv(matrix_100plus, here(output_dir, "matrix_100plus_by_borough_year.csv"))
write_csv(large_schemes,  here(output_dir, "all_100plus_schemes.csv"))

# Fig 1: Heatmap
heatmap_data <- large_schemes %>%
  count(borough, financial_year) %>%
  group_by(borough) %>%
  mutate(borough_total = sum(n)) %>%
  ungroup() %>%
  mutate(borough = fct_reorder(borough, borough_total)) %>%
  complete(borough, financial_year, fill = list(n = 0))

p1 <- ggplot(heatmap_data, aes(x = factor(financial_year), y = borough, fill = n)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(n == 0, "", n)), size = 3) +
  scale_fill_gradient(low = "#f7f7f7", high = "#08306b", name = "Schemes") +
  labs(title = "Major Residential Applications (100+ units) by London Borough, 2014-2025",
       subtitle = "Source: Planning London Datahub (PLD), Financial Year basis",
       x = "Financial Year", y = NULL,
       caption = "Note: FY 2023 data appears incomplete in PLD.") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 13),
        panel.grid = element_blank())

ggsave(here(output_dir, "fig_heatmap_borough_year.png"), p1,
       width = 11, height = 9, dpi = 300, bg = "white")

# Fig 2: Borough bar chart
borough_totals <- large_schemes %>%
  count(borough, sort = TRUE) %>%
  mutate(borough = fct_reorder(borough, n),
         is_top = borough == "Tower Hamlets")

p2 <- ggplot(borough_totals, aes(x = borough, y = n, fill = is_top)) +
  geom_col() +
  geom_text(aes(label = n), hjust = -0.3, size = 3.3) +
  scale_fill_manual(values = c("TRUE" = "#c0392b", "FALSE" = "#3498db"), guide = "none") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Total Major Residential Applications (100+ units), 2014-2025",
       subtitle = "Tower Hamlets leads with 74 schemes",
       x = NULL, y = "Number of approved schemes",
       caption = "Source: Planning London Datahub (PLD)") +
  theme_minimal(base_size = 11)

ggsave(here(output_dir, "fig_borough_totals.png"), p2,
       width = 9, height = 9, dpi = 300, bg = "white")

# Fig 3: Yearly trend
yearly_trend <- large_schemes %>% count(financial_year)

p3 <- ggplot(yearly_trend, aes(x = financial_year, y = n)) +
  geom_line(color = "#2c3e50", linewidth = 1) +
  geom_point(color = "#2c3e50", size = 3) +
  geom_text(aes(label = n), vjust = -1.2, size = 3.3) +
  scale_x_continuous(breaks = 2014:2025) +
  annotate("rect", xmin = 2022.7, xmax = 2023.3,
           ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.1) +
  annotate("text", x = 2023, y = 50, label = "FY 2023 data\nappears incomplete",
           size = 3.2, color = "red", fontface = "italic") +
  labs(title = "Major Residential Applications by FY, London 2014-2025",
       subtitle = "Sharp dip in FY 2023 likely reflects PLD data completeness",
       x = "Financial Year", y = "Number of approved 100+ unit schemes",
       caption = "Source: Planning London Datahub (PLD)") +
  theme_minimal(base_size = 11)

ggsave(here(output_dir, "fig_yearly_trend.png"), p3,
       width = 11, height = 6, dpi = 300, bg = "white")


# ---- STAGE 4: TOWER HAMLETS CORPUS ----

th <- all_data %>%
  filter(borough == "Tower Hamlets", units_proposed >= 100)

cat("Tower Hamlets 100+ schemes:", nrow(th), "\n")


# ---- STAGE 5: PLD-LEVEL KEYWORD SCAN ----

keyword_groups <- list(
  regeneration   = c("regeneration", "regenerate", "renewal"),
  redevelopment  = c("redevelopment", "redevelop", "comprehensive redevelopment"),
  mixed_use      = c("mixed-use", "mixed use"),
  community      = c("community", "community benefit", "neighbourhood"),
  sustainability = c("sustainability", "sustainable", "net zero", "carbon"),
  connectivity   = c("connectivity", "connections", "permeability", "pedestrian", "walkable"),
  public_realm   = c("public realm", "public space", "public square", "plaza"),
  affordable     = c("affordable", "affordable housing", "social rent"),
  placemaking    = c("placemaking", "place-making", "sense of place"),
  high_density   = c("high density", "high-density", "tall building", "tower")
)

th_corpus <- th %>%
  select(financial_year, lpa_number, units_proposed, description) %>%
  mutate(description_lower = str_to_lower(description))

keyword_counts <- map_dfr(names(keyword_groups), function(group_name) {
  patterns <- keyword_groups[[group_name]]
  n_hits <- sum(str_detect(th_corpus$description_lower,
                           paste(patterns, collapse = "|")), na.rm = TRUE)
  tibble(narrative_group = group_name,
         keywords = paste(patterns, collapse = ", "),
         n_projects = n_hits,
         pct_of_corpus = round(n_hits / nrow(th_corpus) * 100, 1))
}) %>% arrange(desc(n_projects))

write_csv(keyword_counts, here(output_dir, "keyword_frequency_summary.csv"))

th_flags <- th_corpus %>%
  mutate(
    has_regeneration   = str_detect(description_lower, "regeneration|regenerate|renewal"),
    has_mixed_use      = str_detect(description_lower, "mixed-use|mixed use"),
    has_community      = str_detect(description_lower, "community|neighbourhood"),
    has_sustainability = str_detect(description_lower, "sustainability|sustainable|net zero|carbon"),
    has_connectivity   = str_detect(description_lower, "connectivity|connections|permeability|pedestrian|walkable"),
    has_public_realm   = str_detect(description_lower, "public realm|public space|public square"),
    has_affordable     = str_detect(description_lower, "affordable")
  ) %>%
  mutate(narrative_count = has_regeneration + has_mixed_use + has_community +
           has_sustainability + has_connectivity +
           has_public_realm + has_affordable)

write_csv(th_flags, here(output_dir, "tower_hamlets_narrative_flags.csv"))


# ---- STAGE 6: SCHEME TYPE CLASSIFICATION ----

th_typed <- th %>%
  mutate(
    lpa_suffix = str_extract(lpa_number, "/[A-Z0-9]+$"),
    lpa_suffix = str_replace(lpa_suffix, "^/", ""),
    scheme_type = case_when(
      str_detect(lpa_suffix, "^(OUT|O)$")  ~ "Outline",
      str_detect(lpa_suffix, "^(FUL|F)$")  ~ "Full",
      str_detect(lpa_suffix, "^(P\\d+)$")  ~ "Post-decision amendment",
      str_detect(lpa_suffix, "^(A\\d+)$")  ~ "Non-material amendment",
      str_detect(lpa_suffix, "^S$")        ~ "Sub / condition submission",
      str_detect(lpa_suffix, "^(RES|R)$")  ~ "Reserved matters",
      TRUE                                 ~ paste0("Other: ", lpa_suffix)
    )
  )

write_csv(th_typed, here(output_dir, "th_74_typed.csv"))


# ---- STAGE 7: PLD-DERIVED INDICATORS ----

th_indicators <- th_typed %>%
  mutate(
    total_units          = units_proposed,
    net_change           = net_units,
    replacement_ratio    = ifelse(units_lost > 0, units_lost / units_proposed, NA_real_),
    involves_demolition  = units_lost > 0
  )

th_indicators %>%
  select(financial_year, lpa_number, site_name, street_name, postcode,
         scheme_type, total_units, units_lost, net_change,
         replacement_ratio, involves_demolition) %>%
  write_csv(here(output_dir, "th_74_indicators.csv"))

p_size <- ggplot(th_indicators, aes(x = total_units)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  scale_x_log10(labels = label_comma()) +
  labs(title = "Distribution of scheme size (units proposed)",
       subtitle = "74 Tower Hamlets schemes of 100+ units, FY 2014-2025",
       x = "Units proposed (log scale)", y = "Number of schemes") +
  theme_minimal()

ggsave(here(output_dir, "fig_scheme_size_dist.png"), p_size,
       width = 8, height = 5, dpi = 150)

cat("\n=== 01_pld_pipeline.R DONE ===\n")
