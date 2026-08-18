# =============================================================================
# figures.R  —  Reproduces the figures and tables in the dissertation
# -----------------------------------------------------------------------------
# Usage:   source(here::here("scripts", "figures.R"))
# Run AFTER the main pipeline (01–04 / 05–24) has produced the files in output/.
#
# STATUS OF EACH BLOCK:
#   [READY]        verified code, should run as-is
#   [VERIFY]       reconstructed from the analysis; core is correct, but check
#                  that styling / filtering matches your original figure
#
# NOT in this script: the spatial figures (LISA / Moran / maps, Fig 4.4, 5.4,
#   5.5) and the validation figures (Fig 4.2, 4.3) are produced by their own
#   pipeline scripts (mainly 09_spatial_analysis.R, plus 01 / 15 / 20 / 21) and
#   are not duplicated here.
#
# Table 5.1 (stance excerpts) is generated separately as a PNG and is not R —
# keep that image file; it is not reproduced here.
# =============================================================================

library(tidyverse)
library(here)

DATA <- "output/corpus_59_full_spatial_framed.csv"   # main analysis table
dir.create("output", showWarnings = FALSE)


# =============================================================================
# Table 3.1 (a–d)  — data sources                                    [READY]
# =============================================================================
# needs: install.packages("gridExtra")
library(gridExtra); library(grid)

wrapw <- c(18, 22, 16, 12, 13, 24)
wrap_df <- function(df) {
  for (j in seq_along(df))
    df[[j]] <- vapply(as.character(df[[j]]),
                      function(s) paste(strwrap(s, wrapw[j]), collapse = "\n"),
                      character(1), USE.NAMES = FALSE)
  df
}
save_tbl <- function(df, file) {
  df <- wrap_df(df)
  th <- ttheme_minimal(
    base_size = 10, padding = unit(c(4.5, 4.5), "mm"),
    core    = list(fg_params = list(hjust = 0.5, x = 0.5),
                   bg_params = list(fill = c("white", "#EDF1F6"), col = "#C9D2DC", lwd = 0.8)),
    colhead = list(fg_params = list(hjust = 0.5, x = 0.5, col = "white", fontface = "bold"),
                   bg_params = list(fill = "#3D5A80", col = "#3D5A80")))
  g <- tableGrob(df, rows = NULL, theme = th)
  tmp <- tempfile(fileext = ".pdf"); pdf(tmp, 30, 30)
  w <- convertWidth(sum(g$widths),  "in", valueOnly = TRUE)
  h <- convertHeight(sum(g$heights), "in", valueOnly = TRUE)
  dev.off(); unlink(tmp)
  png(file, width = w + 0.4, height = h + 0.4, units = "in", res = 200, bg = "white")
  grid.draw(g); dev.off()
  cat("saved", file, sprintf("  (%.1f x %.1f in)\n", w, h))
}
hdr <- c("Dataset","Source","Vintage","Format","Spatial unit","Used for")
mk  <- function(v){ d <- as.data.frame(matrix(v, ncol = 6, byrow = TRUE),
                                       stringsAsFactors = FALSE); names(d) <- hdr; d }

save_tbl(mk(c(
  "Planning statements","Tower Hamlets planning portal","FY 2014/15-2025/26","PDF (77 files)","Per application","Developer discourse: frame and stance coding",
  "London Planning Database (PLD)","London Datastore","12 CSVs, FY 2014/15-2025/26","CSV","Per application","Sampling frame; proposed units, decision, type"
)), "output/table_3_1a_corpus.png")

save_tbl(mk(c(
  "LSOA 2021 boundaries","ONS / London Datastore","2021","Shapefile","LSOA polygon","Spatial join of projects; base map",
  "London wards","London Datastore (Statistical GIS)","2018","Shapefile","Ward polygon","Cartographic base map only",
  "LSOA 2011 to 2021 lookup","ONS (Exact-Fit Lookup, V3)","2011 to 2021","CSV","LSOA crosswalk","Join 2011-LSOA canopy to 2021-LSOA IMD"
)), "output/table_3_1b_boundaries.png")

save_tbl(mk(c(
  "IMD 2025","GOV.UK (MHCLG)","2025","CSV","LSOA 2021","Deprivation: score, decile, sub-domains",
  "PTAL","TfL / London Datastore","2015 grid","MapInfo TAB","100 m contour","Public transport accessibility level"
)), "output/table_3_1c_socioeconomic.png")

save_tbl(mk(c(
  "London stations","doogal.co.uk","Accessed 2026","CSV","Point","Nearest-station distance and zone",
  "Tree canopy %","GLA / London Datastore","Accessed 2026","CSV","LSOA","Green cover",
  "OS Open Greenspace","Ordnance Survey","Accessed 2026","Shapefile","Polygon","Open space area / % / count in 500 m",
  "Schools (EduBase)","GOV.UK (Get Information about Schools)","2026 export","CSV","Point","Nearest school; count in 500 m",
  "Bus stops","DfT Naptan","Accessed 2026","CSV","Point","Nearest stop; count in 500 m",
  "Cycling infrastructure","TfL Cycling Infrastructure Database","Accessed 2026","GeoJSON","Line + point","Nearest lane/parking; count in 500 m"
)), "output/table_3_1d_context.png")


# =============================================================================
# Figure 3.1  — sampling filter funnel                               [READY]
# =============================================================================
lv <- c("All approved major residential\nschemes (>=100 units)",
        "With retrievable planning statement\n(discourse-analysable)",
        "With complete spatial + outcome data\n(full analysis)")
fun <- tibble(stage = factor(lv, levels = rev(lv)),
              n    = c(74, 61, 59),
              note = c("", "-13", "-2"))
ggplot(fun, aes(n, stage)) +
  geom_col(width = .6, fill = "#3D5A80") +
  geom_text(aes(label = paste0("n = ", n)), hjust = -0.15, size = 4.3) +
  geom_text(aes(label = note), hjust = 1.1, colour = "grey35", size = 3.4) +
  scale_x_continuous(limits = c(0, 90), expand = c(0, 0)) +
  labs(x = "Number of schemes", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        axis.text.y = element_text(hjust = 0))
ggsave("output/fig_3_1_filter_funnel.png", width = 8, height = 3.6, dpi = 200, bg = "white")


# =============================================================================
# Figure 3.2  — scheme size distribution (units_proposed)            [READY]
# NOTE: uses units_proposed (PLD, authoritative), NOT dwellings_proposed_total.
# =============================================================================
u <- read_csv(DATA, show_col_types = FALSE) %>%
  transmute(units = as.numeric(units_proposed)) %>% filter(!is.na(units))
cat(sprintf("Fig 3.2: n = %d | median = %g | max = %g\n",
            nrow(u), median(u$units), max(u$units)))    # expect 58 / 440 / 3107
ggplot(u, aes(units)) +
  geom_histogram(bins = 22, fill = "#4E79A7", colour = "white") +
  scale_x_log10(breaks = c(100, 300, 1000, 3000), labels = scales::comma) +
  labs(title = "Distribution of scheme size (units proposed)",
       subtitle = paste0(nrow(u), " Tower Hamlets schemes of 100+ units, FY 2014/15-2025/26"),
       x = "Units proposed (log scale)", y = "Number of schemes") +
  theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank())
ggsave("output/fig_3_2_size.png", width = 8, height = 5, dpi = 200, bg = "white")


# =============================================================================
# Figure 5.1  — stance distribution by narrative                     [VERIFY]
#   Reconstructed from output/llm_contextual_coding_corpus.csv (keyword, dominant_code)
# =============================================================================
cc <- read_csv("output/llm_contextual_coding_corpus.csv", show_col_types = FALSE) %>%
  mutate(stance = str_to_upper(dominant_code),
         stance = if_else(str_detect(stance, "^OFFSET"), "OFFSETTING", stance)) %>%
  filter(stance %in% c("ASSERTIVE", "AVOIDANT", "OFFSETTING", "TECHNICAL"))
stance_prop <- cc %>% count(keyword, stance) %>%
  group_by(keyword) %>% mutate(p = n / sum(n)) %>% ungroup() %>%
  mutate(stance = factor(str_to_title(stance),
                         levels = c("Assertive","Offsetting","Technical","Avoidant")))
ggplot(stance_prop, aes(p, fct_reorder(keyword, p * (stance == "Assertive"), .fun = sum),
                        fill = stance)) +
  geom_col(width = .72) +
  scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
  scale_fill_manual(values = c(Assertive = "#3D5A80", Offsetting = "#9DB4C0",
                               Technical = "#C9D2DC", Avoidant = "#B0413E"), name = NULL) +
  labs(x = "Share of coded mentions", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "top")
ggsave("output/fig_5_1_stance.png", width = 8.5, height = 5, dpi = 200, bg = "white")


# =============================================================================
# Figure 5.2  — social vs economic framing per scheme                [VERIFY]
# =============================================================================
fr <- read_csv(DATA, show_col_types = FALSE) %>%
  transmute(lpa_number,
            economic = as.numeric(economic_pct),
            social   = as.numeric(social_pct)) %>%
  filter(!is.na(economic), !is.na(social))
ggplot(fr, aes(economic, social)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey70", linetype = "dashed") +
  geom_point(size = 2.6, colour = "#3D5A80", alpha = 0.85) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = "Economic framing share", y = "Social framing share") +
  theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank())
ggsave("output/fig_5_2_framing.png", width = 6, height = 6, dpi = 200, bg = "white")
# (points all above the diagonal => social dominates in every scheme)


# =============================================================================
# Figure 5.3  — economic framing vs accessibility (PTAL / distance)  [VERIFY]
# =============================================================================
ac <- read_csv(DATA, show_col_types = FALSE) %>%
  transmute(economic = as.numeric(economic_pct),
            PTAL     = as.numeric(PTAL_numeric),
            dist     = as.numeric(dist_to_station_m))
p_ptal <- ggplot(filter(ac, !is.na(PTAL)), aes(PTAL, economic)) +
  geom_point(size = 2.4, colour = "#3D5A80", alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, colour = "#B0413E") +
  labs(x = "PTAL", y = "Economic framing share") +
  theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank())
p_dist <- ggplot(filter(ac, !is.na(dist)), aes(dist, economic)) +
  geom_point(size = 2.4, colour = "#3D5A80", alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, colour = "#B0413E") +
  labs(x = "Distance to nearest station (m)", y = NULL) +
  theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank())
gridExtra::grid.arrange(p_ptal, p_dist, nrow = 1) -> g53
ggsave("output/fig_5_3_accessibility.png", g53, width = 9, height = 4.2, dpi = 200, bg = "white")


# =============================================================================
# Figure 5.6  — correspondence coefficients                          [READY]
#   raw / length-normalised / size-controlled(units_proposed)
# =============================================================================
pdf_texts <- readRDS("output/pdf_extracted_text_corpus.rds")
wc <- tibble(key = names(pdf_texts),
             words = map_dbl(pdf_texts, ~ str_count(paste(.x, collapse = " "), "\\S+")))
sp <- read_csv(DATA, show_col_types = FALSE)
breeam <- c("Pass" = 1, "Good" = 2, "Very Good" = 3, "Excellent" = 4, "Outstanding" = 5)
sp$breeam_num <- breeam[str_trim(sp$breeam_rating_target)]
norm <- function(x) str_replace_all(str_to_upper(x), "[^A-Z0-9]", "")
wc$k <- norm(wc$key); sp$k <- norm(sp$lpa_number)
d <- left_join(sp, wc %>% select(k, words), by = "k")
size <- as.numeric(d$units_proposed)
psp <- function(x, y, z){ ok <- complete.cases(x, y, z)
x <- rank(x[ok]); y <- rank(y[ok]); z <- rank(z[ok])
cor(resid(lm(x ~ z)), resid(lm(y ~ z))) }
pairs <- tribble(~label, ~narr, ~outcome,
                 "High density -> building height",    "high_density",  "building_height_max_storeys",
                 "Public realm -> public-realm area",  "public_realm",  "public_realm_area_sqm",
                 "Mixed use -> commercial floorspace", "mixed_use",     "commercial_floorspace_sqm",
                 "Affordable -> affordable %",         "affordable",    "affordable_percentage",
                 "Community -> community facilities",  "community",     "community_facilities_count",
                 "Sustainability -> BREEAM",           "sustainability","breeam_num")
res <- pairs %>% rowwise() %>% mutate(
  Raw = cor(d[[narr]], d[[outcome]], method = "spearman", use = "complete.obs"),
  `Length-normalised` = cor(d[[narr]] / (d$words / 10000), d[[outcome]],
                            method = "spearman", use = "complete.obs"),
  `Size-controlled` = psp(d[[narr]], d[[outcome]], size)) %>% ungroup()
plotd <- res %>%
  pivot_longer(c(Raw, `Length-normalised`, `Size-controlled`),
               names_to = "spec", values_to = "rho") %>%
  mutate(spec  = factor(spec, c("Raw", "Length-normalised", "Size-controlled")),
         label = factor(label, levels = rev(pairs$label)))
ggplot(plotd, aes(rho, label, colour = spec)) +
  geom_vline(xintercept = 0, colour = "grey70") +
  geom_point(size = 3.2, alpha = 0.9, position = position_dodge(width = 0.55)) +
  scale_colour_manual(values = c(Raw = "#B0413E", `Length-normalised` = "#9DB4C0",
                                 `Size-controlled` = "#3D5A80"), name = NULL) +
  scale_x_continuous(limits = c(-0.35, 0.6), breaks = seq(-0.2, 0.6, 0.2)) +
  labs(x = "Spearman's rho", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "top")
ggsave("output/fig_5_6_coefficients.png", width = 8.6, height = 4.8, dpi = 300, bg = "white")
print(res)   # prints the coefficient table used in 5.4


# =============================================================================
# Figure 5.7  — the three surviving pairings (scatter panels)        [VERIFY]
#   high density->height, mixed use->commercial, affordable->affordable%
# =============================================================================
sc <- read_csv(DATA, show_col_types = FALSE)
panel <- function(xvar, yvar, xlab, ylab, rho){
  ggplot(sc, aes(.data[[xvar]], as.numeric(.data[[yvar]]))) +
    geom_point(size = 2.4, colour = "#3D5A80", alpha = 0.8) +
    geom_smooth(method = "lm", se = FALSE, colour = "#B0413E") +
    labs(x = xlab, y = ylab, subtitle = paste0("size-controlled rho = ", rho)) +
    theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank())
}
g57 <- gridExtra::arrangeGrob(
  panel("high_density", "building_height_max_storeys", "High-density emphasis", "Building height", "0.30"),
  panel("mixed_use",    "commercial_floorspace_sqm",   "Mixed-use emphasis",    "Commercial floorspace", "0.33"),
  panel("affordable",   "affordable_percentage",       "Affordable emphasis",   "Affordable %", "0.51"),
  nrow = 1)
ggsave("output/fig_5_7_survivors.png", g57, width = 12, height = 4, dpi = 200, bg = "white")


cat("\n========== figures.R complete ==========\n")
cat("Filled: Table 3.1, Fig 3.1, 3.2, 5.1, 5.2, 5.3, 5.6, 5.7\n")
cat("Spatial + validation figures (4.2, 4.3, 4.4, 5.4, 5.5) come from the pipeline scripts.\n")
cat("Table 5.1 is a separately-generated PNG (not R).\n")