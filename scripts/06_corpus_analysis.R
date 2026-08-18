# =============================================================
# 06_corpus_analysis.R
# =============================================================
# Parse Round 2 indicators for 59 corpus + build corpus master
# dataframe + prep for pairing analysis
#
# Input:
#   output/llm_results_v2_corpus_raw.rds      (from 03, Stage 20)
#   output/llm_keyword_matrix_corpus.csv      (from 03, Stage 19)
#   output/llm_specific_patterns_corpus.csv   (from 03, Stage 19)
#
# Output:
#   output/llm_indicators_v2_corpus.csv       (parsed indicators)
#   output/master_pairing_data_corpus.csv     (integrated data)
# =============================================================

source(here::here("scripts", "00_setup.R"))


# ---- Stage 21: Parse Round 2 indicators (corpus) ----

results_v2_corpus <- readRDS(here(output_dir, "llm_results_v2_corpus_raw.rds"))

parse_v2_corpus <- function(res) {
  if (res$status != "OK") return(NULL)
  tryCatch(fromJSON(res$output, simplifyVector = FALSE),
           error = function(e) NULL)
}

parsed_v2_corpus <- map(results_v2_corpus, parse_v2_corpus)

cat("Parsed Round 2 (corpus):", 
    sum(!map_lgl(parsed_v2_corpus, is.null)), "/", 
    length(parsed_v2_corpus), "\n")


# ---- Extract wide indicators dataframe ----

get_value <- function(indicator_data, field_name) {
  x <- indicator_data[[field_name]]
  if (is.null(x)) return(NA)
  val <- x$value
  if (is.null(val)) return(NA)
  val
}

indicator_fields <- c(
  "site_area_hectares", "dwellings_proposed_total",
  "building_height_max_storeys", "building_height_max_metres",
  "affordable_units_total", "affordable_percentage", "affordable_basis",
  "tenure_social_rent_pct", "tenure_intermediate_pct",
  "public_realm_area_sqm", "breeam_rating_target",
  "sustainability_carbon_reduction_pct", "commercial_floorspace_sqm"
)

numeric_fields <- setdiff(indicator_fields, 
                          c("affordable_basis", "breeam_rating_target"))

indicators_wide_corpus <- map_dfr(names(parsed_v2_corpus), function(lpa) {
  p <- parsed_v2_corpus[[lpa]]
  if (is.null(p)) return(NULL)
  
  row <- tibble(lpa_number = lpa)
  for (field in indicator_fields) {
    val <- get_value(p, field)
    if (is.list(val)) val <- NA
    if (field %in% numeric_fields) {
      val <- suppressWarnings(as.numeric(val))
    } else {
      val <- as.character(val)
    }
    row[[field]] <- val
  }
  
  cf <- p$community_facilities_provided
  if (!is.null(cf) && !is.null(cf$value) && length(cf$value) > 0) {
    row$community_facilities_count <- length(cf$value)
    row$community_facilities_list <- paste(unlist(cf$value), collapse = "; ")
  } else {
    row$community_facilities_count <- NA_integer_
    row$community_facilities_list <- NA_character_
  }
  row
})

cat("\nIndicators wide dataframe:", nrow(indicators_wide_corpus), "rows,",
    ncol(indicators_wide_corpus), "cols\n")

write_csv(indicators_wide_corpus, 
          here(output_dir, "llm_indicators_v2_corpus.csv"))


# ---- Report data completeness ----

cat("\n=== Data completeness per indicator (59 corpus) ===\n")
indicators_wide_corpus %>%
  summarise(across(-lpa_number, ~ sum(!is.na(.)))) %>%
  pivot_longer(everything(), names_to = "indicator", values_to = "n_available") %>%
  mutate(pct = round(n_available / nrow(indicators_wide_corpus) * 100, 1)) %>%
  arrange(desc(n_available)) %>%
  print(n = Inf)
# 递归找匹配的 field
find_indicator <- function(obj, keyword_pattern) {
  if (is.null(obj)) return(NA)
  
  if (is.list(obj)) {
    nms <- names(obj)
    if (!is.null(nms)) {
      # 直接匹配当前层的 field 名
      for (nm in nms) {
        if (str_detect(str_to_lower(nm), keyword_pattern)) {
          val <- obj[[nm]]
          # 处理 {value, source_quote} 结构
          if (is.list(val) && !is.null(val$value)) {
            return(val$value)
          }
          # 直接是 value(比如 flat schema)
          if (!is.list(val)) return(val)
          # 是 list 但没 value 字段,取第一个 non-list 子元素
          for (v in val) {
            if (!is.list(v)) return(v)
          }
        }
      }
    }
    # 递归子对象
    for (child in obj) {
      result <- find_indicator(child, keyword_pattern)
      if (!is.na(result)[1] && length(result) > 0) return(result)
    }
  }
  return(NA)
}

# 提取每个 indicator
indicator_patterns <- list(
  site_area_hectares          = "site_area|site.area.hectares",
  dwellings_proposed_total    = "dwellings_proposed|dwellings.proposed|residential_units",
  building_height_max_storeys = "height_max_storeys|building_height.*storeys|max_building_height_storeys",
  building_height_max_metres  = "height_max_metres|building_height.*metres|max_building_height_metres",
  affordable_units_total      = "affordable_units_total|affordable.units.total",
  affordable_percentage       = "affordable_percentage|affordable.*pct|affordable.*percent",
  affordable_basis            = "affordable_basis",
  tenure_social_rent_pct      = "social_rent_pct|social.rent|tenure.social",
  tenure_intermediate_pct     = "intermediate_pct|intermediate.tenure|tenure.intermediate",
  public_realm_area_sqm       = "public_realm_area|public.realm.area",
  breeam_rating_target        = "breeam_rating|breeam.target|breeam",
  sustainability_carbon_reduction_pct = "carbon_reduction|sustainability.*carbon",
  commercial_floorspace_sqm   = "commercial_floorspace|commercial.floorspace"
)

# Helper: 安全转成数字或字符
safe_convert <- function(val, to_numeric = TRUE) {
  if (is.null(val) || length(val) == 0) return(NA)
  if (is.list(val)) val <- unlist(val)[1]
  if (to_numeric) return(suppressWarnings(as.numeric(val)))
  return(as.character(val))
}

numeric_fields <- setdiff(names(indicator_patterns), 
                          c("affordable_basis", "breeam_rating_target"))

indicators_wide_corpus <- map_dfr(names(parsed_v2_corpus), function(lpa) {
  p <- parsed_v2_corpus[[lpa]]
  if (is.null(p)) return(NULL)
  
  row <- tibble(lpa_number = lpa)
  for (field in names(indicator_patterns)) {
    val <- find_indicator(p, indicator_patterns[[field]])
    row[[field]] <- safe_convert(val, to_numeric = field %in% numeric_fields)
  }
  
  # Community facilities: 找 array 字段
  cf_val <- find_indicator(p, "community_facilities|community.facilities.provided")
  if (!is.na(cf_val)[1] && length(cf_val) > 0) {
    if (is.list(cf_val)) cf_val <- unlist(cf_val)
    cf_val <- cf_val[!is.na(cf_val) & cf_val != ""]
    row$community_facilities_count <- length(cf_val)
    row$community_facilities_list <- paste(cf_val, collapse = "; ")
  } else {
    row$community_facilities_count <- NA_integer_
    row$community_facilities_list <- NA_character_
  }
  
  row
})

cat("=== Indicators wide (v2 parser) ===\n")
cat("Rows:", nrow(indicators_wide_corpus), "\n\n")

cat("=== Data completeness ===\n")
indicators_wide_corpus %>%
  summarise(across(-lpa_number, ~ sum(!is.na(.)))) %>%
  pivot_longer(everything(), names_to = "indicator", values_to = "n_available") %>%
  mutate(pct = round(n_available / nrow(indicators_wide_corpus) * 100, 1)) %>%
  arrange(desc(n_available)) %>%
  print(n = Inf)

# Save
write_csv(indicators_wide_corpus, 
          here(output_dir, "llm_indicators_v2_corpus.csv"))
# =============================================================
# STAGE 22 — MASTER PAIRING DATAFRAME (59 CORPUS)
# =============================================================

library(here)

# ---- Load all corpus data ----
keyword_matrix_corpus <- read_csv(here(output_dir, "llm_keyword_matrix_corpus.csv"),
                                  show_col_types = FALSE)
patterns_corpus <- read_csv(here(output_dir, "llm_specific_patterns_corpus.csv"),
                            show_col_types = FALSE)
# indicators_wide_corpus 已经在环境里(刚跑完)

# ---- Load 74 corpus metadata ----
th_74 <- read_csv(here(output_dir, "th_74_typed.csv"),
                  show_col_types = FALSE) %>%
  mutate(base_lpa = str_replace(lpa_number, "/[A-Z][A-Z0-9]*$", "")) %>%
  select(base_lpa, financial_year, units_proposed, units_lost,
         site_name, postcode, description) %>%
  distinct(base_lpa, .keep_all = TRUE)

# ---- Combine ----
# lpa_number from LLM 是 strip 后的 (如 "PA/13/02722"),th_74 base_lpa 同样
# 但 LL/... 项目 base_lpa 是 "16/00451" (strip suffix),LLM 里是 "LL/16/00451"
# 处理这个 mismatch:

th_74_matched <- th_74 %>%
  mutate(
    # 如果 base_lpa 没有 PA/LL 前缀,尝试匹配 LL/前缀
    join_key = ifelse(str_detect(base_lpa, "^(PA|LL)/"),
                      base_lpa,
                      paste0("LL/", base_lpa))
  )

master_corpus <- keyword_matrix_corpus %>%
  left_join(indicators_wide_corpus, by = "lpa_number") %>%
  left_join(patterns_corpus, by = "lpa_number") %>%
  left_join(th_74_matched %>% select(-base_lpa), 
            by = c("lpa_number" = "join_key")) %>%
  mutate(
    density_dph = dwellings_proposed_total / site_area_hectares,
    involves_demolition = units_lost > 0
  )

cat("Master corpus dataframe:", nrow(master_corpus), "rows,",
    ncol(master_corpus), "cols\n\n")

# Check join success
cat("=== Metadata join check ===\n")
cat("With financial_year:", sum(!is.na(master_corpus$financial_year)), "\n")
cat("With units_proposed (from PLD):", sum(!is.na(master_corpus$units_proposed)), "\n\n")

# ---- Core corpus-scale pairing correlations ----

compute_cor <- function(x, y) {
  suppressWarnings(cor(x, y, use = "pairwise.complete.obs", method = "spearman"))
}

cat("=== Corpus-scale narrative × indicator pairings ===\n\n")

cat(sprintf("Regeneration × Density              ρ = %.3f (n=%d)\n",
            compute_cor(master_corpus$regeneration, master_corpus$density_dph),
            sum(!is.na(master_corpus$regeneration) & !is.na(master_corpus$density_dph))))

cat(sprintf("Regeneration × Affordable %%         ρ = %.3f (n=%d)\n",
            compute_cor(master_corpus$regeneration, master_corpus$affordable_percentage),
            sum(!is.na(master_corpus$regeneration) & !is.na(master_corpus$affordable_percentage))))

cat(sprintf("Community × Public realm            ρ = %.3f (n=%d)\n",
            compute_cor(master_corpus$community, master_corpus$public_realm_area_sqm),
            sum(!is.na(master_corpus$community) & !is.na(master_corpus$public_realm_area_sqm))))

cat(sprintf("High-density × Height (storeys)     ρ = %.3f (n=%d)\n",
            compute_cor(master_corpus$high_density, master_corpus$building_height_max_storeys),
            sum(!is.na(master_corpus$high_density) & !is.na(master_corpus$building_height_max_storeys))))

cat(sprintf("Sustainability × Affordable %%       ρ = %.3f (n=%d)\n",
            compute_cor(master_corpus$sustainability, master_corpus$affordable_percentage),
            sum(!is.na(master_corpus$sustainability) & !is.na(master_corpus$affordable_percentage))))

cat(sprintf("Heritage × Site area                ρ = %.3f (n=%d)\n",
            compute_cor(master_corpus$heritage, master_corpus$site_area_hectares),
            sum(!is.na(master_corpus$heritage) & !is.na(master_corpus$site_area_hectares))))

cat(sprintf("Community × Dwellings (scale)       ρ = %.3f (n=%d)\n",
            compute_cor(master_corpus$community, master_corpus$dwellings_proposed_total),
            sum(!is.na(master_corpus$community) & !is.na(master_corpus$dwellings_proposed_total))))

cat(sprintf("Redevelopment × Density             ρ = %.3f (n=%d)\n",
            compute_cor(master_corpus$redevelopment, master_corpus$density_dph),
            sum(!is.na(master_corpus$redevelopment) & !is.na(master_corpus$density_dph))))


# ---- Pattern B analysis (corpus scale) ----

cat("\n=== Pattern B: Displacement term usage (corpus) ===\n")
master_corpus %>%
  count(pattern_b_answer, pattern_b_uses_displacement) %>%
  print()


# ---- Save ----
write_csv(master_corpus, here(output_dir, "master_pairing_data_corpus.csv"))
cat("\nSaved: output/master_pairing_data_corpus.csv\n")
# =============================================================
# STAGE 23a — CONTEXTUAL CODING AGGREGATE (59 CORPUS)
# =============================================================
# Aggregate coding table to see:
#  - Which narrative uses which dominant code
#  - Which narratives most often OFFSETTING / AVOIDANT
# =============================================================

library(here)

coding_corpus <- read_csv(here(output_dir, "llm_contextual_coding_corpus.csv"),
                          show_col_types = FALSE)

# ---- Normalise coding labels ----
coding_corpus <- coding_corpus %>%
  mutate(
    dominant_code = str_to_upper(dominant_code),
    dominant_code = case_when(
      str_detect(dominant_code, "^ASSERT") ~ "ASSERTIVE",
      str_detect(dominant_code, "^OFFSET") ~ "OFFSETTING",
      str_detect(dominant_code, "^AVOID")  ~ "AVOIDANT",
      str_detect(dominant_code, "^TECH")   ~ "TECHNICAL",
      str_detect(dominant_code, "N/?A")    ~ "N/A",
      TRUE ~ dominant_code
    )
  )


# ---- Narrative × Code cross-tab ----

coding_matrix <- coding_corpus %>%
  filter(!is.na(keyword), !is.na(dominant_code)) %>%
  count(keyword, dominant_code) %>%
  group_by(keyword) %>%
  mutate(total = sum(n),
         pct = round(n / total * 100, 1)) %>%
  ungroup()

cat("=== Narrative × Code count table ===\n")
coding_matrix %>%
  select(keyword, dominant_code, n, pct) %>%
  pivot_wider(names_from = dominant_code, values_from = n, values_fill = 0) %>%
  print(n = Inf, width = Inf)


# ---- Percentages per narrative (dominant code proportion) ----

coding_pct <- coding_matrix %>%
  select(keyword, dominant_code, pct) %>%
  pivot_wider(names_from = dominant_code, values_from = pct, values_fill = 0)

cat("\n=== Narrative × Code % table ===\n")
print(coding_pct, n = Inf, width = Inf)


# ---- Which narratives most OFFSETTING/AVOIDANT ----

cat("\n=== Top OFFSETTING narratives ===\n")
coding_matrix %>%
  filter(dominant_code == "OFFSETTING") %>%
  arrange(desc(pct)) %>%
  select(keyword, n, pct) %>%
  print(n = Inf)

cat("\n=== Top AVOIDANT narratives ===\n")
coding_matrix %>%
  filter(dominant_code == "AVOIDANT") %>%
  arrange(desc(pct)) %>%
  select(keyword, n, pct) %>%
  print(n = Inf)


# ---- Save ----
write_csv(coding_matrix, here(output_dir, "coding_matrix_corpus.csv"))
# =============================================================
# STAGE 24.2 — PARSE v3 OUTPUT + OVERWRITE CORPUS CSVs
# =============================================================

`%|%` <- function(x, y) {
  if (length(x) == 0 || is.null(x) || (length(x) == 1 && is.na(x))) y else x
}

safe_str <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  as.character(x)[1]
}

parse_v3 <- function(res) {
  if (res$status != "OK") return(NULL)
  tryCatch(fromJSON(res$output, simplifyVector = FALSE),
           error = function(e) NULL)
}

parsed_v3 <- map(results_v3, parse_v3)

cat("Parsed:", sum(!map_lgl(parsed_v3, is.null)), "/", length(parsed_v3), "\n\n")


# ---- Keyword matrix (schema now consistent, direct access) ----

keyword_matrix_v3 <- map_dfr(names(parsed_v3), function(lpa) {
  p <- parsed_v3[[lpa]]
  if (is.null(p) || is.null(p$keyword_frequency)) return(NULL)
  kf <- p$keyword_frequency
  if (!is.list(kf)) return(NULL)
  kf_int <- map(kf, ~ as.integer(.x[[1]]))
  tibble(lpa_number = lpa, !!!kf_int)
})

cat("Keyword matrix:", nrow(keyword_matrix_v3), "rows,", 
    ncol(keyword_matrix_v3), "cols\n")


# ---- Contextual coding (should now include AVOIDANT + TECHNICAL) ----

coding_v3 <- map_dfr(names(parsed_v3), function(lpa) {
  p <- parsed_v3[[lpa]]
  if (is.null(p) || is.null(p$contextual_coding)) return(NULL)
  cc <- p$contextual_coding
  if (!is.list(cc)) return(NULL)
  
  map_dfr(cc, function(code) {
    if (!is.list(code)) return(NULL)
    tibble(
      lpa_number = lpa,
      keyword = safe_str(code$keyword),
      dominant_code = safe_str(code$dominant_code),
      example_quote = safe_str(code$example_quote),
      confidence = safe_str(code$confidence)
    )
  })
})

cat("Coding entries:", nrow(coding_v3), "\n\n")

# Check for AVOIDANT + TECHNICAL
cat("=== v3 Dominant code distribution ===\n")
coding_v3 %>% count(dominant_code, sort = TRUE) %>% print()


# ---- Specific patterns ----

patterns_v3 <- map_dfr(names(parsed_v3), function(lpa) {
  p <- parsed_v3[[lpa]]
  if (is.null(p) || is.null(p$specific_patterns)) return(NULL)
  sp <- p$specific_patterns
  
  a_answer <- safe_str(sp$harm_offset$answer)
  a_quote <- safe_str(sp$harm_offset$quote)
  b_answer <- safe_str(sp$demolition_relocation$answer)
  b_quote <- safe_str(sp$demolition_relocation$quote)
  b_uses <- safe_str(sp$uses_displacement_term$answer)
  
  tibble(lpa_number = lpa,
         pattern_a_answer = a_answer,
         pattern_a_quote = a_quote,
         pattern_b_answer = b_answer,
         pattern_b_uses_displacement = b_uses,
         pattern_b_quote = b_quote)
})

# Normalise YES/NO/TRUE/FALSE
patterns_v3 <- patterns_v3 %>%
  mutate(across(c(pattern_a_answer, pattern_b_answer, pattern_b_uses_displacement),
                ~ case_when(
                  str_to_upper(.) %in% c("TRUE", "YES") ~ "YES",
                  str_to_upper(.) %in% c("FALSE", "NO") ~ "NO",
                  TRUE ~ NA_character_
                )))

cat("\n=== v3 Pattern A distribution ===\n")
patterns_v3 %>% count(pattern_a_answer) %>% print()

cat("\n=== v3 Pattern B × displacement ===\n")
patterns_v3 %>% count(pattern_b_answer, pattern_b_uses_displacement) %>% print()


# ---- Key claims ----

claims_v3 <- map_dfr(names(parsed_v3), function(lpa) {
  p <- parsed_v3[[lpa]]
  if (is.null(p) || is.null(p$key_claims)) return(NULL)
  claims <- p$key_claims
  if (!is.list(claims) && !is.character(claims)) return(NULL)
  tibble(lpa_number = lpa,
         claim_idx = seq_along(claims),
         claim_text = as.character(unlist(claims)))
})


# ---- Overwrite corpus CSVs with v3 data ----

write_csv(keyword_matrix_v3, here(output_dir, "llm_keyword_matrix_corpus.csv"))
write_csv(coding_v3, here(output_dir, "llm_contextual_coding_corpus.csv"))
write_csv(patterns_v3, here(output_dir, "llm_specific_patterns_corpus.csv"))
write_csv(claims_v3, here(output_dir, "llm_key_claims_corpus.csv"))

cat("\n=== 4 corpus CSVs overwritten with v3 data ===\n")
# =============================================================
# STAGE 25 — RECOMPUTE MASTER + CODING AGGREGATE + PATTERN B
# =============================================================
# Uses v3 corpus data. Regenerates:
#  - master_pairing_data_corpus.csv
#  - coding_matrix_corpus.csv
# =============================================================


# ---- Load v3 corpus data ----

keyword_matrix_corpus <- read_csv(here(output_dir, "llm_keyword_matrix_corpus.csv"),
                                  show_col_types = FALSE)
patterns_corpus <- read_csv(here(output_dir, "llm_specific_patterns_corpus.csv"),
                            show_col_types = FALSE)
coding_corpus <- read_csv(here(output_dir, "llm_contextual_coding_corpus.csv"),
                          show_col_types = FALSE)
indicators_wide_corpus <- read_csv(here(output_dir, "llm_indicators_v2_corpus.csv"),
                                   show_col_types = FALSE)

# ---- Load 74 metadata ----
th_74 <- read_csv(here(output_dir, "th_74_typed.csv"), show_col_types = FALSE) %>%
  mutate(base_lpa = str_replace(lpa_number, "/[A-Z][A-Z0-9]*$", "")) %>%
  select(base_lpa, financial_year, units_proposed, units_lost,
         site_name, postcode, description) %>%
  distinct(base_lpa, .keep_all = TRUE) %>%
  mutate(join_key = ifelse(str_detect(base_lpa, "^(PA|LL)/"),
                           base_lpa,
                           paste0("LL/", base_lpa)))


# ---- Rebuild master_pairing_data_corpus ----

master_corpus <- keyword_matrix_corpus %>%
  left_join(indicators_wide_corpus, by = "lpa_number") %>%
  left_join(patterns_corpus, by = "lpa_number") %>%
  left_join(th_74 %>% select(-base_lpa),
            by = c("lpa_number" = "join_key")) %>%
  mutate(
    density_dph = dwellings_proposed_total / site_area_hectares,
    involves_demolition = units_lost > 0
  )

write_csv(master_corpus, here(output_dir, "master_pairing_data_corpus.csv"))
cat("Master rebuilt:", nrow(master_corpus), "rows\n\n")


# ---- Re-run pairing correlations (v3) ----

compute_cor <- function(x, y) {
  suppressWarnings(cor(x, y, use = "pairwise.complete.obs", method = "spearman"))
}

cat("=== Corpus pairings (v3 data) ===\n")
cat(sprintf("Regeneration × Density        ρ = %.3f\n", compute_cor(master_corpus$regeneration, master_corpus$density_dph)))
cat(sprintf("Regeneration × Affordable %%   ρ = %.3f\n", compute_cor(master_corpus$regeneration, master_corpus$affordable_percentage)))
cat(sprintf("Community × Public realm      ρ = %.3f\n", compute_cor(master_corpus$community, master_corpus$public_realm_area_sqm)))
cat(sprintf("High-density × Height         ρ = %.3f\n", compute_cor(master_corpus$high_density, master_corpus$building_height_max_storeys)))
cat(sprintf("Sustainability × Affordable %% ρ = %.3f\n", compute_cor(master_corpus$sustainability, master_corpus$affordable_percentage)))
cat(sprintf("Heritage × Site area          ρ = %.3f\n", compute_cor(master_corpus$heritage, master_corpus$site_area_hectares)))
cat(sprintf("Community × Dwellings         ρ = %.3f\n", compute_cor(master_corpus$community, master_corpus$dwellings_proposed_total)))
cat(sprintf("Redevelopment × Density       ρ = %.3f\n", compute_cor(master_corpus$redevelopment, master_corpus$density_dph)))


# ---- Recompute coding aggregate with 4 codes ----

# Normalize misspellings (LLM sometimes writes "OFFSETING" instead of "OFFSETTING")
coding_corpus <- coding_corpus %>%
  mutate(
    dominant_code = str_to_upper(dominant_code),
    dominant_code = case_when(
      str_detect(dominant_code, "^ASSERT")           ~ "ASSERTIVE",
      str_detect(dominant_code, "^OFFSET")           ~ "OFFSETTING",
      str_detect(dominant_code, "^AVOID")            ~ "AVOIDANT",
      str_detect(dominant_code, "^TECH")             ~ "TECHNICAL",
      str_detect(dominant_code, "N/?A")              ~ "N/A",
      TRUE ~ dominant_code
    )
  )

coding_matrix <- coding_corpus %>%
  filter(!is.na(keyword), !is.na(dominant_code)) %>%
  count(keyword, dominant_code) %>%
  group_by(keyword) %>%
  mutate(total = sum(n),
         pct = round(n / total * 100, 1)) %>%
  ungroup()

cat("\n=== Narrative × Code (v3, 4 codes, N counts) ===\n")
coding_matrix %>%
  select(keyword, dominant_code, n) %>%
  pivot_wider(names_from = dominant_code, values_from = n, values_fill = 0) %>%
  print(width = Inf)

cat("\n=== Narrative × Code (v3, % row) ===\n")
coding_matrix %>%
  select(keyword, dominant_code, pct) %>%
  pivot_wider(names_from = dominant_code, values_from = pct, values_fill = 0) %>%
  print(width = Inf)

write_csv(coding_matrix, here(output_dir, "coding_matrix_corpus.csv"))
# =============================================================
# STAGE 26 — QUOTE-LEVEL ANALYSIS (v3)
# =============================================================


coding_v3 <- read_csv(here(output_dir, "llm_contextual_coding_corpus.csv"),
                      show_col_types = FALSE) %>%
  mutate(dominant_code = str_to_upper(dominant_code),
         dominant_code = case_when(
           str_detect(dominant_code, "^ASSERT")  ~ "ASSERTIVE",
           str_detect(dominant_code, "^OFFSET")  ~ "OFFSETTING",
           str_detect(dominant_code, "^AVOID")   ~ "AVOIDANT",
           str_detect(dominant_code, "^TECH")    ~ "TECHNICAL",
           str_detect(dominant_code, "N/?A")     ~ "N/A",
           TRUE ~ dominant_code
         ))


# ---- Displacement AVOIDANT quotes: what alternative terms? ----

cat("=== Displacement AVOIDANT quotes (sample 15) ===\n")
coding_v3 %>%
  filter(keyword == "displacement", dominant_code == "AVOIDANT") %>%
  select(lpa_number, example_quote, confidence) %>%
  slice_head(n = 15) %>%
  print(width = Inf)


# ---- The 1 project that uses displacement ----

cat("\n=== Project(s) that DO use displacement ===\n")
coding_v3 %>%
  filter(keyword == "displacement", dominant_code == "ASSERTIVE") %>%
  select(lpa_number, example_quote, confidence) %>%
  print(width = Inf)


# ---- Heritage OFFSETTING quotes: the harm-offset formula ----

cat("\n=== Heritage OFFSETTING quotes (sample 15) ===\n")
coding_v3 %>%
  filter(keyword == "heritage", dominant_code == "OFFSETTING") %>%
  select(lpa_number, example_quote, confidence) %>%
  slice_head(n = 15) %>%
  print(width = Inf)


# ---- Placemaking split: who uses it? ----

cat("\n=== Placemaking usage by project ===\n")
coding_v3 %>%
  filter(keyword == "placemaking") %>%
  count(dominant_code) %>%
  mutate(pct = round(n/sum(n)*100, 1)) %>%
  print()

cat("\n=== Placemaking ASSERTIVE projects ===\n")
coding_v3 %>%
  filter(keyword == "placemaking", dominant_code == "ASSERTIVE") %>%
  select(lpa_number, example_quote) %>%
  slice_head(n = 8) %>%
  print(width = Inf)
# =============================================================
# STAGE 27 — DEEP QUOTE ANALYSIS (4 findings)
# =============================================================

library(tidyverse)
library(here)

coding_v3 <- read_csv(here(output_dir, "llm_contextual_coding_corpus.csv"),
                      show_col_types = FALSE) %>%
  mutate(dominant_code = str_to_upper(dominant_code),
         dominant_code = case_when(
           str_detect(dominant_code, "^ASSERT")  ~ "ASSERTIVE",
           str_detect(dominant_code, "^OFFSET")  ~ "OFFSETTING",
           str_detect(dominant_code, "^AVOID")   ~ "AVOIDANT",
           str_detect(dominant_code, "^TECH")    ~ "TECHNICAL",
           str_detect(dominant_code, "N/?A")     ~ "N/A",
           TRUE ~ dominant_code
         ))

master_corpus <- read_csv(here(output_dir, "master_pairing_data_corpus.csv"),
                          show_col_types = FALSE)


# =============================================================
# Finding 1: HOMOGENEITY 量化
# =============================================================

cat("=== Finding 1: Homogeneity quantification ===\n\n")

# 每个 narrative 的 ASSERTIVE %
narrative_assertive <- coding_v3 %>%
  count(keyword, dominant_code) %>%
  group_by(keyword) %>%
  mutate(pct = round(n/sum(n)*100, 1)) %>%
  filter(dominant_code == "ASSERTIVE") %>%
  arrange(desc(pct))

cat("Narratives ranked by ASSERTIVE %:\n")
narrative_assertive %>% select(keyword, ASSERTIVE_pct = pct) %>% print(n = Inf)

# 有多少 narrative >= 65% ASSERTIVE("corpus-wide convention")
n_over_65 <- narrative_assertive %>% filter(pct >= 65) %>% nrow()
cat(sprintf("\nNarratives with ≥65%% ASSERTIVE: %d/12\n", n_over_65))
cat("These 9 narratives are corpus-wide positive assertions,\n")
cat("not developer-strategic differentiators.\n")


# =============================================================
# Finding 2: HERITAGE NPPF template phrase count
# =============================================================

cat("\n\n=== Finding 2: Heritage OFFSETTING NPPF template ===\n\n")

heritage_offsetting <- coding_v3 %>%
  filter(keyword == "heritage", dominant_code == "OFFSETTING")

cat(sprintf("Heritage OFFSETTING projects: %d\n", nrow(heritage_offsetting)))

# 找 NPPF template phrases in quotes
npmf_phrases <- c(
  "less than substantial",
  "outweigh",
  "balanced against",
  "public benefit"
)

heritage_offsetting_phrases <- heritage_offsetting %>%
  mutate(
    has_less_substantial = str_detect(str_to_lower(example_quote), 
                                      "less than substantial"),
    has_outweigh = str_detect(str_to_lower(example_quote), "outweigh"),
    has_balanced = str_detect(str_to_lower(example_quote), 
                              "balanced against|balance against"),
    has_public_benefit = str_detect(str_to_lower(example_quote), 
                                    "public benefit")
  )

cat("\nNPPF template phrase presence in heritage OFFSETTING quotes:\n")
heritage_offsetting_phrases %>%
  summarise(
    n_less_substantial = sum(has_less_substantial),
    n_outweigh = sum(has_outweigh),
    n_balanced = sum(has_balanced),
    n_public_benefit = sum(has_public_benefit),
    n_any_phrase = sum(has_less_substantial | has_outweigh | 
                         has_balanced | has_public_benefit)
  ) %>%
  pivot_longer(everything(), names_to = "template_element", values_to = "n") %>%
  mutate(pct = round(n/nrow(heritage_offsetting)*100, 1)) %>%
  print()


# =============================================================
# Finding 3a: DISPLACEMENT AVOIDANT alternative terms
# =============================================================

cat("\n\n=== Finding 3a: Displacement AVOIDANT alternative vocabulary ===\n\n")

displacement_avoidant <- coding_v3 %>%
  filter(keyword == "displacement", dominant_code == "AVOIDANT")

cat(sprintf("Displacement AVOIDANT projects: %d\n\n", nrow(displacement_avoidant)))

# 统计 alternative terms
displacement_avoidant %>%
  mutate(
    uses_relocation = str_detect(str_to_lower(example_quote), "relocat"),
    uses_remove = str_detect(str_to_lower(example_quote), "remov"),
    uses_redevelopment = str_detect(str_to_lower(example_quote), "redevelop"),
    uses_demolition = str_detect(str_to_lower(example_quote), "demolit"),
    uses_replacement = str_detect(str_to_lower(example_quote), "replac"),
    uses_transformation = str_detect(str_to_lower(example_quote), "transform")
  ) %>%
  summarise(across(starts_with("uses_"), sum)) %>%
  pivot_longer(everything(), names_to = "alternative_term", values_to = "n") %>%
  mutate(pct = round(n/nrow(displacement_avoidant)*100, 1)) %>%
  arrange(desc(n)) %>%
  print()


# =============================================================
# Finding 3b: The ONE ASSERTIVE displacement — case study
# =============================================================

cat("\n=== Finding 3b: The ONE ASSERTIVE displacement project ===\n\n")

coding_v3 %>%
  filter(keyword == "displacement", dominant_code == "ASSERTIVE") %>%
  select(lpa_number, example_quote, confidence) %>%
  print(width = Inf)

# 那个 project 的其他信息
one_assertive_lpa <- coding_v3 %>%
  filter(keyword == "displacement", dominant_code == "ASSERTIVE") %>%
  pull(lpa_number) %>%
  first()

if (!is.na(one_assertive_lpa)) {
  master_corpus %>%
    filter(lpa_number == one_assertive_lpa) %>%
    select(lpa_number, site_name, units_proposed, units_lost, 
           financial_year, description) %>%
    mutate(description = str_sub(description, 1, 150)) %>%
    print(width = Inf)
}


# =============================================================
# Finding 4: PLACEMAKING vocabulary hierarchy
# =============================================================

cat("\n\n=== Finding 4: Placemaking vs project scale ===\n\n")

placemaking_by_project <- coding_v3 %>%
  filter(keyword == "placemaking") %>%
  select(lpa_number, dominant_code)

placemaking_master <- master_corpus %>%
  left_join(placemaking_by_project, by = "lpa_number")

cat("Placemaking usage vs project characteristics:\n\n")
placemaking_master %>%
  filter(dominant_code %in% c("ASSERTIVE", "N/A")) %>%
  group_by(dominant_code) %>%
  summarise(
    n_projects = n(),
    median_units = median(units_proposed, na.rm = TRUE),
    median_storeys = median(building_height_max_storeys, na.rm = TRUE),
    median_site_ha = median(site_area_hectares, na.rm = TRUE),
    max_units = max(units_proposed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

cat("\nPlacemaking ASSERTIVE project details:\n")
placemaking_master %>%
  filter(dominant_code == "ASSERTIVE") %>%
  select(lpa_number, site_name, units_proposed, 
         building_height_max_storeys, financial_year) %>%
  print(width = Inf)