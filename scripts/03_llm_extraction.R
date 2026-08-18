# =============================================================
# 03_llm_extraction.R
# =============================================================
# LLM-based analysis via OpenAI API — two rounds
#   Round 1 (Stage 12): Keyword frequency + contextual coding + patterns
#   Round 2 (Stage 14): Project-produced indicators
#
# Input:  data/planning_statements/*.pdf
# Output: output/pdf_extraction_summary.csv
#         output/llm_results_raw.rds        (Round 1 raw)
#         output/llm_keyword_matrix.csv
#         output/llm_contextual_coding.csv
#         output/llm_specific_patterns.csv
#         output/llm_key_claims.csv
#         output/llm_results_v2_raw.rds     (Round 2 raw)
#         output/llm_indicators_v2.csv
#
# CRITICAL: This script uses cached rds files if they exist.
#           To force re-run of API calls, delete the .rds files first.
# =============================================================

source(here::here("scripts", "00_setup.R"))


# ---- STAGE 11: DOCUMENT RETRIEVAL LOG (empty placeholder) ----

document_log <- tibble(
  lpa_number = character(),
  primary_document_type = character(),
  n_files = integer(),
  notes = character()
)

write_csv(document_log, here(output_dir, "document_log.csv"))


# ---- STAGE 12.1: PDF INVENTORY ----

all_pdfs <- list.files(pdf_dir, pattern = "\\.pdf$", full.names = TRUE)

pdf_inventory <- tibble(
  file_path = all_pdfs,
  file_name = basename(all_pdfs)
) %>%
  mutate(
    project_code = str_extract(file_name, "^PA_\\d+_\\d+"),
    lpa_number = str_replace_all(project_code, "_", "/"),
    part = str_extract(file_name, "_(\\d+)\\.pdf$") %>%
      str_extract("\\d+") %>%
      as.integer()
  ) %>%
  arrange(lpa_number, part)

cat("Total PDFs:", nrow(pdf_inventory), 
    "| Unique projects:", n_distinct(pdf_inventory$lpa_number), "\n")


# ---- STAGE 12.2: EXTRACT TEXT FROM PDFS ----

extract_pdf_text <- function(file_path) {
  tryCatch({
    text_pages <- pdf_text(file_path)
    tibble(file_path = file_path,
           n_pages = length(text_pages),
           full_text = paste(text_pages, collapse = "\n\n"),
           total_chars = nchar(paste(text_pages, collapse = "\n\n")),
           status = "OK")
  }, error = function(e) {
    tibble(file_path = file_path,
           n_pages = NA_integer_, full_text = NA_character_,
           total_chars = NA_integer_,
           status = paste("ERROR:", e$message))
  })
}

cat("Extracting text from", nrow(pdf_inventory), "PDFs...\n")

extraction_results <- pdf_inventory %>%
  rowwise() %>%
  do(extract_pdf_text(.$file_path)) %>%
  ungroup()

pdf_extracted <- pdf_inventory %>%
  left_join(extraction_results, by = "file_path")

pdf_extracted %>%
  select(-full_text) %>%
  write_csv(here(output_dir, "pdf_extraction_summary.csv"))


# ---- STAGE 12.3: EXCLUDE + TRUNCATE + AGGREGATE ----

excluded_projects <- c("PA/10/00373", "PA/15/02671")
excluded_files <- c("PA_22_00210_3.pdf")

pdf_extracted_clean <- pdf_extracted %>%
  filter(!lpa_number %in% excluded_projects,
         !file_name %in% excluded_files)

truncate_if_large <- function(text, max_chars = 400000) {
  if (nchar(text) > max_chars) substr(text, 1, max_chars) else text
}

pdf_extracted_clean <- pdf_extracted_clean %>%
  mutate(text_final = map_chr(full_text, truncate_if_large),
         chars_used = nchar(text_final),
         truncated = chars_used < total_chars)

project_texts <- pdf_extracted_clean %>%
  arrange(lpa_number, part) %>%
  group_by(lpa_number) %>%
  summarise(
    n_files = n(),
    combined_text = paste(text_final, collapse = "\n\n---DOCUMENT BREAK---\n\n"),
    total_chars = sum(chars_used),
    total_pages = sum(n_pages),
    .groups = "drop"
  )

cat("\n=== Project-level text ready ===\n")
cat("Projects:", nrow(project_texts), 
    "| Total chars:", sum(project_texts$total_chars), "\n")


# ---- STAGE 12.4: API CALL ROUND 1 (CACHED) ----

rds_path_v1 <- here(output_dir, "llm_results_raw.rds")

if (file.exists(rds_path_v1)) {
  cat("\n>>> Loading cached Round 1 results (no API call) <<<\n")
  results <- readRDS(rds_path_v1)
} else {
  
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "") stop("OPENAI_API_KEY not set")
  
  model_name <- "gpt-4o-mini"
  
  system_prompt <- "You are analysing a UK planning application document. Extract structured data about how developer narratives are used in this text. Return ONLY a single valid JSON object, no other text."
  
  user_prompt_template <- 'TASK 1 — KEYWORD FREQUENCY
Count exact occurrences (case-insensitive) of each keyword. Include morphological variants.

Keywords:
- regeneration: regeneration, regenerate, regenerated, renewal
- redevelopment: redevelopment, redevelop
- mixed_use: mixed-use, mixed use
- community: community, communities, neighbourhood, neighbourhoods
- sustainability: sustainability, sustainable
- connectivity: connectivity, connections, connected, permeability
- public_realm: public realm, public space, public spaces, plaza
- affordable: affordable, affordable housing
- placemaking: placemaking, place-making
- high_density: high density, high-density, tall building, tall buildings
- heritage: heritage, historic, listed
- displacement: displacement, displace, relocate, relocation

TASK 2 — CONTEXTUAL CODING
For EVERY keyword above, classify DOMINANT usage pattern:
- ASSERTIVE, OFFSETTING, AVOIDANT, TECHNICAL, or N/A

For each coded keyword (excluding N/A):
- dominant_code, example_quote (20-40 words), confidence (HIGH/MEDIUM/LOW)

TASK 3 — KEY CLAIMS
Extract 5-8 sentences making the strongest positive claims about the scheme.

TASK 4 — SPECIFIC PATTERNS
(a) Does the document acknowledge harm AND argue it is outweighed by public benefits?
(b) Does the document describe demolition/relocation? If YES, does it use "displacement" for this?

OUTPUT: JSON only.

===DOCUMENT===
{DOCUMENT_TEXT}'
  
  call_openai <- function(document_text, lpa_id) {
    user_prompt <- str_replace(user_prompt_template, 
                               fixed("{DOCUMENT_TEXT}"), document_text)
    body <- list(model = model_name,
                 messages = list(list(role = "system", content = system_prompt),
                                 list(role = "user", content = user_prompt)),
                 response_format = list(type = "json_object"),
                 temperature = 0)
    response <- POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(Authorization = paste("Bearer", api_key),
                  `Content-Type` = "application/json"),
      body = toJSON(body, auto_unbox = TRUE),
      encode = "raw", timeout(300))
    
    if (status_code(response) != 200) {
      return(list(lpa_id = lpa_id, status = "ERROR",
                  error = content(response, as = "text"), output = NA))
    }
    parsed <- content(response, as = "parsed")
    list(lpa_id = lpa_id, status = "OK",
         output = parsed$choices[[1]]$message$content,
         input_tokens = parsed$usage$prompt_tokens,
         output_tokens = parsed$usage$completion_tokens,
         total_cost = parsed$usage$prompt_tokens * 2.5 / 1e6 + 
           parsed$usage$completion_tokens * 10 / 1e6)
  }
  
  results <- list()
  for (i in seq_len(nrow(project_texts))) {
    proj <- project_texts$lpa_number[i]
    cat(sprintf("[%d/%d] %s...\n", i, nrow(project_texts), proj))
    results[[proj]] <- call_openai(project_texts$combined_text[i], proj)
    saveRDS(results, rds_path_v1)
    Sys.sleep(1)
  }
}

cat("\n=== Round 1: results loaded ===\n")


# ---- STAGE 12.5: PARSE ROUND 1 ----

parse_result <- function(res) {
  if (res$status != "OK") return(NULL)
  tryCatch(fromJSON(res$output, simplifyVector = FALSE),
           error = function(e) NULL)
}

parsed_results <- map(results, parse_result)

# Keyword matrix
keyword_matrix <- map_dfr(names(parsed_results), function(lpa) {
  p <- parsed_results[[lpa]]
  if (is.null(p) || is.null(p$keyword_frequency)) return(NULL)
  tibble(lpa_number = lpa, !!!p$keyword_frequency)
})

# Contextual coding
coding_table <- map_dfr(names(parsed_results), function(lpa) {
  p <- parsed_results[[lpa]]
  if (is.null(p) || is.null(p$contextual_coding)) return(NULL)
  map_dfr(p$contextual_coding, function(code) {
    tibble(lpa_number = lpa,
           keyword = code$keyword %||% NA_character_,
           dominant_code = code$dominant_code %||% NA_character_,
           example_quote = code$example_quote %||% NA_character_,
           confidence = code$confidence %||% NA_character_)
  })
})

# Specific patterns
patterns_table <- map_dfr(names(parsed_results), function(lpa) {
  p <- parsed_results[[lpa]]
  if (is.null(p) || is.null(p$specific_patterns)) return(NULL)
  tibble(lpa_number = lpa,
         pattern_a_answer = p$specific_patterns$a$answer %||% NA_character_,
         pattern_a_quote = p$specific_patterns$a$quote %||% NA_character_,
         pattern_b_answer = p$specific_patterns$b$answer %||% NA_character_,
         pattern_b_uses_displacement = p$specific_patterns$b$uses_displacement_term %||% NA_character_,
         pattern_b_quote = p$specific_patterns$b$quote %||% NA_character_)
})

# Key claims
claims_table <- map_dfr(names(parsed_results), function(lpa) {
  p <- parsed_results[[lpa]]
  if (is.null(p) || is.null(p$key_claims)) return(NULL)
  tibble(lpa_number = lpa, claim_idx = seq_along(p$key_claims),
         claim_text = unlist(p$key_claims))
})

write_csv(keyword_matrix, here(output_dir, "llm_keyword_matrix.csv"))
write_csv(coding_table, here(output_dir, "llm_contextual_coding.csv"))
write_csv(patterns_table, here(output_dir, "llm_specific_patterns.csv"))
write_csv(claims_table, here(output_dir, "llm_key_claims.csv"))

cat("Round 1 parsed. 4 CSV files saved.\n")


# ---- STAGE 14: API CALL ROUND 2 (CACHED) ----

rds_path_v2 <- here(output_dir, "llm_results_v2_raw.rds")

if (file.exists(rds_path_v2)) {
  cat("\n>>> Loading cached Round 2 results (no API call) <<<\n")
  results_v2 <- readRDS(rds_path_v2)
} else {
  
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "") stop("OPENAI_API_KEY not set")
  
  model_name <- "gpt-4o-mini"
  
  system_prompt_v2 <- "You are analysing a UK planning application document. Extract specific quantitative and categorical indicators. Return ONLY a single valid JSON object. Use null for values not explicitly stated in the document."
  
  user_prompt_v2_template <- 'Extract indicators from this planning document. Use null if not stated. Do not infer.

INDICATORS:
1. site_area_hectares (convert sqm/10000)
2. dwellings_proposed_total (integer)
3. building_height_max_storeys (integer)
4. building_height_max_metres (numeric)
5. affordable_units_total (integer)
6. affordable_percentage (numeric)
7. affordable_basis: "units" / "habitable_rooms" / "floorspace" / "unclear" / null
8. tenure_social_rent_pct (numeric)
9. tenure_intermediate_pct (numeric)
10. public_realm_area_sqm (numeric)
11. community_facilities_provided (array of strings)
12. breeam_rating_target (string or null)
13. sustainability_carbon_reduction_pct (numeric)
14. commercial_floorspace_sqm (numeric)

For each: {"value": <val or null>, "source_quote": <sentence or null>}

OUTPUT: JSON only.

===DOCUMENT===
{DOCUMENT_TEXT}'
  
  call_openai_v2 <- function(document_text, lpa_id) {
    user_prompt <- str_replace(user_prompt_v2_template,
                               fixed("{DOCUMENT_TEXT}"), document_text)
    body <- list(model = model_name,
                 messages = list(list(role = "system", content = system_prompt_v2),
                                 list(role = "user", content = user_prompt)),
                 response_format = list(type = "json_object"),
                 temperature = 0)
    response <- POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(Authorization = paste("Bearer", api_key),
                  `Content-Type` = "application/json"),
      body = toJSON(body, auto_unbox = TRUE),
      encode = "raw", timeout(300))
    
    if (status_code(response) != 200) {
      return(list(lpa_id = lpa_id, status = "ERROR",
                  error = content(response, as = "text"), output = NA))
    }
    parsed <- content(response, as = "parsed")
    list(lpa_id = lpa_id, status = "OK",
         output = parsed$choices[[1]]$message$content,
         total_cost = parsed$usage$prompt_tokens * 0.15 / 1e6 + 
           parsed$usage$completion_tokens * 0.60 / 1e6)
  }
  
  results_v2 <- list()
  for (i in seq_len(nrow(project_texts))) {
    proj <- project_texts$lpa_number[i]
    cat(sprintf("[%d/%d] %s...\n", i, nrow(project_texts), proj))
    results_v2[[proj]] <- call_openai_v2(project_texts$combined_text[i], proj)
    saveRDS(results_v2, rds_path_v2)
    Sys.sleep(1)
  }
}

cat("\n=== Round 2: results loaded ===\n")


# ---- STAGE 14.2: PARSE ROUND 2 ----

parse_v2 <- function(res) {
  if (res$status != "OK") return(NULL)
  tryCatch(fromJSON(res$output, simplifyVector = FALSE),
           error = function(e) NULL)
}

parsed_v2 <- map(results_v2, parse_v2)

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

numeric_fields <- setdiff(indicator_fields, c("affordable_basis", "breeam_rating_target"))

indicators_wide <- map_dfr(names(parsed_v2), function(lpa) {
  p <- parsed_v2[[lpa]]
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

write_csv(indicators_wide, here(output_dir, "llm_indicators_v2.csv"))

cat("Round 2 parsed. llm_indicators_v2.csv saved.\n")
cat("\n=== 03_llm_extraction.R DONE ===\n")

# =============================================================
# STAGE 18 — SCALE-UP TO 61 CORPUS
# =============================================================
# Preview only. Uncomment API sections after approval.
# =============================================================


# ---- Step 18.1: Full corpus PDF inventory ----

all_pdfs_corpus <- list.files(pdf_dir, pattern = "\\.pdf$", full.names = TRUE)

pdf_inventory_corpus <- tibble(
  file_path = all_pdfs_corpus,
  file_name = basename(all_pdfs_corpus)
) %>%
  mutate(
    project_code = str_extract(file_name, "^(PA|LL)_\\d+_\\d+"),
    lpa_number = str_replace_all(project_code, "_", "/"),
    part = str_extract(file_name, "_(\\d+)\\.pdf$") %>%
      str_extract("\\d+") %>%
      as.integer()
  ) %>%
  arrange(lpa_number, part)

cat("=== Full corpus PDF inventory ===\n")
cat("Total PDFs:", nrow(pdf_inventory_corpus), "\n")
cat("Unique projects:", n_distinct(pdf_inventory_corpus$lpa_number), "\n\n")

# 检查每个 project 有几份 PDF
cat("=== PDFs per project ===\n")
pdf_inventory_corpus %>%
  count(lpa_number, name = "n_files") %>%
  count(n_files, name = "n_projects") %>%
  print()


# ---- Step 18.2: Extract text from all PDFs ----

cat("\nExtracting text from", nrow(pdf_inventory_corpus), "PDFs...\n")

extraction_results_corpus <- pdf_inventory_corpus %>%
  rowwise() %>%
  do(extract_pdf_text(.$file_path)) %>%
  ungroup()

pdf_extracted_corpus <- pdf_inventory_corpus %>%
  left_join(extraction_results_corpus, by = "file_path")

cat("\n=== Extraction status ===\n")
pdf_extracted_corpus %>% count(status) %>% print()


# ---- Step 18.3: Exclude previously identified non-analyzable + truncate ----

# 排除 sub-sample 时用 substitute 的 2 个(DAS / ES),继续走 discourse pipeline 一致
excluded_projects_corpus <- c("PA/10/00373", "PA/15/02671")

# 排除 scanned / no-text PDFs
excluded_files_corpus <- c("PA_22_00210_3.pdf")

pdf_corpus_clean <- pdf_extracted_corpus %>%
  filter(!lpa_number %in% excluded_projects_corpus,
         !file_name %in% excluded_files_corpus,
         status == "OK",
         !is.na(full_text))

# Truncate 超大文件 (400K chars ≈ 100K tokens for GPT-4o-mini context)
pdf_corpus_clean <- pdf_corpus_clean %>%
  mutate(text_final = map_chr(full_text, truncate_if_large),
         chars_used = nchar(text_final),
         truncated = chars_used < total_chars)

cat("\n=== Truncated files ===\n")
pdf_corpus_clean %>%
  filter(truncated) %>%
  select(file_name, total_chars, chars_used) %>%
  print(n = Inf)


# ---- Step 18.4: Aggregate to project level ----

project_texts_corpus <- pdf_corpus_clean %>%
  arrange(lpa_number, part) %>%
  group_by(lpa_number) %>%
  summarise(
    n_files = n(),
    combined_text = paste(text_final, collapse = "\n\n---DOCUMENT BREAK---\n\n"),
    total_chars = sum(chars_used),
    total_pages = sum(n_pages),
    .groups = "drop"
  )

cat("\n=== Project-level text ready ===\n")
cat("Projects:", nrow(project_texts_corpus), "\n")

# Preview: project sizes
project_texts_corpus %>%
  mutate(est_tokens = total_chars / 4) %>%
  arrange(desc(est_tokens)) %>%
  select(lpa_number, n_files, total_pages, total_chars, est_tokens) %>%
  print(n = Inf, width = Inf)

# ---- Step 18.4b: Project-combined truncation (safeguard) ----
# Single-PDF truncate (Step 18.3) misses cases where multiple PDFs 
# per project combined exceed context window. This step applies a 
# second truncate at project level.

truncate_combined <- function(text, max_chars = 500000) {
  if (nchar(text) > max_chars) substr(text, 1, max_chars) else text
}

project_texts_corpus <- project_texts_corpus %>%
  mutate(
    combined_truncated = map_chr(combined_text, truncate_combined),
    total_chars_final = nchar(combined_truncated),
    project_truncated = total_chars_final < total_chars
  )

cat("=== Project-level truncation ===\n")
project_texts_corpus %>%
  filter(project_truncated) %>%
  select(lpa_number, n_files, total_chars, total_chars_final) %>%
  print()

cat("\n=== Max tokens after combined truncation ===\n")
cat("Max est_tokens:", 
    max(project_texts_corpus$total_chars_final) / 4, "\n\n")

# Replace combined_text with truncated version, drop helper columns
project_texts_corpus <- project_texts_corpus %>%
  mutate(combined_text = combined_truncated,
         total_chars = total_chars_final) %>%
  select(-combined_truncated, -total_chars_final, -project_truncated)
# ---- Step 18.5: Cost estimate ----

cost_estimate_corpus <- project_texts_corpus %>%
  mutate(
    input_tokens = total_chars / 4,
    # Round 1: gpt-4o-mini input $0.15/M, output $0.60/M, ~2500 output tokens
    round1_cost = input_tokens * 0.15 / 1e6 + 2500 * 0.60 / 1e6,
    # Round 2 same model, ~1500 output tokens (indicators shorter)
    round2_cost = input_tokens * 0.15 / 1e6 + 1500 * 0.60 / 1e6
  )

cat("\n=== ESTIMATED COST (61 corpus, both rounds, GPT-4o-mini) ===\n")
cat(sprintf("Total input tokens: %.0f\n", sum(cost_estimate_corpus$input_tokens)))
cat(sprintf("Round 1 total: $%.2f\n", sum(cost_estimate_corpus$round1_cost)))
cat(sprintf("Round 2 total: $%.2f\n", sum(cost_estimate_corpus$round2_cost)))
cat(sprintf("Grand total: $%.2f USD\n\n", 
            sum(cost_estimate_corpus$round1_cost) + 
              sum(cost_estimate_corpus$round2_cost)))
# =============================================================
# STAGE 19 — 61 CORPUS: API CALLS (BOTH ROUNDS)
# =============================================================
# CRITICAL: This step calls the OpenAI API and consumes credit.
# Cache protection: skips API if rds files already exist.
# Estimated total cost: ~$1.75 USD
# =============================================================


# ---- Step 19.1: Round 1 — Keyword + coding + patterns (61 corpus) ----

rds_path_v1_corpus <- here(output_dir, "llm_results_corpus_raw.rds")

if (file.exists(rds_path_v1_corpus)) {
  cat("\n>>> Round 1 (corpus): loading cached results, no API call <<<\n")
  results_corpus <- readRDS(rds_path_v1_corpus)
} else {
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "") stop("OPENAI_API_KEY not set")
  
  # 复用 sub-sample 时定义的 system_prompt + user_prompt_template + call_openai
  # 但因为它们在 Stage 12.4 的 if-else 里,需要重新定义:
  
  model_name <- "gpt-4o-mini"
  
  system_prompt <- "You are analysing a UK planning application document. Extract structured data about how developer narratives are used in this text. Return ONLY a single valid JSON object, no other text."
  
  user_prompt_template <- 'TASK 1 — KEYWORD FREQUENCY
Count exact occurrences (case-insensitive) of each keyword. Include morphological variants.

Keywords:
- regeneration: regeneration, regenerate, regenerated, renewal
- redevelopment: redevelopment, redevelop
- mixed_use: mixed-use, mixed use
- community: community, communities, neighbourhood, neighbourhoods
- sustainability: sustainability, sustainable
- connectivity: connectivity, connections, connected, permeability
- public_realm: public realm, public space, public spaces, plaza
- affordable: affordable, affordable housing
- placemaking: placemaking, place-making
- high_density: high density, high-density, tall building, tall buildings
- heritage: heritage, historic, listed
- displacement: displacement, displace, relocate, relocation

TASK 2 — CONTEXTUAL CODING
For EVERY keyword above, classify DOMINANT usage pattern:
- ASSERTIVE, OFFSETTING, AVOIDANT, TECHNICAL, or N/A

For each coded keyword (excluding N/A):
- dominant_code, example_quote (20-40 words), confidence (HIGH/MEDIUM/LOW)

TASK 3 — KEY CLAIMS
Extract 5-8 sentences making the strongest positive claims about the scheme.

TASK 4 — SPECIFIC PATTERNS
(a) Does the document acknowledge harm AND argue it is outweighed by public benefits?
(b) Does the document describe demolition/relocation? If YES, does it use "displacement" for this?

OUTPUT: JSON only.

===DOCUMENT===
{DOCUMENT_TEXT}'
  
  call_openai <- function(document_text, lpa_id) {
    user_prompt <- str_replace(user_prompt_template, 
                               fixed("{DOCUMENT_TEXT}"), document_text)
    body <- list(model = model_name,
                 messages = list(list(role = "system", content = system_prompt),
                                 list(role = "user", content = user_prompt)),
                 response_format = list(type = "json_object"),
                 temperature = 0)
    response <- POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(Authorization = paste("Bearer", api_key),
                  `Content-Type` = "application/json"),
      body = toJSON(body, auto_unbox = TRUE),
      encode = "raw", timeout(300))
    
    if (status_code(response) != 200) {
      return(list(lpa_id = lpa_id, status = "ERROR",
                  error = content(response, as = "text"), output = NA))
    }
    parsed <- content(response, as = "parsed")
    list(lpa_id = lpa_id, status = "OK",
         output = parsed$choices[[1]]$message$content,
         input_tokens = parsed$usage$prompt_tokens,
         output_tokens = parsed$usage$completion_tokens,
         total_cost = parsed$usage$prompt_tokens * 0.15 / 1e6 + 
           parsed$usage$completion_tokens * 0.60 / 1e6)
  }
  
  cat("\n=== Starting Round 1 batch (61 projects) ===\n")
  results_corpus <- list()
  for (i in seq_len(nrow(project_texts_corpus))) {
    proj <- project_texts_corpus$lpa_number[i]
    cat(sprintf("[%d/%d] %s...\n", i, nrow(project_texts_corpus), proj))
    results_corpus[[proj]] <- call_openai(
      project_texts_corpus$combined_text[i], proj
    )
    saveRDS(results_corpus, rds_path_v1_corpus)  # save after each
    Sys.sleep(1)
  }
}

cat("\n=== Round 1 (corpus) complete ===\n")
n_ok <- sum(map_chr(results_corpus, "status") == "OK")
cat(sprintf("Success: %d/%d\n", n_ok, length(results_corpus)))
total_cost_r1 <- sum(map_dbl(results_corpus, 
                             ~ if(.$status == "OK") .$total_cost else 0))
cat(sprintf("Actual Round 1 cost: $%.4f USD\n", total_cost_r1))
# ---- Step 19.2: Parse Round 1 output (corpus) — with defensive checks ----

# 助手函数:安全提取嵌套字段
safe_get <- function(obj, ...) {
  path <- list(...)
  for (key in path) {
    if (is.null(obj) || !is.list(obj) || is.null(obj[[key]])) return(NA_character_)
    obj <- obj[[key]]
  }
  if (is.null(obj) || length(obj) == 0) return(NA_character_)
  as.character(obj)
}

parsed_corpus <- map(results_corpus, function(res) {
  if (res$status != "OK") return(NULL)
  tryCatch(fromJSON(res$output, simplifyVector = FALSE),
           error = function(e) NULL)
})

# Keyword matrix (unchanged)
keyword_matrix_corpus <- map_dfr(names(parsed_corpus), function(lpa) {
  p <- parsed_corpus[[lpa]]
  if (is.null(p) || is.null(p$keyword_frequency)) return(NULL)
  tibble(lpa_number = lpa, !!!p$keyword_frequency)
})

# Contextual coding (with defensive check)
coding_corpus <- map_dfr(names(parsed_corpus), function(lpa) {
  p <- parsed_corpus[[lpa]]
  if (is.null(p) || is.null(p$contextual_coding)) return(NULL)
  if (!is.list(p$contextual_coding)) return(NULL)
  map_dfr(p$contextual_coding, function(code) {
    if (!is.list(code)) return(NULL)
    tibble(
      lpa_number = lpa,
      keyword = safe_get(code, "keyword"),
      dominant_code = safe_get(code, "dominant_code"),
      example_quote = safe_get(code, "example_quote"),
      confidence = safe_get(code, "confidence")
    )
  })
})

# Specific patterns (with defensive check)
patterns_corpus <- map_dfr(names(parsed_corpus), function(lpa) {
  p <- parsed_corpus[[lpa]]
  if (is.null(p) || is.null(p$specific_patterns)) return(NULL)
  
  sp <- p$specific_patterns
  
  # a 可能是 list (正确) 或 string / atomic (异常)
  a_answer <- if (is.list(sp$a)) safe_get(sp, "a", "answer") else as.character(sp$a %||% NA)
  a_quote <- if (is.list(sp$a)) safe_get(sp, "a", "quote") else NA_character_
  
  b_answer <- if (is.list(sp$b)) safe_get(sp, "b", "answer") else as.character(sp$b %||% NA)
  b_uses <- if (is.list(sp$b)) safe_get(sp, "b", "uses_displacement_term") else NA_character_
  b_quote <- if (is.list(sp$b)) safe_get(sp, "b", "quote") else NA_character_
  
  tibble(
    lpa_number = lpa,
    pattern_a_answer = a_answer,
    pattern_a_quote = a_quote,
    pattern_b_answer = b_answer,
    pattern_b_uses_displacement = b_uses,
    pattern_b_quote = b_quote
  )
})

# Key claims (with defensive check)
claims_corpus <- map_dfr(names(parsed_corpus), function(lpa) {
  p <- parsed_corpus[[lpa]]
  if (is.null(p) || is.null(p$key_claims)) return(NULL)
  claims <- p$key_claims
  if (!is.list(claims) && !is.character(claims)) return(NULL)
  tibble(lpa_number = lpa, 
         claim_idx = seq_along(claims),
         claim_text = as.character(unlist(claims)))
})

# Save
write_csv(keyword_matrix_corpus, here(output_dir, "llm_keyword_matrix_corpus.csv"))
write_csv(coding_corpus, here(output_dir, "llm_contextual_coding_corpus.csv"))
write_csv(patterns_corpus, here(output_dir, "llm_specific_patterns_corpus.csv"))
write_csv(claims_corpus, here(output_dir, "llm_key_claims_corpus.csv"))

cat("=== Corpus Round 1 parsed ===\n")
cat("Keyword matrix:", nrow(keyword_matrix_corpus), "rows\n")
cat("Coding entries:", nrow(coding_corpus), "\n")
cat("Patterns:", nrow(patterns_corpus), "\n")
cat("Key claims:", nrow(claims_corpus), "\n")


# ---- Preview ----
cat("\n=== Pattern A: Harm-offset ===\n")
patterns_corpus %>% count(pattern_a_answer) %>% print()

cat("\n=== Pattern B: Displacement term usage ===\n")
patterns_corpus %>% count(pattern_b_answer, pattern_b_uses_displacement) %>% print()

cat("\n=== Contextual coding: dominant code distribution ===\n")
coding_corpus %>% count(dominant_code, sort = TRUE) %>% print()


# ---- Identify the problematic project (index 11) ----
cat("\n=== Diagnostic: project 11 raw output structure ===\n")
proj_11 <- names(parsed_corpus)[11]
cat("Project:", proj_11, "\n")
cat("specific_patterns structure:\n")
str(parsed_corpus[[proj_11]]$specific_patterns)

`%|%` <- function(x, y) {
  if (length(x) == 0 || is.null(x) || (length(x) == 1 && is.na(x))) y else x
}
# ---- Robust extractor: handles both schemas cleanly ----

extract_sections <- function(p) {
  if (is.null(p)) return(NULL)
  
  # Schema A (sub-sample): flat top-level keys
  # Schema B (corpus): nested inside TASK_1/2/3/4
  
  kw <- p$keyword_frequency %||% p$TASK_1$keyword_frequency
  cc <- p$contextual_coding %||% p$TASK_2$contextual_coding
  kc <- p$key_claims %||% p$TASK_3
  sp <- p$specific_patterns %||% p$TASK_4
  
  list(keyword_frequency = kw,
       contextual_coding = cc,
       key_claims = kc,
       specific_patterns = sp)
}

safe_str <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  as.character(x)[1]
}

extracted <- map(parsed_corpus, extract_sections)


# ---- Keyword matrix (wide, one row per project) ----
keyword_matrix_corpus <- map_dfr(names(extracted), function(lpa) {
  p <- extracted[[lpa]]
  if (is.null(p) || is.null(p$keyword_frequency)) return(NULL)
  kf <- p$keyword_frequency
  if (!is.list(kf)) return(NULL)
  # Ensure integers
  kf_int <- map(kf, ~ as.integer(.x[[1]]))
  tibble(lpa_number = lpa, !!!kf_int)
})


# ---- Contextual coding ----
# Handles both:
#   Schema A: array of {keyword, dominant_code, ...}
#   Schema B: named list keyword -> {dominant_code, ...}

coding_corpus <- map_dfr(names(extracted), function(lpa) {
  p <- extracted[[lpa]]
  if (is.null(p) || is.null(p$contextual_coding)) return(NULL)
  cc <- p$contextual_coding
  if (!is.list(cc)) return(NULL)
  
  # Detect schema
  has_keyword_field <- length(cc) > 0 && is.list(cc[[1]]) && !is.null(cc[[1]]$keyword)
  
  if (has_keyword_field) {
    # Schema A: array with keyword field
    map_dfr(cc, function(code) {
      if (!is.list(code)) return(NULL)
      tibble(lpa_number = lpa,
             keyword = safe_str(code$keyword),
             dominant_code = safe_str(code$dominant_code),
             example_quote = safe_str(code$example_quote),
             confidence = safe_str(code$confidence))
    })
  } else {
    # Schema B: named list, keyword is the name
    map_dfr(names(cc), function(kw) {
      code <- cc[[kw]]
      if (!is.list(code)) return(NULL)
      tibble(lpa_number = lpa,
             keyword = kw,
             dominant_code = safe_str(code$dominant_code),
             example_quote = safe_str(code$example_quote),
             confidence = safe_str(code$confidence))
    })
  }
})


# ---- Specific patterns ----
patterns_corpus <- map_dfr(names(extracted), function(lpa) {
  p <- extracted[[lpa]]
  if (is.null(p) || is.null(p$specific_patterns)) return(NULL)
  sp <- p$specific_patterns
  if (!is.list(sp)) return(NULL)
  
  # Schema A: sp$a$answer, sp$b$answer, sp$b$uses_displacement_term
  # Schema B: sp$acknowledge_harm, sp$displacement_usage
  # Schema C (PA/14/00944): sp$acknowledge_harm_and_public_benefits, 
  #                        sp$describe_demolition_or_relocation, sp$use_displacement
  
  # Pattern A (harm-offset)
  a_answer <- 
    (if (is.list(sp$a)) safe_str(sp$a$answer) else NA_character_) %|% 
    safe_str(sp$acknowledge_harm) %|% 
    safe_str(sp$acknowledge_harm_and_public_benefits)
  
  a_quote <- 
    (if (is.list(sp$a)) safe_str(sp$a$quote) else NA_character_)
  
  # Pattern B (demolition/relocation)
  b_answer <- 
    (if (is.list(sp$b)) safe_str(sp$b$answer) else NA_character_) %|%
    safe_str(sp$describe_demolition_or_relocation)
  
  # Uses "displacement"
  b_uses <- 
    (if (is.list(sp$b)) safe_str(sp$b$uses_displacement_term) else NA_character_) %|%
    safe_str(sp$displacement_usage) %|%
    safe_str(sp$use_displacement) %|%
    safe_str(sp$uses_displacement)
  
  b_quote <- 
    (if (is.list(sp$b)) safe_str(sp$b$quote) else NA_character_)
  
  tibble(lpa_number = lpa,
         pattern_a_answer = a_answer,
         pattern_a_quote = a_quote,
         pattern_b_answer = b_answer,
         pattern_b_uses_displacement = b_uses,
         pattern_b_quote = b_quote)
})


# ---- Key claims ----
claims_corpus <- map_dfr(names(extracted), function(lpa) {
  p <- extracted[[lpa]]
  if (is.null(p) || is.null(p$key_claims)) return(NULL)
  claims <- p$key_claims
  if (!is.list(claims) && !is.character(claims)) return(NULL)
  tibble(lpa_number = lpa,
         claim_idx = seq_along(claims),
         claim_text = as.character(unlist(claims)))
})


# %|% helper: coalesce for character (rlang has %||% for null, this is for NA)
`%|%` <- function(x, y) if (is.na(x) || is.null(x)) y else x
# Redefine after use
`%|%` <- function(x, y) {
  if (length(x) == 0 || is.null(x) || (length(x) == 1 && is.na(x))) y else x
}


# ---- Save ----
write_csv(keyword_matrix_corpus, here(output_dir, "llm_keyword_matrix_corpus.csv"))
write_csv(coding_corpus, here(output_dir, "llm_contextual_coding_corpus.csv"))
write_csv(patterns_corpus, here(output_dir, "llm_specific_patterns_corpus.csv"))
write_csv(claims_corpus, here(output_dir, "llm_key_claims_corpus.csv"))

cat("=== Corpus Round 1 parsed (v3) ===\n")
cat("Keyword matrix:", nrow(keyword_matrix_corpus), "rows\n")
cat("Coding entries:", nrow(coding_corpus), "\n")
cat("Patterns:", nrow(patterns_corpus), "\n")
cat("Key claims:", nrow(claims_corpus), "\n")


# ---- Preview ----
cat("\n=== Pattern A ===\n")
patterns_corpus %>% count(pattern_a_answer) %>% print()

cat("\n=== Pattern B × displacement ===\n")
patterns_corpus %>% count(pattern_b_answer, pattern_b_uses_displacement) %>% print()

cat("\n=== Coding distribution ===\n")
coding_corpus %>% count(dominant_code, sort = TRUE) %>% print()

`%|%` <- function(x, y) {
  if (length(x) == 0 || is.null(x) || (length(x) == 1 && is.na(x))) y else x
}

safe_str <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  as.character(x)[1]
}

# Recursive: 在任意深度的 list 里搜 keyword 匹配的 field
find_field <- function(obj, keywords) {
  if (is.null(obj)) return(NA_character_)
  
  if (is.list(obj)) {
    nms <- names(obj)
    if (!is.null(nms)) {
      # 直接匹配
      for (kw in keywords) {
        matched <- nms[str_detect(str_to_lower(nms), kw)]
        if (length(matched) > 0) {
          val <- obj[[matched[1]]]
          if (!is.list(val)) return(safe_str(val))
          # 如果 matched 的 value 又是 list,继续 recurse 找 answer/value/result 字段
          for (sub_name in c("answer", "value", "result")) {
            if (!is.null(val[[sub_name]])) return(safe_str(val[[sub_name]]))
          }
          # 或者直接返回第一个 non-list value
          for (v in val) {
            if (!is.list(v)) return(safe_str(v))
          }
        }
      }
    }
    # 没直接匹配,递归子对象
    for (child in obj) {
      result <- find_field(child, keywords)
      if (!is.na(result)) return(result)
    }
  }
  return(NA_character_)
}


# ---- Re-parse patterns with recursive extractor ----

patterns_corpus <- map_dfr(names(parsed_corpus), function(lpa) {
  p <- parsed_corpus[[lpa]]
  if (is.null(p)) return(NULL)
  
  # 用 TASK_4 或整个 p(如果没有 TASK_4)
  root <- p$TASK_4 %||% p$specific_patterns %||% p
  
  # Pattern A: acknowledge harm 相关
  a_answer <- find_field(root, c("acknowledge_harm", 
                                 "harm_acknowledged",
                                 "harm_and_benefit",
                                 "\\ba\\b"))
  
  # Pattern B: demolition/relocation 相关
  b_answer <- find_field(root, c("demolition", 
                                 "relocation",
                                 "describe_demolition",
                                 "\\bb\\b"))
  
  # Uses "displacement" term
  b_uses <- find_field(root, c("use_displacement",
                               "displacement_used",
                               "displacement_usage",
                               "uses_displacement"))
  
  tibble(lpa_number = lpa,
         pattern_a_answer = a_answer,
         pattern_b_answer = b_answer,
         pattern_b_uses_displacement = b_uses)
})


# ---- Normalise answer values (TRUE/YES/true 都统一) ----
patterns_corpus <- patterns_corpus %>%
  mutate(
    pattern_a_answer = case_when(
      str_to_upper(pattern_a_answer) %in% c("TRUE", "YES") ~ "YES",
      str_to_upper(pattern_a_answer) %in% c("FALSE", "NO") ~ "NO",
      TRUE ~ NA_character_
    ),
    pattern_b_answer = case_when(
      str_to_upper(pattern_b_answer) %in% c("TRUE", "YES") ~ "YES",
      str_to_upper(pattern_b_answer) %in% c("FALSE", "NO") ~ "NO",
      TRUE ~ NA_character_
    ),
    pattern_b_uses_displacement = case_when(
      str_to_upper(pattern_b_uses_displacement) %in% c("TRUE", "YES") ~ "YES",
      str_to_upper(pattern_b_uses_displacement) %in% c("FALSE", "NO") ~ "NO",
      TRUE ~ NA_character_
    )
  )


# ---- Save + Preview ----
write_csv(patterns_corpus, here(output_dir, "llm_specific_patterns_corpus.csv"))

cat("=== Patterns re-parsed ===\n")
cat("Rows:", nrow(patterns_corpus), "\n\n")

cat("=== Pattern A distribution ===\n")
patterns_corpus %>% count(pattern_a_answer) %>% print()

cat("\n=== Pattern B distribution ===\n")
patterns_corpus %>% count(pattern_b_answer) %>% print()

cat("\n=== Pattern B × displacement usage ===\n")
patterns_corpus %>% count(pattern_b_answer, pattern_b_uses_displacement) %>% print()

# Still NA?
still_na <- patterns_corpus %>%
  filter(is.na(pattern_a_answer)) %>%
  pull(lpa_number)

cat("\n=== Still NA pattern_a projects ===\n")
cat("Count:", length(still_na), "\n")
if (length(still_na) > 0) print(head(still_na, 5))
# =============================================================
# STAGE 20 — 59 CORPUS: ROUND 2 (INDICATORS EXTRACTION)
# =============================================================
# Same prompt as sub-sample Round 2 (Stage 14).
# Estimated cost: $0.30-0.50 USD
# Cache-protected: skips API if rds exists.
# =============================================================

rds_path_v2_corpus <- here(output_dir, "llm_results_v2_corpus_raw.rds")

if (file.exists(rds_path_v2_corpus)) {
  cat("\n>>> Round 2 (corpus): loading cached results, no API call <<<\n")
  results_v2_corpus <- readRDS(rds_path_v2_corpus)
} else {
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "") stop("OPENAI_API_KEY not set")
  
  model_name <- "gpt-4o-mini"
  
  system_prompt_v2 <- "You are analysing a UK planning application document. Extract specific quantitative and categorical indicators. Return ONLY a single valid JSON object. Use null for values not explicitly stated in the document."
  
  user_prompt_v2_template <- 'Extract indicators from this planning document. Use null if not stated. Do not infer.

INDICATORS:
1. site_area_hectares (convert sqm/10000)
2. dwellings_proposed_total (integer)
3. building_height_max_storeys (integer)
4. building_height_max_metres (numeric)
5. affordable_units_total (integer)
6. affordable_percentage (numeric)
7. affordable_basis: "units" / "habitable_rooms" / "floorspace" / "unclear" / null
8. tenure_social_rent_pct (numeric)
9. tenure_intermediate_pct (numeric)
10. public_realm_area_sqm (numeric)
11. community_facilities_provided (array of strings)
12. breeam_rating_target (string or null)
13. sustainability_carbon_reduction_pct (numeric)
14. commercial_floorspace_sqm (numeric)

For each: {"value": <val or null>, "source_quote": <sentence or null>}

OUTPUT: JSON only.

===DOCUMENT===
{DOCUMENT_TEXT}'
  
  call_openai_v2 <- function(document_text, lpa_id) {
    user_prompt <- str_replace(user_prompt_v2_template,
                               fixed("{DOCUMENT_TEXT}"), document_text)
    body <- list(model = model_name,
                 messages = list(list(role = "system", content = system_prompt_v2),
                                 list(role = "user", content = user_prompt)),
                 response_format = list(type = "json_object"),
                 temperature = 0)
    response <- POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(Authorization = paste("Bearer", api_key),
                  `Content-Type` = "application/json"),
      body = toJSON(body, auto_unbox = TRUE),
      encode = "raw", timeout(300))
    
    if (status_code(response) != 200) {
      return(list(lpa_id = lpa_id, status = "ERROR",
                  error = content(response, as = "text"), output = NA))
    }
    parsed <- content(response, as = "parsed")
    list(lpa_id = lpa_id, status = "OK",
         output = parsed$choices[[1]]$message$content,
         total_cost = parsed$usage$prompt_tokens * 0.15 / 1e6 + 
           parsed$usage$completion_tokens * 0.60 / 1e6)
  }
  
  cat("\n=== Starting Round 2 batch (59 projects) ===\n")
  results_v2_corpus <- list()
  for (i in seq_len(nrow(project_texts_corpus))) {
    proj <- project_texts_corpus$lpa_number[i]
    cat(sprintf("[%d/%d] %s...\n", i, nrow(project_texts_corpus), proj))
    results_v2_corpus[[proj]] <- call_openai_v2(
      project_texts_corpus$combined_text[i], proj
    )
    saveRDS(results_v2_corpus, rds_path_v2_corpus)
    Sys.sleep(1)
  }
}

cat("\n=== Round 2 (corpus) complete ===\n")
n_ok <- sum(map_chr(results_v2_corpus, "status") == "OK")
cat(sprintf("Success: %d/%d\n", n_ok, length(results_v2_corpus)))
total_cost_r2 <- sum(map_dbl(results_v2_corpus, 
                             ~ if(.$status == "OK") .$total_cost else 0))
cat(sprintf("Actual Round 2 cost: $%.4f USD\n", total_cost_r2))

 