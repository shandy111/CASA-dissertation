# =============================================================
# 12_semantic_recode_full.R
# =============================================================
# Full-scale semantic re-code: 56 remaining projects × 4 narratives
# (pilot 3 projects reused from semantic_pilot_v3.csv)
#
# Cap: 10 mentions per project × narrative (random sample, seed=42)
# Two-pass: independent frame + target coding
# Cost estimate: ~$1-1.5
# Time estimate: ~5 hours (2-sec sleep between calls)
# =============================================================

source(here::here("scripts", "00_setup.R"))
library(httr)
library(jsonlite)


# ---- Load PDF texts + master ----

pdf_texts <- readRDS(here(output_dir, "pdf_extracted_text_corpus.rds"))

corpus_master <- read_csv(here(output_dir, "master_pairing_data_corpus.csv"),
                          show_col_types = FALSE)
all_lpas <- corpus_master$lpa_number

# Pilot already done - reuse those 3
pilot_lpas <- c("PA/14/00944", "PA/13/02966", "PA/24/00922")
remaining_lpas <- setdiff(all_lpas, pilot_lpas)

cat("Total corpus:", length(all_lpas), "\n")
cat("Pilot already done:", length(pilot_lpas), "\n")
cat("To code now:", length(remaining_lpas), "\n\n")


# ---- Narratives + keyword patterns ----

focus_narratives <- c("regeneration", "community", "public_realm", "sustainability")

keyword_patterns <- list(
  regeneration = "\\bregenerat\\w*",
  community = "\\bcommunit\\w*",
  public_realm = "\\bpublic\\s+realm\\b|\\bpublic\\s+space\\b",
  sustainability = "\\bsustainab\\w*"
)


# ---- Regex enumerate + random sample cap ----

extract_mentions <- function(text, pattern, window_chars = 250) {
  matches <- str_locate_all(text, regex(pattern, ignore_case = TRUE))[[1]]
  if (nrow(matches) == 0) return(character(0))
  
  contexts <- map_chr(seq_len(nrow(matches)), function(i) {
    start <- max(1, matches[i, "start"] - window_chars)
    end <- min(nchar(text), matches[i, "end"] + window_chars)
    str_sub(text, start, end)
  })
  
  if (length(contexts) > 1) {
    keep <- rep(TRUE, length(contexts))
    for (i in 2:length(contexts)) {
      if (matches[i, "start"] - matches[i-1, "start"] < 200) {
        keep[i] <- FALSE
      }
    }
    contexts <- contexts[keep]
  }
  
  contexts
}

# Build mention list with cap
MAX_MENTIONS <- 10
set.seed(42)

mention_list <- list()
for (lpa in remaining_lpas) {
  text <- pdf_texts[[lpa]]
  if (is.null(text)) next
  
  for (nar in focus_narratives) {
    contexts <- extract_mentions(text, keyword_patterns[[nar]])
    n_total <- length(contexts)
    
    if (n_total == 0) next
    
    # Random sample cap
    if (n_total > MAX_MENTIONS) {
      idx <- sort(sample(seq_len(n_total), MAX_MENTIONS))
      contexts <- contexts[idx]
    }
    
    cat(sprintf("%s x %s: %d/%d sampled\n", lpa, nar, length(contexts), n_total))
    
    for (i in seq_along(contexts)) {
      mention_list[[length(mention_list) + 1]] <- tibble(
        lpa_number = lpa,
        narrative = nar,
        mention_id = i,
        n_total_in_project = n_total,
        context = contexts[i]
      )
    }
  }
}

mentions_df <- bind_rows(mention_list)

cat(sprintf("\nTotal mentions to code (56 projects, cap=10): %d\n\n", 
            nrow(mentions_df)))


# ---- Prompts (reuse from pilot v3) ----

build_frame_prompt <- function(narrative, context) {
  paste0(
    'You are analyzing a UK planning statement sentence containing the word "', narrative, '".\n\n',
    'Your task is to code the FRAMING LANGUAGE used in this sentence -- what type of VOCABULARY does the sentence use?\n\n',
    'Return one of:\n',
    '- "economic_frame": vocabulary emphasizes economic terms (investment, jobs, revenue, commercial value, property yield, market, growth, GVA, finance)\n',
    '- "social_frame": vocabulary emphasizes social terms (community, residents, wellbeing, heritage identity, equity, cultural value, public benefit, families)\n',
    '- "mixed_frame": both economic and social vocabularies substantively present\n',
    '- "not_clear": procedural/technical/design description without value-laden vocabulary\n\n',
    '## IMPORTANT: Distinguish framing (what words are used) from what is claimed\n',
    '- Focus on: what LANGUAGE the sentence uses\n',
    '- Do NOT infer from what the sentence claims will happen\n',
    '- Just: which vocabulary type is the sentence written in?\n\n',
    '## Worked examples:\n\n',
    'Example 1: "The 500 million pound investment will create 300 construction jobs and generate 15M annual tax revenue."\n',
    '- frame: economic_frame (all vocabulary is investment/jobs/revenue)\n\n',
    'Example 2: "The redevelopment will strengthen the community and provide affordable homes for local families."\n',
    '- frame: social_frame (all vocabulary is community/families/affordable)\n\n',
    'Example 3: "The 500M investment will deliver 30 percent affordable housing and community facilities."\n',
    '- frame: economic_frame (words used are: 500M, investment -- economic vocabulary -- even though the OUTCOMES claimed are social)\n\n',
    'Example 4: "Community renewal will unlock commercial vibrancy and job creation."\n',
    '- frame: social_frame (words used are: community, renewal -- social vocabulary -- even though the OUTCOMES claimed are economic)\n\n',
    'Example 5: "The 120M investment will drive community regeneration through jobs, homes, and public realm."\n',
    '- frame: mixed_frame (uses both 120M/investment/jobs AND community/homes/public realm)\n\n',
    'Example 6: "The regeneration strategy is set out in Chapter 4 of this statement."\n',
    '- frame: not_clear (procedural/administrative, no value framing)\n\n',
    'Example 7: "Air Source Heat Pumps will provide the primary heating solution for the development."\n',
    '- frame: not_clear (technical specification, no value framing)\n\n',
    '## Now code this sentence:\n\n',
    'Narrative keyword: ', narrative, '\n',
    'Sentence context: "', str_replace_all(context, '"', "'"), '"\n\n',
    'Return ONLY this JSON:\n',
    '{\n',
    '  "frame": "economic_frame|social_frame|mixed_frame|not_clear",\n',
    '  "reasoning": "brief explanation citing which vocabulary type"\n',
    '}'
  )
}

build_target_prompt <- function(narrative, context) {
  paste0(
    'You are analyzing a UK planning statement sentence containing the word "', narrative, '".\n\n',
    'Your task is to code what OUTCOME the sentence claims -- what will be DELIVERED or ACHIEVED?\n\n',
    'Return one of:\n',
    '- "economic_outcome": claims economic result (jobs, investment, commercial floorspace, property value uplift, tax revenue, growth, business creation)\n',
    '- "social_outcome": claims social result (affordable homes, community facilities, public realm, heritage protection, resident wellbeing, cultural value)\n',
    '- "mixed_outcome": explicitly claims both economic AND social results\n',
    '- "not_clear": no specific claim about outcome, or purely procedural/technical\n\n',
    '## IMPORTANT: Distinguish target (what is claimed) from framing (what words are used)\n',
    '- Focus on: what the sentence claims will be PRODUCED or DELIVERED\n',
    '- Do NOT infer from the vocabulary type\n',
    '- Just: what outcome does the sentence promise?\n\n',
    '## Worked examples:\n\n',
    'Example 1: "The 500 million pound investment will create 300 construction jobs and generate 15M annual tax revenue."\n',
    '- target: economic_outcome (claims: jobs + tax revenue)\n\n',
    'Example 2: "The redevelopment will strengthen the community and provide affordable homes for local families."\n',
    '- target: social_outcome (claims: community + affordable homes)\n\n',
    'Example 3: "The 500M investment will deliver 30 percent affordable housing and community facilities."\n',
    '- target: social_outcome (claims: affordable housing + community facilities -- even though vocabulary is economic)\n\n',
    'Example 4: "Community renewal will unlock commercial vibrancy and job creation."\n',
    '- target: economic_outcome (claims: commercial vibrancy + jobs -- even though vocabulary is social)\n\n',
    'Example 5: "The 120M investment will drive community regeneration through jobs, homes, and public realm."\n',
    '- target: mixed_outcome (claims jobs AND homes AND public realm)\n\n',
    'Example 6: "The regeneration strategy is set out in Chapter 4 of this statement."\n',
    '- target: not_clear (procedural, no outcome claim)\n\n',
    'Example 7: "Air Source Heat Pumps will provide the primary heating solution for the development."\n',
    '- target: not_clear (technical specification, no substantive outcome claim)\n\n',
    '## Now code this sentence:\n\n',
    'Narrative keyword: ', narrative, '\n',
    'Sentence context: "', str_replace_all(context, '"', "'"), '"\n\n',
    'Return ONLY this JSON:\n',
    '{\n',
    '  "target": "economic_outcome|social_outcome|mixed_outcome|not_clear",\n',
    '  "reasoning": "brief explanation citing what is being claimed"\n',
    '}'
  )
}


# ---- API call ----

call_openai <- function(prompt, max_retries = 3) {
  for (attempt in 1:max_retries) {
    response <- tryCatch({
      POST(
        url = "https://api.openai.com/v1/chat/completions",
        add_headers(
          Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
          "Content-Type" = "application/json"
        ),
        body = toJSON(list(
          model = "gpt-4o-mini",
          messages = list(list(role = "user", content = prompt)),
          response_format = list(type = "json_object"),
          temperature = 0.1,
          max_tokens = 300
        ), auto_unbox = TRUE),
        timeout(60)  # increased from 30 to 60
      )
    }, error = function(e) {
      cat(sprintf("  [attempt %d] Error: %s\n", attempt, e$message))
      NULL
    })
    
    if (!is.null(response) && status_code(response) == 200) {
      result <- content(response)
      return(list(
        text = result$choices[[1]]$message$content,
        tokens_in = result$usage$prompt_tokens,
        tokens_out = result$usage$completion_tokens
      ))
    }
    
    # Retry after backoff
    if (attempt < max_retries) {
      backoff <- attempt * 10
      cat(sprintf("  [retry after %d sec]\n", backoff))
      Sys.sleep(backoff)
    }
  }
  
  return(list(error = "api_error_after_retries"))
}
# ---- Load cache if exists (incremental save) ----

cache_path <- here(output_dir, "semantic_full_v3_raw.rds")

if (file.exists(cache_path)) {
  cat("=== Loading cached results ===\n")
  full_results <- readRDS(cache_path)
  cat("Cached rows:", nrow(full_results), "\n\n")
  
  # 找 not yet coded
  coded_ids <- full_results %>%
    filter(!is.na(frame), !is.na(target)) %>%
    mutate(key = paste(lpa_number, narrative, mention_id, sep = "|")) %>%
    pull(key)
  
  mentions_df$key <- paste(mentions_df$lpa_number, mentions_df$narrative, 
                           mentions_df$mention_id, sep = "|")
  to_code <- mentions_df %>% filter(!key %in% coded_ids) %>% select(-key)
  
  cat("Remaining to code:", nrow(to_code), "\n\n")
} else {
  full_results <- mentions_df %>%
    mutate(frame = NA_character_, frame_reasoning = NA_character_,
           target = NA_character_, target_reasoning = NA_character_)
  to_code <- mentions_df
}


# ---- Run 2-pass on all remaining mentions ----

cat("=== Coding: 2-pass on remaining mentions ===\n\n")

total_tokens_in <- 0
total_tokens_out <- 0

for (i in seq_len(nrow(to_code))) {
  ctx <- to_code$context[i]
  nar <- to_code$narrative[i]
  lpa <- to_code$lpa_number[i]
  mid <- to_code$mention_id[i]
  
  # Pass 1
  prompt1 <- build_frame_prompt(nar, ctx)
  r1 <- call_openai(prompt1)
  
  frame_val <- NA_character_
  frame_reasoning <- NA_character_
  if (is.null(r1$error)) {
    parsed1 <- tryCatch(fromJSON(r1$text), error = function(e) NULL)
    if (!is.null(parsed1)) {
      frame_val <- parsed1$frame %||% NA
      frame_reasoning <- parsed1$reasoning %||% NA
    }
    total_tokens_in <- total_tokens_in + r1$tokens_in
    total_tokens_out <- total_tokens_out + r1$tokens_out
  }
  
  Sys.sleep(2)
  
  # Pass 2
  prompt2 <- build_target_prompt(nar, ctx)
  r2 <- call_openai(prompt2)
  
  target_val <- NA_character_
  target_reasoning <- NA_character_
  if (is.null(r2$error)) {
    parsed2 <- tryCatch(fromJSON(r2$text), error = function(e) NULL)
    if (!is.null(parsed2)) {
      target_val <- parsed2$target %||% NA
      target_reasoning <- parsed2$reasoning %||% NA
    }
    total_tokens_in <- total_tokens_in + r2$tokens_in
    total_tokens_out <- total_tokens_out + r2$tokens_out
  }
  
  # Update in full_results
  match_idx <- which(full_results$lpa_number == lpa & 
                       full_results$narrative == nar & 
                       full_results$mention_id == mid)
  if (length(match_idx) > 0) {
    full_results$frame[match_idx] <- frame_val
    full_results$frame_reasoning[match_idx] <- frame_reasoning
    full_results$target[match_idx] <- target_val
    full_results$target_reasoning[match_idx] <- target_reasoning
  }
  
  cat(sprintf("[%d/%d] %s x %s#%d -- frame:%s | target:%s\n",
              i, nrow(to_code), lpa, nar, mid,
              frame_val %||% "NA", target_val %||% "NA"))
  
  # Save every 50 iterations
  if (i %% 50 == 0) {
    saveRDS(full_results, cache_path)
    cat("  [cached]\n")
  }
  
  Sys.sleep(2)
}

# Final save
saveRDS(full_results, cache_path)

cost_in <- total_tokens_in / 1e6 * 0.15
cost_out <- total_tokens_out / 1e6 * 0.60
cat(sprintf("\n=== Session cost: $%.3f ===\n", cost_in + cost_out))


# ---- Merge pilot data with full data ----

pilot_data <- read_csv(here(output_dir, "semantic_pilot_v3.csv"),
                       show_col_types = FALSE) %>%
  mutate(n_total_in_project = NA_integer_) %>%
  select(lpa_number, narrative, mention_id, n_total_in_project, 
         context, frame, frame_reasoning, target, target_reasoning)

combined <- bind_rows(full_results, pilot_data)

cat("\n=== Combined dataset ===\n")
cat("Total instances:", nrow(combined), "\n")
cat("Projects covered:", n_distinct(combined$lpa_number), "\n")


# ---- Summary ----

cat("\n=== Frame x Target packaging matrix (full corpus) ===\n")
combined %>%
  filter(!is.na(frame), !is.na(target)) %>%
  count(frame, target) %>%
  pivot_wider(names_from = target, values_from = n, values_fill = 0) %>%
  print()

cat("\n=== Off-diagonal analysis ===\n")
off_diag <- combined %>%
  filter(!is.na(frame), !is.na(target)) %>%
  filter(!(
    (frame == "economic_frame" & target == "economic_outcome") |
      (frame == "social_frame" & target == "social_outcome") |
      (frame == "mixed_frame" & target == "mixed_outcome") |
      (frame == "not_clear" & target == "not_clear")
  )) %>%
  nrow()

total_coded <- combined %>% filter(!is.na(frame), !is.na(target)) %>% nrow()

cat(sprintf("Off-diagonal: %d / %d (%.1f%%)\n",
            off_diag, total_coded, off_diag / total_coded * 100))


# ---- Save final combined ----

write_csv(combined, here(output_dir, "llm_semantic_frame_v3_full.csv"))
cat("\nSaved: output/llm_semantic_frame_v3_full.csv\n")