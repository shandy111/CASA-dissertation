# =============================================================
# 11_semantic_recode_v3.R
# =============================================================
# PILOT: 3 project × 4 narratives (regeneration, community, 
# public_realm, sustainability), two-pass frame/target coding.
#
# Purpose: verify off-diagonal appears + technical vocab skipped
# Cost: ~$0.30
#
# If pilot passes → scale to 59 project.
# =============================================================

source(here::here("scripts", "00_setup.R"))
library(httr)
library(jsonlite)


# ---- Load PDF texts ----

pdf_texts <- readRDS(here(output_dir, "pdf_extracted_text_corpus.rds"))

# Pilot: 3 project spanning corpus diversity
pilot_lpas <- c("PA/14/00944",   # Royal Mint (heritage-heavy)
                "PA/13/02966",   # Wood Wharf (large mixed-use)
                "PA/24/00922")   # Recent scheme (Teviot Estate)

# 4 focus narratives
focus_narratives <- c("regeneration", "community", "public_realm", "sustainability")


# ---- Regex enumerate: find all mentions of each narrative ----

keyword_patterns <- list(
  regeneration = "\\bregenerat\\w*",
  community = "\\bcommunit\\w*",
  public_realm = "\\bpublic\\s+realm\\b|\\bpublic\\s+space\\b",
  sustainability = "\\bsustainab\\w*"
)

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

mention_list <- list()
for (lpa in pilot_lpas) {
  text <- pdf_texts[[lpa]]
  for (nar in focus_narratives) {
    contexts <- extract_mentions(text, keyword_patterns[[nar]])
    cat(sprintf("%s x %s: %d mentions\n", lpa, nar, length(contexts)))
    
    for (i in seq_along(contexts)) {
      mention_list[[length(mention_list) + 1]] <- tibble(
        lpa_number = lpa,
        narrative = nar,
        mention_id = i,
        context = contexts[i]
      )
    }
  }
}

mentions_df <- bind_rows(mention_list)

cat(sprintf("\nTotal mentions to code: %d\n\n", nrow(mentions_df)))


# ---- Pass 1: FRAME coding prompt ----

build_frame_prompt <- function(narrative, context) {
  prompt_text <- paste0(
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
  prompt_text
}


# ---- Pass 2: TARGET coding prompt ----

build_target_prompt <- function(narrative, context) {
  prompt_text <- paste0(
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
  prompt_text
}


# ---- OpenAI API call ----

call_openai <- function(prompt) {
  response <- POST(
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
    timeout(30)
  )
  
  if (status_code(response) != 200) return(list(error = "api_error"))
  
  result <- content(response)
  list(
    text = result$choices[[1]]$message$content,
    tokens_in = result$usage$prompt_tokens,
    tokens_out = result$usage$completion_tokens
  )
}


# ---- Run pilot: 2-pass on all mentions ----

cat("\n=== Pilot: coding 2-pass on all mentions ===\n\n")

pilot_results <- mentions_df %>%
  mutate(frame = NA_character_, frame_reasoning = NA_character_,
         target = NA_character_, target_reasoning = NA_character_)

total_tokens_in <- 0
total_tokens_out <- 0

for (i in seq_len(nrow(pilot_results))) {
  ctx <- pilot_results$context[i]
  nar <- pilot_results$narrative[i]
  
  # Pass 1
  prompt1 <- build_frame_prompt(nar, ctx)
  r1 <- call_openai(prompt1)
  if (is.null(r1$error)) {
    parsed1 <- tryCatch(fromJSON(r1$text), error = function(e) NULL)
    if (!is.null(parsed1)) {
      pilot_results$frame[i] <- parsed1$frame %||% NA
      pilot_results$frame_reasoning[i] <- parsed1$reasoning %||% NA
    }
    total_tokens_in <- total_tokens_in + r1$tokens_in
    total_tokens_out <- total_tokens_out + r1$tokens_out
  }
  
  Sys.sleep(2)
  
  # Pass 2
  prompt2 <- build_target_prompt(nar, ctx)
  r2 <- call_openai(prompt2)
  if (is.null(r2$error)) {
    parsed2 <- tryCatch(fromJSON(r2$text), error = function(e) NULL)
    if (!is.null(parsed2)) {
      pilot_results$target[i] <- parsed2$target %||% NA
      pilot_results$target_reasoning[i] <- parsed2$reasoning %||% NA
    }
    total_tokens_in <- total_tokens_in + r2$tokens_in
    total_tokens_out <- total_tokens_out + r2$tokens_out
  }
  
  cat(sprintf("[%d/%d] %s x %s#%d -- frame:%s | target:%s\n",
              i, nrow(pilot_results),
              pilot_results$lpa_number[i], nar, pilot_results$mention_id[i],
              pilot_results$frame[i] %||% "NA", 
              pilot_results$target[i] %||% "NA"))
  
  Sys.sleep(2)
}

cost_in <- total_tokens_in / 1e6 * 0.15
cost_out <- total_tokens_out / 1e6 * 0.60
cat(sprintf("\n=== Pilot cost: $%.3f ===\n", cost_in + cost_out))


# ---- Verify pilot: check off-diagonal ----

cat("\n=== Pilot Frame x Target packaging matrix ===\n")
pilot_matrix <- pilot_results %>%
  filter(!is.na(frame), !is.na(target)) %>%
  count(frame, target) %>%
  pivot_wider(names_from = target, values_from = n, values_fill = 0)
print(pilot_matrix)

off_diagonal_count <- pilot_results %>%
  filter(!is.na(frame), !is.na(target)) %>%
  filter(!(
    (frame == "economic_frame" & target == "economic_outcome") |
      (frame == "social_frame" & target == "social_outcome") |
      (frame == "mixed_frame" & target == "mixed_outcome") |
      (frame == "not_clear" & target == "not_clear")
  )) %>%
  nrow()

total_coded <- pilot_results %>% filter(!is.na(frame), !is.na(target)) %>% nrow()

cat(sprintf("\nOff-diagonal instances: %d / %d (%.1f%%)\n",
            off_diagonal_count, total_coded, 
            off_diagonal_count / total_coded * 100))

if (off_diagonal_count / total_coded >= 0.10) {
  cat("\nPILOT PASSED -- off-diagonal appears, ready to scale to 59 projects\n")
} else {
  cat("\nPILOT FAILED -- LLM still conflating, need further prompt refinement\n")
}


# ---- Save pilot for inspection ----

write_csv(pilot_results, here(output_dir, "semantic_pilot_v3.csv"))
cat("\nSaved: output/semantic_pilot_v3.csv\n")