# =============================================================
# Instance-level semantic re-code of 12 narratives.
# Each keyword mention gets: framing + target + confidence + quote
#
# Rationale: Supervisor pushback (twice) on economic vs social 
# distinction. Watt`` 2013 / Colomb 2007 / Slater 2006 argue 
# regeneration discourse packages both dimensions but delivers 
# uneven outcomes. Dissertation needs to distinguish framings AND
# targets, at instance level (not narrative-level label).
#
# Input:  data/planning_statements/ (77 PDFs, 61 projects)
#         output/pdf_extracted_text_corpus.rds (cached)
# Output: output/semantic_recode_v2_raw.rds (API cache)
#         output/llm_semantic_frame.csv (instance-level table)
# =============================================================

source(here::here("scripts", "00_setup.R"))
library(httr)
library(jsonlite)

# API key (should be in .Renviron or set here)
# Sys.setenv(OPENAI_API_KEY = "sk-...")


# ---- Load corpus + PDF text ----

corpus_master <- read_csv(here(output_dir, "master_pairing_data_corpus.csv"),
                          show_col_types = FALSE)

corpus_lpas <- corpus_master$lpa_number
cat("Corpus size:", length(corpus_lpas), "\n\n")


# ---- Reuse PDF extraction (cached from earlier) ----

pdf_text_path <- here(output_dir, "pdf_extracted_text_corpus.rds")

if (file.exists(pdf_text_path)) {
  cat("Loading cached PDF text...\n")
  pdf_texts <- readRDS(pdf_text_path)
  cat("Loaded texts for", length(pdf_texts), "projects\n\n")
} else {
  # =============================================================
  # Rebuild PDF text extraction (was used in-memory in 06 but not cached)
  # =============================================================
  library(pdftools)
  
  cat("=== Rebuilding PDF text extraction ===\n")
  
  pdf_dir <- here("data", "planning_statements")
  
  lpa_to_pdfs <- function(lpa) {
    lpa_underscore <- str_replace_all(lpa, "/", "_")
    all_pdfs <- list.files(pdf_dir, 
                           pattern = paste0("^", lpa_underscore, "_\\d+\\.pdf$"),
                           full.names = TRUE)
    all_pdfs
  }
  
  extract_project_text <- function(lpa) {
    pdfs <- lpa_to_pdfs(lpa)
    if (length(pdfs) == 0) return(NULL)
    
    texts <- map_chr(pdfs, function(pdf) {
      tryCatch({
        pages <- pdf_text(pdf)
        text <- paste(pages, collapse = "\n")
        if (nchar(text) > 380000) text <- str_sub(text, 1, 380000)
        text
      }, error = function(e) "")
    })
    
    combined <- paste(texts, collapse = "\n===FILE BREAK===\n")
    if (nchar(combined) > 500000) combined <- str_sub(combined, 1, 500000)
    combined
  }
  
  pdf_texts <- list()
  for (i in seq_along(corpus_lpas)) {
    lpa <- corpus_lpas[i]
    text <- extract_project_text(lpa)
    
    if (is.null(text) || nchar(text) < 500) {
      cat(sprintf("[%d/%d] %s - NO TEXT (or too short)\n", 
                  i, length(corpus_lpas), lpa))
      pdf_texts[[lpa]] <- NULL
    } else {
      cat(sprintf("[%d/%d] %s - OK (%d chars)\n", 
                  i, length(corpus_lpas), lpa, nchar(text)))
      pdf_texts[[lpa]] <- text
    }
  }
  
  saveRDS(pdf_texts, pdf_text_path)
  cat(sprintf("\n=== Cached %d projects to disk ===\n\n", length(pdf_texts)))
}


# ---- Semantic re-code prompt (v2 - instance-level) ----

build_prompt <- function(text) {
  text_truncated <- str_sub(text, 1, 100000)
  
  sprintf('You are analyzing a UK planning statement. Your task is to identify how 12 development narratives are FRAMED and what they are used to JUSTIFY.

For each narrative below, first determine if it is MENTIONED in the document. If yes, identify up to 5 illustrative sentences (spread across the document) where the narrative appears, and code each sentence-level instance.

The 12 narratives:
1. regeneration
2. redevelopment
3. mixed_use
4. community
5. sustainability
6. connectivity
7. public_realm
8. affordable
9. high_density
10. placemaking
11. heritage
12. displacement

For each sentence-level instance, code THREE dimensions:

## DIMENSION 1 — Framing language:
- "economic_frame": justification uses economic vocabulary (investment, jobs, tax revenue, commercial value, property yield, market efficiency, growth, GVA)
- "social_frame": justification uses social vocabulary (community, residents, equity, wellbeing, heritage value, cultural identity, public benefit)
- "mixed_frame": both economic and social vocabularies substantively present in the sentence
- "not_clear": narrative appears but framing is too brief/ambiguous to classify

## DIMENSION 2 — Justification target (what outcome is claimed):
- "economic_outcome": justification claims economic result (jobs, investment, commercial floorspace, property value uplift, growth)
- "social_outcome": justification claims social result (affordable homes, community facilities, public realm, heritage protection, resident wellbeing)
- "mixed_outcome": explicitly claims both economic and social outcomes
- "not_clear": target outcome not clearly identifiable

## DIMENSION 3 — Confidence:
- "high": framing/target unambiguous from sentence content
- "medium": framing/target inferable but not fully explicit
- "low": framing/target uncertain

For each instance also provide the quote sentence (max 40 words).

===DOCUMENT===
%s

===OUTPUT FORMAT===
Return ONE JSON object with exactly this schema:

{
  "regeneration": {
    "mentioned": true/false,
    "instances": [
      {
        "quote": "...",
        "frame": "economic_frame|social_frame|mixed_frame|not_clear",
        "target": "economic_outcome|social_outcome|mixed_outcome|not_clear",
        "confidence": "high|medium|low"
      }
    ]
  },
  "redevelopment": { ... same schema ... },
  "mixed_use": { ... },
  "community": { ... },
  "sustainability": { ... },
  "connectivity": { ... },
  "public_realm": { ... },
  "affordable": { ... },
  "high_density": { ... },
  "placemaking": { ... },
  "heritage": { ... },
  "displacement": { ... }
}

Rules:
- If mentioned=false, "instances" should be [] (empty array).
- Sample AT MOST 5 instances per narrative, spread across the document.
- Each instance quote must be a single sentence, max 40 words.
- Return ONLY the JSON, no explanation.', text_truncated)
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
      messages = list(
        list(role = "user", content = prompt)
      ),
      response_format = list(type = "json_object"),
      temperature = 0.1,
      max_tokens = 4000
    ), auto_unbox = TRUE),
    timeout(60)
  )
  
  if (status_code(response) != 200) {
    return(list(error = content(response, "text")))
  }
  
  result <- content(response)
  list(
    text = result$choices[[1]]$message$content,
    tokens_input = result$usage$prompt_tokens,
    tokens_output = result$usage$completion_tokens
  )
}


# ---- Run on 59 corpus (with rds cache) ----

cache_path <- here(output_dir, "semantic_recode_v2_raw.rds")

if (file.exists(cache_path)) {
  cat("=== Loading cached results (avoid re-charging API) ===\n")
  semantic_raw <- readRDS(cache_path)
  cat("Cached results:", length(semantic_raw), "projects\n\n")
} else {
  cat("=== Fresh API call - will charge OpenAI ===\n\n")
  
  semantic_raw <- list()
  total_tokens_in <- 0
  total_tokens_out <- 0
  
  for (i in seq_along(corpus_lpas)) {
    lpa <- corpus_lpas[i]
    text <- pdf_texts[[lpa]]
    
    if (is.null(text) || nchar(text) < 500) {
      cat(sprintf("[%d/%d] %s - SKIP (no text)\n", i, length(corpus_lpas), lpa))
      semantic_raw[[lpa]] <- list(error = "no text")
      next
    }
    
    prompt <- build_prompt(text)
    result <- call_openai(prompt)
    
    if (!is.null(result$error)) {
      cat(sprintf("[%d/%d] %s - ERROR: %s\n", i, length(corpus_lpas), lpa,
                  str_sub(result$error, 1, 60)))
      semantic_raw[[lpa]] <- result
      Sys.sleep(60)
      next
    }
    
    total_tokens_in <- total_tokens_in + result$tokens_input
    total_tokens_out <- total_tokens_out + result$tokens_output
    
    cat(sprintf("[%d/%d] %s - OK (in:%d out:%d)\n", 
                i, length(corpus_lpas), lpa,
                result$tokens_input, result$tokens_output))
    
    semantic_raw[[lpa]] <- result
    
    # Intermediate save every 10 projects
    if (i %% 10 == 0) {
      saveRDS(semantic_raw, cache_path)
      cat("  [intermediate cache saved]\n")
    }
    
    Sys.sleep(30)
  }
  
  # Final save
  saveRDS(semantic_raw, cache_path)
  
  cost_in <- total_tokens_in / 1e6 * 0.15
  cost_out <- total_tokens_out / 1e6 * 0.60
  cat(sprintf("\n=== Total cost: $%.3f (in:$%.3f + out:$%.3f) ===\n",
              cost_in + cost_out, cost_in, cost_out))
  cat(sprintf("Total tokens: in=%d, out=%d\n", total_tokens_in, total_tokens_out))
}


# ---- Parse results (instance-level) ----

cat("\n=== Parsing results ===\n")

parse_one <- function(lpa, result) {
  if (!is.null(result$error)) {
    return(tibble(lpa_number = lpa, status = "error", 
                  narrative = NA, instance_id = NA))
  }
  
  parsed <- tryCatch(
    fromJSON(result$text, simplifyDataFrame = FALSE),
    error = function(e) NULL
  )
  
  if (is.null(parsed)) {
    return(tibble(lpa_number = lpa, status = "parse_fail",
                  narrative = NA, instance_id = NA))
  }
  
  all_instances <- list()
  
  for (nar in names(parsed)) {
    entry <- parsed[[nar]]
    
    if (isTRUE(entry$mentioned) && length(entry$instances) > 0) {
      for (i in seq_along(entry$instances)) {
        inst <- entry$instances[[i]]
        all_instances[[length(all_instances) + 1]] <- tibble(
          lpa_number = lpa,
          status = "ok",
          narrative = nar,
          instance_id = i,
          quote = inst$quote %||% NA,
          frame = inst$frame %||% NA,
          target = inst$target %||% NA,
          confidence = inst$confidence %||% NA
        )
      }
    } else {
      # Not mentioned - still record with instance_id=0
      all_instances[[length(all_instances) + 1]] <- tibble(
        lpa_number = lpa,
        status = "ok",
        narrative = nar,
        instance_id = 0,
        quote = NA,
        frame = NA,
        target = NA,
        confidence = NA
      )
    }
  }
  
  bind_rows(all_instances)
}

semantic_df <- map2_dfr(names(semantic_raw), semantic_raw, parse_one)

cat("Parse status:\n")
semantic_df %>% distinct(lpa_number, status) %>% count(status) %>% print()


# ---- Instance-level distribution summary ----

instance_df <- semantic_df %>% filter(status == "ok", instance_id > 0)

cat("\n=== Total instances coded ===\n")
cat("Total:", nrow(instance_df), "\n")
cat("Projects with any instance:", 
    n_distinct(instance_df$lpa_number), "\n\n")

cat("=== Instances per narrative ===\n")
instance_df %>% count(narrative) %>% arrange(desc(n)) %>% print(n = Inf)

cat("\n=== Frame distribution (across all instances) ===\n")
instance_df %>% count(frame) %>% 
  mutate(pct = round(n/sum(n)*100, 1)) %>% print()

cat("\n=== Target distribution (across all instances) ===\n")
instance_df %>% count(target) %>% 
  mutate(pct = round(n/sum(n)*100, 1)) %>% print()

cat("\n=== Confidence distribution ===\n")
instance_df %>% count(confidence) %>% 
  mutate(pct = round(n/sum(n)*100, 1)) %>% print()

cat("\n=== Framing × Target packaging matrix ===\n")
instance_df %>%
  filter(!is.na(frame), !is.na(target)) %>%
  count(frame, target) %>%
  pivot_wider(names_from = target, values_from = n, values_fill = 0) %>%
  print()


# ---- Save ----

write_csv(semantic_df, here(output_dir, "llm_semantic_frame.csv"))
cat("\n=== Saved: output/llm_semantic_frame.csv ===\n")
cat("Format: instance-level (multiple rows per project × narrative)\n")