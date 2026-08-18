# =============================================================
# 13_semantic_recode_v4_pilot.R
# Step 1: Build sentence-level pilot sample
#
# IMPORTANT:
# - This script does NOT modify or delete any v3 files.
# - This script does NOT reuse v3 coding results.
# - All new outputs use the semantic_v4_pilot prefix.
# =============================================================


# ---- Setup ----

source(here::here("scripts", "00_setup.R"))

library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tidyr)
library(tibble)
library(stringi)


# ---- Pilot settings ----

pilot_lpas <- c(
  "PA/14/00944",
  "PA/13/02966",
  "PA/24/00922"
)

focus_narratives <- c(
  "regeneration",
  "community",
  "public_realm",
  "sustainability"
)

keyword_patterns <- list(
  regeneration = "\\bregenerat\\w*",
  community = "\\bcommunit\\w*",
  public_realm = "\\bpublic\\s+realm\\b|\\bpublic\\s+space\\b",
  sustainability = "\\bsustainab\\w*"
)

MAX_MENTIONS <- 10
RANDOM_SEED <- 42


# ---- Output filenames ----
# These are new files and will not overwrite v3 results.

pilot_mentions_csv <- here(
  output_dir,
  "semantic_v4_pilot_mentions.csv"
)

pilot_mentions_rds <- here(
  output_dir,
  "semantic_v4_pilot_mentions.rds"
)


# ---- Load existing data ----

pdf_texts <- readRDS(
  here(output_dir, "pdf_extracted_text_corpus.rds")
)

corpus_master <- read_csv(
  here(output_dir, "master_pairing_data_corpus.csv"),
  show_col_types = FALSE
)


# ---- Check required pilot projects ----

master_lpas <- unique(corpus_master$lpa_number)

missing_from_master <- setdiff(
  pilot_lpas,
  master_lpas
)

missing_from_texts <- setdiff(
  pilot_lpas,
  names(pdf_texts)
)

if (length(missing_from_master) > 0) {
  stop(
    "These pilot LPAs are missing from corpus_master: ",
    paste(missing_from_master, collapse = ", ")
  )
}

if (length(missing_from_texts) > 0) {
  stop(
    "These pilot LPAs are missing from pdf_texts: ",
    paste(missing_from_texts, collapse = ", ")
  )
}

cat("Pilot projects found:", length(pilot_lpas), "\n")
cat(paste0("  - ", pilot_lpas, collapse = "\n"), "\n\n")


# =============================================================
# Text-cleaning functions
# =============================================================

normalize_whitespace <- function(x) {
  x %>%
    str_replace_all("[\\r\\n\\t]+", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}


clean_pdf_text <- function(text) {
  
  # Make sure the PDF text is a single character string
  text <- paste(text, collapse = "\n")
  
  # Remove page-break characters
  text <- str_replace_all(text, "\f", "\n")
  
  # Join words broken across PDF line endings:
  # e.g. "sustain-\nability" becomes "sustainability"
  text <- str_replace_all(
    text,
    "([[:alpha:]])-\\s*\\n\\s*([[:alpha:]])",
    "\\1\\2"
  )
  
  # Convert remaining line breaks to spaces
  text <- str_replace_all(text, "[\\r\\n]+", " ")
  
  # Remove repeated spaces
  text <- str_replace_all(text, "\\s+", " ")
  
  str_trim(text)
}


split_into_sentences <- function(text) {
  
  cleaned_text <- clean_pdf_text(text)
  
  sentences <- stringi::stri_split_boundaries(
    cleaned_text,
    type = "sentence"
  )[[1]]
  
  sentences <- normalize_whitespace(sentences)
  
  # Remove empty units
  sentences <- sentences[
    !is.na(sentences) &
      sentences != ""
  ]
  
  sentences
}


# =============================================================
# Extract sentence-level narrative mentions
# =============================================================

extract_claim_mentions <- function(
    text,
    pattern,
    context_sentences = 1
) {
  
  sentences <- split_into_sentences(text)
  
  if (length(sentences) == 0) {
    return(tibble())
  }
  
  hit_indices <- which(
    str_detect(
      sentences,
      regex(pattern, ignore_case = TRUE)
    )
  )
  
  if (length(hit_indices) == 0) {
    return(tibble())
  }
  
  extracted <- map_dfr(
    hit_indices,
    function(sentence_index) {
      
      before_start <- max(
        1,
        sentence_index - context_sentences
      )
      
      after_end <- min(
        length(sentences),
        sentence_index + context_sentences
      )
      
      before_indices <- seq.int(
        before_start,
        sentence_index - 1
      )
      
      after_indices <- seq.int(
        sentence_index + 1,
        after_end
      )
      
      # seq.int can create unwanted values when the range is empty,
      # so check the boundaries explicitly.
      context_before <- if (sentence_index > before_start) {
        paste(
          sentences[before_indices],
          collapse = " "
        )
      } else {
        ""
      }
      
      context_after <- if (sentence_index < after_end) {
        paste(
          sentences[after_indices],
          collapse = " "
        )
      } else {
        ""
      }
      
      claim_text <- sentences[sentence_index]
      
      # Keep a combined context column for compatibility with the
      # previous script, but mark the actual coding unit explicitly.
      combined_context <- paste(
        if (context_before != "") {
          paste0("CONTEXT BEFORE: ", context_before)
        } else {
          NULL
        },
        paste0("CLAIM TEXT: ", claim_text),
        if (context_after != "") {
          paste0("CONTEXT AFTER: ", context_after)
        } else {
          NULL
        },
        collapse = "\n\n"
      )
      
      tibble(
        source_sentence_index = sentence_index,
        claim_text = claim_text,
        context_before = context_before,
        context_after = context_after,
        context = combined_context
      )
    }
  )
  
  # Remove exact duplicated claim sentences within the same
  # project × narrative.
  extracted %>%
    mutate(
      claim_text_normalized = claim_text %>%
        str_to_lower() %>%
        normalize_whitespace()
    ) %>%
    distinct(
      claim_text_normalized,
      .keep_all = TRUE
    ) %>%
    select(-claim_text_normalized)
}


# =============================================================
# Enumerate and sample pilot claims
# =============================================================

set.seed(RANDOM_SEED)

pilot_mention_list <- list()

for (lpa in pilot_lpas) {
  
  text <- pdf_texts[[lpa]]
  
  if (
    is.null(text) ||
    length(text) == 0 ||
    all(is.na(text))
  ) {
    warning("No usable PDF text for: ", lpa)
    next
  }
  
  for (narrative_name in focus_narratives) {
    
    extracted <- extract_claim_mentions(
      text = text,
      pattern = keyword_patterns[[narrative_name]],
      context_sentences = 1
    )
    
    n_total <- nrow(extracted)
    
    if (n_total == 0) {
      cat(
        sprintf(
          "%s × %s: 0 mentions\n",
          lpa,
          narrative_name
        )
      )
      next
    }
    
    # Assign a stable ID before sampling
    extracted <- extracted %>%
      arrange(source_sentence_index) %>%
      mutate(
        original_mention_id = row_number()
      )
    
    # Reproducible cap of 10 per project × narrative
    if (n_total > MAX_MENTIONS) {
      
      sampled_indices <- sort(
        sample(
          seq_len(n_total),
          size = MAX_MENTIONS,
          replace = FALSE
        )
      )
      
      extracted <- extracted[
        sampled_indices,
        ,
        drop = FALSE
      ]
    }
    
    n_sampled <- nrow(extracted)
    
    extracted <- extracted %>%
      arrange(source_sentence_index) %>%
      mutate(
        lpa_number = lpa,
        narrative = narrative_name,
        
        # Retain the same mention_id style as the old script
        mention_id = row_number(),
        
        n_total_in_project = n_total,
        n_sampled_in_project = n_sampled,
        
        # Useful later if reporting weighted results
        sample_probability = n_sampled / n_total,
        sample_weight = n_total / n_sampled
      ) %>%
      select(
        lpa_number,
        narrative,
        mention_id,
        original_mention_id,
        source_sentence_index,
        n_total_in_project,
        n_sampled_in_project,
        sample_probability,
        sample_weight,
        claim_text,
        context_before,
        context_after,
        context
      )
    
    pilot_mention_list[[
      length(pilot_mention_list) + 1
    ]] <- extracted
    
    cat(
      sprintf(
        "%s × %s: %d/%d sampled\n",
        lpa,
        narrative_name,
        n_sampled,
        n_total
      )
    )
  }
}


# ---- Combine pilot sample ----

if (length(pilot_mention_list) == 0) {
  stop("No pilot mentions were extracted.")
}

pilot_mentions <- bind_rows(
  pilot_mention_list
) %>%
  arrange(
    lpa_number,
    narrative,
    source_sentence_index
  )


# ---- Create a stable key for later cache matching ----

pilot_mentions <- pilot_mentions %>%
  mutate(
    claim_key = paste(
      lpa_number,
      narrative,
      original_mention_id,
      sep = "|"
    )
  )


# =============================================================
# Save new v4 pilot sample
# =============================================================

write_csv(
  pilot_mentions,
  pilot_mentions_csv
)

saveRDS(
  pilot_mentions,
  pilot_mentions_rds
)


# =============================================================
# Console checks
# =============================================================

cat("\n============================================\n")
cat("V4 PILOT STEP 1 COMPLETED\n")
cat("============================================\n")

cat(
  "Total sampled claims:",
  nrow(pilot_mentions),
  "\n"
)

cat(
  "Projects covered:",
  n_distinct(pilot_mentions$lpa_number),
  "\n"
)

cat(
  "Narratives covered:",
  n_distinct(pilot_mentions$narrative),
  "\n\n"
)

cat("Claims by project and narrative:\n")

pilot_mentions %>%
  count(
    lpa_number,
    narrative,
    name = "sampled_n"
  ) %>%
  arrange(
    lpa_number,
    narrative
  ) %>%
  print(n = Inf)

cat("\nPotentially very short claim units:\n")

pilot_mentions %>%
  filter(nchar(claim_text) < 40) %>%
  select(
    lpa_number,
    narrative,
    mention_id,
    claim_text
  ) %>%
  print(n = Inf)

cat("\nNew files saved:\n")
cat("1.", pilot_mentions_csv, "\n")
cat("2.", pilot_mentions_rds, "\n")
# =============================================================
# STEP 1B: Improved structural claim segmentation
#
# This section:
# - preserves bullet-point boundaries
# - preserves numbered-paragraph boundaries
# - separates long policy tables and contents pages
# - creates manageable excerpts for unusually long blocks
# - tracks claims repeated across narratives
#
# It does NOT call the API.
# It does NOT modify any v3 result.
# =============================================================


# ---- New v2 output files ----

pilot_mentions_v2_csv <- here(
  output_dir,
  "semantic_v4_pilot_mentions_v2.csv"
)

pilot_mentions_v2_rds <- here(
  output_dir,
  "semantic_v4_pilot_mentions_v2.rds"
)

pilot_audit_v2_csv <- here(
  output_dir,
  "semantic_v4_pilot_extraction_audit_v2.csv"
)


# ---- Segmentation settings ----

MAX_STANDARD_UNIT_CHARS <- 1200
LONG_BLOCK_WINDOW_CHARS <- 550

STRUCTURAL_BOUNDARY <- "<<<V4_UNIT_BOUNDARY>>>"


# =============================================================
# Improved text preparation
# =============================================================

normalize_v4_unit <- function(x) {
  
  x %>%
    str_replace_all("[\\r\\n\\t]+", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}


prepare_structural_text <- function(text) {
  
  text <- paste(
    text,
    collapse = "\n"
  )
  
  # Normalise Windows line endings
  text <- str_replace_all(
    text,
    "\r\n?",
    "\n"
  )
  
  # Preserve page breaks as structural boundaries
  text <- str_replace_all(
    text,
    "\f",
    paste0(
      "\n",
      STRUCTURAL_BOUNDARY,
      "\n"
    )
  )
  
  # Join words broken across PDF line endings:
  # sustain-\nability -> sustainability
  text <- str_replace_all(
    text,
    "([[:alpha:]])-\\s*\\n\\s*([[:alpha:]])",
    "\\1\\2"
  )
  
  # Add boundaries before common bullet symbols
  text <- str_replace_all(
    text,
    "[•▪◦]",
    paste0(
      "\n",
      STRUCTURAL_BOUNDARY,
      "\n"
    )
  )
  
  # Add boundaries at blank lines
  text <- str_replace_all(
    text,
    "\\n\\s*\\n+",
    paste0(
      "\n",
      STRUCTURAL_BOUNDARY,
      "\n"
    )
  )
  
  # Add boundaries before numbered paragraphs such as:
  # 3.10 The applicant...
  # 7.176 The Sustainability Statement...
  text <- str_replace_all(
    text,
    paste0(
      "(?<![A-Za-z£$])",
      "(\\d{1,2}\\.\\d{1,3}(?:\\.\\d{1,3})?)",
      "\\s+",
      "(?=[A-Z])"
    ),
    paste0(
      "\n",
      STRUCTURAL_BOUNDARY,
      "\n\\1 "
    )
  )
  
  # Separate common PDF page-number strings
  text <- str_replace_all(
    text,
    "(?i)\\bPg\\s+\\d+(?:/\\d+)?\\b",
    paste0(
      "\n",
      STRUCTURAL_BOUNDARY,
      "\n"
    )
  )
  
  # Remaining single line breaks are usually PDF line wrapping
  text <- str_replace_all(
    text,
    "\\n+",
    " "
  )
  
  # Restore structural markers to line boundaries
  text <- str_replace_all(
    text,
    fixed(STRUCTURAL_BOUNDARY),
    paste0(
      "\n",
      STRUCTURAL_BOUNDARY,
      "\n"
    )
  )
  
  text
}


# =============================================================
# Create structural blocks, then split blocks into sentences
# =============================================================

make_v4_base_units <- function(text) {
  
  prepared_text <- prepare_structural_text(text)
  
  structural_blocks <- str_split(
    prepared_text,
    fixed(STRUCTURAL_BOUNDARY)
  )[[1]]
  
  structural_blocks <- normalize_v4_unit(
    structural_blocks
  )
  
  structural_blocks <- structural_blocks[
    !is.na(structural_blocks) &
      structural_blocks != ""
  ]
  
  if (length(structural_blocks) == 0) {
    return(tibble())
  }
  
  unit_list <- map(
    structural_blocks,
    function(block) {
      
      sentence_units <- stringi::stri_split_boundaries(
        block,
        type = "sentence"
      )[[1]]
      
      sentence_units <- normalize_v4_unit(
        sentence_units
      )
      
      sentence_units[
        !is.na(sentence_units) &
          sentence_units != ""
      ]
    }
  )
  
  units <- unlist(
    unit_list,
    use.names = FALSE
  )
  
  units <- normalize_v4_unit(units)
  
  units <- units[
    !is.na(units) &
      units != ""
  ]
  
  tibble(
    source_unit_index = seq_along(units),
    source_unit_text = units
  ) %>%
    mutate(
      source_unit_length = nchar(
        source_unit_text
      ),
      source_context_before = lag(
        source_unit_text,
        default = ""
      ),
      source_context_after = lead(
        source_unit_text,
        default = ""
      )
    )
}


# =============================================================
# Extract keyword claims from structural units
# =============================================================

extract_v4_claim_mentions <- function(
    text,
    pattern,
    max_standard_chars = MAX_STANDARD_UNIT_CHARS,
    window_chars = LONG_BLOCK_WINDOW_CHARS
) {
  
  base_units <- make_v4_base_units(text)
  
  if (nrow(base_units) == 0) {
    return(tibble())
  }
  
  extracted_list <- list()
  
  for (unit_i in seq_len(nrow(base_units))) {
    
    unit_text <- base_units$source_unit_text[unit_i]
    
    matches <- str_locate_all(
      unit_text,
      regex(
        pattern,
        ignore_case = TRUE
      )
    )[[1]]
    
    if (nrow(matches) == 0) {
      next
    }
    
    unit_length <- nchar(unit_text)
    
    # Normal unit: retain the complete sentence/bullet/paragraph
    if (unit_length <= max_standard_chars) {
      
      extracted_list[[
        length(extracted_list) + 1
      ]] <- tibble(
        source_unit_index =
          base_units$source_unit_index[unit_i],
        
        excerpt_start = 1L,
        
        excerpt_end = unit_length,
        
        segmentation_flag = "standard",
        
        claim_text = unit_text,
        
        context_before =
          base_units$source_context_before[unit_i],
        
        context_after =
          base_units$source_context_after[unit_i]
      )
      
    } else {
      
      # Very long table/list block:
      # create a manageable excerpt around each keyword occurrence.
      for (match_i in seq_len(nrow(matches))) {
        
        match_start <- matches[
          match_i,
          "start"
        ]
        
        match_end <- matches[
          match_i,
          "end"
        ]
        
        excerpt_start <- max(
          1,
          match_start - window_chars
        )
        
        excerpt_end <- min(
          unit_length,
          match_end + window_chars
        )
        
        excerpt_text <- str_sub(
          unit_text,
          excerpt_start,
          excerpt_end
        )
        
        excerpt_text <- normalize_v4_unit(
          excerpt_text
        )
        
        extracted_list[[
          length(extracted_list) + 1
        ]] <- tibble(
          source_unit_index =
            base_units$source_unit_index[unit_i],
          
          excerpt_start = excerpt_start,
          
          excerpt_end = excerpt_end,
          
          segmentation_flag =
            "long_block_excerpt",
          
          claim_text = excerpt_text,
          
          context_before = "",
          
          context_after = ""
        )
      }
    }
  }
  
  if (length(extracted_list) == 0) {
    return(tibble())
  }
  
  bind_rows(extracted_list) %>%
    mutate(
      claim_text_normalized =
        claim_text %>%
        str_to_lower() %>%
        normalize_v4_unit()
    ) %>%
    distinct(
      source_unit_index,
      claim_text_normalized,
      .keep_all = TRUE
    ) %>%
    select(
      -claim_text_normalized
    )
}


# =============================================================
# Enumerate all v2 pilot mentions
# =============================================================

all_v2_mentions_list <- list()

for (lpa in pilot_lpas) {
  
  text <- pdf_texts[[lpa]]
  
  if (
    is.null(text) ||
    length(text) == 0 ||
    all(is.na(text))
  ) {
    warning(
      "No usable PDF text for: ",
      lpa
    )
    
    next
  }
  
  for (narrative_name in focus_narratives) {
    
    extracted <- extract_v4_claim_mentions(
      text = text,
      pattern =
        keyword_patterns[[narrative_name]]
    )
    
    if (nrow(extracted) == 0) {
      
      cat(
        sprintf(
          "%s × %s: 0 extracted\n",
          lpa,
          narrative_name
        )
      )
      
      next
    }
    
    extracted <- extracted %>%
      arrange(
        source_unit_index,
        excerpt_start
      ) %>%
      mutate(
        lpa_number = lpa,
        narrative = narrative_name,
        original_mention_id = row_number()
      ) %>%
      select(
        lpa_number,
        narrative,
        original_mention_id,
        source_unit_index,
        excerpt_start,
        excerpt_end,
        segmentation_flag,
        claim_text,
        context_before,
        context_after
      )
    
    all_v2_mentions_list[[
      length(all_v2_mentions_list) + 1
    ]] <- extracted
  }
}


if (length(all_v2_mentions_list) == 0) {
  stop(
    "No v2 pilot mentions were extracted."
  )
}


all_v2_mentions <- bind_rows(
  all_v2_mentions_list
) %>%
  arrange(
    lpa_number,
    narrative,
    source_unit_index,
    excerpt_start
  )


# =============================================================
# Add stable text and claim identifiers
# =============================================================

all_v2_mentions <- all_v2_mentions %>%
  mutate(
    # Same textual location has the same text_id even when it
    # appears under more than one narrative.
    text_id = paste(
      lpa_number,
      source_unit_index,
      excerpt_start,
      excerpt_end,
      sep = "|"
    ),
    
    # claim_key also includes narrative because sampling is
    # stratified by narrative.
    claim_key = paste(
      lpa_number,
      narrative,
      source_unit_index,
      excerpt_start,
      excerpt_end,
      sep = "|"
    )
  ) %>%
  group_by(
    lpa_number,
    narrative
  ) %>%
  mutate(
    n_total_in_project = n()
  ) %>%
  ungroup()


# Save the complete extraction audit before sampling

write_csv(
  all_v2_mentions,
  pilot_audit_v2_csv
)


# =============================================================
# Reproducible sampling: maximum 10 per project × narrative
# =============================================================

set.seed(RANDOM_SEED)

pilot_mentions_v2 <- all_v2_mentions %>%
  group_by(
    lpa_number,
    narrative
  ) %>%
  group_modify(
    function(.x, .y) {
      
      n_to_sample <- min(
        MAX_MENTIONS,
        nrow(.x)
      )
      
      sampled_rows <- sort(
        sample(
          seq_len(nrow(.x)),
          size = n_to_sample,
          replace = FALSE
        )
      )
      
      .x[
        sampled_rows,
        ,
        drop = FALSE
      ]
    }
  ) %>%
  arrange(
    source_unit_index,
    excerpt_start,
    .by_group = TRUE
  ) %>%
  mutate(
    mention_id = row_number(),
    n_sampled_in_project = n(),
    sample_probability =
      n_sampled_in_project /
      n_total_in_project,
    sample_weight =
      n_total_in_project /
      n_sampled_in_project
  ) %>%
  ungroup() %>%
  arrange(
    lpa_number,
    narrative,
    source_unit_index,
    excerpt_start
  )


# Add cross-narrative duplication information

text_id_summary <- pilot_mentions_v2 %>%
  group_by(text_id) %>%
  summarise(
    narratives_for_text = paste(
      sort(unique(narrative)),
      collapse = ";"
    ),
    n_narratives_for_text =
      n_distinct(narrative),
    .groups = "drop"
  )

pilot_mentions_v2 <- pilot_mentions_v2 %>%
  left_join(
    text_id_summary,
    by = "text_id"
  ) %>%
  mutate(
    claim_length = nchar(claim_text),
    
    context = paste(
      if_else(
        context_before != "",
        paste0(
          "CONTEXT BEFORE: ",
          context_before
        ),
        ""
      ),
      paste0(
        "CLAIM TEXT: ",
        claim_text
      ),
      if_else(
        context_after != "",
        paste0(
          "CONTEXT AFTER: ",
          context_after
        ),
        ""
      ),
      sep = "\n\n"
    )
  ) %>%
  select(
    lpa_number,
    narrative,
    mention_id,
    original_mention_id,
    text_id,
    claim_key,
    source_unit_index,
    excerpt_start,
    excerpt_end,
    segmentation_flag,
    n_total_in_project,
    n_sampled_in_project,
    sample_probability,
    sample_weight,
    n_narratives_for_text,
    narratives_for_text,
    claim_length,
    claim_text,
    context_before,
    context_after,
    context
  )


# =============================================================
# Save v2 outputs
# =============================================================

write_csv(
  pilot_mentions_v2,
  pilot_mentions_v2_csv
)

saveRDS(
  pilot_mentions_v2,
  pilot_mentions_v2_rds
)


# =============================================================
# V2 console checks
# =============================================================

cat("\n")
cat("============================================\n")
cat("V4 PILOT STEP 1B COMPLETED\n")
cat("============================================\n")

cat(
  "Total sampled claims:",
  nrow(pilot_mentions_v2),
  "\n"
)

cat(
  "Projects covered:",
  n_distinct(
    pilot_mentions_v2$lpa_number
  ),
  "\n"
)

cat(
  "Narratives covered:",
  n_distinct(
    pilot_mentions_v2$narrative
  ),
  "\n\n"
)


cat("Length summary:\n")

pilot_mentions_v2 %>%
  summarise(
    minimum = min(claim_length),
    median = median(claim_length),
    mean = round(
      mean(claim_length),
      1
    ),
    p90 = as.numeric(
      quantile(
        claim_length,
        0.90
      )
    ),
    maximum = max(claim_length)
  ) %>%
  print()


cat("\nSegmentation flags:\n")

pilot_mentions_v2 %>%
  count(
    segmentation_flag,
    name = "n"
  ) %>%
  mutate(
    pct = round(
      n / sum(n) * 100,
      1
    )
  ) %>%
  print(n = Inf)


cat("\nClaims by project and narrative:\n")

pilot_mentions_v2 %>%
  count(
    lpa_number,
    narrative,
    name = "sampled_n"
  ) %>%
  arrange(
    lpa_number,
    narrative
  ) %>%
  print(n = Inf)


cat("\nClaims matching multiple narratives:\n")

pilot_mentions_v2 %>%
  filter(
    n_narratives_for_text > 1
  ) %>%
  select(
    lpa_number,
    text_id,
    narratives_for_text,
    claim_text
  ) %>%
  distinct() %>%
  print(
    n = Inf,
    width = Inf
  )


cat("\nLongest 10 sampled claims:\n")

pilot_mentions_v2 %>%
  arrange(
    desc(claim_length)
  ) %>%
  select(
    lpa_number,
    narrative,
    mention_id,
    segmentation_flag,
    claim_length,
    claim_text
  ) %>%
  slice_head(
    n = 10
  ) %>%
  print(
    n = 10,
    width = Inf
  )


cat("\nNew v2 files saved:\n")
cat("1.", pilot_mentions_v2_csv, "\n")
cat("2.", pilot_mentions_v2_rds, "\n")
cat("3.", pilot_audit_v2_csv, "\n")
# =============================================================
# STEP 2: Independent discourse-function coding
# Paste below STEP 1B in scripts/13_semantic_recode_v4_pilot.R
# =============================================================

if (!exists("output_dir")) {
  source(here::here("scripts", "00_setup.R"))
}

library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tibble)
library(httr)
library(jsonlite)

if (!exists("pilot_mentions_v2")) {
  pilot_mentions_v2 <- readRDS(
    here(output_dir, "semantic_v4_pilot_mentions_v2.rds")
  )
}

FUNCTION_MODEL <- "gpt-4o-mini"
FUNCTION_PROMPT_VERSION <- "v4_function_1"

function_cache_path <- here(
  output_dir, "semantic_v4_pilot_function_v1_raw.rds"
)

function_coded_csv <- here(
  output_dir, "semantic_v4_pilot_function_v1.csv"
)

function_coded_rds <- here(
  output_dir, "semantic_v4_pilot_function_v1.rds"
)

allowed_function_labels <- c(
  "applicant_claim",
  "policy_claim",
  "technical_assessment",
  "project_description",
  "procedural_record",
  "document_metadata",
  "unusable_fragment"
)

codable_function_labels <- c(
  "applicant_claim",
  "policy_claim",
  "technical_assessment",
  "project_description"
)

noncodable_function_labels <- c(
  "procedural_record",
  "document_metadata",
  "unusable_fragment"
)

allowed_confidence_labels <- c(
  "high",
  "medium",
  "low"
)


# =============================================================
# Helper functions
# =============================================================

normalize_function_text <- function(x) {
  x %>%
    str_replace_all("[\\r\\n\\t]+", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim() %>%
    str_to_lower()
}


scalar_character_v4 <- function(x) {
  if (
    is.null(x) ||
    length(x) == 0 ||
    all(is.na(x))
  ) {
    return(NA_character_)
  }
  
  as.character(x[[1]])
}


scalar_logical_v4 <- function(x) {
  if (
    is.null(x) ||
    length(x) == 0 ||
    all(is.na(x))
  ) {
    return(NA)
  }
  
  if (is.logical(x[[1]])) {
    return(x[[1]])
  }
  
  value <- str_to_lower(
    str_trim(
      as.character(x[[1]])
    )
  )
  
  if (value %in% c("true", "yes", "1")) {
    return(TRUE)
  }
  
  if (value %in% c("false", "no", "0")) {
    return(FALSE)
  }
  
  NA
}


scalar_integer_v4 <- function(x) {
  if (
    is.null(x) ||
    length(x) == 0 ||
    all(is.na(x))
  ) {
    return(0L)
  }
  
  as.integer(x[[1]])
}


valid_choice_v4 <- function(
    x,
    allowed,
    default = NA_character_
) {
  value <- scalar_character_v4(x)
  
  if (
    is.na(value) ||
    !value %in% allowed
  ) {
    return(default)
  }
  
  value
}


evidence_vector_v4 <- function(x) {
  if (
    is.null(x) ||
    length(x) == 0
  ) {
    return(character(0))
  }
  
  values <- as.character(
    unlist(
      x,
      use.names = FALSE
    )
  )
  
  values <- values[
    !is.na(values) &
      str_trim(values) != ""
  ]
  
  unique(values)
}


evidence_is_exact_v4 <- function(
    claim_text,
    evidence
) {
  evidence <- evidence_vector_v4(evidence)
  
  if (length(evidence) == 0) {
    return(FALSE)
  }
  
  normalized_claim <- normalize_function_text(
    claim_text
  )
  
  all(
    map_lgl(
      evidence,
      function(item) {
        normalized_item <- normalize_function_text(
          item
        )
        
        str_detect(
          normalized_claim,
          fixed(normalized_item)
        )
      }
    )
  )
}


# =============================================================
# Reuse exact duplicate text within the same project
# =============================================================

if (anyDuplicated(pilot_mentions_v2$claim_key)) {
  stop(
    "claim_key is not unique in pilot_mentions_v2."
  )
}

pilot_function_input <- pilot_mentions_v2 %>%
  arrange(
    lpa_number,
    narrative,
    source_unit_index,
    excerpt_start
  ) %>%
  mutate(
    claim_text_normalized =
      normalize_function_text(claim_text)
  ) %>%
  group_by(
    lpa_number,
    claim_text_normalized
  ) %>%
  mutate(
    function_unit_key = first(claim_key),
    duplicate_n_within_project = n(),
    is_function_representative =
      row_number() == 1L
  ) %>%
  ungroup() %>%
  group_by(
    lpa_number,
    narrative,
    claim_text_normalized
  ) %>%
  mutate(
    duplicate_n_same_narrative = n()
  ) %>%
  ungroup() %>%
  mutate(
    is_repeated_text =
      duplicate_n_within_project > 1L
  )

function_units <- pilot_function_input %>%
  filter(is_function_representative)

cat(
  "Step 2 sampled rows:",
  nrow(pilot_function_input),
  "\n"
)

cat(
  "Unique texts to classify:",
  nrow(function_units),
  "\n"
)

cat(
  "API calls avoided by duplicate reuse:",
  nrow(pilot_function_input) -
    nrow(function_units),
  "\n\n"
)


# =============================================================
# Function-coding prompt
# =============================================================

build_function_prompt_v4 <- function(row) {
  paste0(
    "You are a careful research coder analysing UK planning application text.\n\n",
    
    "TASK\n",
    "Classify the discourse function of CLAIM_TEXT. ",
    "This classification is independent of later framing and outcome coding.\n\n",
    
    "Choose exactly one category:\n",
    
    "1. applicant_claim: a claim, commitment, prediction, evaluation, ",
    "asserted benefit or asserted cost made about the proposal or its effects.\n",
    
    "2. policy_claim: a substantive policy requirement, objective or ",
    "policy-based evaluative statement.\n",
    
    "3. technical_assessment: a substantive technical finding, calculation, ",
    "performance statement or assessed impact.\n",
    
    "4. project_description: a substantive description of the proposed ",
    "scheme, site condition, existing provision or design feature.\n",
    
    "5. procedural_record: a meeting list, consultation date, submission ",
    "process, report-navigation sentence or other administrative record ",
    "without a substantive planning claim.\n",
    
    "6. document_metadata: a bare heading, policy title, contents entry, ",
    "application-document list, appendix label, page header/footer, ",
    "bibliography item or isolated topic label.\n",
    
    "7. unusable_fragment: text corrupted by PDF layout/OCR, interleaved ",
    "across columns or truncated so severely that CLAIM_TEXT cannot be ",
    "coded reliably.\n\n",
    
    "CODABLE RULE\n",
    "Return codable=true only for applicant_claim, policy_claim, ",
    "technical_assessment or project_description.\n",
    
    "Return codable=false only for procedural_record, document_metadata ",
    "or unusable_fragment.\n\n",
    
    "RULES\n",
    "- Classify CLAIM_TEXT, not the narrative keyword by itself.\n",
    "- CONTEXT_BEFORE and CONTEXT_AFTER may resolve a pronoun or identify ",
    "the section, but may not supply a missing claim.\n",
    "- A substantive policy clause is policy_claim, not document_metadata.\n",
    "- A keyword appearing only in a title or document name is ",
    "document_metadata.\n",
    "- A short bullet is codeable if it states a meaningful provision, ",
    "requirement, benefit or impact.\n",
    "- Do not repair an unusable fragment by guessing from context.\n",
    "- Evidence must be copied exactly from CLAIM_TEXT, never from context.\n\n",
    
    "Examples:\n",
    "- 'Regeneration Area' => document_metadata; codable=false.\n",
    "- 'Policy D8: Public realm' => document_metadata; codable=false.\n",
    "- A dated list of pre-application meetings => procedural_record; ",
    "codable=false.\n",
    "- A document list containing 'REGENERATION STATEMENT' => ",
    "document_metadata; codable=false.\n",
    "- A policy paragraph requiring improved environmental quality => ",
    "policy_claim; codable=true.\n",
    "- 'The scheme will provide a new community centre' => ",
    "applicant_claim; codable=true.\n",
    "- A CO2 calculation for the proposal => technical_assessment; ",
    "codable=true.\n",
    "- Interleaved text from unrelated table columns => ",
    "unusable_fragment; codable=false.\n\n",
    
    "Narrative sampling stratum: ",
    row$narrative,
    "\n\n",
    
    "CLAIM_TEXT\n<<<\n",
    row$claim_text,
    "\n>>>\n\n",
    
    "CONTEXT_BEFORE — interpretation only\n<<<\n",
    row$context_before,
    "\n>>>\n\n",
    
    "CONTEXT_AFTER — interpretation only\n<<<\n",
    row$context_after,
    "\n>>>\n\n",
    
    "Return ONLY one JSON object in this form:\n",
    "{\n",
    "  \"discourse_function\": ",
    "\"applicant_claim|policy_claim|technical_assessment|",
    "project_description|procedural_record|document_metadata|",
    "unusable_fragment\",\n",
    "  \"codable\": true,\n",
    "  \"evidence\": ",
    "[\"one or more exact quotations from CLAIM_TEXT\"],\n",
    "  \"confidence\": \"high|medium|low\",\n",
    "  \"reasoning\": \"brief explanation\"\n",
    "}\n",
    
    "Replace true with false when required by the CODABLE RULE."
  )
}


# =============================================================
# API call
# Same endpoint, model and retry pattern as v3
# =============================================================

call_openai_function_v4 <- function(
    prompt,
    max_retries = 3
) {
  api_key <- Sys.getenv("OPENAI_API_KEY")
  
  if (!nzchar(api_key)) {
    stop("OPENAI_API_KEY is not set.")
  }
  
  last_error <- "unknown_error"
  
  for (attempt in seq_len(max_retries)) {
    response <- tryCatch(
      POST(
        url =
          "https://api.openai.com/v1/chat/completions",
        
        add_headers(
          Authorization = paste(
            "Bearer",
            api_key
          ),
          "Content-Type" =
            "application/json"
        ),
        
        body = toJSON(
          list(
            model = FUNCTION_MODEL,
            
            messages = list(
              list(
                role = "user",
                content = prompt
              )
            ),
            
            response_format = list(
              type = "json_object"
            ),
            
            temperature = 0.1,
            max_tokens = 450
          ),
          auto_unbox = TRUE
        ),
        
        timeout(60)
      ),
      
      error = function(e) {
        last_error <<- e$message
        NULL
      }
    )
    
    if (
      !is.null(response) &&
      status_code(response) == 200
    ) {
      result <- content(
        response,
        as = "parsed",
        encoding = "UTF-8"
      )
      
      return(
        list(
          ok = TRUE,
          
          text =
            result$choices[[1]]$message$content,
          
          tokens_in =
            scalar_integer_v4(
              result$usage$prompt_tokens
            ),
          
          tokens_out =
            scalar_integer_v4(
              result$usage$completion_tokens
            ),
          
          error = NA_character_
        )
      )
    }
    
    if (!is.null(response)) {
      last_error <- paste0(
        "HTTP ",
        status_code(response)
      )
    }
    
    if (attempt < max_retries) {
      backoff <- attempt * 10
      
      cat(
        sprintf(
          paste0(
            "  [attempt %d failed: %s; ",
            "retry after %d sec]\n"
          ),
          attempt,
          last_error,
          backoff
        )
      )
      
      Sys.sleep(backoff)
    }
  }
  
  list(
    ok = FALSE,
    text = NA_character_,
    tokens_in = 0L,
    tokens_out = 0L,
    error = last_error
  )
}


# =============================================================
# Cache structure
# =============================================================

empty_function_cache <- tibble(
  function_unit_key = character(),
  representative_claim_key = character(),
  codable_returned = logical(),
  codable = logical(),
  discourse_function = character(),
  function_evidence = character(),
  function_evidence_valid = logical(),
  function_confidence = character(),
  function_reasoning = character(),
  codable_consistent = logical(),
  function_needs_review = logical(),
  function_api_raw = character(),
  function_api_error = character(),
  function_tokens_in = integer(),
  function_tokens_out = integer(),
  function_model = character(),
  function_prompt_version = character()
)

if (file.exists(function_cache_path)) {
  cat("=== Loading Step 2 cache ===\n")
  
  function_cache <- readRDS(
    function_cache_path
  )
  
  missing_cache_columns <- setdiff(
    names(empty_function_cache),
    names(function_cache)
  )
  
  if (length(missing_cache_columns) > 0) {
    stop(
      "Existing Step 2 cache is incompatible. Missing columns: ",
      paste(
        missing_cache_columns,
        collapse = ", "
      )
    )
  }
  
  function_cache <- function_cache %>%
    select(
      all_of(
        names(empty_function_cache)
      )
    ) %>%
    distinct(
      function_unit_key,
      .keep_all = TRUE
    )
  
} else {
  function_cache <- empty_function_cache
}

completed_function_keys <- function_cache %>%
  filter(
    is.na(function_api_error),
    
    discourse_function %in%
      allowed_function_labels,
    
    function_prompt_version ==
      FUNCTION_PROMPT_VERSION
  ) %>%
  pull(function_unit_key)

to_code_function <- function_units %>%
  filter(
    !function_unit_key %in%
      completed_function_keys
  )

cat(
  "Unique texts remaining:",
  nrow(to_code_function),
  "\n\n"
)


# =============================================================
# Run Step 2
# =============================================================

session_tokens_in <- 0L
session_tokens_out <- 0L

for (i in seq_len(nrow(to_code_function))) {
  row <- to_code_function[i, ]
  
  api_result <- call_openai_function_v4(
    build_function_prompt_v4(row)
  )
  
  if (isTRUE(api_result$ok)) {
    session_tokens_in <-
      session_tokens_in +
      api_result$tokens_in
    
    session_tokens_out <-
      session_tokens_out +
      api_result$tokens_out
    
    parsed <- tryCatch(
      fromJSON(
        api_result$text,
        simplifyVector = FALSE
      ),
      error = function(e) NULL
    )
    
    if (is.null(parsed)) {
      new_result <- empty_function_cache %>%
        add_row(
          function_unit_key =
            row$function_unit_key,
          
          representative_claim_key =
            row$claim_key,
          
          function_api_raw =
            api_result$text,
          
          function_api_error =
            "invalid_json",
          
          function_tokens_in =
            api_result$tokens_in,
          
          function_tokens_out =
            api_result$tokens_out,
          
          function_model =
            FUNCTION_MODEL,
          
          function_prompt_version =
            FUNCTION_PROMPT_VERSION
        )
      
    } else {
      function_label <- valid_choice_v4(
        parsed$discourse_function,
        allowed_function_labels
      )
      
      confidence_label <- valid_choice_v4(
        parsed$confidence,
        allowed_confidence_labels,
        default = "low"
      )
      
      codable_returned <- scalar_logical_v4(
        parsed$codable
      )
      
      # codable is derived from discourse_function.
      # The model cannot silently change eligibility.
      codable_derived <- case_when(
        function_label %in%
          codable_function_labels ~ TRUE,
        
        function_label %in%
          noncodable_function_labels ~ FALSE,
        
        TRUE ~ NA
      )
      
      evidence_values <- evidence_vector_v4(
        parsed$evidence
      )
      
      evidence_valid <- evidence_is_exact_v4(
        row$claim_text,
        evidence_values
      )
      
      reasoning_value <- scalar_character_v4(
        parsed$reasoning
      )
      
      codable_consistent <-
        !is.na(codable_returned) &&
        !is.na(codable_derived) &&
        codable_returned == codable_derived
      
      needs_review <-
        is.na(function_label) ||
        confidence_label == "low" ||
        !evidence_valid ||
        !codable_consistent ||
        is.na(reasoning_value) ||
        !nzchar(str_trim(reasoning_value))
      
      api_error_value <- if (
        is.na(function_label)
      ) {
        "invalid_function_label"
      } else {
        NA_character_
      }
      
      new_result <- tibble(
        function_unit_key =
          row$function_unit_key,
        
        representative_claim_key =
          row$claim_key,
        
        codable_returned =
          codable_returned,
        
        codable =
          codable_derived,
        
        discourse_function =
          function_label,
        
        function_evidence = paste(
          evidence_values,
          collapse = " || "
        ),
        
        function_evidence_valid =
          evidence_valid,
        
        function_confidence =
          confidence_label,
        
        function_reasoning =
          reasoning_value,
        
        codable_consistent =
          codable_consistent,
        
        function_needs_review =
          needs_review,
        
        function_api_raw =
          api_result$text,
        
        function_api_error =
          api_error_value,
        
        function_tokens_in =
          as.integer(api_result$tokens_in),
        
        function_tokens_out =
          as.integer(api_result$tokens_out),
        
        function_model =
          FUNCTION_MODEL,
        
        function_prompt_version =
          FUNCTION_PROMPT_VERSION
      )
    }
    
  } else {
    new_result <- empty_function_cache %>%
      add_row(
        function_unit_key =
          row$function_unit_key,
        
        representative_claim_key =
          row$claim_key,
        
        function_api_error =
          api_result$error,
        
        function_tokens_in = 0L,
        function_tokens_out = 0L,
        
        function_model =
          FUNCTION_MODEL,
        
        function_prompt_version =
          FUNCTION_PROMPT_VERSION
      )
  }
  
  # Replace any previous failed result for this text.
  function_cache <- function_cache %>%
    filter(
      function_unit_key !=
        row$function_unit_key
    ) %>%
    bind_rows(new_result)
  
  cat(
    sprintf(
      paste0(
        "[%d/%d] %s -- function:%s | ",
        "codable:%s | review:%s\n"
      ),
      
      i,
      nrow(to_code_function),
      row$claim_key,
      
      ifelse(
        is.na(new_result$discourse_function),
        "NA",
        new_result$discourse_function
      ),
      
      ifelse(
        is.na(new_result$codable),
        "NA",
        new_result$codable
      ),
      
      ifelse(
        is.na(new_result$function_needs_review),
        "NA",
        new_result$function_needs_review
      )
    )
  )
  
  # Incremental save every 10 calls.
  if (i %% 10 == 0) {
    saveRDS(
      function_cache,
      function_cache_path
    )
    
    cat("  [Step 2 cache saved]\n")
  }
  
  Sys.sleep(2)
}

saveRDS(
  function_cache,
  function_cache_path
)


# =============================================================
# Join results back to every sampled row
# =============================================================

pilot_function_coded <- pilot_function_input %>%
  left_join(
    function_cache,
    by = "function_unit_key"
  ) %>%
  select(
    -claim_text_normalized
  ) %>%
  arrange(
    lpa_number,
    narrative,
    source_unit_index,
    excerpt_start
  )

write_csv(
  pilot_function_coded,
  function_coded_csv
)

saveRDS(
  pilot_function_coded,
  function_coded_rds
)


# =============================================================
# Console checks
# =============================================================

cat("\n")
cat("============================================\n")
cat("V4 PILOT STEP 2 COMPLETED\n")
cat("============================================\n")

cat(
  "Sampled rows:",
  nrow(pilot_function_coded),
  "\n"
)

cat(
  "Unique texts:",
  nrow(function_units),
  "\n"
)

cat(
  "Session input tokens:",
  session_tokens_in,
  "\n"
)

cat(
  "Session output tokens:",
  session_tokens_out,
  "\n\n"
)


cat("Discourse functions across sampled rows:\n")

pilot_function_coded %>%
  count(
    discourse_function,
    codable,
    name = "n"
  ) %>%
  mutate(
    pct = round(
      n / sum(n) * 100,
      1
    )
  ) %>%
  arrange(desc(n)) %>%
  print(n = Inf)


cat("\nRows requiring review or retry:\n")

pilot_function_coded %>%
  filter(
    function_needs_review %in% TRUE |
      is.na(discourse_function) |
      !is.na(function_api_error)
  ) %>%
  select(
    lpa_number,
    narrative,
    claim_key,
    claim_text,
    discourse_function,
    codable,
    function_confidence,
    function_evidence_valid,
    codable_consistent,
    function_api_error,
    function_reasoning
  ) %>%
  print(
    n = Inf,
    width = Inf
  )


cat("\nNon-codeable rows retained in the audit:\n")

pilot_function_coded %>%
  filter(codable %in% FALSE) %>%
  select(
    lpa_number,
    narrative,
    claim_key,
    discourse_function,
    claim_text,
    function_reasoning
  ) %>%
  print(
    n = Inf,
    width = Inf
  )


cat("\nRepeated texts whose classification was reused:\n")

pilot_function_coded %>%
  filter(is_repeated_text) %>%
  select(
    lpa_number,
    narrative,
    claim_key,
    function_unit_key,
    duplicate_n_within_project,
    discourse_function,
    claim_text
  ) %>%
  print(
    n = Inf,
    width = Inf
  )


cat("\nNew Step 2 files saved:\n")
cat("1.", function_cache_path, "\n")
cat("2.", function_coded_csv, "\n")
cat("3.", function_coded_rds, "\n")
# =============================================================
# STEP 2B: Apply adjudicated pilot corrections reproducibly
# Paste below STEP 2 in scripts/13_semantic_recode_v4_pilot.R
#
# This step NEVER overwrites the API-returned columns. It creates
# reviewed columns and an audit file, then uses the reviewed label
# to derive codable_reviewed.
# =============================================================

if (!exists("output_dir")) {
  source(here::here("scripts", "00_setup.R"))
}

library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tibble)


# =============================================================
# Load the Step 2 result when this block is run independently
# =============================================================

if (!exists("pilot_function_coded")) {
  step2_input_rds <- here(
    output_dir,
    "semantic_v4_pilot_function_v1.rds"
  )
  
  step2_input_csv <- here(
    output_dir,
    "semantic_v4_pilot_function_v1.csv"
  )
  
  if (file.exists(step2_input_rds)) {
    pilot_function_coded <- readRDS(
      step2_input_rds
    )
  } else if (file.exists(step2_input_csv)) {
    pilot_function_coded <- read_csv(
      step2_input_csv,
      show_col_types = FALSE
    )
  } else {
    stop(
      "Step 2 input was not found. Run Step 2 first."
    )
  }
}


required_step2b_columns <- c(
  "claim_key",
  "claim_text",
  "segmentation_flag",
  "discourse_function",
  "codable",
  "function_evidence",
  "function_evidence_valid",
  "function_confidence",
  "codable_consistent",
  "function_needs_review"
)

missing_step2b_columns <- setdiff(
  required_step2b_columns,
  names(pilot_function_coded)
)

if (length(missing_step2b_columns) > 0) {
  stop(
    "Step 2 input is missing columns: ",
    paste(
      missing_step2b_columns,
      collapse = ", "
    )
  )
}

if (anyDuplicated(pilot_function_coded$claim_key)) {
  stop(
    "claim_key is not unique in the Step 2 input."
  )
}


# =============================================================
# Ten adjudicated discourse-function corrections
#
# These are keyed by stable claim_key, never by CSV row number.
# =============================================================

function_review_overrides <- tribble(
  ~claim_key,
  ~discourse_function_override,
  ~function_review_note,
  
  "PA/13/02966|community|2733|1|233",
  "unusable_fragment",
  "Interleaved two-column text; the claim cannot be interpreted reliably.",
  
  "PA/13/02966|public_realm|202|1|51",
  "project_description",
  "A substantive list of proposed scheme components, not a bare heading.",
  
  "PA/13/02966|regeneration|306|1|168",
  "project_description",
  "Describes the area's development context rather than stating a policy requirement.",
  
  "PA/13/02966|regeneration|2819|1|510",
  "unusable_fragment",
  "Severely interleaved multi-column content; reliable coding is not possible.",
  
  "PA/13/02966|sustainability|1500|1|170",
  "applicant_claim",
  "The wording 'will seek to achieve' is a future project commitment, not a measured technical result.",
  
  "PA/14/00944|community|589|1|344",
  "applicant_claim",
  "The applicant claims the proposal promotes mixed communities; cited policies support that claim but are not themselves speaking.",
  
  "PA/14/00944|sustainability|392|1|340",
  "applicant_claim",
  "An applicant evaluation that the proposal should benefit from the policy presumption, not an external policy requirement.",
  
  "PA/14/00944|sustainability|2142|1|145",
  "technical_assessment",
  "The stated 'moderate beneficial effect' is an assessed impact.",
  
  "PA/24/00922|regeneration|901|1|102",
  "applicant_claim",
  "The applicant attributes support for regeneration to feedback received; the sentence is truncated, so confidence remains limited.",
  
  "PA/24/00922|sustainability|1933|1|368",
  "applicant_claim",
  "A project-specific policy-compliance conclusion made by the applicant, not the policy text itself."
)


# =============================================================
# Two exact-evidence corrections
#
# For these rows the model silently normalized PDF text. The
# reviewed evidence must preserve the original CLAIM_TEXT exactly.
# =============================================================

evidence_review_overrides <- tribble(
  ~claim_key,
  ~evidence_review_note,
  
  "PA/13/02966|community|733|1|290",
  "Use CLAIM_TEXT verbatim: the PDF extraction contains 'retail,community' without an inserted space.",
  
  "PA/24/00922|sustainability|1766|1|153",
  "Use CLAIM_TEXT verbatim: preserve the extracted OCR form 't0' rather than silently changing it to 'to'."
)


# =============================================================
# Validate every correction target before applying anything
# =============================================================

if (anyDuplicated(function_review_overrides$claim_key)) {
  stop("Duplicate claim_key in function_review_overrides.")
}

if (anyDuplicated(evidence_review_overrides$claim_key)) {
  stop("Duplicate claim_key in evidence_review_overrides.")
}

missing_function_targets <- setdiff(
  function_review_overrides$claim_key,
  pilot_function_coded$claim_key
)

missing_evidence_targets <- setdiff(
  evidence_review_overrides$claim_key,
  pilot_function_coded$claim_key
)

if (length(missing_function_targets) > 0) {
  stop(
    "Function correction keys not found: ",
    paste(
      missing_function_targets,
      collapse = "; "
    )
  )
}

if (length(missing_evidence_targets) > 0) {
  stop(
    "Evidence correction keys not found: ",
    paste(
      missing_evidence_targets,
      collapse = "; "
    )
  )
}


# =============================================================
# Exact-evidence and automatic-review helpers
# =============================================================

normalize_step2b_text <- function(x) {
  x %>%
    str_replace_all("[\\r\\n\\t]+", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim() %>%
    str_to_lower()
}


evidence_is_exact_step2b <- function(
    claim_text,
    evidence_text
) {
  if (
    is.na(evidence_text) ||
    !nzchar(str_trim(evidence_text))
  ) {
    return(FALSE)
  }
  
  evidence_items <- str_split(
    evidence_text,
    fixed(" || ")
  )[[1]]
  
  evidence_items <- evidence_items[
    !is.na(evidence_items) &
      str_trim(evidence_items) != ""
  ]
  
  if (length(evidence_items) == 0) {
    return(FALSE)
  }
  
  normalized_claim <- normalize_step2b_text(
    claim_text
  )
  
  all(
    map_lgl(
      evidence_items,
      function(item) {
        str_detect(
          normalized_claim,
          fixed(
            normalize_step2b_text(item)
          )
        )
      }
    )
  )
}


looks_truncated_step2b <- function(x) {
  str_detect(
    str_trim(x),
    regex(
      paste0(
        "\\b(and|or|to|of|the|from|with|for|by|",
        "as|which|that|in)\\s*[,:;-]?$"
      ),
      ignore_case = TRUE
    )
  )
}


build_auto_review_trigger_step2b <- function(
    previous_flag,
    confidence,
    evidence_valid,
    codable_ok,
    segmentation_flag,
    claim_text
) {
  reasons <- character(0)
  
  if (isTRUE(previous_flag)) {
    reasons <- c(
      reasons,
      "step2_validation_flag"
    )
  }
  
  if (
    is.na(confidence) ||
    confidence %in% c("medium", "low")
  ) {
    reasons <- c(
      reasons,
      "non_high_confidence"
    )
  }
  
  if (!isTRUE(evidence_valid)) {
    reasons <- c(
      reasons,
      "invalid_exact_evidence"
    )
  }
  
  if (!isTRUE(codable_ok)) {
    reasons <- c(
      reasons,
      "codable_inconsistency"
    )
  }
  
  if (
    identical(
      segmentation_flag,
      "long_block_excerpt"
    )
  ) {
    reasons <- c(
      reasons,
      "long_block_excerpt"
    )
  }
  
  if (looks_truncated_step2b(claim_text)) {
    reasons <- c(
      reasons,
      "possible_truncation"
    )
  }
  
  paste(
    unique(reasons),
    collapse = ";"
  )
}


# =============================================================
# Apply corrections while preserving all Step 2 API columns
# =============================================================

codable_function_labels_step2b <- c(
  "applicant_claim",
  "policy_claim",
  "technical_assessment",
  "project_description"
)

noncodable_function_labels_step2b <- c(
  "procedural_record",
  "document_metadata",
  "unusable_fragment"
)

pilot_function_reviewed <- pilot_function_coded %>%
  mutate(
    # Explicit aliases make the untouched API result obvious.
    discourse_function_api = discourse_function,
    codable_api = codable,
    function_evidence_api = function_evidence,
    function_evidence_valid_api =
      function_evidence_valid
  ) %>%
  left_join(
    function_review_overrides,
    by = "claim_key"
  ) %>%
  left_join(
    evidence_review_overrides,
    by = "claim_key"
  ) %>%
  mutate(
    discourse_function_reviewed = coalesce(
      discourse_function_override,
      discourse_function_api
    ),
    
    codable_reviewed = case_when(
      discourse_function_reviewed %in%
        codable_function_labels_step2b ~ TRUE,
      
      discourse_function_reviewed %in%
        noncodable_function_labels_step2b ~ FALSE,
      
      TRUE ~ NA
    ),
    
    # Only the two adjudicated evidence rows use the complete
    # original claim_text. All other evidence remains unchanged.
    function_evidence_reviewed = if_else(
      !is.na(evidence_review_note),
      claim_text,
      function_evidence_api
    ),
    
    function_evidence_valid_reviewed = map2_lgl(
      claim_text,
      function_evidence_reviewed,
      evidence_is_exact_step2b
    ),
    
    function_class_changed =
      discourse_function_reviewed !=
      discourse_function_api,
    
    codable_changed =
      codable_reviewed !=
      codable_api,
    
    evidence_changed =
      function_evidence_reviewed !=
      function_evidence_api,
    
    function_review_note = coalesce(
      function_review_note,
      evidence_review_note
    ),
    
    function_review_status = case_when(
      !is.na(discourse_function_override) ~
        "adjudicated_function_override",
      
      !is.na(evidence_review_note) ~
        "adjudicated_evidence_correction",
      
      TRUE ~
        "api_label_retained_after_pilot_review"
    ),
    
    # This records which rows the full-data workflow should send
    # to review. In this pilot every row has already been manually
    # checked, so these triggers are resolved rather than pending.
    function_auto_review_trigger = pmap_chr(
      list(
        function_needs_review,
        function_confidence,
        function_evidence_valid_api,
        codable_consistent,
        segmentation_flag,
        claim_text
      ),
      build_auto_review_trigger_step2b
    ),
    
    function_auto_flag_for_future =
      function_auto_review_trigger != "",
    
    function_review_resolved = TRUE,
    function_needs_review_reviewed = FALSE,
    function_review_source =
      "complete_pilot_adjudication_v1"
  ) %>%
  select(
    -discourse_function_override,
    -evidence_review_note
  ) %>%
  relocate(
    discourse_function_api,
    discourse_function_reviewed,
    codable_api,
    codable_reviewed,
    function_evidence_api,
    function_evidence_reviewed,
    function_evidence_valid_api,
    function_evidence_valid_reviewed,
    function_class_changed,
    codable_changed,
    evidence_changed,
    function_review_status,
    function_review_note,
    function_auto_flag_for_future,
    function_auto_review_trigger,
    function_review_resolved,
    function_needs_review_reviewed,
    function_review_source,
    .after = claim_key
  )


# =============================================================
# Hard validation: fail rather than silently producing bad data
# =============================================================

if (
  nrow(pilot_function_reviewed) !=
  nrow(pilot_function_coded)
) {
  stop("Step 2B changed the number of rows.")
}

if (anyDuplicated(pilot_function_reviewed$claim_key)) {
  stop("Step 2B produced duplicate claim_key values.")
}

if (
  sum(
    pilot_function_reviewed$function_class_changed,
    na.rm = TRUE
  ) != 10
) {
  stop("Step 2B did not apply exactly 10 function corrections.")
}

if (
  sum(
    pilot_function_reviewed$evidence_changed,
    na.rm = TRUE
  ) != 2
) {
  stop("Step 2B did not apply exactly 2 evidence corrections.")
}

if (any(is.na(pilot_function_reviewed$codable_reviewed))) {
  stop("Step 2B produced an unknown codable_reviewed value.")
}

if (
  any(
    !pilot_function_reviewed$
    function_evidence_valid_reviewed
  )
) {
  stop("Step 2B still contains non-exact reviewed evidence.")
}


expected_reviewed_counts <- tribble(
  ~discourse_function_reviewed, ~expected_n,
  "policy_claim", 42L,
  "applicant_claim", 28L,
  "project_description", 19L,
  "document_metadata", 16L,
  "unusable_fragment", 5L,
  "procedural_record", 4L,
  "technical_assessment", 3L
)

observed_reviewed_counts <-
  pilot_function_reviewed %>%
  count(
    discourse_function_reviewed,
    name = "observed_n"
  )

count_check <- expected_reviewed_counts %>%
  full_join(
    observed_reviewed_counts,
    by = "discourse_function_reviewed"
  ) %>%
  mutate(
    matches = expected_n == observed_n
  )

if (
  any(
    is.na(count_check$matches) |
    !count_check$matches
  )
) {
  print(count_check, n = Inf)
  stop("Reviewed function counts do not match the adjudication.")
}

if (
  sum(
    pilot_function_reviewed$codable_reviewed
  ) != 92L
) {
  stop("Expected 92 reviewed codable rows.")
}


# =============================================================
# Save reviewed data and compact audit
# =============================================================

step2b_reviewed_csv <- here(
  output_dir,
  "semantic_v4_pilot_function_v1_reviewed.csv"
)

step2b_reviewed_rds <- here(
  output_dir,
  "semantic_v4_pilot_function_v1_reviewed.rds"
)

step2b_audit_csv <- here(
  output_dir,
  "semantic_v4_pilot_function_v1_review_audit.csv"
)


pilot_function_review_audit <-
  pilot_function_reviewed %>%
  filter(
    function_class_changed |
      evidence_changed |
      function_auto_flag_for_future
  ) %>%
  select(
    lpa_number,
    narrative,
    claim_key,
    claim_text,
    discourse_function_api,
    discourse_function_reviewed,
    codable_api,
    codable_reviewed,
    function_evidence_api,
    function_evidence_reviewed,
    function_evidence_valid_api,
    function_evidence_valid_reviewed,
    function_class_changed,
    codable_changed,
    evidence_changed,
    function_review_status,
    function_review_note,
    function_auto_review_trigger,
    function_review_resolved
  )


write_csv(
  pilot_function_reviewed,
  step2b_reviewed_csv
)

saveRDS(
  pilot_function_reviewed,
  step2b_reviewed_rds
)

write_csv(
  pilot_function_review_audit,
  step2b_audit_csv
)


# =============================================================
# Console checks
# =============================================================

cat("\n")
cat("============================================\n")
cat("V4 PILOT STEP 2B COMPLETED\n")
cat("============================================\n")

cat(
  "Rows retained:",
  nrow(pilot_function_reviewed),
  "\n"
)

cat(
  "Function corrections applied:",
  sum(pilot_function_reviewed$function_class_changed),
  "\n"
)

cat(
  "Evidence corrections applied:",
  sum(pilot_function_reviewed$evidence_changed),
  "\n"
)

cat(
  "Reviewed codable rows:",
  sum(pilot_function_reviewed$codable_reviewed),
  "\n"
)

cat(
  "Reviewed non-codeable rows:",
  sum(!pilot_function_reviewed$codable_reviewed),
  "\n\n"
)

cat("Reviewed discourse functions:\n")

pilot_function_reviewed %>%
  count(
    discourse_function_reviewed,
    codable_reviewed,
    name = "n"
  ) %>%
  arrange(desc(n)) %>%
  print(n = Inf)


cat("\nRows whose codable status changed:\n")

pilot_function_reviewed %>%
  filter(codable_changed) %>%
  select(
    claim_key,
    discourse_function_api,
    discourse_function_reviewed,
    codable_api,
    codable_reviewed,
    function_review_note
  ) %>%
  print(
    n = Inf,
    width = Inf
  )


cat("\nNew Step 2B files saved:\n")
cat("1.", step2b_reviewed_csv, "\n")
cat("2.", step2b_reviewed_rds, "\n")
cat("3.", step2b_audit_csv, "\n")
# =============================================================
# STEP 3: Independent multi-label frame coding
# Paste below STEP 2B in scripts/13_semantic_recode_v4_pilot.R
#
# Only rows with codable_reviewed == TRUE are sent to the API.
# The four frame signals are coded independently, so one claim may
# receive zero, one, or several TRUE labels.
# =============================================================

if (!exists("output_dir")) {
  source(here::here("scripts", "00_setup.R"))
}

library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tibble)
library(httr)
library(jsonlite)


# =============================================================
# Load the reviewed Step 2B result when run independently
# =============================================================

if (!exists("pilot_function_reviewed")) {
  step3_input_rds <- here(
    output_dir,
    "semantic_v4_pilot_function_v1_reviewed.rds"
  )
  
  step3_input_csv <- here(
    output_dir,
    "semantic_v4_pilot_function_v1_reviewed.csv"
  )
  
  if (file.exists(step3_input_rds)) {
    pilot_function_reviewed <- readRDS(
      step3_input_rds
    )
  } else if (file.exists(step3_input_csv)) {
    pilot_function_reviewed <- read_csv(
      step3_input_csv,
      show_col_types = FALSE
    )
  } else {
    stop(
      "Reviewed Step 2B input was not found. Run Step 2B first."
    )
  }
}


required_step3_columns <- c(
  "lpa_number",
  "narrative",
  "claim_key",
  "function_unit_key",
  "source_unit_index",
  "excerpt_start",
  "claim_text",
  "context_before",
  "context_after",
  "discourse_function_reviewed",
  "codable_reviewed"
)

missing_step3_columns <- setdiff(
  required_step3_columns,
  names(pilot_function_reviewed)
)

if (length(missing_step3_columns) > 0) {
  stop(
    "Reviewed Step 2B input is missing columns: ",
    paste(
      missing_step3_columns,
      collapse = ", "
    )
  )
}

if (anyDuplicated(pilot_function_reviewed$claim_key)) {
  stop(
    "claim_key is not unique in the reviewed Step 2B input."
  )
}

if (
  any(is.na(pilot_function_reviewed$function_unit_key)) ||
  any(!nzchar(str_trim(
    pilot_function_reviewed$function_unit_key
  )))
) {
  stop(
    "function_unit_key contains missing or blank values."
  )
}

if (
  any(is.na(pilot_function_reviewed$claim_text)) ||
  any(!nzchar(str_trim(
    pilot_function_reviewed$claim_text
  )))
) {
  stop(
    "claim_text contains missing or blank values."
  )
}

if (any(is.na(pilot_function_reviewed$codable_reviewed))) {
  stop(
    "codable_reviewed contains missing values."
  )
}

if (nrow(pilot_function_reviewed) != 117L) {
  stop(
    "This pilot expects exactly 117 reviewed rows; found ",
    nrow(pilot_function_reviewed),
    "."
  )
}

if (sum(pilot_function_reviewed$codable_reviewed) != 92L) {
  stop(
    "This pilot expects exactly 92 reviewed codeable rows."
  )
}


# =============================================================
# Step 3 configuration and output paths
# =============================================================

FRAME_MODEL <- "gpt-4o-mini"
FRAME_PROMPT_VERSION <- "v4_frame_1"
FRAME_TEMPERATURE <- 0
FRAME_MAX_RETRIES <- 3L
FRAME_SLEEP_SECONDS <- 2

frame_cache_path <- here(
  output_dir,
  "semantic_v4_pilot_frames_v1_raw.rds"
)

frame_coded_csv <- here(
  output_dir,
  "semantic_v4_pilot_frames_v1.csv"
)

frame_coded_rds <- here(
  output_dir,
  "semantic_v4_pilot_frames_v1.rds"
)

allowed_frame_confidence <- c(
  "high",
  "medium",
  "low"
)


# =============================================================
# Helper functions
# =============================================================

normalize_frame_text_v4 <- function(x) {
  x %>%
    str_replace_all("[\\r\\n\\t]+", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim() %>%
    str_to_lower()
}


frame_prompt_text_v4 <- function(x) {
  if (
    is.null(x) ||
    length(x) == 0 ||
    all(is.na(x))
  ) {
    return("")
  }
  
  as.character(x[[1]])
}


frame_scalar_character_v4 <- function(x) {
  if (
    is.null(x) ||
    length(x) == 0 ||
    all(is.na(x))
  ) {
    return(NA_character_)
  }
  
  as.character(x[[1]])
}


frame_scalar_logical_v4 <- function(x) {
  if (
    is.null(x) ||
    length(x) == 0 ||
    all(is.na(x))
  ) {
    return(NA)
  }
  
  if (is.logical(x[[1]])) {
    return(x[[1]])
  }
  
  value <- str_to_lower(
    str_trim(
      as.character(x[[1]])
    )
  )
  
  if (value %in% c("true", "yes", "1")) {
    return(TRUE)
  }
  
  if (value %in% c("false", "no", "0")) {
    return(FALSE)
  }
  
  NA
}


frame_scalar_integer_v4 <- function(x) {
  if (
    is.null(x) ||
    length(x) == 0 ||
    all(is.na(x))
  ) {
    return(0L)
  }
  
  as.integer(x[[1]])
}


frame_valid_choice_v4 <- function(
    x,
    allowed,
    default = NA_character_
) {
  value <- frame_scalar_character_v4(x)
  
  if (
    is.na(value) ||
    !value %in% allowed
  ) {
    return(default)
  }
  
  value
}


frame_evidence_vector_v4 <- function(x) {
  if (
    is.null(x) ||
    length(x) == 0
  ) {
    return(character(0))
  }
  
  values <- as.character(
    unlist(
      x,
      use.names = FALSE
    )
  )
  
  values <- values[
    !is.na(values) &
      str_trim(values) != ""
  ]
  
  unique(values)
}


frame_as_sublist_v4 <- function(x) {
  if (is.list(x)) {
    return(x)
  }
  
  list()
}


# Empty evidence is valid when all four frame signals are FALSE.
frame_evidence_is_exact_v4 <- function(
    claim_text,
    evidence
) {
  evidence <- frame_evidence_vector_v4(
    evidence
  )
  
  if (length(evidence) == 0) {
    return(TRUE)
  }
  
  normalized_claim <- normalize_frame_text_v4(
    claim_text
  )
  
  all(
    map_lgl(
      evidence,
      function(item) {
        normalized_item <- normalize_frame_text_v4(
          item
        )
        
        nzchar(normalized_item) &&
          str_detect(
            normalized_claim,
            fixed(normalized_item)
          )
      }
    )
  )
}


frame_signal_is_consistent_v4 <- function(
    present,
    evidence
) {
  evidence <- frame_evidence_vector_v4(
    evidence
  )
  
  if (is.na(present)) {
    return(FALSE)
  }
  
  if (isTRUE(present)) {
    return(length(evidence) > 0)
  }
  
  length(evidence) == 0
}


build_frame_review_trigger_v4 <- function(
    schema_valid,
    evidence_valid,
    confidence,
    reasoning,
    api_error
) {
  reasons <- character(0)
  
  if (
    !is.na(api_error) &&
    nzchar(str_trim(api_error))
  ) {
    reasons <- c(
      reasons,
      "api_or_parse_error"
    )
  }
  
  if (!isTRUE(schema_valid)) {
    reasons <- c(
      reasons,
      "invalid_frame_schema"
    )
  }
  
  if (!isTRUE(evidence_valid)) {
    reasons <- c(
      reasons,
      "non_exact_evidence"
    )
  }
  
  if (
    is.na(confidence) ||
    confidence %in% c("medium", "low")
  ) {
    reasons <- c(
      reasons,
      "non_high_confidence"
    )
  }
  
  if (
    is.na(reasoning) ||
    !nzchar(str_trim(reasoning))
  ) {
    reasons <- c(
      reasons,
      "missing_reasoning"
    )
  }
  
  paste(
    unique(reasons),
    collapse = ";"
  )
}


# =============================================================
# Reuse exact duplicate text within the same project
# =============================================================

pilot_frame_input <- pilot_function_reviewed %>%
  mutate(
    frame_unit_key = function_unit_key,
    frame_eligible = codable_reviewed
  )

eligible_frame_rows <- pilot_frame_input %>%
  filter(frame_eligible)

frame_key_text_check <- eligible_frame_rows %>%
  mutate(
    claim_text_normalized_step3 =
      normalize_frame_text_v4(claim_text)
  ) %>%
  group_by(frame_unit_key) %>%
  summarise(
    n_texts = n_distinct(
      claim_text_normalized_step3
    ),
    .groups = "drop"
  )

if (any(frame_key_text_check$n_texts != 1L)) {
  stop(
    "A frame_unit_key maps to more than one claim_text."
  )
}

frame_units <- eligible_frame_rows %>%
  arrange(
    lpa_number,
    narrative,
    source_unit_index,
    excerpt_start
  ) %>%
  group_by(frame_unit_key) %>%
  slice_head(n = 1L) %>%
  ungroup()

if (nrow(frame_units) != 90L) {
  stop(
    "This pilot expects 90 unique codeable texts; found ",
    nrow(frame_units),
    "."
  )
}

cat(
  "Step 3 reviewed rows:",
  nrow(pilot_frame_input),
  "\n"
)

cat(
  "Frame-eligible rows:",
  nrow(eligible_frame_rows),
  "\n"
)

cat(
  "Unique texts to frame-code:",
  nrow(frame_units),
  "\n"
)

cat(
  "API calls avoided by duplicate reuse:",
  nrow(eligible_frame_rows) -
    nrow(frame_units),
  "\n\n"
)


# =============================================================
# Frame-coding prompt
# =============================================================

build_frame_prompt_v4 <- function(row) {
  paste0(
    "You are a careful research coder analysing UK planning application text.\n\n",
    
    "TASK\n",
    "Identify explicit FRAMING SIGNALS in CLAIM_TEXT. ",
    "Code four independent binary fields. This is multi-label coding: ",
    "zero, one or several fields may be true.\n\n",
    
    "FRAME DEFINITION\n",
    "A frame is the value-laden or domain-specific vocabulary through which ",
    "a claim is presented. It is not the same as the concrete outcome ",
    "promised or assessed. Ask: even if the outcome were removed, does the ",
    "remaining vocabulary explicitly invoke this domain?\n\n",
    
    "1. economic\n",
    "Explicit jobs or employment, investment, costs, finance, return, ",
    "viability, commercial or business activity, economic growth or ",
    "activity, GVA, revenue, market or property value, planning gain, ",
    "or monetary valuation. A number or pound amount without a clear ",
    "financial or valuation function is not sufficient.\n\n",
    
    "2. social\n",
    "Explicit affordability, inequality, community, residents or families, ",
    "participation or cohesion, health and wellbeing, safety, inclusion, ",
    "education, play, cultural identity, social value, accessibility for ",
    "people, or public benefit.\n\n",
    
    "3. environmental\n",
    "Explicit carbon or emissions, energy efficiency, climate, biodiversity, ",
    "ecology, air quality, flood-risk reduction, natural resources, ",
    "environmental performance, or pollution.\n\n",
    
    "4. design_heritage\n",
    "Explicit architecture or townscape quality, listed buildings, ",
    "conservation, heritage assets, visual character, landscape design, ",
    "public-realm design, or restoration.\n\n",
    
    "STRICT RULES\n",
    "- Code only vocabulary that appears in CLAIM_TEXT.\n",
    "- CONTEXT may resolve attribution or a pronoun but may not supply ",
    "a frame or evidence.\n",
    "- Do not infer a frame from the narrative sampling stratum, the speaker, ",
    "the document type, or the fact that this is a development proposal.\n",
    "- Do not infer a frame solely from a concrete outcome.\n",
    "- 'high quality', 'development', 'regeneration', 'improvement', ",
    "'infrastructure', 'amenity' and 'sustainability' alone are not ",
    "sufficient evidence for any frame.\n",
    "- General housing or residential vocabulary alone is not economic. ",
    "Affordable or social housing is social; commercial value or market ",
    "return is economic.\n",
    "- A policy statement can contain framing signals; judge its explicit ",
    "vocabulary in the same way as any other claim.\n",
    "- Every field marked present=true must contain at least one exact ",
    "quotation copied from CLAIM_TEXT.\n",
    "- Every field marked present=false must have an empty evidence array.\n",
    "- Do not silently correct OCR, punctuation, spacing or spelling in ",
    "evidence quotations.\n\n",
    
    "Examples\n",
    "- '£3m of investment to generate £278m of social value for residents' ",
    "=> economic=true and social=true.\n",
    "- 'high quality development improving the public realm' ",
    "=> economic=false; design_heritage is true only when the wording ",
    "explicitly concerns design, townscape, architecture, landscape, ",
    "visual character or heritage.\n",
    "- 'secure economic growth and deliver 3,500 new homes' ",
    "=> economic=true; housing alone does not automatically add a ",
    "social or economic frame.\n",
    "- 'the scheme is sustainable' => all four fields=false because ",
    "generic sustainability alone is insufficient.\n\n",
    
    "CLAIM_TEXT\n<<<\n",
    frame_prompt_text_v4(row$claim_text),
    "\n>>>\n\n",
    
    "CONTEXT_BEFORE — interpretation only\n<<<\n",
    frame_prompt_text_v4(row$context_before),
    "\n>>>\n\n",
    
    "CONTEXT_AFTER — interpretation only\n<<<\n",
    frame_prompt_text_v4(row$context_after),
    "\n>>>\n\n",
    
    "Return ONLY one JSON object in this form:\n",
    "{\n",
    "  \"economic\": {\"present\": false, \"evidence\": []},\n",
    "  \"social\": {\"present\": false, \"evidence\": []},\n",
    "  \"environmental\": {\"present\": false, \"evidence\": []},\n",
    "  \"design_heritage\": {\"present\": false, \"evidence\": []},\n",
    "  \"confidence\": \"high|medium|low\",\n",
    "  \"reasoning\": \"brief explanation of framing vocabulary only\"\n",
    "}"
  )
}


# =============================================================
# API call
# Same endpoint, model family, JSON mode and retry structure as Step 2
# =============================================================

call_openai_frame_v4 <- function(
    prompt,
    max_retries = FRAME_MAX_RETRIES
) {
  api_key <- Sys.getenv("OPENAI_API_KEY")
  
  if (!nzchar(api_key)) {
    stop("OPENAI_API_KEY is not set.")
  }
  
  last_error <- "unknown_error"
  
  for (attempt in seq_len(max_retries)) {
    response <- tryCatch(
      POST(
        url =
          "https://api.openai.com/v1/chat/completions",
        
        add_headers(
          Authorization = paste(
            "Bearer",
            api_key
          ),
          "Content-Type" =
            "application/json"
        ),
        
        body = toJSON(
          list(
            model = FRAME_MODEL,
            
            messages = list(
              list(
                role = "user",
                content = prompt
              )
            ),
            
            response_format = list(
              type = "json_object"
            ),
            
            temperature = FRAME_TEMPERATURE,
            max_tokens = 700
          ),
          auto_unbox = TRUE
        ),
        
        timeout(90)
      ),
      
      error = function(e) {
        last_error <<- e$message
        NULL
      }
    )
    
    if (
      !is.null(response) &&
      status_code(response) == 200
    ) {
      result <- content(
        response,
        as = "parsed",
        encoding = "UTF-8"
      )
      
      return(
        list(
          ok = TRUE,
          
          text =
            result$choices[[1]]$message$content,
          
          tokens_in =
            frame_scalar_integer_v4(
              result$usage$prompt_tokens
            ),
          
          tokens_out =
            frame_scalar_integer_v4(
              result$usage$completion_tokens
            ),
          
          error = NA_character_
        )
      )
    }
    
    if (!is.null(response)) {
      status <- status_code(response)
      last_error <- paste0(
        "HTTP ",
        status
      )
      
      # Ordinary client errors will not improve after retries.
      if (
        status >= 400 &&
        status < 500 &&
        status != 429
      ) {
        break
      }
    }
    
    if (attempt < max_retries) {
      backoff <- min(
        60,
        attempt * 10
      )
      
      cat(
        sprintf(
          paste0(
            "  [attempt %d failed: %s; ",
            "retry after %d sec]\n"
          ),
          attempt,
          last_error,
          backoff
        )
      )
      
      Sys.sleep(backoff)
    }
  }
  
  list(
    ok = FALSE,
    text = NA_character_,
    tokens_in = 0L,
    tokens_out = 0L,
    error = last_error
  )
}


# =============================================================
# Cache structure
# =============================================================

empty_frame_cache <- tibble(
  frame_unit_key = character(),
  representative_claim_key = character(),
  frame_economic = logical(),
  frame_social = logical(),
  frame_environmental = logical(),
  frame_design_heritage = logical(),
  frame_economic_evidence = character(),
  frame_social_evidence = character(),
  frame_environmental_evidence = character(),
  frame_design_heritage_evidence = character(),
  frame_confidence = character(),
  frame_reasoning = character(),
  frame_schema_valid = logical(),
  frame_evidence_valid = logical(),
  frame_needs_review = logical(),
  frame_review_trigger = character(),
  frame_api_raw = character(),
  frame_api_error = character(),
  frame_tokens_in = integer(),
  frame_tokens_out = integer(),
  frame_model = character(),
  frame_prompt_version = character()
)

if (file.exists(frame_cache_path)) {
  cat("=== Loading Step 3 cache ===\n")
  
  frame_cache <- readRDS(
    frame_cache_path
  )
  
  missing_frame_cache_columns <- setdiff(
    names(empty_frame_cache),
    names(frame_cache)
  )
  
  if (length(missing_frame_cache_columns) > 0) {
    stop(
      "Existing Step 3 cache is incompatible. Missing columns: ",
      paste(
        missing_frame_cache_columns,
        collapse = ", "
      )
    )
  }
  
  frame_cache <- frame_cache %>%
    select(
      all_of(
        names(empty_frame_cache)
      )
    ) %>%
    distinct(
      frame_unit_key,
      .keep_all = TRUE
    )
  
} else {
  frame_cache <- empty_frame_cache
}

completed_frame_keys <- frame_cache %>%
  filter(
    is.na(frame_api_error),
    frame_schema_valid %in% TRUE,
    frame_evidence_valid %in% TRUE,
    !is.na(frame_economic),
    !is.na(frame_social),
    !is.na(frame_environmental),
    !is.na(frame_design_heritage),
    frame_model == FRAME_MODEL,
    frame_prompt_version ==
      FRAME_PROMPT_VERSION
  ) %>%
  pull(frame_unit_key)

to_code_frames <- frame_units %>%
  filter(
    !frame_unit_key %in%
      completed_frame_keys
  )

cat(
  "Unique frame texts remaining:",
  nrow(to_code_frames),
  "\n\n"
)


# =============================================================
# Run Step 3
# =============================================================

session_frame_tokens_in <- 0L
session_frame_tokens_out <- 0L

for (i in seq_len(nrow(to_code_frames))) {
  row <- to_code_frames[i, ]
  
  api_result <- call_openai_frame_v4(
    build_frame_prompt_v4(row)
  )
  
  if (isTRUE(api_result$ok)) {
    session_frame_tokens_in <-
      session_frame_tokens_in +
      api_result$tokens_in
    
    session_frame_tokens_out <-
      session_frame_tokens_out +
      api_result$tokens_out
    
    parsed <- tryCatch(
      fromJSON(
        api_result$text,
        simplifyVector = FALSE
      ),
      error = function(e) NULL
    )
    
    if (is.null(parsed)) {
      new_result <- empty_frame_cache %>%
        add_row(
          frame_unit_key =
            row$frame_unit_key,
          
          representative_claim_key =
            row$claim_key,
          
          frame_needs_review = TRUE,
          frame_review_trigger =
            "api_or_parse_error",
          
          frame_api_raw =
            api_result$text,
          
          frame_api_error =
            "invalid_json",
          
          frame_tokens_in =
            api_result$tokens_in,
          
          frame_tokens_out =
            api_result$tokens_out,
          
          frame_model =
            FRAME_MODEL,
          
          frame_prompt_version =
            FRAME_PROMPT_VERSION
        )
      
    } else {
      economic <- frame_as_sublist_v4(
        parsed$economic
      )
      
      social <- frame_as_sublist_v4(
        parsed$social
      )
      
      environmental <- frame_as_sublist_v4(
        parsed$environmental
      )
      
      design_heritage <- frame_as_sublist_v4(
        parsed$design_heritage
      )
      
      economic_present <- frame_scalar_logical_v4(
        economic$present
      )
      
      social_present <- frame_scalar_logical_v4(
        social$present
      )
      
      environmental_present <- frame_scalar_logical_v4(
        environmental$present
      )
      
      design_heritage_present <-
        frame_scalar_logical_v4(
          design_heritage$present
        )
      
      economic_evidence <- frame_evidence_vector_v4(
        economic$evidence
      )
      
      social_evidence <- frame_evidence_vector_v4(
        social$evidence
      )
      
      environmental_evidence <-
        frame_evidence_vector_v4(
          environmental$evidence
        )
      
      design_heritage_evidence <-
        frame_evidence_vector_v4(
          design_heritage$evidence
        )
      
      all_frame_evidence <- c(
        economic_evidence,
        social_evidence,
        environmental_evidence,
        design_heritage_evidence
      )
      
      schema_valid <- all(
        frame_signal_is_consistent_v4(
          economic_present,
          economic_evidence
        ),
        frame_signal_is_consistent_v4(
          social_present,
          social_evidence
        ),
        frame_signal_is_consistent_v4(
          environmental_present,
          environmental_evidence
        ),
        frame_signal_is_consistent_v4(
          design_heritage_present,
          design_heritage_evidence
        )
      )
      
      evidence_valid <- frame_evidence_is_exact_v4(
        row$claim_text,
        all_frame_evidence
      )
      
      confidence_label <- frame_valid_choice_v4(
        parsed$confidence,
        allowed_frame_confidence,
        default = "low"
      )
      
      reasoning_value <- frame_scalar_character_v4(
        parsed$reasoning
      )
      
      api_error_value <- if (
        !schema_valid
      ) {
        "invalid_frame_schema"
      } else {
        NA_character_
      }
      
      review_trigger <- build_frame_review_trigger_v4(
        schema_valid = schema_valid,
        evidence_valid = evidence_valid,
        confidence = confidence_label,
        reasoning = reasoning_value,
        api_error = api_error_value
      )
      
      new_result <- tibble(
        frame_unit_key =
          row$frame_unit_key,
        
        representative_claim_key =
          row$claim_key,
        
        frame_economic =
          economic_present,
        
        frame_social =
          social_present,
        
        frame_environmental =
          environmental_present,
        
        frame_design_heritage =
          design_heritage_present,
        
        frame_economic_evidence = paste(
          economic_evidence,
          collapse = " || "
        ),
        
        frame_social_evidence = paste(
          social_evidence,
          collapse = " || "
        ),
        
        frame_environmental_evidence = paste(
          environmental_evidence,
          collapse = " || "
        ),
        
        frame_design_heritage_evidence = paste(
          design_heritage_evidence,
          collapse = " || "
        ),
        
        frame_confidence =
          confidence_label,
        
        frame_reasoning =
          reasoning_value,
        
        frame_schema_valid =
          schema_valid,
        
        frame_evidence_valid =
          evidence_valid,
        
        frame_needs_review =
          review_trigger != "",
        
        frame_review_trigger =
          review_trigger,
        
        frame_api_raw =
          api_result$text,
        
        frame_api_error =
          api_error_value,
        
        frame_tokens_in =
          as.integer(api_result$tokens_in),
        
        frame_tokens_out =
          as.integer(api_result$tokens_out),
        
        frame_model =
          FRAME_MODEL,
        
        frame_prompt_version =
          FRAME_PROMPT_VERSION
      )
    }
    
  } else {
    new_result <- empty_frame_cache %>%
      add_row(
        frame_unit_key =
          row$frame_unit_key,
        
        representative_claim_key =
          row$claim_key,
        
        frame_needs_review = TRUE,
        frame_review_trigger =
          "api_or_parse_error",
        
        frame_api_error =
          api_result$error,
        
        frame_tokens_in = 0L,
        frame_tokens_out = 0L,
        
        frame_model =
          FRAME_MODEL,
        
        frame_prompt_version =
          FRAME_PROMPT_VERSION
      )
  }
  
  # Replace any earlier failed or invalid result for this text.
  frame_cache <- frame_cache %>%
    filter(
      frame_unit_key !=
        row$frame_unit_key
    ) %>%
    bind_rows(new_result)
  
  cat(
    sprintf(
      paste0(
        "[%d/%d] %s -- E:%s | S:%s | ",
        "N:%s | D:%s | review:%s\n"
      ),
      i,
      nrow(to_code_frames),
      row$claim_key,
      ifelse(
        is.na(new_result$frame_economic),
        "NA",
        new_result$frame_economic
      ),
      ifelse(
        is.na(new_result$frame_social),
        "NA",
        new_result$frame_social
      ),
      ifelse(
        is.na(new_result$frame_environmental),
        "NA",
        new_result$frame_environmental
      ),
      ifelse(
        is.na(new_result$frame_design_heritage),
        "NA",
        new_result$frame_design_heritage
      ),
      ifelse(
        is.na(new_result$frame_needs_review),
        "NA",
        new_result$frame_needs_review
      )
    )
  )
  
  # Incremental save every 10 calls.
  if (i %% 10 == 0) {
    saveRDS(
      frame_cache,
      frame_cache_path
    )
    
    cat("  [Step 3 cache saved]\n")
  }
  
  if (FRAME_SLEEP_SECONDS > 0) {
    Sys.sleep(FRAME_SLEEP_SECONDS)
  }
}

saveRDS(
  frame_cache,
  frame_cache_path
)


# =============================================================
# Join frame results back to all 117 reviewed rows
# =============================================================

pilot_frames_coded <- pilot_frame_input %>%
  left_join(
    frame_cache,
    by = "frame_unit_key"
  ) %>%
  arrange(
    lpa_number,
    narrative,
    source_unit_index,
    excerpt_start
  )

if (nrow(pilot_frames_coded) != 117L) {
  stop("Step 3 changed the number of pilot rows.")
}

if (anyDuplicated(pilot_frames_coded$claim_key)) {
  stop("Step 3 produced duplicate claim_key values.")
}

write_csv(
  pilot_frames_coded,
  frame_coded_csv
)

saveRDS(
  pilot_frames_coded,
  frame_coded_rds
)


# =============================================================
# Console checks
# =============================================================

frame_success <- pilot_frames_coded %>%
  filter(
    frame_eligible,
    is.na(frame_api_error),
    frame_schema_valid %in% TRUE
  )

frame_failures <- pilot_frames_coded %>%
  filter(
    frame_eligible,
    !is.na(frame_api_error) |
      !(frame_schema_valid %in% TRUE)
  )

cat("\n")
cat("============================================\n")
cat("V4 PILOT STEP 3 COMPLETED\n")
cat("============================================\n")

cat(
  "Rows retained:",
  nrow(pilot_frames_coded),
  "\n"
)

cat(
  "Frame-eligible rows:",
  sum(pilot_frames_coded$frame_eligible),
  "\n"
)

cat(
  "Unique frame texts:",
  nrow(frame_units),
  "\n"
)

cat(
  "Successful eligible rows:",
  nrow(frame_success),
  "\n"
)

cat(
  "Eligible rows with API/schema failure:",
  nrow(frame_failures),
  "\n"
)

cat(
  "Session input tokens:",
  session_frame_tokens_in,
  "\n"
)

cat(
  "Session output tokens:",
  session_frame_tokens_out,
  "\n\n"
)


cat("Frame signals across successful eligible rows:\n")

frame_success %>%
  summarise(
    economic = sum(frame_economic),
    social = sum(frame_social),
    environmental = sum(frame_environmental),
    design_heritage =
      sum(frame_design_heritage),
    no_explicit_frame = sum(
      !frame_economic &
        !frame_social &
        !frame_environmental &
        !frame_design_heritage
    )
  ) %>%
  print()


cat("\nFrame combinations:\n")

frame_success %>%
  mutate(
    frame_combination = pmap_chr(
      list(
        frame_economic,
        frame_social,
        frame_environmental,
        frame_design_heritage
      ),
      function(e, s, n, d) {
        labels <- c(
          if (isTRUE(e)) "economic",
          if (isTRUE(s)) "social",
          if (isTRUE(n)) "environmental",
          if (isTRUE(d)) "design_heritage"
        )
        
        if (length(labels) == 0) {
          return("none")
        }
        
        paste(
          labels,
          collapse = "+"
        )
      }
    )
  ) %>%
  count(
    frame_combination,
    name = "n"
  ) %>%
  mutate(
    pct = round(
      n / sum(n) * 100,
      1
    )
  ) %>%
  arrange(desc(n)) %>%
  print(n = Inf)


cat("\nRows requiring review or retry:\n")

pilot_frames_coded %>%
  filter(
    frame_eligible,
    frame_needs_review %in% TRUE |
      !is.na(frame_api_error)
  ) %>%
  select(
    lpa_number,
    narrative,
    claim_key,
    claim_text,
    frame_economic,
    frame_social,
    frame_environmental,
    frame_design_heritage,
    frame_confidence,
    frame_schema_valid,
    frame_evidence_valid,
    frame_review_trigger,
    frame_api_error,
    frame_reasoning
  ) %>%
  print(
    n = Inf,
    width = Inf
  )


cat("\nRepeated codeable texts whose frame result was reused:\n")

pilot_frames_coded %>%
  filter(
    frame_eligible,
    duplicated(frame_unit_key) |
      duplicated(
        frame_unit_key,
        fromLast = TRUE
      )
  ) %>%
  select(
    lpa_number,
    narrative,
    claim_key,
    frame_unit_key,
    frame_economic,
    frame_social,
    frame_environmental,
    frame_design_heritage,
    claim_text
  ) %>%
  print(
    n = Inf,
    width = Inf
  )


cat("\nNew Step 3 files saved:\n")
cat("1.", frame_cache_path, "\n")
cat("2.", frame_coded_csv, "\n")
cat("3.", frame_coded_rds, "\n")
# =============================================================
# STEP 3B: Apply reviewed pilot frame corrections
# Paste below STEP 3 in scripts/13_semantic_recode_v4_pilot.R
#
# This step does not call the API and does not overwrite Step 3.
# It preserves the API result, applies the completed full-pilot review,
# validates exact evidence, and writes reviewed data plus an audit table.
# =============================================================

if (!exists("output_dir")) {
  source(here::here("scripts", "00_setup.R"))
}

library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tibble)


# =============================================================
# Load Step 3 result when this block is run independently
# =============================================================

if (!exists("pilot_frames_coded")) {
  step3b_input_rds <- here(
    output_dir,
    "semantic_v4_pilot_frames_v1.rds"
  )
  
  step3b_input_csv <- here(
    output_dir,
    "semantic_v4_pilot_frames_v1.csv"
  )
  
  if (file.exists(step3b_input_rds)) {
    pilot_frames_coded <- readRDS(
      step3b_input_rds
    )
  } else if (file.exists(step3b_input_csv)) {
    pilot_frames_coded <- read_csv(
      step3b_input_csv,
      show_col_types = FALSE
    )
  } else {
    stop(
      "Step 3 input was not found. Run Step 3 first."
    )
  }
}


required_step3b_columns <- c(
  "lpa_number",
  "narrative",
  "claim_key",
  "claim_text",
  "frame_unit_key",
  "frame_eligible",
  "frame_economic",
  "frame_social",
  "frame_environmental",
  "frame_design_heritage",
  "frame_economic_evidence",
  "frame_social_evidence",
  "frame_environmental_evidence",
  "frame_design_heritage_evidence"
)

missing_step3b_columns <- setdiff(
  required_step3b_columns,
  names(pilot_frames_coded)
)

if (length(missing_step3b_columns) > 0) {
  stop(
    "Step 3 input is missing columns: ",
    paste(
      missing_step3b_columns,
      collapse = ", "
    )
  )
}

if (nrow(pilot_frames_coded) != 117L) {
  stop(
    "Step 3B expects 117 pilot rows; found ",
    nrow(pilot_frames_coded),
    "."
  )
}

if (anyDuplicated(pilot_frames_coded$claim_key)) {
  stop("claim_key is not unique in Step 3 input.")
}

if (sum(pilot_frames_coded$frame_eligible) != 92L) {
  stop("Step 3B expects exactly 92 frame-eligible rows.")
}

if (
  n_distinct(
    pilot_frames_coded$frame_unit_key[
      pilot_frames_coded$frame_eligible
    ]
  ) != 90L
) {
  stop("Step 3B expects exactly 90 eligible frame units.")
}


# =============================================================
# Output paths
# =============================================================

frame_reviewed_csv <- here(
  output_dir,
  "semantic_v4_pilot_frames_v1_reviewed.csv"
)

frame_reviewed_rds <- here(
  output_dir,
  "semantic_v4_pilot_frames_v1_reviewed.rds"
)

frame_review_audit_csv <- here(
  output_dir,
  "semantic_v4_pilot_frames_v1_review_audit.csv"
)

FRAME_REVIEW_VERSION <- "v4_frame_review_1"


# =============================================================
# One reviewed record per frame_unit_key
#
# e/s/n/d are the final economic, social, environmental and
# design_heritage labels. Evidence fields contain exact quotations
# separated by " || "; NA means no evidence for a FALSE label.
# =============================================================

review_frame_v4 <- function(
    frame_unit_key,
    e,
    s,
    n,
    d,
    e_evidence = NA_character_,
    s_evidence = NA_character_,
    n_evidence = NA_character_,
    d_evidence = NA_character_,
    note
) {
  tibble(
    frame_unit_key = frame_unit_key,
    review_economic = e,
    review_social = s,
    review_environmental = n,
    review_design_heritage = d,
    review_economic_evidence = e_evidence,
    review_social_evidence = s_evidence,
    review_environmental_evidence = n_evidence,
    review_design_heritage_evidence = d_evidence,
    frame_review_note = note
  )
}


frame_review_map <- bind_rows(
  review_frame_v4(
    "PA/13/02966|community|733|1|290",
    TRUE, TRUE, FALSE, FALSE,
    "jobs",
    "community || leisure facilities",
    note = paste0(
      "Labels retained; evidence narrowed to exact, frame-relevant ",
      "phrases."
    )
  ),
  
  review_frame_v4(
    "PA/13/02966|public_realm|961|1|243",
    FALSE, TRUE, FALSE, FALSE,
    s_evidence = "support local facilities",
    note = paste0(
      "Green/public space is a concrete provision here, without explicit ",
      "ecology, biodiversity or environmental-performance vocabulary."
    )
  ),
  
  review_frame_v4(
    "PA/13/02966|public_realm|2171|1|140",
    FALSE, TRUE, FALSE, TRUE,
    s_evidence = "safe to navigate",
    d_evidence = "well-connected public realm",
    note = paste0(
      "Safety is an explicit social signal; removed invented design ",
      "evidence."
    )
  ),
  
  review_frame_v4(
    "PA/13/02966|regeneration|305|1|104",
    FALSE, FALSE, FALSE, FALSE,
    note = paste0(
      "Housing regeneration alone does not explicitly invoke architecture, ",
      "townscape, landscape, visual character or heritage."
    )
  ),
  
  review_frame_v4(
    "PA/13/02966|regeneration|647|1|259",
    TRUE, FALSE, FALSE, FALSE,
    e_evidence = "offices and retail spaces",
    note = paste0(
      "Offices and retail explicitly invoke commercial/business activity; ",
      "the listed physical outputs do not by themselves establish a design ",
      "frame."
    )
  ),
  
  review_frame_v4(
    "PA/13/02966|sustainability|731|1|218",
    FALSE, FALSE, FALSE, FALSE,
    note = paste0(
      "High quality, sustainable and mixed-use development are generic ",
      "project descriptors under the strict frame rule."
    )
  ),
  
  review_frame_v4(
    "PA/13/02966|sustainability|749|1|352",
    TRUE, TRUE, FALSE, FALSE,
    e_evidence = "jobs",
    s_evidence = "facilities and amenity space",
    note = paste0(
      "Jobs are an explicit economic signal; facilities and amenity space ",
      "provide the social signal."
    )
  ),
  
  review_frame_v4(
    "PA/13/02966|sustainability|1312|1|273",
    FALSE, TRUE, FALSE, FALSE,
    s_evidence = "affordable family homes",
    note = paste0(
      "Affordable family homes are social. Sustainable parking and EV ",
      "facilities are concrete measures, but this sentence contains no ",
      "explicit emissions, climate or environmental-performance claim."
    )
  ),
  
  review_frame_v4(
    "PA/14/00944|community|1530|1|233",
    TRUE, TRUE, FALSE, TRUE,
    "commercial services || employment opportunities",
    "social and community infrastructure",
    d_evidence = paste0(
      "Design of new development should interface with surrounding land"
    ),
    note = paste0(
      "Commercial services and employment are economic; community ",
      "infrastructure is social; the relationship between new development ",
      "and surrounding land is explicit design framing. Invented social ",
      "evidence was removed."
    )
  ),
  
  review_frame_v4(
    "PA/14/00944|public_realm|30|1|140",
    TRUE, FALSE, FALSE, TRUE,
    "retail offer",
    d_evidence = "activate the public realm",
    note = paste0(
      "Retail is economic; activation of the public realm is an explicit ",
      "urban-design signal. High quality alone is not used as design ",
      "evidence."
    )
  ),
  
  review_frame_v4(
    "PA/14/00944|public_realm|220|1|172",
    TRUE, FALSE, FALSE, TRUE,
    "retail (Class A1-A4) provision",
    d_evidence = "active frontages || animation to the public realm",
    note = paste0(
      "Retail is economic; active frontages and public-realm animation are ",
      "explicit urban-design vocabulary."
    )
  ),
  
  review_frame_v4(
    "PA/14/00944|regeneration|173|1|111",
    FALSE, FALSE, FALSE, FALSE,
    note = paste0(
      "Physical regeneration, tired buildings and mixed-use development ",
      "do not alone satisfy the design_heritage definition."
    )
  ),
  
  review_frame_v4(
    "PA/14/00944|regeneration|395|1|265",
    TRUE, FALSE, FALSE, FALSE,
    e_evidence = "jobs",
    note = paste0(
      "Jobs are economic. General new-homes vocabulary does not ",
      "automatically create a social frame under the prompt's strict rule."
    )
  ),
  
  review_frame_v4(
    "PA/14/00944|regeneration|830|1|148",
    FALSE, FALSE, FALSE, TRUE,
    d_evidence = "active frontages",
    note = paste0(
      "Label retained; active frontages supplies the explicit design ",
      "evidence, rather than generic improvement/regeneration wording."
    )
  ),
  
  review_frame_v4(
    "PA/14/00944|regeneration|1389|1|264",
    TRUE, TRUE, FALSE, FALSE,
    "economic convergence || jobs",
    "social and economic convergence",
    note = paste0(
      "Labels retained; the non-contiguous quotation 'social convergence' ",
      "was replaced by exact text."
    )
  ),
  
  review_frame_v4(
    "PA/14/00944|regeneration|1391|1|1167",
    TRUE, TRUE, TRUE, FALSE,
    "employment capacity",
    paste0(
      "social and other infrastructure || ",
      "inclusive access including cycling and walking"
    ),
    "improvements to environmental quality",
    note = paste0(
      "Employment capacity was a missed economic signal; social and ",
      "environmental signals are retained with narrowed exact evidence."
    )
  ),
  
  review_frame_v4(
    "PA/14/00944|sustainability|39|1|224",
    FALSE, TRUE, FALSE, FALSE,
    s_evidence = "sustainable community",
    note = paste0(
      "Community is an explicit social signal. World-class development is ",
      "not specific architecture, townscape or heritage vocabulary."
    )
  ),
  
  review_frame_v4(
    "PA/14/00944|sustainability|1382|1|219",
    FALSE, TRUE, FALSE, FALSE,
    s_evidence = "safely",
    note = paste0(
      "Safety is an explicit social signal. Quality, sustainability and ",
      "waterspace/land use do not establish design_heritage here."
    )
  ),
  
  review_frame_v4(
    "PA/14/00944|sustainability|2146|1|280",
    TRUE, TRUE, FALSE, FALSE,
    "retail (Class A1-A4) and office use",
    paste0(
      "affordable housing || mixed and balanced communities || ",
      "inclusive communities"
    ),
    note = paste0(
      "Retail and office uses explicitly invoke commercial/business ",
      "activity; the social label is retained."
    )
  ),
  
  review_frame_v4(
    "PA/14/00944|sustainability|2150|1|280",
    FALSE, TRUE, FALSE, FALSE,
    s_evidence = "Social Sustainability Assessment || quality of life",
    note = paste0(
      "Label retained; evidence capitalization was restored exactly to the ",
      "claim text."
    )
  ),
  
  review_frame_v4(
    "PA/24/00922|community|434|1|185",
    TRUE, TRUE, FALSE, FALSE,
    "community investment",
    paste0(
      "Social Value Residents Steering Group || ",
      "residents interested in informing"
    ),
    note = paste0(
      "Investment explicitly invokes the economic frame, while residents, ",
      "social value and participation provide the social frame."
    )
  ),
  
  review_frame_v4(
    "PA/24/00922|community|695|1|150",
    FALSE, TRUE, FALSE, FALSE,
    s_evidence = "community centre and nursery",
    note = paste0(
      "Community and nursery provision explicitly invoke the social domain, ",
      "even though the sentence is descriptive."
    )
  ),
  
  review_frame_v4(
    "PA/24/00922|community|1868|1|330",
    TRUE, TRUE, TRUE, TRUE,
    "employment growth",
    "affordable housing outputs || supporting community infrastructure",
    "local environment",
    "townscape",
    note = paste0(
      "Labels retained; evidence was narrowed so each quotation supports ",
      "only its assigned frame."
    )
  ),
  
  review_frame_v4(
    "PA/24/00922|community|1895|1|131",
    FALSE, TRUE, FALSE, TRUE,
    s_evidence = "neighbouring communities",
    d_evidence = "urban fabric",
    note = paste0(
      "Neighbouring communities provide the social signal; urban fabric is ",
      "explicit townscape/urban-design vocabulary."
    )
  ),
  
  review_frame_v4(
    "PA/24/00922|public_realm|630|1|252",
    FALSE, FALSE, FALSE, FALSE,
    note = paste0(
      "A list of green spaces and public-realm outputs does not itself state ",
      "design quality, ecology or environmental performance."
    )
  ),
  
  review_frame_v4(
    "PA/24/00922|public_realm|1367|1|215",
    FALSE, TRUE, FALSE, TRUE,
    s_evidence = "accessibility and usability",
    d_evidence = paste0(
      "landscaping and open space strategy || exemplary public realm"
    ),
    note = paste0(
      "Labels retained; evidence was narrowed to explicit accessibility and ",
      "landscape/public-realm design vocabulary."
    )
  ),
  
  review_frame_v4(
    "PA/24/00922|regeneration|414|1|256",
    FALSE, FALSE, FALSE, TRUE,
    d_evidence = "tall buildings || tall building zone",
    note = paste0(
      "Design label retained, but invented 'townscape character' evidence ",
      "was replaced by exact built-form vocabulary."
    )
  ),
  
  review_frame_v4(
    "PA/24/00922|regeneration|1931|1|228",
    FALSE, TRUE, FALSE, FALSE,
    s_evidence = "existing residents needs",
    note = paste0(
      "Label retained; evidence was narrowed to residents' needs rather than ",
      "generic transformational-regeneration wording."
    )
  ),
  
  review_frame_v4(
    "PA/24/00922|sustainability|1520|1|256",
    FALSE, TRUE, FALSE, FALSE,
    s_evidence = "family and accessible homes",
    note = paste0(
      "Accessible/family homes are social. Parking, cycling and EV measures ",
      "do not state emissions, climate or environmental performance in this ",
      "claim and are not used to infer an environmental frame."
    )
  ),
  
  review_frame_v4(
    "PA/24/00922|sustainability|1766|1|153",
    FALSE, FALSE, TRUE, FALSE,
    n_evidence = "sustainable drainage systems",
    note = paste0(
      "Label retained; invented 'flood risk' evidence was removed."
    )
  )
)


# =============================================================
# Hard checks on the review map
# =============================================================

if (nrow(frame_review_map) != 30L) {
  stop("Step 3B expects exactly 30 reviewed frame units.")
}

if (anyDuplicated(frame_review_map$frame_unit_key)) {
  stop("frame_review_map contains duplicate frame_unit_key values.")
}

eligible_frame_keys <- pilot_frames_coded %>%
  filter(frame_eligible) %>%
  distinct(frame_unit_key) %>%
  pull(frame_unit_key)

missing_review_keys <- setdiff(
  frame_review_map$frame_unit_key,
  eligible_frame_keys
)

if (length(missing_review_keys) > 0) {
  stop(
    "Step 3B review keys were not found among eligible frame units: ",
    paste(
      missing_review_keys,
      collapse = ", "
    )
  )
}


# =============================================================
# Evidence validation helpers
# =============================================================

blank_to_na_v4 <- function(x) {
  value <- as.character(x)
  value[is.na(value) | str_trim(value) == ""] <- NA_character_
  value
}


split_frame_evidence_v4 <- function(x) {
  if (
    is.null(x) ||
    length(x) == 0 ||
    is.na(x) ||
    !nzchar(str_trim(x))
  ) {
    return(character(0))
  }
  
  str_split(
    x,
    fixed(" || "),
    simplify = FALSE
  )[[1]]
}


reviewed_signal_valid_v4 <- function(
    present,
    evidence,
    claim_text
) {
  quotations <- split_frame_evidence_v4(
    evidence
  )
  
  if (is.na(present)) {
    return(FALSE)
  }
  
  if (!isTRUE(present)) {
    return(length(quotations) == 0)
  }
  
  length(quotations) > 0 &&
    all(
      map_lgl(
        quotations,
        function(item) {
          nzchar(item) &&
            str_detect(
              claim_text,
              fixed(item)
            )
        }
      )
    )
}


reviewed_row_valid_v4 <- function(
    e,
    s,
    n,
    d,
    e_evidence,
    s_evidence,
    n_evidence,
    d_evidence,
    claim_text
) {
  all(
    reviewed_signal_valid_v4(
      e,
      e_evidence,
      claim_text
    ),
    reviewed_signal_valid_v4(
      s,
      s_evidence,
      claim_text
    ),
    reviewed_signal_valid_v4(
      n,
      n_evidence,
      claim_text
    ),
    reviewed_signal_valid_v4(
      d,
      d_evidence,
      claim_text
    )
  )
}


# =============================================================
# Preserve API columns and apply reviewed values
# =============================================================

pilot_frames_reviewed <- pilot_frames_coded %>%
  mutate(
    frame_economic_api = frame_economic,
    frame_social_api = frame_social,
    frame_environmental_api = frame_environmental,
    frame_design_heritage_api = frame_design_heritage,
    frame_economic_evidence_api =
      blank_to_na_v4(frame_economic_evidence),
    frame_social_evidence_api =
      blank_to_na_v4(frame_social_evidence),
    frame_environmental_evidence_api =
      blank_to_na_v4(frame_environmental_evidence),
    frame_design_heritage_evidence_api =
      blank_to_na_v4(frame_design_heritage_evidence)
  ) %>%
  left_join(
    frame_review_map,
    by = "frame_unit_key"
  ) %>%
  mutate(
    frame_manually_reviewed =
      !is.na(frame_review_note),
    
    frame_economic_reviewed = if_else(
      frame_manually_reviewed,
      review_economic,
      frame_economic_api
    ),
    
    frame_social_reviewed = if_else(
      frame_manually_reviewed,
      review_social,
      frame_social_api
    ),
    
    frame_environmental_reviewed = if_else(
      frame_manually_reviewed,
      review_environmental,
      frame_environmental_api
    ),
    
    frame_design_heritage_reviewed = if_else(
      frame_manually_reviewed,
      review_design_heritage,
      frame_design_heritage_api
    ),
    
    frame_economic_evidence_reviewed = if_else(
      frame_manually_reviewed,
      review_economic_evidence,
      frame_economic_evidence_api
    ) %>% blank_to_na_v4(),
    
    frame_social_evidence_reviewed = if_else(
      frame_manually_reviewed,
      review_social_evidence,
      frame_social_evidence_api
    ) %>% blank_to_na_v4(),
    
    frame_environmental_evidence_reviewed = if_else(
      frame_manually_reviewed,
      review_environmental_evidence,
      frame_environmental_evidence_api
    ) %>% blank_to_na_v4(),
    
    frame_design_heritage_evidence_reviewed = if_else(
      frame_manually_reviewed,
      review_design_heritage_evidence,
      frame_design_heritage_evidence_api
    ) %>% blank_to_na_v4(),
    
    frame_labels_changed =
      frame_eligible &
      (
        coalesce(
          frame_economic_api !=
            frame_economic_reviewed,
          FALSE
        ) |
          coalesce(
            frame_social_api !=
              frame_social_reviewed,
            FALSE
          ) |
          coalesce(
            frame_environmental_api !=
              frame_environmental_reviewed,
            FALSE
          ) |
          coalesce(
            frame_design_heritage_api !=
              frame_design_heritage_reviewed,
            FALSE
          )
      ),
    
    frame_evidence_changed =
      frame_eligible &
      (
        coalesce(
          frame_economic_evidence_api,
          ""
        ) != coalesce(
          frame_economic_evidence_reviewed,
          ""
        ) |
          coalesce(
            frame_social_evidence_api,
            ""
          ) != coalesce(
            frame_social_evidence_reviewed,
            ""
          ) |
          coalesce(
            frame_environmental_evidence_api,
            ""
          ) != coalesce(
            frame_environmental_evidence_reviewed,
            ""
          ) |
          coalesce(
            frame_design_heritage_evidence_api,
            ""
          ) != coalesce(
            frame_design_heritage_evidence_reviewed,
            ""
          )
      ),
    
    frame_any_reviewed = case_when(
      !frame_eligible ~ NA,
      TRUE ~
        frame_economic_reviewed |
        frame_social_reviewed |
        frame_environmental_reviewed |
        frame_design_heritage_reviewed
    ),
    
    frame_review_status = case_when(
      !frame_eligible ~ "not_eligible",
      frame_labels_changed ~ "human_corrected",
      frame_evidence_changed ~
        "human_evidence_corrected",
      TRUE ~ "api_accepted_after_full_pilot_review"
    ),
    
    frame_review_source = case_when(
      frame_manually_reviewed ~
        "manual_pilot_adjudication",
      frame_eligible ~ "api_accepted",
      TRUE ~ "not_applicable"
    ),
    
    frame_review_resolved = frame_eligible,
    frame_needs_review_reviewed = FALSE,
    
    frame_auto_flag_for_future = case_when(
      !frame_eligible ~ FALSE,
      TRUE ~
        coalesce(frame_needs_review, FALSE) |
        frame_labels_changed |
        frame_evidence_changed
    ),
    
    frame_review_version =
      FRAME_REVIEW_VERSION,
    
    frame_reasoning_reviewed = case_when(
      frame_manually_reviewed ~
        frame_review_note,
      frame_eligible ~
        frame_reasoning,
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    frame_evidence_valid_reviewed = pmap_lgl(
      list(
        frame_economic_reviewed,
        frame_social_reviewed,
        frame_environmental_reviewed,
        frame_design_heritage_reviewed,
        frame_economic_evidence_reviewed,
        frame_social_evidence_reviewed,
        frame_environmental_evidence_reviewed,
        frame_design_heritage_evidence_reviewed,
        claim_text,
        frame_eligible
      ),
      function(
    e,
    s,
    n,
    d,
    e_evidence,
    s_evidence,
    n_evidence,
    d_evidence,
    claim_text,
    eligible
      ) {
        if (!isTRUE(eligible)) {
          return(TRUE)
        }
        
        reviewed_row_valid_v4(
          e,
          s,
          n,
          d,
          e_evidence,
          s_evidence,
          n_evidence,
          d_evidence,
          claim_text
        )
      }
    )
  ) %>%
  select(
    -review_economic,
    -review_social,
    -review_environmental,
    -review_design_heritage,
    -review_economic_evidence,
    -review_social_evidence,
    -review_environmental_evidence,
    -review_design_heritage_evidence
  ) %>%
  arrange(
    lpa_number,
    narrative,
    source_unit_index,
    excerpt_start
  )


# =============================================================
# Final hard checks
# =============================================================

if (nrow(pilot_frames_reviewed) != 117L) {
  stop("Step 3B changed the number of pilot rows.")
}

if (anyDuplicated(pilot_frames_reviewed$claim_key)) {
  stop("Step 3B produced duplicate claim_key values.")
}

if (
  any(
    !pilot_frames_reviewed$frame_evidence_valid_reviewed
  )
) {
  stop(
    "At least one reviewed evidence quotation is missing from claim_text."
  )
}

reviewed_eligible <- pilot_frames_reviewed %>%
  filter(frame_eligible)

if (
  n_distinct(
    reviewed_eligible$frame_unit_key
  ) != 90L
) {
  stop("Step 3B changed the 90 eligible frame units.")
}

reviewed_frame_counts <- reviewed_eligible %>%
  summarise(
    economic = sum(frame_economic_reviewed),
    social = sum(frame_social_reviewed),
    environmental =
      sum(frame_environmental_reviewed),
    design_heritage =
      sum(frame_design_heritage_reviewed),
    no_explicit_frame =
      sum(!frame_any_reviewed)
  )

expected_frame_counts <- c(
  economic = 18L,
  social = 47L,
  environmental = 14L,
  design_heritage = 33L,
  no_explicit_frame = 11L
)

observed_frame_counts <- c(
  economic = reviewed_frame_counts$economic,
  social = reviewed_frame_counts$social,
  environmental =
    reviewed_frame_counts$environmental,
  design_heritage =
    reviewed_frame_counts$design_heritage,
  no_explicit_frame =
    reviewed_frame_counts$no_explicit_frame
)

if (!identical(
  as.integer(observed_frame_counts),
  as.integer(expected_frame_counts)
)) {
  stop(
    "Unexpected reviewed frame counts: ",
    paste(
      names(observed_frame_counts),
      observed_frame_counts,
      sep = "=",
      collapse = ", "
    )
  )
}

unique_reviewed_units <- pilot_frames_reviewed %>%
  filter(frame_eligible) %>%
  distinct(
    frame_unit_key,
    .keep_all = TRUE
  )

if (sum(unique_reviewed_units$frame_labels_changed) != 21L) {
  stop("Step 3B expects 21 unique frame units with label corrections.")
}

if (sum(unique_reviewed_units$frame_evidence_changed) != 30L) {
  stop("Step 3B expects 30 unique frame units with evidence corrections.")
}

if (
  any(
    reviewed_eligible$frame_needs_review_reviewed
  )
) {
  stop("Reviewed eligible rows should have no unresolved review flags.")
}


# =============================================================
# Save reviewed output and one-row-per-unit audit
# =============================================================

frame_review_audit <- unique_reviewed_units %>%
  filter(frame_manually_reviewed) %>%
  select(
    lpa_number,
    narrative,
    frame_unit_key,
    claim_key,
    claim_text,
    frame_economic_api,
    frame_social_api,
    frame_environmental_api,
    frame_design_heritage_api,
    frame_economic_reviewed,
    frame_social_reviewed,
    frame_environmental_reviewed,
    frame_design_heritage_reviewed,
    frame_economic_evidence_api,
    frame_social_evidence_api,
    frame_environmental_evidence_api,
    frame_design_heritage_evidence_api,
    frame_economic_evidence_reviewed,
    frame_social_evidence_reviewed,
    frame_environmental_evidence_reviewed,
    frame_design_heritage_evidence_reviewed,
    frame_labels_changed,
    frame_evidence_changed,
    frame_review_status,
    frame_review_note
  )

if (nrow(frame_review_audit) != 30L) {
  stop("Step 3B audit should contain exactly 30 unique frame units.")
}

write_csv(
  pilot_frames_reviewed,
  frame_reviewed_csv
)

saveRDS(
  pilot_frames_reviewed,
  frame_reviewed_rds
)

write_csv(
  frame_review_audit,
  frame_review_audit_csv
)


# =============================================================
# Console checks
# =============================================================

cat("\n")
cat("============================================\n")
cat("V4 PILOT STEP 3B COMPLETED\n")
cat("============================================\n")

cat(
  "Rows retained:",
  nrow(pilot_frames_reviewed),
  "\n"
)

cat(
  "Frame-eligible rows:",
  nrow(reviewed_eligible),
  "\n"
)

cat(
  "Unique eligible frame units:",
  nrow(unique_reviewed_units),
  "\n"
)

cat(
  "Unique units with label corrections:",
  sum(unique_reviewed_units$frame_labels_changed),
  "\n"
)

cat(
  "Unique units with evidence corrections:",
  sum(unique_reviewed_units$frame_evidence_changed),
  "\n\n"
)

cat("Reviewed frame signals across 92 eligible rows:\n")
print(reviewed_frame_counts)

cat("\nReviewed frame combinations:\n")

reviewed_eligible %>%
  mutate(
    frame_combination_reviewed = pmap_chr(
      list(
        frame_economic_reviewed,
        frame_social_reviewed,
        frame_environmental_reviewed,
        frame_design_heritage_reviewed
      ),
      function(e, s, n, d) {
        labels <- c(
          if (isTRUE(e)) "economic",
          if (isTRUE(s)) "social",
          if (isTRUE(n)) "environmental",
          if (isTRUE(d)) "design_heritage"
        )
        
        if (length(labels) == 0) {
          return("none")
        }
        
        paste(
          labels,
          collapse = "+"
        )
      }
    )
  ) %>%
  count(
    frame_combination_reviewed,
    name = "n"
  ) %>%
  mutate(
    pct = round(
      n / sum(n) * 100,
      1
    )
  ) %>%
  arrange(desc(n)) %>%
  print(n = Inf)

cat("\nReviewed corrections by frame unit:\n")

frame_review_audit %>%
  select(
    frame_unit_key,
    frame_labels_changed,
    frame_evidence_changed,
    frame_review_note
  ) %>%
  print(
    n = Inf,
    width = Inf
  )

cat("\nNew Step 3B files saved:\n")
cat("1.", frame_reviewed_csv, "\n")
cat("2.", frame_reviewed_rds, "\n")
cat("3.", frame_review_audit_csv, "\n")
# =============================================================
# STEP 4: Independent multi-label outcome coding
# Run as a separate script. Do not paste into the long pilot script.
# =============================================================

if (!exists("output_dir")) {
  source(here::here("scripts", "00_setup.R"))
}

library(dplyr)
library(httr)
library(jsonlite)
library(purrr)
library(readr)
library(stringr)
library(tibble)


# ---- Input ---------------------------------------------------

if (!exists("pilot_frames_reviewed")) {
  input_rds <- here::here(
    output_dir,
    "semantic_v4_pilot_frames_v1_reviewed.rds"
  )
  input_csv <- here::here(
    output_dir,
    "semantic_v4_pilot_frames_v1_reviewed.csv"
  )
  
  if (file.exists(input_rds)) {
    pilot_frames_reviewed <- readRDS(input_rds)
  } else if (file.exists(input_csv)) {
    pilot_frames_reviewed <- read_csv(
      input_csv,
      show_col_types = FALSE
    )
  } else {
    stop("Reviewed Step 3B input was not found.")
  }
}

required_columns <- c(
  "lpa_number", "narrative", "claim_key", "claim_text",
  "context_before", "context_after", "source_unit_index",
  "excerpt_start", "frame_unit_key", "codable_reviewed"
)
missing_columns <- setdiff(
  required_columns,
  names(pilot_frames_reviewed)
)

if (length(missing_columns) > 0) {
  stop(
    "Step 3B input is missing columns: ",
    paste(missing_columns, collapse = ", ")
  )
}
if (nrow(pilot_frames_reviewed) != 117L) {
  stop("Step 4 expects exactly 117 pilot rows.")
}
if (anyDuplicated(pilot_frames_reviewed$claim_key)) {
  stop("claim_key is not unique.")
}
if (any(is.na(pilot_frames_reviewed$codable_reviewed))) {
  stop("codable_reviewed contains missing values.")
}
if (sum(pilot_frames_reviewed$codable_reviewed) != 92L) {
  stop("Step 4 expects exactly 92 codeable rows.")
}


# ---- Configuration and paths --------------------------------

OUTCOME_MODEL <- Sys.getenv(
  "V4_OPENAI_MODEL",
  unset = "gpt-4o-mini"
)
OUTCOME_PROMPT_VERSION <- "v4_outcome_1"
OUTCOME_MAX_RETRIES <- 3L
OUTCOME_SLEEP_SECONDS <- 2

cache_path <- here::here(
  output_dir,
  "semantic_v4_pilot_outcomes_v1_raw.rds"
)
output_csv <- here::here(
  output_dir,
  "semantic_v4_pilot_outcomes_v1.csv"
)
output_rds <- here::here(
  output_dir,
  "semantic_v4_pilot_outcomes_v1.rds"
)

allowed_status <- c(
  "committed", "predicted", "required", "existing",
  "evaluative", "not_applicable", "unknown"
)
allowed_confidence <- c("high", "medium", "low")


# ---- Small helpers -------------------------------------------

normalize_text <- function(x) {
  x %>%
    str_replace_all("[\\r\\n\\t]+", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim() %>%
    str_to_lower()
}

scalar_character <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    return(NA_character_)
  }
  as.character(x[[1]])
}

scalar_logical <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    return(NA)
  }
  if (is.logical(x[[1]])) return(x[[1]])
  value <- str_to_lower(str_trim(as.character(x[[1]])))
  if (value %in% c("true", "yes", "1")) return(TRUE)
  if (value %in% c("false", "no", "0")) return(FALSE)
  NA
}

evidence_vector <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  values <- as.character(unlist(x, use.names = FALSE))
  unique(values[!is.na(values) & str_trim(values) != ""])
}

as_sublist <- function(x) {
  if (is.list(x)) x else list()
}

valid_choice <- function(x, allowed, default) {
  value <- scalar_character(x)
  if (is.na(value) || !value %in% allowed) default else value
}

evidence_is_exact <- function(claim_text, evidence) {
  evidence <- evidence_vector(evidence)
  if (length(evidence) == 0) return(TRUE)
  normalized_claim <- normalize_text(claim_text)
  all(map_lgl(evidence, function(item) {
    normalized_item <- normalize_text(item)
    nzchar(normalized_item) &&
      str_detect(normalized_claim, fixed(normalized_item))
  }))
}

signal_is_consistent <- function(present, evidence) {
  evidence <- evidence_vector(evidence)
  if (is.na(present)) return(FALSE)
  if (isTRUE(present)) length(evidence) > 0 else length(evidence) == 0
}

prompt_text <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) "" else as.character(x[[1]])
}

review_trigger <- function(
    schema_valid,
    evidence_valid,
    has_concrete_consistent,
    status_consistent,
    confidence,
    reasoning,
    api_error
) {
  reasons <- character(0)
  if (!is.na(api_error) && nzchar(str_trim(api_error))) {
    reasons <- c(reasons, "api_or_parse_error")
  }
  if (!isTRUE(schema_valid)) {
    reasons <- c(reasons, "invalid_outcome_schema")
  }
  if (!isTRUE(evidence_valid)) {
    reasons <- c(reasons, "non_exact_evidence")
  }
  if (!isTRUE(has_concrete_consistent)) {
    reasons <- c(reasons, "has_concrete_outcome_inconsistent")
  }
  if (!isTRUE(status_consistent)) {
    reasons <- c(reasons, "outcome_status_inconsistent")
  }
  if (is.na(confidence) || confidence %in% c("medium", "low")) {
    reasons <- c(reasons, "non_high_confidence")
  }
  if (is.na(reasoning) || !nzchar(str_trim(reasoning))) {
    reasons <- c(reasons, "missing_reasoning")
  }
  paste(unique(reasons), collapse = ";")
}


# ---- One call per unique project-level text -----------------

outcome_input <- pilot_frames_reviewed %>%
  mutate(
    outcome_eligible = codable_reviewed,
    outcome_unit_key = frame_unit_key
  )

eligible_rows <- outcome_input %>%
  filter(outcome_eligible)

key_check <- eligible_rows %>%
  mutate(text_normalized = normalize_text(claim_text)) %>%
  group_by(outcome_unit_key) %>%
  summarise(n_texts = n_distinct(text_normalized), .groups = "drop")

if (any(is.na(eligible_rows$outcome_unit_key)) ||
    any(!nzchar(str_trim(eligible_rows$outcome_unit_key)))) {
  stop("Eligible outcome_unit_key contains missing or blank values.")
}
if (any(key_check$n_texts != 1L)) {
  stop("An outcome_unit_key maps to more than one claim_text.")
}

outcome_units <- eligible_rows %>%
  arrange(lpa_number, narrative, source_unit_index, excerpt_start) %>%
  group_by(outcome_unit_key) %>%
  slice_head(n = 1L) %>%
  ungroup()

if (nrow(outcome_units) != 90L) {
  stop("Step 4 expects exactly 90 unique codeable texts.")
}

cat("Step 4 reviewed rows:", nrow(outcome_input), "\n")
cat("Outcome-eligible rows:", nrow(eligible_rows), "\n")
cat("Unique texts to outcome-code:", nrow(outcome_units), "\n")
cat(
  "API calls avoided by duplicate reuse:",
  nrow(eligible_rows) - nrow(outcome_units),
  "\n\n"
)


# ---- Outcome prompt -----------------------------------------

build_outcome_prompt <- function(row) {
  paste0(
    "You are a careful research coder analysing UK planning application text.\n\n",
    "TASK\n",
    "Identify concrete OUTCOMES asserted in CLAIM_TEXT. Code four independent ",
    "binary fields. Zero, one or several fields may be true. This task is ",
    "independent of framing: do not code rhetoric or topic words unless the ",
    "text asserts a provision, change, benefit, cost or assessed impact.\n\n",
    
    "OUTCOME DEFINITION\n",
    "Ask what CLAIM_TEXT says will be delivered, produced, required, predicted, ",
    "prevented, achieved, retained or assessed. A mere topic, aspiration, ",
    "method, report description or value word is not an outcome.\n\n",
    
    "1. economic\n",
    "Jobs or employment; investment explicitly brought or delivered; commercial ",
    "or business floorspace/activity; business creation; economic growth or ",
    "activity; revenue, financial return, viability, or market/property-value ",
    "change. Money used only as a means of funding another outcome is not by ",
    "itself an economic outcome.\n\n",
    
    "2. social\n",
    "Affordable or social housing; housing explicitly presented as meeting need; ",
    "community facilities; education; health or wellbeing; safety; inclusion or ",
    "accessibility; play; cultural/community benefit; or resident/public benefit. ",
    "Generic housing provision alone is not sufficient.\n\n",
    
    "3. environmental\n",
    "Reduced emissions or resource use; energy performance; biodiversity or ",
    "ecological change; air-quality improvement; flood mitigation; climate ",
    "resilience; or pollution reduction. A green feature without an asserted ",
    "environmental effect is not sufficient.\n\n",
    
    "4. design_heritage\n",
    "Public-realm, townscape, architectural, landscape or visual-quality change; ",
    "heritage conservation or restoration. A building or public space mentioned ",
    "without an asserted design/heritage change is not sufficient.\n\n",
    
    "STRICT RULES\n",
    "- Code only outcomes asserted in CLAIM_TEXT.\n",
    "- CONTEXT may resolve attribution or a pronoun but may not supply an outcome ",
    "or evidence.\n",
    "- Do not infer an outcome from the narrative sampling stratum, frame labels, ",
    "speaker, document type or general planning knowledge.\n",
    "- A policy requirement can be an outcome with status=required.\n",
    "- A technical assessed effect can be an outcome with status=predicted or ",
    "evaluative.\n",
    "- An existing baseline condition is normally not a proposed outcome. Use ",
    "status=existing only for a concrete existing provision/effect that the claim ",
    "substantively presents as relevant; otherwise code no outcome.\n",
    "- If one or more fields are true, has_concrete_outcome must be true. If all ",
    "four are false, it must be false and status must be not_applicable.\n",
    "- Every true field needs at least one exact quotation from CLAIM_TEXT.\n",
    "- Every false field must have an empty evidence array.\n",
    "- Do not correct OCR, spelling, spacing or punctuation in quotations.\n\n",
    
    "Examples\n",
    "- 'The scheme will create 145 construction jobs' => economic=true; ",
    "status=committed.\n",
    "- 'The scheme will create 145 jobs and provide a community health centre' ",
    "=> economic=true and social=true.\n",
    "- '£3m of investment will generate social value for residents' => ",
    "social=true. The investment is a means here, not a separately delivered ",
    "outcome.\n",
    "- 'Development must reduce operational carbon emissions' => ",
    "environmental=true; status=required.\n",
    "- 'The report considers public realm and sustainability' => all false; ",
    "status=not_applicable.\n\n",
    
    "CLAIM_TEXT\n<<<\n", prompt_text(row$claim_text), "\n>>>\n\n",
    "CONTEXT_BEFORE — interpretation only\n<<<\n",
    prompt_text(row$context_before), "\n>>>\n\n",
    "CONTEXT_AFTER — interpretation only\n<<<\n",
    prompt_text(row$context_after), "\n>>>\n\n",
    
    "Return ONLY one JSON object in this form:\n",
    "{\n",
    "  \"has_concrete_outcome\": false,\n",
    "  \"economic\": {\"present\": false, \"evidence\": []},\n",
    "  \"social\": {\"present\": false, \"evidence\": []},\n",
    "  \"environmental\": {\"present\": false, \"evidence\": []},\n",
    "  \"design_heritage\": {\"present\": false, \"evidence\": []},\n",
    "  \"outcome_status\": ",
    "\"committed|predicted|required|existing|evaluative|not_applicable|unknown\",\n",
    "  \"confidence\": \"high|medium|low\",\n",
    "  \"reasoning\": \"brief explanation of concrete outcomes only\"\n",
    "}"
  )
}


# ---- API call -----------------------------------------------

call_openai_outcome <- function(prompt) {
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (!nzchar(api_key)) stop("OPENAI_API_KEY is not set.")
  last_error <- "unknown_error"
  
  for (attempt in seq_len(OUTCOME_MAX_RETRIES)) {
    response <- tryCatch(
      POST(
        "https://api.openai.com/v1/chat/completions",
        add_headers(
          Authorization = paste("Bearer", api_key),
          "Content-Type" = "application/json"
        ),
        body = toJSON(
          list(
            model = OUTCOME_MODEL,
            messages = list(list(role = "user", content = prompt)),
            response_format = list(type = "json_object"),
            temperature = 0,
            max_tokens = 750
          ),
          auto_unbox = TRUE
        ),
        timeout(90)
      ),
      error = function(e) {
        last_error <<- e$message
        NULL
      }
    )
    
    if (!is.null(response) && status_code(response) == 200) {
      result <- content(response, as = "parsed", encoding = "UTF-8")
      return(list(
        ok = TRUE,
        text = result$choices[[1]]$message$content,
        tokens_in = as.integer(result$usage$prompt_tokens %||% 0L),
        tokens_out = as.integer(result$usage$completion_tokens %||% 0L),
        error = NA_character_
      ))
    }
    
    if (!is.null(response)) {
      status <- status_code(response)
      last_error <- paste0("HTTP ", status)
      if (status >= 400 && status < 500 && status != 429) break
    }
    
    if (attempt < OUTCOME_MAX_RETRIES) {
      backoff <- min(60, attempt * 10)
      cat(
        sprintf(
          "  [attempt %d failed: %s; retry after %d sec]\n",
          attempt, last_error, backoff
        )
      )
      Sys.sleep(backoff)
    }
  }
  
  list(
    ok = FALSE,
    text = NA_character_,
    tokens_in = 0L,
    tokens_out = 0L,
    error = last_error
  )
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}


# ---- Cache ---------------------------------------------------

empty_cache <- tibble(
  outcome_unit_key = character(),
  representative_claim_key = character(),
  has_concrete_outcome_returned = logical(),
  has_concrete_outcome = logical(),
  outcome_economic = logical(),
  outcome_social = logical(),
  outcome_environmental = logical(),
  outcome_design_heritage = logical(),
  outcome_economic_evidence = character(),
  outcome_social_evidence = character(),
  outcome_environmental_evidence = character(),
  outcome_design_heritage_evidence = character(),
  outcome_status = character(),
  outcome_confidence = character(),
  outcome_reasoning = character(),
  outcome_schema_valid = logical(),
  outcome_evidence_valid = logical(),
  has_concrete_outcome_consistent = logical(),
  outcome_status_consistent = logical(),
  outcome_needs_review = logical(),
  outcome_review_trigger = character(),
  outcome_api_raw = character(),
  outcome_api_error = character(),
  outcome_tokens_in = integer(),
  outcome_tokens_out = integer(),
  outcome_model = character(),
  outcome_prompt_version = character()
)

if (file.exists(cache_path)) {
  outcome_cache <- readRDS(cache_path)
  missing_cache_columns <- setdiff(names(empty_cache), names(outcome_cache))
  if (length(missing_cache_columns) > 0) {
    stop(
      "Existing Step 4 cache is incompatible. Missing columns: ",
      paste(missing_cache_columns, collapse = ", ")
    )
  }
  outcome_cache <- outcome_cache %>%
    select(all_of(names(empty_cache))) %>%
    distinct(outcome_unit_key, .keep_all = TRUE)
} else {
  outcome_cache <- empty_cache
}

completed_keys <- outcome_cache %>%
  filter(
    is.na(outcome_api_error),
    outcome_schema_valid %in% TRUE,
    outcome_evidence_valid %in% TRUE,
    has_concrete_outcome_consistent %in% TRUE,
    outcome_status_consistent %in% TRUE,
    outcome_model == OUTCOME_MODEL,
    outcome_prompt_version == OUTCOME_PROMPT_VERSION
  ) %>%
  pull(outcome_unit_key)

to_code <- outcome_units %>%
  filter(!outcome_unit_key %in% completed_keys)

cat("Unique outcome texts remaining:", nrow(to_code), "\n\n")


# ---- Run Step 4 ---------------------------------------------

session_tokens_in <- 0L
session_tokens_out <- 0L

for (i in seq_len(nrow(to_code))) {
  row <- to_code[i, ]
  api <- call_openai_outcome(build_outcome_prompt(row))
  
  if (!isTRUE(api$ok)) {
    new_result <- empty_cache %>%
      add_row(
        outcome_unit_key = row$outcome_unit_key,
        representative_claim_key = row$claim_key,
        outcome_needs_review = TRUE,
        outcome_review_trigger = "api_or_parse_error",
        outcome_api_error = api$error,
        outcome_model = OUTCOME_MODEL,
        outcome_prompt_version = OUTCOME_PROMPT_VERSION
      )
  } else {
    session_tokens_in <- session_tokens_in + api$tokens_in
    session_tokens_out <- session_tokens_out + api$tokens_out
    parsed <- tryCatch(
      fromJSON(api$text, simplifyVector = FALSE),
      error = function(e) NULL
    )
    
    if (is.null(parsed)) {
      new_result <- empty_cache %>%
        add_row(
          outcome_unit_key = row$outcome_unit_key,
          representative_claim_key = row$claim_key,
          outcome_needs_review = TRUE,
          outcome_review_trigger = "api_or_parse_error",
          outcome_api_raw = api$text,
          outcome_api_error = "invalid_json",
          outcome_tokens_in = api$tokens_in,
          outcome_tokens_out = api$tokens_out,
          outcome_model = OUTCOME_MODEL,
          outcome_prompt_version = OUTCOME_PROMPT_VERSION
        )
    } else {
      economic <- as_sublist(parsed$economic)
      social <- as_sublist(parsed$social)
      environmental <- as_sublist(parsed$environmental)
      design <- as_sublist(parsed$design_heritage)
      
      economic_present <- scalar_logical(economic$present)
      social_present <- scalar_logical(social$present)
      environmental_present <- scalar_logical(environmental$present)
      design_present <- scalar_logical(design$present)
      
      economic_evidence <- evidence_vector(economic$evidence)
      social_evidence <- evidence_vector(social$evidence)
      environmental_evidence <- evidence_vector(environmental$evidence)
      design_evidence <- evidence_vector(design$evidence)
      
      schema_valid <- all(
        signal_is_consistent(economic_present, economic_evidence),
        signal_is_consistent(social_present, social_evidence),
        signal_is_consistent(environmental_present, environmental_evidence),
        signal_is_consistent(design_present, design_evidence)
      )
      
      all_evidence <- c(
        economic_evidence, social_evidence,
        environmental_evidence, design_evidence
      )
      evidence_valid <- evidence_is_exact(row$claim_text, all_evidence)
      has_returned <- scalar_logical(parsed$has_concrete_outcome)
      has_derived <- any(c(
        economic_present, social_present,
        environmental_present, design_present
      ) %in% TRUE)
      has_consistent <- !is.na(has_returned) &&
        has_returned == has_derived
      
      status <- valid_choice(
        parsed$outcome_status,
        allowed_status,
        "unknown"
      )
      status_consistent <- if (has_derived) {
        status %in% c(
          "committed", "predicted", "required",
          "existing", "evaluative"
        )
      } else {
        status == "not_applicable"
      }
      
      confidence <- valid_choice(
        parsed$confidence,
        allowed_confidence,
        "low"
      )
      reasoning <- scalar_character(parsed$reasoning)
      api_error <- if (schema_valid) NA_character_ else "invalid_outcome_schema"
      trigger <- review_trigger(
        schema_valid,
        evidence_valid,
        has_consistent,
        status_consistent,
        confidence,
        reasoning,
        api_error
      )
      
      new_result <- tibble(
        outcome_unit_key = row$outcome_unit_key,
        representative_claim_key = row$claim_key,
        has_concrete_outcome_returned = has_returned,
        has_concrete_outcome = has_derived,
        outcome_economic = economic_present,
        outcome_social = social_present,
        outcome_environmental = environmental_present,
        outcome_design_heritage = design_present,
        outcome_economic_evidence = paste(economic_evidence, collapse = " || "),
        outcome_social_evidence = paste(social_evidence, collapse = " || "),
        outcome_environmental_evidence = paste(
          environmental_evidence,
          collapse = " || "
        ),
        outcome_design_heritage_evidence = paste(
          design_evidence,
          collapse = " || "
        ),
        outcome_status = status,
        outcome_confidence = confidence,
        outcome_reasoning = reasoning,
        outcome_schema_valid = schema_valid,
        outcome_evidence_valid = evidence_valid,
        has_concrete_outcome_consistent = has_consistent,
        outcome_status_consistent = status_consistent,
        outcome_needs_review = trigger != "",
        outcome_review_trigger = trigger,
        outcome_api_raw = api$text,
        outcome_api_error = api_error,
        outcome_tokens_in = as.integer(api$tokens_in),
        outcome_tokens_out = as.integer(api$tokens_out),
        outcome_model = OUTCOME_MODEL,
        outcome_prompt_version = OUTCOME_PROMPT_VERSION
      )
    }
  }
  
  outcome_cache <- outcome_cache %>%
    filter(outcome_unit_key != row$outcome_unit_key) %>%
    bind_rows(new_result)
  
  cat(sprintf(
    "[%d/%d] %s -- E:%s | S:%s | N:%s | D:%s | review:%s\n",
    i,
    nrow(to_code),
    row$claim_key,
    ifelse(is.na(new_result$outcome_economic), "NA", new_result$outcome_economic),
    ifelse(is.na(new_result$outcome_social), "NA", new_result$outcome_social),
    ifelse(
      is.na(new_result$outcome_environmental),
      "NA",
      new_result$outcome_environmental
    ),
    ifelse(
      is.na(new_result$outcome_design_heritage),
      "NA",
      new_result$outcome_design_heritage
    ),
    ifelse(
      is.na(new_result$outcome_needs_review),
      "NA",
      new_result$outcome_needs_review
    )
  ))
  
  if (i %% 10 == 0) {
    saveRDS(outcome_cache, cache_path)
    cat("  [Step 4 cache saved]\n")
  }
  if (OUTCOME_SLEEP_SECONDS > 0) Sys.sleep(OUTCOME_SLEEP_SECONDS)
}

saveRDS(outcome_cache, cache_path)


# ---- Join to all 117 rows and export -------------------------

pilot_outcomes_coded <- outcome_input %>%
  left_join(outcome_cache, by = "outcome_unit_key") %>%
  arrange(lpa_number, narrative, source_unit_index, excerpt_start)

if (nrow(pilot_outcomes_coded) != 117L) {
  stop("Step 4 output row count changed.")
}
if (anyDuplicated(pilot_outcomes_coded$claim_key)) {
  stop("claim_key is not unique after Step 4 join.")
}

eligible_output <- pilot_outcomes_coded %>%
  filter(outcome_eligible)

if (nrow(eligible_output) != 92L ||
    n_distinct(eligible_output$outcome_unit_key) != 90L) {
  stop("Step 4 output failed the 92-row / 90-unit check.")
}

write_csv(pilot_outcomes_coded, output_csv)
saveRDS(pilot_outcomes_coded, output_rds)


# ---- Console audit ------------------------------------------

cat("\n============================================\n")
cat("V4 PILOT STEP 4 COMPLETED\n")
cat("============================================\n")
cat("Rows retained:", nrow(pilot_outcomes_coded), "\n")
cat("Outcome-eligible rows:", nrow(eligible_output), "\n")
cat("Unique eligible outcome units:", n_distinct(eligible_output$outcome_unit_key), "\n")
cat("Session input tokens:", session_tokens_in, "\n")
cat("Session output tokens:", session_tokens_out, "\n\n")

cat("Outcome label counts across eligible rows:\n")
eligible_output %>%
  summarise(
    economic = sum(outcome_economic %in% TRUE, na.rm = TRUE),
    social = sum(outcome_social %in% TRUE, na.rm = TRUE),
    environmental = sum(outcome_environmental %in% TRUE, na.rm = TRUE),
    design_heritage = sum(outcome_design_heritage %in% TRUE, na.rm = TRUE),
    no_coded_outcome = sum(has_concrete_outcome %in% FALSE, na.rm = TRUE)
  ) %>%
  print(width = Inf)

cat("\nOutcome status counts:\n")
eligible_output %>%
  count(outcome_status, name = "n") %>%
  arrange(desc(n)) %>%
  print(n = Inf)

cat("\nRows requiring review or retry:\n")
eligible_output %>%
  filter(
    outcome_needs_review %in% TRUE |
      is.na(outcome_needs_review) |
      !is.na(outcome_api_error)
  ) %>%
  select(
    lpa_number, narrative, claim_key, claim_text,
    starts_with("outcome_"), has_concrete_outcome
  ) %>%
  print(n = Inf, width = Inf)

cat("\nRepeated texts whose outcome coding was reused:\n")
eligible_output %>%
  add_count(outcome_unit_key, name = "outcome_duplicate_n") %>%
  filter(outcome_duplicate_n > 1L) %>%
  select(
    lpa_number, narrative, claim_key, outcome_unit_key,
    outcome_economic, outcome_social,
    outcome_environmental, outcome_design_heritage, claim_text
  ) %>%
  print(n = Inf, width = Inf)

cat("\nStep 4 files saved:\n")
cat("1.", cache_path, "\n")
cat("2.", output_csv, "\n")
cat("3.", output_rds, "\n")