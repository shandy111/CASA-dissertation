# =============================================================
# 05_74_corpus_audit.R
# =============================================================
# 74 corpus document eligibility audit
# 
# Purpose: Before scaling LLM pipeline to 74, estimate how many 
# schemes are eligible for full-document discourse analysis based 
# on application type (from PLD description).
# 
# Preparation for Monday meeting with supervisor.
#
# Input:  output/th_74_typed.csv
# Output: output/th_74_document_audit.csv
# =============================================================

source(here::here("scripts", "00_setup.R"))


library(tidyverse)
library(here)

th_74 <- read_csv(here("output", "th_74_typed.csv"), show_col_types = FALSE)
audit <- read_csv(here("output", "th_74_document_audit.csv"), show_col_types = FALSE)

# 你今晚已经下过的 14 个 base LPA
already_downloaded_base <- c(
  "PA/13/02722", "PA/13/02966", "PA/14/00944", "PA/14/02819",
  "PA/15/01231", "PA/15/02675", "PA/15/02959", "PA/15/03074",
  "PA/16/01612", "PA/16/02808", "PA/18/02803", "PA/19/02792",
  "PA/22/00210", "PA/23/02079"
)

# 从 audit 里 filter discourse-analyzable
to_download <- audit %>%
  filter(likely_has_planning_statement == TRUE) %>%
  mutate(base_lpa = str_replace(lpa_number, "/[A-Z][A-Z0-9]*$", "")) %>%
  filter(!base_lpa %in% already_downloaded_base) %>%
  arrange(lpa_number)

cat("=== Correct to-download list ===\n")
cat("Count:", nrow(to_download), "\n\n")

to_download %>%
  select(lpa_number, site_name, financial_year, units_proposed, description) %>%
  mutate(description = str_sub(description, 1, 80)) %>%
  print(n = Inf, width = Inf)

# 重新写清单
to_download %>%
  select(lpa_number, site_name, units_proposed, units_lost,
         financial_year, app_category, description) %>%
  write_csv(here("output", "th_74_download_checklist.csv"))