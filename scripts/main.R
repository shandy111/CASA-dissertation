# =============================================================
# main.R — Run entire dissertation pipeline
# =============================================================
# Usage: source(here::here("scripts", "main.R"))
#
# Scripts in order:
#   01: PLD data → 74 corpus
#   02: Sub-sample + geocoding + PTAL
#   03: LLM extraction (cached if rds exists)
#   04: Pairing analysis + figures
# =============================================================

library(here)

cat("========== PIPELINE START ==========\n\n")

source(here("scripts", "01_pld_pipeline.R"))
source(here("scripts", "02_subsample_spatial.R"))
source(here("scripts", "03_llm_extraction.R"))
source(here("scripts", "04_pairing_analysis.R"))

cat("\n========== PIPELINE COMPLETE ==========\n")