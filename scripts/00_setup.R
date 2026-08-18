# =============================================================
# 00_setup.R — Shared setup for all pipeline scripts
# =============================================================

library(tidyverse)
library(here)
library(sf)
library(pdftools)
library(httr)
library(jsonlite)
library(scales)
library(janitor)
library(fs)

# Paths
data_dir   <- here("data", "pld_raw")
output_dir <- here("output")
pdf_dir    <- here("data", "planning_statements")
ptal_dir   <- here("data", "2015  PTALs Grid Values")

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

cat("Setup loaded. Root:", here(), "\n")