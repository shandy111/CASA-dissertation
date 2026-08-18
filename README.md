# CASA-dissertation

# Narrating Development, Producing Space

Code for the MSc dissertation *Narrating Development, Producing Space: developer
narratives and the spatial and material outcomes proposed in major residential
planning applications, using Tower Hamlets as a case study.*

## Overview
This repository contains the analysis pipeline that (a) codes developer planning
statements for narrative, stance and framing, (b) assembles spatial and material
indicators for each scheme, and (c) tests the correspondence between the two.

## Data sources
The analysis draws on publicly available data (full list in Table 3.1 of the
dissertation). The two primary sources are:
- Tower Hamlets planning register / development management portal (planning statements)
- Greater London Authority, Planning London DataHub / Datastore (application records)

Planning-statement PDFs and large boundary/spatial files are not included in this
repository; they can be obtained from the sources above. The list of the 59 analysed
schemes, by application reference, is in Appendix A of the dissertation.

## Scripts
Scripts in `scripts/` are numbered in run order. `figures.R` reproduces the tables and
non-spatial figures; spatial figures (Moran / LISA / maps) and validation figures are
produced by their own numbered scripts, principally `09_spatial_analysis.R`.

Some earlier-numbered scripts are development iterations (for example, successive
versions of the semantic recoding) that were later superseded; they are kept in the
repository for transparency. The final versions are the highest-numbered ones in each
group (for example, `14_semantic_final.R`).
