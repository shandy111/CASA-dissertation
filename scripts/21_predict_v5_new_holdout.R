# =============================================================
# 21_predict_v5_new_holdout.R
# 只做一件事:用冻结的 v5 对新盲标表打预测,存文件。不算成绩。
# prompt 与 14_semantic_final.R 完全一致(未改一字)。
# =============================================================
source(here::here("scripts", "00_setup.R"))
library(httr)
library(jsonlite)

# ---- Prompt v5: FUNCTION classification ----
build_function_prompt <- function(context, narrative) {
  paste0(
    'You are classifying a sentence or short context window from a UK planning statement containing the word "', narrative, '".\n\n',
    'Return exactly ONE discourse-function category. Never return two labels.\n\n',
    '1. "applicant_claim" - The applicant/developer makes its own claim, commitment, promise, evaluation, or compliance claim about the proposed scheme.\n',
    '   Example: "The Proposed Development will deliver 300 affordable homes."\n',
    '   Example: "The proposal accords with the London Plan and provides a high-quality mixed-use scheme."\n\n',
    '2. "policy_claim" - The substantive proposition is explicitly attributed to planning policy, a plan, an SPD, a framework, a masterplan, a council vision, or an official target/objective/aspiration.\n',
    '   Strong attribution signals include:\n',
    '   - Named policy documents: "London Plan", "NPPF", "Core Strategy", "Local Plan", "Neighbourhood Plan", "SPD"\n',
    '   - Policy identifiers: "Policy 3.7", "Policy DH1", "paragraph 202"\n',
    '   - Attribution verbs: "the policy requires", "the plan states", "the strategy seeks", "the council identifies"\n',
    '   Example: "Policy 3.7 of the London Plan states that development should optimise housing output."\n\n',
    '3. "project_description" - A neutral description of the site, scheme, existing condition, or surrounding context, without a promise or policy proposition.\n',
    '   Example: "The site is 2.5 hectares and contains three existing buildings."\n\n',
    '4. "technical_assessment" - A substantive finding or conclusion from a technical assessment or consultant analysis.\n',
    '   Example: "The air-quality assessment shows NO2 levels below the threshold."\n\n',
    '5. "procedural_record" - A record of consultation, submission, application dates, meetings, or other procedures.\n',
    '   Example: "Consultation was carried out from 1 May to 31 May 2023."\n\n',
    '6. "document_metadata" - A heading, table of contents, cross-reference, caption, list entry, or other document-navigation text.\n',
    '   Example: "See Section 4.2 for full details."\n\n',
    '7. "unusable_fragment" - Broken, truncated, or speaker-mixed text whose substantive proposition cannot be interpreted reliably.\n\n',
    '## Applicant versus policy boundary\n',
    '- Policy vocabulary alone does not make a passage policy_claim.\n',
    '- If policy or a named document is the speaker/source of the substantive proposition, use policy_claim.\n',
    '- If the applicant merely says that its own scheme complies with policy, but the substantive claim is about what the scheme does, use applicant_claim.\n',
    '- If the window contains materially different speakers or sections, classify the proposition containing the narrative word. If that is impossible, use unusable_fragment.\n\n',
    '## Codable\n',
    '- codable = TRUE only for substantive applicant_claim, policy_claim, project_description, or technical_assessment.\n',
    '- codable = FALSE for procedural_record, document_metadata, unusable_fragment, or content-free text.\n\n',
    '===SENTENCE===\n"', str_replace_all(context, '"', "'"), '"\n\n',
    'Return ONLY this JSON:\n',
    '{\n',
    '  "function": "applicant_claim|policy_claim|project_description|technical_assessment|procedural_record|document_metadata|unusable_fragment",\n',
    '  "codable": true|false,\n',
    '  "reasoning": "brief"\n',
    '}'
  )
}

# ---- Prompt v5: FRAME classification (multi-label) ----
build_frame_prompt <- function(context, narrative) {
  paste0(
    'You are analyzing FRAMING VOCABULARY in a sentence or short context window from a UK planning statement containing the word "', narrative, '".\n\n',
    'Identify which of four framing categories are explicitly and substantively present. Multiple categories can be TRUE.\n\n',
    '## General rule\n',
    '- Set a frame TRUE only when the visible text contains explicit, substantive evidence for that domain.\n',
    '- Do not infer a frame from the general desirability of regeneration, development, improvement, or sustainability.\n',
    '- Every TRUE frame must be supported by a verbatim word or phrase copied from the text.\n\n',
    '## Category 1: economic_frame\n',
    'TRUE for explicit economic vocabulary such as:\n',
    '- Jobs, employment, workforce, construction jobs\n',
    '- Investment, financial contribution, capital cost\n',
    '- Commercial/retail/business/office activity or floorspace\n',
    '- Economic growth, economic activity, GVA\n',
    '- Yield, viability, market, commercial value\n',
    'FALSE for mixed use, development, regeneration, improvement, or density alone.\n\n',
    '## Category 2: social_frame\n',
    'TRUE for explicit social constituencies, needs, benefits, access, or harms such as:\n',
    '- Community, residents, families, neighbourhood, local people\n',
    '- Affordable housing, housing need\n',
    '- Wellbeing, safety, inclusion, cohesion\n',
    '- Community facilities, public benefit, public amenity\n',
    '- Explicit wheelchair-accessible housing, ramps, or accessible routes\n',
    '- Explicit relocation or displacement of existing residents or businesses\n',
    'FALSE for homes, housing, residential development, occupiers, public space, public realm, pedestrian routes, or an "accessible location" alone.\n',
    'Demolition or technical site clearance alone is not social framing.\n\n',
    '## Category 3: environmental_frame\n',
    'TRUE for explicit substantive environmental content such as:\n',
    '- Carbon reduction, net zero, energy efficiency/performance\n',
    '- Biodiversity, biodiversity net gain, ecological habitat\n',
    '- BREEAM or another named environmental standard\n',
    '- Defined green infrastructure, green roofs, or concrete tree planting\n',
    '- A green spine only when described as a connected planted corridor, green infrastructure, or having an explicit ecological function\n',
    'FALSE for "sustainable", "sustainability", or "sustainable development" alone.\n',
    'FALSE for brownfield, riverside, landscaping, open space, a project name, promotional green language, or the label "green spine" alone.\n\n',
    '## Category 4: design_heritage_frame\n',
    'TRUE for explicit architecture, urban design, building form/alignment, townscape, public-realm design, placemaking, heritage, conservation, listed-building, historic-character, or character content.\n',
    'FALSE for "built environment" alone.\n',
    'FALSE for generic "high quality", "modern living", development, or improvement without substantive design/heritage evidence.\n\n',
    '===SENTENCE===\n"', str_replace_all(context, '"', "'"), '"\n\n',
    'Return ONLY this JSON:\n',
    '{\n',
    '  "economic": true|false,\n',
    '  "social": true|false,\n',
    '  "environmental": true|false,\n',
    '  "design_heritage": true|false,\n',
    '  "evidence": "verbatim words or phrases supporting every TRUE category"\n',
    '}'
  )
}

# ---- Prompt v5: OUTCOME classification (multi-label) ----
build_outcome_prompt <- function(context, narrative) {
  paste0(
    'You are analyzing OUTCOME CLAIMS in a sentence or short context window from a UK planning statement containing the word "', narrative, '".\n\n',
    '## Step 1: has_concrete_outcome\n',
    'Set has_concrete_outcome = TRUE only when the text directly claims that the proposed development will provide, deliver, create, construct, protect, enhance, achieve, establish, or otherwise realise a visible and specific result in at least one of the four outcome domains below.\n',
    'A concrete result may be a specific number, named facility, tangible intervention, defined design feature, or measurable project target.\n\n',
    'TRUE example: "The scheme will deliver 200 affordable homes and a new community centre."\n',
    'TRUE example: "The development will provide 15,000 sqm of commercial floorspace."\n',
    'TRUE example: "The project targets BREEAM Very Good."\n\n',
    'Set has_concrete_outcome = FALSE when:\n',
    '- The text is an abstract principle, aspiration, objective, topic, or general contribution.\n',
    '- It merely says a scheme may "assist", "support", "contribute to", or "enable" future delivery.\n',
    '- It names a tangible noun but does not claim direct project delivery.\n',
    '- It is a policy statement that development "should", "must", or "is expected to" achieve something, rather than a project-specific applicant commitment.\n',
    '- It only describes demolition, technical site clearance, relocation, or displacement.\n\n',
    'FALSE example: "Sustainable development is important."\n',
    'FALSE example: "The scheme will contribute to regeneration."\n',
    'FALSE example: "The proposal assists in the delivery of further units."\n\n',
    '## Step 2: outcome categories\n',
    'Code the following only when has_concrete_outcome = TRUE. Multiple categories can be TRUE.\n\n',
    '### economic_outcome\n',
    'TRUE for direct delivery of a concrete economic result, including numbered jobs, employment, an investment amount, commercial/retail/office floorspace, or business units.\n',
    'Do not infer an economic outcome from mixed use, density, regeneration, or economic vocabulary without a delivered result.\n\n',
    '### social_outcome\n',
    'TRUE for direct delivery of a concrete social benefit, including affordable housing units/percentages, community facilities, resident-serving public amenity, or explicit wheelchair-accessible housing, ramps, or accessible routes.\n',
    'Generic housing, public realm, public space, pedestrian routes, residential development, or an "accessible location" do not automatically count.\n',
    'Relocation or displacement of existing residents/businesses is a social frame or harm, not a positive social outcome. Demolition alone is not a social outcome.\n\n',
    '### environmental_outcome\n',
    'TRUE for direct delivery of a concrete environmental result, including a carbon/energy target, biodiversity net gain, ecological habitat, planted trees, green roofs, or defined green infrastructure.\n',
    'A green spine counts only when the text commits to a connected planted corridor, green infrastructure, or an explicit ecological function.\n',
    'A project-specific commitment or target to achieve a named BREEAM rating counts. A policy aspiration that a development "should" achieve the rating does not.\n',
    'Do not infer an environmental outcome from "sustainable", landscaping, brownfield, riverside, open space, promotional green language, or the name "green spine" alone.\n\n',
    '### design_heritage_outcome\n',
    'TRUE for direct delivery of a specific heritage protection/enhancement, townscape intervention, public-realm design feature, architectural form, building alignment, mansion-block form, mansard, or other defined design result.\n',
    'The phrase "built environment" and generic claims of "high quality" are insufficient.\n\n',
    '## Mandatory consistency rules\n',
    '- If has_concrete_outcome = FALSE, all four outcome categories must be FALSE.\n',
    '- has_concrete_outcome may be TRUE only when at least one outcome category is TRUE.\n',
    '- Every TRUE outcome must have a visible verbatim phrase showing both the delivered result and its category.\n\n',
    '===SENTENCE===\n"', str_replace_all(context, '"', "'"), '"\n\n',
    'Return ONLY this JSON:\n',
    '{\n',
    '  "has_concrete_outcome": true|false,\n',
    '  "economic": true|false,\n',
    '  "social": true|false,\n',
    '  "environmental": true|false,\n',
    '  "design_heritage": true|false,\n',
    '  "evidence": "verbatim delivery verb plus object for every TRUE category; note relocation/displacement if present"\n',
    '}'
  )
}

# ---- API call and helpers (未改) ----
call_openai <- function(prompt, max_retries = 3) {
  for (attempt in seq_len(max_retries)) {
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
          temperature = 0,
          max_tokens = 500
        ), auto_unbox = TRUE),
        timeout(60)
      )
    }, error = function(e) NULL)
    if (!is.null(response) && status_code(response) == 200) {
      result <- content(response)
      return(list(
        text = result$choices[[1]]$message$content,
        tokens_in = result$usage$prompt_tokens,
        tokens_out = result$usage$completion_tokens
      ))
    }
    if (attempt < max_retries) Sys.sleep(attempt * 10)
  }
  list(error = "api_failed")
}

as_bool <- function(x) {
  if (length(x) != 1 || is.null(x) || is.na(x)) return(NA)
  if (is.logical(x)) return(x)
  if (is.character(x) && tolower(x) %in% c("true", "false")) return(tolower(x) == "true")
  NA
}

allowed_functions <- c(
  "applicant_claim","policy_claim","project_description",
  "technical_assessment","procedural_record","document_metadata","unusable_fragment"
)

# =============================================================
# 只有预测:读新盲标表 -> 3 pass -> 存 new_holdout_v5_predictions.csv
# =============================================================
sheet <- read_csv(here(output_dir, "new_holdout_coding_sheet.csv"), show_col_types = FALSE)
n <- nrow(sheet)
cat(sprintf("=== 预测 %d 条 × 3 pass ===\n\n", n))

results <- sheet %>%
  mutate(
    pred_function = NA_character_, pred_codable = NA,
    pred_frame_economic = NA, pred_frame_social = NA,
    pred_frame_environmental = NA, pred_frame_design_heritage = NA,
    pred_frame_evidence = NA_character_,
    pred_has_outcome = NA,
    pred_outcome_economic = NA, pred_outcome_social = NA,
    pred_outcome_environmental = NA, pred_outcome_design_heritage = NA,
    pred_outcome_evidence = NA_character_
  )

for (i in seq_len(n)) {
  ctx <- results$context[i]; nar <- results$narrative[i]
  
  r1 <- call_openai(build_function_prompt(ctx, nar))
  if (is.null(r1$error)) {
    p1 <- tryCatch(fromJSON(r1$text), error = function(e) NULL)
    if (!is.null(p1)) {
      fv <- p1[["function"]] %||% NA_character_
      if (length(fv) == 1 && fv %in% allowed_functions) results$pred_function[i] <- fv
      results$pred_codable[i] <- as_bool(p1$codable)
    }
  }
  Sys.sleep(1)
  
  r2 <- call_openai(build_frame_prompt(ctx, nar))
  if (is.null(r2$error)) {
    p2 <- tryCatch(fromJSON(r2$text), error = function(e) NULL)
    if (!is.null(p2)) {
      results$pred_frame_economic[i] <- as_bool(p2$economic)
      results$pred_frame_social[i] <- as_bool(p2$social)
      results$pred_frame_environmental[i] <- as_bool(p2$environmental)
      results$pred_frame_design_heritage[i] <- as_bool(p2$design_heritage)
      results$pred_frame_evidence[i] <- p2$evidence %||% NA_character_
    }
  }
  Sys.sleep(1)
  
  r3 <- call_openai(build_outcome_prompt(ctx, nar))
  if (is.null(r3$error)) {
    p3 <- tryCatch(fromJSON(r3$text), error = function(e) NULL)
    if (!is.null(p3)) {
      hs <- as_bool(p3$has_concrete_outcome)
      ov <- c(economic=as_bool(p3$economic), social=as_bool(p3$social),
              environmental=as_bool(p3$environmental), design_heritage=as_bool(p3$design_heritage))
      if (isFALSE(hs)) ov[] <- FALSE
      else if (isTRUE(hs) && !any(ov %in% TRUE)) { hs <- FALSE; ov[] <- FALSE }
      results$pred_has_outcome[i] <- hs
      results$pred_outcome_economic[i] <- ov[["economic"]]
      results$pred_outcome_social[i] <- ov[["social"]]
      results$pred_outcome_environmental[i] <- ov[["environmental"]]
      results$pred_outcome_design_heritage[i] <- ov[["design_heritage"]]
      results$pred_outcome_evidence[i] <- p3$evidence %||% NA_character_
    }
  }
  cat(sprintf("[%d/%d] %s × %s | function: %s | frame_econ: %s\n",
              i, n, results$lpa_number[i], nar, results$pred_function[i], results$pred_frame_economic[i]))
  Sys.sleep(1)
}

write_csv(results, here(output_dir, "new_holdout_v5_predictions.csv"))

cat("\n================ 完成 ================\n")
cat("存了:", nrow(results), "行 -> output/new_holdout_v5_predictions.csv\n")
cat("有 pred 列:", any(grepl("^pred_", names(results))), "(应为 TRUE)\n")