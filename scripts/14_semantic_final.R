# =============================================================
# 14_semantic_final.R
# =============================================================
# Final semantic recode calibration: 4 narratives × 59 projects
# 3-pass: function → frame → outcome
# Multi-label: 4 TRUE/FALSE categories for frame and outcome
#
# Design:
#   - Prompt v5 incorporates adjudicated boundary rules
#   - Independent API calls per pass (no cross-reference)
#   - Calibrate on blind_validation_30.csv
#   - Evaluate function as 3 source-control categories
#   - Evaluate outcomes on ALL codable rows so false positives count
#   - Treat this reused 30-row set as calibration, not fresh holdout evidence
#
# Recommended next step after calibration:
#   - Freeze v5
#   - Run a 5-project workflow pilot
#   - Use targeted researcher adjudication for weak categories
#
# Cost: calibration approximately $0.05
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


# ---- API call and response helpers ----

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
  if (is.character(x) && tolower(x) %in% c("true", "false")) {
    return(tolower(x) == "true")
  }
  NA
}

allowed_functions <- c(
  "applicant_claim",
  "policy_claim",
  "project_description",
  "technical_assessment",
  "procedural_record",
  "document_metadata",
  "unusable_fragment"
)


# =============================================================
# PHASE 1: CALIBRATE ON 30 HUMAN-CODED INSTANCES
# =============================================================

blind <- read_csv(
  here(output_dir, "new_holdout_coding_sheet.csv"),
  show_col_types = FALSE
)

n_calibration <- nrow(blind)
cat(sprintf("=== Calibration: %d instances × 3 passes ===\n\n", n_calibration))

calibration_results <- blind %>%
  mutate(
    pred_function = NA_character_,
    pred_codable = NA,
    pred_frame_economic = NA,
    pred_frame_social = NA,
    pred_frame_environmental = NA,
    pred_frame_design_heritage = NA,
    pred_frame_evidence = NA_character_,
    pred_has_outcome = NA,
    pred_outcome_economic = NA,
    pred_outcome_social = NA,
    pred_outcome_environmental = NA,
    pred_outcome_design_heritage = NA,
    pred_outcome_evidence = NA_character_
  )

total_tokens_in <- 0
total_tokens_out <- 0

for (i in seq_len(n_calibration)) {
  ctx <- calibration_results$context[i]
  nar <- calibration_results$narrative[i]
  
  # Pass 1: Function
  r1 <- call_openai(build_function_prompt(ctx, nar))
  if (is.null(r1$error)) {
    p1 <- tryCatch(fromJSON(r1$text), error = function(e) NULL)
    if (!is.null(p1)) {
      function_value <- p1[["function"]] %||% NA_character_
      if (length(function_value) == 1 && function_value %in% allowed_functions) {
        calibration_results$pred_function[i] <- function_value
      }
      calibration_results$pred_codable[i] <- as_bool(p1$codable)
    }
    total_tokens_in <- total_tokens_in + r1$tokens_in
    total_tokens_out <- total_tokens_out + r1$tokens_out
  }
  Sys.sleep(1)
  
  # Pass 2: Frame
  r2 <- call_openai(build_frame_prompt(ctx, nar))
  if (is.null(r2$error)) {
    p2 <- tryCatch(fromJSON(r2$text), error = function(e) NULL)
    if (!is.null(p2)) {
      calibration_results$pred_frame_economic[i] <- as_bool(p2$economic)
      calibration_results$pred_frame_social[i] <- as_bool(p2$social)
      calibration_results$pred_frame_environmental[i] <- as_bool(p2$environmental)
      calibration_results$pred_frame_design_heritage[i] <- as_bool(p2$design_heritage)
      calibration_results$pred_frame_evidence[i] <- p2$evidence %||% NA_character_
    }
    total_tokens_in <- total_tokens_in + r2$tokens_in
    total_tokens_out <- total_tokens_out + r2$tokens_out
  }
  Sys.sleep(1)
  
  # Pass 3: Outcome
  r3 <- call_openai(build_outcome_prompt(ctx, nar))
  if (is.null(r3$error)) {
    p3 <- tryCatch(fromJSON(r3$text), error = function(e) NULL)
    if (!is.null(p3)) {
      has_outcome <- as_bool(p3$has_concrete_outcome)
      outcome_values <- c(
        economic = as_bool(p3$economic),
        social = as_bool(p3$social),
        environmental = as_bool(p3$environmental),
        design_heritage = as_bool(p3$design_heritage)
      )
      
      # Conservative schema enforcement:
      # a FALSE gate forces all outcomes FALSE;
      # a TRUE gate with no TRUE category is reset to FALSE.
      if (isFALSE(has_outcome)) {
        outcome_values[] <- FALSE
      } else if (isTRUE(has_outcome) && !any(outcome_values %in% TRUE)) {
        has_outcome <- FALSE
        outcome_values[] <- FALSE
      }
      
      calibration_results$pred_has_outcome[i] <- has_outcome
      calibration_results$pred_outcome_economic[i] <- outcome_values[["economic"]]
      calibration_results$pred_outcome_social[i] <- outcome_values[["social"]]
      calibration_results$pred_outcome_environmental[i] <- outcome_values[["environmental"]]
      calibration_results$pred_outcome_design_heritage[i] <- outcome_values[["design_heritage"]]
      calibration_results$pred_outcome_evidence[i] <- p3$evidence %||% NA_character_
    }
    total_tokens_in <- total_tokens_in + r3$tokens_in
    total_tokens_out <- total_tokens_out + r3$tokens_out
  }
  
  cat(sprintf("[%d/%d] %s × %s\n", i, n_calibration, calibration_results$lpa_number[i], nar))
  cat(sprintf(
    "        function: %s | codable: %s\n",
    calibration_results$pred_function[i],
    calibration_results$pred_codable[i]
  ))
  
  Sys.sleep(1)
}

write_csv(
  calibration_results,
  here(output_dir, "calibration_v5_predictions.csv")
)

cost_in <- total_tokens_in / 1e6 * 0.15
cost_out <- total_tokens_out / 1e6 * 0.60
cat(sprintf("\n=== Calibration cost: $%.3f ===\n", cost_in + cost_out))


# =============================================================
# PHASE 2: COMPUTE METRICS USING THE AGREED EVALUATION DESIGN
# =============================================================

cat("\n\n=== Metrics: v5 predictions vs frozen human gold ===\n\n")

compute_metrics <- function(truth, pred) {
  keep <- !is.na(truth) & !is.na(pred)
  truth <- as.logical(truth[keep])
  pred <- as.logical(pred[keep])
  
  tp <- sum(truth & pred)
  fp <- sum(!truth & pred)
  fn <- sum(truth & !pred)
  tn <- sum(!truth & !pred)
  
  precision <- if (tp + fp > 0) tp / (tp + fp) else NA_real_
  recall <- if (tp + fn > 0) tp / (tp + fn) else NA_real_
  f1 <- if (!is.na(precision) && !is.na(recall) && precision + recall > 0) {
    2 * precision * recall / (precision + recall)
  } else {
    NA_real_
  }
  
  tibble(
    N = length(truth),
    TP = tp,
    FP = fp,
    FN = fn,
    TN = tn,
    precision = precision,
    recall = recall,
    f1 = f1
  )
}

collapse_function <- function(x) {
  case_when(
    is.na(x) ~ NA_character_,
    x %in% c("applicant_claim", "project_description", "technical_assessment") ~ "applicant_side",
    x == "policy_claim" ~ "policy_citation",
    x %in% c("procedural_record", "document_metadata", "unusable_fragment") ~ "not_codable",
    TRUE ~ NA_character_
  )
}

eval_v5 <- calibration_results %>%
  mutate(
    truth_function_3cat = collapse_function(discourse_function_my),
    pred_function_3cat = collapse_function(pred_function)
  )


# ---- Function: 3-category source control ----

function_eval <- eval_v5 %>%
  filter(!is.na(truth_function_3cat), !is.na(pred_function_3cat))

function_3cat_accuracy <- mean(
  function_eval$truth_function_3cat == function_eval$pred_function_3cat
)

function_3cat_confusion <- function_eval %>%
  count(truth_function_3cat, pred_function_3cat, name = "n") %>%
  arrange(truth_function_3cat, pred_function_3cat)

cat(sprintf(
  "Function 3-category accuracy: %.1f%% (%d/%d matched)\n",
  function_3cat_accuracy * 100,
  sum(function_eval$truth_function_3cat == function_eval$pred_function_3cat),
  nrow(function_eval)
))
print(function_3cat_confusion)

policy_metrics <- compute_metrics(
  function_eval$truth_function_3cat == "policy_citation",
  function_eval$pred_function_3cat == "policy_citation"
) %>%
  mutate(category = "policy_citation", .before = 1)

cat("\n=== POLICY-CITATION metrics ===\n")
print(policy_metrics)


# ---- Codable gate ----

codable_eval <- eval_v5 %>%
  filter(!is.na(codable_my), !is.na(pred_codable))

codable_accuracy <- mean(codable_eval$codable_my == codable_eval$pred_codable)

cat(sprintf(
  "\nCodable accuracy: %.1f%% (%d/%d matched)\n",
  codable_accuracy * 100,
  sum(codable_eval$codable_my == codable_eval$pred_codable),
  nrow(codable_eval)
))


# ---- Frames: all gold-codable rows ----

cat("\n=== FRAME per-category metrics (all gold-codable rows) ===\n")

frame_metrics <- eval_v5 %>%
  filter(codable_my == TRUE) %>%
  {
    bind_rows(
      compute_metrics(.$frame_economic_my, .$pred_frame_economic) %>%
        mutate(category = "economic"),
      compute_metrics(.$frame_social_my, .$pred_frame_social) %>%
        mutate(category = "social"),
      compute_metrics(.$frame_environmental_my, .$pred_frame_environmental) %>%
        mutate(category = "environmental"),
      compute_metrics(.$frame_design_heritage_my, .$pred_frame_design_heritage) %>%
        mutate(category = "design_heritage")
    )
  } %>%
  select(category, N, TP, FP, FN, TN, precision, recall, f1)

print(frame_metrics)


# ---- Concrete-outcome gate: all gold-codable rows ----

gate_eval <- eval_v5 %>%
  filter(
    codable_my == TRUE,
    !is.na(has_concrete_outcome_my),
    !is.na(pred_has_outcome)
  )

outcome_gate_accuracy <- mean(
  gate_eval$has_concrete_outcome_my == gate_eval$pred_has_outcome
)

outcome_gate_metrics <- compute_metrics(
  gate_eval$has_concrete_outcome_my,
  gate_eval$pred_has_outcome
) %>%
  mutate(category = "has_concrete_outcome", .before = 1)

cat(sprintf(
  "\nConcrete-outcome gate accuracy: %.1f%% (%d/%d matched)\n",
  outcome_gate_accuracy * 100,
  sum(gate_eval$has_concrete_outcome_my == gate_eval$pred_has_outcome),
  nrow(gate_eval)
))
print(outcome_gate_metrics)


# ---- Outcomes: ALL gold-codable rows, including gold negatives ----

cat("\n=== OUTCOME per-category metrics (all gold-codable rows) ===\n")

outcome_metrics <- eval_v5 %>%
  filter(codable_my == TRUE) %>%
  {
    bind_rows(
      compute_metrics(.$outcome_economic_my, .$pred_outcome_economic) %>%
        mutate(category = "economic"),
      compute_metrics(.$outcome_social_my, .$pred_outcome_social) %>%
        mutate(category = "social"),
      compute_metrics(.$outcome_environmental_my, .$pred_outcome_environmental) %>%
        mutate(category = "environmental"),
      compute_metrics(.$outcome_design_heritage_my, .$pred_outcome_design_heritage) %>%
        mutate(category = "design_heritage")
    )
  } %>%
  select(category, N, TP, FP, FN, TN, precision, recall, f1)

print(outcome_metrics)


# =============================================================
# PHASE 3: CALIBRATION THRESHOLDS AND WORKFLOW DECISION
# =============================================================

# These thresholds evaluate whether v5 is adequate for a researcher-reviewed
# workflow. Passing them does not establish fully automatic generalisation,
# because this 30-row set has already informed prompt development.
target_function_3cat <- 0.75
target_policy_precision <- 0.75
target_policy_recall <- 0.75
target_codable <- 0.85
target_min_frame_f1 <- 0.60
target_outcome_gate <- 0.75
target_min_outcome_f1 <- 0.55

min_available <- function(x) {
  if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
}

min_frame_f1 <- min_available(frame_metrics$f1)
min_outcome_f1 <- min_available(outcome_metrics$f1)
policy_precision <- policy_metrics$precision[[1]]
policy_recall <- policy_metrics$recall[[1]]

metric_summary <- tibble(
  metric = c(
    "function_3cat_accuracy",
    "policy_precision",
    "policy_recall",
    "codable_accuracy",
    "min_frame_f1",
    "outcome_gate_accuracy",
    "min_outcome_f1"
  ),
  value = c(
    function_3cat_accuracy,
    policy_precision,
    policy_recall,
    codable_accuracy,
    min_frame_f1,
    outcome_gate_accuracy,
    min_outcome_f1
  ),
  target = c(
    target_function_3cat,
    target_policy_precision,
    target_policy_recall,
    target_codable,
    target_min_frame_f1,
    target_outcome_gate,
    target_min_outcome_f1
  )
) %>%
  mutate(pass = !is.na(value) & value >= target)

cat("\n\n=== CALIBRATION THRESHOLDS ===\n")
print(metric_summary)

calibration_pass <- all(metric_summary$pass)

if (calibration_pass) {
  cat("\n*** CALIBRATION THRESHOLDS MET ***\n")
  cat("Freeze v5 and proceed to a 5-project workflow pilot with targeted researcher adjudication.\n")
} else {
  cat("\n*** CALIBRATION THRESHOLDS NOT MET ***\n")
  cat("Freeze v5; do not tune again on the same 30 rows. Use v5 as an initial screen plus targeted researcher adjudication.\n")
}


# =============================================================
# PHASE 4: SAVE AUDIT OUTPUTS
# =============================================================

calibration_errors <- eval_v5 %>%
  mutate(
    error_function_3cat = coalesce(
      truth_function_3cat != pred_function_3cat,
      TRUE
    ),
    error_codable = coalesce(codable_my != pred_codable, TRUE),
    error_outcome_gate = if_else(
      codable_my == TRUE,
      coalesce(has_concrete_outcome_my != pred_has_outcome, TRUE),
      FALSE,
      missing = FALSE
    ),
    error_any_frame = if_else(
      codable_my == TRUE,
      coalesce(frame_economic_my != pred_frame_economic, TRUE) |
        coalesce(frame_social_my != pred_frame_social, TRUE) |
        coalesce(frame_environmental_my != pred_frame_environmental, TRUE) |
        coalesce(frame_design_heritage_my != pred_frame_design_heritage, TRUE),
      FALSE,
      missing = FALSE
    ),
    error_any_outcome = if_else(
      codable_my == TRUE,
      coalesce(outcome_economic_my != pred_outcome_economic, TRUE) |
        coalesce(outcome_social_my != pred_outcome_social, TRUE) |
        coalesce(outcome_environmental_my != pred_outcome_environmental, TRUE) |
        coalesce(outcome_design_heritage_my != pred_outcome_design_heritage, TRUE),
      FALSE,
      missing = FALSE
    )
  ) %>%
  filter(
    error_function_3cat |
      error_codable |
      error_outcome_gate |
      error_any_frame |
      error_any_outcome
  )

write_csv(
  function_3cat_confusion,
  here(output_dir, "new_holdout_v5_predictions.csv")
)
write_csv(
  policy_metrics,
  here(output_dir, "calibration_v5_policy_metrics.csv")
)
write_csv(
  frame_metrics,
  here(output_dir, "calibration_v5_frame_metrics.csv")
)
write_csv(
  outcome_gate_metrics,
  here(output_dir, "calibration_v5_outcome_gate_metrics.csv")
)
write_csv(
  outcome_metrics,
  here(output_dir, "calibration_v5_outcome_metrics.csv")
)
write_csv(
  metric_summary,
  here(output_dir, "calibration_v5_metric_summary.csv")
)
write_csv(
  calibration_errors,
  here(output_dir, "calibration_v5_errors.csv")
)

cat("\nOutputs saved:\n")
cat("- output/calibration_v5_predictions_FRESH.csv\n")
cat("- output/calibration_v5_function_3cat_confusion.csv\n")
cat("- output/calibration_v5_policy_metrics.csv\n")
cat("- output/calibration_v5_frame_metrics.csv\n")
cat("- output/calibration_v5_outcome_gate_metrics.csv\n")
cat("- output/calibration_v5_outcome_metrics.csv\n")
cat("- output/calibration_v5_metric_summary.csv\n")
cat("- output/calibration_v5_errors.csv\n")