# =============================================================
# 22_run_v5_frame_corpus.R
# 用验证过的 v5,对 59 语料全部 instance 跑 function(取 codable)+ frame。
# 不跑 outcome。每 50 条存盘,可断点续跑(再跑同一脚本会自动跳过已完成)。
# prompt 与 21_predict / 14_semantic_final 完全一致。
# =============================================================
source(here::here("scripts", "00_setup.R"))
library(httr); library(jsonlite)

CAP_PER_CELL <- 10   # 10 = 用全部;想快改成 5(每项目比例更抖)。开跑前定好,别中途改。

# ---- v5 prompts (function + frame),未改一字 ----
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

# ---- helpers (未改) ----
call_openai <- function(prompt, max_retries = 3) {
  for (attempt in seq_len(max_retries)) {
    response <- tryCatch({
      POST(url = "https://api.openai.com/v1/chat/completions",
           add_headers(Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")), "Content-Type" = "application/json"),
           body = toJSON(list(model = "gpt-4o-mini",
                              messages = list(list(role = "user", content = prompt)),
                              response_format = list(type = "json_object"),
                              temperature = 0, max_tokens = 500), auto_unbox = TRUE),
           timeout(60))
    }, error = function(e) NULL)
    if (!is.null(response) && status_code(response) == 200) {
      result <- content(response)
      return(list(text = result$choices[[1]]$message$content,
                  tokens_in = result$usage$prompt_tokens, tokens_out = result$usage$completion_tokens))
    }
    if (attempt < max_retries) Sys.sleep(attempt * 10)
  }
  list(error = "api_failed")
}
as_bool <- function(x) {
  if (length(x) != 1 || is.null(x) || is.na(x)) return(NA)
  if (is.logical(x)) return(x)
  if (is.character(x) && tolower(x) %in% c("true","false")) return(tolower(x) == "true")
  NA
}
allowed_functions <- c("applicant_claim","policy_claim","project_description",
                       "technical_assessment","procedural_record","document_metadata","unusable_fragment")

# ---- 读池子 + 抽样(如设了 CAP) ----
pool <- read_csv(here(output_dir, "llm_semantic_frame_v3_full.csv"), show_col_types = FALSE) %>%
  transmute(lpa_number = as.character(lpa_number), narrative = as.character(narrative), context = as.character(context))
set.seed(42)
if (CAP_PER_CELL < 10) pool <- pool %>% group_by(lpa_number, narrative) %>% slice_sample(n = CAP_PER_CELL) %>% ungroup()

mk <- function(lpa, nar, ctx) paste(lpa, nar, substr(str_squish(ctx), 1, 80), sep = " || ")
pool$.k <- mk(pool$lpa_number, pool$narrative, pool$context)
results <- pool %>% mutate(pred_function=NA_character_, pred_codable=NA,
                           pred_frame_economic=NA, pred_frame_social=NA, pred_frame_environmental=NA,
                           pred_frame_design_heritage=NA, pred_frame_evidence=NA_character_)

out_path <- here(output_dir, "corpus_59_v5_frame_predictions.csv")

# ---- 断点续跑:载入已完成的 ----
if (file.exists(out_path)) {
  prev <- suppressMessages(read_csv(out_path, show_col_types = FALSE))
  if (".k" %in% names(prev)) {
    fill <- prev %>% filter(!is.na(pred_frame_economic)) %>% select(.k, starts_with("pred_"))
    idx <- match(fill$.k, results$.k); ok <- !is.na(idx)
    for (col in setdiff(names(fill), ".k")) results[[col]][idx[ok]] <- fill[[col]][ok]
    cat("续跑:已完成", sum(ok), "条,跳过。\n")
  }
}

todo <- which(is.na(results$pred_frame_economic))
cat("总", nrow(results), "条 | 待跑", length(todo), "条 | 预计约",
    round(length(todo) * 2.8 / 60), "分钟\n\n")

# ---- 主循环,每 50 条存盘 ----
for (j in seq_along(todo)) {
  i <- todo[j]; ctx <- results$context[i]; nar <- results$narrative[i]
  
  r1 <- call_openai(build_function_prompt(ctx, nar))
  if (is.null(r1$error)) {
    p1 <- tryCatch(fromJSON(r1$text), error = function(e) NULL)
    if (!is.null(p1)) {
      fv <- p1[["function"]] %||% NA_character_
      if (length(fv) == 1 && fv %in% allowed_functions) results$pred_function[i] <- fv
      results$pred_codable[i] <- as_bool(p1$codable)
    }
  }
  Sys.sleep(0.3)
  
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
  Sys.sleep(0.3)
  
  if (j %% 50 == 0 || j == length(todo)) {
    write_csv(results, out_path)
    cat(sprintf("[%d/%d] 已存盘 %s\n", j, length(todo), format(Sys.time(), "%H:%M:%S")))
  }
}

write_csv(results, out_path)
n_ok <- sum(!is.na(results$pred_frame_economic)); n_bad <- sum(is.na(results$pred_frame_economic))
cat("\n================ 完成 ================\n")
cat("总", nrow(results), "| 成功", n_ok, "| 还差", n_bad, "\n")
if (n_bad > 0) cat(">>> 还差", n_bad, "条(多半 API 偶发失败)。再跑一次同一脚本,自动只补这些。\n")
cat("存于: output/corpus_59_v5_frame_predictions.csv\n")