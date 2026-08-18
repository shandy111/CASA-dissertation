# 17_score_v5_on_fresh.R —— v5 打在 fresh 上的 frame/outcome 指标,并与 blind 并排
library(tidyverse); library(here)

cm <- function(truth, pred) {
  truth <- as.logical(truth); pred <- as.logical(pred)
  ok <- !is.na(truth) & !is.na(pred); truth <- truth[ok]; pred <- pred[ok]
  TP <- sum(truth & pred); FP <- sum(!truth & pred)
  FN <- sum(truth & !pred); TN <- sum(!truth & !pred)
  prec <- if ((TP+FP)>0) TP/(TP+FP) else NA_real_
  rec  <- if ((TP+FN)>0) TP/(TP+FN) else NA_real_
  f1   <- if (!is.na(prec)&&!is.na(rec)&&(prec+rec)>0) 2*prec*rec/(prec+rec) else NA_real_
  tibble(N=length(truth), TP, FP, FN, TN,
         precision=round(prec,3), recall=round(rec,3), f1=round(f1,3))
}

fr <- read_csv(here("output","calibration_v5_predictions_FRESH.csv"), show_col_types=FALSE)
stopifnot("frame_economic_my" %in% names(fr), "pred_frame_economic" %in% names(fr))
cod <- fr %>% filter(codable_my == TRUE)
cats <- c("economic","social","environmental","design_heritage")

frame_fresh <- map_dfr(cats, ~ cm(cod[[paste0("frame_",.x,"_my")]],
                                  cod[[paste0("pred_frame_",.x)]]) %>% mutate(category=.x,.before=1))
outc_fresh  <- map_dfr(cats, ~ cm(cod[[paste0("outcome_",.x,"_my")]],
                                  cod[[paste0("pred_outcome_",.x)]]) %>% mutate(category=.x,.before=1))
gate_fresh  <- cm(cod$has_concrete_outcome_my, cod$pred_has_outcome) %>% mutate(category="has_concrete_outcome",.before=1)
codable_acc <- mean(fr$codable_my == fr$pred_codable, na.rm=TRUE)

cat(sprintf("\nCodable accuracy (fresh): %.1f%%\n", 100*codable_acc))
cat("\n=== FRAME (fresh) ===\n");          print(frame_fresh)
cat("\n=== OUTCOME gate (fresh) ===\n");    print(gate_fresh)
cat("\n=== OUTCOME per-cat (fresh) ===\n"); print(outc_fresh)

# 并排 blind(你已存的 v5 blind 指标)
blind_frame <- read_csv(here("output","calibration_v5_frame_metrics.csv"), show_col_types=FALSE)
cat("\n=== FRAME F1: blind vs fresh ===\n")
frame_fresh %>% select(category, f1_fresh=f1) %>%
  left_join(blind_frame %>% select(category, f1_blind=f1), by="category") %>%
  print()
