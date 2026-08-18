# 18_find_and_score_fresh.R —— 找到 fresh 预测落哪了,算 frame/outcome,和 blind 并排
library(tidyverse); library(here)
out <- here("output")

fresh_marker <- c("PA/14/02819","LL/17/00344","PA/24/00996","PA/15/02675")  # fresh 独有
blind_marker <- c("PA/16/00772","PA/22/02551","PA/18/01877","PA/16/02808")  # blind 独有
id_set <- function(df) {
  lp <- as.character(df$lpa_number)
  if (any(fresh_marker %in% lp)) "FRESH"
  else if (any(blind_marker %in% lp)) "BLIND" else "UNKNOWN"
}

cat("=== output/ 最近改动的 csv ===\n")
info <- file.info(list.files(out, full.names=TRUE, pattern="\\.csv$"))
print(head(data.frame(file=basename(rownames(info)), mtime=info$mtime)[order(info$mtime, decreasing=TRUE),], 8),
      row.names=FALSE)

cands <- list.files(out, pattern="predict", full.names=TRUE)
cat("\n=== 各预测文件装的是哪个集 ===\n")
pred_file <- NULL
for (f in cands) {
  d <- suppressMessages(read_csv(f, show_col_types=FALSE))
  if (!any(grepl("^pred_", names(d)))) next
  s <- id_set(d); cat(basename(f), "->", s, "(", nrow(d), "行)\n")
  if (s == "FRESH" && grepl("v5", f)) pred_file <- f
}

if (is.null(pred_file)) {
  cat("\n>>> 没找到装 FRESH 的 v5 预测。真正的 write_csv 路径没改成功。\n")
  cat(">>> 回你 v5 预测脚本里搜 write_csv,把写 predictions 那行改成 _FRESH,重跑。\n")
} else {
  cat("\n>>> FRESH 预测在:", basename(pred_file), "\n")
  fr <- suppressMessages(read_csv(pred_file, show_col_types=FALSE))
  cm <- function(truth, pred) {
    truth<-as.logical(truth); pred<-as.logical(pred)
    ok<-!is.na(truth)&!is.na(pred); truth<-truth[ok]; pred<-pred[ok]
    TP<-sum(truth&pred);FP<-sum(!truth&pred);FN<-sum(truth&!pred);TN<-sum(!truth&!pred)
    prec<-if((TP+FP)>0)TP/(TP+FP) else NA; rec<-if((TP+FN)>0)TP/(TP+FN) else NA
    f1<-if(!is.na(prec)&&!is.na(rec)&&(prec+rec)>0)2*prec*rec/(prec+rec) else NA
    tibble(N=length(truth),TP,FP,FN,TN,precision=round(prec,3),recall=round(rec,3),f1=round(f1,3))
  }
  cod <- fr %>% filter(codable_my == TRUE)
  cats <- c("economic","social","environmental","design_heritage")
  frame_fresh <- map_dfr(cats, ~cm(cod[[paste0("frame_",.x,"_my")]], cod[[paste0("pred_frame_",.x)]]) %>% mutate(category=.x,.before=1))
  outc_fresh  <- map_dfr(cats, ~cm(cod[[paste0("outcome_",.x,"_my")]], cod[[paste0("pred_outcome_",.x)]]) %>% mutate(category=.x,.before=1))
  gate_fresh  <- cm(cod$has_concrete_outcome_my, cod$pred_has_outcome) %>% mutate(category="gate",.before=1)
  cat(sprintf("\nCodable acc (fresh): %.1f%%  (n=%d)\n", 100*mean(fr$codable_my==fr$pred_codable,na.rm=TRUE), nrow(fr)))
  cat("\n=== FRAME (fresh) ===\n"); print(frame_fresh)
  cat("\n=== OUTCOME gate (fresh) ===\n"); print(gate_fresh)
  cat("\n=== OUTCOME per-cat (fresh) ===\n"); print(outc_fresh)
  blind_f1 <- tribble(~category,~f1_blind, "economic",0.857,"social",0.882,"environmental",0.667,"design_heritage",0.870)
  cat("\n=== FRAME F1: blind vs fresh ===\n")
  frame_fresh %>% transmute(category, f1_fresh=f1) %>% left_join(blind_f1, by="category") %>% print()
}