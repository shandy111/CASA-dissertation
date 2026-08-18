# 20_score_new_holdout.R —— 整段跑,不用改
library(tidyverse); library(here)
out <- here("output")
gold <- suppressMessages(read_csv(file.path(out,"new_holdout_coding_sheet.csv"), show_col_types=FALSE))
pred <- suppressMessages(read_csv(file.path(out,"new_holdout_v5_predictions.csv"), show_col_types=FALSE))

mk <- function(df){
  ctx <- str_squish(as.character(df$context))
  paste(as.character(df$lpa_number), as.character(df$narrative), substr(ctx,1,80), sep=" || ")
}
gold$.k <- mk(gold); pred$.k <- mk(pred)

cat("手标表:",nrow(gold),"行 | v5预测:",nrow(pred),"行 | 能对上:",
    length(intersect(gold$.k,pred$.k)),"/",nrow(gold),"\n")

if (length(intersect(gold$.k,pred$.k)) < nrow(gold)*0.9) {
  cat("\n>>> 对不上的太多。八成 v5 跑的不是这张新表,或输出文件名弄错。\n")
  cat(">>> 别看结果,回小步1确认输入=new_holdout_coding_sheet.csv、输出=new_holdout_v5_predictions.csv。\n")
} else {
  ev  <- inner_join(gold, pred %>% select(.k, starts_with("pred_")), by=".k")
  cod <- ev %>% filter(codable == 1)
  cat("你标为 codable 的行:",nrow(cod),"\n")
  
  score1 <- function(g,p){
    g<-as.integer(g); p<-as.integer(p); ok<-!is.na(g)&!is.na(p); g<-g[ok]; p<-p[ok]
    TP<-sum(g==1&p==1); FP<-sum(g==0&p==1); FN<-sum(g==1&p==0)
    prec<-if((TP+FP)>0)TP/(TP+FP) else NA; rec<-if((TP+FN)>0)TP/(TP+FN) else NA
    f1<-if(!is.na(prec)&&!is.na(rec)&&(prec+rec)>0)2*prec*rec/(prec+rec) else NA
    tibble(gold_pos=sum(g==1),TP,FP,FN,precision=round(prec,2),recall=round(rec,2),f1=round(f1,2))
  }
  cats <- c("economic","social","environmental","design_heritage")
  res <- map_dfr(cats, ~score1(cod[[paste0("frame_",.x)]], cod[[paste0("pred_frame_",.x)]]) %>% mutate(frame=.x,.before=1))
  
  verdict <- res %>% mutate(判定 = case_when(
    frame=="environmental" ~ "只做描述(不当主结论)",
    frame=="social" & f1>=0.70 & precision>=0.70 & gold_pos>=8 ~ "PASS ✅",
    frame=="social" ~ "FAIL -> 交给空间数据",
    f1>=0.70 & gold_pos>=8 ~ "PASS ✅",
    gold_pos<8 ~ "样本太少,照实报不确定性",
    TRUE ~ "FAIL -> 交给空间数据"
  )) %>% select(frame, gold_pos, precision, recall, f1, 判定)
  
  cat("\n================ 对照及格线 ================\n"); print(verdict)
  cat("\n判读:\n")
  cat("  · economic + social 都 PASS -> 核心 economic/social 轴 held-out 站住,进正文、跑 59。\n")
  cat("  · 哪类 FAIL -> 那条结论交给空间数据扛,不用文字标注单独下结论。\n")
}