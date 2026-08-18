# =============================================================
# 16_join_diagnosis.R
# fresh gold 与 v5 predictions 按 key 0/30 匹配 —— 判定良性/严重。
# 依赖上一脚本已定义的 safe_read()、build_key();tidyverse 已加载。
# =============================================================
peek <- function(fname) {
  df <- safe_read(fname); if (is.null(df)) return(NULL)
  cat("\n--- ", fname, "  (", nrow(df), " rows) ---\n", sep = "")
  cat("列名:\n"); print(names(df))
  k <- build_key(df)
  cat("build_key ->", attr(k, "key_type"), "| 唯一:", !anyDuplicated(k), "\n")
  cat("前 5 个 key:\n"); print(head(k, 5))
  invisible(df)
}

g <- peek("fresh_holdout_30.csv")
b <- peek("blind_validation_30.csv")
p <- peek("calibration_v5_predictions.csv")

proj_of <- function(df) {
  if (is.null(df)) return(character(0))
  nm  <- names(df)
  col <- nm[str_detect(nm, regex("project|pa_?ref|lpa|app.*(no|ref|id)", ignore_case = TRUE))]
  if (!length(col)) return(rep(NA_character_, nrow(df)))
  as.character(df[[col[1]]])
}
pg <- proj_of(g); pb <- proj_of(b); pp <- proj_of(p)

cat("\n================ PROJECT-LEVEL OVERLAP ================\n")
cat("pred ∩ fresh (unique projects):", length(intersect(unique(pp), unique(pg))),
    " | fresh 有", n_distinct(pg), "个, pred 有", n_distinct(pp), "个\n")
cat("pred ∩ blind (unique projects):", length(intersect(unique(pp), unique(pb))),
    " | blind 有", n_distinct(pb), "个\n")
cat("\nfresh 的 project 样例:\n"); print(head(unique(pg)))
cat("pred  的 project 样例:\n"); print(head(unique(pp)))
cat("blind 的 project 样例:\n"); print(head(unique(pb)))

cat("\n判读:\n")
cat("  · pred∩fresh 高、但完整 key 0 匹配 -> 同批 instance,只是 key 字符串格式不同(良性,可按内容重建 key)。\n")
cat("  · pred∩blind 明显更高 -> 预测其实打在 blind 上,报的 F1 用错了 gold(严重)。\n")
cat("  · 两个都低 -> 预测打在第三批 instance 上,得回去找对的预测文件(严重)。\n")