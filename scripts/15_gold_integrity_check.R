# =============================================================
# 15_gold_integrity_check.R
# 目的:在信任任何 v5 指标之前,验证 gold 标签完整性。
# 回答:(1) 哪些 instance 真被裁决;(2) 当前 gold 是否=裁决后 gold;
#       (3) prediction 是否按稳定 key(而非行号)join;(4) 有无重复/孤儿。
# 不重算指标、不改 prompt。
# =============================================================
library(tidyverse)
library(here)

out <- here("output")

safe_read <- function(fname) {
  p <- file.path(out, fname)
  if (!file.exists(p)) { message("  [MISSING] ", fname); return(NULL) }
  readr::read_csv(p, show_col_types = FALSE, progress = FALSE)
}

# ---- 0. 清点相关文件 ----
cat("\n================ FILE INVENTORY (output/) ================\n")
print(list.files(out, pattern = "valid|holdout|predict|v4|v5|gold|calibr",
                 ignore.case = TRUE))

# ---- 认 key:先找显式 id,没有就用 project+keyword+context 组合键 ----
build_key <- function(df) {
  nm <- names(df)
  id_col   <- nm[str_detect(nm, regex("instance_id|^id$|row_id|uid", ignore_case = TRUE))]
  proj_col <- nm[str_detect(nm, regex("project|pa_?ref|lpa|app.*(no|ref|id)", ignore_case = TRUE))]
  kw_col   <- nm[str_detect(nm, regex("keyword|narrative|term", ignore_case = TRUE))]
  ctx_col  <- nm[str_detect(nm, regex("context|evidence|quote|snippet|text", ignore_case = TRUE))]
  
  if (length(id_col) >= 1) {
    key <- as.character(df[[id_col[1]]])
    attr(key, "key_type") <- paste0("explicit id: ", id_col[1]); return(key)
  }
  parts <- list()
  if (length(proj_col) >= 1) parts$proj <- as.character(df[[proj_col[1]]])
  if (length(kw_col)   >= 1) parts$kw   <- as.character(df[[kw_col[1]]])
  if (length(ctx_col)  >= 1) parts$ctx  <- substr(as.character(df[[ctx_col[1]]]), 1, 60)
  if (length(parts) == 0) {
    key <- as.character(seq_len(nrow(df)))
    attr(key, "key_type") <- "NONE FOUND -> 退回行号(脆弱!)"; return(key)
  }
  key <- do.call(paste, c(parts, sep = " || "))
  attr(key, "key_type") <- paste0("composite: ", paste(names(parts), collapse = "+")); key
}

# ---- 1. 当前 gold vs 原始 backup 逐格 diff = 真正的裁决 ----
diff_gold <- function(cur_name, bak_name) {
  cat("\n================ ADJUDICATION DIFF ================\n")
  cat(cur_name, "  vs  ", bak_name, "\n")
  cur <- safe_read(cur_name); bak <- safe_read(bak_name)
  if (is.null(cur) || is.null(bak)) return(invisible(NULL))
  
  kc <- build_key(cur); kb <- build_key(bak)
  cat("  key(current):", attr(kc, "key_type"), "| key(backup):", attr(kb, "key_type"), "\n")
  if (anyDuplicated(kc) || anyDuplicated(kb))
    cat("  [WARN] key 不唯一 —— 比较不可靠,先修 key。\n")
  
  common <- intersect(names(cur), names(bak))
  cur2 <- cur %>% mutate(.k = kc) %>% select(.k, all_of(common))
  bak2 <- bak %>% mutate(.k = kb) %>% select(.k, all_of(common))
  j <- inner_join(cur2, bak2, by = ".k", suffix = c(".cur", ".bak"))
  cat("  按 key 匹配上的行:", nrow(j), " (current n=", nrow(cur),
      ", backup n=", nrow(bak), ")\n", sep = "")
  
  changed <- map_dfr(setdiff(common, ".k"), function(col) {
    a <- j[[paste0(col, ".cur")]]; b <- j[[paste0(col, ".bak")]]
    idx <- which(!(as.character(a) == as.character(b)) | (is.na(a) != is.na(b)))
    if (length(idx) == 0) return(tibble())
    tibble(key = j$.k[idx], column = col,
           backup = as.character(b[idx]), current = as.character(a[idx]))
  })
  if (nrow(changed) == 0) {
    cat("  >>> 没有任何格子变化。当前 gold == backup(这里啥都没裁决)。\n")
  } else {
    cat("  >>>", nrow(changed), "个裁决改动,落在",
        n_distinct(changed$key), "个 instance 上:\n")
    print(changed, n = 100)
  }
  invisible(changed)
}

fresh_changes <- diff_gold("fresh_holdout_30.csv", "fresh_holdout_30_original_backup.csv")
blind_changes <- diff_gold("blind_validation_30.csv", "blind_validation_30_original_backup.csv")

# ---- 2. prediction <-> gold join 完整性 ----
check_join <- function(gold_name, pred_name) {
  cat("\n================ JOIN INTEGRITY ================\n")
  cat("gold:", gold_name, "  pred:", pred_name, "\n")
  gold <- safe_read(gold_name); pred <- safe_read(pred_name)
  if (is.null(gold) || is.null(pred)) return(invisible(NULL))
  kg <- build_key(gold); kp <- build_key(pred)
  cat("  key(gold):", attr(kg, "key_type"), "| key(pred):", attr(kp, "key_type"), "\n")
  cat("  gold key 唯一?", !anyDuplicated(kg), "| pred key 唯一?", !anyDuplicated(kp), "\n")
  cat("  gold 里没有对应 prediction 的行:", length(setdiff(kg, kp)), "\n")
  cat("  prediction 里没有对应 gold 的行:", length(setdiff(kp, kg)), "\n")
  if (grepl("行号", attr(kg, "key_type")) && grepl("行号", attr(kp, "key_type")))
    cat("  [WARN] 两边都在用行号 join —— 这正是那个脆弱拼接。\n")
  invisible(NULL)
}

# >>> 把 pred_name 改成喂给 eval_v5 的那个预测文件,再取消注释运行 <
# check_join("fresh_holdout_30.csv", "holdout_v5_predictions.csv")

cat("\nDONE. 先看 ADJUDICATION DIFF:确认真被裁决的 instance(按稳定 key,\n")
cat("不是 1/6/13 vs 2/10/20 的行号),再填 pred_name 跑 check_join,\n")
cat("确认 F1 是算在裁决后 gold + 按 key join 之后,才能信那批指标。\n")