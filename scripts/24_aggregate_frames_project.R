# 24_aggregate_frames_project.R —— 逐句 frame -> 每个项目的话术比例
library(tidyverse); library(here)
d <- read_csv(here("output","corpus_59_v5_frame_predictions.csv"), show_col_types=FALSE)

cod <- d %>% filter(pred_codable == TRUE)   # 只用能用的句子当分母

proj <- cod %>%
  group_by(lpa_number) %>%
  summarise(
    n_codable = n(),
    economic_pct      = round(mean(pred_frame_economic == TRUE, na.rm=TRUE), 3),
    social_pct        = round(mean(pred_frame_social == TRUE, na.rm=TRUE), 3),
    design_pct        = round(mean(pred_frame_design_heritage == TRUE, na.rm=TRUE), 3),
    environmental_pct = round(mean(pred_frame_environmental == TRUE, na.rm=TRUE), 3),
    .groups = "drop"
  ) %>%
  mutate(econ_minus_social = round(economic_pct - social_pct, 3),
         low_n_flag = n_codable < 5) %>%
  arrange(desc(n_codable))

write_csv(proj, here("output","corpus_59_frame_pct.csv"))

cat("项目数:", nrow(proj), "| 低样本(<5句)标记:", sum(proj$low_n_flag), "个\n\n")
cat("economic_pct 分布:\n"); print(summary(proj$economic_pct))
cat("social_pct 分布:\n");   print(summary(proj$social_pct))
cat("design_pct 分布:\n");   print(summary(proj$design_pct))
cat("\n偏经济最多的 5 个项目:\n")
print(proj %>% filter(!low_n_flag) %>% arrange(desc(econ_minus_social)) %>%
        select(lpa_number, n_codable, economic_pct, social_pct) %>% head(5))
cat("\n偏社会最多的 5 个项目:\n")
print(proj %>% filter(!low_n_flag) %>% arrange(econ_minus_social) %>%
        select(lpa_number, n_codable, economic_pct, social_pct) %>% head(5))
cat("\n存于: output/corpus_59_frame_pct.csv\n")

# ===== 接在 24_aggregate 后面:验 join + 接总表 + 挑例子候选 =====
spatial <- read_csv(here("output","corpus_59_full_spatial.csv"), show_col_types=FALSE)
spatial$lpa_number <- as.character(spatial$lpa_number)

miss <- setdiff(proj$lpa_number, spatial$lpa_number)
cat("=== JOIN 检查 ===\n")
cat("话术表 59 个项目里,总表接不上的:", length(miss), "\n")
if (length(miss) > 0) { cat("接不上的(需先对格式):\n"); print(miss) }

if (length(miss) == 0) {
  merged <- spatial %>%
    left_join(
      proj %>% select(lpa_number, n_codable_frame = n_codable,
                      economic_pct, social_pct, design_pct,
                      environmental_pct, low_n_flag),
      by = "lpa_number"
    )
  write_csv(merged, here("output","corpus_59_full_spatial_framed.csv"))
  cat("\n全部接上。已存 corpus_59_full_spatial_framed.csv (", ncol(merged), "列)\n")
  
  cat("\n=== 讲故事用的例子候选(Chng 式) ===\n")
  cat("经济话术最重(economic_pct 高):\n")
  print(proj %>% filter(!low_n_flag) %>% arrange(desc(economic_pct)) %>%
          select(lpa_number, n_codable, economic_pct, social_pct) %>% head(3))
  cat("最纯社会包装(economic 极低):\n")
  print(proj %>% filter(!low_n_flag) %>% arrange(economic_pct) %>%
          select(lpa_number, n_codable, economic_pct, social_pct) %>% head(3))
} else {
  cat("\n>>> 先别接。把接不上的项目号发我,多半是 lpa_number 后缀/前缀格式差,我给一句归一化再接。\n")
}
# ===== 接在后面:economic/social 话术 × 既有场地条件(空间侧) =====
library(tidyverse); library(here)
m <- read_csv(here("output","corpus_59_full_spatial_framed.csv"), show_col_types=FALSE) %>%
  filter(!low_n_flag)                      # 剔掉 2 个单句项目
cat("用于分析的项目数:", nrow(m), "\n")

num_cols <- names(m)[map_lgl(m, is.numeric)]
ctx_pat  <- "imd|ptal|canopy|tree|bus|cycle|school|station|open.?space|green|dist|depriv|accessib"
excl_pat <- "pct|_pred|outcome|dwelling|density|affordable|commercial|floorspace|storey|height|units|site_area|lat|lon|_x$|_y$|id$|n_codable"
ctx_cols <- num_cols[str_detect(num_cols, regex(ctx_pat, ignore_case=TRUE)) &
                       !str_detect(num_cols, regex(excl_pat, ignore_case=TRUE))]
cat("\n认到的既有条件列(先核对,别混进结果变量):\n"); print(ctx_cols)

sp <- function(x,y){ ok<-!is.na(x)&!is.na(y); if(sum(ok)<10) return(c(rho=NA,p=NA,n=sum(ok)))
t<-suppressWarnings(cor.test(x[ok],y[ok],method="spearman")); c(rho=unname(t$estimate),p=t$p.value,n=sum(ok)) }

if (length(ctx_cols)>0) {
  res <- map_dfr(ctx_cols, function(cc){
    e<-sp(m$economic_pct,m[[cc]]); s<-sp(m$social_pct,m[[cc]])
    tibble(context=cc, econ_rho=round(e["rho"],3), econ_p=round(e["p"],3),
           soc_rho=round(s["rho"],3), soc_p=round(s["p"],3), n=e["n"])
  }) %>% arrange(desc(pmax(abs(econ_rho),abs(soc_rho),na.rm=TRUE)))
  cat("\n=== economic/social 话术 × 既有条件 (Spearman) ===\n"); print(res, n=50)
  cat("(探索性:多组检验没做校正,看效应量 rho,别只盯 p。)\n")
} else cat("\n没认到条件列——把 names(m) 发我,我手动指定。\n")

# ===== social/economic 分类误差随不随 IMD 变(在验证集上查,唯一有 gold 的地方) =====
tryCatch({
  g <- read_csv(here("output","new_holdout_coding_sheet.csv"), show_col_types=FALSE)
  p <- read_csv(here("output","new_holdout_v5_predictions.csv"), show_col_types=FALSE)
  mk <- function(df) paste(df$lpa_number, df$narrative, substr(str_squish(df$context),1,80), sep=" || ")
  g$.k<-mk(g); p$.k<-mk(p)
  ev <- inner_join(g, p %>% select(.k, starts_with("pred_")), by=".k") %>% filter(codable==1)
  imd_col <- names(m)[str_detect(names(m), regex("^imd|imd_score", ignore_case=TRUE))][1]
  imd_map <- read_csv(here("output","corpus_59_full_spatial_framed.csv"), show_col_types=FALSE) %>%
    transmute(lpa_number=as.character(lpa_number), imd=suppressWarnings(as.numeric(.data[[imd_col]])))
  ev <- ev %>% left_join(imd_map, by="lpa_number") %>%
    mutate(imd_band=c("low","mid","high")[ntile(imd,3)],
           soc_err = as.integer(frame_social)!=as.integer(pred_frame_social),
           econ_err= as.integer(frame_economic)!=as.integer(pred_frame_economic))
  chk <- ev %>% filter(!is.na(imd_band)) %>% group_by(imd_band) %>%
    summarise(n=n(), social_err=round(mean(soc_err,na.rm=TRUE),2),
              economic_err=round(mean(econ_err,na.rm=TRUE),2), .groups="drop")
  cat("\n=== 误差 × IMD 档 (验证集 45 条,小样本,只看大差异) ===\n"); print(chk)
  cat("三档 social_err 差不多 -> social×IMD 不是分类器偏差造的,放心用;\n")
  cat("低 IMD 那档明显更高 -> social×IMD 要加偏差 caveat。\n")
}, error=function(e) cat("\n(第二段出错,把红字发我:", conditionMessage(e), ")\n"))

# ===== 接后面:economic/social 话术 × 拟议 outcome —— 对齐/错位矩阵 =====
library(tidyverse); library(here)
d <- read_csv(here("output","corpus_59_full_spatial_framed.csv"), show_col_types=FALSE) %>%
  filter(!low_n_flag)

d$commercial_floorspace_sqm  <- suppressWarnings(as.numeric(d$commercial_floorspace_sqm))
d$affordable_percentage      <- suppressWarnings(as.numeric(d$affordable_percentage))
d$public_realm_area_sqm      <- suppressWarnings(as.numeric(d$public_realm_area_sqm))
d$community_facilities_count <- suppressWarnings(as.numeric(d$community_facilities_count))
# 决定③:affordable 丢掉存疑的 3 个 0 -> 只留 >0
d <- d %>% mutate(affordable_percentage = ifelse(affordable_percentage > 0, affordable_percentage, NA))

sp <- function(x, y){ ok <- !is.na(x) & !is.na(y); n <- sum(ok)
if (n < 10) return(tibble(rho=NA, p=NA, n=n))
t <- suppressWarnings(cor.test(x[ok], y[ok], method="spearman"))
tibble(rho=round(unname(t$estimate),3), p=round(t$p.value,3), n=n) }

cat("=== 对齐/错位矩阵 (Spearman) ===\n")
cat("\nA  economic_pct × commercial   (说经济→给经济?):\n"); print(sp(d$economic_pct, d$commercial_floorspace_sqm))
cat("D  social_pct   × affordable%   (说社会→给社会?):\n");  print(sp(d$social_pct,   d$affordable_percentage))
cat("B  economic_pct × affordable%   (错位):\n");            print(sp(d$economic_pct, d$affordable_percentage))
cat("C  social_pct   × commercial    (错位):\n");            print(sp(d$social_pct,   d$commercial_floorspace_sqm))
cat("\n-- 次要(社会侧补充) --\n")
cat("social_pct × public_realm_sqm:\n");    print(sp(d$social_pct,    d$public_realm_area_sqm))
cat("social_pct × community_facilities:\n"); print(sp(d$social_pct,    d$community_facilities_count))
cat("economic_pct × public_realm_sqm:\n");   print(sp(d$economic_pct,  d$public_realm_area_sqm))

# ===== 反差例子(Chng):话术高但 outcome 低 =====
rk <- function(v){ r <- rank(v, na.last="keep"); r/max(r, na.rm=TRUE) }
cat("\n=== 反差候选:经济话术强 但 商业面积低(说经济不给经济) ===\n")
d %>% mutate(econ_rank=rk(economic_pct), comm_rank=rk(commercial_floorspace_sqm),
             gap=econ_rank - comm_rank) %>% filter(!is.na(gap)) %>% arrange(desc(gap)) %>%
  select(lpa_number, economic_pct, commercial_floorspace_sqm, econ_rank, comm_rank) %>% head(4) %>% print()

cat("\n=== 反差候选:社会话术强 但 affordable% 低(说社会不给社会) ===\n")
d %>% mutate(soc_rank=rk(social_pct), aff_rank=rk(affordable_percentage),
             gap=soc_rank - aff_rank) %>% filter(!is.na(gap)) %>% arrange(desc(gap)) %>%
  select(lpa_number, social_pct, affordable_percentage, soc_rank, aff_rank) %>% head(4) %>% print()

# ===== 接后面:简版 narrative 频率 × 拟议 outcome —— 对齐类型学 =====
library(tidyverse); library(here)
d <- read_csv(here("output","corpus_59_full_spatial_framed.csv"), show_col_types=FALSE) %>%
  filter(!low_n_flag)
nm <- names(d)

find1 <- function(nm, exact, loose){
  if (exact %in% nm) return(exact)
  hit <- nm[str_detect(nm, regex(loose, ignore_case=TRUE)) &
              !str_detect(nm, regex("pct|_pred|_my|outcome|sqm|percentage|units|count|storey|metre|dph|target|basis|list",
                                    ignore_case=TRUE))]
  if (length(hit)>=1) hit[1] else NA_character_
}

pairs <- tribble(
  ~label,                 ~nar_exact,      ~nar_loose,     ~out_col,                      ~kind,
  "high_density→storeys", "high_density",  "high.?dens",   "building_height_max_storeys", "num",
  "affordable→aff%",      "affordable",    "affordab",     "affordable_percentage",       "aff",
  "public_realm→area",    "public_realm",  "public.?realm","public_realm_area_sqm",       "num",
  "mixed_use→commercial", "mixed_use",     "mixed.?use",   "commercial_floorspace_sqm",   "num",
  "community→facilities", "community",     "communit",     "community_facilities_count",  "num",
  "sustainability→BREEAM","sustainability","sustainab",    "breeam_rating_target",        "breeam"
)

sp <- function(x,y){ ok<-!is.na(x)&!is.na(y); n<-sum(ok); if(n<10) return(list(rho=NA,p=NA,n=n))
t<-suppressWarnings(cor.test(x[ok],y[ok],method="spearman")); list(rho=round(unname(t$estimate),3),p=round(t$p.value,3),n=n) }

res <- pmap_dfr(pairs, function(label,nar_exact,nar_loose,out_col,kind){
  ncol <- find1(nm, nar_exact, nar_loose)
  if (is.na(ncol) || !(out_col %in% nm))
    return(tibble(pairing=label, nar_col=ncol, rho=NA, p=NA, n=NA))
  x <- suppressWarnings(as.numeric(d[[ncol]])); y <- d[[out_col]]
  if (kind=="aff"){ y<-suppressWarnings(as.numeric(y)); y<-ifelse(y>0,y,NA) }
  else if (kind=="breeam"){ y<-str_to_lower(as.character(y))
  y<-case_when(str_detect(y,"outstanding")~4, str_detect(y,"excellent")~3,
               str_detect(y,"very good")~2, str_detect(y,"good")~1, TRUE~NA_real_) }
  else y<-suppressWarnings(as.numeric(y))
  s<-sp(x,y); tibble(pairing=label, nar_col=ncol, rho=s$rho, p=s$p, n=s$n)
}) %>% mutate(类型 = case_when(
  is.na(rho) ~ "测不了(列缺失)",
  rho>=0.30 & p<0.05 ~ "clear alignment ✅",
  abs(rho)<0.15 ~ "no detectable alignment",
  TRUE ~ "weak / suggestive"))

cat("=== narrative 频率 × 拟议 outcome (Spearman) ===\n"); print(res, width=Inf)
cat("\n注:频率是原始计数(可能受文书长度混淆);BREEAM 二级序数、n 少;\n")
cat("high_density→storeys 你已有 GWR(全局显著、R² .17→.42、空间异质),这里全局 rho 只作呼应。\n")

# 缺列就顺手打印关键词矩阵结构,省一轮往返
if (any(res$类型=="测不了(列缺失)")) {
  cat("\n有 narrative 列缺失。关键词矩阵结构(下轮用):\n")
  km <- tryCatch(suppressMessages(read_csv(here("output","llm_keyword_matrix_corpus.csv"))), error=function(e) NULL)
  if (!is.null(km)) { print(names(km)); cat("行数:", nrow(km), "\n") } else cat("(没找到该文件)\n")
}

# ===== 接后面:scale-driven —— narrative 频率 × 项目规模 =====
library(tidyverse); library(here)
d <- read_csv(here("output","corpus_59_full_spatial_framed.csv"), show_col_types=FALSE)
nm <- names(d)

scale_pick <- c(
  dwellings = if ("dwellings_proposed_total" %in% nm) "dwellings_proposed_total" else NA_character_,
  storeys   = if ("building_height_max_storeys" %in% nm) "building_height_max_storeys" else NA_character_,
  site_area = nm[str_detect(nm, regex("site_area|site.?ha|hectare", ignore_case=TRUE))][1]
)
scale_pick <- scale_pick[!is.na(scale_pick)]
cat("规模指标列:", paste(scale_pick, collapse=" | "), "\n")

nar_pat  <- "regenerat|redevelop|sustainab|public.?realm|communit|mixed.?use|affordab|high.?dens|connectiv|placemak|heritage|displac"
excl_pat <- "pct|_pred|_my|outcome|sqm|percentage|units|count|storey|metre|dph|target|basis|list|area|imd|ptal|dist_|canopy|n_codable"
nar_cols <- nm[str_detect(nm, regex(nar_pat, ignore_case=TRUE)) &
                 !str_detect(nm, regex(excl_pat, ignore_case=TRUE))]
nar_cols <- nar_cols[map_lgl(nar_cols, ~ is.numeric(d[[.x]]))]
cat("认到的 narrative 频率列(核对):\n"); print(nar_cols)

if (length(nar_cols) >= 3 && length(scale_pick) >= 1) {
  sp <- function(x,y){ ok<-!is.na(x)&!is.na(y); if(sum(ok)<10) return(NA_real_)
  round(unname(suppressWarnings(cor.test(x[ok],y[ok],method="spearman"))$estimate),2) }
  res <- map_dfr(nar_cols, function(nc){
    r <- tibble(narrative=nc)
    for (s in names(scale_pick)) r[[s]] <- sp(as.numeric(d[[nc]]), as.numeric(d[[scale_pick[s]]]))
    r
  })
  cat("\n=== narrative 频率 × 项目规模 (Spearman rho) ===\n"); print(res, width=Inf)
  res2 <- res %>% rowwise() %>%
    mutate(mean_abs = mean(abs(c_across(where(is.numeric))), na.rm=TRUE)) %>%
    ungroup() %>% arrange(desc(mean_abs))
  cat("\n最'随规模'的 narrative(scale-driven 候选):\n"); print(res2 %>% select(narrative, mean_abs), n=Inf)
} else {
  cat("\n主表 narrative 频率列不够。keyword matrix 结构(下轮用):\n")
  km <- tryCatch(suppressMessages(read_csv(here("output","llm_keyword_matrix_corpus.csv"))), error=function(e) NULL)
  if (!is.null(km)) { print(names(km)); cat("行数:", nrow(km), "\n") } else cat("(没找到 keyword matrix)\n")
}
cat("\n注:narrative 频率是原始计数,和规模一样可能受文书长度混淆;这条和长度标准化检查一起看。\n")

# ===== 长度标准化 robustness check（5.5 + scale-driven 共用）=====
library(tidyverse); library(here)
d <- read_csv(here("output","corpus_59_full_spatial_framed.csv"), show_col_types=FALSE)
nm <- names(d)

wc_col <- nm[str_detect(nm, regex("word_?count|n_words|doc.?length|total_words|token", ignore_case=TRUE))][1]
if (!is.na(wc_col)) { d$.words <- suppressWarnings(as.numeric(d[[wc_col]])); cat("用现成字数列:", wc_col, "\n")
} else {
  cat("从 pdf_extracted_text_corpus.rds 算字数...\n")
  txt <- readRDS(here("output","pdf_extracted_text_corpus.rds"))
  cat("RDS class:", class(txt)[1], "\n")
  wc <- NULL
  if (is.data.frame(txt)) {
    cat("列名:", paste(names(txt), collapse=", "), "\n")
    lc <- names(txt)[str_detect(names(txt), regex("lpa|pa_?ref|project|^id$", ignore_case=TRUE))][1]
    tc <- names(txt)[str_detect(names(txt), regex("text|content|full|extract", ignore_case=TRUE))][1]
    if (!is.na(lc) && !is.na(tc))
      wc <- tibble(lpa_number=as.character(txt[[lc]]), .words=str_count(as.character(txt[[tc]]),"\\S+"))
    else cat(">>> 认不出 lpa/text 列,把上面列名发我。\n")
  } else if (is.list(txt) && !is.null(names(txt))) {
    cat("named list, 长度", length(txt), "\n")
    wc <- tibble(lpa_number=names(txt), .words=map_dbl(txt, ~ str_count(paste(as.character(.x),collapse=" "),"\\S+")))
  } else cat(">>> RDS 结构没认出,发我 str(txt) 头几行。\n")
  if (!is.null(wc)) {
    wc <- wc %>% group_by(lpa_number) %>% summarise(.words=sum(.words,na.rm=TRUE), .groups="drop")
    d <- d %>% left_join(wc, by="lpa_number")
    cat("字数接上:", sum(!is.na(d$.words)),"/",nrow(d)," 中位字数:", median(d$.words,na.rm=TRUE),"\n")
  }
}

if (".words" %in% names(d) && any(!is.na(d$.words))) {
  d <- d %>% filter(!low_n_flag)
  per10k <- function(cnt) cnt / (d$.words/10000)
  sp <- function(x,y){ ok<-!is.na(x)&!is.na(y); if(sum(ok)<10) return(NA_real_)
  round(unname(suppressWarnings(cor.test(x[ok],y[ok],method="spearman"))$estimate),2) }
  d$aff_pct <- ifelse(suppressWarnings(as.numeric(d$affordable_percentage))>0,
                      suppressWarnings(as.numeric(d$affordable_percentage)), NA)
  pr <- tribble(~pairing,~nar,~out,
                "high_density→storeys","high_density","building_height_max_storeys",
                "public_realm→area","public_realm","public_realm_area_sqm",
                "mixed_use→commercial","mixed_use","commercial_floorspace_sqm",
                "affordable→aff%","affordable","aff_pct",
                "community→facilities","community","community_facilities_count",
                "sustainability→BREEAM","sustainability","breeam_rating_target")
  cmp <- pmap_dfr(pr, function(pairing,nar,out){
    y <- if (out=="breeam_rating_target"){ z<-str_to_lower(as.character(d[[out]]))
    case_when(str_detect(z,"outstanding")~4,str_detect(z,"excellent")~3,str_detect(z,"very good")~2,str_detect(z,"good")~1,TRUE~NA_real_)
    } else suppressWarnings(as.numeric(d[[out]]))
    tibble(pairing, raw=sp(suppressWarnings(as.numeric(d[[nar]])),y),
           norm=sp(per10k(suppressWarnings(as.numeric(d[[nar]]))),y)) %>% mutate(delta=round(norm-raw,2))
  })
  cat("\n=== 5.5 对齐配对:raw vs 每万词标准化 ===\n"); print(cmp, width=Inf)
  
  scl <- map_dfr(c("public_realm","placemaking","community","connectivity","mixed_use","high_density"), function(nc){
    tibble(narrative=nc,
           raw =sp(suppressWarnings(as.numeric(d[[nc]])), suppressWarnings(as.numeric(d$dwellings_proposed_total))),
           norm=sp(per10k(suppressWarnings(as.numeric(d[[nc]]))), suppressWarnings(as.numeric(d$dwellings_proposed_total)))) %>%
      mutate(delta=round(norm-raw,2)) })
  cat("\n=== scale-driven (× 户数):raw vs 标准化 ===\n"); print(scl, width=Inf)
  cat("\n读法:norm 掉到接近 0 = 原来是长度假象;norm 还在(方向+大致强度) = 真信号。\n")
  cat("注:这只控长度,没控 scheme scale;后者是更进一步、今天不做。\n")
} else cat("\n>>> 没拿到字数。把 RDS 的 class/列名发我,我改一版。\n")

# ===== 甲:规模偏相关 —— 控住项目规模后,5.5 六配对 + compensation 还剩什么 =====
library(tidyverse); library(here)
d <- read_csv(here("output","corpus_59_full_spatial_framed.csv"), show_col_types=FALSE) %>%
  filter(!low_n_flag)

num <- function(v) suppressWarnings(as.numeric(d[[v]]))

# 手算 partial Spearman:各变量 rank 化 -> 对控制变量回归取残差 -> 残差相关
pcor_sp <- function(x, y, z) {
  ok <- complete.cases(x, y, z)
  if (sum(ok) < 12) return(list(partial=NA_real_, n=sum(ok)))
  rx<-rank(x[ok]); ry<-rank(y[ok]); rz<-rank(z[ok])
  ex<-residuals(lm(rx~rz)); ey<-residuals(lm(ry~rz))
  list(partial=round(unname(cor(ex,ey)),2), n=sum(ok))
}
raw_sp <- function(x,y){ ok<-complete.cases(x,y); if(sum(ok)<10) return(NA_real_)
round(unname(suppressWarnings(cor.test(x[ok],y[ok],method="spearman"))$estimate),2) }

size   <- num("dwellings_proposed_total")          # 规模控制变量
d$aff_pct <- ifelse(num("affordable_percentage")>0, num("affordable_percentage"), NA)
breeam <- { z<-str_to_lower(as.character(d$breeam_rating_target))
case_when(str_detect(z,"outstanding")~4,str_detect(z,"excellent")~3,
          str_detect(z,"very good")~2,str_detect(z,"good")~1,TRUE~NA_real_) }

pairs <- list(
  list("high_density→storeys","high_density",  num("building_height_max_storeys")),
  list("public_realm→area","public_realm",      num("public_realm_area_sqm")),
  list("mixed_use→commercial","mixed_use",       num("commercial_floorspace_sqm")),
  list("affordable→aff%","affordable",           d$aff_pct),
  list("community→facilities","community",       num("community_facilities_count")),
  list("sustainability→BREEAM","sustainability", breeam)
)
res <- map_dfr(pairs, function(p){
  x<-num(p[[2]]); out<-p[[3]]; r<-raw_sp(x,out); pc<-pcor_sp(x,out,size)
  tibble(pairing=p[[1]], raw=r, partial_ctrl_size=pc$partial, n=pc$n, delta=round(pc$partial-r,2))
})
cat("=== 5.5 六配对:raw ρ vs 控住规模后的偏相关 ===\n"); print(res, width=Inf)

# ---- compensation 顺带查:public_realm 频率 × 场地,控规模前后 + 顺便看清 IMD 方向 ----
prc <- num("public_realm")
comp <- map_dfr(c("imd_score","imd_decile","canopy_area_kmsq","PTAL_numeric","dist_to_station_m"), function(cc){
  if(!(cc %in% names(d))) return(tibble(indicator=cc, raw=NA_real_, partial_ctrl_size=NA_real_, n=NA_integer_))
  y<-num(cc); pc<-pcor_sp(prc,y,size)
  tibble(indicator=cc, raw=raw_sp(prc,y), partial_ctrl_size=pc$partial, n=pc$n)
})
cat("\n=== compensation:public_realm 频率 × 场地,控规模前后 ===\n"); print(comp, width=Inf)

cat("\n读法:partial 还在(方向+大致强度)= 规模之外真有信号,该配对/发现活;\n")
cat("      partial 掉到 ~0 = 那关系主要是项目规模造的。\n")
cat("控的是户数;想再稳可换 site area 复核,先看这个。\n")