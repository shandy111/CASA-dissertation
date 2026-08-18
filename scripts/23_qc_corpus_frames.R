# 23_qc_corpus_frames.R —— 只读检查,不改文件,不调 API
library(tidyverse); library(here)
d <- read_csv(here("output","corpus_59_v5_frame_predictions.csv"), show_col_types=FALSE)

cat("总行数:", nrow(d), "| 去重后:", n_distinct(d$.k), "(应相等)\n")
cat("覆盖项目数:", n_distinct(d$lpa_number),
    "| narrative:", paste(sort(unique(d$narrative)),collapse=", "), "\n")

pred_cols <- names(d)[str_starts(names(d),"pred_")]
cat("\n各 pred 列的 NA 数(应都接近 0):\n"); print(map_int(d[pred_cols], ~sum(is.na(.))))

cat("\ncodable 率:", round(mean(d$pred_codable==TRUE, na.rm=TRUE),3),
    "| codable 句数:", sum(d$pred_codable==TRUE, na.rm=TRUE), "\n")
cat("各 narrative 的 codable:\n")
print(d %>% group_by(narrative) %>% summarise(n=n(),
                                              codable=sum(pred_codable==TRUE,na.rm=TRUE),
                                              rate=round(mean(pred_codable==TRUE,na.rm=TRUE),2)))

cod <- d %>% filter(pred_codable==TRUE)
cat("\ncodable 句里各 frame 出现率:\n")
print(tibble(frame=c("economic","social","environmental","design_heritage"),
             rate=c(mean(cod$pred_frame_economic==TRUE,na.rm=TRUE),
                    mean(cod$pred_frame_social==TRUE,na.rm=TRUE),
                    mean(cod$pred_frame_environmental==TRUE,na.rm=TRUE),
                    mean(cod$pred_frame_design_heritage==TRUE,na.rm=TRUE)) %>% round(3)))

per_proj <- cod %>% count(lpa_number, name="codable_n")
cat("\n每个项目的 codable 句数分布:\n"); print(summary(per_proj$codable_n))
cat("codable 句 < 5 的项目(比例会不稳,写时标注):\n")
print(per_proj %>% filter(codable_n < 5) %>% arrange(codable_n))

cat("\n项目×narrative 句数分布:\n")
print(summary((cod %>% count(lpa_number, narrative))$n))