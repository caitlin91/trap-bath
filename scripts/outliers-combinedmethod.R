#Functions ----------------------------
#Z sc ore
isnt_out_z <- function(x, thres = 3, na.rm = TRUE) {
  abs(x - mean(x, na.rm = na.rm)) <= thres * sd(x, na.rm = na.rm)
}
#Z score with MAD
isnt_out_mad <- function(x, thres = 3, na.rm = TRUE) {
  abs(x - median(x, na.rm = na.rm)) <= thres * mad(x, na.rm = na.rm)
}
#Tukeys fences
isnt_out_tukey <- function(x, k = 1.5, na.rm = TRUE) {
  quar <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
  iqr <- diff(quar)
  
  (quar[1] - k * iqr <= x) & (x <= quar[2] + k * iqr)
}
#Mahlanobis distance
maha_dist <- . %>% select_if(is.numeric) %>%
  mahalanobis(center = colMeans(.), cov = cov(.))
isnt_out_maha <- function(tbl, isnt_out_f, ...) {
  tbl %>% maha_dist() %>% isnt_out_f(...)
}
#all functions
isnt_out_funs <- list(
  z = isnt_out_z,
  mad = isnt_out_mad,
  tukey = isnt_out_tukey
)
data_small = data_all %>% select(rowNumber_all_fac,lexicalSet_broad,norm_F1,norm_F2)
data_tbl = data_small %>%
  unite(col = "group", lexicalSet_broad)

#compute non outliers
compute_group_non_outliers <- . %>%
  # Compute per group mean values of columns
  group_by(group) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  # Detect outliers among groups
  mutate_if(is.numeric, isnt_out_funs) %>%
  # Remove unnecessary columns
  select_if(Negate(is.numeric))
data_tbl %>% compute_group_non_outliers()

row_packs_isnt_out <- row_packs(
  # Non-outliers based on some column
  column = . %>% transmute_if(is.numeric, isnt_out_funs),
  # Non-outliers based on Mahalanobis distance
  maha = . %>% transmute(maha = maha_dist(.)) %>%
    transmute_at(vars(maha = maha), isnt_out_funs)
)

group_packs_isnt_out <- group_packs(
  # Non-outliers based on grouping
  group = compute_group_non_outliers,
  .group_vars = "group"
)

# Don't remove obeyers to compute total number of applied rules
full_report <- data_tbl %>%
  expose(row_packs_isnt_out, group_packs_isnt_out,
         .remove_obeyers = FALSE) %>%
  get_report()

used_rules <- full_report %>%
  distinct(pack, rule)

breaker_report <- full_report %>%
  filter(!(value %in% TRUE))


group_breakers <- breaker_report %>%
  # Filter group packs
  filter(pack == "group") %>%
  # Expand rows by matching group with its rows
  select(-id) %>%
  left_join(
    y = data_tbl %>% transmute(var = group, id = 1:n()),
    by = "var"
  ) %>%
  select(pack, rule, var, id, value)

outliers <- bind_rows(
  breaker_report %>% filter(pack != "group"),
  group_breakers
) %>%
  select(pack, rule, id)

# Not all group based definitions resulted with outliers
outliers %>%
  count(pack, rule) %>%
  filter(pack == "group") %>%
  print(n = Inf)
outliers %>%
  count(pack, rule, sort = TRUE)

outlier_score <- outliers %>%
  group_by(id) %>%
  # nrow(used_rules) equals total number of applied methods
  summarise(score = n() / nrow(used_rules))
# Top 10 outliers
outlier_score %>% arrange(desc(score)) %>% slice(1:10)
outlier_score = outlier_score %>% mutate(id_char = as.character(id))
# ---------------
data_small = data_small %>% mutate(rowNumber_all_char = as.character(rowNumber_all_fac))
data_tbl <- data_small %>%
  left_join(y = outlier_score, by = c("rowNumber_all_char"="id_char")) %>%
  mutate(
    score = coalesce(score, 0),
    is_out = if_else(score > 0.1, "Outlier", "NotOutlier")
  ) %>%
  select(rowNumber_all_fac,lexicalSet_broad,norm_F1,norm_F2,score,is_out)
table(data_tbl$is_out)

