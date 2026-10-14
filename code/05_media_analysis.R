library(ggplot2)
library(dplyr)
library(lubridate)
library(reshape2)
library(scales)

my_theme = theme(
  panel.background = element_blank(),
  panel.grid.major = element_line(color = "gray97"),
  panel.grid.minor = element_blank(),
  axis.line = element_line(color = "black"),
  axis.ticks = element_line(color = "black"),
  axis.text = element_text(size = 10, color = "black"),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  axis.title = element_text(size = 12, face = "bold"),
  plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
  strip.background = element_blank(),
  strip.text = element_text(size = 12, face = "bold"),
  legend.position = "bottom",
  plot.margin = unit(c(1, 1, 1, 1), "cm")
)

lbls = c("labour.shortages", "supply.chains", "fiscal.policy", "energy.crisis", "money.supply",
         "pent.up.demand", "interest.rates", "expectations", "profiteering", "other")
drivers9 = setdiff(lbls, "other")


# -----------------------------------------------------------------------------
# Table 2: validation of the LLM coding
# -----------------------------------------------------------------------------
# 151 human-coded articles = 118 random + 33 stratified additions (rare categories, hard negatives)

gold = read.csv("data/media/validation_gold_standard.csv")
pred = read.csv("data/media/validation_llm_predictions.csv")
gold_map = c(energy.crisis = "energy.crisis", inflation.expectations = "expectations",
             fiscal.policy.government = "fiscal.policy", interest.rates = "interest.rates",
             labour.shortages = "labour.shortages", money.supply.by.the.ecb = "money.supply",
             pent.up.demand = "pent.up.demand", profiteering = "profiteering",
             supply.chain.issues = "supply.chains", other = "other")
names(gold)[match(names(gold_map), names(gold))] = gold_map

gold_bin = gold[, gold_map]
pred_bin = as.data.frame(lapply(pred[, gold_map], function(x) as.integer(x >= 2)))

table2 = do.call(rbind, lapply(gold_map, function(l) {
  g = gold_bin[[l]]; p = pred_bin[[l]]
  tp = sum(p == 1 & g == 1); fp = sum(p == 1 & g == 0); fn = sum(p == 0 & g == 1); tn = sum(p == 0 & g == 0)
  prec = tp / (tp + fp); rec = tp / (tp + fn)
  data.frame(item = l, n_pos = sum(g), TP = tp, FP = fp, FN = fn, TN = tn,
             precision = prec, recall = rec, F1 = 2 * prec * rec / (prec + rec))
}))
print(table2, row.names = FALSE, digits = 3)

# Macro-F1 (9 substantive items)
mean(table2$F1[table2$item != "other"])

# save locally
# write.csv(table2, "output/tables/table2_validation.csv", row.names = FALSE)



# -----------------------------------------------------------------------------
# Figure 8: drivers in 2022 coverage (full corpus)
# -----------------------------------------------------------------------------

counts = read.csv("data/media/media_llm_labels.csv")
counts[, lbls] = lapply(counts[, lbls], function(col) as.integer(col >= 2))

# build sums
sums = colSums(counts[, lbls])
sums = data.frame(item = names(sums), value = sums, rel = sums / sum(sums))

# plot
ggplot(sums[drivers9, ], aes(item, value)) +
  geom_col(alpha = 0.6) +
  geom_text(aes(label = value), nudge_y = 30) +
  labs(y = "Occurences") +
  my_theme
# ggsave("output/figures/fig08_narrative_counts.jpg", dpi = 300, width = 8, height = 5)



# -----------------------------------------------------------------------------
# Robustness: human gold standard vs. LLM rankings (118 random articles)
# -----------------------------------------------------------------------------

rand = gold$id %in% readLines("data/media/validation_random_ids.txt")
gold_counts = colSums(gold_bin[rand, drivers9])
llm_counts = colSums(pred_bin[rand, drivers9])
corpus_share = sums[drivers9, "value"] / sum(sums[drivers9, "value"])

# Spearman rho, gold vs. 2nd coder on the same articles
cor(gold_counts, llm_counts, method = "spearman")

# Spearman rho, gold subset vs. LLM full corpus
cor(gold_counts, corpus_share, method = "spearman")



# -----------------------------------------------------------------------------
# Table 4: alignment of survey weights with coverage (+ bootstrap)
# -----------------------------------------------------------------------------
# survey side: respondents' probability weights on the ten items (blank = 0); all distributions are renormalised over the nine substantive drivers.

# load data
load("data/survey/responses_experts.RData")
load("data/survey/responses_journalists.RData")

weights = function(df, qs) { m = as.matrix(df[, paste0("Q", qs)]); m[is.na(m)] = 0; colnames(m) = lbls; m }
jou_resp = weights(journalists, 43:52)
exp_resp = weights(experts, 30:39)
med_arts = as.matrix(counts[, lbls])

align_stats = function(jou_m, exp_m, med_m) {
  jou_w = colMeans(jou_m)[drivers9]; jou_w = jou_w / sum(jou_w)
  exp_w = colMeans(exp_m)[drivers9]; exp_w = exp_w / sum(exp_w)
  cov_w = colSums(med_m)[drivers9];  cov_w = cov_w / sum(cov_w)
  c(rho_jou = cor(jou_w, cov_w, method = "spearman"),
    rho_exp = cor(exp_w, cov_w, method = "spearman"),
    rank_ms_jou = unname(rank(-jou_w)["money.supply"]),
    rank_ms_exp = unname(rank(-exp_w)["money.supply"]),
    rank_ms_cov = unname(rank(-cov_w)["money.supply"]))
}

set.seed(20260611)
B = 5000
est = align_stats(jou_resp, exp_resp, med_arts)
boot = t(replicate(B, align_stats(
  jou_resp[sample(nrow(jou_resp), replace = TRUE), , drop = FALSE],
  exp_resp[sample(nrow(exp_resp), replace = TRUE), , drop = FALSE],
  med_arts[sample(nrow(med_arts), replace = TRUE), , drop = FALSE])))
ci = function(x) quantile(x, c(.025, .975))

# Table 4: survey-coverage alignment, bootstrap B = 5000
table4 = data.frame(
  measure = c("Spearman rho journalists", "Spearman rho economists", "Difference (J - E)"),
  estimate = c(est["rho_jou"], est["rho_exp"], est["rho_jou"] - est["rho_exp"]),
  rbind(ci(boot[, "rho_jou"]), ci(boot[, "rho_exp"]), ci(boot[, "rho_jou"] - boot[, "rho_exp"])),
  row.names = NULL, check.names = FALSE)
table4

# save locally
# write.csv(table4, "output/tables/table4_alignment.csv", row.names = FALSE)


# print results
sprintf("Rank of money supply (1 = most frequent): coverage %d, journalists %d, economists %d",
            est["rank_ms_cov"], est["rank_ms_jou"], est["rank_ms_exp"])
sprintf("P(coverage ranks money supply above economists): %.3f",
            mean(boot[, "rank_ms_cov"] < boot[, "rank_ms_exp"]))
sprintf("P(journalists rank money supply above economists): %.3f",
            mean(boot[, "rank_ms_jou"] < boot[, "rank_ms_exp"]))



# -----------------------------------------------------------------------------
# Figures J, K and Table C: coverage over time, Ukraine invasion
# -----------------------------------------------------------------------------
# Month labels follow the system locale (the paper figures were rendered in a German locale).

war_date = as.Date("2022-02-24")
counts$date = as.Date(counts$date)
counts$month = floor_date(counts$date, "month")
counts$post_war = counts$date >= war_date
dated = counts[!is.na(counts$date), ]

monthly_n = dated %>% count(month)

p_freq = ggplot(monthly_n, aes(x = month, y = n)) +
  geom_col(fill = viridis::viridis(9)[5], alpha = 0.8) +
  geom_vline(xintercept = as.numeric(war_date), linetype = "dashed", colour = "red2", linewidth = 0.8) +
  annotate("text", x = war_date + 7, y = max(monthly_n$n) * 0.93,
           label = "Ukraine\ninvasion", hjust = 0, size = 3.2, colour = "red2") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = NULL, y = "Number of articles", title = "Monthly frequency of inflation-related articles (2022)") +
  my_theme
# ggsave("output/figures/figJ_media_monthly_frequency.jpg", plot = p_freq, dpi = 300, width = 9, height = 4)

monthly_key = dated %>%
  group_by(month) %>%
  summarise(`Energy crisis` = mean(energy.crisis), `Money supply / ECB` = mean(money.supply), .groups = "drop") %>%
  melt(id.vars = "month", variable.name = "narrative", value.name = "share")

p_trends = ggplot(monthly_key, aes(x = month, y = share, colour = narrative)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = as.numeric(war_date), linetype = "dashed", colour = "grey40", linewidth = 0.8) +
  annotate("text", x = war_date + 7, y = max(monthly_key$share) * 0.97,
           label = "Feb 24", hjust = 0, size = 3.2, colour = "grey40") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_colour_manual(values = viridis::viridis(9)[c(2, 5)]) +
  labs(x = NULL, y = "Share of articles", colour = NULL,
       title = "Narrative prevalence: energy crisis vs. money supply") +
  my_theme +
  theme(legend.position = "right")
# ggsave("output/figures/figK_media_temporal_trends.jpg", plot = p_trends, dpi = 300, width = 9, height = 4)

tableC = do.call(rbind, lapply(drivers9, function(col) {
  pre = mean(dated[[col]][!dated$post_war]) * 100
  post = mean(dated[[col]][dated$post_war]) * 100
  p = suppressWarnings(chisq.test(table(dated$post_war, dated[[col]])))$p.value
  data.frame(item = col, pre_pct = pre, post_pct = post, delta_pp = post - pre, p_chisq = p)
})); tableC

# Table C: N articles before vs. after Feb 24 2022
sum(!dated$post_war); sum(dated$post_war)

# save locally
# write.csv(tableC, "output/tables/tableC_pre_post_invasion.csv", row.names = FALSE)
