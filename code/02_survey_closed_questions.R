library(ggplot2)
library(gridExtra)
library(dplyr)
library(forcats)

load("data/survey/responses_experts.RData")      # experts, experts_questions
load("data/survey/responses_journalists.RData")  # journalists, journalists_questions

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
group_cols = viridis::viridis(n = 9)[c(2, 5)]

# question blocks (column 1 = respondent id)
block = function(df, qs) data.frame(case = df$Q1, df[, paste0("Q", qs)])
drivers_now_e = block(experts,     30:39); drivers_now_j = block(journalists, 43:52)
drivers_fut_e = block(experts,     41:50); drivers_fut_j = block(journalists, 54:63)
fut12_e       = block(experts,     10:19); fut12_j       = block(journalists, 23:32)
fut60_e       = block(experts,     20:29); fut60_j       = block(journalists, 33:42)

# keep complete answers (blank = 0, probabilities must sum to 100)
complete_answers = function(d) {
  d = d[!apply(d[, -1], 1, function(row) all(is.na(row))), ]
  d[is.na(d)] = 0
  d[rowSums(d[, -1]) == 100, ]
}

# helper function
calculate_stats = function(data, group_name, labels) {
  n = nrow(data)
  means = colMeans(data[, -1])
  sds = apply(data[, -1], 2, sd)
  ses = sds / sqrt(n)
  stats = data.frame(group = group_name, label = labels, n = n, len = means, sd = sds, se = ses,
                     ci_lower = means - 1.96 * ses, ci_upper = means + 1.96 * ses)
  stats$rank = match(stats$len, sort(stats$len, decreasing = TRUE))
  stats
}



# -----------------------------------------------------------------------------
# Figure 2 + Table 3: current inflation drivers
# -----------------------------------------------------------------------------

# perceived current inflation drivers according to Js and Es
drivers_now_j = complete_answers(drivers_now_j)
drivers_now_e = complete_answers(drivers_now_e)

lbls = c("labour shortages", "supply chain issues", "fiscal policy", "energy crisis", "money supply",
         "pent-up demand", "interest rates too low", "expectations", "profiteering", "rest")

# reshape data for plot
drivers_plot = rbind(calculate_stats(drivers_now_j, "journalists", lbls),
                     calculate_stats(drivers_now_e, "experts", lbls))
cols = c("len", "sd", "se", "ci_lower", "ci_upper")
drivers_plot[cols] = drivers_plot[cols] / 100

# plot
ggplot(drivers_plot, aes(x = label, y = len, fill = group)) +
  geom_bar(alpha = 0.8, stat = "identity", position = "dodge", show.legend = TRUE) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = .2, position = position_dodge(.9)) +
  geom_text(aes(label = sprintf("%d", rank), group = group, vjust = -0.5, hjust = 1.4, color = group),
            position = position_dodge(width = 0.9), size = 4, alpha = 0.8) +
  labs(x = "", y = "Expected probability") +
  scale_x_discrete(limits = lbls) +
  scale_fill_manual(values = group_cols) +
  scale_color_manual(values = group_cols) +
  my_theme +
  theme(legend.position = c(0.9, 0.8), axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("output/figures/fig02_current_drivers.jpg", width = 8, height = 4)



# Table 3: two-sided Welch t-tests, journalists vs. experts
table3 = data.frame(
  item = lbls,
  p_value = sapply(2:11, function(i) t.test(drivers_now_e[, i], drivers_now_j[, i])$p.value)
)
table3$sig = ifelse(table3$p_value <= 0.01, "***", "")

print(transform(table3, p_value = round(p_value, 2)), row.names = FALSE)
write.csv(table3, "output/tables/table3_ttests_drivers.csv", row.names = FALSE)



# -----------------------------------------------------------------------------
# Figure 3: expected persistence of inflation
# -----------------------------------------------------------------------------
# A1 = temporary (below 3% within five years), A2 = persistent

# build long data
persist = rbind(
  data.frame(group = "experts",     answer = experts$Q8[experts$Q1 %in% drivers_now_e$case]),
  data.frame(group = "journalists", answer = journalists$Q21[journalists$Q1 %in% drivers_now_j$case])
)
persist = persist[!is.na(persist$answer), ]

# reshape data for plot
persist_plot = persist %>%
  count(group, answer) %>%
  group_by(group) %>%
  mutate(value = n / sum(n), cat = ifelse(answer == "A1", "temporary", "persistent"))
print(persist_plot)

# plot
ggplot(persist_plot) +
  geom_bar(aes(x = cat, y = value, fill = group), alpha = 0.8, position = "dodge", stat = "identity") +
  labs(x = NULL, y = "percentage", fill = "group") +
  scale_fill_manual(values = group_cols) +
  my_theme +
  theme(legend.position = "right") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))
ggsave("output/figures/fig03_inflation_persistence.jpg", dpi = 300, width = 8, height = 4)


# Persistence: Pearson's chi-squared test
print(chisq.test(persist$answer, persist$group))



# -----------------------------------------------------------------------------
# Figure 4: expected inflation in 12 and 60 months (+ Mann-Whitney U)
# -----------------------------------------------------------------------------

# define x labs
lbls_fut = c("< -12", "-12 to -8", "-8 to -4", "-4 to -2", "-2 to 0", "0 to 2", "2 to 4", "4 to 8", "8 to 12", "> 12")
fut12_j = complete_answers(fut12_j); fut12_e = complete_answers(fut12_e)
fut60_j = complete_answers(fut60_j); fut60_e = complete_answers(fut60_e)

# helper function
outlook_plot = function(d_j, d_e, title, xlab) {
  p = rbind(calculate_stats(d_j, "journalists", rev(lbls_fut)),
            calculate_stats(d_e, "experts", rev(lbls_fut)))
  p[cols] = p[cols] / 100
  ggplot(p, aes(x = label, y = len, fill = group)) +
    geom_bar(alpha = 0.8, stat = "identity", position = "dodge", show.legend = TRUE) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = .2, position = position_dodge(.9)) +
    labs(x = xlab, y = "exp. probability", title = title) +
    scale_x_discrete(limits = lbls_fut) +
    scale_fill_manual(values = group_cols) +
    my_theme +
    theme(legend.position = "right")
}
grid = arrangeGrob(outlook_plot(fut12_j, fut12_e, "12-month forcase", ""),
                    outlook_plot(fut60_j, fut60_e, "60-month forcase", "expected inflation rate"),
                    nrow = 2)
ggsave("output/figures/fig04_outlook_12_60.jpg", plot = grid, width = 8, height = 6)

# expected inflation rate per respondent = probability-weighted bin midpoints
bin_midpoints = c(14, 10, 6, 3, 1, -1, -3, -6, -10, -14)   # bins from "> 12" to "< -12"
expected_inf = function(d) apply(d[, -1], 1, function(p) sum(p / 100 * bin_midpoints))


# Figure 4: expected inflation rate, journalists vs. experts
for (h in c("12", "60")) {
  e_j = expected_inf(get(paste0("fut", h, "_j"))); e_e = expected_inf(get(paste0("fut", h, "_e")))
  mwu = wilcox.test(e_j, e_e, exact = FALSE)
  cat(sprintf("%s months: journalists M = %.2f%% (N = %d), experts M = %.2f%% (N = %d), U = %.0f, p = %.4f\n",
              h, mean(e_j), length(e_j), mean(e_e), length(e_e), mwu$statistic, mwu$p.value))
}



# -----------------------------------------------------------------------------
# Figure F: which drivers will be relevant in the next 12 months?
# -----------------------------------------------------------------------------
# A1 = will be relevant, A2 = will not be relevant

future_table = function(d, group_name) {
  yes = colSums(d[, -1] == "A1", na.rm = TRUE)
  no  = colSums(d[, -1] == "A2", na.rm = TRUE)
  data.frame(label = lbls, yes_rel = yes / (yes + no), no_rel = no / (yes + no),
             diff = yes - no, group = group_name, row.names = NULL)
}
fut_plot = rbind(future_table(drivers_fut_j, "journalists"), future_table(drivers_fut_e, "experts"))
fut_plot = fut_plot[fut_plot$label != "rest", ]

custom_labels = function(x) ifelse(x < 0, "will not be\nrelevant", ifelse(x > 0, "will be\nrelevant", ""))

# plot
ggplot(fut_plot, aes(x = label)) +
  geom_bar(aes(y = yes_rel, fill = group), position = "dodge", stat = "identity", alpha = 0.2) +
  geom_bar(aes(y = -no_rel, fill = group), position = "dodge", stat = "identity", alpha = 0.2) +
  geom_bar(aes(y = yes_rel - no_rel, fill = group), position = "dodge", stat = "identity", alpha = 0.8) +
  geom_text(aes(y = yes_rel - no_rel, label = abs(diff), group = group,
                vjust = ifelse(diff >= 0, -0.5, 1.2)),
            position = position_dodge(width = 0.9), size = 3, alpha = 0.5) +
  labs(y = "share of indications", x = "") +
  scale_y_continuous(labels = abs, sec.axis = sec_axis(~., labels = custom_labels, breaks = c(-0.5, 0.5))) +
  scale_fill_manual(values = group_cols) +
  my_theme +
  theme(axis.text.y.right = element_text(angle = 90, hjust = 0.5), legend.position = "right")
ggsave("output/figures/figF_future_impact.jpg", width = 8, height = 5)



# -----------------------------------------------------------------------------
# Figure G: information sources used by journalists (Q65-Q79, multiple choice)
# -----------------------------------------------------------------------------

source_names = c("News Apps", "Social Media", "Online News", "General Interest\nMedia", "Special Interest\nMedia",
                 "podcasts", "(YouTube)\nVideos", "Documentations", "Newsletters", "International\nPress",
                 "Primary Sources", "TV News", "Radio", "News Agencies", "rest")
information_df = data.frame(source = source_names,
                            val = colSums(na.omit(journalists[, paste0("Q", 65:79)])))

# plot
ggplot(information_df, aes(x = reorder(source, val, decreasing = TRUE), y = val)) +
  geom_col(fill = viridis::viridis(n = 9)[5], alpha = 0.8) +
  labs(x = "", y = "# indications") +
  my_theme
ggsave("output/figures/figG_journalists_resources.jpg", width = 8, height = 4)



# -----------------------------------------------------------------------------
# Figure H: inverse weighted average rank of information sources (Q80-Q91)
# -----------------------------------------------------------------------------

dict = c("SQ001" = "News Apps", "SQ002" = "Social Media", "SQ003" = "Online News", "SQ004" = "General Interest",
         "SQ005" = "Podcasts", "SQ006" = "(YouTube) Videos", "SQ007" = "Documentations", "SQ008" = "Newsletters",
         "SQ009" = "Primary Sources", "SQ010" = "TV News", "SQ011" = "rest", "SQ012" = "Radio",
         "SQ013" = "Special Interest Media", "SQ014" = "International Press", "SQ015" = "News Agencies")

ranking = journalists[, paste0("Q", 80:91)]   # column k = source placed on rank k
ranks = 1:12
ranking_summary = sapply(names(dict), function(s) colSums(ranking == s, na.rm = TRUE))

# reshape data for plot
avg_ranks_df = data.frame(
  Item = dict,
  AvgRank = 12 - colSums(ranks * ranking_summary) / colSums(ranking_summary)
)

# plot
ggplot(avg_ranks_df, aes(x = AvgRank, y = reorder(Item, AvgRank, decreasing = FALSE))) +
  geom_bar(stat = "identity", fill = viridis::viridis(n = 9)[5], alpha = 0.8) +
  labs(y = "", x = "Inv. Average Rank") +
  my_theme +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5))
ggsave("output/figures/figH_journalists_resources_rank.jpg", width = 8, height = 5)



# -----------------------------------------------------------------------------
# Figure A: formal economic education of journalists (Q97)
# -----------------------------------------------------------------------------

edu = table(factor(journalists$Q97[journalists$Q1 %in% drivers_now_j$case], levels = paste0("A", 1:5)))
names(edu) = c("None", "Business A Levels", "Bachelors Degree", "Masters Degree", "Graduate Level")
print(edu)

ggplot(data.frame(edu), aes(x = Var1, y = Freq)) +
  geom_col(fill = viridis::viridis(n = 9)[5], alpha = 0.8) +
  my_theme +
  labs(x = "", y = "n respondents")
ggsave("output/figures/figA_education.jpg", dpi = 300, width = 8, height = 4)



# -----------------------------------------------------------------------------
# Figure B: topics covered too much / not enough (hand-coded open answers)
# -----------------------------------------------------------------------------
# Counts of the author's manual categorisation of the two open questions on
# inflation coverage (five most frequent categories per question).

coverage = rbind(
  data.frame(group = "covered too much",
             cat = c("central bank", "energy", "wages", "war", "covid"),
             abs = c(15, 7, 6, 5, 2)),
  data.frame(group = "not covered enough",
             cat = c("central bank", "economic\ninterrelations", "social issues", "expectations", "fiscal policy"),
             abs = c(10, 6, 5, 3, 3))
)

ggplot(coverage, aes(fct_reorder(cat, -abs), abs, fill = group)) +
  geom_col(show.legend = FALSE, alpha = 0.8) +
  facet_wrap(~group, scales = "free") +
  my_theme +
  geom_text(aes(label = abs, group = group, vjust = -0.6, color = group), size = 3, show.legend = FALSE) +
  scale_fill_manual(values = viridis::viridis(n = 9)[c(4, 6)]) +
  labs(x = "", y = "") +
  ylim(0, 17)
ggsave("output/figures/figB_coverage.jpg", width = 8, height = 4, dpi = 300)
