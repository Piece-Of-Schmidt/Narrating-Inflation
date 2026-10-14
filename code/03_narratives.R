library(quanteda)
library(dplyr)
library(ggplot2)
library(proxy)
library(umap)
library(reshape2)
library(tidytext)
library(ggh4x)
library(NbClust)
library(fpc)

load("data/survey/responses_experts.RData")      # experts
load("data/survey/responses_journalists.RData")  # journalists
topic = read.csv("data/survey/journalists_topic_specialisation.csv")

my_theme = theme(
  panel.background = element_blank(),
  panel.grid.major = element_line(color = "gray90"),
  panel.grid.minor = element_blank(),
  axis.line = element_line(color = "black"),
  axis.ticks = element_line(color = "white"),
  axis.text = element_text(size = 10, color = "black"),
  axis.title = element_text(size = 12, face = "bold"),
  plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
  strip.background = element_blank(),
  strip.text = element_text(size = 12, face = "bold"),
  legend.position = "bottom",
  plot.margin = unit(c(1, 1, 1, 1), "cm")
)

# helper funcs
cos_sim = function(a, b) sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))
num = function(x) { v = suppressWarnings(as.numeric(gsub(",", ".", x))); v[is.na(v)] = 0; v }


# -----------------------------------------------------------------------------
# Narrative length (open question: experts Q7, journalists Q20)
# -----------------------------------------------------------------------------

n_words = function(txt) lengths(tokens(txt, remove_punct = TRUE, remove_symbols = TRUE,
                                       remove_numbers = TRUE, remove_url = TRUE, split_hyphens = TRUE))

# Narrative length (words)
print(rbind(experts = summary(n_words(experts$Q7)), journalists = summary(n_words(journalists$Q20))))



# -----------------------------------------------------------------------------
# Bag-of-items representation
# -----------------------------------------------------------------------------

nar = read.csv("data/narratives/narratives_itemized.csv")
nar$case_id = paste0(nar$group, nar$case)


# distinct drivers per narrative (all coded categories)
print(nar %>% distinct(group, case, category) %>% count(group, case) %>%
        group_by(group) %>% summarise(mean = mean(n), median = median(n)))

# keep categories mentioned by at least two respondents within a group
nar = nar %>%
  group_by(group, category) %>%
  filter(n_distinct(case_id) >= 2) %>%
  ungroup()

X = table(nar$case_id, nar$category)   # respondent x item (counts)
class(X) = "matrix"
head(X)

# convert to DFM and assign colnames
dfm = as.dfm(X)
colnames(dfm)[c(5, 6, 7, 9, 13, 18)] = c("monetary policy", "energy crisis", "exchange rates",
                                         "labour shortages", "sanctions against russia", "war in ukraine")
groups = sub("\\d+", "", rownames(dfm))

# Respondents labelled by their sequence number in the raw survey export, in the
# order used for Figure 5 and Table B (UMAP and complete linkage depend on row order).
seq_no = c(setNames(paste0("expert", c(1:15, 19:22, 24:36)), paste0("expert", experts$Q1)),
           setNames(paste0("journalist", c(1:10, 12:15, 17, 19:21, 23:27, 29:36, 38:40, 42:50, 52:56, 58, 60,
                                           62:65, 67, 70:78, 80:82)), paste0("journalist", journalists$Q1)))
X_seq = X
rownames(X_seq) = seq_no[rownames(X)]
X_seq = X_seq[sort(rownames(X_seq)), ]



# -----------------------------------------------------------------------------
# Figure D: share of respondents mentioning each item
# -----------------------------------------------------------------------------

dfm_grp = dfm_group(dfm, groups)
plot_df = t(quanteda::convert(dfm_grp, "data.frame")[, -1])
colnames(plot_df) = c("expert", "journalist")
plot_df = melt(sweep(plot_df, 2, table(groups), "/"))

ggplot(plot_df, aes(x = reorder(Var1, -value), y = value, fill = Var2)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.8) +
  labs(x = "", y = "share", fill = "group") +
  my_theme +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), legend.position = c(0.9, 0.8)) +
  ylim(c(0, 1)) +
  scale_fill_manual(values = viridis::viridis(n = 9)[c(2, 5)])
# ggsave("output/figures/figD_item_popularities.jpg", width = 8, height = 5)



# -----------------------------------------------------------------------------
# Figure E: fightin' words (Monroe et al. 2008), experts vs. journalists
# -----------------------------------------------------------------------------

# helper func
get_fw = function(d, smooth = 1) {
  p = (d + smooth) / rowSums(d + smooth)
  log_odds = log(p[1, ] / (1 - p[1, ])) - log(p[2, ] / (1 - p[2, ]))
  var = 1 / (d[1, ] + smooth) + 1 / (d[2, ] + smooth)
  data.frame(word = colnames(d), z = as.numeric(log_odds / sqrt(var))) %>% arrange(desc(abs(z)))
}

fw = get_fw(dfm_grp)   # row 1 = expert, row 2 = journalist
top_fw = bind_rows(fw %>% slice_max(order_by = z, n = 5) %>% mutate(direction = "Experts"),
                   fw %>% slice_min(order_by = z, n = 5) %>% mutate(direction = "Journalists"))
print(top_fw)

ggplot(top_fw, aes(x = reorder_within(word, abs(z), direction), y = abs(z), fill = direction)) +
  geom_col(show.legend = FALSE, alpha = 0.8) +
  coord_flip() +
  facet_wrap(~direction, scales = "free") +
  scale_x_reordered() +
  labs(x = NULL, y = "z-score (log-odds ratio)") +
  theme_bw(base_size = 12) +
  scale_fill_manual(values = viridis::viridis(n = 9)[c(2, 5)]) +
  theme(strip.text = element_text(size = 13, face = "bold"), axis.text.y = element_text(size = 10)) +
  facetted_pos_scales(y = list(
    direction == "Journalists" ~ scale_y_continuous(labels = function(x) ifelse(x == 0, "0", paste0("-", abs(x)))),
    direction == "Experts" ~ scale_y_continuous()
  )) +
  my_theme
# ggsave("output/figures/figE_fightin_words.jpg", width = 8, height = 4)



# -----------------------------------------------------------------------------
# Figure 5: UMAP projection with group mean embeddings
# -----------------------------------------------------------------------------

dfm_seq = as.dfm(X_seq)
e_mean = colMeans(dfm_seq[grepl("^expert", rownames(dfm_seq)), ])
j_mean = colMeans(dfm_seq[grepl("^journalist", rownames(dfm_seq)), ])
dfm_means = rbind(dfm_seq, as.dfm(t(data.frame(e_mean))), as.dfm(t(data.frame(j_mean))))

set.seed(100)
um5 = umap(dfm_means)
um5_df = data.frame(UMAP1 = um5$layout[, 1], UMAP2 = um5$layout[, 2],
                    Group = factor(sub("\\d+", "", rownames(dfm_means))))

ggplot(um5_df, aes(UMAP1, UMAP2, color = Group)) +
  geom_point(size = 3, alpha = .7) +
  stat_ellipse(level = .8, linewidth = 1) +
  theme_bw() +
  scale_color_manual(values = viridis::viridis(n = 9)[c(9, 2, 8, 5)])
# ggsave("output/figures/fig05_umap.jpg", width = 6, height = 4)



# -----------------------------------------------------------------------------
# Within-group dispersion and similarity of group centroids
# -----------------------------------------------------------------------------

exp_rows = X[grepl("^expert", rownames(X)), ]
jour_rows = X[grepl("^journalist", rownames(X)), ]
exp_c = colMeans(exp_rows)
jour_c = colMeans(jour_rows)

# Narrative dispersion (Mean cosine distance to own group centroid)
# experts
mean(apply(exp_rows, 1, function(v) 1 - cos_sim(v, exp_c)))
# journalists
mean(apply(jour_rows, 1, function(v) 1 - cos_sim(v, jour_c)))

# Cosine similarity of group centroids
cos_sim(exp_c, jour_c)



# -----------------------------------------------------------------------------
# Journalist characteristics vs. similarity to the expert narrative centroid
# -----------------------------------------------------------------------------

jour = journalists %>%
  transmute(case = as.integer(Q1),
            has_econ_degree = Q97 %in% c("A3", "A4", "A5"),   # Bachelor-level economics or higher
            persist_perm = as.integer(Q21 == "A2"),           # 1 = inflation expected to be permanent
            w_energy = num(Q46), w_money = num(Q47), w_rates = num(Q49)) %>%
  left_join(topic, by = "case")

jour = left_join(jour,
                 data.frame(case = as.integer(sub("journalist", "", rownames(jour_rows))),
                            narr_sim = apply(jour_rows, 1, function(v) cos_sim(v, exp_c))),
                 by = "case")

# Journalist characteristics -> narrative similarity to expert centroid
# Economics degree (Mann-Whitney U)
wilcox.test(narr_sim ~ has_econ_degree, data = jour, exact = FALSE)$p.value

# Persistence belief (Spearman)
ct = cor.test(jour$narr_sim, jour$persist_perm, method = "spearman", exact = FALSE)
# rho
ct$estimate
# p
ct$p.value

# Topic specialisation (Mann-Whitney U) | p-value:
wilcox.test(narr_sim ~ macro_any, data = jour, exact = FALSE)$p.value



# -----------------------------------------------------------------------------
# Hierarchical clustering (cosine distance, Ward.D2, k = 2) + Figure I
# -----------------------------------------------------------------------------

dist_cos = proxy::dist(X, method = "cosine")
hc = hclust(dist_cos, method = "ward.D2")
clusters = cutree(hc, k = 2)

# png("output/figures/figI_dendrogram.png", width = 12, height = 6, units = "in", res = 300)
plot(hc, hang = -1, labels = FALSE)
rect.hclust(hc, k = 2, border = "red")
# dev.off()



# -----------------------------------------------------------------------------
# Table B: internal cluster validity indices (NbClust)
# -----------------------------------------------------------------------------

pdf(NULL)   # NbClust draws diagnostic plots; discard them
nb = NbClust(X_seq, distance = "euclidean", min.nc = 2, max.nc = 15,
             method = "complete", index = "alllong")
dev.off()
tableB = data.frame(k = 2:10, round(nb$All.index[as.character(2:10), c("Silhouette", "CH", "Gap")], 4))
tableB

# save locally
# write.csv(tableB, "output/tables/tableB_cluster_validity.csv", row.names = FALSE)



# -----------------------------------------------------------------------------
# Figure 7: items distinguishing the two clusters
# -----------------------------------------------------------------------------

cluster_features = aggregate(as.matrix(dfm), by = list(cluster = clusters), FUN = mean)
out = apply(cluster_features[, -1], 2, diff)   # mean(C2) - mean(C1)
df_diff = data.frame(feature = names(out), diff = out, row.names = NULL)
df_diff$feature[13] = "sanctions\nagainst russia"
df_diff$feature[15] = "strong\nglobal economy"
df_diff = df_diff %>% arrange(diff) %>% mutate(feature = factor(feature, levels = feature))
df_diff = rbind(cbind(head(df_diff, 6), direction = "C1"), cbind(tail(df_diff, 6), direction = "C2"))

# plot
ggplot(df_diff, aes(x = reorder_within(feature, abs(diff), direction), y = abs(diff), fill = direction)) +
  geom_col(show.legend = FALSE, alpha = 0.8) +
  coord_flip() +
  facet_wrap(~direction, scales = "free") +
  scale_x_reordered() +
  labs(x = NULL, y = "Δ mean mentions per cluster") +
  theme_bw(base_size = 12) +
  scale_fill_manual(values = viridis::viridis(n = 9)[c(4, 6)]) +
  theme(strip.text = element_text(size = 13, face = "bold"), axis.text.y = element_text(size = 10)) +
  my_theme
# ggsave("output/figures/fig07_cluster_diffs.jpg", width = 8, height = 4)



# -----------------------------------------------------------------------------
# Figure 6: UMAP coloured by group, shaped by cluster
# -----------------------------------------------------------------------------

set.seed(7)
um6 = umap(X)
um6_df = data.frame(UMAP1 = um6$layout[, 1], UMAP2 = um6$layout[, 2],
                    Cluster = factor(clusters), Group = factor(sub("\\d+", "", rownames(X))))
cluster_labels = aggregate(cbind(UMAP1, UMAP2) ~ Cluster, data = um6_df, FUN = mean)
cluster_labels$label = paste0("C", cluster_labels$Cluster)

# plot
ggplot(um6_df, aes(UMAP1, UMAP2)) +
  geom_point(aes(color = Group, shape = Cluster), size = 3, alpha = .7) +
  stat_ellipse(aes(group = Cluster), type = "norm", level = 0.68, linewidth = 1) +
  geom_label(data = cluster_labels, aes(label = label), size = 6, fontface = "bold",
             fill = "white", alpha = .8, label.size = 0) +
  theme_bw() +
  scale_color_manual(values = viridis::viridis(n = 9)[c(2, 5)])

# ggsave("output/figures/fig06_clusters.png", width = 6, height = 4, dpi = 300)


# -----------------------------------------------------------------------------
# Cluster composition by group
# -----------------------------------------------------------------------------

tab = table(group = sub("\\d+", "", names(clusters)), cluster = clusters)
cat("\n=== Cluster x group ===\n")
print(tab); print(round(prop.table(tab, 1), 2))
cat(sprintf("Fisher's exact test: p = %.3f\n", fisher.test(tab)$p.value))


# -----------------------------------------------------------------------------
# Journalist characteristics vs. cluster membership
# -----------------------------------------------------------------------------

jour = left_join(jour, data.frame(case = as.integer(sub("journalist", "", names(clusters))),
                                  cluster = clusters)[grepl("^journalist", names(clusters)), ],
                 by = "case")

cat("\n=== Journalist characteristics x cluster ===\n")
cat(sprintf("Economics degree (Fisher): p = %.3f\n",
            fisher.test(table(jour$cluster, jour$has_econ_degree))$p.value))
cat(sprintf("Topic specialisation (Fisher): p = %.3f\n",
            fisher.test(table(jour$cluster, jour$macro_share))$p.value))
weights = c(w_money = "ECB money supply", w_rates = "ECB interest rates", w_energy = "Energy crisis")
for (col in names(weights)) {
  c1 = jour[[col]][jour$cluster == 1]; c2 = jour[[col]][jour$cluster == 2]
  cat(sprintf("Weight %-20s C1 = %4.1f%%, C2 = %4.1f%%, Mann-Whitney p = %.3f\n",
              weights[col], mean(c1), mean(c2), wilcox.test(c1, c2, exact = FALSE)$p.value))
}



# -----------------------------------------------------------------------------
# Cluster stability: cluster-wise Jaccard bootstrap (Hennig 2007)
# -----------------------------------------------------------------------------

set.seed(1337)
B = 1000
cboot = clusterboot(as.dist(as.matrix(dist_cos)), B = B, distances = TRUE, bootmethod = "boot",
                    clustermethod = disthclustCBI, method = "ward.D2", k = 2, count = FALSE)

# Jaccard bootstrap (B = 1000)
data.frame(cluster = paste0("C", 1:2), n = as.vector(table(cboot$partition)),
           mean_jaccard = round(cboot$bootmean, 3),
           share_dissolved = cboot$bootbrd / B, share_recovered = cboot$bootrecover / B)
