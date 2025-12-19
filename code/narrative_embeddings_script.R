
# =========================================================================
# Narrative Survey – Replication Script (Selected Figures)
# =========================================================================

library(quanteda)
library(dplyr)
library(ggplot2)
library(viridis)
library(proxy)
library(umap)
library(reshape2)
library(tidytext)
library(ggh4x)
library(umap)



# -------------------------------------------------------------------------
# Plot theme (used across all figures)
# -------------------------------------------------------------------------

my_theme = theme(
  panel.background = element_blank(),   # Entfernt Hintergrund
  panel.grid.major = element_line(color = "gray90"), # Dezente Hauptgitterlinien
  panel.grid.minor = element_blank(),   # Entfernt Nebengitterlinien
  axis.line = element_line(color = "black"),  # Achsenlinien in Schwarz
  axis.ticks = element_line(color = "white"), # Achsenmarkierungen in Schwarz
  axis.text = element_text(size = 10, color = "black"), # Achsentext in ordentlicher Größe
  # axis.text.x = element_text(angle = 45, vjust = 0.5),
  axis.title = element_text(size = 12, face = "bold"),  # Achsentitel fett und etwas größer
  plot.title = element_text(hjust = 0.5, size = 12, face = "bold"), # Titel zentriert und fett
  strip.background = element_blank(), # Entfernt Hintergrund der Facettenbeschriftungen
  strip.text = element_text(size = 12, face = "bold"), # Facettenbeschriftungen fett und leserlich
  legend.position = "bottom", # Optional, falls du doch eine Legende brauchst
  plot.margin = unit(c(1, 1, 1, 1), "cm") # Schöne Randabstände für das Layout
)



# -------------------------------------------------------------------------
# Load and preprocess data
# -------------------------------------------------------------------------

nar = read.csv("narratives_itemized.csv")
nar$case_id = paste0(nar$group, nar$case)

# Keep categories mentioned in at least two group–cases
nar = nar %>%
  group_by(group, category) %>%
  filter(n_distinct(case_id) >= 2) %>%
  ungroup()

# Case × category matrix
X = table(nar$case_id, nar$category)
class(X) = "matrix"
dfm = as.dfm(X)

# Optional: harmonised labels for plotting
colnames(dfm)[c(5,6,7,9,13,18)] = c(
  "monetary policy", "energy crisis", "exchange rates",
  "labour shortages", "sanctions against russia", "war in ukraine"
)

groups = sub("\\d+", "", rownames(dfm))



# -------------------------------------------------------------------------
# Figure 1: Item popularities by group
# -------------------------------------------------------------------------

dfm_grp = dfm_group(dfm, groups)
group_sizes = table(groups)

plot_df = t(quanteda::convert(dfm_grp, "data.frame")[,-1])
colnames(plot_df) = c("expert", "journalist")
plot_df = sweep(plot_df, 2, group_sizes, "/") |>
  reshape2::melt()

ggplot(plot_df, aes(x = reorder(Var1, -value), y = value, fill = Var2)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha=0.8) +
  labs(x = "", y = "share", fill = "group") +
  my_theme +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), legend.position = c(0.9, 0.8) ) +
  ylim(c(0,1))+ scale_fill_manual(values = viridis::viridis(n=9)[c(2,5)])



# -------------------------------------------------------------------------
# Cluster analysis (cosine distance, Ward linkage)
# -------------------------------------------------------------------------

# hierarchical
dist_cos = proxy::dist(X, method="cosine")
hc = hclust(dist_cos, method="ward.D2")

# plot(hc)
plot(hc, hang = -1, labels=F)
rect.hclust(hc, k = 2, border = "red") 

# get clusters
clusters = cutree(hc, k=2)
cluster_features = aggregate(X, by=list(cluster=clusters), FUN=mean)
apply(cluster_features[,-1], 1, sort, decreasing = T, simplify=F)

# get cluster differences
out = apply(cluster_features[,-1], 2, diff)
df_diff = data.frame(feature = names(out), diff = out, row.names = NULL)


# plot --------------------------------------------------------------------

# rename elements for plot
df_diff$feature[13] = "sanctions\nagainst russia"
df_diff$feature[15] = "strong\nglobal economy"

# sort values for plot
df_diff = df_diff %>%
  arrange(diff) %>%
  mutate(feature = factor(feature, levels = feature))
df_diff = rbind(cbind(head(df_diff, 6), direction="C1"), cbind(tail(df_diff, 6), direction="C2"))

# plot
ggplot(df_diff, aes(
  x = reorder_within(feature, abs(diff), direction),  # sortierung je facet
  y = abs(diff),
  fill = direction
)) +
  geom_col(show.legend = FALSE, alpha=0.8) +
  coord_flip() +
  facet_wrap(~ direction, scales = "free") +
  scale_x_reordered() +
  labs(
    x = NULL,
    y = "Δ mean mentions per cluster"
  ) +
  theme_bw(base_size = 12) +
  scale_fill_manual(values = viridis::viridis(n=9)[c(4,6)]) +
  theme(
    strip.text = element_text(size = 13, face = "bold"),
    axis.text.y = element_text(size = 10)
  ) +
  facetted_pos_scales(
    y = list(
      direction == "Journalists" ~ scale_y_continuous(labels = function(x) ifelse(x == 0, "0", paste0("-", abs(x)))),
      direction == "Experts" ~ scale_y_continuous() 
    )
  ) +
  my_theme


# Group × cluster association ---------------------------------------------

# TABLE 
assignments = data.frame(
  group = sub("\\d+","",names(clusters)),
  cluster = clusters,
  row.names = NULL
); round(table(assignments) / rowSums(table(assignments)), 2)


# Stat. Test
tab = table(assignments)
chisq.test(tab)
fisher.test(tab)



# -------------------------------------------------------------------------
# UMAP projection of narrative space
# -------------------------------------------------------------------------

# get projection
set.seed(123)
um = umap(X)

# associate with groups
group_info = factor(sub("\\d+","",rownames(X)))
um_df = data.frame(
  UMAP1 = um$layout[,1],
  UMAP2 = um$layout[,2],
  Cluster = factor(clusters),
  Group = group_info
)


# no clusters -------------------------------------------------------------

# plot
ggplot(um_df, aes(UMAP1, UMAP2)) +
  geom_point(aes(color=Group), size=3, alpha=.7) +
  stat_ellipse(aes(group = Group), type = "norm", level = 0.68, linewidth = 1) + 
  theme_bw() +
  scale_color_manual(values =  viridis::viridis(n=9)[c(2,5)])


# cluster groups ----------------------------------------------------------

# plot
ggplot(um_df, aes(UMAP1, UMAP2)) +
  geom_point(aes(color=Group, shape=Cluster), size=3, alpha=.7) +
  stat_ellipse(aes(group = Cluster), type = "norm", level = 0.68, linewidth = 1) + 
  theme_bw() +
  scale_color_manual(values =  viridis::viridis(n=9)[c(2,5)])


