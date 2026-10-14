library(dplyr)
library(irr)
library(boot)

set.seed(1337)

c1 = read.csv("data/narratives/narratives_itemized.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
c2 = read.csv("data/narratives/second_coding.csv", stringsAsFactors = FALSE, encoding = "UTF-8")

norm = function(df) df %>%
  transmute(unit = paste(group, case, sep = "::"), category = tolower(trimws(category))) %>%
  filter(category != "", category != "other")
c1n = norm(c1); c2n = norm(c2)

# the 18 analysis items: mentioned by >= 2 respondents within a group (coder 1)
cats = c1n %>%
  mutate(group = sub("::.*", "", unit)) %>%
  group_by(group, category) %>%
  summarise(n = n_distinct(unit), .groups = "drop") %>%
  filter(n >= 2) %>%
  pull(category) %>% unique() %>% sort()
units = sort(intersect(c1n$unit, c2n$unit))

to_mat = function(d) {
  m = matrix(0L, length(units), length(cats), dimnames = list(units, cats))
  d = d %>% filter(unit %in% units, category %in% cats) %>% distinct()
  m[cbind(d$unit, d$category)] = 1L
  m
}
M1 = to_mat(c1n); M2 = to_mat(c2n)
v1 = as.vector(M1); v2 = as.vector(M2)

boot_alpha = function(data, idx) kripp.alpha(rbind(as.vector(M1[idx, ]), as.vector(M2[idx, ])), "nominal")$value
ci = boot.ci(boot(seq_along(units), boot_alpha, R = 5000), type = "perc")$percent[4:5]

# Respondents | items | binary judgments
length(units); length(cats); length(v1)

# Percent agreement
mean(v1 == v2)

# Cohen's kappa
kappa2(cbind(v1, v2))$value

# Krippendorff alpha
sprintf("Krippendorff alpha: %.3f  (95%% bootstrap CI [%.3f, %.3f])", kripp.alpha(rbind(v1, v2), "nominal")$value, ci[1], ci[2])
