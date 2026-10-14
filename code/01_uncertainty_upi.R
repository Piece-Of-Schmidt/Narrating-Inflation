library(ggplot2)
library(lubridate)

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

upi = read.csv("data/upi/upi_counts.csv")
upi = subset(upi, type == "docs" & date > "2015-01-01" & date < "2024-01-01")
upi$year = floor_date(as.Date(upi$date), "year")

upi_year = aggregate(cbind(n, n_wirtschaft_unsicher) ~ year, sum, data = upi)
upi_year$rel = upi_year$n_wirtschaft_unsicher / upi_year$n
print(upi_year)

ggplot(upi_year, aes(x = year, y = rel)) +
  geom_col(fill = "black", alpha = 0.2) +
  my_theme +
  labs(x = "date", y = "share of articles")
ggsave("output/figures/fig01_upi_year.jpg", dpi = 300, width = 6, height = 3)
