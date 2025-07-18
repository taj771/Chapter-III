df1 <- read.csv("Tables/WTP_oneUnit_version1.csv")

df2 <- read.csv("Tables/WTP_oneUnit_version2.csv")

df3 <- read.csv("Tables/WTP_oneUnit_version3.csv")

#df4 <- read.csv("Tables/WTP_oneUnit_version4.csv")

df <- rbind(df1,df2,df3)

df$WQ.change.scenario <- factor(df$WQ.change.scenario, levels = c(
  "Local Basin", "Non-Local Basin", "Local Sub Basin", "Non-Local  Sub Basin"
))

p <- ggplot(df, aes(x = WQ.change.scenario, y = Estimate, color = version)) +
  # Shaded background by param index (works with coord_flip)
  scale_fill_identity() +
  
  # Points and error bars
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbar(
    aes(ymin = Estimate - SE, ymax = Estimate + SE),
    position = position_dodge(width = 0.6), width = 0.2
  ) +
  # Axis formatting
  #scale_x_discrete(expand = expansion(mult = 0, add = 0.1)) +
  #coord_flip() +
  theme_minimal() +
  labs(x = "", y = "Marginal WTP", title = "", color = "version") +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    axis.text.x = element_text(hjust = 1, size = 10, angle = 90),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.y = element_line(size = 0.2, color = "black"),
    axis.line.x = element_line(size = 0.2, color = "black")
  ) +
  scale_color_manual(
    values = c(
      "v1" = "#1b9e77",
      "v2" = "#d95f02",
      "v3" = "#7570b3"
    ),
    labels = c(
      "v1" = "Lowest health score",
      "v2" = "One unit better health score",
      "v3" = "Two unit better health score"
    ),
    name = "",
    guide = guide_legend(nrow = 5)
  ) +
  scale_y_continuous(
    limits = c(0, 320),
    breaks = seq(0, 320, by = 50)
  )








# Save to PNG
ggsave("Figures/baseline_wtp_oneunit_improvement.png", plot = p, width = 10, height = 10, units = "in", dpi = 300)

