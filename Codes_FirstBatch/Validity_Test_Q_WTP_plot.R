df1 <- read.csv("Tables/WTP_oneUnit_full_sample.csv")


df2 <- read.csv("Tables/WTP_oneUnit_filter1.csv")

df3 <- read.csv("Tables/WTP_oneUnit_filter2.csv")

df4 <- read.csv("Tables/WTP_oneUnit_filter3.csv")

df5 <- read.csv("Tables/WTP_oneUnit_filter4.csv")

df6 <- read.csv("Tables/WTP_oneUnit_filter5.csv")



#df4 <- read.csv("Tables/WTP_oneUnit_removeing_Q15.csv")

df <- rbind(df1,df2,df3,df4,df5, df6)

df$WQ.change.scenario <- factor(df$WQ.change.scenario, levels = c(
  "Local Basin", "Non-Local Basin", "Local Sub Basin", "Non-Local  Sub Basin"
))

df$Validity_Q <- factor(df$Validity_Q, levels = c("full sample", "Filter 1", "Filter 2", "Filter 3","Filter 4", "Filter 5"))

p <- ggplot(df, aes(x = WQ.change.scenario, y = Estimate, color = Validity_Q)) +
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
  labs(x = "", y = "WTP for one unit improvement", title = "", color = "version") +
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
  )+
  scale_color_manual(
    values = c(
      "full sample" = "#1b9e77",
      "Filter 1" = "#d95f02",
      "Filter 2"  = "#7570b3",
      "Filter 3" = "darkred",
      "Filter 4" = "darkblue",
      "Filter 5" = "yellow3"
      
    ),
    labels = c(
      "full sample" = "Full Sample",
      "Filter 1" = "Criteria 1",
      "Filter 2" = "Criteria 2",
      "Filter 3" = "Criteria 3",
      "Filter 4" = "Criteria 4",
      "Filter 5" = "Criteria 5"
    ),
    name = "",
    guide = guide_legend(nrow = 2)
  ) +
  scale_y_continuous(
    limits = c(300, 600),
    breaks = seq(300, 600, by = 50)
  )

# Save to PNG
ggsave("Figures/sensitivity_WTP_checks.png", plot = p, width = 10, height = 10, units = "in", dpi = 300)

########### Marginal WTP ########### Marginal WTP ########### Marginal WTP

df1 <- read.csv("Tables/WTP_marginal_full_sample.csv")


df2 <- read.csv("Tables/WTP_marginal_filter1.csv")

df3 <- read.csv("Tables/WTP_marginal_filter2.csv")

df4 <- read.csv("Tables/WTP_marginal_filter3.csv")

df5 <- read.csv("Tables/WTP_marginal_filter4.csv")

df6 <- read.csv("Tables/WTP_marginal_filter5.csv")



#df4 <- read.csv("Tables/WTP_oneUnit_removeing_Q15.csv")

df <- rbind(df1,df2,df3,df4,df5,df6)

df$WQ.change.scenario <- factor(df$WQ.change.scenario, levels = c(
  "Local Basin", "Non-Local Basin", "Local Sub Basin", "Non-Local  Sub Basin"
))

df$Validity_Q <- factor(df$Validity_Q, levels = c("full sample", "Filter 1", "Filter 2", "Filter 3","Filter 4","Filter 5"))

p <- ggplot(df, aes(x = WQ.change.scenario, y = Estimate, color = Validity_Q)) +
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
  )+
  scale_color_manual(
    values = c(
      "full sample" = "#1b9e77",
      "Filter 1" = "#d95f02",
      "Filter 2"  = "#7570b3",
      "Filter 3" = "darkred",
      "Filter 4" = "darkblue",
      "Filter 5" = "yellow3"
    ),
    labels = c(
      "full sample" = "Full Sample",
      "Filter 1" = "Criteria 1",
      "Filter 2" = "Criteria 2",
      "Filter 3" = "Criteria 3",
      "Filter 4" = "Criteria 4",
      "Filter 5" = "Criteria 5"
    ),
    name = "",
    guide = guide_legend(nrow = 2)
  ) +
  scale_y_continuous(
    limits = c(50, 300),
    breaks = seq(50, 300, by = 50)
  )

# Save to PNG
ggsave("Figures/sensitivity_WTP_marginal_checks.png", plot = p, width = 10, height = 10, units = "in", dpi = 300)

