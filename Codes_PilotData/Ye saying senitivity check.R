estimates_df1 <- read.csv("output/Model 1_estimates.csv")%>%
  select(X,Estimate,Rob.std.err.,Rob.t.ratio.0.)%>%
  mutate(Estimate = paste0(round(Estimate, 3)))%>%
  rename(Parameter = X,
         SE = Rob.std.err.)%>%
  select(Parameter, Estimate, SE)%>%
  mutate(Parameter = case_when(
    Parameter == "mu_b_asc" ~ "Mean-ASC",
    Parameter == "sigma_b_asc" ~ "Std. Dev.-ASC",
    Parameter == "b_cost" ~ "Cost",
    
    Parameter == "mu_b_wq_local_basin" ~ "Mean-Local Basin",
    Parameter == "sigma_b_wq_local_basin" ~ "Std. Dev.-Local Basin",
    
    Parameter == "mu_b_wq_nonlocal_basin" ~ "Mean-Non-local Basin",
    Parameter == "sigma_b_wq_nonlocal_basin" ~ "Std. Dev.-Non-local Basin",
    
    Parameter == "mu_b_wq_local_sub_basin" ~ "Mean-Local Sub Basin",
    Parameter == "sigma_b_wq_local_sub_basin" ~ "Std. Dev.-Local Sub Basin",
    
    Parameter == "mu_b_wq_nonlocal_sub_basin" ~ "Mean-Non-local Sub Basin",
    Parameter == "sigma_b_wq_nonlocal_sub_basin" ~ "Std. Dev-Non-local Sub Basin"))%>%
  mutate(Estimate = as.numeric(Estimate))%>%
  mutate(SE = as.numeric(SE))%>%
  mutate(type = "Original")%>%
  mutate(Estimate = if_else(str_starts(Parameter, "Std."), abs(Estimate), Estimate))


estimates_df2 <- read.csv("output/Model 10_estimates.csv")%>%
  select(X,Estimate,Rob.std.err.,Rob.t.ratio.0.)%>%
  mutate(Estimate = paste0(round(Estimate, 3)))%>%
  rename(Parameter = X,
         SE = Rob.std.err.)%>%
  select(Parameter, Estimate, SE)%>%
  mutate(Parameter = case_when(
    Parameter == "mu_b_asc" ~ "Mean-ASC",
    Parameter == "sigma_b_asc" ~ "Std. Dev.-ASC",
    Parameter == "b_cost" ~ "Cost",
    
    Parameter == "mu_b_wq_local_basin" ~ "Mean-Local Basin",
    Parameter == "sigma_b_wq_local_basin" ~ "Std. Dev.-Local Basin",
    
    Parameter == "mu_b_wq_nonlocal_basin" ~ "Mean-Non-local Basin",
    Parameter == "sigma_b_wq_nonlocal_basin" ~ "Std. Dev.-Non-local Basin",

    Parameter == "mu_b_wq_local_sub_basin" ~ "Mean-Local Sub Basin",
    Parameter == "sigma_b_wq_local_sub_basin" ~ "Std. Dev.-Local Sub Basin",
    
    Parameter == "mu_b_wq_nonlocal_sub_basin" ~ "Mean-Non-local Sub Basin",
    Parameter == "sigma_b_wq_nonlocal_sub_basin" ~ "Std. Dev-Non-local Sub Basin"))%>%
  mutate(Estimate = as.numeric(Estimate))%>%
  mutate(SE = as.numeric(SE))%>%
  mutate(type = "Remove ye saying")%>%
  mutate(Estimate = if_else(str_starts(Parameter, "Std."), abs(Estimate), Estimate))
    
    


estimates_df3 <- read.csv("output/Model 11_estimates.csv")%>%
  select(X,Estimate,Rob.std.err.,Rob.t.ratio.0.)%>%
  mutate(Estimate = paste0(round(Estimate, 3)))%>%
  rename(Parameter = X,
         SE = Rob.std.err.)%>%
  select(Parameter, Estimate, SE)%>%
  mutate(Parameter = case_when(
    Parameter == "mu_b_asc" ~ "Mean-ASC",
    Parameter == "sigma_b_asc" ~ "Std. Dev.-ASC",
    Parameter == "b_cost" ~ "Cost",
    
    Parameter == "mu_b_wq_local_basin" ~ "Mean-Local Basin",
    Parameter == "sigma_b_wq_local_basin" ~ "Std. Dev.-Local Basin",
    
    Parameter == "mu_b_wq_nonlocal_basin" ~ "Mean-Non-local Basin",
    Parameter == "sigma_b_wq_nonlocal_basin" ~ "Std. Dev.-Non-local Basin",
    
    Parameter == "mu_b_wq_local_sub_basin" ~ "Mean-Local Sub Basin",
    Parameter == "sigma_b_wq_local_sub_basin" ~ "Std. Dev.-Local Sub Basin",
    
    Parameter == "mu_b_wq_nonlocal_sub_basin" ~ "Mean-Non-local Sub Basin",
    Parameter == "sigma_b_wq_nonlocal_sub_basin" ~ "Std. Dev-Non-local Sub Basin"))%>%
  mutate(Estimate = as.numeric(Estimate))%>%
  mutate(SE = as.numeric(SE))%>%
  mutate(type = "Remove uncertain")%>%
  mutate(Estimate = if_else(str_starts(Parameter, "Std."), abs(Estimate), Estimate))


estimates_df4 <- read.csv("output/Model 12_estimates.csv")%>%
  select(X,Estimate,Rob.std.err.,Rob.t.ratio.0.)%>%
  mutate(Estimate = paste0(round(Estimate, 3)))%>%
  rename(Parameter = X,
         SE = Rob.std.err.)%>%
  select(Parameter, Estimate, SE)%>%
  mutate(Parameter = case_when(
    Parameter == "mu_b_asc" ~ "Mean-ASC",
    Parameter == "sigma_b_asc" ~ "Std. Dev.-ASC",
    Parameter == "b_cost" ~ "Cost",
    
    Parameter == "mu_b_wq_local_basin" ~ "Mean-Local Basin",
    Parameter == "sigma_b_wq_local_basin" ~ "Std. Dev.-Local Basin",
    
    Parameter == "mu_b_wq_nonlocal_basin" ~ "Mean-Non-local Basin",
    Parameter == "sigma_b_wq_nonlocal_basin" ~ "Std. Dev.-Non-local Basin",
    
    Parameter == "mu_b_wq_local_sub_basin" ~ "Mean-Local Sub Basin",
    Parameter == "sigma_b_wq_local_sub_basin" ~ "Std. Dev.-Local Sub Basin",
    
    Parameter == "mu_b_wq_nonlocal_sub_basin" ~ "Mean-Non-local Sub Basin",
    Parameter == "sigma_b_wq_nonlocal_sub_basin" ~ "Std. Dev-Non-local Sub Basin"))%>%
  mutate(Estimate = as.numeric(Estimate))%>%
  mutate(SE = as.numeric(SE))%>%
  mutate(type = "Ye saying Q 12")%>%
  mutate(Estimate = if_else(str_starts(Parameter, "Std."), abs(Estimate), Estimate))


estimates_df5 <- read.csv("output/Model 13_estimates.csv")%>%
  select(X,Estimate,Rob.std.err.,Rob.t.ratio.0.)%>%
  mutate(Estimate = paste0(round(Estimate, 3)))%>%
  rename(Parameter = X,
         SE = Rob.std.err.)%>%
  select(Parameter, Estimate, SE)%>%
  mutate(Parameter = case_when(
    Parameter == "mu_b_asc" ~ "Mean-ASC",
    Parameter == "sigma_b_asc" ~ "Std. Dev.-ASC",
    Parameter == "b_cost" ~ "Cost",
    
    Parameter == "mu_b_wq_local_basin" ~ "Mean-Local Basin",
    Parameter == "sigma_b_wq_local_basin" ~ "Std. Dev.-Local Basin",
    
    Parameter == "mu_b_wq_nonlocal_basin" ~ "Mean-Non-local Basin",
    Parameter == "sigma_b_wq_nonlocal_basin" ~ "Std. Dev.-Non-local Basin",
    
    Parameter == "mu_b_wq_local_sub_basin" ~ "Mean-Local Sub Basin",
    Parameter == "sigma_b_wq_local_sub_basin" ~ "Std. Dev.-Local Sub Basin",
    
    Parameter == "mu_b_wq_nonlocal_sub_basin" ~ "Mean-Non-local Sub Basin",
    Parameter == "sigma_b_wq_nonlocal_sub_basin" ~ "Std. Dev-Non-local Sub Basin"))%>%
  mutate(Estimate = as.numeric(Estimate))%>%
  mutate(SE = as.numeric(SE))%>%
  mutate(type = "Ye saying Q 15")%>%
  mutate(Estimate = if_else(str_starts(Parameter, "Std."), abs(Estimate), Estimate))

df <- rbind(estimates_df1,estimates_df2, estimates_df3, estimates_df4,estimates_df5)

#custom_order <- c("Mean-ASC","Std. Dev.-ASC","Cost","Mean-Local Basin","Std. Dev.-Local Basin",
#                  "Mean-Non-local Basin","Std. Dev.-Non-local Basin","Mean-Local Sub Basin","Std. Dev.-Local Sub Basin",
#                  "Mean-Non-local Sub Basin","Std. Dev-Non-local Sub Basin")




# Ensure consistent order and add index
df$Parameter <- factor(df$Parameter, levels = unique(df$Parameter))
df$param_index <- as.numeric(df$Parameter)


custom_order <- c("Std. Dev-Non-local Sub Basin","Mean-Non-local Sub Basin","Std. Dev.-Local Sub Basin","Mean-Local Sub Basin",
                  "Std. Dev.-Non-local Basin","Mean-Non-local Basin","Std. Dev.-Local Basin","Mean-Local Basin","Cost",
                  "Std. Dev.-ASC","Mean-ASC")


df$Parameter <- factor(df$Parameter, levels = custom_order)



# Create a shading dataframe using param_index
shade_df <- data.frame(
  xmin = seq(0.5, length(levels(df$Parameter)) - 0.5),
  xmax = seq(1.5, length(levels(df$Parameter)) + 0.5),
  fill = rep(c("gray95", "white"), length.out = length(levels(df$Parameter)))
)

# Plot
p <- ggplot(df, aes(x = Parameter, y = Estimate, color = type)) +
  # Shaded background by param index (works with coord_flip)
  geom_rect(data = shade_df,
            aes(ymin = -Inf, ymax = Inf, xmin = xmin, xmax = xmax, fill = fill),
            inherit.aes = FALSE, alpha = 0.6) +
  scale_fill_identity() +
  
  # Points and error bars
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbar(
    aes(ymin = Estimate - SE, ymax = Estimate + SE),
    position = position_dodge(width = 0.6), width = 0.2
  ) +
  
  # Axis formatting
  scale_x_discrete(expand = expansion(mult = 0, add = 0.5)) +
  coord_flip() +
  theme_minimal() +
  labs(x = "", y = "Estimation", title = "", color = "Type") +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    axis.text.x = element_text(hjust = 1, size = 10),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.y = element_line(size = 0.2, color = "black"),
    axis.line.x = element_line(size = 0.2, color = "black")
  ) +
  scale_color_manual(
    values = c(
      "Original" = "#1b9e77",
      "Remove ye saying" = "#d95f02",
      "Remove uncertain" = "#7570b3",
      "Ye saying Q 12" = "#F0E68C",
      "Ye saying Q 15" = "#66a61e"
    ),
    labels = c(
      "Original" = "Full sample",
      "Remove ye saying" = "Exclude respondents who said 'Yes' to all options",
      "Remove uncertain" = "Exclude respondents with uncertain votes",
      "Ye saying Q 12" = "Exclude respondents who chose other proposals or were uncertain (Q12)",
      "Ye saying Q 15" = "Exclude respondents with general opinions (Q15)"
    ),
    name = "",
    guide = guide_legend(nrow = 5)
  )+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.3)





# Save to PNG
ggsave("Figures/sensitivity_checks.png", plot = p, width = 10, height = 16, units = "in", dpi = 300)
