###############################################################################

### Clear memory
rm(list = ls())


estimates_df <- read.csv("output/Model 1_estimates.csv")%>%
  select(X,Estimate,Rob.std.err.,Rob.t.ratio.0.)%>%
  mutate(Significance = case_when(
    abs(Rob.t.ratio.0.) >= 3.291 ~ "***",  # p < 0.001
    abs(Rob.t.ratio.0.) >= 2.576 ~ "**",   # p < 0.01
    abs(Rob.t.ratio.0.) >= 1.960 ~ "*",    # p < 0.05
    abs(Rob.t.ratio.0.) >= 1.645 ~ ".",    # p < 0.1
    TRUE ~ ""
  ))%>%
  mutate(Estimate_with_Stars = paste0(round(Estimate, 3), Significance))%>%
  rename(Parameter = X,
         SE = Rob.std.err.)%>%
  select(Parameter, Estimate_with_Stars, SE)

df3 <- estimates_df %>%
  filter(str_starts(Parameter, "b_cost"))%>%
  mutate(SE=round(SE, digits = 4))%>%
  rename(Mean = Estimate_with_Stars) %>%
  mutate(SE = paste("(", SE, ")", sep = ""))

df3_mean <- df3%>%
  select(Parameter,Mean)

df3_SE <- df3%>%
  select(Parameter,SE)%>%
  #mutate(SE=round(SE, digits = 4))%>%
  #mutate(SE = paste("(", SE, ")", sep = ""))%>%
  rename(Mean=SE)%>%
  mutate(Parameter = paste0("se_", Parameter))

df3 <- rbind(df3_mean,df3_SE)%>%
  arrange(Parameter)

df1 <- estimates_df %>%
  filter(str_starts(Parameter, "mu")) %>%
  rename(Mean = Estimate_with_Stars) %>%
  mutate(Parameter = str_remove(Parameter, "mu_"))

df1_mean <- df1%>%
  select(Parameter,Mean)

df1_SE <- df1%>%
  select(Parameter,SE)%>%
  mutate(SE=round(SE, digits = 3))%>%
  mutate(SE = paste("(", SE, ")", sep = ""))%>%
  rename(Mean=SE)%>%
  mutate(Parameter = paste0("se_", Parameter))


df1 <- rbind(df1_mean,df1_SE)%>%
  arrange(Parameter)


df2 <- estimates_df %>%
  filter(str_starts(Parameter, "sigma")) %>%
  rename(Standard_Deviation = Estimate_with_Stars) %>%
  mutate(Parameter = str_remove(Parameter, "sigma_"))

df2_mean <- df2%>%
  select(Parameter,Standard_Deviation)

df2_SE <- df2%>%
  select(Parameter,SE)%>%
  mutate(SE=round(SE, digits = 3))%>%
  mutate(SE = paste("(", SE, ")", sep = ""))%>%
  rename(Standard_Deviation=SE)%>%
  mutate(Parameter = paste0("se_", Parameter))



df2 <- rbind(df2_mean,df2_SE)%>%
  arrange(Parameter)

df1 <- rbind(df1,df3)

df_model1 <- df1%>%
  left_join(df2)


new_row <- data.frame(
  Parameter = c("b_asc_shared_bound", "b_asc_home_provshare","se_b_asc_shared_bound", "se_b_asc_home_provshare","b_asc_rectrip","se_b_asc_rectrip" ),
  Mean = c(NA, NA, NA,NA, NA,NA),
  Standard_Deviation = c(NA, NA,NA,NA,NA,NA)
)

df_model1 <- df_model1%>%
  rbind(new_row )%>%
  rename(Mean_M1 = Mean,
         SE_M1 = Standard_Deviation)

desired_order <- c("b_asc","se_b_asc",
                   "b_asc_shared_bound","se_b_asc_shared_bound",
                   "b_asc_home_provshare","se_b_asc_home_provshare",
                   "b_asc_rectrip","se_b_asc_rectrip",
                   "b_cost","se_b_cost",
                   "b_wq_local_basin","se_b_wq_local_basin",
                   "b_wq_nonlocal_basin", "se_b_wq_nonlocal_basin",
                   "b_wq_local_sub_basin", "se_b_wq_local_sub_basin",
                   "b_wq_nonlocal_sub_basin","se_b_wq_nonlocal_sub_basin")


df_model1 <- df_model1 %>%
  mutate(Flow = factor(Parameter, levels = desired_order)) %>%
  arrange(Flow)

#############################################################################

estimates_df <- read.csv("output/Model 2_estimates.csv")%>%
  select(X,Estimate,Rob.std.err.,Rob.t.ratio.0.)%>%
  mutate(Significance = case_when(
    abs(Rob.t.ratio.0.) >= 3.291 ~ "***",  # p < 0.001
    abs(Rob.t.ratio.0.) >= 2.576 ~ "**",   # p < 0.01
    abs(Rob.t.ratio.0.) >= 1.960 ~ "*",    # p < 0.05
    abs(Rob.t.ratio.0.) >= 1.645 ~ ".",    # p < 0.1
    TRUE ~ ""
  ))%>%
  mutate(Estimate_with_Stars = paste0(round(Estimate, 3), Significance))%>%
  rename(Parameter = X,
         SE = Rob.std.err.)%>%
  select(Parameter, Estimate_with_Stars, SE)

df3 <- estimates_df %>%
  filter(str_starts(Parameter, "b_cost"))%>%
  mutate(SE=round(SE, digits = 4))%>%
  rename(Mean = Estimate_with_Stars) %>%
  mutate(SE = paste("(", SE, ")", sep = ""))

df3_mean <- df3%>%
  select(Parameter,Mean)

df3_SE <- df3%>%
  select(Parameter,SE)%>%
  #mutate(SE=round(SE, digits = 4))%>%
  #mutate(SE = paste("(", SE, ")", sep = ""))%>%
  rename(Mean=SE)%>%
  mutate(Parameter = paste0("se_", Parameter))

df3 <- rbind(df3_mean,df3_SE)%>%
  arrange(Parameter)

df1 <- estimates_df %>%
  filter(str_starts(Parameter, "mu")) %>%
  rename(Mean = Estimate_with_Stars) %>%
  mutate(Parameter = str_remove(Parameter, "mu_"))

df1_mean <- df1%>%
  select(Parameter,Mean)

df1_SE <- df1%>%
  select(Parameter,SE)%>%
  mutate(SE=round(SE, digits = 3))%>%
  mutate(SE = paste("(", SE, ")", sep = ""))%>%
  rename(Mean=SE)%>%
  mutate(Parameter = paste0("se_", Parameter))


df1 <- rbind(df1_mean,df1_SE)%>%
  arrange(Parameter)


df2 <- estimates_df %>%
  filter(str_starts(Parameter, "sigma")) %>%
  rename(Standard_Deviation = Estimate_with_Stars) %>%
  mutate(Parameter = str_remove(Parameter, "sigma_"))

df2_mean <- df2%>%
  select(Parameter,Standard_Deviation)

df2_SE <- df2%>%
  select(Parameter,SE)%>%
  mutate(SE=round(SE, digits = 3))%>%
  mutate(SE = paste("(", SE, ")", sep = ""))%>%
  rename(Standard_Deviation=SE)%>%
  mutate(Parameter = paste0("se_", Parameter))



df2 <- rbind(df2_mean,df2_SE)%>%
  arrange(Parameter)

df1 <- rbind(df1,df3)


df_model2 <- df1%>%
  left_join(df2)%>%
  rename(Mean_M2 = Mean,
         SE_M2 = Standard_Deviation)


new_row <- data.frame(
  Parameter = c("b_asc_rectrip","se_b_asc_rectrip" ),
  Mean_M2 = c(NA, NA),
  SE_M2 = c(NA, NA)
)


df_model2 <- df_model2 %>%
  mutate(Flow = factor(Parameter, levels = desired_order)) %>%
  arrange(Flow)



#############################################################################

estimates_df <- read.csv("output/Model 3_estimates.csv")%>%
  select(X,Estimate,Rob.std.err.,Rob.t.ratio.0.)%>%
  mutate(Significance = case_when(
    abs(Rob.t.ratio.0.) >= 3.291 ~ "***",  # p < 0.001
    abs(Rob.t.ratio.0.) >= 2.576 ~ "**",   # p < 0.01
    abs(Rob.t.ratio.0.) >= 1.960 ~ "*",    # p < 0.05
    abs(Rob.t.ratio.0.) >= 1.645 ~ ".",    # p < 0.1
    TRUE ~ ""
  ))%>%
  mutate(Estimate_with_Stars = paste0(round(Estimate, 3), Significance))%>%
  rename(Parameter = X,
         SE = Rob.std.err.)%>%
  select(Parameter, Estimate_with_Stars, SE)

df3 <- estimates_df %>%
  filter(str_starts(Parameter, "b_cost"))%>%
  mutate(SE=round(SE, digits = 4))%>%
  rename(Mean = Estimate_with_Stars) %>%
  mutate(SE = paste("(", SE, ")", sep = ""))

df3_mean <- df3%>%
  select(Parameter,Mean)

df3_SE <- df3%>%
  select(Parameter,SE)%>%
  #mutate(SE=round(SE, digits = 4))%>%
  #mutate(SE = paste("(", SE, ")", sep = ""))%>%
  rename(Mean=SE)%>%
  mutate(Parameter = paste0("se_", Parameter))

df3 <- rbind(df3_mean,df3_SE)%>%
  arrange(Parameter)

df1 <- estimates_df %>%
  filter(str_starts(Parameter, "mu")) %>%
  rename(Mean = Estimate_with_Stars) %>%
  mutate(Parameter = str_remove(Parameter, "mu_"))

df1_mean <- df1%>%
  select(Parameter,Mean)

df1_SE <- df1%>%
  select(Parameter,SE)%>%
  mutate(SE=round(SE, digits = 3))%>%
  mutate(SE = paste("(", SE, ")", sep = ""))%>%
  rename(Mean=SE)%>%
  mutate(Parameter = paste0("se_", Parameter))


df1 <- rbind(df1_mean,df1_SE)%>%
  arrange(Parameter)


df2 <- estimates_df %>%
  filter(str_starts(Parameter, "sigma")) %>%
  rename(Standard_Deviation = Estimate_with_Stars) %>%
  mutate(Parameter = str_remove(Parameter, "sigma_"))

df2_mean <- df2%>%
  select(Parameter,Standard_Deviation)

df2_SE <- df2%>%
  select(Parameter,SE)%>%
  mutate(SE=round(SE, digits = 3))%>%
  mutate(SE = paste("(", SE, ")", sep = ""))%>%
  rename(Standard_Deviation=SE)%>%
  mutate(Parameter = paste0("se_", Parameter))



df2 <- rbind(df2_mean,df2_SE)%>%
  arrange(Parameter)

df1 <- rbind(df1,df3)


df_model3 <- df1%>%
  left_join(df2)%>%
  rename(Mean_M3 = Mean,
         SE_M3 = Standard_Deviation)%>%
  mutate(Flow = factor(Parameter, levels = desired_order)) %>%
  arrange(Flow)






df_model_tables <- df_model1%>%
  left_join(df_model2)%>%
  left_join(df_model3)%>%
  select(-Flow)%>%
  mutate(Parameter = case_when(
    Parameter == "b_asc" ~ "ASC",
    Parameter == "b_asc_shared_bound" ~ "ASC*Shared Boundary",
    Parameter == "b_asc_home_provshare" ~ "ASC* % of area within local Province",
    Parameter ==  "b_asc_rectrip" ~ "ASC*Recreational Trip",
    Parameter == "b_cost" ~ "Cost",
    Parameter == "b_wq_local_basin" ~ "Local Basin",
    Parameter == "b_wq_nonlocal_basin" ~ "Non-local Basin",
    Parameter == "b_wq_local_sub_basin" ~ "Local Sub Basin",
    Parameter == "b_wq_nonlocal_sub_basin" ~ "Non-local Sub Basin",
    TRUE ~ Parameter  # keep all other values as-is
  ))%>%
  mutate(Parameter = ifelse(grepl("^se_", Parameter), "", Parameter))





# Install if needed
install.packages("xtable")

# Load library
library(xtable)

# Convert to LaTeX
latex_table <- xtable(df_model_tables)

# Save to .tex file
print(latex_table, file = "Tables/df_model_tables.tex", include.rownames = FALSE)



