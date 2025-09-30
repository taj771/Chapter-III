###############################################################################

### Clear memory
rm(list = ls())


estimates_df <- read.csv("output/Model 1_estimates.csv")%>%
  select(X,Estimate,Rob.std.err.,Rob.t.ratio.0.)%>%
  mutate(Significance = case_when(
    #abs(Rob.t.ratio.0.) >= 3.291 ~ "***",  # p < 0.001
    abs(Rob.t.ratio.0.) >= 2.576 ~ "***",   # p < 0.01
    abs(Rob.t.ratio.0.) >= 1.960 ~ "**",    # p < 0.05
    abs(Rob.t.ratio.0.) >= 1.645 ~ "*",    # p < 0.1
    TRUE ~ ""
  ))%>%
  mutate(Estimate_with_Stars = paste0(round(Estimate, 3), Significance))%>%
  rename(Parameter = X,
         SE = Rob.std.err.)%>%
  select(Parameter, Estimate_with_Stars, SE)

df3 <- estimates_df %>%
  filter(str_starts(Parameter, "^b_asc|^b_cost|^b_wq_local_basin|^b_wq_nonlocal_basin|^b_wq_local_sub_basin|^b_wq_nonlocal_sub_basin"))%>%
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


desired_order <- c(
  "b_asc","se_b_asc","b_cost","se_b_cost",
  "b_wq_local_basin", "se_b_wq_local_basin" ,
  "b_wq_nonlocal_basin", "se_b_wq_nonlocal_basin",
  "b_wq_local_sub_basin", "se_b_wq_local_sub_basin",
  "b_wq_nonlocal_sub_basin", "se_b_wq_nonlocal_sub_basin"
  
)

df3 <- df3 %>%
  mutate(Flow = factor(Parameter, levels = desired_order)) %>%
  arrange(Flow)%>%
  select(-Flow)

#df3$Parameter <- ifelse(grepl("^se_", df3$Parameter), "", df3$Parameter)

model1 <- df3%>%
  rename(Mean1 = Mean)

# Model 2

estimates_df <- read.csv("output/Model 2_estimates.csv")%>%
  select(X,Estimate,Rob.std.err.,Rob.t.ratio.0.)%>%
  mutate(Significance = case_when(
    #abs(Rob.t.ratio.0.) >= 3.291 ~ "***",  # p < 0.001
    abs(Rob.t.ratio.0.) >= 2.576 ~ "***",   # p < 0.01
    abs(Rob.t.ratio.0.) >= 1.960 ~ "**",    # p < 0.05
    abs(Rob.t.ratio.0.) >= 1.645 ~ "*",    # p < 0.1
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
  mutate(Flow = factor(Parameter, levels = desired_order)) %>%
  arrange(Flow)%>%
  select(-Flow)

#df_model2$Parameter <- ifelse(grepl("^se_", df_model2$Parameter), "", df_model2$Parameter)


model2 <- df_model2

df_table1 <- model1%>%
  left_join(model2)

df_table1$Parameter <- ifelse(grepl("^se_", df_table1$Parameter), "", df_table1$Parameter)

df_table1 <- df_table1%>%
  mutate(Parameter = ifelse(str_starts(Parameter, "b_asc"), "Program Constant", Parameter))%>%
  mutate(Parameter = ifelse(str_starts(Parameter, "b_cost"), "Cost", Parameter))%>%
  mutate(Parameter = ifelse(str_starts(Parameter, "b_wq_local_basin"), "Basin:local", Parameter))%>%
  mutate(Parameter = ifelse(str_starts(Parameter, "b_wq_nonlocal_basin"), "Basin:non-local", Parameter))%>%
  mutate(Parameter = ifelse(str_starts(Parameter, "b_wq_local_sub_basin"), "Sub Basin:local", Parameter))%>%
  mutate(Parameter = ifelse(str_starts(Parameter, "b_wq_nonlocal_sub_basin"), "Sub Basin:non-local ", Parameter))


# Load library
library(xtable)

# Convert to LaTeX
latex_table <- xtable(df_table1)

# Save to .tex file
print(latex_table, file = "Tables/Table_1.tex", include.rownames = FALSE)



#############################################################################

desired_order <- c("b_asc","se_b_asc",
                   "b_cost","se_b_cost",
                   "b_wq_local_basin","se_b_wq_local_basin",
                   "b_wq_local_sub_basin", "se_b_wq_local_sub_basin",
                   
                   "b_wq_nonlocal_basin", "se_b_wq_nonlocal_basin",
                   "b_wq_nonlocal_sub_basin","se_b_wq_nonlocal_sub_basin",
                   
                   "b_wq_nonlocal_adj_local_basin","se_b_wq_nonlocal_adj_local_basin",
                   "b_wq_nonlocal_adj_local_sub_basin","se_b_wq_nonlocal_adj_local_sub_basin",
                   "b_wq_nonlocal_not_adj_local_basin","se_b_wq_nonlocal_not_adj_local_basin",
                   "b_wq_nonlocal_not_adj_local_sub_basin","se_b_wq_nonlocal_not_adj_local_sub_basin",

                   "b_asc_home_prov_share","se_b_asc_home_prov_share",
                   
                   "b_asc_rectrip","se_b_asc_rectrip",
                   "b_asc_rectrip_choice","se_b_asc_rectrip_choice"
)



estimates_df <- read.csv("output/Model 3_estimates.csv")%>%
  select(X,Estimate,Rob.std.err.,Rob.t.ratio.0.)%>%
  mutate(Significance = case_when(
    #abs(Rob.t.ratio.0.) >= 3.291 ~ "***",  # p < 0.001
    abs(Rob.t.ratio.0.) >= 2.576 ~ "***",   # p < 0.01
    abs(Rob.t.ratio.0.) >= 1.960 ~ "**",    # p < 0.05
    abs(Rob.t.ratio.0.) >= 1.645 ~ "*",    # p < 0.1
    TRUE ~ ""
  ))%>%
  mutate(Estimate_with_Stars = paste0(round(Estimate, 3), Significance))%>%
  rename(Parameter = X,
         SE = Rob.std.err.)%>%
  select(Parameter, Estimate_with_Stars, SE)

df3 <- estimates_df %>%
  filter(str_detect(Parameter, "^b_cost"))%>%
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
  left_join(df2)

new_row <- data.frame(
  Parameter = c("b_wq_nonlocal_basin", "se_b_wq_nonlocal_basin",
                "b_wq_nonlocal_sub_basin","se_b_wq_nonlocal_sub_basin",
                
                "b_asc_home_prov_share","se_b_asc_home_prov_share",
                
                "b_asc_rectrip","se_b_asc_rectrip",
                "b_asc_rectrip_choice","se_b_asc_rectrip_choice"
  ),
  Mean = c(NA, NA, NA,NA,NA,NA,NA,NA,NA,NA),
  Standard_Deviation = c(NA, NA,NA,NA,NA,NA,NA,NA,NA,NA)
)



df_model3 <- df_model3%>%
  rbind(new_row )%>%
  mutate(Flow = factor(Parameter, levels = desired_order)) %>%
  rename(Mean_M3 = Mean,
         SE_M3 = Standard_Deviation)%>%
  arrange(Flow)



#############################################################################

estimates_df <- read.csv("output/Model 4_estimates.csv")%>%
  select(X,Estimate,Rob.std.err.,Rob.t.ratio.0.)%>%
  mutate(Significance = case_when(
    #abs(Rob.t.ratio.0.) >= 3.291 ~ "***",  # p < 0.001
    abs(Rob.t.ratio.0.) >= 2.576 ~ "***",   # p < 0.01
    abs(Rob.t.ratio.0.) >= 1.960 ~ "**",    # p < 0.05
    abs(Rob.t.ratio.0.) >= 1.645 ~ "*",    # p < 0.1
    TRUE ~ ""
  ))%>%
  mutate(Estimate_with_Stars = paste0(round(Estimate, 3), Significance))%>%
  rename(Parameter = X,
         SE = Rob.std.err.)%>%
  select(Parameter, Estimate_with_Stars, SE)


df3 <- estimates_df %>%
  filter(str_detect(Parameter, "^b_cost|^b_asc_home_prov_share"))%>%
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

df_model4 <- df1%>%
  left_join(df2)

new_row <- data.frame(
  Parameter = c("mu_b_wq_nonlocal_adj_local_basin","se_b_wq_nonlocal_adj_local_basin",
                "mu_b_wq_nonlocal_adj_local_sub_basin","se_b_wq_nonlocal_adj_local_sub_basin",
                "mu_b_wq_nonlocal_not_adj_local_basin","se_b_wq_nonlocal_not_adj_local_basin",
                "mu_b_wq_nonlocal_not_adj_local_sub_basin","se_b_wq_nonlocal_not_adj_local_sub_basin",
                "b_asc_rectrip","se_b_asc_rectrip",
                "b_asc_rectrip_choice","se_b_asc_rectrip_choice"
                
  ),
  Mean = c(NA,NA, NA,NA, NA,NA,NA,NA,NA,NA,NA,NA),
  Standard_Deviation = c(NA,NA, NA,NA, NA,NA,NA,NA,NA,NA,NA,NA)
)


df_model4 <- df_model4%>%
  rbind(new_row )%>%
  rename(Mean_M4 = Mean,
         SE_M4 = Standard_Deviation)%>%
  mutate(Flow = factor(Parameter, levels = desired_order)) %>%
  arrange(Flow)

###############################################################################

estimates_df <- read.csv("output/Model 5_estimates.csv")%>%
  select(X,Estimate,Rob.std.err.,Rob.t.ratio.0.)%>%
  mutate(Significance = case_when(
    #abs(Rob.t.ratio.0.) >= 3.291 ~ "***",  # p < 0.001
    abs(Rob.t.ratio.0.) >= 2.576 ~ "***",   # p < 0.01
    abs(Rob.t.ratio.0.) >= 1.960 ~ "**",    # p < 0.05
    abs(Rob.t.ratio.0.) >= 1.645 ~ "*",    # p < 0.1
    TRUE ~ ""
  ))%>%
  mutate(Estimate_with_Stars = paste0(round(Estimate, 3), Significance))%>%
  rename(Parameter = X,
         SE = Rob.std.err.)%>%
  select(Parameter, Estimate_with_Stars, SE)



df3 <- estimates_df %>%
  filter(str_detect(Parameter, 
                    "^b_cost|^b_asc_rectrip|^b_asc_rectrip_choice"))%>%
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


df1 <- rbind(df1_mean,df1_SE)

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

df_model5 <- df1%>%
  left_join(df2)

new_row <- data.frame(
  Parameter = c("mu_b_wq_nonlocal_adj_local_basin","se_b_wq_nonlocal_adj_local_basin",
                "mu_b_wq_nonlocal_adj_local_sub_basin","se_b_wq_nonlocal_adj_local_sub_basin",
                "mu_b_wq_nonlocal_not_adj_local_basin","se_b_wq_nonlocal_not_adj_local_basin",
                "mu_b_wq_nonlocal_not_adj_local_sub_basin","se_b_wq_nonlocal_not_adj_local_sub_basin",
                "b_asc_home_prov_share","se_b_asc_home_prov_share"
                
                

                
  ),
  Mean = c(NA,NA, NA,NA, NA,NA,NA,NA,NA,NA),
  Standard_Deviation = c(NA,NA,NA,NA, NA,NA,NA,NA,NA,NA)
)



df_model5 <- df_model5%>%
  rbind(new_row )%>%
  rename(Mean_M5 = Mean,
         SE_M5 = Standard_Deviation)%>%
  mutate(Flow = factor(Parameter, levels = desired_order)) %>%
  arrange(Flow)

################################################################################

df_model_table2 <- df_model3%>%
  left_join(df_model4)%>%
  left_join(df_model5)%>%
  select(-Flow)%>%
  mutate(Parameter = case_when(
    Parameter == "b_asc" ~ "ASC",
    Parameter == "b_cost" ~ "Cost",

    Parameter == "b_wq_local_basin" ~ "Local Basin",
    Parameter == "b_wq_nonlocal_basin" ~ "Non-local Basin",
    Parameter == "b_wq_local_sub_basin" ~ "Local Sub Basin",
    Parameter == "b_wq_nonlocal_sub_basin" ~ "Non-local Sub Basin",
    
    Parameter == "b_wq_nonlocal_adj_local_basin" ~ "Non_local Basin - Adjacent to Local Basin",
    Parameter == "b_wq_nonlocal_adj_local_sub_basin" ~ "Non_local Sub Basin - Adjacent to Local Sub Basin",
    
    Parameter == "b_wq_nonlocal_not_adj_local_basin" ~ "Non_local Basin - Non Adjacent to Local Basin",
    Parameter == "b_wq_nonlocal_not_adj_local_sub_basin" ~ "Non_local Sub Basin - Non Adjacent to Local Sub Basin",
    
    Parameter == "b_asc_home_prov_share" ~ "ASC* Home Province Share of Policy Site",

    Parameter == "b_asc_rectrip" ~ "ASC*Recreational Trip",
    Parameter == "b_asc_rectrip_choice" ~ "ASC*Recreational Trip to Choice Sub/Basin",

    TRUE ~ Parameter  # keep all other values as-is
  ))%>%
  mutate(Parameter = ifelse(grepl("^se_", Parameter), "", Parameter))


# Convert to LaTeX
latex_table <- xtable(df_model_table2)

# Save to .tex file
print(latex_table, file = "Tables/Table_2.tex", include.rownames = FALSE)



################################################################################
# Version 

estimates_df <- read.csv("output/Model 5_estimates.csv")%>%
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
  filter(str_detect(Parameter, 
                    "^b_cost|^b_asc_version2|^b_asc_version3|^b_asc_version4|^b_wq_x_version2|^b_wq_x_version3|^b_wq_x_version4"))%>%
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


df1 <- rbind(df1_mean,df1_SE)

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

df_model5 <- df1%>%
  left_join(df2)


df_model5 <- df_model5%>%
  rename(Mean_M5 = Mean,
         SE_M5 = Standard_Deviation)



desired_order <- c("b_asc","se_b_asc",
                   "b_wq","se_b_wq",
                   "b_cost","se_b_cost",
                   "b_asc_version2","se_b_asc_version2",
                   "b_asc_version3","se_b_asc_version3",
                   "b_asc_version4","se_b_asc_version4",
                   "b_wq_x_version2","se_b_wq_x_version2",
                   "b_wq_x_version3","se_b_wq_x_version3",
                   "b_wq_x_version4", "se_b_wq_x_version4"
)




df_model_table3 <- df_model5%>%
  mutate(Flow = factor(Parameter, levels = desired_order))%>%
  arrange(Flow)%>%
  select(-Flow)%>%
  mutate(Parameter = case_when(
    Parameter == "b_asc" ~ "ASC",
    Parameter == "b_wq" ~ "Water Quality",
    Parameter == "b_asc_version2" ~ "ASC* Version 2",
    Parameter == "b_asc_version3" ~ "ASC* Version 3",
    Parameter == "b_asc_version4" ~ "ASC* Version 4",
    Parameter == "b_wq_x_version2" ~ "Water Quality* Version 2",
    Parameter == "b_wq_x_version3" ~ "Water Quality* Version 3",
    Parameter == "b_wq_x_version4" ~ "Water Quality* Version 4",
    Parameter == "b_cost" ~ "Cost",
    TRUE ~ Parameter  # keep all other values as-is
  ))%>%
  mutate(Parameter = ifelse(grepl("^se_", Parameter), "", Parameter))



# Convert to LaTeX
latex_table <- xtable(df_model_table3)

# Save to .tex file
print(latex_table, file = "Tables/Table_3.tex", include.rownames = FALSE)

################################################################################

################################################################################
# Version - option 2

estimates_df <- read.csv("output/Model 6_estimates.csv")%>%
  select(X,Estimate,Rob.std.err.,Rob.t.ratio.0.)%>%
  mutate(Significance = case_when(
    #abs(Rob.t.ratio.0.) >= 3.291 ~ "***",  # p < 0.001
    abs(Rob.t.ratio.0.) >= 2.576 ~ "***",   # p < 0.01
    abs(Rob.t.ratio.0.) >= 1.960 ~ "**",    # p < 0.05
    abs(Rob.t.ratio.0.) >= 1.645 ~ "*",    # p < 0.1
    TRUE ~ ""
  ))%>%
  mutate(Estimate_with_Stars = paste0(round(Estimate, 3), Significance))%>%
  rename(Parameter = X,
         SE = Rob.std.err.)%>%
  select(Parameter, Estimate_with_Stars, SE)



df3 <- estimates_df %>%
  filter(str_detect(Parameter, 
                    "^b_cost|^b_basewq|^b_version1|^b_version2|^b_version3"))%>%
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


df1 <- rbind(df1_mean,df1_SE)

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


df_model6 <- df1%>%
  left_join(df2)


new_row <- data.frame(
  Parameter = c(
    "b_baseline", "se_b_baseline",                      
    "b_wq_x_bl",  "se_b_wq_x_bl"              

  ),
  Mean = c(NA, NA, NA, NA),
  Standard_Deviation = c(NA,NA,NA, NA)
)


df_model6 <- df_model6%>%
  rbind(new_row )%>%
  rename(Mean_M6 = Mean,
         SE_M6 = Standard_Deviation)


###############################################################################

estimates_df <- read.csv("output/Model 7_estimates.csv")%>%
  select(X,Estimate,Rob.std.err.,Rob.t.ratio.0.)%>%
  mutate(Significance = case_when(
    #abs(Rob.t.ratio.0.) >= 3.291 ~ "***",  # p < 0.001
    abs(Rob.t.ratio.0.) >= 2.576 ~ "***",   # p < 0.01
    abs(Rob.t.ratio.0.) >= 1.960 ~ "**",    # p < 0.05
    abs(Rob.t.ratio.0.) >= 1.645 ~ "*",    # p < 0.1
    TRUE ~ ""
  ))%>%
  mutate(Estimate_with_Stars = paste0(round(Estimate, 3), Significance))%>%
  rename(Parameter = X,
         SE = Rob.std.err.)%>%
  select(Parameter, Estimate_with_Stars, SE)



df3 <- estimates_df %>%
  filter(str_detect(Parameter, 
                    "^b_cost|^b_baseline|^b_wq_x_bl"))%>%
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


df1 <- rbind(df1_mean,df1_SE)

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

df_model7 <- df1%>%
  left_join(df2)

new_row <- data.frame(
  Parameter = c("b_basewq", "se_b_basewq",
                "b_version1", "se_b_version1",
                "b_version2", "se_b_version2",
                "b_version3", "se_b_version3"
  ),
  Mean = c(NA, NA,NA, NA,NA, NA,NA, NA),
  Standard_Deviation = c(NA, NA,NA, NA,NA, NA,NA, NA)
)



df_model7 <- df_model7%>%
  rbind(new_row )%>%
  rename(Mean_M7 = Mean,
         SE_M7 = Standard_Deviation)

desired_order <- c("b_asc","se_b_asc",
                   "b_wq","se_b_wq",
                   "b_cost","se_b_cost",
                   
                   "b_wq_local_basin","se_b_wq_local_basin",
                   "b_wq_nonlocal_basin","se_b_wq_nonlocal_basin",
                   "b_wq_local_sub_basin", "se_b_wq_local_sub_basin",
                   "b_wq_nonlocal_sub_basin", "se_b_wq_nonlocal_sub_basin",
                   
                   "b_basewq", "se_b_basewq",
                   "b_version1", "se_b_version1",
                   "b_version2", "se_b_version2",
                   "b_version3", "se_b_version3",
                   
                   "b_baseline", "se_b_baseline",
                   "b_wq_x_bl", "se_b_wq_x_bl"
                   
)


df_model_table4 <- df_model6%>%
  left_join(df_model7)%>%
  mutate(Flow = factor(Parameter, levels = desired_order))%>%
  arrange(Flow)%>%
  select(-Flow)%>%
  mutate(Parameter = case_when(
    Parameter == "b_asc" ~ "Program Constant",
    Parameter == "b_cost" ~ "Cost",
    
    Parameter == "b_wq_local_basin" ~ "Basin:local",
    Parameter == "b_wq_nonlocal_basin" ~ "Basin:non-local",
    Parameter == "b_wq_local_sub_basin" ~ "Sub Basin:local",
    Parameter == "b_wq_nonlocal_sub_basin" ~ "Sub Basin:non-local",
    
    Parameter == "b_basewq" ~ "Current Level X delta changes",
    Parameter == "b_version1" ~ "Vesion 1",
    Parameter == "b_version2" ~ "Vesion 2",
    Parameter == "b_version3" ~ "Vesion 3",
    
    Parameter == "b_baseline" ~ "Variation in Baseline WQ",
    Parameter == "b_wq_x_bl" ~ "Variation in Baseline WQ X delta changes",

    TRUE ~ Parameter  # keep all other values as-is
  ))%>%
  mutate(Parameter = ifelse(grepl("^se_", Parameter), "", Parameter)) 

# Convert to LaTeX
latex_table <- xtable(df_model_table4)

# Save to .tex file
print(latex_table, file = "Tables/Table_4.tex", include.rownames = FALSE)

