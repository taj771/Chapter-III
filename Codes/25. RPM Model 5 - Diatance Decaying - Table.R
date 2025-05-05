###############################################################################

### Clear memory
rm(list = ls())

# Load packages
library(officer)
library(flextable)

estimates_df <- read.csv("output/model_20250504223044.796877_estimates.csv")%>%
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
  mutate(SE=round(SE, digits = 2))%>%
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
  mutate(SE=round(SE, digits = 2))%>%
  mutate(SE = paste("(", SE, ")", sep = ""))%>%
  rename(Standard_Deviation=SE)%>%
  mutate(Parameter = paste0("se_", Parameter))



df2 <- rbind(df2_mean,df2_SE)%>%
  arrange(Parameter)

df1 <- rbind(df1,df3)

df_final <- df1%>%
  left_join(df2)%>%
  mutate(Parameter = str_remove(Parameter, "se_"))%>%
  arrange(Parameter)%>%
  mutate(
    Parameter = if_else(
      str_detect(Mean, "^\\(.*\\)$") & str_detect(Standard_Deviation, "^\\(.*\\)$"),
      "",  # Replace Parameter with blank
      Parameter  # Otherwise keep original
    )
  )%>%
  mutate(Parameter = case_when(
    Parameter == "b_asc" ~ "ASC",
    Parameter == "b_cost" ~ "Cost",
    Parameter == "b_wq_local_basin" ~ "Local Basin",
    Parameter == "b_wq_nonlocal_basin" ~ "Non-local Basin",
    Parameter == "b_wq_sub_basin_local_nsb" ~ "Local Sub Basin - No Sharing Boundary",
    Parameter == "b_wq_sub_basin_local_sb" ~ "Local Sub Basin - Sharing Boundary",
    Parameter == "b_wq_sub_basin_nonlocal_nsb_local_prov" ~ "Non-local Sub Basin - within home Province & No Sharing Boundary",
    Parameter == "b_wq_sub_basin_nonlocal_nsb_nonlocal_prov" ~ "Non-local Sub Basin - within non-home Province & No Sharing Boundary",
    Parameter == "b_wq_sub_basin_nonlocal_sb_local_prov" ~ "Non-local Sub Basin within home Province & Sharing Boundary",
    Parameter == "b_wq_sub_basin_nonlocal_sb_nonlocal_prov" ~ "Non-local Sub Basin within non-home Province & Sharing Boundary",
    TRUE ~ Parameter  # keep all other values as-is
  ))



# Example: use your dataframe, e.g., df_final
ft <- flextable(df_final)

# Create Word document
doc <- read_docx() %>%
  body_add_par("Model Output", style = "heading 1") %>%
  body_add_flextable(ft)

# Save to file
print(doc, target = "Tables/model_5_output.docx")
