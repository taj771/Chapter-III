library(haven)
library(dplyr)
library(tidyverse)
library(readr)



library(haven)
df <- read_dta("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/R/Vossler/BCG survey replication data.dta")

df1 <- df%>%
  select(RespondentID,HUC4,HUC4_NL,HUC4Area,HUC4Area_NL,HUC4BCG_1up,HUC4BCG_1up_NL,HUC4BCG_base,
         HUC4BCG_base_NL,HUC4BCG_scenario1,HUC4BCG_scenario1_NL,HUC4BCG_scenario2,HUC4BCG_scenario2_NL,
         HUC4BCG_scenario3,HUC4BCG_scenario3_NL,Referendum,Cost,Vote)



colnames(df)




