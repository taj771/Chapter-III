################################################################################
# create supportig data files to set the choice data set for Apollo
################################################################################
# Clean the memory
rm(list = ls())  # Removes all objects in the environment

library(tidyverse)
library(haven)
library(readxl)
library(tidyr)
library(stringr)
library(readr)
library(haven)
library(purrr)
library(apollo)
library(pacman)
library(car)


# 1. Create data frame with coloum names of Original data file that use for making column names
# consistent
# Read raw data file from CHAISR
df <- read_sav("Rawdata/Water_Quality.sav 2")

# variable
df1 <- df[, grepl("COST", names(df)) | # Original data file the name the column that contain the cost of each policy scenario as "AR_B1_1_COST", 
            #where AR_B1_1 varied depend on the block and the river basin. SO we extarct the all the column with "COST" term
            names(df) == "CaseId" | # Respondents ID - use to map the choices for each repondents
            grepl("PC", names(df))   | # PC - I think vote scenario 1 - yes 2-no - need to clarify it with Danny. Sme as "COST" variable "PC_AR_B1_2", 
            #where _AR_B1_2 varied based on block and river basin
            grepl("POLICY_AVERAGE", names(df))| # Policy Average WQ 
            grepl("CURRENT_AVERAGE", names(df))| # Current/Baseline Avergae WQ
            grepl("POLICY_SIZE_KM", names(df))| # Policy area in SQKM
            grepl("POLICY_SIZE_PERCENT", names(df))| # Policy area in %
            grepl("CURRENT_1", names(df))| # WQ_1 % in current level
            grepl("CURRENT_2", names(df))| # WQ_2 % in current level
            grepl("CURRENT_3", names(df))| # WQ_3 % in current level
            grepl("CURRENT_4", names(df))| # WQ_4 % in current level
            grepl("CURRENT_5", names(df))| # WQ_5 % in current level
            grepl("IMAGE_CURRENT", names(df)) | # image current
            grepl("POLICY_1", names(df))| # WQ_1 % in policy
            grepl("POLICY_2", names(df))| # WQ_2 % in policy
            grepl("POLICY_3", names(df))| # WQ_3 % in policy
            grepl("POLICY_4", names(df))| # WQ_4 % in policy
            grepl("POLICY_5", names(df))| # WQ_5 % in policy
            grepl("IMAGE_POLICY", names(df))           # image current
            ]




# Read colom names and save the colom name as csv that make easy to track the variable names
column_names <- colnames(df1)

# Write the column names to a csv file
write.csv(column_names, "./Deriveddata/column_names.csv")


# 2. we need some information about block number and relevant basin/sub basin to determine
# the choice locality which will be use to create some variables that use in our choice models


ChoiceSetDesigns <- read_excel("./Rawdata/ChoiceSetDesigns.xlsx")


ChoiceSetDesigns <- ChoiceSetDesigns %>%
  select(block, sub_basin,basin) %>%
  distinct(block, .keep_all = TRUE) %>%
  mutate(BLOCK_SUBBASIN_LOCALITY = case_when(
    sub_basin == "all" ~ 0,
    sub_basin == "Assiniboine" ~ 1,
    sub_basin == "Qu'Appelle" ~ 2,
    sub_basin == "Red" ~ 3,
    sub_basin == "Souris" ~ 4,
    sub_basin == "East Lake Winnipeg" ~ 5,
    sub_basin == "Grass and Burntwood" ~ 6,
    sub_basin == "Lake Winnipegosis and Lake Manitoba" ~ 7,
    sub_basin == "Nelson" ~ 8,
    sub_basin == "Saskatchewan" ~ 9,
    sub_basin == "Western Lake Winnipeg" ~ 10,
    sub_basin == "Battle" ~ 11,
    sub_basin == "Central North Saskatchewan" ~ 12,
    sub_basin == "Lower North Saskatchewan" ~ 13,
    sub_basin == "Upper North Saskatchewan" ~ 14,
    sub_basin == "Bow" ~ 15,
    sub_basin == "Lower South Saskatchewan" ~ 16,
    sub_basin == "Red Deer"~ 17,
    sub_basin == "Upper South Saskatchewan"~ 18,
    TRUE ~ NA_real_
  ))%>%
  mutate(BLOCK_BASIN_LOCALITY = case_when(
    basin == "AR" ~ 1,
    basin == "LSN" ~ 2,
    basin == "NS" ~ 3,
    basin == "SS" ~ 4))%>%
  select(block,BLOCK_SUBBASIN_LOCALITY,BLOCK_BASIN_LOCALITY)

write_csv(ChoiceSetDesigns,"./Deriveddata/Block_locality.csv")




