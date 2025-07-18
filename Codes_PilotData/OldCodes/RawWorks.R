df_filtered <- df_f %>%
  mutate(across(where(is.character), ~na_if(., ""))) %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), NA, .)))


# Placeholder for storing results
final_result <- list()

# Loop through each row of df_filtered
for (i in 1:nrow(df_filtered)) {
  # Select the i-th row
  df_filtered_row <- df_filtered[i, , drop = FALSE] 
  
  # Filter out columns where all values are NA
  df_filtered_row <- df_filtered_row %>%
    select(where(~ !all(is.na(.))))
  
  # Ensure consistent data type (e.g., all as character)
  df_filtered_row <- df_filtered_row %>%
    mutate(across(matches(".*B.*"), as.character))
  
  # Check if there are columns containing "B" before pivoting
  if (any(str_detect(names(df_filtered_row), "B"))) {
    # Pivot longer for columns with "B" in their names
    df_long <- df_filtered_row %>%
      pivot_longer(
        cols = contains("B"),   # Select columns with "B" in their names, Because B contain all choice based observation 
        names_to = "name",  # Name for the pivoted column names - This is exactly that Danny use to identify the choice, 
        #in later I dropped this variable but in any case we can cross check based on this
        values_to = "value"     # pivoted values, this colom as mix of all the observation For example bid, policy area, vote 
      ) %>%
      mutate(
        block_number = str_extract(name, "\\d+"), # here will split information in name colom into seprate variable where handling make easy
        #A_term = if_else(str_detect(name, "A"), "A", NA_character_), # Only "A" if "A" is found
        #block_number = if_else(str_detect(name, "A"), paste(block_number, "A", sep = ""), block_number), # Merge block_number and "A" term
        river_basin = str_extract(name, "NS|SS|LSN|AR") # Extract river basin
      ) %>%
      #select(-A_term)%>%
      group_by(block_number) %>%
      mutate(
        river_basin = if_else(is.na(river_basin), first(na.omit(river_basin)), river_basin),
        name = if_else(
          is.na(str_extract(name, "NS|SS|LSN|AR")),
          paste0(str_replace(name, "B\\d+", paste0(river_basin, "_", block_number))),
          name
        )
      ) %>%
      mutate(
        name = str_replace(name, "(PC_)(.*)", "\\2_PC")  # Move PC_ to the end
      )%>%
      mutate(                     #Deal with some inconsisteny of orogainal naming
        choice_number = sapply(
          str_split(name, "_"), 
          function(x) {
            second_part <- x[3] # The part we want to evaluate
            str_extract(second_part, "\\d+A?|\\d+") # Extract the number with optional "A"
          }
        )
      )
    
    
    # Store the result for the current row in the final_result list
    final_result[[i]] <- df_long
  } else {
    # If no columns containing "B", store the original row (no changes made)
    final_result[[i]] <- df_filtered_row
  }
}

# Combine all the row-wise results into a single dataframe
final_df <- bind_rows(final_result)



final_df <- final_df %>%
  mutate(
    block_number = str_extract(name, "(?<=^B)\\d+A?")  # Capture number with optional 'A' after 'B'
  )

final_df <- final_df %>%
  mutate(
    variable = sub("^.*?_.*?_(.*)$", "\\1", name),  # Extract everything after the second underscore
  )

final_df <- final_df %>%
  mutate(
    choice_number = sub("^[^_]*_([^_]+).*", "\\1", name)  # Extract text between the first and second underscore
  )


# extract cost varibale as dataframe
df_cost <- final_df %>%
  filter(variable == "COST") %>%
  select(CaseId,variable, value,name,block_number,choice_number) %>%
  select(-variable) %>%
  rename(Cost = value) %>%
  mutate(Cost = as.numeric(gsub("\\$", "", Cost)))%>%  # Remove $ and convert to numeric
  rename(COST=Cost)


# extract vote variable as data frame
df_vote <- final_df%>%
  filter(variable=="VOTE")%>%
  select(CaseId,variable, value,name,block_number,choice_number)%>%
  select(-variable)%>%
  rename(VOTE=value)

# extract current WQ variable as data frame
df_WQ_current <- final_df%>%
  filter(variable=="CURRENT_AVERAGE")%>%
  select(CaseId,variable, value,name,block_number,choice_number)%>%
  select(-variable)%>%
  rename(CURRENT_AVERAGE=value)

# extract policy WQ variable as data frame
df_WQ_policy <- final_df%>%
  filter(variable=="POLICY_AVERAGE")%>%
  select(CaseId,variable, value,name,block_number,choice_number)%>%
  select(-variable)%>%
  rename(POLICY_AVERAGE=value)

# extract policy area as data frame
df_WQ_policy_areakm <- final_df%>%
  filter(variable=="POLICY_SIZE_KM")%>%
  select(CaseId,variable, value,name,block_number,choice_number)%>%
  select(-variable)%>%
  rename(POLICY_SIZE_KM=value)



# extract policy area % as data frame
df_WQ_policy_percent <- final_df%>%
  filter(variable=="POLICY_SIZE_PERCENT")%>%
  select(CaseId,variable, value,name,block_number,choice_number)%>%
  select(-variable)%>%
  rename(POLICY_SIZE_PERCENT = value)


# extract # extract WQ = 5 as % as data frameWQ = 1 as % as data frame
df_WQ_current_1 <- final_df%>%
  filter(variable=="CURRENT_1")%>%
  select(CaseId,variable, value,name,block_number,choice_number)%>%
  select(-variable)%>%
  rename(CURRENT_1=value)

# extract # extract WQ = 5 as % as data frameWQ = 2 as % as data frame
df_WQ_current_2 <- final_df%>%
  filter(variable=="CURRENT_2")%>%
  select(CaseId,variable, value,name,block_number,choice_number)%>%
  select(-variable)%>%
  rename(CURRENT_2=value)


# extract # extract WQ = 5 as % as data frameWQ = 3 as % as data frame
df_WQ_current_3 <- final_df%>%
  filter(variable=="CURRENT_3")%>%
  select(CaseId,variable, value,name,block_number,choice_number)%>%
  select(-variable)%>%
  rename(CURRENT_3=value)



# extract # extract WQ = 5 as % as data frameWQ = 4 as % as data frame
df_WQ_current_4 <- final_df%>%
  filter(variable=="CURRENT_4")%>%
  select(CaseId,variable, value,name,block_number,choice_number)%>%
  select(-variable)%>%
  rename(CURRENT_4=value)


# extract current WQ = 5 as % as data frame
df_WQ_current_5 <- final_df%>%
  filter(variable=="CURRENT_5")%>%
  select(CaseId,variable, value,name,block_number,choice_number)%>%
  select(-variable)%>%
  rename(CURRENT_5=value)


# extract WQ = 1 as % in policy data frame
df_WQ_policy_1 <- final_df%>%
  filter(variable=="POLICY_1")%>%
  select(CaseId,variable, value,name,block_number,choice_number)%>%
  select(-variable)%>%
  rename(POLICY_1=value)

# extract WQ = 2 as % in policy data frame
df_WQ_policy_2 <- final_df%>%
  filter(variable=="POLICY_2")%>%
  select(CaseId,variable, value,name,block_number,choice_number)%>%
  select(-variable)%>%
  rename(POLICY_2=value)


# extract WQ = 3 as % in policy data frame
df_WQ_policy_3 <- final_df%>%
  filter(variable=="POLICY_3")%>%
  select(CaseId,variable, value,name,block_number,choice_number)%>%
  select(-variable)%>%
  rename(POLICY_3=value)


# extract WQ = 4 as % in policy data frame
df_WQ_policy_4 <- final_df%>%
  filter(variable=="POLICY_4")%>%
  select(CaseId,variable, value,name,block_number,choice_number)%>%
  select(-variable)%>%
  rename(POLICY_4=value)


# extract WQ = 5 as % in policy data frame
df_WQ_policy_5 <- final_df%>%
  filter(variable=="POLICY_5")%>%
  select(CaseId,variable, value,name,block_number,choice_number)%>%
  select(-variable)%>%
  rename(POLICY_5=value)

# Then left joint those variable based on respondent id, block id, river basin and other matching variables 

df_all <- df_cost%>%
  left_join(df_vote)%>%
  left_join(df_WQ_current)%>%
  left_join(df_WQ_policy)%>%
  left_join(df_WQ_policy_areakm)%>%
  left_join(df_WQ_policy_percent)%>%
  left_join(df_WQ_current_1)%>%
  left_join(df_WQ_current_2)%>%
  left_join(df_WQ_current_3)%>%
  left_join(df_WQ_current_4)%>%
  left_join(df_WQ_current_5)%>%
  left_join(df_WQ_policy_1)%>%
  left_join(df_WQ_policy_2)%>%
  left_join(df_WQ_policy_3)%>%
  left_join(df_WQ_policy_4)%>%
  left_join(df_WQ_policy_5)%>%
  mutate(ASC=1)%>%
  mutate(SQ=0)



t <- final_df%>%
  select(CaseId,name,value,block_number)


































# Example df1 with columns 'col_1', 'col_2', 'col_3'
df1 <- data.frame(
  col_1 = c(1, 2, 3),
  col_2 = c(4, 5, 6),
  col_3 = c(7, 8, 9)
)

# Example df2 with original names and new names
df2 <- data.frame(
  original = c("col_1", "col_2", "col_3"),
  new_name = c("COLUMN_1", "COLUMN_2", "COLUMN_3")
)

# Print original df1
print("Original df1:")
print(df1)

# Function to rename columns in df1 based on df2 mapping
rename_columns <- function(df1, df2) {
  # Create a named vector mapping original names to new names
  rename_map <- setNames(df2$new_name, df2$original)
  
  # Loop through and rename columns in df1 if they exist in rename_map
  for (col in names(rename_map)) {
    if (col %in% names(df1)) {
      col_index <- which(names(df1) == col)
      names(df1)[col_index] <- rename_map[col]
    }
  }
  
  return(df1)
}

# Apply the renaming function
df1 <- rename_columns(df1, df2)

# Print the renamed df1
print("Renamed df1:")
print(df1)

















library(dplyr)

# Example dataframe df1
df1 <- data.frame(
  col_1 = c(1, 2, 3),
  col_2 = c(4, 5, 6),
  col_3 = c(7, 8, 9)
)

# Example dataframe df2 with mapping of new column names
df2 <- data.frame(
  original = c("col_1", "col_2", "col_3"),
  new_name = c("COLUMN_1", "COLUMN_2", "COLUMN_3")
)

# Step 1: Verify the column names in df1
print(names(df1))  # Check the column names in df1

# Step 2: Create a named vector for renaming (ensure the original names match df1's column names)
rename_map <- setNames(df2$new_name, df2$original)

# Step 3: Print the rename map to ensure correctness
print(rename_map)

# Step 4: Rename columns in df1 using the rename map
df1 <- df1 %>%
  rename(!!!rename_map)

# Step 5: Verify the result
print(df1)





















t <- read.csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/R/Chapter III/Output/column_names.csv")




t <- t %>%
  select(x) %>%
  filter(x != "CaseId", x != "SUB_BASIN")

t$B_number <- sub(".*B(\\d+[A]?)_(\\d+).*", "\\1_\\2", t$x)


filtered_tt <- t %>%
  filter(grepl("117", x))

filtered_tt <- t %>%
  filter(grepl("1_1", x) & grepl("AR", x))


# Using mutate with ifelse to change values
# Using mutate with case_when to change values


# Define the named list of patterns to replace
pattern_groups <- list(
  "B17_1A" = c(
    "AR_B17_1A_COST", "AR_B17_1ACURRENT_1", "AR_B17_1ACURRENT_2", "AR_B17_1ACURRENT_3",
    "AR_B17_1ACURRENT_4", "AR_B17_1ACURRENT_5", "AR_B17_1ACURRENT_AVERAGE", "AR_B17_1APOLICY_1",
    "AR_B17_1APOLICY_2", "AR_B17_1APOLICY_3", "AR_B17_1APOLICY_4", "AR_B17_1APOLICY_5",
    "AR_B17_1APOLICY_AVERAGE", "AR_B17_1APOLICY_SIZE_KM", "AR_B17_1APOLICY_SIZE_PERCENT",
    "PC_AR_B17_1A"
  ),
  "B17_2A" = c(
    "AR_B17_2A_COST", "AR_B17_2ACURRENT_1", "AR_B17_2ACURRENT_2", "AR_B17_2ACURRENT_3",
    "AR_B17_2ACURRENT_4", "AR_B17_2ACURRENT_5", "AR_B17_2ACURRENT_AVERAGE", "AR_B17_2APOLICY_1",
    "AR_B17_2APOLICY_2", "AR_B17_2APOLICY_3", "AR_B17_2APOLICY_4", "AR_B17_2APOLICY_5",
    "AR_B17_2APOLICY_AVERAGE", "AR_B17_2APOLICY_SIZE_KM", "AR_B17_2APOLICY_SIZE_PERCENT",
    "PC_AR_B17_2A"
  ),
  
  "B22_1A" = c("AR_B22_1A_COST","AR_B22_1ACURRENT_1","AR_B22_1ACURRENT_2","AR_B22_1ACURRENT_3",
               "AR_B22_1ACURRENT_4","AR_B22_1ACURRENT_5","AR_B22_1ACURRENT_AVERAGE","AR_B22_1APOLICY_1",
               "AR_B22_1APOLICY_2","AR_B22_1APOLICY_3","AR_B22_1APOLICY_4","AR_B22_1APOLICY_5",
               "AR_B22_1APOLICY_AVERAGE","AR_B22_1APOLICY_SIZE_KM","AR_B22_1APOLICY_SIZE_PERCENT",
               "PC_AR_B22_1A"
  ),
  
  "B22_2A" = c("AR_B22_2A_COST","AR_B22_2ACURRENT_1","AR_B22_2ACURRENT_2","AR_B22_2ACURRENT_3",
               "AR_B22_2ACURRENT_4","AR_B22_2ACURRENT_5","AR_B22_2ACURRENT_AVERAGE","AR_B22_2APOLICY_1",
               "AR_B22_2APOLICY_2","AR_B22_2APOLICY_3","AR_B22_2APOLICY_4","AR_B22_2APOLICY_5",
               "AR_B22_2APOLICY_AVERAGE","AR_B22_2APOLICY_SIZE_KM","AR_B22_2APOLICY_SIZE_PERCENT",
               "PC_AR_B22_2A"
  ),
  
  "B23_2A" = c("AR_B23_2A_COST","AR_B23_2ACURRENT_1","AR_B23_2ACURRENT_2","AR_B23_2ACURRENT_3",
               "AR_B23_2ACURRENT_4","AR_B23_2ACURRENT_5","AR_B23_2ACURRENT_AVERAGE","AR_B23_2APOLICY_1",
               "AR_B23_2APOLICY_2","AR_B23_2APOLICY_3","AR_B23_2APOLICY_4","AR_B23_2APOLICY_5",
               "AR_B23_2APOLICY_AVERAGE","AR_B23_2APOLICY_SIZE_KM","AR_B23_2APOLICY_SIZE_PERCENT",
               "PC_AR_B23_2A"
  ),
  "B23_1A" = c("AR_B23_1A_COST","AR_B23_1ACURRENT_1","AR_B23_1ACURRENT_2","AR_B23_1ACURRENT_3",
               "AR_B23_1ACURRENT_4","AR_B23_1ACURRENT_5","AR_B23_1ACURRENT_AVERAGE","AR_B23_1APOLICY_1",
               "AR_B23_1APOLICY_2","AR_B23_1APOLICY_3","AR_B23_1APOLICY_4","AR_B23_1APOLICY_5",
               "AR_B23_1APOLICY_AVERAGE","AR_B23_1APOLICY_SIZE_KM","AR_B23_1APOLICY_SIZE_PERCENT",
               "PC_AR_B23_1A"
  ),
  "B24_1A" = c("AR_B24_1A_COST","AR_B24_1ACURRENT_1","AR_B24_1ACURRENT_2","AR_B24_1ACURRENT_3",
               "AR_B24_1ACURRENT_4","AR_B24_1ACURRENT_5","AR_B24_1ACURRENT_AVERAGE",
               "AR_B24_1APOLICY_1","AR_B24_1APOLICY_2","AR_B24_1APOLICY_3","AR_B24_1APOLICY_4",
               "AR_B24_1APOLICY_5","AR_B24_1APOLICY_AVERAGE","AR_B24_1APOLICY_SIZE_KM","AR_B24_1APOLICY_SIZE_PERCENT",
               "PC_AR_B24_1A"
  ),
  "B24_2A" = c("AR_B24_2A_COST","AR_B24_2ACURRENT_1","AR_B24_2ACURRENT_2","AR_B24_2ACURRENT_3",
               "AR_B24_2ACURRENT_4","AR_B24_2ACURRENT_5","AR_B24_2ACURRENT_AVERAGE",
               "AR_B24_2APOLICY_1","AR_B24_2APOLICY_2","AR_B24_2APOLICY_3","AR_B24_2APOLICY_4",
               "AR_B24_2APOLICY_5","AR_B24_2APOLICY_AVERAGE","AR_B24_2APOLICY_SIZE_KM","AR_B24_2APOLICY_SIZE_PERCENT",
               "PC_AR_B24_2A"
  ),
  "B25_1A" = c("AR_B25_1A_COST","AR_B25_1ACURRENT_1","AR_B25_1ACURRENT_2","AR_B25_1ACURRENT_3",
               "AR_B25_1ACURRENT_4","AR_B25_1ACURRENT_5","AR_B25_1ACURRENT_AVERAGE",
               "AR_B25_1APOLICY_1","AR_B25_1APOLICY_2","AR_B25_1APOLICY_3","AR_B25_1APOLICY_4",
               "AR_B25_1APOLICY_5","AR_B25_1APOLICY_AVERAGE","AR_B25_1APOLICY_SIZE_KM",
               "AR_B25_1APOLICY_SIZE_PERCENT",
               "PC_AR_B25_1A"
  ),
  "B25_2A" = c("AR_B25_2A_COST","AR_B25_2ACURRENT_1","AR_B25_2ACURRENT_2","AR_B25_2ACURRENT_3",
               "AR_B25_2ACURRENT_4","AR_B25_2ACURRENT_5","AR_B25_2ACURRENT_AVERAGE",
               "AR_B25_2APOLICY_1","AR_B25_2APOLICY_2","AR_B25_2APOLICY_3","AR_B25_2APOLICY_4",
               "AR_B25_2APOLICY_5","AR_B25_2APOLICY_AVERAGE","AR_B25_2APOLICY_SIZE_KM",
               "AR_B25_2APOLICY_SIZE_PERCENT",
               "PC_AR_B25_2A"
  ),
  "B3_1R2" = c("AR_B3_1_COSTR2","AR_B3_1CURRENT_1R2","AR_B3_1CURRENT_2R2","AR_B3_1CURRENT_3R2",
               "AR_B3_1CURRENT_4R2","AR_B3_1CURRENT_5R2","AR_B3_1CURRENT_AVERAGER2","AR_B3_1POLICY_1R2",
               "AR_B3_1POLICY_2R2","AR_B3_1POLICY_3R2","AR_B3_1POLICY_4R2","AR_B3_1POLICY_5R2",
               "AR_B3_1POLICY_AVERAGER2","AR_B3_1POLICY_SIZE_KMR2","AR_B3_1POLICY_SIZE_PERCENTR2",
               "PC_AR_B3_1R2"
  ),
  "B3_1R1" = c("AR_B3_1_COSTR1","AR_B3_1CURRENT_1R1","AR_B3_1CURRENT_2R1","AR_B3_1CURRENT_3R1",
              "AR_B3_1CURRENT_4R1","AR_B3_1CURRENT_5R1","AR_B3_1CURRENT_AVERAGER1","AR_B3_1POLICY_1R1",
              "AR_B3_1POLICY_2R1","AR_B3_1POLICY_3R1","AR_B3_1POLICY_4R1","AR_B3_1POLICY_5R1",
              "AR_B3_1POLICY_AVERAGER1","AR_B3_1POLICY_SIZE_KMR1","AR_B3_1POLICY_SIZE_PERCENTR1",
              "PC_AR_B3_1R1"
  ),
  "B3_2R1" = c("AR_B3_2_COSTR1","AR_B3_2CURRENT_1R1","AR_B3_2CURRENT_2R1","AR_B3_2CURRENT_3R1",
              "AR_B3_2CURRENT_4R1","AR_B3_2CURRENT_5R1","AR_B3_2CURRENT_AVERAGER1","AR_B3_2POLICY_1R1",
              "AR_B3_2POLICY_2R1","AR_B3_2POLICY_3R1","AR_B3_2POLICY_4R1","AR_B3_2POLICY_5R1",
              "AR_B3_2POLICY_AVERAGER1","AR_B3_2POLICY_SIZE_KMR1","AR_B3_2POLICY_SIZE_PERCENTR1",
              "PC_AR_B3_2R1"
  ),
  "B3_2R2" = c("AR_B3_2_COSTR2","AR_B3_2CURRENT_1R2","AR_B3_2CURRENT_2R2","AR_B3_2CURRENT_3R2",
               "AR_B3_2CURRENT_4R2","AR_B3_2CURRENT_5R2","AR_B3_2CURRENT_AVERAGER2","AR_B3_2POLICY_1R2",
               "AR_B3_2POLICY_2R2","AR_B3_2POLICY_3R2","AR_B3_2POLICY_4R2","AR_B3_2POLICY_5R2",
               "AR_B3_2POLICY_AVERAGER2","AR_B3_2POLICY_SIZE_KMR2","AR_B3_2POLICY_SIZE_PERCENTR2",
               "PC_AR_B3_2R2"
  ),
  
  "B7_3A" = c("AR_B7_3_COST","AR_B7_3CURRENT_1","AR_B7_3CURRENT_2","AR_B7_3CURRENT_3","AR_B7_3CURRENT_4",     #### clarify with Danny
               "AR_B7_3CURRENT_5","AR_B7_3CURRENT_AVERAGE","AR_B7_3POLICY_1","AR_B7_3POLICY_2",
               "AR_B7_3POLICY_3","AR_B7_3POLICY_4","AR_B7_3POLICY_5","AR_B7_3POLICY_AVERAGE",
               "AR_B7_3POLICY_SIZE_KM","AR_B7_3POLICY_SIZE_PERCENT","PC_AR_B7_3"
               
  ),
  "B56_1" = c("B56_COST","B56CURRENT_1","B56CURRENT_2","B56CURRENT_3","B56CURRENT_4",
              "B56CURRENT_5","B56CURRENT_AVERAGE","B56POLICY_1","B56POLICY_2","B56POLICY_3",
              "B56POLICY_4","B56POLICY_5","B56POLICY_AVERAGE","B56POLICY_SIZE_KM",
              "B56POLICY_SIZE_PERCENT", "PC_LSN_B56_1"
              
  )
  )



# Flatten the pattern_groups into a single dataframe for matching
patterns_df <- bind_rows(
  lapply(names(pattern_groups), function(name) {
    data.frame(pattern = pattern_groups[[name]], replacement = name, stringsAsFactors = FALSE)
  })
)

# Perform the transformation
t <- t %>%
  left_join(patterns_df, by = c("x" = "pattern")) %>%
  mutate(B_number_new = ifelse(!is.na(replacement), replacement, B_number))


# Summarize how many rows fall under each number after B
summary_df <- as.data.frame(table(t$B_number_new))

# Rename columns for clarity
colnames(summary_df) <- c("B_number", "row_count")




"B56_COST","B56CURRENT_1","B56CURRENT_2","B56CURRENT_3","B56CURRENT_4"
"B56CURRENT_5","B56CURRENT_AVERAGE","B56POLICY_1","B56POLICY_2","B56POLICY_3",
"B56POLICY_4","B56POLICY_5","B56POLICY_AVERAGE","B56POLICY_SIZE_KM",
"B56POLICY_SIZE_PERCENT", "PC_LSN_B56_1"

# Print the summary
print(summary_df)




# Example dataframe
t <- data.frame(
  x = c("AR_B1_1CURRENT_2", "AR_B1_2CURRENT_3", "B104A_1_COST", "LSN_B12_4POLICY_1")
)

# Extract the number after 'B' (handling both with and without prefix)
t$B_number <- sub(".*B(\\d+_?\\d+).*", "\\1", t$x)

# View the result
print(t)






t <- t%>%
  select(x)

filtered_rows <- t[!grepl("B", t[["x"]]), ]



t <- df2%>%
  select(PC_LSN_NS_2,PC_32_1)



tt <- t%>%
  select(RespondentID,BLK_NUMBER,CHOICE_NUMBER,POLICY_SIZE_KM,POLICY_SIZE_PERCENT,COST,VOTE,ASC,SQ)\

tt <- database%>%
  select(RespondentID,BLK_NUMBER,CHOICE_NUMBER,POLICY_SIZE_KM,POLICY_SIZE_PERCENT,COST,VOTE,ASC,SQ)


tt <- tt%>%
  filter(RespondentID==64)


library(dplyr)
t <- df%>%
  select(RespondentID,Referendum,Cost,Vote,HUC4BCG_scenario3,ASC,SQ)

tt <- apollo_modeChoiceData%>%
  select(RespondentID,BLK_NUMBER,CHOICE_NUMBER,POLICY_SIZE_KM,POLICY_SIZE_PERCENT,COST,VOTE,ASC,SQ)


tt <- tt %>%
  arrange(RespondentID, BLK_NUMBER,CHOICE_NUMBER) # First sort by Group, then by Value within each group



t <- df_all%>%
  select(RespondentID,POLICY_SIZE_KM,POLICY_SIZE_PERCENT,COST,VOTE,ASC,SQ)


t <- t %>%
  arrange(RespondentID,CHOICE_NUMBER) # First sort by Group, then by Value within each group



t <- database%>%
  filter(`{Case ID}`==23)

t <- t[, grepl("PC", names(t))]

t <- t %>%
  select(where(~ !all(is.na(.))))




# Placeholder for storing results
final_result <- list()

# Loop through each row of df_filtered
for (i in 1:nrow(df_filtered)) {
  # Select the i-th row
  df_filtered_row <- df_filtered[i, , drop = FALSE] 
  
  # Filter out columns where all values are NA
  df_filtered_row <- df_filtered_row %>%
    select(where(~ !all(is.na(.))))
  
  # Ensure consistent data type (e.g., all as character)
  df_filtered_row <- df_filtered_row %>%
    mutate(across(matches(".*B.*"), as.character))
  
  # Check if there are columns containing "B" before pivoting
  if (any(str_detect(names(df_filtered_row), "B"))) {
    # Pivot longer for columns with "B" in their names
    df_long <- df_filtered_row %>%
      pivot_longer(
        cols = contains("B"),   # Select columns with "B" in their names, Because B contain all choice based observation 
        names_to = "name",  # Name for the pivoted column names - This is exactly that Danny use to identify the choice, 
        #in later I dropped this variable but in any case we can cross check based on this
        values_to = "value"     # pivoted values, this colom as mix of all the observation For example bid, policy area, vote 
      ) %>%
      mutate(
        block_number = str_extract(name, "\\d+"), # here will split information in name colom into seprate variable where handling make easy
        #A_term = if_else(str_detect(name, "A"), "A", NA_character_), # Only "A" if "A" is found
        #block_number = if_else(str_detect(name, "A"), paste(block_number, "A", sep = ""), block_number), # Merge block_number and "A" term
        river_basin = str_extract(name, "NS|SS|LSN|AR") # Extract river basin
      ) %>%
      #select(-A_term)%>%
      group_by(block_number) %>%
      mutate(
        river_basin = if_else(is.na(river_basin), first(na.omit(river_basin)), river_basin),
        name = if_else(
          is.na(str_extract(name, "NS|SS|LSN|AR")),
          paste0(str_replace(name, "B\\d+", paste0(river_basin, "_", block_number))),
          name
        )
      ) %>%
      ungroup() %>%
      mutate(
        status = if_else(str_detect(name, locality), "local", "nonlocal") # Determin the choice is local oe non local based on their sub basin and choice's dub basin code
      )%>%
      mutate(
        name = str_replace(name, "(PC_)(.*)", "\\2_PC")  # Move PC_ to the end
      )%>%
      mutate(                     #Deal with some inconsisteny of orogainal naming
        choice_number = sapply(
          str_split(name, "_"), 
          function(x) {
            second_part <- x[3] # The part we want to evaluate
            str_extract(second_part, "\\d+A?|\\d+") # Extract the number with optional "A"
          }
        )
      )
    
    
    # Store the result for the current row in the final_result list
    final_result[[i]] <- df_long
  } else {
    # If no columns containing "B", store the original row (no changes made)
    final_result[[i]] <- df_filtered_row
  }
}

# Combine all the row-wise results into a single dataframe
final_df <- bind_rows(final_result)