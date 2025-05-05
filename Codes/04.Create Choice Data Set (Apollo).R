########################################################################################
# Description: This code aims to clean the survey data and set the data ready for Apollo
# First it create choice data set (respondent id, choice id, vote, treatment, cost, 
# other design variable) and later it add the respondent characteristics (create with
# using 02. Respondent characteristics. R) to complete the choice dataset for apallo
########################################################################################

# Clean the memory
rm(list = ls())  # Removes all objects in the environment


# Read raw data file from CHAISR
df <- read_sav("./Rawdata/Water_Quality.sav")

# In original file it has create a column for each data point recorded. For example, it create separate colom for 118 block desperately 
# along with all other relevant parameters for each block (i.e. Policy area, baseline WQ, improved WQ). SO in this portion of the code we 
# work on those variables of each choice that fit into Appollo format


# First we extract the all variables that varied with choice. This is based on the naming patterns of each 
# variable
df1 <- df[, grepl("COST", names(df)) |           # Original data file the name the column that contain the cost of each policy scenario as "AR_B1_1_COST", 
                                                 #where AR_B1_1 varied depend on the block and the river basin. SO we extarct the all the column with "COST" term
                     names(df) == "CaseId" |     # Respondents ID - use to map the choices for each respondents
                     names(df) == "VERSION" |    # Version - use to map the choices for each respondents
                     names(df) == "BASIN" |      # Basin - use to map the choices for each respondents
                     names(df) == "SUB_BASIN" |  # Sub basin - use to map the choices for each respondents
                     names(df) == "NON_LOCAL" |  # Basin code for their non-local basin
                     names(df) == "LOCAL_AR" |   # A local watershed, which does  include your home A non-local watershed (1), which does not include your home(2)
                     names(df) == "CONDITION" |  # WQ survey = 1, General survey =2
                     names(df) == "TREATMENT" |  # Spatial =1, WQ =2
            
            
                     grepl("PC", names(df))   |               # PC - vote scenario 1 - yes 2-no. 
                     grepl("POLICY_AVERAGE", names(df))|      # Policy Average WQ 
                     grepl("CURRENT_AVERAGE", names(df))|     # Current/Baseline Average WQ
                     grepl("POLICY_SIZE_KM", names(df))|      # Policy area in SQKM
                     grepl("POLICY_SIZE_PERCENT", names(df))| # Policy area in %
                     grepl("CURRENT_1", names(df))|           # WQ_1 % in current level
                     grepl("CURRENT_2", names(df))|           # WQ_2 % in current level
                     grepl("CURRENT_3", names(df))|           # WQ_3 % in current level
                     grepl("CURRENT_4", names(df))|           # WQ_4 % in current level
                     grepl("CURRENT_5", names(df))|           # WQ_5 % in current level
                     grepl("IMAGE_CURRENT", names(df)) |      # image current
                     grepl("POLICY_1", names(df))|            # WQ_1 % in policy
                     grepl("POLICY_2", names(df))|            # WQ_2 % in policy
                     grepl("POLICY_3", names(df))|            # WQ_3 % in policy
                     grepl("POLICY_4", names(df))|            # WQ_4 % in policy
                     grepl("POLICY_5", names(df))|            # WQ_5 % in policy
                     grepl("IMAGE_POLICY", names(df))         # image policy
                     ]%>%
         rename(spatial_1=BASIN,
         spatial_2 = SUB_BASIN)
                     #names(df)=="SUB_BASIN" ] # Sub Basin - The codes for each sub basin needs to be clarify with Danny, where each sub absin code as unique number (18)


#coding errors - missing observation so add variabel manually and may be reove woth final data

#df1 <- df1%>%
  #mutate(B117_2POLICY_SIZE_PERCENT="6%") # this is missing filed in pilot data and Danny will fix it in final version 


df1 <- df1%>%
  select(-PC_32_1, -PC_LSN_NS_2) # confirmed with Danny mistake in coding and fix it in final version


# open the data frame with corrected coloum names (05.Create_consistent_ColNmeas_for_choices.R). 
# In original Danny has used different patents to name choices
# I have manually brings to a consistent pattern to handle them easy
df2 <- read_csv("./Deriveddata/corrected_colnames.csv")%>%
  select(choice_name,vaiable_name)%>%
  rename(original=choice_name,
         new_name=vaiable_name)%>%
  filter(original!="LSN_B12_1IMAGE_CURRENT")

# rename the original data file name by matching the pattern
df1 <- df1 %>%
  rename_with(~ifelse(!is.na(match(., df2$original)), df2$new_name[match(., df2$original)], .), .cols = names(df1))


# Take only sample of data - for initial attempts later can remove this chunk

df_filtered <- df1%>%
  slice(1:1004)


# Now df_filtered data frame has all the choices with above filtered data, but most of those column are empty for 
# as per respondent receive only two block from their local and non-local river basin. So all other remain NA,
# So I create a loop that operate row wise where identify column with observed data for each respondent
# Here loop go over each respondent (in original data file observation for each respondents summarized into 
# one row)

# convert to numeric
df_filtered <- df_filtered %>%
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
  
  # Ensure consistent data type
  df_filtered_row <- df_filtered_row %>%
    mutate(across(matches(".*B.*"), as.character))
  
  # Check if there are columns containing "B" before pivoting
  if (any(str_detect(names(df_filtered_row), "B"))) {
    # Pivot longer for columns with "B" in their names
    df_long <- df_filtered_row %>%
      pivot_longer(
        cols = contains("B"),   # Select columns with "B" in their names
        names_to = "name",       # Name for the pivoted column names
        values_to = "value"      # Pivoted values
      )
    
    # Store the result for the current row in the final_result list
    final_result[[i]] <- df_long
  } else {
    # If no columns containing "B", store the original row (no changes made)
    final_result[[i]] <- df_filtered_row
  }
}

# Combine all the row-wise results into a single data frame. This data frame has all data in lone format
final_df <- bind_rows(final_result)


# extract block number and choice number from the choice name we created (consistent format)

final_df <- final_df %>%
  mutate(
    block_number = str_extract(name, "(?<=^B)\\d+A?"),  # Capture number with optional 'A' after 'B'
    choice_number = sub("^[^_]*_([^_]+).*", "\\1", name),  # Extract text between the first and second underscore
    VAR_NAME = sub("^.*?_.*?_(.*)$", "\\1", name),  # Extract everything after the second underscore
    
  )

# Here I extract all choice specific variables into different subset 
# extract cost variable as data frame
df_cost <- final_df %>%
  filter(VAR_NAME == "COST") %>%
  select(CaseId,name,CONDITION,VERSION,TREATMENT,spatial_1,spatial_2,NON_LOCAL,LOCAL_AR,value,block_number,choice_number, -VAR_NAME) %>%
  select(-name) %>%
  rename(Cost = value,
         BASIN = spatial_1,
         SUB_BASIN = spatial_2) %>%
  mutate(Cost = as.numeric(gsub("\\$", "", Cost)))%>%  # Remove $ and convert to numeric
  rename(COST=Cost)


# local watershed include home (1) or non-local watershed which does not include home (2)
df_vote <- final_df%>%
  filter(VAR_NAME=="VOTE")%>%
  select(CaseId,name, value,block_number,choice_number, -VAR_NAME) %>%
  select(-name)%>%
  rename(VOTE=value)%>%
  mutate(VOTE = as.numeric(VOTE))


# extract current WQ name as data frame
df_WQ_current <- final_df%>%
  filter(VAR_NAME=="CURRENT_AVERAGE")%>%
  select(CaseId,name, value,block_number,choice_number, -VAR_NAME) %>%
  select(-name)%>%
  rename(CURRENT_AVERAGE=value)%>%
  mutate(CURRENT_AVERAGE = as.numeric(CURRENT_AVERAGE))

# extract policy WQ name as data frame
df_WQ_policy <- final_df%>%
  filter(VAR_NAME=="POLICY_AVERAGE")%>%
  select(CaseId,name, value,block_number,choice_number, -VAR_NAME) %>%
  select(-name)%>%
  rename(POLICY_AVERAGE=value)%>%
  mutate(POLICY_AVERAGE = as.numeric(POLICY_AVERAGE))

# extract policy area as data frame
df_WQ_policy_areakm <- final_df%>%
  filter(VAR_NAME=="POLICY_SIZE_KM")%>%
  select(CaseId,name, value,block_number,choice_number, -VAR_NAME) %>%
  select(-name)%>%
  rename(POLICY_SIZE_KM=value)%>%
  mutate(
    POLICY_SIZE_KM = as.numeric(gsub("[^0-9\\.]", "", POLICY_SIZE_KM)) # Remove non-numeric characters
  )

# extract policy area % as data frame
df_WQ_policy_percent <- final_df%>%
  filter(VAR_NAME=="POLICY_SIZE_PERCENT")%>%
  select(CaseId,name, value,block_number,choice_number, -VAR_NAME) %>%
  select(-name)%>%
  rename(POLICY_SIZE_PERCENT = value)%>%
  mutate(POLICY_SIZE_PERCENT = as.numeric(gsub("%", "", POLICY_SIZE_PERCENT)) / 100) # Remove '%' and divide by 100

# extract # extract WQ = 5 as % as data frameWQ = 1 as % as data frame
df_WQ_current_1 <- final_df%>%
  filter(VAR_NAME=="CURRENT1")%>%
  select(CaseId,name, value,block_number,choice_number, -VAR_NAME) %>%
  select(-name)%>%
  rename(CURRENT_1=value)%>%
  mutate(CURRENT_1 = as.numeric(gsub("%", "", CURRENT_1)) / 100) # Remove '%' and divide by 100


# extract # extract WQ = 5 as % as data frameWQ = 2 as % as data frame
df_WQ_current_2 <- final_df%>%
  filter(VAR_NAME=="CURRENT2")%>%
  select(CaseId,name, value,block_number,choice_number, -VAR_NAME) %>%
  select(-name)%>%
  rename(CURRENT_2=value)%>%
  mutate(CURRENT_2 = as.numeric(gsub("%", "", CURRENT_2)) / 100) # Remove '%' and divide by 100


# extract # extract WQ = 5 as % as data frameWQ = 3 as % as data frame
df_WQ_current_3 <- final_df%>%
  filter(VAR_NAME=="CURRENT3")%>%
  select(CaseId,name, value,block_number,choice_number, -VAR_NAME) %>%
  select(-name)%>%
  rename(CURRENT_3=value)%>%
  mutate(CURRENT_3 = as.numeric(gsub("%", "", CURRENT_3)) / 100) # Remove '%' and divide by 100

# extract # extract WQ = 5 as % as data frameWQ = 4 as % as data frame
df_WQ_current_4 <- final_df%>%
  filter(VAR_NAME=="CURRENT4")%>%
  select(CaseId,name, value,block_number,choice_number, -VAR_NAME) %>%
  select(-name)%>%
  rename(CURRENT_4=value)%>%
  mutate(CURRENT_4 = as.numeric(gsub("%", "", CURRENT_4)) / 100) # Remove '%' and divide by 100


# extract current WQ = 5 as % as data frame
df_WQ_current_5 <- final_df%>%
  filter(VAR_NAME=="CURRENT5")%>%
  select(CaseId,name, value,block_number,choice_number, -VAR_NAME) %>%
  select(-name)%>%
  rename(CURRENT_5=value)%>%
  mutate(CURRENT_5 = as.numeric(gsub("%", "", CURRENT_5)) / 100) # Remove '%' and divide by 100


# extract image info-current as data frame
df_WQ_current_image<- final_df%>%
  filter(VAR_NAME=="IMAGECURRENT")%>%
  select(CaseId,name, value,block_number,choice_number, -VAR_NAME) %>%
  select(-name)%>%
  rename(IMAGECURRENT=value)

# extract image info-policy as data frame
df_WQ_policy_image<- final_df%>%
  filter(VAR_NAME=="IMAGEPOLICY")%>%
  select(CaseId,name, value,block_number,choice_number, -VAR_NAME) %>%
  select(-name)%>%
  rename(IMAGEPOLICY=value)


# extract WQ = 1 as % in policy data frame
df_WQ_policy_1 <- final_df%>%
  filter(VAR_NAME=="POLICY1")%>%
  select(CaseId,name, value,block_number,choice_number, -VAR_NAME) %>%
  select(-name)%>%
  rename(POLICY_1=value)%>%
  mutate(POLICY_1 = as.numeric(gsub("%", "", POLICY_1)) / 100) # Remove '%' and divide by 100

# extract WQ = 2 as % in policy data frame
df_WQ_policy_2 <- final_df%>%
  filter(VAR_NAME=="POLICY2")%>%
  select(CaseId,name, value,block_number,choice_number, -VAR_NAME) %>%
  select(-name)%>%
  rename(POLICY_2=value)%>%
  mutate(POLICY_2 = as.numeric(gsub("%", "", POLICY_2)) / 100) # Remove '%' and divide by 100


# extract WQ = 3 as % in policy data frame
df_WQ_policy_3 <- final_df%>%
  filter(VAR_NAME=="POLICY3")%>%
  select(CaseId,name, value,block_number,choice_number, -VAR_NAME) %>%
  select(-name)%>%
  rename(POLICY_3=value)%>%
  mutate(POLICY_3 = as.numeric(gsub("%", "", POLICY_3)) / 100) # Remove '%' and divide by 100


# extract WQ = 4 as % in policy data frame
df_WQ_policy_4 <- final_df%>%
  filter(VAR_NAME=="POLICY4")%>%
  select(CaseId,name, value,block_number,choice_number, -VAR_NAME) %>%
  select(-name)%>%
  rename(POLICY_4=value)%>%
  mutate(POLICY_4 = as.numeric(gsub("%", "", POLICY_4)) / 100) # Remove '%' and divide by 100


# extract WQ = 5 as % in policy data frame
df_WQ_policy_5 <- final_df%>%
  filter(VAR_NAME=="POLICY5")%>%
  select(CaseId,name, value,block_number,choice_number, -VAR_NAME) %>%
  select(-name)%>%
  rename(POLICY_5=value)%>%
  mutate(POLICY_5 = as.numeric(gsub("%", "", POLICY_5)) / 100) # Remove '%' and divide by 100

# Merge thos subset to represent combine dataframe
# Then left joint those variable based on respondent id, block id, river basin and other matching variables 

df_choice_all <- df_vote%>%
  left_join(df_cost)%>%
  left_join(df_WQ_current)%>%
  left_join(df_WQ_policy)%>%
  left_join(df_WQ_policy_areakm)%>%
  left_join(df_WQ_policy_percent)%>%
  #left_join(df_WQ_current_1)%>%
  #left_join(df_WQ_current_2)%>%
  #left_join(df_WQ_current_3)%>%
  #left_join(df_WQ_current_4)%>%
  #left_join(df_WQ_current_5)%>%
  #left_join(df_WQ_current_image)%>%
  left_join(df_WQ_policy_image)%>%
  #left_join(df_WQ_policy_1)%>%
  #left_join(df_WQ_policy_2)%>%
  #left_join(df_WQ_policy_3)%>%
  #left_join(df_WQ_policy_4)%>%
  #left_join(df_WQ_policy_5)%>%
  mutate(VOTE1 = case_when(
    VOTE == "1" ~ 1, 
    VOTE == "2" ~ 0, 
    TRUE ~ as.numeric(VOTE)  # Convert VOTE to numeric for other cases
  ))%>%
  rename(
    BLK_NUMBER = block_number,
    CHOICE_NUMBER = choice_number
  )%>%
  mutate(BLOCK_NUMBER = str_extract_all(BLK_NUMBER, "\\d+"), # Danny use letter A fro some blocks so I remove it for consistent
         BLOCK_NUMBER = sapply(BLOCK_NUMBER, function(x) paste(x, collapse = "")))%>%
  mutate(ID = paste(BLOCK_NUMBER, CHOICE_NUMBER, sep = "_"))%>%
  select(-BLOCK_NUMBER) # drop ir and keep the BLK_NUMBER ONLT


# There are some information needs to add from the choice design excel sheet 
# i.e. respondents WQ at sub basin level (policy average is fro whole area, but need WQ at sub basin level)
 
# Define the mapping for basin and sub-basin codes
basin_mapping <- c("arb" = 1, "lsn" = 2, "nsb" = 3, "ssb" = 4)
sub_basin_mapping <- c(
  "as" = 1, "qa" = 2, "re" = 3, "so" = 4, "elw" = 5, "gb" = 6, "lwl" = 7, 
  "ne" = 8, "sa" = 9, "wlw" = 10, "ba" = 11, "cns" = 12, "lns" = 13, 
  "uns" = 14, "bw" = 15, "lss" = 16, "rd" = 17, "uss" = 18
)


ChoiceSetDesigns <- read_excel("Rawdata/ChoiceSetDesigns.xlsx")%>%
  select(block,choice_number,policy_description,basin,sub_basin,image_current,image_policy,version)%>%
  mutate(CHOICE_SUB_BASIN = case_when(
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
  mutate(CHOICE_AREA = case_when(
    CHOICE_SUB_BASIN == 0 ~ "BASIN",          # Choice is based on full basin
    CHOICE_SUB_BASIN > 0 ~ "SUBBASIN",        # Choice is based on sub basin
    TRUE ~ NA_character_  
  ))%>%
  mutate(CHOICE_BASIN = case_when(            # Choice basin map with survey keys
    basin == "AR" ~ 1,
    basin == "LSN" ~ 2,
    basin == "NS" ~ 3,
    basin == "SS" ~ 4))%>%
  #mutate(TREATMENT = case_when(
    #treatment == "spatial" ~1,
    #treatment == "wq" ~ 2
  #))%>%
  mutate(wq_change = str_extract(policy_description, "^[^-]+"))%>%      # WQ change scenario
  mutate(wq_change = str_replace(wq_change, "Improve WQ up to 2 $", "Improve WQ up to 2"))%>%
  mutate(WQ_UP1 = case_when(wq_change == "Improve WQ up to 1 " ~ 1,TRUE ~ 0),
         WQ_UP2 = case_when(wq_change == "Improve WQ up to 2" ~ 1, TRUE ~0),
         WQ_UP3 = case_when(wq_change == "Improve WQ up to 3 " ~ 1, TRUE ~0),
         WQ_BY1 = case_when(wq_change == "Improve WQ by 1 " ~ 1, TRUE ~0)
  )%>%
  rename(BLOCK_NUMBER = block, 
         CHOICE_NUMBER = choice_number
  )%>%
  select(BLOCK_NUMBER,CHOICE_NUMBER,CHOICE_AREA,CHOICE_BASIN,
         CHOICE_SUB_BASIN, WQ_UP1,WQ_UP2,WQ_UP3,WQ_BY1,image_current,image_policy,version)%>%
  mutate(ID = paste(BLOCK_NUMBER, CHOICE_NUMBER, sep = "_"))%>% # consitent key with usrvey data and ChoiceDesign.xlsx
  #select( -BLOCK_NUMBER, -CHOICE_NUMBER)%>%
  distinct(ID, .keep_all = T)%>% # Some version repeat the same so need to get unique
  
  separate(                            # use image names to determine the WQ at sub basin - policy
    col = image_policy,
    into = c("basin", paste0("sub_policy_", 1:6)),
    sep = "_"
  ) %>%
  
  separate(                            # use image names to determine the WQ at sub basin - current
    col = image_current,
    into = c("basin_current", paste0("sub_current_", 1:6)),
    sep = "_"
  ) %>%
  mutate(
    CH_BASIN = basin_mapping[basin]   # map the basin keys
  ) %>%
  mutate(across(
    starts_with("sub"), 
    list(
      name = ~ sub_basin_mapping[str_extract(.x, "[a-zA-Z]+")],
      wq = ~ str_extract(.x, "[0-9]+")
    ),
    .names = "{.col}_{.fn}"
  ))%>%
  select(-BLOCK_NUMBER,-CHOICE_NUMBER)



################################################################################
# Join df_all data frame with survey data
# make some variables by matching the locality of the choice and respondents local basin
df_all <- df_choice_all %>%
  left_join(ChoiceSetDesigns, by = c("ID")) %>%
  mutate(
    BASIN = as.character(BASIN),
    CHOICE_BASIN = as.character(CHOICE_BASIN),
    POLICY_AVERAGE = as.numeric(POLICY_AVERAGE)
  ) %>%
  mutate(CHOICE_LOCALITY_BASIN = case_when(          # indicator choice basin and local basin same
    BASIN == CHOICE_BASIN ~ "LOCAL",       
    BASIN != CHOICE_BASIN ~ "NONLOCAL",   
    TRUE ~ NA_character_                        
  )) %>%
  mutate(CHOICE_LOCALITY_SUBBASIN = case_when(      # indicator choice sub basin and local sub basin same
    SUB_BASIN == CHOICE_SUB_BASIN ~ "LOCAL",       
    SUB_BASIN != CHOICE_SUB_BASIN ~ "NONLOCAL",   
    TRUE ~ NA_character_                        
  )) %>%
  
  mutate(across(matches("^sub_current\\d+_wq$"), ~as.numeric(.))) %>%
  
  mutate(                                           # WQ level at sub-basin - this is use to determine the WQ at the home subwatershed
    WQ_HOME_CURRENT = if_else(                       # What is the WQ at home sub watershed when choice is with their local-sub watershed
      CHOICE_LOCALITY_BASIN == "LOCAL",             
      case_when(
        sub_current_1_name == SUB_BASIN ~ as.numeric(sub_current_1_wq),   # these will capture the choice scenario presented as full basin
        sub_current_2_name == SUB_BASIN ~ as.numeric(sub_current_2_wq),   # when we present it as full basin for their locality
        sub_current_3_name == SUB_BASIN ~ as.numeric(sub_current_3_wq),   # one of sub basin would be their local sub basin
        sub_current_4_name == SUB_BASIN ~ as.numeric(sub_current_4_wq),
        sub_current_5_name == SUB_BASIN ~ as.numeric(sub_current_5_wq),
        sub_current_6_name == SUB_BASIN ~ as.numeric(sub_current_6_wq),
        TRUE ~ 0  
      ),
      0  
    )
  ) %>%
  mutate(across(matches("^sub_policy\\d+_wq$"), ~as.numeric(.))) %>%
  
  mutate(
    WQ_HOME_POLICY = if_else(
      CHOICE_LOCALITY_BASIN == "LOCAL",
      case_when(
        sub_policy_1_name == SUB_BASIN ~ as.numeric(sub_policy_1_wq),
        sub_policy_2_name == SUB_BASIN ~ as.numeric(sub_policy_2_wq),
        sub_policy_3_name == SUB_BASIN ~ as.numeric(sub_policy_3_wq),
        sub_policy_4_name == SUB_BASIN ~ as.numeric(sub_policy_4_wq),
        sub_policy_5_name == SUB_BASIN ~ as.numeric(sub_policy_5_wq),
        sub_policy_6_name == SUB_BASIN ~ as.numeric(sub_policy_6_wq),
        TRUE ~ 0  # Ensure numeric output
      ),
      0  # Ensure numeric output
    )
  ) %>%
  
  
  mutate(
    WQ_LOCAL_CURRENT = if_else(CHOICE_LOCALITY_BASIN == "LOCAL",CURRENT_AVERAGE,0),
    WQ_NL_CURRENT = if_else(CHOICE_LOCALITY_BASIN == "NONLOCAL",CURRENT_AVERAGE,0),
    WQ_LOCAL_POLICY = if_else(CHOICE_LOCALITY_BASIN == "LOCAL",POLICY_AVERAGE,0),
    WQ_NL_POLICY = if_else(CHOICE_LOCALITY_BASIN == "NONLOCAL",POLICY_AVERAGE,0)
    )%>%
  
  mutate(
    WQ_HOME_CURRENT = if_else(WQ_HOME_CURRENT == WQ_HOME_POLICY, 0, WQ_HOME_CURRENT),
    WQ_HOME_POLICY = if_else(WQ_HOME_CURRENT == 0 & WQ_HOME_POLICY != 0, 0, WQ_HOME_POLICY)  
  )%>%
  
  mutate(
    WQ_SUBBASIN_LOCAL_CURRENT = if_else(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "LOCAL",CURRENT_AVERAGE,0),
    WQ_SUBBASIN_NL_CURRENT = if_else(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "NONLOCAL", CURRENT_AVERAGE,0),
    WQ_SUBBASIN_LOCAL_POLICY = if_else(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "LOCAL", POLICY_AVERAGE,0),
    WQ_SUBBASIN_NL_POLICY = if_else(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "NONLOCAL", POLICY_AVERAGE,0)
  )%>%
  
  mutate(
    WQ_BASIN_LOCAL_CURRENT = if_else(CHOICE_AREA == "BASIN" & CHOICE_LOCALITY_BASIN == "LOCAL",CURRENT_AVERAGE,0),
    WQ_BASIN_NL_CURRENT = if_else(CHOICE_AREA == "BASIN" & CHOICE_LOCALITY_BASIN == "NONLOCAL", CURRENT_AVERAGE,0),
    WQ_BASIN_LOCAL_POLICY = if_else(CHOICE_AREA == "BASIN" & CHOICE_LOCALITY_BASIN == "LOCAL", POLICY_AVERAGE,0),
    WQ_BASIN_NL_POLICY = if_else(CHOICE_AREA == "BASIN" & CHOICE_LOCALITY_BASIN == "NONLOCAL", POLICY_AVERAGE,0)
  )%>%
  select(CaseId,CONDITION,TREATMENT,VERSION,BLK_NUMBER,CHOICE_NUMBER,BASIN,SUB_BASIN,NON_LOCAL,
         IMAGEPOLICY,CURRENT_AVERAGE,POLICY_AVERAGE,
         CHOICE_AREA,CHOICE_BASIN,CHOICE_SUB_BASIN,CHOICE_LOCALITY_BASIN,CHOICE_LOCALITY_SUBBASIN,CHOICE_LOCALITY_BASIN,
         POLICY_SIZE_KM,POLICY_SIZE_PERCENT,WQ_UP1,WQ_UP2,WQ_UP3,WQ_BY1,
         WQ_LOCAL_CURRENT,WQ_NL_CURRENT,WQ_LOCAL_POLICY,WQ_NL_POLICY,
         WQ_HOME_CURRENT,WQ_HOME_POLICY,
         WQ_SUBBASIN_LOCAL_CURRENT,WQ_SUBBASIN_NL_CURRENT,WQ_SUBBASIN_LOCAL_POLICY,WQ_SUBBASIN_NL_POLICY,
         WQ_BASIN_LOCAL_CURRENT,WQ_BASIN_NL_CURRENT,WQ_BASIN_LOCAL_POLICY,WQ_BASIN_NL_POLICY,
         COST,VOTE1,VOTE)

##############################################################################################################
# In second we need to add the respondent specific data collected such as age, and data on all othe questions
# Open the data frame that create respondent characteristics (responses to non-valuation questions)
##############################################################################################################

df_RespondsUnique <- read.csv("./Deriveddata/respondent_characteristics.csv")

df_RespondsUnique$CaseId <- as.character(df_RespondsUnique$CaseId)
#df_RespondsUnique$SUB_BASIN <- as.character(df_RespondsUnique$SUB_BASIN)
df_RespondsUnique$BASIN <- as.character(df_RespondsUnique$BASIN)


# Merge them with choice related questions
df_final <- df_all%>%
  select(-VOTE)%>%
  rename(VOTE=VOTE1)%>%
  left_join(df_RespondsUnique)
  


################################################################################
# To address the distance decaying effects, we add few more variables based on the
# provinces and whether sub basins are share across the province


# Read shapefiles
river_basins <- st_read("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/Shapefile/study_area.shp")%>%
  mutate(sub_basin = case_when(
    WSCSDA_E == "Qu'Appelle" ~ "Qu'Appelle", 
    WSCSDA_E == "Assiniboine" ~ "Assiniboine", 
    WSCSDA_E == "Souris" ~ "Souris", 
    WSCSDA_E == "Red" ~ "Red", 
    
    WSCSDA_E == "Grass and Burntwood River Basin" ~ "Grass and Burntwood", 
    WSCSDA_E == "Nelson River Basin" ~ "Nelson", 
    WSCSDA_E == "Saskatchewan River Basin" ~ "Saskatchewan", 
    WSCSDA_E == "Eastern Lake Winnipeg River Basin" ~ "Eastern Lake Winnipeg", 
    WSCSDA_E == "Lake Winnipegosis and Lake Manitoba River Basin" ~ "Lake Winnipegosis and Lake Manitoba", 
    WSCSDA_E == "Western Lake Winnipeg River Basin" ~ "Western Lake Winnipeg", 
    
    WSCSDA_E == "Central North Saskatchewan Sub River Basin" ~ "Central North Saskatchewan", 
    WSCSDA_E == "Upper North Saskatchewan Sub River Basin" ~ "Upper North Saskatchewan", 
    WSCSDA_E == "Battle Sub River Basin" ~ "Battle", 
    WSCSDA_E == "Lower North Saskatchewan Sub River Basin" ~ "Lower North Saskatchewan", 
    
    WSCSDA_E == "Bow Sub River Basin" ~ "Bow", 
    WSCSDA_E == "Red Deer Sub River Basin" ~ "Red Deer", 
    WSCSDA_E == "Lower South Saskatchewan Sub River Basin" ~ "Lower South Saskatchewan", 
    WSCSDA_E == "Upper South Saskatchewan Sub River Basin" ~ "Upper South Saskatchewan", 
    
    TRUE ~ NA_character_  # Otherwise, assign 0
  ))
ab <- st_read("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/Shapefile/AB.shp")
mb <- st_read("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/Shapefile/MB.shp")
sk <- st_read("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/Shapefile/SK.shp")

# Combine all states into one object
prov<- rbind(ab, mb, sk)

# Make geometries valid
river_basins <- st_make_valid(river_basins)
prov <- st_make_valid(prov)

prov <- st_transform(prov, st_crs(river_basins))

# Perform spatial intersection
intersections <- st_intersection(river_basins, prov)

# Calculate area in km²
intersections <- intersections %>%
  mutate(area_km2 = as.numeric(st_area(.)) / 1e6)

# Summarize: area of each river basin within each state
summary_df <- intersections %>%
  st_drop_geometry() %>%
  group_by(sub_basin, PRNAME) %>%
  summarize(area_km2 = sum(area_km2), .groups = "drop")

# OPTIONAL: Spread into wide format (one row per basin, with area in each state)
df <- tidyr::pivot_wider(summary_df,
                         names_from = PRNAME,
                         values_from = area_km2,
                         values_fill = 0)%>%
  mutate(CHOICE_SUB_BASIN = case_when(
    sub_basin == "all" ~ 0,
    sub_basin == "Assiniboine" ~ 1,
    sub_basin == "Qu'Appelle" ~ 2,
    sub_basin == "Red" ~ 3,
    sub_basin == "Souris" ~ 4,
    sub_basin == "Eastern Lake Winnipeg" ~ 5,
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
  select(sub_basin,CHOICE_SUB_BASIN,Manitoba,Saskatchewan,Alberta)

# calculate area % withineach province
df<- df%>%
  rowwise() %>%
  mutate(
    total_area = sum(c_across(c(Manitoba, Saskatchewan, Alberta))),
    PERC_MB = ceiling(10000 * Manitoba / total_area) / 100,
    PERC_SK = ceiling(10000 * Saskatchewan / total_area) / 100,
    PERC_AB = ceiling(10000 * Alberta / total_area) / 100
  ) %>%
  ungroup()%>%
  rowwise() %>%
  mutate(SHARED_BOADER = ifelse(sum(c_across(c(PERC_MB, PERC_SK, PERC_AB)) > 0) > 1,
                                0, 1)) %>%
  ungroup()%>%
  select(sub_basin,CHOICE_SUB_BASIN,SHARED_BOADER,PERC_MB,PERC_SK,PERC_AB)

df_final <- df_final%>%
  left_join(df)%>%
  mutate(WQ_SUBBASIN_LOCAL_NSB_CURRENT =  if_else(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "LOCAL" & 
                                                    SHARED_BOADER == 1 ,CURRENT_AVERAGE,0))%>%
  mutate(WQ_SUBBASIN_LOCAL_SB_CURRENT =  if_else(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "LOCAL" & 
                                                   SHARED_BOADER == 0 ,CURRENT_AVERAGE,0))%>%
  mutate(WQ_SUBBASIN_LOCAL_NSB_POLICY =  if_else(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "LOCAL" & 
                                                   SHARED_BOADER == 1 ,POLICY_AVERAGE,0))%>%
  mutate(WQ_SUBBASIN_LOCAL_SB_POLICY =  if_else(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "LOCAL" & 
                                                  SHARED_BOADER == 0 ,POLICY_AVERAGE,0))%>%
  
  mutate(WQ_SUBBASIN_NL_NSB_LP_CURRENT = if_else(CHOICE_AREA == "SUBBASIN" &  CHOICE_LOCALITY_SUBBASIN == "NONLOCAL" & SHARED_BOADER == 1 & 
                                                   ((PROVINCE == 1 & PERC_AB > 0) | (PROVINCE == 3 & PERC_MB > 0) | (PROVINCE == 12 & PERC_SK > 0)),
                                                 CURRENT_AVERAGE,0 ))%>%
  mutate(WQ_SUBBASIN_NL_SB_LP_CURRENT = if_else(CHOICE_AREA == "SUBBASIN" &  CHOICE_LOCALITY_SUBBASIN == "NONLOCAL" & SHARED_BOADER == 0 & 
                                                  ((PROVINCE == 1 & PERC_AB > 0) | (PROVINCE == 3 & PERC_MB > 0) | (PROVINCE == 12 & PERC_SK > 0)),
                                                CURRENT_AVERAGE,0 ))%>%
  
  mutate(WQ_SUBBASIN_NL_NSB_LP_POLICY = if_else(CHOICE_AREA == "SUBBASIN" &  CHOICE_LOCALITY_SUBBASIN == "NONLOCAL" & SHARED_BOADER == 1 & 
                                                  ((PROVINCE == 1 & PERC_AB > 0) | (PROVINCE == 3 & PERC_MB > 0) | (PROVINCE == 12 & PERC_SK > 0)),
                                                POLICY_AVERAGE,0 ))%>%
  mutate(WQ_SUBBASIN_NL_SB_LP_POLICY = if_else(CHOICE_AREA == "SUBBASIN" &  CHOICE_LOCALITY_SUBBASIN == "NONLOCAL" & SHARED_BOADER == 0 & 
                                                 ((PROVINCE == 1 & PERC_AB > 0) | (PROVINCE == 3 & PERC_MB > 0) | (PROVINCE == 12 & PERC_SK > 0)),
                                               POLICY_AVERAGE,0 ))%>%
  
  mutate(WQ_SUBBASIN_NL_NSB_NLP_CURRENT = if_else(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "NONLOCAL" & SHARED_BOADER == 1 &
                                                    ((PROVINCE == 1 & (PERC_MB > 0 | PERC_SK > 0) & PERC_AB == 0) |
                                                       (PROVINCE == 3 & (PERC_AB > 0 | PERC_SK > 0) & PERC_MB == 0) |
                                                       (PROVINCE == 12 & (PERC_AB > 0 | PERC_MB > 0) & PERC_SK == 0)), CURRENT_AVERAGE,0 )) %>%
  mutate(WQ_SUBBASIN_NL_SB_NLP_CURRENT = if_else(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "NONLOCAL" & SHARED_BOADER == 0 &
                                                   ((PROVINCE == 1 & (PERC_MB > 0 | PERC_SK > 0) & PERC_AB == 0) |
                                                      (PROVINCE == 3 & (PERC_AB > 0 | PERC_SK > 0) & PERC_MB == 0) |
                                                      (PROVINCE == 12 & (PERC_AB > 0 | PERC_MB > 0) & PERC_SK == 0)), CURRENT_AVERAGE,0 ))%>%
  
  mutate(WQ_SUBBASIN_NL_NSB_NLP_POLICY = if_else(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "NONLOCAL" & SHARED_BOADER == 1 &
                                                   ((PROVINCE == 1 & (PERC_MB > 0 | PERC_SK > 0) & PERC_AB == 0) |
                                                      (PROVINCE == 3 & (PERC_AB > 0 | PERC_SK > 0) & PERC_MB == 0) |
                                                      (PROVINCE == 12 & (PERC_AB > 0 | PERC_MB > 0) & PERC_SK == 0)), POLICY_AVERAGE,0 )) %>%
  mutate(WQ_SUBBASIN_NL_SB_NLP_POLICY = if_else(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "NONLOCAL" & SHARED_BOADER == 0 &
                                                  ((PROVINCE == 1 & (PERC_MB > 0 | PERC_SK > 0) & PERC_AB == 0) |
                                                     (PROVINCE == 3 & (PERC_AB > 0 | PERC_SK > 0) & PERC_MB == 0) |
                                                     (PROVINCE == 12 & (PERC_AB > 0 | PERC_MB > 0) & PERC_SK == 0)), POLICY_AVERAGE,0 ))


  
# Reorder columns to allingn with the order of the survey

df_final <- df_final[, c( "CaseId","CONDITION","TREATMENT","VERSION","BLK_NUMBER","CHOICE_NUMBER","BASIN","SUB_BASIN","NON_LOCAL",
                          "IMAGEPOLICY","CURRENT_AVERAGE","POLICY_AVERAGE",
                          "CHOICE_AREA","CHOICE_BASIN","CHOICE_SUB_BASIN","CHOICE_LOCALITY_BASIN","CHOICE_LOCALITY_SUBBASIN",
                          "POLICY_SIZE_KM","POLICY_SIZE_PERCENT","WQ_UP1","WQ_UP2","WQ_UP3","WQ_BY1",
                          "WQ_LOCAL_CURRENT","WQ_NL_CURRENT","WQ_LOCAL_POLICY","WQ_NL_POLICY",
                          "WQ_HOME_CURRENT","WQ_HOME_POLICY",
                          "WQ_SUBBASIN_LOCAL_CURRENT","WQ_SUBBASIN_NL_CURRENT","WQ_SUBBASIN_LOCAL_POLICY","WQ_SUBBASIN_NL_POLICY",
                          "WQ_BASIN_LOCAL_CURRENT","WQ_BASIN_NL_CURRENT","WQ_BASIN_LOCAL_POLICY","WQ_BASIN_NL_POLICY",
                          "WQ_SUBBASIN_LOCAL_NSB_CURRENT","WQ_SUBBASIN_LOCAL_SB_CURRENT","WQ_SUBBASIN_LOCAL_NSB_POLICY","WQ_SUBBASIN_LOCAL_SB_POLICY",
                          "WQ_SUBBASIN_NL_NSB_LP_CURRENT","WQ_SUBBASIN_NL_SB_LP_CURRENT","WQ_SUBBASIN_NL_NSB_LP_POLICY","WQ_SUBBASIN_NL_SB_LP_POLICY",
                          "WQ_SUBBASIN_NL_NSB_NLP_CURRENT","WQ_SUBBASIN_NL_SB_NLP_CURRENT","WQ_SUBBASIN_NL_NSB_NLP_POLICY","WQ_SUBBASIN_NL_SB_NLP_POLICY",
                          "COST","VOTE",
                          "UID","PROVINCE", "AGE", "GENDER", "LANGUAGE", "INCOME", "POSTALCODE",
                          "FAMILIARITY_RIVER_LAKES",
                          "Q2_NOT_IMPORTANT","Q2_HABITAT","Q2_RECREATION","Q2_LANDSCAPE","Q2_COMP_NATURAL_ENVIR","Q2_OTHER",
                          "Q3_DIMINISH_VISUAL","Q3_DIMINISH_NATIVE_PLANT","Q3_SWIMMING_ADVI","Q3_DIMINISH_RECREATION",
                          "Q3_HARMFUL_ALGAE","Q3_CONSUMPTIPN_ADVI_FISH","Q3_DRINKING_WATER_ADVI","Q3_NONE","Q3_NOT_AWARE",
                          "Q4_UNPLEASANT_ODORS","Q4_Q4_LIMITED_CLARITY","Q4_SAMLL_ALGAE","Q4_LARGE_ALGAE",
                          "Q4_MURKY_WATER","Q4_TRASH_SHORE","Q4_UNHEALTHY_VEGETATION","Q4_NOT_BEEN_NEAR_LAKE","Q4_NOT_NOTICED",
                          "Q5_WATER_APPEAR_IMAGE_TEST","Q6_NATURAL_FLOW_IMAGE_TEST","Q7_DIVERSITY_IMAGE_TEST",
                          "Q8_SK_NON_LOCALMAP_TEST","Q8_PA_NON_LOCALMAP_TEST","Q9_LOCALMAP_TEST","LOCAL_WQ_OPINION",
                          "Q12_VOTE_WITHOUT_CONSIDER_OTHER","Q12_VOTE_HOUSEHOLD_FACE_COST","Q12_VOTE_CERTAIN_PUBLIC_ELEC",
                          "Q12_VOTE_INFORM_POLICY_MAKERS","Q12_VOTE_POLICY_ACHIEVE_IMPROV",
                          "Q13_INFLUENCE_WQ_LEVEL","Q13_INFLUENCE_NEAR_HOME","Q13_INFLUENCE_COST","Q13_INFLUENCE_REGIONSIZE",
                          "Q14_CERTAIN_VOTE","CHECK_NEXTDAY_AFTER_FRIDAY",
                          "Q15_GENERAL_THOUGHTS_CH1","Q15_GENERAL_THOUGHTS_CH2","Q15_GENERAL_THOUGHTS_CH3",
                          "Q15_GENERAL_THOUGHTS_CH4","Q15_GENERAL_THOUGHTS_CH5","Q15_GENERAL_THOUGHTS_CH6",
                          "Q15_GENERAL_THOUGHTS_CH7","Q15_GENERAL_THOUGHTS_CH8","Q15_GENERAL_THOUGHTS_CH9","Q15_GENERAL_THOUGHTS_CH10",
                          "Q16_SURVEY_PUSH_VOTE",
                          "Q17_HUMAN_CAN_MODIFY","Q17_HUMAN_ABUSING","Q17_PLANTS_ANIMAL_RIGHT","Q17_NATURE_CAPABILITY",
                          "Q17_HUMAN_RULE","Q17_NATURE_DELICATE",
                          "Q18_CURRENT_LOCATION_STAY","Q19_MEMBER_OF_ENVIRON_ORG","Q20_WQ_CONSIDER_CURRENT_LIVING",
                          "Q21_REC_TRIP_TWO_YEARS","Q22_DISTANCE_TRAVEL",
                          "Q23_FISHING","Q23_SWIMMING","Q23_CANNONING", "Q23_HUNTING", "Q23_OTHER",
                          "Q24_REC_TRIP_LAST_YEAR",
                          "Q25_WB1_NAME","Q25_WB2_NAME","Q25_WB3_NAME","Q25_WB4_NAME","Q25_WB5_NAME","Q25_WB1_WQ_LEVEL",
                          "Q25_WB2_WQ_LEVEL","Q25_WB3_WQ_LEVEL","Q25_WB4_WQ_LEVEL","Q25_WB5_WQ_LEVEL","Q25_WB1_NEAR_TOWN",
                          "Q25_WB2_NEAR_TOWN","Q25_WB3_NEAR_TOWN","Q25_WB4_NEAR_TOWN","Q25_WB5_NEAR_TOWN",
                          "COMMENTS",
                          "Q1_POLI","Q2_POLI","Q3_POLI","Q4_POLI","Q5_POLI","Q6_POLI","Q7_POLI","Q8_POLI","Q9_POLI","Q10_POLI",
                          "Q11_POLI","Q12_POLI","Q13_POLI","Q14_POLI","Q15_POLI","Q16_POLI","Q17_POLI","Q18_POLI","Q19_POLI",
                          "Q20_POLI","Q21_POLI","Q22_POLI","Q23_POLI","Q24_POLI","Q25_POLI","Q26_POLI","Q1_MOVIE","Q2_MOVIE",
                          "Q3_MOVIE","Q4_MOVIE","Q5_MOVIE","Q6_MOVIE","Q7_MOVIE","Q8_MOVIE","Q9_MOVIE","Q10_MOVIE","Q11_MOVIE",
                          "Q12_MOVIE","Q13_MOVIE","Q14_MOVIE","Q15_MOVIE","Q16_MOVIE","Q17_MOVIE","Q18_MOVIE","Q19_MOVIE",
                          "Q20_MOVIE","Q21_MOVIE","Q22_MOVIE","Q23_MOVIE","Q24_MOVIE","Q25_MOVIE","Q26_MOVIE","Q27_MOVIE",
                          "Q28_MOVIE","Q29_MOVIE","Q30_MOVIE"
                     )]







# Write final data frame to a csv file that can ffed into Apollo
write.csv(df_final, "./Deriveddata/processed_pilotdata_1_Apollo.csv",row.names = FALSE)

###############################################################################


