rm(list=ls(all=TRUE))

# Load required libraries
library(haven)
library(dplyr)
library(cansim)
library(janitor)
library(anesrake)

# Read raw data file
df <- read_sav("Rawdata//Water_Quality.sav 2")

# Prepare survey data
df_survey_data <- df %>%
  select(CaseId, PROVINCE, AGE, GENDER, EDUCATION, INCOME, LANGUAGE) %>%
  mutate(
    income_group = case_when(
      INCOME %in% c(0, 1) ~ "Under $19,999",
      INCOME %in% c(2, 3) ~ "$20,000 to $39,999",
      INCOME %in% c(4, 5) ~ "$40,000 to $59,999",
      INCOME %in% c(6, 7) ~ "$60,000 to $79,999",
      INCOME %in% c(8, 9) ~ "$80,000 to $99,999",
      INCOME %in% c(10, 11) ~ "$100,000 to $149,999",
      INCOME == 12 ~ "$150,000 and over",
      TRUE ~ NA_character_
    ),
    education_group = case_when(
      EDUCATION %in% c(1) ~ "High school or less",
      EDUCATION %in% c(2) ~ "Some college or technical training (no diploma)",
      EDUCATION %in% c(3) ~ "College diploma or technical/trade certificate",
      EDUCATION %in% c(4) ~ "Bachelor's degree",
      EDUCATION %in% c(5) ~ "Advanced/professional degree",
      TRUE ~ NA_character_
    ),
    gender_group = case_when(
      GENDER %in% c(1) ~ "Man",
      GENDER %in% c(2) ~ "Woman",
      GENDER %in% c(3) ~ "Other"
    ),
    age_group = case_when(
      AGE %in% c(1, 2) ~ "18 to 29 years",
      AGE %in% c(3, 4) ~ "30 to 39 years",
      AGE %in% c(5, 6) ~ "40 to 49 years",
      AGE %in% c(7, 8) ~ "50 to 59 years",
      AGE %in% c(9, 10) ~ "60 to 69 years",
      AGE %in% c(11, 12, 13, 14) ~ "70 years and older",
      TRUE ~ NA_character_
    )
  ) %>%
  select(CaseId, PROVINCE, income_group, education_group, gender_group, age_group)

# Filter for Saskatchewan
df_survey_data_sk <- df_survey_data %>%
  filter(PROVINCE == 12) %>%
  select(-PROVINCE) %>%
  # Convert to factors with consistent levels
  mutate(
    income_group = factor(income_group, 
                          levels = c("Under $19,999", "$20,000 to $39,999", 
                                     "$40,000 to $59,999", "$60,000 to $79,999",
                                     "$80,000 to $99,999", "$100,000 to $149,999",
                                     "$150,000 and over")),
    education_group = factor(education_group,
                             levels = c("High school or less", 
                                        "Some college or technical training (no diploma)",
                                        "College diploma or technical/trade certificate",
                                        "Bachelor's degree",
                                        "Advanced/professional degree")),
    gender_group = factor(gender_group,
                          levels = c("Man", "Woman", "Other")),
    age_group = factor(age_group,
                       levels = c("18 to 29 years", "30 to 39 years",
                                  "40 to 49 years", "50 to 59 years",
                                  "60 to 69 years", "70 years and older"))
  ) %>%
  # Remove rows with NA values in weighting variables
  filter(complete.cases(.))




# 2. Remove rows with NA values in weighting variables
df_survey_data_sk <- df_survey_data_sk %>%
  filter(
    !is.na(income_group),
    !is.na(education_group), 
    !is.na(gender_group),
    !is.na(age_group)
  )


# Get population targets from CANSIM
df_income <- get_cansim("11-10-0237-01") %>%
  clean_names() %>%
  filter(geo == "Saskatchewan", 
         ref_date == "2021", 
         income_concept == "After-tax income",
         economic_family_type == "Economic families and persons not in an economic family") %>%
  select(statistics, value) %>%
  mutate(income_group = case_when(
    statistics %in% c("Percentage under $10,000 (including zeros and losses)", "$10,000 to $19,999") ~ "Under $19,999",
    statistics %in% c("$20,000 to $29,999", "$30,000 to $39,999") ~ "$20,000 to $39,999",
    statistics %in% c("$40,000 to $49,999", "$50,000 to $59,999") ~ "$40,000 to $59,999",
    statistics %in% c("$60,000 to $69,999", "$70,000 to $79,999") ~ "$60,000 to $79,999",
    statistics %in% c("$80,000 to $89,999", "$90,000 to $99,999") ~ "$80,000 to $99,999",
    statistics == "$100,000 to 149,999" ~ "$100,000 to $149,999",
    statistics == "$150,000 and over" ~ "$150,000 and over",
    TRUE ~ NA_character_
  )) %>%
  group_by(income_group) %>%
  summarise(percentage = sum(value, na.rm = TRUE)) %>%
  filter(!is.na(income_group)) %>%
  mutate(percentage = percentage / sum(percentage))

df_income <- df_income %>%
  mutate(
    income_group = factor(
      income_group,
      levels = c(
        "Under $19,999",
        "$20,000 to $39,999",
        "$40,000 to $59,999",
        "$60,000 to $79,999",
        "$80,000 to $99,999",
        "$100,000 to $149,999",
        "$150,000 and over"
      )
    )
  ) %>%
  arrange(income_group)  # Sort rows to match factor order


df_education <- get_cansim("98-10-0384-01") %>%
  clean_names() %>%
  filter(geo == "Saskatchewan", 
         ref_date == "2021", 
         census_year_4 == "2021", 
         gender_3a == "Total - Gender",
         statistics_2a == "Count") %>%
  select(
    year = ref_date, 
    geo, 
    age_group = age_15a, 
    statistics = statistics_2a, 
    count = value, 
    education_census = highest_certificate_diploma_or_degree_15
  ) %>%
  filter(
    age_group %in% c("20 to 24 years", "25 to 64 years", "65 years and over"),
    !education_census %in% c(
      "Total - Highest certificate, diploma or degree",
      "Non-apprenticeship trades certificate or diploma",
      "Apprenticeship certificate",
      "Bachelor's degree or higher",
      "Postsecondary certificate, diploma or degree"
    )
  ) %>%
  mutate(
    education_group = case_when(
      education_census %in% c(
        "No certificate, diploma or degree",
        "High (secondary) school diploma or equivalency certificate"
      ) ~ "High school or less",
      
      education_census == "University certificate or diploma below bachelor level" ~ 
        "Some college or technical training (no diploma)",
      
      education_census %in% c(
        "College, CEGEP or other non-university certificate or diploma",
        "Apprenticeship or trades certificate or diploma"
      ) ~ "College diploma or technical/trade certificate",
      
      education_census == "Bachelor's degree" ~ "Bachelor's degree",
      
      education_census %in% c(
        "University certificate or diploma above bachelor level",
        "Degree in medicine, dentistry, veterinary medicine or optometry",
        "Master's degree",
        "Earned doctorate"
      ) ~ "Advanced/professional degree",
      
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(education_group) %>%
  summarise(count = sum(count, na.rm = TRUE)) %>%
  filter(!is.na(education_group)) %>%
  mutate(percentage = count / sum(count))


df_education <- df_education %>%
  mutate(
    education_group = factor(
      education_group,
      levels = c(
        "High school or less",
        "Some college or technical training (no diploma)",
        "College diploma or technical/trade certificate",
        "Bachelor's degree",
        "Advanced/professional degree"
      )
    )
  ) %>%
  arrange(education_group)  # Sort rows to match factor order


df_gender <- get_cansim("17-10-0005-01") %>%
  clean_names() %>%
  filter(geo == "Saskatchewan", 
         ref_date == "2021", 
         age_group == "18 years and older", 
         gender != "Both sexes") %>%
  mutate(gender = case_when(
    gender == "Men+" ~ "Man",
    gender == "Women+" ~ "Woman",
    TRUE ~ "Other"
  )) %>%
  group_by(gender) %>%
  summarise(percentage = sum(value, na.rm = TRUE)) %>%
  mutate(percentage = percentage / sum(percentage))


df_gender <- df_gender %>%
  mutate(
    gender_group = factor(
      gender,
      levels = c(
        "Man",
        "Woman",
        "Other"
      )
    )
  ) %>%
  arrange(gender_group)  # Sort rows to match factor order




df_age <- get_cansim("17-10-0005-01") %>%
  clean_names() %>%
  filter(geo == "Saskatchewan", 
         ref_date == "2021", 
         gender == "Total - gender") %>%
  mutate(age = case_when(
    age_group %in% c("18 to 24 years", "25 to 29 years") ~ "18 to 29 years",
    age_group %in% c("30 to 34 years", "35 to 39 years") ~ "30 to 39 years",
    age_group %in% c("40 to 44 years", "45 to 49 years") ~ "40 to 49 years",
    age_group %in% c("50 to 54 years", "55 to 59 years") ~ "50 to 59 years",
    age_group %in% c("60 to 64 years", "65 to 69 years") ~ "60 to 69 years",
    age_group %in% c("70 to 74 years", "75 to 79 years", "80 to 84 years",
                     "85 to 89 years", "90 years and older") ~ "70 years and older",
    TRUE ~ NA_character_
  )) %>%
  group_by(age) %>%
  summarise(percentage = sum(value, na.rm = TRUE)) %>%
  filter(!is.na(age)) %>%
  mutate(percentage = percentage / sum(percentage))

df_age <- df_age %>%
  mutate(
    age_group = factor(
      age,
      levels = c(
        "18 to 29 years",
        "30 to 39 years",
        "40 to 49 years",
        "50 to 59 years",
        "60 to 69 years",
        "70 years and older"
      )
    )
  ) %>%
  arrange(age_group)  # Sort rows to match factor order


# Corrected target list creation
target <- list(
  age_group = setNames(df_age$percentage, df_age$age),
  gender_group = setNames(df_gender$percentage, df_gender$gender),
  income_group = setNames(df_income$percentage, df_income$income_group),
  education_group = setNames(df_education$percentage, df_education$education_group)  # Changed from 'education' to 'education_group'
)

# Verify all levels match exactly
check_level_match <- function(data_var, target_names) {
  all(levels(data_var) %in% target_names) && all(target_names %in% levels(data_var))
}

if (!all(
  check_level_match(df_survey_data_sk$age_group, names(target$age_group)),
  check_level_match(df_survey_data_sk$gender_group, names(target$gender_group)), 
  check_level_match(df_survey_data_sk$education_group, names(target$education_group)),
  check_level_match(df_survey_data_sk$income_group, names(target$income_group))
)) {
  stop("Levels in survey data don't match target levels exactly")
}

#Check all targets
list(
  gender = identical(names(target$gender_group), levels(df_survey_data_sk$gender_group)),
  education = identical(names(target$education_group), levels(df_survey_data_sk$education_group)),
  income = identical(names(target$income_group), levels(df_survey_data_sk$income_group)),
  age = identical(names(target$age_group), levels(df_survey_data_sk$age_group))
)




df_survey_data_sk <- as.data.frame(df_survey_data_sk) %>%
  mutate(
    across(all_of(names(target)), ~factor(as.character(.x), levels = names(target[[cur_column()]]))),
    CaseId = as.numeric(as.character(CaseId))
  )


# 5. Run raking
raked_result <- anesrake(
  inputter = target,
  dataframe = df_survey_data_sk,
  caseid = df_survey_data_sk$CaseId,
  cap = 3,
  force1 = TRUE,
  maxit = 10000,  # Increased from default 50
  verbose = TRUE
)

raked_result$converge  

summary(raked_result$weightvec)


# Examine final weights
summary(raked_result$weightvec)
hist(raked_result$weightvec)


# Add weights to original data
df_survey_data_sk$weight <- raked_result$weightvec

# Check results
summary(raked_result)
print(raked_result$converge)

# Examine weight distribution
summary(df_survey_data_sk$weight)
hist(df_survey_data_sk$weight, main = "Weight Distribution")

# Calculate effective sample size
n_eff <- 1 / sum((df_survey_data_sk$weight/sum(df_survey_data_sk$weight))^2)
cat("Effective sample size:", round(n_eff), "of", nrow(df_survey_data_sk), "observations\n")

###############################################################################
#Alberta

# Filter for Saskatchewan
df_survey_data_ab <- df_survey_data %>%
  filter(PROVINCE == 1) %>%
  select(-PROVINCE) %>%
  # Convert to factors with consistent levels
  mutate(
    income_group = factor(income_group, 
                          levels = c("Under $19,999", "$20,000 to $39,999", 
                                     "$40,000 to $59,999", "$60,000 to $79,999",
                                     "$80,000 to $99,999", "$100,000 to $149,999",
                                     "$150,000 and over")),
    education_group = factor(education_group,
                             levels = c("High school or less", 
                                        "Some college or technical training (no diploma)",
                                        "College diploma or technical/trade certificate",
                                        "Bachelor's degree",
                                        "Advanced/professional degree")),
    gender_group = factor(gender_group,
                          levels = c("Man", "Woman", "Other")),
    age_group = factor(age_group,
                       levels = c("18 to 29 years", "30 to 39 years",
                                  "40 to 49 years", "50 to 59 years",
                                  "60 to 69 years", "70 years and older"))
  ) %>%
  # Remove rows with NA values in weighting variables
  filter(complete.cases(.))


# 2. Remove rows with NA values in weighting variables
df_survey_data_sk <- df_survey_data_sk %>%
  filter(
    !is.na(income_group),
    !is.na(education_group), 
    !is.na(gender_group),
    !is.na(age_group)
  )


df_income <- get_cansim("11-10-0237-01",refresh = TRUE) %>%
  clean_names() %>%
  filter(geo == "Alberta", 
         ref_date == "2021", 
         income_concept == "After-tax income",
         economic_family_type == "Economic families and persons not in an economic family") %>%
  select(statistics, value) %>%
  mutate(income_group = case_when(
    statistics %in% c("Percentage under $10,000 (including zeros and losses)", "$10,000 to $19,999") ~ "Under $19,999",
    statistics %in% c("$20,000 to $29,999", "$30,000 to $39,999") ~ "$20,000 to $39,999",
    statistics %in% c("$40,000 to $49,999", "$50,000 to $59,999") ~ "$40,000 to $59,999",
    statistics %in% c("$60,000 to $69,999", "$70,000 to $79,999") ~ "$60,000 to $79,999",
    statistics %in% c("$80,000 to $89,999", "$90,000 to $99,999") ~ "$80,000 to $99,999",
    statistics == "$100,000 to 149,999" ~ "$100,000 to $149,999",
    statistics == "$150,000 and over" ~ "$150,000 and over",
    TRUE ~ NA_character_
  )) %>%
  group_by(income_group) %>%
  summarise(percentage = sum(value, na.rm = TRUE)) %>%
  filter(!is.na(income_group)) %>%
  mutate(percentage = percentage / sum(percentage))

df_income <- df_income %>%
  mutate(
    income_group = factor(
      income_group,
      levels = c(
        "Under $19,999",
        "$20,000 to $39,999",
        "$40,000 to $59,999",
        "$60,000 to $79,999",
        "$80,000 to $99,999",
        "$100,000 to $149,999",
        "$150,000 and over"
      )
    )
  ) %>%
  arrange(income_group)  # Sort rows to match factor order


df_education <- get_cansim("98-10-0384-01") %>%
  clean_names() %>%
  filter(geo == "Alberta", 
         ref_date == "2021", 
         census_year_4 == "2021", 
         gender_3a == "Total - Gender",
         statistics_2a == "Count") %>%
  select(
    year = ref_date, 
    geo, 
    age_group = age_15a, 
    statistics = statistics_2a, 
    count = value, 
    education_census = highest_certificate_diploma_or_degree_15
  ) %>%
  filter(
    age_group %in% c("20 to 24 years", "25 to 64 years", "65 years and over"),
    !education_census %in% c(
      "Total - Highest certificate, diploma or degree",
      "Non-apprenticeship trades certificate or diploma",
      "Apprenticeship certificate",
      "Bachelor's degree or higher",
      "Postsecondary certificate, diploma or degree"
    )
  ) %>%
  mutate(
    education_group = case_when(
      education_census %in% c(
        "No certificate, diploma or degree",
        "High (secondary) school diploma or equivalency certificate"
      ) ~ "High school or less",
      
      education_census == "University certificate or diploma below bachelor level" ~ 
        "Some college or technical training (no diploma)",
      
      education_census %in% c(
        "College, CEGEP or other non-university certificate or diploma",
        "Apprenticeship or trades certificate or diploma"
      ) ~ "College diploma or technical/trade certificate",
      
      education_census == "Bachelor's degree" ~ "Bachelor's degree",
      
      education_census %in% c(
        "University certificate or diploma above bachelor level",
        "Degree in medicine, dentistry, veterinary medicine or optometry",
        "Master's degree",
        "Earned doctorate"
      ) ~ "Advanced/professional degree",
      
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(education_group) %>%
  summarise(count = sum(count, na.rm = TRUE)) %>%
  filter(!is.na(education_group)) %>%
  mutate(percentage = count / sum(count))


df_education <- df_education %>%
  mutate(
    education_group = factor(
      education_group,
      levels = c(
        "High school or less",
        "Some college or technical training (no diploma)",
        "College diploma or technical/trade certificate",
        "Bachelor's degree",
        "Advanced/professional degree"
      )
    )
  ) %>%
  arrange(education_group)  # Sort rows to match factor order


df_gender <- get_cansim("17-10-0005-01") %>%
  clean_names() %>%
  filter(geo == "Alberta", 
         ref_date == "2021", 
         age_group == "18 years and older", 
         gender != "Both sexes") %>%
  mutate(gender = case_when(
    gender == "Men+" ~ "Man",
    gender == "Women+" ~ "Woman",
    TRUE ~ "Other"
  )) %>%
  group_by(gender) %>%
  summarise(percentage = sum(value, na.rm = TRUE)) %>%
  mutate(percentage = percentage / sum(percentage))


df_gender <- df_gender %>%
  mutate(
    gender_group = factor(
      gender,
      levels = c(
        "Man",
        "Woman",
        "Other"
      )
    )
  ) %>%
  arrange(gender_group)  # Sort rows to match factor order




df_age <- get_cansim("17-10-0005-01") %>%
  clean_names() %>%
  filter(geo == "Alberta", 
         ref_date == "2021", 
         gender == "Total - gender") %>%
  mutate(age = case_when(
    age_group %in% c("18 to 24 years", "25 to 29 years") ~ "18 to 29 years",
    age_group %in% c("30 to 34 years", "35 to 39 years") ~ "30 to 39 years",
    age_group %in% c("40 to 44 years", "45 to 49 years") ~ "40 to 49 years",
    age_group %in% c("50 to 54 years", "55 to 59 years") ~ "50 to 59 years",
    age_group %in% c("60 to 64 years", "65 to 69 years") ~ "60 to 69 years",
    age_group %in% c("70 to 74 years", "75 to 79 years", "80 to 84 years",
                     "85 to 89 years", "90 years and older") ~ "70 years and older",
    TRUE ~ NA_character_
  )) %>%
  group_by(age) %>%
  summarise(percentage = sum(value, na.rm = TRUE)) %>%
  filter(!is.na(age)) %>%
  mutate(percentage = percentage / sum(percentage))

df_age <- df_age %>%
  mutate(
    age_group = factor(
      age,
      levels = c(
        "18 to 29 years",
        "30 to 39 years",
        "40 to 49 years",
        "50 to 59 years",
        "60 to 69 years",
        "70 years and older"
      )
    )
  ) %>%
  arrange(age_group)  # Sort rows to match factor order


# Corrected target list creation
target <- list(
  age_group = setNames(df_age$percentage, df_age$age),
  gender_group = setNames(df_gender$percentage, df_gender$gender),
  income_group = setNames(df_income$percentage, df_income$income_group),
  education_group = setNames(df_education$percentage, df_education$education_group)  # Changed from 'education' to 'education_group'
)

# Verify all levels match exactly
check_level_match <- function(data_var, target_names) {
  all(levels(data_var) %in% target_names) && all(target_names %in% levels(data_var))
}

if (!all(
  check_level_match(df_survey_data_sk$age_group, names(target$age_group)),
  check_level_match(df_survey_data_sk$gender_group, names(target$gender_group)), 
  check_level_match(df_survey_data_sk$education_group, names(target$education_group)),
  check_level_match(df_survey_data_sk$income_group, names(target$income_group))
)) {
  stop("Levels in survey data don't match target levels exactly")
}

#Check all targets
list(
  gender = identical(names(target$gender_group), levels(df_survey_data_sk$gender_group)),
  education = identical(names(target$education_group), levels(df_survey_data_sk$education_group)),
  income = identical(names(target$income_group), levels(df_survey_data_sk$income_group)),
  age = identical(names(target$age_group), levels(df_survey_data_sk$age_group))
)




df_survey_data_ab <- as.data.frame(df_survey_data_ab) %>%
  mutate(
    across(all_of(names(target)), ~factor(as.character(.x), levels = names(target[[cur_column()]]))),
    CaseId = as.numeric(as.character(CaseId))
  )


# 5. Run raking
raked_result <- anesrake(
  inputter = target,
  dataframe = df_survey_data_ab,
  caseid = df_survey_data_ab$CaseId,
  cap = 3,
  force1 = TRUE,
  maxit = 1000,
  pctlim = 0.01,
  verbose = TRUE
)

raked_result$converge  

summary(raked_result$weightvec)


# Examine final weights
summary(raked_result$weightvec)
hist(raked_result$weightvec)


# Add weights to original data
df_survey_data_ab$weight <- raked_result$weightvec

# Check results
summary(raked_result)
print(raked_result$converge)

# Examine weight distribution
summary(df_survey_data_sk$weight)
hist(df_survey_data_sk$weight, main = "Weight Distribution")

# Calculate effective sample size
n_eff <- 1 / sum((df_survey_data_sk$weight/sum(df_survey_data_sk$weight))^2)
cat("Effective sample size:", round(n_eff), "of", nrow(df_survey_data_sk), "observations\n")

###############################################################################
#Manitoba

# Filter for Saskatchewan
df_survey_data_mb <- df_survey_data %>%
  filter(PROVINCE == 3) %>%
  select(-PROVINCE) %>%
  # Convert to factors with consistent levels
  mutate(
    income_group = factor(income_group, 
                          levels = c("Under $19,999", "$20,000 to $39,999", 
                                     "$40,000 to $59,999", "$60,000 to $79,999",
                                     "$80,000 to $99,999", "$100,000 to $149,999",
                                     "$150,000 and over")),
    education_group = factor(education_group,
                             levels = c("High school or less", 
                                        "Some college or technical training (no diploma)",
                                        "College diploma or technical/trade certificate",
                                        "Bachelor's degree",
                                        "Advanced/professional degree")),
    gender_group = factor(gender_group,
                          levels = c("Man", "Woman", "Other")),
    age_group = factor(age_group,
                       levels = c("18 to 29 years", "30 to 39 years",
                                  "40 to 49 years", "50 to 59 years",
                                  "60 to 69 years", "70 years and older"))
  ) %>%
  # Remove rows with NA values in weighting variables
  filter(complete.cases(.))


# 2. Remove rows with NA values in weighting variables
df_survey_data_sk <- df_survey_data_sk %>%
  filter(
    !is.na(income_group),
    !is.na(education_group), 
    !is.na(gender_group),
    !is.na(age_group)
  )



df_income <- get_cansim("11-10-0237-01",refresh = TRUE) %>%
  clean_names() %>%
  filter(geo == "Manitoba", 
         ref_date == "2021", 
         income_concept == "After-tax income",
         economic_family_type == "Economic families and persons not in an economic family") %>%
  select(statistics, value) %>%
  mutate(income_group = case_when(
    statistics %in% c("Percentage under $10,000 (including zeros and losses)", "$10,000 to $19,999") ~ "Under $19,999",
    statistics %in% c("$20,000 to $29,999", "$30,000 to $39,999") ~ "$20,000 to $39,999",
    statistics %in% c("$40,000 to $49,999", "$50,000 to $59,999") ~ "$40,000 to $59,999",
    statistics %in% c("$60,000 to $69,999", "$70,000 to $79,999") ~ "$60,000 to $79,999",
    statistics %in% c("$80,000 to $89,999", "$90,000 to $99,999") ~ "$80,000 to $99,999",
    statistics == "$100,000 to 149,999" ~ "$100,000 to $149,999",
    statistics == "$150,000 and over" ~ "$150,000 and over",
    TRUE ~ NA_character_
  )) %>%
  group_by(income_group) %>%
  summarise(percentage = sum(value, na.rm = TRUE)) %>%
  filter(!is.na(income_group)) %>%
  mutate(percentage = percentage / sum(percentage))

df_income <- df_income %>%
  mutate(
    income_group = factor(
      income_group,
      levels = c(
        "Under $19,999",
        "$20,000 to $39,999",
        "$40,000 to $59,999",
        "$60,000 to $79,999",
        "$80,000 to $99,999",
        "$100,000 to $149,999",
        "$150,000 and over"
      )
    )
  ) %>%
  arrange(income_group)  # Sort rows to match factor order


df_education <- get_cansim("98-10-0384-01") %>%
  clean_names() %>%
  filter(geo == "Manitoba", 
         ref_date == "2021", 
         census_year_4 == "2021", 
         gender_3a == "Total - Gender",
         statistics_2a == "Count") %>%
  select(
    year = ref_date, 
    geo, 
    age_group = age_15a, 
    statistics = statistics_2a, 
    count = value, 
    education_census = highest_certificate_diploma_or_degree_15
  ) %>%
  filter(
    age_group %in% c("20 to 24 years", "25 to 64 years", "65 years and over"),
    !education_census %in% c(
      "Total - Highest certificate, diploma or degree",
      "Non-apprenticeship trades certificate or diploma",
      "Apprenticeship certificate",
      "Bachelor's degree or higher",
      "Postsecondary certificate, diploma or degree"
    )
  ) %>%
  mutate(
    education_group = case_when(
      education_census %in% c(
        "No certificate, diploma or degree",
        "High (secondary) school diploma or equivalency certificate"
      ) ~ "High school or less",
      
      education_census == "University certificate or diploma below bachelor level" ~ 
        "Some college or technical training (no diploma)",
      
      education_census %in% c(
        "College, CEGEP or other non-university certificate or diploma",
        "Apprenticeship or trades certificate or diploma"
      ) ~ "College diploma or technical/trade certificate",
      
      education_census == "Bachelor's degree" ~ "Bachelor's degree",
      
      education_census %in% c(
        "University certificate or diploma above bachelor level",
        "Degree in medicine, dentistry, veterinary medicine or optometry",
        "Master's degree",
        "Earned doctorate"
      ) ~ "Advanced/professional degree",
      
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(education_group) %>%
  summarise(count = sum(count, na.rm = TRUE)) %>%
  filter(!is.na(education_group)) %>%
  mutate(percentage = count / sum(count))


df_education <- df_education %>%
  mutate(
    education_group = factor(
      education_group,
      levels = c(
        "High school or less",
        "Some college or technical training (no diploma)",
        "College diploma or technical/trade certificate",
        "Bachelor's degree",
        "Advanced/professional degree"
      )
    )
  ) %>%
  arrange(education_group)  # Sort rows to match factor order


df_gender <- get_cansim("17-10-0005-01") %>%
  clean_names() %>%
  filter(geo == "Manitoba", 
         ref_date == "2021", 
         age_group == "18 years and older", 
         gender != "Both sexes") %>%
  mutate(gender = case_when(
    gender == "Men+" ~ "Man",
    gender == "Women+" ~ "Woman",
    TRUE ~ "Other"
  )) %>%
  group_by(gender) %>%
  summarise(percentage = sum(value, na.rm = TRUE)) %>%
  mutate(percentage = percentage / sum(percentage))


df_gender <- df_gender %>%
  mutate(
    gender_group = factor(
      gender,
      levels = c(
        "Man",
        "Woman",
        "Other"
      )
    )
  ) %>%
  arrange(gender_group)  # Sort rows to match factor order




df_age <- get_cansim("17-10-0005-01") %>%
  clean_names() %>%
  filter(geo == "Manitoba", 
         ref_date == "2021", 
         gender == "Total - gender") %>%
  mutate(age = case_when(
    age_group %in% c("18 to 24 years", "25 to 29 years") ~ "18 to 29 years",
    age_group %in% c("30 to 34 years", "35 to 39 years") ~ "30 to 39 years",
    age_group %in% c("40 to 44 years", "45 to 49 years") ~ "40 to 49 years",
    age_group %in% c("50 to 54 years", "55 to 59 years") ~ "50 to 59 years",
    age_group %in% c("60 to 64 years", "65 to 69 years") ~ "60 to 69 years",
    age_group %in% c("70 to 74 years", "75 to 79 years", "80 to 84 years",
                     "85 to 89 years", "90 years and older") ~ "70 years and older",
    TRUE ~ NA_character_
  )) %>%
  group_by(age) %>%
  summarise(percentage = sum(value, na.rm = TRUE)) %>%
  filter(!is.na(age)) %>%
  mutate(percentage = percentage / sum(percentage))

df_age <- df_age %>%
  mutate(
    age_group = factor(
      age,
      levels = c(
        "18 to 29 years",
        "30 to 39 years",
        "40 to 49 years",
        "50 to 59 years",
        "60 to 69 years",
        "70 years and older"
      )
    )
  ) %>%
  arrange(age_group)  # Sort rows to match factor order


# Corrected target list creation
target <- list(
  age_group = setNames(df_age$percentage, df_age$age),
  gender_group = setNames(df_gender$percentage, df_gender$gender),
  income_group = setNames(df_income$percentage, df_income$income_group),
  education_group = setNames(df_education$percentage, df_education$education_group)  # Changed from 'education' to 'education_group'
)

# Verify all levels match exactly
check_level_match <- function(data_var, target_names) {
  all(levels(data_var) %in% target_names) && all(target_names %in% levels(data_var))
}

if (!all(
  check_level_match(df_survey_data_sk$age_group, names(target$age_group)),
  check_level_match(df_survey_data_sk$gender_group, names(target$gender_group)), 
  check_level_match(df_survey_data_sk$education_group, names(target$education_group)),
  check_level_match(df_survey_data_sk$income_group, names(target$income_group))
)) {
  stop("Levels in survey data don't match target levels exactly")
}

#Check all targets
list(
  gender = identical(names(target$gender_group), levels(df_survey_data_sk$gender_group)),
  education = identical(names(target$education_group), levels(df_survey_data_sk$education_group)),
  income = identical(names(target$income_group), levels(df_survey_data_sk$income_group)),
  age = identical(names(target$age_group), levels(df_survey_data_sk$age_group))
)




df_survey_data_mb <- as.data.frame(df_survey_data_mb) %>%
  mutate(
    across(all_of(names(target)), ~factor(as.character(.x), levels = names(target[[cur_column()]]))),
    CaseId = as.numeric(as.character(CaseId))
  )


# 5. Run raking
raked_result <- anesrake(
  inputter = target,
  dataframe = df_survey_data_mb,
  caseid = df_survey_data_mb$CaseId,
  cap = 3,
  force1 = TRUE,
  maxit = 1000,
  pctlim = 0.01,
  verbose = TRUE
)

raked_result$converge  

summary(raked_result$weightvec)


# Examine final weights
summary(raked_result$weightvec)
hist(raked_result$weightvec)


# Add weights to original data
df_survey_data_ab$weight <- raked_result$weightvec

# Check results
summary(raked_result)
print(raked_result$converge)

# Examine weight distribution
summary(df_survey_data_sk$weight)
hist(df_survey_data_sk$weight, main = "Weight Distribution")

# Calculate effective sample size
n_eff <- 1 / sum((df_survey_data_sk$weight/sum(df_survey_data_sk$weight))^2)
cat("Effective sample size:", round(n_eff), "of", nrow(df_survey_data_sk), "observations\n")
