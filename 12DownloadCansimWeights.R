


df_income = get_cansim("11-10-0237-01") %>%
  clean_names(.) %>%
  filter(geo == "Canada", ref_date == "2021", income_concept == "Total income",
         economic_family_type == "Economic families and persons not in an economic family") %>%
  select(year = ref_date, geo, statistics, income_concept, value) %>%
  mutate(income_group = case_when(statistics == "Percentage under $10,000 (including zeros and losses)" |
                                    statistics == "$10,000 to $19,999" |
                                    statistics == "$20,000 to $29,999" ~ "$0-$29,999",
                                  statistics == "$30,000 to $39,999" |
                                    statistics == "$40,000 to $49,999" ~ "$30,000-$49,999",
                                  statistics == "$50,000 to $59,999" |
                                    statistics == "$60,000 to $69,999" ~ "$50,000-$69,999",
                                  statistics == "$70,000 to $79,999" |
                                    statistics == "$80,000 to $89,999" ~ "$70,000-$89,999",
                                  statistics == "$90,000 to $99,999" |
                                    statistics == "$100,000 to 149,999" ~ "$90,000-$149,999",      
                                  statistics == "$150,000 and over" ~ "$150,000 and over", 
                                  TRUE ~ NA_character_)) %>%
  group_by(geo,year,income_group) %>%
  summarise(percentage = sum(value)) %>%
  filter(!is.na(income_group))  %>%
  mutate(percentage = percentage / sum(percentage))

df_education = get_cansim("98-10-0384-01")%>%
  clean_names(.) %>%
  filter(geo == "Canada", ref_date == "2021", census_year_4 == "2021", 
         gender_3a == "Total - Gender",
         statistics_2a == "Count") %>%
  select(year = ref_date, geo, age_15a, statistics_2a, value, education_census = highest_certificate_diploma_or_degree_15) %>%
  filter(age_15a == "20 to 24 years" |
           age_15a == "25 to 64 years" |
           age_15a == "65 years and over",
         education_census != "Total - Highest certificate, diploma or degree",
         education_census != "Non-apprenticeship trades certificate or diploma",
         education_census != "Apprenticeship certificate",
         education_census != "Bachelor’s degree or higher",
         education_census != "Postsecondary certificate, diploma or degree") %>%
  mutate(education = case_when(
    education_census == "No certificate, diploma or degree" |
      education_census == "High (secondary) school diploma or equivalency certificate" ~ "High school or less",
    education_census == "Apprenticeship or trades certificate or diploma"  ~ "Vocational/Trade/Technical School",
    education_census == "University certificate or diploma below bachelor level" |
      education_census == "College, CEGEP or other non-university certificate or diploma" ~ "Some University/College",
    education_census == "Bachelor's degree" ~ "Bachelors degree",
    education_census == "University certificate or diploma above bachelor level" |
      education_census == "Degree in medicine, dentistry, veterinary medicine or optometry" |
      education_census == "Master's degree" |
      education_census == "Earned doctorate"  ~ "Advanced degree",
    TRUE ~ NA_character_)) %>%
  group_by(year, geo, education ) %>%
  summarise(count = sum(value)) %>%
  filter(!is.na(education))  %>%
  mutate(percentage = count / sum(count))



df_male = df_province %>%
  filter(geo == "Canada", ref_date == "2021", age_group == "18 years and over", gender != "Both sexes") %>%
  select(year = ref_date, geo, gender, count = value) %>%
  mutate(gender = ifelse(gender == "Males", "Man", "Woman"))  %>%
  mutate(percentage = count / sum(count))

df_age = df_province %>%
  filter(geo == "Canada", ref_date == "2021", gender == "Both sexes") %>%
  select(year = ref_date, geo, age_group, count = value) %>%
  mutate(age = case_when(
    age_group == "18 to 24 years" |
      age_group == "25 to 29 years" |
      age_group == "30 to 34 years" ~ "18 to 34 years",
    age_group == "35 to 39 years" |
      age_group == "40 to 44 years" |
      age_group == "45 to 49 years" |
      age_group == "50 to 54 years" ~ "35 to 54 years",
    age_group == "55 to 59 years" |
      age_group == "60 to 64 years" |
      age_group == "65 years and over" ~ "55 and over",
    TRUE ~ NA_character_))  %>%
  group_by(year, geo, age ) %>%
  summarise(count = sum(count)) %>%
  filter(!is.na(age)) %>%
  mutate(percentage = count / sum(count))

df_province = get_cansim("17-10-0005-01")  %>%
  clean_names(.)

df_province = df_province  %>%
  filter(ref_date == "2021", gender == "Total - gender", age_group == "18 years and over",
         geo != "Canada") %>%
  mutate(geo = as.factor(case_when(
    geo == "Northwest Territories" |
      geo == "Yukon" |
      geo == "Nunavut" ~ "Territories",
    geo == "New Brunswick" |
      geo == "Prince Edward Island" |
      geo == "Newfoundland and Labrador" |
      geo == "Nova Scotia" ~ "Atlantic Provinces",
    TRUE ~ geo))) %>%
  select(year = ref_date, geo, age_group, count = value)  %>%
  group_by(year, geo) %>%
  summarise(count = sum(count)) %>%
  mutate(percentage = count / sum(count))



df_province <- get_cansim("17-10-0005-01") %>%
  clean_names()

df_province <- df_province %>%
  filter(
    ref_date == "2021",
    gender == "Total - gender",
    age_group == "18 years and older",
    geo %in% c("Saskatchewan", "Alberta","Manitoba")
  ) %>%
  mutate(prov = case_when(
    geo == "Saskatchewan" ~ "sk",
    geo == "Alberta" ~ "ab",
    geo == "Manitoba" ~ "mb"
  ))%>%
  
  
  select(year = ref_date, prov, age_group, count = value) %>%
  group_by(year, prov) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  mutate(percentage = count / sum(count))



#-----------------------------------------------------------------------------
## Create target pop dataframe with variables of interest

target = list(
  age_group = df_age$percentage,
  gender = df_male$percentage,
  income_group = df_income$percentage,
  province = df_province$percentage,
  education_group = df_education$percentage)

names(target$age_group) = df_age$age
names(target$gender) = df_male$gender
names(target$income_group) = df_income$income_group
names(target$province) = df_province$geo
names(target$education_group) = df_education$education

saveRDS(target, "data/derived/TargetPopulationWeights.RDS")
