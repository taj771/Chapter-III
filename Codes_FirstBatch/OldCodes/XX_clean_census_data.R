#rm(list=ls(all=TRUE))
install.packages(cansim)

#population data 
library(anesrake)
#library(tidyverse)
library(cansim)
library(janitor)
library(pacman)
#library(fmodelsummary)



df_raw <- get_cansim("17-10-0005-01") %>% clean_names()

df_raw %>%
	filter(ref_date == "2021",
				 gender == "Both sexes",
				 geo == "Saskatchewan") %>%
	nrow()
df_income = get_cansim("11-10-0237-01") %>%
  clean_names(.) %>%
  filter(geo == "Saskatchewan", 
  			 ref_date == "2021", 
  			 income_concept == "After-tax income",
         economic_family_type == "Economic families and persons not in an economic family") %>%
  select(year = ref_date, geo, statistics, income_concept, value) %>%
	mutate(income_group = case_when(
		statistics %in% c("Percentage under $10,000 (including zeros and losses)", "$10,000 to $19,999") ~ "Under $19,999",
		statistics %in% c("$20,000 to $29,999", "$30,000 to $39,999") ~ "$20,000 to $39,999",
		statistics %in% c("$40,000 to $49,999", "$50,000 to $59,999") ~ "$40,000 to $59,999",
		statistics %in% c("$60,000 to $69,999", "$70,000 to $79,999") ~ "$60,000 to $79,999",
		statistics %in% c("$80,000 to $89,999", "$90,000 to $99,999") ~ "$80,000 to $99,999",
		statistics == "$100,000 to 149,999" ~ "$100,000 to $149,999",
		statistics == "$150,000 and over" ~ "$150,000 and over",
		TRUE ~ NA_character_)) %>%
  group_by(geo,year,income_group) %>%
  summarise(percentage = sum(value)) %>%
  filter(!is.na(income_group))  %>%
  mutate(percentage = percentage / sum(percentage))


df_education = get_cansim("98-10-0384-01")%>%
  clean_names(.) %>%
  filter(geo == "Saskatchewan", 
  			 ref_date == "2021", 
  			 census_year_4 == "2021", 
         gender_3a == "Total - Gender",
         statistics_2a == "Count") %>%
  select(year = ref_date, 
  			 geo, age_15a, statistics_2a, value, 
  			 education_census = highest_certificate_diploma_or_degree_15) %>%
  filter(age_15a == "20 to 24 years" |
           age_15a == "25 to 64 years" |
           age_15a == "65 years and over",
         education_census != "Total - Highest certificate, diploma or degree",
         education_census != "Non-apprenticeship trades certificate or diploma",
         education_census != "Apprenticeship certificate",
         education_census != "Bachelor’s degree or higher",
         education_census != "Postsecondary certificate, diploma or degree") %>%
	
	mutate(education = case_when(
		education_census %in% c(
			"No certificate, diploma or degree",
			"High (secondary) school diploma or equivalency certificate"
		) ~ "High school or less",
		
		education_census == "University certificate or diploma below bachelor level" ~ "Some college or technical training (no diploma)",
		
		education_census %in% c(
			"College, CEGEP or other non-university certificate or diploma",
			"Apprenticeship or trades certificate or diploma"
		) ~ "College diploma or technical/trade certificate",
		
		education_census == "Bachelor's degree" ~ "Bachelor’s degree",
		
		education_census %in% c(
			"University certificate or diploma above bachelor level",
			"Degree in medicine, dentistry, veterinary medicine or optometry",
			"Master's degree",
			"Earned doctorate"
		) ~ "Advanced/professional degree",
    TRUE ~ NA_character_)) %>%
  group_by(year, education ) %>%
  summarise(count = sum(value)) %>%
  filter(!is.na(education))  %>%
  mutate(percentage = count / sum(count))#add a geo variable if necessary

df_male_raw = get_cansim("17-10-0005-01")  %>%
  clean_names(.)

df_male = df_male_raw %>%
  filter(geo == "Saskatchewan", 
  			 ref_date == "2021", 
  			 age_group == "18 years and older", 
  			 gender != "Both sexes") %>%
  select(year = ref_date, geo, gender, count = value) %>%
	mutate(	gender = case_when(
		gender == "Men+" ~ "Man",
		gender == "Women+" ~ "Woman",
			TRUE ~ "Other"  
	)) %>%
  group_by(year, gender)%>%
  summarise(count = sum(count))%>%
  mutate(percentage = count / sum(count))



df_age = df_raw %>%
  filter(geo == "Saskatchewan", ref_date == "2021", gender == "Total - gender") %>%
  select(year = ref_date, geo, age_group, count = value) %>%
	mutate(age = case_when(
		age_group %in% c("18 to 24 years", "25 to 29 years") ~ "18 to 29 years",
		age_group %in% c("30 to 34 years", "35 to 39 years") ~ "30 to 39 years",
		age_group %in% c("40 to 44 years", "45 to 49 years") ~ "40 to 49 years",
		age_group %in% c("50 to 54 years", "55 to 59 years") ~ "50 to 59 years",
		age_group %in% c("60 to 64 years", "65 to 69 years") ~ "60 to 69 years",
		age_group %in% c("70 to 74 years", "75 to 79 years", "80 to 84 years",
										 "85 to 89 years", "90 years and older") ~ "70 years and older",
		TRUE ~ NA_character_))  %>%
  group_by(year, age ) %>%
  summarise(count = sum(count)) %>%
  filter(!is.na(age)) %>%
  mutate(percentage = count / sum(count))


df_province <- get_cansim("17-10-0005-01") %>%
	clean_names()

df_province <- df_province %>%
	filter(
		ref_date == "2021",
		gender == "Total - gender",
		age_group == "18 years and older",
		geo == "Saskatchewan"
	) %>%
	mutate(prov = "sk") %>%
	select(year = ref_date, prov, age_group, count = value) %>%
	group_by(year, prov) %>%
	summarise(count = sum(count), .groups = "drop") %>%
	mutate(percentage = count / sum(count))


#-----------------------------------------------------------------------------
## Create target pop dataframe with variables of interest

target = list(
  age_group = df_age$percentage,
  gender_group = df_male$percentage,
  income_group = df_income$percentage,
  prov = df_province$percentage,
  education_group = df_education$percentage)

names(target$age_group) = df_age$age
names(target$gender_group) = df_male$gender
names(target$income_group) = df_income$income_group
names(target$prov) = df_province$prov
names(target$education_group) = df_education$education

saveRDS(target, "data/target_weights.RDS")  

