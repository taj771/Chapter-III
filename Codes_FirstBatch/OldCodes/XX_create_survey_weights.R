#rm(list=ls(all=TRUE))


library(anesrake)
#library(tidyverse)
library(cansim)
library(janitor)
library(pacman)
# summary statistics 
#################################
############Main Model###########
#################################

df_weighting <- df_demographic
target <- readRDS("Data/target_weights.RDS")
df_weighting <- as.data.frame(df_weighting)
df_weighting <- df_weighting %>%
	janitor::clean_names()
df_weighting$s6_postalcode <- toupper(df_weighting$s6_postalcode)

df_weighing <- data %>%
	filter(S@S2_GENDER!="")

table(df_weighting$s2_gender, useNA = "ifany")

df_weighting <- df_weighting %>%
	filter(!is.na(s2_gender))

table(df_weighting$s6_postalcode, useNA = "ifany")
table(df_weighting$s1_age, useNA = "ifany")
table(df_weighting$s5_income, useNA = "ifany")
table(df_weighting$s4_education, useNA = "ifany")
table(df_weighting$s2_gender, useNA = "ifany")
table(df_weighting$s3_language, useNA = "ifany")



# 2️⃣ Recode the values to human-readable labels
#df_weighting <- df_weighting %>%
#	mutate(s2_gender_label = recode(S2_GENDER,
#																	`1` = "Man",
#																	`2` = "Woman",
#																	`3` = "Nonbinary"))

df_weighting$prov <- as.factor(df_weighting$s6_postalcode)
df_weighting$age_group <- as.factor(df_weighting$s1_age)
df_weighting$income_group <- as.factor(df_weighting$s5_income)
df_weighting$education_group <- as.factor(df_weighting$s4_education)  
df_weighting$gender_group <- as.factor(df_weighting$s2_gender)    
df_weighting$language_group <- as.factor(df_weighting$s3_language)    

           #currently, gender non-binary respondents are included 
#in the sample but are not weighted.

#run anesrake
#outsave <- anesrake(target, df_weighting,
 #                   pctlim = 5,
  #                  cap = 5,
   #                 caseid = df_weighting$caseid)
?anesrake
outsave <- anesrake(
	target,
	df_weighting,
	"caseid",
	pctlim = 0,
	cap = 5
)

target$income_group
names(target$income_group)
table(df_weighting$income_group)
target$education_group
names(target$education_group)
table(df_weighting$income_group)



survey_weights <- data.frame(caseid = outsave$caseid, weights = outsave$weightvec)



write_csv(survey_weights, "Data/survey_weights.csv")
