#################################################################################################
# Description: Script to create respondent characteristics (responses to non-valuation questions)
#################################################################################################

# Clean the memory
rm(list = ls())  # Removes all objects in the environment


# Read raw data file from CHAISR
df <- read_sav("Rawdata//Water_Quality.sav 2")


df_RespondsUnique <- df %>%
  select(CaseId,
         UID,PROVINCE, AGE, GENDER, LANGUAGE, INCOME, POSTALCODE,
         BASIN,SUB_BASIN,BASIN ,CONDITION, VERSION, TREATMENT, 
         LOCAL_AR,NON_LOCAL,
         Q1, 
         Q2M1, Q2M2, Q2M3, Q2M4, Q2M5, Q2M6,
         Q3M1,Q3M2,Q3M3,Q3M4,Q3M5,Q3M6,Q3M7,Q3M8,Q3M9,
         Q4M1,Q4M2,Q4M3,Q4M4,Q4M5,Q4M6,Q4M7,Q4M8,Q4M9,
         Q5,Q6,Q7,
         Q8_SK,Q8_PA,Q9,Q10,
         Q12_WHY_VOTED_A1,Q12_WHY_VOTED_A2,Q12_WHY_VOTED_A3,Q12_WHY_VOTED_A4,Q12_WHY_VOTED_A5,
         Q13_POLICY_INFLUENCE_A1,Q13_POLICY_INFLUENCE_A2,Q13_POLICY_INFLUENCE_A3,Q13_POLICY_INFLUENCE_A4,
         Q14_CERTAIN,
         CHECK,
         Q15_GENERAL_THOUGHTSM1,Q15_GENERAL_THOUGHTSM2,Q15_GENERAL_THOUGHTSM3,Q15_GENERAL_THOUGHTSM4,
         Q15_GENERAL_THOUGHTSM5,Q15_GENERAL_THOUGHTSM6,Q15_GENERAL_THOUGHTSM7,Q15_GENERAL_THOUGHTSM8,
         Q15_GENERAL_THOUGHTSM9,Q15_GENERAL_THOUGHTSM10,
         Q16_SURVEY_VOTE_CHANGE,
         SURVEY_CONTENT,
         
         Q1_POLI,Q2_POLI,Q3_POLI,Q4_POLI,Q5_POLI,Q6_POLI,Q7_POLI,Q8_POLI,Q9_POLI,Q10_POLI,
         Q12_POLI,Q13_POLI,Q14_POLI,Q15_POLI,Q16_POLI,Q17_POLI,Q18_POLI,Q19_POLI,
         Q20_POLI,Q21_POLI,Q22_POLI,Q23_POLI,Q24_POLI,Q25_POLI,Q26_POLI,
         
         
         Q1_MOVIE,Q2_MOVIE,Q3_MOVIE,Q4_MOVIE,Q5_MOVIE,Q6_MOVIE,Q7_MOVIE,Q8_MOVIE,Q9_MOVIE,Q10_MOVIE,Q11_MOVIE,
         Q12_MOVIE,Q13_MOVIE,Q14_MOVIE,Q15_MOVIE,Q16_MOVIE,Q17_MOVIE,Q18_MOVIE,Q19_MOVIE,
         Q20_MOVIE,Q21_MOVIE,Q22_MOVIE,Q23_MOVIE,Q24_MOVIE,Q25_MOVIE,Q26_MOVIE,Q27_MOVIE,
         Q28_MOVIE,Q29_MOVIE,Q30_MOVIE,
         
         Q17_A1_2,Q17_A2_2,Q17_A3_2,Q17_A4_2,Q17_A5_2,Q17_A6_2,
         
         Q18_2,
         Q19_2,
         Q20_2,
         Q21_2,
         Q22_2,
         
         Q23_2M1,Q23_2M2,Q23_2M3,Q23_2M4,Q23_2M5,
         
         Q24_2,
         
         Q25_A1_2, Q25_A2_2, Q25_A3_2, Q25_A4_2, Q25_A5_2,
         
         Q25_B1_2, Q25_B2_2, Q25_B3_2, Q25_B4_2, Q25_B5_2,
         
         Q25_C1_2, Q25_C2_2, Q25_C3_2, Q25_C4_2, Q25_C5_2,
         
         Q26_2
         
  ) %>%
  rowwise() %>%
  mutate(
    Q2_NOT_IMPORTANT = as.integer(any(c_across(Q2M1:Q2M6) == 1)),
    Q2_HABITAT = as.integer(any(c_across(Q2M1:Q2M6) == 2)),
    Q2_RECREATION = as.integer(any(c_across(Q2M1:Q2M6) == 3)),
    Q2_LANDSCAPE = as.integer(any(c_across(Q2M1:Q2M6) == 4)),
    Q2_COMP_NATURAL_ENVIR = as.integer(any(c_across(Q2M1:Q2M6) == 5)),
    Q2_OTHER = as.integer(any(c_across(Q2M1:Q2M6) == 6)),
    
    Q3_DIMINISH_VISUAL = as.integer(any(c_across(Q3M1:Q3M9)==1)),
    Q3_DIMINISH_NATIVE_PLANT = as.integer(any(c_across(Q3M1:Q3M9)==2)),
    Q3_SWIMMING_ADVI  = as.integer(any(c_across(Q3M1:Q3M9)==3)),
    Q3_DIMINISH_RECREATION  = as.integer(any(c_across(Q3M1:Q3M9)==4)),
    Q3_HARMFUL_ALGAE  = as.integer(any(c_across(Q3M1:Q3M9)==5)),
    Q3_CONSUMPTIPN_ADVI_FISH  = as.integer(any(c_across(Q3M1:Q3M9)==6)),
    Q3_DRINKING_WATER_ADVI  = as.integer(any(c_across(Q3M1:Q3M9)==7)),
    Q3_NONE  = as.integer(any(c_across(Q3M1:Q3M9)==8)),
    Q3_NOT_AWARE = as.integer(any(c_across(Q3M1:Q3M9)==9)),
    
    Q4_UNPLEASANT_ODORS = as.integer(any(c_across(Q4M1:Q4M9)==1)),
    Q4_Q4_LIMITED_CLARITY = as.integer(any(c_across(Q4M1:Q4M9)==2)),
    Q4_SAMLL_ALGAE  = as.integer(any(c_across(Q4M1:Q4M9)==3)),
    Q4_LARGE_ALGAE  = as.integer(any(c_across(Q4M1:Q4M9)==4)),
    Q4_MURKY_WATER  = as.integer(any(c_across(Q4M1:Q4M9)==5)),
    Q4_TRASH_SHORE  = as.integer(any(c_across(Q4M1:Q4M9)==6)),
    Q4_UNHEALTHY_VEGETATION  = as.integer(any(c_across(Q4M1:Q4M9)==7)),
    Q4_NOT_BEEN_NEAR_LAKE  = as.integer(any(c_across(Q4M1:Q4M9)==8)),
    Q4_NOT_NOTICED = as.integer(any(c_across(Q4M1:Q4M9)==9)),
    
    Q15_GENERAL_THOUGHTS_CH1 = as.integer(any(c_across(Q15_GENERAL_THOUGHTSM1:Q15_GENERAL_THOUGHTSM10)==1)), # Randomize choice chcek with danny
    Q15_GENERAL_THOUGHTS_CH2 = as.integer(any(c_across(Q15_GENERAL_THOUGHTSM1:Q15_GENERAL_THOUGHTSM10)==2)),
    Q15_GENERAL_THOUGHTS_CH3  = as.integer(any(c_across(Q15_GENERAL_THOUGHTSM1:Q15_GENERAL_THOUGHTSM10)==3)),
    Q15_GENERAL_THOUGHTS_CH4  = as.integer(any(c_across(Q15_GENERAL_THOUGHTSM1:Q15_GENERAL_THOUGHTSM10)==4)),
    Q15_GENERAL_THOUGHTS_CH5  = as.integer(any(c_across(Q15_GENERAL_THOUGHTSM1:Q15_GENERAL_THOUGHTSM10)==5)),
    Q15_GENERAL_THOUGHTS_CH6  = as.integer(any(c_across(Q15_GENERAL_THOUGHTSM1:Q15_GENERAL_THOUGHTSM10)==6)),
    Q15_GENERAL_THOUGHTS_CH7  = as.integer(any(c_across(Q15_GENERAL_THOUGHTSM1:Q15_GENERAL_THOUGHTSM10)==7)),
    Q15_GENERAL_THOUGHTS_CH8  = as.integer(any(c_across(Q15_GENERAL_THOUGHTSM1:Q15_GENERAL_THOUGHTSM10)==8)),
    Q15_GENERAL_THOUGHTS_CH9 = as.integer(any(c_across(Q15_GENERAL_THOUGHTSM1:Q15_GENERAL_THOUGHTSM10)==9)),
    Q15_GENERAL_THOUGHTS_CH10 = as.integer(any(c_across(Q15_GENERAL_THOUGHTSM1:Q15_GENERAL_THOUGHTSM10)==10)),
    
    Q23_FISHING = as.integer(any(c_across(Q23_2M1:Q23_2M5) == 1)),
    Q23_SWIMMING = as.integer(any(c_across(Q23_2M1:Q23_2M5) == 2)),
    Q23_CANNONING = as.integer(any(c_across(Q23_2M1:Q23_2M5) == 3)),
    Q23_HUNTING = as.integer(any(c_across(Q23_2M1:Q23_2M5) == 4)),
    Q23_OTHER = as.integer(any(c_across(Q23_2M1:Q23_2M5) == 5))
    
  ) %>%
  ungroup() %>%
  select(-Q2M1, -Q2M2, -Q2M3, -Q2M4, -Q2M5, -Q2M6,
         -Q3M1,-Q3M2,-Q3M3,-Q3M4,-Q3M5,-Q3M6,-Q3M7,-Q3M8,-Q3M9,
         -Q4M1,-Q4M2,-Q4M3,-Q4M4,-Q4M5,-Q4M6,-Q4M7,-Q4M8,-Q4M9,
         -Q15_GENERAL_THOUGHTSM1,-Q15_GENERAL_THOUGHTSM2,-Q15_GENERAL_THOUGHTSM3,-Q15_GENERAL_THOUGHTSM4,
         -Q15_GENERAL_THOUGHTSM5,-Q15_GENERAL_THOUGHTSM6,-Q15_GENERAL_THOUGHTSM7,-Q15_GENERAL_THOUGHTSM8,
         -Q15_GENERAL_THOUGHTSM9,-Q15_GENERAL_THOUGHTSM10,
         -Q23_2M1,-Q23_2M2,-Q23_2M3,-Q23_2M4,-Q23_2M5,
         
         
  )%>%
  rename(
    FAMILIARITY_RIVER_LAKES = Q1,
    Q5_WATER_APPEAR_IMAGE_TEST = Q5,
    Q6_NATURAL_FLOW_IMAGE_TEST = Q6,
    Q7_DIVERSITY_IMAGE_TEST = Q7,
    Q8_SK_NON_LOCALMAP_TEST = Q8_SK,
    Q8_PA_NON_LOCALMAP_TEST = Q8_PA,
    Q9_LOCALMAP_TEST = Q9,
    LOCAL_WQ_OPINION = Q10,
    
    
    Q12_VOTE_WITHOUT_CONSIDER_OTHER = Q12_WHY_VOTED_A3, # As randomization occuing here confirm the pattern with Danny
    Q12_VOTE_HOUSEHOLD_FACE_COST = Q12_WHY_VOTED_A1,
    Q12_VOTE_CERTAIN_PUBLIC_ELEC= Q12_WHY_VOTED_A2,
    Q12_VOTE_INFORM_POLICY_MAKERS= Q12_WHY_VOTED_A5,
    Q12_VOTE_POLICY_ACHIEVE_IMPROV= Q12_WHY_VOTED_A4,
    
    
    Q13_INFLUENCE_WQ_LEVEL= Q13_POLICY_INFLUENCE_A4,
    Q13_INFLUENCE_NEAR_HOME = Q13_POLICY_INFLUENCE_A3,
    Q13_INFLUENCE_COST = Q13_POLICY_INFLUENCE_A1,
    Q13_INFLUENCE_REGIONSIZE = Q13_POLICY_INFLUENCE_A2,
    
    Q14_CERTAIN_VOTE  = Q14_CERTAIN,
    CHECK_NEXTDAY_AFTER_FRIDAY  = CHECK,
    Q16_SURVEY_PUSH_VOTE = Q16_SURVEY_VOTE_CHANGE,
    
    Q17_HUMAN_CAN_MODIFY= Q17_A1_2,
    Q17_HUMAN_ABUSING = Q17_A2_2,
    Q17_PLANTS_ANIMAL_RIGHT = Q17_A3_2,
    Q17_NATURE_CAPABILITY = Q17_A4_2,
    Q17_HUMAN_RULE = Q17_A5_2,
    Q17_NATURE_DELICATE = Q17_A6_2,
    
    Q18_CURRENT_LOCATION_STAY= Q18_2,
    Q19_MEMBER_OF_ENVIRON_ORG= Q19_2,
    Q20_WQ_CONSIDER_CURRENT_LIVING = Q20_2,
    Q21_REC_TRIP_TWO_YEARS =Q21_2,
    Q22_DISTANCE_TRAVEL = Q22_2,
    Q24_REC_TRIP_LAST_YEAR  = Q24_2,
    Q25_WB1_NAME = Q25_A1_2,
    Q25_WB2_NAME = Q25_A2_2,
    Q25_WB3_NAME = Q25_A3_2,
    Q25_WB4_NAME = Q25_A4_2,
    Q25_WB5_NAME = Q25_A5_2,
    Q25_WB1_WQ_LEVEL = Q25_B1_2,
    Q25_WB2_WQ_LEVEL = Q25_B2_2,
    Q25_WB3_WQ_LEVEL = Q25_B3_2,
    Q25_WB4_WQ_LEVEL = Q25_B4_2,
    Q25_WB5_WQ_LEVEL = Q25_B5_2,
    Q25_WB1_NEAR_TOWN = Q25_C1_2,
    Q25_WB2_NEAR_TOWN = Q25_C2_2,
    Q25_WB3_NEAR_TOWN = Q25_C3_2,
    Q25_WB4_NEAR_TOWN = Q25_C4_2,
    Q25_WB5_NEAR_TOWN = Q25_C5_2,
    COMMENTS = Q26_2
  )

write.csv(df_RespondsUnique, "./Deriveddata/respondent_characteristics.csv", row.names = FALSE)
