library(readxl)
ChoiceSetDesigns <- read_excel("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/ChoiceSetDesigns.xlsx", 
                               sheet = "SummaryAll")



df1 <- ChoiceSetDesigns %>%
  filter(sub_basin != "all") %>%
  mutate(
    current_average = str_extract(image_current, "\\d+"),
    policy_average = str_extract(image_policy, "\\d+")
  ) %>%
  mutate(across(any_of(c("current_1", "current_2", "current_3", "current_4", "current_5")), ~ 0)) %>%
  mutate(
    current_1 = ifelse(current_average == "1", 1, 0),
    current_2 = ifelse(current_average == "2", 1, 0),
    current_3 = ifelse(current_average == "3", 1, 0),
    current_4 = ifelse(current_average == "4", 1, 0),
    current_5 = ifelse(current_average == "5", 1, 0)
  )%>%
  mutate(across(any_of(c("policy_1", "policy_2", "policy_3", "policy_4", "policy_5")), ~ 0)) %>%
  mutate(
    policy_1 = ifelse(policy_average == "1", 1, 0),
    policy_2 = ifelse(policy_average == "2", 1, 0),
    policy_3 = ifelse(policy_average == "3", 1, 0),
    policy_4 = ifelse(policy_average == "4", 1, 0),
    policy_5 = ifelse(policy_average == "5", 1, 0)
  )

df2 <- ChoiceSetDesigns%>%
  filter(sub_basin == "all")


df_final <- rbind(df1,df2)%>%
  arrange(block,choice_number)

write.csv(df_final,"~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/ChoiceSetDesigns_subbasinWQadjusted_5_7.csv")
  