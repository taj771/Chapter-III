###############################################################################
# This code results some exploratory summaries/visualizations graphs that 
# use to assess the results in pilot survey data
###############################################################################

# Clean the memory
rm(list = ls())  # Removes all objects in the environment

library(readr)
library(dplyr)
library(apollo)
library(ggplot2)
library(officer)
library(flextable)

# Load dataset
df <- read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/R/PilotData/processed_pilotdata_1_Apollo.csv")


# 1. Vote count % at different bid levels
df1 <- df%>%
  select(COST,VOTE)


df_summary <- df1 %>%
  group_by(COST) %>%
  summarize(
    Percent_1 = sum(VOTE == 1) / n() * 100,
    Percent_0 = sum(VOTE == 0) / n() * 100
  )

df_long <- df_summary %>%
  pivot_longer(cols = starts_with("Percent"), names_to = "Vote", values_to = "Percentage") %>%
  mutate(Vote = ifelse(Vote == "Percent_1", "1", "0"))

ggplot(df_long, aes(x = factor(COST), y = Percentage, fill = Vote)) +
  geom_bar(stat = "identity", position = "dodge") +  # Add position = "dodge" for side-by-side bars
  labs(
    title = "Votes Count % at Different Cost Levels",
    x = "Cost Level",
    y = "Percentage",
    fill = "Vote"
  ) +
  theme_minimal()+
  scale_fill_manual(
    values = c("0" = "lightblue", "1" = "maroon"),          # Assign colors
    labels = c("0" = "No Vote", "1" = "Yes Vote")   # Rename legend elements
  ) +
  theme(
    panel.grid = element_blank(),                   # Remove grid lines
    axis.line = element_line(size = 0.8, color = "black"),  # Add main axes
    axis.ticks = element_line(size = 0.8, color = "black"), # Add tick marks
    axis.ticks.length = unit(5, "pt")              # Length of tick marks
  )


# 2. Vote count % at different bid levels in a summary table

df_summary <- df1 %>%
  group_by(COST) %>%
  summarize(
    Yes_count = sum(VOTE == 1),  # Count of votes equal to 1
    No_count = sum(VOTE == 0),  # Count of votes equal to 0
    Total_Votes = n(),         # Total number of votes
    Percent_Yes = (Yes_count / Total_Votes) * 100,  # Percentage of votes equal to 1
    Percent_No = (No_count / Total_Votes) * 100   # Percentage of votes equal to 0
  )

# Convert the dataframe to a flextable
table <- flextable(df_summary)

doc <- read_docx() %>% 
  body_add_flextable(table) %>% 
  print(target = "./Output/df_summary_table.docx")

# 3. Percent breakdown of in the 'choose topic treatment' into whether they choose movies, politics, environment

df <- read_sav("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/PilotDataAnalysis/data/rawdata/Water_Quality.sav")

df2 <- df%>%
  filter(CONDITION == 2)%>% # 1 = WQ survey | 2 = General Survey. However there are some scenarios it coded as a General survey but it has WQ survey data
                         # need to clarify it with Danny 
  select(CaseId,SURVEY_CONTENT)%>%
  group_by(SURVEY_CONTENT)%>%
  summarise(
    Movie = sum(SURVEY_CONTENT == 1),
    Politics = sum(SURVEY_CONTENT == 2),
    Environ = sum(SURVEY_CONTENT == 3),
  )%>%
  ungroup()%>%
  mutate(Total = 243)%>%
  mutate(
    Percent_movie = (Movie / Total) * 100,  # Percentage of Movie
    Percent_politics = (Politics / Total) * 100,  # Percentage of Politics
    Percent_Enviro = (Environ / Total) * 100,  # Percentage of Politics
  )%>%
  mutate(Percentage = case_when(
    SURVEY_CONTENT == 1 ~ Percent_movie,
    SURVEY_CONTENT == 2 ~ Percent_politics,
    SURVEY_CONTENT == 3 ~ Percent_Enviro,
    TRUE ~ 0  # Default value if no condition is met
  ))%>%
  select(SURVEY_CONTENT,Percentage)%>%
  mutate(SURVEY_CONTENT = as_factor(SURVEY_CONTENT))  

# Plot
ggplot(df2, aes(x = SURVEY_CONTENT, y = Percentage, fill = SURVEY_CONTENT)) +
  geom_bar(stat = "identity", position = "dodge") +  # "dodge" for side-by-side bars
  labs(
    title = "",
    x = "",
    y = "%",
    fill = ""
  ) +
  theme_minimal() +  # Clean theme
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "none",
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),   # Remove minor gridlines
    axis.line = element_line()  # Keep axis lines visible
  )+
  scale_y_continuous(
    limits = c(0, 50),  # Set the range of the y-axis
    breaks = seq(0, 100, by = 10),  # Set breaks at intervals of 10
    labels = seq(0, 100, by = 10)   # Label the y-axis at intervals of 10
  )


df_temp <- df%>%
  select(CaseId,SURVEY_CONTENT,AGE, GENDER, INCOME)%>%
  mutate(age = case_when(AGE ==1 ~ "18 to 24 years",
                         AGE ==2 ~ "25 to 29 years",
                         AGE ==3 ~ "30 to 34 year",
                         AGE ==4 ~ "35 to 39 years",
                         AGE ==5 ~ "40 to 44 years",
                         AGE ==6 ~ "45 to 49 years",
                         AGE ==7 ~ "50 to 54 years",
                         AGE ==8 ~ "55 to 59 years",
                         AGE ==9 ~ "60 to 64 years",
                         AGE ==10 ~ "65 to 69 years",
                         AGE ==11 ~ "70 to 74 years",
                         AGE ==12 ~ "75 to 79 years",
                         AGE ==13 ~ "80 to 84 years",
                         AGE ==14 ~ "85 years and older",
                         TRUE ~ NA_character_ ))%>%
  mutate(gender = case_when(GENDER ==1 ~ "Male",
                             GENDER ==2 ~ "Female",
                             GENDER ==3 ~ "Other",
                             TRUE ~ NA_character_ ))%>%
  mutate(income = case_when(INCOME ==0 ~ "Under $10,000",
                            INCOME ==1 ~ "$10,000 to $19,999",
                            INCOME ==2 ~ "$20,000 to $29,999",
                            INCOME ==3 ~ "$30,000 to $39,999",
                            INCOME ==4 ~ "$40,000 to $49,999",
                            INCOME ==5 ~ "$50,000 to $59,999",
                            INCOME ==6 ~ "$60,000 to $69,999",
                            INCOME ==7 ~ "$70,000 to $79,999",
                            INCOME ==8 ~ "$80,000 to $89,999",
                            INCOME ==9 ~ "$90,000 to $99,999",
                            INCOME ==10 ~ "$100,000 to $124,999",
                            INCOME ==11 ~ "$125,000 to $149,999",
                            INCOME ==12 ~ "$150,000 and over",
                            TRUE ~ NA_character_ 
                            
  ))
  

# movie # movie # movie # movie # movie # movie # movie # movie # movie
df_movie <- df_temp%>%
  filter(SURVEY_CONTENT==1)
  
# Calculate percentages = Movies
age_percentages <- df_movie %>%
  filter(!is.na(age)) %>%
  count(age) %>%
  mutate(percentage = n / sum(n) * 100)

# Plot with ggplot2
ggplot(age_percentages, aes(x = reorder(age, -percentage), y = percentage, fill = age)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),  # Set y-axis breaks
    limits = c(0, max(age_percentages$percentage) + 5)  # Adjust y-axis limits
  ) +
  labs(
    x = "",
    y = "Percentage (%)",
    title = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10), # Rotate and resize x-axis labels
    legend.position = "none", # Remove the legend (optional),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line()  # Keep axis lines visible
  )+
  scale_y_continuous(
    breaks = seq(0, 100, by = 2),  # Set y-axis breaks
    labels = seq(0, 100, by = 2),  # Set y-axis labels
    limits = c(0, 20)  # Set y-axis limits (optional)
  )


# Calculate percentages = Movies - gender
gender_percentages <- df_movie %>%
  filter(!is.na(gender)) %>%
  count(gender) %>%
  mutate(percentage = n / sum(n) * 100)

# Plot with ggplot2
ggplot(gender_percentages, aes(x = reorder(gender, -percentage), y = percentage, fill = gender)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),  # Set y-axis breaks
    limits = c(0, max(gender_percentages$percentage) + 5)  # Adjust y-axis limits
  ) +
  labs(
    x = "",
    y = "Percentage (%)",
    title = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10), # Rotate and resize x-axis labels
    legend.position = "none", # Remove the legend (optional),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line()  # Keep axis lines visible
  )+
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),  # Set y-axis breaks
    labels = seq(0, 100, by = 10),  # Set y-axis labels
    limits = c(0, 70)  # Set y-axis limits (optional)
  )



# Calculate percentages = Movies - Income
income_percentages <- df_movie %>%
  filter(!is.na(income)) %>%
  count(income) %>%
  mutate(percentage = n / sum(n) * 100)


# Plot with ggplot2
ggplot(income_percentages, aes(x = reorder(income, -percentage), y = percentage, fill = income)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),  # Set y-axis breaks
    limits = c(0, max(income_percentages$percentage) + 5)  # Adjust y-axis limits
  ) +
  labs(
    x = "",
    y = "Percentage (%)",
    title = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10), # Rotate and resize x-axis labels
    legend.position = "none", # Remove the legend (optional),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line()  # Keep axis lines visible
  )+
  scale_y_continuous(
    breaks = seq(0, 100, by = 2),  # Set y-axis breaks
    labels = seq(0, 100, by = 2),  # Set y-axis labels
    limits = c(0, 15)  # Set y-axis limits (optional)
  )

# politics # politics # politics # politics # politics # politics # politics # politics # politics
df_politics <- df_temp%>%
  filter(SURVEY_CONTENT==2)

# Calculate percentages = politicss
age_percentages <- df_politics %>%
  filter(!is.na(age)) %>%
  count(age) %>%
  mutate(percentage = n / sum(n) * 100)

# Plot with ggplot2
ggplot(age_percentages, aes(x = reorder(age, -percentage), y = percentage, fill = age)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),  # Set y-axis breaks
    limits = c(0, max(age_percentages$percentage) + 5)  # Adjust y-axis limits
  ) +
  labs(
    x = "",
    y = "Percentage (%)",
    title = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10), # Rotate and resize x-axis labels
    legend.position = "none", # Remove the legend (optional),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line()  # Keep axis lines visible
  )+
  scale_y_continuous(
    breaks = seq(0, 100, by = 2),  # Set y-axis breaks
    labels = seq(0, 100, by = 2),  # Set y-axis labels
    limits = c(0, 25)  # Set y-axis limits (optional)
  )


# Calculate percentages = politicss - gender
gender_percentages <- df_politics %>%
  filter(!is.na(gender)) %>%
  count(gender) %>%
  mutate(percentage = n / sum(n) * 100)

# Plot with ggplot2
ggplot(gender_percentages, aes(x = reorder(gender, -percentage), y = percentage, fill = gender)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),  # Set y-axis breaks
    limits = c(0, max(gender_percentages$percentage) + 5)  # Adjust y-axis limits
  ) +
  labs(
    x = "",
    y = "Percentage (%)",
    title = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10), # Rotate and resize x-axis labels
    legend.position = "none", # Remove the legend (optional),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line()  # Keep axis lines visible
  )+
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),  # Set y-axis breaks
    labels = seq(0, 100, by = 10),  # Set y-axis labels
    limits = c(0, 70)  # Set y-axis limits (optional)
  )



# Calculate percentages = politicss - Income
income_percentages <- df_politics %>%
  filter(!is.na(income)) %>%
  count(income) %>%
  mutate(percentage = n / sum(n) * 100)


# Plot with ggplot2
ggplot(income_percentages, aes(x = reorder(income, -percentage), y = percentage, fill = income)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),  # Set y-axis breaks
    limits = c(0, max(income_percentages$percentage) + 5)  # Adjust y-axis limits
  ) +
  labs(
    x = "",
    y = "Percentage (%)",
    title = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10), # Rotate and resize x-axis labels
    legend.position = "none", # Remove the legend (optional),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line()  # Keep axis lines visible
  )+
  scale_y_continuous(
    breaks = seq(0, 100, by = 2),  # Set y-axis breaks
    labels = seq(0, 100, by = 2),  # Set y-axis labels
    limits = c(0, 15)  # Set y-axis limits (optional)
  )


# environment # environment # environment # environment # environment # environment # environment # environment # environment
df_environment <- df_temp%>%
  filter(SURVEY_CONTENT==3)

# Calculate percentages = environments
age_percentages <- df_environment %>%
  filter(!is.na(age)) %>%
  count(age) %>%
  mutate(percentage = n / sum(n) * 100)

# Plot with ggplot2
ggplot(age_percentages, aes(x = reorder(age, -percentage), y = percentage, fill = age)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),  # Set y-axis breaks
    limits = c(0, max(age_percentages$percentage) + 5)  # Adjust y-axis limits
  ) +
  labs(
    x = "",
    y = "Percentage (%)",
    title = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10), # Rotate and resize x-axis labels
    legend.position = "none", # Remove the legend (optional),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line()  # Keep axis lines visible
  )+
  scale_y_continuous(
    breaks = seq(0, 100, by = 2),  # Set y-axis breaks
    labels = seq(0, 100, by = 2),  # Set y-axis labels
    limits = c(0, 20)  # Set y-axis limits (optional)
  )


# Calculate percentages = environments - gender
gender_percentages <- df_environment %>%
  filter(!is.na(gender)) %>%
  count(gender) %>%
  mutate(percentage = n / sum(n) * 100)

# Plot with ggplot2
ggplot(gender_percentages, aes(x = reorder(gender, -percentage), y = percentage, fill = gender)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),  # Set y-axis breaks
    limits = c(0, max(gender_percentages$percentage) + 5)  # Adjust y-axis limits
  ) +
  labs(
    x = "",
    y = "Percentage (%)",
    title = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10), # Rotate and resize x-axis labels
    legend.position = "none", # Remove the legend (optional),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line()  # Keep axis lines visible
  )+
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),  # Set y-axis breaks
    labels = seq(0, 100, by = 10),  # Set y-axis labels
    limits = c(0, 70)  # Set y-axis limits (optional)
  )



# Calculate percentages = environments - Income
income_percentages <- df_environment %>%
  filter(!is.na(income)) %>%
  count(income) %>%
  mutate(percentage = n / sum(n) * 100)


# Plot with ggplot2
ggplot(income_percentages, aes(x = reorder(income, -percentage), y = percentage, fill = income)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),  # Set y-axis breaks
    limits = c(0, max(income_percentages$percentage) + 5)  # Adjust y-axis limits
  ) +
  labs(
    x = "",
    y = "Percentage (%)",
    title = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 10), # Rotate and resize x-axis labels
    legend.position = "none", # Remove the legend (optional),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line()  # Keep axis lines visible
  )+
  scale_y_continuous(
    breaks = seq(0, 100, by = 2),  # Set y-axis breaks
    labels = seq(0, 100, by = 2),  # Set y-axis labels
    limits = c(0, 20)  # Set y-axis limits (optional)
  )




# 3 Q12. Please think about why you voted the way you did for the proposals.

df3 <- df %>%
  select(Q12_WHY_VOTED_A1, Q12_WHY_VOTED_A2, Q12_WHY_VOTED_A3, Q12_WHY_VOTED_A4, Q12_WHY_VOTED_A5) %>%
  rename(A1=Q12_WHY_VOTED_A1,
         A2=Q12_WHY_VOTED_A2,
         A3=Q12_WHY_VOTED_A3,
         A4=Q12_WHY_VOTED_A4,
         A5=Q12_WHY_VOTED_A5)%>%
  mutate(across(everything(), as.character)) %>%
  gather(key = "column", value = "value", A1,A2,A3,A4,A5) %>%
  group_by(column, value) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(column) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

# Make sure that NA is included in the factor levels
df3$value <- factor(df3$value, levels = c("1", "2", "3", "NA"))



# Create a bar plot based on the output with legend

ggplot(df3, aes(x = value, y = percentage, fill = value)) +
  geom_bar(stat = "identity", position = "dodge",width = 0.3) +  # "dodge" for side-by-side bars
  facet_wrap(~ column,ncol = 5) +  # Facet by 'column' to create separate plots for each
  labs(
    title = "Q12",
    y = "Percentage",
    fill = "Response Value"  # Rename the legend here
  ) +
  scale_fill_manual(
    values = c("1" = "maroon", "2" = "darkgreen", "3" = "darkblue", "NA" = "gray"),  # Customize colors
    labels = c("1" = "Disagree", "2" = "Neutral", "3" = "Agree", "NA" = "No Response")  # Rename the legend items
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.title = element_blank(),  # Remove the legend title if not needed
    axis.text = element_text(size = 12),  # Adjust axis text size
    axis.title = element_text(size = 14),  # Adjust axis title size
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.title.x = element_blank(),  # Remove the x-axis label
    axis.text.x = element_blank(),  # Remove x-axis text
    axis.ticks.x = element_blank(),  # Remove x-axis ticks

  )+
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),  # Set y-axis breaks
    labels = seq(0, 100, by = 10),  # Set y-axis labels
    limits = c(0, 60)  # Set y-axis limits (optional)
  )


# 4 Q13. When deciding whether to vote yes or no on the water quality proposals, 
# how much did each of the policy features influence your votes?

df4 <- df %>%
  select(Q13_POLICY_INFLUENCE_A1,Q13_POLICY_INFLUENCE_A2,Q13_POLICY_INFLUENCE_A3,Q13_POLICY_INFLUENCE_A4) %>%
  rename(A1=Q13_POLICY_INFLUENCE_A1,
         A2=Q13_POLICY_INFLUENCE_A2,
         A3=Q13_POLICY_INFLUENCE_A3,
         A4=Q13_POLICY_INFLUENCE_A4)%>%
  mutate(across(everything(), as.character)) %>%
  gather(key = "column", value = "value", A1,A2,A3,A4) %>%
  group_by(column, value) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(column) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

ggplot(df4, aes(x = value, y = percentage, fill = value)) +
  geom_bar(stat = "identity", position = "dodge",width = 0.3) +  # "dodge" for side-by-side bars
  facet_wrap(~ column,ncol = 5) +  # Facet by 'column' to create separate plots for each
  labs(
    title = "Q13",
    y = "Percentage",
    fill = "Response Value"  # Rename the legend here
  ) +
  scale_fill_manual(
    values = c("1" = "maroon", "2" = "darkgreen", "3" = "darkblue", "NA" = "gray"),  # Customize colors
    labels = c("1" = "Little or ni effect", "2" = "Moderate effect", "3" = "Large effect", "NA" = "No Response")  # Rename the legend items
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.title = element_blank(),  # Remove the legend title if not needed
    axis.text = element_text(size = 12),  # Adjust axis text size
    axis.title = element_text(size = 14),  # Adjust axis title size
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.title.x = element_blank(),  # Remove the x-axis label
    axis.text.x = element_blank(),  # Remove x-axis text
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    
  )+
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),  # Set y-axis breaks
    labels = seq(0, 100, by = 10),  # Set y-axis labels
    limits = c(0, 60)  # Set y-axis limits (optional)
  )


# 5. Q14.How certain are you about your votes on the water quality proposals?



df5 <- df %>%
  select(Q14_CERTAIN)%>%
  mutate(across(everything(), as.character)) %>%
  gather(key = "column", value = "value",Q14_CERTAIN) %>%
  group_by(column, value) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(column) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()


ggplot(df5, aes(x = value, y = percentage, fill = value)) +
  geom_bar(stat = "identity", position = "dodge",width = 0.3) +  # "dodge" for side-by-side bars
  labs(
    title = "Q14",
    y = "Percentage",
    fill = "Response Value"  # Rename the legend here
  ) +
  scale_fill_manual(
    values = c("1" = "maroon", "2" = "darkgreen", "3" = "yellow", "4" = "darkblue", "NA" = "gray"),  # Customize colors
    labels = c("1" = "Uncertain", "2" = "Somewhat Uncertain", "3" = "Somewhat certain","4"="Certain", "NA" = "No Response")  # Rename the legend items
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.title = element_blank(),  # Remove the legend title if not needed
    axis.text = element_text(size = 12),  # Adjust axis text size
    axis.title = element_text(size = 14),  # Adjust axis title size
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.title.x = element_blank(),  # Remove the x-axis label
    axis.text.x = element_blank(),  # Remove x-axis text
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    
  )+
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),  # Set y-axis breaks
    labels = seq(0, 100, by = 10),  # Set y-axis labels
    limits = c(0, 60)  # Set y-axis limits (optional)
  )



# 6. Q15. To learn more about your general thoughts towards the water quality proposals you voted on, 
#         please select all statements that apply to you.


df6 <- df %>%
  select(Q15_GENERAL_THOUGHTSM1,Q15_GENERAL_THOUGHTSM2,Q15_GENERAL_THOUGHTSM3,Q15_GENERAL_THOUGHTSM4,
         Q15_GENERAL_THOUGHTSM5,Q15_GENERAL_THOUGHTSM6,Q15_GENERAL_THOUGHTSM7,Q15_GENERAL_THOUGHTSM8,
         Q15_GENERAL_THOUGHTSM9,Q15_GENERAL_THOUGHTSM10)%>%
  rename(CH1=Q15_GENERAL_THOUGHTSM1,
         CH2=Q15_GENERAL_THOUGHTSM2,
         CH3=Q15_GENERAL_THOUGHTSM3,
         CH4=Q15_GENERAL_THOUGHTSM4,
         CH5=Q15_GENERAL_THOUGHTSM5,
         CH6=Q15_GENERAL_THOUGHTSM6,
         CH7=Q15_GENERAL_THOUGHTSM7,
         CH8=Q15_GENERAL_THOUGHTSM8,
         CH9=Q15_GENERAL_THOUGHTSM9,
         CH10=Q15_GENERAL_THOUGHTSM10)%>%
  summarise(across(everything(), ~ sum(!is.na(.)), .names = "{col}"))%>%
  pivot_longer(
    cols = everything(),         # Select all columns to pivot
    names_to = "Column",         # Name for the column with original column names
    values_to = "NonNA_Count"    # Name for the column with counts
  )%>%
  mutate(Value = NonNA_Count/1004*100,
         )

df6 <- df6 %>%
  mutate(Column = factor(Column, levels = paste0("CH", 1:10)))  # Set levels explicitly

  
ggplot(df6, aes(x = Column, y = Value, fill = Column)) +
  geom_bar(stat = "identity") +  # Use 'identity' to plot the actual values
  labs(
    title = "",
    x = "",
    y = "Percentage (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    legend.title = element_blank(),  # Remove the legend title if not needed
    #axis.text = element_text(size = 12),  # Adjust axis text size
    #axis.title = element_text(size = 14),  # Adjust axis title size
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
  )+
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),  # Set y-axis breaks
    labels = seq(0, 100, by = 10),  # Set y-axis labels
    limits = c(0, 85)  # Set y-axis limits (optional)
  )


# 7. Q16. Thinking about this survey and what you read, did the survey push you to vote one way or another, 
#         or did it let you make up your mind about which way to vote on the various proposals?



df7 <- df %>%
  select(Q16_SURVEY_VOTE_CHANGE)%>%
  mutate(across(everything(), as.character)) %>%
  gather(key = "column", value = "value",Q16_SURVEY_VOTE_CHANGE) %>%
  group_by(column, value) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(column) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()




ggplot(df7, aes(x = value, y = percentage, fill = value)) +
  geom_bar(stat = "identity", position = "dodge",width = 0.3) +  # "dodge" for side-by-side bars
  labs(
    title = "Q16",
    y = "Percentage (%)",
    fill = "Response Value"  # Rename the legend here
  ) +
  scale_fill_manual(
    values = c("1" = "maroon", "2" = "darkgreen", "3" = "darkblue", "NA" = "gray"),  # Customize colors
    labels = c("1" = "Choice 1", "2" = "Choice 2", "3" = "Choice 3","NA" = "No Response")  # Rename the legend items
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.title = element_blank(),  # Remove the legend title if not needed
    axis.text = element_text(size = 12),  # Adjust axis text size
    axis.title = element_text(size = 14),  # Adjust axis title size
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.title.x = element_blank(),  # Remove the x-axis label
    axis.text.x = element_blank(),  # Remove x-axis text
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    
  )+
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),  # Set y-axis breaks
    labels = seq(0, 100, by = 10),  # Set y-axis labels
    limits = c(0, 60)  # Set y-axis limits (optional)
  )


# 8 Q17. To what extent to you agree or disagree with the following statements. 

df8 <- df %>%
  select(Q17_A1_2,Q17_A2_2,Q17_A3_2,Q17_A4_2,Q17_A5_2) %>%
  rename(A1=Q17_A1_2,
         A2=Q17_A2_2,
         A3=Q17_A3_2,
         A4=Q17_A4_2,
         A5=Q17_A5_2,)%>%
  mutate(across(everything(), as.character)) %>%
  gather(key = "column", value = "value", A1,A2,A3,A4,A5) %>%
  group_by(column, value) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(column) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()


# Make sure that NA is included in the factor levels
df8$value <- factor(df8$value, levels = c("1", "2", "3", "NA"))



# Create a bar plot based on the output with legend

ggplot(df8, aes(x = value, y = percentage, fill = value)) +
  geom_bar(stat = "identity", position = "dodge",width = 0.3) +  # "dodge" for side-by-side bars
  facet_wrap(~ column,ncol = 5) +  # Facet by 'column' to create separate plots for each
  labs(
    title = "Q12",
    y = "Percentage (%)",
    fill = "Response Value"  # Rename the legend here
  ) +
  scale_fill_manual(
    values = c("1" = "maroon", "2" = "darkgreen", "3" = "darkblue","4"="yellow", "5"="purple", "NA" = "gray"),  # Customize colors
    labels = c("1" = "Strongly agree", "2" = "Agree", "3" = "Unsure", "4"= "Disagree", "5"="Strongly disagree ", "NA" = "No Response")  # Rename the legend items
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.title = element_blank(),  # Remove the legend title if not needed
    axis.text = element_text(size = 12),  # Adjust axis text size
    axis.title = element_text(size = 14),  # Adjust axis title size
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.title.x = element_blank(),  # Remove the x-axis label
    axis.text.x = element_blank(),  # Remove x-axis text
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    
  )+
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),  # Set y-axis breaks
    labels = seq(0, 100, by = 10),  # Set y-axis labels
    limits = c(0, 60)  # Set y-axis limits (optional)
  )




# 9. Q19.Are you currently a member of an environmental organization?



df9 <- df %>%
  select(Q19_2)%>%
  mutate(across(everything(), as.character)) %>%
  gather(key = "column", value = "value", Q19_2) %>%
  group_by(column, value) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(column) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()

ggplot(df9, aes(x = value, y = percentage, fill = value)) +
  geom_bar(stat = "identity", position = "dodge",width = 0.3) +  # "dodge" for side-by-side bars
  labs(
    title = "Q19",
    y = "Percentage",
    fill = "Response Value"  # Rename the legend here
  ) +
  scale_fill_manual(
    values = c("1" = "darkgreen", "2" = "maroon", "NA" = "gray"),  # Customize colors
    labels = c("1" = "Yes", "2" = "No", "NA" = "No Response")  # Rename the legend items
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.title = element_blank(),  # Remove the legend title if not needed
    axis.text = element_text(size = 12),  # Adjust axis text size
    axis.title = element_text(size = 14),  # Adjust axis title size
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.title.x = element_blank(),  # Remove the x-axis label
    axis.text.x = element_blank(),  # Remove x-axis text
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    
  )+
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),  # Set y-axis breaks
    labels = seq(0, 100, by = 10),  # Set y-axis labels
    limits = c(0, 100)  # Set y-axis limits (optional)
  )


# 10. Q20.When you decided to live in your current house, 
#.    did you consider the water quality in nearby rivers or streams?


df10 <- df %>%
  select(Q20_2)%>%
  mutate(across(everything(), as.character)) %>%
  gather(key = "column", value = "value", Q20_2) %>%
  group_by(column, value) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(column) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ungroup()


ggplot(df10, aes(x = value, y = percentage, fill = value)) +
  geom_bar(stat = "identity", position = "dodge",width = 0.3) +  # "dodge" for side-by-side bars
  labs(
    title = "Q19",
    y = "Percentage",
    fill = "Response Value"  # Rename the legend here
  ) +
  scale_fill_manual(
    values = c("1" = "darkgreen", "2" = "maroon","3"="yellow" ,"NA" = "gray"),  # Customize colors
    labels = c("1" = "Yes", "2" = "No", "3"="I did not choose my current location", "NA" = "No Response")  # Rename the legend items
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.title = element_blank(),  # Remove the legend title if not needed
    axis.text = element_text(size = 12),  # Adjust axis text size
    axis.title = element_text(size = 14),  # Adjust axis title size
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.title.x = element_blank(),  # Remove the x-axis label
    axis.text.x = element_blank(),  # Remove x-axis text
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    
  )+
  scale_y_continuous(
    breaks = seq(0, 100, by = 10),  # Set y-axis breaks
    labels = seq(0, 100, by = 10),  # Set y-axis labels
    limits = c(0, 100)  # Set y-axis limits (optional)
  )









df3 <- df%>%
  filter(ASC==1)%>%
  select(COST,Q13_INFLUENCE_COST,VOTE)%>%
  filter(VOTE==0)



ggplot(df3, aes(x = factor(COST), fill = factor(Q13_INFLUENCE_COST))) +
  geom_bar(position = "dodge") +
  labs(
    title = "How much did each of the policy features influence your votes?
             Answer : The cost of the policy in additional taxes ",
    x = "Cost Level",
    y = "Count",
    fill = ""
  ) +
  theme_minimal()+
  theme(
    panel.grid = element_blank(),                   # Remove grid lines
    axis.line = element_line(size = 0.8, color = "black"),  # Add main axes
    axis.ticks = element_line(size = 0.8, color = "black"), # Add tick marks
    axis.ticks.length = unit(5, "pt")              # Length of tick marks
  )+
  scale_fill_manual(
    values = c("1" = "lightblue", "2" = "lightgreen","3"="maroon","4"="grey"),          # Assign colors
    labels = c("1" = "Little or no effect", "2" = "Moderate effect", "3"= "Large efefct", "4"="NA")   # Rename legend elements
  )




df4 <- df%>%
  filter(ASC==1)%>%
  select(COST,Q13_INFLUENCE_COST,VOTE)%>%
  filter(VOTE==0)




ggplot(df3, aes(x = factor(COST), fill = factor(Q13_INFLUENCE_COST))) +
  geom_bar(position = "dodge") +
  labs(
    title = "Votes (1 and 0) at Each Cost Level",
    x = "Cost Level",
    y = "Count",
    fill = "Vote"
  ) +
  theme_minimal()