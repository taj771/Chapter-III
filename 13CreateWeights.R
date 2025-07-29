################################################################################
### Applying survey weights to full dataset
################################################################################

rm(list=ls(all=TRUE))


#-----------------------------------------------------------------------------
## create weights for full dataset
#-----------------------------------------------------------------------------

target = readRDS("data/derived/TargetPopulationWeights.RDS")

df_anomolies = read_csv("data/derived/AnomolyResponses.csv")

df <- read_csv("data/derived/WeightingCharacteristicsData.csv") %>%
  filter(elicit_format == "wtp", sbc == "ce") %>%
  left_join(df_anomolies)  %>%
  select(-lean_program, -lean_sq) %>%
  mutate(across(!is.numeric, ~as.factor(.)))

summary(df)

isTRUE(length(unique(df$age_group)) == length(target$age_group))
isTRUE(length(unique(df$gender)) == length(target$gender))
isTRUE(length(unique(df$income_group)) == length(target$income_group))
isTRUE(length(unique(df$age_group)) == length(target$age_group))
isTRUE(length(unique(df$age_group)) == length(target$age_group))

df = as.data.frame(df)

outsave<- anesrake(target, df, 
                   pctlim = 5,
                   cap = 10,
                   caseid=df$case_id)

df_weight_all <- data.frame(case_id=outsave$caseid, 
                            weights_all=outsave$weightvec)

summary(df_weight_all)

df = as.data.frame(df) %>%
  filter(warmglow_6 == 0)

outsave<- anesrake(target, df, 
                   pctlim = 5,
                   cap = 10,
                   caseid=df$case_id)

df_weight <- data.frame(case_id=outsave$caseid, 
                        weights=outsave$weightvec)
summary(df_weight)

weights_out = summary(outsave)


df = as.data.frame(df) %>%
  filter(warmglow_5 == 0)

outsave<- anesrake(target, df, 
                   pctlim = 5,
                   cap = 10,
                   caseid=df$case_id)

df_weight_5 <- data.frame(case_id=outsave$caseid, 
                        weights_5=outsave$weightvec)
summary(df_weight_5)

df_weight_all = df_weight_all |>
  left_join(df_weight) |>
  left_join(df_weight_5)

write_csv(df_weight_all, "data/derived/SurveyWeights.csv")


df_test = as_tibble(weights_out$age_group) |>
  mutate(names = dimnames(weights_out$age_group)[[1]],
         type = "Age groups")

df_test = as_tibble(weights_out$gender) |>
  mutate(names = dimnames(weights_out$gender)[[1]],
         type = "Male") |>
  bind_rows(df_test)

df_test = as_tibble(weights_out$income_group) |>
  mutate(names = dimnames(weights_out$income_group)[[1]],
         type = "Income Group") |>
  bind_rows(df_test)

df_test = as_tibble(weights_out$province) |>
  mutate(names = dimnames(weights_out$province)[[1]],
         type = "Province") |>
  bind_rows(df_test)

df_test = as_tibble(weights_out$education_group) |>
  mutate(names = dimnames(weights_out$education_group)[[1]],
         type = "Education group") |>
  bind_rows(df_test)|>
  clean_names() |>
  select(population = target,
         sample = unweighted_percent,
         names, type) |>
  pivot_longer(-c(names, type), names_to = "comparison") |>
  mutate(value = value * 100) |>
  filter(names != "Total")


p1 = df_test |>
  filter(type == "Age groups") |>
  mutate(names = fct_rev(as.factor(names))) |>
ggplot(aes(x = value, y = names, fill = comparison)) +
  geom_bar(position = "dodge", stat="identity") +
#  facet_wrap(~type, ncol = 2,
 #            scales = "free") +
  labs(fill = "",
       x = "",
       y = "",
       title = "Age group") +
  theme_bw()+ theme(legend.position = "none")

p2 = df_test |>
  filter(type == "Province") |>
  mutate(names = fct_rev(as.factor(names))) |>
  mutate(names = fct_rev(fct_relevel(names,
         "British Columbia",
         "Alberta",
         "Saskatchewan",
         "Manitoba",
         "Ontario",
         "Quebec",
         "Atlantic Provinces"))) |>
  ggplot(aes(x = value, y = names, fill = comparison)) +
  geom_bar(position = "dodge", stat="identity") +
  #  facet_wrap(~type, ncol = 2,
  #            scales = "free") +
  labs(fill = "",
       x = "",
       y = "",
       title = "Geographic region") +
  theme_bw()+ theme(legend.position = "none")

p3 = df_test |>
  filter(type == "Male") |>
  mutate(names = ifelse(names == "Man", "Male", "Female")) |>
  ggplot(aes(x = value, y = names, fill = comparison)) +
  geom_bar(position = "dodge", stat="identity") +
  #  facet_wrap(~type, ncol = 2,
  #            scales = "free") +
  labs(fill = "",
       x = "",
       y = "",
       title = "Gender") +
  theme_bw()+ theme(legend.position = "none")


p4 = df_test |>
  filter(type == "Education group") |>
  mutate(names = fct_rev(fct_relevel(as.factor(names),
                                     "Advanced degree",
                                     "Bachelors degree",
                                     "Some University/College",
                                     "Vocational/Trade/Technical School",
                                     "High school or less"))) |>
  ggplot(aes(x = value, y = names, fill = comparison)) +
  geom_bar(position = "dodge", stat="identity") +
  #  facet_wrap(~type, ncol = 2,
  #            scales = "free") +
  labs(fill = "",
       x = "",
       y = "",
       title = "Educational Attainment") +
  theme_bw()+ theme(legend.position = "none")


p5 = df_test |>
  filter(type == "Income Group") |>
  mutate(names = fct_rev(fct_relevel(as.factor(names),
                                     "$150,000 and over", after = Inf))) |>
  ggplot(aes(x = value, y = names, fill = comparison)) +
  geom_bar(position = "dodge", stat="identity") +
  #  facet_wrap(~type, ncol = 2,
  #            scales = "free") +
  labs(fill = "",
       x = "Percentage",
       y = "",
       title = "Income Level") +
  theme_bw()

p4 + p5 + p2 + (p1 / p3)

ggsave("figs/sample_comparison.png")

df_test |>
  ggplot(aes(x = value, y = names, fill = comparison)) +
  geom_bar(position = "dodge", stat="identity") +
  facet_wrap(~type, ncol = 2,
             scales = "free") +
  labs(fill = "Group") +
  theme_bw()

ggsave("figs/sample_population.png")

df_test |>
  ggplot(aes(y = value, x = names, fill = comparison, colour = comparison, group = comparison)) +
  geom_line() +
  facet_wrap(~type, ncol = 2,
             scales = "free") +
  coord_flip()


df_test |>
  pivot_wider(id_cols = c(type, names), 
              values_from = value, 
              names_from = comparison) |>
  write_csv("output/SamplePopulationComparison.csv")




