database <- read_csv("Deriveddata/processed_pilotdata_1_Apollo.csv")

Q12 <- database%>%
  select(CaseId,Q12_VOTE_WITHOUT_CONSIDER_OTHER,Q12_VOTE_HOUSEHOLD_FACE_COST,
         Q12_VOTE_CERTAIN_PUBLIC_ELEC, Q12_VOTE_INFORM_POLICY_MAKERS,Q12_VOTE_POLICY_ACHIEVE_IMPROV)%>%
  distinct(CaseId, .keep_all = T)

                      
Q12_1 <- Q12 %>%
  group_by(Q12_VOTE_HOUSEHOLD_FACE_COST) %>%
  summarise(count = n()) %>%
  mutate(percent = 100 * count / sum(count))

Q12_2 <- Q12 %>%
  group_by(Q12_VOTE_CERTAIN_PUBLIC_ELEC) %>%
  summarise(count = n()) %>%
  mutate(percent = 100 * count / sum(count))

Q12_3 <- Q12 %>%
  group_by(Q12_VOTE_WITHOUT_CONSIDER_OTHER) %>%
  summarise(count = n()) %>%
  mutate(percent = 100 * count / sum(count))

Q12_4 <- Q12 %>%
  group_by(Q12_VOTE_POLICY_ACHIEVE_IMPROV) %>%
  summarise(count = n()) %>%
  mutate(percent = 100 * count / sum(count))

Q12_5 <- Q12 %>%
  group_by(Q12_VOTE_INFORM_POLICY_MAKERS) %>%
  summarise(count = n()) %>%
  mutate(percent = 100 * count / sum(count))


Q13 <- database%>%
  select(CaseId,Q13_INFLUENCE_WQ_LEVEL,Q13_INFLUENCE_NEAR_HOME,Q13_INFLUENCE_COST,Q13_INFLUENCE_REGIONSIZE)%>%
  distinct(CaseId, .keep_all = T)


Q13_1 <- Q13 %>%
  group_by(Q13_INFLUENCE_COST) %>%
  summarise(count = n()) %>%
  mutate(percent = 100 * count / sum(count))

Q13_1 %>%
  filter(Q13_INFLUENCE_COST %in% c(2, 3)) %>%
  summarise(total_percent = sum(percent))


Q13_2 <- Q13 %>%
  group_by(Q13_INFLUENCE_REGIONSIZE) %>%
  summarise(count = n()) %>%
  mutate(percent = 100 * count / sum(count))


Q13_2 %>%
  filter(Q13_INFLUENCE_REGIONSIZE %in% c(2, 3)) %>%
  summarise(total_percent = sum(percent))


Q13_3 <- Q13 %>%
  group_by(Q13_INFLUENCE_NEAR_HOME) %>%
  summarise(count = n()) %>%
  mutate(percent = 100 * count / sum(count))

Q13_3 %>%
  filter(Q13_INFLUENCE_NEAR_HOME %in% c(2, 3)) %>%
  summarise(total_percent = sum(percent))


Q13_4 <- Q13 %>%
  group_by(Q13_INFLUENCE_WQ_LEVEL) %>%
  summarise(count = n()) %>%
  mutate(percent = 100 * count / sum(count))

Q13_4 %>%
  filter(Q13_INFLUENCE_WQ_LEVEL %in% c(2, 3)) %>%
  summarise(total_percent = sum(percent))


Q16 <- database%>%
  select(CaseId,Q16_SURVEY_PUSH_VOTE)%>%
  distinct(CaseId, .keep_all = T)%>%
  group_by(Q16_SURVEY_PUSH_VOTE)%>%
  summarise(count = n()) %>%
  mutate(percent = 100 * count / sum(count))

Q14 <- database%>%
  select(CaseId,Q14_CERTAIN_VOTE)%>%
  distinct(CaseId, .keep_all = T)%>%
  group_by(Q14_CERTAIN_VOTE)%>%
  summarise(count = n()) %>%
  mutate(percent = 100 * count / sum(count))

Q14 %>%
  filter(Q14_CERTAIN_VOTE %in% c(3, 4)) %>%
  summarise(total_percent = sum(percent))




