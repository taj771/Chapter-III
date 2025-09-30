# Count NA values in "COST"
sum(is.na(database$COST))


# Count NA values in "COST"
sum(is.na(database$WQ_SUBBASIN_NL_CURRENT_SUBONLY))

t <- database%>%
  filter(is.na(COST))%>%
  distinct(CaseId, .keep_all = T)



# Read raw data file from CHAISR
df <- read_sav("./Rawdata/Water_Quality_batch2.sav")

tt <- df_all%>%
  filter(is.na(COST))%>%
  filter(CaseId == 50075)%>%
  select(COST)

ttt <- df%>%
  filter(ChoiceID == "B43_3")

ChoiceID = c("B43_3")