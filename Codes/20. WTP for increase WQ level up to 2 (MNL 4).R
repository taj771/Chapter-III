########################################################################################
# Description: Calculation of WTP of increasing WQ level up to level 2 (based on current level)
# MNL Model 4
#######################################################################################

# Clean the memory
rm(list = ls())  # Removes all objects in the environment

# Load dataset
apollo_modeChoiceData <- read_csv("Deriveddata/processed_pilotdata_1_Apollo.csv")

# Arrange data by RespondentID
database <- apollo_modeChoiceData %>%
  arrange(CaseId)


database <- database %>%
  filter(!is.na(VOTE))%>%
  
  filter(!is.na(WQ_SUBBASIN_LOCAL_CURRENT))%>%
  filter(!is.na(WQ_SUBBASIN_NL_CURRENT))%>%
  
  filter(!is.na(WQ_SUBBASIN_LOCAL_POLICY))%>%
  filter(!is.na(WQ_SUBBASIN_NL_POLICY))%>%
  
  filter(!is.na(WQ_BASIN_LOCAL_CURRENT))%>%
  filter(!is.na(WQ_BASIN_NL_CURRENT))%>%
  
  filter(!is.na(WQ_BASIN_LOCAL_POLICY))%>%
  filter(!is.na(WQ_BASIN_NL_POLICY))





# Initialise Apollo
apollo_initialise()

# Set core controls
apollo_control = list(
  modelName       = "MNL_SP",
  modelDescr      = "Simple MNL model on mode choice SP data",
  indivID         = "CaseId",
  outputDirectory = "output"
)

# Define model parameters
apollo_beta = c(
  b_asc     = 0, 
  b_cost  = 0,   
  b_wq_local_basin =0,           # WQ chnage in local basin
  b_wq_nonlocal_basin =0,        # WQ chnage in non-local basin
  b_wq_local_sub_basin = 0,      # WQ chnage in local sub basin
  b_wq_nonlocal_sub_basin = 0.   # WQ chnage in non-local sub basin
  
) 



# No parameters are fixed
apollo_fixed = c()

# Validate inputs
apollo_inputs = apollo_validateInputs()

# Define model and likelihood function
apollo_probabilities = function(apollo_beta, apollo_inputs, functionality = "estimate") {
  
  # Attach inputs
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  # Create list of probabilities
  P = list()
  
  # Define utilities
  V = list()
  V[["policy"]]  = b_asc + b_cost *COST + 
    b_wq_local_basin*WQ_BASIN_LOCAL_POLICY +
    b_wq_nonlocal_basin*WQ_BASIN_NL_POLICY +
    b_wq_local_sub_basin*WQ_SUBBASIN_LOCAL_POLICY +
    b_wq_nonlocal_sub_basin*WQ_SUBBASIN_NL_POLICY
  
  
  V[["opt_out"]] = 
    b_wq_local_basin*WQ_BASIN_LOCAL_CURRENT +
    b_wq_nonlocal_basin*WQ_BASIN_NL_CURRENT 
  b_wq_local_sub_basin*WQ_SUBBASIN_LOCAL_CURRENT+
    b_wq_nonlocal_sub_basin*WQ_SUBBASIN_NL_CURRENT
  
  
  
  # Define MNL settings
  mnl_settings = list(
    alternatives  = c(policy = 1, opt_out = 0),  # Match to VOTE column coding
    avail         = 1,  # Both alternatives are always available
    choiceVar     = VOTE,  # Choice variable in the dataset
    utilities     = V
  )
  
  # Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  # Take product across observations for the same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  # Prepare and return probabilities
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# Estimate the model
model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# Display model outputs
apollo_modelOutput(model)


# Extract coefficients and covariance matrix
coef_values <- model$estimate
vcov_matrix <- model$robvarcov


# Calculation of WTP for increasing WQ level upto 2 at non-local subbasin scale

# filter choices of non-local - subbasin level
# then calculate WTP for each individual separately based on their current WQ level

df1 <- database%>%
  filter(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "NONLOCAL")%>%
  select(CaseId,CHOICE_AREA,CHOICE_LOCALITY_SUBBASIN,WQ_SUBBASIN_NL_CURRENT)


df1 <- df1 %>%
  mutate(delta_result = map(WQ_SUBBASIN_NL_CURRENT, function(wq) {
    g_expr <- "-((b_asc + b_wq_nonlocal_sub_basin*2 - (b_wq_nonlocal_sub_basin*wq))/b_cost)"
    deltaMethod(object = coef_values, vcov. = vcov_matrix, g = g_expr)
  })) %>%
  mutate(estimate = map_dbl(delta_result, ~ .x$Estimate),
         se       = map_dbl(delta_result, ~ .x$SE),
         `2.5%`   = map_dbl(delta_result, ~ .x$`2.5 %`),
         `97.5%`  = map_dbl(delta_result, ~ .x$`97.5 %`))



mean(df1$estimate)



# Calculation of WTP for increasing WQ level upto 2 at local sub-basin scale
# filter choices of local - sub-basin level
# then calculate WTP for each individual separately based on their current WQ level


df2 <- database%>%
  filter(CHOICE_AREA == "SUBBASIN" & CHOICE_LOCALITY_SUBBASIN == "LOCAL")%>%
  select(CaseId,CHOICE_AREA,CHOICE_LOCALITY_SUBBASIN,WQ_SUBBASIN_LOCAL_CURRENT)



df2 <- df2 %>%
  mutate(delta_result = map(WQ_SUBBASIN_LOCAL_CURRENT, function(wq) {
    g_expr <- "-((b_asc + b_wq_local_sub_basin*2 - (b_wq_local_sub_basin*wq))/b_cost)"
    deltaMethod(object = coef_values, vcov. = vcov_matrix, g = g_expr)
  })) %>%
  mutate(estimate = map_dbl(delta_result, ~ .x$Estimate),
         se       = map_dbl(delta_result, ~ .x$SE),
         `2.5%`   = map_dbl(delta_result, ~ .x$`2.5 %`),
         `97.5%`  = map_dbl(delta_result, ~ .x$`97.5 %`))

mean(df2$estimate)

# Calculation of WTP for increasing WQ level upto 2 at non-local basin scale
# filter choices of non-local - basin level
# then calculate WTP for each individual separately based on their current WQ level


df3 <- database%>%
  filter(CHOICE_AREA == "BASIN" & CHOICE_LOCALITY_SUBBASIN == "NONLOCAL")


df3 <- df3 %>%
  mutate(delta_result = map(WQ_BASIN_NL_CURRENT, function(wq) {
    g_expr <- "-((b_asc + b_wq_nonlocal_basin*2 - (b_wq_nonlocal_basin*wq))/b_cost)"
    deltaMethod(object = coef_values, vcov. = vcov_matrix, g = g_expr)
  })) %>%
  mutate(estimate = map_dbl(delta_result, ~ .x$Estimate),
         se       = map_dbl(delta_result, ~ .x$SE),
         `2.5%`   = map_dbl(delta_result, ~ .x$`2.5 %`),
         `97.5%`  = map_dbl(delta_result, ~ .x$`97.5 %`))


mean(df3$estimate)

# Calculation of WTP for increasing WQ level upto 2 at local basin scale
# filter choices of local - basin level
# then calculate WTP for each individual separately based on their current WQ level



df4 <- database%>%
  filter(CHOICE_AREA == "BASIN" & CHOICE_LOCALITY_BASIN == "LOCAL")


df4 <- df4 %>%
  mutate(delta_result = map(WQ_BASIN_LOCAL_CURRENT, function(wq) {
    g_expr <- "-((b_asc + b_wq_local_basin*2 - (b_wq_local_basin*wq))/b_cost)"
    deltaMethod(object = coef_values, vcov. = vcov_matrix, g = g_expr)
  })) %>%
  mutate(estimate = map_dbl(delta_result, ~ .x$Estimate),
         se       = map_dbl(delta_result, ~ .x$SE),
         `2.5%`   = map_dbl(delta_result, ~ .x$`2.5 %`),
         `97.5%`  = map_dbl(delta_result, ~ .x$`97.5 %`))


mean(df4$estimate)


