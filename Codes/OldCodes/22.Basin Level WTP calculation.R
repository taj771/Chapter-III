########################################################################################
# Description: MNL Model 4
# WQ parameters further seperate into local basin changes, local sub basin changes, 
# non-local basin changes and non-local sub basin chnages
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



# Qu'Appelle basin -  if it is local basin
deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "-((b_asc + b_wq_local_basin*2 - (b_wq_local_basin*3))/b_cost)"
)

# Qu'Appelle basin -  if it is nonlocal basin
deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "-((b_asc + b_wq_nonlocal_basin*2 - (b_wq_nonlocal_basin*3))/b_cost)"
)

# Assiniboine basin -  if it is local basin
deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "-((b_asc + b_wq_local_basin*2 - (b_wq_local_basin*3))/b_cost)"
)

# Assiniboine basin -  if it is nonlocal basin
deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "-((b_asc + b_wq_nonlocal_basin*2 - (b_wq_nonlocal_basin*3))/b_cost)"
)


# Souris basin -  if it is local basin
deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "-((b_asc + b_wq_local_basin*2 - (b_wq_local_basin*3))/b_cost)"
)

# Souris basin -  if it is nonlocal basin
deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "-((b_asc + b_wq_nonlocal_basin*2 - (b_wq_nonlocal_basin*3))/b_cost)"
)

# Red basin -  if it is local basin
deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "-((b_asc + b_wq_local_basin*2 - (b_wq_local_basin*3))/b_cost)"
)

# Red basin -  if it is nonlocal basin
deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "-((b_asc + b_wq_nonlocal_basin*2 - (b_wq_nonlocal_basin*3))/b_cost)"
)

# Grass and Burntwood basin -  if it is local basin
deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "-((b_asc + b_wq_local_basin*2 - (b_wq_local_basin*5))/b_cost)"
)

# Grass and Burntwood basin -  if it is nonlocal basin
deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "-((b_asc + b_wq_nonlocal_basin*2 - (b_wq_nonlocal_basin*5))/b_cost)"
)


# Nelson  basin -  if it is local basin
deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "-((b_asc + b_wq_local_basin*2 - (b_wq_local_basin*4))/b_cost)"
)

# Nelson  basin -  if it is nonlocal basin
deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "-((b_asc + b_wq_nonlocal_basin*2 - (b_wq_nonlocal_basin*4))/b_cost)"
)







# Battle basin -  if it is local basin
deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "-((b_asc + b_wq_local_basin*2 - (b_wq_local_basin*5))/b_cost)"
)

# Battle basin -  if it is nonlocal basin
deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "-((b_asc + b_wq_nonlocal_basin*2 - (b_wq_nonlocal_basin*5))/b_cost)"
)



