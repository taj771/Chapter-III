########################################################################################
# Description: Calculation of WTP of increasing WQ level by one unit based on
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

# Use deltaMethod with explicit inputs
deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "-(b_asc)/(b_cost)"
)

deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "-((b_asc + b_wq_local_basin))/b_cost"
)


# one level improvment 
# Expression: -((b_asc + b_wq_local_basin*(WQ_BASIN_LOCAL_CURRENT-1)) - (b_wq_local_basin*WQ_BASIN_LOCAL_CURRENT))/b_cost


# WTP for one unit improvement of WQ at local basin level

deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "-(b_asc + b_wq_local_basin*(-1))/b_cost"
)

# WTP for one unit improvement of WQ at nonlocal basin level

deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "-(b_asc + b_wq_nonlocal_basin*(-1))/b_cost"
)

# WTP for one unit improvement of WQ at local subbasin level

deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "-(b_asc + b_wq_local_sub_basin*(-1))/b_cost"
)


# WTP for one unit improvement of WQ at nonlocal subbasin level

deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "-(b_asc + b_wq_nonlocal_sub_basin*(-1))/b_cost"
)
