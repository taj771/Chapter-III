########################################################################################
# Description: MNL Model 4_1 (This is an extension of MNL model 4 where we incorporate WQ change
# at sub basin level as WQ at sub basin level insted of Basin level average compared to MNL Model 4)
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
  
  filter(!is.na(WQ_SUBBASIN_LOCAL_CURRENT_SUBONLY))%>%
  filter(!is.na(WQ_SUBBASIN_NL_CURRENT_SUBONLY))%>%
  
  filter(!is.na(WQ_SUBBASIN_LOCAL_POLICY_SUBONLY))%>%
  filter(!is.na(WQ_SUBBASIN_NL_POLICY_SUBONLY))%>%
  
  filter(!is.na(WQ_BASIN_LOCAL_CURRENT))%>%
  filter(!is.na(WQ_BASIN_NL_CURRENT))%>%
  
  filter(!is.na(WQ_BASIN_LOCAL_POLICY))%>%
  filter(!is.na(WQ_BASIN_NL_POLICY))





# Initialise Apollo
apollo_initialise()

# Set core controls
apollo_control = list(
  modelName       = "MNL Model 4_1",
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
    b_wq_local_sub_basin*WQ_SUBBASIN_LOCAL_POLICY_SUBONLY +
    b_wq_nonlocal_sub_basin*WQ_SUBBASIN_NL_POLICY_SUBONLY
  
  
  V[["opt_out"]] = 
    b_wq_local_basin*WQ_BASIN_LOCAL_CURRENT +
    b_wq_nonlocal_basin*WQ_BASIN_NL_CURRENT 
  b_wq_local_sub_basin*WQ_SUBBASIN_LOCAL_CURRENT_SUBONLY +
    b_wq_nonlocal_sub_basin*WQ_SUBBASIN_NL_CURRENT_SUBONLY
  
  
  
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



# Save model outputs
apollo_saveOutput(model)