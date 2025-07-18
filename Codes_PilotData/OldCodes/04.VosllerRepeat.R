# Clean the memory
rm(list = ls())  # Removes all objects in the environment

library(readr)
library(dplyr)
library(apollo)

# Load dataset
library(haven)
apollo_modeChoiceData <- read_dta("~/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Other/Vossler/BCG survey replication data.dta")




# Arrange data by RespondentID
database <- apollo_modeChoiceData %>%
  arrange(RespondentID)

database$RespondentID <- as.vector(database$RespondentID)
database$Cost <- as.vector(database$Cost)
database$Vote <- as.vector(database$Vote)
database$WQ_HUC8 <- as.vector(database$WQ_HUC8)
database$WQ_HUC4 <- as.vector(database$WQ_HUC4)
database$WQ_Medium <- as.vector(database$WQ_Medium)
database$WQ_Large <- as.vector(database$WQ_Large)
database$WQ_HUC4_NL <- as.vector(database$WQ_HUC4_NL)
database$WQ_Medium_NL <- as.vector(database$WQ_Medium_NL)





database <- database %>%
  filter(!is.na(Vote))

# Initialise Apollo
apollo_initialise()

# Set core controls
apollo_control = list(
  modelName       = "MNL_SP",
  modelDescr      = "Simple MNL model on mode choice SP data",
  indivID         = "RespondentID",
  outputDirectory = "output"
)

# Define model parameters
apollo_beta = c(
  asc     = 0,  # Intercept for policy
  b_cost  = 0   # Coefficient for cost
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
  V[["policy"]]  = asc + b_cost *Cost  # Utility for choosing the policy
  V[["opt_out"]] = 0  # Utility for opting out
  
  # Define MNL settings
  mnl_settings = list(
    alternatives  = c(policy = 1, opt_out = 0),  # Match to VOTE column coding
    avail         = 1,  # Both alternatives are always available
    choiceVar     = Vote,  # Choice variable in the dataset
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


###############################################################################
# Initialise Apollo
apollo_initialise()

# Set core controls
apollo_control = list(
  modelName       = "MNL_SP",
  modelDescr      = "Simple MNL model on mode choice SP data",
  indivID         = "RespondentID",
  outputDirectory = "output"
)

# Define model parameters
apollo_beta = c(
  asc     = 0,  # Intercept for policy
  b_cost  = 0,   # Coefficient for cost
  b_WQ_HUC8 = 0,
  b_WQ_HUC4 =0,
  b_WQ_Medium = 0,
  b_WQ_Large = 0,
  b_WQ_HUC4_NL = 0,
  b_WQ_Medium_NL = 0 
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
  V[["policy"]]  = asc + b_cost * Cost + b_WQ_HUC8*WQ_HUC8 + b_WQ_HUC4*WQ_HUC4 + b_WQ_Medium*WQ_Medium +
    b_WQ_Large*WQ_Large + b_WQ_HUC4_NL*WQ_HUC4_NL + b_WQ_Medium_NL*WQ_Medium_NL   # Utility for choosing the policy
  V[["opt_out"]] = 0  # Utility for opting out
  
  # Define MNL settings
  mnl_settings = list(
    alternatives  = c(policy = 1, opt_out = 0),  # Match to VOTE column coding
    avail         = 1,  # Both alternatives are always available
    choiceVar     = Vote,  # Choice variable in the dataset
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
