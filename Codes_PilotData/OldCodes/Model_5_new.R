########################################################################################
# Description: RPM Model 5
#######################################################################################

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)

### Set working directory (only works in RStudio)
#apollo_setWorkDir()

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "Model 5",
  modelDescr      = "Mixed-MNL",
  indivID         = "CaseId",  
  nCores          = 4,
  outputDirectory = "output"
)

# ################################################################# #
#### LOAD DATA                                                   ####
# ################################################################# #

database <- read_csv("Deriveddata/processed_pilotdata_1_Apollo.csv")

# Arrange data by RespondentID
database <- database %>%
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

database <- database%>%
  filter(BASELINE_WQ_3UNIT != 1)

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

apollo_beta = c(
  mu_b_asc     = 0,  
  sigma_b_asc = 0.01,
  b_cost  = 0,  
  
  mu_b_wq_local_basin_x_baseline_wq_1unit = 0,
  sigma_b_wq_local_basin_x_baseline_wq_1unit = 0,
  
  mu_b_wq_local_basin_x_baseline_wq_2unit = 0,
  sigma_b_wq_local_basin_x_baseline_wq_2unit = 0,
  
  #mu_b_wq_local_basin_x_baseline_wq_3unit = 0,
  #sigma_b_wq_local_basin_x_baseline_wq_3unit = 0,
  
  mu_b_wq_nonlocal_basin_x_baseline_wq_1unit = 0,
  sigma_b_wq_nonlocal_basin_x_baseline_wq_1unit = 0,
  
  mu_b_wq_nonlocal_basin_x_baseline_wq_2unit = 0,
  sigma_b_wq_nonlocal_basin_x_baseline_wq_2unit = 0,
  
  #mu_b_wq_nonlocal_basin_x_baseline_wq_3unit = 0,
  #sigma_b_wq_nonlocal_basin_x_baseline_wq_3unit = 0,
  
  mu_b_wq_local_sub_basin_x_baseline_wq_1unit = 0,
  sigma_b_wq_local_sub_basin_x_baseline_wq_1unit = 0,
  
  mu_b_wq_local_sub_basin_x_baseline_wq_2unit = 0,
  sigma_b_wq_local_sub_basin_x_baseline_wq_2unit = 0,
  
  #mu_b_wq_local_sub_basin_x_baseline_wq_3unit = 0,
  #sigma_b_wq_local_sub_basin_x_baseline_wq_3unit = 0,
  
  mu_b_wq_nonlocal_sub_basin_x_baseline_wq_1unit = 0,
  sigma_b_wq_nonlocal_sub_basin_x_baseline_wq_1unit = 0,
  
  mu_b_wq_nonlocal_sub_basin_x_baseline_wq_2unit = 0,
  sigma_b_wq_nonlocal_sub_basin_x_baseline_wq_2unit = 0
  
  #mu_b_wq_nonlocal_sub_basin_x_baseline_wq_3unit = 0,
  #sigma_b_wq_nonlocal_sub_basin_x_baseline_wq_3unit = 0

  
)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c()


# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 1000,
  interUnifDraws = c(),
  interNormDraws = c("draws_asc",
                     "draws_b_wq_local_basin_x_baseline_wq_1unit",
                     "draws_b_wq_local_basin_x_baseline_wq_2unit",
                     "draws_b_wq_local_basin_x_baseline_wq_3unit",
                     "draws_b_wq_nonlocal_basin_x_baseline_wq_1unit",
                     "draws_b_wq_nonlocal_basin_x_baseline_wq_2unit",
                     "draws_b_wq_nonlocal_basin_x_baseline_wq_3unit",
                     "draws_b_wq_local_sub_basin_x_baseline_wq_1unit",
                     "draws_b_wq_local_sub_basin_x_baseline_wq_2unit",
                     "draws_b_wq_local_sub_basin_x_baseline_wq_3unit",
                     "draws_b_wq_nonlocal_sub_basin_x_baseline_wq_1unit",
                     "draws_b_wq_nonlocal_sub_basin_x_baseline_wq_2unit",
                     "draws_b_wq_nonlocal_sub_basin_x_baseline_wq_3unit"
  ),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)


### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["b_asc"]] = mu_b_asc + sigma_b_asc*draws_asc 
  
  randcoeff[["b_wq_local_basin_x_baseline_wq_1unit"]] =  mu_b_wq_local_basin_x_baseline_wq_1unit + sigma_b_wq_local_basin_x_baseline_wq_1unit*draws_b_wq_local_basin_x_baseline_wq_1unit
  
  randcoeff[["b_wq_local_basin_x_baseline_wq_2unit"]] =  mu_b_wq_local_basin_x_baseline_wq_2unit + sigma_b_wq_local_basin_x_baseline_wq_2unit*draws_b_wq_local_basin_x_baseline_wq_2unit
  
  #randcoeff[["b_wq_local_basin_x_baseline_wq_3unit"]] =  mu_b_wq_local_basin_x_baseline_wq_3unit + sigma_b_wq_local_basin_x_baseline_wq_3unit*draws_b_wq_local_basin_x_baseline_wq_3unit
  
  
  randcoeff[["b_wq_nonlocal_basin_x_baseline_wq_1unit"]] =  mu_b_wq_nonlocal_basin_x_baseline_wq_1unit + sigma_b_wq_nonlocal_basin_x_baseline_wq_1unit*draws_b_wq_nonlocal_basin_x_baseline_wq_1unit
  
  randcoeff[["b_wq_nonlocal_basin_x_baseline_wq_2unit"]] =  mu_b_wq_nonlocal_basin_x_baseline_wq_2unit + sigma_b_wq_nonlocal_basin_x_baseline_wq_2unit*draws_b_wq_nonlocal_basin_x_baseline_wq_2unit
  
  #randcoeff[["b_wq_nonlocal_basin_x_baseline_wq_3unit"]] =  mu_b_wq_nonlocal_basin_x_baseline_wq_3unit + sigma_b_wq_nonlocal_basin_x_baseline_wq_3unit*draws_b_wq_nonlocal_basin_x_baseline_wq_3unit
  
  
  randcoeff[["b_wq_local_sub_basin_x_baseline_wq_1unit"]] =  mu_b_wq_local_sub_basin_x_baseline_wq_1unit + sigma_b_wq_local_sub_basin_x_baseline_wq_1unit*draws_b_wq_local_sub_basin_x_baseline_wq_1unit
  
  randcoeff[["b_wq_local_sub_basin_x_baseline_wq_2unit"]] =  mu_b_wq_local_sub_basin_x_baseline_wq_2unit + sigma_b_wq_local_sub_basin_x_baseline_wq_2unit*draws_b_wq_local_sub_basin_x_baseline_wq_2unit
  
  #randcoeff[["b_wq_local_sub_basin_x_baseline_wq_3unit"]] =  mu_b_wq_local_sub_basin_x_baseline_wq_3unit + sigma_b_wq_local_sub_basin_x_baseline_wq_3unit*draws_b_wq_local_sub_basin_x_baseline_wq_3unit
  

  randcoeff[["b_wq_nonlocal_sub_basin_x_baseline_wq_1unit"]] =  mu_b_wq_nonlocal_sub_basin_x_baseline_wq_1unit + sigma_b_wq_nonlocal_sub_basin_x_baseline_wq_1unit*draws_b_wq_nonlocal_sub_basin_x_baseline_wq_1unit
  
  randcoeff[["b_wq_nonlocal_sub_basin_x_baseline_wq_2unit"]] =  mu_b_wq_nonlocal_sub_basin_x_baseline_wq_2unit + sigma_b_wq_nonlocal_sub_basin_x_baseline_wq_2unit*draws_b_wq_nonlocal_sub_basin_x_baseline_wq_2unit
  
  #randcoeff[["b_wq_nonlocal_sub_basin_x_baseline_wq_3unit"]] =  mu_b_wq_nonlocal_sub_basin_x_baseline_wq_3unit + sigma_b_wq_nonlocal_sub_basin_x_baseline_wq_3unit*draws_b_wq_nonlocal_sub_basin_x_baseline_wq_3unit
  

  return(randcoeff)
}


# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

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
  V[["policy"]]  = b_asc +
                   b_cost *COST + 
                   b_wq_local_basin_x_baseline_wq_1unit*WQ_BASIN_LOCAL_POLICY_X_BASELINE_WQ_1UNIT+
                   b_wq_local_basin_x_baseline_wq_2unit*WQ_BASIN_LOCAL_POLICY_X_BASELINE_WQ_2UNIT+
                   #b_wq_local_basin_x_baseline_wq_3unit*WQ_BASIN_LOCAL_POLICY_X_BASELINE_WQ_3UNIT+
                   
                   b_wq_nonlocal_basin_x_baseline_wq_1unit*WQ_BASIN_NL_POLICY_X_BASELINE_WQ_1UNIT+
                   b_wq_nonlocal_basin_x_baseline_wq_2unit*WQ_BASIN_NL_POLICY_X_BASELINE_WQ_2UNIT+
                   #b_wq_nonlocal_basin_x_baseline_wq_3unit*WQ_BASIN_NL_POLICY_X_BASELINE_WQ_3UNIT+
                   
                   b_wq_local_sub_basin_x_baseline_wq_1unit*WQ_SUBBASIN_LOCAL_POLICY_SUBONLY_X_BASELINE_WQ_1UNIT+
                   b_wq_local_sub_basin_x_baseline_wq_2unit*WQ_SUBBASIN_LOCAL_POLICY_SUBONLY_X_BASELINE_WQ_2UNIT+
                   #b_wq_local_sub_basin_x_baseline_wq_3unit*WQ_SUBBASIN_LOCAL_POLICY_SUBONLY_X_BASELINE_WQ_3UNIT+
                   
                   b_wq_nonlocal_sub_basin_x_baseline_wq_1unit*WQ_SUBBASIN_NL_POLICY_SUBONLY_X_BASELINE_WQ_1UNIT+
                   b_wq_nonlocal_sub_basin_x_baseline_wq_2unit*WQ_SUBBASIN_NL_POLICY_SUBONLY_X_BASELINE_WQ_2UNIT
                   #b_wq_nonlocal_sub_basin_x_baseline_wq_3unit*WQ_SUBBASIN_NL_POLICY_SUBONLY_X_BASELINE_WQ_3UNIT
    
    
  V[["opt_out"]] = b_wq_local_basin_x_baseline_wq_1unit*WQ_BASIN_LOCAL_CURRENT_X_BASELINE_WQ_1UNIT+
                   b_wq_local_basin_x_baseline_wq_2unit*WQ_BASIN_LOCAL_CURRENT_X_BASELINE_WQ_2UNIT+
                   #b_wq_local_basin_x_baseline_wq_3unit*WQ_BASIN_LOCAL_CURRENT_X_BASELINE_WQ_3UNIT+
    
                   b_wq_nonlocal_basin_x_baseline_wq_1unit*WQ_BASIN_NL_CURRENT_X_BASELINE_WQ_1UNIT+
                   b_wq_nonlocal_basin_x_baseline_wq_2unit*WQ_BASIN_NL_CURRENT_X_BASELINE_WQ_2UNIT+
                   #b_wq_nonlocal_basin_x_baseline_wq_3unit*WQ_BASIN_NL_CURRENT_X_BASELINE_WQ_3UNIT+
    
                   b_wq_local_sub_basin_x_baseline_wq_1unit*WQ_SUBBASIN_LOCAL_CURRENT_SUBONLY_X_BASELINE_WQ_1UNIT+
                   b_wq_local_sub_basin_x_baseline_wq_2unit*WQ_SUBBASIN_LOCAL_CURRENT_SUBONLY_X_BASELINE_WQ_2UNIT+
                   #b_wq_local_sub_basin_x_baseline_wq_3unit*WQ_SUBBASIN_LOCAL_CURRENT_SUBONLY_X_BASELINE_WQ_3UNIT+
    
                   b_wq_nonlocal_sub_basin_x_baseline_wq_1unit*WQ_SUBBASIN_NL_CURRENT_SUBONLY_X_BASELINE_WQ_1UNIT+
                   b_wq_nonlocal_sub_basin_x_baseline_wq_2unit*WQ_SUBBASIN_NL_CURRENT_SUBONLY_X_BASELINE_WQ_2UNIT
                   #b_wq_nonlocal_sub_basin_x_baseline_wq_3unit*WQ_SUBBASIN_NL_CURRENT_SUBONLY_X_BASELINE_WQ_3UNIT
  
  # Define MNL settings
  mnl_settings = list(
    alternatives  = c(policy = 1, opt_out = 0),  # Match to VOTE column coding
    avail         = 1,  # Both alternatives are always available
    choiceVar     = VOTE,  # Choice variable in the dataset
    utilities     = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)

# Display model outputs
apollo_modelOutput(model)

# Save model outputs
apollo_saveOutput(model)
