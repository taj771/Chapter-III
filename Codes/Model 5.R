########################################################################################
# Description: Model 5 (RPM)
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
  modelName       = "Model 3",
  modelDescr      = "Mixed-MNL",
  indivID         = "CaseId",  
  nCores          = 8,
  outputDirectory = "output",
  weights = "WEIGHT"
)

# ################################################################# #
#### LOAD DATA                                                   ####
# ################################################################# #

database <- read_csv("./Deriveddata/processed_finaldata_batch_1_Apollo.csv")


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
  filter(!is.na(WQ_BASIN_NL_POLICY))%>%
  
  filter(!is.na(AREA_INSTATE_LOCAL_BASIN))%>%
  filter(!is.na(AREA_INSTATE_NL_BASIN))%>%
  
  filter(!is.na(AREA_INSTATE_LOCAL_SUBBASIN))%>%
  filter(!is.na(AREA_INSTATE_NL_SUBBASIN))%>%
  
  filter(!is.na(WQ_NON_LOCAL_ADJUCENT_LOCAL_BASIN_POLICY))%>%
  filter(!is.na(WQ_NON_LOCAL_ADJUCENT_LOCAL_SUBBASIN_POLCIY))%>%
  
  filter(!is.na(WQ_NON_LOCAL_NOT_ADJUCENT_LOCAL_BASIN_POLICY))%>%
  filter(!is.na(WQ_NON_LOCAL_NOT_ADJUCENT_LOCAL_SUBBASIN_POLCIY))%>%
  
  filter(!is.na(WQ_NON_LOCAL_ADJUCENT_LOCAL_BASIN_CURRENT))%>%
  filter(!is.na(WQ_NON_LOCAL_ADJUCENT_LOCAL_SUBBASIN_CURRENT))%>%
  
  filter(!is.na(WQ_NON_LOCAL_NOT_ADJUCENT_LOCAL_BASIN_CURRENT))%>%
  filter(!is.na(WQ_NON_LOCAL_NOT_ADJUCENT_LOCAL_SUBBASIN_CURRENT))
  
  
  
  
  



# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

apollo_beta = c(
  mu_b_asc     = 0,  
  sigma_b_asc = 0.01,
  
  b_cost  = 0,  
  
  mu_b_wq_local_basin = 0,
  sigma_b_wq_local_basin = 0.01,
  
  mu_b_wq_local_sub_basin = 0,
  sigma_b_wq_local_sub_basin = 0.01,
  
  mu_b_wq_nonlocal_adj_local_basin = 0,
  sigma_b_wq_nonlocal_adj_local_basin = 0,
  
  mu_b_wq_nonlocal_adj_local_sub_basin = 0,
  sigma_b_wq_nonlocal_adj_local_sub_basin = 0,
  
  mu_b_wq_nonlocal_not_adj_local_basin = 0,
  sigma_b_wq_nonlocal_not_adj_local_basin = 0,
  
  mu_b_wq_nonlocal_not_adj_local_sub_basin = 0,
  sigma_b_wq_nonlocal_not_adj_local_sub_basin = 0
  
)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c()


# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "sobol",
  interNDraws    = 5000,
  interUnifDraws = c(),
  interNormDraws = c("draws_asc",
                     "draws_wq_local_basin",
                     "draws_wq_local_sub_basin",
                     "draws_wq_nonlocal_adj_local_basin", "draws_wq_nonlocal_adj_local_sub_basin",  
                     "draws_wq_nonlocal_not_adj_local_basin", "draws_wq_nonlocal_not_adj_local_sub_basin"
  ),
  intraDrawsType = "sobol",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)


### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["b_asc"]] = mu_b_asc + sigma_b_asc*draws_asc 
  
  randcoeff[["b_wq_local_basin"]] =  mu_b_wq_local_basin + sigma_b_wq_local_basin*draws_wq_local_basin
  randcoeff[["b_wq_local_sub_basin"]] =  mu_b_wq_local_sub_basin + sigma_b_wq_local_sub_basin*draws_wq_local_sub_basin

  randcoeff[["b_wq_nonlocal_adj_local_basin"]] =  mu_b_wq_nonlocal_adj_local_basin + sigma_b_wq_nonlocal_adj_local_basin*draws_wq_nonlocal_adj_local_basin
  randcoeff[["b_wq_nonlocal_adj_local_sub_basin"]] =  mu_b_wq_nonlocal_adj_local_sub_basin + sigma_b_wq_nonlocal_adj_local_sub_basin*draws_wq_nonlocal_adj_local_sub_basin
  
  randcoeff[["b_wq_nonlocal_not_adj_local_basin"]] =  mu_b_wq_nonlocal_not_adj_local_basin + sigma_b_wq_nonlocal_not_adj_local_basin*draws_wq_nonlocal_not_adj_local_basin
  randcoeff[["b_wq_nonlocal_not_adj_local_sub_basin"]] =  mu_b_wq_nonlocal_not_adj_local_sub_basin + sigma_b_wq_nonlocal_not_adj_local_sub_basin*draws_wq_nonlocal_not_adj_local_sub_basin
  

  
  
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
    b_wq_local_basin*WQ_BASIN_LOCAL_POLICY +
    b_wq_local_sub_basin*WQ_SUBBASIN_LOCAL_POLICY_SUBONLY +
    b_wq_nonlocal_adj_local_basin*WQ_NON_LOCAL_ADJUCENT_LOCAL_BASIN_POLICY +
    b_wq_nonlocal_adj_local_sub_basin*WQ_NON_LOCAL_ADJUCENT_LOCAL_SUBBASIN_POLCIY +
    b_wq_nonlocal_not_adj_local_basin*WQ_NON_LOCAL_NOT_ADJUCENT_LOCAL_BASIN_POLICY +
    b_wq_nonlocal_not_adj_local_sub_basin*WQ_NON_LOCAL_NOT_ADJUCENT_LOCAL_SUBBASIN_POLCIY
  
  V[["opt_out"]] = 
    b_wq_local_basin*WQ_BASIN_LOCAL_CURRENT +
    b_wq_local_sub_basin*WQ_SUBBASIN_LOCAL_CURRENT_SUBONLY+
    b_wq_nonlocal_adj_local_basin*WQ_NON_LOCAL_ADJUCENT_LOCAL_BASIN_CURRENT +
    b_wq_nonlocal_adj_local_sub_basin*WQ_NON_LOCAL_ADJUCENT_LOCAL_SUBBASIN_CURRENT +
    b_wq_nonlocal_not_adj_local_basin*WQ_NON_LOCAL_NOT_ADJUCENT_LOCAL_BASIN_CURRENT +
    b_wq_nonlocal_not_adj_local_sub_basin*WQ_NON_LOCAL_NOT_ADJUCENT_LOCAL_SUBBASIN_CURRENT
  
  
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
  
  ### Apply weights here (note the functionality argument)
  P = apollo_weighting(P, apollo_inputs, functionality)
  
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

