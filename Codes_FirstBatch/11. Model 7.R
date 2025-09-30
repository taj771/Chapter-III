########################################################################################
# Description: RPM Model 8
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
  modelName       = "Model 8",
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
  filter(!is.na(WQ_BASIN_NL_POLICY))

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

apollo_beta = c(
  mu_b_asc     = 0,  
  sigma_b_asc = 0.01,
  b_cost  = 0,   
  mu_b_wq_local_basin = 0,
  sigma_b_wq_local_basin = 0.1,
  mu_b_wq_nonlocal_basin = 0,
  sigma_b_wq_nonlocal_basin = 0.1,
  mu_b_wq_local_sub_basin = 0,
  sigma_b_wq_local_sub_basin = 0.1,
  mu_b_wq_nonlocal_sub_basin = 0,
  sigma_b_wq_nonlocal_sub_basin = 0.1,
  
  b_asc_baseline_wq_0_1 = 0,
  b_asc_baseline_wq_1_2 =0,
  b_asc_baseline_wq_2_3 =0,
  
  #b_cost_baseline_0_1 = 0,
  #b_cost_baseline_1_2 = 0,
  #b_cost_baseline_2_3 = 0,
  
  b_wq_basin_x_bl_0_1 = 0,
  b_wq_basin_x_bl_1_2 = 0,
  b_wq_basin_x_bl_2_3 = 0,
  
  b_wq_subbasin_x_bl_0_1 = 0,
  b_wq_subbasin_x_bl_1_2 = 0,
  b_wq_subbasin_x_bl_2_3 = 0
  
)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c()


# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "sobol",
  interNDraws    = 1000,
  interUnifDraws = c(),
  interNormDraws = c("draws_asc",
                     "draws_wq_local_basin","draws_wq_nonlocal_basin",
                     "draws_wq_local_sub_basin","draws_wq_nonlocal_sub_basin"),
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
  randcoeff[["b_wq_nonlocal_basin"]] =  mu_b_wq_nonlocal_basin + sigma_b_wq_nonlocal_basin*draws_wq_nonlocal_basin
  
  randcoeff[["b_wq_local_sub_basin"]] =  mu_b_wq_local_sub_basin + sigma_b_wq_local_sub_basin*draws_wq_local_sub_basin
  randcoeff[["b_wq_nonlocal_sub_basin"]] =  mu_b_wq_nonlocal_sub_basin + sigma_b_wq_nonlocal_sub_basin*draws_wq_nonlocal_sub_basin
  
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
  V[["policy"]] = b_asc + 
    b_cost * COST + 
    
    # Proposed WQ improvements (policy attributes)
    b_wq_local_basin * WQ_BASIN_LOCAL_POLICY +
    b_wq_nonlocal_basin * WQ_BASIN_NL_POLICY +
    b_wq_local_sub_basin * WQ_SUBBASIN_LOCAL_POLICY_SUBONLY +
    b_wq_nonlocal_sub_basin * WQ_SUBBASIN_NL_POLICY_SUBONLY +
    
    # Baseline WQ direct effects
    b_asc_baseline_wq_0_1 * BASELINE_WQ_0_1UNIT +
    b_asc_baseline_wq_1_2 * BASELINE_WQ_1_2UNIT +
    b_asc_baseline_wq_2_3 * BASELINE_WQ_2_3UNIT +
    
    # Cost × Baseline interactions (existing)
    #b_cost_baseline_0_1 * COST * BASELINE_WQ_0_1UNIT +
    #b_cost_baseline_1_2 * COST * BASELINE_WQ_1_2UNIT +
    #b_cost_baseline_2_3 * COST * BASELINE_WQ_2_3UNIT +
    
    # NEW: Interactions between WQ improvements and baseline WQ
    b_wq_basin_x_bl_0_1 * WQ_POLICY_BASIN * BASELINE_WQ_0_1UNIT *(CHOICE_AREA == "BASIN") +
    b_wq_basin_x_bl_1_2 * WQ_POLICY_BASIN * BASELINE_WQ_1_2UNIT *(CHOICE_AREA == "BASIN") +
    b_wq_basin_x_bl_2_3 * WQ_POLICY_BASIN * BASELINE_WQ_2_3UNIT *(CHOICE_AREA == "BASIN") +
    
    b_wq_subbasin_x_bl_0_1 * WQ_POLICT_SUBBASIN * BASELINE_WQ_0_1UNIT *(CHOICE_AREA == "SUBBASIN") +  
    b_wq_subbasin_x_bl_1_2 * WQ_POLICT_SUBBASIN * BASELINE_WQ_1_2UNIT *(CHOICE_AREA == "SUBBASIN") +
    b_wq_subbasin_x_bl_2_3 * WQ_POLICT_SUBBASIN * BASELINE_WQ_2_3UNIT *(CHOICE_AREA == "SUBBASIN")
    

  V[["opt_out"]] = 
    b_wq_local_basin * WQ_BASIN_LOCAL_CURRENT +
    b_wq_nonlocal_basin * WQ_BASIN_NL_CURRENT +
    b_wq_local_sub_basin * WQ_SUBBASIN_LOCAL_CURRENT_SUBONLY +
    b_wq_nonlocal_sub_basin * WQ_SUBBASIN_NL_CURRENT_SUBONLY
  
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

