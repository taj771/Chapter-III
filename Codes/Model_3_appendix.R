########################################################################################
# Description: RPM Model 2
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
  modelName       = "RPM Model 1",
  modelDescr      = "Mixed-MNL",
  indivID         = "CaseId",  
  nCores          = 8,
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

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

apollo_beta = c(
  mu_b_asc     = 0,  
  sigma_b_asc = 0.01,
  
  mu_b_asc_shared_bound = 0,
  sigma_b_asc_shared_bound = 0,
  
  mu_b_asc_home_provshare = 0,
  sigma_b_asc_home_provshare = 0,
  
  b_cost  = 0,  
  
  mu_b_wq_local_basin = 0,
  sigma_b_wq_local_basin = 0.01,
  
  mu_b_wq_nonlocal_basin = 0,
  sigma_b_wq_nonlocal_basin = 0.01,
  
  mu_b_wq_sub_basin_local_nsb = 0,
  sigma_b_wq_sub_basin_local_nsb = 0.01,
  
  mu_b_wq_sub_basin_local_sb = 0,
  sigma_b_wq_sub_basin_local_sb = 0.01,
  
  mu_b_wq_sub_basin_nonlocal_nsb_local_prov = 0,
  sigma_b_wq_sub_basin_nonlocal_nsb_local_prov = 0.01,
  
  mu_b_wq_sub_basin_nonlocal_sb_local_prov = 0,
  sigma_b_wq_sub_basin_nonlocal_sb_local_prov = 0.01,
  
  mu_b_wq_sub_basin_nonlocal_nsb_nonlocal_prov = 0,
  sigma_b_wq_sub_basin_nonlocal_nsb_nonlocal_prov = 0.01,
  
  mu_b_wq_sub_basin_nonlocal_sb_nonlocal_prov = 0,
  sigma_b_wq_sub_basin_nonlocal_sb_nonlocal_prov = 0.01
  
  
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
                     "draws_asc_shared_bound",
                     "draws_asc_home_provshare",
                     "draws_wq_local_basin","draws_wq_nonlocal_basin",
                     "draws_wq_sub_basin_local_nsb", "draws_wq_sub_basin_local_sb",
                     "draws_wq_sub_basin_nonlocal_nsb_local_prov", "draws_wq_sub_basin_nonlocal_sb_local_prov",
                     "draws_wq_sub_basin_nonlocal_nsb_nonlocal_prov", "draws_wq_sub_basin_nonlocal_sb_nonlocal_prov"
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
  
  randcoeff[["b_asc_shared_bound"]] = mu_b_asc_shared_bound + sigma_b_asc_shared_bound*draws_asc_shared_bound
  
  randcoeff[["b_asc_home_provshare"]] = mu_b_asc_home_provshare + sigma_b_asc_home_provshare*draws_asc_home_provshare 
  
  randcoeff[["b_wq_local_basin"]] =  mu_b_wq_local_basin + sigma_b_wq_local_basin*draws_wq_local_basin
  randcoeff[["b_wq_nonlocal_basin"]] =  mu_b_wq_nonlocal_basin + sigma_b_wq_nonlocal_basin*draws_wq_nonlocal_basin
  
  randcoeff[["b_wq_sub_basin_local_nsb"]] =  mu_b_wq_sub_basin_local_nsb + sigma_b_wq_sub_basin_local_nsb*draws_wq_sub_basin_local_nsb
  randcoeff[["b_wq_sub_basin_local_sb"]] =  mu_b_wq_sub_basin_local_sb + sigma_b_wq_sub_basin_local_sb*draws_wq_sub_basin_local_sb
  
  randcoeff[["b_wq_sub_basin_nonlocal_nsb_local_prov"]] =  mu_b_wq_sub_basin_nonlocal_nsb_local_prov + sigma_b_wq_sub_basin_nonlocal_nsb_local_prov*draws_wq_sub_basin_nonlocal_nsb_local_prov
  randcoeff[["b_wq_sub_basin_nonlocal_sb_local_prov"]] =  mu_b_wq_sub_basin_nonlocal_sb_local_prov + sigma_b_wq_sub_basin_nonlocal_sb_local_prov*draws_wq_sub_basin_nonlocal_sb_local_prov
  
  randcoeff[["b_wq_sub_basin_nonlocal_nsb_nonlocal_prov"]] =  mu_b_wq_sub_basin_nonlocal_nsb_nonlocal_prov + sigma_b_wq_sub_basin_nonlocal_nsb_nonlocal_prov*draws_wq_sub_basin_nonlocal_nsb_nonlocal_prov
  randcoeff[["b_wq_sub_basin_nonlocal_sb_nonlocal_prov"]] =  mu_b_wq_sub_basin_nonlocal_sb_nonlocal_prov + sigma_b_wq_sub_basin_nonlocal_sb_nonlocal_prov*draws_wq_sub_basin_nonlocal_sb_nonlocal_prov
  
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
    b_asc_shared_bound*SHARED_BOADER_PROV +
    b_asc_home_provshare*HOME_PROV_SHARE +
    b_cost *COST + 
    b_wq_local_basin*WQ_BASIN_LOCAL_POLICY +
    b_wq_nonlocal_basin*WQ_BASIN_NL_POLICY +
    b_wq_sub_basin_local_nsb*WQ_SUBBASIN_LOCAL_NSB_POLICY_SUBONLY +
    b_wq_sub_basin_local_sb*WQ_SUBBASIN_LOCAL_SB_POLICY_SUBONLY +
    b_wq_sub_basin_nonlocal_nsb_local_prov*WQ_SUBBASIN_NL_NSB_LP_POLICY_SUBONLY +
    b_wq_sub_basin_nonlocal_sb_local_prov*WQ_SUBBASIN_NL_SB_LP_POLICY_SUBONLY +
    b_wq_sub_basin_nonlocal_nsb_nonlocal_prov*WQ_SUBBASIN_NL_NSB_NLP_POLICY_SUBONLY +
    b_wq_sub_basin_nonlocal_sb_nonlocal_prov*WQ_SUBBASIN_NL_SB_NLP_POLICY_SUBONLY 
  
  
  V[["opt_out"]] = 
    b_wq_local_basin*WQ_BASIN_LOCAL_CURRENT +
    b_wq_nonlocal_basin*WQ_BASIN_NL_CURRENT +
    b_wq_sub_basin_local_nsb*WQ_SUBBASIN_LOCAL_NSB_CURRENT_SUBONLY +
    b_wq_sub_basin_local_sb*WQ_SUBBASIN_LOCAL_SB_CURRENT_SUBONLY +
    b_wq_sub_basin_nonlocal_nsb_local_prov*WQ_SUBBASIN_NL_NSB_LP_CURRENT_SUBONLY +
    b_wq_sub_basin_nonlocal_sb_local_prov*WQ_SUBBASIN_NL_SB_LP_CURRENT_SUBONLY +
    b_wq_sub_basin_nonlocal_nsb_nonlocal_prov*WQ_SUBBASIN_NL_NSB_NLP_CURRENT_SUBONLY +
    b_wq_sub_basin_nonlocal_sb_nonlocal_prov*WQ_SUBBASIN_NL_SB_NLP_CURRENT_SUBONLY 
  
  
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
#apollo_saveOutput(model)

