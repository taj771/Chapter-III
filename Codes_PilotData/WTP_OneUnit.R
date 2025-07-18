########################################################################################
# Description: RPM Model 1
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
  modelName       = "Model 1",
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
  b_cost  = 0,   
  mu_b_wq_local_basin = 0,
  sigma_b_wq_local_basin = 0.1,
  mu_b_wq_nonlocal_basin = 0,
  sigma_b_wq_nonlocal_basin = 0.1,
  mu_b_wq_local_sub_basin = 0,
  sigma_b_wq_local_sub_basin = 0.1,
  mu_b_wq_nonlocal_sub_basin = 0,
  sigma_b_wq_nonlocal_sub_basin = 0.1
  
  
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
                     "draws_wq_local_basin","draws_wq_nonlocal_basin",
                     "draws_wq_local_sub_basin","draws_wq_nonlocal_sub_basin"),
  intraDrawsType = "halton",
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
  V[["policy"]]  = b_asc + b_cost *COST + 
    b_wq_local_basin*WQ_BASIN_LOCAL_POLICY +
    b_wq_nonlocal_basin*WQ_BASIN_NL_POLICY +
    b_wq_local_sub_basin*WQ_SUBBASIN_LOCAL_POLICY_SUBONLY +
    b_wq_nonlocal_sub_basin*WQ_SUBBASIN_NL_POLICY_SUBONLY
  
  
  V[["opt_out"]] = 
    b_wq_local_basin*WQ_BASIN_LOCAL_CURRENT +
    b_wq_nonlocal_basin*WQ_BASIN_NL_CURRENT +
    b_wq_local_sub_basin*WQ_SUBBASIN_LOCAL_CURRENT_SUBONLY+
    b_wq_nonlocal_sub_basin*WQ_SUBBASIN_NL_CURRENT_SUBONLY
  
  
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

# Extract coefficients and covariance matrix
coef_values <- model$estimate
vcov_matrix <- model$robvarcov


# WTP for one unit improvement of WQ at Local Basin

df1 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "-(mu_b_asc + mu_b_wq_local_basin*(-1))/b_cost"
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(`WQ change scenario` = "Local Basin")%>%
  relocate(`WQ change scenario`, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# WTP for one unit improvement of WQ at Non-Local Basin

df2 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "-(mu_b_asc + mu_b_wq_nonlocal_basin*(-1))/b_cost"
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(`WQ change scenario` = "Non-Local Basin")%>%
  relocate(`WQ change scenario`, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# WTP for one unit improvement of WQ at Local subbasin

df3 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "-(mu_b_asc + mu_b_wq_local_sub_basin*(-1))/b_cost"
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(`WQ change scenario` = "Local Sub Basin")%>%
  relocate(`WQ change scenario`, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# WTP for one unit improvement of WQ at non-local subbasin

df4 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "-(mu_b_asc + mu_b_wq_nonlocal_sub_basin*(-1))/b_cost"
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(`WQ change scenario` = "Non-Local  Sub Basin")%>%
  relocate(`WQ change scenario`, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


df_all <- rbind(df1,df2,df3,df4)

# Load library
library(xtable)

# Convert to LaTeX
latex_table <- xtable(df_all)

# Save to .tex file
print(latex_table, file = "Tables/WTP_oneunit.tex", include.rownames = FALSE)



