########################################################################################
# Description: RPM Model 9
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
  modelName       = "Model 7",
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
  
  b_baseline = 0,
  b_wq_x_bl = 0
  
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
    
    # WQ improvements (unchanged)
    b_wq_local_basin * WQ_BASIN_LOCAL_POLICY +
    b_wq_nonlocal_basin * WQ_BASIN_NL_POLICY +
    b_wq_local_sub_basin * WQ_SUBBASIN_LOCAL_POLICY_SUBONLY +
    b_wq_nonlocal_sub_basin * WQ_SUBBASIN_NL_POLICY_SUBONLY +
    
    # Continuous baseline effects
    b_baseline * BASELINE_WQ_VARIATION +  # Linear effect
    #b_baseline_sq * BASELINE_WQ_VARIATION^2 +  # Optional curvature
    
    b_wq_x_bl*BASELINE_WQ_VARIATION*WQ_CHANGE
  
  
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


# Extract coefficients and covariance matrix
coef_values <- model$estimate
vcov_matrix <- model$robvarcov



# Calculate Marginal WTP (MWTP) at each baseline level
mwtp_3 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "-(mu_b_wq_local_sub_basin - b_wq_x_bl * 3) / (-b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "BL3") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

mwtp_2 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "-(mu_b_wq_local_sub_basin - b_wq_x_bl * 2) / (-b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "BL2") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

mwtp_1 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = "-(mu_b_wq_local_sub_basin - b_wq_x_bl * 1) / (-b_cost)"
) %>% 
  {`rownames<-`(., NULL)} %>%
  mutate(Scenario = "BL1") %>%
  relocate(Scenario, .before = 1) %>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# Combine all MWTP results
all_mwtp <- bind_rows(mwtp_1, mwtp_2, mwtp_3)%>%
    mutate(type = "MWTP")

print(all_mwtp)



# Minimalist version
mwtp_plot_minimal <- ggplot(all_mwtp, aes(x = factor(Scenario), y = Estimate)) +
  geom_point(size = 4, color = "#A23B72") +
  geom_errorbar(aes(ymin = `2.5 %`, ymax = `97.5 %`), 
                width = 0.1, color = "#A23B72", alpha = 0.7) +
  labs(
    title = "",
    x = "Current Health Score",
    y = "Marginal WTP ($)"
  ) +
  scale_y_continuous(
    limits = c(-50, 180),
    breaks = seq(-50, 180, by = 20)
  ) +
  #scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_discrete(labels = c("1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5")) +
  theme_minimal() +
  theme(
  legend.position = "bottom",
  legend.text = element_text(size = 12),
  axis.text.x = element_text(hjust = 1, size = 12, angle = 0),
  axis.text.y = element_text(size = 12),
  axis.title.y = element_text(size = 12),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line.y = element_line(size = 0.2, color = "black"),
  axis.line.x = element_line(size = 0.2, color = "black")
  )

print(mwtp_plot_minimal)

# Save to PNG
ggsave("Figures/baseline_effect_MWTP.png", plot = mwtp_plot_minimal, width = 10, height = 10, units = "in", dpi = 300)


stop()



# Test all pairwise differences
mwtp_scenarios <- list(
  list(level = 5, name = "BL5"),
  list(level = 4, name = "BL4"),
  list(level = 3, name = "BL3"),
  list(level = 2, name = "BL2"),
  list(level = 1, name = "BL1")
)

all_mwtp_pairs <- combn(length(mwtp_scenarios), 2, simplify = FALSE)

mwtp_differences <- lapply(all_mwtp_pairs, function(pair) {
  i <- pair[1]
  j <- pair[2]
  
  sc1 <- mwtp_scenarios[[i]]
  sc2 <- mwtp_scenarios[[j]]
  
  result <- deltaMethod(
    object = coef_values,
    vcov. = vcov_matrix,
    g = paste0("((mu_b_wq_local_sub_basin + b_wq_sub_x_bl * ", sc1$level, 
               ") - (mu_b_wq_local_sub_basin + b_wq_sub_x_bl * ", sc2$level, 
               ")) / (-b_cost)")
  )
  
  result %>%
    as.data.frame() %>%
    {`rownames<-`(., NULL)} %>%
    mutate(
      Comparison = paste0("MWTP ", sc1$name, " vs ", sc2$name),
      CI_lower = Estimate - 1.96 * SE,
      CI_upper = Estimate + 1.96 * SE,
      p_value = 2 * (1 - pnorm(abs(Estimate / SE))),
      Significance = ifelse(p_value < 0.05, "Yes", "No")
    )
})

# Combine all MWTP difference results
mwtp_diff_df <- bind_rows(mwtp_differences) %>%
  relocate(Comparison, .before = 1) %>%
  mutate(across(c(Estimate, SE, CI_lower, CI_upper), ~ round(.x, 2)),
         p_value = round(p_value, 4))

# Display significant differences
significant_mwtp_diffs <- mwtp_diff_df %>%
  filter(Significance == "Yes")

if(nrow(significant_mwtp_diffs) > 0) {
  cat("\nSTATISTICALLY SIGNIFICANT MWTP DIFFERENCES:\n")
  cat("===========================================\n")
  print(significant_mwtp_diffs %>% select(Comparison, Estimate, SE, p_value))
} else {
  cat("\nNo statistically significant differences in MWTP values found.\n")
}

# 10%

# Test all pairwise differences at 10% significance level
mwtp_scenarios <- list(
  list(level = 5, name = "BL5"),
  list(level = 4, name = "BL4"),
  list(level = 3, name = "BL3"),
  list(level = 2, name = "BL2"),
  list(level = 1, name = "BL1")
)

all_mwtp_pairs <- combn(length(mwtp_scenarios), 2, simplify = FALSE)

mwtp_differences <- lapply(all_mwtp_pairs, function(pair) {
  i <- pair[1]
  j <- pair[2]
  
  sc1 <- mwtp_scenarios[[i]]
  sc2 <- mwtp_scenarios[[j]]
  
  result <- deltaMethod(
    object = coef_values,
    vcov. = vcov_matrix,
    g = paste0("((mu_b_wq_local_sub_basin + b_wq_sub_x_bl * ", sc1$level, 
               ") - (mu_b_wq_local_sub_basin + b_wq_sub_x_bl * ", sc2$level, 
               ")) / (-b_cost)")
  )
  
  result %>%
    as.data.frame() %>%
    {`rownames<-`(., NULL)} %>%
    mutate(
      Comparison = paste0("MWTP ", sc1$name, " vs ", sc2$name),
      CI_lower_90 = Estimate - 1.645 * SE,  # 90% CI uses 1.645
      CI_upper_90 = Estimate + 1.645 * SE,
      p_value = 2 * (1 - pnorm(abs(Estimate / SE))),
      Significance_10pct = ifelse(p_value < 0.10, "Yes", "No")  # 10% significance level
    )
})

# Combine all MWTP difference results
mwtp_diff_df <- bind_rows(mwtp_differences) %>%
  relocate(Comparison, .before = 1) %>%
  mutate(across(c(Estimate, SE, CI_lower_90, CI_upper_90), ~ round(.x, 2)),
         p_value = round(p_value, 4))

# Display significant differences at 10% level
significant_mwtp_diffs <- mwtp_diff_df %>%
  filter(Significance_10pct == "Yes")

if(nrow(significant_mwtp_diffs) > 0) {
  cat("\nSTATISTICALLY SIGNIFICANT MWTP DIFFERENCES (10% level):\n")
  cat("=========================================================\n")
  print(significant_mwtp_diffs %>% select(Comparison, Estimate, SE, p_value, CI_lower_90, CI_upper_90))
} else {
  cat("\nNo statistically significant differences in MWTP values found at the 10% level.\n")
}

# Additionally, you might want to see all comparisons
cat("\nALL PAIRWISE COMPARISONS (90% Confidence Intervals):\n")
cat("=====================================================\n")
print(mwtp_diff_df %>% select(Comparison, Estimate, SE, p_value, CI_lower_90, CI_upper_90, Significance_10pct))