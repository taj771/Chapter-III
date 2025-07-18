########################################################################################
# Description: RPM Model 4
#######################################################################################


### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)
library(dplyr)
library(purrr)
library(msm)  # for deltaMethod


### Set working directory (only works in RStudio)
#apollo_setWorkDir()

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "RPM Model 5_1",
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

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

apollo_beta = c(
  mu_b_asc     = 0,  
  sigma_b_asc = 0.01,
  b_cost  = 0,  
  
  mu_b_wq_local_basin = 0,
  sigma_b_wq_local_basin = 0.01,
  
  mu_b_wq_nonlocal_basin = 0,
  sigma_b_wq_nonlocal_basin = 0.01,
  
  #mu_b_wq_local_sub_basin = 0,
  #sigma_b_wq_local_sub_basin = 0.01,
  
  #mu_b_wq_nonlocal_sub_basin = 0,
  #sigma_b_wq_nonlocal_sub_basin = 0.01,
  
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
  interNormDraws = c("draws_asc","draws_wq_local_basin","draws_wq_nonlocal_basin","draws_wq_local_sub_basin",
                     "draws_wq_nonlocal_sub_basin","draws_b_wq_sub_basin_sb","draws_b_wq_sub_basin_nsb",
                     "draws_b_wq_sub_basin_nonlocal_nsb_local_prov","draws_b_wq_sub_basin_nonlocal_sb_local_prov",
                     "draws_b_wq_sub_basin_nonlocal_nsb_nonlocal_prov","draws_b_wq_sub_basin_nonlocal_sb_nonlocal_prov"),
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
  
  #randcoeff[["b_wq_local_sub_basin"]] =  mu_b_wq_local_sub_basin + sigma_b_wq_local_sub_basin*draws_wq_local_sub_basin
  #randcoeff[["b_wq_nonlocal_sub_basin"]] =  mu_b_wq_nonlocal_sub_basin + sigma_b_wq_nonlocal_sub_basin*draws_wq_nonlocal_sub_basin
  
  randcoeff[["b_wq_sub_basin_local_nsb"]] =  mu_b_wq_sub_basin_local_nsb + sigma_b_wq_sub_basin_local_nsb*draws_b_wq_sub_basin_nsb
  randcoeff[["b_wq_sub_basin_local_sb"]] =  mu_b_wq_sub_basin_local_sb + sigma_b_wq_sub_basin_local_sb*draws_b_wq_sub_basin_sb
  
  randcoeff[["b_wq_sub_basin_nonlocal_nsb_local_prov"]] =  mu_b_wq_sub_basin_nonlocal_nsb_local_prov + sigma_b_wq_sub_basin_nonlocal_nsb_local_prov*draws_b_wq_sub_basin_nonlocal_nsb_local_prov
  randcoeff[["b_wq_sub_basin_nonlocal_sb_local_prov"]] =  mu_b_wq_sub_basin_nonlocal_sb_local_prov + sigma_b_wq_sub_basin_nonlocal_sb_local_prov*draws_b_wq_sub_basin_nonlocal_sb_local_prov
  
  randcoeff[["b_wq_sub_basin_nonlocal_nsb_nonlocal_prov"]] =  mu_b_wq_sub_basin_nonlocal_nsb_nonlocal_prov + sigma_b_wq_sub_basin_nonlocal_nsb_nonlocal_prov*draws_b_wq_sub_basin_nonlocal_nsb_nonlocal_prov
  randcoeff[["b_wq_sub_basin_nonlocal_sb_nonlocal_prov"]] =  mu_b_wq_sub_basin_nonlocal_sb_nonlocal_prov + sigma_b_wq_sub_basin_nonlocal_sb_nonlocal_prov*draws_b_wq_sub_basin_nonlocal_sb_nonlocal_prov
  
  
  
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
    
    #b_wq_local_sub_basin*WQ_SUBBASIN_LOCAL_POLICY +
    #b_wq_nonlocal_sub_basin*WQ_SUBBASIN_NL_POLICY +
    
    b_wq_sub_basin_local_nsb*WQ_SUBBASIN_LOCAL_NSB_POLICY_SUBONLY +
    b_wq_sub_basin_local_sb*WQ_SUBBASIN_LOCAL_SB_POLICY_SUBONLY +
    
    b_wq_sub_basin_nonlocal_nsb_local_prov*WQ_SUBBASIN_NL_NSB_LP_POLICY_SUBONLY +
    b_wq_sub_basin_nonlocal_sb_local_prov*WQ_SUBBASIN_NL_SB_LP_POLICY_SUBONLY +
    
    b_wq_sub_basin_nonlocal_nsb_nonlocal_prov*WQ_SUBBASIN_NL_NSB_NLP_POLICY_SUBONLY +
    b_wq_sub_basin_nonlocal_sb_nonlocal_prov*WQ_SUBBASIN_NL_SB_NLP_POLICY_SUBONLY 
  
  
  
  
  
  
  
  V[["opt_out"]] = 
    b_wq_local_basin*WQ_BASIN_LOCAL_CURRENT +
    b_wq_nonlocal_basin*WQ_BASIN_NL_CURRENT  +
    
    #b_wq_local_sub_basin*WQ_SUBBASIN_LOCAL_CURRENT +
    #b_wq_nonlocal_sub_basin*WQ_SUBBASIN_NL_CURRENT +
    
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


# Extract coefficients and covariance matrix
coef_values <- model$estimate
vcov_matrix <- model$robvarcov



df <- st_read("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/Shapefile/study_area_map_with_WQ.shp")%>%
  mutate(WQ_V1 = str_remove(WQ_V1, "Level"))%>%
  mutate(WQ_V2 = str_remove(WQ_V2, "Level"))%>%
  mutate(WQ_V1 = as.numeric(WQ_V1))%>%
  mutate(WQ_V2 = as.numeric(WQ_V2))%>%
  group_by(basin)%>%
  mutate(AVE_WQ_BASIN_V1 = mean(WQ_V1))%>%
  mutate(AVE_WQ_BASIN_V2 = mean(WQ_V2))




# List of basin codes
basin_codes <- c("SS", "NS", "LSN", "AR")

# Loop to assign each variable dynamically
walk(basin_codes, function(code) {
  value <- df %>%
    filter(basin == code) %>%
    pull(AVE_WQ_BASIN_V2) %>%
    .[[1]]
  
  assign(paste0("wq_basin_current_", tolower(code)), value, envir = .GlobalEnv)
})

# Create a lookup table of basins and their current WQ values
basins <- tibble(
  basin = c("SS", "NS", "LSN", "AR"),
  wq_value = c(wq_basin_current_ss, wq_basin_current_ns, 
               wq_basin_current_lsn, wq_basin_current_ar)
)

# Apply deltaMethod over each basin using map2_dfr
df_basin <- map2_dfr(
  basins$basin, basins$wq_value,
  ~ deltaMethod(
    object = coef_values,
    vcov. = vcov_matrix,
    g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*", .y, "))/b_cost)")
  ) %>%
    mutate(basin = .x, subbasin = "all")
)



## sub basins 

# Define your codes
subbasin_codes <- c("USS", "LSS", "RD", "BO", "LNS", "BA", "UNS", "CNS", 
                    "WLW", "LWM", "ELW", "SA", "NE", "GB", "AS", "QU", 
                    "RE", "SO")

# Loop and assign variables dynamically
walk(subbasin_codes, function(code) {
  value <- df %>%
    filter(name_code == code) %>%
    pull(WQ_V2) %>%
    .[[1]]
  
  assign(paste0("wq_subbasin_current_", tolower(code)), value, envir = .GlobalEnv)
})



subbasins <- tibble(
  subbasin = c("USS", "LSS", "RD", "BO", "LNS", "BA", "UNS", "CNS", 
               "WLW", "LWM", "ELW", "SA", "NE", "GB", "AS", "QU", 
               "RE", "SO"),
  wq_value = c(
    wq_subbasin_current_uss, wq_subbasin_current_lss, wq_subbasin_current_rd,
    wq_subbasin_current_bo, wq_subbasin_current_lns, wq_subbasin_current_ba,
    wq_subbasin_current_uns, wq_subbasin_current_cns, wq_subbasin_current_wlw,
    wq_subbasin_current_lwm, wq_subbasin_current_elw, wq_subbasin_current_sa,
    wq_subbasin_current_ne, wq_subbasin_current_gb, wq_subbasin_current_as,
    wq_subbasin_current_qu, wq_subbasin_current_re, wq_subbasin_current_so
  )
)


# Apply deltaMethod over each basin using map2_dfr
df_subbasin <- map2_dfr(
  subbasins$subbasin, subbasins$wq_value,
  ~ deltaMethod(
    object = coef_values,
    vcov. = vcov_matrix,
    g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*", .y, "))/b_cost)")
  ) %>%
    mutate(basin = .x, subbasin = "all")
)





df_basin <- df_basin%>%
  select(Estimate,basin)%>%
  rename(WTP_2 = Estimate)%>%
  as.data.frame()

df_basin_map <- df%>%
  left_join(df_basin)

ab <- st_read("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/Shapefile/AB.shp")
mb <- st_read("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/Shapefile/MB.shp")
sk <- st_read("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/Shapefile/SK.shp")


tm_shape(df_basin_map, crs = 3347)+
  tm_fill(col = "WTP_2", palette = "YlGnBu", style = "quantile", n = 5) +
  tm_borders() +
  tm_text("name_code", size = 0.8, col = "black", remove.overlap = TRUE)+  # Adjust size,
  tm_shape(ab, crs = 3347) +
  tm_borders(col = "black", lwd = 2)+
  tm_shape(mb, crs = 3347) +
  tm_borders(col = "black", lwd = 2)+
  tm_shape(sk, crs = 3347) +
  tm_borders(col = "black", lwd = 2)+ 
  #tm_shape(ab_cities) +
  #tm_borders(col = "black", lwd = 2)+
  #tm_text("name", size = 0.6, col = "black", remove.overlap = TRUE)+  # Adjust size,
  #tm_shape(mb_cities) +
  #tm_borders(col = "black", lwd = 2)+
  #tm_text("name", size = 0.6, col = "black", remove.overlap = TRUE)+  # Adjust size,
  #tm_shape(sk_cities) +
  #tm_borders(col = "black", lwd = 2)+ 
  #tm_text("name", size = 0.6, col = "black", remove.overlap = TRUE)+  # Adjust size,
  tm_layout(frame = FALSE)+
  tm_legend(frame = F)



df_subbasin <- df_subbasin%>%
  select(Estimate,basin)%>%
  rename(name_code = basin)%>%
  rename(WTP_2 = Estimate)%>%
  as.data.frame()

df_subbasin_map <- df%>%
  left_join(df_subbasin)


# Define custom breaks and palette
custom_breaks <- c(100, 200, 300, 400, 500, 600, 700, 800, 900, 950)
custom_palette <- RColorBrewer::brewer.pal(n = 9, name = "YlOrRd")  # 9 colors for 10 breaks

# Plot
tm_shape(df_subbasin_map, crs = 3347) +
  tm_fill(
    col = "WTP_2",
    palette = custom_palette,
    breaks = custom_breaks,
    title = "WTP ($)"
  ) +
  tm_borders() +
  tm_text("name_code", size = 0.8, col = "black", remove.overlap = TRUE) +
  tm_shape(ab, crs = 3347) + tm_borders(col = "black", lwd = 2) +
  tm_shape(mb, crs = 3347) + tm_borders(col = "black", lwd = 2) +
  tm_shape(sk, crs = 3347) + tm_borders(col = "black", lwd = 2) +
  tm_layout(frame = FALSE) +
  tm_legend(frame = FALSE)








