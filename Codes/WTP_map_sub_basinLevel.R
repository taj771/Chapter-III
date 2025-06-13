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



df_basin <- st_read("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/Shapefile/study_area_map_with_WQ.shp")%>%
  mutate(WQ_V1 = str_remove(WQ_V1, "Level"))%>%
  mutate(WQ_V2 = str_remove(WQ_V2, "Level"))%>%
  mutate(WQ_V1 = as.numeric(WQ_V1))%>%
  mutate(WQ_V2 = as.numeric(WQ_V2))%>%
  group_by(basin)%>%
  mutate(AVE_WQ_BASIN_V1 = mean(WQ_V1))%>%
  mutate(AVE_WQ_BASIN_V2 = mean(WQ_V2))%>%
  select(basin,AVE_WQ_BASIN_V1,AVE_WQ_BASIN_V2)%>%
  distinct(basin, .keep_all = T )%>%
  mutate(AVE_WQ_BASIN = (AVE_WQ_BASIN_V1+AVE_WQ_BASIN_V2)/2)


df_subbasin <- st_read("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/Shapefile/study_area_map_with_WQ.shp")%>%
  mutate(WQ_V1 = str_remove(WQ_V1, "Level"))%>%
  mutate(WQ_V2 = str_remove(WQ_V2, "Level"))%>%
  mutate(WQ_V1 = as.numeric(WQ_V1))%>%
  mutate(WQ_V2 = as.numeric(WQ_V2))%>%
  group_by(WSCSDA_E)%>%
  select(name_code,WQ_V1,WQ_V2)%>%
  distinct(name_code, .keep_all = T )%>%
  mutate(AVE_WQ_SUB_BASIN = (WQ_V1+WQ_V2)/2)





# Sub-basin - BO

df1 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*2", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "BO")%>%
  relocate(name_code, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# Sub-basin - UNS

df2 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*2", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "UNS")%>%
  relocate(name_code, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))

# Sub-basin - AS
df3 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*2.5", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "AS")%>%
  relocate(name_code, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# Sub-basin - QU
df4 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*2.5", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "QU")%>%
  relocate(name_code, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# Sub-basin - RE
df5 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*2.5", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "RE")%>%
  relocate(name_code, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# Sub-basin - SO
df6 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*2.5", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "SO")%>%
  relocate(name_code, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))




# Sub-basin - LNS
df7 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*3", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "LNS")%>%
  relocate(name_code, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# Sub-basin - CNS
df8 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*3", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "CNS")%>%
  relocate(name_code, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# Sub-basin - LWM
df9 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*3", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "LWM")%>%
  relocate(name_code, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



# Sub-basin - USS
df10 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*3.5", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "USS")%>%
  relocate(name_code, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# Sub-basin - LSS
df11 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*3.5", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "LSS")%>%
  relocate(name_code, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# Sub-basin - 	RD
df12 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*3.5", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "RD")%>%
  relocate(name_code, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# Sub-basin - 	ELW
df13 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*3.5", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "ELW")%>%
  relocate(name_code, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# Sub-basin - 	NE
df14 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*3.5", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "NE")%>%
  relocate(name_code, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# Sub-basin - 	BA
df15 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*4", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "BA")%>%
  relocate(name_code, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# Sub-basin - 	SA
df16 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*4", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "SA")%>%
  relocate(name_code, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# Sub-basin - 	GB
df17 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*4", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "GB")%>%
  relocate(name_code, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))


# Sub-basin - 	WLW
df18 <- deltaMethod(
  object = coef_values, 
  vcov. = vcov_matrix, 
  g = paste0("-((mu_b_asc + mu_b_wq_local_basin*2 - (mu_b_wq_local_basin*5", "))/b_cost)")
)%>% 
  {`rownames<-`(., NULL)}%>%
  mutate(name_code = "WLW")%>%
  relocate(name_code, .before = 1)%>%
  mutate(across(where(is.numeric), ~ round(.x, 0)))



df <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,
            df11,df12,df13,df14,df15,df16,df17,df18)


df_map <- st_read("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/Shapefile/study_area_map_with_WQ.shp")


df <- df%>%
  select(Estimate,name_code)%>%
  rename(WTP_2 = Estimate)%>%
  as.data.frame()

df_map<- df_map%>%
  left_join(df)


ab <- st_read("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/Shapefile/AB.shp")
mb <- st_read("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/Shapefile/MB.shp")
sk <- st_read("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/Shapefile/SK.shp")


library(tidygeocoder)

# Example data frame with city names
cities <- data.frame(city = c("Grand Prairie","Slave Lake","Fort McMurray","Edson","Edmonton","Red Deer","Banf","Calgary","Brooks","Drumheller","Brooks",
                              "Cold lake","North Battleford", "Medicine Hat", "Moose Jaw","Saskatoon","Regina","Prince Albert", "Fort Qu'Apelle",
                              "Swan River", "The pas", "Split Lake", "Cross Lake", "Oxford House", "Altona", "Stonewall", "Minnedosa"))

# Geocode using OSM (default)
geocoded_cities <- cities %>%
  geocode(address = city, method = "osm", lat = latitude, long = longitude)

# Convert to sf object (spatial point)
cities <- st_as_sf(geocoded_cities, coords = c("longitude", "latitude"), crs = 4326)


# Get the number of unique values in WTP_2
n_colors <- length(unique(df_map$WTP_2))

# Generate a vector of n unique colors
library(viridis)
custom_palette <- viridis(n_colors, option = "I")



value_map <- tm_shape(df_map, crs = 3347)+
  tm_fill(    col = "WTP_2",
              palette = custom_palette,
              style = "cat",
              title = "Willingness to Pay ($)"
  ) +
  tm_borders() +
  #tm_text("name_code", size = 0.8, col = "black", remove.overlap = TRUE)+  # Adjust size,
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
  tm_scale_bar(
    breaks = c(0, 100, 200,300,400),       # custom distance labels
    text.size = 0.5,               # smaller text
    position = c(0.6, 0.008),       # custom position
    color.dark = "black",          # tick and text color
    color.light = "white"          # fill for alternating blocks
  ) +
  tm_compass(
    type = "arrow",              # options: "arrow", "8star", "radar"
    size = 2,                    # size of the compass
    position = c(0.9, 0.9)       # custom position (x, y: 0 to 1 scale)
  )+
  
  tm_shape(cities) +
  tm_symbols(col = "blue", size = 0.1) +
  tm_text("city", size = 0.7, col = "black",ymod = -0.5)+
  
  tm_legend(frame = F)

# Save to PNG
tmap_save(value_map, "Figures/value_map.png", width = 10, height = 8, units = "in", dpi = 300)

