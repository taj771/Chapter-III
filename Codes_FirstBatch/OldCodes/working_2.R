
### Clear memory
rm(list = ls())


postal_code <- st_read("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/CanMapPostalCodeSuitev2022.3/PostalCodeRBFinal.shp") %>%
  mutate(
    FSA_1 = str_sub(POSTALCODE, 1, 3),
    FSA_2 = str_sub(POSTALCODE, 4, 6)
  )%>%
  group_by(FSA_1)%>%
  mutate(count = n())%>%
  ungroup()

postal_fsa_poly <- postal_code%>%
  group_by(FSA_1)%>%
  summarise(geometry = st_combine(geometry)) %>%
  mutate(geometry = st_convex_hull(geometry))



# Reproject to a projected CRS for accurate distance/area calculations
postal_code_proj <- st_transform(postal_code, crs = 3347)  # NAD83 / Statistics Canada Lambert

# Group by FSA and calculate centroid of the group (mean coordinates)
fsa_centroids <- postal_code_proj %>%
  group_by(FSA_1) %>%
  summarise(geometry = st_centroid(st_union(geometry)))

df <- st_read("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/Shapefile/study_area.shp")

tm_shape(postal_code) +
  tm_dots(col = "white")+
  tm_shape(df)+
  tm_borders(col = "black", lwd = 2)+
  tm_shape(fsa_centroids) +
  tm_dots(col = "red")




tm_shape(fsa_centroids) +
  tm_dots()+
  tm_shape(df)+
  tm_borders(col = "black", lwd = 2)
  
  
  
  tm_shape(df)+
  tm_borders(col = "black", lwd = 2)+
  tm_shape(fsa_centroids) +
  tm_dots(col = "red")
  
  
  
t <- postal_code%>%
  filter(FSA_1 == "T2J")

t1 <- fsa_centroids%>%
  filter(FSA_1 == "T2J")
  
  
tm_shape(t)+
  tm_dots(col = "black", lwd = 2)+
  tm_shape(t1) +
  tm_dots(col = "red", size = 4) 

tm_shape(df)+
  tm_borders(col = "black", lwd = 2)+
  tm_shape(t)+
  tm_dots(col = "black", lwd = 2)+
  tm_shape(t1) +
  tm_dots(col = "red", size = 1) 

