# Load necessary libraries
library(sf)
library(tmap)
library(tidyverse)

# Read shapefiles
df <- st_read("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/Shapefile/study_area.shp")%>%
  mutate(name = case_when(
    WSCSDA_E == "Qu'Appelle" ~ "Qu'Appelle", 
    WSCSDA_E == "Assiniboine" ~ "Assiniboine", 
    WSCSDA_E == "Souris" ~ "Souris", 
    WSCSDA_E == "Red" ~ "Red", 
    
    WSCSDA_E == "Grass and Burntwood River Basin" ~ "Grass and Burntwood", 
    WSCSDA_E == "Nelson River Basin" ~ "Nelson", 
    WSCSDA_E == "Saskatchewan River Basin" ~ "Saskatchewan", 
    WSCSDA_E == "Eastern Lake Winnipeg River Basin" ~ "Eastern Lake Winnipeg", 
    WSCSDA_E == "Lake Winnipegosis and Lake Manitoba River Basin" ~ "Lake Winnipegosis and Lake Manitoba", 
    WSCSDA_E == "Western Lake Winnipeg River Basin" ~ "Western Lake Winnipeg", 
    
    WSCSDA_E == "Central North Saskatchewan Sub River Basin" ~ "Central North Saskatchewan", 
    WSCSDA_E == "Upper North Saskatchewan Sub River Basin" ~ "Upper North Saskatchewan", 
    WSCSDA_E == "Battle Sub River Basin" ~ "Battle", 
    WSCSDA_E == "Lower North Saskatchewan Sub River Basin" ~ "Lower North Saskatchewan", 
    
    WSCSDA_E == "Bow Sub River Basin" ~ "Bow", 
    WSCSDA_E == "Red Deer Sub River Basin" ~ "Red Deer", 
    WSCSDA_E == "Lower South Saskatchewan Sub River Basin" ~ "Lower South Saskatchewan", 
    WSCSDA_E == "Upper South Saskatchewan Sub River Basin" ~ "Upper South Saskatchewan", 
    
    TRUE ~ NA_character_  # Otherwise, assign 0
  ))%>%
  mutate(WQ_V1 = case_when(
    WSCSDA_E == "Qu'Appelle" ~ "Level 3", 
    WSCSDA_E == "Assiniboine" ~ "Level 3", 
    WSCSDA_E == "Souris" ~ "Level 3", 
    WSCSDA_E == "Red" ~ "Level 3", 
    
    WSCSDA_E == "Grass and Burntwood River Basin" ~ "Level 5", 
    WSCSDA_E == "Nelson River Basin" ~ "Level 4", 
    WSCSDA_E == "Saskatchewan River Basin" ~ "Level 5", 
    WSCSDA_E == "Eastern Lake Winnipeg River Basin" ~ "Level 4", 
    WSCSDA_E == "Lake Winnipegosis and Lake Manitoba River Basin" ~ "Level 4", 
    WSCSDA_E == "Western Lake Winnipeg River Basin" ~ "Level 5", 
    
    WSCSDA_E == "Central North Saskatchewan Sub River Basin" ~ "Level 4", 
    WSCSDA_E == "Upper North Saskatchewan Sub River Basin" ~ "Level 3", 
    WSCSDA_E == "Battle Sub River Basin" ~ "Level 5", 
    WSCSDA_E == "Lower North Saskatchewan Sub River Basin" ~ "Level 4", 
    
    WSCSDA_E == "Bow Sub River Basin" ~ "Level 3", 
    WSCSDA_E == "Red Deer Sub River Basin" ~ "Level 4", 
    WSCSDA_E == "Lower South Saskatchewan Sub River Basin" ~ "Level 4", 
    WSCSDA_E == "Upper South Saskatchewan Sub River Basin" ~ "Level 4", 
    
    TRUE ~ NA_character_  # Otherwise, assign 0
  ))%>%
  mutate(WQ_V2 = case_when(
    WSCSDA_E == "Qu'Appelle" ~ "Level 2", 
    WSCSDA_E == "Assiniboine" ~ "Level 2", 
    WSCSDA_E == "Souris" ~ "Level 2", 
    WSCSDA_E == "Red" ~ "Level 2", 
    
    WSCSDA_E == "Grass and Burntwood River Basin" ~ "Level 3", 
    WSCSDA_E == "Nelson River Basin" ~ "Level 3", 
    WSCSDA_E == "Saskatchewan River Basin" ~ "Level 3", 
    WSCSDA_E == "Eastern Lake Winnipeg River Basin" ~ "Level 3", 
    WSCSDA_E == "Lake Winnipegosis and Lake Manitoba River Basin" ~ "Level 2", 
    WSCSDA_E == "Western Lake Winnipeg River Basin" ~ "Level 5", 
    
    WSCSDA_E == "Central North Saskatchewan Sub River Basin" ~ "Level 2", 
    WSCSDA_E == "Upper North Saskatchewan Sub River Basin" ~ "Level 1", 
    WSCSDA_E == "Battle Sub River Basin" ~ "Level 3", 
    WSCSDA_E == "Lower North Saskatchewan Sub River Basin" ~ "Level 2", 
    
    WSCSDA_E == "Bow Sub River Basin" ~ "Level 1", 
    WSCSDA_E == "Red Deer Sub River Basin" ~ "Level 3", 
    WSCSDA_E == "Lower South Saskatchewan Sub River Basin" ~ "Level 3", 
    WSCSDA_E == "Upper South Saskatchewan Sub River Basin" ~ "Level 3", 
    
    TRUE ~ NA_character_  # Otherwise, assign 0
  ))




ab <- st_read("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/Shapefile/AB.shp")
mb <- st_read("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/Shapefile/MB.shp")
sk <- st_read("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/Shapefile/SK.shp")

ab_cities <- st_read("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/Shapefile/AB_cities.shp")%>%
  filter(name != 	"Bruderheim",
         name != "Mundare",
         name != "Fort Saskatchewan",
         name != "Vegreville",
         name != "Sherwood Park")
mb_cities <- st_read("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/Shapefile/MB_cities.shp")
sk_cities <- st_read("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/Shapefile/SK_cities.shp")





# Ensure both layers are in the same CRS (EPSG:4326, lat/lon) first, if needed
df <- st_transform(df, crs = 4326)  
ab <- st_transform(ab, crs = 4326)  
mb <- st_transform(mb, crs = 4326) 
sk <- st_transform(sk, crs = 4326) 



# Plot
tmap_mode("view")

df$WQ_V1 <- as.factor(df$WQ_V1)  # or as.character()

# Define custom colors (names must match factor levels exactly)
custom_colors <- c("Level 1" = "lightblue", "Level 2" = "darkgreen", "Level 3" = "yellow", "Level 4" = "orange", "Level 5" = "red")


tm_shape(df) +
  tm_fill(col = "WQ_V1", palette = custom_colors, style = "quantile", title = "Legend Title") +
  tm_borders() +
  tm_text("name", size = 0.8, col = "black", remove.overlap = TRUE)+  # Adjust size,
  tm_shape(ab) +
  tm_borders(col = "black", lwd = 2)+
  tm_shape(mb) +
  tm_borders(col = "black", lwd = 2)+
  tm_shape(sk) +
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
  tm_basemap("CartoDB.Positron")  # Change to Esri World Imagery

