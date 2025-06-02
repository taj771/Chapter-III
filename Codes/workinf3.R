### Clear memory
rm(list = ls())


database <- read_csv("Deriveddata/processed_pilotdata_1_Apollo.csv")


df <- database%>%
  select(CaseId,Q24_REC_TRIP_LAST_YEAR,Q25_WB1_NAME,Q25_WB2_NAME,Q25_WB3_NAME,Q25_WB4_NAME,Q25_WB5_NAME,
         Q25_WB1_WQ_LEVEL,Q25_WB2_WQ_LEVEL,Q25_WB3_WQ_LEVEL,Q25_WB4_WQ_LEVEL,Q25_WB5_WQ_LEVEL,
         Q25_WB1_NEAR_TOWN,Q25_WB2_NEAR_TOWN,Q25_WB3_NEAR_TOWN,Q25_WB4_NEAR_TOWN,Q25_WB5_NEAR_TOWN,
         COMMENTS)%>%
  distinct(CaseId, .keep_all = T)%>%
  filter(Q24_REC_TRIP_LAST_YEAR==1)




cities_sk <- st_read("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/Shapefile/saskatchewan-latest-free/gis_osm_places_free_1.shp")
cities_mb <- st_read("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/Shapefile/map_images/manitoba-latest-free/gis_osm_places_free_1.shp")
cities_ab <- st_read("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/Shapefile/map_images/alberta-latest-free/gis_osm_places_free_1.shp")

cities_sk  <- st_transform(cities_sk, st_crs(cities_mb))
cities_ab <- st_transform(cities_ab, st_crs(cities_sk))


cities <- bind_rows(cities_sk,cities_mb,cities_ab)

study_area <- st_read("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/Shapefile/study_area.shp")


# Ensure both layers use the same CRS
cities <- st_transform(cities, st_crs(study_area))


# Spatial join: assign each point to the polygon it falls into
cities_in_study_area <- st_join(cities, study_area, join = st_within)




water_sk <- st_read("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter III-UseNonUseValue/Survey/Shapefile/saskatchewan-latest-free/gis_osm_water_a_free_1.shp")


tm_shape(water_sk)+
  tm_borders(col = "black", lwd = 2)

gis_osm_water_a_free_1.cpg











t <- study_area%>%
  filter(WSCSDA_E== "Assiniboine")
t1 <- cities_in_study_area%>%
  filter(WSCSDA_E== "Assiniboine")

tm_shape(t)+
  tm_borders(col = "black", lwd = 2)+
  tm_shape(t1) +
  tm_dots()
