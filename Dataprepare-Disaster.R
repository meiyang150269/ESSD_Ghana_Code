# This script processes and extracts Natural disasters data from various sources.
# Geocoded Disasters (GDIS) Dataset (1960�C2018) The Geocoded Disasters (GDIS) Dataset (1960�C2018) records information on floods, storms (typhoons, monsoons, etc.), earthquakes, landslides, droughts, and volcanic activity.
# Using Python GEE, point data was extracted; however, due to limited valid data in recent years, disaster data for the study area from 1999 to 2018 was extracted. 
# As there were no records for earthquakes, landslides, or volcanic activity in the study area, only floods, storms, and droughts were output.
# Saved respectively as "Disaster/Extraction/Flood_GDIS_1999_2018.shp","Storm_GDIS_1999_2018.shp","Drought_GDIS_1999_2018.shp".
# Geocoded Disasters (GDIS) Dataset (1960�C2018): https://gee-community-catalog.org/projects/gdis/
# (a) Earthquake: USGS Global Earthquake dataset provides earthquake record data from 1923 to 2024. Using Python GEE, point data was extracted.
# Due to a lack of valid data in recent years, earthquake data from 2003 to 2022 for the study area was extracted and saved as "Disaster/Extraction/Earthquake_USGUE_2003_2022.shp".
# USGS Global Earthquake dataset: https://gee-community-catalog.org/projects/global_earthquakes/
# (b) Cyclone: International Best Track Archive for Climate Stewardship (IBTrACS). Download to Disaster\IBTrACS.last3years.list.v04r01.points.
# IBTrACS dataset contains no data within the study area.
# Storm: from GDIS data, i.e., Storm_GDIS_1999_2018.shp (only one record)
# IBTrACS: https://www.ncei.noaa.gov/products/international-best-track-archive
# (c) Drought: from GDIS data, i.e., Drought_GDIS_1999_2018.shp
# The Standardized Precipitation-Evapotranspiration Index (SPEI), which can also represent drought information, was extracted in "Dataprepare-SPEI.R".
# (d) Flood: Global Flood Database v1 (2000-2018) contains maps of the extent and temporal distribution of 913 flood events occurring between 2000-2018.
# Using the Python API, the duration (number of days a flood event lasted) for the study area's floods in 2018 was extracted, mapping all floods to generate the satellite-observed historical floodplain and overlaying permanent water to distinguish flood water.
# The results were saved as "Disaster/Extraction/Flood_GLD_duration_2018.tif","Flood_GLD_flooded_2018.tif",""Flood_GLD_flooded_jrc_perm_water_2018.tif��.
# The three flood TIFF datasets maintain the original resolution of 30 meters.
# Meanwhile, flood event information was extracted. Since there was only one flood event (in 2018) within five years.
# flood events from 2009 to 2018 were extracted and saved as ��Flood_GLD_image_properties_2009_2018.shp.��
# Flood: from GDIS data, i.e., Flood_GDIS_1999_2018.shp
# Global Flood Database v1 (2000-2018): https://developers.google.com/earth-engine/datasets/catalog/GLOBAL_FLOOD_DB_MODIS_EVENTS_V1
# (e) Volcano: NESDIS Volcano Locations Database. Queried online by expanding the study area, and the results were saved as ��volcanoes-2024-10-30_18-37-42_+0800.tsv��.
# Based on this data, volcano point data was generated using ArcGIS and saved as ��Disaster/Extraction/Volcano_NESDIS.shp��.
# No records for NESDIS Volcano in the study area.
# In GDIS data, no records for volcanic activity in the study area
# Volcano: https://www.ngdc.noaa.gov/hazard/volcano.shtml
# Thence, no valid data for volcanoes in study area.
# (f) Landslide: Global Landslide Catalog: NASA Goddard (1970-2019).Due to the lack of valid data records for recent years within the study area, only global landslide records from 2000 to 2019 were extracted.
# These global landslide records were saved as ��Disaster/Extraction/Landslide_GLC_2000_2019.shp.��
# Landslide: In GDIS data, no records for Landslide in the study area
# Global Landslide Catalog: NASA Goddard (1970-2019) https://gee-community-catalog.org/projects/landslide/
# To calculate the distance from the center points of a 0.005-degree raster to each type of disaster points (Earthquake, Drought, Flood, Landslide).
# Then aggregate the data to 0.01-degree and 0.05-degree resolutions
# Note: There is no valid data for volcanoes, so no further calculations will be performed for volcanoes.
#       There is only one record for cyclones (from storms), so no further calculations will be performed for cyclones. 
# Before running the code, please ensure that you have completed the following preparations:
# 1. Download and convert the disaster datasets and store them in the "F:/[2024.07]Ghana/Processed Data/Disaster/Extraction/" folder.
# 2. Install the required R libraries.
# If not already installed, you can use the following commands to install:
# install.packages("sf")
# install.packages("raster")
# install.packages("dplyr")

# Load the necessary libraries
library(sf)
library(raster)
library(dplyr)
# Load the raster template from the Raster_Templates folder
raster_0.005 <- raster("F:/[2024.07]Ghana/Processed Data/Raster_Templates/0.005.tif")
# Define the output folder path for saving processed Disaster data
outputfold <- "F:/[2024.07]Ghana/Processed Data/Disaster/"
dir.create(outputfold, showWarnings = F, recursive = T)
# Load disaster point data
Drought_GDIS_1999_2018 <- st_read("F:/[2024.07]Ghana/Processed Data/Disasters/Extraction/Drought_GDIS_1999_2018.shp")
Earthguake_USGUE_2003_2022 <- st_read("F:/[2024.07]Ghana/Processed Data/Disasters/Extraction/Earthguake_USGUE_2003_2022.shp")
Flood_GLD_image_properties_2009_2018 <- st_read("F:/[2024.07]Ghana/Processed Data/Disasters/Extraction/Flood_GLD_image_properties_2009_2018.shp")
Flood_GLD_image_properties_2009_2018 <- st_crop(Flood_GLD_image_properties_2009_2018, extent(raster_0.005))
Flood_GLD <- Flood_GLD_image_properties_2009_2018 %>%
  select(Ind = sys_index, 
         Country = countries, 
         Info1 = dfo_main_c) %>%
  mutate(Source = "GLD")
Flood_GDIS_1999_2018 <- st_read(paste0("F:/[2024.07]Ghana/Processed Data/Disasters/Extraction/Flood_GDIS_1999_2018.shp"))
Flood_GDIS <- Flood_GDIS_1999_2018 %>%
  select(Ind = system_ind, 
         Country = country, 
         Info1 = year) %>%
  mutate(Info1 = as.character(Info1), # Convert Info1 to character type
         Source = "GDIS")
Flood_1999_2018 <- rbind(Flood_GLD, Flood_GDIS)
Landslide_GLC_2000_2019 <- st_read(paste0("F:/[2024.07]Ghana/Processed Data/Disasters/Extraction/Landslide_GLC_2000_2019.shp"))
Landslide_GLC_2000_2019 <- st_crop(Landslide_GLC_2000_2019, extent(raster_0.005))
# Convert to projection coordinates
Drought_utm <- st_transform(Drought_GDIS_1999_2018, crs = 32630) # plot(Drought_utm)
Earthguake_utm <- st_transform(Earthguake_USGUE_2003_2022, crs = 32630) # plot(Earthguake_utm)
Flood_utm <- st_transform(Flood_1999_2018, crs = 32630) # plot(Flood_utm)
Landslide_utm <- st_transform(Landslide_GLC_2000_2019, crs = 32630) # plot(Landslide_utm)
# Extract raster
raster_0.005[] <- 0 # Prepare for subsequent extraction of grid center points
raster_centers <- st_as_sf(as.data.frame(rasterToPoints(raster_0.005)), coords = c("x", "y"))
st_crs(raster_centers) <- 4326 # define CRS, EPSG:4326
# Convert the grid center to UTM zone 30N
raster_centers_utm <- st_transform(raster_centers, crs = 32630)
# Calculate the distance from the center of the grid to disaster points data
dist_Drought <- st_distance(raster_centers_utm, Drought_utm)
dist_Earthguake <- st_distance(raster_centers_utm, Earthguake_utm)
dist_Flood <- st_distance(raster_centers_utm, Flood_utm)
dist_Landslide <- st_distance(raster_centers_utm, Landslide_utm)
# Gets the minimum distance 
min_dist_Drought_values <- apply(dist_Drought, 1, min)
min_dist_Earthguake_values <- apply(dist_Earthguake, 1, min)
min_dist_Flood_values <- apply(dist_Flood, 1, min)
min_dist_Landslide_values <- apply(dist_Landslide, 1, min)
# Generate the 0.005 degree calculation result, and output the result.
raster_Drought <- raster_0.005
values(raster_Drought) <- min_dist_Drought_values
writeRaster(raster_Drought, filename = paste0(outputfold,"Drought_1999_2018_distance_0.005.tif"), format = "GTiff", overwrite = TRUE)
raster_Earthguake <- raster_0.005
values(raster_Earthguake) <- min_dist_Earthguake_values
writeRaster(raster_Earthguake, filename = paste0(outputfold,"Earthguake_2003_2022_distance_0.005.tif"), format = "GTiff", overwrite = TRUE)
raster_Flood <- raster_0.005
values(raster_Flood) <- min_dist_Flood_values
writeRaster(raster_Flood, filename = paste0(outputfold,"Flood_1999_2018_distance_0.005.tif"), format = "GTiff", overwrite = TRUE)
raster_Landslide <- raster_0.005
values(raster_Landslide) <- min_dist_Landslide_values
writeRaster(raster_Landslide, filename = paste0(outputfold,"Landslide_2000_2019_distance_0.005.tif"), format = "GTiff", overwrite = TRUE)
# Aggregate to 0.01 degree and save data
Drought_aggregated_0.01 <- aggregate(raster_Drought, fact = 0.01/ res(raster_0.005)[1], fun = mean)
writeRaster(Drought_aggregated_0.01, filename = paste0(outputfold,"Drought_1999_2018_distance_0.01.tif"), format = "GTiff", overwrite = TRUE)
Earthguake_aggregated_0.01 <- aggregate(raster_Earthguake, fact = 0.01/ res(raster_0.005)[1], fun = mean)
writeRaster(Earthguake_aggregated_0.01, filename = paste0(outputfold,"Earthguake_2003_2022_distance_0.01.tif"), format = "GTiff", overwrite = TRUE)
Flood_aggregated_0.01 <- aggregate(raster_Flood, fact = 0.01/ res(raster_0.005)[1], fun = mean)
writeRaster(Flood_aggregated_0.01, filename = paste0(outputfold,"Flood_1999_2018_distance_0.01.tif"), format = "GTiff", overwrite = TRUE)
Landslide_aggregated_0.01 <- aggregate(raster_Landslide, fact = 0.01/ res(raster_0.005)[1], fun = mean)
writeRaster(Landslide_aggregated_0.01, filename = paste0(outputfold,"Landslide_2000_2019_distance_0.01.tif"), format = "GTiff", overwrite = TRUE)
# Aggregate to 0.05 degree and save data
Drought_aggregated_0.05 <- aggregate(raster_Drought, fact = 0.05/ res(raster_0.005)[1], fun = mean)
writeRaster(Drought_aggregated_0.05, filename = paste0(outputfold,"Drought_1999_2018_distance_0.05.tif"), format = "GTiff", overwrite = TRUE)
Earthguake_aggregated_0.05 <- aggregate(raster_Earthguake, fact = 0.05/ res(raster_0.005)[1], fun = mean)
writeRaster(Earthguake_aggregated_0.05, filename = paste0(outputfold,"Earthguake_2003_2022_distance_0.05.tif"), format = "GTiff", overwrite = TRUE)
Flood_aggregated_0.05 <- aggregate(raster_Flood, fact = 0.05/ res(raster_0.005)[1], fun = mean)
writeRaster(Flood_aggregated_0.05, filename = paste0(outputfold,"Flood_1999_2018_distance_0.05.tif"), format = "GTiff", overwrite = TRUE)
Landslide_aggregated_0.05 <- aggregate(raster_Landslide, fact = 0.05/ res(raster_0.005)[1], fun = mean)
writeRaster(Landslide_aggregated_0.05, filename = paste0(outputfold,"Landslide_2000_2019_distance_0.05.tif"), format = "GTiff", overwrite = TRUE)
# For flood tiff datasets
# Calculate the distance from the raster to the floodplain area (excluding non-permanent water bodies).
# flood_duration_2018 <- raster("F:/[2024.07]Ghana/Processed Data/Disaster/Extraction/Flood_GLD_duration_2018.tif")
# flooded_2018 <- raster("F:/[2024.07]Ghana/Processed Data/Disaster/Extraction/Flood_GLD_flooded_2018.tif")
flooded_perm_water_2018 <- raster("F:/[2024.07]Ghana/Processed Data/Disaster/Extraction/Flood_GLD_flooded_jrc_perm_water_2018.tif")
flooded_perm_water_2018[flooded_perm_water_2018 == 255] <- NA
writeRaster(flooded_perm_water_2018, filename = paste0(outputfold,"Flooded_GLD_without perm water.tif"), format = "GTiff", overwrite = TRUE)
flooded <- flooded_perm_water_2018
flooded[flooded == 0] <- NA
writeRaster(flooded, filename = paste0(outputfold,"Flooded_waiting.tif"), format = "GTiff", overwrite = TRUE)
# flooded <- raster(paste0(outputfold,"Flooded_waiting.tif"))
flooded_utm <- projectRaster(flooded, crs = CRS("EPSG:32630"))
flooded_perm_water_distance <- distance(flooded_utm)
writeRaster(flooded_perm_water_distance, filename = paste0(outputfold,"flooded_perm_water_distance.tif"), format = "GTiff", overwrite = TRUE)

raster_utm <- projectRaster(raster_0.005, res = c(559.7,559), crs = 32630) # When res = c(559.7,559), raster rows and columns are the same before and after projection conversion

#-------
# End of the script
# For flood tiff datasets
# Calculate the distance from the raster to the floodplain area (excluding non-permanent water bodies).
# flood_duration_2018 <- raster("F:/[2024.07]Ghana/Processed Data/Disaster/Extraction/Flood_GLD_duration_2018.tif")
# flooded_2018 <- raster("F:/[2024.07]Ghana/Processed Data/Disaster/Extraction/Flood_GLD_flooded_2018.tif")

# ����鷺���������������ˮ�壩��ռ��
flooded_perm_water_2018 <- raster("F:/[2024.07]Ghana/Processed Data/Disaster/Extraction/Flood_GLD_flooded_jrc_perm_water_2018.tif")
flooded_perm_water_2018[flooded_perm_water_2018 == 255] <- NA
raster_0.005[] <- 0
high_res_raster <- disaggregate(raster_0.005,fact=19)
flooded_high_res <- resample(flooded_perm_water_2018, high_res_raster, method = "ngb")
flooded_aggregated <- aggregate(flooded_high_res, fact = 19, fun = sum)  # ����ۺ�����
flooded_ratio_0.005 <- flooded_aggregated / (19*19)
flooded_ratio_0.01 <- aggregate(flooded_ratio_0.005, fact = 2, fun = mean)
flooded_ratio_0.05 <- aggregate(flooded_ratio_0.005, fact = 10, fun = mean)
writeRaster(flooded_ratio_0.005, filename = paste0(outputfold,"flooded_perm_water_ratio_0.005.tif"), format = "GTiff", overwrite = TRUE)
writeRaster(flooded_ratio_0.01, filename = paste0(outputfold,"flooded_perm_water_ratio_0.01.tif"), format = "GTiff", overwrite = TRUE)
writeRaster(flooded_ratio_0.05, filename = paste0(outputfold,"flooded_perm_water_ratio_0.05.tif"), format = "GTiff", overwrite = TRUE)
# ��ȡȡֵΪ1����Ԫ���ģ����ɵ����ݣ����о���������ܶȼ��㡣
flooded_perm_water_2018 <- raster("E:/YanJ/Disaster/Flooded_GLD_without perm water.tif")
coords <- xyFromCell(flooded_perm_water_2018, which(values(flooded_perm_water_2018) == 1))
points_sf <- st_as_sf(data.frame(x = coords[, 1], y = coords[, 2]), coords = c("x", "y"), crs = st_crs(raster_0.005))
st_write(points_sf, "E:/YanJ/Disaster/flooded_perm_water_points.shp", delete_dsn = TRUE) # Write the merged lines if necessary

points_sf_utm <- st_transform(points_sf, crs = 32630)
st_write(points_sf_utm, "E:/YanJ/Disaster/flooded_perm_water_points_utm.shp", delete_dsn = TRUE) # Write the merged lines if necessary

points_sf_utm <- st_read("E:/YanJ/Disaster/flooded_perm_water_points_utm.shp")

