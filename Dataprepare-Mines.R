# This script processes and extracts Mineral related data from three data sources.
# a. GMF: Global mining footprint mapped from high-resolution satellite imagery.https://zenodo.org/records/6806817
# b. AMMD: Africa Major Mineral Deposits Dataset. https://gmesgeoportal.rcmrd.org/datasets/rcmrd::africa-major-mineral-deposits-dataset/about
# c. CGD: Compilation of Geospatial Data (GIS) for the Mineral Industries and Related Infrastructure of Africa. https://www.sciencebase.gov/catalog/item/607611a9d34e018b3201cbbf
# For the above three products, product a is polygon data, and products b and c are point data.
# Product b is mineral deposits, while Product c contains mineral facilities, mineral exploration, and mineral deposits. 
# Here, the points from Products b and c are combined to represent the mineral point data.
# CGD data has been saved as shapefiles using Arcgis.
# GMF polygon data has been projected into a geographic coordinate system in ArcGIS and cropped to the study area. 
# It calculates the distance from the center points of a 0.005-degree raster to the nearest mines points and polygons, respectively.
# Finally, based on the calculated 0.005-degree grids, the data are aggregated to 0.01-degree and 0.05-degree resolutions, representing the average values within those grid cells for subsequent modeling.
# Before running the code, please ensure that you have completed the following preparations:
# 1. Download and simply convert the Mineral related datasets and store them in the "F:/[2024.07]Ghana/Original Data/Mines/" folder.
# 2. Install the required R libraries.
# If not already installed, you can use the following commands to install:
# install.packages("sf")
# install.packages("raster")
# install.packages("dplyr")

# Load the necessary libraries
library(sf)
library(raster)
library(dplyr)
# Define the output folder path for saving processed mines data
output_folder <- "F:/[2024.07]Ghana/Processed Data/Mines/"
dir.create(output_folder, recursive = TRUE)
# Load the raster template from the Raster_Templates folder
raster_0.005 <- raster("F:/[2024.07]Ghana/Processed Data/Raster_Templates/0.005.tif")
# Load mineral point data
major_mineral_deposits <- st_read("F:/[2024.07]Ghana/Original Data/Mines/Africa_Major_Mineral_Deposits_8988280847527565120/Africa_Major_Mineral_Deposits.shp")
mineral_facilities <- st_read("F:/[2024.07]Ghana/Original Data/Mines/Africa_GIS/AFR_Mineral_Facilities.shp")
mineral_exploration <- st_read("F:/[2024.07]Ghana/Original Data/Mines/Africa_GIS/AFR_Mineral_Exploration.shp")
mineral_deposits <- st_read("F:/[2024.07]Ghana/Original Data/Mines/Africa_GIS/AFR_Mineral_Deposits.shp")
# Expand raster extent by 1 degree
expanded_extent <- extent(raster_0.005) + c(-1, 1, -1, 1)
# Transform vector data coordinate system to match the raster��s coordinate system
raster_crs <- crs(raster_0.005)
major_mineral_deposits_transformed <- st_transform(major_mineral_deposits, raster_crs)
# Clip data to the expanded extent
expanded_deposits <- st_crop(major_mineral_deposits_transformed, expanded_extent)
expanded_facilities <- st_crop(mineral_facilities, expanded_extent)
expanded_exploration <- st_crop(mineral_exploration, expanded_extent)
expanded_deposits_gdb <- st_crop(mineral_deposits, expanded_extent)
# Warning message: attribute variables are assumed to be spatially constant throughout all geometries 
# Check the data, the data is reasonable and the warning does not affect the result, so it can be ignored.
# plot(expanded_deposits)
# print(expanded_deposits) # Check the results after cropping
# Combine the points from Products B and C to represent the mineral point data.
# Standardize the column names of the point data
expanded_deposits_simply <- expanded_deposits %>%
  select(Name = DEP_NAME, 
         Country = COUNTRY, 
         Info1 = COMMODITY, 
         Info2 = DEP_TYPE, 
         Info3 = URL) %>%
  mutate(Source = "Deposits")
expanded_facilities_simply <- expanded_facilities %>%
  select(Name = `FeatureNam`, 
         Country = `Country`, 
         Info1 = `FeatureTyp`, 
         Info2 = DsgAttr02, 
         Info3 = `InfSource1`)%>%
  mutate(Source = "Facilities")
expanded_exploration_simply <- expanded_exploration %>%
  select(Name = `FeatureNam`, 
         Country = `Country`, 
         Info1 = `DsgAttr11`, 
         Info2 = `DsgAttr01`, 
         Info3 = `InfSource1`)%>%
  mutate(Info1 = as.character(Info1), # Convert Info1 (int type) to character type
         Source = "Exploration")
expanded_deposits_gdb_simply <- expanded_deposits_gdb %>%
  select(Name = `FeatureNam`, 
         Country = `Country`, 
         Info1 = `DsgAttr01`, 
         Info2 = `DsgAttr09`, 
         Info3 = `InfSource1`)%>%
  mutate(Source = "Deposits_GDB")
# Merge the four point datasets
combined_data <- bind_rows(expanded_deposits_simply, 
  expanded_facilities_simply, 
  expanded_exploration_simply, 
  expanded_deposits_gdb_simply
)
# Remove duplicate points based on latitude and longitude
combined_data_unique <- combined_data %>%
  distinct(geometry, .keep_all = TRUE)
st_write(combined_data_unique, paste0(output_folder,"combined_mineral_points.shp"), delete_dsn = TRUE)
# Load mineral point and polygon data
mines_points <- st_read(paste0(output_folder,"combined_mineral_points.shp"))
mines_polygons <- st_read("F:/[2024.07]Ghana/Original Data/Mines/clip7894216/clip_74548_projected polygons.shp")
mines_points_utm <- st_transform(mines_points, crs = 32630)
mines_polygons_utm <- st_transform(mines_polygons, crs = 32630)
# plot(mines_points_utm)
# Define the chunk size
chunk_size <- 0.005*400  # Adjust as needed
# Get longitude and latitude ranges
lon_range <- extent(raster_0.005)[1:2]
lat_range <- extent(raster_0.005)[3:4]
# Calculate the number of steps based on the chunk size
lon_steps <- ceiling((lon_range[2] - lon_range[1]) / chunk_size)
lat_steps <- ceiling((lat_range[2] - lat_range[1]) / chunk_size)
# Generate longitude and lontitude sequence, ensuring the last value is the maximum
lon_seq <- seq(from = lon_range[1], length.out = lon_steps, by = chunk_size)
lon_seq <- c(lon_seq, lon_range[2])  # Make sure the last value is the maximum
lat_seq <- seq(from = lat_range[1], length.out = lat_steps, by = chunk_size)
lat_seq <- c(lat_seq, lat_range[2])  # Make sure the last value is the maximum
# Extract raster
raster_0.005[] <- 0 # Prepare for subsequent extraction of grid center points
# Create an empty list to store results
rasters_list1 <- list()
rasters_list2 <- list()
# According to the generated longitude and latitude sequence, subregion processing
counter <- 0
for (i in 1:(length(lon_seq)-1)) {
  for (j in 1:(length(lat_seq)-1)) {
    # Create an extent object representing the current chunk
    current_extent <- extent(c(lon_seq[i], lon_seq[i + 1], lat_seq[j], lat_seq[j + 1]))
    # Crop the raster to the current extent
    raster_chunk <- crop(raster_0.005, current_extent)
    # Converts the raster to points data
    raster_centers_chunk <- st_as_sf(as.data.frame(rasterToPoints(raster_chunk)), coords = c("x", "y"))
    st_crs(raster_centers_chunk) <- 4326 # define CRS, EPSG:4326
    # Convert the grid center to UTM zone 30N
    raster_centers_chunk_utm <- st_transform(raster_centers_chunk, crs = 32630)
    # Calculate the distance from the center of the grid to conflicts points data
    dist1_chunk <- st_distance(raster_centers_chunk_utm, mines_points_utm)
    dist2_chunk <- st_distance(raster_centers_chunk_utm, mines_polygons_utm)
    # Gets the minimum distance 
    min_dist1_values <- apply(dist1_chunk, 1, min)
    min_dist2_values <- apply(dist2_chunk, 1, min)
    # Assigns the minimum distance value back to the current raster
    raster1_chunk <- raster_chunk
    raster2_chunk <- raster_chunk
    values(raster1_chunk) <- min_dist1_values
    values(raster2_chunk) <- min_dist2_values
    # Adds the current raster to the raster list
    rasters_list1[[length(rasters_list1) + 1]] <- raster1_chunk
    rasters_list2[[length(rasters_list2) + 1]] <- raster2_chunk
    counter <- counter + 1
    cat("Batch", counter, "processed successfully (i =", i, ", j =", j, ")\n")
    rm(current_extent,raster_chunk,raster_centers_chunk,raster_centers_chunk_utm,
       dist1_chunk,dist2_chunk, min_dist1_values,min_dist2_values,raster1_chunk,raster2_chunk)
  }
}
# Merge all the raster in the raster list, generate the 0.005 degree calculation result, and output the result.
raster1_0.005 <- do.call(merge, rasters_list1)
raster2_0.005 <- do.call(merge, rasters_list2)
writeRaster(raster1_0.005, filename = paste0(output_folder,"point_mines_distance_0.005.tif"), format = "GTiff", overwrite = TRUE)
writeRaster(raster2_0.005, filename = paste0(output_folder,"polygon_mines_distance_0.005.tif"), format = "GTiff", overwrite = TRUE)
# Aggregate to 0.01 degree and save data
raster1_aggregated_0.01 <- aggregate(raster1_0.005, fact = 0.01/ res(raster1_0.005)[1], fun = mean)
raster2_aggregated_0.01 <- aggregate(raster2_0.005, fact = 0.01/ res(raster2_0.005)[1], fun = mean)
writeRaster(raster1_aggregated_0.01, filename = paste0(output_folder, "point_mines_distance_0.01.tif"), format = "GTiff", overwrite = TRUE)
writeRaster(raster2_aggregated_0.01, filename = paste0(output_folder, "polygon_mines_distance_0.01.tif"), format = "GTiff", overwrite = TRUE)
# Aggregate to 0.05 degree and save data
raster1_aggregated_0.05 <- aggregate(raster1_0.005, fact = 0.05 / res(raster1_0.005)[1], fun = mean)
raster2_aggregated_0.05 <- aggregate(raster2_0.005, fact = 0.05 / res(raster2_0.005)[1], fun = mean)
writeRaster(raster1_aggregated_0.05, filename = paste0(output_folder, "point_mines_distance_0.05.tif"), format = "GTiff", overwrite = TRUE)
writeRaster(raster2_aggregated_0.05, filename = paste0(output_folder, "polygon_mines_distance_0.05.tif"), format = "GTiff", overwrite = TRUE)
#-------
# End of the script

