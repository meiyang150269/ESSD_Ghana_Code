
# This script processes the River data from two sources.
# [Source 1] Product A: HydroSHEDS database offers a suite of global digital data layers in support of hydro-ecological research.
# https://www.hydrosheds.org/
# The 15-arc-seconds resolution data for The LUP-Flow length upstream and LDN-Flow length downstream has been used here.
# (a) LUP-Flow length upstream: distance to furthest source of river in meters
# (b) LDN-Flow length downstream: distance to final point at ocean or inland sink in meters 
# The HydroRIVERS and HydroLAKES products provide river and lake data, which have been clipped to the study area using ArcGIS.
# Rivers and Lakes data have been saved in "F:/[2024.07]Ghana/Processed Data/River/Hydro_Rivers_Lakes" as "HydroRIVERS_Clip.shp" and "HydroLAKES_Clip.shp," respectively.
# (c) HydroRIVERS Density: Calculate the river network density based on the HydroRIVERS data.
# [Source 2] OSM water data for 2021 and 2022: "Water body" data (gis_osm_water_a_free_1.shp) and "waterways" data (gis_osm_waterways_free_1.shp). 
# "water body" encompasses various types of aquatic environments, including lakes, reservoirs, rivers, docks, and wetlands.
# "water" refers to unspecified bodies of water, typically lakes, but it can also include larger rivers, harbors, and similar formations.
# The reservoirs in OSM's water body data represent artificial lakes. 
# Therefore, the Reservoirs dataset will be merged with HydroLAKES in ArcGIS. 
# This merged Lakes dataset have been saved in the directory "F:[2024.07]Ghana\Processed Data\River\Lakes_Reservoir" as "Lakes_reservoir2021.shp" and "Lakes_reservoir2022.shp".
# (d) Lake Distance: Calculate the nearest distance from the raster center points to the lakes based on the new lake data.
# (e) waterways Distance: Calculate the nearest distance from the raster center points to the different waterways.
# waterways data, it includes rivers, streams, canals, and drains. Extract the different types of waterways for the years 2021 and 2022 using ArcGIS.
# If you want to perform batch extraction using R, you can refer to the script for extracting different types of attributes in "Dataprepare-Road.R".
# Before running the code, please ensure that you have completed the following preparations:
# 1. Download and clip the OSM data, then store them in the "F:/[2024.07]Ghana/Processed Data/River" folder.
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
raster_0.005[] <- 0 # Prepare for subsequent extraction of grid center points
# Source the min_distance function from the R script
source("F:/[2024.07]Ghana/Processed Data/Script/updated/Dataprepare_calculate_min_distance.R")
# source("F:/[2024.07]Ghana/Processed Data/Script/Dataprepare_calculate_min_distance.R")
#----(a) LUP-Flow length upstream and (b) LDN-Flow length downstream
# Define the paths for the images
ldn_lup_path <- paste0("F:/[2024.07]Ghana/Original Data/River/",
                       c("hyd_af_lup_15s/hyd_af_lup_15s.tif","hyd_af_ldn_15s/hyd_af_ldn_15s.tif"))
output_folder <- "F:/[2024.07]Ghana/Processed Data/River/"
filenames <- c("LUP","LND")
for (k in 1:length(ldn_lup_path)) {
  # Read the raster image
  raster_image <- raster(ldn_lup_path[k])
  # Resample to raster_0.005 resolution using average
  raster_resampled <- resample(raster_image, raster_0.005, method = "ngb")
  writeRaster(raster_resampled, filename = paste0(output_folder,filenames[k],"_0.005.tif"), format = "GTiff", overwrite = TRUE)
  # Aggregate to 0.01 degrees
  raster_aggregate_001 <- aggregate(raster_resampled, fact = 0.01/0.005, fun = mean)
  writeRaster(raster_aggregate_001, filename = paste0(output_folder,filenames[k],"_0.01.tif"), format = "GTiff", overwrite = TRUE)
  # Aggregate to 0.05 degrees
  raster_aggregate_005 <- aggregate(raster_resampled, fact = 0.05/0.005, fun = mean)
  writeRaster(raster_aggregate_005, filename = paste0(output_folder,filenames[k],"_0.05.tif"), format = "GTiff", overwrite = TRUE)
  cat(paste0("Processed: ", filenames[k], "\n"))
  rm(raster_image,raster_resampled,raster_aggregate_001,raster_aggregate_005)
}
#----(c) HydroRIVERS Density: 
# Load the line data from the shapefile
line_data_path <- "F:/[2024.07]Ghana/Processed Data/River/Hydro_Rivers_Lakes/HydroRIVERS_Clip.shp"
HydroRIVERS_data <- st_read(line_data_path)
# Transform line data to EPSG:32630
HydroRIVERS_utm <- st_transform(HydroRIVERS_data, crs = 32630)
raster_utm <- projectRaster(raster_0.005, res = c(559.7,559), crs = 32630) # When res = c(559.7,559), raster rows and columns are the same before and after projection conversion
raster_utm[] <- 1:ncell(raster_utm) # raster pixel area with units: [m^2]
raster_polygons <- rasterToPolygons(raster_utm, na.rm = TRUE, dissolve = TRUE)
raster_polygons_sf <- st_as_sf(raster_polygons)
st_crs(raster_polygons_sf) <- st_crs(raster_utm)
# st_write(raster_polygons_sf, paste0(output_folder,"raster_polygons.shp"), delete_dsn = TRUE)
# raster_polygons_sf <- st_read(paste0(output_folder,"raster_polygons.shp"))
raster_polygons_sf <- st_read("F:/[2024.07]Ghana/Processed Data/Settlement/raster_polygons.shp")
# Calculate the intersection of the projected River lines with the raster polygons
cropped_lines_intersected <- st_intersection(HydroRIVERS_utm, raster_polygons_sf) # This line (st_intersection) may take a long time to run 
# Warning: attribute variables are assumed to be spatially constant throughout all geometries 
# This warning usually means that there are some attribute variables involved in the intersection 
# operation that are assumed to be constant across all geometries.
# The st_intersection operation generates overlapping polygons, as it creates a separate geometry for each intersecting part.
# Merging all intersecting lines within each grid cell allows these parts to be aggregated, accurately calculating the total intersection area within each grid cell.
# Merge intersected lines by the attribute X0.005 using library(dplyr)
# Merging may take a long time to run
merged_lines_intersected <- cropped_lines_intersected %>%
  group_by(X0.005) %>%  # Group by the X0.005 attribute
  summarise(geometry = st_union(geometry), .groups = 'drop')  # Merge geometries
# Calculate the line length of the merged lines
merged_lines_intersected$intersection_length <- st_length(merged_lines_intersected)
# st_write(merged_lines_intersected, paste0(output_folder,"merged_lines_intersected.shp"), delete_dsn = TRUE) # Write the merged lines if necessary
# Create a data frame to hold cell IDs and corresponding intersection lines
line_data <- data.frame(
  cell_id = merged_lines_intersected$X0.005, # Extract cell IDs from merged polygons
  intersection_length = merged_lines_intersected$intersection_length # Extract intersection areas
)
new_raster <- raster_utm  # Create a new raster copy
new_raster[] <- 0 # Initialize the raster with zeros
# Assign the sum lenghts of intersection lines to the corresponding raster cells
raster_utm_area <- (res(raster_utm)[1]*res(raster_utm)[2]) # The area of each pixel of the UTM raster_0.005, they are the same
new_raster[line_data$cell_id] <- line_data$intersection_length/raster_utm_area
# writeRaster(new_raster, filename = paste0(output_folder, "UTM_HydroRIVERS_0.005.tif"), format = "GTiff", overwrite = TRUE)
# Project the new raster to raster_0.005's CRS and resolution
LineDens_0.005 <- projectRaster(new_raster, crs = crs(raster_0.005), res = res(raster_0.005), method = "ngb") 
writeRaster(LineDens_0.005, filename = paste0(output_folder, "HydroRIVERS_linear_density_0.005.tif"), format = "GTiff", overwrite = TRUE)
# spplot(LineDens_0.005)
# LineDens_0.005 <- raster(paste0(output_folder, "HydroRIVERS_linear_density_0.005.tif"))
# Load the raster template of 0.01 degree from the Raster_Templates folder
raster_0.01 <- raster("F:/[2024.07]Ghana/Processed Data/Raster_Templates/0.01.tif")
raster_0.05 <- aggregate(raster_0.01,fact=5)
names(raster_0.05) <- "X0.05"
# Aggregate to 0.01 and 0.05 degrees and save data
LineDens_0.01 <- aggregate(LineDens_0.005, fact = res(raster_0.01)[1] / res(LineDens_0.005)[1], fun = mean)
LineDens_0.05 <- aggregate(LineDens_0.005, fact = res(raster_0.05)[1] / res(LineDens_0.005)[1], fun = mean)
writeRaster(LineDens_0.01, filename = paste0(output_folder, "HydroRIVERS_linear_density_0.01.tif"), format = "GTiff", overwrite = TRUE)
writeRaster(LineDens_0.05, filename = paste0(output_folder, "HydroRIVERS_linear_density_0.05.tif"), format = "GTiff", overwrite = TRUE)
# spplot(LineDens_0.01)
# spplot(LineDens_0.05)
#----(d) Lake Distance
# Define the folder paths
lakes_path <- "F:/[2024.07]Ghana/Processed Data/River/Lakes_Reservoir"
# Read Lakes_Reservoir folder shapefiles
lakes_files <- list.files(lakes_path, pattern = "\\.shp$", full.names = TRUE)
# Loop through each shapefile in the Lakes folder
for (file in lakes_files) {
  # file <- lakes_files[1]
  # Extract the file name as a variable name (without path and extension)
  var_name <- tools::file_path_sans_ext(basename(file))
  # Read the shapefile
  shapefile_data <- st_read(file)
  # Transform the shapefile to UTM projection (EPSG:32630)
  eval(parse(text = paste0(var_name, "_utm <- st_transform(shapefile_data, crs = 32630)")))
  # Call the function to calculate the minimum distance
  min_distance <- Dataprepare_calculate_min_distance(
    chunk_size = 0.005 * 400,  # Set the chunk size
    rasterT = raster_0.005,     # Assume raster_0.005 is defined elsewhere
    shape_utm = get(paste0(var_name, "_utm")),  # Get the transformed shape object
    aggregate_degrees = c(0.01, 0.05), 
    output_folder = "F:/[2024.07]Ghana/Processed Data/River/",
    output_filename = paste0(var_name, "_min_distance")
  )
  cat(paste0("Processed: ", var_name, "\n"))
}
#----(e) waterways Distance
# Define the folder paths
waterways_path <- "F:/[2024.07]Ghana/Processed Data/River/OSM_Waterways"
# Read OSM_Waterways folder shapefiles
waterways_files <- list.files(waterways_path, pattern = "\\.shp$", full.names = TRUE)
# Loop through each shapefile in the Waterways folder
for (file in waterways_files) {
  # Extract the file name as a variable name (without path and extension)
  var_name <- tools::file_path_sans_ext(basename(file))
  # Read the shapefile
  shapefile_data <- st_read(file)
  # Transform the shapefile to UTM projection (EPSG:32630)
  eval(parse(text = paste0(var_name, "_utm <- st_transform(shapefile_data, crs = 32630)")))
  # Call the function to calculate the minimum distance
  min_distance <- Dataprepare_calculate_min_distance(
    chunk_size = 0.005 * 400,  # Set the chunk size
    rasterT = raster_0.005,     # Assume raster_0.005 is defined elsewhere
    shape_utm = get(paste0(var_name, "_utm")),  # Get the transformed shape object
    aggregate_degrees = c(0.01, 0.05), 
    output_folder = "F:/[2024.07]Ghana/Processed Data/River/",
    output_filename = paste0(var_name, "_min_distance")
  )
  cat(paste0("Processed: ", var_name, "\n"))
}
#-------
# End of the script

