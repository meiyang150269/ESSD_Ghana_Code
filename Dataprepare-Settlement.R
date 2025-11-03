
# This script processes Settlement data to generate the proportion of settlements within pixels at different resolutions based on polygon data.
# Considering that multiple input data were used to create the GRID3 Settlement Extents, including Open Buildings data, we have chosen to use GRID3's Settlement data.
# Currently, GRID3 provides three versions of Settlement data, each based on data collected at different times, and the release dates of the three versions also vary.
# Here, we select version 2.0 of GRID3 as the target dataset.
# Due to the need to calculate the area of polygons that fall within each grid cell, this code involves extensive geometric computations, making key steps computationally intensive.
# The script was successfully executed on a machine with 64.0 GB of RAM (63.8 GB available), a 13th Gen Intel(R) Core(TM) i9-13900 processor at 2.00 GHz.
# Windows 10 Pro, a 64-bit operating system based on x64 architecture, and R version 4.4.1. 
# However, it required several hours to complete the process.
# Before running the code, please ensure that you have completed the following preparations:
# 1. Download the Settlement data and store it in the "F:/[2024.07]Ghana/Original Data/Settlement/GRID3/Grid2.0/" folder.
# 2. Install the required R libraries.
# If not already installed, you can use the following commands to install:
# install.packages("sf")  
# install.packages("raster")
# install.packages("dplyr")

# Load the necessary libraries
library(sf)
library(raster)
library(dplyr)
# Load the raster template of 0.005 degree from the Raster_Templates folder
raster_0.005 <- raster("F:/[2024.07]Ghana/Processed Data/Raster_Templates/0.005.tif")
# Define the output folder path for saving processed Settlement data
outputfold <- "F:/[2024.07]Ghana/Processed Data/Settlement/"
dir.create(outputfold, showWarnings = F, recursive = T)
# Set the path to the folders
base_path <- "F:/[2024.07]Ghana/Original Data/Settlement/GRID3/Grid2.0"
# List all folders in the base path
folder_list <- list.dirs(base_path, full.names = TRUE, recursive = FALSE)
# Initialize a list to hold all polygon shapefiles
all_polygons <- list()
# Loop through each subfolder and read the shapefile
for (folder in folder_list) {
  shapefile <- list.files(folder, pattern = "\\.shp$", full.names = TRUE)
  if (length(shapefile) > 0) {
    # Read the shapefile
    polygons <- st_read(shapefile)
    # Ensure all polygons have the same CRS (GCS_WGS_1984, EPSG: 4326)
    if (st_crs(polygons) != st_crs(4326)) {
      polygons <- st_transform(polygons, crs = 4326)  # Reproject to GCS_WGS_1984 if needed
    }
    # Filter polygons based on attributes: remove false positives with prob_fp > 0.5
    polygons <- polygons[!(polygons$is_fp == 1 & polygons$prob_fp > 0.5), ]
    # Standardize columns: keep only specified attributes
    polygons <- polygons[, c("country", "iso", "bld_count", "dou_level1", 
                             "dou_level2", "status", "is_fp", 
                             "prob_fp", "mgrs", "pcode", "date"), drop = FALSE]
    # Append to the list
    all_polygons <- c(all_polygons, list(polygons))
  }
}
# Combine all polygons into a single sf object
combined_polygons <- do.call(rbind, lapply(all_polygons, st_sf))
combined_polygons_sf  <- st_as_sf(combined_polygons) # Ensure the combined polygons are in sf format
# Check for invalid geometries in the combined polygons
invalid_geometries <- !st_is_valid(combined_polygons_sf) # This line may take a long time to run 
# Print the number of invalid geometries found
cat("Number of invalid geometries found:", sum(invalid_geometries), "\n")
# Repair invalid geometries
invalid_indices <- which(!st_is_valid(combined_polygons_sf)) # Identify invalid geometries and may take a long time to run 
invalid_polygons <- combined_polygons_sf[invalid_indices, ] # Subset to invalid geometries
valid_polygons <- st_make_valid(invalid_polygons)
combined_polygons_sf[invalid_indices, ] <- valid_polygons
# Check again for invalid geometries after attempting to repair
invalid_geometries_after <- !st_is_valid(combined_polygons_sf)
# Print the number of invalid geometries remaining
cat("Number of invalid geometries after repair:", sum(invalid_geometries_after), "\n")
# If there are still invalid geometries, remove them
if (sum(invalid_geometries_after) > 0) {
  combined_polygons_sf <- combined_polygons_sf[st_is_valid(combined_polygons_sf), ]
  cat("Invalid geometries removed. Remaining geometries:", nrow(combined_polygons_sf), "\n")
} else {
  cat("All geometries are valid after repair.\n")
}
# Crop combined polygons to the bounding box of the raster extent and save as a shapefile
raster_extent <- extent(raster_0.005)
raster_bbox <- st_as_sfc(st_bbox(raster_extent))
cropped_polygons_sf <- st_crop(combined_polygons_sf, raster_bbox) # This line may take a long time to run
st_write(cropped_polygons_sf, paste0(outputfold,"cropped_polygons.shp"), delete_dsn = TRUE) # Write the cropped polygons to a shapefile in the specified output folder
# cropped_polygons_sf <- st_read(paste0(outputfold, "cropped_polygons.shp"))
cropped_polygons_utm <- st_transform(cropped_polygons_sf, crs = 32630)
# st_write(cropped_polygons_utm, paste0(outputfold,"cropped_polygons_utm.shp"), delete_dsn = TRUE)
raster_utm <- projectRaster(raster_0.005, res = c(559.7,559), crs = 32630) # When res = c(559.7,559), raster rows and columns are the same before and after projection conversion
raster_utm[] <- 1:ncell(raster_utm) # raster pixel area with units: [m^2]
raster_polygons <- rasterToPolygons(raster_utm, na.rm = TRUE, dissolve = TRUE)
raster_polygons_sf <- st_as_sf(raster_polygons)
st_crs(raster_polygons_sf) <- st_crs(raster_utm)
# st_write(raster_polygons_sf, paste0(outputfold,"raster_polygons.shp"), delete_dsn = TRUE)
# Calculate the intersection of the projected settlement polygons with the raster polygons
cropped_polygons_intersected <- st_intersection(cropped_polygons_utm, raster_polygons_sf) # This line (st_intersection) may take a long time to run 
# Warning: attribute variables are assumed to be spatially constant throughout all geometries 
# This warning usually means that there are some attribute variables involved in the intersection 
# operation that are assumed to be constant across all geometries.
# The st_intersection operation generates overlapping polygons, as it creates a separate geometry for each intersecting part.
# Merging all intersecting polygons within each grid cell allows these parts to be aggregated, accurately calculating the total intersection area within each grid cell.
# Merge intersected polygons by the attribute X0.005 using library(dplyr)
# Merging may take a long time to run
merged_polygons_intersected <- cropped_polygons_intersected %>%
  group_by(X0.005) %>%  # Group by the X0.005 attribute
  summarise(geometry = st_union(geometry), .groups = 'drop')  # Merge geometries
# Calculate the area of the merged polygons
merged_polygons_intersected$intersection_area <- st_area(merged_polygons_intersected)
# st_write(merged_polygons_intersected, paste0(outputfold,"merged_polygons_intersected.shp"), delete_dsn = TRUE) # Write the merged polygons if necessary
# Create a data frame to hold cell IDs and corresponding intersection areas
polygon_data <- data.frame(
  cell_id = merged_polygons_intersected$X0.005, # Extract cell IDs from merged polygons
  intersection_area = merged_polygons_intersected$intersection_area # Extract intersection areas
)
new_raster <- raster_utm  # Create a new raster copy
new_raster[] <- 0 # Initialize the raster with zeros
# Assign intersection area proportions to the corresponding raster cells
raster_utm_area <- (res(raster_utm)[1]*res(raster_utm)[2]) # The area of each pixel of the UTM raster_0.005, they are the same
new_raster[polygon_data$cell_id] <- polygon_data$intersection_area/raster_utm_area
# writeRaster(new_raster, filename = paste0(outputfold, "UTM_proportion_0.005.tif"), format = "GTiff", overwrite = TRUE)
# Project the new raster to raster_0.005's CRS and resolution
proportion_0.005 <- projectRaster(new_raster, crs = crs(raster_0.005), res = res(raster_0.005), method = "ngb") 
writeRaster(proportion_0.005, filename = paste0(outputfold, "proportion_0.005.tif"), format = "GTiff", overwrite = TRUE)
# spplot(proportion_0.005)
# proportion_0.005 <- raster(paste0(outputfold, "proportion_0.005.tif"))
# Load the raster template of 0.01 degree from the Raster_Templates folder
raster_0.01 <- raster("F:/[2024.07]Ghana/Processed Data/Raster_Templates/0.01.tif")
raster_0.05 <- aggregate(raster_0.01,fact=5)
names(raster_0.05) <- "X0.05"
# Aggregate to 0.01 and 0.05 degrees and save data
proportion_0.01 <- aggregate(proportion_0.005, fact = res(raster_0.01)[1] / res(proportion_0.005)[1], fun = mean)
proportion_0.05 <- aggregate(proportion_0.005, fact = res(raster_0.05)[1] / res(proportion_0.005)[1], fun = mean)
writeRaster(proportion_0.01, filename = paste0(outputfold, "proportion_0.01.tif"), format = "GTiff", overwrite = TRUE)
writeRaster(proportion_0.05, filename = paste0(outputfold, "proportion_0.05.tif"), format = "GTiff", overwrite = TRUE)
# spplot(proportion_0.01)
# spplot(proportion_0.05)
#-------
# End of the script
