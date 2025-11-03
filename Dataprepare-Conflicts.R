
# This script processes conflict point data using the UCDP Candidate Events Dataset (UCDP Candidate) version 22.0.X (global) and version 21.0.X (global) data.
# It calculates the distance from the center points of a 0.005-degree raster to the nearest conflict points and computes the total number of deaths from all conflict points within a 10 km radius around each 0.005-degree raster center point.
# select the best death count (��best�� attribute) for statistics. 
# Considering that the nearest conflict points to the study area boundary may be located outside the study area, the conflict point data needs to be clipped based on the study area. 
# Therefore, the range was expanded by 5 degrees in the experiment. 
# Finally, based on the calculated 0.005-degree grids, the data are aggregated to 0.01-degree and 0.05-degree resolutions, representing the average values within those grid cells for subsequent modeling.
# Before running the code, please ensure that you have completed the following preparations:
# 1. Download the conflict point data and store it in the "F:/[2024.07]Ghana/Original Data/conflicts" folder.
# 2. Install the required R libraries.
# If not already installed, you can use the following commands to install:
# install.packages("sf")  
# install.packages("raster")

# Load the necessary libraries
library(sf)
library(raster)
# Load the raster template from the Raster_Templates folder
raster_0.005 <- raster("F:/[2024.07]Ghana/Processed Data/Raster_Templates/0.005.tif")
raster_0.01 <- raster("F:/[2024.07]Ghana/Processed Data/Raster_Templates/0.01.tif")
raster_0.05 <- aggregate(raster_0.01,fact=5)
names(raster_0.05) <- "X0.05"
# Define the output folder path for saving processed Conflicts data
outputfold <- "F:/[2024.07]Ghana/Processed Data/Conflicts/"
dir.create(outputfold, showWarnings = F, recursive = T)
# Set the working directory to where your CSV files are located
setwd("F:/[2024.07]Ghana/Processed Data/Conflicts")
# Read the CSV files
data_v21 <- read.csv("GEDEvent_v21_new.csv")
data_v21 <- data_v21[, c("latitude", "longitude", "best")]
data_v22 <- read.csv("GEDEvent_v22_new.csv")
data_v22 <- data_v22[, c("latitude", "longitude", "best")]
# Convert the latitude and longitude into sf objects (points)
points_v21 <- st_as_sf(data_v21, coords = c("longitude", "latitude"), crs = 4326)  # WGS 84
points_v22 <- st_as_sf(data_v22, coords = c("longitude", "latitude"), crs = 4326)  # WGS 84
expanded_extent <- extend(extent(raster_0.005), 5) # Expand the bounding box by 5 degree
expanded_bbox <- st_as_sfc(st_bbox(expanded_extent)) # Convert the expanded bounding box to an sf object
# Crop points_v21 and points_v22 using the expanded bounding box
cropped_points_v21 <- st_crop(points_v21, expanded_bbox)
# Warning message: attribute variables are assumed to be spatially constant throughout all geometries 
# Check the data, the data is reasonable and the warning does not affect the result, so it can be ignored.
# plot(cropped_points_v21)
# print(cropped_points_v21) # Check the results after cropping
cropped_points_v22 <- st_crop(points_v22, expanded_bbox)
# plot(cropped_points_v22)
# print(cropped_points_v22) # Check the results after cropping
# st_write(cropped_points_v21, paste0(outputfold,"cropped_points_v21.shp"), delete_dsn = TRUE)
# st_write(cropped_points_v22, paste0(outputfold,"cropped_points_v22.shp"), delete_dsn = TRUE)
points_v21_utm <- st_transform(cropped_points_v21, crs = 32630)
points_v22_utm <- st_transform(cropped_points_v22, crs = 32630)
# plot(cropped_points_v22)
# Define the chunk size
chunk_size <- 0.005*200  # Adjust as needed
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
rasters_list21 <- list()
rasters_list22 <- list()
rastersum_list21 <- list()
rastersum_list22 <- list()
buffer_distance <- 10000  # buffer with 10 km
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
    dist21_chunk <- st_distance(raster_centers_chunk_utm, points_v21_utm)
    dist22_chunk <- st_distance(raster_centers_chunk_utm, points_v22_utm)
    # Gets the minimum distance 
    min_dist21_values <- apply(dist21_chunk, 1, min)
    min_dist22_values <- apply(dist22_chunk, 1, min)
    # Assigns the minimum distance value back to the current raster
    raster21_chunk <- raster_chunk
    raster22_chunk <- raster_chunk
    values(raster21_chunk) <- min_dist21_values
    values(raster22_chunk) <- min_dist22_values
    # Create buffers for each center point
    buffers <- st_buffer(raster_centers_chunk_utm, dist = buffer_distance)
    # run the codes in if{}: plot buffers for check
    if (FALSE) {
      library(ggplot2)
      ggplot() +
        geom_sf(data = buffers, fill = "lightblue", alpha = 0.5) +  # ���ƻ�����
        geom_sf(data = raster_centers_chunk_utm, aes(color = "red"), size = 1.5) +  # �������ĵ�
        theme_minimal() +
        labs(title = "Buffers and Raster Centers",
             color = "Raster Centers") +
        coord_sf()  # ȷ���������ʺϿռ�����
    }
    # Calculate the total number of deaths at all conflict points in the buffer zone
    best_sum21 <- sapply(1:nrow(buffers), function(k) {
      total_best <- sum(st_within(points_v21_utm, buffers[k, ], sparse = FALSE) * points_v21_utm$best, na.rm = TRUE)
      cat(sprintf("buffers %d processed successfully.\n", k))
      return(total_best)
      rm(total_best)
    })
    best_sum22 <- sapply(1:nrow(buffers), function(k) {
      total_best <- sum(st_within(points_v22_utm, buffers[k, ], sparse = FALSE) * points_v22_utm$best, na.rm = TRUE)
      cat(sprintf("buffers %d processed successfully.\n", k))
      return(total_best)
      rm(total_best)
      })
    # Assigns the total number of deaths value back to the current raster
    raster21_chunk_sum <- raster_chunk
    raster22_chunk_sum <- raster_chunk
    values(raster21_chunk_sum) <- best_sum21
    values(raster22_chunk_sum) <- best_sum22
    # Adds the current raster to the raster list
    rasters_list21[[length(rasters_list21) + 1]] <- raster21_chunk
    rasters_list22[[length(rasters_list22) + 1]] <- raster22_chunk
    rastersum_list21[[length(rastersum_list21) + 1]] <- raster21_chunk_sum
    rastersum_list22[[length(rastersum_list22) + 1]] <- raster22_chunk_sum
    counter <- counter + 1
    cat("Batch", counter, "processed successfully (i =", i, ", j =", j, ")\n")
    rm(current_extent,raster_chunk,raster_centers_chunk,raster_centers_chunk_utm,
       dist21_chunk,dist22_chunk, min_dist21_values,min_dist22_values,raster21_chunk,raster22_chunk,
       buffers,best_sum21,best_sum22,raster21_chunk_sum,raster22_chunk_sum)
  }
}
# Merge all the raster in the raster list, generate the 0.005 degree calculation result, and output the result.
raster21_0.005 <- do.call(merge, rasters_list21)
raster22_0.005 <- do.call(merge, rasters_list22)
raster21sum_0.005 <- do.call(merge, rastersum_list21)
raster22sum_0.005 <- do.call(merge, rastersum_list22)
writeRaster(raster21_0.005, filename = paste0(outputfold,"min_distance_raster21_0.005.tif"), format = "GTiff", overwrite = TRUE)
writeRaster(raster22_0.005, filename = paste0(outputfold,"min_distance_raster22_0.005.tif"), format = "GTiff", overwrite = TRUE)
writeRaster(raster21sum_0.005, filename = paste0(outputfold,"sum_buffer_raster21_0.005.tif"), format = "GTiff", overwrite = TRUE)
writeRaster(raster22sum_0.005, filename = paste0(outputfold,"sum_buffer_raster22_0.005.tif"), format = "GTiff", overwrite = TRUE)
raster21_0.005 <- raster(paste0(outputfold, "min_distance_raster21_0.005.tif"))
raster22_0.005 <- raster(paste0(outputfold, "min_distance_raster22_0.005.tif"))
raster21sum_0.005 <- raster(paste0(outputfold, "sum_buffer_raster21_0.005.tif"))
raster22sum_0.005 <- raster(paste0(outputfold, "sum_buffer_raster22_0.005.tif"))
# Aggregate to 0.01 degree and save data
raster21_aggregated_0.01 <- aggregate(raster21_0.005, fact = res(raster_0.01)[1] / res(raster21_0.005)[1], fun = mean)
raster22_aggregated_0.01 <- aggregate(raster22_0.005, fact = res(raster_0.01)[1] / res(raster22_0.005)[1], fun = mean)
raster21sum_aggregated_0.01 <- ceiling(aggregate(raster21sum_0.005, fact = res(raster_0.01)[1] / res(raster21sum_0.005)[1], fun = mean))
raster22sum_aggregated_0.01 <- ceiling(aggregate(raster22sum_0.005, fact = res(raster_0.01)[1] / res(raster22sum_0.005)[1], fun = mean))
writeRaster(raster21_aggregated_0.01, filename = paste0(outputfold, "min_distance_raster21_0.01.tif"), format = "GTiff", overwrite = TRUE)
writeRaster(raster22_aggregated_0.01, filename = paste0(outputfold, "min_distance_raster22_0.01.tif"), format = "GTiff", overwrite = TRUE)
writeRaster(raster21sum_aggregated_0.01, filename = paste0(outputfold, "sum_buffer_raster21_0.01.tif"), format = "GTiff", overwrite = TRUE)
writeRaster(raster22sum_aggregated_0.01, filename = paste0(outputfold, "sum_buffer_raster22_0.01.tif"), format = "GTiff", overwrite = TRUE)
# Aggregate to 0.05 degree and save data
raster21_aggregated_0.05 <- aggregate(raster21_0.005, fact = res(raster_0.05)[1] / res(raster21_0.005)[1], fun = mean)
raster22_aggregated_0.05 <- aggregate(raster22_0.005, fact = res(raster_0.05)[1] / res(raster22_0.005)[1], fun = mean)
raster21sum_aggregated_0.05 <- ceiling(aggregate(raster21sum_0.005, fact = res(raster_0.05)[1] / res(raster21sum_0.005)[1], fun = mean))
raster22sum_aggregated_0.05 <- ceiling(aggregate(raster22sum_0.005, fact = res(raster_0.05)[1] / res(raster22sum_0.005)[1], fun = mean))
writeRaster(raster21_aggregated_0.05, filename = paste0(outputfold, "min_distance_raster21_0.05.tif"), format = "GTiff", overwrite = TRUE)
writeRaster(raster22_aggregated_0.05, filename = paste0(outputfold, "min_distance_raster22_0.05.tif"), format = "GTiff", overwrite = TRUE)
writeRaster(raster21sum_aggregated_0.05, filename = paste0(outputfold, "sum_buffer_raster21_0.05.tif"), format = "GTiff", overwrite = TRUE)
writeRaster(raster22sum_aggregated_0.05, filename = paste0(outputfold, "sum_buffer_raster22_0.05.tif"), format = "GTiff", overwrite = TRUE)
#-------
# End of the script

