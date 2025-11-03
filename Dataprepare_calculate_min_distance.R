# Function to calculate the minimum distance from raster grid centers to shape file data, i.e., points, polygons and lines.
# Save the results as raster files at specified resolutions.
# Example of calling the function
# Dataprepare_calculate_min_distance(
#  chunk_size = 0.005 * 400, # Adjust as needed
#  rasterT = raster_0.005, 
#  shape_utm = Unknown_utm, 
#  aggregate_degrees = c(0.01, 0.05), 
#  output_folder = "F:/[2024.07]Ghana/Processed Data/OSM/Road/",
#  output_filename = paste0(class_name,"_min_distance_",year,".tif")
#  )

# Load necessary packages
library(sf)
library(dplyr)
library(raster)
Dataprepare_calculate_min_distance <- function(chunk_size, rasterT, shape_utm, aggregate_degrees, output_folder, output_filename) {
  # Initialize an empty list to store generated raster objects
  raster_output_list <- list()
   # Get longitude and latitude ranges
  lon_range <- extent(rasterT)[1:2]
  lat_range <- extent(rasterT)[3:4]
  # Calculate the number of steps based on the chunk size
  lon_steps <- ceiling((lon_range[2] - lon_range[1]) / chunk_size)
  lat_steps <- ceiling((lat_range[2] - lat_range[1]) / chunk_size)
  # Generate longitude and latitude sequence, ensuring the last value is the maximum
  lon_seq <- seq(from = lon_range[1], length.out = lon_steps, by = chunk_size)
  lon_seq <- c(lon_seq, lon_range[2])  # Make sure the last value is the maximum
  lat_seq <- seq(from = lat_range[1], length.out = lat_steps, by = chunk_size)
  lat_seq <- c(lat_seq, lat_range[2])  # Make sure the last value is the maximum
  # Prepare raster for extraction of grid center points
  rasterT[] <- 0
  # Create an empty list to store results
  rasters_list <- list()
  # Initialize a counter for progress tracking
  counter <- 0
  # Loop through each chunk to process sub-regions
  for (i in 1:(length(lon_seq)-1)) {
    for (j in 1:(length(lat_seq)-1)) {
      # Create an extent object representing the current chunk
      current_extent <- extent(c(lon_seq[i], lon_seq[i + 1], lat_seq[j], lat_seq[j + 1]))
      # Crop the raster to the current extent
      raster_chunk <- crop(rasterT, current_extent)
      # Convert raster to points and define CRS as WGS84
      raster_centers_chunk <- st_as_sf(as.data.frame(rasterToPoints(raster_chunk)), coords = c("x", "y"))
      st_crs(raster_centers_chunk) <- 4326 
      # Convert the grid center points to UTM zone 30N
      raster_centers_chunk_utm <- st_transform(raster_centers_chunk, crs = 32630)
      # Calculate the distance from the center of the grid to mines points data
      dist_chunk <- st_distance(raster_centers_chunk_utm, shape_utm)
      # Get minimum distance for each point
      min_dist_values <- apply(dist_chunk, 1, min)
      # Assign minimum distance values to raster cells
      raster_chunk[] <- min_dist_values
      rasters_list[[length(rasters_list) + 1]] <- raster_chunk
      # Increment counter and print progress
      counter <- counter + 1
      cat("Chunk", counter, "processed successfully (i =", i, ", j =", j, ")\n")
      # Cleanup temporary objects to free memory
      rm(current_extent, raster_chunk, raster_centers_chunk, raster_centers_chunk_utm, dist_chunk, min_dist_values)
    }
  }
  # Merge all processed rasters
  merged_raster <- do.call(merge, rasters_list)
  # Save the 0.005-degree raster result
  output_path <- file.path(output_folder, paste0(output_filename, "_",res(merged_raster)[1],".tif"))
  writeRaster(merged_raster, filename = output_path, format = "GTiff", overwrite = TRUE)
  raster_output_list[[paste0("raster_",res(raster_0.005)[1])]] <- merged_raster
  # Aggregate to different degrees and save results
  for (degree in aggregate_degrees) {
    aggregated_raster <- aggregate(merged_raster, fact = degree / res(merged_raster)[1], fun = mean)
    agg_output_path <- file.path(output_folder, paste0(output_filename, "_", degree, ".tif"))
    writeRaster(aggregated_raster, filename = agg_output_path, format = "GTiff", overwrite = TRUE)
    raster_output_list[[paste0("raster_",degree)]] <- merged_raster
  }
  cat("All rasters processed and saved successfully.\n")
  return(raster_output_list)
}
#-------
# End of the script

