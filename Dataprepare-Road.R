

# This script processes the Road data from OSM source.
# OSM roads data for 2021 and 2022: "road" data (gis_osm_roads_free_1.shp) and "railways" data (gis_osm_railways_free_1.shp). 
# The "road" data contains 28 specific road types and 6 levels. Extract the road data for the six levels for the years 2021 and 2022.
# calculate the nearest distance for three levels of "Highway_Links","Unknown","Major_Roads".
# calculate the line density for six levels of road data.
# The "railways" data has a small volume, so directly calculate the nearest distance at 0.005��, then aggregate by average at 0.01�� and 0.05��.
# Before running the code, please ensure that you have completed the following preparations:
# 1. Download and clip the OSM data, then store them in the "F:/[2024.07]Ghana/Processed Data/OSM/Road" folder.
# 2. Install the required R libraries.
# If not already installed, you can use the following commands to install:
# install.packages("sf")
# install.packages("raster")
# install.packages("dplyr")

# Load necessary packages
library(sf)
library(dplyr)
library(raster)
output_path <- "F:/[2024.07]Ghana/Processed Data/OSM/Road/"
# Load the raster template from the Raster_Templates folder
raster_0.005 <- raster("F:/[2024.07]Ghana/Processed Data/Raster_Templates/0.005.tif")
raster_0.005[] <- 0 # Prepare for subsequent extraction of grid center points
# Source the min_distance function from the R script
source("F:/[2024.07]Ghana/Processed Data/Script/updated/Dataprepare_calculate_min_distance.R")
# source("F:/[2024.07]Ghana/Processed Data/Script/Dataprepare_calculate_min_distance.R")
#----"road" data of OSM
# Define file paths
base_path <- "F:/[2024.07]Ghana/Processed Data/OSM/"
# Create output folders if they do not exist
dir.create(file.path(output_path, "2021"), showWarnings = FALSE)
dir.create(file.path(output_path, "2022"), showWarnings = FALSE)
# Read road data for 2021 and 2022
road_fclass <- c("unclassified", "residential", "tertiary", "tertiary_link", 
                 "secondary", "primary", "service", "trunk", 
                 "motorway_link", "primary_link", "secondary_link", 
                 "trunk_link", "footway", "path", "track", "motorway", 
                 "track_grade3", "track_grade4", "steps", "pedestrian", 
                 "bridleway", "cycleway", "track_grade2", "track_grade5", 
                 "track_grade1", "living_street", "unknown")
# Define road classifications
road_classes <- list(
  Major_Roads = c("motorway", "trunk", "primary", "secondary", "tertiary"),
  Minor_Roads = c("unclassified", "residential", "living_street", "pedestrian", "busway"),
  Highway_Links = c("motorway_link", "trunk_link", "primary_link", "secondary_link", "tertiary_link"),
  Very_Small_Roads = c("service", "track", "track_grade1", "track_grade2", "track_grade3", "track_grade4", "track_grade5"),
  Paths_Unsuitable_for_Cars = c("bridleway", "cycleway", "footway", "path", "steps"),
  Unknown = c("unknown")
)
for (year in c("2021", "2022")) {
  # Read the road data shapefile
  road_data <- st_read(paste0(base_path, year, "/gis_osm_roads_free_1.shp"))
  # road_fclass <-unique(road_data$fclass) # Extract different road levels based on the 'fclass' attribute
  # Loop through each road class and extract data
  for (class_name in names(road_classes)) {
    # Filter data for the specific road class
    class_data <- road_data %>% filter(fclass %in% road_classes[[class_name]])
    # Define the output filename
    output_filename <- paste0(output_path, year, "/Road_", class_name, "_", year, ".shp")
    # Save the extracted data to a shapefile
    st_write(class_data, output_filename, append = FALSE)
    cat(class_name, "processed successfully (Year =", year, ")\n")
    rm(class_data,output_filename)
  }
}
# Batch read the stored files and Convert to projection coordinates
# distance calculation 
for (year in c("2021", "2022")) {
  for (class_name in names(road_classes)) {
    eval(parse(text = paste0(class_name, " <- st_read('", output_path, year, "/Road_", class_name, "_", year, ".shp')")))
    eval(parse(text = paste0(class_name, "_utm <- st_transform(", class_name, ", crs = 32630)")))
    # Call the function to calculate minimum distance
    min_distance<- Dataprepare_calculate_min_distance(
      chunk_size = 0.005 * 400,  # Set the chunk size
      rasterT = raster_0.005,     # Assume raster_0.005 is defined elsewhere
      shape_utm = get(paste0(class_name, "_utm")),  # Get the transformed shape object
      aggregate_degrees = c(0.01, 0.05), 
      output_folder = "F:/[2024.07]Ghana/Processed Data/OSM/Road/",
      output_filename = paste0(class_name, "_min_distance_", year)
    )
    cat(paste0("Processed: ", class_name, "\n"))
  }
}
#----"railways" data of OSM
# Define file paths
rail_path <- "F:/[2024.07]Ghana/Processed Data/OSM/"
# distance calculation 
for (year in c("2021", "2022")) {
  eval(parse(text = paste0("railways <- st_read('", rail_path, year, "/gis_osm_railways_free_1.shp')")))
  eval(parse(text = "railways_utm <- st_transform(railways, crs = 32630)"))
  # Call the function to calculate minimum distance
  min_distance<- Dataprepare_calculate_min_distance(
    chunk_size = 0.005 * 400,  # Set the chunk size
    rasterT = raster_0.005,     # Assume raster_0.005 is defined elsewhere
    shape_utm = railways_utm,
    aggregate_degrees = c(0.01, 0.05), 
    output_folder = "F:/[2024.07]Ghana/Processed Data/OSM/Road/",
    output_filename = paste0("Railways_min_distance_", year)
    )
  cat(paste0("Processed: Railways_min_distance_", year, "\n"))
  print(Sys.time())
}
#--
# Although this code has already implemented distance calculations for other road types, the efficiency of R language is too low for distance calculations with large data volumes.
# The line feature data for "Minor_Roads," "Very_Small_Roads," and "Paths_Unsuitable_for_Cars" is too large, and the road data is relatively dense in these areas (the latter two road types are particularly dense in regions with road data).
# Therefore, the spatial distribution of these all road types will be represented using line density. 
# Define file paths
base_path <- "F:/[2024.07]Ghana/Processed Data/OSM/"
output_folder <- "F:/[2024.07]Ghana/Processed Data/OSM/Road/Road_line_density/"
raster_0.005 <- raster("F:/[2024.07]Ghana/Processed Data/Raster_Templates/0.005.tif")
raster_0.005[] <- 0 # Prepare for subsequent extraction of grid center points
raster_utm <- projectRaster(raster_0.005, res = c(559.7,559), crs = 32630) # When res = c(559.7,559), raster rows and columns are the same before and after projection conversion
raster_utm[] <- 1:ncell(raster_utm) # raster pixel area with units: [m^2]
raster_polygons_sf <- st_read("F:/[2024.07]Ghana/Processed Data/Raster_Templates/raster_polygons.shp")
# Load the raster template of 0.01 degree from the Raster_Templates folder
raster_0.01 <- aggregate(raster_0.005,fact=2)
raster_0.05 <- aggregate(raster_0.005,fact=10)
# Load the line data from the shapefile
linenames <- paste0("Road_", c("Minor_Roads","Very_Small_Roads","Paths_Unsuitable_for_Cars", "Highway_Links","Unknown","Major_Roads"))
# year <- "2021"
for (year in c("2021","2022")){
  for (linename in linenames){
    # linename <- linenames[1]
    line_data_path <- paste0(output_folder,year,"/", linename,"_",year,".shp")
    lines_data <- st_read(line_data_path)
    lines_utm <- st_transform(lines_data, crs = 32630)
    cropped_lines_intersected <- st_intersection(lines_utm, raster_polygons_sf) # This line (st_intersection) may take a long time to run 
    # Merging may take a long time to run
    merged_lines_intersected <- cropped_lines_intersected %>%
      group_by(X0_005) %>%  # Group by the X0.005 attribute
      summarise(geometry = st_union(geometry), .groups = 'drop')  # Merge geometries
    # Calculate the line length of the merged lines
    merged_lines_intersected$intersection_length <- st_length(merged_lines_intersected)
    # Create a data frame to hold cell IDs and corresponding intersection lines
    line_data <- data.frame(
      cell_id = merged_lines_intersected$X0_005, # Extract cell IDs from merged polygons
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
    writeRaster(LineDens_0.005, filename = paste0(output_folder, linename,"_",year,"_lineD_0.005.tif"), format = "GTiff", overwrite = TRUE)
    # spplot(LineDens_0.005)
    # LineDens_0.005 <- raster(paste0(output_folder, "HydroRIVERS_linear_density_0.005.tif"))
    # Aggregate to 0.01 and 0.05 degrees and save data
    LineDens_0.01 <- aggregate(LineDens_0.005, fact = res(raster_0.01)[1] / res(LineDens_0.005)[1], fun = mean)
    LineDens_0.05 <- aggregate(LineDens_0.005, fact = res(raster_0.05)[1] / res(LineDens_0.005)[1], fun = mean)
    writeRaster(LineDens_0.01, filename = paste0(output_folder, linename,"_",year,"_lineD_0.01.tif"), format = "GTiff", overwrite = TRUE)
    writeRaster(LineDens_0.05, filename = paste0(output_folder, linename,"_",year,"_lineD_0.05.tif"), format = "GTiff", overwrite = TRUE)
    # spplot(LineDens_0.01)
    cat(paste0("Processed: ", linename, "_", year,"\n"))
  }
}
#-------
# End of the script

