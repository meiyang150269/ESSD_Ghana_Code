
# This script processes the Composite POIs from OSM source.
# Various OSM data for 2021 and 2022: gis_osm_pois_free_1.shp, gis_osm_pofw_free_1.shp, gis_osm_traffic_free_1.shp, gis_osm_transport_free_1.shp
# The Composite POIs includes "POI" data, Fuel_Parking from ��traffic�� data, "transport" data and "pofw" data.
# Calculate the kernel density at 0.005��, then aggregate by average at 0.01�� and 0.05��.
# Before running the code, please ensure that you have completed the following preparations:
# 1. Download and clip the OSM data, then store them in the "F:/[2024.07]Ghana/Processed Data/OSM/POIs" folder.
# 2. Install the required R libraries.
# If not already installed, you can use the following commands to install:
# install.packages("sf")
# install.packages("raster")
# install.packages("dplyr")
# install.packages("randomForest")
# install.packages("caret")
# install.packages("spatialEco")
# install.packages("MASS")

# Load necessary packages
library(randomForest)
library(sf)
library(dplyr)
library(raster)
library(spatialEco)
library(MASS)
output_path <- "F:/[2024.07]Ghana/Processed Data/OSM/POIs/"
dir.create(output_path, showWarnings = FALSE)
# Load the raster template from the Raster_Templates folder
raster_0.005 <- raster("F:/[2024.07]Ghana/Processed Data/Raster_Templates/0.005.tif")
raster_0.005[] <- 0 # Prepare for subsequent extraction of grid center points
# Define file paths
base_path <- "F:/[2024.07]Ghana/Processed Data/OSM/"
# Create output folders if they do not exist
dir.create(file.path(output_path, "2021"), showWarnings = FALSE)
dir.create(file.path(output_path, "2022"), showWarnings = FALSE)
# Load various OSM data for 2021 and 2022: gis_osm_pois_free_1.shp, gis_osm_pofw_free_1.shp, gis_osm_traffic_free_1.shp, gis_osm_transport_free_1.shp
# Extract different types of POIs data to a specified directory, 
#----"POI" data of OSM
# Define various POIs classifications
poi_classes <- list(
  # gis_osm_pois_free_1.shp
  public = c("police", "fire_station", "post_box", "post_office", "telephone", "library", "town_hall",
             "courthouse", "prison", "embassy", "community_centre", "nursing_home", "arts_centre",
             "graveyard", "market_place", "recycling", "recycling_glass", "recycling_paper",
             "recycling_clothes", "recycling_metal", "public_building"),
  # "Education" section originally belonged to "public" but is now extracted separately.
  Education = c("university","school", "clinic", "college"), 
  health = c("pharmacy", "hospital", "clinic", "doctors", "dentist", "veterinary"),
  leisure = c("theatre", "nightclub", "cinema", "park", "playground", "dog_park"),
  # "Sports" section originally belonged to "leisure" but is now extracted separately.
  Sports = c("sports_centre", "pitch", "swimming_pool", "tennis_court", "golf_course", "stadium", "ice_rink"),
  catering = c("restaurant", "fast_food", "cafe", "pub", "bar", "food_court", "biergarten"),
  # "accommodation" section includes accommodation indoor and outdoor.
  accomm_indoor = c("hotel", "motel", "bed_and_breakfast", "guesthouse", "hostel", "chalet"),
  accomm_outdoor = c("shelter", "camp_site", "alpine_hut", "caravan_site"),
  shopping = c("supermarket", "bakery", "kiosk", "mall", "department_store", "general", "convenience",
               "clothes", "florist", "chemist", "bookshop", "butcher", "shoe_shop",
               "beverages", "optician", "jeweller", "gift_shop", "sports_shop",
               "stationery", "outdoor_shop", "mobile_phone_shop","toy_shop", "newsagent", "greengrocer",
               "beauty_shop", "video_shop", "car_dealership","bicycle_shop", "doityourself", "furniture_shop",
               "computer_shop", "garden_centre", "hairdresser","car_repair", "car_rental", "car_wash", "car_sharing",
               "bicycle_rental", "travel_agent","laundry", "vending_machine", "vending_cigarette", "vending_parking"),
  money = c("bank", "atm"),
  tourism = c("tourist_info", "tourist_map", "tourist_board", "tourist_guidepost", "attraction", "museum",
              "monument", "memorial", "art", "castle", "ruins", "archaeological", "wayside_cross",
              "wayside_shrine", "battlefield", "fort", "picnic_site", "viewpoint", "zoo", "theme_park"),
  miscpoi = c("toilet", "bench", "drinking_water", "fountain", "hunting_stand", "waste_basket",
              "camera_surveillance", "emergency_phone", "fire_hydrant", "emergency_access", "tower", 
              "tower_comms", "water_tower", "tower_observation", "windmill", "lighthouse", 
              "wastewater_plant", "water_well", "water_mill", "water_works")
)
for (year in c("2021", "2022")) {
  # Read the POIs data shapefile
  POIs_data <- st_read(paste0(base_path, year, "/gis_osm_pois_free_1.shp"))
  # POIs_fclass <-unique(POIs_data$fclass) # Extract different pois levels based on the 'fclass' attribute
  # Loop through each POIs class and extract data
  for (class_name in names(poi_classes)) {
    # Filter data for the specific pois class
    class_data <- POIs_data %>% filter(fclass %in% poi_classes[[class_name]])
    # Define the output filename
    output_filename <- paste0(output_path, year, "/POIs_", class_name, "_", year, ".shp")
    # Save the extracted data to a shapefile
    st_write(class_data, output_filename, append = FALSE)
    cat(class_name, "processed successfully (Year =", year, ")\n")
    rm(class_data,output_filename)
  }
}
#----"traffic" data of OSM
# Fuel_Parking from ��traffic�� data
traffic_classes <- list(Fuel_Parking = c("fuel", "service", "parking", "parking_site", 
                                         "parking_multistorey","parking_underground", "parking_bicycle"))
for (year in c("2021", "2022")) {
  # Read the traffic data shapefile
  traffic_data <- st_read(paste0(base_path, year, "/gis_osm_traffic_free_1.shp"))
  # traffic_fclass <-unique(traffic_data$fclass) # Extract different traffic levels based on the 'fclass' attribute
  # Loop through each traffic class and extract data
  # Filter data for the specific class
  Fuel_Parking_data <- traffic_data %>% filter(fclass %in% traffic_classes[[names(traffic_classes)]])
  # Define the output filename
  output_filename <- paste0(output_path, year, "/Fuel_Parking_", year, ".shp")
  # Save the extracted data to a shapefile
  st_write(Fuel_Parking_data, output_filename, append = FALSE)
  cat("Fuel_Parking processed successfully (Year =", year, ")\n")
  rm(traffic_data,output_filename)
}
#----"transport" data of OSM
for (year in c("2021", "2022")) {
  transport_data <- st_read(paste0(base_path, year, "/gis_osm_transport_free_1.shp"))
  output_filename <- paste0(output_path, year, "/transport_", year, ".shp")
  # Save the extracted data to a shapefile
  st_write(transport_data, output_filename, append = FALSE)
  cat("transport processed successfully (Year =", year, ")\n")
  rm(transport_data,output_filename)
}
#----"pofw" data of OSM
for (year in c("2021", "2022")) {
  pofw_data <- st_read(paste0(base_path, year, "/gis_osm_pofw_free_1.shp"))
  output_filename <- paste0(output_path, year, "/pofw_", year, ".shp")
  # Save the extracted data to a shapefile
  st_write(pofw_data, output_filename, append = FALSE)
  cat("pofw processed successfully (Year =", year, ")\n")
  rm(pofw_data,output_filename)
}
# Modeling based on the 2021 population density (based on total population) and POIs to determine the optimal bandwidth for kernel density estimation.
Ghana <- st_read("F:/[2024.07]Ghana/Processed Data/Census21_district/Census21_district.shp") # pop_dis
binary_raster <- raster_0.005
values(binary_raster) <- 1
pixel_counts <- extract(binary_raster, Ghana, fun = sum, na.rm = TRUE)
Ghana$pixel_count <- pixel_counts
Ghana$pop_dens <- ifelse(is.na(Ghana$pop_dis) | pixel_counts == 0, NA, Ghana$pop_dis / pixel_counts) #  2021 population density
raster_utm <- projectRaster(raster_0.005, res = 559, crs = 32630) # When res = c(559.7,559), raster rows and columns are the same before and after projection conversion
# Using data from 2021, simulate total population density and POIs kernel density with random forest modeling under different bandwidths.
# Select the optimal bandwidth based on the modeling results.
years <- "2021"
inputdir <- paste0("F:/[2024.07]Ghana/Processed Data/OSM/POIs/",years)
filenames <- c(paste0("POIs_",names(poi_classes)),names(traffic_classes), "transport", "pofw")
bestbw <- c()
for (k in 1:length(inputdir)) {
  files <- paste0(inputdir[k],"/",filenames,"_",years[k],".shp")
  for (i in 1:length(filenames)){
    eval(parse(text = paste0(filenames[i],"<- st_read(","\"", files[i],"\")")))
    eval(parse(text = paste0(filenames[i],"_utm <- st_transform(",filenames[i], ", crs = 32630)")))
  }
  # various bandwidth for RF model
  bwth <- c(seq(500,1000,100),seq(1200,2700,300),seq(3000,8000,1000),seq(10000,20000,5000))
  oobvalue <- c()
  for(b in 1:length(bwth)){
    for (i in 1:length(filenames)){
      eval(parse(text = paste0("coords <- st_coordinates(", filenames[i],"_utm)")))
      # coords <- st_coordinates(POIs_public)
      eval(parse(text = paste0(filenames[i],".kde <- kde2d(coords[, 1], coords[, 2], n = c(nrow(raster_utm), ncol(raster_utm)),h =", bwth[b],")")))
      # POIs_public.kde <- kde2d(coords[, 1], coords[, 2], n = c(nrow(raster_utm), ncol(raster_utm)))
      rasT <- raster_utm
      values(rasT) <- NA
      eval(parse(text = paste0("values(rasT) <- matrix(", filenames[i],".kde$z, nrow = nrow(raster_utm), ncol = ncol(raster_utm))")))
      #values(rasT) <- matrix(POIs_public.kde$z, nrow = nrow(raster_utm), ncol = ncol(raster_utm))
      eval(parse(text = paste0(filenames[i],"_kde <- projectRaster(rasT, res = c(0.005, 0.005), crs = crs(raster_0.005))")))
      # POIs_public_kde <- projectRaster(rasT, res = c(0.005, 0.005), crs = crs(raster_0.005))
      eval(parse(text = paste0(filenames[i],"_kde <- resample(",filenames[i],"_kde, raster_0.005, method=\"ngb\")")))
      eval(parse(text = paste0(filenames[i],"_kde<- (",filenames[i],"_kde-minValue(",filenames[i],"_kde))/(maxValue(",filenames[i],"_kde)-minValue(",filenames[i],"_kde))")))
      # Since we need to select the optimal bandwidth through random forest modeling, normalization is performed here.
      eval(parse(text = paste0(filenames[i],"_Ghasum <- as.numeric(extract(",filenames[i],"_kde, Ghana, fun = sum, na.rm=TRUE))")))
      eval(parse(text = paste0(filenames[i],"_k <-",filenames[i],"_kde")))
      eval(parse(text = paste0(filenames[i],"_k[",filenames[i],"_k <= 0]<- 0")))
      eval(parse(text = paste0(filenames[i],"_k[",filenames[i],"_k >0 ]<- 1")))
      eval(parse(text = paste0(filenames[i],"_ksum<- as.numeric(extract(",filenames[i],"_k, Ghana, fun = sum))")))
      eval(parse(text = paste0(filenames[i],"_Gha <-",filenames[i],"_Ghasum/",filenames[i],"_ksum")))
      eval(parse(text = paste0(filenames[i],"_Gha[is.nan(",filenames[i],"_Gha)] <- 0")))
      cat(paste0("Processed: ", bwth[b],"-", filenames[i], "_Gha","\n"))
    }
    eval(parse(text = paste0("admPopdata <- data.frame(Pop = log(Ghana$pop_dens),",paste(filenames," =", filenames,"_Gha",sep = "",collapse = ","),")")))
    admPopdata <- na.omit(admPopdata)
    set.seed(123)
    RF_pop <- randomForest(Pop ~ ., data=admPopdata, importance=TRUE)
    oobvalue[b] <- RF_pop$mse[length(RF_pop$mse)]
    rm(RF_pop,admPopdata)
  }
  write.csv(oobvalue , file=paste0("F:/[2024.07]Ghana/Processed Data/OSM/POIs/oobvaule_",years[k],".csv"), row.names = TRUE)
  # write.csv(oobvaule1 , file=paste0("F:/[2024.07]Ghana/Processed Data/OSM/POIs/oobvaule1_",years[k],".csv"), row.names = TRUE)
  bestbw[k] <- bwth[which.min(oobvalue)]
  cat(paste0("Processed: ", bwth[b], "\n"))
}
# After selecting the optimal bandwidth, re-run the kernel density calculation to generate different results for the POIs kernel density.
years <- c("2021","2022")
inputdir <- paste0("F:/[2024.07]Ghana/Processed Data/OSM/POIs/",years)
filenames <- c(paste0("POIs_",names(poi_classes)),names(traffic_classes), "transport", "pofw")
for (k in 1:length(inputdir)) {
  files <- paste0(inputdir[k],"/",filenames,"_",years[k],".shp")
  for (i in 1:length(filenames)){
    eval(parse(text = paste0(filenames[i],"<- st_read(","\"", files[i],"\")")))
    eval(parse(text = paste0(filenames[i],"_utm <- st_transform(",filenames[i], ", crs = 4326)")))
  }
  # various bandwidth for RF model
  bwth <- 5000
  for (i in 1:length(filenames)){
    eval(parse(text = paste0("coords <- st_coordinates(", filenames[i],"_utm)")))
    # coords <- st_coordinates(POIs_public)
    eval(parse(text = paste0(filenames[i],".kde <- kde2d(coords[, 1], coords[, 2], n = c(nrow(raster_utm), ncol(raster_utm)))")))
    # POIs_public.kde <- kde2d(coords[, 1], coords[, 2], n = c(nrow(raster_utm), ncol(raster_utm)))
    rasT <- raster_utm
    values(rasT) <- NA
    eval(parse(text = paste0("values(rasT) <- matrix(", filenames[i],".kde$z, nrow = nrow(raster_utm), ncol = ncol(raster_utm))")))
    #values(rasT) <- matrix(POIs_public.kde$z, nrow = nrow(raster_utm), ncol = ncol(raster_utm))
    eval(parse(text = paste0(filenames[i],"_kde <- projectRaster(rasT, res = c(0.005, 0.005), crs = crs(raster_0.005))")))
    # POIs_public_kde <- projectRaster(rasT, res = c(0.005, 0.005), crs = crs(raster_0.005))
    eval(parse(text = paste0(filenames[i],"_kde <- resample(",filenames[i],"_kde, raster_0.005, method=\"ngb\")")))
    eval(parse(text = paste0(filenames[i],"_kde_0.01 <- aggregate(",filenames[i],"_kde, fact = 2, fun = mean)")))
    eval(parse(text = paste0(filenames[i],"_kde_0.05 <- aggregate(",filenames[i],"_kde, fact = 10, fun = mean)")))
    eval(parse(text = paste0("writeRaster(",filenames[i],"_kde, filename = \"F:/[2024.07]Ghana/Processed Data/OSM/POIs/",
                             filenames[i],"KD_0.005.tif\", format=\"GTiff\",overwrite=TRUE)")))
    eval(parse(text = paste0("writeRaster(",filenames[i],"_kde_0.01, filename = \"F:/[2024.07]Ghana/Processed Data/OSM/POIs/",
                             filenames[i],"KD_0.01.tif\", format=\"GTiff\",overwrite=TRUE)")))
    eval(parse(text = paste0("writeRaster(",filenames[i],"_kde_0.05, filename = \"F:/[2024.07]Ghana/Processed Data/OSM/POIs/",
                             filenames[i],"KD_0.05.tif\", format=\"GTiff\",overwrite=TRUE)")))
    cat(paste0("Processed: Kernel density_", filenames[i],"_",years[k], "\n"))
    rm(rasT)
  }
  cat(paste0("Processed: Kernel density_", years[k], "\n"))
  
}
#-------
# End of the script
  
