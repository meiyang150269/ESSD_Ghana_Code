
# This script prepares model input data for random forest modeling.
# If a grid contains at least one nonzero value in each of the five covariates, the grid is labeled as an inhabited area.
# i.e., night light (ntl), building height (bltH), percentage of building (blt_pro), settlement (setl), slums (slum)
# During the downscaling process of the census data, population counts were allocated only to grids located within the inhabited areas.
# Before running, please ensure that you have completed the following preparations:
# 1. Download the processed data from cloud disk, then decompressing them in the "F:/[2024.07]Ghana/Processed Data-202411" folder.
# 2. Snow_and_ice and Moss_and_lichen in land cover have been deleted, they all have a value of 0.
# The data processing is done within the expanded boundaries. Here, we recreate the raster template based on the extent of Ghana_Expanded_Boundaries.shp to reduce the data size.
# All auxiliary variables are normalized
# The values of multi-source auxiliary variables may have different ranges, leading to significant differences in their magnitudes.
# Although Random Forest is generally less sensitive to the scale of variables due to its tree-based splitting mechanism.
# Large differences in magnitudes among variables could still affect the model's performance. 
# In such cases, some features might be either underrepresented or overrepresented in terms of their contribution to the model.
# install.packages("sf")
# install.packages("raster")

# Load necessary packages
library(sf)
library(raster)
filefold <- "F:/[2024.07]Ghana/Processed Data-202411/"
border_path <- "F:/[2024.07]Ghana/Processed Data-202411/Border/Ghana_Expanded_Boundaries.shp"
border <- st_read(border_path)
Tras_0.005 <- raster(resolution = 0.005, crs = "+proj=longlat +datum=WGS84", ext = extent(-4.326, 1.424, 4.6287, 12.0287))
Tras_0.01 <- aggregate(Tras_0.005, fact = 2)
Tras_0.05 <- aggregate(Tras_0.005, fact = 10)
#-----------For 2021
# Load processed data at three resolutions of 0.005, 0.01 and 0.05
# 0.005 vs 0.01, the latter adds three variables of NTL, i.e., NLVI, NLVS,VCNLI
# 0.01 vs 0.05, the latter adds four variables, including precipitation (i.e., rain) and SPEI (i.e., spei01, spei03, spei06)
# for 0.005 degree data
# DEM
dem_files <- list.files(paste0(filefold,"DEM(GLO-30)/0.005"), pattern = "*.tif", full.names = TRUE, recursive = TRUE)
names(dem_files) <- sub("_.*", "", tools::file_path_sans_ext(basename(dem_files)))
dem_rasters <- lapply(dem_files, raster)
# Vegetation
veg_files <- paste0(filefold,c("NDVI","EVI","NIRv"),"/0.005/",c("NDVI","EVI","NIRv"),"Median_2021_0.005.tif")
names(veg_files) <- c("NDVI","EVI","NIRv")
veg_rasters <- lapply(veg_files, raster)    
# Landcover
lc_files <- list.files(paste0(filefold,"Landcover/LC_21_0.005"), pattern = "*.tif", full.names = TRUE, recursive = TRUE)
names(lc_files) <- sub("_.*", "", tools::file_path_sans_ext(basename(lc_files)))
lc_rasters <- lapply(lc_files, raster)
# LST
lst_files <- paste0(filefold,"LST/0.005/","LSTmedian_2021_0.005.tif")
names(lst_files) <- "lst"
lst_rasters <- lapply(lst_files, raster)
# NDWI
ndwi_files <- paste0(filefold,"Water_indices/NDWI_21_0.005/","NDWI_2021_median_0.005.tif")
names(ndwi_files) <- "ndwi"
ndwi_rasters <- lapply(ndwi_files, raster)
# NPP
npp_files <- paste0(filefold,"NPP/0.005/","NPP_2021_0.005.tif")
names(npp_files) <- "npp"
npp_rasters <- lapply(npp_files, raster)
# NTL
ntl_files <- paste0(filefold,"NTL/",c("NTL_2021_"),"0.005.tif")
names(ntl_files) <- "ntl"
ntl_rasters <- lapply(ntl_files, raster)
# Precipitation: only at 0.05 degree
# Settlement
setl_files <- paste0(filefold,"Settlement/proportion_0.005.tif")
names(setl_files) <- "setl"
setl_rasters <- lapply(setl_files, raster)
# Conflicts
conf_files <- paste0(filefold,"Conflicts/",c("min_distance_raster21","sum_buffer_raster21"),"_0.005.tif")
names(conf_files) <- c("conf_dis","conf_buf")
conf_rasters <- lapply(conf_files, raster)
# Disaster, SPEI_ median only at 0.05 degree
disa_files <- list.files(paste0(filefold,"Disaster/"), pattern = "_0.005\\.tif$", full.names = TRUE)
names(disa_files) <- sub("_.*", "", tools::file_path_sans_ext(basename(disa_files)))
disa_rasters <- lapply(disa_files, raster)
# Gold mines
mine_files <- list.files(paste0(filefold,"Mines/"), pattern = "_0.005\\.tif$", full.names = TRUE)
names(mine_files) <- c("mine_dis1","mine_dis2") # mine_dis1 for point mines, mine_dis2 for polygon mines 
mine_rasters <- lapply(mine_files, raster)
# River
river_files <- c(paste0(filefold,"River/",c("HydroRIVERS_line_density","LND","LUP"),"_0.005.tif"),
                 paste0(filefold,"River/",c("Lakes_reservoir","Waterways_Canal","Waterways_Drain","Waterways_River","Waterways_Stream"),"_2021_min_distance_0.005.tif"))
names(river_files) <- c("river_LD","LND","LUP","lake","canal","drain","river","stream") 
river_rasters <- lapply(river_files, raster)
# OSM
osm_files <- c(paste0(filefold,"OSM/Road/",c("Railways","Major_Roads","Highway_Links"),"_min_distance_2021_0.005.tif"),
               list.files(paste0(filefold,"OSM/Road/Road_line_density/"), pattern = "2021_lineD_0.005\\.tif$", full.names = TRUE),
               list.files(paste0(filefold,"OSM/POIs/bandwidth-3000/"), pattern = "2021_KD_0.005\\.tif$", full.names = TRUE))
names(osm_files) <- sub("_2021.*", "", tools::file_path_sans_ext(basename(osm_files)))
osm_rasters <- lapply(osm_files, raster)
# Slums
slum_files <- paste0(filefold,"Slums/","slum_proportion_2022","_0.005.tif")
names(slum_files) <- c("slum") 
slum_rasters <- lapply(slum_files, raster)
# Building_height
blt_files <- c("F:/[2024.07]Ghana/Processed Data-202411/Building_height/2021/0.005/Building_Height_2021_0.005_Median.tif",
               "F:/[2024.07]Ghana/Processed Data-202411/Building_height/2021/0.005/Building_Proportion_0.005_2021.tif")
names(blt_files) <- c("bltH","blt_pro") 
blt_rasters <- lapply(blt_files, raster)
# raster and variable names of 0.005 degree
org_rasters_0.005 <- c(dem_rasters, veg_rasters,lc_rasters,lst_rasters,ndwi_rasters,npp_rasters,ntl_rasters,setl_rasters,
                       conf_rasters,disa_rasters,mine_rasters,river_rasters,osm_rasters,slum_rasters,blt_rasters)
rasters_0.005 <- lapply(org_rasters_0.005, function(raster_layer) {
  mask(resample(crop(raster_layer, extent(Tras_0.005)), Tras_0.005, method = "ngb"), border)
})
varnames_0.005 <- c(names(dem_files),names(veg_files),names(lc_files),names(lst_files),names(ndwi_files),names(npp_files),names(ntl_files),
                    names(setl_files),names(conf_files),names(disa_files),names(mine_files),names(river_files),names(osm_files),names(slum_files),
                    names(blt_files))
# length(rasters_0.005)
# length(varnames_0.005)
# for 0.01 degree data
# DEM
dem_files <- list.files(paste0(filefold,"DEM(GLO-30)/0.01"), pattern = "*.tif", full.names = TRUE, recursive = TRUE)
names(dem_files) <- sub("_.*", "", tools::file_path_sans_ext(basename(dem_files)))
dem_rasters <- lapply(dem_files, raster)
# Vegetation
veg_files <- paste0(filefold,c("NDVI","EVI","NIRv"),"/0.01/",c("NDVI","EVI","NIRv"),"Median_2021_0.01.tif")
names(veg_files) <- c("NDVI","EVI","NIRv")
veg_rasters <- lapply(veg_files, raster)    
# Landcover
lc_files <- list.files(paste0(filefold,"Landcover/LC_21_0.01"), pattern = "*.tif", full.names = TRUE, recursive = TRUE)
names(lc_files) <- sub("_.*", "", tools::file_path_sans_ext(basename(lc_files)))
lc_rasters <- lapply(lc_files, raster)
# LST
lst_files <- paste0(filefold,"LST/0.01/","LSTmedian_2021_0.01.tif")
names(lst_files) <- "lst"
lst_rasters <- lapply(lst_files, raster)
# NDWI
ndwi_files <- paste0(filefold,"Water_indices/NDWI_21_0.01/","NDWI_2021_median_0.01.tif")
names(ndwi_files) <- "ndwi"
ndwi_rasters <- lapply(ndwi_files, raster)
# NPP
npp_files <- paste0(filefold,"NPP/0.01/","NPP_2021_0.01.tif")
names(npp_files) <- "npp"
npp_rasters <- lapply(npp_files, raster)
# NTL
ntl_files <- list.files(paste0(filefold,"NTL/"), pattern = "2021_0.01\\.tif$", full.names = TRUE, recursive = TRUE)
names(ntl_files) <- c("ntl","NLVI","NLVS","VCNLI")
ntl_rasters <- lapply(ntl_files, raster)
# Precipitation: only at 0.05 degree
# Settlement
setl_files <- paste0(filefold,"Settlement/proportion_0.01.tif")
names(setl_files) <- "setl"
setl_rasters <- lapply(setl_files, raster)
# Conflicts
conf_files <- paste0(filefold,"Conflicts/",c("min_distance_raster21","sum_buffer_raster21"),"_0.01.tif")
names(conf_files) <- c("conf_dis","conf_buf")
conf_rasters <- lapply(conf_files, raster)
# Disaster, SPEI_ median only at 0.05 degree
disa_files <- list.files(paste0(filefold,"Disaster/"), pattern = "_0.01\\.tif$", full.names = TRUE)
names(disa_files) <- sub("_.*", "", tools::file_path_sans_ext(basename(disa_files)))
disa_rasters <- lapply(disa_files, raster)
# Gold mines
mine_files <- list.files(paste0(filefold,"Mines/"), pattern = "_0.01\\.tif$", full.names = TRUE)
names(mine_files) <- c("mine_dis1","mine_dis2") # mine_dis1 for point mines, mine_dis2 for polygon mines 
mine_rasters <- lapply(mine_files, raster)
# River
river_files <- c(paste0(filefold,"River/",c("HydroRIVERS_line_density","LND","LUP"),"_0.01.tif"),
                 paste0(filefold,"River/",c("Lakes_reservoir","Waterways_Canal","Waterways_Drain","Waterways_River","Waterways_Stream"),"_2021_min_distance_0.01.tif"))
names(river_files) <- c("river_LD","LND","LUP","lake","canal","drain","river","stream") 
river_rasters <- lapply(river_files, raster)
# OSM
osm_files <- c(paste0(filefold,"OSM/Road/",c("Railways","Major_Roads","Highway_Links"),"_min_distance_2021_0.01.tif"),
               list.files(paste0(filefold,"OSM/Road/Road_line_density/"), pattern = "2021_lineD_0.01\\.tif$", full.names = TRUE),
               list.files(paste0(filefold,"OSM/POIs/bandwidth-3000/"), pattern = "2021_KD_0.01\\.tif$", full.names = TRUE))
names(osm_files) <- sub("_2021.*", "", tools::file_path_sans_ext(basename(osm_files)))
osm_rasters <- lapply(osm_files, raster)
# Slums
slum_files <- paste0(filefold,"Slums/","slum_proportion_2022","_0.01.tif")
names(slum_files) <- c("slum") 
slum_rasters <- lapply(slum_files, raster)
# Building_height
blt_files <- c("F:/[2024.07]Ghana/Processed Data-202411/Building_height/2021/0.01/Building_Height_2021_0.01_Median.tif",
               "F:/[2024.07]Ghana/Processed Data-202411/Building_height/2021/0.01/Building_Proportion_0.01_2021.tif")
names(blt_files) <- c("bltH","blt_pro") 
blt_rasters <- lapply(blt_files, raster)
# raster and variable names of 0.005 degree
org_rasters_0.01 <- c(dem_rasters, veg_rasters,lc_rasters,lst_rasters,ndwi_rasters,npp_rasters,ntl_rasters,setl_rasters,
                      conf_rasters,disa_rasters,mine_rasters,river_rasters,osm_rasters,slum_rasters,blt_rasters)
rasters_0.01 <- lapply(org_rasters_0.01, function(raster_layer) {
  mask(resample(crop(raster_layer, extent(Tras_0.01)), Tras_0.01, method = "ngb"), border)
})
varnames_0.01 <- c(names(dem_files),names(veg_files),names(lc_files),names(lst_files),names(ndwi_files),names(npp_files),names(ntl_files),
                   names(setl_files),names(conf_files),names(disa_files),names(mine_files),names(river_files),names(osm_files),names(slum_files),
                   names(blt_files))
# length(rasters_0.01)
# length(varnames_0.01)
# for 0.05 degree data
# DEM
dem_files <- list.files(paste0(filefold,"DEM(GLO-30)/0.05"), pattern = "*.tif", full.names = TRUE, recursive = TRUE)
names(dem_files) <- sub("_.*", "", tools::file_path_sans_ext(basename(dem_files)))
dem_rasters <- lapply(dem_files, raster)
# Vegetation
veg_files <- paste0(filefold,c("NDVI","EVI","NIRv"),"/0.05/",c("NDVI","EVI","NIRv"),"Median_2021_0.05.tif")
names(veg_files) <- c("NDVI","EVI","NIRv")
veg_rasters <- lapply(veg_files, raster)    
# Landcover
lc_files <- list.files(paste0(filefold,"Landcover/LC_21_0.05"), pattern = "*.tif", full.names = TRUE, recursive = TRUE)
names(lc_files) <- sub("_.*", "", tools::file_path_sans_ext(basename(lc_files)))
lc_rasters <- lapply(lc_files, raster)
# LST
lst_files <- paste0(filefold,"LST/0.05/","LSTmedian_2021_0.05.tif")
names(lst_files) <- "lst"
lst_rasters <- lapply(lst_files, raster)
# NDWI
ndwi_files <- paste0(filefold,"Water_indices/NDWI_21_0.05/","NDWI_2021_median_0.05.tif")
names(ndwi_files) <- "ndwi"
ndwi_rasters <- lapply(ndwi_files, raster)
# NPP
npp_files <- paste0(filefold,"NPP/0.05/","NPP_2021_0.05.tif")
names(npp_files) <- "npp"
npp_rasters <- lapply(npp_files, raster)
# NTL
ntl_files <- list.files(paste0(filefold,"NTL/"), pattern = "2021_0.05\\.tif$", full.names = TRUE, recursive = TRUE)
names(ntl_files) <- c("ntl","NLVI","NLVS","VCNLI")
ntl_rasters <- lapply(ntl_files, raster)
# Precipitation: only at 0.05 degree
rain_files <- paste0(filefold,"Precipitation/precipitation_21_0.05/","mean_precipitation_2021_0.05.tif")
names(rain_files) <- "rain"
rain_rasters <- lapply(rain_files, raster)
# Settlement
setl_files <- paste0(filefold,"Settlement/proportion_0.05.tif")
names(setl_files) <- "setl"
setl_rasters <- lapply(setl_files, raster)
# Conflicts
conf_files <- paste0(filefold,"Conflicts/",c("min_distance_raster21","sum_buffer_raster21"),"_0.05.tif")
names(conf_files) <- c("conf_dis","conf_buf")
conf_rasters <- lapply(conf_files, raster)
# Disaster, SPEI_ median only at 0.05 degree
disa_files <- c(list.files(paste0(filefold,"Disaster/"), pattern = "_0.05\\.tif$", full.names = TRUE),
                paste0(filefold,"SPEI/",c("SPEI01","SPEI03","SPEI06"),"_2021_mean_0.05.tif"))
names(disa_files) <- sub("_.*", "", tools::file_path_sans_ext(basename(disa_files)))
disa_rasters <- lapply(disa_files, raster)
# Gold mines
mine_files <- list.files(paste0(filefold,"Mines/"), pattern = "_0.05\\.tif$", full.names = TRUE)
names(mine_files) <- c("mine_dis1","mine_dis2") # mine_dis1 for point mines, mine_dis2 for polygon mines 
mine_rasters <- lapply(mine_files, raster)
# River
river_files <- c(paste0(filefold,"River/",c("HydroRIVERS_line_density","LND","LUP"),"_0.05.tif"),
                 paste0(filefold,"River/",c("Lakes_reservoir","Waterways_Canal","Waterways_Drain","Waterways_River","Waterways_Stream"),"_2021_min_distance_0.05.tif"))
names(river_files) <- c("river_LD","LND","LUP","lake","canal","drain","river","stream") 
river_rasters <- lapply(river_files, raster)
# OSM
osm_files <- c(paste0(filefold,"OSM/Road/",c("Railways","Major_Roads","Highway_Links"),"_min_distance_2021_0.05.tif"),
               list.files(paste0(filefold,"OSM/Road/Road_line_density/"), pattern = "2021_lineD_0.05\\.tif$", full.names = TRUE),
               list.files(paste0(filefold,"OSM/POIs/bandwidth-3000/"), pattern = "2021_KD_0.05\\.tif$", full.names = TRUE))
names(osm_files) <- sub("_2021.*", "", tools::file_path_sans_ext(basename(osm_files)))
osm_rasters <- lapply(osm_files, raster)
# Slums
slum_files <- paste0(filefold,"Slums/","slum_proportion_2022","_0.05.tif")
names(slum_files) <- c("slum") 
slum_rasters <- lapply(slum_files, raster)
# Building_height
blt_files <- c("F:/[2024.07]Ghana/Processed Data-202411/Building_height/2021/0.05/Building_Height_2021_0.05_Median.tif",
               "F:/[2024.07]Ghana/Processed Data-202411/Building_height/2021/0.05/Building_Proportion_0.05_2021.tif")
names(blt_files) <- c("bltH","blt_pro") 
blt_rasters <- lapply(blt_files, raster)
# raster and variable names of 0.005 degree
org_rasters_0.05 <- c(dem_rasters, veg_rasters,lc_rasters,lst_rasters,ndwi_rasters,npp_rasters,ntl_rasters,rain_rasters,setl_rasters,
                      conf_rasters,disa_rasters,mine_rasters,river_rasters,osm_rasters,slum_rasters,blt_rasters)
rasters_0.05 <- lapply(org_rasters_0.05, function(raster_layer) {
  mask(resample(crop(raster_layer, extent(Tras_0.05)), Tras_0.05, method = "ngb"), border)
})


varnames_0.05 <- c(names(dem_files),names(veg_files),names(lc_files),names(lst_files),names(ndwi_files),names(npp_files),names(ntl_files),names(rain_files),
                   names(setl_files),names(conf_files),names(disa_files),names(mine_files),names(river_files),names(osm_files),names(slum_files),
                   names(blt_files))
# length(rasters_0.05)
# length(varnames_0.05)
# Normalization of all auxiliary variables
min_max_values <- list() # Create an empty list to store the minimum and maximum values for each raster.
get_min_max <- function(rasters) {
  min_max <- lapply(rasters, function(raster_layer) {
    # Get the minimum and maximum values of the raster layer, ignoring NA values
    min_val <- cellStats(raster_layer, stat = "min", na.rm = TRUE)
    max_val <- cellStats(raster_layer, stat = "max", na.rm = TRUE)
    # Return a list with the minimum and maximum values
    return(c(min = min_val, max = max_val))
  })
  return(min_max)
}
# Obtain the minimum and maximum values of the raster for each resolution.
min_max_values$`0.005` <- get_min_max(rasters_0.005)
min_max_values$`0.01` <- get_min_max(rasters_0.01)
min_max_values$`0.05` <- get_min_max(rasters_0.05)
overall_min_max <- list() # Create an empty list to store the overall minimum and maximum values for each raster.
# Calculate the overall min and max values for each raster, handling missing values (NA)
for (raster_name in unique(names(rasters_0.05))) {
  # Retrieve the min and max values for this raster across different resolutions
  min_vals <- c(
    # Check if the raster exists for the each resolution and get its min value
    ifelse(!is.null(min_max_values$`0.005`[[raster_name]]), min_max_values$`0.005`[[raster_name]]["min"], NA),
    ifelse(!is.null(min_max_values$`0.01`[[raster_name]]), min_max_values$`0.01`[[raster_name]]["min"], NA),
    ifelse(!is.null(min_max_values$`0.05`[[raster_name]]), min_max_values$`0.05`[[raster_name]]["min"], NA)
  )
  max_vals <- c(
    ifelse(!is.null(min_max_values$`0.005`[[raster_name]]), min_max_values$`0.005`[[raster_name]]["max"], NA),
    ifelse(!is.null(min_max_values$`0.01`[[raster_name]]), min_max_values$`0.01`[[raster_name]]["max"], NA),
    ifelse(!is.null(min_max_values$`0.05`[[raster_name]]), min_max_values$`0.05`[[raster_name]]["max"], NA)
  )
  # Calculate the overall min and max values for this raster, ignoring NA values
  overall_min_max[[raster_name]] <- c(
    min = min(min_vals, na.rm = TRUE),
    max = max(max_vals, na.rm = TRUE)
  )
}
# Create empty lists to store the normalized rasters for each resolution
normalized_rasters_0.005 <- list()
normalized_rasters_0.01 <- list()
normalized_rasters_0.05 <- list()
# Normalize rasters for 0.005 resolution
for (raster_name in names(rasters_0.005)) {
  # Retrieve the raster
  raster_layer <- rasters_0.005[[raster_name]]
  # Get the overall min and max values for this raster
  overall_min <- overall_min_max[[raster_name]]["min"]
  overall_max <- overall_min_max[[raster_name]]["max"]
  # Normalize the raster
  normalized_rasters_0.005[[raster_name]] <- (raster_layer - overall_min) / (overall_max - overall_min)
}
# Normalize rasters for 0.01 resolution
for (raster_name in names(rasters_0.01)) {
  raster_layer <- rasters_0.01[[raster_name]]
  overall_min <- overall_min_max[[raster_name]]["min"]
  overall_max <- overall_min_max[[raster_name]]["max"]
  normalized_rasters_0.01[[raster_name]] <- (raster_layer - overall_min) / (overall_max - overall_min)
}
# Normalize rasters for 0.05 resolution
for (raster_name in names(rasters_0.05)) {
  raster_layer <- rasters_0.05[[raster_name]]
  overall_min <- overall_min_max[[raster_name]]["min"]
  overall_max <- overall_min_max[[raster_name]]["max"]
  normalized_rasters_0.05[[raster_name]] <- (raster_layer - overall_min) / (overall_max - overall_min)
}
save(normalized_rasters_0.005, normalized_rasters_0.01, normalized_rasters_0.05, 
     file = "F:/[2024.07]Ghana/Processed Data/Modelling/normalized_rasters.RData")
save(rasters_0.005, rasters_0.01, rasters_0.05, 
     file = "F:/[2024.07]Ghana/Processed Data/Modelling/rasters.RData")
# Load the saved RData file
# load("F:/[2024.07]Ghana/Processed Data/Modelling/normalized_rasters.RData")
# load("F:/[2024.07]Ghana/Processed Data/Modelling/rasters.RData")
# print(normalized_rasters_0.005) # length(normalized_rasters_0.005) # 64
# print(normalized_rasters_0.01) # length(normalized_rasters_0.01) # 67
# print(normalized_rasters_0.05) # length(normalized_rasters_0.05) # 71
# inhabited area extraction at 0.05¡ã
inhabited_rasters <- rasters_0.05[c("ntl", "bltH", "blt_pro", "setl", "slum")]
stacked_rasters <- stack(inhabited_rasters)
binary_rasters <- stacked_rasters != 0
mask_raster <- calc(binary_rasters, fun = function(x) {
  return(ifelse(rowSums(x) > 0, 1, 0))
}) # plot(mask_raster)
mask_raster[mask_raster == 0] <- NA
writeRaster(mask_raster, "F:/[2024.07]Ghana/Processed Data/Modelling/inhabited_2021_0.05.tif", overwrite = TRUE)
# inhabited area extraction at 0.01¡ã
inhabited_rasters <- rasters_0.01[c("ntl", "bltH", "blt_pro", "setl", "slum")]
stacked_rasters <- stack(inhabited_rasters)
binary_rasters <- stacked_rasters != 0
mask_raster <- calc(binary_rasters, fun = function(x) {
  return(ifelse(rowSums(x) > 0, 1, 0))
}) # plot(mask_raster)
mask_raster[mask_raster == 0] <- NA
writeRaster(mask_raster, "F:/[2024.07]Ghana/Processed Data/Modelling/inhabited_2021_0.01.tif", overwrite = TRUE)
# inhabited area extraction at 0.005¡ã
# expanded_border <- st_buffer(border, dist = 0.005) # For different resolutions, the dist value needs to be modified
inhabited_rasters <- rasters_0.005[c("ntl", "bltH", "blt_pro", "setl", "slum")]
stacked_rasters <- stack(inhabited_rasters)
binary_rasters <- stacked_rasters != 0
mask_raster <- calc(binary_rasters, fun = function(x) {
  return(ifelse(rowSums(x) > 0, 1, 0))
}) # plot(mask_raster)
mask_raster[mask_raster == 0] <- NA
# masked_rasters <- mask(mask_raster, expanded_border) # plot(masked_rasters)
# masked_rasters[masked_rasters == 0] <- NA
writeRaster(mask_raster, "F:/[2024.07]Ghana/Processed Data/Modelling/inhabited_2021_0.005.tif", overwrite = TRUE)
# 0.05 degree: Extract the mean value of all auxiliary variables for each administrative district in pop_district
shapefile_path <- "F:/[2024.07]Ghana/Processed Data-202411/Pop(census)-shp-all/Pop_district_2021.shp"
pop_district <- st_read(shapefile_path)
ext_raster <- list()  # Initialize an empty list to store the extracted mean values
# dataframe of all covariates data at 0.05 degree
codegre0.05 <- coordinates(raster(normalized_rasters_0.05[[1]])) # corresponding by row
frameall_0.05 <- data.frame(longti = as.numeric(codegre0.05[,1]),latitute = as.numeric(codegre0.05[,2]))
# Loop through each raster in the normalized_rasters_0.05 list
masked_rasters <- raster("F:/[2024.07]Ghana/Processed Data/Modelling/inhabited_2021_0.05.tif")
for (i in 1:length(normalized_rasters_0.05)) {
  # Extract the name of the current raster
  raster_name <- names(normalized_rasters_0.05)[i]
  # Extract the mean value of the raster for each administrative district in pop_district
  rastermask <- mask(normalized_rasters_0.05[[i]], masked_rasters)
  avg_values <- extract(rastermask, pop_district, fun = mean, na.rm = TRUE)
  # Store the extracted mean values in the list, appending "_ext" to the raster name
  ext_raster[[raster_name]] <- avg_values
  # data frame of covariate with inhabited area masked (i.e., masked_rasters)
  rastermask_vec <- as.vector(t(as.matrix(rastermask)))
  frameall_0.05[[raster_name]] <- rastermask_vec
  cat(paste0(raster_name, " ¡ú ",i," of ", length(normalized_rasters_0.05), " has processed successfully!\n"))
}
saveRDS(ext_raster, file = paste0("F:/[2024.07]Ghana/Processed Data/Modelling/ext_raster_covariates_unit.rds"))
saveRDS(frameall_0.05, file = "F:/[2024.07]Ghana/Processed Data/Modelling/all_covariates_2021_0.05.rds")
# frameall_0.05 <- readRDS(file = "F:/[2024.07]Ghana/Processed Data/Modelling/all_covariates_2021_0.05.rds")
# 0.01 degree: dataframe of all covariates data at 0.01 degree
codegre0.01 <- coordinates(raster(normalized_rasters_0.01[[1]])) # corresponding by row
frameall_0.01 <- data.frame(longti = as.numeric(codegre0.01[,1]),latitute = as.numeric(codegre0.01[,2]))
masked_rasters <- raster("F:/[2024.07]Ghana/Processed Data/Modelling/inhabited_2021_0.01.tif")
# Loop through each raster in the normalized_rasters_0.01 list
for (i in 1:length(normalized_rasters_0.01)) {
  # Extract the name of the current raster
  raster_name <- names(normalized_rasters_0.01)[i]
  rastermask <- mask(normalized_rasters_0.01[[i]], masked_rasters)
  # data frame of covariate with inhabited area masked (i.e., masked_rasters)
  rastermask_vec <- as.vector(t(as.matrix(rastermask)))
  frameall_0.01[[raster_name]] <- rastermask_vec
  cat(paste0(raster_name, " ¡ú ",i," of ", length(normalized_rasters_0.01), " has processed successfully!\n"))
}
saveRDS(frameall_0.01, file = "F:/[2024.07]Ghana/Processed Data/Modelling/all_covariates_2021_0.01.rds")
# frameall_0.01 <- readRDS(file = "F:/[2024.07]Ghana/Processed Data/Modelling/all_covariates_2021_0.01.rds")
# 0.005 degree: dataframe of all covariates data at 0.005 degree
codegre0.005 <- coordinates(raster(normalized_rasters_0.005[[1]])) # corresponding by row
frameall_0.005 <- data.frame(longti = as.numeric(codegre0.005[,1]),latitute = as.numeric(codegre0.005[,2]))
masked_rasters <- raster("F:/[2024.07]Ghana/Processed Data/Modelling/inhabited_2021_0.005.tif")
# Loop through each raster in the normalized_rasters_0.01 list
for (i in 1:length(normalized_rasters_0.005)) {
  # Extract the name of the current raster
  raster_name <- names(normalized_rasters_0.005)[i]
  rastermask <- mask(normalized_rasters_0.005[[i]], masked_rasters)
  # data frame of covariate with inhabited area masked (i.e., masked_rasters)
  rastermask_vec <- as.vector(t(as.matrix(rastermask)))
  frameall_0.005[[raster_name]] <- rastermask_vec
  cat(paste0(raster_name, " ¡ú ",i," of ", length(normalized_rasters_0.005), " has processed successfully!\n"))
}
saveRDS(frameall_0.005, file = "F:/[2024.07]Ghana/Processed Data/Modelling/all_covariates_2021_0.005.rds")
# frameall_0.005 <- readRDS(file = "F:/[2024.07]Ghana/Processed Data/Modelling/all_covariates_2021_0.005.rds")
#-----------For 2022
# The 2022 data has not yet been processed
#-------
# End of the script


