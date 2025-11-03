
# This script computes four essential nighttime light indices: Averaged Nighttime Light (NLAve), Nighttime Light Intensity (NLVI), Nighttime Light Lit Area Proportion (NLVS), and Compounded Nighttime Light Index (VCNLI).
# Due to the spatial resolution of the original nighttime light data being approximately 500m, it is not possible to calculate nighttime light indices at resolutions less than or equal to this.
# Therefore, four nighttime light indices were calculated at scales of 0.01 degrees, 0.05 degrees, and administrative unit (district) levels, as well as the resampled nighttime light information at 0.005 degrees.
# Before running the code, please ensure that you have completed the following preparations:
# 1. Download the nighttime light dataand store it in the "F:/[2024.07]Ghana/Original Data/NTL" folder.
# 2. Install the required R libraries.
# If not already installed, you can use the following commands to install:
# install.packages("rgdal")  
# install.packages("rgeos")
# install.packages("raster")
# Note: The rgdal package has been removed from the CRAN repository as of October 16, 2023, at the maintainer's request. 
# For users of newer versions of R, this means that you will no longer be able to install rgdal directly from CRAN.Consider using 'sf' or 'terra' instead. 
# Formerly available versions can be obtained from the https://cran.r-project.org/src/contrib/Archive/rgdal/. 

# Load the necessary libraries
library(rgdal)
library(rgeos)
library(raster)
# Load the raster template from the Raster_Templates folder
raster_0.005 <- raster("F:/[2024.07]Ghana/Processed Data/Raster_Templates/0.005.tif")
raster_0.01 <- raster("F:/[2024.07]Ghana/Processed Data/Raster_Templates/0.01.tif")
raster_0.05 <- aggregate(raster_0.01,fact=5)
names(raster_0.05) <- "X0.05"
# Define the output folder path for saving processed NTL data
outputfold <- "F:/[2024.07]Ghana/Processed Data/NTL/"
dir.create(outputfold, showWarnings = F, recursive = T)
# Load the NTL raster data for 2021 and 2022
NL2021 <- raster("F:/[2024.07]Ghana/Original Data/NTL/2021/elds_AllAngle_Composite_Snow_FreeVNP46A4.A2021.tif")
NL2022 <- raster("F:/[2024.07]Ghana/Original Data/NTL/2022/elds_AllAngle_Composite_Snow_FreeVNP46A4.A2022.tif")
#-------NTL indices at resolutions of 0.005 degree in 2021 and 2022
# There is no finer-resolution nighttime light observation data within the 0.005-degree resolution grid, 
# making it impossible to calculate Averaged Nighttime Light (NLAve), Nighttime Light Intensity (NLVI), 
# Nighttime Light Lit Area Proportion (NLVS), and Compounded Nighttime Light Index (VCNLI).
# for 2021
NL2021_cropped <- crop(NL2021, extent(raster_0.005))
NL2021_resampled <- resample(NL2021_cropped, raster_0.005, method = "ngb")
writeRaster(NL2021_resampled, paste0(outputfold,"NTL_2021_0.005.tif"), format = "GTiff", overwrite = TRUE)
# for 2022
NL2022_cropped <- crop(NL2022, extent(raster_0.005))
NL2022_resampled <- resample(NL2022_cropped, raster_0.005, method = "ngb")
writeRaster(NL2022_resampled, paste0(outputfold,"NTL_2022_0.005.tif"), format = "GTiff", overwrite = TRUE)
#-------NTL indices at resolutions of 0.01 degree and 0.05 degree in 2021 and 2022
NL_resampled_files <- paste0(outputfold,c("NTL2021_ghana_0.005.tif","NTL2022_ghana_0.005.tif"))
years <- c("2021","2022")
#--for 0.01 degree
Tarscale <- "0.01" # Set the target resolution (modifiable).
aggfactor <- 2 # If modified, ensure the parameters in the aggregate function are adjusted accordingly.
aggregated_cell <-  aggfactor^2 # Calculate how many original cells are aggregated to form one new cell.
NL_NA_rast <- raster(aggregate(raster(NL_resampled_files[1]),aggfactor))
values(NL_NA_rast) <- NA
for (i in 1:length(years)){
  eval(parse(text = paste0("NLAve",years[i],"_",Tarscale, " <- aggregate(raster(\"",NL_resampled_files[i],"\"),", aggfactor,", fun = mean, na.rm=TRUE)")))
  eval(parse(text = paste0("writeRaster(NLAve",years[i],"_",Tarscale, ",filename = \"",outputfold,"NLAve_",years[i],"_", Tarscale,".tif\", 
                           format=\"GTiff\",overwrite=TRUE)")))
  eval(parse(text = paste0("NLmax",years[i],"_",Tarscale, " <- aggregate(raster(\"",NL_resampled_files[i],"\"),", aggfactor,", fun = max, na.rm=TRUE)")))
  eval(parse(text = paste0("NLsum",years[i],"_",Tarscale, " <- aggregate(raster(\"",NL_resampled_files[i],"\"),", aggfactor,", fun = sum, na.rm=TRUE)")))
  # zero_count <- sum(getValues(NLsum2021_0.01) == 0, na.rm = TRUE)
  eval(parse(text = paste0("NLVI", years[i],"_",Tarscale, " <- NL_NA_rast")))                     
  eval(parse(text = paste0("NLVI", years[i],"_",Tarscale, "[NLsum",years[i],"_",Tarscale, " != 0] <- (NLsum",years[i],"_",Tarscale, "[NLsum",years[i],
                           "_",Tarscale, " != 0])/(",aggregated_cell,"*NLmax",years[i],"_",Tarscale, "[NLsum",years[i],"_",Tarscale, " != 0])")))
  eval(parse(text = paste0("NLVI", years[i],"_",Tarscale, "[NLsum",years[i],"_",Tarscale, " == 0] <- 0")))
  eval(parse(text = paste0("writeRaster(NLVI",years[i],"_",Tarscale, ", filename = \"",outputfold,"NLVI_",years[i],"_", Tarscale,".tif\", 
                           format=\"GTiff\",overwrite=TRUE)")))                     
  eval(parse(text = paste0("NLvalid", years[i]," <- raster(\"",NL_resampled_files[i],"\")")))
  eval(parse(text = paste0("NLvalid", years[i],"[NLvalid",years[i],"<= 0] <- 0")))
  eval(parse(text = paste0("NLvalid", years[i],"[NLvalid",years[i],"> 0] <- 1")))
  eval(parse(text = paste0("NLVS", years[i],"_",Tarscale, " <- aggregate(NLvalid", years[i],",",aggfactor,", fun=sum, na.rm=TRUE)/",aggregated_cell)))
  eval(parse(text = paste0("writeRaster(NLVS",years[i],"_",Tarscale, ", filename = \"",outputfold,"NLVS_",years[i],"_", Tarscale,".tif\", 
                           format=\"GTiff\",overwrite=TRUE)")))
  eval(parse(text = paste0("VCNLI", years[i],"_",Tarscale, " <- NLVI",years[i],"_",Tarscale, "*NLVS",years[i],"_",Tarscale)))
  eval(parse(text = paste0("writeRaster(VCNLI",years[i],"_",Tarscale, ", filename = \"",outputfold,"VCNLI_",years[i],"_", Tarscale,".tif\", 
                           format=\"GTiff\",overwrite=TRUE)")))
}
#--for 0.05 degree
Tarscale <- "0.05" # Set the target resolution (modifiable).
aggfactor <- 10 # If modified, ensure the parameters in the aggregate function are adjusted accordingly.
aggregated_cell <-  aggfactor^2 # Calculate how many original cells are aggregated to form one new cell.
NL_NA_rast <- raster(aggregate(raster(NL_resampled_files[1]),aggfactor))
values(NL_NA_rast) <- NA
for (i in 1:length(years)){
  eval(parse(text = paste0("NLAve",years[i],"_",Tarscale, " <- aggregate(raster(\"",NL_resampled_files[i],"\"),", aggfactor,", fun = mean, na.rm=TRUE)")))
  eval(parse(text = paste0("writeRaster(NLAve",years[i],"_",Tarscale, ",filename = \"",outputfold,"NLAve_",years[i],"_", Tarscale,".tif\", 
                           format=\"GTiff\",overwrite=TRUE)")))
  eval(parse(text = paste0("NLmax",years[i],"_",Tarscale, " <- aggregate(raster(\"",NL_resampled_files[i],"\"),", aggfactor,", fun = max, na.rm=TRUE)")))
  eval(parse(text = paste0("NLsum",years[i],"_",Tarscale, " <- aggregate(raster(\"",NL_resampled_files[i],"\"),", aggfactor,", fun = sum, na.rm=TRUE)")))
  # zero_count <- sum(getValues(NLsum2021_0.01) == 0, na.rm = TRUE)
  eval(parse(text = paste0("NLVI", years[i],"_",Tarscale, " <- NL_NA_rast")))                     
  eval(parse(text = paste0("NLVI", years[i],"_",Tarscale, "[NLsum",years[i],"_",Tarscale, " != 0] <- (NLsum",years[i],"_",Tarscale, "[NLsum",years[i],
                           "_",Tarscale, " != 0])/(",aggregated_cell,"*NLmax",years[i],"_",Tarscale, "[NLsum",years[i],"_",Tarscale, " != 0])")))
  eval(parse(text = paste0("NLVI", years[i],"_",Tarscale, "[NLsum",years[i],"_",Tarscale, " == 0] <- 0")))
  eval(parse(text = paste0("writeRaster(NLVI",years[i],"_",Tarscale, ", filename = \"",outputfold,"NLVI_",years[i],"_", Tarscale,".tif\", 
                           format=\"GTiff\",overwrite=TRUE)")))                     
  eval(parse(text = paste0("NLvalid", years[i]," <- raster(\"",NL_resampled_files[i],"\")")))
  eval(parse(text = paste0("NLvalid", years[i],"[NLvalid",years[i],"<= 0] <- 0")))
  eval(parse(text = paste0("NLvalid", years[i],"[NLvalid",years[i],"> 0] <- 1")))
  eval(parse(text = paste0("NLVS", years[i],"_",Tarscale, " <- aggregate(NLvalid", years[i],",",aggfactor,", fun=sum, na.rm=TRUE)/",aggregated_cell)))
  eval(parse(text = paste0("writeRaster(NLVS",years[i],"_",Tarscale, ", filename = \"",outputfold,"NLVS_",years[i],"_", Tarscale,".tif\", 
                           format=\"GTiff\",overwrite=TRUE)")))
  eval(parse(text = paste0("VCNLI", years[i],"_",Tarscale, " <- NLVI",years[i],"_",Tarscale, "*NLVS",years[i],"_",Tarscale)))
  eval(parse(text = paste0("writeRaster(VCNLI",years[i],"_",Tarscale, ", filename = \"",outputfold,"VCNLI_",years[i],"_", Tarscale,".tif\", 
                           format=\"GTiff\",overwrite=TRUE)")))
}
#--for Administrative unit scale
# Load the district-level administrative boundary shape file from the Border folder
Dists_only <- readOGR(dsn = "F:/[2024.07]Ghana/Processed Data/Border", layer = "Ghana_Expanded_Only_Administrative_District")
# Dists_only <- readOGR(dsn = "F:/[2024.07]Ghana/Processed Date-20241016T023349Z-001/Processed Date/Border", layer = "ghana_shp_intersecting")
ras0.005_all <- raster_0.005
ras0.005_all <- setValues(ras0.005_all, 1) # Set all values of the 0.005-degree raster to 1 to facilitate the calculation of the number of pixels within administrative units.
NL_full <- mask(ras0.005_all,Dists_only)
# for 2021
NTL2021_ghana_0.005 <- raster(NL_resampled_files[1]) # load the processed NTL2021_ghana_0.005 data.
NL <- mask(NTL2021_ghana_0.005,Dists_only)
NL_vld <- NL # Assign a value of 1 to the pixels with nighttime light values to represent the number of valid pixels.
NL_vld[NL_vld <= 0] <- 0
NL_vld[NL_vld > 0] <- 1
NLAve_Dists <- extract(NL, Dists_only, fun = mean, na.rm=TRUE)
VTNL_Dists <- extract(NL, Dists_only, fun = sum, na.rm=TRUE)
NLmax_Dists <- extract(NL, Dists_only, fun = max, na.rm=TRUE)
NLnum_Dists <- extract(NL_full, Dists_only, fun = sum, na.rm=TRUE)
NLvld_Dists <- extract(NL_vld, Dists_only, fun = sum, na.rm=TRUE) 
VI_Dists <- VTNL_Dists/(NLmax_Dists*NLvld_Dists)
VS_Dists <- NLvld_Dists/NLnum_Dists
VCNLI_Dists <- VI_Dists*VS_Dists
Dists_only$NLAve <- factor(NLAve_Dists)
Dists_only$NLVI <- factor(VI_Dists)
Dists_only$NLVS <- factor(VS_Dists)
Dists_only$VCNLI <- factor(VCNLI_Dists)
writeOGR(obj=Dists_only, dsn = outputfold, layer="NTL_Dists_only_2021", driver="ESRI Shapefile",overwrite=TRUE)
# for 2022
NTL2022_ghana_0.005 <- raster(NL_resampled_files[2]) # load the processed NTL2021_ghana_0.005 data.
NL <- mask(NTL2022_ghana_0.005,Dists_only)
NL_vld <- NL # Assign a value of 1 to the pixels with nighttime light values to represent the number of valid pixels.
NL_vld[NL_vld <= 0] <- 0
NL_vld[NL_vld > 0] <- 1
NLAve_Dists <- extract(NL, Dists_only, fun = mean, na.rm=TRUE)
VTNL_Dists <- extract(NL, Dists_only, fun = sum, na.rm=TRUE)
NLmax_Dists <- extract(NL, Dists_only, fun = max, na.rm=TRUE)
NLnum_Dists <- extract(NL_full, Dists_only, fun = sum, na.rm=TRUE)
NLvld_Dists <- extract(NL_vld, Dists_only, fun = sum, na.rm=TRUE) 
VI_Dists <- VTNL_Dists/(NLmax_Dists*NLvld_Dists)
VS_Dists <- NLvld_Dists/NLnum_Dists
VCNLI_Dists <- VI_Dists*VS_Dists
Dists_only$NLAve <- factor(NLAve_Dists)
Dists_only$NLVI <- factor(VI_Dists)
Dists_only$NLVS <- factor(VS_Dists)
Dists_only$VCNLI <- factor(VCNLI_Dists)
writeOGR(obj=Dists_only, dsn = outputfold, layer="NTL_Dists_only_2022", driver="ESRI Shapefile",overwrite=TRUE)
#-------
# End of the script

