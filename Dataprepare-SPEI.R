# This script processes and extracts annual Standardized Precipitation Evaporation Index (SPEI) data at 0.05-degree resolution.
# The Standardized Precipitation Evaporation Index (SPEI) assesses the balance between precipitation and evaporation demand across different timescales, providing a measure of drought severity and duration.
# The Global high-resolution drought indices for 1981�C2022 offers monthly SPEI values at 0.05-degree resolution, available at timescales from 1 to 48 months.
# Gebrechorkos, S. H., Peng, J., Dyer, E., Miralles, D. G., Vicente-Serrano, S. M., Funk, C., Beck, H. E., Asfaw, D. T., Singer, M. B., and Dadson, S. J.: Global high-resolution drought indices for 1981�C2022, Earth Syst. Sci. Data, 15, 5449�C5466, https://doi.org/10.5194/essd-15-5449-2023, 2023.
# Short timescales (e.g., 1�C3 months) are suited for evaluating short-term meteorological drought, often used to analyze seasonal moisture deficits, such as precipitation anomalies between seasons.
# Medium timescales (e.g., 6�C12 months) are more suitable for assessing agricultural drought, reflecting impacts on crop growth cycles or agricultural seasons.
# Thence, 1, 3, and 6-month timescale data are selected in this experiment. 
# Monthly data of 2021 and 2022 are then aggregated to annual averages, medians, maximum, and minimum values.
# Before running the code, please ensure that you have completed the following preparations:
# 1. Download the Africa high-resolution drought datasets from 1981-2022 and store it in the "F:/[2024.07]Ghana/Original Data/SPEI/" folder.
# 2. Install the required R libraries.
# If not already installed, you can use the following commands to install:
# install.packages("terra")

# Load the necessary libraries
library(terra)
# Load the raster template from the Raster_Templates folder
raster_0.01 <- rast("F:/[2024.07]Ghana/Processed Data/Raster_Templates/0.01.tif")
raster_0.05 <- aggregate(raster_0.01,fact=5)
names(raster_0.05) <- "X0.05"
# Define the output folder path for saving processed SPEI data
outputfold <- "F:/[2024.07]Ghana/Processed Data/SPEI/"
dir.create(outputfold, showWarnings = F, recursive = T)
# Load the SPEI nc data for timescales of 1, 3, and 6 months
file_paths <- paste0("F:/[2024.07]Ghana/Original Data/SPEI/",c("Africa_spei01.nc","Africa_spei03.nc","Africa_spei06.nc"))
spei_data <- list() # Initialize an empty list to store SPEI data
# Loop through each file path to load data and extract the last 24 layers
for (i in 1:length(file_paths)) {
  nc_data <- rast(file_paths[i])
  last_24_layers <- tail(names(nc_data), 24) # Extract the last 24 layers (months) from the nc data
  spei_data[[i]] <- nc_data[[last_24_layers]] # Store the extracted layers in the list
}
# Define a function to calculate annual statistics and resample to the specified template raster_0.05
calculate_annual_stats <- function(raster_stack) {
  # Calculate annual mean by grouping the first 12 layers (2021) and the next 12 layers (2022)
  annual_mean <- tapp(raster_stack, index = rep(1:2, each = 12), fun = mean, na.rm = TRUE)
  annual_median <- tapp(raster_stack, index = rep(1:2, each = 12), fun = median, na.rm = TRUE)
  annual_max <- tapp(raster_stack, index = rep(1:2, each = 12), fun = max, na.rm = TRUE)
  annual_min <- tapp(raster_stack, index = rep(1:2, each = 12), fun = min, na.rm = TRUE)
  # Resample the statistics to the raster_0.05 template using nearest neighbor method
  return(list(
    mean = resample(annual_mean, raster_0.05, method = "near"),
    median = resample(annual_median, raster_0.05, method = "near"),
    max = resample(annual_max, raster_0.05, method = "near"),
    min = resample(annual_min, raster_0.05, method = "near")
  ))
}
# Calculate annual statistics for SPEI01, SPEI03, and SPEI06
spei_stats <- list(
  SPEI01 = calculate_annual_stats(spei_data[[1]]),
  SPEI03 = calculate_annual_stats(spei_data[[2]]),
  SPEI06 = calculate_annual_stats(spei_data[[3]])
) # In the Results, X1 is 2021 and X2 is 2022
# Save the statistical results for each SPEI tiff
stat_names <- c("mean", "median", "max", "min")
timescales <- c(1,3,6)
for (j in 1:length(spei_stats)) {
  for (stat in stat_names) {
    output_file <- paste0(outputfold, "SPEI", sprintf("%02d", timescales[j]), "_",c("2021","2022"),"_",stat, "_0.05.tif")
    # Save the 2021 annual statistic
    writeRaster(spei_stats[[j]][[stat]]$X1, output_file[1], overwrite = TRUE)
    # Save the 2022 annual statistic
    writeRaster(spei_stats[[j]][[stat]]$X2, output_file[2], overwrite = TRUE)
    cat(paste0("SPEI", sprintf("%02d", timescales[j])," Annual ",stat," raster files saved successfully!\n"))
  }
}
#-------
# End of the script

