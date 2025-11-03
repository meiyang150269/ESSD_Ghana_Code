# This script processes the random forest modelling.
# If a grid contains at least one nonzero value in each of the five covariates, the grid is labeled as an inhabited area.
# i.e., night light (ntl), building height (bltH), percentage of building (blt_pro), settlement (setl), slums (slum)
# During the downscaling process of the census data, population counts were allocated only to grids located within the inhabited areas.
# Population downscaling (gridding) involves downscaling population density and then calculating the total population at the grid level. 
# Various levels of population density, such as district-level population density, were calculated by dividing the population count of each census unit by the size of the inhabited area of the corresponding unit.
# In calculating population density, to avoid potential errors arising from area calculations (which require projection conversion).
# we use the formula: population density = population ¡Â number of pixels in the inhabited area. 
# Here, the pixel size is treated as the unit of area. As a result, the population density represents the population per unit area (pixel) after downscaling.
# It should be noted that when scaling down to different resolutions, the number of pixels in the inhabited area varies due to the differing pixel sizes at each resolution.
# Before running the RF modeling, please ensure that you have completed the following preparations:
# 1. modelRF_data.R has run the results, saving all the auxiliary variable data and its normalized results.
# ext_raster <- readRDS("F:/[2024.07]Ghana/Processed Data/Modelling/Unit/ext_raster_covariates_unit.rds")
# frameall_0.05 <- readRDS(file = "F:/[2024.07]Ghana/Processed Data/Modelling/all_covariates_2021_0.05.rds")
# frameall_0.01 <- readRDS(file = "F:/[2024.07]Ghana/Processed Data/Modelling/all_covariates_2021_0.01.rds")
# frameall_0.005 <- readRDS(file = "F:/[2024.07]Ghana/Processed Data/Modelling/all_covariates_2021_0.005.rds")
# install.packages("sf")
# install.packages("raster")
# install.packages("caret")

library(sf)
library(raster)
library(randomForest)
library(caret)

# Define the custom random forest model for regression
customRF <- list(type = "Regression",  # "Regression" indicate a regression task
                 library = "randomForest",  # Use the randomForest package
                 loop = NULL)  # No looping required
# Define the hyperparameters for the model: mtry (number of variables to consider for each split) and ntree (number of trees)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), 
                                  class = rep("numeric", 2),  # Both parameters are numeric
                                  label = c("mtry", "ntree"))  # Labels for the parameters
# Define the grid search function; leave it empty as we are using grid search by default
customRF$grid <- function(x, y, len = NULL, search = "grid") {
  # This function is empty as the default grid search method is used
}
# Define the model fitting function, which uses the randomForest algorithm for regression
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  # Train the random forest model with the given parameters mtry and ntree
  randomForest(x, y, mtry = param$mtry, ntree = param$ntree, ...)  # Fit the random forest regression model
}
# Define the prediction function, which predicts the continuous values for regression
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
  # For regression, the model will return continuous predicted values, not class labels
  predict(modelFit, newdata)  # Predict on new data (returns continuous values for regression)
}
# Define the probability function; for regression, this is usually not needed as we predict continuous values
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
  # In regression, we do not use probabilities, but directly return the predicted continuous values
  predict(modelFit, newdata)  # Return predicted continuous values (no class probabilities in regression)
}
# Define the sorting function, which sorts the output based on the first column (usually predicted values)
customRF$sort <- function(x) x[order(x[,1]),]  # Sort the results by the predicted value
# Define the levels function; for regression, there are no class labels, so this function returns NULL
customRF$levels <- function(x) NULL  # For regression, no class labels exist, so return NULL
# Set training control parameters, using repeated cross-validation (repeatedcv)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3) # method = "repeatedcv" means repeated cross-validation, with 10 folds and 3 repeats
# Define the parameter grid for tuning, specifying values for mtry and ntree
tunegrid <- expand.grid(.mtry = c(1:30), .ntree = c(5, 10, 20, 50, 100, 300, 500, 800))
# Use expand.grid to create a grid for mtry (number of variables to consider) and ntree (number of trees)
# Load the saved RData file of all the auxiliary variable data and its normalized results in 2021
load("F:/[2024.07]Ghana/Processed Data/Modelling/normalized_rasters.RData")
load("F:/[2024.07]Ghana/Processed Data/Modelling/rasters.RData")
ext_raster <- readRDS("F:/[2024.07]Ghana/Processed Data/Modelling/Unit/ext_raster_covariates_unit.rds")
frameall_0.05 <- readRDS(file = "F:/[2024.07]Ghana/Processed Data/Modelling/all_covariates_2021_0.05.rds")
frameall_0.01 <- readRDS(file = "F:/[2024.07]Ghana/Processed Data/Modelling/all_covariates_2021_0.01.rds")
frameall_0.005 <- readRDS(file = "F:/[2024.07]Ghana/Processed Data/Modelling/all_covariates_2021_0.005.rds")
#-- for 0.05 degree
masked_rasters <- raster("F:/[2024.07]Ghana/Processed Data/Modelling/inhabited_2021_0.05.tif")
masked_rasters_vec <- as.vector(t(as.matrix(masked_rasters)))
masked_NAind <- which(is.na(masked_rasters_vec)) # pixel without modelling
# Load Ghana Census data of 2021
shapefile_path <- "F:/[2024.07]Ghana/Processed Data-202411/Pop(census)-shp-all/Pop_district_2021.shp"
pop_district <- st_read(shapefile_path)
pop_region <- st_read("F:/[2024.07]Ghana/Processed Data-202411/Pop(census)-shp-all/Pop_region_2021.shp") # downscaling adjustment using region census data
Ghana_border <- st_read("F:/[2024.07]Ghana/Processed Data-202411/Border/shapefile/Districts_261.shp")
inhabited_pixs <- extract(masked_rasters, pop_district, fun = sum, na.rm = TRUE)
# Extract column names related to population data
pop_names <- "pop_dis" 
region_names <- "pop_reg" # downscaling adjustment using region census data
outvarnames <- sub("_di.*", "", pop_names)
# Iterate over each variable name in pop_names
# The original data for NPP, SPEI01, SPEI03, and SPEI06 have missing values.
# NPP, SPEI01, SPEI03, SPEI06 have missing values in the administrative unit scale, that is, some administrative units are missing data. However, NA also represents an attribute value, so NA is assigned -1 here to participate in modeling.
adm_df <- as.data.frame(ext_raster)
adm_df[is.na(adm_df)] <- -1
var_name <- pop_names
# Perform the operation
adm_df[var_name] <- log(pop_district[[var_name]] / inhabited_pixs + 1) # Adding 1 to avoid log(0) which would result in NaN or undefined values.
adm_df <- na.omit(adm_df) # adm_df: all variables of the administrative unit scale
# write.csv(adm_df,file= "F:/[2024.07]Ghana/Processed Data/Modelling/adm_df.csv")
# adm_df <- read.csv("F:/[2024.07]Ghana/Processed Data/Modelling/adm_df.csv")
# Set a random seed for reproducibility of the results
set.seed(123)  # Ensure reproducibility by setting a fixed random seed
# Train the model using the custom random forest method for regression
# Create a folder to save results in downscaling from administrative unit to 0.05 degrees
outfold <- "F:/[2024.07]Ghana/Processed Data/Modelling/Unit/"
dir.create(outfold, recursive = TRUE, showWarnings = FALSE)
# Create a folder to save hyperparameter data for random forest modeling of different population structures
tunefold <- paste0(outfold,"Tune/")
dir.create(tunefold, recursive = TRUE, showWarnings = FALSE)
# Initializes the optimal hyperparameter and model information data frame
Tunebest <- data.frame(names = outvarnames, mtry = rep(NA, length(pop_names)), ntree = rep(NA, length(pop_names)))
model_summary <- data.frame(names = outvarnames, Mean_Squared_Residuals = rep(NA, length(pop_names)), 
                            Percent_Var_Explained = rep(NA, length(pop_names)))
independent_vars <- names(normalized_rasters_0.05)
# vars_to_remove <- c("npp", "SPEI01", "SPEI03", "SPEI06")
# independent_vars <- setdiff(independent_vars, vars_to_remove)
Accury <- list() # downscaling adjustment using region census data
Me <- function(A,B) {sum(B-A)/length(A)} # downscaling adjustment using region census data
Mae <- function(A,B) {sum(abs(B-A))/length(A)} # downscaling adjustment using region census data
Rmse <- function(A,B) {sqrt((sum((B-A)^2)/length(A)))}  # the same with rmse(B,A) in library(hydroGOF)
Rr <- function(A,B) {sum(((A-mean(A))*(B-mean(B))))/(length(A)-1)/(sd(A)*sd(B))}  # the same with cor()
slop <- function(A,B) {Rr(A,B)*sd(B)/sd(A)}  # the same with the  coef(lm(B~A))
Bs <- function(A,B) {mean(B)-mean(A)}
RmseR <- function(A,B) {sqrt((sum((B-A)^2)/length(A)))/mean(A)}  
# RF model for population data
# Construct the model formula, with the dependent variable as var_name and independent variables as independent_vars."
var_name <- pop_names
formula <- as.formula(paste(var_name, "~", paste(independent_vars, collapse = "+")))
# Training model
custom1 <- train(formula, data = adm_df, method = customRF, metric = "RMSE", tuneGrid = tunegrid, trControl = control)
png(paste0(tunefold, "Tune_", outvarnames, "_unit.png")) # Open a PNG device to save the plot
plot(custom1)
dev.off() # Close the PNG device to save the file
write.csv(custom1$results,file = paste0(tunefold,"Tune_",outvarnames,"_unit.csv"))
Tunebest[which(Tunebest$names == outvarnames), "mtry"] <- custom1$bestTune$mtry
Tunebest[which(Tunebest$names == outvarnames), "ntree"] <- custom1$bestTune$ntree
RF_pop <- randomForest(formula, data = adm_df, mtry = custom1$bestTune$mtry,
                       ntree = custom1$bestTune$ntree, importance=TRUE)
saveRDS(RF_pop, file = paste0(outfold,"RF_", outvarnames,"_unit.rds"))
model_summary[which(model_summary$names == outvarnames), "Mean_Squared_Residuals"] <- tail(RF_pop$mse, 1) 
model_summary[which(model_summary$names == outvarnames), "Percent_Var_Explained"] <- tail(RF_pop$rsq, 1)
# RF_pop_loaded <- readRDS(file = paste0(outfold, "RF_", outvarnames, "_unit.rds"))
var_importance <- varImp(RF_pop, scale = TRUE)
importance_df <- data.frame(Variable = rownames(var_importance), Importance = var_importance[, 1],
                            IncMSE = RF_pop$importance[, "%IncMSE"], IncNodePurity = RF_pop$importance[, "IncNodePurity"])
importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]
write.csv(importance_df,file = paste0(outfold,"VarImp_",outvarnames,"_unit.csv"))
png(paste0(outfold, "VarImp_", outvarnames, "_unit.png"),width = 950, height = 650)
varImpPlot(RF_pop, n.var = min(20, nrow(importance_df)), 
           main = 'Top 20 - variable importance')
dev.off()
# get the top 15 variables
top15_vars <- head(importance_df$Variable, 15)
formula_top15 <- as.formula(paste(var_name, "~", paste(top15_vars, collapse = "+")))
custom1 <- train(formula_top15, data = adm_df, method = customRF, metric = "RMSE", tuneGrid = tunegrid, trControl = control)
# write.csv(custom1$results,file = paste0(tunefold,"Tune_",outvarnames,"_unit_top15.csv"))
RF_pop_top15 <- randomForest(formula_top15, data = adm_df,
                             mtry = custom1$bestTune$mtry,
                             ntree = custom1$bestTune$ntree,
                             importance = TRUE)
# prediction at 0.05 degree
frameall_0.05[ , !(names(frameall_0.05) %in% c("longti", "latitute"))] <- 
  lapply(frameall_0.05[ , !(names(frameall_0.05) %in% c("longti", "latitute"))], 
         function(x) { ifelse(is.na(x), -1, x) })
frameall_0.05[masked_NAind, !(names(frameall_0.05) %in% c("longti", "latitute"))] <- NA
frameall_0.05_datavec <- rep(NA, mode = "numeric", length = nrow(frameall_0.05))
frameall_0.05_datavec[-masked_NAind] <- predict(RF_pop_top15, na.omit(frameall_0.05))
Var_Trend <- cbind(frameall_0.05$longti,frameall_0.05$latitute,frameall_0.05_datavec)
Var_Trend_ras <- rasterFromXYZ(Var_Trend, res = res(raster(masked_rasters)), crs = crs(raster(masked_rasters)))
Var_Trend_ras <-  exp(Var_Trend_ras)-1 # log(pop_district[[var_name]] / inhabited_pixs + 1)
Var_Trend_ras[Var_Trend_ras < 0] <- 0 # Population is a non-zero value.
writeRaster(Var_Trend_ras, filename =  paste0(outfold,"RF_",outvarnames,"_0.05.tif"), format="GTiff",overwrite=TRUE)
# plot(Var_Trend_ras)
# using region census to adjust the results
pop_region$predA1 <- pop_region[[region_names]]/as.vector(extract(Var_Trend_ras, pop_region, fun = sum, na.rm=TRUE))
raster_A1 <- rasterize(pop_region, field="predA1", masked_rasters)
raster_A1 <- mask(raster_A1, masked_rasters)
Var_Trend_ras_adjust1 <- raster_A1*Var_Trend_ras  # spplot(AdRF20101km)
Var_Trend_ras_adjust1 <- mask(Var_Trend_ras_adjust1,Ghana_border)
writeRaster(Var_Trend_ras_adjust1, filename = paste0(outfold,"Ad1RF_",outvarnames,"_0.05.tif"), format="GTiff",overwrite=TRUE)
plot(Var_Trend_ras_adjust1)
Comdata <- data.frame(Tvalue1 = pop_district[[var_name]], Pvalue1 = as.vector(extract(Var_Trend_ras_adjust1, pop_district, fun = sum, na.rm=TRUE)))
Comdata <- na.omit(Comdata)
Tvalue1 <- Comdata$Tvalue1
Pvalue1 <- Comdata$Pvalue1
Accury[[outvarnames]] <- c (Rmse(Tvalue1,Pvalue1),Rr(Tvalue1,Pvalue1),slop(Tvalue1,Pvalue1),Me(Tvalue1,Pvalue1),
                               Mae(Tvalue1,Pvalue1),  Bs(Tvalue1,Pvalue1),RmseR(Tvalue1,Pvalue1))
cat(paste0(outvarnames, " ¡ú ", " has processed successfully!\n"))
write.csv(Tunebest,file= paste0(outfold,"Tunebest_unit.csv"))
write.csv(model_summary,file = paste0(outfold,"model_summary_unit.csv"))
Accury_df <- as.data.frame(Accury)
rownames(Accury_df) <- c("RMSE", "Rr", "slop", "ME", "MAE", "Bs", "RMSER")
saveRDS(Accury, file = paste0(outfold,"RF_Accury_0.05.rds")) # downscaling adjustment using region census data
saveRDS(Accury_df, file = paste0(outfold,"RF_Accury_0.05.csv")) # downscaling adjustment using region census data
Accury <- readRDS(file = paste0(outfold,"RF_Accury_0.005.rds"))
#-- for 0.01 degree
frameall_0.05 <- readRDS(file = "F:/[2024.07]Ghana/Processed Data/Modelling/all_covariates_2021_0.05.rds")
frameall_0.01 <- readRDS(file = "F:/[2024.07]Ghana/Processed Data/Modelling/all_covariates_2021_0.01.rds")
masked_rasters <- raster("F:/[2024.07]Ghana/Processed Data/Modelling/inhabited_2021_0.01.tif")
masked_rasters_vec <- as.vector(t(as.matrix(masked_rasters)))
masked_NAind <- which(is.na(masked_rasters_vec)) # pixel without modelling
inhabited_pixs <- aggregate(masked_rasters, fact = 5, fun = sum)
inputfold <- "F:/[2024.07]Ghana/Processed Data/Modelling/Unit/"
outfold <- "F:/[2024.07]Ghana/Processed Data/Modelling/0.05/"
dir.create(outfold, recursive = TRUE, showWarnings = FALSE)
# Create a folder to save hyperparameter data for random forest modeling of population
tunefold <- paste0(outfold,"Tune/")
dir.create(tunefold, recursive = TRUE, showWarnings = FALSE)
shapefile_path <- "F:/[2024.07]Ghana/Processed Data-202411/Pop(census)-shp-all/Pop_district_2021.shp"
pop_district <- st_read(shapefile_path)
pop_names <- tail(names(pop_district), 27)[-27] 
outvarnames <- sub("_di.*", "", pop_names)
# Iterate over each variable name in pop_names
frameall_0.05_df <- as.data.frame(frameall_0.05)
frameall_0.05_df[is.na(frameall_0.05_df)] <- -1
# Set a random seed for reproducibility of the results
set.seed(123)  # Ensure reproducibility by setting a fixed random seed
# Initializes the optimal hyperparameter and model information data frame
Tunebest <- data.frame(names = outvarnames, mtry = rep(NA, length(pop_names)), ntree = rep(NA, length(pop_names)))
model_summary <- data.frame(names = outvarnames, Mean_Squared_Residuals = rep(NA, length(pop_names)), 
                            Percent_Var_Explained = rep(NA, length(pop_names)))
independent_vars <- setdiff(names(frameall_0.01), c("longti", "latitute"))
# RF model for population data
k=1 # pop_names <- "pop_dis" 
var_name <- outvarnames[k]
popraster <- raster(paste0(inputfold,"Ad1RF_",var_name,"_0.05.tif"))
popdens <- log(popraster/inhabited_pixs+ 1) # Adding 1 to avoid log(0) which would result in NaN or undefined values.
frameall_0.05_df[var_name] <- as.vector(t(as.matrix(popdens)))
formula <- as.formula(paste(var_name, "~", paste(independent_vars, collapse = "+")))
# Training model
custom1 <- train(formula, data = na.omit(frameall_0.05_df), method = customRF, metric = "RMSE", tuneGrid = tunegrid, trControl = control)
png(paste0(tunefold, "Tune_", outvarnames[k], "_0.05.png")) # Open a PNG device to save the plot
plot(custom1)
dev.off() # Close the PNG device to save the file
write.csv(custom1$results,file = paste0(tunefold,"Tune_",outvarnames[k],"_0.05.csv"))
Tunebest[which(Tunebest$names == outvarnames[k]), "mtry"] <- custom1$bestTune$mtry
Tunebest[which(Tunebest$names == outvarnames[k]), "ntree"] <- custom1$bestTune$ntree
RF_pop <- randomForest(formula, data = na.omit(frameall_0.05_df), mtry = custom1$bestTune$mtry,
                       ntree = custom1$bestTune$ntree, importance=TRUE)
saveRDS(RF_pop, file = paste0(outfold,"RF_", outvarnames[k],"_0.05.rds"))
model_summary[which(model_summary$names == outvarnames[k]), "Mean_Squared_Residuals"] <- tail(RF_pop$mse, 1) 
model_summary[which(model_summary$names == outvarnames[k]), "Percent_Var_Explained"] <- tail(RF_pop$rsq, 1)
# RF_pop_loaded <- readRDS(file = paste0(outfold, "RF_", outvarnames[k], "_unit.rds"))
var_importance <- varImp(RF_pop, scale = TRUE)
importance_df <- data.frame(Variable = rownames(var_importance), Importance = var_importance[, 1],
                            IncMSE = RF_pop$importance[, "%IncMSE"], IncNodePurity = RF_pop$importance[, "IncNodePurity"])
importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]
write.csv(importance_df,file = paste0(outfold,"VarImp_",outvarnames[k],"_0.05.csv"))
png(paste0(outfold, "VarImp_", outvarnames[k], "_0.05.png"),width = 950, height = 650)
varImpPlot(RF_pop, n.var = min(20, nrow(importance_df)), 
           main = 'Top 20 - variable importance')
dev.off()
# prediction at 0.01 degree
frameall_0.01[ , !(names(frameall_0.01) %in% c("longti", "latitute"))] <- 
  lapply(frameall_0.01[ , !(names(frameall_0.01) %in% c("longti", "latitute"))], 
         function(x) { ifelse(is.na(x), -1, x) })
frameall_0.01[masked_NAind, !(names(frameall_0.01) %in% c("longti", "latitute"))] <- NA
frameall_0.01_datavec <- rep(NA, mode = "numeric", length = nrow(frameall_0.01))
frameall_0.01_datavec[-masked_NAind] <- predict(RF_pop, na.omit(frameall_0.01))
Var_Trend <- cbind(frameall_0.01$longti,frameall_0.01$latitute,frameall_0.01_datavec)
Var_Trend_ras <- rasterFromXYZ(Var_Trend, res = res(raster(masked_rasters)), crs = crs(raster(masked_rasters)))
Var_Trend_ras <-  exp(Var_Trend_ras)-1 # log(pop_district[[var_name]] / inhabited_pixs + 1)
Var_Trend_ras[Var_Trend_ras < 0] <- 0 # Population is a non-zero value.
writeRaster(Var_Trend_ras, filename =  paste0(outfold,"RF_",outvarnames[k],"_0.01.tif"), format="GTiff",overwrite=TRUE)
# plot(Var_Trend_ras)
# using 0.05 predictions to adjust the results
predA <- raster(paste0(inputfold,"Ad1RF_",var_name,"_0.05.tif"))/aggregate(Var_Trend_ras, fact = 5, fun = sum)
raster_A <- resample(predA, masked_rasters, method="ngb")
raster_A <- mask(raster_A, masked_rasters)
Var_Trend_ras_adjust <- raster_A*Var_Trend_ras  # spplot(AdRF20101km)
writeRaster(Var_Trend_ras_adjust, filename = paste0(outfold,"AdRF_",outvarnames[k],"_0.01.tif"), format="GTiff",overwrite=TRUE)
cat(paste0(outvarnames[k], " ¡ú ",k," of ", length(outvarnames), " has processed successfully!\n"))
Comdata <- data.frame(Tvalue1 = pop_district[[var_name]], Pvalue1 = as.vector(extract(Var_Trend_ras_adjust, pop_district, fun = sum, na.rm=TRUE)))
Comdata <- na.omit(Comdata)
Tvalue1 <- Comdata$Tvalue1
Pvalue1 <- Comdata$Pvalue1
Accury[[outvarnames[k]]] <- c (Rmse(Tvalue1,Pvalue1),Rr(Tvalue1,Pvalue1),slop(Tvalue1,Pvalue1),Me(Tvalue1,Pvalue1),
                               Mae(Tvalue1,Pvalue1),  Bs(Tvalue1,Pvalue1),RmseR(Tvalue1,Pvalue1))
write.csv(Tunebest,file= paste0(outfold,"Tunebest_0.05.csv"))
write.csv(model_summary,file = paste0(outfold,"model_summary_0.05.csv"))
Accury_df <- as.data.frame(Accury)
rownames(Accury_df) <- c("RMSE", "Rr", "slop", "ME", "MAE", "Bs", "RMSER")
saveRDS(Accury, file = paste0(outfold,"RF_Accury_0.01.rds")) # downscaling adjustment using region census data
#-- for 0.005 degree
frameall_0.005 <- readRDS(file = "F:/[2024.07]Ghana/Processed Data/Modelling/all_covariates_2021_0.005.rds")
frameall_0.01 <- readRDS(file = "F:/[2024.07]Ghana/Processed Data/Modelling/all_covariates_2021_0.01.rds")
masked_rasters <- raster("F:/[2024.07]Ghana/Processed Data/Modelling/inhabited_2021_0.005.tif")
masked_rasters_vec <- as.vector(t(as.matrix(masked_rasters)))
masked_NAind <- which(is.na(masked_rasters_vec)) # pixel without modelling
inhabited_pixs <- aggregate(masked_rasters, fact = 2, fun = sum)
inputfold <- "F:/[2024.07]Ghana/Processed Data/Modelling/0.01/"
outfold <- "F:/[2024.07]Ghana/Processed Data/Modelling/0.005/"
dir.create(outfold, recursive = TRUE, showWarnings = FALSE)
# Create a folder to save hyperparameter data for random forest modeling of population
tunefold <- paste0(outfold,"Tune/")
dir.create(tunefold, recursive = TRUE, showWarnings = FALSE)
shapefile_path <- "F:/[2024.07]Ghana/Processed Data-202411/Pop(census)-shp-all/Pop_district_2021.shp"
pop_district <- st_read(shapefile_path)
pop_names <- tail(names(pop_district), 27)[-27] 
outvarnames <- sub("_di.*", "", pop_names)
# Iterate over each variable name in pop_names
frameall_0.01_df <- as.data.frame(frameall_0.01)
frameall_0.01_df[is.na(frameall_0.01_df)] <- -1
# Set a random seed for reproducibility of the results
set.seed(123)  # Ensure reproducibility by setting a fixed random seed
# Initializes the optimal hyperparameter and model information data frame
Tunebest <- data.frame(names = outvarnames, mtry = rep(NA, length(pop_names)), ntree = rep(NA, length(pop_names)))
model_summary <- data.frame(names = outvarnames, Mean_Squared_Residuals = rep(NA, length(pop_names)), 
                            Percent_Var_Explained = rep(NA, length(pop_names)))
independent_vars <- setdiff(names(frameall_0.005), c("longti", "latitute"))
# RF model for population data
k=1
var_name <- outvarnames[k]
popraster <- raster(paste0(inputfold,"AdRF_",var_name,"_0.01.tif"))
popdens <- log(popraster/inhabited_pixs+ 1) # Adding 1 to avoid log(0) which would result in NaN or undefined values.
frameall_0.01_df[var_name] <- as.vector(t(as.matrix(popdens)))
formula <- as.formula(paste(var_name, "~", paste(independent_vars, collapse = "+")))
# Training model
custom1 <- train(formula, data = na.omit(frameall_0.01_df), method = customRF, metric = "RMSE", tuneGrid = tunegrid, trControl = control)
png(paste0(tunefold, "Tune_", outvarnames[k], "_0.01.png")) # Open a PNG device to save the plot
plot(custom1)
dev.off() # Close the PNG device to save the file
write.csv(custom1$results,file = paste0(tunefold,"Tune_",outvarnames[k],"_0.01.csv"))
Tunebest[which(Tunebest$names == outvarnames[k]), "mtry"] <- custom1$bestTune$mtry
Tunebest[which(Tunebest$names == outvarnames[k]), "ntree"] <- custom1$bestTune$ntree
RF_pop <- randomForest(formula, data = na.omit(frameall_0.01_df), mtry = custom1$bestTune$mtry,
                       ntree = custom1$bestTune$ntree, importance=TRUE)
saveRDS(RF_pop, file = paste0(outfold,"RF_", outvarnames[k],"_0.01.rds"))
model_summary[which(model_summary$names == outvarnames[k]), "Mean_Squared_Residuals"] <- tail(RF_pop$mse, 1) 
model_summary[which(model_summary$names == outvarnames[k]), "Percent_Var_Explained"] <- tail(RF_pop$rsq, 1)
# RF_pop_loaded <- readRDS(file = paste0(outfold, "RF_", outvarnames[k], "_0.01.rds"))
var_importance <- varImp(RF_pop, scale = TRUE)
importance_df <- data.frame(Variable = rownames(var_importance), Importance = var_importance[, 1],
                            IncMSE = RF_pop$importance[, "%IncMSE"], IncNodePurity = RF_pop$importance[, "IncNodePurity"])
importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]
write.csv(importance_df,file = paste0(outfold,"VarImp_",outvarnames[k],"_0.01.csv"))
png(paste0(outfold, "VarImp_", outvarnames[k], "_0.01.png"),width = 950, height = 650)
varImpPlot(RF_pop, n.var = min(20, nrow(importance_df)), 
           main = 'Top 20 - variable importance')
dev.off()
# prediction at 0.005 degree
frameall_0.005[ , !(names(frameall_0.005) %in% c("longti", "latitute"))] <- 
  lapply(frameall_0.005[ , !(names(frameall_0.005) %in% c("longti", "latitute"))], 
         function(x) { ifelse(is.na(x), -1, x) })
frameall_0.005[masked_NAind, !(names(frameall_0.005) %in% c("longti", "latitute"))] <- NA
frameall_0.005_datavec <- rep(NA, mode = "numeric", length = nrow(frameall_0.005))
frameall_0.005_datavec[-masked_NAind] <- predict(RF_pop, na.omit(frameall_0.005))
Var_Trend <- cbind(frameall_0.005$longti,frameall_0.005$latitute,frameall_0.005_datavec)
Var_Trend_ras <- rasterFromXYZ(Var_Trend, res = res(raster(masked_rasters)), crs = crs(raster(masked_rasters)))
Var_Trend_ras <-  exp(Var_Trend_ras)-1 # log(pop_district[[var_name]] / inhabited_pixs + 1)
Var_Trend_ras[Var_Trend_ras < 0] <- 0 # Population is a non-zero value.
writeRaster(Var_Trend_ras, filename =  paste0(outfold,"RF_",outvarnames[k],"_0.005.tif"), format="GTiff",overwrite=TRUE)
# plot(Var_Trend_ras)
# using 0.01 predictions to adjust the results
predA <- raster(paste0(inputfold,"AdRF_",var_name,"_0.01.tif"))/aggregate(Var_Trend_ras, fact = 2, fun = sum)
raster_A <- resample(predA, masked_rasters, method="ngb")
raster_A <- mask(raster_A, masked_rasters)
Var_Trend_ras_adjust <- raster_A*Var_Trend_ras  # spplot(AdRF20101km)
writeRaster(Var_Trend_ras_adjust, filename = paste0(outfold,"AdRF_",outvarnames[k],"_0.005.tif"), format="GTiff",overwrite=TRUE)
cat(paste0(outvarnames[k], " ¡ú ", " has processed successfully!\n"))
write.csv(Tunebest,file= paste0(outfold,"Tunebest_0.01.csv"))
write.csv(model_summary,file = paste0(outfold,"model_summary_0.01.csv"))
#-------
# End of the script

