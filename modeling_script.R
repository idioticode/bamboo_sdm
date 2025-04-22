# =============================================================================
# Bambusa alami (bam_alam)------------------------------------------------------
# =============================================================================

## loading Libraries--------

library_function()

## working directory. ----------
setwd("D:/Drive/Bamboo_SDM/bam_alam")

## Nepal map-------
nepal <- sf::st_read(
  "D:/MISC/points/hermes shape files/hermes_NPL_new_wgs_0.shp"
)

## Importing and stacking the raster files the raster files -------

directory <- "D:/MISC/MaxEnt SDM/data/final_data/current_data"

raster_files <- list.files(directory, pattern = "\\.tif$", full.names = TRUE)

parameter <- stack(raster_files)

names(parameter)


## Occurrence Points ------------
occurrence_points <- readxl::read_xlsx(
  "D:/MISC/MaxEnt SDM/data/final_data/occurrence_points/occurrence_points.xlsx"
)|>
  dplyr::filter(species_code == "bam_alam") |>
  dplyr::select(c("LON", "LAT"))



## Testing Model ---------


test_model <- run_test_model(
  occurrence_df = occurrence_points,
  predictor_variables = parameter
)

test_model


## Final Model -------------
mod_bam_alam <- run_final_model(
  occurrence_df = occurrence_points,
  predictor_variables = parameter,
  replication_number = 2,
  model_names = c('maxent',
                  'rf')
)

mod_bam_alam

## Threshold Values and saving the excel files ------------
threshold_value <- evaluation_function(
  model_name = mod_bam_alam,
  species_name = "bam_alam"
)

threshold_value

## Predicting Current Model and getting the raster file -----------
current_ensembled <- current_prediction_model(
  model_name =mod_bam_alam,
  stacked_predictor = parameter,
  species_name = "bam_alam"
)

## exporting the classified raster file
writeRaster(
  current_ensembled >= threshold_value,
  filename = "bam_alam_classified_current.tif",
  overwrite=TRUE
)


## Future Prediction -------------

### ssp245 

#### 2050 
ssp245_2050 <- future_prediction_model(
  model_name = mod_bam_alam,
  current_rasters = parameter,
  species_name = "bam_alam",
  year = 2050,
  ssp = 245
)


## exporting the classified raster file
writeRaster(
  ssp245_2050 >= threshold_value,
  filename = "bam_alam_classified_ssp245_2050.tif",
  overwrite=TRUE
)


#### 2070 
ssp245_2070 <- future_prediction_model(
  model_name = mod_bam_alam,
  current_rasters = parameter,
  species_name = "bam_alam",
  year = 2070,
  ssp = 245
)

# exporting the classified raster file
writeRaster(
  ssp245_2070 >= threshold_value,
  filename = "bam_alam_classified_ssp245_2070.tif",
  overwrite=TRUE
)


#### 2090 
ssp245_2090 <- future_prediction_model(
  model_name = mod_bam_alam,
  current_rasters = parameter,
  species_name = "bam_alam",
  year = 2090,
  ssp = 245
)

# exporting the classified raster file
writeRaster(
  ssp245_2090 >= threshold_value,
  filename = "bam_alam_classified_ssp245_2090.tif",
  overwrite=TRUE
)


### ssp585 

#### 2050 
ssp585_2050 <- future_prediction_model(
  model_name = mod_bam_alam,
  current_rasters = parameter,
  species_name = "bam_alam",
  year = 2050,
  ssp = 585
)


# exporting the classified raster file
writeRaster(
  ssp585_2050 >= threshold_value,
  filename = "bam_alam_classified_ssp585_2050.tif",
  overwrite=TRUE
)


#### 2070 
ssp585_2070 <- future_prediction_model(
  model_name = mod_bam_alam,
  current_rasters = parameter,
  species_name = "bam_alam",
  year = 2070,
  ssp = 585
)


# exporting the classified raster file
writeRaster(
  ssp585_2070 >= threshold_value,
  filename = "bam_alam_classified_ssp585_2070.tif",
  overwrite=TRUE
)


#### 2090 
ssp585_2090 <- future_prediction_model(
  model_name = mod_bam_alam,
  current_rasters = parameter,
  species_name = "bam_alam",
  year = 2090,
  ssp = 585
)


# exporting the classified raster file
writeRaster(
  ssp585_2090 >= threshold_value,
  filename = "bam_alam_classified_ssp585_2090.tif",
  overwrite=TRUE
)



## Relative variable Importance ----------


importance_variable <- variable_imp_function(
  model_name = mod_bam_alam,
  species_name = "bam_alam"
)


## Response Curves -------------------

# Example usage:
response_curve_function(
  model_name = mod_bam_alam,      # Your model
  variable_df = importance_variable,  # Data frame with predictor variables
  species_name = "bam_alam"       # Species name
)

## Saving ROC-AUC- Plots -------------

save_roc_plots(model_name = mod_bam_alam,
               species_name = "bam_alam",
               methods = c( 'maxent',
                            'rf',
                            'brt',
                            'fda',
                            'brt',
                            'glm', 
                            'gam'))


## Final Plots ----------


sdm_plot(shape_file = nepal, 
         species_name = "bam_alam", 
         ssp = 245, 
         year = 2050)


sdm_plot(shape_file = nepal, 
         species_name = "bam_alam", 
         ssp = 245, 
         year = 2070)

sdm_plot(shape_file = nepal, 
         species_name = "bam_alam", 
         ssp = 245, 
         year = 2090)


sdm_plot(shape_file = nepal, 
         species_name = "bam_alam", 
         ssp = 585, 
         year = 2050)


sdm_plot(shape_file = nepal, 
         species_name = "bam_alam", 
         ssp = 585, 
         year = 2070)

sdm_plot(shape_file = nepal, 
         species_name = "bam_alam", 
         ssp = 585, 
         year = 2090)





# =============================================================================
# Bambusa balcooa (bam_bal)----------------------------------------------------
# =============================================================================

## loading Libraries--------

library_function()

## working directory. ----------
setwd("D:/Drive/Bamboo_SDM/bam_bal")

## Nepal map-------
nepal <- sf::st_read(
  "D:/MISC/points/hermes shape files/hermes_NPL_new_wgs_0.shp"
)

## Importing and stacking the raster files the raster files -------

directory <- "D:/MISC/MaxEnt SDM/data/final_data/current_data"

raster_files <- list.files(directory, pattern = "\\.tif$", full.names = TRUE)

parameter <- stack(raster_files)

names(parameter)


## Occurrence Points ------------
occurrence_points <- readxl::read_xlsx(
  "D:/MISC/MaxEnt SDM/data/final_data/occurrence_points/occurrence_points.xlsx"
)|>
  dplyr::filter(species_code == "bam_bal") |>
  dplyr::select(c("LON", "LAT"))



## Testing Model ---------
test_model <- run_test_model(
  occurrence_df = occurrence_points,
  predictor_variables = parameter
)

test_model

models_used = c('maxent',
                'rf')

## Final Model -------------
mod_bam_bal <- run_final_model(
  occurrence_df = occurrence_points,
  predictor_variables = parameter,
  replication_number = 10,
  model_names = models_used
)

mod_bam_bal

## Threshold Values and saving the excel files ------------
threshold_value <- evaluation_function(
  model_name = mod_bam_bal,
  species_name = "bam_bal"
)

threshold_value

## Predicting Current Model and getting the raster file -----------
current_ensembled <- current_prediction_model(
  model_name =mod_bam_bal,
  stacked_predictor = parameter,
  species_name = "bam_bal"
)

## exporting the classified raster file
writeRaster(
  current_ensembled >= threshold_value,
  filename = "bam_bal_classified_current.tif",
  overwrite=TRUE
)


## Future Prediction -------------

### ssp245 

#### 2050 
ssp245_2050 <- future_prediction_model(
  model_name = mod_bam_bal,
  current_rasters = parameter,
  species_name = "bam_bal",
  year = 2050,
  ssp = 245
)


## exporting the classified raster file
writeRaster(
  ssp245_2050 >= threshold_value,
  filename = "bam_bal_classified_ssp245_2050.tif",
  overwrite=TRUE
)


#### 2070 
ssp245_2070 <- future_prediction_model(
  model_name = mod_bam_bal,
  current_rasters = parameter,
  species_name = "bam_bal",
  year = 2070,
  ssp = 245
)

# exporting the classified raster file
writeRaster(
  ssp245_2070 >= threshold_value,
  filename = "bam_bal_classified_ssp245_2070.tif",
  overwrite=TRUE
)


#### 2090 
ssp245_2090 <- future_prediction_model(
  model_name = mod_bam_bal,
  current_rasters = parameter,
  species_name = "bam_bal",
  year = 2090,
  ssp = 245
)

# exporting the classified raster file
writeRaster(
  ssp245_2090 >= threshold_value,
  filename = "bam_bal_classified_ssp245_2090.tif",
  overwrite=TRUE
)


### ssp585 

#### 2050 
ssp585_2050 <- future_prediction_model(
  model_name = mod_bam_bal,
  current_rasters = parameter,
  species_name = "bam_bal",
  year = 2050,
  ssp = 585
)


# exporting the classified raster file
writeRaster(
  ssp585_2050 >= threshold_value,
  filename = "bam_bal_classified_ssp585_2050.tif",
  overwrite=TRUE
)


#### 2070 
ssp585_2070 <- future_prediction_model(
  model_name = mod_bam_bal,
  current_rasters = parameter,
  species_name = "bam_bal",
  year = 2070,
  ssp = 585
)


# exporting the classified raster file
writeRaster(
  ssp585_2070 >= threshold_value,
  filename = "bam_bal_classified_ssp585_2070.tif",
  overwrite=TRUE
)


#### 2090 
ssp585_2090 <- future_prediction_model(
  model_name = mod_bam_bal,
  current_rasters = parameter,
  species_name = "bam_bal",
  year = 2090,
  ssp = 585
)


# exporting the classified raster file
writeRaster(
  ssp585_2090 >= threshold_value,
  filename = "bam_bal_classified_ssp585_2090.tif",
  overwrite=TRUE
)



## Relative variable Importance ----------


importance_variable <- variable_imp_function(
  model_name = mod_bam_bal,
  species_name = "bam_bal"
)


## Response Curves -------------------

# Example usage:
response_curve_function(
  model_name = mod_bam_bal,      # Your model
  variable_df = importance_variable,  # Data frame with predictor variables
  species_name = "bam_bal"       # Species name
)

## Saving ROC-AUC- Plots -------------

save_roc_plots(model_name = mod_bam_bal,
               species_name = "bam_bal",
               methods = models_used)


## Final Plots ----------


sdm_plot(shape_file = nepal, 
         species_name = "bam_bal", 
         ssp = 245, 
         year = 2050)


sdm_plot(shape_file = nepal, 
         species_name = "bam_bal", 
         ssp = 245, 
         year = 2070)

sdm_plot(shape_file = nepal, 
         species_name = "bam_bal", 
         ssp = 245, 
         year = 2090)


sdm_plot(shape_file = nepal, 
         species_name = "bam_bal", 
         ssp = 585, 
         year = 2050)


sdm_plot(shape_file = nepal, 
         species_name = "bam_bal", 
         ssp = 585, 
         year = 2070)

sdm_plot(shape_file = nepal, 
         species_name = "bam_bal", 
         ssp = 585, 
         year = 2090)


# =============================================================================
# Bambusa nepalensis (bam_nep)----------------------------------------------------
# =============================================================================

## loading Libraries--------

library_function()

## working directory. ----------
setwd("D:/Drive/Bamboo_SDM/bam_nep")

## Nepal map-------
nepal <- sf::st_read(
  "D:/MISC/points/hermes shape files/hermes_NPL_new_wgs_0.shp"
)

## Importing and stacking the raster files the raster files -------

directory <- "D:/MISC/MaxEnt SDM/data/final_data/current_data"

raster_files <- list.files(directory, pattern = "\\.tif$", full.names = TRUE)

parameter <- stack(raster_files)

names(parameter)


## Occurrence Points ------------
occurrence_points <- readxl::read_xlsx(
  "D:/MISC/MaxEnt SDM/data/final_data/occurrence_points/occurrence_points.xlsx"
)|>
  dplyr::filter(species_code == "bam_nep") |>
  dplyr::select(c("LON", "LAT"))



## Testing Model ---------
test_model <- run_test_model(
  occurrence_df = occurrence_points,
  predictor_variables = parameter
)

test_model

models_used <- c("rf")

## Final Model -------------
mod_bam_nep <- run_final_model(
  occurrence_df = occurrence_points,
  predictor_variables = parameter,
  replication_number = 40,
  model_names = models_used
)

mod_bam_nep

## Threshold Values and saving the excel files ------------
threshold_value <- evaluation_function(
  model_name = mod_bam_nep,
  species_name = "bam_nep"
)

threshold_value

## Predicting Current Model and getting the raster file -----------
current_ensembled <- current_prediction_model(
  model_name =mod_bam_nep,
  stacked_predictor = parameter,
  species_name = "bam_nep"
)

## exporting the classified raster file
writeRaster(
  current_ensembled >= threshold_value,
  filename = "bam_nep_classified_current.tif",
  overwrite=TRUE
)


## Future Prediction -------------

### ssp245 

#### 2050 
ssp245_2050 <- future_prediction_model(
  model_name = mod_bam_nep,
  current_rasters = parameter,
  species_name = "bam_nep",
  year = 2050,
  ssp = 245
)


## exporting the classified raster file
writeRaster(
  ssp245_2050 >= threshold_value,
  filename = "bam_nep_classified_ssp245_2050.tif",
  overwrite=TRUE
)


#### 2070 
ssp245_2070 <- future_prediction_model(
  model_name = mod_bam_nep,
  current_rasters = parameter,
  species_name = "bam_nep",
  year = 2070,
  ssp = 245
)

# exporting the classified raster file
writeRaster(
  ssp245_2070 >= threshold_value,
  filename = "bam_nep_classified_ssp245_2070.tif",
  overwrite=TRUE
)


#### 2090 
ssp245_2090 <- future_prediction_model(
  model_name = mod_bam_nep,
  current_rasters = parameter,
  species_name = "bam_nep",
  year = 2090,
  ssp = 245
)

# exporting the classified raster file
writeRaster(
  ssp245_2090 >= threshold_value,
  filename = "bam_nep_classified_ssp245_2090.tif",
  overwrite=TRUE
)


### ssp585 

#### 2050 
ssp585_2050 <- future_prediction_model(
  model_name = mod_bam_nep,
  current_rasters = parameter,
  species_name = "bam_nep",
  year = 2050,
  ssp = 585
)


# exporting the classified raster file
writeRaster(
  ssp585_2050 >= threshold_value,
  filename = "bam_nep_classified_ssp585_2050.tif",
  overwrite=TRUE
)


#### 2070 
ssp585_2070 <- future_prediction_model(
  model_name = mod_bam_nep,
  current_rasters = parameter,
  species_name = "bam_nep",
  year = 2070,
  ssp = 585
)


# exporting the classified raster file
writeRaster(
  ssp585_2070 >= threshold_value,
  filename = "bam_nep_classified_ssp585_2070.tif",
  overwrite=TRUE
)


#### 2090 
ssp585_2090 <- future_prediction_model(
  model_name = mod_bam_nep,
  current_rasters = parameter,
  species_name = "bam_nep",
  year = 2090,
  ssp = 585
)


# exporting the classified raster file
writeRaster(
  ssp585_2090 >= threshold_value,
  filename = "bam_nep_classified_ssp585_2090.tif",
  overwrite=TRUE
)



## Relative variable Importance ----------


importance_variable <- variable_imp_function(
  model_name = mod_bam_nep,
  species_name = "bam_nep"
)


## Response Curves -------------------

# Example usage:
response_curve_function(
  model_name = mod_bam_nep,      # Your model
  variable_df = importance_variable,  # Data frame with predictor variables
  species_name = "bam_nep"       # Species name
)

## Saving ROC-AUC- Plots -------------

save_roc_plots(model_name = mod_bam_nep,
               species_name = "bam_nep",
               methods = models_used)


## Final Plots ----------


sdm_plot(shape_file = nepal, 
         species_name = "bam_nep", 
         ssp = 245, 
         year = 2050)


sdm_plot(shape_file = nepal, 
         species_name = "bam_nep", 
         ssp = 245, 
         year = 2070)

sdm_plot(shape_file = nepal, 
         species_name = "bam_nep", 
         ssp = 245, 
         year = 2090)


sdm_plot(shape_file = nepal, 
         species_name = "bam_nep", 
         ssp = 585, 
         year = 2050)


sdm_plot(shape_file = nepal, 
         species_name = "bam_nep", 
         ssp = 585, 
         year = 2070)

sdm_plot(shape_file = nepal, 
         species_name = "bam_nep", 
         ssp = 585, 
         year = 2090)






# =============================================================================
# Bambusa nutans subsp. cupulata (bam_nut_cup)----------------------------------------------------
# =============================================================================

## loading Libraries--------

library_function()

## working directory. ----------
setwd("D:/Drive/Bamboo_SDM/bam_nut_cup")

## Nepal map-------
nepal <- sf::st_read(
  "D:/MISC/points/hermes shape files/hermes_NPL_new_wgs_0.shp"
)

## Importing and stacking the raster files the raster files -------

directory <- "D:/MISC/MaxEnt SDM/data/final_data/current_data"

raster_files <- list.files(directory, pattern = "\\.tif$", full.names = TRUE)

parameter <- stack(raster_files)

names(parameter)


## Occurrence Points ------------
occurrence_points <- readxl::read_xlsx(
  "D:/MISC/MaxEnt SDM/data/final_data/occurrence_points/occurrence_points.xlsx"
)|>
  dplyr::filter(species_code == "bam_nut_cup") |>
  dplyr::select(c("LON", "LAT"))



## Testing Model ---------
test_model <- run_test_model(
  occurrence_df = occurrence_points,
  predictor_variables = parameter
)

test_model

models_used <- c("rf",
                 'maxent',
                 'mars',
                 'gam')

## Final Model -------------
mod_bam_nut_cup <- run_final_model(
  occurrence_df = occurrence_points,
  predictor_variables = parameter,
  replication_number = 10,
  model_names = models_used
)

mod_bam_nut_cup

## Threshold Values and saving the excel files ------------
threshold_value <- evaluation_function(
  model_name = mod_bam_nut_cup,
  species_name = "bam_nut_cup"
)

threshold_value

## Predicting Current Model and getting the raster file -----------
current_ensembled <- current_prediction_model(
  model_name =mod_bam_nut_cup,
  stacked_predictor = parameter,
  species_name = "bam_nut_cup"
)

## exporting the classified raster file
writeRaster(
  current_ensembled >= threshold_value,
  filename = "bam_nut_cup_classified_current.tif",
  overwrite=TRUE
)


## Future Prediction -------------

### ssp245 

#### 2050 
ssp245_2050 <- future_prediction_model(
  model_name = mod_bam_nut_cup,
  current_rasters = parameter,
  species_name = "bam_nut_cup",
  year = 2050,
  ssp = 245
)


## exporting the classified raster file
writeRaster(
  ssp245_2050 >= threshold_value,
  filename = "bam_nut_cup_classified_ssp245_2050.tif",
  overwrite=TRUE
)


#### 2070 
ssp245_2070 <- future_prediction_model(
  model_name = mod_bam_nut_cup,
  current_rasters = parameter,
  species_name = "bam_nut_cup",
  year = 2070,
  ssp = 245
)

# exporting the classified raster file
writeRaster(
  ssp245_2070 >= threshold_value,
  filename = "bam_nut_cup_classified_ssp245_2070.tif",
  overwrite=TRUE
)


#### 2090 
ssp245_2090 <- future_prediction_model(
  model_name = mod_bam_nut_cup,
  current_rasters = parameter,
  species_name = "bam_nut_cup",
  year = 2090,
  ssp = 245
)

# exporting the classified raster file
writeRaster(
  ssp245_2090 >= threshold_value,
  filename = "bam_nut_cup_classified_ssp245_2090.tif",
  overwrite=TRUE
)


### ssp585 

#### 2050 
ssp585_2050 <- future_prediction_model(
  model_name = mod_bam_nut_cup,
  current_rasters = parameter,
  species_name = "bam_nut_cup",
  year = 2050,
  ssp = 585
)


# exporting the classified raster file
writeRaster(
  ssp585_2050 >= threshold_value,
  filename = "bam_nut_cup_classified_ssp585_2050.tif",
  overwrite=TRUE
)


#### 2070 
ssp585_2070 <- future_prediction_model(
  model_name = mod_bam_nut_cup,
  current_rasters = parameter,
  species_name = "bam_nut_cup",
  year = 2070,
  ssp = 585
)


# exporting the classified raster file
writeRaster(
  ssp585_2070 >= threshold_value,
  filename = "bam_nut_cup_classified_ssp585_2070.tif",
  overwrite=TRUE
)


#### 2090 
ssp585_2090 <- future_prediction_model(
  model_name = mod_bam_nut_cup,
  current_rasters = parameter,
  species_name = "bam_nut_cup",
  year = 2090,
  ssp = 585
)


# exporting the classified raster file
writeRaster(
  ssp585_2090 >= threshold_value,
  filename = "bam_nut_cup_classified_ssp585_2090.tif",
  overwrite=TRUE
)



## Relative variable Importance ----------


importance_variable <- variable_imp_function(
  model_name = mod_bam_nut_cup,
  species_name = "bam_nut_cup"
)


## Response Curves -------------------

# Example usage:
response_curve_function(
  model_name = mod_bam_nut_cup,      # Your model
  variable_df = importance_variable,  # Data frame with predictor variables
  species_name = "bam_nut_cup"       # Species name
)

## Saving ROC-AUC- Plots -------------

save_roc_plots(model_name = mod_bam_nut_cup,
               species_name = "bam_nut_cup",
               methods = models_used)


## Final Plots ----------


sdm_plot(shape_file = nepal, 
         species_name = "bam_nut_cup", 
         ssp = 245, 
         year = 2050)


sdm_plot(shape_file = nepal, 
         species_name = "bam_nut_cup", 
         ssp = 245, 
         year = 2070)

sdm_plot(shape_file = nepal, 
         species_name = "bam_nut_cup", 
         ssp = 245, 
         year = 2090)


sdm_plot(shape_file = nepal, 
         species_name = "bam_nut_cup", 
         ssp = 585, 
         year = 2050)


sdm_plot(shape_file = nepal, 
         species_name = "bam_nut_cup", 
         ssp = 585, 
         year = 2070)

sdm_plot(shape_file = nepal, 
         species_name = "bam_nut_cup", 
         ssp = 585, 
         year = 2090)




# =============================================================================
# Bambusa nutans subsp. cupulata (bam_nut_nut)---------------------------------
# =============================================================================

## loading Libraries--------

library_function()

## working directory. ----------
setwd("D:/Drive/Bamboo_SDM/bam_nut_nut")

## Nepal map-------
nepal <- sf::st_read(
  "D:/MISC/points/hermes shape files/hermes_NPL_new_wgs_0.shp"
)

## Importing and stacking the raster files the raster files -------

directory <- "D:/MISC/MaxEnt SDM/data/final_data/current_data"

raster_files <- list.files(directory, pattern = "\\.tif$", full.names = TRUE)

parameter <- stack(raster_files)

names(parameter)


## Occurrence Points ------------
occurrence_points <- readxl::read_xlsx(
  "D:/MISC/MaxEnt SDM/data/final_data/occurrence_points/occurrence_points.xlsx"
)|>
  dplyr::filter(species_code == "bam_nut_nut") |>
  dplyr::select(c("LON", "LAT"))



## Testing Model ---------
test_model <- run_test_model(
  occurrence_df = occurrence_points,
  predictor_variables = parameter
)

test_model

models_used <- c("rf",
                 'maxent',
                 'brt',
                 'gam')

## Final Model -------------
mod_bam_nut_nut <- run_final_model(
  occurrence_df = occurrence_points,
  predictor_variables = parameter,
  replication_number = 10,
  model_names = models_used
)

mod_bam_nut_nut

## Threshold Values and saving the excel files ------------
threshold_value <- evaluation_function(
  model_name = mod_bam_nut_nut,
  species_name = "bam_nut_nut"
)

threshold_value

## Predicting Current Model and getting the raster file -----------
current_ensembled <- current_prediction_model(
  model_name =mod_bam_nut_nut,
  stacked_predictor = parameter,
  species_name = "bam_nut_nut"
)

## exporting the classified raster file
writeRaster(
  current_ensembled >= threshold_value,
  filename = "bam_nut_nut_classified_current.tif",
  overwrite=TRUE
)


## Future Prediction -------------

### ssp245 

#### 2050 
ssp245_2050 <- future_prediction_model(
  model_name = mod_bam_nut_nut,
  current_rasters = parameter,
  species_name = "bam_nut_nut",
  year = 2050,
  ssp = 245
)


## exporting the classified raster file
writeRaster(
  ssp245_2050 >= threshold_value,
  filename = "bam_nut_nut_classified_ssp245_2050.tif",
  overwrite=TRUE
)


#### 2070 
ssp245_2070 <- future_prediction_model(
  model_name = mod_bam_nut_nut,
  current_rasters = parameter,
  species_name = "bam_nut_nut",
  year = 2070,
  ssp = 245
)

# exporting the classified raster file
writeRaster(
  ssp245_2070 >= threshold_value,
  filename = "bam_nut_nut_classified_ssp245_2070.tif",
  overwrite=TRUE
)


#### 2090 
ssp245_2090 <- future_prediction_model(
  model_name = mod_bam_nut_nut,
  current_rasters = parameter,
  species_name = "bam_nut_nut",
  year = 2090,
  ssp = 245
)

# exporting the classified raster file
writeRaster(
  ssp245_2090 >= threshold_value,
  filename = "bam_nut_nut_classified_ssp245_2090.tif",
  overwrite=TRUE
)


### ssp585 

#### 2050 
ssp585_2050 <- future_prediction_model(
  model_name = mod_bam_nut_nut,
  current_rasters = parameter,
  species_name = "bam_nut_nut",
  year = 2050,
  ssp = 585
)


# exporting the classified raster file
writeRaster(
  ssp585_2050 >= threshold_value,
  filename = "bam_nut_nut_classified_ssp585_2050.tif",
  overwrite=TRUE
)


#### 2070 
ssp585_2070 <- future_prediction_model(
  model_name = mod_bam_nut_nut,
  current_rasters = parameter,
  species_name = "bam_nut_nut",
  year = 2070,
  ssp = 585
)


# exporting the classified raster file
writeRaster(
  ssp585_2070 >= threshold_value,
  filename = "bam_nut_nut_classified_ssp585_2070.tif",
  overwrite=TRUE
)


#### 2090 
ssp585_2090 <- future_prediction_model(
  model_name = mod_bam_nut_nut,
  current_rasters = parameter,
  species_name = "bam_nut_nut",
  year = 2090,
  ssp = 585
)


# exporting the classified raster file
writeRaster(
  ssp585_2090 >= threshold_value,
  filename = "bam_nut_nut_classified_ssp585_2090.tif",
  overwrite=TRUE
)



## Relative variable Importance ----------


importance_variable <- variable_imp_function(
  model_name = mod_bam_nut_nut,
  species_name = "bam_nut_nut"
)


## Response Curves -------------------

# Example usage:
response_curve_function(
  model_name = mod_bam_nut_nut,      # Your model
  variable_df = importance_variable,  # Data frame with predictor variables
  species_name = "bam_nut_nut"       # Species name
)

## Saving ROC-AUC- Plots -------------

save_roc_plots(model_name = mod_bam_nut_nut,
               species_name = "bam_nut_nut",
               methods = models_used)


## Final Plots ----------


sdm_plot(shape_file = nepal, 
         species_name = "bam_nut_nut", 
         ssp = 245, 
         year = 2050)


sdm_plot(shape_file = nepal, 
         species_name = "bam_nut_nut", 
         ssp = 245, 
         year = 2070)

sdm_plot(shape_file = nepal, 
         species_name = "bam_nut_nut", 
         ssp = 245, 
         year = 2090)


sdm_plot(shape_file = nepal, 
         species_name = "bam_nut_nut", 
         ssp = 585, 
         year = 2050)


sdm_plot(shape_file = nepal, 
         species_name = "bam_nut_nut", 
         ssp = 585, 
         year = 2070)

sdm_plot(shape_file = nepal, 
         species_name = "bam_nut_nut", 
         ssp = 585, 
         year = 2090)



# =============================================================================
# Dendrocalamus hamiltonii var hamiltonii and undulatus (dendro_hamil)---------
# =============================================================================

## loading Libraries--------

library_function()

## working directory. ----------
setwd("D:/Drive/Bamboo_SDM/dendro_hamil")

## Nepal map-------
nepal <- sf::st_read(
  "D:/MISC/points/hermes shape files/hermes_NPL_new_wgs_0.shp"
)

## Importing and stacking the raster files the raster files -------

directory <- "D:/MISC/MaxEnt SDM/data/final_data/current_data"

raster_files <- list.files(directory, pattern = "\\.tif$", full.names = TRUE)

parameter <- stack(raster_files)

names(parameter)


## Occurrence Points ------------
occurrence_points <- readxl::read_xlsx(
  "D:/MISC/MaxEnt SDM/data/final_data/occurrence_points/occurrence_points.xlsx"
)|>
  dplyr::filter(species_code == "dendro_hamil") |>
  dplyr::select(c("LON", "LAT"))



## Testing Model ---------
test_model <- run_test_model(
  occurrence_df = occurrence_points,
  predictor_variables = parameter
)

test_model

models_used <- c("rf",
                 'maxent',
                 'brt',
                 'gam',
                 'mars')

## Final Model -------------
mod_dendro_hamil <- run_final_model(
  occurrence_df = occurrence_points,
  predictor_variables = parameter,
  replication_number = 10,
  model_names = models_used
)

mod_dendro_hamil

## Threshold Values and saving the excel files ------------
threshold_value <- evaluation_function(
  model_name = mod_dendro_hamil,
  species_name = "dendro_hamil"
)

threshold_value

## Predicting Current Model and getting the raster file -----------
current_ensembled <- current_prediction_model(
  model_name =mod_dendro_hamil,
  stacked_predictor = parameter,
  species_name = "dendro_hamil"
)

## exporting the classified raster file
writeRaster(
  current_ensembled >= threshold_value,
  filename = "dendro_hamil_classified_current.tif",
  overwrite=TRUE
)


## Future Prediction -------------

### ssp245 

#### 2050 
ssp245_2050 <- future_prediction_model(
  model_name = mod_dendro_hamil,
  current_rasters = parameter,
  species_name = "dendro_hamil",
  year = 2050,
  ssp = 245
)


## exporting the classified raster file
writeRaster(
  ssp245_2050 >= threshold_value,
  filename = "dendro_hamil_classified_ssp245_2050.tif",
  overwrite=TRUE
)


#### 2070 
ssp245_2070 <- future_prediction_model(
  model_name = mod_dendro_hamil,
  current_rasters = parameter,
  species_name = "dendro_hamil",
  year = 2070,
  ssp = 245
)

# exporting the classified raster file
writeRaster(
  ssp245_2070 >= threshold_value,
  filename = "dendro_hamil_classified_ssp245_2070.tif",
  overwrite=TRUE
)


#### 2090 
ssp245_2090 <- future_prediction_model(
  model_name = mod_dendro_hamil,
  current_rasters = parameter,
  species_name = "dendro_hamil",
  year = 2090,
  ssp = 245
)

# exporting the classified raster file
writeRaster(
  ssp245_2090 >= threshold_value,
  filename = "dendro_hamil_classified_ssp245_2090.tif",
  overwrite=TRUE
)


### ssp585 

#### 2050 
ssp585_2050 <- future_prediction_model(
  model_name = mod_dendro_hamil,
  current_rasters = parameter,
  species_name = "dendro_hamil",
  year = 2050,
  ssp = 585
)


# exporting the classified raster file
writeRaster(
  ssp585_2050 >= threshold_value,
  filename = "dendro_hamil_classified_ssp585_2050.tif",
  overwrite=TRUE
)


#### 2070 
ssp585_2070 <- future_prediction_model(
  model_name = mod_dendro_hamil,
  current_rasters = parameter,
  species_name = "dendro_hamil",
  year = 2070,
  ssp = 585
)


# exporting the classified raster file
writeRaster(
  ssp585_2070 >= threshold_value,
  filename = "dendro_hamil_classified_ssp585_2070.tif",
  overwrite=TRUE
)


#### 2090 
ssp585_2090 <- future_prediction_model(
  model_name = mod_dendro_hamil,
  current_rasters = parameter,
  species_name = "dendro_hamil",
  year = 2090,
  ssp = 585
)


# exporting the classified raster file
writeRaster(
  ssp585_2090 >= threshold_value,
  filename = "dendro_hamil_classified_ssp585_2090.tif",
  overwrite=TRUE
)



## Relative variable Importance ----------


importance_variable <- variable_imp_function(
  model_name = mod_dendro_hamil,
  species_name = "dendro_hamil"
)


## Response Curves -------------------

# Example usage:
response_curve_function(
  model_name = mod_dendro_hamil,      # Your model
  variable_df = importance_variable,  # Data frame with predictor variables
  species_name = "dendro_hamil"       # Species name
)

## Saving ROC-AUC- Plots -------------

save_roc_plots(model_name = mod_dendro_hamil,
               species_name = "dendro_hamil",
               methods = models_used)


## Final Plots ----------


sdm_plot(shape_file = nepal, 
         species_name = "dendro_hamil", 
         ssp = 245, 
         year = 2050)


sdm_plot(shape_file = nepal, 
         species_name = "dendro_hamil", 
         ssp = 245, 
         year = 2070)

sdm_plot(shape_file = nepal, 
         species_name = "dendro_hamil", 
         ssp = 245, 
         year = 2090)


sdm_plot(shape_file = nepal, 
         species_name = "dendro_hamil", 
         ssp = 585, 
         year = 2050)


sdm_plot(shape_file = nepal, 
         species_name = "dendro_hamil", 
         ssp = 585, 
         year = 2070)

sdm_plot(shape_file = nepal, 
         species_name = "dendro_hamil", 
         ssp = 585, 
         year = 2090)



# =============================================================================
# Dendrocalamus hamiltonii var hamiltonii and undulatus (dendro_hook)---------
# =============================================================================

## loading Libraries--------

library_function()

## working directory. ----------
setwd("D:/Drive/Bamboo_SDM/dendro_hook")

## Nepal map-------
nepal <- sf::st_read(
  "D:/MISC/points/hermes shape files/hermes_NPL_new_wgs_0.shp"
)

## Importing and stacking the raster files the raster files -------

directory <- "D:/MISC/MaxEnt SDM/data/final_data/current_data"

raster_files <- list.files(directory, pattern = "\\.tif$", full.names = TRUE)

parameter <- stack(raster_files)

names(parameter)


## Occurrence Points ------------
occurrence_points <- readxl::read_xlsx(
  "D:/MISC/MaxEnt SDM/data/final_data/occurrence_points/occurrence_points.xlsx"
)|>
  dplyr::filter(species_code == "dendro_hook") |>
  dplyr::select(c("LON", "LAT"))



## Testing Model ---------
test_model <- run_test_model(
  occurrence_df = occurrence_points,
  predictor_variables = parameter
)

test_model

models_used <- c("rf",
                 'maxent',
                 'brt',
                 'fda',
                 'svm',
                 'glm',
                 'mars',
                 'gam',
                 'mars')

## Final Model -------------
mod_dendro_hook <- run_final_model(
  occurrence_df = occurrence_points,
  predictor_variables = parameter,
  replication_number = 8,
  model_names = models_used
)

mod_dendro_hook

## Threshold Values and saving the excel files ------------
threshold_value <- evaluation_function(
  model_name = mod_dendro_hook,
  species_name = "dendro_hook"
)

threshold_value

## Predicting Current Model and getting the raster file -----------
current_ensembled <- current_prediction_model(
  model_name =mod_dendro_hook,
  stacked_predictor = parameter,
  species_name = "dendro_hook"
)

## exporting the classified raster file
writeRaster(
  current_ensembled >= threshold_value,
  filename = "dendro_hook_classified_current.tif",
  overwrite=TRUE
)


## Future Prediction -------------

### ssp245 

#### 2050 
ssp245_2050 <- future_prediction_model(
  model_name = mod_dendro_hook,
  current_rasters = parameter,
  species_name = "dendro_hook",
  year = 2050,
  ssp = 245
)


## exporting the classified raster file
writeRaster(
  ssp245_2050 >= threshold_value,
  filename = "dendro_hook_classified_ssp245_2050.tif",
  overwrite=TRUE
)


#### 2070 
ssp245_2070 <- future_prediction_model(
  model_name = mod_dendro_hook,
  current_rasters = parameter,
  species_name = "dendro_hook",
  year = 2070,
  ssp = 245
)

# exporting the classified raster file
writeRaster(
  ssp245_2070 >= threshold_value,
  filename = "dendro_hook_classified_ssp245_2070.tif",
  overwrite=TRUE
)


#### 2090 
ssp245_2090 <- future_prediction_model(
  model_name = mod_dendro_hook,
  current_rasters = parameter,
  species_name = "dendro_hook",
  year = 2090,
  ssp = 245
)

# exporting the classified raster file
writeRaster(
  ssp245_2090 >= threshold_value,
  filename = "dendro_hook_classified_ssp245_2090.tif",
  overwrite=TRUE
)


### ssp585 

#### 2050 
ssp585_2050 <- future_prediction_model(
  model_name = mod_dendro_hook,
  current_rasters = parameter,
  species_name = "dendro_hook",
  year = 2050,
  ssp = 585
)


# exporting the classified raster file
writeRaster(
  ssp585_2050 >= threshold_value,
  filename = "dendro_hook_classified_ssp585_2050.tif",
  overwrite=TRUE
)


#### 2070 
ssp585_2070 <- future_prediction_model(
  model_name = mod_dendro_hook,
  current_rasters = parameter,
  species_name = "dendro_hook",
  year = 2070,
  ssp = 585
)


# exporting the classified raster file
writeRaster(
  ssp585_2070 >= threshold_value,
  filename = "dendro_hook_classified_ssp585_2070.tif",
  overwrite=TRUE
)


#### 2090 
ssp585_2090 <- future_prediction_model(
  model_name = mod_dendro_hook,
  current_rasters = parameter,
  species_name = "dendro_hook",
  year = 2090,
  ssp = 585
)


# exporting the classified raster file
writeRaster(
  ssp585_2090 >= threshold_value,
  filename = "dendro_hook_classified_ssp585_2090.tif",
  overwrite=TRUE
)



## Relative variable Importance ----------


importance_variable <- variable_imp_function(
  model_name = mod_dendro_hook,
  species_name = "dendro_hook"
)


## Response Curves -------------------

# Example usage:
response_curve_function(
  model_name = mod_dendro_hook,      # Your model
  variable_df = importance_variable,  # Data frame with predictor variables
  species_name = "dendro_hook"       # Species name
)

## Saving ROC-AUC- Plots -------------

save_roc_plots(model_name = mod_dendro_hook,
               species_name = "dendro_hook",
               methods = models_used)


## Final Plots ----------


sdm_plot(shape_file = nepal, 
         species_name = "dendro_hook", 
         ssp = 245, 
         year = 2050)


sdm_plot(shape_file = nepal, 
         species_name = "dendro_hook", 
         ssp = 245, 
         year = 2070)

sdm_plot(shape_file = nepal, 
         species_name = "dendro_hook", 
         ssp = 245, 
         year = 2090)


sdm_plot(shape_file = nepal, 
         species_name = "dendro_hook", 
         ssp = 585, 
         year = 2050)


sdm_plot(shape_file = nepal, 
         species_name = "dendro_hook", 
         ssp = 585, 
         year = 2070)

sdm_plot(shape_file = nepal, 
         species_name = "dendro_hook", 
         ssp = 585, 
         year = 2090)
