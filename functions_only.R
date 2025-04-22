# Library Function --------

library_function <- function(
){
  library(sf)
  library(usdm)
  library(sdm)
  library(ggplot2)
  library(dplyr)
  library(dismo)
  library(readxl)
  library(rJava)
  library(readxl)
  extrafont::loadfonts(quiet = T)
}


# Modeling Function ----------
run_test_model <- function(
    occurrence_df,
    predictor_variables
){
  
  occurrence_df$Species <- 1
  coordinates(occurrence_df) <- c("LON", "LAT")
  extracted_cells <- raster::extract(predictor_variables, occurrence_df)
  collinearity_test <- vifcor(extracted_cells, th = 0.7)
  print(collinearity_test)
  bio <- usdm::exclude(predictor_variables, collinearity_test)
  
  data_model <- sdmData(Species ~., 
                        occurrence_df, 
                        predictors = bio, 
                        bg = list(method = "gRandom",
                                  n= 10000))
  print(data_model)
  
  model_run <- sdm(Species ~., 
                   data = data_model,
                   methods = c(
                     'maxent',
                     'rf',
                     'brt',
                     'fda',
                     'svm',
                     'cart',
                     'brt',
                     'mars',
                     'glm', 
                     'gam'
                   ), 
                   replication = 'boot', 
                   test.p = 20,
                   n= 5)
  
  print(model_run)
  return(model_run)
}



## Final Model -------------
run_final_model <- function(
    occurrence_df,
    predictor_variables,
    replication_number, 
    model_names
){
  
  occurrence_df$Species <- 1
  coordinates(occurrence_df) <- c("LON", "LAT")
  extracted_cells <- raster::extract(predictor_variables, occurrence_df)
  collinearity_test <- vifcor(extracted_cells, th = 0.7)
  print(collinearity_test)
  bio <- usdm::exclude(predictor_variables, collinearity_test)
  
  data_model <- sdmData(Species ~., 
                        occurrence_df, 
                        predictors = bio, 
                        bg = list(method = "gRandom",
                                  n= 10000))
  print(data_model)
  
  model_run <- sdm(Species ~., 
                   data = data_model,
                   methods = model_names, 
                   replication = 'boot', 
                   test.p = 20,
                   n= replication_number)
  
  print(model_run)
  return(model_run)
}



# Threshold Evaluation function ------------------------

evaluation_function <- function(model_name,
                                species_name){
  
  values <- getEvaluation(model_name, stat = c("AUC", "TSS", "threshold", "kappa"), opt = 2)
  values
  
  
  # creating and saving in working directory
  saving_directory <- paste0("D:/Drive/Bamboo_SDM/", species_name, "/result_files")
  dir.create(saving_directory)
  setwd(saving_directory)
  
  csv_name <- paste0(species_name, "_evaluation_parameters.csv")
  
  write.csv(
    values,
    csv_name
  )
  
  
  threshold <- mean(values$threshold)
  return(threshold)
  
}






# Current and future Prediction function ------------------


 current_prediction_model <- function(model_name, stacked_predictor, species_name) {
  
  
  # Predict the current distribution
  predict_current <- predict(model_name, stacked_predictor)
  
  # Ensemble the predictions
  ensemble_current <- ensemble(
    model_name,
    predict_current,
    setting = list(method = 'weighted', stat = 'tss', opt = 2)
  )
  
  # creating and saving in working directory
  saving_directory <- paste0("D:/Drive/Bamboo_SDM/", species_name, "/final_plots/raster_plots")
  dir.create(saving_directory)
  setwd(saving_directory)
  
  
  # Save the current ensemble raster
  unclassified_name <- paste0(species_name, "_unclassified_current.tif")
  writeRaster(ensemble_current, filename = unclassified_name, overwrite = TRUE)
  
  # Return the present ensemble object and threshold value for future use
  return(ensemble_current)
 }
 
 
 
 
future_prediction_model <- function(
    model_name,
    current_rasters,
    species_name,
    year,
    ssp) {
   
   directory <- paste0("D:/MISC/MaxEnt SDM/data/final_data/", "ssp", ssp,"/year_", year)
   
   raster_files <- list.files(directory, pattern = "\\.tif$", full.names = TRUE)
   
   future_parameters <- stack(raster_files)
   
   names(future_parameters) <- names(current_rasters)
   
   ensemble_future <- ensemble(
     model_name,
     future_parameters,
     setting = list(method = 'weighted', stat = 'tss', opt = 2)
   )
   
   # creating and saving in working directory
   saving_directory <- paste0("D:/Drive/Bamboo_SDM/", species_name, "/final_plots/raster_plots")
   dir.create(saving_directory)
   setwd(saving_directory)
   
   
   raster_name <- paste0(species_name, "_unclassified_ssp_", ssp, "_", year, ".tif")
   
   writeRaster(
     ensemble_future,
     raster_name,
     overwrite = T
   )
   
   return(ensemble_future)
   
 }
 
 
 # Relative Variable Importance -----------
 
 variable_imp_function <- function(model_name, species_name){
   
   
   variable_importance_df <- getVarImp(model_name)@varImportanceList[["1"]]@varImportance
   
   # Create a new column 'variable_name' with the renamed parameters
   variable_importance_df <- variable_importance_df %>%
     mutate(variable_name = case_when(
       variables == "bio1_current" ~ "bio1",
       variables == "bio2_current" ~ "bio2",
       variables == "bio3_current" ~ "bio3",
       variables == "bio4_current" ~ "bio4",
       variables == "bio5_current" ~ "bio5",
       variables == "bio6_current" ~ "bio6",
       variables == "bio7_current" ~ "bio7",
       variables == "bio8_current" ~ "bio8",
       variables == "bio9_current" ~ "bio9",
       variables == "bio10_current" ~ "bio10",
       variables == "bio11_current" ~ "bio11",
       variables == "bio12_current" ~ "bio12",
       variables == "bio13_current" ~ "bio13",
       variables == "bio14_current" ~ "bio14",
       variables == "bio15_current" ~ "bio15",
       variables == "bio16_current" ~ "bio16",
       variables == "bio17_current" ~ "bio17",
       variables == "bio18_current" ~ "bio18",
       variables == "bio19_current" ~ "bio19",
       variables == "slope" ~ "slope",
       variables == "aspect" ~ "aspect",
       variables == "elevation" ~ "elevation",
         # Handle any cases that don't match
     ))
   
   
   # creating and saving in working directory
   saving_directory <- paste0("D:/Drive/Bamboo_SDM/", species_name, "/result_files")
   dir.create(saving_directory)
   setwd(saving_directory)
   csv_name <- paste0(species_name, "_variable_importance.csv")
   
   write.csv(
     variable_importance_df,
     csv_name
   )
   
   # plotting 
   var_plot <- ggplot(data = variable_importance_df,
                      aes(x =  variable_name, 
                          y = corTest
                      ))+
     geom_col(
       fill = "black",
       width = 0.7
     )+
     coord_flip()+
     theme_minimal()+
     labs(
       y = "Relative Variable Importance",
       x = "Variables"
     )+
     theme(
       axis.text = element_text(color = "black",
                                family = "Segoe UI",
                                size = 10.5),
       axis.title = element_text(color = "black",
                                 family = "Segoe UI",
                                 face = "bold",
                                 size = 11),
       panel.grid.minor = element_blank(),
       panel.grid.major.y = element_blank(),
       panel.grid.major.x = element_line(color = "darkgrey")
     )
   
   var_plot
   
   # creating and saving in working directory
   saving_directory <- paste0("D:/Drive/Bamboo_SDM/", species_name, "/final_plots/plots")
   dir.create(saving_directory)
   setwd(saving_directory)
   
   plot_name <- paste0(species_name, "_variable_importance.svg")
   
   ggsave(
     filename = plot_name,  # First argument should be the file name
     plot = var_plot,       # Second argument should be the plot object
     width = 5,
     height = 2.5
   )
   
   return(variable_importance_df)
   
 }
 

 # Response curves -----------------
 
 response_curve_function <- function(
    model_name,
    variable_df,
    species_name
 ) {
   # Loop through each row of the variable_df
   for (i in 1:nrow(variable_df)) {
     # Get the predictor name and the corresponding variable name
     predictor_name <- variable_df$variables[i]
     variable_name <- variable_df$variable_name[i]
     
     # Generate the response curve for each predictor variable
     response_curve_plot <- rcurve(model_name, n = predictor_name) +
       theme_bw() +
       theme(
         panel.grid.major = element_blank(),
         title = element_blank(),
         axis.text = element_text(color = "black",
                                  family = "Segoe UI",
                                  size = 10.5),
         axis.title = element_text(color = "black",
                                   family = "Segoe UI",
                                   face = "bold",
                                   size = 11),
         strip.background = element_rect(fill = "transparent")
       )
     
     
     # creating and saving in working directory
     saving_directory <- paste0("D:/Drive/Bamboo_SDM/", species_name, "/final_plots/plots")
     dir.create(saving_directory)
     setwd(saving_directory)
     
     
     # Define the file name using variable_name
     file_name <- paste0(variable_name, "_", species_name, ".svg")
     
     # Save the plot with the corresponding file name
     ggsave(
       filename = file_name,
       plot = response_curve_plot,  # Specify the plot to save
       width = 6,  # Adjust width and height as needed
       height = 4
     )
     
     print(paste0("Saved response curve for: ", variable_name))
   }
 }
 

 
 ### Roc - auc function ------------

save_roc_plots <- function(model_name, methods, species_name, width = 500, height = 400) {
  # Iterate over each method and generate the ROC plot
  for (method in methods) {
    # Generate the ROC-AUC plot for the current method
    roc(model_name, method = method, smooth = TRUE)
    
    # creating and saving in working directory
    saving_directory <- paste0("D:/Drive/Bamboo_SDM/", species_name, "/final_plots/plots")
    dir.create(saving_directory)
    setwd(saving_directory)
    
    
    # Define the filename for the plot
    filename <- paste0(method,"_", species_name, ".jpg")
    
    # Save the plot as a JPEG
    dev.print(device = jpeg,              # Save to JPEG
              filename = filename,        # Filename based on method
              width = width,              # Width of image
              height = height)            # Height of image
    
    # Inform the user
    message(paste("Saved ROC plot for method:", method))
  }
}
 
# Final Maps -----------

# future plots -----------
 
 sdm_plot <- function(shape_file, species_name, ssp, year){
   
   current_directory <- paste0("D:/Drive/Bamboo_SDM/", species_name, "/final_plots/raster_plots/", species_name, "_classified_current.tif")
   future_directory <- paste0("D:/Drive/Bamboo_SDM/", species_name, "/final_plots/raster_plots/", species_name, "_classified_ssp", ssp, "_", year, ".tif")
   
   current_raster <- raster::raster(current_directory)
   future_raster <- raster::raster(future_directory)
   
   #stacking the file
   stacked_file <- stack(current_raster, future_raster)
   
   # converting into data frame
   stacked_df <- stacked_file %>% 
     as.data.frame(xy = TRUE) %>% 
     na.omit()
   
   # with the pipe operation of magrittr function we can just put . to select the df. 
   stacked_df <- stacked_df %>%
     mutate(change = case_when(
       .[[3]] == "0" & .[[4]] == "0" ~ "0",
       .[[3]] == "0" & .[[4]] == "1" ~ "GAIN",
       .[[3]] == "1" & .[[4]] == "0" ~ "LOSS",
       .[[3]] == "1" & .[[4]] == "1" ~ "NO CHANGE",
     ))
   
   
   # printing the change values. 
   print(as.data.frame(table(stacked_df$change)))
   
   # Define the color value based on the SSP scenario
   color_value <- if (ssp == 585) {
     "#AA188F"
   } else if (ssp == 245) {
     "#FF8600"
   } else {
     "#000000" # Default color in case of an unexpected SSP
   }
   
   # plotting using ggplot2 
   ggplot() +
     geom_raster(
       data = stacked_df,
       aes(x = x, y = y, fill = change)
     ) +
     geom_sf(
       data = shape_file,
       fill = "transparent",
       color = color_value,
       linewidth = 0.4
     )+
     scale_fill_manual(
       values = c(
         "LOSS" = "red",
         "GAIN" = "blue",
         "NO CHANGE" = "green",
         "0" = "white"
       )
     )+
     guides(
       fill =  "none"
     )+
     theme_void()+
     theme(
       plot.background = element_rect(fill = "transparent", color = "transparent"),
       panel.background = element_rect(fill = "transparent", color = "transparent")
     )
   
   # creating and saving in working directory
   saving_directory <- paste0("D:/Drive/Bamboo_SDM/", species_name, "/final_plots/maps")
   dir.create(saving_directory)
   setwd(saving_directory)
   
   name_plot <- paste0(species_name, "_ssp", ssp, "_", year, ".svg")
   
   
   ggsave(name_plot,
          height = 3,
          width = 6)
 }
 
