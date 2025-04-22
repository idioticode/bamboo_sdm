# data extraction and tables 


# PIXEL COUNT FINAL  ------------------

zero_one_values <- function() {
  # Define the list of species
  species_list <- c("bam_alam", "bam_bal", "bam_nep", "bam_nut_cup", "bam_nut_nut", "dendro_hamil", "dendro_hook")
  # Initialize an empty list to store results for each species
  final_binded_value <- list()
  
  # Loop through each species
  for (species in species_list) {
    # Read data for the current species
    bam_data <- readxl::read_excel(
      "D:/Drive/Bamboo_SDM/WORKING DOCUMENTS/final_pixel_analysis.xlsx",
      sheet = species
    )
    
    # Define columns of interest to analyze
    columns_of_interest <- c(
      paste0(species, "_current"),
      paste0(species, "_classified_ssp245_2050"),
      paste0(species, "_classified_ssp245_2070"),
      paste0(species, "_classified_ssp245_2090"),
      paste0(species, "_classified_ssp585_2050"),
      paste0(species, "_classified_ssp585_2070"),
      paste0(species, "_classified_ssp585_2090"),
      paste0("change_ssp245_2050")
    )
    
    # Initialize a temporary list to store results for each column in the species
    species_values <- list()
    
    # Loop through each column of interest
    for (col in columns_of_interest) {
      if (col %in% colnames(bam_data)) {
        # Count 0s and 1s and store in a data frame
        value_counts <- as.data.frame(table(bam_data[[col]]))
        value_counts$metadata <- col
        value_counts$species <- species
        
        # Append the result for this column to the species values
        species_values[[col]] <- value_counts
      }
    }
    
    # Bind all results for the species
    final_binded_value[[species]] <- do.call(rbind, species_values)
  }
  
  # Bind all species results into a single data frame
  final_result <- do.call(rbind, final_binded_value)
  return(final_result)
}

# Run the function
final_values <- zero_one_values()



# GAIN LOSS DATA -------------


library(dplyr)
library(readxl)

gain_loss_values <- function() {
  # Define species list
  species_list <- c("bam_alam", "bam_bal", "bam_nep", "bam_nut_cup", "bam_nut_nut", "dendro_hamil", "dendro_hook")
  
  # Initialize an empty list to store results for each species
  final_binded_values <- list()
  
  # Loop over each species
  for (species in species_list) {
    # Read the Excel data for the current species
    species_data <- readxl::read_excel(
      "D:/Drive/Bamboo_SDM/WORKING DOCUMENTS/raster_to_excel_files/pixel_data_frame_final.xlsx",
      sheet = species
    )
    
    # Define changes for each scenario using mutate and case_when
    scenarios <- list(
      change_ssp245_2050 = case_when(
        species_data[[3]] == "0" & species_data[[4]] == "0" ~ "0",
        species_data[[3]] == "0" & species_data[[4]] == "1" ~ "GAIN",
        species_data[[3]] == "1" & species_data[[4]] == "0" ~ "LOSS",
        species_data[[3]] == "1" & species_data[[4]] == "1" ~ "NO CHANGE",
        TRUE ~ "Other"
      ),
      change_ssp245_2070 = case_when(
        species_data[[3]] == "0" & species_data[[5]] == "0" ~ "0",
        species_data[[3]] == "0" & species_data[[5]] == "1" ~ "GAIN",
        species_data[[3]] == "1" & species_data[[5]] == "0" ~ "LOSS",
        species_data[[3]] == "1" & species_data[[5]] == "1" ~ "NO CHANGE",
        TRUE ~ "Other"
      ),
      change_ssp245_2090 = case_when(
        species_data[[3]] == "0" & species_data[[6]] == "0" ~ "0",
        species_data[[3]] == "0" & species_data[[6]] == "1" ~ "GAIN",
        species_data[[3]] == "1" & species_data[[6]] == "0" ~ "LOSS",
        species_data[[3]] == "1" & species_data[[6]] == "1" ~ "NO CHANGE",
        TRUE ~ "Other"
      ),
      change_ssp585_2050 = case_when(
        species_data[[3]] == "0" & species_data[[7]] == "0" ~ "0",
        species_data[[3]] == "0" & species_data[[7]] == "1" ~ "GAIN",
        species_data[[3]] == "1" & species_data[[7]] == "0" ~ "LOSS",
        species_data[[3]] == "1" & species_data[[7]] == "1" ~ "NO CHANGE",
        TRUE ~ "Other"
      ),
      change_ssp585_2070 = case_when(
        species_data[[3]] == "0" & species_data[[8]] == "0" ~ "0",
        species_data[[3]] == "0" & species_data[[8]] == "1" ~ "GAIN",
        species_data[[3]] == "1" & species_data[[8]] == "0" ~ "LOSS",
        species_data[[3]] == "1" & species_data[[8]] == "1" ~ "NO CHANGE",
        TRUE ~ "Other"
      ),
      change_ssp585_2090 = case_when(
        species_data[[3]] == "0" & species_data[[9]] == "0" ~ "0",
        species_data[[3]] == "0" & species_data[[9]] == "1" ~ "GAIN",
        species_data[[3]] == "1" & species_data[[9]] == "0" ~ "LOSS",
        species_data[[3]] == "1" & species_data[[9]] == "1" ~ "NO CHANGE",
        TRUE ~ "Other"
      )
    )
    
    # Create frequency tables for each scenario
    scenario_tables <- lapply(names(scenarios), function(scenario) {
      data_frame <- as.data.frame(table(scenarios[[scenario]]))
      data_frame$metadata <- scenario  # Add metadata column
      data_frame$species <- species    # Add species column for identification
      return(data_frame)
    })
    
    # Combine the tables for the current species
    species_combined <- do.call(rbind, scenario_tables)
    
    # Append species_combined to the final list
    final_binded_values[[species]] <- species_combined
  }
  
  # Combine all species' data into a single data frame
  final_binded_values_df <- do.call(rbind, final_binded_values)
  
  return(final_binded_values_df)
}

# Run the function and store the result
gain_loss_final <- gain_loss_values()


# HOTSPOT/SPECIES RICHNESS DATA EXTRACTION -----------------------

overall_values <- readxl::read_excel(
  "D:/Drive/Bamboo_SDM/WORKING DOCUMENTS/raster_to_excel_files/pixel_overall_df.xlsx"
)

names(overall_values)

library(dplyr)

# Summarize columns by grouping them based on pattern matches
hotspot_values <- overall_values %>%
  mutate(
    # Sum all "current" columns
    current_sum = rowSums(select(., contains("current")), na.rm = TRUE),
    
    # Sum all "ssp245_2050" columns
    ssp245_2050_sum = rowSums(select(., contains("ssp245_2050")), na.rm = TRUE),
    
    # Sum all "ssp245_2070" columns
    ssp245_2070_sum = rowSums(select(., contains("ssp245_2070")), na.rm = TRUE),
    
    # Sum all "ssp245_2090" columns
    ssp245_2090_sum = rowSums(select(., contains("ssp245_2090")), na.rm = TRUE),
    
    # Sum all "ssp585_2050" columns
    ssp585_2050_sum = rowSums(select(., contains("ssp585_2050")), na.rm = TRUE),
    
    # Sum all "ssp585_2070" columns
    ssp585_2070_sum = rowSums(select(., contains("ssp585_2070")), na.rm = TRUE),
    
    # Sum all "ssp585_2090" columns
    ssp585_2090_sum = rowSums(select(., contains("ssp585_2090")), na.rm = TRUE)
  )


# Select x, y, and the newly created summary columns
selected_values <- hotspot_values %>%
  select(x, y, current_sum, ssp245_2050_sum, ssp245_2070_sum, ssp245_2090_sum,
         ssp585_2050_sum, ssp585_2070_sum, ssp585_2090_sum)


writexl::write_xlsx(
  selected_values,
  "D:/Drive/Bamboo_SDM/WORKING DOCUMENTS/hotspot_species_richnes.xlsx"
)



# RASTER TO DATA FRAME TO TABLE ---------------------------------------------------
# Extracting the raster values in the form of data frame to save in excel. 
# Load necessary libraries
library(raster)
library(dplyr)

# Define the function to load, stack, and convert rasters to a data frame
load_and_stack_rasters <- function(species_list, ssp_list, year_list) {
  
  # Initialize an empty list to store all raster paths
  raster_paths <- list()
  
  # Loop through each species, SSP, and year to collect raster paths
  for (species_name in species_list) {
    # Path for the current raster
    current_directory <- paste0("D:/Drive/Bamboo_SDM/", species_name, "/final_plots/raster_plots/", species_name, "_classified_current.tif")
    raster_paths <- append(raster_paths, current_directory)
    
    # Paths for future rasters (for each SSP and year)
    for (ssp in ssp_list) {
      for (year in year_list) {
        future_directory <- paste0("D:/Drive/Bamboo_SDM/", species_name, "/final_plots/raster_plots/", species_name, "_classified_ssp", ssp, "_", year, ".tif")
        raster_paths <- append(raster_paths, future_directory)
      }
    }
  }
  
  # Load all rasters and stack them
  raster_list <- lapply(raster_paths, raster::raster)
  raster_stack <- raster::stack(raster_list)
  
  # Convert the stacked raster to a data frame and remove NAs
  stacked_df <- as.data.frame(raster_stack, xy = TRUE) %>% na.omit()
  
  # Return the stacked raster and the data frame
  list(raster_stack = raster_stack, data_frame = stacked_df)
}

# Define your lists of species, SSPs, and years
species_list <- c("bam_alam", "bam_bal", "bam_nep", "bam_nut_cup", "bam_nut_nut", "dendro_hamil", "dendro_hook")
ssp_list <- c(245, 585)
year_list <- c(2050, 2070, 2090)

# Run the function
result <- load_and_stack_rasters(species_list, ssp_list, year_list)

# Extract the stacked raster and data frame from the result
stacked_raster <- result$raster_stack
stacked_data_frame <- result$data_frame

# View the resulting data frame
print(stacked_data_frame)

colnames(stacked_data_frame)

# extracting as an excel file. 
writexl::write_xlsx(
  stacked_data_frame,
  "D:/Drive/Bamboo_SDM/WORKING DOCUMENTS/pixel_overall_df.xlsx"
)


# PIXELS PER PHYSIOGRAPHIC REGION --------------------

# Function to process current raster files for each province and species
process_current_rasters <- function(shape_file, column_name) {
  
  # List of species names (constant)
  species_list <- c("bam_alam", "bam_bal", "bam_nep", 
                    "bam_nut_cup", "bam_nut_nut", 
                    "dendro_hamil", "dendro_hook")
  
  # Empty data frame to store the results
  all_pixel_counts_current <- data.frame()
  
  # Loop over each species
  for (species_name in species_list) {
    
    # Load current raster file
    current_raster <- raster::raster(
      paste0("D:/Drive/Bamboo_SDM/", species_name, "/final_plots/raster_plots/", species_name, "_classified_current.tif")
    )
    
    # Loop over each province
    for (province_name in unique(shape_file[[column_name]])) {
      
      # Filter the province
      province <- shape_file %>% 
        filter(!!sym(column_name) == province_name)
      
      # Crop and mask the current raster for this province
      cropped_current <- crop(current_raster, province)
      masked_current <- mask(cropped_current, province)
      
      # Convert to data frame and count pixels
      province_df_current <- masked_current %>% as.data.frame(xy = TRUE) %>% na.omit()
      pixel_frequency_current <- table(province_df_current$ensemble_weighted) %>% as.data.frame()
      
      
      # Add metadata columns
      pixel_frequency_current <- pixel_frequency_current %>%
        mutate(
          species = species_name,
          column_string = province_name,
          ssp = NA,
          year = "current"
        )
      
      # Append to the final data frame
      all_pixel_counts_current <- bind_rows(all_pixel_counts_current, pixel_frequency_current)
    }
  }
  
  return(all_pixel_counts_current)
}


current_pixel_count <- process_current_rasters(
  shape_file = physiography_nepal,
  column_name = "Physio", # name of the column in the shape file based on which area is calculated
)

current_pixel_count <- current_pixel_count |> filter(Var1 == "1")




# for future time period
# Function to process future raster files for each province, species, SSP, and year
process_future_rasters <- function(shape_file, column_name) {
  
  # List of species names, SSP scenarios, and future years
  species_list <- c("bam_alam", "bam_bal", "bam_nep", 
                    "bam_nut_cup", "bam_nut_nut", 
                    "dendro_hamil", "dendro_hook")
  ssp_list <- c(245, 585)
  year_list <- c(2050, 2070, 2090)
  
  # Empty data frame to store the results
  all_pixel_counts_future <- data.frame()
  
  # Loop over each species, SSP, and year
  for (species_name in species_list) {
    for (ssp in ssp_list) {
      for (year in year_list) {
        
        # Construct the file path and check if it exists
        future_raster_path <- paste0("D:/Drive/Bamboo_SDM/", species_name, "/final_plots/raster_plots/", species_name, "_classified_ssp", ssp, "_", year, ".tif")
        
        # Load the future raster
        future_raster <- raster::raster(future_raster_path)
        
        # Loop over each province
        for (province_name in unique(shape_file[[column_name]])) {
          
          # Filter the province
          province <- shape_file %>% 
            filter(!!sym(column_name) == province_name)
          
          # Crop and mask the future raster for this province
          cropped_future <- crop(future_raster, province)
          masked_future <- mask(cropped_future, province)
          
          # Check if masked_future has data
          
          # Convert to data frame and count pixels
          province_df_future <- masked_future %>% as.data.frame(xy = TRUE) %>% na.omit()
          
          
          # Count pixel values for `ensemble_weighted`
          names(province_df_future)[3] <- "pixel_parameters"
          
          pixel_frequency_future <- table(province_df_future$pixel_parameters) %>% as.data.frame()
          
          # Add metadata columns
          pixel_frequency_future <- pixel_frequency_future %>%
            mutate(
              species = species_name,
              column_string = province_name,
              ssp = paste0("ssp", ssp),
              year = year
            )
          
          # Append to the final data frame
          all_pixel_counts_future <- bind_rows(all_pixel_counts_future, pixel_frequency_future)
        }
      }
    }
  }
  
  return(all_pixel_counts_future)
}


future_pixel_count <- process_future_rasters(
  shape_file = physiography_nepal,
  column_name = "Physio"
)

future_pixel_count <- future_pixel_count |> dplyr::filter(Var1 == "1")

binded_values <- rbind(current_pixel_count, future_pixel_count)
binded_values

write.csv(binded_values,
          "pixels_physiography.csv")


# DISTRICT PIXEL COUNT/ MAP IN WEBSITE ---------------


district_nepal <- sf::st_read(
  "D:/MISC/points/hermes shape files/hermes_NPL_new_wgs_2.shp"
)

# Function to process each province for all species
frequency_count <- function(shape_file, column_name) {
  
  # List of species names (constant)
  species_list <- c("bam_alam", "bam_bal", "bam_nep", 
                    "bam_nut_cup", "bam_nut_nut", 
                    "dendro_hamil", "dendro_hook")
  
  # Empty data frame to store the results for all species
  all_pixel_counts <- data.frame()
  
  # Loop over each species
  for (species_name in species_list) {
    
    pixel_counts <- data.frame() # Empty data frame for each species
    
    # Load the raster using raster() function
    current_raster <- raster::raster(
      paste0("D:/Drive/Bamboo_SDM/", species_name, "/final_plots/raster_plots/", species_name, "_classified_current.tif")
    )
    
    # Loop over each province
    for (i in unique(shape_file[[column_name]])) {
      
      # Filter the province
      province <- shape_file %>% 
        filter(!!sym(column_name) == i)
      
      # Crop and mask raster for this province
      cropped <- crop(current_raster, province)
      masked <- mask(cropped, province)
      
      # Convert to data frame and count pixels
      province_df <- masked %>% as.data.frame(xy = TRUE) %>% na.omit()
      pixel_frequency <- table(province_df$ensemble_weighted) %>% as.data.frame()
      
      # Exclude 0 values
      pixel_frequency <- pixel_frequency %>%
        filter(Freq != 0) %>%
        mutate(province = unique(shape_file %>% 
                                   filter(!!sym(column_name) == i) %>% 
                                   pull(!!sym(column_name))))
      
      # Append the results for pixel presence (Var1 == 1)
      pixel_counts <- bind_rows(pixel_counts, pixel_frequency)
      pixel_counts_presence <- pixel_counts |> 
        dplyr::filter(Var1 == 1) |> # Only keep presence values
        dplyr::select(c("Freq", "province")) |>
        mutate(species = species_name) # Add the species name column
    }
    
    # Append results for each species to the final data frame
    all_pixel_counts <- bind_rows(all_pixel_counts, pixel_counts_presence)
  }
  
  return(all_pixel_counts)
}

district_count <- frequency_count(
  shape_file = district_nepal,
  column_name = "DISTRICT"
)


# Print the combined data
print(district_count)

write.csv(district_count,
          "district_count.csv")

# Assuming binded_values is your data frame
district_matrix <- district_count %>%
  pivot_wider(
    names_from = species,  # Columns will be species names
    values_from = Freq     # Values will be the frequencies
  )

# Replace any NA values with 0 (optional)
district_matrix[is.na(district_matrix)] <- 0

# Print the matrix
print(district_matrix)

# Function to calculate species richness (number of non-zero species per row)
district_matrix <- district_matrix %>%
  rowwise() %>%
  mutate(
    species_richness = sum(c_across(bam_alam:dendro_hook) > 0) # Count the non-zero values across species columns
  )

# Print the updated district matrix with species richness
print(district_matrix)

write.csv(
  district_matrix,
  "district_matrix_frequency.csv"
)


# Extracting hotspot raster file ==================


library(raster)

# Function to create hotspot raster for specified scenario and year
create_hotspot_raster <- function(ssp = NULL, year = NULL) {
  # Define species names
  species_name <- c("bam_alam", "bam_bal", "bam_nep", "bam_nut_cup", "bam_nut_nut", "dendro_hamil", "dendro_hook")
  
  # Check if it is for the current scenario
  if (is.null(ssp) && is.null(year)) {
    # Load current rasters
    rasters <- lapply(species_name, function(species) {
      raster(paste0("D:/Drive/Bamboo_SDM/", species, "/final_plots/raster_plots/", species, "_classified_current.tif"))
    })
    
    # Sum the rasters to create a combined raster for current scenario
    combined_raster <- Reduce(`+`, rasters)
    
    # Write the combined raster for the current scenario
    writeRaster(combined_raster, "D:/Drive/Bamboo_SDM/HOTSPOT RASTER FILES/hotspot_species_richness_current.tif", overwrite = TRUE)
    
  } else {
    # Validate inputs for future scenarios
    if (!ssp %in% c(245, 585) || !year %in% c(2050, 2070, 2090)) {
      stop("Invalid SSP or year. SSP should be 245 or 585, and year should be 2050, 2070, or 2090.")
    }
    
    # Load rasters for the specified SSP and year
    rasters <- lapply(species_name, function(species) {
      raster(paste0("D:/Drive/Bamboo_SDM/", species, "/final_plots/raster_plots/", species, "_classified_ssp", ssp, "_", year, ".tif"))
    })
    
    # Sum the rasters to create a combined raster for the specified SSP and year
    combined_raster <- Reduce(`+`, rasters)
    
    # Write the combined raster for the specified SSP and year
    output_file <- paste0("D:/Drive/Bamboo_SDM/HOTSPOT RASTER FILES/hotspot_species_richness_ssp", ssp, "_", year, ".tif")
    writeRaster(combined_raster, output_file, overwrite = TRUE)
  }
}

# Usage examples:
# To create the current scenario hotspot raster
create_hotspot_raster()

# To create the hotspot raster for SSP 245 
create_hotspot_raster(ssp = 245, year = 2050)

create_hotspot_raster(ssp = 245, year = 2070)

create_hotspot_raster(ssp = 245, year = 2090)

# To create the hotspot raster for SSP 585
create_hotspot_raster(ssp = 585, year = 2050)

create_hotspot_raster(ssp = 585, year = 2070)

create_hotspot_raster(ssp = 585, year = 2090)


# hotspot/species richness frequency zonation for districts table -------------

# Load necessary libraries
library(raster)
library(readxl)
library(dplyr)

#shape file
district_nepal <- sf::st_read(
  "D:/MISC/points/hermes shape files/hermes_NPL_new_wgs_2.shp"
)

# Frequency count function to analyze raster classes by district
frequency_count <- function(raster_file, shape_file, district_column, metadata_label) {
  
  # Ensure shape file has only XY dimensions
  shape_file <- sf::st_zm(shape_file)
  
  # Initialize an empty data frame to store frequency counts
  pixel_counts <- data.frame()
  
  # Loop over each unique district
  for(district in unique(shape_file[[district_column]])) {
    
    # Filter shape file for the current district
    district_shape <- shape_file %>% filter(!!sym(district_column) == district)
    
    # Crop and mask the raster by the current district
    cropped <- crop(raster_file, district_shape)
    masked <- mask(cropped, district_shape)
    
    # Convert masked raster to a data frame and omit NA values
    raster_data <- as.data.frame(masked, xy = TRUE) %>% na.omit()
    
    # Rename the third column to "class" for easier handling
    colnames(raster_data)[3] <- "class"
    
    # Calculate frequency of each class (0 or 1)
    class_counts <- table(raster_data$class) %>% as.data.frame()
    colnames(class_counts) <- c("class", "Freq")
    
    class_counts <- class_counts |> dplyr::filter(class != 0)
    
    # Add district and metadata information
    class_counts[[district_column]] <- district
    class_counts$metadata <- metadata_label
    
    # Append results to the main data frame
    pixel_counts <- bind_rows(pixel_counts, class_counts)
  }
  
  return(pixel_counts)
}

# Main function to aggregate frequency counts for multiple raster files
hotspot_excel_file <- function(shape_file, raster_files, district_column) {
  
  # Data frame to store all frequency counts
  richness_counts <- data.frame()
  
  # Loop through each raster file and calculate frequencies
  for (raster_path in raster_files) {
    
    # Load raster
    raster_file <- raster::raster(raster_path)
    
    # Extract metadata from file name (e.g., ssp and year)
    metadata_label <- tools::file_path_sans_ext(basename(raster_path))
    
    # Calculate frequency counts for the current raster file
    frequency_values <- frequency_count(
      raster_file = raster_file,
      shape_file = shape_file,
      district_column = district_column,
      metadata_label = metadata_label
    )
    
    # Append results to the main data frame
    richness_counts <- bind_rows(richness_counts, frequency_values)
  }
  
  return(richness_counts)
}

# Usage example:
# Define the paths to raster files
raster_files <- list.files("D:/Drive/Bamboo_SDM/HOTSPOT RASTER FILES", pattern = "\\.tif$", full.names = TRUE)

# Calculate the richness counts
richness_counts <- hotspot_excel_file(
  shape_file = district_nepal,      # Your shapefile with districts
  raster_files = raster_files,       # List of raster files
  district_column = "DISTRICT"       # Column name in the shapefile for districts
)

# Display the result
head(richness_counts, n = 10)

writexl::write_xlsx(
  richness_counts,
  "D:/Drive/Bamboo_SDM/WORKING DOCUMENTS/hotspot_species_richness_pixel_frequency_district.xlsx"
)


# COUNTING NUMBER OF PIXELS IN EACH DISTRICTS. ------------

# function 

frequency_count <- function(raster_file, shape_file, sf_column_name, raster_column_name) {
  
  shape_file <- sf::st_zm(shape_file)
  pixel_counts <- data.frame()
  
  for (district in unique(shape_file[[sf_column_name]])) {
    
    # Filter for the current district
    district_shape <- shape_file %>%
      filter(!!sym(sf_column_name) == district)
    
    # Crop and mask raster based on district
    cropped <- crop(raster_file, district_shape)
    masked <- mask(cropped, district_shape)
    
    # Convert the masked raster to a data frame, remove NAs
    df <- as.data.frame(masked, xy = TRUE)
    
    # Rename third column for easier reference and handle missing columns
    colnames(df)[3] <- "value"
    
    # Count the frequency of each unique value in the specified column
    pixel_frequency <- table(df$value) %>% as.data.frame()
    colnames(pixel_frequency) <- c("class", "Freq")
    
    # Add district and metadata info
    pixel_frequency[[sf_column_name]] <- district
    pixel_frequency$metadata <- raster_column_name
    
    # Combine with main results
    pixel_counts <- bind_rows(pixel_counts, pixel_frequency)
  }
  
  return(pixel_counts)
}

# shape and raster files 


district_nepal <-  sf::st_read(
  "D:/MISC/points/hermes shape files/hermes_NPL_new_wgs_2.shp"
) |>
  dplyr::select(DISTRICT)

current_hotspot <- raster::raster(
  "D:/Drive/Bamboo_SDM/HOTSPOT RASTER FILES/hotspot_species_richness_current.tif",
)

ssp245_2050_hotspot <- raster::raster(
  "D:/Drive/Bamboo_SDM/HOTSPOT RASTER FILES/hotspot_species_richness_ssp245_2050.tif",
)


# Example call to the function
district_pixels_current <- frequency_count(
  shape_file = district_nepal,               # Your shapefile
  sf_column_name = "DISTRICT",              # Column name in shapefile for districts
  raster_file = current_hotspot,             # Raster file for current hotspots
  raster_column_name = "hotspot_species_richness_current" # Metadata label
)


# Example call to the function
district_pixels_future <- frequency_count(
  shape_file = district_nepal,               # Your shapefile
  sf_column_name = "DISTRICT",              # Column name in shapefile for districts
  raster_file = ssp245_2050_hotspot,             # Raster file for current hotspots
  raster_column_name = "hotspot_species_richness_ssp245_2050" # Metadata label
)

# binding them together and extracting the csv file. 
binded_values <- bind_rows(district_pixels_current, district_pixels_future)

writexl::write_xlsx(
  binded_values,
  "D:/Drive/Bamboo_sdm/district_pixel_counts.xlsx"
)


# CALCULATING AREA OF EACH DISTRICTS ----------------
area_district <- raster::shapefile(
  "D:/MISC/points/hermes shape files/hermes_NPL_new_wgs_2.shp"
)

# adding a new column with the area
area_district$AREA <- raster::area(area_district)/1000000
head(area_district, 10)

# to view the data we need to convert it into the data frame
area_df <- area_district |> as.data.frame()

# exporting the area in the csv format. 
write.csv(
  area_df,
  "D:/Drive/Bamboo_SDM/area_of_districts.csv" # Extension is the must.  
)


# Principle component analysis (PCA) [VARIABLE IMPORTANCE ]---------------

# data table 
var_imp <- readxl::read_excel(
  "D:/Drive/Bamboo_SDM/WORKING DOCUMENTS/variable_importance.xlsx",
  sheet = "result_sheet"
)

var_imp

library(tidyverse)
extrafont::loadfonts(quiet = T)

# Transform data to wide format for PCA or clustering and convert into matrix. 
data_wide <- var_imp %>%
  column_to_rownames(var = "variables") %>% # Set 'variables' column as rownames
  as.matrix()                             # Convert to a numeric matrix

# Option 1: Replace NA with 0
data_wide[is.na(data_wide)] <- 0

# Perform PCA on the wide-format data
pca_result <- prcomp(data_wide, scale. = TRUE)

# Summary of PCA
summary(pca_result)

pca_result



# Get the PCA scores (PCs for each sample)
pca_scores <- as.data.frame(pca_result$x)

pca_scores_rowname <- cbind(RowNames = rownames(pca_scores), pca_scores)

writexl::write_xlsx(
  pca_scores_rowname,
  "D:/Drive/Bamboo_SDM/WORKING DOCUMENTS/PCA_scores.xlsx"
)

# Get the PCA loadings (coefficients of the original variables)
pca_loadings <- as.data.frame(pca_result$rotation)

# Add row names as a new column to the data frame
pca_loadings_with_rownames <- cbind(RowNames = rownames(pca_loadings), pca_loadings)

writexl::write_xlsx(
  pca_loadings_with_rownames,
  "D:/Drive/Bamboo_SDM/WORKING DOCUMENTS/PCA_loadings.xlsx"
)

# Principle component analysis (PCA) [MODEL PERFORMANCE]---------------

# data table 
var_imp_models <- readxl::read_excel(
  "D:/Drive/Bamboo_SDM/WORKING DOCUMENTS/model_performance.xlsx",
  sheet = "tss"
)

var_imp_models

library(tidyverse)
extrafont::loadfonts(quiet = T)

# Transform data to wide format for PCA or clustering and convert into matrix. 
data_wide_models <- var_imp_models %>%
  column_to_rownames(var = "models") %>% # Set 'variables' column as rownames
  as.matrix()                             # Convert to a numeric matrix

# Option 1: Replace NA with 0
data_wide_models[is.na(data_wide_models)] <- 0

# Perform PCA on the wide-format data
pca_result_models <- prcomp(data_wide_models, scale. = TRUE)

# Summary of PCA
summary(pca_result_models)

pca_result_models



# Get the PCA scores (PCs for each sample)
pca_scores_models <- as.data.frame(pca_result_models$x)

pca_scores_rowname_models <- cbind(RowNames = rownames(pca_scores_models), pca_scores_models)

writexl::write_xlsx(
  pca_scores_rowname__models,
  "D:/Drive/Bamboo_SDM/WORKING DOCUMENTS/PCA_scores_models.xlsx"
)

# Get the PCA loadings (coefficients of the original variables)
pca_loadings_models <- as.data.frame(pca_result_models$rotation)

# Add row names as a new column to the data frame
pca_loadings_with_rownames_models <- cbind(RowNames = rownames(pca_loadings_models), pca_loadings_models)

writexl::write_xlsx(
  pca_loadings_with_rownames_models,
  "D:/Drive/Bamboo_SDM/WORKING DOCUMENTS/PCA_loadings_models.xlsx"
)

