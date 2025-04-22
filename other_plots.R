#=================================================-
# PIXEL CHANGE BASED ON SPECIES -----------------
#=================================================-

library(tidyverse)
extrafont::loadfonts(quiet = T)

pixel_change <- readxl::read_excel(
  "D:/Drive/Bamboo_SDM/WORKING DOCUMENTS/final_area.xlsx",
  sheet = "final_pixel_values"
)

pixel_change <- pixel_change |>
  mutate(new_value = Freq/1000)

filtered_current <- pixel_change |>
  dplyr::filter(year == "current")


# plot. 

pixel_change_plot <- pixel_change |>
  ggplot(aes(
    x = year,
    y = new_value,
    group = ssp_type
  )
  )+
  geom_line(
    aes(color = ssp_type, group = ssp_type),
    size = 0.7
  )+
  geom_point(
    aes(color = ssp_type),
    fill = "white",
    shape = 21,
    size = 1.7,   # Adjust point size
    stroke = 1.2 # Adjust outline width
  )+
  geom_point(
    data = filtered_current,
    aes(
      x = year,
      y = new_value,
      group = ssp_type),
    color = "black",
    fill = "black",
    shape = 21,
    size = 1.7,   # Adjust point size
    stroke = 1.5 # Adjust outline width
  )+
  geom_point(
    data = filtered_current,
    aes(
      x = year,
      y = new_value,
      group = ssp_type),
    color = "white",
    fill = "black",
    shape = 21,
    size = 1.7,   # Adjust point size
    stroke = 0.8 # Adjust outline width
  )+
  facet_wrap(
    ~species_name,
    ncol = 4,
    scales = "free_y",  # Allows different y-axis scales for each species
    #labeller = labeller(Species = c("bam_alam" = "a.",  "bam_bal" = "b.", "bam_nep" = "c.", "bam_nut_cup" = "d.", "bam_nut_nut" = "e.", "dendro_hamil" = "f.", "dendro_hook" = "g."))
  )+
  scale_color_manual(
    values = c(
      "ssp245" = "#FF8600",
      "ssp585" = "#AA188F"
    ),
    labels = c(
      "ssp245" = "SSP2-4.5",
      "ssp585" = "SSP5-8.5"
    ),
    name = "Emission Scenario"
  )+
  scale_x_discrete(breaks = c(
    "current",
    "2050",
    "2070",
    "2090"
  ),
  limits = c("current", "2050", "2070", "2090")
  )+
  theme_bw()+
  theme(
    strip.text = element_text(color = "black",
                              family = "Verdana",
                              face = "italic",
                              size = 10),
    legend.position = c(hjust = 0.88, 
                        vjust = 0.25),
    axis.text.x = element_text(color = "black",
                               family = "Verdana",
                               angle = 90,
                               size = 9,
                               hjust = 1),
    axis.text.y = element_text(color = "black",
                               family = "Verdana",
                               size = 9,
                               hjust = 1),
    axis.title = element_text(color = "black",
                              face = "bold",
                              family = "Verdana",
                              size = 9),
    legend.text = element_text(color = "black",
                               family = "Verdana",
                               size = 10),
    legend.title = element_text(color = "black",
                                family = "Verdana",
                                size = 10,
                                face = "bold"),
    legend.background = element_rect(color = "black",
                                     linewidth = 0.3),
    strip.background = element_blank(),
    plot.background = element_rect(color = "black"),
    legend.key = element_rect(fill = "transparent", color = NA)
  )+
  labs(
    x = "Year",
    y = "Number of Pixels (* 1000)"
  )+
  guides(color = guide_legend(override.aes = list(fill = NA))) # to remove the grey background in legend key

pixel_change_plot

ggsave(
  "D:/Drive/Bamboo_SDM/OTHER PLOTS/pixel_change.svg",
  pixel_change_plot,
  width = 7,
  height = 6
)

ggsave(
  "D:/Drive/Bamboo_SDM/OTHER PLOTS/JPEG Images/pixel_change.jpg",
  pixel_change_plot,
  width = 7,
  height = 6,
  dpi = 600
)


#=================================================-
# VARIABLE IMPORTANCE PLOT -----------------
#=================================================-

library(tidyverse)
extrafont::loadfonts(quiet = T)
library(tidytext) # for reordering the x axis values. 


#source("D:/Drive/Bamboo_SDM/R FILES/other_plots.R", echo=TRUE)

# Define the variable importance function
variable_importance <- function(species_list) {
  
  # Initialize an empty list to store data frames for each species
  data_files <- list()
  
  # Loop through each species name in the species list
  for (species_name in species_list) {
    # Construct the file path dynamically based on the species name
    data_file <- read.csv(
      paste0("D:/Drive/Bamboo_SDM/", species_name, "/result_files/", species_name, "_variable_importance.csv")
    )
    
    # Add a new column to identify the species in the data
    data_file <- dplyr::mutate(data_file, species_code = species_name)
    
    # Append each species data frame to the list
    data_files[[species_name]] <- data_file
  }
  
  # Bind all data frames in the list into a single data frame
  binded_data <- dplyr::bind_rows(data_files)
  
  # Return the final combined data frame
  return(binded_data)
}


# Define the species list outside of the function to make it reusable
species_list <- c("bam_alam", "bam_bal", "bam_nep", "bam_nut_cup", "bam_nut_nut", "dendro_hamil", "dendro_hook")


# Run the function to get the combined data frame
combined_data <- variable_importance(species_list)

head(combined_data, 10)

ordered_data <- combined_data %>%
  group_by(species_code) %>%
  mutate(variable_name = reorder_within(variable_name, desc(corTest), species_code)) %>%
  ungroup()

unique(ordered_data$variable_name)


# Plotting the ordered data
variable_importance_plot <- ordered_data |>
  ggplot() +
  geom_col(aes(x = variable_name, y = corTest), color = "white", fill = "#08ABD9") +
  facet_wrap(~species_code, scales = "free_x", ncol = 2) +
  scale_x_reordered() +  # Ensure the x-axis is reordered to plot in descending order
  theme_bw()  +
  labs(y = "Relative Variable Importance") +
  theme(
    strip.text = element_text(size = 9, color = "black", face = "bold", family = "Verdana"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 90, size = 7.8, color = "black", family = "Verdana", hjust = 1, vjust = 0.5),
    axis.text.y = element_text(size = 7.8, family = "Verdana", color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 9, face = "bold", family = "Verdana"),
    legend.position = "none",
    legend.text = element_text(size = 7.5, family = "Banschrift"),
    legend.background = element_rect(fill = "transparent"),
    legend.title = element_blank(),
    strip.background = element_blank()
  )

variable_importance_plot


ggsave("D:/Drive/Bamboo_SDM/OTHER PLOTS/variable_importance.svg",
       variable_importance_plot,
       height = 7.5,
       width = 4.5)

ggsave("D:/Drive/Bamboo_SDM/OTHER PLOTS/JPEG Images/variable_importance.jpg",
       variable_importance_plot,
       height = 7.5,
       width = 4.5,
       dpi = 600
       )



scale_x_discrete(
  limits = c(
    "bio2", "bio3", "bio4", "bio6", "bio7", "bio9", 
    "bio12", "bio13", "bio14", "bio15", "bio18", "bio19", 
    "slope", "aspect", "elevation"
  )
)


scale_fill_manual(
  values = c(
    "bam_alam" = "#FF6E6E",
    "bam_bal" = "#08ABD9",
    "bam_nep" = "#a900b8",
    "bam_nut_cup" = "#6CB284",
    "bam_nut_nut" = "#B4556A",
    "dendro_hook" = "#C4B95A",
    "dendro_hamil" = "#E59500"
  )
)

#=================================================-
# Pixels Physiographic regions. ------------------
#=================================================-

physiography_nepal <- sf::st_read(
  "D:/MISC/points/Physiography/physiography_nepal_updated.shp"
) |>
  dplyr::select(Physio)


## current --
# Function to process current raster files for each province and species
process_current_rasters <- function(shape_file, column_name, new_column_name) {
  
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
  column_name = "Physio",
)

current_pixel_count <- current_pixel_count |> filter(Var1 == "1")

# future rasters ---
# Function to process future raster files for each province, species, SSP, and year
process_future_rasters <- function(shape_file, column_name, new_column_name) {
  
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


current_ssp245 <- current_pixel_count
current_ssp245[5] <- "ssp245"

current_ssp585 <- current_pixel_count
current_ssp585[5] <- "ssp585"

pixel_physio <- rbind(current_ssp245, current_ssp585, future_pixel_count)  

pixel_physio <- pixel_physio %>% mutate(lowered_pixels = Freq/1000)

pixel_physio$year <- as.character(as.factor(pixel_physio$year))

# Define the correct order of levels in order to plot in x axis. 
pixel_physio$year <- factor(pixel_physio$year, levels = c("current", "2050", "2070", "2090"))


pixel_physiography_plot <- ggplot(pixel_physio,
       aes(x = year,  # Convert year to a factor here
           y = lowered_pixels)) +
  geom_line(
    aes(colour = column_string,
        group = column_string), 
    size = 1,
    
  ) +
  geom_point(
    aes(colour = column_string),
    size = 2,
    shape = 21,
    stroke =1,
    fill = "white"
  ) +
  scale_color_manual(
    values = c(
      "High Himalaya" = "#C06CFD",
      "High Mountains" = "#82584A",
      "Middle Mountains" = "#00CF9F",
      "Siwalik" = "#EE4266",
      "Terai" = "#FFD23F"
    ),
    name = "Physiographic Regions"
  ) +
  facet_wrap(~species + ssp, scales = "free_y", ncol = 4) +  # Use facet_wrap with ncol
  labs(y = "Number of Pixels (* 1000)") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 9, color = "black", family = "Verdana"),
    axis.text.y = element_text(hjust = 1, size = 8, color = "black", family = "Verdana"),
    axis.title = element_text(size = 8, color = "black", face = "bold", family = "Verdana"),
    axis.title.x = element_blank(),
    legend.position = c(hjust = 0.66, vjust = 0.1),
    strip.background = element_rect(color = "black", fill = "white"),
    legend.title = element_text(color = "black",
                                face = "bold",
                                family = "Verdana",
                                size = 8),
    legend.text = element_text(size = 8,
                               color = "black",
                               face = "bold",
                               family = "Verdana"),
    strip.text = element_text(size = 9, face = "bold", color = "black", family = "Verdana"),
    legend.background = element_rect(color = "black", fill = 'transparent')
  ) +
  scale_x_discrete(breaks = c("current", "2050", "2070", "2090"))

pixel_physiography_plot


ggsave("D:/Drive/Bamboo_SDM/OTHER PLOTS/pixels_physiographic_regions.svg",
       pixel_physiography_plot,
       width  = 7,
       height =  10)

ggsave("D:/Drive/Bamboo_SDM/OTHER PLOTS/JPEG Images/pixels_physiographic_regions.jpg",
       pixel_physiography_plot,
       width  = 7,
       height =  10,
       dpi = 600
       )

#=================================================-
# Elevation plot [BEESWARM] ---------------
#=================================================-

elevation_data <- readxl::read_excel(
  "D:/Drive/Bamboo_SDM/WORKING DOCUMENTS/elevation_of_species.xlsx"
)

library(tidyverse)
extrafont::loadfonts(quiet = T)
library(ggbeeswarm)


elevation_plot <- ggplot(data = elevation_data,
       aes(x = species_code,
           y = RASTERVALU))+
  theme_bw()+
  geom_beeswarm(
    #aes(color = species_code),
    priority = "none",
    cex = 0.8,
    size = 1.5,
    shape = 21,
    stroke = 1,
    fill = "cyan"
  )+
  labs(x = "",
       y = "Elevation (m)")+
  theme(
    axis.title = element_text(color = "black",
                              face= "bold",
                              family = "Verdana",
                              size = 11),
    axis.text = element_text(color = "black",
                             size = 10,
                             family = "Verdana"),
    axis.text.x = element_text(color = "black",
                             size = 10,
                             family = "Verdana",
                             angle = 90,
                             hjust = 1,
                             vjust  = 0.5),
    panel.grid = element_blank()
  )

elevation_plot

ggsave(
  "D:/Drive/Bamboo_SDM/OTHER PLOTS/elevation_beeswarm.svg",
  elevation_plot,
  height = 4,
  width = 7.5
)

ggsave(
  "D:/Drive/Bamboo_SDM/OTHER PLOTS/JPEG Images/elevation_beeswarm.jpg",
  elevation_plot,
  height = 3.8,
  width = 7.5,
  dpi = 600 
)
#=================================================-
# PHYSIOGRAPHIC REGIONS (AVERAGE VALUES) ---------------
#=================================================-

physiography_average <- readxl::read_excel(
  "D:/Drive/Bamboo_SDM/WORKING DOCUMENTS/pixels_physiography.xlsx",
  sheet = "visualization"
)


library(tidyverse)
extrafont::loadfonts(quiet = T)

physiography_average <- physiography_average |> 
  select(physiographic_regions,	ssp_type,	year,	pixels
)|>
  mutate(pixels_lowered = pixels/1000)

current_filtered <- physiography_average |> filter(year == "current")

str(physiography_average)
str(current_filtered)

physio_pixel_change <- physiography_average |>
  ggplot(aes(
    x = year,
    y = pixels_lowered,
    group = ssp_type
  )
  )+
  geom_line(
    aes(color = ssp_type, group = ssp_type),
    size = 0.7
  )+
  geom_point(
    aes(color = ssp_type),
    fill = "white",
    shape = 21,
    size = 1.7,   # Adjust point size
    stroke = 1.2 # Adjust outline width
  )+
  geom_point(
    data = current_filtered,
    aes(
      x = year,
      y = pixels_lowered,
      group = ssp_type),
    color = "black",
    fill = "black",
    shape = 21,
    size = 1.7,   # Adjust point size
    stroke = 1.5 # Adjust outline width
  )+
  geom_point(
    data = current_filtered,
    aes(
      x = year,
      y = pixels_lowered,
      group = ssp_type),
    color = "white",
    fill = "black",
    shape = 21,
    size = 1.7,   # Adjust point size
    stroke = 0.8 # Adjust outline width
  )+
  
  facet_wrap(
    ~physiographic_regions,
    ncol = 3,
    scales = "free_y",  # Allows different y-axis scales for each species
    #labeller = labeller(Species = c("bam_alam" = "a.",  "bam_bal" = "b.", "bam_nep" = "c.", "bam_nut_cup" = "d.", "bam_nut_nut" = "e.", "dendro_hamil" = "f.", "dendro_hook" = "g."))
  )+
  scale_color_manual(
    values = c(
      "ssp245" = "#FF8600",
      "ssp585" = "#AA188F"
    ),
    labels = c(
      "ssp245" = "SSP2-4.5",
      "ssp585" = "SSP5-8.5"
    ),
    name = "Emission Scenario"
  )+
  scale_x_discrete(breaks = c(
    "current",
    "2050",
    "2070",
    "2090"
  ),
  limits = c("current", "2050", "2070", "2090")
  )+
  theme_bw()+
  theme(
    strip.text = element_text(color = "black",
                              family = "Verdana",
                              size = 10),
    legend.position = c(hjust = 0.8, 
                        vjust = 0.25),
    axis.text.x = element_text(color = "black",
                               family = "Verdana",
                               angle = 90,
                               size = 9,
                               hjust = 1,
                               vjust = 0.5),
    axis.text.y = element_text(color = "black",
                               family = "Verdana",
                               size = 9,
                               hjust = 1),
    axis.title = element_text(color = "black",
                              face = "bold",
                              family = "Verdana",
                              size = 9),
    legend.text = element_text(color = "black",
                               family = "Verdana",
                               size = 10),
    legend.title = element_text(color = "black",
                                family = "Verdana",
                                size = 10,
                                face = "bold"),
    legend.background = element_rect(color = "black",
                                     linewidth = 0.3),
    strip.background = element_blank(),
    plot.background = element_rect(color = "black"),
    legend.key = element_rect(fill = "transparent", color = NA)
  )+
  labs(
    x = "Year",
    y = "Average Number of Pixels (* 1000)"
  )+
  guides(color = guide_legend(override.aes = list(fill = NA))) # to remove the grey background in legend key


physio_pixel_change

ggsave(
  "D:/Drive/Bamboo_SDM/OTHER PLOTS/pixel_change_physio.svg",
  physio_pixel_change,
  width = 6,
  height = 6
)

ggsave(
  "D:/Drive/Bamboo_SDM/OTHER PLOTS/JPEG Images/pixel_change_physio.jpg",
  physio_pixel_change,
  width = 6,
  height = 6,
  dpi = 600
)

#=================================================-
# Principle component analysis (PCA) ---------------
#=================================================-

# data table 
var_imp <- readxl::read_excel(
  "D:/Drive/Bamboo_SDM/WORKING DOCUMENTS/variable_importance.xlsx",
  sheet = "result_sheet"
)

var_imp

library(tidyverse)
library(factoextra)
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

# plotting the pca_plot. 
fviz_pca_biplot(pca_result, repel = TRUE, col.var = "blue", col.ind = "red")


## BIPLOT ------

# extracting data for plotting with ggplot2. 
# Get the PCA scores (PCs for each sample)
pca_scores <- as.data.frame(pca_result$x)

# Get the PCA loadings (coefficients of the original variables)
pca_loadings <- as.data.frame(pca_result$rotation)

pca_loadings

# Add the explained variance to the loadings for labeling
explained_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2) * 100
pca_loadings$explained_variance <- explained_variance

# Create a biplot using ggplot2

## plot 1 

# Choose a scaling factor for extending the loadings vectors
scaling_factor <- 4.5  # You can adjust this value to make the arrows longer

# Plot the first two principal components with extended line segments
ggplot(data = pca_scores, aes(x = PC1, y = PC2)) +
  geom_point(color = "black", 
             size = 2) +  # Points for samples
  geom_segment(data = pca_loadings, 
               aes(x = 0, 
                   y = 0,
                   xend = PC1 * scaling_factor,
                   yend = PC2 * scaling_factor,
                   color = rownames(pca_loadings)
               ), 
               arrow = arrow(type = "closed", 
                             length = unit(4, "mm")), 
               size = 0.7)+
  scale_color_manual(
    values = c(
      "bam_alam" = "red",          
      "bam_bal" = "#E0BF00",
      "bam_nep" = "navyblue",
      "bam_nut_cup" = "#f15bb5",
      "bam_nut_nut" = "#a900b8",
      "dendro_hamil" = "#0033ff",
      "dendro_hook" = "#C70039"
    ),
    name = "Species"
  )+
  geom_text(data = pca_scores, 
            aes(x = PC1 -.25, 
                y = PC2 +0.06 ,
                label = rownames(pca_scores)), 
            color = "black", 
            size = 3.5,
            family = "Verdana") +  # Add variable names as labels
  geom_hline(aes(yintercept= 0))+
  geom_vline(aes(xintercept = 0))+
  labs(x = paste("PC1 (", round(explained_variance[1], 1), "%)", sep = ""),
       y = paste("PC2 (", round(explained_variance[2], 1), "%)", sep = "")) +
  theme_bw() +
  theme(
    legend.position = c(vlust = 0.9,
                        hjust = 0.145),
    axis.text = element_text(color = "black",
                             family = "Verdana",
                             size = 9.5),
    axis.title = element_text(color = "black",
                              face = "bold",
                              family = "Verdana",
                              size = 10),
    legend.text = element_text(color = "black",
                               family = "Verdana",
                               size = 9.5),
    legend.title = element_text(color = "black",
                                family = "Verdana",
                                face = "bold",
                                size = 10),
    plot.background = element_blank(),
    legend.background = element_rect(fill = "transparent",
                                     color = "black"),
    panel.grid = element_blank()
)

ggsave("D:/Drive/Bamboo_SDM/OTHER PLOTS/pca.svg",
       width = 8,
       height = 8
)

#fff

## Plot 2. 

# setting the scaling factor. 
scaling_factor_2 <- 2  # You can adjust this value to make the arrows longer

# Plot the first two principal components with extended line segments
ggplot(data = pca_scores, aes(x = PC1, y = PC2)) +
  geom_point(data = pca_loadings,
             aes(x = PC1*scaling_factor_2,
                 y = PC2*scaling_factor_2,
                 color = rownames(pca_loadings)),
             size = 3) +  # Points for samples
  geom_segment(data = pca_scores, 
               aes(x = 0, 
                   y = 0,
                   xend = PC1,
                   yend = PC2,),
               arrow = arrow(type = "open", 
                             length = unit(2, "mm")), 
               color = "#5D2509",
               size = 0.7)+
  scale_color_manual(
    values = c(
      "bam_alam" = "red",          
      "bam_bal" = "#E0BF00",
      "bam_nep" = "navyblue",
      "bam_nut_cup" = "#f15bb5",
      "bam_nut_nut" = "#a900b8",
      "dendro_hamil" = "#0033ff",
      "dendro_hook" = "#C70039"
    ),
    name = "Species"
  )+
  geom_text(data = pca_scores, 
            aes(x = PC1 -.25, 
                y = PC2 +0.06 ,
                label = rownames(pca_scores)), 
            color = "black", 
            size = 3.5,
            family = "Verdana") +  # Add variable names as labels
  geom_hline(aes(yintercept= 0))+
  geom_vline(aes(xintercept = 0))+
  labs(x = paste("PC1 (", round(explained_variance[1], 1), "%)", sep = ""),
       y = paste("PC2 (", round(explained_variance[2], 1), "%)", sep = "")) +
  theme_bw() +
  theme(
    legend.position = c(vlust = 0.9,
                        hjust = 0.145),
    axis.text = element_text(color = "black",
                             family = "Verdana",
                             size = 9.5),
    axis.title = element_text(color = "black",
                              face = "bold",
                              family = "Verdana",
                              size = 10),
    legend.text = element_text(color = "black",
                               family = "Verdana",
                               size = 9.5),
    legend.title = element_text(color = "black",
                                family = "Verdana",
                                face = "bold",
                                size = 10),
    plot.background = element_blank(),
    legend.background = element_rect(fill = "transparent",
                                     color = "black"),
    panel.grid = element_blank()
  )

ggsave("D:/Drive/Bamboo_SDM/OTHER PLOTS/pca_1.svg",
       width = 8,
       height = 8)

## SCREE PLOT -------

# Extract the standard deviations (sdev) and square them to get the eigenvalues
eigenvalues <- pca_result$sdev^2

# Calculate the proportion of variance explained by each PC
variance_explained <- eigenvalues / sum(eigenvalues) * 100

# Create a data frame for ggplot
scree_data <- data.frame(
  PC = 1:length(variance_explained),  # Principal Component numbers
  VarianceExplained = variance_explained
)

# Plot the scree plot using ggplot2
library(ggplot2)

ggplot(scree_data, aes(x = PC, y = VarianceExplained)) +
  geom_bar(stat = "identity", fill = "skyblue") +  # Bar plot for variance explained
  labs(title = "Scree Plot", 
       x = "Principal Component", 
       y = "Variance Explained (%)") +
  geom_point(color = "red")+
  geom_line(color = "red")+
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))


## PCA plot [MODEL PERFORMANCE]---------------

# data table 
var_imp_models <- readxl::read_excel(
  "D:/Drive/Bamboo_SDM/WORKING DOCUMENTS/model_performance.xlsx",
  sheet = "auc"
)

var_imp_models

library(factoextra)
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


# plotting the pca_plot. 
fviz_pca_biplot(pca_result_models, repel = TRUE, col.var = "blue", col.ind = "red")


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

#=================================================-
# HEATMAP OF VARIABLE IMPORTANCE --------------
#=================================================-

library(tidyverse)
extrafont::loadfonts(quiet = T)

heatmap_data <- readxl::read_excel(
  "D:/Drive/Bamboo_SDM/WORKING DOCUMENTS/variable_importance.xlsx",
  sheet = "visualization"
)


head(heatmap_data)
# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Convert the data into a long format suitable for ggplot2
heatmap_data_long <- heatmap_data %>%
  pivot_longer(cols = starts_with("B.") | starts_with("D."), 
               names_to = "species", 
               values_to = "value")


# Create the heatmap
heat_plot <- ggplot(heatmap_data_long, aes(x = species, y = variables, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "orange", high = "blue", na.value = "white") +
  theme_bw() +
  labs(x = "Species", y = "Predictor Variables", fill = "Variable Importance") +
  theme(axis.text.x = element_text(angle = 60, 
                                   hjust = 1,
                                   color = "black",
                                   family = "verdana",
                                   face = "italic",
                                   size = 10),
        axis.text.y = element_text(color = "black",
                                   family = "verdana",
                                   size = 10),
        panel.grid.major = element_line(),
        legend.position = "top",
        legend.title = element_text(color = "black",
                                    family = "verdana",
                                    size = 10,
                                    face = "bold"),
        legend.text = element_text(color = "black",
                                   family = "verdana",
                                   size = 10),
        axis.title = element_text(color = "black",
                                  family = "verdana",
                                  size = 12,
                                  face = "bold"),
        legend.title.position = "top",
)+
  geom_text(aes(label = ifelse(is.na(value), "", round(value, 2))), color = "black", size = 3, family = "verdana")  # Adding text labels

heat_plot



ggsave("D:/Drive/Bamboo_SDM/OTHER PLOTS/heatmap.svg",
       heat_plot,
       width = 4,
       height = 7)

ggsave("D:/Drive/Bamboo_SDM/OTHER PLOTS/JPEG Images/heatmap.jpg",
       heat_plot,
       width = 4,
       height = 7,
       dpi = 600)


#======================================================================-
# Species Richness Barplot --------------------------------------------
#======================================================================-

# loading excel file and necessary libraries


library(tidyverse)
library(tidytext)
extrafont::loadfonts(quiet = T)

richness_data <- readxl::read_excel(
  "D:/Drive/Bamboo_SDM/WORKING DOCUMENTS/hotspot_species_richness_pixel_frequency_district.xlsx",
  sheet = 'visualization'
) %>% 
  mutate(
    richness_string = case_when(
      richness == 1 ~ "Richness: 1",
      richness == 2 ~ "Richness: 2",
      richness == 3 ~ "Richness: 3",
      richness == 4 ~ "Richness: 4",
      richness == 5 ~ "Richness: 5",
      richness == 6 ~ "Richness: 6",
      richness == 7 ~ "Richness: 7"
    )
  )

richness_ordered <- richness_data %>%
  group_by(richness_string) %>%  # or the correct grouping variable
  mutate(district = reorder_within(district, desc(area), richness_string)) %>%
  ungroup() %>% 
  mutate(richness = as.character(richness))


dolakha_rich7 <- richness_ordered %>%
  filter(district == "Dolakha___Richness: 7")

other_districts <- richness_ordered %>%
  filter(district != "Dolakha___Richness: 7")


richness_plot <- ggplot() +
  geom_bar(
    data = other_districts,
    aes(x = district, y = area, fill = richness),
    stat = "identity",
    width = 0.9  # Standard width
  ) +
  # Dolakha's bar in richness = 7 with reduced width
  geom_bar(
    data = dolakha_rich7,
    aes(x = district, y = area, fill = richness),
    stat = "identity",
    width = 0.1  # Narrower bar for Dolakha
  ) +
  scale_fill_viridis_d(
    option = "viridis", 
    direction = -1, 
    begin = 0, 
    end = 0.9,
    name = "Species Richness"
  ) +
  facet_wrap(~ richness_string, scale = "free", ncol = 4) +
  labs(y = "Area (sq. km.)") +
  theme_minimal() +
  scale_x_reordered() +
  guides(
    fill = guide_legend(
      ncol = 4,
      byrow = T,
      label.position = "right",
      keyheight = unit(6, units = "mm"), 
      keywidth = unit(5.5, units = "mm")
    )
  ) +
  theme(
    strip.text = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 90, size = 9, color = "black", family = "Verdana", hjust = 1, vjust = 0.5),
    axis.text.y = element_text(size = 7.8, family = "Verdana", color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 9, face = "bold", family = "Verdana"),
    legend.position = c(hjust = 0.878, vjust = 0.2),
    legend.text = element_text(size = 9, family = "Verdana"),
    legend.background = element_rect(fill = "transparent", color = "black", linewidth = 0.3),
    legend.title = element_text(size = 9, face = "bold", family = "Verdana"),
    strip.background = element_blank(),
    plot.background = element_rect(color = "black", fill = "transparent")
  )


ggsave("D:/Drive/Bamboo_SDM/OTHER PLOTS/species_richness_districts.svg",
       richness_plot,
       width = 9,
       height = 6)



ggsave("D:/Drive/Bamboo_SDM/OTHER PLOTS/JPEG Images/species_richness_districts.jpg",
       richness_plot,
       width = 9,
       height = 6,
       dpi = 600)

