library(raster)
library(tidyverse)
extrafont::loadfonts(quiet = T)

## Nepal ---------
nepal <- sf::st_read(
  "D:/MISC/points/hermes shape files/hermes_NPL_new_wgs_0.shp"
)


district_nepal <- sf::st_read(
  "D:/MISC/points/hermes shape files/hermes_NPL_new_wgs_2.shp"
)

physiography_nepal <- sf::st_read(
  "D:/MISC/points/Physiography/physiography_nepal_updated.shp"
) |>
  dplyr::select(Physio)


###############################################
sdm_plot_updated <- function(shape_file, species_name, ssp, year, label_position){
  
  # Directories for current and future rasters
  current_directory <- paste0("D:/Drive/Bamboo_SDM/", species_name, "/final_plots/raster_plots/", species_name, "_classified_current.tif")
  future_directory <- paste0("D:/Drive/Bamboo_SDM/", species_name, "/final_plots/raster_plots/", species_name, "_classified_ssp", ssp, "_", year, ".tif")
  
  # Load the raster files
  current_raster <- raster::raster(current_directory)
  future_raster <- raster::raster(future_directory)
  
  # Stack the raster files
  stacked_file <- stack(current_raster, future_raster)
  
  # Convert raster stack to a dataframe and remove NAs
  stacked_df <- stacked_file %>% 
    as.data.frame(xy = TRUE) %>% 
    na.omit()
  
  # Classify the changes between current and future rasters
  stacked_df <- stacked_df %>%
    mutate(change = case_when(
      .[[3]] == "0" & .[[4]] == "0" ~ "0",
      .[[3]] == "0" & .[[4]] == "1" ~ "GAIN",
      .[[3]] == "1" & .[[4]] == "0" ~ "LOSS",
      .[[3]] == "1" & .[[4]] == "1" ~ "NO CHANGE"
    ))
  
  # Create a frequency table
  frequency_table <- as.data.frame(table(stacked_df$change))
  
  # Filter out the row with Var1 = 0
  filtered_table <- frequency_table[frequency_table$Var1 != 0, ]
  
  # Create the column plot using geom_col and custom fill colors
  bar_plot <- ggplot(data = filtered_table, aes(x = Var1, y = Freq, fill = Var1)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = Freq), 
              nudge_y = label_position,  # Adjust the nudge value based on your data scale
              color = "black", # Text color for visibility
              size = 4,
              #fontface = "bold",
              family = "Segoe UI") + # Adjust size as needed
    scale_fill_manual(
      values = c(
        "LOSS" = "red",
        "GAIN" = "blue",
        "NO CHANGE" = "green"
      )
    ) +
    labs(title = "Pixels") +
    theme_void() +
    theme(legend.position = "none") +
    theme(
      panel.background = element_rect(fill = "white", color = "transparent"),
      plot.background = element_rect(fill = "white", color = "transparent"),
      title = element_text(color = "black",
                           size = 9,
                           face = "bold",
                           family = "Eras Demi ITC")
    )
  
  
  # Convert bar_plot to a grob
  bar_grob <- ggplotGrob(bar_plot)
  
  # Define the color value based on the SSP scenario
  color_value_ssp <- if (ssp == 585) {
    "#AA188F"
  } else if (ssp == 245) {
    "#FF8600"
  } else {
    "#000000" # Default color in case of an unexpected SSP
  }
  
  color_value_year <- "#000000"
  
  # plotting using ggplot2 
  ggplot() +
    geom_raster(
      data = stacked_df,
      aes(x = x, y = y, fill = change)
    ) +
    geom_sf(
      data = shape_file,
      fill = "transparent",
      color = color_value_year,
      linewidth = 0.3
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
      panel.grid = element_line(colour = "grey",
                                linewidth = 0.5),
      plot.background = element_rect(fill = "transparent", color = "transparent"),
      panel.background = element_rect(fill = "transparent", color = color_value_ssp, linewidth = 2),
     
    )+
    annotate(
    "text", x = 83, y = 30.4, 
    label = paste0(as.character(year)) , 
    size = 4.5, 
    color = "black",
    fontface = "bold",
    family = "Segoe UI"
    )+
    annotation_custom(grob = bar_grob, 
                      xmin = 86.1, xmax = 88.5, 
                      ymin = 28.4, ymax = 30.6)  # Adjust these limits as necessary
  
  # Define the directory where the plot will be saved
  saving_directory <- paste0("D:/Drive/Bamboo_SDM/", species_name, "/final_plots/maps")
  
  # Create the full directory path with recursive = TRUE
  dir.create(saving_directory, recursive = TRUE)
  
  # Set the working directory to the newly created path
  setwd(saving_directory)
  
  # Name the plot file
  name_plot <- paste0(species_name, "_ssp", ssp, "_", year, ".svg")
  
  # Save the plot
  ggsave(name_plot,
         height = 3,
         width = 5.2)
}



#############

sdm_plot_updated(shape_file = nepal, label_position = 300,
                 species_name = "bam_alam", 
                 ssp = 245, 
                 year = 2050)


sdm_plot_updated(shape_file = nepal, label_position = 300,
                 species_name = "bam_alam", 
                 ssp = 245, 
                 year = 2070)

sdm_plot_updated(shape_file = nepal, label_position = 350,
                 species_name = "bam_alam", 
                 ssp = 245, 
                 year = 2090)


sdm_plot_updated(shape_file = nepal, label_position = 300,
                 species_name = "bam_alam", 
                 ssp = 585, 
                 year = 2050)


sdm_plot_updated(shape_file = nepal, label_position = 300,
                 species_name = "bam_alam", 
                 ssp = 585, 
                 year = 2070)

sdm_plot_updated(shape_file = nepal, label_position = 300,
                 species_name = "bam_alam", 
                 ssp = 585, 
                 year = 2090)

##################



sdm_plot_updated(shape_file = nepal, label_position = 1500,
                 species_name = "bam_bal", 
                 ssp = 245, 
                 year = 2050)


sdm_plot_updated(shape_file = nepal, label_position = 1800,
                 species_name = "bam_bal", 
                 ssp = 245, 
                 year = 2070)

sdm_plot_updated(shape_file = nepal, label_position = 1800,
                 species_name = "bam_bal", 
                 ssp = 245, 
                 year = 2090)


sdm_plot_updated(shape_file = nepal, label_position = 1800,
                 species_name = "bam_bal", 
                 ssp = 585, 
                 year = 2050)


sdm_plot_updated(shape_file = nepal, label_position = 2400,
                 species_name = "bam_bal", 
                 ssp = 585, 
                 year = 2070)

sdm_plot_updated(shape_file = nepal, label_position = 2500,
                 species_name = "bam_bal", 
                 ssp = 585, 
                 year = 2090)


#################################################


sdm_plot_updated(shape_file = nepal, label_position = 2400,
                 species_name = "bam_nep", 
                 ssp = 245, 
                 year = 2050)


sdm_plot_updated(shape_file = nepal, label_position = 3000,
                 species_name = "bam_nep", 
                 ssp = 245, 
                 year = 2070)

sdm_plot_updated(shape_file = nepal, label_position = 3000,
                 species_name = "bam_nep", 
                 ssp = 245, 
                 year = 2090)


sdm_plot_updated(shape_file = nepal, label_position = 3000,
                 species_name = "bam_nep", 
                 ssp = 585, 
                 year = 2050)


sdm_plot_updated(shape_file = nepal, label_position = 3500,
                 species_name = "bam_nep", 
                 ssp = 585, 
                 year = 2070)

sdm_plot_updated(shape_file = nepal, label_position = 7000,
                 species_name = "bam_nep", 
                 ssp = 585, 
                 year = 2090)

##############################################


sdm_plot_updated(shape_file = nepal, label_position = 2000,
                 species_name = "bam_nut_cup", 
                 ssp = 245, 
                 year = 2050)


sdm_plot_updated(shape_file = nepal, label_position = 2000,
                 species_name = "bam_nut_cup", 
                 ssp = 245, 
                 year = 2070)

sdm_plot_updated(shape_file = nepal, label_position = 2000,
                 species_name = "bam_nut_cup", 
                 ssp = 245, 
                 year = 2090)


sdm_plot_updated(shape_file = nepal, label_position = 2000,
                 species_name = "bam_nut_cup", 
                 ssp = 585, 
                 year = 2050)


sdm_plot_updated(shape_file = nepal, label_position = 2000,
                 species_name = "bam_nut_cup", 
                 ssp = 585, 
                 year = 2070)

sdm_plot_updated(shape_file = nepal, label_position = 2000,
                 species_name = "bam_nut_cup", 
                 ssp = 585, 
                 year = 2090)


##############################################


sdm_plot_updated(shape_file = nepal, label_position = 2000,
                 species_name = "bam_nut_nut", 
                 ssp = 245, 
                 year = 2050)


sdm_plot_updated(shape_file = nepal, label_position = 2000,
                 species_name = "bam_nut_nut", 
                 ssp = 245, 
                 year = 2070)

sdm_plot_updated(shape_file = nepal, label_position = 1800,
                 species_name = "bam_nut_nut", 
                 ssp = 245, 
                 year = 2090)


sdm_plot_updated(shape_file = nepal, label_position = 2000,
                 species_name = "bam_nut_nut", 
                 ssp = 585, 
                 year = 2050)


sdm_plot_updated(shape_file = nepal, label_position = 2000,
                 species_name = "bam_nut_nut", 
                 ssp = 585, 
                 year = 2070)

sdm_plot_updated(shape_file = nepal, label_position = 2000,
                 species_name = "bam_nut_nut", 
                 ssp = 585, 
                 year = 2090)


##############################################


sdm_plot_updated(shape_file = nepal, label_position = 2500,
                 species_name = "dendro_hamil", 
                 ssp = 245, 
                 year = 2050)


sdm_plot_updated(shape_file = nepal, label_position = 2500,
                 species_name = "dendro_hamil", 
                 ssp = 245, 
                 year = 2070)

sdm_plot_updated(shape_file = nepal, label_position = 2500,
                 species_name = "dendro_hamil", 
                 ssp = 245, 
                 year = 2090)


sdm_plot_updated(shape_file = nepal, label_position = 2500,
                 species_name = "dendro_hamil", 
                 ssp = 585, 
                 year = 2050)


sdm_plot_updated(shape_file = nepal, label_position = 2500,
                 species_name = "dendro_hamil", 
                 ssp = 585, 
                 year = 2070)

sdm_plot_updated(shape_file = nepal, label_position = 2500,
                 species_name = "dendro_hamil", 
                 ssp = 585, 
                 year = 2090)

##############################################


sdm_plot_updated(shape_file = nepal, label_position = 1000,
                 species_name = "dendro_hook", 
                 ssp = 245, 
                 year = 2050)


sdm_plot_updated(shape_file = nepal, label_position = 1000,
                 species_name = "dendro_hook", 
                 ssp = 245, 
                 year = 2070)

sdm_plot_updated(shape_file = nepal, label_position = 1000,
                 species_name = "dendro_hook", 
                 ssp = 245, 
                 year = 2090)


sdm_plot_updated(shape_file = nepal, label_position = 1000,
                 species_name = "dendro_hook", 
                 ssp = 585, 
                 year = 2050)


sdm_plot_updated(shape_file = nepal, label_position = 1000,
                 species_name = "dendro_hook", 
                 ssp = 585, 
                 year = 2070)

sdm_plot_updated(shape_file = nepal, label_position = 1800,
                 species_name = "dendro_hook", 
                 ssp = 585, 
                 year = 2090)




# CURRENT PLOT ----------------------------------------------------------------



###############################################
current_plot <- function(shape_file, species_name){
  
  # Directories for current and future rasters
  current_directory <- paste0("D:/Drive/Bamboo_SDM/", species_name, "/final_plots/raster_plots/", species_name, "_classified_current.tif")
  
  # Load the raster files
  current_raster <- raster::raster(current_directory)
  
  current_df <- current_raster |> as.data.frame(xy = T) |> na.omit()
  
    # plotting using ggplot2 
  ggplot() +
    geom_raster(
      data = current_df,
      aes(x = x,
          y = y,
          fill = as.factor(ensemble_weighted))
    )+
    geom_sf(
      data = district_nepal,
      fill = "transparent",
      color = "black",
      linewidth = 0.5
    )+
    scale_fill_manual(
      values = c(
        "0" = "white",
        "1" = "#b0132b"
      )
    )+
    guides(
      fill =  "none"
    )+
    theme_void()+
    theme(
      plot.background = element_rect(fill = "transparent", color = "transparent"),
      panel.background = element_rect(fill = "transparent", color = "transparent"),
      panel.grid = element_line(colour = "grey",
                                linewidth = 0.5),
      axis.text = element_text(color = "black",
                               face = "bold",
                               size = 11,
                               family = "Segoe UI")
      
    ) +
    scale_x_continuous(breaks = c(80, 82, 84, 86, 88)) +  # Specify custom x-axis markings
    scale_y_continuous(breaks = c(27, 28, 29, 30))+    # Specify custom y-axis markings
    annotate(
      "text", x = 83, y = 30.4, 
      label = "Current" , 
      size = 5, 
      color = "black",
      fontface = "bold",
      family = "Segoe UI"
    )
  
  
  
  # Define the directory where the plot will be saved
  saving_directory <- paste0("D:/Drive/Bamboo_SDM/", species_name, "/final_plots/maps")
  
  # Create the full directory path with recursive = TRUE
  dir.create(saving_directory, recursive = TRUE)
  
  # Set the working directory to the newly created path
  setwd(saving_directory)
  
  # Name the plot file
  name_plot <- paste0(species_name, "_current.svg")
  
  # Save the plot
  ggsave(name_plot,
         height = 3,
         width = 5.2)
}

current_plot(species_name = "bam_alam",
             shape_file = nepal)

current_plot(species_name = "bam_bal",
             shape_file = nepal)

current_plot(species_name = "bam_nep",
             shape_file = nepal)

current_plot(species_name = "bam_nut_cup",
             shape_file = nepal)

current_plot(species_name = "bam_nut_nut",
             shape_file = nepal)

current_plot(species_name = "dendro_hamil",
             shape_file = nepal)

current_plot(species_name = "dendro_hook",
             shape_file = nepal)




###############################################
pixel_values <- function(shape_file, species_name){
  
  # Directories for current and future rasters
  current_directory <- paste0("D:/Drive/Bamboo_SDM/", species_name, "/final_plots/raster_plots/", species_name, "_classified_current.tif")
  
  # Load the raster files
  current_raster <- raster::raster(current_directory)
  
  current_df <- current_raster |> as.data.frame() |> na.omit()
  
  print(table(current_df))
  
}


## values 

pixel_values(species_name = "bam_alam",
             shape_file = nepal)

pixel_values(species_name = "bam_bal",
             shape_file = nepal)

pixel_values(species_name = "bam_nep",
             shape_file = nepal)

pixel_values(species_name = "bam_nut_cup",
             shape_file = nepal)

pixel_values(species_name = "bam_nut_nut",
             shape_file = nepal)

pixel_values(species_name = "dendro_hamil",
             shape_file = nepal)

pixel_values(species_name = "dendro_hook",
             shape_file = nepal)





# combined all the plots using cowplot -----------------------------------------

library(raster)
library(dplyr)
library(ggplot2)
library(cowplot)
library(grid)
extrafont::loadfonts(quiet = T)

# LABEL PLOT -----------

# Rectangle for the label points. 

# Create a data frame for the rectangles
rect_data <- data.frame(
  xmin = c(1, 1.75, 2.5, 4, 5.25),
  xmax = c(1.5, 2.25, 3.75, 5, 6.25),
  ymin = c(2, 2, 2, 2, 2),  # Set ymin to a specific position
  ymax = c(2.3, 2.3, 2.3, 2.3, 2.3),  # Set ymax to maintain rectangle height
  fill = c("blue", "red", "green", NA, NA),
  border = c(NA, NA, NA, "#FF8600", "#AA188F"),
  label = c("Gain", "Loss", "Unchanged", "ssp245", "ssp585")
)


# Create the plot
label_plot <- ggplot(rect_data, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)) +
  geom_rect(aes(fill=fill), color=NA, na.rm=TRUE) +
  geom_rect(aes(fill=NA, color=border), size=1, na.rm=TRUE) +
  geom_text(aes(x=(xmin + xmax) / 2, y = 1.85, label=label), 
            size= 3.3, 
            family = "Segoe UI",
            fontface  = "bold") +  # Use the specific position
  scale_fill_identity() +
  scale_color_identity() +
  coord_fixed() +
  theme_void() +
  scale_y_continuous(limits = c(1.5, 2.3))+  # Set limits from 0 to 2.5
  scale_x_continuous(limits = c(1, 9))

label_plot


combined_plots <- function(district_shape_file, nepal_shape_file, species_name) {
  
  # Define label positions based on species, SSP, and year
  label_positions <- list(
    bam_alam = list(`245` = list(`2050` = 300, `2070` = 300, `2090` = 350),
                    `585` = list(`2050` = 300, `2070` = 300, `2090` = 300)),
    bam_bal = list(`245` = list(`2050` = 1500, `2070` = 1800, `2090` = 1800),
                   `585` = list(`2050` = 1800, `2070` = 2400, `2090` = 2500)),
    bam_nep = list(`245` = list(`2050` = 2400, `2070` = 3000, `2090` = 3000),
                   `585` = list(`2050` = 3000, `2070` = 3500, `2090` = 7000)),
    bam_nut_cup = list(`245` = list(`2050` = 2000, `2070` = 2000, `2090` = 2000),
                       `585` = list(`2050` = 2000, `2070` = 2000, `2090` = 2000)),
    bam_nut_nut = list(`245` = list(`2050` = 2000, `2070` = 2000, `2090` = 1800),
                       `585` = list(`2050` = 2000, `2070` = 2000, `2090` = 2000)),
    dendro_hamil = list(`245` = list(`2050` = 2500, `2070` = 2500, `2090` = 2500),
                        `585` = list(`2050` = 2500, `2070` = 2500, `2090` = 2500)),
    dendro_hook = list(`245` = list(`2050` = 1000, `2070` = 1000, `2090` = 1000),
                       `585` = list(`2050` = 1000, `2070` = 1000, `2090` = 1800))
  )
  
  
  # Directory for current raster
  current_directory <- paste0("D:/Drive/Bamboo_SDM/", species_name, "/final_plots/raster_plots/", species_name, "_classified_current.tif")
  
  # Load the current raster
  current_raster <- raster::raster(current_directory)
  
  # Convert raster to dataframe and remove NAs
  current_df <- current_raster |> as.data.frame(xy = T) |> na.omit()
  
  # Plotting the current raster data with ggplot2
  current_plot <- ggplot() +
    geom_raster(
      data = current_df,
      aes(x = x, y = y, fill = as.factor(ensemble_weighted))
    ) +
    geom_sf(
      data = district_shape_file,
      fill = "transparent",
      color = "black",
      linewidth = 0.5
    ) +
    scale_fill_manual(
      values = c("0" = "white", "1" = "#b0132b")
    ) +
    guides(
      fill = "none"
    ) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "transparent", color = "transparent"),
      panel.background = element_rect(fill = "transparent", color = "transparent"),
      panel.grid = element_line(colour = "grey", linewidth = 0.5),
      axis.text = element_text(color = "black", face = "bold", size = 8, family = "Segoe UI")
    ) +
    scale_x_continuous(breaks = c(80, 82, 84, 86, 88)) +
    scale_y_continuous(breaks = c(27, 28, 29, 30)) +
    annotate("text", x = 83, y = 30.4, label = "Current", size = 5, color = "black", fontface = "bold", family = "Segoe UI")
  
  # SSP and year lists
  ssp_list <- c(245, 585)
  year_list <- c(2050, 2070, 2090)
  
  # Initialize list to hold future plots
  future_plots <- list()
  
  # Loop through SSP and year combinations to create future plots
  for (ssp in ssp_list) {
    for (year in year_list) {
      # Define directory for future raster
      future_directory <- paste0("D:/Drive/Bamboo_SDM/", species_name, "/final_plots/raster_plots/", species_name, "_classified_ssp", ssp, "_", year, ".tif")
      
      # Load the future raster
      future_raster <- raster::raster(future_directory)
      
      # Stack current and future rasters
      stacked_file <- raster::stack(current_raster, future_raster)
      
      # Convert stacked raster to dataframe and remove NAs
      stacked_df <- stacked_file %>%
        as.data.frame(xy = TRUE) %>%
        na.omit()
      
      # Classify changes
      stacked_df <- stacked_df %>%
        mutate(change = case_when(
          .[[3]] == "0" & .[[4]] == "0" ~ "0",
          .[[3]] == "0" & .[[4]] == "1" ~ "GAIN",
          .[[3]] == "1" & .[[4]] == "0" ~ "LOSS",
          .[[3]] == "1" & .[[4]] == "1" ~ "NO CHANGE"
        ))
      
      # Create a frequency table and filter out '0' values
      frequency_table <- as.data.frame(table(stacked_df$change))
      filtered_table <- frequency_table[frequency_table$Var1 != "0", ]
      
      # Retrieve the label position based on species, SSP, and year
      label_position <- label_positions[[species_name]][[as.character(ssp)]][[as.character(year)]]
      
      # Bar plot code
      bar_plot <- ggplot(data = filtered_table, aes(x = Var1, y = Freq, fill = Var1)) +
        geom_col(width = 0.7) +
        geom_text(aes(label = Freq), 
                  nudge_y = label_position, 
                  color = "black", 
                  size = 1.8, 
                  fontface= "bold",
                  family = "Segoe UI") + 
        scale_fill_manual(
          values = 
            c("LOSS" = "red", 
              "GAIN" = "blue", 
              "NO CHANGE" = "green")
        ) +
        labs(y = "Pixels") +
        theme_void() +
        theme(
          panel.background = element_rect(fill = "white", 
                                          color = "transparent"),
          plot.background = element_rect(fill = "white", 
                                         color = "transparent"),
          axis.title.y = element_text(color = "black", 
                                      face = "bold", 
                                      size = 8, 
                                      family = "Verdana", 
                                      angle = 90),
          legend.position = "none"
        )
      
      
      
      # Convert bar_plot to a grob
      bar_grob <- ggplotGrob(bar_plot)
      
      # Set the color based on SSP
      color_value_ssp <- if (ssp == 585) {
        "#AA188F"
      } else {
        "#FF8600"
      }
      
      # Create the future plot with the bar grob
      future_plot <-  ggplot() +
        geom_raster(
          data = stacked_df,
          aes(x = x, y = y, fill = change)
        ) +
        geom_sf(
          data = nepal_shape_file,
          fill = "transparent",
          color = "black",
          linewidth = 0.3
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
          panel.grid = element_line(colour = "grey",
                                    linewidth = 0.5),
          plot.background = element_rect(fill = "transparent", 
                                         color = "transparent"),
          panel.background = element_rect(fill = "transparent", 
                                          color = color_value_ssp, 
                                          linewidth = 2),
          
        )+
        annotate(
          "text", x = 83, y = 30.3, 
          label = paste0(as.character(year)) , 
          size = 4, 
          color = "black",
          fontface = "bold",
          family = "Segoe UI"
        )+
        annotation_custom(grob = bar_grob, 
                          xmin = 86.1, xmax = 88.5, 
                          ymin = 28.4, ymax = 30.5)  # Adjust these limits as necessary
      
      # Save each plot with its name in the list
      future_plots[[paste0("ssp", ssp, "_", year)]] <- future_plot
    }
  }
  
  # Using cowplot to combine the current plot with future plots
  merged_plot <- cowplot::plot_grid(
    current_plot,  # Top large plot
    cowplot::plot_grid(
      future_plots[["ssp245_2050"]],future_plots[["ssp585_2050"]],
      future_plots[["ssp245_2070"]], future_plots[["ssp585_2070"]],
      future_plots[["ssp245_2090"]], future_plots[["ssp585_2090"]],
      ncol = 2,
      rel_widths = c(1, 1),
      align = "hv",
      axis = "t",
      greedy = TRUE
    ),
    nrow = 3,
    rel_heights = c(0.96, 1.5),
    align = "hv",
    axis = "tb"
  ) +
    theme(
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      panel.spacing = unit(0, "lines"),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
  
  return(merged_plot)
}

setwd("D:/Drive/Bamboo_SDM/OTHER PLOTS/final_maps_combined")

## bam_alam -------
bam_alam_final <- combined_plots(
  district_shape_file = district_nepal, 
  nepal_shape_file = nepal,
  species_name = "bam_alam"
)


# Combine the plots using cowplot
bam_alam_combined <- plot_grid(
  bam_alam_final,
  NULL,
  label_plot,
  nrow = 3,
  rel_heights = c(3, -0.88, 0.2)  # Adjust heights as necessary
)


ggsave(
  "bam_alam_combined_plots.svg", 
  bam_alam_combined, 
  width = 5.08,
  height = 7.8
)


# bam_bal-------
bam_bal_final <- combined_plots(
  district_shape_file = district_nepal, 
  nepal_shape_file = nepal,
  species_name = "bam_bal"
)


# Combine the plots using cowplot
bam_bal_combined <- plot_grid(
  bam_bal_final,
  NULL,
  label_plot,
  nrow = 3,
  rel_heights = c(3, -0.88, 0.2)  # Adjust heights as necessary
)


ggsave(
  "bam_bal_combined_plots.svg", 
  bam_bal_combined, 
  width = 5.08,
  height = 7.8
)


# bam_nep -------
bam_nep_final <- combined_plots(
  district_shape_file = district_nepal, 
  nepal_shape_file = nepal,
  species_name = "bam_nep"
)


# Combine the plots using cowplot
bam_nep_combined <- plot_grid(
  bam_nep_final,
  NULL,
  label_plot,
  nrow = 3,
  rel_heights = c(3, -0.88, 0.2)  # Adjust heights as necessary
)


ggsave(
  "bam_nep_combined_plots.svg", 
  bam_nep_combined, 
  width = 5.08,
  height = 7.8
)


# bam_nut_cup -------
bam_nut_cup_final <- combined_plots(
  district_shape_file = district_nepal, 
  nepal_shape_file = nepal,
  species_name = "bam_nut_cup"
)


# Combine the plots using cowplot
bam_nut_cup_combined <- plot_grid(
  bam_nut_cup_final,
  NULL,
  label_plot,
  nrow = 3,
  rel_heights = c(3, -0.88, 0.2)  # Adjust heights as necessary
)


ggsave(
  "bam_nut_cup_combined_plots.svg", 
  bam_nut_cup_combined, 
  width = 5.08,
  height = 7.8
)


# bam_nut_nut -------
bam_nut_nut_final <- combined_plots(
  district_shape_file = district_nepal, 
  nepal_shape_file = nepal,
  species_name = "bam_nut_nut"
)


# Combine the plots using cowplot
bam_nut_nut_combined <- plot_grid(
  bam_nut_nut_final,
  NULL,
  label_plot,
  nrow = 3,
  rel_heights = c(3, -0.88, 0.2)  # Adjust heights as necessary
)


ggsave(
  "bam_nut_nut_combined_plots.svg", 
  bam_nut_nut_combined, 
  width = 5.08,
  height = 7.8
)


# dendro_hamil -------
dendro_hamil_final <- combined_plots(
  district_shape_file = district_nepal, 
  nepal_shape_file = nepal,
  species_name = "dendro_hamil"
)


# Combine the plots using cowplot
dendro_hamil_combined <- plot_grid(
  dendro_hamil_final,
  NULL,
  label_plot,
  nrow = 3,
  rel_heights = c(3, -0.88, 0.2)  # Adjust heights as necessary
)


ggsave(
  "dendro_hamil_combined_plots.svg", 
  dendro_hamil_combined, 
  width = 5.08,
  height = 7.8
)


# dendro_hook -------
dendro_hook_final <- combined_plots(
  district_shape_file = district_nepal, 
  nepal_shape_file = nepal,
  species_name = "dendro_hook"
)


# Combine the plots using cowplot
dendro_hook_combined <- plot_grid(
  dendro_hook_final,
  NULL,
  label_plot,
  nrow = 3,
  rel_heights = c(3, -0.88, 0.2)  # Adjust heights as necessary
)


ggsave(
  "dendro_hook_combined_plots.svg", 
  dendro_hook_combined, 
  width = 5.08,
  height = 7.8
)





# FINAL PLOTS PHYSIOGRAPHY -----------



# combined all the plots using cowplot -----------------------------------------

library(raster)
library(dplyr)
library(ggplot2)
library(cowplot)
library(grid)
extrafont::loadfonts(quiet = T)


combined_plots <- function(physio_shape_file, nepal_shape_file, species_name) {
  
  # Define label positions based on species, SSP, and year
  label_positions <- list(
    bam_alam = list(`245` = list(`2050` = 300, `2070` = 300, `2090` = 350),
                    `585` = list(`2050` = 300, `2070` = 300, `2090` = 300)),
    bam_bal = list(`245` = list(`2050` = 1500, `2070` = 1800, `2090` = 1800),
                   `585` = list(`2050` = 1800, `2070` = 2400, `2090` = 2500)),
    bam_nep = list(`245` = list(`2050` = 2400, `2070` = 3000, `2090` = 3000),
                   `585` = list(`2050` = 3000, `2070` = 3500, `2090` = 7000)),
    bam_nut_cup = list(`245` = list(`2050` = 2000, `2070` = 2000, `2090` = 2000),
                       `585` = list(`2050` = 2000, `2070` = 2000, `2090` = 2000)),
    bam_nut_nut = list(`245` = list(`2050` = 2000, `2070` = 2000, `2090` = 1800),
                       `585` = list(`2050` = 2000, `2070` = 2000, `2090` = 2000)),
    dendro_hamil = list(`245` = list(`2050` = 2500, `2070` = 2500, `2090` = 2500),
                        `585` = list(`2050` = 2500, `2070` = 2500, `2090` = 2500)),
    dendro_hook = list(`245` = list(`2050` = 1000, `2070` = 1000, `2090` = 1000),
                       `585` = list(`2050` = 1000, `2070` = 1000, `2090` = 1800))
  )
  
  
  # Directory for current raster
  current_directory <- paste0("D:/Drive/Bamboo_SDM/", species_name, "/final_plots/raster_plots/", species_name, "_classified_current.tif")
  
  # Load the current raster
  current_raster <- raster::raster(current_directory)
  
  # Convert raster to dataframe and remove NAs
  current_df <- current_raster |> as.data.frame(xy = T) |> na.omit()
  
  # Plotting the current raster data with ggplot2
  current_plot <- ggplot() +
    geom_raster(
      data = current_df,
      aes(x = x, y = y, fill = as.factor(ensemble_weighted))
    ) +
    geom_sf(
      data = physio_shape_file,
      fill = "transparent",
      color = "black",
      linewidth = 0.2
    ) +
    scale_fill_manual(
      values = c("0" = "white", "1" = "#8E1749")
    ) +
    guides(
      fill = "none"
    ) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "transparent", color = "transparent"),
      panel.background = element_rect(fill = "transparent", color = "black",
                                      linewidth = 2),
    ) +
    scale_x_continuous(breaks = c(80, 82, 84, 86, 88)) +
    scale_y_continuous(breaks = c(27, 28, 29, 30)) +
    annotate("text", x = 84, y = 30, label = "Current", size = 3, color = "black", fontface = "bold", family = "Verdana")
  
  # SSP and year lists
  ssp_list <- c(245, 585)
  year_list <- c(2050, 2070, 2090)
  
  # Initialize list to hold future plots
  future_plots <- list()
  
  # Loop through SSP and year combinations to create future plots
  for (ssp in ssp_list) {
    for (year in year_list) {
      # Define directory for future raster
      future_directory <- paste0("D:/Drive/Bamboo_SDM/", species_name, "/final_plots/raster_plots/", species_name, "_classified_ssp", ssp, "_", year, ".tif")
      
      # Load the future raster
      future_raster <- raster::raster(future_directory)
      
      # Stack current and future rasters
      stacked_file <- raster::stack(current_raster, future_raster)
      
      # Convert stacked raster to dataframe and remove NAs
      stacked_df <- stacked_file %>%
        as.data.frame(xy = TRUE) %>%
        na.omit()
      
      # Classify changes
      stacked_df <- stacked_df %>%
        mutate(change = case_when(
          .[[3]] == "0" & .[[4]] == "0" ~ "0",
          .[[3]] == "0" & .[[4]] == "1" ~ "GAIN",
          .[[3]] == "1" & .[[4]] == "0" ~ "LOSS",
          .[[3]] == "1" & .[[4]] == "1" ~ "NO CHANGE"
        ))
      
      # Create a frequency table and filter out '0' values
      frequency_table <- as.data.frame(table(stacked_df$change))
      filtered_table <- frequency_table[frequency_table$Var1 != "0", ]
      
      # Retrieve the label position based on species, SSP, and year
      label_position <- label_positions[[species_name]][[as.character(ssp)]][[as.character(year)]]
      
      # Bar plot code
      bar_plot <- ggplot(data = filtered_table, aes(x = Var1, y = Freq, fill = Var1)) +
        geom_col(width = 0.7) +
        geom_text(aes(label = Freq), 
                  nudge_y = label_position, 
                  color = "black", 
                  size = 1.8, 
                  fontface= "bold",
                  family = "Segoe UI") + 
        scale_fill_manual(
          values = 
            c("LOSS" = "red", 
              "GAIN" = "blue", 
              "NO CHANGE" = "green")
        ) +
        labs(y = "Pixels") +
        theme_void() +
        theme(
          panel.background = element_rect(fill = "white", 
                                          color = "transparent"),
          plot.background = element_rect(fill = "white", 
                                         color = "transparent"),
          axis.title.y = element_text(color = "black", 
                                      face = "bold", 
                                      size = 8, 
                                      family = "Verdana", 
                                      angle = 90),
          legend.position = "none"
        )
      
      
      
      # Convert bar_plot to a grob
      bar_grob <- ggplotGrob(bar_plot)
      
      # Set the color based on SSP
      color_value_ssp <- if (ssp == 585) {
        "#AA188F"
      } else {
        "#FF8600"
      }
      
      # Create the future plot with the bar grob
      future_plot <-  ggplot() +
        geom_raster(
          data = stacked_df,
          aes(x = x, y = y, fill = change)
        ) +
        geom_sf(
          data = nepal_shape_file,
          fill = "transparent",
          color = "black",
          linewidth = 0.3
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
          plot.background = element_rect(fill = "transparent", 
                                         color = "transparent"),
          panel.background = element_rect(fill = "transparent", 
                                          color = color_value_ssp, 
                                          linewidth = 2),
          
        )+
        annotate(
          "text", x = 84, y = 30, 
          label = paste0(as.character(year)) , 
          size = 3, 
          color = "black",
          fontface = "bold",
          family = "Verdana"
        )+
        annotation_custom(grob = bar_grob, 
                          xmin = 86.1, xmax = 88.5, 
                          ymin = 28.4, ymax = 30.5)  # Adjust these limits as necessary
      
      # Save each plot with its name in the list
      future_plots[[paste0("ssp", ssp, "_", year)]] <- future_plot
    }
  }
  
  # Using cowplot to combine the current plot with future plots
  merged_plot <- cowplot::plot_grid(
    current_plot,  # Top large plot
    cowplot::plot_grid(
      future_plots[["ssp245_2050"]],future_plots[["ssp585_2050"]],
      future_plots[["ssp245_2070"]], future_plots[["ssp585_2070"]],
      future_plots[["ssp245_2090"]], future_plots[["ssp585_2090"]],
      ncol = 2,
      rel_widths = c(1, 1),
      align = "hv",
      axis = "t",
      greedy = TRUE
    ),
    nrow = 3,
    rel_heights = c(1, 1.5),
    align = "hv",
    axis = "tb"
  ) +
    theme(
      plot.margin = unit(c(0, 0, 0, 0), "cm"),
      panel.spacing = unit(0, "lines"),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
  
  return(merged_plot)
}


# Create a data frame for the rectangles
rect_data <- data.frame(
  xmin = c(1, 1.75, 2.5, 4, 5.25),
  xmax = c(1.5, 2.25, 3.75, 5, 6.25),
  ymin = c(2, 2, 2, 2, 2),  # Set ymin to a specific position
  ymax = c(2.3, 2.3, 2.3, 2.3, 2.3),  # Set ymax to maintain rectangle height
  fill = c("blue", "red", "green", NA, NA),
  border = c(NA, NA, NA, "#FF8600", "#AA188F"),
  label = c("Gain", "Loss", "Unchanged", "SSP2-4.5", "SSP5-8.5")
)


# Create the plot
label_plot <- ggplot(rect_data, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)) +
  geom_rect(aes(fill=fill), color=NA, na.rm=TRUE) +
  geom_rect(aes(fill=NA, color=border), lwd=1, na.rm=TRUE) +
  geom_text(aes(x=(xmin + xmax) / 2, y = 1.85, label=label), 
            size= 2.8, 
            family = "Verdana",
            fontface  = "bold") +  # Use the specific position
  scale_fill_identity() +
  scale_color_identity() +
  coord_fixed() +
  theme_void() +
  scale_y_continuous(limits = c(1.5, 2.3))+  # Set limits from 0 to 2.5
  scale_x_continuous(limits = c(1, 9))

label_plot



setwd("D:/Drive/Bamboo_SDM/OTHER PLOTS/JPEG Images/final_maps_combined")

## bam_alam -------
bam_alam_final <- combined_plots(
  physio_shape_file = physiography_nepal, 
  nepal_shape_file = nepal,
  species_name = "bam_alam"
)


# Combine the plots using cowplot
bam_alam_combined <- plot_grid(
  bam_alam_final,
  NULL,
  label_plot,
  nrow = 3,
  rel_heights = c(3, -0.86, 0.2)  # Adjust heights as necessary
)


ggsave(
  "bam_alam_combined_plots.jpg", 
  bam_alam_combined, 
  width = 4.97,
  height = 7.8,
  dpi = 900
)


# bam_bal-------
bam_bal_final <- combined_plots(
  physio_shape_file = physiography_nepal, 
  nepal_shape_file = nepal,
  species_name = "bam_bal"
)

# Combine the plots using cowplot
bam_bal_combined <- plot_grid(
  bam_bal_final,
  NULL,
  label_plot,
  nrow = 3,
  rel_heights = c(3, -0.86, 0.2)  # Adjust heights as necessary
)


ggsave(
  "bam_bal_combined_plots.jpg", 
  bam_bal_combined, 
  width = 4.97,
  height = 7.8,
  dpi = 900
)


# bam_nep -------
bam_nep_final <- combined_plots(
  physio_shape_file = physiography_nepal, 
  nepal_shape_file = nepal,
  species_name = "bam_nep"
)


# Combine the plots using cowplot
bam_nep_combined <- plot_grid(
  bam_nep_final,
  NULL,
  label_plot,
  nrow = 3,
  rel_heights = c(3, -0.86, 0.2)  # Adjust heights as necessary
)


ggsave(
  "bam_nep_combined_plots.jpg", 
  bam_nep_combined, 
  width = 4.97,
  height = 7.8,
  dpi = 900
)


# bam_nut_cup -------
bam_nut_cup_final <- combined_plots(
  physio_shape_file = physiography_nepal, 
  nepal_shape_file = nepal,
  species_name = "bam_nut_cup"
)


# Combine the plots using cowplot
bam_nut_cup_combined <- plot_grid(
  bam_nut_cup_final,
  NULL,
  label_plot,
  nrow = 3,
  rel_heights = c(3, -0.86, 0.2)  # Adjust heights as necessary
)


ggsave(
  "bam_nut_cup_combined_plots.jpg", 
  bam_nut_cup_combined, 
  width = 4.97,
  height = 7.8,
  dpi = 900
)


# bam_nut_nut -------
bam_nut_nut_final <- combined_plots(
  physio_shape_file = physiography_nepal, 
  nepal_shape_file = nepal,
  species_name = "bam_nut_nut"
)


# Combine the plots using cowplot
bam_nut_nut_combined <- plot_grid(
  bam_nut_nut_final,
  NULL,
  label_plot,
  nrow = 3,
  rel_heights = c(3, -0.86, 0.2)  # Adjust heights as necessary
)


ggsave(
  "bam_nut_nut_combined_plots.jpg", 
  bam_nut_nut_combined, 
  width = 4.97,
  height = 7.8,
  dpi = 900
)


# dendro_hamil -------
dendro_hamil_final <- combined_plots(
  physio_shape_file = physiography_nepal, 
  nepal_shape_file = nepal,
  species_name = "dendro_hamil"
)


# Combine the plots using cowplot
dendro_hamil_combined <- plot_grid(
  dendro_hamil_final,
  NULL,
  label_plot,
  nrow = 3,
  rel_heights = c(3, -0.86, 0.2)  # Adjust heights as necessary
)


ggsave(
  "dendro_hamil_combined_plots.jpg", 
  dendro_hamil_combined, 
  width = 4.97,
  height = 7.8,
  dpi = 900
)


# dendro_hook -------
dendro_hook_final <- combined_plots(
  physio_shape_file = physiography_nepal, 
  nepal_shape_file = nepal,
  species_name = "dendro_hook"
)


# Combine the plots using cowplot
dendro_hook_combined <- plot_grid(
  dendro_hook_final,
  NULL,
  label_plot,
  nrow = 3,
  rel_heights = c(3, -0.86, 0.2)  # Adjust heights as necessary
)


ggsave(
  "dendro_hook_combined_plots.jpg", 
  dendro_hook_combined, 
  width = 4.97,
  height = 7.8,
  dpi = 900
)

