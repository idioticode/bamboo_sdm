
# STUDY AREA MAP --------------------

# PHYSIOGRAPHY AND OCCURRENCE POINTS 

library(ggplot2)
library(cowplot)
library(ggnewscale)
extrafont::loadfonts(quiet = T)


occurrence_points <- readxl::read_excel(
  "D:/MISC/MaxEnt SDM/data/final_data/occurrence_points/occurrence_points.xlsx"
)

physiography_nepal <- sf::st_read(
  "D:/MISC/points/Physiography/physiography_nepal_updated.shp"
) |>
  dplyr::select(Physio)


main_plot <- ggplot() +
  geom_sf(
    data = physiography_nepal,
    aes(fill = Physio),
    color = "white",
    alpha = 1,
    linewidth = 0.1,
    size = 0.1
  ) +
  scale_fill_viridis_d(
    option = "viridis", 
    direction = 1, 
    begin = 0.4, 
    end = 0.8,
    na.translate = FALSE
  ) +
  new_scale_fill() +  # Reset the fill scale for the next layer using ggnewscale package
  geom_point(
    data = occurrence_points,
    aes(x = LON, y = LAT, fill = species_name,),
    size = 2.1,
    shape = 21,
    color = "black",
    stroke = 0.3
  ) +
  scale_fill_manual(
    values = c(
      "Bambusa jaintiana" = "red",          
      "Bambusa balcooa" = "yellow",
      "Bambusa nepalensis" = "#6C158C",
      "Bambusa teres" = "#FF00E1",
      "Bambusa nutans" = "#00F5F5",
      "Dendrocalamus hamiltonii" = "#0033ff",
      "Dendrocalamus hookeri" = "#00E000"
    )
  )+
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "false",
    pad_x = unit(1, "cm"),
    pad_y = unit(0.3, "cm"),
    style = ggspatial::north_arrow_fancy_orienteering(
      fill = c("black", "black"),
      line_col = "white",
      text_family = "Britannic Bold",  # Changed to a commonly available font
      text_col = "Black",
      text_size = 11
    )
  )+
  ggspatial::annotation_scale(
    location = "bl",
    style = "ticks",
    line_col = "black",
    pad_x = unit(14, "cm"),
    pad_y = unit(0.9, "cm"),
    text_col = "black",
    text_family = "Verdana",
    text_cex = 0.7
  )+
  geom_linerange(
    aes(x = c(83, 86.5),
        ymin = 26.3,
        ymax = 30.4)
  )+
  annotate(
    "text",
    x = 83,
    y = 30.5,
    label = "83° E",
    size = 3,
    color = "black",
    family = "Verdana",
  )+
  annotate(
    "text",
    x = 81.5,
    y = 30.7,
    label = "Western Nepal",
    size = 3,
    fontface = "bold",
    color = "black",
    family = "Verdana",
  )+
  annotate(
    "text",
    x = 84.7,
    y = 30.7,
    label = "Central Nepal",
    size = 3,
    fontface = "bold",
    color = "black",
    family = "Verdana",
  )+
  annotate(
    "text",
    x = 87.6,
    y = 30.7,
    label = "Eastern Nepal",
    size = 3,
    fontface = "bold",
    color = "black",
    family = "Verdana",
  )+
  annotate(
    "text",
    x = 86.6,
    y = 30.5,
    label = "86°30' E",
    size = 3,
    color = "black",
    family = "Verdana",
  )+
  theme_minimal() +  # Change this line from theme_void() to theme_minimal()
  theme(legend.position = "none", # Suppress the legends in the main plot
        axis.text = element_text(size = 8, 
                                 color = "black",
                                 face = "bold",
                                 family = "Verdana"),
        axis.ticks = element_line(color = "black",
                                  linewidth = 0.4),
        panel.background = element_rect(fill = "transparent",
                                        color = "black"),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.length.y = unit(-2, "mm"),
        axis.ticks.length.x = unit(-2, "mm"),
        axis.text.x = element_text(hjust = 0.5,
                                   vjust = 2,
                                   margin = margin(t = -14)),
        axis.text.y = element_text(hjust = -1,
                                   vjust = 0.5,
                                   margin = margin(r = 5)),   
  )  +
  scale_y_continuous(expand = expansion(mult = c(0.12, 0.05)),
                     breaks = c(27, 28, 29, 30))+
  scale_x_continuous(expand = expansion(mult = c(0.1, 0.05)))
  

main_plot

# Manually place the legends on top of the plot
physio_final <- ggdraw(main_plot) +
  draw_grob(legend_fill_physio, width = 1.9, height = 1.15) +  # Adjust position for fill legend
  draw_grob(legend_color_points, width = 0.25, height = 0.55)  # Adjust position for color legend


ggsave("D:/Drive/Bamboo_SDM/OTHER PLOTS/physio_occurrence.svg",
       physio_final,
       width = 8,
       height = 5)

  # Extract the two legends using cowplot


# Try extracting just the fill legend
legend_fill_physio <- cowplot::get_legend(
  ggplot() +
    geom_sf(data = physiography_nepal, aes(fill = Physio)) +
    scale_fill_viridis_d(option = "viridis",
                         direction = 1,
                         begin = 0.4,
                         end = 0.8,
                         alpha = 1,
                         name = "Physiographic Regions") +
    theme_void() +
    theme(legend.title = element_text(color = "black",
                                      face = "bold",
                                      family= "Verdana",
                                      size= 9,
                                      hjust = 1),
          legend.text = element_text(color = "black",
                                     family = "Verdana",
                                     size = 8),
          legend.background = element_rect(fill = "white", 
                                           color = "white")
    ) +
    guides(
      fill = guide_legend(
        direction = "vertical", 
        title.position = "top",
        label.position = "left",
        keyheight = unit(6, "mm"), 
        keywidth = unit(10, "mm"),
        ncol = 1,
        bycol = T
      )
    )
)


# Legend for color (species)
legend_color_points <- get_legend(
  ggplot() +
    geom_point(
      data = occurrence_points,
      aes(x = LON, y = LAT, fill = species_name),
      shape = 21
    ) +
    scale_fill_manual(
      values = c(
        "Bambusa jaintiana" = "red",          
        "Bambusa balcooa" = "yellow",
        "Bambusa nepalensis" = "#6C158C",
        "Bambusa teres" = "#FF00E1",
        "Bambusa nutans" = "#00F5F5",
        "Dendrocalamus hamiltonii" = "#0033ff",
        "Dendrocalamus hookeri" = "#00E000"
      ),
      name = "Species"
    )+
    theme(
      legend.title = element_text(color = "black",
                                  face = "bold",
                                  family= "Verdana",
                                  size  = 9),
      legend.text = element_text(color = "black",
                                 family = "Verdana",
                                 size = 8,
                                 face = "italic"),
      legend.background = element_rect(fill = "white", 
                                       color = "white"),
      legend.box.background = element_rect(fill = "white", color = "white"), 
    )+ # Set the legend box background color
    guides(
      fill = guide_legend(
        direction = "horizontal", 
        title.position = "top",
        label.position = "right",
        keyheight = unit(4.24, "mm"), 
        keywidth = unit(4, "mm"),
        ncol = 1,
        bycol = F,
        override.aes = list(size = 4)  # Set fill to NA to remove the box and adjust size if needed
      )
    )
)


# Manually place the legends on top of the plot
physio_final <- ggdraw(main_plot) +
  draw_grob(legend_fill_physio, width = 1.9, height = 1.15) +  # Adjust position for fill legend
  draw_grob(legend_color_points, width = 0.25, height = 0.55)  # Adjust position for color legend

ggsave("D:/Drive/Bamboo_SDM/OTHER PLOTS/physio_occurrence.svg",
       physio_final,
       width = 8, 
       height = 5)

ggsave("D:/Drive/Bamboo_SDM/OTHER PLOTS/JPEG Images/physio_occurrence.jpeg",
       physio_final,
       width = 8, 
       height = 5,
       dpi = 1000)


  # Hotspot Map ----------------------

# this map has updated for districts of Nepal. The svg plot has not been updated. only jpg image has been created. 

library(tidyverse)
library(cowplot)
extrafont::loadfonts(quiet = T)
library(raster)

# shape files 
nepal <- sf::st_read(
  "D:/MISC/points/hermes shape files/hermes_NPL_new_wgs_0.shp"
)

district_nepal <-  sf::st_read(
  "D:/MISC/points/hermes shape files/hermes_NPL_new_wgs_2.shp"
) |>
  dplyr::select(DISTRICT)



## CURRENT PLOT --

current_hotspot_raster <- raster::raster(
  "D:/Drive/Bamboo_SDM/HOTSPOT RASTER FILES/hotspot_species_richness_current.tif",
)

names(current_hotspot_raster)

current_data  <- current_hotspot_raster |> as.data.frame(xy = T) |> na.omit() |> filter(hotspot_species_richness_current != 0)

current_plot <- ggplot() +
  geom_raster(data = current_data, aes(x = x, y = y, fill = as.factor(hotspot_species_richness_current))) +
  geom_sf(data = district_nepal, fill = "transparent", color = "black") +
  scale_fill_viridis_d(option = "viridis", direction = -1, begin = 0, end = 0.9) +
  theme_void() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "transparent", color = "black", linewidth = 2))+
  annotate("text", x = 87.5, y = 30, label = "Current", size = 4.5, color = "black", fontface = "bold", family = "Verdana")
  #geom_linerange(aes(x = c(83, 86.5), ymin = 26.3, ymax = 30.5))
    

current_plot



## FUTURE PLOTS --

# Define the SSP and year combinations for future scenarios
ssps <- c(245, 585)
years <- c(2050, 2070, 2090)

# Create an empty list to store each future plot
future_plots <- list()

# Loop over each SSP and year combination to create future plots
for (ssp in ssps) {
  for (yr in years) {
    # Set the color based on SSP
    color_value_ssp <- if (ssp == 585) "#AA188F" else "#FF8600"
    
    # Load the raster file for the specific SSP and year
    raster_name <- paste0("D:/Drive/Bamboo_SDM/HOTSPOT RASTER FILES/hotspot_species_richness_ssp", ssp, "_", yr, ".tif")
    future_raster <- raster::raster(raster_name)
    
    # Convert raster data to a data frame and rename the richness column
    future_data <- as.data.frame(future_raster, xy = TRUE, na.rm = TRUE)
    colnames(future_data)[3] <- "richness"  # Rename the third column to "richness"
    
    # Filter out rows where richness is 0
    future_data <- future_data %>% filter(richness != 0)
    
    # Generate the future plot
    future_plot <- ggplot() + 
      coord_fixed() +
      geom_raster(data = future_data, aes(x = x, y = y, fill = as.factor(richness))) +
      geom_sf(data = district_nepal, fill = "transparent", color = "black") +
      scale_fill_viridis_d(option = "viridis", direction = -1, begin = 0, end = 0.9) +
      theme_void() +
      theme(
        legend.position = "none",
        panel.background = element_rect(fill = "transparent", color = color_value_ssp, linewidth = 2)
      ) +
      annotate("text", x = 87.5, y = 30, label = as.character(yr), size = 3, color = "black", fontface = "bold", family = "Verdana")
    
    # Store the plot in the list
    future_plots[[paste0("ssp", ssp, "_", yr)]] <- future_plot
  }
}

# Combine future plots using cowplot
combined_future_plot <- cowplot::plot_grid(
  future_plots[["ssp245_2050"]], future_plots[["ssp585_2050"]],
  future_plots[["ssp245_2070"]], future_plots[["ssp585_2070"]],
  future_plots[["ssp245_2090"]], future_plots[["ssp585_2090"]],
  ncol = 2, nrow = 3, align = "hv"
)

# Display the combined plot
print(combined_future_plot)


## LEGENDS --
legend_fill <- cowplot::get_legend(
  ggplot() +
    geom_raster(data = current_data, aes(x = x, y = y, fill = as.factor(hotspot_species_richness_current))) +
    scale_fill_viridis_d(option = "viridis", direction = -1, begin = 0, end = 0.9, name = "Species Richness") +
    theme_void() +
    theme(legend.title = element_text(color = "black", face = "bold", family= "Verdana", size= 9),
          legend.text = element_text(color = "black", family = "Verdana", size = 8, face = "bold"),
          legend.background = element_rect(fill = "white", color = "white")) +
    guides(fill = guide_legend(direction = "horizontal", title.position = "top",
                               label.position = "bottom", keyheight = unit(6, "mm"), 
                               keywidth = unit(5, "mm"), ncol = 7))
)

legend_fill

# Create a data frame for the thrtwoee required rectangles
rect_data <- data.frame(
  xmin = c(1, 1.75),
  xmax = c(1.5, 2.25),
  ymin = c(2, 2),  # Set ymin to a specific position
  ymax = c(2.2, 2.2),  # Set ymax to maintain rectangle height
  label = c("ssp245", "ssp585")  # Labels for the boxes
)


legend_color <- cowplot::get_legend(
  ggplot(rect_data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax))+
    geom_rect(aes(color = label), fill = "transparent", size = 1, na.rm = TRUE) +
    scale_color_manual(values = c(
      "ssp245" = "#FF8600",
      "ssp585" = "#AA188F"
    ),
    labels = c(
      "ssp245" = "SSP2-4.5",
      "ssp585" = "SSP5-8.5"
    ),
    name =  "SSP Type"
    )+
    guides(color = guide_legend(direction = "horizontal", title.position = "top",
                                label.position = "bottom", keyheight = unit(6, "mm"), 
                                keywidth = unit(15, "mm"), ncol = 3))+
    theme(legend.title = element_text(color = "black", face = "bold", family= "Verdana", size= 9),
          legend.text = element_text(color = "black", family = "Verdana", size = 8, face = "bold"),
          legend.background = element_rect(fill = "white", color = "white")) 
)

legend_color

# Combine plots with cowplot
legend_plot <-  cowplot::plot_grid(
  legend_color, legend_fill,
  ncol = 2, nrow = 1, rel_heights = c(1, 1)
)


final_hotspot_plot <- cowplot::plot_grid(
  current_plot, combined_future_plot, legend_plot,
  ncol = 1, nrow = 3, rel_heights = c(0.8, 1.2, 0.25)
)

final_hotspot_plot

ggsave(
  "D:/Drive/Bamboo_SDM/OTHER PLOTS/hotspot_combined.svg",
  final_hotspot_plot,
  width = 4.85,
  height = 8
)

ggsave(
  "D:/Drive/Bamboo_SDM/OTHER PLOTS/JPEG Images/hotspot_combined_districts.jpg",
  final_hotspot_plot,
  width = 4.95,
  height = 8,
  dpi = 1000
)


