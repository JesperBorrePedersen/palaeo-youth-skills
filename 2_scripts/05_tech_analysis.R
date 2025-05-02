# Load libraries
library(FactoMineR)
library(ggplot2)
library(dplyr)
library(patchwork)

# Load dataset
tech_data <- readr::read_delim("1_data/tech_data.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Subset your data (numeric columns only)
subset_data <- tech_data[, c(20:63, 69)]


# Perform correspondence analysis
ca_result <- CA(subset_data, graph = FALSE)


################################
############ Blades ############
################################

# Extract row coordinates
row_coords <- as.data.frame(ca_result$row$coord)

# Add classification and site columns
row_coords$classification <- tech_data$classification
row_coords$site <- tech_data$site

# Rename classification levels
row_coords$chronozone <- recode(row_coords$classification,
                                "Hamburgian" = "Bølling/GI-1e",
                                "Bromme" = "Allerød/GI-1a-c",
                                "Federmesser" = "Allerød/GI-1a-c")

# Rename site names
row_coords$site <- recode(row_coords$site,
                          "Hoejgaard" = "Højgård")




# Create the CA plot of the blades
plot1 <- ggplot(row_coords, aes(x = `Dim 1`, y = `Dim 2`, color = chronozone, shape = site)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Allerød/GI-1a-c" = "forestgreen", 
                                "Bølling/GI-1e" = "blue4")) +
  scale_shape_manual(values = c("Egtved mark" = 16,      # Circle
                                "Højgård" = 17,          # Triangle
                                "Trollesgave" = 8,       # Star
                                "Oldeholtwolde" = 13)) + # Diamond
  xlim(-2, 2) +                              
  ylim(-3, 3) +                                 
  geom_hline(yintercept = 0, linetype = "dashed") + # Add horizontal axis at y=0
  geom_vline(xintercept = 0, linetype = "dashed") + # Add vertical axis at x=0
  labs(title = "A)",
       x = "Dimension 1",
       y = "Dimension 2",
       color = "Period",
       shape = "Site") +
  theme_minimal() +
  theme(legend.position = "right")


################################
########## Attributes ##########
################################

# Extract column coordinates from the CA result

col_coords <- as.data.frame(ca_result$col$coord)
col_coords$label <- rownames(col_coords)  # Add column labels

# Create new columns for attributes

col_coords$attri_group <- recode(col_coords$label,
                                 "ba_3_1" = "Curvature",
                                 "ba_3_2" = "Curvature",
                                 "ba_3_3" = "Curvature", 
                                 "ba_4_1" = "Regularity",
                                 "ba_4_2" = "Regularity", 
                                 "ba_5_1" = "Ventral Ripples",  
                                 "ba_5_2" = "Ventral Ripples", 
                                 "ba_5_3" = "Ventral Ripples",
                                 "ba_6_2" = "Blade Termination",  
                                 "ba_6_3" = "Blade Termination", 
                                 "ba_6_4" = "Blade Termination",
                                 "ba_6_5" = "Blade Termination", 
                                 "ba_6_6" = "Blade Termination",
                                 "ba_7_1" = "Bulb Morphology",
                                 "ba_7_2" = "Bulb Morphology", 
                                 "ba_7_3" = "Lip Morphology",
                                 "ba_7_5" = "Bulb Morphology",
                                 "ba_8_1" = "Bulbar Scar",
                                 "ba_9_1" = "Conus Formation",
                                 "ba_9_2" = "Conus Formation", 
                                 "ba_9_3" = "Conus Formation",
                                 "ba_10_1" = "Butt Morphology",
                                 "ba_10_2" = "Butt Morphology",
                                 "ba_10_3" = "Butt Morphology",
                                 "ba_10_4" = "Butt Morphology",
                                 "ba_10_5" = "Butt Morphology",
                                 "ba_10_6" = "Butt Morphology",
                                 "ba_10_7" = "Butt Morphology",
                                 "ba_11_1" = "Butt Preperation",
                                 "ba_11_2" = "Butt Preperation",
                                 "ba_11_3" = "Butt Preperation",
                                 "ba_11_5" = "Butt Preperation",
                                 "ba_12_2" = "Blade Preperation",
                                 "ba_12_3" = "Blade Preperation",
                                 "ba_12_4" = "Blade Preperation",
                                 "ba_12_6" = "Blade Preperation",
                                 "ba_12_7"= "Blade Preperation",
                                 "ba_15_1" = "Hammer Marks")


col_coords$attri_cat <- recode(col_coords$label,
                               "ba_3_1" = "Straight",
                               "ba_3_2" = "Distal",
                               "ba_3_3" = "Even", 
                               "ba_4_1" = "Irregular",
                               "ba_4_2" = "Regular", 
                               "ba_5_1" = "Smooth",  
                               "ba_5_2" = "Visible", 
                               "ba_5_3" = "Pronounced",
                               "ba_6_2" = "Feathered",  
                               "ba_6_3" = "Plunging", 
                               "ba_6_4" = "Slightly Hinged",
                               "ba_6_5" = "Strongly Hinged", 
                               "ba_6_6" = "Burinated",
                               "ba_7_1" = "Pronounced Bulb",
                               "ba_7_2" = "Bulb", 
                               "ba_7_3" = "Lip",
                               "ba_7_5" = "Double Bulb",
                               "ba_8_1" = "Bulbar Scar",
                               "ba_9_1" = "Ring Crack",
                               "ba_9_2" = "Ventral Fissures", 
                               "ba_9_3" = "Detached Bulb",
                               "ba_10_1" = "Large Thick",
                               "ba_10_2" = "Large Oval",
                               "ba_10_3" = "Thin Oval",
                               "ba_10_4" = "Small Thick",
                               "ba_10_5" = "Small",
                               "ba_10_6" = "Punctiform",
                               "ba_10_7" = "Broken",
                               "ba_11_1" = "Plain",
                               "ba_11_2" = "Two Facets",
                               "ba_11_3" = "> Two Facets",
                               "ba_11_5" = "Cortical",
                               "ba_12_2" = "Unprepared",
                               "ba_12_3" = "Dorsal Trimming",
                               "ba_12_4" = "Dorsal Abrasion",
                               "ba_12_6" = "Broken Butt",
                               "ba_12_7"= "Corniche",
                               "ba_15_1" = "Hammer Marks")

col_coords$attri_kid <- recode(col_coords$label,
                               "ba_3_1" = "N",
                               "ba_3_2" = "N",
                               "ba_3_3" = "N", 
                               "ba_4_1" = "Y",
                               "ba_4_2" = "N", 
                               "ba_5_1" = "N",  
                               "ba_5_2" = "N", 
                               "ba_5_3" = "Y",
                               "ba_6_2" = "N",  
                               "ba_6_3" = "N", 
                               "ba_6_4" = "N",
                               "ba_6_5" = "Y", 
                               "ba_6_6" = "N",
                               "ba_7_1" = "N",
                               "ba_7_2" = "N", 
                               "ba_7_3" = "N",
                               "ba_7_5" = "N",
                               "ba_8_1" = "N",
                               "ba_9_1" = "N",
                               "ba_9_2" = "N", 
                               "ba_9_3" = "Y",
                               "ba_10_1" = "Y",
                               "ba_10_2" = "Y",
                               "ba_10_3" = "N",
                               "ba_10_4" = "N",
                               "ba_10_5" = "N",
                               "ba_10_6" = "N",
                               "ba_10_7" = "N",
                               "ba_11_1" = "Y",
                               "ba_11_2" = "N",
                               "ba_11_3" = "N",
                               "ba_11_5" = "Y",
                               "ba_12_2" = "Y",
                               "ba_12_3" = "N",
                               "ba_12_4" = "N",
                               "ba_12_6" = "Y",
                               "ba_12_7"= "N",
                               "ba_15_1" = "Y")






# Plot with colours for novice related attributes and shapes for attribute catagories
plot2 <- ggplot(col_coords, aes(x = `Dim 1`, y = `Dim 2`, color = attri_kid, shape = attri_group)) +
  geom_point(size = 3) +
  geom_text(aes(label = attri_cat), 
            vjust = -1,
            size = 3, 
            show.legend = FALSE) + # remove lower case "a"s from legend
  scale_color_manual(values = c("Y" = "black", # Custom colors and labels for 'attri_kid'
                                "N" = "grey"), 
                     labels = c("Y" = "Novice Related",
                                "N" = "Not Novice Related")) +
  scale_shape_manual(values = c("Curvature" = 1,
                                "Regularity" = 2,
                                "Ventral Ripples" = 3,  
                                "Blade Termination" = 4,  
                                "Bulb Morphology" = 5,
                                "Lip Morphology" = 6,
                                "Bulbar Scar" = 7,
                                "Conus Formation" = 8,
                                "Butt Morphology" = 9,
                                "Butt Preperation" = 10,
                                "Blade Preperation" = 11,
                                "Hammer Marks" = 12)) +
  labs(title = "B)",
       x = "Dimension 1",
       y = "Dimension 2",
       color = "Novice related atrributes",  # Legend title for colors
       shape = "Attribute Catagory") + # Legend title for shapes
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-2, 2) +
  ylim(-4, 4) +
  theme(legend.position = "right")






# Combine the plots in one column and two rows
combined_plot <- plot1 / plot2

# Display the combined plot
print(combined_plot)



# Save the plot with high resolution
ggsave(
  filename = "3_output/ca_plots.png",
  plot = combined_plot,
  dpi = "retina",
  width = 25,
  height = 25,
  units = "cm"
)

