# Loading packages
library(ggplot2)
library(dplyr)

# Reading the data
data <- read.csv("1_data/tools.csv")


# Subsetting all points from data
points <- subset(data, !artefact.cat == "scraper")


# Map old artefact.cat values to new titles
points$artefact.cat <- factor(points$artefact.cat, 
                              levels = c("hp", "abp", "ltp", "arrow", "dart"))

# Reordering data according to site
points$site <- factor(points$site, levels = c("jels_1",
                                              "jels_2",
                                              "Oldeholtwolde",
                                              "borneck",
                                              "bromme",
                                              "hoejgaard",
                                              "trollesgave"))

# Creating a named vector to map old artefact.cat values to new titles
new_titles <- c("hp" = "Hamburgian Point", 
                "abp" = "Arch Backed Point",
                "ltp" = "Large Tanged Point",
                "arrow" = "Ethnographic Arrow",
                "dart" = "Ethnographic Dart")

# Calculate mean and standard deviation for each artefact category and rounding values to 1 decimal place
stats <- points %>%
  group_by(artefact.cat) %>%
  summarise(mean_width = mean(max.width, na.rm = TRUE),
            sd_width = sd(max.width, na.rm = TRUE)) %>%
  mutate(
    # First sigma (±1 standard deviation)
    sigma1_upper = mean_width + sd_width,
    sigma1_lower = mean_width - sd_width,
    # Second sigma (±2 standard deviations)
    sigma2_upper = mean_width + 2 * sd_width,
    sigma2_lower = mean_width - 2 * sd_width
  ) %>%
  # Add new_titles as a new column
  mutate(new_title = new_titles[artefact.cat]) %>%
  # Round numerical values to 1 decimal place
  mutate(across(c(mean_width, sd_width, sigma1_upper, sigma1_lower, sigma2_upper, sigma2_lower), ~ round(.x, 1)))

# Saving stats as csv for table in paper
write.csv(stats, "3_output/artefact_summary_stats.csv", row.names = FALSE)



## Making histograms

# Define the custom facet layout
facet_layout <- data.frame(
  artefact.cat = c("arrow", "dart", "hp", "abp", "ltp"),
  row = c("top", "top", "middle", "bottom", "bottom"),
  col = c("A", "B", "A", "A", "B")
)

# Merge layout into datasets (only once!)
points <- merge(points, facet_layout, by = "artefact.cat", all.x = TRUE)
stats <- merge(stats, facet_layout, by = "artefact.cat", all.x = TRUE)

# Ensure row and col are ordered factors
points$row <- factor(points$row, levels = c("top", "middle", "bottom"))
points$col <- factor(points$col, levels = c("A", "B"))
stats$row <- factor(stats$row, levels = c("top", "middle", "bottom"))
stats$col <- factor(stats$col, levels = c("A", "B"))

# Subset data
stacked <- points[points$artefact.cat %in% c("hp", "ltp", "abp"), ]
nonstacked <- points[points$artefact.cat %in% c("dart", "arrow"), ]



# Plot
histogram <- ggplot() + 
  # Stacked categories
  geom_histogram(data = stacked, 
                 aes(x = max.width, fill = site),
                 binwidth = 1, color = "black", alpha = 0.5, position = "stack") +
  
  # Non-stacked categories
  geom_histogram(data = nonstacked, 
                 aes(x = max.width),
                 binwidth = 1, color = "black", alpha = 0.5, position = "identity") + 
  
  # Custom facet text labels
  geom_text(data = stats, 
            aes(x = max(points$max.width) * 0.95,  
                y = Inf, label = new_title),
            vjust = 2, hjust = 1, size = 4.5, fontface = "bold") + 
  
  # ±1σ lines (dashed)
  geom_vline(data = stats, aes(xintercept = sigma1_lower),
             linetype = "dashed", linewidth = 0.6, color = "black") +
  geom_vline(data = stats, aes(xintercept = sigma1_upper),
             linetype = "dashed", linewidth = 0.6, color = "black") +
  
  # ±2σ lines (dotted)
  geom_vline(data = stats, aes(xintercept = sigma2_lower),
             linetype = "dotted", linewidth = 0.6, color = "black") +
  geom_vline(data = stats, aes(xintercept = sigma2_upper),
             linetype = "dotted", linewidth = 0.6, color = "black") +
  
  
  # Custom layout
  facet_grid(row ~ col) + 
  
  # Manual site colors
  scale_fill_manual(values = c("jels_1" = "blue4",
                               "jels_2" = "blue",
                               "Oldeholtwolde" = "royalblue",
                               "borneck" = "#1b7837",
                               "bromme" = "#5aae61",
                               "hoejgaard" = "#a6dba0",
                               "trollesgave" = "#d9f0d3"), 
                    name = "Site", 
                    labels = c("jels_1" = "Jels 1", 
                               "jels_2" = "Jels 2",
                               "Oldeholtwolde" = "Oldeholtwolde",
                               "borneck" = "Borneck", 
                               "bromme" = "Bromme", 
                               "hoejgaard" = "Højgård", 
                               "trollesgave" = "Trollesgave")) +
  
  # Clean theme with hidden facet labels
  theme_void() +
  theme(
    axis.title.x = element_text(size = 10, face = "bold"),
    axis.title.y = element_text(size = 10, face = "bold", angle = -90),
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    strip.text.x = element_blank(),
    strip.text.y = element_blank(),
    legend.text = element_text(size = 10, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"),
    
    # Optional: place legend in the empty panel (middle-right)
    legend.position = c(0.8, 0.5),
    legend.background = element_blank(),
    legend.key.size = unit(0.7, "lines")
  ) +
  
  xlim(min(points$max.width), max(points$max.width)) + 
  labs(x = "Width (mm)", 
       y = "Density",
       tag = NULL)

# Save the plot with high resolution
ggsave(
  filename = "3_output/histogram_points.png",       
  plot = histogram,                 
  dpi = "retina",                  
  width = 30,                     
  height = 20,                    
  units = "cm"                   
)

