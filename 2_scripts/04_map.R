library(ggplot2)

data <- readr::read_csv(file.path("1_data", "child_data.csv"))


####################
### distribution map
####################

world <- rworldmap::getMap(resolution = "high")

# create an extent which spans only the distribution of our samples
# potentially, the extents have to be manually in-/decreased
data_extent <- as(raster::extent(min(data$long, #minimum longitude
                                     na.rm = T)-1,
                                 max(data$long, #maximum longitude
                                     na.rm = T)+1,
                                 min(data$lat, #minimum latitude
                                     na.rm = T)-0.5,
                                 max(data$lat, #maximum latidude
                                     na.rm = T)+2), # order: xmin, xmax, ymin, ymax
                  "SpatialPolygons")

sp::proj4string(data_extent) <- sp::CRS(sp::proj4string(world)) # set the coordinate reference system of the data to be the same as the world map.

world_clip <- raster::intersect(world, data_extent) # select only those parts of the world map within our bounding box/extent

world_clip_f <- ggplot2::fortify(world_clip) # transforms it into a data frame

# base map
base_map <-
ggplot() +
  geom_polygon(data = world_clip_f,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = "grey",
               colour = "white") +
  coord_fixed() +
  coord_quickmap() +
  theme_classic() +
  xlab("Longitude") +
  ylab("Latitude")  +
  # labs(color = "Country") + # capitalize
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  theme(legend.position = "right", # or "none"
        text = element_text(size=20))


### data sites w culture
data_sites <- unique(dplyr::select(data, c(Site, long, lat, Period)))
data_sites$Period <- factor(data_sites$Period,
                            levels = c("Bølling/GI-1e", "Allerød/GI-1a-c"))
data_sites$Site <- factor(data_sites$Site,
                          levels = c("Oldeholtwolde", 
                                     "Borneck-Ost",
                                     "Jels 2",
                                     "Egtved mark",
                                     "Højgård",
                                     "Trollesgave",
                                     "Bromme"))

# plot sites + dates by events
sites_map <-
base_map +
  geom_jitter(data = data_sites,
              aes(x = long, y = lat,
                  shape = Site,
                  color = Period),
              size = 5) +
  # ggrepel::geom_label_repel(data = data_sites,
  #                          aes(x = long, y = lat,
  #                              label = Site),
  #                          min.segment.length = 0.2,
  #                          force = 100,
  #                          size = 4) +
  ggplot2::scale_color_manual(values = c("Allerød/GI-1a-c" = "forestgreen", 
                                        "Bølling/GI-1e" = "blue4")) +
  ggplot2::scale_shape_manual(values = c("Egtved mark" = 16,
                                        "Højgård" = 17,
                                        "Trollesgave" = 8,
                                        "Borneck-Ost" = 15,
                                        "Bromme" = 9,
                                        "Oldeholtwolde" = 13,
                                        "Jels 2" = 14))

sites_map


ggsave(sites_map,
       path = file.path("3_output"),
       filename = "sites_map.png",
       width = 20,
       height = 14,
       units = "cm")
