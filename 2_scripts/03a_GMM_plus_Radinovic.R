library(ggplot2)
library(Momocs)

# load outlines
outlines_combined_centered_metadata <- 
  readRDS(file.path("1_data", "outlines_combined_interp_centered_scaled_sl_metadata.RDS"))

# Momocs::panel(outlines_combined_centered_metadata) # show all artefacts
# 
Momocs::filter(outlines_combined_centered_metadata,
               Site == "Oldeholtwolde") %>%  #show artefacts for each site separately
  Momocs::panel(cols = "black")
Momocs::filter(outlines_combined_centered_metadata,
               Site == "Jels 2") %>%
  Momocs::panel(cols = "black")
Momocs::filter(outlines_combined_centered_metadata,
               Site == "Egtved mark") %>%
  Momocs::panel(cols = "black")
Momocs::filter(outlines_combined_centered_metadata,
               Site == "Højgård") %>%
  Momocs::panel(cols = "black")
Momocs::filter(outlines_combined_centered_metadata,
               Site == "Trollesgave") %>%
  Momocs::panel(cols = "black")
Momocs::filter(outlines_combined_centered_metadata,
               Site == "Bromme") %>% 
  Momocs::panel(cols = "black")



## add Radinovic/Kajtez 2021 outlines
outlines_radinovic <- 
  readRDS(file = file.path("1_data", "outlines_radinovicNew_centered_scaled_sl_metadata.RDS"))

Momocs::panel(outlines_radinovic,
              cols = "black") # show all artefacts
Momocs::filter(outlines_radinovic,
               id == "direct") %>% # technique
  Momocs::panel(cols = "black")
Momocs::filter(outlines_radinovic,
               id == "indirect") %>% # technique
  Momocs::panel(cols = "black")

# combine
# are they both clockwise/counterclockwise?
stack(outlines_combined_centered_metadata)
stack(outlines_radinovic)

outlines_BOTH <- 
  Momocs::combine(outlines_combined_centered_metadata, 
                  Momocs::filter(outlines_radinovic, 
                                 id != "pressure"))

# # center, scale, slide direction
outlines_BOTH_centered <- Momocs::coo_center(outlines_BOTH)
outlines_BOTH_centered_scaled <- Momocs::coo_scale(outlines_BOTH_centered)
outlines_BOTH_centered_scaled_sl <- Momocs::coo_slidedirection(outlines_BOTH_centered_scaled, direction = "up")


panel(outlines_BOTH_centered_scaled_sl)
stack(outlines_BOTH_centered_scaled_sl)

### HARMONIC CALIBRATION ###
# Estimates the number of harmonics required for the Fourier methods implemented in Momocs. This is the only step in this section that produces data we need in the subsequent step.
outlines_harmonics <- 
  Momocs::calibrate_harmonicpower_efourier(outlines_BOTH_centered_scaled_sl, 
                                           plot = F)  

###  EFOURIER FILE CREATION ###
outlines_efourier <- 
  Momocs::efourier(outlines_BOTH_centered_scaled_sl,
                   nb.h = as.matrix(outlines_harmonics[["minh"]])[[4,1]], # harmonics for 99.9% 
                   norm = F) # see above

# symmetry measurements from efourier file
outlines_efourier_symmetry <-
  Momocs::symmetry(outlines_efourier) %>% #extraction of symmetry values
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "outline_name")


## add symmetry data to outline-fac
outlines_metadata_sym <- 
  dplyr::left_join(outlines_BOTH_centered_scaled_sl$fac,
                   outlines_efourier_symmetry,
                   by = "outline_name")

outlines_BOTH_centered_scaled_sl_sym <- 
  Momocs::Out(x = outlines_BOTH_centered_scaled_sl$coo,
              fac = outlines_metadata_sym[match(names(outlines_BOTH_centered_scaled_sl$coo), outlines_metadata_sym$outline_name),])



########### PCA
outlines_PCA <-
  Momocs::PCA(outlines_efourier) # PCA on Coe objects

# plot % of variation captured per PC axis
scree_plot <- 
  Momocs::scree_plot(outlines_PCA,
                     nax = 1:12) +  
  theme_bw() +
  theme(text = element_text(size=20))
# scree_plot

# choose all PC axes as the minimum number of PC axes
minimum_no_of_pcs_outlines_PCA <- ncol(outlines_PCA$x)


# plot visualisation of PCs
gg <-
  Momocs::PCcontrib(outlines_PCA,
                    nax = 1:10,
                    sd.r = c(-2,-1,0,1,2))

pc_contrib_plot <-
  gg$gg +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        text = element_text(size=20))

# pc_contrib_plot

ggsave(plot = pc_contrib_plot,
       filename = file.path("3_output", "PCA_contrib_outlines_BOTH_centered_scaled_sl.png"),
       device = "png",
       width = 5,
       height = 7,
       units = "in")


##########
# plot PCA
##########

# Momocs::plot_PCA(outlines_PCA,
#                  labelpoints = T)


PCA_df <- 
  as.data.frame(outlines_PCA$x)
PCA_df$outline_name <- row.names(PCA_df)

PCA_df <- 
  dplyr::left_join(
    outlines_BOTH_centered_scaled_sl_sym$fac,
    PCA_df)


PCA_df$Period <- factor(PCA_df$Period,
                        levels = c("Bølling/GI-1e", "Allerød/GI-1a-c", "Comparison"))
PCA_df$Site <- factor(PCA_df$Site,
                      levels = c("Oldeholtwolde", 
                                 "Jels 2",
                                 "Egtved mark",
                                 "Højgård",
                                 "Trollesgave",
                                 "Bromme",
                                 "Comparison (direct)",
                                 "Comparison (indirect)"
                      ))

#########################################################################
#######################   plot PCA  #####################################
#########################################################################

# PCs 1+2

PCA_1_2 <- 
  ggplot(data = PCA_df) +
  geom_point(size = 3,
             aes(x = PC1,
                 y = PC2,
                 shape = Site,
                 fill = Period,
                 color = Period)) +
  theme_bw() +
  ggplot2::scale_fill_manual(values = c("Allerød/GI-1a-c" = "forestgreen", 
                                        "Bølling/GI-1e" = "blue4",
                                        "Comparison" = "grey70")) +
  ggplot2::scale_color_manual(values = c("Allerød/GI-1a-c" = "forestgreen", 
                                         "Bølling/GI-1e" = "blue4",
                                         "Comparison" = "black")) +
  ggplot2::scale_shape_manual(values = c("Egtved mark" = 16,
                                         "Højgård" = 17,
                                         "Trollesgave" = 8,
                                         "Oldeholtwolde" = 13,
                                         "Jels 2" = 14,
                                         "Bromme" = 9,
                                         "Comparison (direct)" = 23,
                                         "Comparison (indirect)" = 21)) +
  ggplot2::coord_fixed() 
PCA_1_2

ggplot2::ggsave(filename = "PCA_1_2_outlines_BOTH_centered_scaled_sl.png",
                plot = PCA_1_2,
                path = file.path("3_output"),
                width = 20, height = 20, units = "cm",
                dpi = "retina",
                bg = "white")



PCA_1_2_ellipse_facetWrap <- 
  PCA_1_2 + 
  stat_ellipse(data = PCA_df,
               aes(x = PC1,
                   y = PC2,
                   color = Site)) +
  facet_wrap(~Site) +
  geom_vline(xintercept = 0,
             size = 0.5,
             linetype="dotted") +
  geom_hline(yintercept = 0,
             size = 0.5,
             linetype="dotted")
PCA_1_2_ellipse_facetWrap

ggplot2::ggsave(filename = "PCA_1_2_outlines_BOTH_centered_scaled_sl_ellipse_facetWrap.png",
                plot = PCA_1_2_ellipse_facetWrap,
                path = file.path("3_output"),
                width = 30, height = 17, units = "cm",
                dpi = "retina",
                bg = "white")



# PCs 3+4

PCA_3_4 <- 
  ggplot(data = PCA_df) +
  geom_point(size = 3,
             aes(x = PC3,
                 y = PC4,
                 shape = Site,
                 fill = Period,
                 color = Period)) +
  theme_bw() +
  ggplot2::scale_fill_manual(values = c("Allerød/GI-1a-c" = "forestgreen", 
                                        "Bølling/GI-1e" = "blue4",
                                        "Comparison" = "grey70")) +
  ggplot2::scale_color_manual(values = c("Allerød/GI-1a-c" = "forestgreen", 
                                         "Bølling/GI-1e" = "blue4",
                                         "Comparison" = "black")) +
  ggplot2::scale_shape_manual(values = c("Egtved mark" = 16,
                                         "Højgård" = 17,
                                         "Trollesgave" = 8,
                                         "Oldeholtwolde" = 13,
                                         "Jels 2" = 14,
                                         "Bromme" = 9,
                                         "Comparison (direct)" = 23,
                                         "Comparison (indirect)" = 21)) +
  ggplot2::coord_fixed() 
PCA_3_4

ggplot2::ggsave(filename = "PCA_3_4_outlines_BOTH_centered_scaled_sl.png",
                plot = PCA_3_4,
                path = file.path("3_output"),
                width = 20, height = 20, units = "cm",
                dpi = "retina",
                bg = "white")



PCA_3_4_ellipse_facetWrap <- 
  PCA_3_4 + 
  stat_ellipse(data = PCA_df,
               aes(x = PC3,
                   y = PC4,
                   color = Site)) +
  facet_wrap(~Site) +
  geom_vline(xintercept = 0,
             size = 0.5,
             linetype="dotted") +
  geom_hline(yintercept = 0,
             size = 0.5,
             linetype="dotted")
PCA_3_4_ellipse_facetWrap

ggplot2::ggsave(filename = "PCA_3_4_outlines_BOTH_centered_scaled_sl_ellipse_facetWrap.png",
                plot = PCA_3_4_ellipse_facetWrap,
                path = file.path("3_output"),
                width = 30, height = 17, units = "cm",
                dpi = "retina",
                bg = "white")

# PCs 5+6

PCA_5_6 <- 
  ggplot(data = PCA_df) +
  geom_point(size = 3,
             aes(x = PC5,
                 y = PC6,
                 shape = Site,
                 fill = Period,
                 color = Period)) +
  theme_bw() +
  ggplot2::scale_fill_manual(values = c("Allerød/GI-1a-c" = "forestgreen", 
                                        "Bølling/GI-1e" = "blue4",
                                        "Comparison" = "grey70")) +
  ggplot2::scale_color_manual(values = c("Allerød/GI-1a-c" = "forestgreen", 
                                         "Bølling/GI-1e" = "blue4",
                                         "Comparison" = "black")) +
  ggplot2::scale_shape_manual(values = c("Egtved mark" = 16,
                                         "Højgård" = 17,
                                         "Trollesgave" = 8,
                                         "Oldeholtwolde" = 13,
                                         "Jels 2" = 14,
                                         "Bromme" = 9,
                                         "Comparison (direct)" = 23,
                                         "Comparison (indirect)" = 21)) +
  ggplot2::coord_fixed() 
PCA_5_6

ggplot2::ggsave(filename = "PCA_5_6_outlines_BOTH_centered_scaled_sl.png",
                plot = PCA_5_6,
                path = file.path("3_output"),
                width = 20, height = 20, units = "cm",
                dpi = "retina",
                bg = "white")



PCA_5_6_ellipse_facetWrap <- 
  PCA_5_6 + 
  stat_ellipse(data = PCA_df,
               aes(x = PC5,
                   y = PC6,
                   color = Site)) +
  facet_wrap(~Site) +
  geom_vline(xintercept = 0,
             size = 0.5,
             linetype="dotted") +
  geom_hline(yintercept = 0,
             size = 0.5,
             linetype="dotted")
PCA_5_6_ellipse_facetWrap

ggplot2::ggsave(filename = "PCA_5_6_outlines_BOTH_centered_scaled_sl_ellipse_facetWrap.png",
                plot = PCA_5_6_ellipse_facetWrap,
                path = file.path("3_output"),
                width = 30, height = 17, units = "cm",
                dpi = "retina",
                bg = "white")


#########################################################################
####################### disparity  ######################################
#########################################################################


source(file.path("2_scripts", "13_dispRity_FUN.R"))


disparity_subsets <- 
  all_in_one_disparity_FUN(data = as.data.frame(PCA_df), 
                           subset_by = "Site", 
                           ncol_PCA = which(colnames(PCA_df) == "PC1"):ncol(PCA_df)) +
  ggplot2::scale_fill_manual(values = c("Egtved mark" = "forestgreen",
                                        "Højgård" = "forestgreen",
                                        "Trollesgave" = "forestgreen",
                                        "Bromme" = "forestgreen",
                                        "Oldeholtwolde" = "blue4",
                                        "Jels 2" = "blue4")) 
disparity_subsets

ggplot2::ggsave(filename = "disparity_subsets_outlines_BOTH_centered_scaled_sl.png",
                plot = disparity_subsets,
                path = file.path("3_output"),
                width = 35, height = 17, units = "cm",
                dpi = "retina",
                bg = "white")

############# plot symmetry
# plot symmetry
symmetry_boxplots <- 
  ggplot(data = PCA_df,
         aes(x = Site,
             y = sym,
             fill = Period)) +
  geom_boxplot() +
  theme_bw()  +
  ylab("Symmetry") +
  ggplot2::scale_fill_manual(values = c("Allerød/GI-1a-c" = "forestgreen", 
                                        "Bølling/GI-1e" = "blue4")) 
symmetry_boxplots

ggplot2::ggsave(filename = "symmetry_boxplots_outlines_BOTH_centered_scaled_sl.png",
                plot = symmetry_boxplots,
                path = file.path("3_output"),
                width = 35, height = 17, units = "cm",
                dpi = "retina",
                bg = "white")


# coefficient of variation for sym
symmetry_CV_list <- list()
for (current_site in levels(PCA_df$Site)) {
  
  data <- subset(PCA_df, Site == current_site)
  symmetry_CV_list[[current_site]] <- data.frame(Site = current_site,
                                                 N = nrow(data),
                                                 cv = sd(data$sym) / mean(data$sym) * 100)
}
do.call(rbind.data.frame, symmetry_CV_list)


kruskal.test(sym ~ Site, data = PCA_df) %>% #Kruskal-Wallis test for comparing symmetry values among groups
  broom::tidy()

pairwise.wilcox.test(PCA_df$sym, PCA_df$Site, p.adj = "bonf") %>% #pairwise Mann-Whitney-U test by Site
  broom::tidy()


ggplot(data = PCA_df) +
  geom_point(size = 3,
             aes(x = length,
                 y = sym,
                 shape = Site,
                 color = Period)) +
  theme_bw() +
  ggplot2::scale_color_manual(values = c("Allerød/GI-1a-c" = "forestgreen", 
                                         "Bølling/GI-1e" = "blue4",
                                         "Comparison" = "grey70")) +
  ggplot2::scale_shape_manual(values = c("Egtved mark" = 16,
                                         "Højgård" = 17,
                                         "Trollesgave" = 8,
                                         "Bromme" = 9,
                                         "Oldeholtwolde" = 13,
                                         "Jels 2" = 14,
                                         "Comparison" = 18)) 




