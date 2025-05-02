library(magrittr)

# load outlines
outlines_combined_centered_metadata <- 
  readRDS(file.path("1_data", "outlines_combined_interp_centered_scaled_sl_metadata.RDS"))

symmetry_CV_list <- list()
for (current_site in unique(outlines_combined_centered_metadata$fac$site_id)) {
  
  outlines_SITESindivid <- Momocs::filter(outlines_combined_centered_metadata,
                                          site_id == current_site)
  
  # center, scale, slide direction
  outlines_SITESindivid_centered <- Momocs::coo_center(outlines_SITESindivid)
  outlines_SITESindivid_centered_scaled <- Momocs::coo_scale(outlines_SITESindivid_centered)
  outlines_SITESindivid_centered_scaled_sl <- Momocs::coo_slidedirection(outlines_SITESindivid_centered_scaled, direction = "up")

  # HARMONIC CALIBRATION
   outlines_harmonics <- 
    Momocs::calibrate_harmonicpower_efourier(outlines_SITESindivid_centered_scaled_sl, 
                                             plot = F)  

  #  EFOURIER FILE CREATION
  outlines_efourier <- 
    Momocs::efourier(outlines_SITESindivid_centered_scaled_sl,
                     nb.h = as.matrix(outlines_harmonics[["minh"]])[[4,1]], # harmonics for 99.9% 
                     norm = F) # see above
  
  # symmetry measurements from efourier file
  outlines_efourier_symmetry <-
    Momocs::symmetry(outlines_efourier) %>% #extraction of symmetry values
    as.data.frame() %>% 
    tibble::rownames_to_column(var = "outline_name")
  
  # add symmetry data to outline-fac
  outlines_metadata_sym <- 
    dplyr::left_join(outlines_SITESindivid_centered_scaled_sl$fac,
                     outlines_efourier_symmetry,
                     by = "outline_name")
  
  outlines_SITESindivid_centered_scaled_sl_sym <- 
    Momocs::Out(x = outlines_SITESindivid_centered_scaled_sl$coo,
                fac = outlines_metadata_sym[match(names(outlines_SITESindivid_centered_scaled_sl$coo), outlines_metadata_sym$outline_name),])
  
  
  
  # PCA
  outlines_PCA <-
    Momocs::PCA(outlines_efourier) # PCA on Coe objects
  
  # plot % of variation captured per PC axis
  scree_plot <- 
    Momocs::scree_plot(outlines_PCA,
                       nax = 1:12) +  
    theme_bw() +
    theme(text = element_text(size=20))
  
  # choose all PC axes as the minimum number of PC axes
  minimum_no_of_pcs_outlines_PCA <- 
    ncol(outlines_PCA$x)
  
  # plot visualisation of PCs
  if(minimum_no_of_pcs_outlines_PCA >= 10){
    
    gg <-
      Momocs::PCcontrib(outlines_PCA,
                        nax = 1:10,
                        sd.r = c(-2,-1,0,1,2))
  } else {
    
    gg <-
      Momocs::PCcontrib(outlines_PCA,
                        sd.r = c(-2,-1,0,1,2))
  }
  
  pc_contrib_plot <-
    gg$gg +
    theme_bw() +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          text = element_text(size=20))
  
  ggsave(plot = pc_contrib_plot,
         filename = file.path("3_output", "sites_individually", paste0(current_site, "_PCA_contrib_outlines_SITESindividually.png")),
         device = "png",
         width = 5,
         height = 7,
         units = "in")
  

  # plot PCA

  PCA_df <- 
    as.data.frame(outlines_PCA$x)
  PCA_df$outline_name <- row.names(PCA_df)
  
  PCA_df <- 
    dplyr::left_join(
      outlines_SITESindivid_centered_scaled_sl_sym$fac,
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
  # PCA_1_2
  ggplot2::ggsave(filename = paste0(current_site, "_PCA_1_2_outlines_SITESindivid.png"),
                  plot = PCA_1_2,
                  path = file.path("3_output", "sites_individually"),
                  width = 20, height = 20, units = "cm",
                  dpi = "retina",
                  bg = "white")
  
  png(filename = file.path("3_output", "sites_individually", paste0(current_site, "_PCA_1_2_outlines_SITESindivid_MomocsPCA.png")), 
      units="px", width=2500, height=2000, res=350, 
      bg = "white")
  Momocs::plot_PCA(outlines_PCA, morphospace_position = "xy")
  dev.off()
  
  # symmetry
  # coefficient of variation for sym

  symmetry_CV_list[[current_site]] <- data.frame(Site = current_site,
                                                 N = nrow(PCA_df),
                                                 mean = mean(PCA_df$sym),
                                                 median = median(PCA_df$sym),
                                                 sd = sd(PCA_df$sym),
                                                 cv = sd(PCA_df$sym) / mean(PCA_df$sym) * 100)

  # clear workspace
  rm(list = ls()[which(ls() != "outlines_combined_centered_metadata" & ls() != "symmetry_CV_list")]) # neither symmetry_CV_list
  
}
do.call(rbind.data.frame, symmetry_CV_list)
