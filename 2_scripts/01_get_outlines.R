# extract the outlines, interpolate to 3000 semi-landmarks, center, scale, etc., save.

library(outlineR)

inpath <- file.path("1_data", "prep_rescaled")
outpath <- file.path("1_data", "outlineR_out")

# # separate artefacts
# separate_single_artefacts(inpath = inpath,
#                           outpath = outpath)

# # extract outlines
single_outlines_list <- get_outlines(outpath = outpath, 
                                     tps_file_rescale = NULL)
outlines_combined <- combine_outlines(single_outlines_list = single_outlines_list)

# # interpolate to 3000 semi-landmarks as in Radinovic & Kajtez 2021
outlines_combined_interp <- Momocs::coo_interpolate(outlines_combined, 
                                                    n = 3000)

# # center, scale, slide direction
outlines_combined_interp_centered <- Momocs::coo_center(outlines_combined_interp)
outlines_combined_interp_centered_scaled <- Momocs::coo_scale(outlines_combined_interp_centered)
outlines_combined_interp_centered_scaled_sl <- Momocs::coo_slidedirection(outlines_combined_interp_centered_scaled, direction = "up")

# # check number of semi-landmarks
Momocs::coo_nb(outlines_combined_interp_centered_scaled_sl)

# # inspect
length(outlines_combined_interp_centered_scaled_sl) #how many outlines do you have?
stack(outlines_combined_interp_centered_scaled_sl) # shows all outlines above one another(you might want to center and scale them first using Momocs)
Momocs::panel(outlines_combined_interp_centered_scaled_sl) # shows all outlines next to each other

# # save the outlines
saveRDS(outlines_combined_interp_centered_scaled_sl,
        file = file.path("1_data", "outlines_combined_interp_centered_scaled_sl.RDS"))
