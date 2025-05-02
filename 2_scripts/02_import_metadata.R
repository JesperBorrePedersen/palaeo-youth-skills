# # import meta-data

data <- readr::read_csv(file.path("1_data", "child_data.csv"))
data

# missing:
# jels 2
# boerneck-ost

# # import outlines
outlines_combined_interp_centered_scaled_sl <- readRDS(file = file.path("1_data", 
                                                                        "outlines_combined_interp_centered_scaled_sl.RDS"))

# are there outlines that have no entry in the spreadsheet?
names(outlines_combined_interp_centered_scaled_sl$coo)[which(!(names(outlines_combined_interp_centered_scaled_sl$coo) %in% data$outline_name))]

# subset spreadsheet to rows that have an associated outline
data_outlines <- 
  subset(data, outline_name %in% names(outlines_combined_interp_centered_scaled_sl$coo))

# combine outlines with metadata (in the right order)
outlines_combined_interp_centered_scaled_sl_metadata <- 
  Momocs::Out(x = outlines_combined_interp_centered_scaled_sl$coo,
              fac = data_outlines[match(names(outlines_combined_interp_centered_scaled_sl$coo), data_outlines$outline_name),])

# save the outlines
saveRDS(outlines_combined_interp_centered_scaled_sl_metadata,
        file = file.path("1_data", "outlines_combined_interp_centered_scaled_sl_metadata.RDS"))
