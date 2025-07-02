library(raster)

# Set the folder path
setwd("D:/Graduation_Thesis/01 Download/10 JRC_GFC2020/NA_GFC")

# Get all the .tif files in the current working directory
tif_files <- list.files(pattern = "\\.tif$", full.names = TRUE)

# Create an empty list to store the rasters
raster_list <- list()

# Create an empty list to store the rasters
for (file in tif_files) {
  r <- raster(file)
  raster_list[[length(raster_list) + 1]] <- r
}

# Merge the rasters, taking the mode for overlapping areas
merged_raster <- do.call(mosaic, c(raster_list, fun = modal, na.rm = TRUE))

# Display the result
plot(merged_raster)

# Output the merged raster
output_folder <- "D:/Graduation_Thesis/01 Download/10 JRC_GFC2020/NA_Results"
output_file <- file.path(output_folder, "NA_GFC_30km.tif")

# Check if the output folder exists, if not, create it
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

writeRaster(merged_raster, filename = output_file, format = "GTiff", overwrite = TRUE)
writeRaster(merged_raster, filename = "D:/Graduation_Thesis/NA_Results/NA_GFC_30km.tif", format = "GTiff", overwrite = TRUE)



