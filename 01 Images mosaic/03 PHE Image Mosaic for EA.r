library(raster)
# 文件夹路径
setwd("D:/VegetationImpact/01 Download/03 PhenologicalEvents_download/EA_PHE")

#######################  2.文件的纬度带合并 EA  ############################

# 获取当前工作目录中所有的tif文件
tif_files <- list.files(pattern = "\\.tif$", full.names = TRUE)

# 创建一个列表，按照文件名前25个字符分组
file_groups <- list()
for (file in tif_files) {
  file_name <- basename(file)
  key <- substr(file_name, 1, 30)
  if (!key %in% names(file_groups)) {
    file_groups[[key]] <- list()
  }
  file_groups[[key]] <- c(file_groups[[key]], file)
}

# # 显示输出每个分组中的文件列表
# for (key in names(file_groups)) {
#   cat(paste("文件名前30个字符为 '", key, "' 的文件组:\n", sep = ""))
#   cat(paste(file_groups[[key]], collapse = "\n"))
#   cat("\n---------------\n")
# }

for (key in names(file_groups)) {
  cat(paste("文件名前25个字符为 '", key, "' 的文件组:\n", sep = ""))
  
  # 读取文件列表中的所有Raster文件
  raster_list <- lapply(file_groups[[key]], raster)
  
  # 进行拼接
  raster_list$fun <- mean
  raster_list$na.rm <- TRUE
  merged_raster <- do.call(mosaic, raster_list)
  
  # 生成输出文件名 # 将参数转换为字符向量
  first_file <- as.character(file_groups[[key]][1]) 
  prefix <- substr(first_file, 1, 30)  # 提取前30个字符
  
  # 设定输出文件夹路径
  output_folder <- "D:/VegetationImpact/01 Download/03 PhenologicalEvents_download/EA_PHE/EA_PHE_merged"
  if (!file.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  
  output_path <- file.path(output_folder, paste0(prefix, "_merged.tif"))
  
  # 保存拼接后的文件
  writeRaster(merged_raster, filename = output_path, format = "GTiff", 
              options = "COMPRESS=LZW",overwrite=TRUE)
  
  cat("拼接完成，文件已保存至", output_path, "\n")
}

#######################  2. 文件按年份分组  ##############################

setwd("D:/VegetationImpact/01 Download/03 PhenologicalEvents_download/EA_PHE/EA_PHE_merged")

# Get a list of all TIFF files in the directory
merged_tif_files <- list.files(pattern = "\\.tif$", full.names = TRUE)

# Create folders based on the year and move files to respective folders
for (file in merged_tif_files) {
  # Extract the year from the file name (first four characters)
  year <- substr(basename(file), 1, 4)
  
  # Create a directory for the year if it doesn't exist
  year_folder <- file.path("D:/VegetationImpact/01 Download/03 PhenologicalEvents_download/EA_PHE/EA_PHE_merged", year)
  if (!dir.exists(year_folder)) {
    dir.create(year_folder)
  }
  
  # Move the file to the respective year folder
  file.rename(file, file.path(year_folder, basename(file)))
  cat("Moved", basename(file), "to", year, "folder.\n")
}

########################### WELL DONE!!!#####################################



