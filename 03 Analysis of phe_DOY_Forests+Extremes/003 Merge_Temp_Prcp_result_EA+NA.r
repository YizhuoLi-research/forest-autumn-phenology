rm(list = ls())

##################   00 加载包  ######################################################################

library(terra)
library(tidyverse)
library(raster)

setwd("D:/Graduation_Thesis")


##################   01 欧洲美洲3.Clim_Duration_Extremes影像合并  ####################################

# 设置输入文件路径
file_list1 <- list.files("./NA_Results/3.Clim_Duration_Extremes/", pattern = "\\.tif$", full.names = TRUE)
file_list2 <- list.files("./EA_Results/3.Clim_Duration_Extremes/", pattern = "\\.tif$", full.names = TRUE)


# 设置输出文件夹路径
output_path <- "./EA+NA_Results/"
output_folder <- file.path(output_path, "merged_Clim_Duration_Extremes_years")

# 创建输出文件夹（如果不存在）
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

file_list2_names <- basename(file_list2)


# 遍历 file_list1
for (file1 in file_list1) {
  # 提取文件名（不带路径）
  
  filename <- basename(file1)  # 只获取文件名，例如 "Duration_phe1_3_2013.tif"
  # filename <- basename("Temp_phe4_6_2022.tif")  # e.g., "Duration_phe1_3_2013.tif"
  
  
  # 构造 file2 的完整路径
  file2 <- file.path("./EA_Results/3.Clim_Duration_Extremes/", filename)
  
  
  # 检查 file2 是否存在（只比较文件名）
  if (filename %in% file_list2_names) {
    # 读取两个栅格
    raster1 <- rast(file1)
    raster2 <- rast(file2)
    
    # 合并两个栅格
    merged_raster <- merge(raster1, raster2)
    
    # 生成新的文件名
    merged_filename <- paste0("merged_", tools::file_path_sans_ext(filename), ".tif")
    
    # 保存合并后的栅格
    writeRaster(merged_raster, filename = file.path(output_folder, merged_filename), overwrite = TRUE)
    
    cat("Merged and saved:", merged_filename, "\n")
  } else {
    cat("No matching file found for:", filename, "\n")
  }
}

cat("All matching files processed.\n")


# # 检验
# a <- rast("D:/Graduation_Thesis/EA+NA_Results/merged_Clim_Duration_Extremes_years/merged_Temp_phe1_3_2016.tif")
# r1 <- rast("D:/Graduation_Thesis/EA_Results/3.Clim_Duration_Extremes/Temp_phe1_3_2016.tif")
# r2 <- rast("D:/Graduation_Thesis/NA_Results/3.Clim_Duration_Extremes/Temp_phe1_3_2016.tif")


##################   02 合并后的影像按照对应--物候阶段--文件夹归类  ####################################

merged_files <- list.files(output_folder, pattern = "^merged_.*\\.tif$", full.names = TRUE)


# 遍历所有已合并的 .tif 文件
for (file in merged_files) {
  # 获取文件名
  filename <- basename(file)
  
  # 提取年份（文件名的最后4位）
  phe_phase <- substr(filename, nchar(filename) - 14, nchar(filename) - 9)
  
  # 构造年份文件夹路径
  phe_phase_folder <- file.path(output_folder, phe_phase)
  
  # 如果年份文件夹不存在，则创建
  if (!dir.exists(phe_phase_folder)) {
    dir.create(phe_phase_folder, recursive = TRUE)
  }
  
  # 移动文件到对应的年份文件夹
  file.rename(file, file.path(phe_phase_folder, filename))
  
  # 输出日志信息
  cat("Moved:", filename, "to", phe_phase_folder, "\n")
}



##################   03 将Temp、Prcp文件（5层）按层名称拆分  ####################################

#library(terra)

# 定义要处理的目录列表
phe_dirs <- c("phe1_3", "phe3_4", "phe4_6")

# 定义批量处理函数
split_raster_layers <- function(file_path) {
  # 读取栅格文件
  r <- rast(file_path)
  
  # 获取层的名称
  layer_names <- names(r)
  
  # 提取年份（文件名的最后四位）
  year <- sub(".*_(\\d{4})\\.tif$", "\\1", basename(file_path))
  
  # 逐层保存
  for (i in seq_along(layer_names)) {
    # 生成新文件名
    new_filename <- paste0(dirname(file_path), "/", layer_names[i], "_", year, ".tif")
    
    # 写入新文件
    writeRaster(r[[i]], new_filename, overwrite = TRUE)
  }
  
  # 删除原文件
  file.remove(file_path)
}

# 遍历每个目录并处理文件
for (dir in phe_dirs) {
  # 获取当前目录的所有 .tif 文件
  files_in_dir <- list.files(paste0("./EA+NA_Results/merged_Clim_Duration_Extremes_years/", dir), 
                             full.names = TRUE, pattern = "\\.tif$")
  
  # 按变量分类
  prcp_files <- grep("merged_Prcp", files_in_dir, value = TRUE)
  temp_files <- grep("merged_Temp", files_in_dir, value = TRUE)
  
  # 批量处理降水数据
  lapply(prcp_files, split_raster_layers)
  
  # 批量处理温度数据
  lapply(temp_files, split_raster_layers)
  
  cat(paste0("处理完成：", dir, "\n"))
}

cat("所有目录的降水和温度数据已拆分并删除原文件！\n")




# ##################   04 合并后的影像按照对应year文件夹归类  ####################################
# 
# merged_files <- list.files(output_folder, pattern = "^merged_.*\\.tif$", full.names = TRUE)
# 
# 
# # 遍历所有已合并的 .tif 文件
# for (file in merged_files) {
#   # 获取文件名
#   filename <- basename(file)
#   
#   # 提取年份（文件名的最后4位）
#   year <- substr(filename, nchar(filename) - 7, nchar(filename) - 4)
#   
#   # 构造年份文件夹路径
#   year_folder <- file.path(output_folder, year)
#   
#   # 如果年份文件夹不存在，则创建
#   if (!dir.exists(year_folder)) {
#     dir.create(year_folder, recursive = TRUE)
#   }
#   
#   # 移动文件到对应的年份文件夹
#   file.rename(file, file.path(year_folder, filename))
#   
#   # 输出日志信息
#   cat("Moved:", filename, "to", year_folder, "\n")
# }










