###### 0. 加载包 ####
library(terra)
library(tidyverse)
library(raster)

setwd("D:/Graduation_Thesis")

##################   02 欧洲美洲phe_DOY栅格数据 合并 (6)  #################################

# 定义需要处理的 phe 文件类型
phe_events <- c("phe1_DOY_", "phe2_DOY_", "phe3_DOY_", "phe4_DOY_", "phe5_DOY_", "phe6_DOY_")


# 循环处理每种 phe 文件类型
for (j in 1:length(phe_events)) {
  
  # 构建对应的文件列表，动态模式匹配每个 phe_x
  file_list1 <- list.files("./NA_Results/2.phe_DOY_PHE_analysis/0common_pixel/", 
                           pattern = paste0(phe_events[j], ".*\\.tif$"), 
                           full.names = TRUE)
  file_list11 <- list.files("./EA_Results/2.phe_DOY_PHE_analysis/0common_pixel/", 
                            pattern = paste0(phe_events[j], ".*\\.tif$"), 
                            full.names = TRUE)
  
  # 将文件列表转换为栅格对象
  rasters1 <- lapply(file_list1, rast)
  rasters11 <- lapply(file_list11, rast)
  
  # 动态创建文件夹名  不要phe1_DOY_中的最后一个下划线而形成文件名（ sub("_$", "", phe_events[j])）
  output_folder <- paste0("./EA+NA_Results/merged_Phe_DOY_years_2_PHE_analysis/merged_", sub("_$", "", phe_events[j]))
  
  # 检查并创建输出文件夹（如果不存在）
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)
  }
  
  # 循环遍历每个栅格文件并合并
  for (i in seq_along(rasters1)) {
    # 合并栅格
    merged_raster <- merge(rasters1[[i]], rasters11[[i]])
    
    # 提取年份（假设文件名后4位表示年份）
    year <- substr(basename(file_list1[i]), nchar(basename(file_list1[i])) - 7, nchar(basename(file_list1[i])) - 4)
    
    # 定义输出文件路径
    filename <- file.path(output_folder, paste0("EA+NA_merged_", phe_events[j], year, ".tif"))
    
    # 写入合并后的栅格文件
    writeRaster(merged_raster, filename = filename, overwrite = TRUE)
  }
}



# 对于单一物候事件的运行计算
# # phe1_DOY_. phe2_DOY_. phe3_DOY_. phe4_DOY_. phe5_DOY_. phe6_DOY_.

# file_list1 <- list.files("./NA_Results/2.phe_DOY_PHE_analysis/0common_pixel/", pattern = "phe6_DOY_.*\\.tif$", full.names = TRUE)
# file_list11 <- list.files("./EA_Results/2.phe_DOY_PHE_analysis/0common_pixel/", pattern = "phe6_DOY_.*\\.tif$", full.names = TRUE)
# 
# rasters1 <- sapply(file_list1, rast)
# rasters11 <- sapply(file_list11, rast)
# 
# output_path <- "./EA+NA_Results/merged_Phe_DOY_years_2_PHE_analysis"
# output_folder <- paste0(output_path, "merged_phe6_DOY")
# # 检查并创建文件夹
# if (!file.exists(output_folder)) { dir.create(output_folder)  }
# 
# for (i in seq_along(rasters1)) {
#   # i=1
#   merged_raster <- merge(rasters1[[i]], rasters11[[i]])
#   year <- substr(basename(file_list1[i]), nchar(basename(file_list1[i])) - 7, nchar(basename(file_list1[i])) - 4)
#   filename <- file.path(output_folder, paste0("EA+NA_merged_phe6_DOY_", year, ".tif"))
#   writeRaster(merged_raster, filename = filename, overwrite = TRUE)
# }

