###### 0. 加载包 ####
# library(terra)
# library(tidyverse)
# library(raster)
# 
# setwd("D:/Graduation_Thesis")

# ##################   02 欧洲美洲phe_DOY栅格数据 合并 (6)  #################################
# 
# # 定义需要处理的 phe 文件类型
# phe_events <- c("phe1_DOY_", "phe2_DOY_", "phe3_DOY_", "phe4_DOY_", "phe5_DOY_", "phe6_DOY_")
# 
# 
# # 循环处理每种 phe 文件类型
# for (j in 1:length(phe_events)) {
#   
#   # 构建对应的文件列表，动态模式匹配每个 phe_x
#   file_list1 <- list.files("./NA_Results/2.phe_DOY_PHE_analysis/0common_pixel/", 
#                            pattern = paste0(phe_events[j], ".*\\.tif$"), 
#                            full.names = TRUE)
#   file_list11 <- list.files("./EA_Results/2.phe_DOY_PHE_analysis/0common_pixel/", 
#                             pattern = paste0(phe_events[j], ".*\\.tif$"), 
#                             full.names = TRUE)
#   
#   # 将文件列表转换为栅格对象
#   rasters1 <- lapply(file_list1, rast)
#   rasters11 <- lapply(file_list11, rast)
#   
#   # 动态创建文件夹名  不要phe1_DOY_中的最后一个下划线而形成文件名（ sub("_$", "", phe_events[j])）
#   output_folder <- paste0("./EA+NA_Results/merged_Phe_DOY_years_2_PHE_analysis/merged_", sub("_$", "", phe_events[j]))
#   
#   # 检查并创建输出文件夹（如果不存在）
#   if (!dir.exists(output_folder)) {
#     dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)
#   }
#   
#   # 循环遍历每个栅格文件并合并
#   for (i in seq_along(rasters1)) {
#     # 合并栅格
#     merged_raster <- merge(rasters1[[i]], rasters11[[i]])
#     
#     # 提取年份（假设文件名后4位表示年份）
#     year <- substr(basename(file_list1[i]), nchar(basename(file_list1[i])) - 7, nchar(basename(file_list1[i])) - 4)
#     
#     # 定义输出文件路径
#     filename <- file.path(output_folder, paste0("EA+NA_merged_", phe_events[j], year, ".tif"))
#     
#     # 写入合并后的栅格文件
#     writeRaster(merged_raster, filename = filename, overwrite = TRUE)
#   }
# }


################# 0. 加载包 ####   ##接上一步得到的EA+NA与三种林分10年未变像元的common phe结果操作
library(terra)
library(tidyverse)

setwd("D:/Graduation_Thesis")

################## 01. 计算每个阶段的天数--EA+NA每年 #################################

# 定义phe事件类型和对应的阶段
phe_events <- c("phe1_DOY_", "phe2_DOY_", "phe3_DOY_", "phe4_DOY_", "phe5_DOY_", "phe6_DOY_")
phase_combinations <- list(
  days_13 = c(1, 3),
  days_34 = c(3, 4),
  days_46 = c(4, 6))

# 创建输出根目录
output_root <- "./EA+NA_Results/merged_Phe_days_counts_2_PHE_analysis/"
if (!dir.exists(output_root)) {
  dir.create(output_root, recursive = TRUE)
}

# 循环遍历每个阶段组合
for (phase_name in names(phase_combinations)) {
  
  # phase_name = "days_12"
  
  phases <- phase_combinations[[phase_name]]
  
  # 创建阶段输出文件夹
  output_folder <- file.path(output_root, phase_name)
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  
  # 提取对应phe事件的文件夹
  phe_start <- phe_events[phases[1]]
  phe_end <- phe_events[phases[2]]
  
  # 获取两个事件的文件列表
  file_list_start <- list.files(
    paste0("./EA+NA_Results/merged_Phe_DOY_years_2_PHE_analysis/merged_", sub("_$", "", phe_start)),
    pattern = "\\.tif$", full.names = TRUE
  )
  file_list_end <- list.files(
    paste0("./EA+NA_Results/merged_Phe_DOY_years_2_PHE_analysis/merged_", sub("_$", "", phe_end)),
    pattern = "\\.tif$", full.names = TRUE
  )
  
  # 检查文件列表是否匹配
  if (length(file_list_start) != length(file_list_end)) {
    stop("Start and end phe files do not match in number.")
  }
  
  # 计算每年的阶段天数
  for (i in seq_along(file_list_start)) {
    # 读取栅格文件
    # i=1 
    
    phe_start_raster <- rast(file_list_start[i])
    phe_end_raster <- rast(file_list_end[i])
    
    # 计算天数差异
    days_raster <- phe_end_raster - phe_start_raster
    
    # 提取年份
    year <- substr(basename(file_list_start[i]), nchar(basename(file_list_start[i])) - 7, nchar(basename(file_list_start[i])) - 4)
    
    # 定义输出文件路径
    output_file <- file.path(output_folder, paste0(phase_name, "_", year, ".tif"))
    
    # 保存结果
    writeRaster(days_raster, filename = output_file, overwrite = TRUE)
  }
}



################## 02. 计算每个阶段的天数--EA+NA-10年平均 #################################

# 设置输出根目录
output_root <- "./EA+NA_Results/merged_Phe_days_counts_2_PHE_analysis/"

# 定义phe事件类型和阶段组合
phase_combinations <- list(
  days_13 = c(1, 3),
  days_34 = c(3, 4),
  days_46 = c(4, 6))

# 循环遍历每个阶段组合
for (phase_name in names(phase_combinations)) {
  
  # 获取该阶段的phe事件
  phases <- phase_combinations[[phase_name]]
  
  # 获取对应phe事件的文件列表
  phe_start <- phe_events[phases[1]]
  phe_end <- phe_events[phases[2]]
  
  # 获取开始和结束事件的文件列表
  file_list_start <- list.files(
    paste0("./EA+NA_Results/merged_Phe_DOY_years_2_PHE_analysis/merged_", sub("_$", "", phe_start)),
    pattern = "\\.tif$", full.names = TRUE
  )
  file_list_end <- list.files(
    paste0("./EA+NA_Results/merged_Phe_DOY_years_2_PHE_analysis/merged_", sub("_$", "", phe_end)),
    pattern = "\\.tif$", full.names = TRUE
  )
  
  # 检查文件列表是否匹配
  if (length(file_list_start) != length(file_list_end)) {
    stop("Start and end phe files do not match in number.")
  }
  
  # 计算每年阶段天数的均值
  # 创建空列表保存每年栅格数据
  raster_stack_list <- list()
  
  for (i in seq_along(file_list_start)) {
    # 读取栅格文件
    phe_start_raster <- rast(file_list_start[i])
    phe_end_raster <- rast(file_list_end[i])
    
    # 计算天数差异
    days_raster <- phe_end_raster - phe_start_raster
    
    # 将计算结果添加到栅格堆栈列表中
    raster_stack_list[[i]] <- days_raster
  }
  
  # 创建栅格堆栈对象
  raster_stack <- do.call(c, raster_stack_list)
  
  # 计算均值栅格
  mean_raster <- mean(raster_stack, na.rm = TRUE)
  
  # 定义输出文件路径
  output_file <- file.path(output_root, paste0(phase_name, "_mean.tif"))
  
  # 保存均值栅格
  writeRaster(mean_raster, filename = output_file, overwrite = TRUE)
  
  # 提示完成
  message(paste0("Mean raster for ", phase_name, " has been saved at: ", output_file))
}

