###### 0. 加载包 ####
library(terra)
library(tidyverse)
library(raster)

setwd("D:/Graduation_Thesis")

##################   01 按照森林类型提取共有像元的栅格图像 (9年)  ################################

# Define the years to process
years <- 2013:2022

# 定义数据目录路径
input_dir  <- "./01 Download/03 PhenologicalEvents_download/EA_PHE/EA_PHE_merged/"
output_dir <- "./EA_Results/2.phe_DOY_PHE_analysis/0common_pixel/"
if (!dir.exists(output_dir)) {  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)}

for (year in years) {
  # year = 2014
  # 动态拼接文件路径
  phe1 <- rast(paste0(input_dir, year, "/", year, "-Onset_Greenness_Increas_merged.tif"))  # SOS
  phe2 <- rast(paste0(input_dir, year, "/", year, "-Date_Mid_Greenup_Phase__merged.tif"))  # MGP
  phe3 <- rast(paste0(input_dir, year, "/", year, "-Onset_Greenness_Maximum_merged.tif"))  # GMO
  phe4 <- rast(paste0(input_dir, year, "/", year, "-Onset_Greenness_Decreas_merged.tif"))  # GDO
  phe5 <- rast(paste0(input_dir, year, "/", year, "-Date_Mid_Senescence_Pha_merged.tif"))  # MSP
  phe6 <- rast(paste0(input_dir, year, "/", year, "-Onset_Greenness_Minimum_merged.tif"))  # EOS
  
# 构建共有像元样本
  sample <- phe1
  sample[is.finite(sample)] <- 1
  sample2 <- phe2
  sample2[is.finite(sample2)] <- 1
  sample3 <- phe3
  sample3[is.finite(sample3)] <- 1
  sample4 <- phe4
  sample4[is.finite(sample4)] <- 1
  sample5 <- phe5
  sample5[is.finite(sample5)] <- 1
  sample6 <- phe6
  sample6[is.finite(sample6)] <- 1
  
  valid_pixels <- sum(!is.na(values(phe5)))
  cat("有效像元数量：", valid_pixels, "\n")
  
  # 计算所有栅格的公共范围

  # 加载ForestType文件 # 找与ForestType文件共同像元
  # 创建掩膜，仅保留值为 1, 2, 3 的像元--对应自然林、次生林、人工林
  aa <- rast("D:/Graduation_Thesis/01 Download/10 JRC_GFC2020/EA_Results/EA_GFC_30km.tif")
  aa_filtered <- ifel(aa %in% c(1, 2, 3), 1, NA)  # 只保留 1, 2, 3 的像元
  sample_forest <- aa_filtered
  sample_forest[is.finite(sample_forest)] <- 1
  # #根据公共范围裁剪 sample_forest：
  # sample_forest_cropped <- crop(sample_forest[[1]], common_extent)
  
  common_mask <- sample_forest[[1]]*sample[[1]]*sample2[[1]]*sample3[[1]]*sample4[[1]]*sample5[[1]]*sample6[[1]]
  
  # Multiply all "phe" rasters by the common pixel mask
  phe1 <- phe1 * common_mask
  phe2 <- phe2 * common_mask
  phe3 <- phe3 * common_mask
  phe4 <- phe4 * common_mask
  phe5 <- phe5 * common_mask
  phe6 <- phe6 * common_mask
  

  valid_pixels <- sum(!is.na(values(phe5)))
  cat("有效像元数量：", valid_pixels, "\n")
  
  ##################################   02 去掉phe1-6数据异常的像元  ################################
  
  phe1 <- ceiling(phe1)     # 若有小数位进一天，因为在GEE中存在偶数去中值平均有0.5的情况
  phe2 <- ceiling(phe2)
  phe3 <- ceiling(phe3)
  phe4 <- ceiling(phe4)
  phe5 <- ceiling(phe5)
  phe6 <- ceiling(phe6)
   
  # 定义异常值范围
  valid_range <- c(1, 365)
  
  # 剔除每个栅格图像中的异常值
  phe1[phe1 < valid_range[1] | phe1 > valid_range[2]] <- NA
  phe2[phe2 < valid_range[1] | phe2 > valid_range[2]] <- NA
  phe3[phe3 < valid_range[1] | phe3 > valid_range[2]] <- NA
  phe4[phe4 < valid_range[1] | phe4 > valid_range[2]] <- NA
  phe5[phe5 < valid_range[1] | phe5 > valid_range[2]] <- NA
  phe6[phe6 < valid_range[1] | phe6 > valid_range[2]] <- NA
  
  # 合并栅格图像为多层
  PHE_q <- rast(list(phe1, phe2, phe3, phe4, phe5, phe6))                 # 检查栅格对象是否包含有效值
  
  # check_order <- function(pixels) {
  #   if (any(is.na(pixels))) {            # 输入中是否有缺失值，如果有，则返回NA向量
  #     return(rep(NA, 6))                 # 如果没有缺失值再进行比较
  #   } else {   if (pixels[6] - pixels[5] < 0 | pixels[5] - pixels[4] < 0 | pixels[4] - pixels[3] < 0 | 
  #                  pixels[3] - pixels[2] < 0 | pixels[2] - pixels[1] < 0) {
  #       return(rep(NA, 6)) # 不处理边界外数据
  #     } else {     return(pixels)     }   }  }
  
  # 定义逐像元检查函数（简化）
  check_order <- function(pixels) {
    if (any(is.na(pixels)) || any(diff(pixels) <= 0)) {
      return(rep(NA, 6))  # 若存在NA值或不满足【递增条件】，返回NA
    } else {
      return(pixels)      # 否则保留原值
    }
  }
  
  PHE <- app(PHE_q, check_order)     # 应用逐像元逻辑过滤

  
  # 检查计算非空值像元数量
  summary(PHE)
  valid_pixels <- sum(!is.na(values(PHE[[1]])))
  cat("有效像元数量：", valid_pixels, "\n")
  
  
  ###################################   03 输出结果phe1-phe6的DOY   #################################
  
  # 保存处理后的栅格文件
  # 写入结果文件
  writeRaster(PHE[[1]], paste0(output_dir, "phe1_DOY_", year, ".tif"), overwrite = TRUE)
  writeRaster(PHE[[2]], paste0(output_dir, "phe2_DOY_", year, ".tif"), overwrite = TRUE)
  writeRaster(PHE[[3]], paste0(output_dir, "phe3_DOY_", year, ".tif"), overwrite = TRUE)
  writeRaster(PHE[[4]], paste0(output_dir, "phe4_DOY_", year, ".tif"), overwrite = TRUE)
  writeRaster(PHE[[5]], paste0(output_dir, "phe5_DOY_", year, ".tif"), overwrite = TRUE)
  writeRaster(PHE[[6]], paste0(output_dir, "phe6_DOY_", year, ".tif"), overwrite = TRUE)
  
}


# 最终获得 按每年 phe1-Phe6得到的共有像元
# 注： 每年的像元数量是不一致的