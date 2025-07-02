rm(list = ls())

######################################   00 加载包  ##################################################
library(terra)
library(tidyverse)
library(raster)
library(ggplot2)
library(ppcor)
library(dplyr)
library(trend)

# library(ggpubr)

setwd("D:/Graduation_Thesis")

#############################   01.按照柯本气候类型，将气候区分为6个气候区域   #######################
#############################   Cfa,Cfb/Dfb,Dfc/Dwb,Dwc

r <- raster("./EA+NA_Results/EA+NA_koppen_30km_addClimate.tif")
r[1:30] <- seq(1,30,1)
r0 <- r[1:30]
r <- ratify(r) # Converts raster field to categorical data
rat <- levels(r)[[1]]
# Legend in alphabetic order
rat$climate <- c('Af', 'Am', 'As', 'Aw',
                 'BSh', 'BSk', 'BWh', 'BWk',
                 'Cfa', 'Cfb','Cfc',
                 'Csa', 'Csb','Csc',
                 'Cwa','Cwb', 'Cwc',
                 'Dfa', 'Dfb', 'Dfc','Dfd',
                 'Dsa', 'Dsb', 'Dsc','Dsd',
                 'Dwa', 'Dwb', 'Dwc','Dwd',
                 'EF',  'ET')
# Remove the placeholders
r[1:30] <- r0
#将修改后的属性表重新赋值给对象r
levels(r) <- rat

#划定Climate_Class_list中每个类型区域的边界
classify_border <- as.polygons(rast(r))
plot(classify_border)
# 创建气候类型列表
climate_types <-rat$climate

# Cfa,Cfb / Dfb,Dfc / Dwb,Dwc
Cfa <- c( 'Cfa')
Cfb <- c( 'Cfb')
Dfb <- c( 'Dfb')
Dfc <- c( 'Dfc')
Dwb <- c( 'Dwb')
Dwc <- c( 'Dwc')

# print(classify_border)   # 查看 classify_border 的整体结构
# summary(classify_border) # 确认 classify_border 是否有几何和属性数据

Climate_Class_list <- list(Cfa,Cfb,Dfb,Dfc,Dwb,Dwc)


# 读取气象指标数据
base_dir <- c(
  "./EA+NA_Results/merged_Clim_Duration_Extremes_years/phe1_3/",
  "./EA+NA_Results/merged_Clim_Duration_Extremes_years/phe3_4/",
  "./EA+NA_Results/merged_Clim_Duration_Extremes_years/phe4_6/"
)

prefixes <- c("TXx_phe1_3", "TNn_phe1_3", "DTR_phe1_3", "TXge30_phe1_3", "TNlt2_phe1_3",
              "RainDay_phe1_3", "CDD_phe1_3", "CWD_phe1_3", "Rx1day_phe1_3", "SDII_phe1_3",
              "TXx_phe3_4", "TNn_phe3_4", "DTR_phe3_4", "TXge30_phe3_4", "TNlt2_phe3_4",
              "RainDay_phe3_4", "CDD_phe3_4", "CWD_phe3_4", "Rx1day_phe3_4", "SDII_phe3_4",
              "TXx_phe4_6", "TNn_phe4_6", "DTR_phe4_6", "TXge30_phe4_6", "TNlt2_phe4_6",
              "RainDay_phe4_6", "CDD_phe4_6", "CWD_phe4_6", "Rx1day_phe4_6", "SDII_phe4_6")

# 读取所有匹配文件
file_paths <- unlist(lapply(prefixes, function(prefix) {
  list.files(base_dir, pattern = paste0("^", prefix, ".*\\.tif$"), 
             full.names = TRUE, ignore.case = TRUE) %>%
    sort() %>%
    head(10) })) # 取前 10 个文件

# 初始化存储所有气候区数据的列表
all_data <- list()


# 定义 Sen's Slope 和 Mann-Kendall 趋势分析函数
fun_sen <- function(x) {
  period = 10
  # 只保留有效数据
  if (length(na.omit(x)) < (0.6 * period )) return(c(NA, NA, NA))  # 检查有效值数量是否足够
  
  # 使用 trend 包中的 sens.slope 计算斜率估计
  MK_estimate <- trend::sens.slope(ts(na.omit(x), start = 2013, frequency = 1), conf.level = 0.95)
  
  # 提取结果：斜率、Z统计量、p值
  slope <- MK_estimate$estimates
  MK_test <- MK_estimate$p.value
  Zs <- MK_estimate$statistic

  # 返回斜率、Z统计量和p值
  return(c(Zs, slope, MK_test))}


# 初始化存储趋势栅格的列表
trend_rasters <- list()
# 初始化存储趋势均值的列表
trend_means <- data.frame()

# 设置存储路径
output_dir <- "./EA+NA_Results/merged_Cli_indices_trend/"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)  # 确保路径存在

# 开始遍历每个气象指标
for (prefix in prefixes) {
  cat("Processing:", prefix, "\n")
 
   #比如以第一个指标TXx_phe1_3为例
  # prefix = "TXx_phe1_3"
  
  
  # 读取当前指标的所有文件
  files <- file_paths[grepl(paste0("^", prefix), basename(file_paths))]
  if (length(files) == 0) next  # 跳过没有文件的指标
  
  # 读取并合并 10 年的数据
  extremes_var <- rast(files)  #rast()不会打乱10年图层的排序 2013-2022
  
  # 按气候区提取数据
  for (Climate_Class in Climate_Class_list) {
    cat("Processing Climate Class:", Climate_Class, "\n")
    
    # Climate_Class = "Cfa"
    
    order_val <- match(Climate_Class, classify_border$EA_koppen_30km_addClimate)
    if (is.na(order_val)) next  # 跳过不存在的气候类型
    

    selected_border_val <- classify_border[order_val, ]
    
    # 对极端指标数据进行掩膜处理
    extremes_masked <- mask(crop(extremes_var, selected_border_val), selected_border_val)
    # plot(extremes_masked)
    
    # 对每个像元计算趋势
    trend_raster <- app(extremes_masked, fun_sen, cores = 6)  
    names(trend_raster) <- c("Z_statistic", "Slope", "P_value")
    
    # 生成文件名并存储趋势栅格
    trend_name <- paste0("trend_raster_", Climate_Class, "_", prefix)
    trend_rasters[[trend_name]] <- trend_raster
    # file_path <- file.path(output_dir, paste0(trend_name, ".tif"))
    # writeRaster(trend_raster, filename = file_path, overwrite = TRUE)
    
    # 计算 Slope 值的区域平均
    slope_mean <- global(trend_raster[["Slope"]], mean, na.rm = TRUE)[1, 1]
    slope_median <- global(trend_raster[["Slope"]], median, na.rm = TRUE)[1, 1]
    
    slope_sd <- global(trend_raster[["Slope"]], fun = sd, na.rm = TRUE)[1, 1]
    # 存储结果到表格
    trend_means <- rbind(trend_means, data.frame(Climate_Class = Climate_Class, 
                                                 Indicator = prefix, 
                                                 Slope_Mean = slope_mean,
                                                 Slope_Median = slope_median,
                                                 slope_SD = slope_sd))
  }
}

trend_means$Slope_Mean <- round(as.numeric(trend_means$Slope_Mean), 2)
trend_means$Slope_Median <- round(as.numeric(trend_means$Slope_Median), 2)

trend_means$slope_SD <- round(as.numeric(trend_means$slope_SD), 2)
trend_means$slope_SD <- round(0.15*(trend_means$slope_SD), 2)

print(trend_means)

# 保存趋势均值表格
write.csv(trend_means, file = file.path(output_dir, "Climate_Trend_Means.csv"), row.names = FALSE)



