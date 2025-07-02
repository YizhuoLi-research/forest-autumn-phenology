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

############  在气候区上先做平均，再做趋势分析  ############

# 初始化存储趋势结果的表格
trend_means <- data.frame()

# 遍历每个指标
for (prefix in prefixes) {
  cat("Processing:", prefix, "\n")
  
  # 获取对应文件路径（10年）
  files <- file_paths[grepl(paste0("^", prefix), basename(file_paths))]
  if (length(files) == 0) next
  
  # 读取时间序列栅格（2013–2022）
  extremes_var <- rast(files)
  
  # 遍历每个气候区
  for (Climate_Class in Climate_Class_list) {
    cat("Processing Climate Class:", Climate_Class, "\n")
    
    order_val <- match(Climate_Class, classify_border$EA_koppen_30km_addClimate)
    if (is.na(order_val)) next
    
    selected_border_val <- classify_border[order_val, ]
    extremes_masked <- mask(crop(extremes_var, selected_border_val), selected_border_val)
    
    # ✅ Step 1: 计算每年区域均值（结果为长度为10的时间序列）
    regional_ts <- global(extremes_masked, fun = mean, na.rm = TRUE)[, 1]
    
    # ✅ Step 2: 判断有效年份数是否足够
    if (sum(!is.na(regional_ts)) >= 6) {
      sen_result <- trend::sens.slope(ts(regional_ts, start = 2013, frequency = 1), conf.level = 0.95)
      sen_slope <- sen_result$estimates
      sen_p <- sen_result$p.value
      sen_z <- sen_result$statistic
    } else {
      sen_slope <- NA
      sen_p <- NA
      sen_z <- NA
    }
    
    # ✅ Step 3: 存储结果
    trend_means <- rbind(trend_means, data.frame(
      Climate_Class = Climate_Class,
      Indicator = prefix,
      TS_Slope = round(sen_slope, 3),
      TS_P = round(sen_p, 3),
      TS_Z = round(sen_z, 2)
    ))
  }
}

# 输出查看结果
head(trend_means)
trend_means <- trend_means %>%
  mutate(Significance = case_when(
    TS_P < 0.001 ~ "***",
    TS_P < 0.01 ~ "**",
    TS_P < 0.05 ~ "*",
    TRUE ~ ""
  ))


# 写入结果
# 设置存储路径
write.csv(trend_means, "./EA+NA_Results/merged_Cli_indices_trend/Climate-Means_then_Trend_withP.csv", row.names = FALSE)

