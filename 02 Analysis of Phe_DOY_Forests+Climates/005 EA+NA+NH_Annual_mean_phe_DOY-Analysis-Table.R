###### 0. 加载包 ####
library(terra)
library(tidyverse)
library(raster)

setwd("D:/Graduation_Thesis")

################################   01  计算DOY-NorthAmerica  ##################################
###############################################################################################         
# NA
# # SOS: phe1_DOY_.   ;  MGP: phe2_DOY_.    ；  GMO：phe3_DOY_.
# # GDO: phe4_DOY_.   ;  MSP: phe5_DOY_.    ；  EOS：phe6_DOY_.

# #  The code modifies the pattern in list.files() to match all six phenological
# #  events (phe1_DOY_ to phe6_DOY_) in a single run for processing.


file_list <- list.files("./NA_Results/2.phe_DOY_PHE_analysis/0common_pixel/", 
                        pattern = "phe1_DOY_.*\\.tif$", full.names = TRUE)

# 读取所有影像
rasters <- stack(file_list)
mean_raster <- calc(rasters, fun = mean, na.rm = TRUE)


# 计算均值、最大、最小、0.15倍标准差
mean_value <- ceiling(cellStats(mean_raster, stat = 'mean', na.rm = TRUE))
max_value <- round(cellStats(mean_raster, stat = 'max', na.rm = TRUE),2)
min_value <- round(cellStats(mean_raster, stat = 'min', na.rm = TRUE),2)
sd_value <- round(cellStats(mean_raster, stat = 'sd', na.rm = TRUE),2)
spatial_sd_variation <- round(0.15 * sd_value,2)

# 打印结果
# cat("Mean:", mean_value, "\n",
#     "Max:", max_value, "\n",
#     "Min:", min_value, "\n",
#     "0.15 * SD (Spatial Variation):", spatial_sd_variation, "\n")

##########计算逐渐趋势
# 定义用于计算趋势的函数


calc_trend <- function(pixel_values, ...) {
  if (all(is.na(pixel_values))) {
    return(NA)  # 如果所有像元值都是NA，返回NA
  }
  
  # 去除 NA 值
  valid_values <- !is.na(pixel_values)
  if (sum(valid_values) < 3) {
    return(NA)  # 如果有效数据点少于2个，返回NA
  }
  
  # 定义年份
  years <- 2013:2022  # 与数据文件对应的年份
  # year==2013
  # 执行线性回归
  model <- lm(pixel_values ~ years)
  
  # 返回回归斜率（trend）
  return(coef(model)[2])
}

# 使用 calc 函数对栅格逐像元计算趋势
trend_raster <- calc(rasters, fun = calc_trend, na.rm = TRUE)

summary(trend_raster[])

# # 查看趋势影像
# plot(trend_raster, main = "DOY Trend (2013-2022)")
# summary(trend_raster)
# 计算平均趋势值±0.15sd
mean_trend <- mean(trend_raster[], na.rm = TRUE)
sd_trend <- sd(trend_raster[], na.rm = TRUE)
spatial_sd_trend <- round(0.15 * sd_trend,2)

##总结结果
results_df <- data.frame(
  Metric = c("Mean_DOY", "Max_DOY", "Min_DOY", 
             "Mean_Trend", "Max_Trend", "Min_Trend"),
  Value = c(paste(mean_value, "±", spatial_sd_variation),  # 取整
            round(max_value, 2),                           # 保留2位小数
            round(min_value, 2),                           # 保留2位小数
            paste(round(mean_trend, 2), "±", round(spatial_sd_trend, 2)), # 保留2位小数
            round(max(trend_raster[], na.rm = TRUE), 2),   # 保留2位小数
            round(min(trend_raster[], na.rm = TRUE), 2) )  # 保留2位小数
)

# 打印结果表格
print(results_df)

# #SOS  phe1_DOY_.
# 1   Mean_DOY   118 ± 3.11
# 2    Max_DOY        171.5
# 3    Min_DOY           15
# 4 Mean_Trend -0.09 ± 0.19
# 5  Max_Trend         21.6
# 6  Min_Trend          -17

##MGP  phe2_DOY_.
# 1   Mean_DOY  140 ± 2.76
# 2    Max_DOY       183.3
# 3    Min_DOY          50
# 4 Mean_Trend 0.06 ± 0.14
# 5  Max_Trend        20.5
# 6  Min_Trend         -16

##GMO  phe3_DOY_.
# 1   Mean_DOY  162 ± 2.49
# 2    Max_DOY      204.33
# 3    Min_DOY          89
# 4 Mean_Trend 0.19 ± 0.14
# 5  Max_Trend          15
# 6  Min_Trend       -17.5

##GDO  phe4_DOY_.
# 1   Mean_DOY  227 ± 1.16
# 2    Max_DOY       260.5
# 3    Min_DOY         126
# 4 Mean_Trend 0.18 ± 0.15
# 5  Max_Trend          18
# 6  Min_Trend         -12

##MSP  phe5_DOY_.
# 1   Mean_DOY  263 ± 1.71
# 2    Max_DOY      295.22
# 3    Min_DOY         175
# 4 Mean_Trend 0.04 ± 0.12
# 5  Max_Trend          14
# 6  Min_Trend      -15.21

##EOS  phe6_DOY_.
# 1   Mean_DOY   298 ± 3.16
# 2    Max_DOY        346.6
# 3    Min_DOY        211.5
# 4 Mean_Trend -0.09 ± 0.15
# 5  Max_Trend         18.5
# 6  Min_Trend       -19.86
#####################################   02  计算DOY-Euraisa  #################################
###############################################################################################      
####Eurasia
file_list <- list.files("./EA_Results/2.phe_DOY_PHE_analysis/0common_pixel/",
                        pattern = "phe5_DOY_.*\\.tif$", full.names = TRUE)


# 读取所有影像
rasters <- stack(file_list)
mean_raster <- calc(rasters, fun = mean, na.rm = TRUE)


# 计算均值、最大、最小、0.15倍标准差
mean_value <- ceiling(cellStats(mean_raster, stat = 'mean', na.rm = TRUE))
max_value <- round(cellStats(mean_raster, stat = 'max', na.rm = TRUE),2)
min_value <- round(cellStats(mean_raster, stat = 'min', na.rm = TRUE),2)
sd_value <- round(cellStats(mean_raster, stat = 'sd', na.rm = TRUE),2)
spatial_sd_variation <- round(0.15 * sd_value,2)

##########计算逐渐趋势
# 定义用于计算趋势的函数

calc_trend <- function(pixel_values, ...) {
  if (all(is.na(pixel_values))) {
    return(NA)  # 如果所有像元值都是NA，返回NA
  }

  # 去除 NA 值
  valid_values <- !is.na(pixel_values)
  if (sum(valid_values) < 3) {
    return(NA)  # 如果有效数据点少于2个，返回NA
  }

  # 定义年份
  years <- 2013:2022  # 与数据文件对应的年份
  # year==2013
  # 执行线性回归
  model <- lm(pixel_values ~ years)

  # 返回回归斜率（trend）
  return(coef(model)[2])
}

# 使用 calc 函数对栅格逐像元计算趋势
trend_raster <- calc(rasters, fun = calc_trend, na.rm = TRUE)

summary(trend_raster[])

# # 查看趋势影像
# plot(trend_raster, main = "DOY Trend (2013-2022)")
# summary(trend_raster)
# 计算平均趋势值±0.15sd
mean_trend <- mean(trend_raster[], na.rm = TRUE)
sd_trend <- sd(trend_raster[], na.rm = TRUE)
spatial_sd_trend <- round(0.15 * sd_trend,2)

##总结结果
results_df <- data.frame(
  Metric = c("Mean_DOY", "Max_DOY", "Min_DOY",
             "Mean_Trend", "Max_Trend", "Min_Trend"),
  Value = c(paste(mean_value, "±", spatial_sd_variation),  # 取整
            round(max_value, 2),                           # 保留一位小数
            round(min_value, 2),                           # 保留一位小数
            paste(round(mean_trend, 2), "±", round(spatial_sd_trend, 2)), # 保留一位小数
            round(max(trend_raster[], na.rm = TRUE), 2),   # 保留一位小数
            round(min(trend_raster[], na.rm = TRUE), 2) )  # 保留一位小数
)

# 打印结果表格
print(results_df)

##SOS  phe1_DOY_.
# 1   Mean_DOY   123 ± 2.92
# 2    Max_DOY          181
# 3    Min_DOY         31.5
# 4 Mean_Trend -0.12 ± 0.15
# 5  Max_Trend           18
# 6  Min_Trend        -29.5

##MGP  phe2_DOY_.
# 1   Mean_DOY   143 ± 2.37
# 2    Max_DOY          193
# 3    Min_DOY         65.5
# 4 Mean_Trend -0.08 ± 0.12
# 5  Max_Trend           12
# 6  Min_Trend          -16

##GMO  phe3_DOY_.
# 1   Mean_DOY   164 ± 2.02
# 2    Max_DOY          217
# 3    Min_DOY           99
# 4 Mean_Trend -0.04 ± 0.14
# 5  Max_Trend           19
# 6  Min_Trend          -13

##GDO   phe4_DOY_.
# 1   Mean_DOY   225 ± 0.98
# 2    Max_DOY          282
# 3    Min_DOY          122
# 4 Mean_Trend -0.01 ± 0.14
# 5  Max_Trend         22.5
# 6  Min_Trend        -20.5

##MSP  phe5_DOY_.
# 1   Mean_DOY 256 ± 1.89
# 2    Max_DOY        307
# 3    Min_DOY        150
# 4 Mean_Trend 0.1 ± 0.11
# 5  Max_Trend         14
# 6  Min_Trend        -22

##EOS  phe6_DOY_.
# 1   Mean_DOY  287 ± 3.38
# 2    Max_DOY         356
# 3    Min_DOY         178
# 4 Mean_Trend 0.21 ± 0.13
# 5  Max_Trend          10
# 6  Min_Trend       -23.5
############################   03  计算DOY-Northern Hemisphere  ###############################
###############################################################################################      
file_list <- list.files("./EA+NA_Results/merged_Phe_DOY_years_2_PHE_analysis/merged_phe6_DOY/", 
                        pattern = "phe6_DOY_.*\\.tif$", full.names = TRUE)


#phe1_DOY_. phe2_DOY_. phe3_DOY_. phe4_DOY_. phe5_DOY_. phe6_DOY_.

# 读取所有影像
rasters <- stack(file_list)
mean_raster <- calc(rasters, fun = mean, na.rm = TRUE)


# 计算均值、最大、最小、0.15倍标准差
mean_value <- ceiling(cellStats(mean_raster, stat = 'mean', na.rm = TRUE))
max_value <- cellStats(mean_raster, stat = 'max', na.rm = TRUE)
min_value <- cellStats(mean_raster, stat = 'min', na.rm = TRUE)
sd_value <- cellStats(mean_raster, stat = 'sd', na.rm = TRUE)
spatial_sd_variation <- round(0.15 * sd_value,2)

# 打印结果
# cat("Mean:", mean_value, "\n",
#     "Max:", max_value, "\n",
#     "Min:", min_value, "\n",
#     "0.15 * SD (Spatial Variation):", spatial_sd_variation, "\n")

##########计算逐渐趋势
# 定义用于计算趋势的函数

calc_trend <- function(pixel_values, ...) {
  if (all(is.na(pixel_values))) {
    return(NA)  # 如果所有像元值都是NA，返回NA
  }
  
  # 去除 NA 值
  valid_values <- !is.na(pixel_values)
  if (sum(valid_values) < 3) {
    return(NA)  # 如果有效数据点少于2个，返回NA
  }
  
  # 定义年份
  years <- 2013:2022  # 与数据文件对应的年份
  # year==2013
  # 执行线性回归
  model <- lm(pixel_values ~ years)
  
  # 返回回归斜率（trend）
  return(coef(model)[2])
}

# 使用 calc 函数对栅格逐像元计算趋势
trend_raster <- calc(rasters, fun = calc_trend, na.rm = TRUE)

summary(trend_raster[])

# # 查看趋势影像
# plot(trend_raster, main = "DOY Trend (2013-2022)")
# summary(trend_raster)
# 计算平均趋势值±0.15sd
mean_trend <- mean(trend_raster[], na.rm = TRUE)
sd_trend <- sd(trend_raster[], na.rm = TRUE)
spatial_sd_trend <- round(0.15 * sd_trend,2)

##总结结果
results_df <- data.frame(
  Metric = c("Mean_DOY", "Max_DOY", "Min_DOY", 
             "Mean_Trend", "Max_Trend", "Min_Trend"),
  Value = c(paste(mean_value, "±", spatial_sd_variation),
            max_value,
            min_value,
            paste(round(mean_trend, 2), "±", round(spatial_sd_trend, 2)),
            round(max(trend_raster[], na.rm = TRUE), 2),  
            round(min(trend_raster[], na.rm = TRUE), 2) )
)

# 打印结果表格
print(results_df)


##SOS  phe1_DOY_.
# 1   Mean_DOY   122 ± 2.98
# 2    Max_DOY          181
# 3    Min_DOY           15
# 4 Mean_Trend -0.11 ± 0.16
# 5  Max_Trend         21.6
# 6  Min_Trend        -29.5

# ##MGP  phe2_DOY_.
# 1   Mean_DOY   142 ± 2.48
# 2    Max_DOY          193
# 3    Min_DOY           50
# 4 Mean_Trend -0.05 ± 0.13
# 5  Max_Trend         20.5
# 6  Min_Trend          -16

##GMO  phe3_DOY_.
# 1   Mean_DOY  163 ± 2.15
# 2    Max_DOY         217
# 3    Min_DOY          89
# 4 Mean_Trend 0.01 ± 0.14
# 5  Max_Trend          19
# 6  Min_Trend       -17.5

##GDO  phe4_DOY_.
# 1   Mean_DOY  225 ± 1.04
# 2    Max_DOY         282
# 3    Min_DOY         122
# 4 Mean_Trend 0.04 ± 0.14
# 5  Max_Trend        22.5
# 6  Min_Trend       -20.5

##MSP  phe5_DOY_.
# 1   Mean_DOY   257 ± 1.9
# 2    Max_DOY         307
# 3    Min_DOY         150
# 4 Mean_Trend 0.09 ± 0.11
# 5  Max_Trend          14
# 6  Min_Trend         -22

##EOS  phe6_DOY_.
# 1   Mean_DOY  289 ± 3.41
# 2    Max_DOY         356
# 3    Min_DOY         178
# 4 Mean_Trend 0.14 ± 0.14
# 5  Max_Trend        18.5
# 6  Min_Trend       -23.5