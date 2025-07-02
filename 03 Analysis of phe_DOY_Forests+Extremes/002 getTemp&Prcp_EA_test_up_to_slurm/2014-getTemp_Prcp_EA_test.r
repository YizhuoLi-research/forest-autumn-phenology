###### 0. 加载包 ####
library(terra)
library(raster)
library(purrr)

# setwd("D:/Graduation_Thesis")

###### 1. 导入数据 ####
tmax <- rast(list.files("./01 Download/11 Tmax_download/EA_Tmax/2014/", pattern = ".tif$", full.names = TRUE))  # 1-365张图
tmin <- rast(list.files("./01 Download/12 Tmin_download/EA_Tmin/2014/", pattern = ".tif$", full.names = TRUE))  # 1-365张图
prcp <- rast(list.files("./01 Download/13 Precipitation_download/EA_Precipitation/2014/", pattern = ".tif$", full.names = TRUE))  # 1-365张图
PHE <- rast(c(
  "./EA_Results/2.phe_DOY_PHE_analysis/0common_pixel/phe1_DOY_2014.tif",
  "./EA_Results/2.phe_DOY_PHE_analysis/0common_pixel/phe2_DOY_2014.tif",
  "./EA_Results/2.phe_DOY_PHE_analysis/0common_pixel/phe3_DOY_2014.tif",
  "./EA_Results/2.phe_DOY_PHE_analysis/0common_pixel/phe4_DOY_2014.tif",
  "./EA_Results/2.phe_DOY_PHE_analysis/0common_pixel/phe5_DOY_2014.tif",
  "./EA_Results/2.phe_DOY_PHE_analysis/0common_pixel/phe6_DOY_2014.tif"
))
##### 缩小数据量-节省时间的试运行 ####
# tmax <- aggregate(tmax, 10)
# PHE <- round(aggregate(PHE, 10))
##### 2. 数据处理 ####
##### 2.1 取共有像元 ####
sample1 <- PHE
sample1[is.finite(sample1)] <- 1

sample2 <- tmax
sample2[is.finite(sample2)] <- 1
sample3 <- tmin
sample3[is.finite(sample3)] <- 1
sample4 <- prcp
sample4[is.finite(sample4)] <- 1

PHE <- PHE * sample1[[1]] * sample2[[1]] * sample3[[1]] * sample4[[1]] 
tmax <- tmax * sample1[[1]] * sample2[[1]] * sample3[[1]] * sample4[[1]] 
tmin <- tmin * sample1[[1]] * sample2[[1]] * sample3[[1]] * sample4[[1]] 
prcp <- prcp * sample1[[1]] * sample2[[1]] * sample3[[1]] * sample4[[1]] 

##### 2.2 初始化结果栅格 ####
# ① 最高温度
TXx_phe1_3 <- tmax[[1]]
TXx_phe3_4 <- tmax[[1]]
TXx_phe4_6 <- tmax[[1]]

# ② 最低温度
TNn_phe1_3 <- tmin[[1]]
TNn_phe3_4 <- tmin[[1]]
TNn_phe4_6 <- tmin[[1]]

# ③ 平均日较差 (DTR)
DTR_phe1_3 <- tmax[[1]]
DTR_phe3_4 <- tmax[[1]]
DTR_phe4_6 <- tmax[[1]]

# ④ 最高温超过 30℃ 的天数
TXge30_phe1_3 <- tmax[[1]]
TXge30_phe3_4 <- tmax[[1]]
TXge30_phe4_6 <- tmax[[1]]

# ⑤ 最低温低于 2℃ 的天数
TNlt2_phe1_3 <- tmin[[1]]
TNlt2_phe3_4 <- tmin[[1]]
TNlt2_phe4_6 <- tmin[[1]]

# ① 降水天数
RainDay_phe1_3 <- prcp[[1]]
RainDay_phe3_4 <- prcp[[1]]
RainDay_phe4_6 <- prcp[[1]]

# ② 最大连续干旱天数
CDD_phe1_3 <- prcp[[1]]
CDD_phe3_4 <- prcp[[1]]
CDD_phe4_6 <- prcp[[1]]

# ③ 最大连续湿润天数
CWD_phe1_3 <- prcp[[1]]
CWD_phe3_4 <- prcp[[1]]
CWD_phe4_6 <- prcp[[1]]

# ④ 最大日降水量
Rx1day_phe1_3 <- prcp[[1]]
Rx1day_phe3_4 <- prcp[[1]]
Rx1day_phe4_6 <- prcp[[1]]

# ⑤ 简单降水强度指数
SDII_phe1_3 <- prcp[[1]]
SDII_phe3_4 <- prcp[[1]]
SDII_phe4_6 <- prcp[[1]]

# 生长阶段持续时间
Duration_phe1_3 <- tmax[[1]]
Duration_phe3_4 <- tmax[[1]]
Duration_phe4_6 <- tmax[[1]]

##### 2.3 逐像元计算 ####
threshold <- 1  # 降水阈值，1mm


for (n in 1:ncell(tmax)) {
  cat(paste0(n, "/", ncell(tmax), "\n"))
  
  # n=9296
  
  # 检查 tmax、tmin 和 PHE 是否有缺失值
  if (is.na(tmax[[1]][n]) || is.na(tmin[[1]][n]) || any(is.na(PHE[[1:6]][n]))) {
    next
  }
  
  # plot(tmax[[9296]])
  # tmax_df <- as.data.frame(tmax[[1]], xy = TRUE)
  
  # 提取当前像元的物候日期
  phe_1 <- as.numeric(PHE[[1]][n])
  phe_2 <- as.numeric(PHE[[2]][n])
  phe_3 <- as.numeric(PHE[[3]][n])
  phe_4 <- as.numeric(PHE[[4]][n])
  phe_5 <- as.numeric(PHE[[5]][n])
  phe_6 <- as.numeric(PHE[[6]][n])
  
## 1.温度相关指标
  # 提取当前像元的逐日最高温度和最低温度
  tmax_pixels <- as.vector(unlist(tmax[n]))
  tmin_pixels <- as.vector(unlist(tmin[n]))
  # 提取当前像元的逐日降水量，并将单位从米（m）转换为毫米（mm）
  prcp_pixels <- as.vector(unlist(prcp[n])) * 1000
  
  # ①计算每个物候阶段的最高温度 (TXx) K-->℃
  TXx_phe1_3[n] <- round(max(tmax_pixels[phe_1:(phe_3 - 1)], na.rm = TRUE) - 273.15, 2)
  TXx_phe3_4[n] <- round(max(tmax_pixels[phe_3:(phe_4 - 1)], na.rm = TRUE) - 273.15, 2)
  TXx_phe4_6[n] <- round(max(tmax_pixels[phe_4:phe_6], na.rm = TRUE) - 273.15, 2)
  
  # ②计算每个物候阶段的最低温度 (TNn) K-->℃
  TNn_phe1_3[n] <- round(min(tmin_pixels[phe_1:(phe_3 - 1)], na.rm = TRUE) - 273.15, 2)
  TNn_phe3_4[n] <- round(min(tmin_pixels[phe_3:(phe_4 - 1)], na.rm = TRUE) - 273.15, 2)
  TNn_phe4_6[n] <- round(min(tmin_pixels[phe_4:phe_6], na.rm = TRUE) - 273.15, 2)
  
  # ③计算每个物候阶段的平均日较差 (DTR)℃
  DTR_phe1_3[n] <- mean((tmax_pixels[phe_1:(phe_3 - 1)] - tmin_pixels[phe_1:(phe_3 - 1)]), na.rm = TRUE)
  DTR_phe3_4[n] <- mean((tmax_pixels[phe_3:(phe_4 - 1)] - tmin_pixels[phe_3:(phe_4 - 1)]), na.rm = TRUE)
  DTR_phe4_6[n] <- mean((tmax_pixels[phe_4:phe_6] - tmin_pixels[phe_4:phe_6]), na.rm = TRUE)
  
  # ④计算最高温超过 30℃ 的天数
  TXge30_phe1_3[n] <- sum(tmax_pixels[phe_1:(phe_3 - 1)] - 273.15 >= 30, na.rm = TRUE)
  TXge30_phe3_4[n] <- sum(tmax_pixels[phe_3:(phe_4 - 1)] - 273.15 >= 30, na.rm = TRUE)
  TXge30_phe4_6[n] <- sum(tmax_pixels[phe_4:phe_6] - 273.15 >= 30, na.rm = TRUE)
  
  # ⑤计算最低温低于 2℃ 的天数
  TNlt2_phe1_3[n] <- sum(tmin_pixels[phe_1:(phe_3 - 1)] - 273.15 < 2, na.rm = TRUE)
  TNlt2_phe3_4[n] <- sum(tmin_pixels[phe_3:(phe_4 - 1)] - 273.15 < 2, na.rm = TRUE)
  TNlt2_phe4_6[n] <- sum(tmin_pixels[phe_4:phe_6] - 273.15 < 2, na.rm = TRUE)
  
## 2.降水相关指标
  # 定义函数计算最大连续天数
  max_consecutive_days <- function(data, condition) {
    max_count <- 0
    current_count <- 0
    for (value in data) {
      if (condition(value)) {
        current_count <- current_count + 1
        if (current_count > max_count) {
          max_count <- current_count
        }
      } else {
        current_count <- 0
      }
    }
    return(max_count)
  }
  
  # ①计算 RainDay（降水日数，prcp ≥ 1mm）
  RainDay_phe1_3[n] <- sum(prcp_pixels[phe_1:(phe_3 - 1)] >= threshold)
  RainDay_phe3_4[n] <- sum(prcp_pixels[phe_3:(phe_4 - 1)] >= threshold)
  RainDay_phe4_6[n] <- sum(prcp_pixels[phe_4:phe_6] >= threshold)
  
  # ②计算 CWD（最大连续湿润天数，prcp ≥ 1mm）
  CWD_phe1_3[n] <- max_consecutive_days(prcp_pixels[phe_1:(phe_3 - 1)], function(x) x >= threshold)
  CWD_phe3_4[n] <- max_consecutive_days(prcp_pixels[phe_3:(phe_4 - 1)], function(x) x >= threshold)
  CWD_phe4_6[n] <- max_consecutive_days(prcp_pixels[phe_4:phe_6], function(x) x >= threshold)
  
  # ③计算 CDD 最大连续干旱天数，prcp < 1mm）
  CDD_phe1_3[n] <- max_consecutive_days(prcp_pixels[phe_1:(phe_3 - 1)], function(x) x < threshold)
  CDD_phe3_4[n] <- max_consecutive_days(prcp_pixels[phe_3:(phe_4 - 1)], function(x) x < threshold)
  CDD_phe4_6[n] <- max_consecutive_days(prcp_pixels[phe_4:phe_6], function(x) x < threshold)
  
  # ④计算Rx1day 最大日降水量
  Rx1day_phe1_3[n] <- max(prcp_pixels[phe_1:(phe_3 - 1)], na.rm = TRUE)
  Rx1day_phe3_4[n] <- max(prcp_pixels[phe_3:(phe_4 - 1)], na.rm = TRUE) 
  Rx1day_phe4_6[n] <- max(prcp_pixels[phe_4:phe_6], na.rm = TRUE)
  
  # ⑤计算 SDII（湿润日平均降水量）
  # wet_days_phe1_3 <- sum(prcp_pixels[phe_1:(phe_3 - 1)] >= threshold)
  # wet_days_phe3_4 <- sum(prcp_pixels[phe_3:(phe_4 - 1)] >= threshold)
  # wet_days_phe4_6 <- sum(prcp_pixels[phe_4:phe_6] >= threshold)
  SDII_phe1_3[n] <- ifelse(RainDay_phe1_3[n] > 0, 
                              sum(prcp_pixels[phe_1:(phe_3 - 1)][prcp_pixels[phe_1:(phe_3 - 1)] >= threshold]) /  RainDay_phe1_3[n], 
                              NA)
  SDII_phe3_4[n] <- ifelse(RainDay_phe3_4[n] > 0, 
                              sum(prcp_pixels[phe_3:(phe_4 - 1)][prcp_pixels[phe_3:(phe_4 - 1)] >= threshold]) /  RainDay_phe3_4[n], 
                              NA)
  SDII_phe4_6[n] <- ifelse(RainDay_phe4_6[n] > 0, 
                              sum(prcp_pixels[phe_4:phe_6][prcp_pixels[phe_4:phe_6] >= threshold]) /  RainDay_phe4_6[n], 
                              NA)

## 3.生长阶段指标 Duration_phe1_3 Duration_phe3_4 Duration_phe4_6
  Duration_phe1_3[n] <- (phe_3 - 1) - phe_1 + 1
  Duration_phe3_4[n] <- (phe_4 - 1) - phe_3 + 1
  Duration_phe4_6[n] <- (phe_6 - 1) - phe_4 + 1
  
}
  
  
  

##### 2.4 可视化结果 ####
# 平均日较差 (DTR)
#  plot(dtr_phe1_3, main = "Mean DTR in PHE1-3")
#  plot(dtr_phe3_4, main = "Mean DTR in PHE3-4")
#  plot(dtr_phe4_6, main = "Mean DTR in PHE4-6")


##### 2.5 合并并保存结果 ####
# 合并温度相关指标
Temp_phe1_3 <- c(TXx_phe1_3, TNn_phe1_3, DTR_phe1_3, TXge30_phe1_3, TNlt2_phe1_3)
names(Temp_phe1_3) <- c("TXx_phe1_3", "TNn_phe1_3", "DTR_phe1_3", "TXge30_phe1_3", "TNlt2_phe1_3")

Temp_phe3_4 <- c(TXx_phe3_4, TNn_phe3_4, DTR_phe3_4, TXge30_phe3_4, TNlt2_phe3_4)
names(Temp_phe3_4) <- c("TXx_phe3_4", "TNn_phe3_4", "DTR_phe3_4", "TXge30_phe3_4", "TNlt2_phe3_4")

Temp_phe4_6 <- c(TXx_phe4_6, TNn_phe4_6, DTR_phe4_6, TXge30_phe4_6, TNlt2_phe4_6)
names(Temp_phe4_6) <- c("TXx_phe4_6", "TNn_phe4_6", "DTR_phe4_6", "TXge30_phe4_6", "TNlt2_phe4_6")

# 合并降水相关指标
Prcp_phe1_3 <- c(RainDay_phe1_3, CWD_phe1_3, CDD_phe1_3, Rx1day_phe1_3, SDII_phe1_3)
names(Prcp_phe1_3) <- c("RainDay_phe1_3", "CWD_phe1_3", "CDD_phe1_3", "Rx1day_phe1_3", "SDII_phe1_3")

Prcp_phe3_4 <- c(RainDay_phe3_4, CWD_phe3_4, CDD_phe3_4, Rx1day_phe3_4, SDII_phe3_4)
names(Prcp_phe3_4) <- c("RainDay_phe3_4", "CWD_phe3_4", "CDD_phe3_4", "Rx1day_phe3_4", "SDII_phe3_4")

Prcp_phe4_6 <- c(RainDay_phe4_6, CWD_phe4_6, CDD_phe4_6, Rx1day_phe4_6, SDII_phe4_6)
names(Prcp_phe4_6) <- c("RainDay_phe4_6", "CWD_phe4_6", "CDD_phe4_6", "Rx1day_phe4_6", "SDII_phe4_6")


# 定义输出文件夹路径
output_dir <- "./EA_Results/3.Clim_Duration_Extremes/"

# 检查文件夹是否存在，如果不存在则创建
if (!dir.exists(output_dir)) {  dir.create(output_dir, recursive = TRUE)}

# 保存结果
# 1.温度指标 5层
writeRaster(Temp_phe1_3, "./EA_Results/3.Clim_Duration_Extremes/Temp_phe1_3_2014.tif", overwrite = TRUE)
writeRaster(Temp_phe3_4, "./EA_Results/3.Clim_Duration_Extremes/Temp_phe3_4_2014.tif", overwrite = TRUE)
writeRaster(Temp_phe4_6, "./EA_Results/3.Clim_Duration_Extremes/Temp_phe4_6_2014.tif", overwrite = TRUE)

# 2.降水指标 5层
writeRaster(Prcp_phe1_3, "./EA_Results/3.Clim_Duration_Extremes/Prcp_phe1_3_2014.tif", overwrite = TRUE)
writeRaster(Prcp_phe3_4, "./EA_Results/3.Clim_Duration_Extremes/Prcp_phe3_4_2014.tif", overwrite = TRUE)
writeRaster(Prcp_phe4_6, "./EA_Results/3.Clim_Duration_Extremes/Prcp_phe4_6_2014.tif", overwrite = TRUE)

# 3.物候指标 1层
writeRaster(Duration_phe1_3, "./EA_Results/3.Clim_Duration_Extremes/Duration_phe1_3_2014.tif", overwrite = TRUE)
writeRaster(Duration_phe3_4, "./EA_Results/3.Clim_Duration_Extremes/Duration_phe3_4_2014.tif", overwrite = TRUE)
writeRaster(Duration_phe4_6, "./EA_Results/3.Clim_Duration_Extremes/Duration_phe4_6_2014.tif", overwrite = TRUE)
