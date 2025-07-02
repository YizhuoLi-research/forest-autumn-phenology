rm(list = ls())

############################# 0. 加载包 ################################################################

library(terra)
library(raster)
setwd("D:/Graduation_Thesis")

############################# 1. 导入数据 ##############################################################
############################# 10年/ NA-EA ##############################################################


tmax <- rast(list.files("./01 Download/11 Tmax_download/EA_Tmax/2022/", pattern = ".tif$", full.names = TRUE))  # 1-365张图
tmin <- rast(list.files("./01 Download/12 Tmin_download/EA_Tmin/2022/", pattern = ".tif$", full.names = TRUE))  # 1-365张图
prcp <- rast(list.files("./01 Download/13 Precipitation_download/EA_precipitation/2022/", pattern = ".tif$", full.names = TRUE))  # 1-365张图
PHE <- rast(c(
  "./EA_Results/2.phe_DOY_PHE_analysis/0common_pixel/phe1_DOY_2022.tif",
  "./EA_Results/2.phe_DOY_PHE_analysis/0common_pixel/phe2_DOY_2022.tif",
  "./EA_Results/2.phe_DOY_PHE_analysis/0common_pixel/phe3_DOY_2022.tif",
  "./EA_Results/2.phe_DOY_PHE_analysis/0common_pixel/phe4_DOY_2022.tif",
  "./EA_Results/2.phe_DOY_PHE_analysis/0common_pixel/phe5_DOY_2022.tif",
  "./EA_Results/2.phe_DOY_PHE_analysis/0common_pixel/phe6_DOY_2022.tif"
))

##### 缩小数据量-节省时间的试运行 ####
# tmax <- aggregate(tmax, 10)
# PHE <- round(aggregate(PHE, 10))

############################# 2. 数据处理 ##############################################################
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

# PHE <- PHE[tmax[[1]],drop=F]
# PHE <- PHE[tmin[[1]],drop=F]
# PHE <- PHE[prcp[[1]],drop=F]
# sum(!is.na(values(PHE[[1]])))
# sum(!is.na(values(tmin[[1]])))
# sum(!is.na(values(tmax[[1]])))
# sum(!is.na(values(prcp[[1]])))

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

##### 2.3 像元计算[app] ####

new_data = c(tmax,tmin,prcp,PHE)
# new_data <- c(tmax = tmax, tmin = tmin, prcp = prcp, PHE = PHE)
# tmax_lyr <- new_data[["tmax"]]

# library(terra)   #######有值像元单独测试
# # 获取 new_data 的行数和列数
# n_rows <- nrow(new_data)
# n_cols <- ncol(new_data)
# # 计算行和列索引
# row_index <- ((9296 - 1) %/% n_cols) + 1
# col_index <- ((9296 - 1) %% n_cols) + 1
# # 提取第 9296 个像元的所有层数据
# single_pixel_data <- new_data[row_index, col_index, drop = FALSE]
# 
# # 构建极端指标计算公式，设想单个像元后面形成一个向量，代表每层的数据
# 
multi <- function(pixels) {
  #   # pixels <- single_pixel_data
  
  count <-  length(pixels)
  # 提取物候事件的 DOY
  phe_1 <- pixels[[count - 5]] # 第 1 个物候事件的 DOY
  phe_2 <- pixels[[count - 4]] # 第 2 个物候事件的 DOY
  phe_3 <- pixels[[count - 3]] # 第 3 个物候事件的 DOY
  phe_4 <- pixels[[count - 2]] # 第 4 个物候事件的 DOY
  phe_5 <- pixels[[count - 1]] # 第 5 个物候事件的 DOY
  phe_6 <- pixels[[count]]      # 第 6 个物候事件的 DOY
  
  # if (any(is.na(c(phe_1, phe_2, phe_3, phe_4, phe_5, phe_6))) ||
  #     any(c(phe_1, phe_2, phe_3, phe_4, phe_5, phe_6) < 1) ||
  #     any(c(phe_1, phe_2, phe_3, phe_4, phe_5, phe_6) > 365)) {
  #   return(rep(NA, 33))  # 返回一个致的 NA 值  #因为要输出33层的结果
  # }
  
  # 使用 tryCatch 捕获错误
  result <- tryCatch({
    
    # 判断闰年的函数
    year <- 2022
    is_leap_year <- function(year) {
      (year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0)
    }
    
    days_in_year <- ifelse(is_leap_year(year), 366, 365)  # 2022 年是平年，days_in_year = 365
    # 提取逐日数据
    tmax_data <- pixels[1:days_in_year]                            # 提取 tmax 数据  Unit:K
    tmin_data <- pixels[(days_in_year + 1):(2 * days_in_year)]      # 提取 tmin 数据  Unit:K 
    prcp_data <- pixels[(2 * days_in_year + 1):(3 * days_in_year)]*1000  # 提取 prcp 数据  Unit:m  直接换算成mm
    
    # phe_1 <- as.numeric(values(phe_1))  # 提取DOY数值
    # phe_3 <- as.numeric(values(phe_3))
    # phe_4 <- as.numeric(values(phe_4))  # 提取DOY数值
    # phe_6 <- as.numeric(values(phe_6))
    ## 1.温度指标
    # ①计算每个物候阶段的最高温度 (TXx) K-->℃
    TXx_phe1_3 <- round(max(tmax_data[phe_1:(phe_3 - 1)], na.rm = TRUE) - 273.15, 2)
    TXx_phe3_4 <- round(max(tmax_data[phe_3:(phe_4 - 1)], na.rm = TRUE) - 273.15, 2)
    TXx_phe4_6 <- round(max(tmax_data[phe_4:phe_6], na.rm = TRUE) - 273.15, 2)
    
    # ②计算每个物候阶段的最低温度 (TNn) K-->℃
    TNn_phe1_3 <- round(min(tmin_data[phe_1:(phe_3 - 1)], na.rm = TRUE) - 273.15, 2)
    TNn_phe3_4 <- round(min(tmin_data[phe_3:(phe_4 - 1)], na.rm = TRUE) - 273.15, 2)
    TNn_phe4_6 <- round(min(tmin_data[phe_4:phe_6], na.rm = TRUE) - 273.15, 2)
    
    # ③计算每个物候阶段的平均日较差 (DTR)℃
    DTR_phe1_3 <- mean((tmax_data[phe_1:(phe_3 - 1)] - tmin_data[phe_1:(phe_3 - 1)]), na.rm = TRUE)
    DTR_phe3_4 <- mean((tmax_data[phe_3:(phe_4 - 1)] - tmin_data[phe_3:(phe_4 - 1)]), na.rm = TRUE)
    DTR_phe4_6 <- mean((tmax_data[phe_4:phe_6] - tmin_data[phe_4:phe_6]), na.rm = TRUE)
    
    # ④计算最高温超过 30℃ 的天数
    TXge30_phe1_3 <- sum(tmax_data[phe_1:(phe_3 - 1)] - 273.15 >= 30, na.rm = TRUE)
    TXge30_phe3_4 <- sum(tmax_data[phe_3:(phe_4 - 1)] - 273.15 >= 30, na.rm = TRUE)
    TXge30_phe4_6 <- sum(tmax_data[phe_4:phe_6] - 273.15 >= 30, na.rm = TRUE)
    
    # ⑤计算最低温低于 2℃ 的天数
    TNlt2_phe1_3 <- sum(tmin_data[phe_1:(phe_3 - 1)] - 273.15 < 2, na.rm = TRUE)
    TNlt2_phe3_4 <- sum(tmin_data[phe_3:(phe_4 - 1)] - 273.15 < 2, na.rm = TRUE)
    TNlt2_phe4_6 <- sum(tmin_data[phe_4:phe_6] - 273.15 < 2, na.rm = TRUE)
    
    ## 2.降水指标
    # 降水阈值，1mm
    threshold <- 1 
    # 定义函数计算最大连续天数
    max_consecutive_days <- function(data, condition) {
      rle_data <- rle(condition(data))
      valid_lengths <- rle_data$lengths[rle_data$values]
      return(ifelse(length(valid_lengths) > 0, max(valid_lengths), 0))
    }
    # max_consecutive_days <- function(data, condition) {
    #   max_count <- 0
    #   current_count <- 0
    #   for (value in data) {
    #     if (condition(value)) {
    #       current_count <- current_count + 1
    #       if (current_count > max_count) {
    #         max_count <- current_count
    #       }
    #     } else {
    #       current_count <- 0
    #     }
    #   }
    #   return(max_count)
    # }
    
    # 6-①计算 RainDay（降水日数，prcp ≥ 1mm）
    RainDay_phe1_3 <- sum(prcp_data[phe_1:(phe_3 - 1)] >= threshold)
    RainDay_phe3_4 <- sum(prcp_data[phe_3:(phe_4 - 1)] >= threshold)
    RainDay_phe4_6 <- sum(prcp_data[phe_4:phe_6] >= threshold)
    
    # 7-②计算 CWD（最大连续湿润天数，prcp ≥ 1mm）
    CWD_phe1_3 <- max_consecutive_days(as.vector(prcp_data[phe_1:(phe_3 - 1)]) , function(x) x >= threshold)
    CWD_phe3_4 <- max_consecutive_days(as.vector(prcp_data[phe_3:(phe_4 - 1)]) , function(x) x >= threshold)
    CWD_phe4_6 <- max_consecutive_days(as.vector(prcp_data[phe_4:phe_6]) , function(x) x >= threshold)
    
    # 8-③计算 CDD 最大连续干旱天数，prcp < 1mm）
    CDD_phe1_3 <- max_consecutive_days(as.vector(prcp_data[phe_1:(phe_3 - 1)]), function(x) x < threshold)
    CDD_phe3_4 <- max_consecutive_days(as.vector(prcp_data[phe_3:(phe_4 - 1)]), function(x) x < threshold)
    CDD_phe4_6 <- max_consecutive_days(as.vector(prcp_data[phe_4:phe_6]), function(x) x < threshold)
    
    # 9-④计算Rx1day 最大日降水量
    Rx1day_phe1_3 <- max(prcp_data[phe_1:(phe_3 - 1)], na.rm = TRUE)
    Rx1day_phe3_4 <- max(prcp_data[phe_3:(phe_4 - 1)], na.rm = TRUE) 
    Rx1day_phe4_6 <- max(prcp_data[phe_4:phe_6], na.rm = TRUE)
    
    # 10-⑤计算 SDII（湿润日平均降水量）
    SDII_phe1_3 <- ifelse(RainDay_phe1_3 > 0, 
                          sum(prcp_data[phe_1:(phe_3 - 1)][prcp_data[phe_1:(phe_3 - 1)] >= threshold]) / RainDay_phe1_3, 0)
    SDII_phe3_4 <- ifelse(RainDay_phe3_4 > 0, 
                          sum(prcp_data[phe_3:(phe_4 - 1)][prcp_data[phe_3:(phe_4 - 1)] >= threshold]) / RainDay_phe3_4, 0)
    SDII_phe4_6 <- ifelse(RainDay_phe4_6 > 0, 
                          sum(prcp_data[phe_4:phe_6][prcp_data[phe_4:phe_6] >= threshold]) / RainDay_phe4_6, 0)
    
    ## 11- 3.生长阶段指标 Duration_phe1_3 Duration_phe3_4 Duration_phe4_6
    Duration_phe1_3 <- (phe_3 - 1) - phe_1 + 1
    Duration_phe3_4 <- (phe_4 - 1) - phe_3 + 1
    Duration_phe4_6 <- (phe_6 - 1) - phe_4 + 1
    
    # 返回结果
    result <- c(TXx_phe1_3, TNn_phe1_3, DTR_phe1_3, TXge30_phe1_3, TNlt2_phe1_3,
                TXx_phe3_4, TNn_phe3_4, DTR_phe3_4, TXge30_phe3_4, TNlt2_phe3_4,
                TXx_phe4_6, TNn_phe4_6, DTR_phe4_6, TXge30_phe4_6, TNlt2_phe4_6,
                RainDay_phe1_3, CWD_phe1_3, CDD_phe1_3, Rx1day_phe1_3, SDII_phe1_3,
                RainDay_phe3_4, CWD_phe3_4, CDD_phe3_4, Rx1day_phe3_4, SDII_phe3_4,
                RainDay_phe4_6, CWD_phe4_6, CDD_phe4_6, Rx1day_phe4_6, SDII_phe4_6,
                Duration_phe1_3, Duration_phe3_4, Duration_phe4_6)
  }, error = function(e) {
    return(rep(NA, 33))                         # 如果出现错误，返回 33 个 NA 值
  })
  
  return(result)
}

############################# 3. 并行计算 ##############################################################

result <- app(new_data, fun = multi, cores = 10)
# result <- app(single_pixel_data, fun = multi, cores = 10)
layer_16 <- result[[1]]   #CWD_phe1_3---0

non_na_pixels <- sum(!is.na(values(result[[1]])))
cat("影像非空像元数量:", non_na_pixels, "\n")     
#  NA 13-9593  14-9585  15-9575   16-9588  17-9573  18-9567  19-9521  20-9453  21-9448  22-7557
#  EA 13-31369 14-31389 15-31358  16-31331 17-31274 18-31237 19-31107 20-30914 21-30822 22-24256 
############################# 4. 指标合并输出 ##########################################################

## 温度相关指标共5个，每一层代表一个指标的影像
## 降水相关指标共5个，每一层代表一个指标的影像
## 物候指标1个，1层

# ① 提取温度相关指标 (5 个 /阶段)
Temp_phe1_3_2022 <- subset(result, 1:5)
names(Temp_phe1_3_2022) <- c("TXx_phe1_3", "TNn_phe1_3", "DTR_phe1_3", "TXge30_phe1_3", "TNlt2_phe1_3")
Temp_phe3_4_2022 <- subset(result, 6:10)
names(Temp_phe3_4_2022) <- c("TXx_phe3_4", "TNn_phe3_4", "DTR_phe3_4", "TXge30_phe3_4", "TNlt2_phe3_4")
Temp_phe4_6_2022 <- subset(result, 11:15)
names(Temp_phe4_6_2022) <- c("TXx_phe4_6", "TNn_phe4_6", "DTR_phe4_6", "TXge30_phe4_6", "TNlt2_phe4_6")


# ② 提取降水相关指标 (5 个 /阶段)
Prcp_phe1_3_2022 <- subset(result, 16:20)
names(Prcp_phe1_3_2022) <- c("RainDay_phe1_3", "CWD_phe1_3", "CDD_phe1_3", "Rx1day_phe1_3", "SDII_phe1_3")
Prcp_phe3_4_2022 <- subset(result, 21:25)
names(Prcp_phe3_4_2022) <- c("RainDay_phe3_4", "CWD_phe3_4", "CDD_phe3_4", "Rx1day_phe3_4", "SDII_phe3_4")
Prcp_phe4_6_2022 <- subset(result, 26:30)
names(Prcp_phe4_6_2022) <- c("RainDay_phe4_6", "CWD_phe4_6", "CDD_phe4_6", "Rx1day_phe4_6", "SDII_phe4_6")


# ③ 提取物候指标     (1 个 /阶段)
Duration_phe1_3_2022 <- subset(result, 31)
names(Duration_phe1_3_2022) <- "Duration_phe1_3"
Duration_phe3_4_2022 <- subset(result, 32)
names(Duration_phe3_4_2022) <- "Duration_phe3_4"
Duration_phe4_6_2022 <- subset(result, 33)
names(Duration_phe4_6_2022) <- "Duration_phe4_6"


# 设置保存目录
output_path <- "./EA_Results/3.Clim_Duration_Extremes/"

# 确保目录存在
if (!dir.exists(output_path)) {  dir.create(output_path, recursive = TRUE) }

# 保存数据，并允许覆盖
writeRaster(Temp_phe1_3_2022, filename = paste0(output_path, "Temp_phe1_3_2022.tif"), overwrite = TRUE)
writeRaster(Temp_phe3_4_2022, filename = paste0(output_path, "Temp_phe3_4_2022.tif"), overwrite = TRUE)
writeRaster(Temp_phe4_6_2022, filename = paste0(output_path, "Temp_phe4_6_2022.tif"), overwrite = TRUE)

writeRaster(Prcp_phe1_3_2022, filename = paste0(output_path, "Prcp_phe1_3_2022.tif"), overwrite = TRUE)
writeRaster(Prcp_phe3_4_2022, filename = paste0(output_path, "Prcp_phe3_4_2022.tif"), overwrite = TRUE)
writeRaster(Prcp_phe4_6_2022, filename = paste0(output_path, "Prcp_phe4_6_2022.tif"), overwrite = TRUE)

writeRaster(Duration_phe1_3_2022, filename = paste0(output_path, "Duration_phe1_3_2022.tif"), overwrite = TRUE)
writeRaster(Duration_phe3_4_2022, filename = paste0(output_path, "Duration_phe3_4_2022.tif"), overwrite = TRUE)
writeRaster(Duration_phe4_6_2022, filename = paste0(output_path, "Duration_phe4_6_2022.tif"), overwrite = TRUE)


