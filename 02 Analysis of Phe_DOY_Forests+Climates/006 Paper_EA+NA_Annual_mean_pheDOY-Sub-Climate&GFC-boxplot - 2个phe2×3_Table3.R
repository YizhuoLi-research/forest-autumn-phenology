###### 0. 加载包 ####
library(terra)
library(tidyverse)
library(raster)
library(ggplot2)
setwd("D:/Graduation_Thesis")

###############################################################################################
#############################   0. 计算不同气候区、不同GFC的DOY差异  ###########################
###############################################################################################

#############################   00.按照温度类型，将气候区分为6个气候区域   ####################
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

#划定filter_values_list中每个类型区域的边界
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

print(classify_border)   # 查看 classify_border 的整体结构
summary(classify_border) # 确认 classify_border 是否有几何和属性数据

# ############################## 001 按三级气候分区作图  SOS   #########################################      

# # SOS: phe1_DOY ;  MGP: phe2_DOY ;  GMO：phe3_DOY
# # GDO: phe4_DOY ;  MSP: phe5_DOY ;  EOS：phe6_DOY

## 1. 读取数据
r1 <- rast("./EA+NA_Results/merged_Phe_DOY_mean_2_PHE_analysis/merged_10yr_mean_phe1_DOY.tif")
r1 <- app(r1, fun = function(x) as.integer(round(x)))
r2 <-  rast("./EA+NA_Results/EA+NA_GFC_30km.tif")
## 2. 提取各个区域的边界
Cfa_border <- classify_border[classify_border$EA_koppen_30km_addClimate %in% Cfa, ]
Cfb_border <- classify_border[classify_border$EA_koppen_30km_addClimate %in% Cfb, ]
Dfb_border <- classify_border[classify_border$EA_koppen_30km_addClimate %in% Dfb, ]
Dfc_border <- classify_border[classify_border$EA_koppen_30km_addClimate %in% Dfc, ]
Dwb_border <- classify_border[classify_border$EA_koppen_30km_addClimate %in% Dwb, ]
Dwc_border <- classify_border[classify_border$EA_koppen_30km_addClimate %in% Dwc, ]

## 3. 掩膜气候区域的DOY和林类数据
Cfa_doy <- mask(r1, Cfa_border, touches = FALSE)
Cfb_doy <- mask(r1, Cfb_border, touches = FALSE)
Dfb_doy <- mask(r1, Dfb_border, touches = FALSE)
Dfc_doy <- mask(r1, Dfc_border, touches = FALSE)
Dwb_doy <- mask(r1, Dwb_border, touches = FALSE)
Dwc_doy <- mask(r1, Dwc_border, touches = FALSE)


library(terra)
library(ggplot2)
library(dplyr)
library(ggpubr)

# 1. 定义气候类型和区域边界
climate_types <- list(
  Cfa = Cfa_border,
  Cfb = Cfb_border,
  Dfb = Dfb_border,
  Dfc = Dfc_border,
  Dwb = Dwb_border,
  Dwc = Dwc_border
)

# 2. 初始化空数据框，用于存储所有气候区域的数据
all_data <- data.frame()



# ############################## 003 按三级气候分区作图  GDO   #########################################      

# # SOS: phe1_DOY ;  MGP: phe2_DOY ;  GMO：phe3_DOY
# # GDO: phe4_DOY ;  MSP: phe5_DOY ;  EOS：phe6_DOY

## 1. 读取数据
r1 <- rast("./EA+NA_Results/merged_Phe_DOY_mean_2_PHE_analysis/merged_10yr_mean_phe4_DOY.tif")
r1 <- app(r1, fun = function(x) as.integer(round(x)))
r2 <-  rast("./EA+NA_Results/EA+NA_GFC_30km.tif")

Cfa_doy <- mask(r1, Cfa_border, touches = FALSE)
Cfb_doy <- mask(r1, Cfb_border, touches = FALSE)
Dfb_doy <- mask(r1, Dfb_border, touches = FALSE)
Dfc_doy <- mask(r1, Dfc_border, touches = FALSE)
Dwb_doy <- mask(r1, Dwb_border, touches = FALSE)
Dwc_doy <- mask(r1, Dwc_border, touches = FALSE)

climate_types <- list(
  Cfa = Cfa_border,
  Cfb = Cfb_border,
  Dfb = Dfb_border,
  Dfc = Dfc_border,
  Dwb = Dwb_border,
  Dwc = Dwc_border
)

all_data <- data.frame()

for (climate in names(climate_types)) {
  border <- climate_types[[climate]]
  
  doy_data <- mask(r1, border)
  forest_data <- mask(r2, border)
  
  forest1_DOY <- mask(doy_data, ifel(forest_data == 1, 1, NA))  # 次生林
  forest2_DOY <- mask(doy_data, ifel(forest_data == 2, 1, NA))  # 原生林
  forest3_DOY <- mask(doy_data, ifel(forest_data == 3, 1, NA))  # 人工林
  
  df_forest1 <- as.data.frame(forest1_DOY, xy = TRUE, na.rm = TRUE)
  df_forest2 <- as.data.frame(forest2_DOY, xy = TRUE, na.rm = TRUE)
  df_forest3 <- as.data.frame(forest3_DOY, xy = TRUE, na.rm = TRUE)
  
  df_forest1$ForestType <- "Naturally regenerating forest"
  df_forest2$ForestType <- "Primary forest"
  df_forest3$ForestType <- "Planted forest"
  
  combined <- rbind(
    data.frame(DOY = df_forest1[, 3], ForestType = df_forest1$ForestType),
    data.frame(DOY = df_forest2[, 3], ForestType = df_forest2$ForestType),
    data.frame(DOY = df_forest3[, 3], ForestType = df_forest3$ForestType)
  )
  combined$Climate <- climate
  
  all_data <- rbind(all_data, combined)
}

all_data$Climate <- factor(all_data$Climate,levels = c("Cfa", "Dfb","Dwb","Cfb", "Dfc","Dwc"))
all_data$ForestType <- factor(all_data$ForestType, levels = c("Primary forest","Naturally regenerating forest", "Planted forest"))
head(all_data)
# 按组分类计算 Table
summary_stats <- all_data %>%
  group_by(Climate, ForestType) %>%
  summarise(
    mean_DOY = round(mean(DOY, na.rm = TRUE), 2),
    sd_0.15 = round(0.15 * sd(DOY, na.rm = TRUE), 2),
    .groups = "drop"
  )
summary_stats %>%
  mutate(
    mean_DOY = format(mean_DOY, nsmall = 2),
    sd_0.15 = format(sd_0.15, nsmall = 2)
  )


# Climate ForestType                    mean_DOY sd_0.15
# <fct>   <fct>                         <chr>    <chr>  
#   1 Cfa     Primary forest                223.65   1.18   
# 2 Cfa     Naturally regenerating forest 222.62   1.04   
# 3 Cfa     Planted forest                224.87   1.14   
# 4 Dfb     Primary forest                226.13   1.37   
# 5 Dfb     Naturally regenerating forest 224.77   1.22   
# 6 Dfb     Planted forest                222.52   1.12   
# 7 Dwb     Primary forest                222.36   0.67   
# 8 Dwb     Naturally regenerating forest 225.20   0.78   
# 9 Dwb     Planted forest                226.00   0.73   
# 10 Cfb     Primary forest                230.46   1.22   
# 11 Cfb     Naturally regenerating forest 225.71   0.86   
# 12 Cfb     Planted forest                231.90   1.02   
# 13 Dfc     Primary forest                224.74   0.66   
# 14 Dfc     Naturally regenerating forest 225.01   0.75   
# 15 Dfc     Planted forest                230.06   0.61   
# 16 Dwc     Primary forest                223.55   0.59   
# 17 Dwc     Naturally regenerating forest 222.77   0.55   
# 18 Dwc     Planted forest                219.66   0.24  


# ############################## 004 按三级气候分区作图  EOS   #########################################      

# # SOS: phe1_DOY ;  MGP: phe2_DOY ;  GMO：phe3_DOY
# # GDO: phe4_DOY ;  MSP: phe5_DOY ;  EOS：phe6_DOY

## 1. 读取数据
r1 <- rast("./EA+NA_Results/merged_Phe_DOY_mean_2_PHE_analysis/merged_10yr_mean_phe6_DOY.tif")
r1 <- app(r1, fun = function(x) as.integer(round(x)))
r2 <-  rast("./EA+NA_Results/EA+NA_GFC_30km.tif")

Cfa_doy <- mask(r1, Cfa_border, touches = FALSE)
Cfb_doy <- mask(r1, Cfb_border, touches = FALSE)
Dfb_doy <- mask(r1, Dfb_border, touches = FALSE)
Dfc_doy <- mask(r1, Dfc_border, touches = FALSE)
Dwb_doy <- mask(r1, Dwb_border, touches = FALSE)
Dwc_doy <- mask(r1, Dwc_border, touches = FALSE)

climate_types <- list(
  Cfa = Cfa_border,
  Cfb = Cfb_border,
  Dfb = Dfb_border,
  Dfc = Dfc_border,
  Dwb = Dwb_border,
  Dwc = Dwc_border
)

all_data <- data.frame()

for (climate in names(climate_types)) {
  border <- climate_types[[climate]]
  
  doy_data <- mask(r1, border)
  forest_data <- mask(r2, border)
  
  forest1_DOY <- mask(doy_data, ifel(forest_data == 1, 1, NA))  # 次生林
  forest2_DOY <- mask(doy_data, ifel(forest_data == 2, 1, NA))  # 原生林
  forest3_DOY <- mask(doy_data, ifel(forest_data == 3, 1, NA))  # 人工林
  
  df_forest1 <- as.data.frame(forest1_DOY, xy = TRUE, na.rm = TRUE)
  df_forest2 <- as.data.frame(forest2_DOY, xy = TRUE, na.rm = TRUE)
  df_forest3 <- as.data.frame(forest3_DOY, xy = TRUE, na.rm = TRUE)
  
  df_forest1$ForestType <- "Naturally regenerating forest"
  df_forest2$ForestType <- "Primary forest"
  df_forest3$ForestType <- "Planted forest"
  
  combined <- rbind(
    data.frame(DOY = df_forest1[, 3], ForestType = df_forest1$ForestType),
    data.frame(DOY = df_forest2[, 3], ForestType = df_forest2$ForestType),
    data.frame(DOY = df_forest3[, 3], ForestType = df_forest3$ForestType)
  )
  combined$Climate <- climate
  
  all_data <- rbind(all_data, combined)
}

all_data$Climate <- factor(all_data$Climate,levels = c("Cfa", "Dfb","Dwb","Cfb", "Dfc","Dwc"))
all_data$ForestType <- factor(all_data$ForestType, levels = c("Primary forest", "Naturally regenerating forest", "Planted forest"))

# 按组分类计算 Table
summary_stats <- all_data %>%
  group_by(Climate, ForestType) %>%
  summarise(
    mean_DOY = round(mean(DOY, na.rm = TRUE), 2),
    sd_0.15 = round(0.15 * sd(DOY, na.rm = TRUE), 2),
    .groups = "drop"
  )
summary_stats %>%
  mutate(
    mean_DOY = format(mean_DOY, nsmall = 2),
    sd_0.15 = format(sd_0.15, nsmall = 2)
  )

# # A tibble: 18 × 4
# Climate ForestType                    mean_DOY sd_0.15
# <fct>   <fct>                         <chr>    <chr>  
#   1 Cfa     Primary forest                325.15   1.16   
# 2 Cfa     Naturally regenerating forest 326.20   0.98   
# 3 Cfa     Planted forest                327.18   0.90   
# 4 Dfb     Primary forest                285.81   1.44   
# 5 Dfb     Naturally regenerating forest 292.20   1.78   
# 6 Dfb     Planted forest                298.38   1.68   
# 7 Dwb     Primary forest                286.80   1.00   
# 8 Dwb     Naturally regenerating forest 292.16   1.44   
# 9 Dwb     Planted forest                296.66   1.78   
# 10 Cfb     Primary forest                300.66   2.64   
# 11 Cfb     Naturally regenerating forest 318.86   1.54   
# 12 Cfb     Planted forest                316.55   1.36   
# 13 Dfc     Primary forest                267.58   1.27   
# 14 Dfc     Naturally regenerating forest 275.84   1.32   
# 15 Dfc     Planted forest                290.16   1.15   
# 16 Dwc     Primary forest                271.54   0.88   
# 17 Dwc     Naturally regenerating forest 278.67   1.26   
# 18 Dwc     Planted forest                278.52   0.47 



######################## 05 Table-all-GDO 仅按气候区分类  ##################


## 1. 读取数据
r1 <- rast("./EA+NA_Results/merged_Phe_DOY_mean_2_PHE_analysis/merged_10yr_mean_phe4_DOY.tif")
r1 <- app(r1, fun = function(x) as.integer(round(x)))
r2 <-  rast("./EA+NA_Results/EA+NA_GFC_30km.tif")

Cfa_doy <- mask(r1, Cfa_border, touches = FALSE)
Cfb_doy <- mask(r1, Cfb_border, touches = FALSE)
Dfb_doy <- mask(r1, Dfb_border, touches = FALSE)
Dfc_doy <- mask(r1, Dfc_border, touches = FALSE)
Dwb_doy <- mask(r1, Dwb_border, touches = FALSE)
Dwc_doy <- mask(r1, Dwc_border, touches = FALSE)

climate_types <- list(
  Cfa = Cfa_border,
  Cfb = Cfb_border,
  Dfb = Dfb_border,
  Dfc = Dfc_border,
  Dwb = Dwb_border,
  Dwc = Dwc_border
)

all_data <- data.frame()

# 按气候区掩膜提取DOY数据
for (climate in names(climate_types)) {
  border <- climate_types[[climate]]
  doy_data <- mask(r1, border, touches = FALSE)
  
  df <- as.data.frame(doy_data, xy = TRUE, na.rm = TRUE)
  
  # 如果r1只有一个波段，列名是"layer"；多个波段请确认列名
  df <- data.frame(DOY = df[, 3], Climate = climate)
  
  all_data <- rbind(all_data, df)
}

# 确保Climate是因子并设定顺序（可选）
all_data$Climate <- factor(all_data$Climate, levels = c("Cfa", "Dfb", "Dwb", "Cfb", "Dfc", "Dwc"))

# 分组计算DOY平均值和标准差的15%
summary_stats <- all_data %>%
  group_by(Climate) %>%
  summarise(
    mean_DOY = round(mean(DOY, na.rm = TRUE), 2),
    sd_0.15 = round(0.15 * sd(DOY, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  mutate(
    mean_DOY = format(mean_DOY, nsmall = 2),
    sd_0.15 = format(sd_0.15, nsmall = 2)
  )

print(summary_stats)
# Climate mean_DOY sd_0.15
# 1 Cfa     223.34   1.08   
# 2 Dfb     224.68   1.24   
# 3 Dwb     224.19   0.77   
# 4 Cfb     229.11   1.06   
# 5 Dfc     224.65   0.72   
# 6 Dwc     223.23   0.58 


######################## 06 Table-all-EOS 仅按气候区分类  ##################


## 1. 读取数据
r1 <- rast("./EA+NA_Results/merged_Phe_DOY_mean_2_PHE_analysis/merged_10yr_mean_phe6_DOY.tif")
r1 <- app(r1, fun = function(x) as.integer(round(x)))
r2 <-  rast("./EA+NA_Results/EA+NA_GFC_30km.tif")

Cfa_doy <- mask(r1, Cfa_border, touches = FALSE)
Cfb_doy <- mask(r1, Cfb_border, touches = FALSE)
Dfb_doy <- mask(r1, Dfb_border, touches = FALSE)
Dfc_doy <- mask(r1, Dfc_border, touches = FALSE)
Dwb_doy <- mask(r1, Dwb_border, touches = FALSE)
Dwc_doy <- mask(r1, Dwc_border, touches = FALSE)

climate_types <- list(
  Cfa = Cfa_border,
  Cfb = Cfb_border,
  Dfb = Dfb_border,
  Dfc = Dfc_border,
  Dwb = Dwb_border,
  Dwc = Dwc_border
)

all_data <- data.frame()

# 按气候区掩膜提取DOY数据
for (climate in names(climate_types)) {
  border <- climate_types[[climate]]
  doy_data <- mask(r1, border, touches = FALSE)
  
  df <- as.data.frame(doy_data, xy = TRUE, na.rm = TRUE)
  
  # 如果r1只有一个波段，列名是"layer"；多个波段请确认列名
  df <- data.frame(DOY = df[, 3], Climate = climate)
  
  all_data <- rbind(all_data, df)
}

# 确保Climate是因子并设定顺序（可选）
all_data$Climate <- factor(all_data$Climate, levels = c("Cfa", "Dfb", "Dwb", "Cfb", "Dfc", "Dwc"))

# 分组计算DOY平均值和标准差的15%
summary_stats <- all_data %>%
  group_by(Climate) %>%
  summarise(
    mean_DOY = round(mean(DOY, na.rm = TRUE), 2),
    sd_0.15 = round(0.15 * sd(DOY, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  mutate(
    mean_DOY = format(mean_DOY, nsmall = 2),
    sd_0.15 = format(sd_0.15, nsmall = 2)
  )

print(summary_stats)

# Climate mean_DOY sd_0.15
# 1 Cfa     326.49   0.96   
# 2 Dfb     292.07   1.79   
# 3 Dwb     290.47   1.40   
# 4 Cfb     317.30   1.52   
# 5 Dfc     270.22   1.44   
# 6 Dwc     273.33   1.09  

######################## 07 Table-all-GDO 仅按森林类型分类  ##################
library(terra)
library(dplyr)

# 1. 读取数据
r1 <- rast("./EA+NA_Results/merged_Phe_DOY_mean_2_PHE_analysis/merged_10yr_mean_phe4_DOY.tif") # GDO
r1 <- app(r1, fun = function(x) as.integer(round(x)))
r2 <- rast("./EA+NA_Results/EA+NA_GFC_30km.tif") # GFC：1=次生林，2=原生林，3=人工林

# 2. 提取不同林类型的DOY数据
forest1_DOY <- mask(r1, ifel(r2 == 1, 1, NA))  # 次生林
forest2_DOY <- mask(r1, ifel(r2 == 2, 1, NA))  # 原生林
forest3_DOY <- mask(r1, ifel(r2 == 3, 1, NA))  # 人工林

# 3. 转为数据框
df_forest1 <- as.data.frame(forest1_DOY, xy = TRUE, na.rm = TRUE)
df_forest2 <- as.data.frame(forest2_DOY, xy = TRUE, na.rm = TRUE)
df_forest3 <- as.data.frame(forest3_DOY, xy = TRUE, na.rm = TRUE)
df_all <- as.data.frame(r1, xy = TRUE, na.rm = TRUE)  # 所有森林像元

# 4. 添加林类标签
df_forest1$ForestType <- "Naturally regenerating forest"
df_forest2$ForestType <- "Primary forest"
df_forest3$ForestType <- "Planted forest"
df_all$ForestType <- "All forests"

# 5. 合并数据
all_data <- rbind(
  data.frame(DOY = df_forest1[, 3], ForestType = df_forest1$ForestType),
  data.frame(DOY = df_forest2[, 3], ForestType = df_forest2$ForestType),
  data.frame(DOY = df_forest3[, 3], ForestType = df_forest3$ForestType),
  data.frame(DOY = df_all[, 3], ForestType = df_all$ForestType)
)

# 6. 设置顺序并计算均值和0.15标准差
all_data$ForestType <- factor(all_data$ForestType,
                              levels = c("Primary forest", "Naturally regenerating forest", "Planted forest", "All forests"))

summary_stats <- all_data %>%
  group_by(ForestType) %>%
  summarise(
    mean_DOY = round(mean(DOY, na.rm = TRUE), 2),
    sd_0.15 = round(0.15 * sd(DOY, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  mutate(
    mean_DOY = format(mean_DOY, nsmall = 2),
    sd_0.15 = format(sd_0.15, nsmall = 2)
  )

print(summary_stats)
# ForestType                    mean_DOY sd_0.15
# 1 Primary forest                224.47   0.80   
# 2 Naturally regenerating forest 224.57   1.11   
# 3 Planted forest                227.54   1.25   
# 4 All forests                   224.82   1.04  

######################## 08 Table-all-EOS 仅按森林类型分类  ##################
library(terra)
library(dplyr)

# 1. 读取数据
r1 <- rast("./EA+NA_Results/merged_Phe_DOY_mean_2_PHE_analysis/merged_10yr_mean_phe6_DOY.tif") # EOS
r1 <- app(r1, fun = function(x) as.integer(round(x)))
r2 <- rast("./EA+NA_Results/EA+NA_GFC_30km.tif") # GFC：1=次生林，2=原生林，3=人工林

# 2. 提取不同林类型的DOY数据
forest1_DOY <- mask(r1, ifel(r2 == 1, 1, NA))  # 次生林
forest2_DOY <- mask(r1, ifel(r2 == 2, 1, NA))  # 原生林
forest3_DOY <- mask(r1, ifel(r2 == 3, 1, NA))  # 人工林

# 3. 转为数据框
df_forest1 <- as.data.frame(forest1_DOY, xy = TRUE, na.rm = TRUE)
df_forest2 <- as.data.frame(forest2_DOY, xy = TRUE, na.rm = TRUE)
df_forest3 <- as.data.frame(forest3_DOY, xy = TRUE, na.rm = TRUE)
df_all <- as.data.frame(r1, xy = TRUE, na.rm = TRUE)  # 所有森林像元

# 4. 添加林类标签
df_forest1$ForestType <- "Naturally regenerating forest"
df_forest2$ForestType <- "Primary forest"
df_forest3$ForestType <- "Planted forest"
df_all$ForestType <- "All forests"

# 5. 合并数据
all_data <- rbind(
  data.frame(DOY = df_forest1[, 3], ForestType = df_forest1$ForestType),
  data.frame(DOY = df_forest2[, 3], ForestType = df_forest2$ForestType),
  data.frame(DOY = df_forest3[, 3], ForestType = df_forest3$ForestType),
  data.frame(DOY = df_all[, 3], ForestType = df_all$ForestType)
)

# 6. 设置顺序并计算均值和0.15标准差
all_data$ForestType <- factor(all_data$ForestType,
                              levels = c("Primary forest", "Naturally regenerating forest", "Planted forest", "All forests"))

summary_stats <- all_data %>%
  group_by(ForestType) %>%
  summarise(
    mean_DOY = round(mean(DOY, na.rm = TRUE), 2),
    sd_0.15 = round(0.15 * sd(DOY, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  mutate(
    mean_DOY = format(mean_DOY, nsmall = 2),
    sd_0.15 = format(sd_0.15, nsmall = 2)
  )

print(summary_stats)
# ForestType                    mean_DOY sd_0.15
# 1 Primary forest                271.39   1.79   
# 2 Naturally regenerating forest 296.65   3.09   
# 3 Planted forest                312.64   2.26   
# 4 All forests                   288.92   3.41 