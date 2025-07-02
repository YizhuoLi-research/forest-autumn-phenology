rm(list = ls())

######################################   00 加载包  ##################################################
library(terra)
library(tidyverse)
library(raster)
library(ggplot2)
library(ppcor)
library(dplyr)

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
    head(10)  # 取前 10 个文件
}))

# 初始化存储所有气候区数据的列表
all_data <- list()

# 遍历每个气象指标
for (prefix in prefixes) {
  cat("Processing:", prefix, "\n")
  
  
  # prefix = "TXx_phe1_3"
  
  
  # 读取当前指标的所有文件
  files <- file_paths[grepl(paste0("^", prefix), basename(file_paths))]
  if (length(files) == 0) next  # 跳过没有文件的指标
  
  # 读取并合并 10 年的数据
  extremes_var <- rast(files)
  
  # 按气候区提取数据
  for (Climate_Class in Climate_Class_list) {
    cat("Processing Climate Class:", Climate_Class, "\n")
    
    # Climate_Class = "Cfa"
    
    order_val <- match(Climate_Class, classify_border$EA_koppen_30km_addClimate)
    if (is.na(order_val)) next  # 跳过不存在的气候类型
    
    
    selected_border_val <- classify_border[order_val, ]
    
    # 对 该极端指标变量、phe变量 进行掩膜
    extremes_masked <- mask(crop(extremes_var, selected_border_val), selected_border_val)
    plot(extremes_masked)
    
    # 提取像元值
    values <- terra::values(extremes_masked, na.rm = TRUE)
    
    # 将数据存储到列表中
    if (length(values) > 0) {
      # 创建数据框，包含变量名、气候区、索引和数值
      df <- data.frame(
        Variable = prefix,
        Climate = Climate_Class,
        Index = seq_along(values),  # 添加索引列
        Value = as.numeric(values)
      )
      all_data[[paste0(prefix, "_", Climate_Class)]] <- df
    }
  }
}

# 合并所有数据
all_data_combined <- do.call(rbind, all_data)

# 查看整理后的数据
head(all_data_combined)

# 合并所有数据
all_data_combined <- do.call(rbind, all_data)

all_data_combined <- all_data_combined %>%
  mutate(Variable = case_when(
    str_detect(Variable, "phe1_3") ~ str_replace(Variable, "phe1_3", "SOS-GMO"),
    str_detect(Variable, "phe3_4") ~ str_replace(Variable, "phe3_4", "GMO-GDO"),
    str_detect(Variable, "phe4_6") ~ str_replace(Variable, "phe4_6", "GDO-EOS"),
    TRUE ~ Variable  # 如果没有匹配到，保留原值
  ))

# # 定义指标类型的顺序
# variable_order <- c(
#   "TXx_SOS-GMO", "TNn_SOS-GMO", "DTR_SOS-GMO", "TXge30_SOS-GMO", "TNlt2_SOS-GMO",
#   "RainDay_SOS-GMO", "CDD_SOS-GMO", "CWD_SOS-GMO", "Rx1day_SOS-GMO", "SDII_SOS-GMO",
#   "TXx_GMO-GDO", "TNn_GMO-GDO", "DTR_GMO-GDO", "TXge30_GMO-GDO", "TNlt2_GMO-GDO",
#   "RainDay_GMO-GDO", "CDD_GMO-GDO", "CWD_GMO-GDO", "Rx1day_GMO-GDO", "SDII_GMO-GDO",
#   "TXx_GDO-EOS", "TNn_GDO-EOS", "DTR_GDO-EOS", "TXge30_GDO-EOS", "TNlt2_GDO-EOS",
#   "RainDay_GDO-EOS", "CDD_GDO-EOS", "CWD_GDO-EOS", "Rx1day_GDO-EOS", "SDII_GDO-EOS"
# )

# # 将 Variable 列转换为因子，并设置顺序
# all_data_combined$Variable <- factor(all_data_combined$Variable, levels = variable_order)

############################   Temperature-related indice  ################################
#######   1-1.TXx   ########

df_TXx_13 <- all_data_combined %>%
  filter(Variable == "TXx_SOS-GMO")

df_TXx_34 <- all_data_combined %>%
  filter(Variable == "TXx_GMO-GDO")

df_TXx_46 <- all_data_combined %>%
  filter(Variable == "TXx_GDO-EOS")

#######   1-2.TXge30   ########

df_TXge30_13 <- all_data_combined %>%
  filter(Variable == "TXge30_SOS-GMO")

df_TXge30_34 <- all_data_combined %>%
  filter(Variable == "TXge30_GMO-GDO")

df_TXge30_46 <- all_data_combined %>%
  filter(Variable == "TXge30_GDO-EOS")

#######   1-3.TNn   ########

df_TNn_13 <- all_data_combined %>%
  filter(Variable == "TNn_SOS-GMO")

df_TNn_34 <- all_data_combined %>%
  filter(Variable == "TNn_GMO-GDO")

df_TNn_46 <- all_data_combined %>%
  filter(Variable == "TNn_GDO-EOS")

#######   1-4.TNlt2   ########

df_TNlt2_13 <- all_data_combined %>%
  filter(Variable == "TNlt2_SOS-GMO")

df_TNlt2_34 <- all_data_combined %>%
  filter(Variable == "TNlt2_GMO-GDO")

df_TNlt2_46 <- all_data_combined %>%
  filter(Variable == "TNlt2_GDO-EOS")

#######   1-5.DTR   ########

df_DTR_13 <- all_data_combined %>%
  filter(Variable == "DTR_SOS-GMO")

df_DTR_34 <- all_data_combined %>%
  filter(Variable == "DTR_GMO-GDO")

df_DTR_46 <- all_data_combined %>%
  filter(Variable == "DTR_GDO-EOS")

#######   2-1.Raindays   ########

df_RainDay_13 <- all_data_combined %>%
  filter(Variable == "RainDay_SOS-GMO")

df_RainDay_34 <- all_data_combined %>%
  filter(Variable == "RainDay_GMO-GDO")

df_RainDay_46 <- all_data_combined %>%
  filter(Variable == "RainDay_GDO-EOS")

#######   2-2.CWD   ########

df_CWD_13 <- all_data_combined %>%
  filter(Variable == "CWD_SOS-GMO")

df_CWD_34 <- all_data_combined %>%
  filter(Variable == "CWD_GMO-GDO")

df_CWD_46 <- all_data_combined %>%
  filter(Variable == "CWD_GDO-EOS")

#######   2-3.CDD   ########

df_CDD_13 <- all_data_combined %>%
  filter(Variable == "CDD_SOS-GMO")

df_CDD_34 <- all_data_combined %>%
  filter(Variable == "CDD_GMO-GDO")

df_CDD_46 <- all_data_combined %>%
  filter(Variable == "CDD_GDO-EOS")

#######   2-4.Rx1day   ########

df_Rx1day_13 <- all_data_combined %>%
  filter(Variable == "Rx1day_SOS-GMO")

df_Rx1day_34 <- all_data_combined %>%
  filter(Variable == "Rx1day_GMO-GDO")

df_Rx1day_46 <- all_data_combined %>%
  filter(Variable == "Rx1day_GDO-EOS")

#######   2-5.SDII   ########

df_SDII_13 <- all_data_combined %>%
  filter(Variable == "SDII_SOS-GMO")

df_SDII_34 <- all_data_combined %>%
  filter(Variable == "SDII_GMO-GDO")

df_SDII_46 <- all_data_combined %>%
  filter(Variable == "SDII_GDO-EOS")


# 步骤 1：构建数据框列表（用名字自动作为标签）
density_inputs <- list(
  TXx_13 = df_TXx_13, TXx_34 = df_TXx_34, TXx_46 = df_TXx_46,
  TXge30_13 = df_TXge30_13, TXge30_34 = df_TXge30_34, TXge30_46 = df_TXge30_46,
  TNn_13 = df_TNn_13, TNn_34 = df_TNn_34, TNn_46 = df_TNn_46,
  TNlt2_13 = df_TNlt2_13, TNlt2_34 = df_TNlt2_34, TNlt2_46 = df_TNlt2_46,
  DTR_13 = df_DTR_13, DTR_34 = df_DTR_34, DTR_46 = df_DTR_46,
  RainDay_13 = df_RainDay_13, RainDay_34 = df_RainDay_34, RainDay_46 = df_RainDay_46,
  CWD_13 = df_CWD_13, CWD_34 = df_CWD_34, CWD_46 = df_CWD_46,
  CDD_13 = df_CDD_13, CDD_34 = df_CDD_34, CDD_46 = df_CDD_46,
  Rx1day_13 = df_Rx1day_13, Rx1day_34 = df_Rx1day_34, Rx1day_46 = df_Rx1day_46,
  SDII_13 = df_SDII_13, SDII_34 = df_SDII_34, SDII_46 = df_SDII_46
)

# 步骤 2：定义通用密度提取函数
library(dplyr)
library(ggplot2)

get_density_peaks <- function(df, label) {
  df %>%
    group_by(Climate) %>%
    summarise({
      d <- density(Value, na.rm = TRUE)  # 自动选择合适范围
      peak_index <- which.max(d$y)
      peak_x <- d$x[peak_index]
      peak_y <- d$y[peak_index]
      tibble(Variable = label, Peak_Value = peak_x, Frequency = peak_y)
    }, .groups = "drop")
}


#  步骤 3：循环批量计算
# 批量运行密度提取
density_results <- lapply(names(density_inputs), function(name) {
  get_density_peaks(density_inputs[[name]], name)
})

# 合并所有结果
peak_density_all <- bind_rows(density_results)
peak_density_all <- peak_density_all %>%
  mutate(
    Peak_Value = round(as.numeric(Peak_Value), 2),
    Frequency = round(as.numeric(Frequency), 2)
  )

# 展示结果，输出
peak_density_all
write.csv(peak_density_all,
          "./EA+NA_Results/merged_Cli_indices_trend/Cli-indices-Peak&density_results.csv", row.names = FALSE)
