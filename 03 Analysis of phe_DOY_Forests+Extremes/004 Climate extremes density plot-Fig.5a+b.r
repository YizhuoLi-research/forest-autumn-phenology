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

#######  (1) TXx_13   ########
p_TXx_13 <- ggplot(df_TXx_13, aes(x = Value, color = Climate, fill = Climate)) +
  geom_line(stat = "density", linewidth = 1.0) +  # 加粗线条
  labs(
    x = expression("TXx from SOS to GMO "(degree~C)),  # x 轴标签
    y = NULL ) + # 移除 y 轴标题
  theme_minimal() +  # 使用 minimal 主题
  theme(
    legend.position = "none",  # 移除图例
    axis.text.x = element_text(size = 20),  # x 轴刻度字体大小
    axis.text.y = element_text(size = 20),  # y 轴刻度字体大小
    axis.title = element_text(size = 19),  # 坐标轴标题字体大小
    axis.title.y = element_blank(),  # 移除 y 轴标题
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),  # 设置主要网格线
    panel.grid.minor = element_blank(),  # 移除次要网格线（可选）
    axis.line = element_line(linewidth = 0.5, color = "black"),  # 添加坐标轴线
    axis.ticks = element_line(linewidth = 0.5, color = "black")) + # 添加刻度线
  xlim(0, 45)+  # 设置 x 轴范围
  # scale_y_continuous(
  #   limits = c(0.0, 0.25),  # 设置 y 轴范围
  #   breaks = seq(0.0,0.25, by = 0.1),  # 设置刻度间隔
  #   labels = scales::number_format(accuracy = 0.1))  # 保留 1 位小数
  scale_y_continuous(
    limits = c(0.0, 0.3),  # 设置 y 轴范围
    breaks = seq(0.0,0.3, by = 0.1),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 0.1))
print(p_TXx_13)

#######  (2) TXx_34   ########

p_TXx_34 <- ggplot(df_TXx_34, aes(x = Value, color = Climate, fill = Climate)) +
  geom_line(stat = "density", linewidth = 1.0) +  # 加粗线条
  labs(
    x = expression("TXx from GMO to GDO "(degree~C)),  # x 轴标签
    y = NULL ) + # 移除 y 轴标题
  theme_minimal() +  # 使用 minimal 主题
  theme(
    legend.position = "none",  # 移除图例
    axis.text.x = element_text(size = 20),  # x 轴刻度字体大小
    axis.text.y = element_text(size = 20),  # y 轴刻度字体大小
    axis.title = element_text(size = 19),  # 坐标轴标题字体大小
    axis.title.y = element_blank(),  # 移除 y 轴标题
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),  # 设置主要网格线
    panel.grid.minor = element_blank(),  # 移除次要网格线（可选）
    axis.line = element_line(linewidth = 0.5, color = "black"),  # 添加坐标轴线
    axis.ticks = element_line(linewidth = 0.5, color = "black")) + # 添加刻度线
  # ylim(0.00, 0.25) +  # 设置 y 轴范围
  xlim(0, 45)+  # 设置 x 轴范围
  # scale_y_continuous(
  #   limits = c(0.0, 0.25),  # 设置 y 轴范围
  #   breaks = seq(0.0, 0.25, by = 0.1),  # 设置刻度间隔
  #   labels = scales::number_format(accuracy = 0.1))  # 保留 1 位小数
  scale_y_continuous(
    limits = c(0.0, 0.3),  # 设置 y 轴范围
    breaks = seq(0.0,0.3, by = 0.1),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 0.1))
print(p_TXx_34)

#######  (3) TXx_46   ########

p_TXx_46 <- ggplot(df_TXx_46, aes(x = Value, color = Climate, fill = Climate)) +
  geom_line(stat = "density", linewidth = 1.0) +  # 加粗线条
  labs(
    x = expression("TXx from GDO to EOS "(degree~C)),  # x 轴标签
    y = NULL ) + # 移除 y 轴标题
  theme_minimal() +  # 使用 minimal 主题
  theme(
    legend.position = "none",  # 移除图例
    axis.text.x = element_text(size = 20),  # x 轴刻度字体大小
    axis.text.y = element_text(size = 20),  # y 轴刻度字体大小
    axis.title = element_text(size = 19),  # 坐标轴标题字体大小
    axis.title.y = element_blank(),  # 移除 y 轴标题
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),  # 设置主要网格线
    panel.grid.minor = element_blank(),  # 移除次要网格线（可选）
    axis.line = element_line(linewidth = 0.5, color = "black"),  # 添加坐标轴线
    axis.ticks = element_line(linewidth = 0.5, color = "black")) + # 添加刻度线
  # ylim(0.00, 0.25) +  # 设置 y 轴范围
  xlim(0, 45) + # 设置 x 轴范围
  # scale_y_continuous(
  #   limits = c(0.0, 0.25),  # 设置 y 轴范围
  #   breaks = seq(0.0, 0.25, by = 0.1),  # 设置刻度间隔
  #   labels = scales::number_format(accuracy = 0.1))  # 保留 1 位小数
  scale_y_continuous(
    limits = c(0.0, 0.3),  # 设置 y 轴范围
    breaks = seq(0.0,0.3, by = 0.1),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 0.1))
print(p_TXx_46)

#######   1-2.TXge30   ########

df_TXge30_13 <- all_data_combined %>%
  filter(Variable == "TXge30_SOS-GMO")

df_TXge30_34 <- all_data_combined %>%
  filter(Variable == "TXge30_GMO-GDO")

df_TXge30_46 <- all_data_combined %>%
  filter(Variable == "TXge30_GDO-EOS")

max_value <- max(df_TXge30_13$Value, na.rm = TRUE)
min_value <- min(df_TXge30_13$Value, na.rm = TRUE)
print(paste("最大值:", max_value))
print(paste("最小值:", min_value))
# 0-34  0-113  0-92

#######  (1) TXge30_13   ########
p_TXge30_13 <- ggplot(df_TXge30_13, aes(x = Value, color = Climate, fill = Climate)) +
  geom_line(stat = "density", linewidth = 1.0) +  # 加粗线条
  labs(
    x = expression("TXge30 from SOS to GMO "(days)),  # x 轴标签
    y = NULL ) + # 移除 y 轴标题
  theme_minimal() +  # 使用 minimal 主题
  theme(
    legend.position = "none",  # 移除图例
    axis.text.x = element_text(size = 20),  # x 轴刻度字体大小
    axis.text.y = element_text(size = 20),  # y 轴刻度字体大小
    axis.title = element_text(size = 19),  # 坐标轴标题字体大小
    axis.title.y = element_blank(),  # 移除 y 轴标题
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),  # 设置主要网格线
    panel.grid.minor = element_blank(),  # 移除次要网格线（可选）
    axis.line = element_line(linewidth = 0.5, color = "black"),  # 添加坐标轴线
    axis.ticks = element_line(linewidth = 0.5, color = "black")) + # 添加刻度线
    # scale_y_continuous(
    #   limits = c(0.0, 0.2),  # 设置 y 轴范围
    #   breaks = seq(0.0,0.2, by = 0.1),  # 设置刻度间隔
    #   labels = scales::number_format(accuracy = 0.1))+
    scale_x_continuous(
      limits = c(0, 90),  # 设置 y 轴范围      
      breaks = seq(0,113, by = 30),  # 设置刻度间隔
      labels = scales::number_format(accuracy = 30))
print(p_TXge30_13)

#######  (2) TXge30_34   ########

p_TXge30_34 <- ggplot(df_TXge30_34, aes(x = Value, color = Climate, fill = Climate)) +
  geom_line(stat = "density", linewidth = 1.0) +  # 加粗线条
  labs(
    x = expression("TXge30 from GMO to GDO "(days)),  # x 轴标签
    y = NULL ) + # 移除 y 轴标题
  theme_minimal() +  # 使用 minimal 主题
  theme(
    legend.position = "none",  # 移除图例
    axis.text.x = element_text(size = 20),  # x 轴刻度字体大小
    axis.text.y = element_text(size = 20),  # y 轴刻度字体大小
    axis.title = element_text(size = 19),  # 坐标轴标题字体大小
    axis.title.y = element_blank(),  # 移除 y 轴标题
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),  # 设置主要网格线
    panel.grid.minor = element_blank(),  # 移除次要网格线（可选）
    axis.line = element_line(linewidth = 0.5, color = "black"),  # 添加坐标轴线
    axis.ticks = element_line(linewidth = 0.5, color = "black"))+  # 添加刻度线
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1))+  # 纵坐标保留 1 位小数
    # scale_y_continuous(
    #   limits = c(0.0, 0.2),  # 设置 y 轴范围
    #   breaks = seq(0.0,0.2, by = 0.1),  # 设置刻度间隔
    #   labels = scales::number_format(accuracy = 0.1))+
  scale_x_continuous(
    limits = c(0, 90),  # 设置 y 轴范围      Removed 206 rows
    breaks = seq(0,113, by = 30),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 30))
print(p_TXge30_34)

#######  (3) TXge30_46   ########

p_TXge30_46 <- ggplot(df_TXge30_46, aes(x = Value, color = Climate, fill = Climate)) +
  geom_line(stat = "density", linewidth = 1.0) +  # 加粗线条
  labs(
    x = expression("TXge30 from GDO to EOS "(degree~C)),  # x 轴标签
    y = NULL ) + # 移除 y 轴标题
  theme_minimal() +  # 使用 minimal 主题
  theme(
    legend.position = "none",  # 移除图例
    axis.text.x = element_text(size = 20),  # x 轴刻度字体大小
    axis.text.y = element_text(size = 20),  # y 轴刻度字体大小
    axis.title = element_text(size = 19),  # 坐标轴标题字体大小
    axis.title.y = element_blank(),  # 移除 y 轴标题
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),  # 设置主要网格线
    panel.grid.minor = element_blank(),  # 移除次要网格线（可选）
    axis.line = element_line(linewidth = 0.5, color = "black"),  # 添加坐标轴线
    axis.ticks = element_line(linewidth = 0.5, color = "black")) + # 添加刻度线
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) + # 纵坐标保留 1 位小数
    # scale_y_continuous(
    #   limits = c(0.0, 0.2),  # 设置 y 轴范围
    #   breaks = seq(0.0,0.2, by = 0.1),  # 设置刻度间隔
    #   labels = scales::number_format(accuracy = 0.1))+
    scale_x_continuous(
      limits = c(0, 90),  # 设置 y 轴范围         Removed 2 rows
      breaks = seq(0,92, by = 30),  # 设置刻度间隔
      labels = scales::number_format(accuracy = 30))
print(p_TXge30_46)

#######   1-3.TNn   ########

df_TNn_13 <- all_data_combined %>%
  filter(Variable == "TNn_SOS-GMO")

df_TNn_34 <- all_data_combined %>%
  filter(Variable == "TNn_GMO-GDO")

df_TNn_46 <- all_data_combined %>%
  filter(Variable == "TNn_GDO-EOS")

max_value <- max(df_TNn_13$Value, na.rm = TRUE)
min_value <- min(df_TNn_13$Value, na.rm = TRUE)
print(paste("最大值:", max_value))
print(paste("最小值:", min_value))
#-33~12    -11~23   -28~22

#######  (1) TXx_13   ########
p_TNn_13 <- ggplot(df_TNn_13, aes(x = Value, color = Climate, fill = Climate)) +
  geom_line(stat = "density", linewidth = 1.0) +  # 加粗线条
  labs(
    x = expression("TNn from SOS to GMO "(degree~C)),  # x 轴标签
    y = NULL ) + # 移除 y 轴标题
  theme_minimal() +  # 使用 minimal 主题
  theme(
    legend.position = "none",  # 移除图例
    axis.text.x = element_text(size = 20),  # x 轴刻度字体大小
    axis.text.y = element_text(size = 20),  # y 轴刻度字体大小
    axis.title = element_text(size = 19),  # 坐标轴标题字体大小
    axis.title.y = element_blank(),  # 移除 y 轴标题
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),  # 设置主要网格线
    panel.grid.minor = element_blank(),  # 移除次要网格线（可选）
    axis.line = element_line(linewidth = 0.5, color = "black"),  # 添加坐标轴线
    axis.ticks = element_line(linewidth = 0.5, color = "black")) + # 添加刻度线
  scale_y_continuous(
    limits = c(0.0, 0.3),  # 设置 y 轴范围
    breaks = seq(0.0,0.3, by = 0.1),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 0.1))+
  scale_x_continuous(
    limits = c(-35, 25),  # 设置 y 轴范围
    breaks = seq(-30,20, by = 10),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 10))  # 保留 1 位小数
print(p_TNn_13)

#######  (2) TNn_34   ########

p_TNn_34 <- ggplot(df_TNn_34, aes(x = Value, color = Climate, fill = Climate)) +
  geom_line(stat = "density", linewidth = 1.0) +  # 加粗线条
  labs(
    x = expression("TNn from GMO to GDO "(degree~C)),  # x 轴标签
    y = NULL ) + # 移除 y 轴标题
  theme_minimal() +  # 使用 minimal 主题
  theme(
    legend.position = "none",  # 移除图例
    axis.text.x = element_text(size = 20),  # x 轴刻度字体大小
    axis.text.y = element_text(size = 20),  # y 轴刻度字体大小
    axis.title = element_text(size = 19),  # 坐标轴标题字体大小
    axis.title.y = element_blank(),  # 移除 y 轴标题
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),  # 设置主要网格线
    panel.grid.minor = element_blank(),  # 移除次要网格线（可选）
    axis.line = element_line(linewidth = 0.5, color = "black"),  # 添加坐标轴线
    axis.ticks = element_line(linewidth = 0.5, color = "black")) + # 添加刻度线
  scale_y_continuous(
    limits = c(0.0, 0.3),  # 设置 y 轴范围
    breaks = seq(0.0,0.3, by = 0.1),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 0.1))+
  scale_x_continuous(
    limits = c(-35, 25),  # 设置 y 轴范围
    breaks = seq(-30,20, by = 10),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 10))  # 保留 1 位小数
print(p_TNn_34)

#######  (3) TNn_46   ########

p_TNn_46 <- ggplot(df_TNn_46, aes(x = Value, color = Climate, fill = Climate)) +
  geom_line(stat = "density", linewidth = 1.0) +  # 加粗线条
  labs(
    x = expression("TNn from GDO to EOS "(degree~C)),  # x 轴标签
    y = NULL ) + # 移除 y 轴标题
  theme_minimal() +  # 使用 minimal 主题
  theme(
    legend.position = "none",  # 移除图例
    axis.text.x = element_text(size = 20),  # x 轴刻度字体大小
    axis.text.y = element_text(size = 20),  # y 轴刻度字体大小
    axis.title = element_text(size = 19),  # 坐标轴标题字体大小
    axis.title.y = element_blank(),  # 移除 y 轴标题
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),  # 设置主要网格线
    panel.grid.minor = element_blank(),  # 移除次要网格线（可选）
    axis.line = element_line(linewidth = 0.5, color = "black"),  # 添加坐标轴线
    axis.ticks = element_line(linewidth = 0.5, color = "black")) + # 添加刻度线
  scale_y_continuous(
    limits = c(0.0, 0.3),  # 设置 y 轴范围
    breaks = seq(0.0,0.3, by = 0.1),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 0.1))+
  scale_x_continuous(
    limits = c(-35, 25),  # 设置 y 轴范围
    breaks = seq(-30,20, by = 10),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 10))  # 保留 1 位小数
print(p_TNn_46)

#######   1-4.TNlt2   ########

df_TNlt2_13 <- all_data_combined %>%
  filter(Variable == "TNlt2_SOS-GMO")

df_TNlt2_34 <- all_data_combined %>%
  filter(Variable == "TNlt2_GMO-GDO")

df_TNlt2_46 <- all_data_combined %>%
  filter(Variable == "TNlt2_GDO-EOS")

max_value <- max(df_TNlt2_46$Value, na.rm = TRUE)
min_value <- min(df_TNlt2_46$Value, na.rm = TRUE)
print(paste("最大值:", max_value))
print(paste("最小值:", min_value))
# 0-111  0-37  0-67

#######  (1) TNlt2_13   ########
p_TNlt2_13 <- ggplot(df_TNlt2_13, aes(x = Value, color = Climate, fill = Climate)) +
  geom_line(stat = "density", linewidth = 1.0) +  # 加粗线条
  labs(
    x = expression("TNlt2 from SOS to GMO "(days)),  # x 轴标签
    y = NULL ) + # 移除 y 轴标题
  theme_minimal() +  # 使用 minimal 主题
  theme(
    legend.position = "none",  # 移除图例
    axis.text.x = element_text(size = 20),  # x 轴刻度字体大小
    axis.text.y = element_text(size = 20),  # y 轴刻度字体大小
    axis.title = element_text(size = 19),  # 坐标轴标题字体大小
    axis.title.y = element_blank(),  # 移除 y 轴标题
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),  # 设置主要网格线
    panel.grid.minor = element_blank(),  # 移除次要网格线（可选）
    axis.line = element_line(linewidth = 0.5, color = "black"),  # 添加坐标轴线
    axis.ticks = element_line(linewidth = 0.5, color = "black"))+  # 添加刻度线
    # ylim(0.00, 10) +  # 设置 y 轴范围
    # xlim(0, 37)  # 设置 x 轴范围
    scale_y_continuous(
      limits = c(0.0, 1.0),  # 设置 y 轴范围
      breaks = seq(0.0,1.0, by = 0.2),  # 设置刻度间隔
      labels = scales::number_format(accuracy = 0.2))+
    scale_x_continuous(
      limits = c(0, 60),  # 设置 y 轴范围         Removed 324 rows
      breaks = seq(0,113, by = 20),  # 设置刻度间隔
      labels = scales::number_format(accuracy = 20))
print(p_TNlt2_13)

#######  (2) TNlt2_34   ########

p_TNlt2_34 <- ggplot(df_TNlt2_34, aes(x = Value, color = Climate, fill = Climate)) +
  geom_line(stat = "density", linewidth = 1.0) +  # 加粗线条
  labs(
    x = expression("TNlt2 from GMO to GDO "(days)),  # x 轴标签
    y = NULL ) + # 移除 y 轴标题
  theme_minimal() +  # 使用 minimal 主题
  theme(
    legend.position = "none",  # 移除图例
    axis.text.x = element_text(size = 20),  # x 轴刻度字体大小
    axis.text.y = element_text(size = 20),  # y 轴刻度字体大小
    axis.title = element_text(size = 19),  # 坐标轴标题字体大小
    axis.title.y = element_blank(),  # 移除 y 轴标题
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),  # 设置主要网格线
    panel.grid.minor = element_blank(),  # 移除次要网格线（可选）
    axis.line = element_line(linewidth = 0.5, color = "black"),  # 添加坐标轴线
    axis.ticks = element_line(linewidth = 0.5, color = "black"))+  # 添加刻度线
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1))+  # 纵坐标保留 1 位小数
    # ylim(0.00, 85.0) +  # 设置 y 轴范围
    # xlim(0, 111)  # 设置 x 轴范围
    # scale_y_continuous(
    #   limits = c(0.0, 0.2),  # 设置 y 轴范围
    #   breaks = seq(0.0,0.2, by = 0.1),  # 设置刻度间隔
    #   labels = scales::number_format(accuracy = 0.1))+
    scale_x_continuous(
      limits = c(0, 60),  # 设置 y 轴范围         Removed 324 rows
      breaks = seq(0,60, by = 20),  # 设置刻度间隔   #max=37
      labels = scales::number_format(accuracy = 20))
print(p_TNlt2_34)

#######  (3) TNlt2_46   ########

p_TNlt2_46 <- ggplot(df_TNlt2_46, aes(x = Value, color = Climate, fill = Climate)) +
  geom_line(stat = "density", linewidth = 1.0) +  # 加粗线条
  labs(
    x = expression("TNlt2 from GDO to EOS "(days)),  # x 轴标签
    y = NULL ) + # 移除 y 轴标题
  theme_minimal() +  # 使用 minimal 主题
  theme(
    legend.position = "none",  # 移除图例
    axis.text.x = element_text(size = 20),  # x 轴刻度字体大小
    axis.text.y = element_text(size = 20),  # y 轴刻度字体大小
    axis.title = element_text(size = 19),  # 坐标轴标题字体大小
    axis.title.y = element_blank(),  # 移除 y 轴标题
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),  # 设置主要网格线
    panel.grid.minor = element_blank(),  # 移除次要网格线（可选）
    axis.line = element_line(linewidth = 0.5, color = "black"),  # 添加坐标轴线
    axis.ticks = element_line(linewidth = 0.5, color = "black")) + # 添加刻度线
    scale_y_continuous(
      limits = c(0.0, 0.3),  # 设置 y 轴范围
      breaks = seq(0.0,0.3, by = 0.1),  # 设置刻度间隔
      labels = scales::number_format(accuracy = 0.1))+  # 保留 1 位小数  
    scale_x_continuous(
      limits = c(0, 60),  # 设置 y 轴范围         Removed 324 rows
      breaks = seq(0,67, by = 20),  # 设置刻度间隔
      labels = scales::number_format(accuracy = 20))
print(p_TNlt2_46)


#######   1-5.DTR   ########

df_DTR_13 <- all_data_combined %>%
  filter(Variable == "DTR_SOS-GMO")

df_DTR_34 <- all_data_combined %>%
  filter(Variable == "DTR_GMO-GDO")

df_DTR_46 <- all_data_combined %>%
  filter(Variable == "DTR_GDO-EOS")

max_value <- max(df_DTR_46$Value, na.rm = TRUE)
min_value <- min(df_DTR_46$Value, na.rm = TRUE)
print(paste("最大值:", max_value))
print(paste("最小值:", min_value))
#1-18    1~17   2~17

#######  (1) TXx_13   ########
p_DTR_13 <- ggplot(df_DTR_13, aes(x = Value, color = Climate, fill = Climate)) +
  geom_line(stat = "density", linewidth = 1.0) +  # 加粗线条
  labs(
    x = expression("DTR from SOS to GMO "(degree~C)),  # x 轴标签
    y = NULL ) + # 移除 y 轴标题
  theme_minimal() +  # 使用 minimal 主题
  theme(
    legend.position = "none",  # 移除图例
    axis.text.x = element_text(size = 20),  # x 轴刻度字体大小
    axis.text.y = element_text(size = 20),  # y 轴刻度字体大小
    axis.title = element_text(size = 19),  # 坐标轴标题字体大小
    axis.title.y = element_blank(),  # 移除 y 轴标题
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),  # 设置主要网格线
    panel.grid.minor = element_blank(),  # 移除次要网格线（可选）
    axis.line = element_line(linewidth = 0.5, color = "black"),  # 添加坐标轴线
    axis.ticks = element_line(linewidth = 0.5, color = "black")) + # 添加刻度线
  scale_y_continuous(
    limits = c(0.0, 0.55),  # 设置 y 轴范围
    breaks = seq(0.0,0.5, by = 0.1),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 0.1))+
  scale_x_continuous(
    limits = c(0, 20),  # 设置 y 轴范围
    breaks = seq(0,20, by = 5),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 5))  # 保留 1 位小数
print(p_DTR_13)

#######  (2) DTR_34   ########

p_DTR_34 <- ggplot(df_DTR_34, aes(x = Value, color = Climate, fill = Climate)) +
  geom_line(stat = "density", linewidth = 1.0) +  # 加粗线条
  labs(
    x = expression("DTR from GMO to GDO "(degree~C)),  # x 轴标签
    y = NULL ) + # 移除 y 轴标题
  theme_minimal() +  # 使用 minimal 主题
  theme(
    legend.position = "none",  # 移除图例
    axis.text.x = element_text(size = 20),  # x 轴刻度字体大小
    axis.text.y = element_text(size = 20),  # y 轴刻度字体大小
    axis.title = element_text(size = 19),  # 坐标轴标题字体大小
    axis.title.y = element_blank(),  # 移除 y 轴标题
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),  # 设置主要网格线
    panel.grid.minor = element_blank(),  # 移除次要网格线（可选）
    axis.line = element_line(linewidth = 0.5, color = "black"),  # 添加坐标轴线
    axis.ticks = element_line(linewidth = 0.5, color = "black")) + # 添加刻度线
  scale_y_continuous(
    limits = c(0.0, 0.55),  # 设置 y 轴范围
    breaks = seq(0.0,0.5, by = 0.1),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 0.1))+
  scale_x_continuous(
    limits = c(0, 20),  # 设置 y 轴范围
    breaks = seq(0,20, by = 5),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 5))  # 保留 1 位小数
print(p_DTR_34)

#######  (3) DTR_46   ########

p_DTR_46 <- ggplot(df_DTR_46, aes(x = Value, color = Climate, fill = Climate)) +
  geom_line(stat = "density", linewidth = 1.0) +  # 加粗线条
  labs(
    x = expression("DTR from GDO to EOS "(degree~C)),  # x 轴标签
    y = NULL ) + # 移除 y 轴标题
  theme_minimal() +  # 使用 minimal 主题
  theme(
    legend.position = "none",  # 移除图例
    axis.text.x = element_text(size = 20),  # x 轴刻度字体大小
    axis.text.y = element_text(size = 20),  # y 轴刻度字体大小
    axis.title = element_text(size = 19),  # 坐标轴标题字体大小
    axis.title.y = element_blank(),  # 移除 y 轴标题
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),  # 设置主要网格线
    panel.grid.minor = element_blank(),  # 移除次要网格线（可选）
    axis.line = element_line(linewidth = 0.5, color = "black"),  # 添加坐标轴线
    axis.ticks = element_line(linewidth = 0.5, color = "black")) + # 添加刻度线
  scale_y_continuous(
    limits = c(0.0, 0.55),  # 设置 y 轴范围
    breaks = seq(0.0,0.5, by = 0.1),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 0.1))+
  scale_x_continuous(
    limits = c(0, 20),  # 设置 y 轴范围
    breaks = seq(0,20, by = 5),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 5))  # 保留 1 位小数
print(p_DTR_46)




############################   Precipitation-related indice  ################################
#######   2-1.Raindays   ########

df_RainDay_13 <- all_data_combined %>%
  filter(Variable == "RainDay_SOS-GMO")

df_RainDay_34 <- all_data_combined %>%
  filter(Variable == "RainDay_GMO-GDO")

df_RainDay_46 <- all_data_combined %>%
  filter(Variable == "RainDay_GDO-EOS")

max_value <- max(df_RainDay_34$Value, na.rm = TRUE)
min_value <- min(df_RainDay_34$Value, na.rm = TRUE)
print(paste("最大值:", max_value))
print(paste("最小值:", min_value))
#-97    1~83   0-89

#######  (1) RainDay_13   ########
p_RainDay_13 <- ggplot(df_RainDay_13, aes(x = Value, color = Climate, fill = Climate)) +
  geom_line(stat = "density", linewidth = 1.0) +  # 加粗线条
  labs(
    x = expression("RainDay from SOS to GMO "(days)),  # x 轴标签
    y = NULL ) + # 移除 y 轴标题
  theme_minimal() +  # 使用 minimal 主题
  theme(
    legend.position = "none",  # 移除图例
    axis.text.x = element_text(size = 20),  # x 轴刻度字体大小
    axis.text.y = element_text(size = 20),  # y 轴刻度字体大小
    axis.title = element_text(size = 19),  # 坐标轴标题字体大小
    axis.title.y = element_blank(),  # 移除 y 轴标题
    axis.title.x = element_text(hjust = 0.65),  # 将 x 轴标题向右调整
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),  # 设置主要网格线
    panel.grid.minor = element_blank(),  # 移除次要网格线（可选）
    axis.line = element_line(linewidth = 0.5, color = "black"),  # 添加坐标轴线
    axis.ticks = element_line(linewidth = 0.5, color = "black")) + # 添加刻度线
  scale_y_continuous(
    limits = c(0.0, 0.08),  # 设置 y 轴范围
    breaks = seq(0.0,0.08, by = 0.02),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 0.02))+
  scale_x_continuous(
    limits = c(0, 100),  # 设置 y 轴范围
    breaks = seq(0,100, by = 25),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 25))  # 保留 1 位小数
print(p_RainDay_13)

#######  (2) RainDay_34   ########

p_RainDay_34 <- ggplot(df_RainDay_34, aes(x = Value, color = Climate, fill = Climate)) +
  geom_line(stat = "density", linewidth = 1.0) +  # 加粗线条
  labs(
    x = expression("RainDay from GMO to GDO "(days)),  # x 轴标签
    y = NULL ) + # 移除 y 轴标题
  theme_minimal() +  # 使用 minimal 主题
  theme(
    legend.position = "none",  # 移除图例
    axis.text.x = element_text(size = 20),  # x 轴刻度字体大小
    axis.text.y = element_text(size = 20),  # y 轴刻度字体大小
    axis.title = element_text(size = 19),  # 坐标轴标题字体大小
    axis.title.x = element_text(hjust = 0.65),  # 将 x 轴标题向右调整
    axis.title.y = element_blank(),  # 移除 y 轴标题
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),  # 设置主要网格线
    panel.grid.minor = element_blank(),  # 移除次要网格线（可选）
    axis.line = element_line(linewidth = 0.5, color = "black"),  # 添加坐标轴线
    axis.ticks = element_line(linewidth = 0.5, color = "black")) + # 添加刻度线
  scale_y_continuous(
    limits = c(0.0, 0.08),  # 设置 y 轴范围
    breaks = seq(0.0,0.08, by = 0.02),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 0.02))+
  scale_x_continuous(
    limits = c(0, 100),  # 设置 y 轴范围
    breaks = seq(0,100, by = 25),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 25))  # 保留 1 位小数
print(p_RainDay_34)

#######  (3) RainDay_46   ########

p_RainDay_46 <- ggplot(df_RainDay_46, aes(x = Value, color = Climate, fill = Climate)) +
  geom_line(stat = "density", linewidth = 1.0) +  # 加粗线条
  labs(
    x = expression("RainDay from GDO to EOS "(days)),  # x 轴标签
    y = NULL ) + # 移除 y 轴标题
  theme_minimal() +  # 使用 minimal 主题
  theme(
    legend.position = "none",  # 移除图例
    axis.text.x = element_text(size = 20),  # x 轴刻度字体大小
    axis.text.y = element_text(size = 20),  # y 轴刻度字体大小
    axis.title = element_text(size = 19),  # 坐标轴标题字体大小
    axis.title.y = element_blank(),  # 移除 y 轴标题
    axis.title.x = element_text(hjust = 0.65),  # 将 x 轴标题向右调整
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),  # 设置主要网格线
    panel.grid.minor = element_blank(),  # 移除次要网格线（可选）
    axis.line = element_line(linewidth = 0.5, color = "black"),  # 添加坐标轴线
    axis.ticks = element_line(linewidth = 0.5, color = "black")) + # 添加刻度线
  scale_y_continuous(
    limits = c(0.0, 0.08),  # 设置 y 轴范围
    breaks = seq(0.0,0.08, by = 0.02),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 0.02))+
  scale_x_continuous(
    limits = c(0, 100),  # 设置 y 轴范围
    breaks = seq(0,100, by = 25),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 25))  # 保留 1 位小数
print(p_RainDay_46)

#######   2-2.CWD   ########

df_CWD_13 <- all_data_combined %>%
  filter(Variable == "CWD_SOS-GMO")

df_CWD_34 <- all_data_combined %>%
  filter(Variable == "CWD_GMO-GDO")

df_CWD_46 <- all_data_combined %>%
  filter(Variable == "CWD_GDO-EOS")

max_value <- max(df_CWD_46$Value, na.rm = TRUE)
min_value <- min(df_CWD_46$Value, na.rm = TRUE)
print(paste("最大值:", max_value))
print(paste("最小值:", min_value))
# 0-56  0-73  0-60

#######  (1) CWD_13   ########
p_CWD_13 <- ggplot(df_CWD_13, aes(x = Value, color = Climate, fill = Climate)) +
  geom_line(stat = "density", linewidth = 1.0) +  # 加粗线条
  labs(
    x = expression("CWD from SOS to GMO "(days)),  # x 轴标签
    y = NULL ) + # 移除 y 轴标题
  theme_minimal() +  # 使用 minimal 主题
  theme(
    legend.position = "none",  # 移除图例
    axis.text.x = element_text(size = 20),  # x 轴刻度字体大小
    axis.text.y = element_text(size = 20),  # y 轴刻度字体大小
    axis.title = element_text(size = 19),  # 坐标轴标题字体大小
    axis.title.y = element_blank(),  # 移除 y 轴标题
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),  # 设置主要网格线
    panel.grid.minor = element_blank(),  # 移除次要网格线（可选）
    axis.line = element_line(linewidth = 0.5, color = "black"),  # 添加坐标轴线
    axis.ticks = element_line(linewidth = 0.5, color = "black"))+  # 添加刻度线
  scale_y_continuous(
    limits = c(0.0, 0.2),  # 设置 y 轴范围
    breaks = seq(0.0,0.2, by = 0.1),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 0.1))+
  # scale_x_continuous(
  #   limits = c(0, 60),  # 设置 y 轴范围
  #   breaks = seq(0,75, by = 20),  # 设置刻度间隔
  #   labels = scales::number_format(accuracy = 20))
  scale_x_continuous(
    limits = c(0, 50),  # 设置 y 轴范围       Removed 4 rows
    breaks = seq(0,75, by = 15),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 15))
print(p_CWD_13)

#######  (2) CWD_34   ########

p_CWD_34 <- ggplot(df_CWD_34, aes(x = Value, color = Climate, fill = Climate)) +
  geom_line(stat = "density", linewidth = 1.0) +  # 加粗线条
  labs(
    x = expression("CWD from GMO to GDO "(days)),  # x 轴标签
    y = NULL ) + # 移除 y 轴标题
  theme_minimal() +  # 使用 minimal 主题
  theme(
    legend.position = "none",  # 移除图例
    axis.text.x = element_text(size = 20),  # x 轴刻度字体大小
    axis.text.y = element_text(size = 20),  # y 轴刻度字体大小
    axis.title = element_text(size = 19),  # 坐标轴标题字体大小
    axis.title.y = element_blank(),  # 移除 y 轴标题
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),  # 设置主要网格线
    panel.grid.minor = element_blank(),  # 移除次要网格线（可选）
    axis.line = element_line(linewidth = 0.5, color = "black"),  # 添加坐标轴线
    axis.ticks = element_line(linewidth = 0.5, color = "black"))+  # 添加刻度线
  scale_y_continuous(
    limits = c(0.0, 0.2),  # 设置 y 轴范围
    breaks = seq(0.0,0.2, by = 0.1),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 0.1))+
  # scale_x_continuous(
  #   limits = c(0, 60),  # 设置 y 轴范围
  #   breaks = seq(0,75, by = 20),  # 设置刻度间隔   #Removed 18 rows 
  #   labels = scales::number_format(accuracy = 20))
  scale_x_continuous(
    limits = c(0, 50),  # 设置 y 轴范围       Removed 41 rows 
    breaks = seq(0,75, by = 15),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 15))  
print(p_CWD_34)

#######  (3) CWD_46   ########

p_CWD_46 <- ggplot(df_CWD_46, aes(x = Value, color = Climate, fill = Climate)) +
  geom_line(stat = "density", linewidth = 1.0) +  # 加粗线条
  labs(
    x = expression("CWD from GDO to EOS "(days)),  # x 轴标签
    y = NULL ) + # 移除 y 轴标题
  theme_minimal() +  # 使用 minimal 主题
  theme(
    legend.position = "none",  # 移除图例
    axis.text.x = element_text(size = 20),  # x 轴刻度字体大小
    axis.text.y = element_text(size = 20),  # y 轴刻度字体大小
    axis.title = element_text(size = 19),  # 坐标轴标题字体大小
    axis.title.y = element_blank(),  # 移除 y 轴标题
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),  # 设置主要网格线
    panel.grid.minor = element_blank(),  # 移除次要网格线（可选）
    axis.line = element_line(linewidth = 0.5, color = "black"),  # 添加坐标轴线
    axis.ticks = element_line(linewidth = 0.5, color = "black")) + # 添加刻度线
  scale_y_continuous(
    limits = c(0.0, 0.2),  # 设置 y 轴范围
    breaks = seq(0.0,0.2, by = 0.1),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 0.1))+
  # scale_x_continuous(
  #   limits = c(0, 60),  # 设置 y 轴范围
  #   breaks = seq(0,75, by = 20),  # 设置刻度间隔
  #   labels = scales::number_format(accuracy = 20))
  scale_x_continuous(
    limits = c(0, 50),  # 设置 y 轴范围       Removed 3 rows 
    breaks = seq(0,75, by = 15),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 15)) 
print(p_CWD_46)

#######   2-3.CDD   ########

df_CDD_13 <- all_data_combined %>%
  filter(Variable == "CDD_SOS-GMO")

df_CDD_34 <- all_data_combined %>%
  filter(Variable == "CDD_GMO-GDO")

df_CDD_46 <- all_data_combined %>%
  filter(Variable == "CDD_GDO-EOS")

max_value <- max(df_CDD_34$Value, na.rm = TRUE)
min_value <- min(df_CDD_34$Value, na.rm = TRUE)
print(paste("最大值:", max_value))
print(paste("最小值:", min_value))
# 0-44  0-52  0-71

#######  (1) CDD_13   ########
p_CDD_13 <- ggplot(df_CDD_13, aes(x = Value, color = Climate, fill = Climate)) +
  geom_line(stat = "density", linewidth = 1.0) +  # 加粗线条
  labs(
    x = expression("CDD from SOS to GMO "(days)),  # x 轴标签
    y = NULL ) + # 移除 y 轴标题
  theme_minimal() +  # 使用 minimal 主题
  theme(
    legend.position = "none",  # 移除图例
    axis.text.x = element_text(size = 20),  # x 轴刻度字体大小
    axis.text.y = element_text(size = 20),  # y 轴刻度字体大小
    axis.title = element_text(size = 19),  # 坐标轴标题字体大小
    axis.title.y = element_blank(),  # 移除 y 轴标题
    axis.title.x = element_text(hjust = 0.56),  # 将 x 轴标题向右调整
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),  # 设置主要网格线
    panel.grid.minor = element_blank(),  # 移除次要网格线（可选）
    axis.line = element_line(linewidth = 0.5, color = "black"),  # 添加坐标轴线
    axis.ticks = element_line(linewidth = 0.5, color = "black"))+  # 添加刻度线
  scale_y_continuous(
    limits = c(0.0, 0.2),  # 设置 y 轴范围
    breaks = seq(0.0,0.2, by = 0.1),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 0.1))+
  # scale_x_continuous(
  #   limits = c(0, 60),  # 设置 y 轴范围
  #   breaks = seq(0,75, by = 20),  # 设置刻度间隔
  #   labels = scales::number_format(accuracy = 20))
  scale_x_continuous(
    limits = c(0, 50),  # 设置 y 轴范围
    breaks = seq(0,75, by = 15),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 15))
print(p_CDD_13)

#######  (2) CDD_34   ########

p_CDD_34 <- ggplot(df_CDD_34, aes(x = Value, color = Climate, fill = Climate)) +
  geom_line(stat = "density", linewidth = 1.0) +  # 加粗线条
  labs(
    x = expression("CDD from GMO to GDO "(days)),  # x 轴标签
    y = NULL ) + # 移除 y 轴标题
  theme_minimal() +  # 使用 minimal 主题
  theme(
    legend.position = "none",  # 移除图例
    axis.text.x = element_text(size = 20),  # x 轴刻度字体大小
    axis.text.y = element_text(size = 20),  # y 轴刻度字体大小
    axis.title = element_text(size = 19),  # 坐标轴标题字体大小
    axis.title.y = element_blank(),  # 移除 y 轴标题
    axis.title.x = element_text(hjust = 0.56),  # 将 x 轴标题向右调整
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),  # 设置主要网格线
    panel.grid.minor = element_blank(),  # 移除次要网格线（可选）
    axis.line = element_line(linewidth = 0.5, color = "black"),  # 添加坐标轴线
    axis.ticks = element_line(linewidth = 0.5, color = "black"))+  # 添加刻度线
  scale_y_continuous(
    limits = c(0.0, 0.2),  # 设置 y 轴范围
    breaks = seq(0.0,0.2, by = 0.1),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 0.1))+
  # scale_x_continuous(
  #   limits = c(0, 60),  # 设置 y 轴范围
  #   breaks = seq(0,75, by = 20),  # 设置刻度间隔
  #   labels = scales::number_format(accuracy = 20))
  scale_x_continuous(
    limits = c(0, 50),  # 设置 y 轴范围     #Removed 4 rows
    breaks = seq(0,75, by = 15),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 15))
print(p_CDD_34)

#######  (3) CDD_46   ########

p_CDD_46 <- ggplot(df_CDD_46, aes(x = Value, color = Climate, fill = Climate)) +
  geom_line(stat = "density", linewidth = 1.0) +  # 加粗线条
  labs(
    x = expression("CDD from GDO to EOS "(days)),  # x 轴标签
    y = NULL ) + # 移除 y 轴标题
  theme_minimal() +  # 使用 minimal 主题
  theme(
    legend.position = "none",  # 移除图例
    axis.text.x = element_text(size = 20),  # x 轴刻度字体大小
    axis.text.y = element_text(size = 20),  # y 轴刻度字体大小
    axis.title = element_text(size = 19),  # 坐标轴标题字体大小
    axis.title.y = element_blank(),  # 移除 y 轴标题
    axis.title.x = element_text(hjust = 0.56),  # 将 x 轴标题向右调整
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),  # 设置主要网格线
    panel.grid.minor = element_blank(),  # 移除次要网格线（可选）
    axis.line = element_line(linewidth = 0.5, color = "black"),  # 添加坐标轴线
    axis.ticks = element_line(linewidth = 0.5, color = "black")) + # 添加刻度线
  scale_y_continuous(
    limits = c(0.0, 0.2),  # 设置 y 轴范围
    breaks = seq(0.0,0.2, by = 0.1),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 0.1))+
  # scale_x_continuous(
  #   limits = c(0, 60),  # 设置 y 轴范围
  #   breaks = seq(0,75, by = 20),  # 设置刻度间隔
  #   labels = scales::number_format(accuracy = 20))
  scale_x_continuous(
    limits = c(0, 50),  # 设置 y 轴范围        Removed 47 rows
    breaks = seq(0,75, by = 15),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 15))
print(p_CDD_46)

#######   2-4.Rx1day   ########

df_Rx1day_13 <- all_data_combined %>%
  filter(Variable == "Rx1day_SOS-GMO")

df_Rx1day_34 <- all_data_combined %>%
  filter(Variable == "Rx1day_GMO-GDO")

df_Rx1day_46 <- all_data_combined %>%
  filter(Variable == "Rx1day_GDO-EOS")

max_value <- max(df_Rx1day_34$Value, na.rm = TRUE)
min_value <- min(df_Rx1day_34$Value, na.rm = TRUE)
print(paste("最大值:", max_value))
print(paste("最小值:", min_value))
# 0-188  0-276  0-365

#######  (1) Rx1day_13   ########
p_Rx1day_13 <- ggplot(df_Rx1day_13, aes(x = Value, color = Climate, fill = Climate)) +
  geom_line(stat = "density", linewidth = 1.0) +  # 加粗线条
  labs(
    x = expression("Rx1day from SOS to GMO "(mm)),  # x 轴标签
    y = NULL ) + # 移除 y 轴标题
  theme_minimal() +  # 使用 minimal 主题
  theme(
    legend.position = "none",  # 移除图例
    axis.text.x = element_text(size = 20),  # x 轴刻度字体大小
    axis.text.y = element_text(size = 20),  # y 轴刻度字体大小
    axis.title = element_text(size = 19),  # 坐标轴标题字体大小
    axis.title.y = element_blank(),  # 移除 y 轴标题
    axis.title.x = element_text(hjust = 0.8),  # 将 x 轴标题向右调整
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),  # 设置主要网格线
    panel.grid.minor = element_blank(),  # 移除次要网格线（可选）
    axis.line = element_line(linewidth = 0.5, color = "black"),  # 添加坐标轴线
    axis.ticks = element_line(linewidth = 0.5, color = "black"))+  # 添加刻度线
  scale_y_continuous(
    limits = c(0.0, 0.08),  # 设置 y 轴范围
    breaks = seq(0.0,0.08, by = 0.02),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 0.02))+
  scale_x_continuous(
    limits = c(0, 200),  # 设置 y 轴范围     
    breaks = seq(0,300, by = 50),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 50))
print(p_Rx1day_13)

#######  (2) Rx1day_34   ########

p_Rx1day_34 <- ggplot(df_Rx1day_34, aes(x = Value, color = Climate, fill = Climate)) +
  geom_line(stat = "density", linewidth = 1.0) +  # 加粗线条
  labs(
    x = expression("Rx1day from GMO to GDO "(mm)),  # x 轴标签
    y = NULL ) + # 移除 y 轴标题
  theme_minimal() +  # 使用 minimal 主题
  theme(
    legend.position = "none",  # 移除图例
    axis.text.x = element_text(size = 20),  # x 轴刻度字体大小
    axis.text.y = element_text(size = 20),  # y 轴刻度字体大小
    axis.title = element_text(size = 19),  # 坐标轴标题字体大小
    axis.title.y = element_blank(),  # 移除 y 轴标题
    axis.title.x = element_text(hjust = 0.8),  # 将 x 轴标题向右调整
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),  # 设置主要网格线
    panel.grid.minor = element_blank(),  # 移除次要网格线（可选）
    axis.line = element_line(linewidth = 0.5, color = "black"),  # 添加坐标轴线
    axis.ticks = element_line(linewidth = 0.5, color = "black"))+  # 添加刻度线
  scale_y_continuous(
    limits = c(0.0, 0.08),  # 设置 y 轴范围
    breaks = seq(0.0,0.08, by = 0.02),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 0.02))+
  scale_x_continuous(
    limits = c(0, 200),  # 设置 y 轴范围    # Removed 52 rows 
    breaks = seq(0,300, by = 50),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 50))
print(p_Rx1day_34)

#######  (3) Rx1day_46   ########

p_Rx1day_46 <- ggplot(df_Rx1day_46, aes(x = Value, color = Climate, fill = Climate)) +
  geom_line(stat = "density", linewidth = 1.0) +  # 加粗线条
  labs(
    x = expression("Rx1day from GDO to EOS "(mm)),  # x 轴标签
    y = NULL ) + # 移除 y 轴标题
  theme_minimal() +  # 使用 minimal 主题
  theme(
    legend.position = "none",  # 移除图例
    axis.text.x = element_text(size = 20),  # x 轴刻度字体大小
    axis.text.y = element_text(size = 20),  # y 轴刻度字体大小
    axis.title = element_text(size = 19),  # 坐标轴标题字体大小
    axis.title.y = element_blank(),  # 移除 y 轴标题
    axis.title.x = element_text(hjust = 0.7),  # 将 x 轴标题向右调整
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),  # 设置主要网格线
    panel.grid.minor = element_blank(),  # 移除次要网格线（可选）
    axis.line = element_line(linewidth = 0.5, color = "black"),  # 添加坐标轴线
    axis.ticks = element_line(linewidth = 0.5, color = "black")) + # 添加刻度线
  scale_y_continuous(
    limits = c(0.0, 0.08),  # 设置 y 轴范围
    breaks = seq(0.0,0.08, by = 0.02),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 0.02))+
  scale_x_continuous(
    limits = c(0, 200),  # 设置 y 轴范围    # 250--Removed 24 row比例很小OK  280--Removed 10 rows
    breaks = seq(0,300, by = 50),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 50))
print(p_Rx1day_46)


#######   2-5.SDII   ########

df_SDII_13 <- all_data_combined %>%
  filter(Variable == "SDII_SOS-GMO")

df_SDII_34 <- all_data_combined %>%
  filter(Variable == "SDII_GMO-GDO")

df_SDII_46 <- all_data_combined %>%
  filter(Variable == "SDII_GDO-EOS")

max_value <- max(df_SDII_34$Value, na.rm = TRUE)
min_value <- min(df_SDII_34$Value, na.rm = TRUE)
print(paste("最大值:", max_value))
print(paste("最小值:", min_value))
# 0-33  0-32  0-33

#######  (1) SDII_13   ########
p_SDII_13 <- ggplot(df_SDII_13, aes(x = Value, color = Climate, fill = Climate)) +
  geom_line(stat = "density", linewidth = 1.0) +  # 加粗线条
  labs(
    x = expression("SDII from SOS to GMO "(mm/day)),  # x 轴标签
    y = NULL ) + # 移除 y 轴标题
  theme_minimal() +  # 使用 minimal 主题
  theme(
    legend.position = "none",  # 移除图例
    axis.text.x = element_text(size = 20),  # x 轴刻度字体大小
    axis.text.y = element_text(size = 20),  # y 轴刻度字体大小
    axis.title = element_text(size = 19),  # 坐标轴标题字体大小
    axis.title.y = element_blank(),  # 移除 y 轴标题
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),  # 设置主要网格线
    panel.grid.minor = element_blank(),  # 移除次要网格线（可选）
    axis.line = element_line(linewidth = 0.5, color = "black"),  # 添加坐标轴线
    axis.ticks = element_line(linewidth = 0.5, color = "black"))+  # 添加刻度线
  scale_y_continuous(
    limits = c(0.0, 0.3),  # 设置 y 轴范围
    breaks = seq(0.0,0.3, by = 0.1),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 0.1))+
  scale_x_continuous(
    limits = c(0, 33),  # 设置 y 轴范围
    breaks = seq(0,30, by = 10),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 10))
print(p_SDII_13)

#######  (2) SDII_34   ########

p_SDII_34 <- ggplot(df_SDII_34, aes(x = Value, color = Climate, fill = Climate)) +
  geom_line(stat = "density", linewidth = 1.0) +  # 加粗线条
  labs(
    x = expression("SDII from GMO to GDO "(mm/day)),  # x 轴标签
    y = NULL ) + # 移除 y 轴标题
  theme_minimal() +  # 使用 minimal 主题
  theme(
    legend.position = "none",  # 移除图例
    axis.text.x = element_text(size = 20),  # x 轴刻度字体大小
    axis.text.y = element_text(size = 20),  # y 轴刻度字体大小
    axis.title = element_text(size = 19),  # 坐标轴标题字体大小
    axis.title.y = element_blank(),  # 移除 y 轴标题
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),  # 设置主要网格线
    panel.grid.minor = element_blank(),  # 移除次要网格线（可选）
    axis.line = element_line(linewidth = 0.5, color = "black"),  # 添加坐标轴线
    axis.ticks = element_line(linewidth = 0.5, color = "black"))+  # 添加刻度线
  scale_y_continuous(
    limits = c(0.0, 0.3),  # 设置 y 轴范围
    breaks = seq(0.0,0.3, by = 0.1),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 0.1))+
  scale_x_continuous(
    limits = c(0, 33),  # 设置 y 轴范围
    breaks = seq(0,30, by = 10),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 10))
print(p_SDII_34)

#######  (3) SDII_46   ########

p_SDII_46 <- ggplot(df_SDII_46, aes(x = Value, color = Climate, fill = Climate)) +
  geom_line(stat = "density", linewidth = 1.0) +  # 加粗线条
  labs(
    x = expression("SDII from GDO to EOS "(mm/day)),  # x 轴标签
    y = NULL ) + # 移除 y 轴标题
  theme_minimal() +  # 使用 minimal 主题
  theme(
    legend.position = "none",  # 移除图例
    axis.text.x = element_text(size = 20),  # x 轴刻度字体大小
    axis.text.y = element_text(size = 20),  # y 轴刻度字体大小
    axis.title = element_text(size = 19),  # 坐标轴标题字体大小
    axis.title.y = element_blank(),  # 移除 y 轴标题
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),  # 设置主要网格线
    panel.grid.minor = element_blank(),  # 移除次要网格线（可选）
    axis.line = element_line(linewidth = 0.5, color = "black"),  # 添加坐标轴线
    axis.ticks = element_line(linewidth = 0.5, color = "black")) + # 添加刻度线
  scale_y_continuous(
    limits = c(0.0, 0.3),  # 设置 y 轴范围
    breaks = seq(0.0,0.3, by = 0.1),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 0.1))+
  scale_x_continuous(
    limits = c(0, 33),  # 设置 y 轴范围
    breaks = seq(0,30, by = 10),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 10))
print(p_SDII_46)






############################   30张图片排列保存  ################################
library(grid)
library(gridExtra)
library(ggplot2)
library(ggpubr)


####  #### 创建标题1、组合图形、保存Fig.a. Temperature-based indices  ####  ####  ####

# title.grob_1 <- textGrob(
#   label = "a. Temperature-based indices", x = unit(0, "lines"), y = unit(0, "lines"),
#   hjust = 0, vjust = 0, gp = gpar(fontsize = 20.5))

g1 <- ggarrange(p_TXx_13, p_TXge30_13, p_TNn_13, p_TNlt2_13, p_DTR_13,
                p_TXx_34, p_TXge30_34, p_TNn_34, p_TNlt2_34, p_DTR_34,
                p_TXx_46, p_TXge30_46, p_TNn_46, p_TNlt2_46, p_DTR_46,
                nrow = 3, ncol = 5, common.legend = TRUE, legend = "none",
                widths = c(1.3, 1.3, 1.3, 1.3, 1.3),  # 调整横向间距
                heights = c(1.2, 1.2, 1.2))  # 调整纵向间距)

g1 <- annotate_figure(g1, left = textGrob("Density", rot = 90, vjust = 1,
                                          x = unit(1, "npc") - unit(0.6, "cm"),  # 调整 x 位置
                                      gp = gpar(fontsize = 22)))
plot(g1)

ggsave(filename = "./0.figure/03 Paper/Fig.5-a-CEI_T_DensityPlot.tiff", 
       plot = g1, width = 20, height = 8, dpi = 300)


#### #### 创建标题2、组合图形、保存Fig.b. Precipitation-based indices  ####  ####  ####


# 设置全局图例
p_SDII_46_legend <- ggplot(df_SDII_46, aes(x = Value, color = Climate)) +  # 只保留 color 映射
  geom_line(stat = "density", linewidth = 1.0) +  # 加粗线条
  labs(color = "Climate type") +  # 只设置 color 的图例标题
  guides(color = guide_legend(nrow = 1)) +  # 图例排列为一行
  theme(
    legend.text = element_text(size = 25),  # 图例文字大小
    legend.title = element_text(size = 25),  # 图例标题大小
    legend.position = "bottom",  # 图例放在最下方
    legend.key = element_blank(),  # 去掉图例的填充背景
    legend.key.width = unit(2, "lines")  # 调整图例键的宽度
  )

# 提取全局图例
get_legend <- function(myggplot) {
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  if (length(leg) > 0) {
    legend <- tmp$grobs[[leg]]
    return(legend)
  } else {
    warning("No legend found in the plot.")
    return(NULL)
  }
}

legend <- get_legend(p_SDII_46_legend)

# 组合图形并应用全局图例
g2 <- ggarrange(p_RainDay_13, p_CWD_13, p_CDD_13, p_Rx1day_13, p_SDII_13,
                p_RainDay_34, p_CWD_34, p_CDD_34, p_Rx1day_34, p_SDII_34,
                p_RainDay_46, p_CWD_46, p_CDD_46, p_Rx1day_46, p_SDII_46,
                nrow = 3, ncol = 5, 
                widths = c(1.2, 1.2, 1.2, 1.2, 1.2),  # 调整横向间距
                heights = c(1.2, 1.2, 1.2))  # 调整纵向间距)

# 添加全局 ylab 和图例
g2 <- annotate_figure(g2, left = textGrob("Density", rot = 90, vjust = 1.0,
                                          x = unit(1, "npc") - unit(0.8, "cm"),  # 调整 x 位置
                      gp = gpar(fontsize = 25)),bottom = legend)  # 将图例放在最下方

# 保存图形
ggsave(filename = "./0.figure/03 Paper/Fig.5-b-CEI_P_DensityPlot.tiff", 
       plot = g2, width = 20, height =10, dpi = 300)

