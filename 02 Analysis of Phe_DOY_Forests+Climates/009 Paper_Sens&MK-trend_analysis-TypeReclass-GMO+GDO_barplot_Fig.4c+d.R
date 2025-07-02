rm(list = ls())

###### 0. 加载包 ####
library(terra)
library(tidyverse)
library(raster)
library(ggplot2)
library(ggpubr)


setwd("D:/Graduation_Thesis")

###############################################################################################
############################# 000. 查看不同气候区、趋势的概率密度差异  ########################
###############################################################################################

############################# 000.按照柯本气候类型，将气候区分为6个气候区域   #################
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


############################### 001 按三级气候分区作图  GDO   #################################  



# # SOS: phe1_DOY ;  MGP: phe2_DOY ;  GMO：phe3_DOY
# # GDO: phe4_DOY ;  MSP: phe5_DOY ;  EOS：phe6_DOY

## 1. 读取数据
# 列出文件夹中的所有文件
r2 <-  rast("./EA+NA_Results/EA+NA_GFC_30km.tif")
r1_files <- list.files("./EA+NA_Results/merged_Phe_DOY_years_2_PHE_analysis/merged_phe4_DOY/", 
                       full.names = TRUE)
# 加载所有栅格文件并处理为整数
r1_list <- lapply(r1_files, function(file) {
  r <- rast(file)  # 加载栅格文件
  app(r, fun = function(x) as.integer(round(x)))  # 转换为整数
})
# 将所有处理后的栅格合并为一个栅格栈
r1 <- do.call(c, r1_list)
# 设置图层名称为年份
file_names <- basename(r1_files)
years <- substr(file_names, nchar(file_names) - 7, nchar(file_names) - 4)
names(r1) <- years
r1

# 2.定义气候类型和森林类型；定义年份序列
climate_types <- c("Cfa", "Cfb", "Dfb", "Dfc", "Dwb", "Dwc")
forest_types <- c(1, 2, 3)  # 1: 再生林, 2:原生林, 3: 人工林
years <- 2013:2022

# 3.定义趋势分析函数
library(trend)
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
  return(c(Zs, slope, MK_test))
}


# 创建空列表，用于存储趋势结果
trend_rasters <- list()

# 4.遍历每种气候类型、森林类型
for (climate in climate_types) {
  # 提取气候区域的 DOY 栅格
  doy_stack <- do.call(c, lapply(1:nlyr(r1), function(i) {
    mask(r1[[i]], classify_border[classify_border$EA_koppen_30km_addClimate %in% climate, ], touches = FALSE)
  }))
  
  # 提取气候区域的森林类型
  forest_layer <- mask(r2, classify_border[classify_border$EA_koppen_30km_addClimate %in% climate, ])
  
  # 遍历每种森林类型
  for (forest in forest_types) {
    # 根据林类值掩膜
    forest_doy_stack <- mask(doy_stack, ifel(forest_layer == forest, 1, NA))
    
    # 对每个像元应用 Sen's Slope 和 Mann-Kendall 趋势分析
    trend_raster <- app(forest_doy_stack, fun_sen,cores=6  ) # 使用你定义的 fun_sen 函数
    
    # 将趋势结果保存到列表中
    trend_rasters[[paste0("trend_raster_", climate, "_forest", forest)]] <- trend_raster
  }
}

# 5.提取栅格数据并合并为数据框
all_data <- do.call(rbind, lapply(names(trend_rasters), function(name) {
  raster_df <- as.data.frame(trend_rasters[[name]], xy = TRUE)
  colnames(raster_df) <- c("Longitude", "Latitude", "Z", "slope", "p_value")  # 确保列名与返回的值匹配
  
  # 提取气候和森林信息
  parts <- strsplit(name, "_")[[1]]
  raster_df$Climate <- parts[3]
  raster_df$ForestType <- parts[4]
  return(raster_df)
}))

all_data$ForestType <- factor(all_data$ForestType, 
                              levels = c("forest1", "forest2", "forest3"),
                              labels = c( "NRF","PF", "P"))

summary(all_data)
write.csv(all_data, file = "./0.figure/03 Paper/01 Sens+MK_trend_analysis_GDO.csv", row.names = FALSE)

################# 002 绘图 C.precentage_barplot_GDO,按气候区、林地类型分类计算  ####################


library(dplyr)
all_data <- read.csv("./0.figure/03 Paper/01 Sens+MK_trend_analysis_GDO.csv")

all_data <- all_data %>%
  mutate(
    # 根据slope划分为三级
    slope_class = case_when(
      slope > 0.5 ~ 1,        # 延迟
      slope < -0.5 ~ -1,      # 提前
      TRUE ~ 0                # 稳定
    ),
    # 根据Z统计量划分为显著和不显著
    z_class = case_when(
      abs(Z) > 1.96 ~ 2,      # 显著
      abs(Z) <= 1.96 ~ 1      # 不显著
    ),
    # 计算class的最终值，slope_class * z_class
    class = slope_class * z_class
  )

# 查看结果
head(all_data)

# Longitude Latitude         Z     slope   p_value Climate ForestType slope_class z_class class
# 103640  26.81471 49.45226 0.6286186 0.6000000 0.5295988     Cfa        NRF           1       1     1
# 104977  27.08421 49.18276 1.1801937 1.1666667 0.2379232     Cfa        NRF           1       1     1
# 104978  27.35370 49.18276 0.0000000 0.0000000 1.0000000     Cfa        NRF           0       1     0
# 106313  27.08421 48.91327 0.6286186 0.3750000 0.5295988     Cfa        NRF           0       1     0
# 106314  27.35370 48.91327 1.2623375 0.6666667 0.2068274     Cfa        NRF           1       1     1
# 106315  27.62319 48.91327 0.0000000 0.1111111 1.0000000     Cfa        NRF           0       1     0


################# 绘图 C.precentage_barplot_GDO,按气候区、林地类型分类计算  ####################

# 计算每个 Climate 和 ForestType 组合中 class 的占比
df_summary <- all_data %>%
  filter(class != 0) %>%  # 删除 class == 0 的数据
  group_by(Climate, ForestType, class) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Climate, ForestType) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  mutate(percentage = ifelse(class %in% c("-2", "-1"), -percentage, percentage))  # 负值表示提前

# 创建映射表
# class_labels <- c("-2" = "Significant advance",
#                   "-1" = "Nonsignificant advance",
#                   "1"  = "Nonsignificant delay",
#                   "2"  = "Significant delay")

df_summary$ForestType <- factor(df_summary$ForestType, levels = c("PF", "NRF", "P"))
df_summary$class <- factor(df_summary$class, levels = c( "1","2", "-1","-2"))
# df_summary$climate <- factor(df_summary$Climate, levels = c("Dwc", "Dwb", "Dfc", "Dfb", "Cfb", "Cfa"))
df_summary$climate <- factor(df_summary$Climate, levels = c("Cfa", "Cfb", "Dfb", "Dfc", "Dwb", "Dwc"))

# 作图  x取反 percentage → 负值变正、正值变负，改变柱子的位置方向。
p3 <- ggplot(df_summary, aes(x = climate, y = -percentage, fill = factor(class))) +  
  geom_bar(stat = "identity", position = "stack", width = 0.7, color = "black") +  # 堆叠显示
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.6) +  # 改为水平参考线
  # geom_text(aes(label = sprintf("%.1f%%", abs(percentage)), hjust = ifelse(percentage > 0, -0.1, 1.1)),
  #           position = position_stack(vjust = 0.5), size = 3, color = "black") +  # 文字放在柱状图的前方
  facet_grid(. ~ ForestType, switch = "y") +  # Facet 按 ForestType 分组
  scale_fill_manual(values = c( "1" = "lightblue", "2" = "#006699","-1" = "#fc8d59", "-2" = "#CC3333"), 
                    name = "Gradation of Phenological Trends",
                    labels = class_labels) +  # 自定义图例标签
  labs(x = "Climate type", y = "Percentage (%)") +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_rect(size = 0.6),
    axis.text.y = element_text(size = 16),
    axis.ticks.y = element_line(size = 0.6),
    axis.ticks.length.y = unit(0.1, "cm"),
    axis.title.y = element_text(size = 18),  #margin = margin(r = 10)
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1, vjust = 1),  # 角度设为30度
    axis.title.x = element_text(size = 18),
    axis.ticks.x = element_line(size = 0.6),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"),
    legend.position = "none",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    strip.text.y = element_text(size = 16, angle = 0),  # 右对齐 Facet 标签
    # strip.text.y = element_blank(),  # 不显示Facet 标签
    strip.placement = "outside",  # 放置 Facet 标签在外侧
    # strip.background = element_blank()  # 移除 Facet 背景
    strip.text.x = element_text(size = 16),  # 分面标签加粗并增加下边距,  margin = margin(b = 5)
    panel.spacing = unit(0.2, "cm"),  # 调整分面间距
    strip.background = element_rect(fill = "gray90", color = "black")
  ) +
  scale_y_continuous(limits = c(-100, 100), breaks = seq(-100, 100, by = 50), labels = abs)

# 打印图形
print(p3)

ggsave(
  filename = "./0.figure/03 Paper/Fig.4-c.Reclassified_fancetplot_GDO.tiff",
  plot = p3, width = 16,  height = 12,  units = "cm",  dpi = 300)


############################### 001 按三级气候分区作图  EOS   #################################  



# # SOS: phe1_DOY ;  MGP: phe2_DOY ;  GMO：phe3_DOY
# # GDO: phe4_DOY ;  MSP: phe5_DOY ;  EOS：phe6_DOY

## 1. 读取数据
# 列出文件夹中的所有文件
r2 <-  rast("./EA+NA_Results/EA+NA_GFC_30km.tif")
r1_files <- list.files("./EA+NA_Results/merged_Phe_DOY_years_2_PHE_analysis/merged_phe6_DOY/", 
                       full.names = TRUE)
# 加载所有栅格文件并处理为整数
r1_list <- lapply(r1_files, function(file) {
  r <- rast(file)  # 加载栅格文件
  app(r, fun = function(x) as.integer(round(x)))  # 转换为整数
})
# 将所有处理后的栅格合并为一个栅格栈
r1 <- do.call(c, r1_list)
# 设置图层名称为年份
file_names <- basename(r1_files)
years <- substr(file_names, nchar(file_names) - 7, nchar(file_names) - 4)
names(r1) <- years
r1

# 2.定义气候类型和森林类型；定义年份序列
climate_types <- c("Cfa", "Cfb", "Dfb", "Dfc", "Dwb", "Dwc")
forest_types <- c(1, 2, 3)  # 1: 再生林, 2:原生林, 3: 人工林
years <- 2013:2022

# 3.定义趋势分析函数
library(trend)
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
  return(c(Zs, slope, MK_test))
}


# 创建空列表，用于存储趋势结果
trend_rasters <- list()

# 4.遍历每种气候类型、森林类型
for (climate in climate_types) {
  # 提取气候区域的 DOY 栅格
  doy_stack <- do.call(c, lapply(1:nlyr(r1), function(i) {
    mask(r1[[i]], classify_border[classify_border$EA_koppen_30km_addClimate %in% climate, ], touches = FALSE)
  }))
  
  # 提取气候区域的森林类型
  forest_layer <- mask(r2, classify_border[classify_border$EA_koppen_30km_addClimate %in% climate, ])
  
  # 遍历每种森林类型
  for (forest in forest_types) {
    # 根据林类值掩膜
    forest_doy_stack <- mask(doy_stack, ifel(forest_layer == forest, 1, NA))
    
    # 对每个像元应用 Sen's Slope 和 Mann-Kendall 趋势分析
    trend_raster <- app(forest_doy_stack, fun_sen,cores=6  ) # 使用你定义的 fun_sen 函数
    
    # 将趋势结果保存到列表中
    trend_rasters[[paste0("trend_raster_", climate, "_forest", forest)]] <- trend_raster
  }
}

# 5.提取栅格数据并合并为数据框
all_data <- do.call(rbind, lapply(names(trend_rasters), function(name) {
  raster_df <- as.data.frame(trend_rasters[[name]], xy = TRUE)
  colnames(raster_df) <- c("Longitude", "Latitude", "Z", "slope", "p_value")  # 确保列名与返回的值匹配
  
  # 提取气候和森林信息
  parts <- strsplit(name, "_")[[1]]
  raster_df$Climate <- parts[3]
  raster_df$ForestType <- parts[4]
  return(raster_df)
}))

all_data$ForestType <- factor(all_data$ForestType, 
                              levels = c("forest1", "forest2", "forest3"),
                              labels = c( "NRF","PF", "P"))

summary(all_data)
write.csv(all_data, file = "./0.figure/03 Paper/01 Sens+MK_trend_analysis_EOS.csv", row.names = FALSE)


################# 绘图 d.precentage_barplot_GDO,按气候区、林地类型分类计算  ####################


library(dplyr)
all_data <- read.csv("./0.figure/03 Paper/01 Sens+MK_trend_analysis_EOS.csv")

all_data <- all_data %>%
  mutate(
    # 根据slope划分为三级
    slope_class = case_when(
      slope > 0.5 ~ 1,        # 延迟
      slope < -0.5 ~ -1,      # 提前
      TRUE ~ 0                # 稳定
    ),
    # 根据Z统计量划分为显著和不显著
    z_class = case_when(
      abs(Z) > 1.96 ~ 2,      # 显著
      abs(Z) <= 1.96 ~ 1      # 不显著
    ),
    # 计算class的最终值，slope_class * z_class
    class = slope_class * z_class
  )

# 查看结果
head(all_data)

# Longitude Latitude         Z     slope   p_value Climate ForestType slope_class z_class class
# 1  26.81471 49.45226 0.4490133 0.6666667 0.6534221     Cfa        NRF           1       1     1
# 2  27.08421 49.18276 1.4585691 1.0000000 0.1446837     Cfa        NRF           1       1     1
# 3  27.35370 49.18276 0.9878292 1.0000000 0.3232363     Cfa        NRF           1       1     1
# 4  27.08421 48.91327 1.1674345 1.2000000 0.2430350     Cfa        NRF           1       1     1
# 5  27.35370 48.91327 1.4585691 1.0000000 0.1446837     Cfa        NRF           1       1     1
# 6  27.62319 48.91327 0.7213357 1.0000000 0.4707030     Cfa        NRF           1       1     1


# 计算每个 Climate 和 ForestType 组合中 class 的占比
df_summary <- all_data %>%
  filter(class != 0) %>%  # 删除 class == 0 的数据
  group_by(Climate, ForestType, class) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Climate, ForestType) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  mutate(percentage = ifelse(class %in% c("-2", "-1"), -percentage, percentage))  # 负值表示提前

# 创建映射表
class_labels <- c("-2" = "Significant advance",
                  "-1" = "Nonsignificant advance",
                  "1"  = "Nonsignificant delay",
                  "2"  = "Significant delay")

df_summary$ForestType <- factor(df_summary$ForestType, levels = c("PF", "NRF", "P"))
df_summary$class <- factor(df_summary$class, levels = c( "1","2", "-1","-2"))
# df_summary$climate <- factor(df_summary$Climate, levels = c("Dwc", "Dwb", "Dfc", "Dfb", "Cfb", "Cfa"))
df_summary$climate <- factor(df_summary$Climate, levels = c("Cfa", "Cfb", "Dfb", "Dfc", "Dwb", "Dwc"))

# 作图  x取反 percentage → 负值变正、正值变负，改变柱子的位置方向。
p4 <- ggplot(df_summary, aes(x = climate, y = -percentage, fill = factor(class))) +  
  geom_bar(stat = "identity", position = "stack", width = 0.7, color = "black") +  # 堆叠显示
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.6) +  # 改为水平参考线
  # geom_text(aes(label = sprintf("%.1f%%", abs(percentage)), hjust = ifelse(percentage > 0, -0.1, 1.1)),
  #           position = position_stack(vjust = 0.5), size = 3, color = "black") +  # 文字放在柱状图的前方
  facet_grid(. ~ ForestType, switch = "y") +  # Facet 按 ForestType 分组
  scale_fill_manual(values = c( "1" = "lightblue", "2" = "#006699","-1" = "#fc8d59", "-2" = "#CC3333"), 
                    name = "Gradation of Phenological Trends",
                    labels = class_labels) +  # 自定义图例标签
  labs(x = "Climate type", y = "Percentage (%)") +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_rect(size = 0.6),
    axis.text.y = element_text(size = 16),
    axis.ticks.y = element_line(size = 0.6),
    axis.ticks.length.y = unit(0.1, "cm"),
    axis.title.y = element_text(size = 18),  #margin = margin(r = 10)
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1, vjust = 1),  # 角度设为30度
    axis.title.x = element_text(size = 18),
    axis.ticks.x = element_line(size = 0.6),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"),
    legend.position = "none",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    strip.text.y = element_text(size = 16, angle = 0),  # 右对齐 Facet 标签
    # strip.text.y = element_blank(),  # 不显示Facet 标签
    strip.placement = "outside",  # 放置 Facet 标签在外侧
    # strip.background = element_blank()  # 移除 Facet 背景
    strip.text.x = element_text(size = 16),  # 分面标签加粗并增加下边距,  margin = margin(b = 5)
    panel.spacing = unit(0.2, "cm"),  # 调整分面间距
    strip.background = element_rect(fill = "gray90", color = "black")
  ) +
  scale_y_continuous(limits = c(-100, 100), breaks = seq(-100, 100, by = 50), labels = abs)

# 打印图形
print(p4)

ggsave(
  filename = "./0.figure/03 Paper/Fig.4-c.Reclassified_fancetplot_EOS.tiff",
  plot = p3, width = 16,  height = 12,  units = "cm",  dpi = 300)


################# 绘图 ------图例  ####################


library(dplyr)
all_data <- read.csv("./0.figure/03 Paper/01 Sens+MK_trend_analysis_EOS.csv")

all_data <- all_data %>%
  mutate(
    # 根据slope划分为三级
    slope_class = case_when(
      slope > 0.5 ~ 1,        # 延迟
      slope < -0.5 ~ -1,      # 提前
      TRUE ~ 0                # 稳定
    ),
    # 根据Z统计量划分为显著和不显著
    z_class = case_when(
      abs(Z) > 1.96 ~ 2,      # 显著
      abs(Z) <= 1.96 ~ 1      # 不显著
    ),
    # 计算class的最终值，slope_class * z_class
    class = slope_class * z_class
  )



# 计算每个 Climate 和 ForestType 组合中 class 的占比
df_summary <- all_data %>%
  group_by(Climate, ForestType, class) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Climate, ForestType) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  mutate(percentage = ifelse(class %in% c("-2", "-1"), -percentage, percentage))  # 负值表示提前

# 创建映射表
class_labels <- c("-2" = "Significant advance",
                  "-1" = "Nonsignificant advance",
                  "0"  = "Stable",
                  "1"  = "Nonsignificant delay",
                  "2"  = "Significant delay")

df_summary$ForestType <- factor(df_summary$ForestType, levels = c("PF", "NRF", "P"))
df_summary$class <- factor(df_summary$class, levels = c( "1","2","0", "-1","-2"))
df_summary$climate <- factor(df_summary$Climate, levels = c("Cfa", "Cfb", "Dfb", "Dfc", "Dwb", "Dwc"))


library(ggplot2)

# 1. 定义图例标签顺序（您提供的）
class_labels <- c("2"  = "Significant delay",
                  "1"  = "Nonsignificant delay",
                  "0"  = "Stable",
                  "-1" = "Nonsignificant advance",
                  "-2" = "Significant advance")

# 2. 确保数据中的class列是因子且按此顺序排列
df_summary <- df_summary %>%
  mutate(class = factor(class, levels = names(class_labels)))

# 3. 绘图代码
# 作图  x取反 percentage → 负值变正、正值变负，改变柱子的位置方向。
p5 <- ggplot(df_summary, aes(x = climate, y = -percentage, fill = factor(class))) +  
  geom_bar(stat = "identity", position = "stack", width = 0.7, color = "black") +  # 堆叠显示
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.6) +  # 改为水平参考线
  # geom_text(aes(label = sprintf("%.1f%%", abs(percentage)), hjust = ifelse(percentage > 0, -0.1, 1.1)),
  #           position = position_stack(vjust = 0.5), size = 3, color = "black") +  # 文字放在柱状图的前方
  facet_grid(. ~ ForestType, switch = "y") +  # Facet 按 ForestType 分组
  scale_fill_manual(values = c("2" = "#006699", "1" = "lightblue","0"="#FFFFCC", "-1" = "#fc8d59", "-2" = "#CC3333"), 
                    name = "Gradation of Phenological Trends",
                    labels = class_labels, breaks = names(class_labels))+  # 自定义图例标签
  labs(x = "Climate type", y = "Percentage (%)") +
  theme_bw() +
  theme(
    legend.direction = "vertical",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_rect(size = 0.6),
    axis.text.y = element_text(size = 16),
    axis.ticks.y = element_line(size = 0.6),
    axis.ticks.length.y = unit(0.1, "cm"),
    axis.title.y = element_text(size = 18),  #margin = margin(r = 10)
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1, vjust = 1),  # 角度设为30度
    axis.title.x = element_text(size = 18),
    axis.ticks.x = element_line(size = 0.6),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"),
    legend.position = "bottom",
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 25,hjust = 0.5),
    strip.text.y = element_text(size = 16, angle = 0),  # 右对齐 Facet 标签
    # strip.text.y = element_blank(),  # 不显示Facet 标签
    strip.placement = "outside",  # 放置 Facet 标签在外侧
    # strip.background = element_blank()  # 移除 Facet 背景
    strip.text.x = element_text(size = 16),  # 分面标签加粗并增加下边距,  margin = margin(b = 5)
    panel.spacing = unit(0.2, "cm"),  # 调整分面间距
    strip.background = element_rect(fill = "gray90", color = "black")
  ) +
  scale_y_continuous(limits = c(-100, 100), breaks = seq(-100, 100, by = 50), labels = abs)

# 打印图形
print(p5)

ggsave(
  filename = "./0.figure/03 Paper/Fig.4-c.Reclassified_fancetplot_EOS-legend.tiff",
  plot = p5, width = 60,  height = 12,  units = "cm",  dpi = 300)

