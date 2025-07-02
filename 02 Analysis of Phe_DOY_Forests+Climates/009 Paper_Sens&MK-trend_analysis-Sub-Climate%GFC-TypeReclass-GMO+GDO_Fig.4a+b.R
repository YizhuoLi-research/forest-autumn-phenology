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

############################### 001 按三级气候分区作图  SOS   #################################      
# # SOS: phe1_DOY ;  MGP: phe2_DOY ;  GMO：phe3_DOY
# # GDO: phe4_DOY ;  MSP: phe5_DOY ;  EOS：phe6_DOY

## 1. 读取数据
# 列出文件夹中的所有文件
r2 <-  rast("./EA+NA_Results/EA+NA_GFC_30km.tif")
r1_files <- list.files("./EA+NA_Results/merged_Phe_DOY_years_2_PHE_analysis/merged_phe1_DOY/", 
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
forest_types <- c(1, 2, 3)  # 1: 再生林, 2: 原生林, 3: 人工林
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

# 4.遍历每种气候类型
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
    trend_raster <- app(forest_doy_stack, fun_sen, cores=6  ) # 使用你定义的 fun_sen 函数
    
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
                              labels = c("Naturally regenerating forest", "Primary forest", "Planted forest"))

summary(all_data)

# 6.按类别归类整理
library(tidyverse)
summary_data = all_data %>% 
  mutate(grade = slope * Z) %>%     #
  group_by(Climate,ForestType) %>% 
  summarise(slope_mean = mean(slope,na.rm=T),
            grade_mean = mean(grade,na.rm=T) )
# 7.设置因子
summary_data$Climate <- factor(summary_data$Climate, levels = climate_types)
print(summary_data)

# Climate ForestType                    slope_mean grade_mean
# 1 Cfa     Naturally regenerating forest    -0.283       0.811
# 2 Cfa     Primary forest                    0.250       1.15 
# 3 Cfa     Planted forest                   -0.554       0.833
# 4 Cfb     Naturally regenerating forest     0.500       0.892
# 5 Cfb     Primary forest                    0.953       1.54 
# 6 Cfb     Planted forest                    0.146       0.758
# 7 Dfb     Naturally regenerating forest    -0.179       0.604
# 8 Dfb     Primary forest                   -0.497       0.846
# 9 Dfb     Planted forest                    0.0525      0.388
# 10 Dfc     Naturally regenerating forest    -0.0389      0.661
# 11 Dfc     Primary forest                   -0.122       0.772
# 12 Dfc     Planted forest                    0.266       0.493
# 13 Dwb     Naturally regenerating forest    -0.162       0.378
# 14 Dwb     Primary forest                   -0.162       0.511
# 15 Dwb     Planted forest                   -0.0389      0.365
# 16 Dwc     Naturally regenerating forest     0.103       0.428
# 17 Dwc     Primary forest                    0.269       0.492
# 18 Dwc     Planted forest                    0.119       0.245

#######  SOS 趋势数据计算--Table 
library(dplyr)
library(tidyr)

# 计算每种 Climate 和 ForestType 的均值和 0.15 标准差
result <- all_data %>%
  group_by(Climate, ForestType) %>%
  summarise(
    mean = mean(slope, na.rm = TRUE),   # 计算均值
    sd = sd(slope, na.rm = TRUE),      # 计算标准差
    .groups = "drop"                        # 解除分组
  ) %>%
  mutate(
    `0.15sd` = sd * 0.15,                   # 计算 0.15 标准差
    Value = sprintf("%.2f ± %.2f", mean, `0.15sd`) # 格式化为 "mean ± 0.15sd"
  ) %>%
  ungroup() %>%                             # 确保解除分组
  dplyr::select(Climate, ForestType, Value) %>%    # 显式调用 dplyr::select
  pivot_wider(names_from = ForestType, values_from = Value) # 转换为宽表格

print(result)
# Climate `Naturally regenerating forest` `Primary forest` `Planted forest`
# 1 Cfa     -0.28 ± 0.12                    0.25 ± 0.19      -0.55 ± 0.09    
# 2 Cfb     0.50 ± 0.15                     0.95 ± 0.31      0.15 ± 0.17     
# 3 Dfb     -0.18 ± 0.11                    -0.50 ± 0.13     0.05 ± 0.08     
# 4 Dfc     -0.04 ± 0.15                    -0.12 ± 0.15     0.27 ± 0.11     
# 5 Dwb     -0.16 ± 0.08                    -0.16 ± 0.08     -0.04 ± 0.08    
# 6 Dwc     0.10 ± 0.11                     0.27 ± 0.10      0.12 ± 0.05   

# 计算 Kruskal-Wallis Test
kw_results <- all_data %>%
  group_by(Climate) %>%
  summarise(
    p_value = kruskal.test(slope ~ ForestType, data = across(everything()))$p.value
  ) %>%
  mutate(significance = case_when(
    p_value <= 0.001 ~ "***",
    p_value <= 0.01 ~ "**",
    p_value <= 0.05 ~ "*",
    TRUE ~ "NS"
  ))

# Climate  p_value significance
#   1 Cfa     9.24e-25 ***         
#   2 Cfb     1.28e-34 ***         
#   3 Dfb     7.78e-91 ***         
#   4 Dfc     8.46e-11 ***         
#   5 Dwb     2.95e- 2 *           
#   6 Dwc     8.54e-19 ***  

# 8.作图
# 定义显著性符号函数
get_significance_label <- function(p) {
  if (p <= 0.001) "***" else if (p <= 0.01) "**" else if (p <= 0.05) "*" else "ns"
}

# 显著性计算并生成标记数据
significance_plot_data <- all_data %>%
  group_by(Climate) %>%
  summarise(
    p1 = t.test(slope[ForestType == "Primary forest"], slope[ForestType == "Naturally regenerating forest"])$p.value,
    p2 = t.test(slope[ForestType == "Primary forest"], slope[ForestType == "Planted forest"])$p.value,
    p3 = t.test(slope[ForestType == "Naturally regenerating forest"], slope[ForestType == "Planted forest"])$p.value,
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    label1 = get_significance_label(p1),
    label2 = get_significance_label(p2),
    label3 = get_significance_label(p3)
  ) %>%
  ungroup() %>%  # 移除 rowwise 分组
  pivot_longer(
    cols = starts_with("label"),
    names_to = "Comparison",
    values_to = "Significance"
  ) %>%
  mutate(
    x_start = ifelse(Comparison == "label1", 1, ifelse(Comparison == "label2", 1, 2)),
    x_end = ifelse(Comparison == "label1", 2, 3),
    y = ifelse(Comparison == "label1", 3.7, ifelse(Comparison == "label2", 4.5, 5.3))   # 向上平移 0.5
  )

################# 绘图 SOS-trend-map  ####################

g1 <- ggplot(all_data, aes(x = slope, color = ForestType, fill = ForestType)) +
  # geom_density(aes(y = ..scaled..), alpha = 0.3, linewidth = 1) +  # 使用相对频率分布，绘制密度曲线，添加透明度和线条宽度
  geom_density( alpha = 0.3, linewidth = 1) +  # 使用相对频率分布，绘制密度曲线，添加透明度和线条宽度
  facet_wrap(~Climate,  ncol=1, switch = "y",scales = "free")+
  scale_color_manual(values = c(
    "Primary forest" = "#2E8B57",                # 原生林--深绿色
    "Naturally regenerating forest" = "#9ACD32", # 次生林--浅绿色
    "Planted forest" = "#FFA500"                 # 人工林--橙色
  )) +
  scale_fill_manual(values = c(
    "Primary forest" = "#2E8B57",
    "Naturally regenerating forest" = "#9ACD32",
    "Planted forest" = "#FFA500"
  )) +
  labs(title = "",
       x = expression("Trend of SOS (days yr"^-1*")"),
       y = NULL,  # 移除Y轴标题
       color = "Forest type",fill = "Forest type" ) +
  theme_minimal() +
  theme( legend.position = "none",                # 隐藏图例
         axis.text.x = element_text(size = 11),   # 强制显示所有分面的X轴刻度文字
         axis.text.y = element_text(size = 11),   # 纵坐标刻度文字
         axis.title.x = element_text(size = 12),  # 横坐标标题文字
         axis.title.y = element_blank(),          # 移除Y轴标题
         strip.text.y.left = element_text(size = 12), # 左侧分面标题文字
         strip.placement = "outside",             # 将分面标题放置到图的外侧
         # panel.spacing = unit(1, "lines"),        # 设置面板间距
         panel.grid.major = element_blank(),      # 去掉主网格线
         panel.grid.minor = element_blank(),      # 去掉次网格线
         axis.line = element_line(linewidth = 0.4, color = "black"), # 添加轴线
         strip.background = element_blank(),      # 去掉分面背景
         panel.border = element_blank(),          # 去掉面板边框
         panel.grid = element_blank() ,           # 去掉所有网格线
         panel.spacing = unit(0.2, "lines"),     # 缩小分面间距
         plot.margin = unit(c(0.1, 0.5, 0.2, 0.2), "lines") # 缩小上下左右边距
         # aspect.ratio = 1 / 2.3                   # 设置长宽比为 1:2.5
  ) +
  coord_cartesian(xlim = c(-3.0, 4.0), ylim = c(0, 2.5)) +  # 设置横纵坐标范围
  scale_x_continuous( breaks = seq(-2.0, 4.0, by = 2.0),
                      labels = scales::number_format(accuracy = 0.1),  # 设置为显示一位小数
                      guide = guide_axis(check.overlap = TRUE)  # 确保每个分面都显示X轴标签
  ) +
  # scale_y_continuous(breaks = NULL) +         # 隐藏X轴的刻度
  scale_y_continuous(breaks = seq(0.0, 2.0, by = 1.0),
                     labels = scales::number_format(accuracy = 0.1) ) +    # 设置纵坐标刻度
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4)   # 添加x=0的虚线，设置size
# geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50", linewidth = 0.4)  # 添加y=0.5的虚线，设置size
# scale_y_continuous(limits = c(0.0, 1.8), breaks = seq(0.0, 2.0, by = 0.5)) 

g1

ggsave(
  filename = "./0.figure/03 Paper/Fig.DOY_trend_Densityplot_GFT_sub_SOS-Frequen.tiff",
  plot = g1,  width = 2.4,  height = 7,  units = "in",  dpi = 300)

############ --显著性分类地图SOS #########
library(dplyr)

all_data <- all_data %>%
  mutate(
    # 根据slope划分为三级
    slope_class = case_when(
      slope > 0.5 ~ 1,        # 提前
      slope < -0.5 ~ -1,      # 延迟
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
wr <- map_data("world") %>%
  filter(lat > 20)

x_lines <- seq(-120,180, by = 60)# Defines the x axes required

library(ggplot2)
library(dplyr)

# 假设all_data包含你计算出的class列，并且df是你绘制地图的原始数据
wr <- map_data("world") %>%
  filter(lat > 20)

x_lines <- seq(-120, 180, by = 60)  # Defines the x axes required

all_data$class <- factor(all_data$class, levels = c("-2", "-1", "0", "1", "2"))

################# 绘图 Classified_map_SOS  ####################

p1 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    # 画地图背景
  geom_tile(data = all_data, aes(x = Longitude , y = Latitude , fill = class)) +  # 使用class列填充地图
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "gray40", fill = alpha("gray40", 0), linewidth = 0.3) +
  expand_limits(x = wr$long, y = wr$lat) +  # 指定经纬度范围
  scale_fill_manual(
    values = c("-2" = "#CC3333",   # 显著提前
               "-1" = "#FC8D59",   # 轻微提前
               "0"  = "#FFFFCC",   # 稳定不变
               "1"  = "lightblue", # 轻微延迟
               "2"  = "#006699" ),        # 显著延迟
    na.value = "transparent"  # 对于缺失值设置透明
  ) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "none",  # 移除图例
        legend.direction = "vertical") +  # 取消图例方向
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  geom_segment(size = 0.5, aes(x = -180, xend = -180, y = 20, yend = 90), colour = "gray40") +  # 经线
  geom_segment(size = 0.5, aes(x = 200, xend = 200, y = 20, yend = 90), colour = "gray40") +  # 经线
  geom_segment(size = 0.5, aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray40") +  # 纬线20
  geom_segment(size = 0.5, aes(y = 20, yend = 20, x = 200, xend = 0), colour = "gray40") +
  geom_segment(size = 0.5, aes(y = 90, yend = 90, x = -180, xend = 0), colour = "gray40") +  # 纬线90
  geom_segment(size = 0.5, aes(y = 90, yend = 90, x = 200, xend = 0), colour = "gray40") +
  geom_segment(size = 0.3, aes(y = 20, yend = 90, x = x_lines, xend = x_lines), colour = "gray40", linetype = "dashed") +  # 经线
  geom_segment(size = 0.3, aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # 纬线30
  geom_segment(size = 0.3, aes(y = 30, yend = 30, x = 200, xend = 0), colour = "gray40", linetype = "dashed") +  # 纬线30
  geom_segment(size = 0.3, aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # 纬线50
  geom_segment(size = 0.3, aes(y = 50, yend = 50, x = 200, xend = 0), colour = "gray40", linetype = "dashed") +  # 纬线50
  geom_segment(size = 0.3, aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # 纬线70
  geom_segment(size = 0.3, aes(y = 70, yend = 70, x = 200, xend = 0), colour = "gray40", linetype = "dashed") +  # 纬线70
  geom_text(aes(x = -193, y = seq(30, 70, by = 20), label = paste0(seq(30, 70, by = 20), "°N")), size = 4) +
  geom_text(aes(y = 15.5, x = x_lines,
                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°E")), size = 4) +
  coord_quickmap()  # 保持地图比例接近真实地球比例

p1


# 保存图像
ggsave(
  filename = "./0.figure/03 Paper/Fig.Classified_map_SOS.tiff",
  plot = p1, width = 8, height = 3, units = "in", dpi = 300)

############################### 002 按三级气候分区作图  GMO   #################################      
# # SOS: phe1_DOY ;  MGP: phe2_DOY ;  GMO：phe3_DOY
# # GDO: phe4_DOY ;  MSP: phe5_DOY ;  EOS：phe6_DOY

## 1. 读取数据
# 列出文件夹中的所有文件
r2 <-  rast("./EA+NA_Results/EA+NA_GFC_30km.tif")
r1_files <- list.files("./EA+NA_Results/merged_Phe_DOY_years_2_PHE_analysis/merged_phe3_DOY/", 
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
forest_types <- c(1, 2, 3)  # 1: 再生林, 2: 原生林, 3: 人工林
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

# 4.遍历每种气候类型
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
                              labels = c("Naturally regenerating forest","Primary forest",  "Planted forest"))

summary(all_data)

# 6.按类别归类整理
library(tidyverse)
summary_data = all_data %>% 
  mutate(grade = slope * Z) %>%     #
  group_by(Climate,ForestType) %>% 
  summarise(slope_mean = mean(slope,na.rm=T),
            grade_mean = mean(grade,na.rm=T) )
# 7.设置因子
summary_data$Climate <- factor(summary_data$Climate, levels = climate_types)
print(summary_data)

# > summary_data
# Climate ForestType                    slope_mean grade_mean
# 1 Cfa     Naturally regenerating forest     0.165       0.444
# 2 Cfa     Primary forest                    0.242       0.783
# 3 Cfa     Planted forest                   -0.163       0.581
# 4 Cfb     Naturally regenerating forest     0.144       0.903
# 5 Cfb     Primary forest                    0.510       1.14 
# 6 Cfb     Planted forest                    0.137       1.02 
# 7 Dfb     Naturally regenerating forest     0.143       0.893
# 8 Dfb     Primary forest                   -0.425       0.834
# 9 Dfb     Planted forest                    0.615       1.25 
# 10 Dfc     Naturally regenerating forest    -0.0523      0.606
# 11 Dfc     Primary forest                   -0.260       0.574
# 12 Dfc     Planted forest                    0.720       0.840
# 13 Dwb     Naturally regenerating forest    -0.0131      0.403
# 14 Dwb     Primary forest                    0.0942      0.234
# 15 Dwb     Planted forest                   -0.0818      0.379
# 16 Dwc     Naturally regenerating forest    -0.0688      0.375
# 17 Dwc     Primary forest                    0.0406      0.407
# 18 Dwc     Planted forest                    0.154       0.201

#######  GMO 趋势数据计算--Table 
library(dplyr)
library(tidyr)

# 计算每种 Climate 和 ForestType 的均值和 0.15 标准差
result <- all_data %>%
  group_by(Climate, ForestType) %>%
  summarise(
    mean = mean(slope, na.rm = TRUE),   # 计算均值
    sd = sd(slope, na.rm = TRUE),      # 计算标准差
    .groups = "drop"                        # 解除分组
  ) %>%
  mutate(
    `0.15sd` = sd * 0.15,                   # 计算 0.15 标准差
    Value = sprintf("%.2f ± %.2f", mean, `0.15sd`) # 格式化为 "mean ± 0.15sd"
  ) %>%
  ungroup() %>%                             # 确保解除分组
  dplyr::select(Climate, ForestType, Value) %>%    # 显式调用 dplyr::select
  pivot_wider(names_from = ForestType, values_from = Value) # 转换为宽表格

print(result)
# Climate `Naturally regenerating forest` `Primary forest` `Planted forest
# 1 Cfa     0.16 ± 0.09      0.24 ± 0.12                     -0.16 ± 0.11    
# 2 Cfb     0.14 ± 0.15      0.51 ± 0.22                     0.14 ± 0.17     
# 3 Dfb     0.14 ± 0.14      -0.43 ± 0.13                    0.62 ± 0.13     
# 4 Dfc     -0.05 ± 0.13     -0.26 ± 0.11                    0.72 ± 0.12     
# 5 Dwb     -0.01 ± 0.08     0.09 ± 0.05                     -0.08 ± 0.10    
# 6 Dwc     -0.07 ± 0.08     0.04 ± 0.08                     0.15 ± 0.04    


# 计算 Kruskal-Wallis Test
kw_results <- all_data %>%
  group_by(Climate) %>%
  summarise(
    p_value = kruskal.test(slope ~ ForestType, data = across(everything()))$p.value
  ) %>%
  mutate(significance = case_when(
    p_value <= 0.001 ~ "***",
    p_value <= 0.01 ~ "**",
    p_value <= 0.05 ~ "*",
    TRUE ~ "NS"
  ))
print(kw_results)
# Climate  p_value significance
# 1 Cfa     4.79e- 51 ***         
# 2 Cfb     1.30e-  1 NS          
# 3 Dfb     8.05e-187 ***         
# 4 Dfc     8.71e-103 ***         
# 5 Dwb     9.85e-  5 ***         
# 6 Dwc     7.09e-  9 ***  

# 8.作图
# 定义显著性符号函数
get_significance_label <- function(p) {
  if (p <= 0.001) "***" else if (p <= 0.01) "**" else if (p <= 0.05) "*" else "ns"
}

# 显著性计算并生成标记数据
significance_plot_data <- all_data %>%
  group_by(Climate) %>%
  summarise(
    p1 = t.test(slope[ForestType == "Primary forest"], slope[ForestType == "Naturally regenerating forest"])$p.value,
    p2 = t.test(slope[ForestType == "Primary forest"], slope[ForestType == "Planted forest"])$p.value,
    p3 = t.test(slope[ForestType == "Naturally regenerating forest"], slope[ForestType == "Planted forest"])$p.value,
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    label1 = get_significance_label(p1),
    label2 = get_significance_label(p2),
    label3 = get_significance_label(p3)
  ) %>%
  ungroup() %>%  # 移除 rowwise 分组
  pivot_longer(
    cols = starts_with("label"),
    names_to = "Comparison",
    values_to = "Significance"
  ) %>%
  mutate(
    x_start = ifelse(Comparison == "label1", 1, ifelse(Comparison == "label2", 1, 2)),
    x_end = ifelse(Comparison == "label1", 2, 3),
    y = ifelse(Comparison == "label1", 3.7, ifelse(Comparison == "label2", 4.5, 5.3))   # 向上平移 0.5
  )

################# 绘图 GMO-trend-map  ####################

g1 <- ggplot(all_data, aes(x = slope, color = ForestType, fill = ForestType)) +
  # geom_density(aes(y = ..scaled..), alpha = 0.3, linewidth = 1) +  # 使用相对频率分布，绘制密度曲线，添加透明度和线条宽度
  geom_density( alpha = 0.3, linewidth = 1) +  # 使用相对频率分布，绘制密度曲线，添加透明度和线条宽度
  facet_wrap(~Climate,  ncol=1, switch = "y",scales = "free")+
  scale_color_manual(values = c(
    "Primary forest" = "#2E8B57",                # 原生林--深绿色
    "Naturally regenerating forest" = "#9ACD32", # 次生林--浅绿色
    "Planted forest" = "#FFA500"                 # 人工林--橙色
  )) +
  scale_fill_manual(values = c(
    "Primary forest" = "#2E8B57",
    "Naturally regenerating forest" = "#9ACD32",
    "Planted forest" = "#FFA500"
  )) +
  labs(title = "",
       x = expression("Trend of GMO (days yr"^-1*")"),
       y = NULL,  # 移除Y轴标题
       color = "Forest type",fill = "Forest type" ) +
  theme_minimal() +
  theme( legend.position = "none",                # 隐藏图例
         axis.text.x = element_text(size = 11),   # 强制显示所有分面的X轴刻度文字
         axis.text.y = element_text(size = 11),   # 纵坐标刻度文字
         axis.title.x = element_text(size = 12),  # 横坐标标题文字
         axis.title.y = element_blank(),          # 移除Y轴标题
         strip.text.y.left = element_text(size = 12), # 左侧分面标题文字
         strip.placement = "outside",             # 将分面标题放置到图的外侧
         # panel.spacing = unit(1, "lines"),        # 设置面板间距
         panel.grid.major = element_blank(),      # 去掉主网格线
         panel.grid.minor = element_blank(),      # 去掉次网格线
         axis.line = element_line(linewidth = 0.4, color = "black"), # 添加轴线
         strip.background = element_blank(),      # 去掉分面背景
         panel.border = element_blank(),          # 去掉面板边框
         panel.grid = element_blank() ,           # 去掉所有网格线
         panel.spacing = unit(0.2, "lines"),     # 缩小分面间距
         plot.margin = unit(c(0.1, 0.5, 0.2, 0.2), "lines") # 缩小上下左右边距
         # aspect.ratio = 1 / 2.3                   # 设置长宽比为 1:2.5
  ) +
  coord_cartesian(xlim = c(-3.0, 4.0), ylim = c(0, 2.5)) +  # 设置横纵坐标范围
  scale_x_continuous( breaks = seq(-2.0, 4.0, by = 2.0),
                      labels = scales::number_format(accuracy = 0.1),  # 设置为显示一位小数
                      guide = guide_axis(check.overlap = TRUE)  # 确保每个分面都显示X轴标签
  ) +
  # scale_y_continuous(breaks = NULL) +         # 隐藏X轴的刻度
  scale_y_continuous(breaks = seq(0.0, 2.0, by = 1.0),
                     labels = scales::number_format(accuracy = 0.1) ) +    # 设置纵坐标刻度
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4)   # 添加x=0的虚线，设置size
# geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50", linewidth = 0.4)  # 添加y=0.5的虚线，设置size
# scale_y_continuous(limits = c(0.0, 1.8), breaks = seq(0.0, 2.0, by = 0.5)) 

g1

ggsave(
  filename = "./0.figure/03 Paper/Fig.DOY_trend_Densityplot_GFT_sub_GMO-Frequen.tiff",
  plot = g1,  width = 2.4,  height = 7,  units = "in",  dpi = 300)

############ --显著性分类地图GMO #########
library(dplyr)

all_data <- all_data %>%
  mutate(
    # 根据slope划分为三级
    slope_class = case_when(
      slope > 0.5 ~ 1,        # 提前
      slope < -0.5 ~ -1,      # 延迟
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
wr <- map_data("world") %>%
  filter(lat > 20)

x_lines <- seq(-120,180, by = 60)# Defines the x axes required

library(ggplot2)
library(dplyr)

# 假设all_data包含你计算出的class列，并且df是你绘制地图的原始数据
wr <- map_data("world") %>%
  filter(lat > 20)

x_lines <- seq(-120, 180, by = 60)  # Defines the x axes required

all_data$class <- factor(all_data$class, levels = c("-2", "-1", "0", "1", "2"))

################# 绘图 Classified_map_GMO  ####################

p1 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    # 画地图背景
  geom_tile(data = all_data, aes(x = Longitude , y = Latitude , fill = class)) +  # 使用class列填充地图
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "gray40", fill = alpha("gray40", 0), linewidth = 0.3) +
  expand_limits(x = wr$long, y = wr$lat) +  # 指定经纬度范围
  scale_fill_manual(
    values = c("-2" = "#CC3333",   # 显著提前
               "-1" = "#FC8D59",   # 轻微提前
               "0"  = "#FFFFCC",   # 稳定不变
               "1"  = "lightblue", # 轻微延迟
               "2"  = "#006699" ),        # 显著延迟
    na.value = "transparent",  # 对于缺失值设置透明
  ) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "none",  # 移除图例
        legend.direction = "vertical") +  # 取消图例方向
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  geom_segment(size = 0.5, aes(x = -180, xend = -180, y = 20, yend = 90), colour = "gray40") +  # 经线
  geom_segment(size = 0.5, aes(x = 200, xend = 200, y = 20, yend = 90), colour = "gray40") +  # 经线
  geom_segment(size = 0.5, aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray40") +  # 纬线20
  geom_segment(size = 0.5, aes(y = 20, yend = 20, x = 200, xend = 0), colour = "gray40") +
  geom_segment(size = 0.5, aes(y = 90, yend = 90, x = -180, xend = 0), colour = "gray40") +  # 纬线90
  geom_segment(size = 0.5, aes(y = 90, yend = 90, x = 200, xend = 0), colour = "gray40") +
  geom_segment(size = 0.3, aes(y = 20, yend = 90, x = x_lines, xend = x_lines), colour = "gray40", linetype = "dashed") +  # 经线
  geom_segment(size = 0.3, aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # 纬线30
  geom_segment(size = 0.3, aes(y = 30, yend = 30, x = 200, xend = 0), colour = "gray40", linetype = "dashed") +  # 纬线30
  geom_segment(size = 0.3, aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # 纬线50
  geom_segment(size = 0.3, aes(y = 50, yend = 50, x = 200, xend = 0), colour = "gray40", linetype = "dashed") +  # 纬线50
  geom_segment(size = 0.3, aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  # 纬线70
  geom_segment(size = 0.3, aes(y = 70, yend = 70, x = 200, xend = 0), colour = "gray40", linetype = "dashed") +  # 纬线70
  geom_text(aes(x = -193, y = seq(30, 70, by = 20), label = paste0(seq(30, 70, by = 20), "°N")), size = 4) +
  geom_text(aes(y = 15.5, x = x_lines,
                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°E")), size = 4) +
  coord_quickmap()  # 保持地图比例接近真实地球比例

p1


# 保存图像
ggsave(
  filename = "./0.figure/03 Paper/Fig.Classified_map_GMO.tiff",
  plot = p1, width = 8, height = 3, units = "in", dpi = 300)

############################### 003 按三级气候分区作图  GDO   #################################      
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
forest_types <- c(1, 2, 3)  # 1: 再生林, 2: 原生林, 3: 人工林
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

# 4.遍历每种气候类型
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
                              labels = c("Naturally regenerating forest","Primary forest",  "Planted forest"))

summary(all_data)

# 6.按类别归类整理
library(tidyverse)
summary_data = all_data %>% 
  mutate(grade = slope * Z) %>%     #
  group_by(Climate,ForestType) %>% 
  summarise(slope_mean = mean(slope,na.rm=T),
            grade_mean = mean(grade,na.rm=T) )
# 7.设置因子
summary_data$Climate <- factor(summary_data$Climate, levels = climate_types)
print(summary_data)

# > summary_data
# Climate ForestType slope_mean grade_mean
# 1 Cfa     Naturally regenerating forest     0.0255      0.471
# 2 Cfa     Primary forest                    0.0429      0.773
# 3 Cfa     Planted forest                   -0.279       0.602
# 4 Cfb     Naturally regenerating forest     0.0376      0.606
# 5 Cfb     Primary forest                    0.678       1.21 
# 6 Cfb     Planted forest                    0.251       1.14 
# 7 Dfb     Naturally regenerating forest     0.275       0.637
# 8 Dfb     Primary forest                    0.0247      0.591
# 9 Dfb     Planted forest                    0.445       0.877
# 10 Dfc     Naturally regenerating forest     0.201       0.852
# 11 Dfc     Primary forest                   -0.156       0.488
# 12 Dfc     Planted forest                    1.29        2.78 
# 13 Dwb     Naturally regenerating forest    -0.213       0.513
# 14 Dwb     Primary forest                   -0.160       0.335
# 15 Dwb     Planted forest                   -0.0852      0.497
# 16 Dwc     Naturally regenerating forest    -0.602       0.996
# 17 Dwc     Primary forest                   -0.358       0.698
# 18 Dwc     Planted forest                   -0.107       0.336
# #######  GDO 趋势数据计算--Table 
library(dplyr)
library(tidyr)

# 计算每种 Climate 和 ForestType 的均值和 0.15 标准差
result <- all_data %>%
  group_by(Climate, ForestType) %>%
  summarise(
    mean = mean(slope, na.rm = TRUE),   # 计算均值
    sd = sd(slope, na.rm = TRUE),      # 计算标准差
    .groups = "drop"                        # 解除分组
  ) %>%
  mutate(
    `0.15sd` = sd * 0.15,                   # 计算 0.15 标准差
    Value = sprintf("%.2f ± %.2f", mean, `0.15sd`) # 格式化为 "mean ± 0.15sd"
  ) %>%
  ungroup() %>%                             # 确保解除分组
  dplyr::select(Climate, ForestType, Value) %>%    # 显式调用 dplyr::select
  pivot_wider(names_from = ForestType, values_from = Value) # 转换为宽表格

print(result)
#  Climate `Naturally regenerating forest` `Primary forest` `Planted forest`
# 1 Cfa     0.03 ± 0.10      0.04 ± 0.14                     -0.28 ± 0.10    
# 2 Cfb     0.04 ± 0.13      0.68 ± 0.18                     0.25 ± 0.17     
# 3 Dfb     0.28 ± 0.10      0.02 ± 0.12                     0.45 ± 0.12     
# 4 Dfc     0.20 ± 0.14      -0.16 ± 0.10                    1.29 ± 0.14     
# 5 Dwb     -0.21 ± 0.08     -0.16 ± 0.07                    -0.09 ± 0.10    
# 6 Dwc     -0.60 ± 0.09     -0.36 ± 0.10                    -0.11 ± 0.10   


# 计算 Kruskal-Wallis Test
kw_results <- all_data %>%
  group_by(Climate) %>%
  summarise(
    p_value = kruskal.test(slope ~ ForestType, data = across(everything()))$p.value
  ) %>%
  mutate(significance = case_when(
    p_value <= 0.001 ~ "***",
    p_value <= 0.01 ~ "**",
    p_value <= 0.05 ~ "*",
    TRUE ~ "NS"
  ))
print(kw_results)
# Climate  p_value significance
# 1 Cfa     2.93e- 44 ***         
# 2 Cfb     4.01e- 19 ***         
# 3 Dfb     6.55e- 61 ***         
# 4 Dfc     3.49e-188 ***         
# 5 Dwb     4.32e-  2 *           
# 6 Dwc     8.62e- 27 *** 

# 8.作图
# 定义显著性符号函数
get_significance_label <- function(p) {
  if (p <= 0.001) "***" else if (p <= 0.01) "**" else if (p <= 0.05) "*" else "ns"
}

# 显著性计算并生成标记数据
significance_plot_data <- all_data %>%
  group_by(Climate) %>%
  summarise(
    p1 = t.test(slope[ForestType == "Primary forest"], slope[ForestType == "Naturally regenerating forest"])$p.value,
    p2 = t.test(slope[ForestType == "Primary forest"], slope[ForestType == "Planted forest"])$p.value,
    p3 = t.test(slope[ForestType == "Naturally regenerating forest"], slope[ForestType == "Planted forest"])$p.value,
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    label1 = get_significance_label(p1),
    label2 = get_significance_label(p2),
    label3 = get_significance_label(p3)
  ) %>%
  ungroup() %>%  # 移除 rowwise 分组
  pivot_longer(
    cols = starts_with("label"),
    names_to = "Comparison",
    values_to = "Significance"
  ) %>%
  mutate(
    x_start = ifelse(Comparison == "label1", 1, ifelse(Comparison == "label2", 1, 2)),
    x_end = ifelse(Comparison == "label1", 2, 3),
    y = ifelse(Comparison == "label1", 3.7, ifelse(Comparison == "label2", 4.5, 5.3))   # 向上平移 0.5
  )

################# 绘图 GDO-trend-map  ####################

g1 <- ggplot(all_data, aes(x = slope, color = ForestType, fill = ForestType)) +
  # geom_density(aes(y = ..scaled..), alpha = 0.3, linewidth = 1) +  # 使用相对频率分布，绘制密度曲线，添加透明度和线条宽度
  geom_density( alpha = 0.3, linewidth = 1) +  # 使用相对频率分布，绘制密度曲线，添加透明度和线条宽度
  facet_wrap(~Climate,  ncol=1, switch = "y",scales = "free")+
  scale_color_manual(values = c(
    "Primary forest" = "#2E8B57",                # 原生林--深绿色
    "Naturally regenerating forest" = "#9ACD32", # 次生林--浅绿色
    "Planted forest" = "#FFA500"                 # 人工林--橙色
  )) +
  scale_fill_manual(values = c(
    "Primary forest" = "#2E8B57",
    "Naturally regenerating forest" = "#9ACD32",
    "Planted forest" = "#FFA500"
  )) +
  labs(title = "",
       x = expression("Trend of GDO (days yr"^-1*")"),
       y = NULL,  # 移除Y轴标题
       color = "Forest type",fill = "Forest type" ) +
  theme_minimal() +
  theme( legend.position = "none",                # 隐藏图例
         axis.text.x = element_text(size = 11),   # 强制显示所有分面的X轴刻度文字
         axis.text.y = element_text(size = 11),   # 纵坐标刻度文字
         axis.title.x = element_text(size = 12),  # 横坐标标题文字
         axis.title.y = element_blank(),          # 移除Y轴标题
         strip.text.y.left = element_text(size = 12), # 左侧分面标题文字
         strip.placement = "outside",             # 将分面标题放置到图的外侧
         # panel.spacing = unit(1, "lines"),        # 设置面板间距
         panel.grid.major = element_blank(),      # 去掉主网格线
         panel.grid.minor = element_blank(),      # 去掉次网格线
         axis.line = element_line(linewidth = 0.4, color = "black"), # 添加轴线
         strip.background = element_blank(),      # 去掉分面背景
         panel.border = element_blank(),          # 去掉面板边框
         panel.grid = element_blank() ,           # 去掉所有网格线
         panel.spacing = unit(0.2, "lines"),     # 缩小分面间距
         plot.margin = unit(c(0.1, 0.5, 0.2, 0.2), "lines") # 缩小上下左右边距
         # aspect.ratio = 1 / 2.3                   # 设置长宽比为 1:2.5
  ) +
  coord_cartesian(xlim = c(-3.0, 4.0), ylim = c(0, 2.5)) +  # 设置横纵坐标范围
  scale_x_continuous( breaks = seq(-2.0, 4.0, by = 2.0),
                      labels = scales::number_format(accuracy = 0.1),  # 设置为显示一位小数
                      guide = guide_axis(check.overlap = TRUE)  # 确保每个分面都显示X轴标签
  ) +
  # scale_y_continuous(breaks = NULL) +         # 隐藏X轴的刻度
  scale_y_continuous(breaks = seq(0.0, 2.0, by = 1.0),
                     labels = scales::number_format(accuracy = 0.1) ) +    # 设置纵坐标刻度
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4)   # 添加x=0的虚线，设置size
# geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50", linewidth = 0.4)  # 添加y=0.5的虚线，设置size
# scale_y_continuous(limits = c(0.0, 1.8), breaks = seq(0.0, 2.0, by = 0.5)) 

g1

ggsave(
  filename = "./0.figure/03 Paper/Fig.DOY_trend_Densityplot_GFT_sub_GDO-Frequen.tiff",
  plot = g1,  width = 2.4,  height = 7,  units = "in",  dpi = 300)

############ --显著性分类地图GDO #########
library(dplyr)

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
wr <- map_data("world") %>%
  filter(lat > 20)

x_lines <- seq(-120,180, by = 60)# Defines the x axes required

library(ggplot2)
library(dplyr)

# 假设all_data包含你计算出的class列，并且df是你绘制地图的原始数据
wr <- map_data("world") %>%
  filter(lat > 20)

x_lines <- seq(-120, 180, by = 60)  # Defines the x axes required

all_data$class <- factor(all_data$class, levels = c("-2", "-1", "0", "1", "2"))

################# 绘图 Classified_map_GDO  ####################

p1 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    # 画地图背景
  geom_tile(data = all_data, aes(x = Longitude , y = Latitude , fill = class)) +  # 使用class列填充地图
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "gray40", fill = alpha("gray40", 0), linewidth = 0.3) +
  expand_limits(x = wr$long, y = wr$lat) +  # 指定经纬度范围
  scale_fill_manual(
    values = c("-2" = "#CC3333",   # 显著提前
               "-1" = "#FC8D59",   # 轻微提前
               "0"  = "#FFFFCC",   # 稳定不变
               "1"  = "lightblue", # 轻微延迟
               "2"  = "#006699" ),        # 显著延迟
    na.value = "transparent"  # 对于缺失值设置透明
  ) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "none",  # 移除图例
        legend.direction = "vertical") +  # 取消图例方向
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  geom_hline(aes(yintercept = 20), size = 0.1) +  
  geom_segment(size = 0.4 ,aes(y = 20, yend = 90, x = x_lines, xend = x_lines),colour = "gray40", linetype = "dashed") +  # 经线
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  # 纬线20
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = 180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  #30
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x =  180, xend = 0), colour = "gray40",linetype = "dashed") + 
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  #50
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x =  180, xend = 0), colour = "gray40",linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  #70
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x =  180, xend = 0), colour = "gray40",linetype = "dashed") +
  # Adds labels
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 12) +
  # geom_text(aes( y = 15, x = x_lines,
  #                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 10) +
  geom_text(aes( y = 15+ c(-2.5, -2.5, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 12) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # 如果要显示北极投影
    
print(p1)# 保存图像
ggsave(
  filename = "./0.figure/03 Paper/Fig.4-a.Classified_map_GDO.tiff",
  plot = p1, width = 15, height = 15, units = "in", dpi = 300)


################# 绘图 precentage_barplot_GDO  ####################
# 加载必要的包
library(ggplot2)
library(dplyr)

# 假设你的数据框名为 all_data
# 计算每个 class 的百分比
class_percentages <- all_data %>%
  group_by(class) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# 定义颜色
class_colors <- c("-2" = "#CC3333", 
                  "-1" = "#FC8D59" , 
                  "0"  = "#FFFFCC", 
                  "1"  = "lightblue", 
                  "2"  = "#006699")

class_percentages$class <- factor(class_percentages$class, levels = c("2", "1", "0", "-1", "-2"))

# 创建直方图 红色表示提前，y = - percentage添加-放前面

g1 <- ggplot(class_percentages, aes(x = as.factor(class), y = percentage, fill = as.factor(class))) +
  # geom_bar(stat = "identity", width = 0.7) +  # 调整条形宽度
  geom_bar(stat = "identity", width = 0.75, color = "black", linewidth = 1.0) +  # 添加黑色框线
  geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5, size = 25, color = "black") +  # 调整文本大小和位置
  scale_fill_manual(values = class_colors) +  # 使用自定义颜色
  labs(x = "", y = "Percentage (%)", title = "") +  # 设置轴标题
  theme_bw() +  # 使用黑白主题
  theme(
    panel.grid.major.x = element_blank(),  # 隐藏x轴主要网格线
    panel.grid.minor.x = element_blank(),  # 隐藏x轴次要网格线
    panel.grid.major.y = element_blank(),  # 设置y轴网格线 #element_line(color = "gray80", size = 0.5), 
    panel.grid.minor.y = element_blank(),  # 隐藏y轴次要网格线
    panel.border = element_rect(size = 2.5, color = "black"),  # 设置边框大小和颜色
    axis.text.y = element_text(size = 72, color = "black"),  # 设置y轴文字大小
    axis.ticks.y = element_line(size = 2.5, color = "black"),  # 设置y轴刻度线
    axis.ticks.length.y = unit(0.3, "cm"),  # 设置y轴刻度长度
    axis.title.y = element_text(size = 81, margin = margin(r = 20)),  # 设置y轴标题大小和边距
    axis.title.x = element_blank(),  # 隐藏x轴标题
    axis.text.x = element_blank(),  # 隐藏x轴文字
    axis.ticks.x = element_blank(),  # 隐藏x轴刻度
    plot.background = element_rect(fill = "white", color = NA),  # 设置背景颜色
    plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"),  # 设置边距
    legend.position = "none") + # 隐藏图例
  scale_y_continuous(limits = c(0, 75), breaks = seq(0, 75, by = 20))   # 设置 y 轴范围和刻度

print(g1)
ggsave(
  filename = "./0.figure/03 Paper/Fig.4-a.Classified_barplot_GDO.tiff",
  plot = g1, width = 16,  height = 9.5,  units = "in",  dpi = 300)


############################### 004 按三级气候分区作图  EOS   #################################      
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
forest_types <- c(1, 2, 3)  # 1: 再生林, 2: 原生林, 3: 人工林
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

# 4.遍历每种气候类型
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
                              labels = c("Naturally regenerating forest", "Primary forest",  "Planted forest"))

summary(all_data)

# 6.按类别归类整理
library(tidyverse)
summary_data = all_data %>% 
  mutate(grade = slope * Z) %>%     #
  group_by(Climate,ForestType) %>% 
  summarise(slope_mean = mean(slope,na.rm=T),
            grade_mean = mean(grade,na.rm=T) )
# 7.设置因子
summary_data$Climate <- factor(summary_data$Climate, levels = climate_types)
print(summary_data)

# > summary_data
# # Climate ForestType slope_mean grade_mean
# 1 Cfa     Naturally regenerating forest    -0.0379      0.458
# 2 Cfa     Primary forest                    0.0979      0.941
# 3 Cfa     Planted forest                   -0.303       0.657
# 4 Cfb     Naturally regenerating forest     0.284       0.663
# 5 Cfb     Primary forest                   -0.171       1.28 
# 6 Cfb     Planted forest                    0.131       0.803
# 7 Dfb     Naturally regenerating forest     0.367       0.716
# 8 Dfb     Primary forest                    0.129       0.541
# 9 Dfb     Planted forest                    0.551       0.878
# 10 Dfc     Naturally regenerating forest     0.103       0.534
# 11 Dfc     Primary forest                    0.233       0.536
# 12 Dfc     Planted forest                   -0.131       0.530
# 13 Dwb     Naturally regenerating forest     0.170       0.371
# 14 Dwb     Primary forest                    0.245       0.404
# 15 Dwb     Planted forest                    0.242       0.412
# 16 Dwc     Naturally regenerating forest    -0.0883      0.367
# 17 Dwc     Primary forest                   -0.0496      0.295
# 18 Dwc     Planted forest                    0.311       0.519

#######  EOS 趋势数据计算--Table 
library(dplyr)
library(tidyr)

# 计算每种 Climate 和 ForestType 的均值和 0.15 标准差
result <- all_data %>%
  group_by(Climate, ForestType) %>%
  summarise(
    mean = mean(slope, na.rm = TRUE),   # 计算均值
    sd = sd(slope, na.rm = TRUE),      # 计算标准差
    .groups = "drop"                        # 解除分组
  ) %>%
  mutate(
    `0.15sd` = sd * 0.15,                   # 计算 0.15 标准差
    Value = sprintf("%.2f ± %.2f", mean, `0.15sd`) # 格式化为 "mean ± 0.15sd"
  ) %>%
  ungroup() %>%                             # 确保解除分组
  dplyr::select(Climate, ForestType, Value) %>%    # 显式调用 dplyr::select
  pivot_wider(names_from = ForestType, values_from = Value) # 转换为宽表格

print(result)
# Climate `Naturally regenerating forest` `Primary forest` `Planted forest`
# 1 Cfa     -0.04 ± 0.10     0.10 ± 0.15                     -0.30 ± 0.11    
# 2 Cfb     0.28 ± 0.13      -0.17 ± 0.27                    0.13 ± 0.17     
# 3 Dfb     0.37 ± 0.11      0.13 ± 0.10                     0.55 ± 0.11     
# 4 Dfc     0.10 ± 0.11      0.23 ± 0.10                     -0.13 ± 0.13    
# 5 Dwb     0.17 ± 0.06      0.25 ± 0.05                     0.24 ± 0.06     
# 6 Dwc     -0.09 ± 0.06     -0.05 ± 0.06                    0.31 ± 0.05   


# 计算 Kruskal-Wallis Test
kw_results <- all_data %>%
  group_by(Climate) %>%
  summarise(
    p_value = kruskal.test(slope ~ ForestType, data = across(everything()))$p.value
  ) %>%
  mutate(significance = case_when(
    p_value <= 0.001 ~ "***",
    p_value <= 0.01 ~ "**",
    p_value <= 0.05 ~ "*",
    TRUE ~ "NS"
  ))
print(kw_results)
# Climate  p_value significance
# 1 Cfa     2.93e- 44 ***         
# 2 Cfb     4.01e- 19 ***         
# 3 Dfb     6.55e- 61 ***         
# 4 Dfc     3.49e-188 ***         
# 5 Dwb     4.32e-  2 *           
# 6 Dwc     8.62e- 27 *** 

# 8.作图
# 定义显著性符号函数
get_significance_label <- function(p) {
  if (p <= 0.001) "***" else if (p <= 0.01) "**" else if (p <= 0.05) "*" else "ns"
}

# 显著性计算并生成标记数据
significance_plot_data <- all_data %>%
  group_by(Climate) %>%
  summarise(
    p1 = t.test(slope[ForestType == "Primary forest"], slope[ForestType == "Naturally regenerating forest"])$p.value,
    p2 = t.test(slope[ForestType == "Primary forest"], slope[ForestType == "Planted forest"])$p.value,
    p3 = t.test(slope[ForestType == "Naturally regenerating forest"], slope[ForestType == "Planted forest"])$p.value,
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    label1 = get_significance_label(p1),
    label2 = get_significance_label(p2),
    label3 = get_significance_label(p3)
  ) %>%
  ungroup() %>%  # 移除 rowwise 分组
  pivot_longer(
    cols = starts_with("label"),
    names_to = "Comparison",
    values_to = "Significance"
  ) %>%
  mutate(
    x_start = ifelse(Comparison == "label1", 1, ifelse(Comparison == "label2", 1, 2)),
    x_end = ifelse(Comparison == "label1", 2, 3),
    y = ifelse(Comparison == "label1", 3.7, ifelse(Comparison == "label2", 4.5, 5.3))   # 向上平移 0.5
  )

################# 绘图 EOS-trend-map  ####################

g1 <- ggplot(all_data, aes(x = slope, color = ForestType, fill = ForestType)) +
  # geom_density(aes(y = ..scaled..), alpha = 0.3, linewidth = 1) +  # 使用相对频率分布，绘制密度曲线，添加透明度和线条宽度
  geom_density( alpha = 0.3, linewidth = 1) +  # 使用相对频率分布，绘制密度曲线，添加透明度和线条宽度
  facet_wrap(~Climate,  ncol=1, switch = "y",scales = "free")+
  scale_color_manual(values = c(
    "Primary forest" = "#2E8B57",                # 原生林--深绿色
    "Naturally regenerating forest" = "#9ACD32", # 次生林--浅绿色
    "Planted forest" = "#FFA500"                 # 人工林--橙色
  )) +
  scale_fill_manual(values = c(
    "Primary forest" = "#2E8B57",
    "Naturally regenerating forest" = "#9ACD32",
    "Planted forest" = "#FFA500"
  )) +
  labs(title = "",
       x = expression("Trend of EOS (days yr"^-1*")"),
       y = NULL,  # 移除Y轴标题
       color = "Forest type",fill = "Forest type" ) +
  theme_minimal() +
  theme( legend.position = "none",                # 隐藏图例
         axis.text.x = element_text(size = 11),   # 强制显示所有分面的X轴刻度文字
         axis.text.y = element_text(size = 11),   # 纵坐标刻度文字
         axis.title.x = element_text(size = 12),  # 横坐标标题文字
         axis.title.y = element_blank(),          # 移除Y轴标题
         strip.text.y.left = element_text(size = 12), # 左侧分面标题文字
         strip.placement = "outside",             # 将分面标题放置到图的外侧
         # panel.spacing = unit(1, "lines"),        # 设置面板间距
         panel.grid.major = element_blank(),      # 去掉主网格线
         panel.grid.minor = element_blank(),      # 去掉次网格线
         axis.line = element_line(linewidth = 0.4, color = "black"), # 添加轴线
         strip.background = element_blank(),      # 去掉分面背景
         panel.border = element_blank(),          # 去掉面板边框
         panel.grid = element_blank() ,           # 去掉所有网格线
         panel.spacing = unit(0.2, "lines"),     # 缩小分面间距
         plot.margin = unit(c(0.1, 0.5, 0.2, 0.2), "lines") # 缩小上下左右边距
         # aspect.ratio = 1 / 2.3                   # 设置长宽比为 1:2.5
  ) +
  coord_cartesian(xlim = c(-3.0, 4.0), ylim = c(0, 2.5)) +  # 设置横纵坐标范围
  scale_x_continuous( breaks = seq(-2.0, 4.0, by = 2.0),
                      labels = scales::number_format(accuracy = 0.1),  # 设置为显示一位小数
                      guide = guide_axis(check.overlap = TRUE)  # 确保每个分面都显示X轴标签
  ) +
  # scale_y_continuous(breaks = NULL) +         # 隐藏X轴的刻度
  scale_y_continuous(breaks = seq(0.0, 2.0, by = 1.0),
                     labels = scales::number_format(accuracy = 0.1) ) +    # 设置纵坐标刻度
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4)   # 添加x=0的虚线，设置size
# geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50", linewidth = 0.4)  # 添加y=0.5的虚线，设置size
# scale_y_continuous(limits = c(0.0, 1.8), breaks = seq(0.0, 2.0, by = 0.5)) 

g1

ggsave(
  filename = "./0.figure/03 Paper/Fig.DOY_trend_Densityplot_GFT_sub_EOS-Frequen.tiff",
  plot = g1,  width = 2.4,  height = 7,  units = "in",  dpi = 300)

############ --显著性分类地图EOS #########
library(dplyr)

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
wr <- map_data("world") %>%
  filter(lat > 20)

x_lines <- seq(-120,180, by = 60)# Defines the x axes required

library(ggplot2)
library(dplyr)

# 假设all_data包含你计算出的class列，并且df是你绘制地图的原始数据
wr <- map_data("world") %>%
  filter(lat > 20)

x_lines <- seq(-120, 180, by = 60)  # Defines the x axes required

all_data$class <- factor(all_data$class, levels = c("-2", "-1", "0", "1", "2"))

################# 绘图 Classified_map_EOS  ####################

p1 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    # 画地图背景
  geom_tile(data = all_data, aes(x = Longitude , y = Latitude , fill = class)) +  # 使用class列填充地图
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "gray40", fill = alpha("gray40", 0), linewidth = 0.3) +
  expand_limits(x = wr$long, y = wr$lat) +  # 指定经纬度范围
  scale_fill_manual(
    values = c("-2" = "#CC3333",   # 显著提前
               "-1" = "#FC8D59",   # 轻微提前
               "0"  = "#FFFFCC",   # 稳定不变
               "1"  = "lightblue", # 轻微延迟
               "2"  = "#006699" ),        # 显著延迟
    na.value = "transparent",  # 对于缺失值设置透明
    name = "Trend",  # 设置图例标题为 "Trend"
    labels = c("-2" = "Significant advance",  # 图例标签
               "-1" = "Slight advance", 
               "0" = "Stable", 
               "1" = "Slight delay", 
               "2" = "Significant delay"),  # 对应英文标签
    guide = guide_legend(
      title.position = "left", 
      title.hjust = 0,  # 标题左对齐
      label.position = "right",
      title.vjust = 0.5,
      label.vjust = 0.5,
      keyheight = 0.7,  # 设置色块高度，减小为扁平
      keywidth = 0.8,     # 设置色块宽度，增大为宽扁
      direction = "horizontal",  # 使图例横向排列
      position = "bottom",  # 将图例放置在下方
      label.spacing = unit(0.05, "cm")  # 缩小标签间距
      # reverse = TRUE  # 反转图例顺序
    )) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.title = element_text(size = 11.5, hjust = 0),  # 标题左对齐
        legend.text = element_text(size = 11.5, vjust = 0.5),
        legend.position = c(0.5, -0.06),  # 图例放置在下方并稍微下移
        legend.direction = "vertical") +  # 图例竖向排列
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  geom_hline(aes(yintercept = 20), size = 0.1) +  
  geom_segment(size = 0.4 ,aes(y = 20, yend = 90, x = x_lines, xend = x_lines),colour = "gray40", linetype = "dashed") +  # 经线
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  # 纬线20
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = 180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  #30
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x =  180, xend = 0), colour = "gray40",linetype = "dashed") + 
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  #50
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x =  180, xend = 0), colour = "gray40",linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40",linetype = "dashed") +  #70
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x =  180, xend = 0), colour = "gray40",linetype = "dashed") +
  # Adds labels
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 12) +
  # geom_text(aes( y = 15, x = x_lines,
  #                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 10) +
  geom_text(aes( y = 15+ c(-2.5, -2.5, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 12) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # 如果要显示北极投影

p1
# 保存图像
ggsave(
  filename = "./0.figure/03 Paper/Fig.4-b.Classified_map_EOS.tiff",
  plot = p1, width = 15, height = 15, units = "in", dpi = 300)


################# 绘图 precentage_barplot_EOS  ####################

# 加载必要的包
library(ggplot2)
library(dplyr)

# 假设你的数据框名为 all_data
# 计算每个 class 的百分比
class_percentages <- all_data %>%
  group_by(class) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# 定义颜色
class_colors <- c("-2" = "#CC3333", 
                  "-1" = "#FC8D59" , 
                  "0"  = "#FFFFCC", 
                  "1"  = "lightblue", 
                  "2"  = "#006699")

class_percentages$class <- factor(class_percentages$class, levels = c( "2","1","0", "-1","-2"))

# 创建直方图

g1 <- ggplot(class_percentages, aes(x = as.factor(class), y = percentage, fill = as.factor(class))) +
  # geom_bar(stat = "identity", width = 0.7) +  # 调整条形宽度
  geom_bar(stat = "identity", width = 0.75, color = "black", linewidth = 1.0) +  # 添加黑色框线
  geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5, size = 25, color = "black") +  # 调整文本大小和位置
  scale_fill_manual(values = class_colors) +  # 使用自定义颜色
  labs(x = "", y = "Percentage (%)", title = "") +  # 设置轴标题
  theme_bw() +  # 使用黑白主题
  theme(
    panel.grid.major.x = element_blank(),  # 隐藏x轴主要网格线
    panel.grid.minor.x = element_blank(),  # 隐藏x轴次要网格线
    panel.grid.major.y = element_blank(),  # 设置y轴网格线 #element_line(color = "gray80", size = 0.5), 
    panel.grid.minor.y = element_blank(),  # 隐藏y轴次要网格线
    panel.border = element_rect(size = 2.5, color = "black"),  # 设置边框大小和颜色
    axis.text.y = element_text(size = 72, color = "black"),  # 设置y轴文字大小
    axis.ticks.y = element_line(size = 2.5, color = "black"),  # 设置y轴刻度线
    axis.ticks.length.y = unit(0.3, "cm"),  # 设置y轴刻度长度
    axis.title.y = element_text(size = 81, margin = margin(r = 20)),  # 设置y轴标题大小和边距
    axis.title.x = element_blank(),  # 隐藏x轴标题
    axis.text.x = element_blank(),  # 隐藏x轴文字
    axis.ticks.x = element_blank(),  # 隐藏x轴刻度
    plot.background = element_rect(fill = "white", color = NA),  # 设置背景颜色
    plot.margin = margin(t = 20, b = 10, l = 10, r = 20, unit = "pt"),  # 设置边距
    legend.position = "none") + # 隐藏图例
    # legend.position = "bottom") + # 隐藏图例
  scale_y_continuous(limits = c(0, 75), breaks = seq(0, 75, by = 20))   # 设置 y 轴范围和刻度

print(g1)
ggsave(
  filename = "./0.figure/03 Paper/Fig.4-b.Classified_barplot_EOS.tiff",
  plot = g1, width = 16,  height = 9.5,  units = "in",  dpi = 300)
