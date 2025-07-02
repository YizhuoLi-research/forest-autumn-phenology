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
forest_types <- c(1, 2, 3)  # 1: 自然再生林； 2：原生林；   3：人工林
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

# summary(all_data)

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

#######  GDO 趋势数据计算--TableS1  #####
library(dplyr)
library(tidyr)

# 计算每种 Climate 和 ForestType 的均值和 0.15 标准差

result <- all_data %>%
  group_by(Climate, ForestType) %>%
  summarise(
    mean = mean(slope, na.rm = TRUE),
    sd = sd(slope, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    `0.15sd` = sd * 0.15,
    Value = sprintf("%.2f ± %.2f", mean, `0.15sd`)
  ) %>%
  dplyr::select(Climate, ForestType, Value)

# 再对所有森林计算 (不分 ForestType)
result_all <- all_data %>%
  group_by(Climate) %>%
  summarise(
    mean = mean(slope, na.rm = TRUE),
    sd = sd(slope, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    `0.15sd` = sd * 0.15,
    Value = sprintf("%.2f ± %.2f", mean, `0.15sd`),
    ForestType = "All forests"
  ) %>%
  dplyr::select(Climate, ForestType, Value)

# 合并两部分结果
result_combined <- bind_rows(result, result_all) %>%
  pivot_wider(names_from = ForestType, values_from = Value)

# 如果你想保持 Climate 顺序一致：
climate_types <- c("Cfa", "Cfb", "Dfb", "Dfc", "Dwb", "Dwc")
result_combined$Climate <- factor(result_combined$Climate, levels = climate_types)
result_combined <- result_combined %>% arrange(Climate)

# 打印最终结果
print(result_combined)  #GDO  

# A tibble: 6 × 5
# Climate `Naturally regenerating forest` `Primary forest` `Planted forest` `All forests`
# 1 Cfa     0.03 ± 0.10                     0.04 ± 0.14      -0.28 ± 0.10     -0.07 ± 0.10 
# 2 Cfb     0.04 ± 0.13                     0.68 ± 0.18      0.25 ± 0.17      0.16 ± 0.15  
# 3 Dfb     0.28 ± 0.10                     0.02 ± 0.12      0.45 ± 0.12      0.26 ± 0.11  
# 4 Dfc     0.20 ± 0.14                     -0.16 ± 0.10     1.29 ± 0.14      -0.02 ± 0.12 
# 5 Dwb     -0.21 ± 0.08                    -0.16 ± 0.07     -0.09 ± 0.10     -0.18 ± 0.08 
# 6 Dwc     -0.60 ± 0.09                    -0.36 ± 0.10     -0.11 ± 0.10     -0.41 ± 0.10 

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

############### 绘图--GDO trend ######################

g1 <- ggplot(all_data, aes(x = slope, color = ForestType, fill = ForestType)) +
  # geom_density(aes(y = ..scaled..), alpha = 0.3, linewidth = 1) +  # 使用相对频率分布，绘制密度曲线，添加透明度和线条宽度
  geom_density( alpha = 0.3, linewidth = 1) +  # 使用相对频率分布，绘制密度曲线，添加透明度和线条宽度
  facet_wrap(~Climate,  nrow=1, switch = "y",scales = "free")+
  facet_wrap(~Climate, nrow = 1, switch = "y", scales = "free") +
  scale_color_manual(
    limits = c("Primary forest", "Naturally regenerating forest", "Planted forest"),
    values = c(
      "Primary forest" = "#2E8B57",                # 原生林--深绿色
      "Naturally regenerating forest" = "#9ACD32", # 次生林--浅绿色
      "Planted forest" = "#FFA500"                 # 人工林--橙色
    ),
    labels = c(
      "Primary forest" = "PF",                     # 图例缩写为 PF
      "Naturally regenerating forest" = "NRF",     # 图例缩写为 NRF
      "Planted forest" = "P"                       # 图例缩写为 P
    )
  ) +
  scale_fill_manual(
    limits = c("Primary forest", "Naturally regenerating forest", "Planted forest"),
    values = c(
      "Primary forest" = "#2E8B57",
      "Naturally regenerating forest" = "#9ACD32",
      "Planted forest" = "#FFA500"
    ),
    labels = c(
      "Primary forest" = "PF",                     # 图例缩写为 PF
      "Naturally regenerating forest" = "NRF",     # 图例缩写为 NRF
      "Planted forest" = "P"                       # 图例缩写为 P
    )
  ) +
  labs(
    title = "",
    x = expression("Trend of GDO (days yr"^-1*")"),
    y = NULL,  # 移除Y轴标题
    color = "Forest type", 
    fill = "Forest type"
  ) +
  theme_minimal() +
  theme( #legend.position = "none",                # 隐藏图例
    legend.position = "right",  # 设置图例位置为右侧
    legend.direction = "vertical",  # 设置图例为竖向排列
    legend.title = element_text(size = 17),  # 图例标题字体大小
    legend.text = element_text(size = 17), 
    axis.text.x = element_text(size = 17),   # 强制显示所有分面的X轴刻度文字
    axis.text.y = element_text(size = 17),   # 纵坐标刻度文字
    axis.title.x = element_text(size = 19),  # 横坐标标题文字
    axis.title.y = element_blank(),          # 移除Y轴标题
    strip.text.y.left = element_text(size = 19), # 左侧分面标题文字
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
  filename = "./0.figure/03 Paper/Fig.3-a.DOY_trend_Densityplot_GFT_sub_GDO-Frequen_new.tiff",
  plot = g1,  width = 16,  height = 2.2,  units = "in",  dpi = 300)



############################### 002 按三级气候分区作图  EOS   #################################      
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
forest_types <- c(1, 2, 3)  # 1: 自然再生林, 2: 原生林, 3: 人工林
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
                              labels = c("Naturally regenerating forest","Primary forest", "Planted forest"))

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

#######  EOS 趋势数据计算--Table 
library(dplyr)
library(tidyr)

# 计算每种 Climate 和 ForestType 的均值和 0.15 标准差

result <- all_data %>%
  group_by(Climate, ForestType) %>%
  summarise(
    mean = mean(slope, na.rm = TRUE),
    sd = sd(slope, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    `0.15sd` = sd * 0.15,
    Value = sprintf("%.2f ± %.2f", mean, `0.15sd`)
  ) %>%
  dplyr::select(Climate, ForestType, Value)

# 再对所有森林计算 (不分 ForestType)
result_all <- all_data %>%
  group_by(Climate) %>%
  summarise(
    mean = mean(slope, na.rm = TRUE),
    sd = sd(slope, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    `0.15sd` = sd * 0.15,
    Value = sprintf("%.2f ± %.2f", mean, `0.15sd`),
    ForestType = "All forests"
  ) %>%
  dplyr::select(Climate, ForestType, Value)

# 合并两部分结果
result_combined <- bind_rows(result, result_all) %>%
  pivot_wider(names_from = ForestType, values_from = Value)

# 如果你想保持 Climate 顺序一致：
climate_types <- c("Cfa", "Cfb", "Dfb", "Dfc", "Dwb", "Dwc")
result_combined$Climate <- factor(result_combined$Climate, levels = climate_types)
result_combined <- result_combined %>% arrange(Climate)

# 打印最终结果
print(result_combined)  #EOS

# Climate `Naturally regenerating forest` `Primary forest` `Planted forest` `All forests`
# 1 Cfa     -0.04 ± 0.10                    0.10 ± 0.15      -0.30 ± 0.11     -0.12 ± 0.11 
# 2 Cfb     0.28 ± 0.13                     -0.17 ± 0.27     0.13 ± 0.17      0.19 ± 0.15  
# 3 Dfb     0.37 ± 0.11                     0.13 ± 0.10      0.55 ± 0.11      0.36 ± 0.11  
# 4 Dfc     0.10 ± 0.11                     0.23 ± 0.10      -0.13 ± 0.13     0.19 ± 0.10  
# 5 Dwb     0.17 ± 0.06                     0.25 ± 0.05      0.24 ± 0.06      0.20 ± 0.05  
# 6 Dwc     -0.09 ± 0.06                    -0.05 ± 0.06     0.31 ± 0.05      -0.05 ± 0.06 

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
#   1 Cfa     1.19e-31 ***         
#   2 Cfb     1.61e- 7 ***         
#   3 Dfb     6.24e-62 ***         
#   4 Dfc     7.38e-31 ***         
#   5 Dwb     1.07e- 3 **          
#   6 Dwc     9.30e-18 *** 

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

############### 绘图--EOS trend ######################

g2 <- ggplot(all_data, aes(x = slope, color = ForestType, fill = ForestType)) +
  geom_density(alpha = 0.3, linewidth = 1) +  # 使用相对频率分布，绘制密度曲线，添加透明度和线条宽度
  facet_wrap(~Climate, nrow = 1, switch = "y", scales = "free") +
  scale_color_manual(
    limits = c("Primary forest", "Naturally regenerating forest", "Planted forest"), # 控制图例顺序
    values = c(
      "Primary forest" = "#2E8B57",                # 原生林--深绿色
      "Naturally regenerating forest" = "#9ACD32", # 次生林--浅绿色
      "Planted forest" = "#FFA500"                 # 人工林--橙色
    ),
    labels = c(
      "Primary forest" = "PF",                     # 图例缩写为 PF
      "Naturally regenerating forest" = "NRF",     # 图例缩写为 NRF
      "Planted forest" = "P"                       # 图例缩写为 P
    )
  ) +
  scale_fill_manual(
    limits = c("Primary forest", "Naturally regenerating forest", "Planted forest"), # 控制图例顺序
    values = c(
      "Primary forest" = "#2E8B57",
      "Naturally regenerating forest" = "#9ACD32",
      "Planted forest" = "#FFA500"
    ),
    labels = c(
      "Primary forest" = "PF",                     # 图例缩写为 PF
      "Naturally regenerating forest" = "NRF",     # 图例缩写为 NRF
      "Planted forest" = "P"                       # 图例缩写为 P
    )
  ) +
  labs(
    title = "",
    x = expression("Trend of EOS (days yr"^-1*")"),
    y = NULL,  # 移除Y轴标题
    color = "Forest type", 
    fill = "Forest type"
  ) +
  theme_minimal() +
  theme( #legend.position = "none",                # 隐藏图例
    legend.position = "right",  # 设置图例位置为右侧
    legend.direction = "vertical",  # 设置图例为竖向排列
    legend.title = element_text(size = 17),  # 图例标题字体大小
    legend.text = element_text(size = 17), 
         axis.text.x = element_text(size = 17),   # 强制显示所有分面的X轴刻度文字
         axis.text.y = element_text(size = 17),   # 纵坐标刻度文字
         axis.title.x = element_text(size = 19),  # 横坐标标题文字
         axis.title.y = element_blank(),          # 移除Y轴标题
         strip.text.y.left = element_text(size = 19), # 左侧分面标题文字
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

g2

ggsave(
  filename = "./0.figure/03 Paper/Fig.3-b.DOY_trend_Densityplot_GFT_sub_EOS-Frequen_new.tiff",
  plot = g2,  width = 16,  height = 2.2,  units = "in",  dpi = 300)

############################### 003 按三级气候分区作图  SOS-TableS1   #################################      
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
forest_types <- c(1, 2, 3)  # 1: 自然再生林； 2：原生林；   3：人工林
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

# summary(all_data)

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

#######  SOS 趋势数据计算--Table 
library(dplyr)
library(tidyr)

# 计算每种 Climate 和 ForestType 的均值和 0.15 标准差
# 先对三种林型分别计算
result <- all_data %>%
  group_by(Climate, ForestType) %>%
  summarise(
    mean = mean(slope, na.rm = TRUE),
    sd = sd(slope, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    `0.15sd` = sd * 0.15,
    Value = sprintf("%.2f ± %.2f", mean, `0.15sd`)
  ) %>%
  dplyr::select(Climate, ForestType, Value)

# 再对所有森林计算 (不分 ForestType)
result_all <- all_data %>%
  group_by(Climate) %>%
  summarise(
    mean = mean(slope, na.rm = TRUE),
    sd = sd(slope, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    `0.15sd` = sd * 0.15,
    Value = sprintf("%.2f ± %.2f", mean, `0.15sd`),
    ForestType = "All forests"
  ) %>%
  dplyr::select(Climate, ForestType, Value)

# 合并两部分结果
result_combined <- bind_rows(result, result_all) %>%
  pivot_wider(names_from = ForestType, values_from = Value)

# 如果你想保持 Climate 顺序一致：
climate_types <- c("Cfa", "Cfb", "Dfb", "Dfc", "Dwb", "Dwc")
result_combined$Climate <- factor(result_combined$Climate, levels = climate_types)
result_combined <- result_combined %>% arrange(Climate)

# 打印最终结果
print(result_combined)  #SOS
# Climate `Naturally regenerating forest` `Primary forest` `Planted forest` `All forests`
# 1 Cfa     -0.28 ± 0.12                    0.25 ± 0.19      -0.55 ± 0.09     -0.36 ± 0.12 
# 2 Cfb     0.50 ± 0.15                     0.95 ± 0.31      0.15 ± 0.17      0.32 ± 0.17  
# 3 Dfb     -0.18 ± 0.11                    -0.50 ± 0.13     0.05 ± 0.08      -0.19 ± 0.11 
# 4 Dfc     -0.04 ± 0.15                    -0.12 ± 0.15     0.27 ± 0.11      -0.09 ± 0.15 
# 5 Dwb     -0.16 ± 0.08                    -0.16 ± 0.08     -0.04 ± 0.08     -0.15 ± 0.08 
# 6 Dwc     0.10 ± 0.11                     0.27 ± 0.10      0.12 ± 0.05      0.23 ± 0.10    

############################### 004 按三级气候分区作图  GMO-TableS1   #################################      
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
forest_types <- c(1, 2, 3)  # 1: 自然再生林； 2：原生林；   3：人工林
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

# summary(all_data)

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

#######  GMO 趋势数据计算--Table 
library(dplyr)
library(tidyr)

# 计算每种 Climate 和 ForestType 的均值和 0.15 标准差

result <- all_data %>%
  group_by(Climate, ForestType) %>%
  summarise(
    mean = mean(slope, na.rm = TRUE),
    sd = sd(slope, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    `0.15sd` = sd * 0.15,
    Value = sprintf("%.2f ± %.2f", mean, `0.15sd`)
  ) %>%
  dplyr::select(Climate, ForestType, Value)

# 再对所有森林计算 (不分 ForestType)
result_all <- all_data %>%
  group_by(Climate) %>%
  summarise(
    mean = mean(slope, na.rm = TRUE),
    sd = sd(slope, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    `0.15sd` = sd * 0.15,
    Value = sprintf("%.2f ± %.2f", mean, `0.15sd`),
    ForestType = "All forests"
  ) %>%
  dplyr::select(Climate, ForestType, Value)

# 合并两部分结果
result_combined <- bind_rows(result, result_all) %>%
  pivot_wider(names_from = ForestType, values_from = Value)

# 如果你想保持 Climate 顺序一致：
climate_types <- c("Cfa", "Cfb", "Dfb", "Dfc", "Dwb", "Dwc")
result_combined$Climate <- factor(result_combined$Climate, levels = climate_types)
result_combined <- result_combined %>% arrange(Climate)

# 打印最终结果
print(result_combined)  #GMO

# Climate `Naturally regenerating forest` `Primary forest` `Planted forest` `All forests`
# 1 Cfa     0.16 ± 0.09                     0.24 ± 0.12      -0.16 ± 0.11     0.06 ± 0.10  
# 2 Cfb     0.14 ± 0.15                     0.51 ± 0.22      0.14 ± 0.17      0.15 ± 0.16  
# 3 Dfb     0.14 ± 0.14                     -0.43 ± 0.13     0.62 ± 0.13      0.12 ± 0.14  
# 4 Dfc     -0.05 ± 0.13                    -0.26 ± 0.11     0.72 ± 0.12      -0.18 ± 0.12 
# 5 Dwb     -0.01 ± 0.08                    0.09 ± 0.05      -0.08 ± 0.10     0.02 ± 0.07  
# 6 Dwc     -0.07 ± 0.08                    0.04 ± 0.08      0.15 ± 0.04      0.02 ± 0.08 


##简单线性最小二乘法
# # 3.定义趋势分析函数
# trend_analysis <- function(values) {
#   if (all(is.na(values))) return(NA)
#   model <- lm(values ~ years)
#   return(coef(model)[2])
# }
# 
# # 4.创建空列表用于存储结果
# trend_rasters <- list()
# 
# # 5.循环处理每种气候类型和森林类型
# for (climate in climate_types) {
#   # 提取气候区域的DOY栅格
#   doy_stack <- do.call(c, lapply(1:nlyr(r1), function(i) {
#     mask(r1[[i]], classify_border[classify_border$EA_koppen_30km_addClimate %in% climate, ], touches = FALSE)
#   }))
#   
#   # 提取气候区域的森林类型
#   forest_layer <- mask(r2, classify_border[classify_border$EA_koppen_30km_addClimate %in% climate, ])
#   
#   for (forest in forest_types) {
#     # 根据林类值掩膜
#     forest_doy_stack <- mask(doy_stack, ifel(forest_layer == forest, 1, NA))
#     # 应用趋势分析函数
#     trend_raster <- app(forest_doy_stack, trend_analysis)
#     # 保存结果到列表
#     trend_rasters[[paste0("trend_raster_", climate, "_forest", forest)]] <- trend_raster
#   }
# }
# 
# # 6.提取栅格数据并合并为数据框
# all_data <- do.call(rbind, lapply(names(trend_rasters), function(name) {
#   raster_df <- as.data.frame(trend_rasters[[name]], xy = TRUE)
#   colnames(raster_df) <- c("Longitude", "Latitude", "TrendValue")
#   # 提取气候和森林信息
#   parts <- strsplit(name, "_")[[1]]
#   raster_df$Climate <- parts[3]
#   raster_df$ForestType <- parts[4]
#   return(raster_df)
# }))
# 
# # 7.设置因子
# all_data$Climate <- factor(all_data$Climate, levels = climate_types)
# all_data$ForestType <- factor(all_data$ForestType, 
#                               levels = c("forest1", "forest2", "forest3"),
#                               labels = c("Primary forest", "Naturally regenerating forest", "Planted forest"))
# 
# head(all_data)
# 
# 
# #######  SOS 趋势数据计算--Table ######
# library(dplyr)
# library(tidyr)
# 
# # 计算每种 Climate 和 ForestType 的均值和 0.15 标准差
# # result <- all_data %>%
# #   group_by(Climate, ForestType) %>%
# #   summarise(
# #     mean = mean(TrendValue, na.rm = TRUE),   # 计算均值
# #     sd = sd(TrendValue, na.rm = TRUE),      # 计算标准差
# #     .groups = "drop"                        # 解除分组
# #   ) %>%
# #   mutate(
# #     `0.15sd` = sd * 0.15,                   # 计算 0.15 标准差
# #     Value = sprintf("%.2f ± %.2f", mean, `0.15sd`) # 格式化为 "mean ± 0.15sd"
# #   ) %>%
# #   ungroup() %>%                             # 确保解除分组
# #   dplyr::select(Climate, ForestType, Value) %>%    # 显式调用 dplyr::select
# #   pivot_wider(names_from = ForestType, values_from = Value) # 转换为宽表格
# # 
# # # print(result)
# # # 将宽表格转置
# # result_transposed <- result %>%
# #   pivot_longer(cols = -Climate, names_to = "ForestType", values_to = "Value") %>%
# #   pivot_wider(names_from = Climate, values_from = Value)
# # 
# # # 查看转置结果
# # print(result_transposed)
# # # 查看结果
# # # Climate `Primary forest` `Naturally regenerating forest` `Planted forest`
# # # 1 Cfa     -0.31 ± 0.15     0.09 ± 0.16                     -0.54 ± 0.13
# # # 2 Cfb     0.34 ± 0.20      1.48 ± 0.48                     -0.04 ± 0.23
# # # 3 Dfb     -0.25 ± 0.14     -0.64 ± 0.21                    0.04 ± 0.13
# # # 4 Dfc     -0.06 ± 0.29     -0.18 ± 0.22                    0.20 ± 0.13
# # # 5 Dwb     -0.18 ± 0.12     -0.16 ± 0.08                    -0.03 ± 0.08
# # # 6 Dwc     0.10 ± 0.15      0.30 ± 0.14                     0.14 ± 0.05
# 
# 
# # 计算 Kruskal-Wallis Test
# # kw_results <- all_data %>%
# #   group_by(Climate) %>%
# #   summarise(
# #     p_value = kruskal.test(TrendValue ~ ForestType, data = cur_data())$p.value
# #   ) %>%
# #   mutate(significance = case_when(
# #     p_value <= 0.001 ~ "***",
# #     p_value <= 0.01 ~ "**",
# #     p_value <= 0.05 ~ "*",
# #     TRUE ~ "NS"
# #   ))
# # # Climate   p_value significance
# # # <fct>       <dbl> <chr>       
# # #   1 Cfa     1.54e- 19 ***         
# # #   2 Cfb     7.62e- 48 ***         
# # #   3 Dfb     4.43e-150 ***         
# # #   4 Dfc     2.92e- 20 ***         
# # #   5 Dwb     9.62e-  4 ***         
# # #   6 Dwc     5.26e- 26 ***  
# 
# # 8.作图
# # 定义显著性符号函数
# get_significance_label <- function(p) {
#   if (p <= 0.001) "***" else if (p <= 0.01) "**" else if (p <= 0.05) "*" else "NS"
# }
# 
# # 显著性计算并生成标记数据
# significance_plot_data <- all_data %>%
#   group_by(Climate) %>%
#   summarise(
#     p1 = t.test(TrendValue[ForestType == "Primary forest"], TrendValue[ForestType == "Naturally regenerating forest"])$p.value,
#     p2 = t.test(TrendValue[ForestType == "Primary forest"], TrendValue[ForestType == "Planted forest"])$p.value,
#     p3 = t.test(TrendValue[ForestType == "Naturally regenerating forest"], TrendValue[ForestType == "Planted forest"])$p.value,
#     .groups = "drop"
#   ) %>%
#   rowwise() %>%
#   mutate(
#     label1 = get_significance_label(p1),
#     label2 = get_significance_label(p2),
#     label3 = get_significance_label(p3)
#   ) %>%
#   ungroup() %>%  # 移除 rowwise 分组
#   pivot_longer(
#     cols = starts_with("label"),
#     names_to = "Comparison",
#     values_to = "Significance"
#   ) %>%
#   mutate(
#     x_start = ifelse(Comparison == "label1", 1, ifelse(Comparison == "label2", 1, 2)),
#     x_end = ifelse(Comparison == "label1", 2, 3),
#     y = ifelse(Comparison == "label1", 3.7, ifelse(Comparison == "label2", 4.5, 5.3))   # 向上平移 0.5
#   )
# # 绘图
# g1 <- ggplot(all_data, aes(x = TrendValue, color = ForestType, fill = ForestType)) +
#   # geom_density(aes(y = ..scaled..), alpha = 0.3, size = 1) +  # 使用相对频率分布，绘制密度曲线，添加透明度和线条宽度
#   geom_density( alpha = 0.3, size = 1) +  # 使用相对频率分布，绘制密度曲线，添加透明度和线条宽度
#   facet_wrap(~Climate,  ncol=1, switch = "y",scales = "free")+
#   scale_color_manual(values = c(
#     "Primary forest" = "#2E8B57",                # 原生林--深绿色
#     "Naturally regenerating forest" = "#9ACD32", # 次生林--浅绿色
#     "Planted forest" = "#FFA500"                 # 人工林--橙色
#   )) +
#   scale_fill_manual(values = c(
#     "Primary forest" = "#2E8B57",
#     "Naturally regenerating forest" = "#9ACD32",
#     "Planted forest" = "#FFA500"
#   )) +
#   labs(
#     title = "",
#     x = expression("Trend of SOS (days yr"^-1*")"),
#     y = NULL,  # 移除Y轴标题
#     color = "Forest type",
#     fill = "Forest type"
#   ) +
#   theme_minimal() +
#   theme(
#     legend.position = "none",                # 隐藏图例
#     axis.text.x = element_text(size = 11),   # 强制显示所有分面的X轴刻度文字
#     axis.text.y = element_text(size = 11),   # 纵坐标刻度文字
#     axis.title.x = element_text(size = 12),  # 横坐标标题文字
#     axis.title.y = element_blank(),          # 移除Y轴标题
#     strip.text.y.left = element_text(size = 12), # 左侧分面标题文字
#     strip.placement = "outside",             # 将分面标题放置到图的外侧
#     # panel.spacing = unit(1, "lines"),        # 设置面板间距
#     panel.grid.major = element_blank(),      # 去掉主网格线
#     panel.grid.minor = element_blank(),      # 去掉次网格线
#     axis.line = element_line(size = 0.4, color = "black"), # 添加轴线
#     strip.background = element_blank(),      # 去掉分面背景
#     panel.border = element_blank(),          # 去掉面板边框
#     panel.grid = element_blank() ,           # 去掉所有网格线
#     panel.spacing = unit(0.2, "lines"),     # 缩小分面间距
#     plot.margin = unit(c(0.1, 0.5, 0.2, 0.2), "lines") # 缩小上下左右边距
#     # aspect.ratio = 1 / 2.3                   # 设置长宽比为 1:2.5
#   ) +
#   coord_cartesian(xlim = c(-3.0, 4.0), ylim = c(0, 2.5)) +  # 设置横纵坐标范围
#   scale_x_continuous( breaks = seq(-2.0, 4.0, by = 2.0),
#                       labels = scales::number_format(accuracy = 0.1),  # 设置为显示一位小数
#                       guide = guide_axis(check.overlap = TRUE)  # 确保每个分面都显示X轴标签
#   ) +
#   # scale_y_continuous(breaks = NULL) +         # 隐藏X轴的刻度
#   scale_y_continuous(breaks = seq(0.0, 2.0, by = 1.0),
#                      labels = scales::number_format(accuracy = 0.1) ) +    # 设置纵坐标刻度
#   geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", size = 0.4)   # 添加x=0的虚线，设置size
# # geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey50", size = 0.4)  # 添加y=0.5的虚线，设置size
# # scale_y_continuous(limits = c(0.0, 1.8), breaks = seq(0.0, 2.0, by = 0.5)) 
# 
# g1
# 
# ggsave(
#   filename = "./0.figure/03 Paper/Fig.DOY_trend_Densityplot_GFT_sub_SOS-Frequen.tiff",
#   plot = g1,  width = 2.4,  height = 7,  units = "in",  dpi = 300)
