###### 0. 加载包 ####
library(terra)
library(tidyverse)
library(raster)

setwd("D:/Graduation_Thesis")

###############################################################################################
#############################   0. 计算不同气候区、不同GFC的DOY差异  ###########################
###############################################################################################

#############################   00.按照温度类型，将气候区分为7个气候区域   ####################
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


# 1. 掩膜Cfa区域边界，提取DOY和林类数据
Cfa_doy <- mask(r1, Cfa_border)       # 提取Cfa区域的DOY数据
Cfa_forest <- mask(r2, Cfa_border)    # 提取Cfa区域的林类数据

## 2. 根据林类值创建掩膜条件，提取DOY数据
# Cfa_forest1_DOY <- mask(Cfa_doy, ifel(Cfa_forest == 1, 1, NA))  # 原生林
# Cfa_forest2_DOY <- mask(Cfa_doy, ifel(Cfa_forest == 2, 1, NA))  # 次生林
# Cfa_forest3_DOY <- mask(Cfa_doy, ifel(Cfa_forest == 3, 1, NA))  # 人工林
# Cfa_forest0_DOY <- mask(Cfa_doy, ifel(Cfa_forest == 0, 1, NA))  # 0-非这三种林
# valid_pixels <- sum(!is.na(values(Cfa_forest3_DOY)))
# cat("有效像元数量：", valid_pixels, "\n")
# plot(Cfa_doy)


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

# 3. 遍历每个气候区域，提取 DOY 和林类数据
for (climate in names(climate_types)) {
  border <- climate_types[[climate]]
  
  # 掩膜提取 DOY 和林类数据
  doy_data <- mask(r1, border)
  forest_data <- mask(r2, border)
  
  # 根据林类值创建掩膜条件
  forest1_DOY <- mask(doy_data, ifel(forest_data == 1, 1, NA))  # 原生林
  forest2_DOY <- mask(doy_data, ifel(forest_data == 2, 1, NA))  # 次生林
  forest3_DOY <- mask(doy_data, ifel(forest_data == 3, 1, NA))  # 人工林
  
  # 转为数据框
  df_forest1 <- as.data.frame(forest1_DOY, xy = TRUE, na.rm = TRUE)
  df_forest2 <- as.data.frame(forest2_DOY, xy = TRUE, na.rm = TRUE)
  df_forest3 <- as.data.frame(forest3_DOY, xy = TRUE, na.rm = TRUE)
  
  # 添加林类标签
  df_forest1$ForestType <- "Primary forest"
  df_forest2$ForestType <- "Naturally regenerating forest"
  df_forest3$ForestType <- "Planted forest"
  
  # 合并并添加气候类型
  combined <- rbind(
    data.frame(DOY = df_forest1[, 3], ForestType = df_forest1$ForestType),
    data.frame(DOY = df_forest2[, 3], ForestType = df_forest2$ForestType),
    data.frame(DOY = df_forest3[, 3], ForestType = df_forest3$ForestType)
  )
  combined$Climate <- climate
  
  # 合并到总数据框
  all_data <- rbind(all_data, combined)
}

# 4. 将气候类型和林类设置为因子
all_data$Climate <- factor(all_data$Climate,levels = c("Cfa", "Cfb", "Dfb","Dfc","Dwb", "Dwc"))
all_data$ForestType <- factor(all_data$ForestType, levels = c("Primary forest", "Naturally regenerating forest", "Planted forest"))

# 5. 绘制组图
g1 <- ggplot(all_data, aes(x = ForestType, y = DOY, fill = ForestType,)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA) +
  stat_compare_means(method = "t.test", label = "p.signif",
                    size = 4, # 调整显著性标记的字体大小
                     comparisons = list(c("Primary forest", "Naturally regenerating forest"), 
                                        c("Primary forest", "Planted forest"), 
                                        c("Naturally regenerating forest", "Planted forest")),
                     label.y = c(160, 190, 220),  # 设置每个显著性标记的垂直位置
                     symnum.args = list(
                       cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                       symbols = c("***", "**", "*", "NS"))) +
  stat_summary(fun = mean, geom = "point", color = "darkred", size = 2, shape = 16) +  # 添加深红色平均值点
  geom_text(data = all_data %>% group_by(Climate, ForestType) %>% summarise(Count = n()), 
            aes(x = ForestType, y =20, label = paste0("n=", Count)), size = 2.6, 
            inherit.aes = FALSE) +
  facet_wrap(~ Climate, nrow = 1) +
  scale_fill_manual(values = c(
    "Primary forest" = "#2E8B57",                # 原生林--深绿色
    "Naturally regenerating forest" = "#9ACD32", # 次生林--浅绿色
    "Planted forest" = "#FFA500"                 # 人工林--橙色
  )) +
  scale_x_discrete(labels = c(
    "Primary forest" = "PF", 
    "Naturally regenerating forest" = "NRF", 
    "Planted forest" = "P")) +                   # 修改横坐标标签
  labs(#title = "DOY Distribution Across Climate Regions",
    title = "", x = "Forest type", y = "DOY", fill = "Forest type") +
  # theme_minimal() +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 11),        # 坐标轴刻度文字
    # axis.text.x = element_text(size = 12, angle = 10, vjust = 0.5),  # X 轴刻度文字向下移动
    axis.text.y = element_text(size = 11),        # 坐标轴刻度文字
    axis.title = element_text(size = 12),        # 坐标轴标题文字
    strip.text = element_text(size = 12),       # 分面标题
    # plot.title = element_text(size = 18, face = "bold"),  # 图标题
    # legend.title = element_text(size = 13),     # 图例标题
    # legend.text = element_text(size = 12)       # 图例文字
  ) +
  coord_cartesian(ylim = c(20, 240)) + scale_y_continuous(breaks = seq(20, 240, by = 50))

# 显示组图
print(g1)


ggsave(
  filename = "./0.figure/03 Chapter3/Fig.phe_DOY_barplot_GFT_sub_SOS.tiff",
  # plot = g1,  width = 8,  height = 6,  units = "in",  dpi = 300)
  plot = g1,  width = 9,  height = 2.6,  units = "in",  dpi = 300)

# ############################## 002 按三级气候分区作图  MGP   #########################################      

# # SOS: phe1_DOY ;  MGP: phe2_DOY ;  GMO：phe3_DOY
# # GDO: phe4_DOY ;  MSP: phe5_DOY ;  EOS：phe6_DOY

## 1. 读取数据
r1 <- rast("./EA+NA_Results/merged_Phe_DOY_mean_2_PHE_analysis/merged_10yr_mean_phe2_DOY.tif")
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
  
  forest1_DOY <- mask(doy_data, ifel(forest_data == 1, 1, NA))  # 原生林
  forest2_DOY <- mask(doy_data, ifel(forest_data == 2, 1, NA))  # 次生林
  forest3_DOY <- mask(doy_data, ifel(forest_data == 3, 1, NA))  # 人工林
  
  df_forest1 <- as.data.frame(forest1_DOY, xy = TRUE, na.rm = TRUE)
  df_forest2 <- as.data.frame(forest2_DOY, xy = TRUE, na.rm = TRUE)
  df_forest3 <- as.data.frame(forest3_DOY, xy = TRUE, na.rm = TRUE)
  
  df_forest1$ForestType <- "Primary forest"
  df_forest2$ForestType <- "Naturally regenerating forest"
  df_forest3$ForestType <- "Planted forest"
  
  combined <- rbind(
    data.frame(DOY = df_forest1[, 3], ForestType = df_forest1$ForestType),
    data.frame(DOY = df_forest2[, 3], ForestType = df_forest2$ForestType),
    data.frame(DOY = df_forest3[, 3], ForestType = df_forest3$ForestType)
  )
  combined$Climate <- climate
  
  all_data <- rbind(all_data, combined)
}

all_data$Climate <- factor(all_data$Climate,levels = c("Cfa", "Cfb", "Dfb","Dfc","Dwb", "Dwc"))
all_data$ForestType <- factor(all_data$ForestType, levels = c("Primary forest", "Naturally regenerating forest", "Planted forest"))

g2 <- ggplot(all_data, aes(x = ForestType, y = DOY, fill = ForestType,)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA) +
  stat_compare_means(method = "t.test", label = "p.signif",
                    size = 4, # 调整显著性标记的字体大小
                     comparisons = list(c("Primary forest", "Naturally regenerating forest"), 
                                        c("Primary forest", "Planted forest"), 
                                        c("Naturally regenerating forest", "Planted forest")),
                     label.y = c(220, 240, 270),  # 设置每个显著性标记的垂直位置
                     symnum.args = list(
                       cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                       symbols = c("***", "**", "*", "NS"))) +
  stat_summary(fun = mean, geom = "point", color = "darkred", size = 2, shape = 16) +  # 添加深红色平均值点
  geom_text(data = all_data %>% group_by(Climate, ForestType) %>% summarise(Count = n()), 
            aes(x = ForestType, y =70, label = paste0("n=", Count)), size = 2.6, 
            inherit.aes = FALSE) +
  facet_wrap(~ Climate, nrow = 1) +
  scale_fill_manual(values = c(
    "Primary forest" = "#2E8B57",                # 原生林--深绿色
    "Naturally regenerating forest" = "#9ACD32", # 次生林--浅绿色
    "Planted forest" = "#FFA500"                 # 人工林--橙色
  )) +
  scale_x_discrete(labels = c(
    "Primary forest" = "PF", 
    "Naturally regenerating forest" = "NRF", 
    "Planted forest" = "P")) +                   # 修改横坐标标签
  labs(#title = "DOY Distribution Across Climate Regions",
    title = "", x = "Forest type", y = "DOY", fill = "Forest type") +
  # theme_minimal() +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 11),        # 坐标轴刻度文字
    # axis.text.x = element_text(size = 12, angle = 10, vjust = 0.5),  # X 轴刻度文字向下移动
    axis.text.y = element_text(size = 11),        # 坐标轴刻度文字
    axis.title = element_text(size = 12),        # 坐标轴标题文字
    strip.text = element_text(size = 12),       # 分面标题
    # plot.title = element_text(size = 18, face = "bold"),  # 图标题
    # legend.title = element_text(size = 13),     # 图例标题
    # legend.text = element_text(size = 12)       # 图例文字
  ) +
  coord_cartesian(ylim = c(70, 290)) + scale_y_continuous(breaks = seq(70, 290, by = 50))


# 显示组图
print(g2)


ggsave(
  filename = "./0.figure/03 Chapter3/Fig.phe_DOY_barplot_GFT_sub_MGP.tiff",
  # plot = g2,  width = 8,  height = 6,  units = "in",  dpi = 300)
  plot = g2,  width = 9,  height = 2.6,  units = "in",  dpi = 300)



# ############################## 003 按三级气候分区作图  GMO   #########################################      

# # SOS: phe1_DOY ;  MGP: phe2_DOY ;  GMO：phe3_DOY
# # GDO: phe4_DOY ;  MSP: phe5_DOY ;  EOS：phe6_DOY

## 1. 读取数据
r1 <- rast("./EA+NA_Results/merged_Phe_DOY_mean_2_PHE_analysis/merged_10yr_mean_phe3_DOY.tif")
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
  
  forest1_DOY <- mask(doy_data, ifel(forest_data == 1, 1, NA))  # 原生林
  forest2_DOY <- mask(doy_data, ifel(forest_data == 2, 1, NA))  # 次生林
  forest3_DOY <- mask(doy_data, ifel(forest_data == 3, 1, NA))  # 人工林
  
  df_forest1 <- as.data.frame(forest1_DOY, xy = TRUE, na.rm = TRUE)
  df_forest2 <- as.data.frame(forest2_DOY, xy = TRUE, na.rm = TRUE)
  df_forest3 <- as.data.frame(forest3_DOY, xy = TRUE, na.rm = TRUE)
  
  df_forest1$ForestType <- "Primary forest"
  df_forest2$ForestType <- "Naturally regenerating forest"
  df_forest3$ForestType <- "Planted forest"
  
  combined <- rbind(
    data.frame(DOY = df_forest1[, 3], ForestType = df_forest1$ForestType),
    data.frame(DOY = df_forest2[, 3], ForestType = df_forest2$ForestType),
    data.frame(DOY = df_forest3[, 3], ForestType = df_forest3$ForestType)
  )
  combined$Climate <- climate
  
  all_data <- rbind(all_data, combined)
}

all_data$Climate <- factor(all_data$Climate,levels = c("Cfa", "Cfb", "Dfb","Dfc","Dwb", "Dwc"))
all_data$ForestType <- factor(all_data$ForestType, levels = c("Primary forest", "Naturally regenerating forest", "Planted forest"))

g3 <- ggplot(all_data, aes(x = ForestType, y = DOY, fill = ForestType,)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA) +
  stat_compare_means(method = "t.test", label = "p.signif",
                    size = 4, # 调整显著性标记的字体大小
                     comparisons = list(c("Primary forest", "Naturally regenerating forest"), 
                                        c("Primary forest", "Planted forest"), 
                                        c("Naturally regenerating forest", "Planted forest")),
                     label.y = c(210, 240, 270),  # 设置每个显著性标记的垂直位置
                     symnum.args = list(
                       cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                       symbols = c("***", "**", "*", "NS"))) +
  stat_summary(fun = mean, geom = "point", color = "darkred", size = 2, shape = 16) +  # 添加深红色平均值点
  geom_text(data = all_data %>% group_by(Climate, ForestType) %>% summarise(Count = n()), 
            aes(x = ForestType, y =70, label = paste0("n=", Count)), size = 2.6, 
            inherit.aes = FALSE) +
  facet_wrap(~ Climate, nrow = 1) +
  scale_fill_manual(values = c(
    "Primary forest" = "#2E8B57",                # 原生林--深绿色
    "Naturally regenerating forest" = "#9ACD32", # 次生林--浅绿色
    "Planted forest" = "#FFA500"                 # 人工林--橙色
  )) +
  scale_x_discrete(labels = c(
    "Primary forest" = "PF", 
    "Naturally regenerating forest" = "NRF", 
    "Planted forest" = "P")) +                   # 修改横坐标标签
  labs(#title = "DOY Distribution Across Climate Regions",
    title = "", x = "Forest type", y = "DOY", fill = "Forest type") +
  # theme_minimal() +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 11),        # 坐标轴刻度文字
    # axis.text.x = element_text(size = 12, angle = 10, vjust = 0.5),  # X 轴刻度文字向下移动
    axis.text.y = element_text(size = 11),        # 坐标轴刻度文字
    axis.title = element_text(size = 12),        # 坐标轴标题文字
    strip.text = element_text(size = 12),       # 分面标题
    # plot.title = element_text(size = 18, face = "bold"),  # 图标题
    # legend.title = element_text(size = 13),     # 图例标题
    # legend.text = element_text(size = 12)       # 图例文字
  ) +
  coord_cartesian(ylim = c(70, 290)) + scale_y_continuous(breaks = seq(70, 290, by = 50))

# 显示组图
print(g3)


ggsave(
  filename = "./0.figure/03 Chapter3/Fig.phe_DOY_barplot_GFT_sub_GMO.tiff",
  # plot = g3,  width = 8,  height = 6,  units = "in",  dpi = 300)
  plot = g3,  width = 9,  height = 2.6,  units = "in",  dpi = 300)



# ############################## 004 按三级气候分区作图  GDO   #########################################      

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
  
  forest1_DOY <- mask(doy_data, ifel(forest_data == 1, 1, NA))  # 原生林
  forest2_DOY <- mask(doy_data, ifel(forest_data == 2, 1, NA))  # 次生林
  forest3_DOY <- mask(doy_data, ifel(forest_data == 3, 1, NA))  # 人工林
  
  df_forest1 <- as.data.frame(forest1_DOY, xy = TRUE, na.rm = TRUE)
  df_forest2 <- as.data.frame(forest2_DOY, xy = TRUE, na.rm = TRUE)
  df_forest3 <- as.data.frame(forest3_DOY, xy = TRUE, na.rm = TRUE)
  
  df_forest1$ForestType <- "Primary forest"
  df_forest2$ForestType <- "Naturally regenerating forest"
  df_forest3$ForestType <- "Planted forest"
  
  combined <- rbind(
    data.frame(DOY = df_forest1[, 3], ForestType = df_forest1$ForestType),
    data.frame(DOY = df_forest2[, 3], ForestType = df_forest2$ForestType),
    data.frame(DOY = df_forest3[, 3], ForestType = df_forest3$ForestType)
  )
  combined$Climate <- climate
  
  all_data <- rbind(all_data, combined)
}

all_data$Climate <- factor(all_data$Climate,levels = c("Cfa", "Cfb", "Dfb","Dfc","Dwb", "Dwc"))
all_data$ForestType <- factor(all_data$ForestType, levels = c("Primary forest", "Naturally regenerating forest", "Planted forest"))

g4 <- ggplot(all_data, aes(x = ForestType, y = DOY, fill = ForestType,)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA) +
  stat_compare_means(method = "t.test", label = "p.signif",
                    size = 4, # 调整显著性标记的字体大小
                     comparisons = list(c("Primary forest", "Naturally regenerating forest"), 
                                        c("Primary forest", "Planted forest"), 
                                        c("Naturally regenerating forest", "Planted forest")),
                     label.y = c(300, 330, 360),  # 设置每个显著性标记的垂直位置
                     symnum.args = list(
                       cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                       symbols = c("***", "**", "*", "NS"))) +
  stat_summary(fun = mean, geom = "point", color = "darkred", size = 2, shape = 16) +  # 添加深红色平均值点
  geom_text(data = all_data %>% group_by(Climate, ForestType) %>% summarise(Count = n()), 
            aes(x = ForestType, y =160, label = paste0("n=", Count)), size = 2.6, 
            inherit.aes = FALSE) +
  facet_wrap(~ Climate, nrow = 1) +
  scale_fill_manual(values = c(
    "Primary forest" = "#2E8B57",                # 原生林--深绿色
    "Naturally regenerating forest" = "#9ACD32", # 次生林--浅绿色
    "Planted forest" = "#FFA500"                 # 人工林--橙色
  )) +
  scale_x_discrete(labels = c(
    "Primary forest" = "PF", 
    "Naturally regenerating forest" = "NRF", 
    "Planted forest" = "P")) +                   # 修改横坐标标签
  labs(#title = "DOY Distribution Across Climate Regions",
    title = "", x = "Forest type", y = "DOY", fill = "Forest type") +
  # theme_minimal() +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 11),        # 坐标轴刻度文字
    # axis.text.x = element_text(size = 12, angle = 10, vjust = 0.5),  # X 轴刻度文字向下移动
    axis.text.y = element_text(size = 11),        # 坐标轴刻度文字
    axis.title = element_text(size = 12),        # 坐标轴标题文字
    strip.text = element_text(size = 12),       # 分面标题
    # plot.title = element_text(size = 18, face = "bold"),  # 图标题
    # legend.title = element_text(size = 13),     # 图例标题
    # legend.text = element_text(size = 12)       # 图例文字
  ) +
  coord_cartesian(ylim = c(160, 380)) + scale_y_continuous(breaks = seq(160, 380, by = 50))

# 显示组图
print(g4)


ggsave(
  filename = "./0.figure/03 Chapter3/Fig.phe_DOY_barplot_GFT_sub_GDO.tiff",
  # plot = g4,  width = 8,  height = 6,  units = "in",  dpi = 300)
  plot = g4,  width = 9,  height = 2.6,  units = "in",  dpi = 300)
  

# ############################## 005 按三级气候分区作图  MSP   #########################################      

# # SOS: phe1_DOY ;  MGP: phe2_DOY ;  GMO：phe3_DOY
# # GDO: phe4_DOY ;  MSP: phe5_DOY ;  EOS：phe6_DOY

## 1. 读取数据
r1 <- rast("./EA+NA_Results/merged_Phe_DOY_mean_2_PHE_analysis/merged_10yr_mean_phe5_DOY.tif")
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
  
  forest1_DOY <- mask(doy_data, ifel(forest_data == 1, 1, NA))  # 原生林
  forest2_DOY <- mask(doy_data, ifel(forest_data == 2, 1, NA))  # 次生林
  forest3_DOY <- mask(doy_data, ifel(forest_data == 3, 1, NA))  # 人工林
  
  df_forest1 <- as.data.frame(forest1_DOY, xy = TRUE, na.rm = TRUE)
  df_forest2 <- as.data.frame(forest2_DOY, xy = TRUE, na.rm = TRUE)
  df_forest3 <- as.data.frame(forest3_DOY, xy = TRUE, na.rm = TRUE)
  
  df_forest1$ForestType <- "Primary forest"
  df_forest2$ForestType <- "Naturally regenerating forest"
  df_forest3$ForestType <- "Planted forest"
  
  combined <- rbind(
    data.frame(DOY = df_forest1[, 3], ForestType = df_forest1$ForestType),
    data.frame(DOY = df_forest2[, 3], ForestType = df_forest2$ForestType),
    data.frame(DOY = df_forest3[, 3], ForestType = df_forest3$ForestType)
  )
  combined$Climate <- climate
  
  all_data <- rbind(all_data, combined)
}

all_data$Climate <- factor(all_data$Climate,levels = c("Cfa", "Cfb", "Dfb","Dfc","Dwb", "Dwc"))
all_data$ForestType <- factor(all_data$ForestType, levels = c("Primary forest", "Naturally regenerating forest", "Planted forest"))

g5 <- ggplot(all_data, aes(x = ForestType, y = DOY, fill = ForestType,)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA) +
  stat_compare_means(method = "t.test", label = "p.signif",
                    size = 4, # 调整显著性标记的字体大小
                     comparisons = list(c("Primary forest", "Naturally regenerating forest"), 
                                        c("Primary forest", "Planted forest"), 
                                        c("Naturally regenerating forest", "Planted forest")),
                     label.y = c(310, 340, 370),  # 设置每个显著性标记的垂直位置
                     symnum.args = list(
                       cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                       symbols = c("***", "**", "*", "NS"))) +
  stat_summary(fun = mean, geom = "point", color = "darkred", size = 2, shape = 16) +  # 添加深红色平均值点
  geom_text(data = all_data %>% group_by(Climate, ForestType) %>% summarise(Count = n()), 
            aes(x = ForestType, y =170, label = paste0("n=", Count)), size = 2.6, 
            inherit.aes = FALSE) +
  facet_wrap(~ Climate, nrow = 1) +
  scale_fill_manual(values = c(
    "Primary forest" = "#2E8B57",                # 原生林--深绿色
    "Naturally regenerating forest" = "#9ACD32", # 次生林--浅绿色
    "Planted forest" = "#FFA500"                 # 人工林--橙色
  )) +
  scale_x_discrete(labels = c(
    "Primary forest" = "PF", 
    "Naturally regenerating forest" = "NRF", 
    "Planted forest" = "P")) +                   # 修改横坐标标签
  labs(#title = "DOY Distribution Across Climate Regions",
    title = "", x = "Forest type", y = "DOY", fill = "Forest type") +
  # theme_minimal() +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 11),        # 坐标轴刻度文字
    # axis.text.x = element_text(size = 12, angle = 10, vjust = 0.5),  # X 轴刻度文字向下移动
    axis.text.y = element_text(size = 11),        # 坐标轴刻度文字
    axis.title = element_text(size = 12),        # 坐标轴标题文字
    strip.text = element_text(size = 12),       # 分面标题
    # plot.title = element_text(size = 18, face = "bold"),  # 图标题
    # legend.title = element_text(size = 13),     # 图例标题
    # legend.text = element_text(size = 12)       # 图例文字
  ) +
  coord_cartesian(ylim = c(170, 390)) + scale_y_continuous(breaks = c(seq(170, 360, by = 50), 360))



# 显示组图
print(g5)


ggsave(
  filename = "./0.figure/03 Chapter3/Fig.phe_DOY_barplot_GFT_sub_MSP.tiff",
  # plot = g5,  width = 8,  height = 6,  units = "in",  dpi = 300)
  plot = g5,  width = 9,  height = 2.6,  units = "in",  dpi = 300)


# ############################## 006 按三级气候分区作图  EOS   #########################################      

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
  
  forest1_DOY <- mask(doy_data, ifel(forest_data == 1, 1, NA))  # 原生林
  forest2_DOY <- mask(doy_data, ifel(forest_data == 2, 1, NA))  # 次生林
  forest3_DOY <- mask(doy_data, ifel(forest_data == 3, 1, NA))  # 人工林
  
  df_forest1 <- as.data.frame(forest1_DOY, xy = TRUE, na.rm = TRUE)
  df_forest2 <- as.data.frame(forest2_DOY, xy = TRUE, na.rm = TRUE)
  df_forest3 <- as.data.frame(forest3_DOY, xy = TRUE, na.rm = TRUE)
  
  df_forest1$ForestType <- "Primary forest"
  df_forest2$ForestType <- "Naturally regenerating forest"
  df_forest3$ForestType <- "Planted forest"
  
  combined <- rbind(
    data.frame(DOY = df_forest1[, 3], ForestType = df_forest1$ForestType),
    data.frame(DOY = df_forest2[, 3], ForestType = df_forest2$ForestType),
    data.frame(DOY = df_forest3[, 3], ForestType = df_forest3$ForestType)
  )
  combined$Climate <- climate
  
  all_data <- rbind(all_data, combined)
}

all_data$Climate <- factor(all_data$Climate,levels = c("Cfa", "Cfb", "Dfb","Dfc","Dwb", "Dwc"))
all_data$ForestType <- factor(all_data$ForestType, levels = c("Primary forest", "Naturally regenerating forest", "Planted forest"))

g6 <- ggplot(all_data, aes(x = ForestType, y = DOY, fill = ForestType,)) +
  geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA) +
  stat_compare_means(method = "t.test", label = "p.signif",
                    size = 4, # 调整显著性标记的字体大小
                     comparisons = list(c("Primary forest", "Naturally regenerating forest"), 
                                        c("Primary forest", "Planted forest"), 
                                        c("Naturally regenerating forest", "Planted forest")),
                     label.y = c(370, 400, 430),  # 设置每个显著性标记的垂直位置
                     symnum.args = list(
                       cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                       symbols = c("***", "**", "*", "NS"))) +
  stat_summary(fun = mean, geom = "point", color = "darkred", size = 2, shape = 16) +  # 添加深红色平均值点
  geom_text(data = all_data %>% group_by(Climate, ForestType) %>% summarise(Count = n()), 
            aes(x = ForestType, y =230, label = paste0("n=", Count)), size = 2.6, 
            inherit.aes = FALSE) +
  facet_wrap(~ Climate, nrow = 1) +
  scale_fill_manual(values = c(
    "Primary forest" = "#2E8B57",                # 原生林--深绿色
    "Naturally regenerating forest" = "#9ACD32", # 次生林--浅绿色
    "Planted forest" = "#FFA500"                 # 人工林--橙色
  )) +
  scale_x_discrete(labels = c(
    "Primary forest" = "PF", 
    "Naturally regenerating forest" = "NRF", 
    "Planted forest" = "P")) +                   # 修改横坐标标签
  labs(#title = "DOY Distribution Across Climate Regions",
    title = "", x = "Forest type", y = "DOY", fill = "Forest type") +
  # theme_minimal() +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 11),        # 坐标轴刻度文字
    # axis.text.x = element_text(size = 12, angle = 10, vjust = 0.5),  # X 轴刻度文字向下移动
    axis.text.y = element_text(size = 11),        # 坐标轴刻度文字
    axis.title = element_text(size = 12),        # 坐标轴标题文字
    strip.text = element_text(size = 12),       # 分面标题
    # plot.title = element_text(size = 18, face = "bold"),  # 图标题
    # legend.title = element_text(size = 13),     # 图例标题
    # legend.text = element_text(size = 12)       # 图例文字
  ) +
  coord_cartesian(ylim = c(230, 450)) + scale_y_continuous(breaks = c(seq(230, 360, by = 50), 360))

# 显示组图
print(g6)


ggsave(
  filename = "./0.figure/03 Chapter3/Fig.phe_DOY_barplot_GFT_sub_EOS.tiff",
  # plot = g6,  width = 8,  height = 6,  units = "in",  dpi = 300)
  plot = g6,  width = 9,  height = 2.6,  units = "in",  dpi = 300)
