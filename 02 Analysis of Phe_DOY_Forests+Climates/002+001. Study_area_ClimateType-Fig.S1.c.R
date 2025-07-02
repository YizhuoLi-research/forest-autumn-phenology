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
  # climate = Cfa
  
  border <- climate_types[[climate]]
  
  doy_data <- mask(r1, border)
  forest_data <- mask(r2, border)
  
  forest1_DOY <- mask(doy_data, ifel(forest_data == 1, 1, NA))  # 次生林
  forest2_DOY <- mask(doy_data, ifel(forest_data == 2, 1, NA))  # 原生林
  forest3_DOY <- mask(doy_data, ifel(forest_data == 3, 1, NA))  # 人工林
  
  df_forest1 <- as.data.frame(forest1_DOY, xy = TRUE, na.rm = TRUE)
  df_forest2 <- as.data.frame(forest2_DOY, xy = TRUE, na.rm = TRUE)
  df_forest3 <- as.data.frame(forest3_DOY, xy = TRUE, na.rm = TRUE)
  
  df_forest1$ForestType <- "Naturally regenerating forest"  #已经对应好
  df_forest2$ForestType <- "Primary forest"
  df_forest3$ForestType <- "Planted forest"
  
  # 统一列名（确保列顺序一致）
  combined <- rbind(
    df_forest1[, c("x", "y", names(df_forest1)[3], "ForestType")],
    df_forest2[, c("x", "y", names(df_forest2)[3], "ForestType")],
    df_forest3[, c("x", "y", names(df_forest3)[3], "ForestType")]
  )
  
  # 为 DOY 列统一命名（可选）
  colnames(combined)[3] <- "DOY"
  combined$Climate <- climate
  
  all_data <- rbind(all_data, combined)
}

all_data$Climate <- factor(all_data$Climate, levels = c("Cfa", "Dfb", "Dwb", "Cfb", "Dfc", "Dwc"))
all_data$ForestType <- factor(all_data$ForestType, levels = c("Primary forest","Naturally regenerating forest", "Planted forest"))

head(all_data)

x_lines <- seq(-120,180, by = 60) # 定义经度线
wr <- map_data("world") %>%
  filter(lat > 20)

# 绘制土地覆盖类型的地图
p1 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    # 填充背景为灰色
  geom_tile(data = all_data, aes(x = x, y = y, fill = factor(Climate))) +  # 根据Climate的分类填色
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat)  +  # 指定经纬度范围
  # 这里我们使用指定的颜色映射，分别为1、2、3对应不同的颜色
  scale_fill_manual(values = c("Cfa" = "#005000", "Cfb" = "#00AA00", "Dfb" = "#820082",
                               "Dwb" = "#C89BFA", "Dfc" = "#C800C8", "Dwc" = "#6E28B4"), 
                    na.value = "transparent",
                    breaks = c("Cfa", "Cfb", "Dfb", "Dfc","Dwb", "Dwc"),  # 控制图例显示顺序
                    name = "Climate type")+  # 图例标题
                    # guide = guide_legend(nrow = 1, byrow = TRUE) )+ # 设置两行三列
                    # guide = guide_legend(nrow = 2, ncol = 3) )+ # 设置两行三列
                    # labels = c("1" = "落叶针叶林", "2" = "落叶阔叶林", "3" = "混交林"),  # 自定义图例标签
                    # labels = c("1" = "DNF", "2" = "DBF", "3" = "MF"),  # 自定义图例标签
                    # guide = "legend") +
  theme_minimal() +
  guides(fill = guide_legend(keywidth = 3, nrow = 1, byrow = TRUE)) +  # 设置图例色块的宽度，一行展示
  theme( # plot.title = element_text(size = 45),
    panel.background = element_blank(),
    axis.ticks=element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    # legend.position = "none")+
    legend.title = element_text(size = 36),
    legend.text = element_text(size =35),
    legend.position = "bottom",
    legend.direction = "horizontal",  # 强制水平排列
    legend.box = "horizontal")+         # 水平排列 # 强制图例横向排列
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  # 添加经纬线
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

ggsave(
  filename = "./0.figure/03 Paper/Fig.S1-c-ClimateType_map.tiff",
  plot = p1,  width = 15,  height = 15,  units = "in",  dpi = 300)
