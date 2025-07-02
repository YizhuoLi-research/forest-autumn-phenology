###### 0. 加载包 ####
library(terra)
library(tidyverse)
library(raster)
library(ggplot2)
library(dplyr)
library(ggpubr)

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

# ############################## 001 对所有气候阶段文件按三级气候分区作图     #########################################      

# 定义函数用于处理栅格数据
process_raster <- function(raster_data, r_GFC, climate_types) {
  all_data <- data.frame()  # 初始化空数据框
  
  # 遍历每个气候区域，提取DOY和林类数据
  for (climate in names(climate_types)) {
    border <- climate_types[[climate]]
    
    # 掩膜提取DOY和林类数据
    phase_data <- mask(raster_data, border)
    forest_data <- mask(r_GFC, border)
    
    # 根据林类值创建掩膜条件
    forest1_DAYS <- mask(phase_data, ifel(forest_data == 1, 1, NA))  # 再生林
    forest2_DAYS <- mask(phase_data, ifel(forest_data == 2, 1, NA))  # 原生林
    forest3_DAYS <- mask(phase_data, ifel(forest_data == 3, 1, NA))  # 人工林
    
    # 转为数据框
    df_forest1 <- as.data.frame(forest1_DAYS, xy = TRUE, na.rm = TRUE)
    df_forest2 <- as.data.frame(forest2_DAYS, xy = TRUE, na.rm = TRUE)
    df_forest3 <- as.data.frame(forest3_DAYS, xy = TRUE, na.rm = TRUE)
    
    # 添加林类标签
    df_forest1$ForestType <- "Naturally regenerating forest"
    df_forest2$ForestType <- "Primary forest"
    df_forest3$ForestType <- "Planted forest"
    
    # 合并数据
    combined <- rbind(
      data.frame(DAYS = df_forest1[, 3], ForestType = df_forest1$ForestType),  #再生林
      data.frame(DAYS = df_forest2[, 3], ForestType = df_forest2$ForestType),
      data.frame(DAYS = df_forest3[, 3], ForestType = df_forest3$ForestType)
    )
    
    # 添加气候类型
    combined$Climate <- climate
    
    # 将气候类型和林类设置为因子
    combined$Climate <- factor(combined$Climate, levels = c("Cfa", "Cfb", "Dfb", "Dfc", "Dwb", "Dwc"))
    combined$ForestType <- factor(combined$ForestType, levels = c( "Naturally regenerating forest","Primary forest", "Planted forest"))
    
    # 计算每个气候类型下每种林类的平均值和标准差
    summary_stats <- combined %>%
      group_by(Climate, ForestType) %>%
      summarize(
        Mean_DAYS = mean(DAYS, na.rm = TRUE),
        SD_value = sd(DAYS, na.rm = TRUE)
      )
    
    # 将结果添加到总数据框
    all_data <- rbind(all_data, summary_stats)
  }
  
  return(all_data)
}

# 1. 读取数据
r1 <- rast("./EA+NA_Results/merged_Phe_days_counts_2_PHE_analysis/days_13_mean.tif")
r2 <- rast("./EA+NA_Results/merged_Phe_days_counts_2_PHE_analysis/days_34_mean.tif")
r3 <- rast("./EA+NA_Results/merged_Phe_days_counts_2_PHE_analysis/days_46_mean.tif")

r_GFC <-  rast("./EA+NA_Results/EA+NA_GFC_30km.tif")

# 2. 提取各个区域的边界
Cfa_border <- classify_border[classify_border$EA_koppen_30km_addClimate %in% Cfa, ]
Cfb_border <- classify_border[classify_border$EA_koppen_30km_addClimate %in% Cfb, ]
Dfb_border <- classify_border[classify_border$EA_koppen_30km_addClimate %in% Dfb, ]
Dfc_border <- classify_border[classify_border$EA_koppen_30km_addClimate %in% Dfc, ]
Dwb_border <- classify_border[classify_border$EA_koppen_30km_addClimate %in% Dwb, ]
Dwc_border <- classify_border[classify_border$EA_koppen_30km_addClimate %in% Dwc, ]

# 定义气候类型和区域边界
climate_types <- list(
  Cfa = Cfa_border,
  Cfb = Cfb_border,
  Dfb = Dfb_border,
  Dfc = Dfc_border,
  Dwb = Dwb_border,
  Dwc = Dwc_border
)

# 3. 对每个栅格数据调用函数处理并合并
all_data_r1 <- process_raster(r1, r_GFC, climate_types)
all_data_r2 <- process_raster(r2, r_GFC, climate_types)
all_data_r3 <- process_raster(r3, r_GFC, climate_types)

all_data_r1$phase <- "days13"
all_data_r2$phase <- "days34"
all_data_r3$phase <- "days46"


# 4. 合并所有数据
all_data_combined <- rbind(all_data_r1, all_data_r2, all_data_r3)

# 查看合并后的数据
head(all_data_combined)


all_data_combined <- all_data_combined %>%
  mutate(
    phase = factor(phase, levels = c("days46", "days34", "days13")),
    Climate = factor(Climate, levels = c("Cfa", "Dfb", "Dwb", "Cfb", "Dfc", "Dwc"))
  )
## 注意： errorbar显示的是均值在区域上的变异程度

############################### 002 对所有气候阶段文件按三级气候分区作图     #########################################      
# 堆叠的效果图
# error_bars <- all_data_combined %>%
#   arrange(Climate, ForestType, desc(phase)) %>%  # 按气候、森林类型和相位降序排列
#   group_by(Climate, ForestType) %>%             # 按气候和森林类型分组
#   mutate(Mean_DAYS_new = cumsum(Mean_DAYS)) %>% # 计算累积和
#   ungroup()
# 
# g1 <- ggplot(all_data_combined, aes(x = ForestType, y = Mean_DAYS, fill = phase)) +
#   geom_bar(stat = "identity", position = "dodge", width = 0.5) +  # 不堆叠
#   # geom_bar(stat = "identity", position = "stack", width = 0.5) +  # 堆叠柱状图
#   facet_wrap(~ Climate, nrow = 2) +  # 按气候区分面
#   scale_fill_manual(values = c("days13" = "#99CC33",
#                                "days34" = "#006600",
#                                "days46" = "#CC9900"),    # "#FFCC33"
#     labels = c("days13" = "SOS-GMO",
#                "days34" = "GMO-GDO",
#                "days46" = "GDO-EOS"),
#     breaks = c("days13", "days34", "days46")  ) +  # 设置图例顺序
#   scale_x_discrete(
#     limits = c("Planted forest","Naturally regenerating forest","Primary forest" ),  # 按指定顺序排列
#     labels = c("Primary forest" = "PF", 
#                "Naturally regenerating forest" = "NRF", 
#                "Planted forest" = "P" )) +  # 修改横坐标标签和顺序
#   #添加0.15sd误差线
#   # geom_errorbar(aes(ymin = Mean_DAYS  - SD_value, ymax = Mean_DAYS  + SD_value), width = 0.2)+
#   # geom_text(aes(label = paste(round(Mean_DAYS,2),"%")), position = position_stack(vjust =  0.5))+
#   labs(title =   "" ,  #Mean DOY Across Forest Types and Climate Zones",
#        x = "Forest type", y = " Days", fill = "Phenological transition") +   #Phenological transitions
#   theme_bw() +
#   theme(
#     legend.position = "right",  # 不显示图例
#     # legend.position = "none",  # 不显示图例
#     # legend.position = "bottom",         # 显示图例并放置在右侧
#     # legend.direction = "horizontal",          # 图例横向排列
#     legend.title = element_text(size = 16),  # 图例标题字体 #face = "bold"
#     legend.text = element_text(size = 14), # 图例文本字体
#     legend.key.size = unit(0.7, "lines"),        # 调整图例键的尺寸
#     axis.text.x = element_text(size = 14),        # 坐标轴刻度文字
#     axis.text.y = element_text(size = 14),        # 坐标轴刻度文字
#     axis.title = element_text(size = 14),        # 坐标轴标题文字
#     strip.text = element_text(size = 15)         # 分面标题
#   )+
#   geom_errorbar(data = error_bars,
#                           aes(x = ForestType, ymax = Mean_DAYS_new + SD_value, ymin = Mean_DAYS_new - SD_value), 
#                           width = 0.3,size = 0.7)+
#   coord_flip()  # 翻转 x 和 y 轴
# 
#   # facet_grid(~Climate)
# # 显示绘图
# print(g1)

error_bars2 <- all_data_combined %>%
  arrange(Climate, ForestType, phase) %>%  # 按气候、森林类型和相位排列
  group_by(Climate, ForestType, phase) %>% # 仍然按相位分组
  mutate(Mean_DAYS_new = Mean_DAYS) %>%    # 直接使用 Mean_DAYS 作为误差线的基准
  ungroup()

g1 <- ggplot(all_data_combined, aes(x = ForestType, y = Mean_DAYS, fill = phase)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +  # 采用 dodge 形式
  facet_wrap(~ Climate, nrow = 2) +  
  scale_fill_manual(values = c("days13" = "#99CC33",
                               "days34" = "#006600",
                               "days46" = "#CC9900"),    
                    labels = c("days13" = "SOS-GMO",
                               "days34" = "GMO-GDO",
                               "days46" = "GDO-EOS"),
                    breaks = c("days13", "days34", "days46")) +  
  scale_x_discrete(
    limits = c("Planted forest","Naturally regenerating forest","Primary forest"),  
    labels = c("Primary forest" = "PF", 
               "Naturally regenerating forest" = "NRF", 
               "Planted forest" = "P")) +  
  labs(title = "",  
       x = "Forest type", y = "Days", fill = "Phenological transition") +   
  theme_bw() +
  theme(
    legend.position = "right",  
    # legend.title = element_text(size = 16),  
    legend.title = element_blank(),  
    
    legend.text = element_text(size = 14),  
    legend.key.size = unit(0.7, "lines"),  
    legend.key.height = unit(0.6, "cm"),  # 调整图例键的高度，增大行间距
    legend.spacing.y = unit(0.7, 'cm'),  # 增大图例项与标题之间的垂直间距
    axis.text.x = element_text(size = 14),  
    axis.text.y = element_text(size = 14),  
    axis.title = element_text(size = 15),  
    strip.text = element_text(size = 14)  
  ) +
  # 修正误差线，使其与 dodge 柱状图对齐
  geom_errorbar(data = error_bars2,
                aes(x = ForestType, 
                    ymax = Mean_DAYS_new + SD_value, 
                    ymin = Mean_DAYS_new - SD_value), 
                position = position_dodge(width = 0.6),  # 让误差线与柱子对齐
                width = 0.3, size = 0.7) +
  # 添加数值标签，确保与柱状图对齐
  geom_text(aes(y = Mean_DAYS + SD_value, label = sprintf("%.1f", Mean_DAYS)),  # 将文本放在误差线的最大值处
            position = position_dodge(width = 0.6),  # 让文本与柱状图对齐
            hjust = -0.2, size = 4, vjust = 0.5) +  # 调整文本位置
  ylim(0, 140) +  # 设置 y 轴范围
  coord_flip() +
  scale_y_continuous(
    limits = c(0, 140),  # 设置横坐标（原 y 轴）范围
    breaks = seq(0, 120, by = 40),  # 设置刻度间隔
    labels = scales::number_format(accuracy = 40)  # 设置刻度标签格式
  )

print(g1)

ggsave(
  filename = "./0.figure/03 Paper/Fig.2.phaseDAYS_barplot_GFT_sub_dodge.tiff",
  # plot = g1,  width = 8,  height = 6,  units = "in",  dpi = 300)
  plot = g1,  width = 9,  height = 6,  units = "in",  dpi = 300)


####legend####
# g2 <- ggplot(all_data_combined, aes(x = ForestType, y = Mean_DAYS, fill = phase)) +
#   geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +  # 采用 dodge 形式
#   facet_wrap(~ Climate, nrow = 2) +  
#   scale_fill_manual(values = c("days13" = "#99CC33",
#                                "days34" = "#006600",
#                                "days46" = "#CC9900"),    
#                     labels = c("days13" = "SOS-GMO",
#                                "days34" = "GMO-GDO",
#                                "days46" = "GDO-EOS"),
#                     breaks = c("days13", "days34", "days46")) +  
#   scale_x_discrete(
#     limits = c("Planted forest","Naturally regenerating forest","Primary forest"),  
#     labels = c("Primary forest" = "PF", 
#                "Naturally regenerating forest" = "NRF", 
#                "Planted forest" = "P")) +  
#   labs(title = "",  
#        x = "Forest type", y = "Days", fill = "Phenological transition") +   
#   theme_bw() +
#   theme(
#     legend.position = "right",  
#     legend.title = element_text(size = 17),  
#     legend.text = element_text(size = 15),  
#     legend.key.size = unit(0.7, "lines"),  
#     legend.key.height = unit(0.6, "cm"),  # 调整图例键的高度，增大行间距
#     legend.spacing.y = unit(0.5, 'cm'),  # 增大图例项与标题之间的垂直间距
#     axis.text.x = element_text(size = 14),  
#     axis.text.y = element_text(size = 14),  
#     axis.title = element_text(size = 15),  
#     strip.text = element_text(size = 14)  
#   ) +
#   # 修正误差线，使其与 dodge 柱状图对齐
#   geom_errorbar(data = error_bars2,
#                 aes(x = ForestType, 
#                     ymax = Mean_DAYS_new + SD_value, 
#                     ymin = Mean_DAYS_new - SD_value), 
#                 position = position_dodge(width = 0.6),  # 让误差线与柱子对齐
#                 width = 0.3, size = 0.7) +
#   # 添加数值标签，确保与柱状图对齐
#   geom_text(aes(y = Mean_DAYS + SD_value, label = sprintf("%.1f", Mean_DAYS)),  # 将文本放在误差线的最大值处
#             position = position_dodge(width = 0.6),  # 让文本与柱状图对齐
#             hjust = -0.2, size = 4, vjust = 0.5) +  # 调整文本位置
#   ylim(0, 140) +  # 设置 y 轴范围
#   coord_flip() +
#   scale_y_continuous(
#     limits = c(0, 140),  # 设置横坐标（原 y 轴）范围
#     breaks = seq(0, 120, by = 40),  # 设置刻度间隔
#     labels = scales::number_format(accuracy = 40)  # 设置刻度标签格式
#   )
# 
# print(g2)
# 
# ggsave(
#   filename = "./0.figure/03 Paper/Fig.3.phaseDAYS_barplot_GFT_sub_dodge-legend.tiff",
#   # plot = g1,  width = 8,  height = 6,  units = "in",  dpi = 300)
#   plot = g2,  width = 10,  height = 6,  units = "in",  dpi = 300)
