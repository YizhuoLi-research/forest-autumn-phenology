library(terra)
library(raster)
library(dplyr)
library(ggplot2)
library("scales")

setwd("D:/Graduation_Thesis")

############################### 01 LandCover极地图-落叶林种类  ##################################
wr <- map_data("world") %>%
  filter(lat > 20)
# head(wr)


r_NA <- raster("D:/Graduation_Thesis/01 Download/09 MODIS_Landcover/NA_Results/NA_LandCover_30km.tif")
r_EA <- raster("D:/Graduation_Thesis/01 Download/09 MODIS_Landcover/EA_Results/EA_LandCover_30km.tif")

# non_empty_pixels <- sum(!is.na(values(r_EA)))
# print(paste("非空像素数量为:", non_empty_pixels))

LandCover_1 <- merge(r_NA, r_EA)
df1 <- as.data.frame(LandCover_1, xy = TRUE, na.rm = TRUE)  
colnames(df1) <- c("long", "lat","landcover") 
df1$landcover <- (as.numeric(as.character(df1$landcover)))    #################
summary(df1)

x_lines <- seq(-120,180, by = 60) # 定义经度线

# 绘制土地覆盖类型的地图
p1 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    # 填充背景为灰色
  geom_tile(data = df1, aes(x = long, y = lat, fill = factor(landcover))) +  # 根据landcover的分类填色
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat)  +  # 指定经纬度范围
  # 这里我们使用指定的颜色映射，分别为1、2、3对应不同的颜色
  scale_fill_manual(values = c("1" = "#009933", "2" = "#99CC00", "3" = "#336600"), 
                    na.value = "transparent",
                    name = "Land cover type",  # 图例标题
                    # labels = c("1" = "落叶针叶林", "2" = "落叶阔叶林", "3" = "混交林"),  # 自定义图例标签
                    labels = c("1" = "DNF", "2" = "DBF", "3" = "MF"),  # 自定义图例标签
                    guide = "legend") +
  theme_minimal() +
  theme( # plot.title = element_text(size = 45),
    panel.background = element_blank(),
    axis.ticks=element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    # legend.position = "none")+
    legend.title = element_text(size = 36),
    legend.text = element_text(size =35),
    legend.position = "bottom")+
  guides(fill = guide_legend(keywidth = 3)) +  # 设置图例色块的宽度
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
  filename = "./0.figure/03 Paper/Fig.S1-a-LandCoverType_map.tiff",
  plot = p1,  width = 15,  height = 15,  units = "in",  dpi = 300)


############################### 02 Forest Type极地图  林地类型-天然/人工##################################

# library(dplyr)
# library("scales")
wr <- map_data("world") %>%
  filter(lat > 20)

rr_NA <- rast("D:/Graduation_Thesis/01 Download/10 JRC_GFC2020/NA_Results/NA_GCF_30km.tif")
rr_EA <- rast("D:/Graduation_Thesis/01 Download/10 JRC_GFC2020/EA_Results/EA_GCF_30km.tif")

non_empty_pixels <- sum(!is.na(values(rr_EA)))
print(paste("非空像素数量为:", non_empty_pixels))

ForestType_1 <- merge(rr_NA, rr_EA)
df2 <- as.data.frame(ForestType_1, xy = TRUE, na.rm = TRUE)  
colnames(df2) <- c("long", "lat","ForestType") 
df2$ForestType <- (as.numeric(as.character(df2$ForestType)))    #################
summary(df2)
x_lines <- seq(-120,180, by = 60)# Defines the x axes required

p2 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    # 填充背景为灰色
  geom_tile(data = df2, aes(x = long, y = lat, fill = factor(ForestType))) +  # 根据landcover的分类填色
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat)  +  # 指定经纬度范围
  # 这里我们使用指定的颜色映射，分别为1、2、3对应不同的颜色
  scale_fill_manual(values = c("2" = "#2E8B57", "1" = "#9ACD32", "3" = "#FFA500"), 
                    na.value = "transparent",
                    name = "Forest type", #图例标题
                    # labels = c("2" = "原生林", "1" = "自然再生林", "3" = "人工林"),  # 自定义图例标签
                    labels = c("2" = "PF", "1" = "NRF", "3" = "P"),  # 自定义图例标签
                    breaks = c("2", "1", "3"),  # 设置图例的显示顺序
                    guide = "legend") +
  theme_minimal() +
  theme( # plot.title = element_text(size = 45),
    panel.background = element_blank(),
    axis.ticks=element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    # legend.position = "none")+
    legend.title = element_text(size = 36),
    legend.text = element_text(size = 35),
    legend.position = "bottom")+
  guides(fill = guide_legend(keywidth = 3)) +  # 设置图例色块的宽度
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
p2

ggsave(
  filename = "./0.figure/03 Paper/Fig.S1-b-ForestType_map.tiff",
  plot = p2,  width = 15,  height = 15,  units = "in",  dpi = 300)



#################  03 计算每种森林管理类型下落叶林种类占比 ######


library(terra)
library(dplyr)

# 读取两个raster数据
r_LandCover <- merge(rast("D:/Graduation_Thesis/01 Download/09 MODIS_Landcover/NA_Results/NA_LandCover_30km.tif"),
                     rast("D:/Graduation_Thesis/01 Download/09 MODIS_Landcover/EA_Results/EA_LandCover_30km.tif"))

r_ForestType <- merge(rast("D:/Graduation_Thesis/01 Download/10 JRC_GFC2020/NA_Results/NA_GCF_30km.tif"),
                      rast("D:/Graduation_Thesis/01 Download/10 JRC_GFC2020/EA_Results/EA_GCF_30km.tif"))

# 确保两个栅格一致
r_LandCover <- resample(r_LandCover, r_ForestType, method = "near")

# 叠加两个栅格
r_stack <- c(r_LandCover, r_ForestType)
names(r_stack) <- c("LandCover", "ForestType")

# 转为 dataframe
df <- as.data.frame(r_stack, xy=FALSE, na.rm=TRUE)

# 先筛选掉 ForestType=0 或 LandCover 不是1/2/3的像元
df_clean <- df %>%
  filter(ForestType != 0, LandCover %in% c(1, 2, 3))

# 转换为分类名称
df_clean$ForestType <- factor(df_clean$ForestType,
                              levels = c(2, 1, 3),
                              labels = c("PF", "NRF", "P"))
df_clean$LandCover <- factor(df_clean$LandCover,
                             levels = c(1, 2, 3),
                             labels = c("DNF", "DBF", "MF"))

# 现在在干净的数据上做占比计算
result <- df_clean %>%
  group_by(ForestType, LandCover) %>%
  summarise(count = n()) %>%
  group_by(ForestType) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  arrange(ForestType, LandCover)

print(result)
