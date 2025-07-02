###### 0. 加载包 ####
library(terra)
library(tidyverse)
library(raster)
library(ggplot2)
library(ggpubr)


setwd("D:/Graduation_Thesis")

###############################################################################################
#############################   0. 计算不同气候区、不同GFC的DOY差异  ###########################
###############################################################################################

#############################   00.按照柯本气候类型，将气候区分为6个气候区域   ####################
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


############################### 003 按三级气候分区作图  GDO   #########################################      
############################   分区分森林类型计算趋势  
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
forest_types <- c(1, 2, 3)  # 1: 次生林, 2: 原生林, 3: 人工林
years <- 2013:2022


############################   计算DOY 9年均值值影像   #######################

r1_stack <- stack(r1_files)
phe4_DOY_mean <- calc(r1_stack, fun = function(x) ceiling(mean(x, na.rm = TRUE)))
df <- as.data.frame(phe4_DOY_mean, xy = TRUE, na.rm = TRUE)  
colnames(df) <- c("long", "lat","phe4_DOY_mean") 
df$phe4_DOY_mean <-as.numeric(as.character(df$phe4_DOY_mean))
summary(df$phe4_DOY_mean)

# # 计算均值和标准差
# mean_value <- mean(df$phe4_DOY_mean, na.rm = TRUE)
# sd_value <- sd(df$phe4_DOY_mean, na.rm = TRUE)
# # 三倍标准差范围
# lower_limit <- mean_value - 3 * sd_value
# upper_limit <- mean_value + 3 * sd_value
# # 打印范围
# cat("三倍标准差范围为:", lower_limit, "到", upper_limit, "\n")
# # 三倍标准差范围为: 204.4929 到 246.0177 
# 计算
# count_less_than_160 <- sum(df$phe4_DOY_mean < 160, na.rm = TRUE)
# cat("有", count_less_than_160, "phe4_DOY_mean<160")
# count_more_than_240 <- sum(df$phe4_DOY_mean > 240, na.rm = TRUE)
# cat("有", count_more_than_240, "phe4_DOY_mean>240")
# # 有 8 phe4_DOY_mean<160;有 780 phe4_DOY_mean>240

# count_less_than_200 <- sum(df$phe4_DOY_mean < 200, na.rm = TRUE)   √
# cat("有", count_less_than_200, "phe4_DOY_mean<200")
# count_more_than_280 <- sum(df$phe4_DOY_mean > 280, na.rm = TRUE)
# cat("有", count_more_than_280, "phe4_DOY_mean>280")
# # 有 有 107 phe4_DOY_mean<200;有 1 phe4_DOY_mean>280


# count_less_than_180 <- sum(df$phe4_DOY_mean < 180, na.rm = TRUE)   √
# cat("有", count_less_than_180, "phe4_DOY_mean<180")
# count_more_than_260 <- sum(df$phe4_DOY_mean > 260, na.rm = TRUE)
# cat("有", count_more_than_260, "phe4_DOY_mean>260")
# # 有 有 15 phe4_DOY_mean<200;有 7 phe4_DOY_mean>260

#设置显示范围
df$phe4_DOY_mean <- ifelse(df$phe4_DOY_mean < 180, 160, 
                           ifelse(df$phe4_DOY_mean > 260, 280, df$phe4_DOY_mean))


# library("scales")
wr <- map_data("world") %>%
  filter(lat > 20)

x_lines <- seq(-120,180, by = 60)# Defines the x axes required

p1 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    #fill = "lightgray"
  geom_tile(data = df, aes(x = long, y = lat, fill = phe4_DOY_mean)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "gray40", fill = alpha("gray40", 0), linewidth = 0.3) +
  expand_limits(x = wr$long, y = wr$lat)  +                           #指定经纬度
  scale_fill_gradientn(colours = c("#009900","#006600","#003300","#FFCC33","#996600"),  #  "#006600"  "#336600"
                       na.value = "transparent",
                       name = "DOY",
                       values = scales::rescale(c(160, 180, 220,260,280)),
                       limits = c(160,280),
                       breaks = c(160,180,220,260,280),  # Specify the breaks for labels
                       labels = c("","180","220","260",""),  # Specify the corresponding labels
                       guide = guide_colorbar(title.position = "bottom",
                                              title.hjust = 0.5,
                                              barwidth = 40,
                                              title.vjust = 1,
                                              barheight = 1.8,
                                              ticks = TRUE,
                                              ticks.colour = "white",
                                              ticks.linewidth = 3.0/.pt))+   #刻度线)) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        axis.ticks=element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.title = element_text(size = 37) ,
        legend.text = element_text(size = 37),
        legend.position = "bottom")+  # Make the legend horizontal)+
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  geom_hline(aes(yintercept = 20), size = 0.1)+  
  geom_segment(size = 0.4 ,aes(y = 20, yend = 90, x = x_lines, xend = x_lines),colour = "gray40",linetype = "dashed") +     #经线
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  #纬线20
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x =  180, xend = 0), colour = "gray20") +
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
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 12) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # 如果要显示北极投影
p1

ggsave(
  filename = "./0.figure/03 Paper/Fig.1-a.Combined_meanDOY&trend9yrs_GDO-a_new.tiff",
  plot = p1,  width = 22,  height = 15,  units = "in",  dpi = 300)  #18


# ############################   计算DOY 9年均值纬度分布   #######################
# 
# dat1 <- df %>%
#   mutate(lat = lat) %>%
#   group_by(lat) %>%
#   summarize(row_means = mean(phe4_DOY_mean),
#             row_sds = sd(phe4_DOY_mean))
# t1 <- ggplot(dat1, aes(x = lat, y = row_means)) +
#   geom_path(size = 1) +  # 增加折线粗细
#   labs(x = "Latitude(°N)", y = "DOY") +
#   geom_ribbon(aes(y = row_means, ymin = row_means - row_sds, ymax = row_means + row_sds), fill = "lightgrey", alpha = 0.5) +
#   theme_classic() +
#   theme(
#     aspect.ratio = 1.5,  # 设置纵横比为 0.25
#     axis.title = element_text(size = 14, face = "plain"),  # 纵坐标字体，设置为加粗
#     axis.text.x = element_text(size = 13, face = "plain"),  # 横坐标刻度字体
#     axis.text.y = element_text(size = 13, face = "plain"),  # 纵坐标刻度字体
#     # plot.title = element_text(size = 16, face = "plain"),  # 设置图形标题字体
#     strip.text.x = element_text(size = 13, face = "plain"),  # 行标题字体
#     strip.text.y = element_text(size = 13, face = "plain")   # 列标题字体
#   )+
#   coord_flip()   # 交换 x 轴和 y 轴
# t1
# 
# ggsave(
#   filename = "./0.figure/03 Paper/Fig.Combined_meanDOY&trend9yrs_GDO-b.tiff",
#   plot = t1,  width = 2.4,   height = 3,  # 总高度 = p1高度 + g1高度
#   units = "in",   dpi = 300 )
# 


############################### 004 按三级气候分区作图  EOS   #########################################      
############################   分区分森林类型计算趋势   
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
forest_types <- c(1, 2, 3)  # 1: 次生林, 2: 原生林, 3: 人工林
years <- 2013:2022


############################   计算DOY 9年均值值影像   #######################

r1_stack <- stack(r1_files)
phe6_DOY_mean <- calc(r1_stack, fun = function(x) ceiling(mean(x, na.rm = TRUE)))
df <- as.data.frame(phe6_DOY_mean, xy = TRUE, na.rm = TRUE)  
colnames(df) <- c("long", "lat","phe6_DOY_mean") 
df$phe6_DOY_mean <-as.numeric(as.character(df$phe6_DOY_mean))
summary(df$phe6_DOY_mean)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 178.0   271.0   283.0   289.4   308.0   356.0  
# 计算均值和标准差
mean_value <- mean(df$phe6_DOY_mean, na.rm = TRUE)
sd_value <- sd(df$phe6_DOY_mean, na.rm = TRUE)
# 三倍标准差范围
lower_limit <- mean_value - 3 * sd_value
upper_limit <- mean_value + 3 * sd_value
# 打印范围
cat("三倍标准差范围为:", lower_limit, "到", upper_limit, "\n")
# 三倍标准差范围为: 221.1452 到 357.5754 
# 计算
count_less_than_240 <- sum(df$phe6_DOY_mean < 240, na.rm = TRUE)  
cat("有", count_less_than_240, "phe6_DOY_mean<240")
count_more_than_320 <- sum(df$phe6_DOY_mean > 320, na.rm = TRUE)
cat("有", count_more_than_320, "phe6_DOY_mean>320")
# 有 20 phe6_DOY_mean<240;有 6329 phe6_DOY_mean>320

# count_less_than_280 <- sum(df$phe6_DOY_mean < 280, na.rm = TRUE)
# cat("有", count_less_than_280, "phe6_DOY_mean<280")
# count_more_than_360 <- sum(df$phe6_DOY_mean > 360, na.rm = TRUE)
# cat("有", count_more_than_360, "phe6_DOY_mean>360")
# # 有 19433 phe6_DOY_mean<280 ;有 0 phe6_DOY_mean>360

count_less_than_260 <- sum(df$phe6_DOY_mean < 260, na.rm = TRUE)
cat("有", count_less_than_260, "phe6_DOY_mean<260")
count_more_than_340 <- sum(df$phe6_DOY_mean > 340, na.rm = TRUE)
cat("有", count_more_than_340, "phe6_DOY_mean>340")
# 有 2192 phe6_DOY_mean<260 ;有 134 phe6_DOY_mean>340


#设置显示范围
df$phe6_DOY_mean <- ifelse(df$phe6_DOY_mean < 260, 240, 
                           ifelse(df$phe6_DOY_mean > 340, 360, df$phe6_DOY_mean))


# library("scales")
wr <- map_data("world") %>%
  filter(lat > 20)

x_lines <- seq(-120,180, by = 60)# Defines the x axes required


p1 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    #fill = "lightgray"
  geom_tile(data = df, aes(x = long, y = lat, fill = phe6_DOY_mean)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "gray40", fill = alpha("gray40", 0), linewidth = 0.3) +
  expand_limits(x = wr$long, y = wr$lat)  +                           #指定经纬度
  scale_fill_gradientn(colours = c("#003300","#FFCC33","#996600","#663300","#330000"),  # "#006600"  "#336600"
                       na.value = "transparent",
                       name = "DOY",
                       values = scales::rescale(c(240,260,300,340,360)),
                       limits = c(220,340),
                       breaks = c(220,240,280,320,340),  # Specify the breaks for labels
                       labels = c("","240","280","320",""),  # Specify the corresponding labels
                       guide = guide_colorbar(title.position = "bottom",
                                              title.hjust = 0.5,
                                              barwidth = 40,
                                              title.vjust = 1,
                                              barheight = 1.8,
                                              ticks = TRUE,
                                              ticks.colour = "white",
                                              ticks.linewidth = 3.0/.pt))+   #刻度线)) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        axis.ticks=element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        legend.title = element_text(size = 37) ,
        legend.text = element_text(size = 37),
        legend.position = "bottom")+  # Make the legend horizontal)+
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  geom_hline(aes(yintercept = 20), size = 0.1)+  
  geom_segment(size = 0.4 ,aes(y = 20, yend = 90, x = x_lines, xend = x_lines),colour = "gray40",linetype = "dashed") +     #经线
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  #纬线20
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x =  180, xend = 0), colour = "gray20") +
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
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 12) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # 如果要显示北极投影
p1

ggsave(
  filename = "./0.figure/03 Paper/Fig.1-c.Combined_meanDOY&trend9yrs_EOS-a_new.tiff",
  plot = p1,  width = 22,  height = 15,  units = "in",  dpi = 300)  #18


# ############################   计算DOY 9年均值纬度分布   #######################
# 
# dat1 <- df %>%
#   mutate(lat = lat) %>%
#   group_by(lat) %>%
#   summarize(row_means = mean(phe6_DOY_mean),
#             row_sds = sd(phe6_DOY_mean))
# t1 <- ggplot(dat1, aes(x = lat, y = row_means)) +
#   geom_path(size = 1) +  # 增加折线粗细
#   labs(x = "Latitude(°N)", y = "DOY") +
#   geom_ribbon(aes(y = row_means, ymin = row_means - row_sds, ymax = row_means + row_sds), fill = "lightgrey", alpha = 0.5) +
#   theme_classic() +
#   theme(
#     aspect.ratio = 1.5,  # 设置纵横比为 0.25
#     axis.title = element_text(size = 14, face = "plain"),  # 纵坐标字体，设置为加粗
#     axis.text.x = element_text(size = 13, face = "plain"),  # 横坐标刻度字体
#     axis.text.y = element_text(size = 13, face = "plain"),  # 纵坐标刻度字体
#     # plot.title = element_text(size = 16, face = "plain"),  # 设置图形标题字体
#     strip.text.x = element_text(size = 13, face = "plain"),  # 行标题字体
#     strip.text.y = element_text(size = 13, face = "plain")   # 列标题字体
#   )+
#   coord_flip()   # 交换 x 轴和 y 轴
# t1
# 
# ggsave(
#   filename = "./0.figure/03 Paper/Fig.Combined_meanDOY&trend9yrs_EOS-b.tiff",
#   plot = t1,  width = 2.4,   height = 3,  # 总高度 = p1高度 + g1高度
#   units = "in",   dpi = 300 )






# ## 2. 提取各个区域的边界
# Cfa_border <- classify_border[classify_border$EA_koppen_30km_addClimate %in% Cfa, ]
# Cfb_border <- classify_border[classify_border$EA_koppen_30km_addClimate %in% Cfb, ]
# Dfb_border <- classify_border[classify_border$EA_koppen_30km_addClimate %in% Dfb, ]
# Dfc_border <- classify_border[classify_border$EA_koppen_30km_addClimate %in% Dfc, ]
# Dwb_border <- classify_border[classify_border$EA_koppen_30km_addClimate %in% Dwb, ]
# Dwc_border <- classify_border[classify_border$EA_koppen_30km_addClimate %in% Dwc, ]
# 
# ## 3. 掩膜气候区域的DOY数据
# Cfa_doy_stack <- do.call(c, lapply(1:nlyr(r1), function(i) { 
#   mask(r1[[i]], Cfa_border, touches = FALSE) 
# }))
# Cfb_doy_stack <- do.call(c, lapply(1:nlyr(r1), function(i) { 
#   mask(r1[[i]], Cfb_border, touches = FALSE) 
# }))
# Dfb_doy_stack <- do.call(c, lapply(1:nlyr(r1), function(i) { 
#   mask(r1[[i]], Dfb_border, touches = FALSE) 
# }))
# Dfc_doy_stack <- do.call(c, lapply(1:nlyr(r1), function(i) { 
#   mask(r1[[i]], Dfc_border, touches = FALSE) 
# }))
# Dwb_doy_stack <- do.call(c, lapply(1:nlyr(r1), function(i) { 
#   mask(r1[[i]], Dwb_border, touches = FALSE) 
# }))
# Dwc_doy_stack <- do.call(c, lapply(1:nlyr(r1), function(i) { 
#   mask(r1[[i]], Dwc_border, touches = FALSE) 
# }))
# ## 4. 提取Cfa区域的林类数据
# Cfa_forest <- mask(r2, Cfa_border)
# Cfb_forest <- mask(r2, Cfb_border)
# Dfb_forest <- mask(r2, Dfb_border)
# Dfc_forest <- mask(r2, Dfc_border)
# Dwb_forest <- mask(r2, Dwb_border)
# Dwc_forest <- mask(r2, Dwc_border)
# 
# ## 5. 根据林类值创建掩膜条件并提取DOY数据
# Cfa_forest1_DOY <- mask(Cfa_doy_stack, ifel(Cfa_forest == 1, 1, NA))  # 原生林
# Cfa_forest2_DOY <- mask(Cfa_doy_stack, ifel(Cfa_forest == 2, 1, NA))  # 次生林
# Cfa_forest3_DOY <- mask(Cfa_doy_stack, ifel(Cfa_forest == 3, 1, NA))  # 人工林
# 
# Cfb_forest1_DOY <- mask(Cfb_doy_stack, ifel(Cfb_forest == 1, 1, NA))  # 原生林
# Cfb_forest2_DOY <- mask(Cfb_doy_stack, ifel(Cfb_forest == 2, 1, NA))  # 次生林
# Cfb_forest3_DOY <- mask(Cfb_doy_stack, ifel(Cfb_forest == 3, 1, NA))  # 人工林
# 
# Dfb_forest1_DOY <- mask(Dfb_doy_stack, ifel(Dfb_forest == 1, 1, NA))  # 原生林
# Dfb_forest2_DOY <- mask(Dfb_doy_stack, ifel(Dfb_forest == 2, 1, NA))  # 次生林
# Dfb_forest3_DOY <- mask(Dfb_doy_stack, ifel(Dfb_forest == 3, 1, NA))  # 人工林
# 
# Dfc_forest1_DOY <- mask(Dfc_doy_stack, ifel(Dfc_forest == 1, 1, NA))  # 原生林
# Dfc_forest2_DOY <- mask(Dfc_doy_stack, ifel(Dfc_forest == 2, 1, NA))  # 次生林
# Dfc_forest3_DOY <- mask(Dfc_doy_stack, ifel(Dfc_forest == 3, 1, NA))  # 人工林
# 
# Dwb_forest1_DOY <- mask(Dwb_doy_stack, ifel(Dwb_forest == 1, 1, NA))  # 原生林
# Dwb_forest2_DOY <- mask(Dwb_doy_stack, ifel(Dwb_forest == 2, 1, NA))  # 次生林
# Dwb_forest3_DOY <- mask(Dwb_doy_stack, ifel(Dwb_forest == 3, 1, NA))  # 人工林
# 
# Dwc_forest1_DOY <- mask(Dwc_doy_stack, ifel(Dwc_forest == 1, 1, NA))  # 原生林
# Dwc_forest2_DOY <- mask(Dwc_doy_stack, ifel(Dwc_forest == 2, 1, NA))  # 次生林
# Dwc_forest3_DOY <- mask(Dwc_doy_stack, ifel(Dwc_forest == 3, 1, NA))  # 人工林
# 
# 
# # 创建年份序列
# years <- 2013:2022  # 假设数据对应的年份是2013到2022
# 
# # 定义趋势分析函数
# trend_analysis <- function(values) {
#   if (all(is.na(values))) {
#     return(NA)  # 如果所有值都是NA，返回NA
#   }
#   model <- lm(values ~ years)  # 线性回归
#   return(coef(model)[2])  # 返回斜率
# }
# 
# # 应用趋势分析函数到每个像元
# trend_raster_Cfa_forest1_DOY <- app(Cfa_forest1_DOY, trend_analysis)
# trend_raster_Cfa_forest2_DOY <- app(Cfa_forest2_DOY, trend_analysis)
# trend_raster_Cfa_forest3_DOY <- app(Cfa_forest3_DOY, trend_analysis)
# 
# trend_raster_Cfb_forest1_DOY <- app(Cfb_forest1_DOY, trend_analysis)
# trend_raster_Cfb_forest2_DOY <- app(Cfb_forest2_DOY, trend_analysis)
# trend_raster_Cfb_forest3_DOY <- app(Cfb_forest3_DOY, trend_analysis)
# 
# trend_raster_Dfb_forest1_DOY <- app(Dfb_forest1_DOY, trend_analysis)
# trend_raster_Dfb_forest2_DOY <- app(Dfb_forest2_DOY, trend_analysis)
# trend_raster_Dfb_forest3_DOY <- app(Dfb_forest3_DOY, trend_analysis)
# 
# trend_raster_Dfc_forest1_DOY <- app(Dfc_forest1_DOY, trend_analysis)
# trend_raster_Dfc_forest2_DOY <- app(Dfc_forest2_DOY, trend_analysis)
# trend_raster_Dfc_forest3_DOY <- app(Dfc_forest3_DOY, trend_analysis)
# 
# trend_raster_Dwb_forest1_DOY <- app(Dwb_forest1_DOY, trend_analysis)
# trend_raster_Dwb_forest2_DOY <- app(Dwb_forest2_DOY, trend_analysis)
# trend_raster_Dwb_forest3_DOY <- app(Dwb_forest3_DOY, trend_analysis)
# 
# trend_raster_Dwc_forest1_DOY <- app(Dwc_forest1_DOY, trend_analysis)
# trend_raster_Dwc_forest2_DOY <- app(Dwc_forest2_DOY, trend_analysis)
# trend_raster_Dwc_forest3_DOY <- app(Dwc_forest3_DOY, trend_analysis)
# 
# # 提取非 NA 像元并计算均值
# valid_values <- values(trend_raster_Cfa_forest1_DOY, na.rm = TRUE)
# mean_valid <- mean(valid_values)
# print(mean_valid)
