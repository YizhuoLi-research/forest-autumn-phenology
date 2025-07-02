rm(list = ls())

######################################   00 加载包  ##################################################
library(terra)
library(dplyr)
library(raster)
library(ggplot2)
library("scales")

setwd("D:/Graduation_Thesis")

wr <- map_data("world") %>%
  filter(lat > 20)
# head(wr)
x_lines <- seq(-120,180, by = 60)# Defines the x axes required

#####################  01 TXx_phe1_3   ###################


r1 <- rast("./EA+NA_Results/merged_partial_corr_result/with_EOS/partial_corr_TXx_phe1_3.tif")
r2 <- rast("./EA+NA_Results/merged_partial_corr_result/with_EOS/partial_corr_TXge30_phe1_3.tif")
r3 <- rast("./EA+NA_Results/merged_partial_corr_result/with_EOS/partial_corr_TNn_phe1_3.tif")
r4 <- rast("./EA+NA_Results/merged_partial_corr_result/with_EOS/partial_corr_TNlt2_phe1_3.tif")
r5 <- rast("./EA+NA_Results/merged_partial_corr_result/with_EOS/partial_corr_DTR_phe1_3.tif")
r6 <- rast("./EA+NA_Results/merged_partial_corr_result/with_EOS/partial_corr_RainDay_phe1_3.tif")
r7 <- rast("./EA+NA_Results/merged_partial_corr_result/with_EOS/partial_corr_CWD_phe1_3.tif")
r8 <- rast("./EA+NA_Results/merged_partial_corr_result/with_EOS/partial_corr_CDD_phe1_3.tif")
r9 <- rast("./EA+NA_Results/merged_partial_corr_result/with_EOS/partial_corr_RX1day_phe1_3.tif")
r10 <- rast("./EA+NA_Results/merged_partial_corr_result/with_EOS/partial_corr_SDII_phe1_3.tif")
r11 <- rast("./EA+NA_Results/merged_partial_corr_result/with_EOS/partial_corr_merged_Duration_phe1_3.tif")

non_empty_pixels <- sum(!is.na(values(r1)))
print(paste("非空像素数量为:", non_empty_pixels))

df1 <- as.data.frame(r1, xy = TRUE, na.rm = TRUE)  
colnames(df1) <- c("long", "lat","value") 

p1 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    #fill = "lightgray"
  geom_tile(data = df1, aes(x = long, y = lat, fill = value)) +
  
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat)  +                           #指定经纬度
  scale_fill_stepsn(
    colours = c("#000033", "#003366", "#006699", "#3399CC", "#66CCFF", 
                "#fcae91", "#FC8D59", "#FF6633", "#CC3333", "#990000"), # 10 个颜色
    breaks = seq(-1.0, 1.0, length.out = 11),  # 10 个区间需要 11 个断点
    name = "Correlation",  # 图例标题
    limits = c(-1.0, 1.0),  # 值范围
    guide = guide_colorsteps(
      barwidth = 30,  # 调整图例宽度
      barheight = 1.5,  # 调整图例高度
      frame.colour = "gray50",  # 框线颜色
      frame.linewidth = 1  )) + # 框线宽度
  theme_minimal() +
  theme( panel.background = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_blank(),
         axis.text = element_blank(),
         legend.position = "none")+
  # legend.title = element_text(size = 40),
  # legend.text = element_text(size = 40),
  # legend.position = "bottom")+
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  # Adds axes
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
                label = paste0(seq(30, 70, by = 20), "°N")), size = 17) +
  # geom_text(aes( y = 15, x = x_lines,
  #                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 10) +
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 17) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # 如果要显示北极投影
p1

ggsave(
  filename = "./0.figure/03 Paper/Fig.S.EOS/Fig.S-EOS_TXX_phe1_3.tiff",
  plot = p1,  width = 15,  height = 15,  units = "in",  dpi = 300)

#### 计算正、负值的数量、总数量（排除 NA 值）# 计算正、负值的占比（保留到整数的百分数）
positive_count <- sum(df1$value > 0, na.rm = TRUE)
negative_count <- sum(df1$value < 0, na.rm = TRUE)
total_count <- sum(!is.na(df1$value))
positive_ratio <- positive_count / total_count * 100
negative_ratio <- negative_count / total_count * 100
cat("正值的占比:", round(positive_ratio), "%\n")   # 52 %
cat("负值的占比:", round(negative_ratio), "%\n")   # 48 %


#####################  02 TXge30_phe1_3   ###################


r2 <- rast("./EA+NA_Results/merged_partial_corr_result/with_EOS/partial_corr_TXge30_phe1_3.tif")

df2 <- as.data.frame(r2, xy = TRUE, na.rm = TRUE)  
colnames(df2) <- c("long", "lat","value") 

p2 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    #fill = "lightgray"
  geom_tile(data = df2, aes(x = long, y = lat, fill = value)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat)  +                           #指定经纬度
  scale_fill_stepsn(
    colours = c("#000033", "#003366", "#006699", "#3399CC", "#66CCFF", 
                "#fcae91", "#FC8D59", "#FF6633", "#CC3333", "#990000"), # 10 个颜色
    breaks = seq(-1.0, 1.0, length.out = 11),  # 10 个区间需要 11 个断点
    name = "Correlation",  # 图例标题
    limits = c(-1.0, 1.0),  # 值范围
    guide = guide_colorsteps(
      barwidth = 30,  # 调整图例宽度
      barheight = 1.5,  # 调整图例高度
      frame.colour = "gray50",  # 框线颜色
      frame.linewidth = 1  )) + # 框线宽度
  theme_minimal() +
  theme( panel.background = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_blank(),
         axis.text = element_blank(),
         legend.position = "none")+
  # legend.title = element_text(size = 40),
  # legend.text = element_text(size = 40),
  # legend.position = "bottom")+
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  # Adds axes
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
                label = paste0(seq(30, 70, by = 20), "°N")), size = 17) +
  # geom_text(aes( y = 15, x = x_lines,
  #                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 10) +
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 17) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # 如果要显示北极投影
p2

ggsave(
  filename = "./0.figure/03 Paper/Fig.S.EOS/Fig.S-EOS_TXge30_phe1_3.tiff",
  plot = p2,  width = 15,  height = 15,  units = "in",  dpi = 300)

#### 计算正、负值的数量、总数量（排除 NA 值）# 计算正、负值的占比（保留到整数的百分数）
positive_count <- sum(df2$value > 0, na.rm = TRUE)
negative_count <- sum(df2$value < 0, na.rm = TRUE)
total_count <- sum(!is.na(df2$value))
positive_ratio <- positive_count / total_count * 100
negative_ratio <- negative_count / total_count * 100
cat("正值的占比:", round(positive_ratio), "%\n")   # 55%
cat("负值的占比:", round(negative_ratio), "%\n")   # 45%


#####################  03 TNn_phe1_3   ###################


r3 <- rast("./EA+NA_Results/merged_partial_corr_result/with_EOS/partial_corr_TNn_phe1_3.tif")

df3 <- as.data.frame(r3, xy = TRUE, na.rm = TRUE)  
colnames(df3) <- c("long", "lat","value") 

p3 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    #fill = "lightgray"
  geom_tile(data = df3, aes(x = long, y = lat, fill = value)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat)  +                           #指定经纬度
  scale_fill_stepsn(
    colours = c("#000033", "#003366", "#006699", "#3399CC", "#66CCFF", 
                "#fcae91", "#FC8D59", "#FF6633", "#CC3333", "#990000"), # 10 个颜色
    breaks = seq(-1.0, 1.0, length.out = 11),  # 10 个区间需要 11 个断点
    name = "Correlation",  # 图例标题
    limits = c(-1.0, 1.0),  # 值范围
    guide = guide_colorsteps(
      barwidth = 30,  # 调整图例宽度
      barheight = 1.5,  # 调整图例高度
      frame.colour = "gray50",  # 框线颜色
      frame.linewidth = 1  )) + # 框线宽度
  theme_minimal() +
  theme( panel.background = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_blank(),
         axis.text = element_blank(),
         legend.position = "none")+
  # legend.title = element_text(size = 40),
  # legend.text = element_text(size = 40),
  # legend.position = "bottom")+
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  # Adds axes
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
                label = paste0(seq(30, 70, by = 20), "°N")), size = 17) +
  # geom_text(aes( y = 15, x = x_lines,
  #                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 10) +
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 17) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # 如果要显示北极投影
p3

ggsave(
  filename = "./0.figure/03 Paper/Fig.S.EOS/Fig.S-EOS_TNn_phe1_3.tiff",
  plot = p3,  width = 15,  height = 15,  units = "in",  dpi = 300)

#### 计算正、负值的数量、总数量（排除 NA 值）# 计算正、负值的占比（保留到整数的百分数）
positive_count <- sum(df3$value > 0, na.rm = TRUE)
negative_count <- sum(df3$value < 0, na.rm = TRUE)
total_count <- sum(!is.na(df3$value))
positive_ratio <- positive_count / total_count * 100
negative_ratio <- negative_count / total_count * 100
cat("正值的占比:", round(positive_ratio), "%\n")   # 49%
cat("负值的占比:", round(negative_ratio), "%\n")   # 51%


#####################  04 TNlt2_phe1_3   ###################


r4 <- rast("./EA+NA_Results/merged_partial_corr_result/with_EOS/partial_corr_TNlt2_phe1_3.tif")

df4 <- as.data.frame(r4, xy = TRUE, na.rm = TRUE)  
colnames(df4) <- c("long", "lat","value") 

p4 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    #fill = "lightgray"
  geom_tile(data = df4, aes(x = long, y = lat, fill = value)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat)  +                           #指定经纬度
  scale_fill_stepsn(
    colours = c("#000033", "#003366", "#006699", "#3399CC", "#66CCFF", 
                "#fcae91", "#FC8D59", "#FF6633", "#CC3333", "#990000"), # 10 个颜色
    breaks = seq(-1.0, 1.0, length.out = 11),  # 10 个区间需要 11 个断点
    name = "Correlation",  # 图例标题
    limits = c(-1.0, 1.0),  # 值范围
    guide = guide_colorsteps(
      barwidth = 30,  # 调整图例宽度
      barheight = 1.5,  # 调整图例高度
      frame.colour = "gray50",  # 框线颜色
      frame.linewidth = 1  )) + # 框线宽度
  theme_minimal() +
  theme( panel.background = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_blank(),
         axis.text = element_blank(),
         legend.position = "none")+
  # legend.title = element_text(size = 40),
  # legend.text = element_text(size = 40),
  # legend.position = "bottom")+
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  # Adds axes
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
                label = paste0(seq(30, 70, by = 20), "°N")), size = 17) +
  # geom_text(aes( y = 15, x = x_lines,
  #                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 10) +
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 17) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # 如果要显示北极投影
p4

ggsave(
  filename = "./0.figure/03 Paper/Fig.S.EOS/Fig.S-EOS_TNlt2_phe1_3.tiff",
  plot = p4,  width = 15,  height = 15,  units = "in",  dpi = 300)

#### 计算正、负值的数量、总数量（排除 NA 值）# 计算正、负值的占比（保留到整数的百分数）
positive_count <- sum(df4$value > 0, na.rm = TRUE)
negative_count <- sum(df4$value < 0, na.rm = TRUE)
total_count <- sum(!is.na(df4$value))
positive_ratio <- positive_count / total_count * 100
negative_ratio <- negative_count / total_count * 100
cat("正值的占比:", round(positive_ratio), "%\n")   # 50%
cat("负值的占比:", round(negative_ratio), "%\n")   # 50%


#####################  05 DTR_phe1_3   ###################


r5 <- rast("./EA+NA_Results/merged_partial_corr_result/with_EOS/partial_corr_DTR_phe1_3.tif")

df5 <- as.data.frame(r5, xy = TRUE, na.rm = TRUE)  
colnames(df5) <- c("long", "lat","value") 

p5 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    #fill = "lightgray"
  geom_tile(data = df5, aes(x = long, y = lat, fill = value)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat)  +                           #指定经纬度
  scale_fill_stepsn(
    colours = c("#000033", "#003366", "#006699", "#3399CC", "#66CCFF", 
                "#fcae91", "#FC8D59", "#FF6633", "#CC3333", "#990000"), # 10 个颜色
    breaks = seq(-1.0, 1.0, length.out = 11),  # 10 个区间需要 11 个断点
    name = "Correlation",  # 图例标题
    limits = c(-1.0, 1.0),  # 值范围
    guide = guide_colorsteps(
      barwidth = 30,  # 调整图例宽度
      barheight = 1.5,  # 调整图例高度
      frame.colour = "gray50",  # 框线颜色
      frame.linewidth = 1  )) + # 框线宽度
  theme_minimal() +
  theme( panel.background = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_blank(),
         axis.text = element_blank(),
         legend.position = "none")+
  # legend.title = element_text(size = 40),
  # legend.text = element_text(size = 40),
  # legend.position = "bottom")+
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  # Adds axes
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
                label = paste0(seq(30, 70, by = 20), "°N")), size = 17) +
  # geom_text(aes( y = 15, x = x_lines,
  #                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 10) +
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 17) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # 如果要显示北极投影
p5

ggsave(
  filename = "./0.figure/03 Paper/Fig.S.EOS/Fig.S-EOS_DTR_phe1_3.tiff",
  plot = p5,  width = 15,  height = 15,  units = "in",  dpi = 300)

#### 计算正、负值的数量、总数量（排除 NA 值）# 计算正、负值的占比（保留到整数的百分数）
positive_count <- sum(df5$value > 0, na.rm = TRUE)
negative_count <- sum(df5$value < 0, na.rm = TRUE)
total_count <- sum(!is.na(df5$value))
positive_ratio <- positive_count / total_count * 100
negative_ratio <- negative_count / total_count * 100
cat("正值的占比:", round(positive_ratio), "%\n")   #  50%
cat("负值的占比:", round(negative_ratio), "%\n")   #  50%


#####################  06 Rainday_phe1_3   ###################


r6 <- rast("./EA+NA_Results/merged_partial_corr_result/with_EOS/partial_corr_RainDay_phe1_3.tif")

df6 <- as.data.frame(r6, xy = TRUE, na.rm = TRUE)  
colnames(df6) <- c("long", "lat","value") 

p6 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    #fill = "lightgray"
  geom_tile(data = df6, aes(x = long, y = lat, fill = value)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat)  +                           #指定经纬度
  scale_fill_stepsn(
    colours = c("#000033", "#003366", "#006699", "#3399CC", "#66CCFF", 
                "#fcae91", "#FC8D59", "#FF6633", "#CC3333", "#990000"), # 10 个颜色
    breaks = seq(-1.0, 1.0, length.out = 11),  # 10 个区间需要 11 个断点
    name = "Correlation",  # 图例标题
    limits = c(-1.0, 1.0),  # 值范围
    guide = guide_colorsteps(
      barwidth = 30,  # 调整图例宽度
      barheight = 1.5,  # 调整图例高度
      frame.colour = "gray50",  # 框线颜色
      frame.linewidth = 1  )) + # 框线宽度
  theme_minimal() +
  theme( panel.background = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_blank(),
         axis.text = element_blank(),
         legend.position = "none")+
  # legend.title = element_text(size = 40),
  # legend.text = element_text(size = 40),
  # legend.position = "bottom")+
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  # Adds axes
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
                label = paste0(seq(30, 70, by = 20), "°N")), size = 17) +
  # geom_text(aes( y = 15, x = x_lines,
  #                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 10) +
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 17) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # 如果要显示北极投影
p6

ggsave(
  filename = "./0.figure/03 Paper/Fig.S.EOS/Fig.S-EOS_Rainday_phe1_3.tiff",
  plot = p6,  width = 15,  height = 15,  units = "in",  dpi = 300)

#### 计算正、负值的数量、总数量（排除 NA 值）# 计算正、负值的占比（保留到整数的百分数）
positive_count <- sum(df6$value > 0, na.rm = TRUE)
negative_count <- sum(df6$value < 0, na.rm = TRUE)
total_count <- sum(!is.na(df6$value))
positive_ratio <- positive_count / total_count * 100
negative_ratio <- negative_count / total_count * 100
cat("正值的占比:", round(positive_ratio), "%\n")   # 53 %
cat("负值的占比:", round(negative_ratio), "%\n")   # 47 %


#####################  07 CWD_phe1_3   ###################


r7 <- rast("./EA+NA_Results/merged_partial_corr_result/with_EOS/partial_corr_CWD_phe1_3.tif")

df7 <- as.data.frame(r7, xy = TRUE, na.rm = TRUE)  
colnames(df7) <- c("long", "lat","value") 

p7 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    #fill = "lightgray"
  geom_tile(data = df7, aes(x = long, y = lat, fill = value)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat)  +                           #指定经纬度
  scale_fill_stepsn(
    colours = c("#000033", "#003366", "#006699", "#3399CC", "#66CCFF", 
                "#fcae91", "#FC8D59", "#FF6633", "#CC3333", "#990000"), # 10 个颜色
    breaks = seq(-1.0, 1.0, length.out = 11),  # 10 个区间需要 11 个断点
    name = "Correlation",  # 图例标题
    limits = c(-1.0, 1.0),  # 值范围
    guide = guide_colorsteps(
      barwidth = 30,  # 调整图例宽度
      barheight = 1.5,  # 调整图例高度
      frame.colour = "gray50",  # 框线颜色
      frame.linewidth = 1  )) + # 框线宽度
  theme_minimal() +
  theme( panel.background = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_blank(),
         axis.text = element_blank(),
         legend.position = "none")+
  # legend.title = element_text(size = 40),
  # legend.text = element_text(size = 40),
  # legend.position = "bottom")+
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  # Adds axes
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
                label = paste0(seq(30, 70, by = 20), "°N")), size = 17) +
  # geom_text(aes( y = 15, x = x_lines,
  #                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 10) +
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 17) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # 如果要显示北极投影
p7

ggsave(
  filename = "./0.figure/03 Paper/Fig.S.EOS/Fig.S-EOS_CWD_phe1_3.tiff",
  plot = p7,  width = 15,  height = 15,  units = "in",  dpi = 300)

#### 计算正、负值的数量、总数量（排除 NA 值）# 计算正、负值的占比（保留到整数的百分数）
positive_count <- sum(df7$value > 0, na.rm = TRUE)
negative_count <- sum(df7$value < 0, na.rm = TRUE)
total_count <- sum(!is.na(df7$value))
positive_ratio <- positive_count / total_count * 100
negative_ratio <- negative_count / total_count * 100
cat("正值的占比:", round(positive_ratio), "%\n")   # 51 %
cat("负值的占比:", round(negative_ratio), "%\n")   # 49 %

#####################  08 CDD_phe1_3   ###################


r8 <- rast("./EA+NA_Results/merged_partial_corr_result/with_EOS/partial_corr_CDD_phe1_3.tif")

df8 <- as.data.frame(r8, xy = TRUE, na.rm = TRUE)  
colnames(df8) <- c("long", "lat","value") 

p8 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    #fill = "lightgray"
  geom_tile(data = df8, aes(x = long, y = lat, fill = value)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat)  +                           #指定经纬度
  scale_fill_stepsn(
    colours = c("#000033", "#003366", "#006699", "#3399CC", "#66CCFF", 
                "#fcae91", "#FC8D59", "#FF6633", "#CC3333", "#990000"), # 10 个颜色
    breaks = seq(-1.0, 1.0, length.out = 11),  # 10 个区间需要 11 个断点
    name = "Correlation",  # 图例标题
    limits = c(-1.0, 1.0),  # 值范围
    guide = guide_colorsteps(
      barwidth = 30,  # 调整图例宽度
      barheight = 1.5,  # 调整图例高度
      frame.colour = "gray50",  # 框线颜色
      frame.linewidth = 1  )) + # 框线宽度
  theme_minimal() +
  theme( panel.background = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_blank(),
         axis.text = element_blank(),
         legend.position = "none")+
  # legend.title = element_text(size = 40),
  # legend.text = element_text(size = 40),
  # legend.position = "bottom")+
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  # Adds axes
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
                label = paste0(seq(30, 70, by = 20), "°N")), size = 17) +
  # geom_text(aes( y = 15, x = x_lines,
  #                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 10) +
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 17) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # 如果要显示北极投影
p8

ggsave(
  filename = "./0.figure/03 Paper/Fig.S.EOS/Fig.S-EOS_CDD_phe1_3.tiff",
  plot = p8,  width = 15,  height = 15,  units = "in",  dpi = 300)

#### 计算正、负值的数量、总数量（排除 NA 值）# 计算正、负值的占比（保留到整数的百分数）
positive_count <- sum(df8$value > 0, na.rm = TRUE)
negative_count <- sum(df8$value < 0, na.rm = TRUE)
total_count <- sum(!is.na(df8$value))
positive_ratio <- positive_count / total_count * 100
negative_ratio <- negative_count / total_count * 100
cat("正值的占比:", round(positive_ratio), "%\n")   # 50 %
cat("负值的占比:", round(negative_ratio), "%\n")   #  50 %

#####################  09 Rx1day_phe1_3   ###################


r9 <- rast("./EA+NA_Results/merged_partial_corr_result/with_EOS/partial_corr_Rx1day_phe1_3.tif")

df9 <- as.data.frame(r9, xy = TRUE, na.rm = TRUE)  
colnames(df9) <- c("long", "lat","value") 

p9 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    #fill = "lightgray"
  geom_tile(data = df9, aes(x = long, y = lat, fill = value)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat)  +                           #指定经纬度
  scale_fill_stepsn(
    colours = c("#000033", "#003366", "#006699", "#3399CC", "#66CCFF", 
                "#fcae91", "#FC8D59", "#FF6633", "#CC3333", "#990000"), # 10 个颜色
    breaks = seq(-1.0, 1.0, length.out = 11),  # 10 个区间需要 11 个断点
    name = "Correlation",  # 图例标题
    limits = c(-1.0, 1.0),  # 值范围
    guide = guide_colorsteps(
      barwidth = 30,  # 调整图例宽度
      barheight = 1.5,  # 调整图例高度
      frame.colour = "gray50",  # 框线颜色
      frame.linewidth = 1  )) + # 框线宽度
  theme_minimal() +
  theme( panel.background = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_blank(),
         axis.text = element_blank(),
         legend.position = "none")+
  # legend.title = element_text(size = 40),
  # legend.text = element_text(size = 40),
  # legend.position = "bottom")+
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  # Adds axes
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
                label = paste0(seq(30, 70, by = 20), "°N")), size = 17) +
  # geom_text(aes( y = 15, x = x_lines,
  #                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 10) +
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 17) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # 如果要显示北极投影
p9

ggsave(
  filename = "./0.figure/03 Paper/Fig.S.EOS/Fig.S-EOS_Rx1day_phe1_3.tiff",
  plot = p9,  width = 15,  height = 15,  units = "in",  dpi = 300)

#### 计算正、负值的数量、总数量（排除 NA 值）# 计算正、负值的占比（保留到整数的百分数）
positive_count <- sum(df9$value > 0, na.rm = TRUE)
negative_count <- sum(df9$value < 0, na.rm = TRUE)
total_count <- sum(!is.na(df9$value))
positive_ratio <- positive_count / total_count * 100
negative_ratio <- negative_count / total_count * 100
cat("正值的占比:", round(positive_ratio), "%\n")   #  50%
cat("负值的占比:", round(negative_ratio), "%\n")   #  50%


#####################  10 SDII_phe1_3   ###################


r10 <- rast("./EA+NA_Results/merged_partial_corr_result/with_EOS/partial_corr_SDII_phe1_3.tif")

df10 <- as.data.frame(r10, xy = TRUE, na.rm = TRUE)  
colnames(df10) <- c("long", "lat","value") 

p10 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    #fill = "lightgray"
  geom_tile(data = df10, aes(x = long, y = lat, fill = value)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat)  +                           #指定经纬度
  scale_fill_stepsn(
    colours = c("#000033", "#003366", "#006699", "#3399CC", "#66CCFF", 
                "#fcae91", "#FC8D59", "#FF6633", "#CC3333", "#990000"), # 10 个颜色
    breaks = seq(-1.0, 1.0, length.out = 11),  # 10 个区间需要 11 个断点
    name = "Correlation",  # 图例标题
    limits = c(-1.0, 1.0),  # 值范围
    guide = guide_colorsteps(
      barwidth = 30,  # 调整图例宽度
      barheight = 1.5,  # 调整图例高度
      frame.colour = "gray50",  # 框线颜色
      frame.linewidth = 1  )) + # 框线宽度
  theme_minimal() +
  theme( panel.background = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_blank(),
         axis.text = element_blank(),
         legend.position = "none")+
  # legend.title = element_text(size = 40),
  # legend.text = element_text(size = 40),
  # legend.position = "bottom")+
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  # Adds axes
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
                label = paste0(seq(30, 70, by = 20), "°N")), size = 17) +
  # geom_text(aes( y = 15, x = x_lines,
  #                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 10) +
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 17) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # 如果要显示北极投影
p10

ggsave(
  filename = "./0.figure/03 Paper/Fig.S.EOS/Fig.S-EOS_SDII_phe1_3.tiff",
  plot = p10,  width = 15,  height = 15,  units = "in",  dpi = 300)

#### 计算正、负值的数量、总数量（排除 NA 值）# 计算正、负值的占比（保留到整数的百分数）
positive_count <- sum(df10$value > 0, na.rm = TRUE)
negative_count <- sum(df10$value < 0, na.rm = TRUE)
total_count <- sum(!is.na(df10$value))
positive_ratio <- positive_count / total_count * 100
negative_ratio <- negative_count / total_count * 100
cat("正值的占比:", round(positive_ratio), "%\n")   # 50 %
cat("负值的占比:", round(negative_ratio), "%\n")   # 50 %


#####################  11 Duration_phe1_3   ###################


r11 <- rast("./EA+NA_Results/merged_partial_corr_result/with_EOS/partial_corr_merged_Duration_phe1_3.tif")

df11 <- as.data.frame(r11, xy = TRUE, na.rm = TRUE)  
colnames(df11) <- c("long", "lat","value") 


p11 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    #fill = "lightgray"
  geom_tile(data = df11, aes(x = long, y = lat, fill = value)) +
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "black", fill = alpha("black", 0), linewidth = 0.5) +
  expand_limits(x = wr$long, y = wr$lat)  +                           #指定经纬度
  scale_fill_stepsn(
    colours = c("#000033", "#003366", "#006699", "#3399CC", "#66CCFF", 
                "#fcae91", "#FC8D59", "#FF6633", "#CC3333", "#990000"), # 10 个颜色
    breaks = seq(-1.0, 1.0, length.out = 11),  # 10 个区间需要 11 个断点
    name = "Correlation",  # 图例标题
    limits = c(-1.0, 1.0),  # 值范围
    guide = guide_colorsteps(
      barwidth = 30,  # 调整图例宽度
      barheight = 1.5,  # 调整图例高度
      frame.colour = "gray50",  # 框线颜色
      frame.linewidth = 1  )) + # 框线宽度
  theme_minimal() +
  theme( panel.background = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_blank(),
         axis.text = element_blank(),
         legend.position = "none")+
  # legend.title = element_text(size = 40),
  # legend.text = element_text(size = 40),
  # legend.position = "bottom")+
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  # Adds axes
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
                label = paste0(seq(30, 70, by = 20), "°N")), size = 17) +
  # geom_text(aes( y = 15, x = x_lines,
  #                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 10) +
  geom_text(aes( y = 15+ c(-2, -2, 1, -2, -2, 1), x = x_lines,
                 label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 17) +
  coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # 如果要显示北极投影
p11

ggsave(
  filename = "./0.figure/03 Paper/Fig.S.EOS/Fig.S-EOS_Duration_phe1_3.tiff",
  plot = p11,  width = 15,  height = 15,  units = "in",  dpi = 300)

#### 计算正、负值的数量、总数量（排除 NA 值）# 计算正、负值的占比（保留到整数的百分数）
positive_count <- sum(df11$value > 0, na.rm = TRUE)
negative_count <- sum(df11$value < 0, na.rm = TRUE)
total_count <- sum(!is.na(df11$value))
positive_ratio <- positive_count / total_count * 100
negative_ratio <- negative_count / total_count * 100
cat("正值的占比:", round(positive_ratio), "%\n")   # 51 %
cat("负值的占比:", round(negative_ratio), "%\n")   # 49 %




