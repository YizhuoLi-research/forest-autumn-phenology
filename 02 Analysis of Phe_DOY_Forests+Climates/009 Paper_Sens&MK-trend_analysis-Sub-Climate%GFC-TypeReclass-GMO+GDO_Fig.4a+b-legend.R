p1 <- ggplot() +
  geom_map(aes(map_id = region), map = wr, data = wr, fill = "gray90") +    # 画地图背景
  geom_tile(data = all_data, aes(x = Longitude , y = Latitude , fill = class)) +  # 使用class列填充地图
  geom_map(aes(map_id = region), map = wr, data = wr,
           colour = "gray40", fill = alpha("gray40", 0), linewidth = 0.3) +
  expand_limits(x = wr$long, y = wr$lat) +  # 指定经纬度范围
  scale_fill_manual(
    values = c("-2" = "#006699",   # 显著延迟
               "-1" = "lightblue",  # 轻微延迟
               "0" = "#FFFFCC",       # 稳定不变
               "1" = "#FC8D59",      # 轻微提前
               "2" = "#CC3333"),        # 显著提前
    na.value = "transparent",  # 对于缺失值设置透明
    name = "Gradation of phenological trend",  # 设置图例标题为 "Trend"
    labels = c("-2" = "Significant delay",  # 图例标签
               "-1" = "Slight delay", 
               "0" = "Stable", 
               "1" = "Slight advance", 
               "2" = "Significant advance"),  # 对应英文标签
    guide = guide_legend(
      title.position = "top", 
      title.hjust = 0.5,  # 标题居中
      label.position = "right",
      title.vjust = 0.5,
      label.vjust = 0.5,
      keyheight = 0.7,  # 设置色块高度，减小为扁平
      keywidth = 1.2,     # 设置色块宽度，增大为宽扁
      direction = "horizontal",  # 使图例横向排列
      override.aes = list(colour = "black"),  # 给每个色块加黑色边框
      label.spacing = unit(0.05, "cm")  # 缩小标签间距
    )) +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.title = element_text(size = 27, hjust = 0),  # 标题左对齐
    legend.text = element_text(size = 27, vjust = 0.5),
    legend.position = "bottom",  # 图例放置在下方
    legend.direction = "horizontal"  # 图例横向排列
  ) +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  xlab("") + 
  ylab("") +
  geom_hline(aes(yintercept = 20), size = 0.1) +  
  geom_segment(size = 0.4 ,aes(y = 20, yend = 90, x = x_lines, xend = x_lines), colour = "gray40", linetype = "dashed") +  # 经线
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = -180, xend = 0), colour = "gray20") +  # 纬线20
  geom_segment(size = 0.8 ,aes(y = 20, yend = 20, x = 180, xend = 0), colour = "gray20") +
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  #30
  geom_segment(size = 0.4 ,aes(y = 30, yend = 30, x =  180, xend = 0), colour = "gray40", linetype = "dashed") + 
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  #50
  geom_segment(size = 0.4 ,aes(y = 50, yend = 50, x =  180, xend = 0), colour = "gray40", linetype = "dashed") +
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x = -180, xend = 0), colour = "gray40", linetype = "dashed") +  #70
  geom_segment(size = 0.4 ,aes(y = 70, yend = 70, x =  180, xend = 0), colour = "gray40", linetype = "dashed") +
  # Adds labels
  geom_text(aes(x = 180, y = seq(30, 70, by = 20), hjust = -0.1, vjust = -0.15, 
                label = paste0(seq(30, 70, by = 20), "°N")), size = 12) +
  geom_text(aes(y = 15 + c(-2.5, -2.5, 1, -2, -2, 1), x = x_lines,
                label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")), size = 12) 
  # coord_map(projection = "stereographic", orientation = c(90, 0, 0))  # 如果要显示北极投影

print(p1)
# 保存图像
ggsave(
  filename = "./0.figure/03 Paper/Fig.4-b.Classified_map_EOS-legend.tiff",
  plot = p1, width = 15, height = 15, units = "in", dpi = 300)
