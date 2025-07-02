
library(dplyr)
library(tidyr)
library(ggpubr)
library(ggplot2)

setwd("D:/Graduation_Thesis")

###########################  重要性排序作图 Fig.8  ######################

importance_data <- read.csv("./EA+NA_Results/merged_Indices_Importance_result/EOS_Indices_Importance_Results.csv")
# 查看数据前几行
head(importance_data)
# 查看数据结构
str(importance_data)

importance_data$IncMSE <- round(importance_data$IncMSE, digits = 2)

# 创建imp_df_13：筛选_phe1_3结尾的变量
imp_df_13 <- importance_data %>%
  filter(grepl("_phe1_3$", Variable)) %>%  # 筛选以_phe1_3结尾的变量
  mutate(Variable = gsub("_phe1_3$", "", Variable)) %>%  # 去掉后缀
  mutate(Variable = ifelse(Variable == "RainDay", "Rainday", Variable)) %>% #重命名RainDay
  mutate(Variable = ifelse(Variable == "merged_Duration", "Duration", Variable))  # 重命名RainDay

# 创建imp_df_34：筛选_phe3_4结尾的变量
imp_df_34 <- importance_data %>%
  filter(grepl("_phe3_4$", Variable)) %>%  # 筛选以_phe3_4结尾的变量
  mutate(Variable = gsub("_phe3_4$", "", Variable)) %>%  # 去掉后缀
  mutate(Variable = ifelse(Variable == "RainDay", "Rainday", Variable)) %>% #重命名RainDay
  mutate(Variable = ifelse(Variable == "merged_Duration", "Duration", Variable))  # 重命名RainDay

# 创建imp_df_46：筛选_phe3_4结尾的变量
imp_df_46 <- importance_data %>%
  filter(grepl("_phe4_6$", Variable)) %>%  # 筛选以_phe3_4结尾的变量
  mutate(Variable = gsub("_phe4_6$", "", Variable)) %>%  # 去掉后缀
  mutate(Variable = ifelse(Variable == "RainDay", "Rainday", Variable)) %>% #重命名RainDay
  mutate(Variable = ifelse(Variable == "merged_Duration", "Duration", Variable))  # 重命名RainDay

# 检查结果
# head(imp_df_13);# str(imp_df_13)
# head(imp_df_34);# str(imp_df_34)
# head(imp_df_46);# str(imp_df_46)


####################################### 01 对Cfa区域#####  #########################################
######01-1 Cfa_phe13   #########

# 数据加载和预处理
Cfa_phe13_data <- imp_df_13 %>% 
  # 筛选Cfa气候区
  filter(Climate_Class == "Cfa") %>%
  # 转换森林类型为因子
  mutate(Forest_Type = factor(Forest_Type, 
                              levels = c("2", "1", "3"),
                              labels = c("PF", "NRF", "P"))) %>%
  # 获取NRF的重要性值并排序
  group_by(Variable) %>%
  mutate(NRF_Importance = IncMSE[Forest_Type == "NRF"]) %>%
  ungroup() %>%
  # 按NRF重要性降序排列
  arrange(desc(NRF_Importance)) %>%
  # 将Variable转换为因子（关键修改：使用rev()使最重要的在顶部）
  mutate(Variable = factor(Variable, levels = rev(unique(Variable))))


# 查看最大最小值   0.07  3.98
Cfa_phe13_data %>% 
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

Cfa_phe13_data %>%
  filter(Variable == "Duration") %>%
  select(Variable, Forest_Type, IncMSE, NRF_Importance) %>%  # 选择需要显示的列
  print(n = Inf)  # 确保显示所有行（不截断）
# Variable Forest_Type IncMSE NRF_Importance
# 1 Duration NRF           3.98           3.98
# 2 Duration PF            0.89           3.98
# 3 Duration P             3.77           3.98

# 移除phe13的Duration后，查看最大最小值作图  0 3.66
Cfa_phe13_data <- Cfa_phe13_data[Cfa_phe13_data$Variable != "Duration", ]
Cfa_phe13_data %>%
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

## 定义颜色和形状
forest_colors <- c("PF" = "#2E8B57",  # 原生林-深绿
                   "NRF" = "#9ACD32", # 再生林-浅绿
                   "P" = "#FFA500")   # 人工林-橙色


# 创建绘图
p_phe13_Cfa <- ggplot(Cfa_phe13_data, aes(x = Variable, y = IncMSE, color = Forest_Type)) +
  # geom_line(aes(group = Variable), color = "grey50",linewidth = 0.8,alpha = 0.7 ) +
  # geom_point(shape = 16, size = 5, alpha = 0.9) + #数据点（统一圆形，大小4，透明度0.9）
  geom_point(shape = 1, size = 2, stroke = 2, alpha = 0.9) +
  scale_color_manual(values = forest_colors) +
  # scale_shape_manual(values = c("PF" = 16,"NRF" = 17, "P" = 15)) +   # 原生林：实心圆；# 再生林：实心三角；# 人工林：实心方块
  scale_y_continuous(expand = c(0, 0), limits = c(0, 18), breaks = c(0, 8, 16)) +
  labs(title = "Cfa",
       x = "",
       y = "%IncMSE") +
  theme_classic() + # 使用classic主题作为基础
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
        plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
        legend.position = "none",
        panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
        panel.background = element_blank(),
        # panel.grid = element_blank()         # 网格线（全部移除）
        panel.grid.major.y = element_line(color = "grey50",linetype = "dashed",linewidth = 0.5), # x轴对应的网格线（翻转后为垂直）
        panel.grid.minor.y = element_blank(),  # 移除次要网格线
        panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
        panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe13_Cfa)

######01-2 Cfa_phe34   #########

# 数据加载和预处理
Cfa_phe34_data <- imp_df_34 %>% 
  # 筛选Cfa气候区
  filter(Climate_Class == "Cfa") %>%
  # 转换森林类型为因子
  mutate(Forest_Type = factor(Forest_Type, 
                              levels = c("2", "1", "3"),
                              labels = c("PF", "NRF", "P"))) %>%
  # 获取NRF的重要性值并排序
  group_by(Variable) %>%
  mutate(NRF_Importance = IncMSE[Forest_Type == "NRF"]) %>%
  ungroup() %>%
  # 按NRF重要性降序排列
  arrange(desc(NRF_Importance)) %>%
  # 将Variable转换为因子（关键修改：使用rev()使最重要的在顶部）
  mutate(Variable = factor(Variable, levels = rev(unique(Variable))))


# 查看最大最小值   0.0  26.1
Cfa_phe34_data %>% 
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))
Cfa_phe34_data$IncMSE[Cfa_phe34_data$IncMSE < 0] <- 0

Cfa_phe34_data %>%
  filter(Variable == "Duration") %>%
  select(Variable, Forest_Type, IncMSE, NRF_Importance) %>%  # 选择需要显示的列
  print(n = Inf)  # 确保显示所有行（不截断）
# Variable Forest_Type IncMSE NRF_Importance
# 1 Duration NRF           26.1           26.1
# 2 Duration PF            20.6           26.1
# 3 Duration P             13.8           26.1

# 移除phe13的Duration后，查看最大最小值作图  0 12
Cfa_phe34_data <- Cfa_phe34_data[Cfa_phe34_data$Variable != "Duration", ]
Cfa_phe34_data %>%
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

## 定义颜色和形状
forest_colors <- c("PF" = "#2E8B57",  # 原生林-深绿
                   "NRF" = "#9ACD32", # 再生林-浅绿
                   "P" = "#FFA500")   # 人工林-橙色


# 创建绘图
p_phe34_Cfa <- ggplot(Cfa_phe34_data, aes(x = Variable, y = IncMSE, color = Forest_Type)) +
  # geom_line(aes(group = Variable), color = "grey50",linewidth = 0.8,alpha = 0.7 ) +
  # geom_point(shape = 16, size = 5, alpha = 0.9) + #数据点（统一圆形，大小4，透明度0.9）
  geom_point(shape = 1, size = 2, stroke = 2, alpha = 0.9) +
  scale_color_manual(values = forest_colors) +
  # scale_shape_manual(values = c("PF" = 16,"NRF" = 17, "P" = 15)) +   # 原生林：实心圆；# 再生林：实心三角；# 人工林：实心方块
  scale_y_continuous(expand = c(0, 0), limits = c(0, 18), breaks = c(0, 8, 16)) +
  labs(title = "Cfa",
       x = "",
       y = "%IncMSE") +
  theme_classic() + # 使用classic主题作为基础
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "none",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    # panel.grid = element_blank()         # 网格线（全部移除）
    panel.grid.major.y = element_line(color = "grey50",linetype = "dashed",linewidth = 0.5), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe34_Cfa)


######01-3 Cfa_phe46   #########

# 数据加载和预处理
Cfa_phe46_data <- imp_df_46 %>% 
  # 筛选Cfa气候区
  filter(Climate_Class == "Cfa") %>%
  # 转换森林类型为因子
  mutate(Forest_Type = factor(Forest_Type, 
                              levels = c("2", "1", "3"),
                              labels = c("PF", "NRF", "P"))) %>%
  # 获取NRF的重要性值并排序
  group_by(Variable) %>%
  mutate(NRF_Importance = IncMSE[Forest_Type == "NRF"]) %>%
  ungroup() %>%
  # 按NRF重要性降序排列
  arrange(desc(NRF_Importance)) %>%
  # 将Variable转换为因子（关键修改：使用rev()使最重要的在顶部）
  mutate(Variable = factor(Variable, levels = rev(unique(Variable))))


# 查看最大最小值   0.53  92.5
Cfa_phe46_data %>% 
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

# 显示phe4_6的重要性
Cfa_phe46_data %>%
  filter(Variable == "Duration") %>%
  select(Variable, Forest_Type, IncMSE, NRF_Importance) %>%  # 选择需要显示的列
  print(n = Inf)  # 确保显示所有行（不截断）
# Variable Forest_Type IncMSE NRF_Importance
# 1 Duration NRF           78.9           78.9
# 2 Duration PF            92.5           78.9
# 3 Duration P             85.9           78.9

# 移除phe4_6的Duration重要性后，查看最大最小值作图  0.53 16.9
Cfa_phe46_data <- Cfa_phe46_data[Cfa_phe46_data$Variable != "Duration", ]
Cfa_phe46_data %>%
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

## 定义颜色和形状
forest_colors <- c("PF" = "#2E8B57",  # 原生林-深绿
                   "NRF" = "#9ACD32", # 再生林-浅绿
                   "P" = "#FFA500")   # 人工林-橙色


# 创建绘图
p_phe46_Cfa <- ggplot(Cfa_phe46_data, aes(x = Variable, y = IncMSE, color = Forest_Type)) +
  # geom_line(aes(group = Variable), color = "grey50",linewidth = 0.8,alpha = 0.7 ) +
  # geom_point(shape = 16, size = 5, alpha = 0.9) + #数据点（统一圆形，大小4，透明度0.9）
  geom_point(shape = 1, size = 2, stroke = 2, alpha = 0.9) +
  scale_color_manual(values = forest_colors) +
  # scale_shape_manual(values = c("PF" = 16,"NRF" = 17, "P" = 15)) +   # 原生林：实心圆；# 再生林：实心三角；# 人工林：实心方块
  scale_y_continuous(expand = c(0, 0), limits = c(0, 18), breaks = c(0, 8, 16)) +
  labs(title = "Cfa",
       x = "",
       y = "%IncMSE") +
  theme_classic() + # 使用classic主题作为基础
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "none",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    # panel.grid = element_blank()         # 网格线（全部移除）
    panel.grid.major.y = element_line(color = "grey50",linetype = "dashed",linewidth = 0.5), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe46_Cfa)


####################################### 02 对Cfb区域#####  #########################################
######02-1 Cfb_phe13   #########

# 数据加载和预处理
Cfb_phe13_data <- imp_df_13 %>% 
  # 筛选Cfb气候区
  filter(Climate_Class == "Cfb") %>%
  # 转换森林类型为因子
  mutate(Forest_Type = factor(Forest_Type, 
                              levels = c("2", "1", "3"),
                              labels = c("PF", "NRF", "P"))) %>%
  # 获取NRF的重要性值并排序
  group_by(Variable) %>%
  mutate(NRF_Importance = IncMSE[Forest_Type == "NRF"]) %>%
  ungroup() %>%
  # 按NRF重要性降序排列
  arrange(desc(NRF_Importance)) %>%
  # 将Variable转换为因子（关键修改：使用rev()使最重要的在顶部）
  mutate(Variable = factor(Variable, levels = rev(unique(Variable))))


# 查看最大最小值   0.03  7.08
Cfb_phe13_data %>% 
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

Cfb_phe13_data %>%
  filter(Variable == "Duration") %>%
  select(Variable, Forest_Type, IncMSE, NRF_Importance) %>%  # 选择需要显示的列
  print(n = Inf)  # 确保显示所有行（不截断）
# Variable Forest_Type IncMSE NRF_Importance
# 1 Duration NRF           5.61           5.61
# 2 Duration PF            7.08           5.61
# 3 Duration P             5.67           5.61

# 移除phe13的Duration后，查看最大最小值作图  0.03 5.51
Cfb_phe13_data <- Cfb_phe13_data[Cfb_phe13_data$Variable != "Duration", ]
Cfb_phe13_data %>%
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))
## 定义颜色和形状
forest_colors <- c("PF" = "#2E8B57",  # 原生林-深绿
                   "NRF" = "#9ACD32", # 再生林-浅绿
                   "P" = "#FFA500")   # 人工林-橙色


# 创建绘图
p_phe13_Cfb <- ggplot(Cfb_phe13_data, aes(x = Variable, y = IncMSE, color = Forest_Type)) +
  geom_point(shape = 1, size = 2, stroke = 2, alpha = 0.9) +
  scale_color_manual(values = forest_colors) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 24), breaks = c(0, 10, 20)) +
  labs(title = "Cfb",
       x = "",
       y = "%IncMSE") +
  theme_classic() + # 使用classic主题作为基础
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "none",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = "grey50",linetype = "dashed",linewidth = 0.5), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe13_Cfb)

######02-2 Cfb_phe34   #########

# 数据加载和预处理
Cfb_phe34_data <- imp_df_34 %>% 
  # 筛选Cfb气候区
  filter(Climate_Class == "Cfb") %>%
  # 转换森林类型为因子
  mutate(Forest_Type = factor(Forest_Type, 
                              levels = c("2", "1", "3"),
                              labels = c("PF", "NRF", "P"))) %>%
  # 获取PF的重要性值并排序
  group_by(Variable) %>%
  mutate(NRF_Importance = IncMSE[Forest_Type == "NRF"]) %>%
  ungroup() %>%
  # 按NRF重要性降序排列
  arrange(desc(NRF_Importance)) %>%
  # 将Variable转换为因子（关键修改：使用rev()使最重要的在顶部）
  mutate(Variable = factor(Variable, levels = rev(unique(Variable))))

# 查看最大最小值   0.0  90.5
Cfb_phe34_data %>% 
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))
Cfb_phe34_data$IncMSE[Cfb_phe34_data$IncMSE < 0] <- 0

Cfb_phe34_data %>%
  filter(Variable == "Duration") %>%
  select(Variable, Forest_Type, IncMSE, NRF_Importance) %>%  # 选择需要显示的列
  print(n = Inf)  # 确保显示所有行（不截断）
# Variable Forest_Type IncMSE NRF_Importance
# 1 Duration NRF           34.2           34.2
# 2 Duration PF            90.5           34.2
# 3 Duration P             40.2           34.2

# 移除phe13的Duration后，查看最大最小值作图  0.0 9.2
Cfb_phe34_data <- Cfb_phe34_data[Cfb_phe34_data$Variable != "Duration", ]
Cfb_phe34_data %>%
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

## 定义颜色和形状
forest_colors <- c("PF" = "#2E8B57",  # 原生林-深绿
                   "NRF" = "#9ACD32", # 再生林-浅绿
                   "P" = "#FFA500")   # 人工林-橙色


# 创建绘图
p_phe34_Cfb <- ggplot(Cfb_phe34_data, aes(x = Variable, y = IncMSE, color = Forest_Type)) +
  geom_point(shape = 1, size = 2, stroke = 2, alpha = 0.9) +
  scale_color_manual(values = forest_colors) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 24), breaks = c(0, 10, 20)) +
  labs(title = "Cfb",
       x = "",
       y = "%IncMSE") +
  theme_classic() + # 使用classic主题作为基础
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "none",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = "grey50",linetype = "dashed",linewidth = 0.5), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe34_Cfb)

######02-3 Cfb_phe46   #########

# 数据加载和预处理
Cfb_phe46_data <- imp_df_46 %>% 
  # 筛选Cfb气候区
  filter(Climate_Class == "Cfb") %>%
  # 转换森林类型为因子
  mutate(Forest_Type = factor(Forest_Type, 
                              levels = c("2", "1", "3"),
                              labels = c("PF", "NRF", "P"))) %>%
  # 获取PF的重要性值并排序
  group_by(Variable) %>%
  mutate(NRF_Importance = IncMSE[Forest_Type == "NRF"]) %>%
  ungroup() %>%
  # 按NRF重要性降序排列
  arrange(desc(NRF_Importance)) %>%
  # 将Variable转换为因子（关键修改：使用rev()使最重要的在顶部）
  mutate(Variable = factor(Variable, levels = rev(unique(Variable))))

# 查看最大最小值   0.64  323
Cfb_phe46_data %>% 
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

# 显示phe4_6的重要性
Cfb_phe46_data %>%
  filter(Variable == "Duration") %>%
  select(Variable, Forest_Type, IncMSE, NRF_Importance) %>%  # 选择需要显示的列
  print(n = Inf)  # 确保显示所有行（不截断）
# Variable Forest_Type IncMSE NRF_Importance
# 1 Duration NRF           145             145
# 2 Duration PF            323.            145
# 3 Duration P             143.            145

# 移除phe4_6的Duration后，查看最大最小值作图    0.64 22.5
Cfb_phe46_data <- Cfb_phe46_data[Cfb_phe46_data$Variable != "Duration", ]
Cfb_phe46_data %>%
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

## 定义颜色和形状
forest_colors <- c("PF" = "#2E8B57",  # 原生林-深绿
                   "NRF" = "#9ACD32", # 再生林-浅绿
                   "P" = "#FFA500")   # 人工林-橙色


# 创建绘图
p_phe46_Cfb <- ggplot(Cfb_phe46_data, aes(x = Variable, y = IncMSE, color = Forest_Type)) +
  geom_point(shape = 1, size = 2, stroke = 2, alpha = 0.9) +
  scale_color_manual(values = forest_colors) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 24), breaks = c(0, 10, 20)) +
  labs(title = "Cfb",
       x = "",
       y = "%IncMSE") +
  theme_classic() + # 使用classic主题作为基础
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "none",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = "grey50",linetype = "dashed",linewidth = 0.5), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe46_Cfb)


####################################### 03 对Dfb区域#####  #########################################
######03-1 Dfb_phe13   #########

# 数据加载和预处理
Dfb_phe13_data <- imp_df_13 %>% 
  # 筛选Dfb气候区
  filter(Climate_Class == "Dfb") %>%
  # 转换森林类型为因子
  mutate(Forest_Type = factor(Forest_Type, 
                              levels = c("2", "1", "3"),
                              labels = c("PF", "NRF", "P"))) %>%
  # 获取NRF的重要性值并排序
  group_by(Variable) %>%
  mutate(NRF_Importance = IncMSE[Forest_Type == "NRF"]) %>%
  ungroup() %>%
  # 按NRF重要性降序排列
  arrange(desc(NRF_Importance)) %>%
  # 将Variable转换为因子（关键修改：使用rev()使最重要的在顶部）
  mutate(Variable = factor(Variable, levels = rev(unique(Variable))))


# 查看最大最小值   0.12  32.1
Dfb_phe13_data %>% 
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

Dfb_phe13_data %>%
  filter(Variable == "Duration") %>%
  select(Variable, Forest_Type, IncMSE, NRF_Importance) %>%  # 选择需要显示的列
  print(n = Inf)  # 确保显示所有行（不截断）
# Variable Forest_Type IncMSE NRF_Importance
# 1 Duration NRF           22.0           22.0
# 2 Duration PF            11.5           22.0
# 3 Duration P             32.1           22.0

# 移除phe13的Duration后，查看最大最小值作图  0.12 14.0
Dfb_phe13_data <- Dfb_phe13_data[Dfb_phe13_data$Variable != "Duration", ]
Dfb_phe13_data %>%
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))
## 定义颜色和形状
forest_colors <- c("PF" = "#2E8B57",  # 原生林-深绿
                   "NRF" = "#9ACD32", # 再生林-浅绿
                   "P" = "#FFA500")   # 人工林-橙色


# 创建绘图
p_phe13_Dfb <- ggplot(Dfb_phe13_data, aes(x = Variable, y = IncMSE, color = Forest_Type)) +
  geom_point(shape = 1, size = 2, stroke = 2, alpha = 0.9) +
  scale_color_manual(values = forest_colors) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 18), breaks = c(0, 8, 16)) +
  labs(title = "Dfb",
       x = "",
       y = "%IncMSE") +
  theme_classic() + # 使用classic主题作为基础
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "none",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = "grey50",linetype = "dashed",linewidth = 0.5), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe13_Dfb)

######03-2 Dfb_phe34   #########

# 数据加载和预处理
Dfb_phe34_data <- imp_df_34 %>% 
  # 筛选Dfb气候区
  filter(Climate_Class == "Dfb") %>%
  # 转换森林类型为因子
  mutate(Forest_Type = factor(Forest_Type, 
                              levels = c("2", "1", "3"),
                              labels = c("PF", "NRF", "P"))) %>%
  # 获取PF的重要性值并排序
  group_by(Variable) %>%
  mutate(NRF_Importance = IncMSE[Forest_Type == "NRF"]) %>%
  ungroup() %>%
  # 按NRF重要性降序排列
  arrange(desc(NRF_Importance)) %>%
  # 将Variable转换为因子（关键修改：使用rev()使最重要的在顶部）
  mutate(Variable = factor(Variable, levels = rev(unique(Variable))))

# 查看最大最小值   0.02  64.9
Dfb_phe34_data %>% 
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

Dfb_phe34_data %>%
  filter(Variable == "Duration") %>%
  select(Variable, Forest_Type, IncMSE, NRF_Importance) %>%  # 选择需要显示的列
  print(n = Inf)  # 确保显示所有行（不截断）
# Variable Forest_Type IncMSE NRF_Importance
# 1 Duration NRF           59.8           59.8
# 2 Duration PF            45.0           59.8
# 3 Duration P             49.9           59.8

# 移除phe34的Duration后，查看最大最小值作图  0 7.99
Dfb_phe34_data <- Dfb_phe34_data[Dfb_phe34_data$Variable != "Duration", ]
Dfb_phe34_data %>%
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))
## 定义颜色和形状
forest_colors <- c("PF" = "#2E8B57",  # 原生林-深绿
                   "NRF" = "#9ACD32", # 再生林-浅绿
                   "P" = "#FFA500")   # 人工林-橙色


# 创建绘图
p_phe34_Dfb <- ggplot(Dfb_phe34_data, aes(x = Variable, y = IncMSE, color = Forest_Type)) +
  geom_point(shape = 1, size = 2, stroke = 2, alpha = 0.9) +
  scale_color_manual(values = forest_colors) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 18), breaks = c(0, 8, 16)) +
  labs(title = "Dfb",
       x = "",
       y = "%IncMSE") +
  theme_classic() + # 使用classic主题作为基础
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "none",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = "grey50",linetype = "dashed",linewidth = 0.5), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe34_Dfb)

######03-3 Dfb_phe46   #########

# 数据加载和预处理
Dfb_phe46_data <- imp_df_46 %>% 
  # 筛选Dfb气候区
  filter(Climate_Class == "Dfb") %>%
  # 转换森林类型为因子
  mutate(Forest_Type = factor(Forest_Type, 
                              levels = c("2", "1", "3"),
                              labels = c("PF", "NRF", "P"))) %>%
  # 获取PF的重要性值并排序
  group_by(Variable) %>%
  mutate(NRF_Importance = IncMSE[Forest_Type == "NRF"]) %>%
  ungroup() %>%
  # 按NRF重要性降序排列
  arrange(desc(NRF_Importance)) %>%
  # 将Variable转换为因子（关键修改：使用rev()使最重要的在顶部）
  mutate(Variable = factor(Variable, levels = rev(unique(Variable))))


# 查看最大最小值   0.0  121
Dfb_phe46_data %>% 
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

# 显示phe4_6的重要性
Dfb_phe46_data %>%
  filter(Variable == "Duration") %>%
  select(Variable, Forest_Type, IncMSE, NRF_Importance) %>%  # 选择需要显示的列
  print(n = Inf)  # 确保显示所有行（不截断）
# Variable Forest_Type IncMSE NRF_Importance
# 1 Duration NRF          121.            121.
# 2 Duration PF            68.4           121.
# 3 Duration P            119.            121.

# 移除phe4_6的Duration后，查看最大最小值作图  0 16.2
Dfb_phe46_data <- Dfb_phe46_data[Dfb_phe46_data$Variable != "Duration", ]
Dfb_phe46_data %>%
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

## 定义颜色和形状
forest_colors <- c("PF" = "#2E8B57",  # 原生林-深绿
                   "NRF" = "#9ACD32", # 再生林-浅绿
                   "P" = "#FFA500")   # 人工林-橙色


# 创建绘图
p_phe46_Dfb <- ggplot(Dfb_phe46_data, aes(x = Variable, y = IncMSE, color = Forest_Type)) +
  geom_point(shape = 1, size = 2, stroke = 2, alpha = 0.9) +
  scale_color_manual(values = forest_colors) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 18), breaks = c(0, 8, 16)) +
  labs(title = "Dfb",
       x = "",
       y = "%IncMSE") +
  theme_classic() + # 使用classic主题作为基础
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "none",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = "grey50",linetype = "dashed",linewidth = 0.5), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe46_Dfb)


####################################### 04 对Dfc区域#####  #########################################
######04-1 Dfc_phe13   #########

# 数据加载和预处理
Dfc_phe13_data <- imp_df_13 %>% 
  # 筛选Dfc气候区
  filter(Climate_Class == "Dfc") %>%
  # 转换森林类型为因子
  mutate(Forest_Type = factor(Forest_Type, 
                              levels = c("2", "1", "3"),
                              labels = c("PF", "NRF", "P"))) %>%
  # 获取NRF的重要性值并排序
  group_by(Variable) %>%
  mutate(NRF_Importance = IncMSE[Forest_Type == "NRF"]) %>%
  ungroup() %>%
  # 按NRF重要性降序排列
  arrange(desc(NRF_Importance)) %>%
  # 将Variable转换为因子（关键修改：使用rev()使最重要的在顶部）
  mutate(Variable = factor(Variable, levels = rev(unique(Variable))))


# 查看最大最小值   0.02  12.4
Dfc_phe13_data %>% 
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

Dfc_phe13_data %>%
  filter(Variable == "Duration") %>%
  select(Variable, Forest_Type, IncMSE, NRF_Importance) %>%  # 选择需要显示的列
  print(n = Inf)  # 确保显示所有行（不截断）
# Variable Forest_Type IncMSE NRF_Importance
# 1 Duration NRF           4.04           4.04
# 2 Duration PF            12.4           4.04
# 3 Duration P             5.51           4.04

# 移除phe13的Duration后，查看最大最小值作图  0 10.6
Dfc_phe13_data <- Dfc_phe13_data[Dfc_phe13_data$Variable != "Duration", ]
Dfc_phe13_data %>%
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))
## 定义颜色和形状
forest_colors <- c("PF" = "#2E8B57",  # 原生林-深绿
                   "NRF" = "#9ACD32", # 再生林-浅绿
                   "P" = "#FFA500")   # 人工林-橙色


# 创建绘图
p_phe13_Dfc <- ggplot(Dfc_phe13_data, aes(x = Variable, y = IncMSE, color = Forest_Type)) +
  geom_point(shape = 1, size = 2, stroke = 2, alpha = 0.9) +
  scale_color_manual(values = forest_colors) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 22), breaks = c(0, 10, 20)) +
  labs(title = "Dfc",
       x = "",
       y = "%IncMSE") +
  theme_classic() + # 使用classic主题作为基础
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "none",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = "grey50",linetype = "dashed",linewidth = 0.5), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe13_Dfc)


######04-2 Dfc_phe34   #########

# 数据加载和预处理
Dfc_phe34_data <- imp_df_34 %>% 
  # 筛选Dfc气候区
  filter(Climate_Class == "Dfc") %>%
  # 转换森林类型为因子
  mutate(Forest_Type = factor(Forest_Type, 
                              levels = c("2", "1", "3"),
                              labels = c("PF", "NRF", "P"))) %>%
  # 获取PF的重要性值并排序
  group_by(Variable) %>%
  mutate(NRF_Importance = IncMSE[Forest_Type == "NRF"]) %>%
  ungroup() %>%
  # 按NRF重要性降序排列
  arrange(desc(NRF_Importance)) %>%
  # 将Variable转换为因子（关键修改：使用rev()使最重要的在顶部）
  mutate(Variable = factor(Variable, levels = rev(unique(Variable))))

# 查看最大最小值   0.0  23.2
Dfc_phe34_data %>% 
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

Dfc_phe34_data %>%
  filter(Variable == "Duration") %>%
  select(Variable, Forest_Type, IncMSE, NRF_Importance) %>%  # 选择需要显示的列
  print(n = Inf)  # 确保显示所有行（不截断）
# Variable Forest_Type IncMSE NRF_Importance
# 1 Duration NRF           23.2           23.2
# 2 Duration PF            20.8           23.2
# 3 Duration P             18.3           23.2

# 移除phe13的Duration后，查看最大最小值作图  0 7.37
Dfc_phe34_data <- Dfc_phe34_data[Dfc_phe34_data$Variable != "Duration", ]
Dfc_phe34_data %>%
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))


## 定义颜色和形状
forest_colors <- c("PF" = "#2E8B57",  # 原生林-深绿
                   "NRF" = "#9ACD32", # 再生林-浅绿
                   "P" = "#FFA500")   # 人工林-橙色


# 创建绘图
p_phe34_Dfc <- ggplot(Dfc_phe34_data, aes(x = Variable, y = IncMSE, color = Forest_Type)) +
  geom_point(shape = 1, size = 2, stroke = 2, alpha = 0.9) +
  scale_color_manual(values = forest_colors) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 22), breaks = c(0, 10, 20)) +
  labs(title = "Dfc",
       x = "",
       y = "%IncMSE") +
  theme_classic() + # 使用classic主题作为基础
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "none",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = "grey50",linetype = "dashed",linewidth = 0.5), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe34_Dfc)

######04-3 Dfc_phe46   #########

# 数据加载和预处理
Dfc_phe46_data <- imp_df_46 %>% 
  # 筛选Dfc气候区
  filter(Climate_Class == "Dfc") %>%
  # 转换森林类型为因子
  mutate(Forest_Type = factor(Forest_Type, 
                              levels = c("2", "1", "3"),
                              labels = c("PF", "NRF", "P"))) %>%
  # 获取PF的重要性值并排序
  group_by(Variable) %>%
  mutate(NRF_Importance = IncMSE[Forest_Type == "NRF"]) %>%
  ungroup() %>%
  # 按NRF重要性降序排列
  arrange(desc(NRF_Importance)) %>%
  # 将Variable转换为因子（关键修改：使用rev()使最重要的在顶部）
  mutate(Variable = factor(Variable, levels = rev(unique(Variable))))

# 查看最大最小值   0.0  50.6
Dfc_phe13_data %>% 
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))
# 负值替换为0后取最小; 负值项影响低参考https://stackoverflow.com/questions/27918320/what-does-negative-incmse-in-randomforest-package-mean
Dfc_phe46_data$IncMSE[Dfc_phe46_data$IncMSE < 0] <- 0
# 显示phe4_6的重要性
Dfc_phe46_data %>%
  filter(Variable == "Duration") %>%
  select(Variable, Forest_Type, IncMSE, NRF_Importance) %>%  # 选择需要显示的列
  print(n = Inf)  # 确保显示所有行（不截断）
# Variable Forest_Type IncMSE NRF_Importance
# 1 Duration NRF           91.1           91.1
# 2 Duration PF            79.4           91.1
# 3 Duration P             77.2           91.1

# 移除phe4_6的Duration后，查看最大最小值作图  0 19.8
Dfc_phe46_data <- Dfc_phe46_data[Dfc_phe46_data$Variable != "Duration", ]
Dfc_phe46_data %>%
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))
## 定义颜色和形状
forest_colors <- c("PF" = "#2E8B57",  # 原生林-深绿
                   "NRF" = "#9ACD32", # 再生林-浅绿
                   "P" = "#FFA500")   # 人工林-橙色


# 创建绘图
p_phe46_Dfc <- ggplot(Dfc_phe46_data, aes(x = Variable, y = IncMSE, color = Forest_Type)) +
  geom_point(shape = 1, size = 2, stroke = 2, alpha = 0.9) +
  scale_color_manual(values = forest_colors) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 22), breaks = c(0, 10, 20)) +
  labs(title = "Dfc",
       x = "",
       y = "%IncMSE") +
  theme_classic() + # 使用classic主题作为基础
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "none",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = "grey50",linetype = "dashed",linewidth = 0.5), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe46_Dfc)


####################################### 05 对Dwb区域#####  #########################################
######05-1 Dwb_phe13   #########

# 数据加载和预处理
Dwb_phe13_data <- imp_df_13 %>% 
  # 筛选Dwb气候区
  filter(Climate_Class == "Dwb") %>%
  # 转换森林类型为因子
  mutate(Forest_Type = factor(Forest_Type, 
                              levels = c("2", "1", "3"),
                              labels = c("PF", "NRF", "P"))) %>%
  # 获取NRF的重要性值并排序
  group_by(Variable) %>%
  mutate(NRF_Importance = IncMSE[Forest_Type == "NRF"]) %>%
  ungroup() %>%
  # 按NRF重要性降序排列
  arrange(desc(NRF_Importance)) %>%
  # 将Variable转换为因子（关键修改：使用rev()使最重要的在顶部）
  mutate(Variable = factor(Variable, levels = rev(unique(Variable))))


# 查看最大最小值   0.09  4.94
Dwb_phe13_data %>% 
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

Dwb_phe13_data %>%
  filter(Variable == "Duration") %>%
  select(Variable, Forest_Type, IncMSE, NRF_Importance) %>%  # 选择需要显示的列
  print(n = Inf)  # 确保显示所有行（不截断）
# Variable Forest_Type IncMSE NRF_Importance
# 1 Duration NRF           3.76           3.76
# 2 Duration PF            0.73           3.76
# 3 Duration P             4.94           3.76

# 移除phe13的Duration后，查看最大最小值作图  0 2.47
Dwb_phe13_data <- Dwb_phe13_data[Dwb_phe13_data$Variable != "Duration", ]
Dwb_phe13_data %>%
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

## 定义颜色和形状
forest_colors <- c("PF" = "#2E8B57",  # 原生林-深绿
                   "NRF" = "#9ACD32", # 再生林-浅绿
                   "P" = "#FFA500")   # 人工林-橙色


# 创建绘图
p_phe13_Dwb <- ggplot(Dwb_phe13_data, aes(x = Variable, y = IncMSE, color = Forest_Type)) +
  geom_point(shape = 1, size = 2, stroke = 2, alpha = 0.9) +
  scale_color_manual(values = forest_colors) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10), breaks = c(0, 4, 8)) +
  labs(title = "Dwb",
       x = "",
       y = "%IncMSE") +
  theme_classic() + # 使用classic主题作为基础
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "none",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = "grey50",linetype = "dashed",linewidth = 0.5), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe13_Dwb)

######05-2 Dwb_phe34   #########

# 数据加载和预处理
Dwb_phe34_data <- imp_df_34 %>% 
  # 筛选Dwb气候区
  filter(Climate_Class == "Dwb") %>%
  # 转换森林类型为因子
  mutate(Forest_Type = factor(Forest_Type, 
                              levels = c("2", "1", "3"),
                              labels = c("PF", "NRF", "P"))) %>%
  # 获取PF的重要性值并排序
  group_by(Variable) %>%
  mutate(NRF_Importance = IncMSE[Forest_Type == "NRF"]) %>%
  ungroup() %>%
  # 按NRF重要性降序排列
  arrange(desc(NRF_Importance)) %>%
  # 将Variable转换为因子（关键修改：使用rev()使最重要的在顶部）
  mutate(Variable = factor(Variable, levels = rev(unique(Variable))))

# 查看最大最小值   0.0  72.7
Dwb_phe34_data %>% 
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

Dwb_phe34_data %>%
  filter(Variable == "Duration") %>%
  select(Variable, Forest_Type, IncMSE, NRF_Importance) %>%  # 选择需要显示的列
  print(n = Inf)  # 确保显示所有行（不截断）
# Variable Forest_Type IncMSE NRF_Importance
# 1 Duration NRF           72.7           72.7
# 2 Duration PF            34.9           72.7
# 3 Duration P             65.4           72.7

# 移除phe34的Duration后，查看最大最小值作图  0 8.67
Dwb_phe34_data <- Dwb_phe34_data[Dwb_phe34_data$Variable != "Duration", ]
Dwb_phe34_data %>%
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

## 定义颜色和形状
forest_colors <- c("PF" = "#2E8B57",  # 原生林-深绿
                   "NRF" = "#9ACD32", # 再生林-浅绿
                   "P" = "#FFA500")   # 人工林-橙色


# 创建绘图
p_phe34_Dwb <- ggplot(Dwb_phe34_data, aes(x = Variable, y = IncMSE, color = Forest_Type)) +
  geom_point(shape = 1, size = 2, stroke = 2, alpha = 0.9) +
  scale_color_manual(values = forest_colors) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10), breaks = c(0, 4, 8)) +
  labs(title = "Dwb",
       x = "",
       y = "%IncMSE") +
  theme_classic() + # 使用classic主题作为基础
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "none",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = "grey50",linetype = "dashed",linewidth = 0.5), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe34_Dwb)

######05-3 Dwb_phe46   #########

# 数据加载和预处理
Dwb_phe46_data <- imp_df_46 %>% 
  # 筛选Dwb气候区
  filter(Climate_Class == "Dwb") %>%
  # 转换森林类型为因子
  mutate(Forest_Type = factor(Forest_Type, 
                              levels = c("2", "1", "3"),
                              labels = c("PF", "NRF", "P"))) %>%
  # 获取PF的重要性值并排序
  group_by(Variable) %>%
  mutate(NRF_Importance = IncMSE[Forest_Type == "NRF"]) %>%
  ungroup() %>%
  # 按NRF重要性降序排列
  arrange(desc(NRF_Importance)) %>%
  # 将Variable转换为因子（关键修改：使用rev()使最重要的在顶部）
  mutate(Variable = factor(Variable, levels = rev(unique(Variable))))


# 查看最大最小值   0.0  104
Dwb_phe46_data %>% 
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

# 显示phe4_6的重要性
Dwb_phe46_data %>%
  filter(Variable == "Duration") %>%
  select(Variable, Forest_Type, IncMSE, NRF_Importance) %>%  # 选择需要显示的列
  print(n = Inf)  # 确保显示所有行（不截断）
# Variable Forest_Type IncMSE NRF_Importance
# 1 Duration NRF           54.0           54.0
# 2 Duration PF            32.8           54.0
# 3 Duration P             104.           54.0

# 移除phe4_6的Duration后，查看最大最小值作图  0 5.19
Dwb_phe46_data <- Dwb_phe46_data[Dwb_phe46_data$Variable != "Duration", ]
Dwb_phe46_data %>%
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

## 定义颜色和形状
forest_colors <- c("PF" = "#2E8B57",  # 原生林-深绿
                   "NRF" = "#9ACD32", # 再生林-浅绿
                   "P" = "#FFA500")   # 人工林-橙色


# 创建绘图
p_phe46_Dwb <- ggplot(Dwb_phe46_data, aes(x = Variable, y = IncMSE, color = Forest_Type)) +
  geom_point(shape = 1, size = 2, stroke = 2, alpha = 0.9) +
  scale_color_manual(values = forest_colors) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10), breaks = c(0, 4, 8)) +
  labs(title = "Dwb",
       x = "",
       y = "%IncMSE") +
  theme_classic() + # 使用classic主题作为基础
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "none",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = "grey50",linetype = "dashed",linewidth = 0.5), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe46_Dwb)


####################################### 06 对Dwc区域#####  #########################################
######06-1 Dwc_phe13   #########

# 数据加载和预处理
Dwc_phe13_data <- imp_df_13 %>% 
  # 筛选Dwc气候区
  filter(Climate_Class == "Dwc") %>%
  # 转换森林类型为因子
  mutate(Forest_Type = factor(Forest_Type, 
                              levels = c("2", "1", "3"),
                              labels = c("PF", "NRF", "P"))) %>%
  # 获取NRF的重要性值并排序
  group_by(Variable) %>%
  mutate(NRF_Importance = IncMSE[Forest_Type == "NRF"]) %>%
  ungroup() %>%
  # 按NRF重要性降序排列
  arrange(desc(NRF_Importance)) %>%
  # 将Variable转换为因子（关键修改：使用rev()使最重要的在顶部）
  mutate(Variable = factor(Variable, levels = rev(unique(Variable))))

# 查看最大最小值   0.01  9.69
Dwc_phe13_data %>% 
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

Dwc_phe13_data %>%
  filter(Variable == "Duration") %>%
  select(Variable, Forest_Type, IncMSE, NRF_Importance) %>%  # 选择需要显示的列
  print(n = Inf)  # 确保显示所有行（不截断）
# Variable Forest_Type IncMSE NRF_Importance
# 1 Duration NRF           4.19           4.19
# 2 Duration PF            6.82           4.19
# 3 Duration P             0.33           4.19

# 移除phe13的Duration后，查看最大最小值作图  0.01  9.69
Dwc_phe13_data <- Dwc_phe13_data[Dwc_phe13_data$Variable != "Duration", ]
Dwc_phe13_data %>%
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

## 定义颜色和形状
forest_colors <- c("PF" = "#2E8B57",  # 原生林-深绿
                   "NRF" = "#9ACD32", # 再生林-浅绿
                   "P" = "#FFA500")   # 人工林-橙色


# 创建绘图
p_phe13_Dwc <- ggplot(Dwc_phe13_data, aes(x = Variable, y = IncMSE, color = Forest_Type)) +
  geom_point(shape = 1, size = 2, stroke = 2, alpha = 0.9) +
  scale_color_manual(values = forest_colors) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 31), breaks = c(0, 14, 28)) +
  labs(title = "Dwc",
       x = "",
       y = "%IncMSE") +
  theme_classic() + # 使用classic主题作为基础
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "none",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = "grey50",linetype = "dashed",linewidth = 0.5), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe13_Dwc)

######06-2 Dwc_phe34   #########

# 数据加载和预处理
Dwc_phe34_data <- imp_df_34 %>% 
  # 筛选Dwc气候区
  filter(Climate_Class == "Dwc") %>%
  # 转换森林类型为因子
  mutate(Forest_Type = factor(Forest_Type, 
                              levels = c("2", "1", "3"),
                              labels = c("PF", "NRF", "P"))) %>%
  # 获取PF的重要性值并排序
  group_by(Variable) %>%
  mutate(NRF_Importance = IncMSE[Forest_Type == "NRF"]) %>%
  ungroup() %>%
  # 按NRF重要性降序排列
  arrange(desc(NRF_Importance)) %>%
  # 将Variable转换为因子（关键修改：使用rev()使最重要的在顶部）
  mutate(Variable = factor(Variable, levels = rev(unique(Variable))))

# 查看最大最小值   0.0  11.2
Dwc_phe34_data %>% 
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

Dwc_phe34_data %>%
  filter(Variable == "Duration") %>%
  select(Variable, Forest_Type, IncMSE, NRF_Importance) %>%  # 选择需要显示的列
  print(n = Inf)  # 确保显示所有行（不截断）
# Variable Forest_Type IncMSE NRF_Importance
# 1 Duration NRF           8.18           8.18
# 2 Duration PF            11.2           8.18
# 3 Duration P             1.23           8.18

# 移除phe13的Duration后，查看最大最小值作图  0 6.02
Dwc_phe34_data <- Dwc_phe34_data[Dwc_phe34_data$Variable != "Duration", ]
Dwc_phe34_data %>%
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

## 定义颜色和形状
forest_colors <- c("PF" = "#2E8B57",  # 原生林-深绿
                   "NRF" = "#9ACD32", # 再生林-浅绿
                   "P" = "#FFA500")   # 人工林-橙色


# 创建绘图
p_phe34_Dwc <- ggplot(Dwc_phe34_data, aes(x = Variable, y = IncMSE, color = Forest_Type)) +
  geom_point(shape = 1, size = 2, stroke = 2, alpha = 0.9) +
  scale_color_manual(values = forest_colors) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 31), breaks = c(0, 14, 28)) +
  labs(title = "Dwc",
       x = "",
       y = "%IncMSE") +
  theme_classic() + # 使用classic主题作为基础
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "none",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = "grey50",linetype = "dashed",linewidth = 0.5), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe34_Dwc)


######06-3 Dwc_phe46   #########

# 数据加载和预处理
Dwc_phe46_data <- imp_df_46 %>% 
  # 筛选Dwc气候区
  filter(Climate_Class == "Dwc") %>%
  # 转换森林类型为因子
  mutate(Forest_Type = factor(Forest_Type, 
                              levels = c("2", "1", "3"),
                              labels = c("PF", "NRF", "P"))) %>%
  # 获取PF的重要性值并排序
  group_by(Variable) %>%
  mutate(NRF_Importance = IncMSE[Forest_Type == "NRF"]) %>%
  ungroup() %>%
  # 按NRF重要性降序排列
  arrange(desc(NRF_Importance)) %>%
  # 将Variable转换为因子（关键修改：使用rev()使最重要的在顶部）
  mutate(Variable = factor(Variable, levels = rev(unique(Variable))))


# 查看最大最小值   0.0  37.6
Dwc_phe46_data %>% 
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

# 显示phe4_6的重要性
Dwc_phe46_data %>%
  filter(Variable == "Duration") %>%
  select(Variable, Forest_Type, IncMSE, NRF_Importance) %>%  # 选择需要显示的列
  print(n = Inf)  # 确保显示所有行（不截断）
# Variable Forest_Type IncMSE NRF_Importance
# 1 Duration NRF          36.5            36.5
# 2 Duration PF           37.6            36.5
# 3 Duration P            2.46            36.5

# 移除phe4_6的Duration后，查看最大最小值作图  0 28.7
Dwc_phe46_data <- Dwc_phe46_data[Dwc_phe46_data$Variable != "Duration", ]
Dwc_phe46_data %>%
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

## 定义颜色和形状
forest_colors <- c("PF" = "#2E8B57",  # 原生林-深绿
                   "NRF" = "#9ACD32", # 再生林-浅绿
                   "P" = "#FFA500")   # 人工林-橙色


# 创建绘图
p_phe46_Dwc <- ggplot(Dwc_phe46_data, aes(x = Variable, y = IncMSE, color = Forest_Type)) +
  geom_point(shape = 1, size = 2, stroke = 2, alpha = 0.9) +
  scale_color_manual(values = forest_colors) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 31), breaks = c(0, 14, 28)) +
  labs(title = "Dwc",
       x = "",
       y = "%IncMSE") +
  theme_classic() + # 使用classic主题作为基础
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "none",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = "grey50",linetype = "dashed",linewidth = 0.5), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe46_Dwc)


####################################### 8 Combine Plots #####  #########################################


# 第一行的 6 张图
row1 <- list(p_phe13_Cfa, p_phe13_Cfb, p_phe13_Dfb, p_phe13_Dfc, p_phe13_Dwb, p_phe13_Dwc)

# 第二行的 6 张图
row2 <- list(p_phe34_Cfa, p_phe34_Cfb, p_phe34_Dfb, p_phe34_Dfc, p_phe34_Dwb, p_phe34_Dwc)

# 第三行的 6 张图
row3 <- list(p_phe46_Cfa, p_phe46_Cfb, p_phe46_Dfb, p_phe46_Dfc, p_phe46_Dwb, p_phe46_Dwc)

# 组合成 1 行 6 列的布局（第一行）
combined_plot1 <- ggarrange(plotlist = row1, ncol = 6, nrow = 1)

# 组合成 1 行 6 列的布局（第二行）
combined_plot2 <- ggarrange(plotlist = row2, ncol = 6, nrow = 1)

# 组合成 1 行 6 列的布局（第三行）
combined_plot3 <- ggarrange(plotlist = row3, ncol = 6, nrow = 1)

# 保存第一行图像
ggsave(
  filename = "./0.figure/03 Paper/Fig.9-a.Importance_EOS_plot.tiff",
  plot = combined_plot1, width = 14, height = 3.5, units = "in", dpi = 300)

# 保存第二行图像
ggsave(
  filename = "./0.figure/03 Paper/Fig.9-b.Importance_EOS_plot.tiff",
  plot = combined_plot2, width = 14, height = 3.5, units = "in", dpi = 300)


# 保存第三行图像
ggsave(
  filename = "./0.figure/03 Paper/Fig.9-c.Importance_EOS_plot.tiff",
  plot = combined_plot3, width = 14, height = 3.5, units = "in", dpi = 300)


