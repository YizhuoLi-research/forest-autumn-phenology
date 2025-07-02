
library(dplyr)
library(tidyr)
library(ggpubr)
library(ggplot2)


setwd("D:/Graduation_Thesis")

###########################  重要性排序作图 Fig.8  ######################

importance_data <- read.csv("./EA+NA_Results/merged_Indices_Importance_result/GDO_Indices_Importance_Results.csv")
# 查看数据前几行
head(importance_data)
# 查看数据结构
str(importance_data)

importance_data$IncMSE <- round(importance_data$IncMSE, digits = 2)

# 创建imp_df_1：筛选_phe1_3结尾的变量
imp_df_13 <- importance_data %>%
  filter(grepl("_phe1_3$", Variable)) %>%  # 筛选以_phe1_3结尾的变量
  mutate(Variable = gsub("_phe1_3$", "", Variable)) %>%  # 去掉后缀
  mutate(Variable = ifelse(Variable == "RainDay", "Rainday", Variable)) %>% #重命名RainDay
  mutate(Variable = ifelse(Variable == "merged_Duration", "Duration", Variable))  # 重命名RainDay

# 创建imp_df_2：筛选_phe3_4结尾的变量
imp_df_34 <- importance_data %>%
  filter(grepl("_phe3_4$", Variable)) %>%  # 筛选以_phe3_4结尾的变量
  mutate(Variable = gsub("_phe3_4$", "", Variable)) %>%  # 去掉后缀
  mutate(Variable = ifelse(Variable == "RainDay", "Rainday", Variable)) %>% #重命名RainDay
  mutate(Variable = ifelse(Variable == "merged_Duration", "Duration", Variable))  # 重命名RainDay

# 检查结果
# head(imp_df_13);# str(imp_df_13)
# head(imp_df_34);# str(imp_df_34)


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


# 查看最大最小值   0.2  19.4
Cfa_phe13_data %>% 
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

Cfa_phe13_data %>%
  filter(Variable == "Duration") %>%
  select(Variable, Forest_Type, IncMSE, NRF_Importance) %>%  # 选择需要显示的列
  print(n = Inf)  # 确保显示所有行（不截断）
# Variable Forest_Type IncMSE NRF_Importance
# 1 Duration NRF           9.94           9.94
# 2 Duration PF            1.7            9.94
# 3 Duration P            19.4            9.94

# 移除phe13的Duration后，查看最大最小值作图   0.2 16
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
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50), breaks = c(0, 20, 40)) +
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


# 查看最大最小值   0.0  56.9
Cfa_phe34_data %>% 
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

# 显示phe3_4的重要性
Cfa_phe34_data %>%
  filter(Variable == "Duration") %>%
  select(Variable, Forest_Type, IncMSE, NRF_Importance) %>%  # 选择需要显示的列
  print(n = Inf)  # 确保显示所有行（不截断）
# Variable Forest_Type IncMSE NRF_Importance
# 1 Duration NRF           56.9           56.9
# 2 Duration PF            26.5           56.9
# 3 Duration P             25.7           56.9

# 移除phe3_4的Duration重要性后，查看最大最小值作图
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
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50), breaks = c(0, 20, 40)) +
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


# 查看最大最小值   0.09  24.3
Cfb_phe13_data %>% 
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

Cfb_phe13_data %>%
  filter(Variable == "Duration") %>%
  select(Variable, Forest_Type, IncMSE, NRF_Importance) %>%  # 选择需要显示的列
  print(n = Inf)  # 确保显示所有行（不截断）
# Variable Forest_Type IncMSE NRF_Importance
# 1 Duration NRF          18.6            18.6
# 2 Duration PF            7.16           18.6
# 3 Duration P            24.3            18.6

# 移除phe13的Duration后，查看最大最小值作图  0 22.6
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
  scale_y_continuous(expand = c(0, 0), limits = c(0, 47), breaks = c(0, 20, 40)) +
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

# 查看最大最小值   0.0  56.9
Cfb_phe34_data %>% 
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

# 显示phe3_4的重要性
Cfb_phe34_data %>%
  filter(Variable == "Duration") %>%
  select(Variable, Forest_Type, IncMSE, NRF_Importance) %>%  # 选择需要显示的列
  print(n = Inf)  # 确保显示所有行（不截断）
# Variable Forest_Type IncMSE NRF_Importance
# 1 Duration NRF           46.1           46.1
# 2 Duration PF            13.0           46.1
# 3 Duration P             53.1           46.1

# 移除phe3_4的Duration后，查看最大最小值作图    0 43.5
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
  scale_y_continuous(expand = c(0, 0), limits = c(0, 47), breaks = c(0, 20, 40)) +
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


# 查看最大最小值   0.36  42.3
Dfb_phe13_data %>% 
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

Dfb_phe13_data %>%
  filter(Variable == "Duration") %>%
  select(Variable, Forest_Type, IncMSE, NRF_Importance) %>%  # 选择需要显示的列
  print(n = Inf)  # 确保显示所有行（不截断）
# Variable Forest_Type IncMSE NRF_Importance
# 1 Duration NRF           35.4           35.4
# 2 Duration PF            31.4           35.4
# 3 Duration P             42.2           35.48

# 移除phe13的Duration后，查看最大最小值作图  0.36 19.8
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
  scale_y_continuous(expand = c(0, 0), limits = c(0, 36), breaks = c(0, 15, 30)) +
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

# 显示phe3_4的重要性
Dfb_phe34_data %>%
  filter(Variable == "Duration") %>%
  select(Variable, Forest_Type, IncMSE, NRF_Importance) %>%  # 选择需要显示的列
  print(n = Inf)  # 确保显示所有行（不截断）
# Variable Forest_Type IncMSE NRF_Importance
# 1 Duration NRF           50.0           50.0
# 2 Duration PF            64.9           50.0
# 3 Duration P             51.6           50.0

# 移除phe3_4的Duration后，查看最大最小值作图  0 33.5
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
  scale_y_continuous(expand = c(0, 0), limits = c(0, 36), breaks = c(0, 15, 30)) +
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


# 查看最大最小值   0.02  29.7
Dfc_phe13_data %>% 
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

Dfc_phe13_data %>%
  filter(Variable == "Duration") %>%
  select(Variable, Forest_Type, IncMSE, NRF_Importance) %>%  # 选择需要显示的列
  print(n = Inf)  # 确保显示所有行（不截断）
# Variable Forest_Type IncMSE NRF_Importance
# 1 Duration NRF           6.09           6.09
# 2 Duration PF            6.72           6.09
# 3 Duration P            12.4            6.09

# 移除phe13的Duration后，查看最大最小值作图  0.02 29.7
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
  scale_y_continuous(expand = c(0, 0), limits = c(0, 32), breaks = c(0, 15, 30)) +
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

# 查看最大最小值   0.0  50.6
Dfc_phe34_data %>% 
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))
# 负值替换为0后取最小; 负值项影响低参考https://stackoverflow.com/questions/27918320/what-does-negative-incmse-in-randomforest-package-mean
Dfc_phe34_data$IncMSE[Dfc_phe34_data$IncMSE < 0] <- 0
# 显示phe3_4的重要性
Dfc_phe34_data %>%
  filter(Variable == "Duration") %>%
  select(Variable, Forest_Type, IncMSE, NRF_Importance) %>%  # 选择需要显示的列
  print(n = Inf)  # 确保显示所有行（不截断）
# Variable Forest_Type IncMSE NRF_Importance
# 1 Duration NRF           42.8           42.8
# 2 Duration PF            50.6           42.8
# 3 Duration P             43.4           42.8

# 移除phe3_4的Duration后，查看最大最小值作图  0 11.6
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
  scale_y_continuous(expand = c(0, 0), limits = c(0, 32), breaks = c(0, 15, 30)) +
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


# 查看最大最小值   0.33  8.63
Dwb_phe13_data %>% 
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

Dwb_phe13_data %>%
  filter(Variable == "Duration") %>%
  select(Variable, Forest_Type, IncMSE, NRF_Importance) %>%  # 选择需要显示的列
  print(n = Inf)  # 确保显示所有行（不截断）
# # Variable Forest_Type IncMSE NRF_Importance
# 1 Duration NRF           8.63           8.63
# 2 Duration PF            7.91           8.63
# 3 Duration P             4.51           8.63

# 移除phe13的Duration后，查看最大最小值作图  0.33 3.23
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
  scale_y_continuous(expand = c(0, 0), limits = c(0, 11), breaks = c(0, 5, 10)) +
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


# 查看最大最小值   0.0  57.8
Dwb_phe34_data %>% 
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

# 显示phe3_4的重要性
Dwb_phe34_data %>%
  filter(Variable == "Duration") %>%
  select(Variable, Forest_Type, IncMSE, NRF_Importance) %>%  # 选择需要显示的列
  print(n = Inf)  # 确保显示所有行（不截断）
# Variable Forest_Type IncMSE NRF_Importance
# 1 Duration NRF           57.8           57.8
# 2 Duration PF            37.5           57.8
# 3 Duration P             40.3           57.8

# 移除phe3_4的Duration后，查看最大最小值作图  0 6.59
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
  scale_y_continuous(expand = c(0, 0), limits = c(0, 11), breaks = c(0, 5, 10)) +
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

# 查看最大最小值   0.29  6.91
Dwc_phe13_data %>% 
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))
Dwc_phe13_data %>%
  filter(Variable == "Duration") %>%
  select(Variable, Forest_Type, IncMSE, NRF_Importance) %>%  # 选择需要显示的列
  print(n = Inf)  # 确保显示所有行（不截断）
# Variable Forest_Type IncMSE NRF_Importance
# 1 Duration NRF           4.56           4.56
# 2 Duration PF            2.75           4.56
# 3 Duration P             1.78           4.56

# 移除phe13的Duration后，查看最大最小值作图  0 6.91
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
  scale_y_continuous(expand = c(0, 0), limits = c(0, 11), breaks = c(0, 5, 10)) +
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


# 查看最大最小值   0.0  38.0
Dwc_phe34_data %>% 
  summarise(min = min(IncMSE, na.rm = TRUE),
            max = max(IncMSE, na.rm = TRUE))

# 显示phe3_4的重要性
Dwc_phe34_data %>%
  filter(Variable == "Duration") %>%
  select(Variable, Forest_Type, IncMSE, NRF_Importance) %>%  # 选择需要显示的列
  print(n = Inf)  # 确保显示所有行（不截断）
# Variable Forest_Type IncMSE NRF_Importance
# 1 Duration NRF           33.3           33.3
# 2 Duration PF            38.0           33.3
# 3 Duration P             31.9           33.3

# 移除phe3_4的Duration后，查看最大最小值作图  0 9.62
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
  scale_y_continuous(expand = c(0, 0), limits = c(0, 11), breaks = c(0, 5, 10)) +
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


####################################### 7 legend#####  #########################################

# 创建绘图
p_phe34_Dwc_legend <- ggplot(Dwc_phe34_data, aes(x = Variable, y = IncMSE, color = Forest_Type)) +
  geom_point(shape = 1, size = 2, stroke = 2, alpha = 0.9) +
  scale_color_manual(values = forest_colors,name = "Forest type",
                     breaks = c("PF", "NRF", "P")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 11), breaks = c(0, 5, 10)) +
  labs(title = "Dwc",
       x = "",
       y = "%IncMSE") +
  theme_classic() + # 使用classic主题作为基础
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "bottom",
    legend.title = element_text(size = 17,vjust = 1),  # 图例标题 高度居中
    legend.text = element_text(size = 16), 
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color = "grey50",linetype = "dashed",linewidth = 0.5), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()
# 显示图形
print(p_phe34_Dwc_legend)


####################################### 8 Combine Plots #####  #########################################


# 第一行的 6 张图
row1 <- list(p_phe13_Cfa, p_phe13_Cfb, p_phe13_Dfb, p_phe13_Dfc, p_phe13_Dwb, p_phe13_Dwc)

# 第二行的 6 张图
row2 <- list(p_phe34_Cfa, p_phe34_Cfb, p_phe34_Dfb, p_phe34_Dfc, p_phe34_Dwb, p_phe34_Dwc)

# 组合成 1 行 6 列的布局（第一行）
combined_plot1 <- ggarrange(plotlist = row1, ncol = 6, nrow = 1)

# 组合成 1 行 6 列的布局（第二行）
combined_plot2 <- ggarrange(plotlist = row2, ncol = 6, nrow = 1)

# 保存第一行图像
ggsave(
  filename = "./0.figure/03 Paper/Fig.8-a.Importance_GDO_plot.tiff",
  plot = combined_plot1, width = 14, height = 3.5, units = "in", dpi = 300)

# 保存第二行图像
ggsave(
  filename = "./0.figure/03 Paper/Fig.8-b.Importance_GDO_plot.tiff",
  plot = combined_plot2, width = 14, height = 3.5, units = "in", dpi = 300)

# 保存带图例的图像
ggsave(
  filename = "./0.figure/03 Paper/Fig.8-b.Importance_plot_legend.tiff",
  plot = p_phe34_Dwc_legend, width = 6, height = 3.5, units = "in", dpi = 300)
