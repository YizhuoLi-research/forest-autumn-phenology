
library(dplyr)
library(tidyr)
library(ggpubr)
library(ggplot2)


setwd("D:/Graduation_Thesis")

###########################  重要性时间序列作图 Fig.15  ######################
###################################  00.筛选人工林 #####################################

imp_df_13 <- read.csv("./EA+NA_Results/merged_Indices_Importance_result/EOS_10yrTrend_of_Indices_Importance_13.csv", stringsAsFactors = FALSE)
range(imp_df_13$mk_tau, na.rm = TRUE)  # [1] -0.62  0.55

## 原生林-SOS-GMO的指标趋势结果
imp_df_13_PF <- imp_df_13 %>%
  filter(Forest_Type == 3)

imp_df_34 <- read.csv("./EA+NA_Results/merged_Indices_Importance_result/EOS_10yrTrend_of_Indices_Importance_34.csv", stringsAsFactors = FALSE)
range(imp_df_34$mk_tau, na.rm = TRUE)  # [1] -0.6  0.66

## 原生林-GMO-EOS的指标趋势结果
imp_df_34_PF <- imp_df_34 %>%
  filter(Forest_Type == 3)

imp_df_46 <- read.csv("./EA+NA_Results/merged_Indices_Importance_result/EOS_10yrTrend_of_Indices_Importance_46.csv", stringsAsFactors = FALSE)
range(imp_df_46$mk_tau, na.rm = TRUE)  # [1] -0.51  0.64

## 原生林-GDO-EOS的指标趋势结果
imp_df_46_PF <- imp_df_46 %>%
  filter(Forest_Type == 3)


####################################### 01 对Cfa区域  ################################################
######01-1 Cfa_phe13   #########

imp_df_13_PF_Cfa <- imp_df_13_PF %>%
  filter(Climate_Class == "Cfa")

# 添加颜色分类
imp_df_13_PF_Cfa <- imp_df_13_PF_Cfa %>%
  mutate(color = ifelse(mk_tau > 0, "Positive", "Negative"))

#显示数据结果：
imp_df_13_PF_Cfa_selected <- imp_df_13_PF_Cfa %>%
  select(Climate_Class, Forest_Type, Variable, mk_tau, mk_p, stars) %>%
  mutate(mk_tau = formatC(mk_tau, format = "f", digits = 2))
print(imp_df_13_PF_Cfa_selected)
# Climate_Class Forest_Type Variable mk_tau       mk_p stars
# 1            Cfa           3      CDD   0.47 0.07363832      
# 2            Cfa           3      CWD  -0.02 1.00000000      
# 3            Cfa           3      DTR   0.20 0.47427440      
# 4            Cfa           3 Duration  -0.33 0.21049768      
# 5            Cfa           3  Rainday  -0.07 0.85802764      
# 6            Cfa           3   Rx1day   0.22 0.41896176      
# 7            Cfa           3     SDII   0.27 0.32323623      
# 8            Cfa           3    TNlt2  -0.13 0.65342212      
# 9            Cfa           3      TNn   0.31 0.24303496      
# 10           Cfa           3   TXge30   0.24 0.37109327      
# 11           Cfa           3      TXx   0.47 0.07363832

# PF-phe13-Cfa绘制趋势条形图
p_phe13_Cfa <- ggplot(imp_df_13_PF_Cfa,aes(x = reorder(Variable, mk_tau),y = mk_tau, fill = color)) +
  geom_col(width = 0.8) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.6, linetype = "dashed") +  # 添加 x=0 的竖线
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#fc8d59", "Negative" = "lightblue")) +
  scale_y_continuous(limits = c(-0.8, 0.8)) +
  labs(title = "Cfa",
       x = "",
       y = "Trend") +
  theme_classic() + # 使用classic主题作为基础
  geom_text(aes(label = stars),
            vjust = 0.7,  # 往下移动星号（0.5 为正中，>0.5 往下）
            hjust = ifelse(imp_df_13_PF_Cfa$mk_tau >= 0, -0.1, 1.3),  # 斜率为正则星号放在右侧，否则左侧
            size = 7) +
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "none",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    # panel.grid = element_blank()         # 网格线（全部移除）
    panel.grid.major.y = element_line(color = "grey90",linetype = "solid",linewidth = 0.1), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe13_Cfa) 

######01-2 Cfa_phe34   #########

imp_df_34_PF_Cfa <- imp_df_34_PF %>%
  filter(Climate_Class == "Cfa")

# 添加颜色分类
imp_df_34_PF_Cfa <- imp_df_34_PF_Cfa %>%
  mutate(color = ifelse(mk_tau > 0, "Positive", "Negative"))

#显示数据结果：
imp_df_34_PF_Cfa_selected <- imp_df_34_PF_Cfa %>%
  select(Climate_Class, Forest_Type, Variable, mk_tau, mk_p, stars) %>%
  mutate(mk_tau = formatC(mk_tau, format = "f", digits = 2))
print(imp_df_34_PF_Cfa_selected)
# Climate_Class Forest_Type Variable mk_tau       mk_p stars
# 1            Cfa           3      CDD   0.02 1.00000000      
# 2            Cfa           3      CWD   0.07 0.85802770      
# 3            Cfa           3      DTR   0.16 0.59150505      
# 4            Cfa           3 Duration   0.24 0.37109327      
# 5            Cfa           3  Rainday   0.00 1.00000000      
# 6            Cfa           3   Rx1day   0.18 0.52959883      
# 7            Cfa           3     SDII   0.11 0.72051477      
# 8            Cfa           3    TNlt2   0.00 1.00000000      
# 9            Cfa           3      TNn  -0.16 0.59150505      
# 10           Cfa           3   TXge30   0.60 0.02004468     *
# 11           Cfa           3      TXx   0.02 1.00000000 

# PF-phe34-Cfa绘制趋势条形图
p_phe34_Cfa <- ggplot(imp_df_34_PF_Cfa,aes(x = reorder(Variable, mk_tau),y = mk_tau, fill = color)) +
  geom_col(width = 0.8) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.6, linetype = "dashed") +  # 添加 x=0 的竖线
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#fc8d59", "Negative" = "lightblue")) +
  scale_y_continuous(limits = c(-0.8, 0.8)) +
  labs(title = "Cfa",
       x = "",
       y = "Trend") +
  theme_classic() + # 使用classic主题作为基础
  
  geom_text(aes(label = stars),
            vjust = 0.7,  # 往下移动星号（0.5 为正中，>0.5 往下）
            hjust = ifelse(imp_df_34_PF_Cfa$mk_tau >= 0, -0.1, 1.3),  # 斜率为正则星号放在右侧，否则左侧
            size = 7) +
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "none",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    # panel.grid = element_blank()         # 网格线（全部移除）
    panel.grid.major.y = element_line(color = "grey90",linetype = "solid",linewidth = 0.1), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe34_Cfa) 

######01-3 Cfa_phe46   #########

imp_df_46_PF_Cfa <- imp_df_46_PF %>%
  filter(Climate_Class == "Cfa")

# 添加颜色分类
imp_df_46_PF_Cfa <- imp_df_46_PF_Cfa %>%
  mutate(color = ifelse(mk_tau > 0, "Positive", "Negative"))

#显示数据结果：
imp_df_46_PF_Cfa_selected <- imp_df_46_PF_Cfa %>%
  select(Climate_Class, Forest_Type, Variable, mk_tau, mk_p, stars) %>%
  mutate(mk_tau = formatC(mk_tau, format = "f", digits = 2))
print(imp_df_46_PF_Cfa_selected)
# Climate_Class Forest_Type Variable mk_tau      mk_p stars
# 1            Cfa           3      CDD  -0.16 0.5915051      
# 2            Cfa           3      CWD   0.36 0.1779674      
# 3            Cfa           3      DTR   0.20 0.4742744      
# 4            Cfa           3 Duration  -0.33 0.2104977      
# 5            Cfa           3  Rainday   0.29 0.2831309      
# 6            Cfa           3   Rx1day  -0.24 0.3710933      
# 7            Cfa           3     SDII  -0.29 0.2831308      
# 8            Cfa           3    TNlt2  -0.38 0.1524063      
# 9            Cfa           3      TNn   0.42 0.1074047      
# 10           Cfa           3   TXge30   0.33 0.2104976      
# 11           Cfa           3      TXx  -0.02 1.0000000 

# PF-phe46-Cfa绘制趋势条形图
p_phe46_Cfa <- ggplot(imp_df_46_PF_Cfa,aes(x = reorder(Variable, mk_tau),y = mk_tau, fill = color)) +
  geom_col(width = 0.8) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.6, linetype = "dashed") +  # 添加 x=0 的竖线
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#fc8d59", "Negative" = "lightblue")) +
  scale_y_continuous(limits = c(-0.8, 0.8)) +
  labs(title = "Cfa",
       x = "",
       y = "Trend") +
  theme_classic() + # 使用classic主题作为基础
  
  geom_text(aes(label = stars),
            vjust = 0.7,  # 往下移动星号（0.5 为正中，>0.5 往下）
            hjust = ifelse(imp_df_46_PF_Cfa$mk_tau >= 0, -0.1, 1.3),  # 斜率为正则星号放在右侧，否则左侧
            size = 7) +
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "none",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    # panel.grid = element_blank()         # 网格线（全部移除）
    panel.grid.major.y = element_line(color = "grey90",linetype = "solid",linewidth = 0.1), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe46_Cfa) 


####################################### 02 对Cfb区域#####  #########################################
######02-1 Cfb_phe13   #########

imp_df_13_PF_Cfb <- imp_df_13_PF %>%
  filter(Climate_Class == "Cfb")

# 添加颜色分类
imp_df_13_PF_Cfb <- imp_df_13_PF_Cfb %>%
  mutate(color = ifelse(mk_tau > 0, "Positive", "Negative"))

#显示数据结果：
imp_df_13_PF_Cfb_selected <- imp_df_13_PF_Cfb %>%
  select(Climate_Class, Forest_Type, Variable, mk_tau, mk_p, stars) %>%
  mutate(mk_tau = formatC(mk_tau, format = "f", digits = 2))
print(imp_df_13_PF_Cfb_selected)
# Climate_Class Forest_Type Variable mk_tau       mk_p stars
# 1            Cfb           3      CDD  -0.13 0.65342212      
# 2            Cfb           3      CWD  -0.22 0.41896176      
# 3            Cfb           3      DTR  -0.33 0.21049768      
# 4            Cfb           3 Duration  -0.47 0.07363828      
# 5            Cfb           3  Rainday  -0.38 0.15240626      
# 6            Cfb           3   Rx1day  -0.04 0.92844403      
# 7            Cfb           3     SDII  -0.56 0.03182312     *
# 8            Cfb           3    TNlt2   0.16 0.59150505      
# 9            Cfb           3      TNn  -0.20 0.47427434      
# 10           Cfb           3   TXge30   0.49 0.08379805      
# 11           Cfb           3      TXx   0.29 0.28313088 

# PF-phe13-Cfb绘制趋势条形图
p_phe13_Cfb <- ggplot(imp_df_13_PF_Cfb,aes(x = reorder(Variable, mk_tau),y = mk_tau, fill = color)) +
  geom_col(width = 0.8) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.6, linetype = "dashed") +  # 添加 x=0 的竖线
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#fc8d59", "Negative" = "lightblue")) +
  scale_y_continuous(limits = c(-0.8, 0.8)) +
  labs(title = "Cfb",
       x = "",
       y = "Trend") +
  theme_classic() + # 使用classic主题作为基础
  geom_text(aes(label = stars),
            vjust = 0.7,  # 往下移动星号（0.5 为正中，>0.5 往下）
            hjust = ifelse(imp_df_13_PF_Cfb$mk_tau >= 0, -0.1, 1.3),  # 斜率为正则星号放在右侧，否则左侧
            size = 7) +
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "none",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    # panel.grid = element_blank()         # 网格线（全部移除）
    panel.grid.major.y = element_line(color = "grey90",linetype = "solid",linewidth = 0.1), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe13_Cfb) 

######02-2 Cfb_phe34   #########

imp_df_34_PF_Cfb <- imp_df_34_PF %>%
  filter(Climate_Class == "Cfb")

# 添加颜色分类
imp_df_34_PF_Cfb <- imp_df_34_PF_Cfb %>%
  mutate(color = ifelse(mk_tau > 0, "Positive", "Negative"))

#显示数据结果：
imp_df_34_PF_Cfb_selected <- imp_df_34_PF_Cfb %>%
  select(Climate_Class, Forest_Type, Variable, mk_tau, mk_p, stars) %>%
  mutate(mk_tau = formatC(mk_tau, format = "f", digits = 2))
print(imp_df_34_PF_Cfb_selected)
# Climate_Class Forest_Type Variable mk_tau      mk_p stars
# 1            Cfb           3      CDD  -0.20 0.4742743      
# 2            Cfb           3      CWD  -0.11 0.7205148      
# 3            Cfb           3      DTR  -0.20 0.4742743      
# 4            Cfb           3 Duration  -0.07 0.8580276      
# 5            Cfb           3  Rainday  -0.16 0.5915051      
# 6            Cfb           3   Rx1day   0.07 0.8580277      
# 7            Cfb           3     SDII  -0.29 0.2831308      
# 8            Cfb           3    TNlt2   0.00 1.0000000      
# 9            Cfb           3      TNn   0.24 0.3710933      
# 10           Cfb           3   TXge30   0.20 0.4742744      
# 11           Cfb           3      TXx   0.16 0.5915051  

# PF-phe34-Cfb绘制趋势条形图
p_phe34_Cfb <- ggplot(imp_df_34_PF_Cfb,aes(x = reorder(Variable, mk_tau),y = mk_tau, fill = color)) +
  geom_col(width = 0.8) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.6, linetype = "dashed") +  # 添加 x=0 的竖线
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#fc8d59", "Negative" = "lightblue")) +
  scale_y_continuous(limits = c(-0.8, 0.8)) +
  labs(title = "Cfb",
       x = "",
       y = "Trend") +
  theme_classic() + # 使用classic主题作为基础
  
  geom_text(aes(label = stars),
            vjust = 0.7,  # 往下移动星号（0.5 为正中，>0.5 往下）
            hjust = ifelse(imp_df_34_PF_Cfb$mk_tau >= 0, -0.1, 1.3),  # 斜率为正则星号放在右侧，否则左侧
            size = 7) +
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "none",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    # panel.grid = element_blank()         # 网格线（全部移除）
    panel.grid.major.y = element_line(color = "grey90",linetype = "solid",linewidth = 0.1), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe34_Cfb) 

######02-3 Cfb_phe46   #########

imp_df_46_PF_Cfb <- imp_df_46_PF %>%
  filter(Climate_Class == "Cfb")

# 添加颜色分类
imp_df_46_PF_Cfb <- imp_df_46_PF_Cfb %>%
  mutate(color = ifelse(mk_tau > 0, "Positive", "Negative"))

#显示数据结果：
imp_df_46_PF_Cfb_selected <- imp_df_46_PF_Cfb %>%
  select(Climate_Class, Forest_Type, Variable, mk_tau, mk_p, stars) %>%
  mutate(mk_tau = formatC(mk_tau, format = "f", digits = 2))
print(imp_df_46_PF_Cfb_selected)
# Climate_Class Forest_Type Variable mk_tau       mk_p stars
# 1            Cfb           3      CDD   0.20 0.47427440      
# 2            Cfb           3      CWD   0.29 0.28313088      
# 3            Cfb           3      DTR  -0.24 0.37109333      
# 4            Cfb           3 Duration   0.29 0.28313088      
# 5            Cfb           3  Rainday   0.64 0.01226604     *
# 6            Cfb           3   Rx1day  -0.33 0.21049768      
# 7            Cfb           3     SDII  -0.11 0.72051477      
# 8            Cfb           3    TNlt2  -0.24 0.37109333      
# 9            Cfb           3      TNn  -0.02 1.00000000      
# 10           Cfb           3   TXge30   0.24 0.37109327      
# 11           Cfb           3      TXx   0.20 0.47427440

# PF-phe46-Cfb绘制趋势条形图
p_phe46_Cfb <- ggplot(imp_df_46_PF_Cfb,aes(x = reorder(Variable, mk_tau),y = mk_tau, fill = color)) +
  geom_col(width = 0.8) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.6, linetype = "dashed") +  # 添加 x=0 的竖线
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#fc8d59", "Negative" = "lightblue")) +
  scale_y_continuous(limits = c(-0.8, 0.8)) +
  labs(title = "Cfb",
       x = "",
       y = "Trend") +
  theme_classic() + # 使用classic主题作为基础
  
  geom_text(aes(label = stars),
            vjust = 0.7,  # 往下移动星号（0.5 为正中，>0.5 往下）
            hjust = ifelse(imp_df_46_PF_Cfb$mk_tau >= 0, -0.1, 1.3),  # 斜率为正则星号放在右侧，否则左侧
            size = 7) +
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "none",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    # panel.grid = element_blank()         # 网格线（全部移除）
    panel.grid.major.y = element_line(color = "grey90",linetype = "solid",linewidth = 0.1), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe46_Cfb) 


####################################### 03 对Dfb区域#####  #########################################
######03-1 Dfb_phe13   #########

imp_df_13_PF_Dfb <- imp_df_13_PF %>%
  filter(Climate_Class == "Dfb")

# 添加颜色分类
imp_df_13_PF_Dfb <- imp_df_13_PF_Dfb %>%
  mutate(color = ifelse(mk_tau > 0, "Positive", "Negative"))

#显示数据结果：
imp_df_13_PF_Dfb_selected <- imp_df_13_PF_Dfb %>%
  select(Climate_Class, Forest_Type, Variable, mk_tau, mk_p, stars) %>%
  mutate(mk_tau = formatC(mk_tau, format = "f", digits = 2))
print(imp_df_13_PF_Dfb_selected)
# Climate_Class Forest_Type Variable mk_tau      mk_p stars
# 1            Dfb           3      CDD   0.20 0.4742744      
# 2            Dfb           3      CWD  -0.09 0.7876158      
# 3            Dfb           3      DTR   0.36 0.1779674      
# 4            Dfb           3 Duration  -0.07 0.8580276      
# 5            Dfb           3  Rainday  -0.02 1.0000000      
# 6            Dfb           3   Rx1day   0.07 0.8580277      
# 7            Dfb           3     SDII   0.11 0.7205148      
# 8            Dfb           3    TNlt2   0.02 1.0000000      
# 9            Dfb           3      TNn   0.07 0.8580277      
# 10           Dfb           3   TXge30   0.21 0.4658276      
# 11           Dfb           3      TXx   0.04 0.9284440   

# PF-phe13-Dfb绘制趋势条形图
p_phe13_Dfb <- ggplot(imp_df_13_PF_Dfb,aes(x = reorder(Variable, mk_tau),y = mk_tau, fill = color)) +
  geom_col(width = 0.8) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.6, linetype = "dashed") +  # 添加 x=0 的竖线
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#fc8d59", "Negative" = "lightblue")) +
  scale_y_continuous(limits = c(-0.8, 0.8)) +
  labs(title = "Dfb",
       x = "",
       y = "Trend") +
  theme_classic() + # 使用classic主题作为基础
  geom_text(aes(label = stars),
            vjust = 0.7,  # 往下移动星号（0.5 为正中，>0.5 往下）
            hjust = ifelse(imp_df_13_PF_Dfb$mk_tau >= 0, -0.1, 1.3),  # 斜率为正则星号放在右侧，否则左侧
            size = 7) +
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "none",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    # panel.grid = element_blank()         # 网格线（全部移除）
    panel.grid.major.y = element_line(color = "grey90",linetype = "solid",linewidth = 0.1), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe13_Dfb) 

######03-2 Dfb_phe34   #########

imp_df_34_PF_Dfb <- imp_df_34_PF %>%
  filter(Climate_Class == "Dfb")

# 添加颜色分类
imp_df_34_PF_Dfb <- imp_df_34_PF_Dfb %>%
  mutate(color = ifelse(mk_tau > 0, "Positive", "Negative"))

#显示数据结果：
imp_df_34_PF_Dfb_selected <- imp_df_34_PF_Dfb %>%
  select(Climate_Class, Forest_Type, Variable, mk_tau, mk_p, stars) %>%
  mutate(mk_tau = formatC(mk_tau, format = "f", digits = 2))
print(imp_df_34_PF_Dfb_selected)
# Climate_Class Forest_Type Variable mk_tau      mk_p stars
# 1            Dfb           3      CDD  -0.18 0.5295988      
# 2            Dfb           3      CWD   0.07 0.8580277      
# 3            Dfb           3      DTR   0.38 0.1524062      
# 4            Dfb           3 Duration  -0.20 0.4742743      
# 5            Dfb           3  Rainday  -0.04 0.9284440      
# 6            Dfb           3   Rx1day   0.07 0.8580277      
# 7            Dfb           3     SDII  -0.24 0.3710933      
# 8            Dfb           3    TNlt2   0.05 1.0000000      
# 9            Dfb           3      TNn   0.02 1.0000000      
# 10           Dfb           3   TXge30   0.24 0.3710933      
# 11           Dfb           3      TXx  -0.02 1.0000000 

# PF-phe34-Dfb绘制趋势条形图
p_phe34_Dfb <- ggplot(imp_df_34_PF_Dfb,aes(x = reorder(Variable, mk_tau),y = mk_tau, fill = color)) +
  geom_col(width = 0.8) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.6, linetype = "dashed") +  # 添加 x=0 的竖线
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#fc8d59", "Negative" = "lightblue")) +
  scale_y_continuous(limits = c(-0.8, 0.8)) +
  labs(title = "Dfb",
       x = "",
       y = "Trend") +
  theme_classic() + # 使用classic主题作为基础
  
  geom_text(aes(label = stars),
            vjust = 0.7,  # 往下移动星号（0.5 为正中，>0.5 往下）
            hjust = ifelse(imp_df_34_PF_Dfb$mk_tau >= 0, -0.1, 1.3),  # 斜率为正则星号放在右侧，否则左侧
            size = 7) +
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "none",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    # panel.grid = element_blank()         # 网格线（全部移除）
    panel.grid.major.y = element_line(color = "grey90",linetype = "solid",linewidth = 0.1), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe34_Dfb) 

######03-3 Dfb_phe46   #########

imp_df_46_PF_Dfb <- imp_df_46_PF %>%
  filter(Climate_Class == "Dfb")

# 添加颜色分类
imp_df_46_PF_Dfb <- imp_df_46_PF_Dfb %>%
  mutate(color = ifelse(mk_tau > 0, "Positive", "Negative"))

#显示数据结果：
imp_df_46_PF_Dfb_selected <- imp_df_46_PF_Dfb %>%
  select(Climate_Class, Forest_Type, Variable, mk_tau, mk_p, stars) %>%
  mutate(mk_tau = formatC(mk_tau, format = "f", digits = 2))
print(imp_df_46_PF_Dfb_selected)
# Climate_Class Forest_Type Variable mk_tau      mk_p stars
# 1            Dfb           3      CDD  -0.07 0.8580276      
# 2            Dfb           3      CWD  -0.04 0.9284440      
# 3            Dfb           3      DTR   0.07 0.8580277      
# 4            Dfb           3 Duration   0.02 1.0000000      
# 5            Dfb           3  Rainday   0.20 0.4742744      
# 6            Dfb           3   Rx1day  -0.24 0.3710933      
# 7            Dfb           3     SDII  -0.33 0.2104977      
# 8            Dfb           3    TNlt2  -0.02 1.0000000      
# 9            Dfb           3      TNn  -0.20 0.4742743      
# 10           Dfb           3   TXge30  -0.33 0.2104977      
# 11           Dfb           3      TXx  -0.24 0.3710933 

# PF-phe46-Dfb绘制趋势条形图
p_phe46_Dfb <- ggplot(imp_df_46_PF_Dfb,aes(x = reorder(Variable, mk_tau),y = mk_tau, fill = color)) +
  geom_col(width = 0.8) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.6, linetype = "dashed") +  # 添加 x=0 的竖线
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#fc8d59", "Negative" = "lightblue")) +
  scale_y_continuous(limits = c(-0.8, 0.8)) +
  labs(title = "Dfb",
       x = "",
       y = "Trend") +
  theme_classic() + # 使用classic主题作为基础
  
  geom_text(aes(label = stars),
            vjust = 0.7,  # 往下移动星号（0.5 为正中，>0.5 往下）
            hjust = ifelse(imp_df_46_PF_Dfb$mk_tau >= 0, -0.1, 1.3),  # 斜率为正则星号放在右侧，否则左侧
            size = 7) +
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "none",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    # panel.grid = element_blank()         # 网格线（全部移除）
    panel.grid.major.y = element_line(color = "grey90",linetype = "solid",linewidth = 0.1), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe46_Dfb) 

####################################### 04 对Dfc区域#####  #########################################
######04-1 Dfc_phe13   #########

imp_df_13_PF_Dfc <- imp_df_13_PF %>%
  filter(Climate_Class == "Dfc")

# 添加颜色分类
imp_df_13_PF_Dfc <- imp_df_13_PF_Dfc %>%
  mutate(color = ifelse(mk_tau > 0, "Positive", "Negative"))

#显示数据结果：
imp_df_13_PF_Dfc_selected <- imp_df_13_PF_Dfc %>%
  select(Climate_Class, Forest_Type, Variable, mk_tau, mk_p, stars) %>%
  mutate(mk_tau = formatC(mk_tau, format = "f", digits = 2))
print(imp_df_13_PF_Dfc_selected)
# Climate_Class Forest_Type Variable mk_tau       mk_p stars
# 1            Dfc           3      CDD   0.00 1.00000000      
# 2            Dfc           3      CWD  -0.13 0.65342212      
# 3            Dfc           3      DTR  -0.16 0.59150505      
# 4            Dfc           3 Duration  -0.36 0.17796741      
# 5            Dfc           3  Rainday  -0.16 0.59150505      
# 6            Dfc           3   Rx1day  -0.16 0.59150505      
# 7            Dfc           3     SDII  -0.04 0.92844403      
# 8            Dfc           3    TNlt2   0.07 0.85802770      
# 9            Dfc           3      TNn   0.11 0.72051477      
# 10           Dfc           3   TXge30   0.25 0.48623431      
# 11           Dfc           3      TXx   0.45 0.08796144

# PF-phe13-Dfc绘制趋势条形图
p_phe13_Dfc <- ggplot(imp_df_13_PF_Dfc,aes(x = reorder(Variable, mk_tau),y = mk_tau, fill = color)) +
  geom_col(width = 0.8) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.6, linetype = "dashed") +  # 添加 x=0 的竖线
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#fc8d59", "Negative" = "lightblue")) +
  scale_y_continuous(limits = c(-0.8, 0.8)) +
  labs(title = "Dfc",
       x = "",
       y = "Trend") +
  theme_classic() + # 使用classic主题作为基础
  geom_text(aes(label = stars),
            vjust = 0.7,  # 往下移动星号（0.5 为正中，>0.5 往下）
            hjust = ifelse(imp_df_13_PF_Dfc$mk_tau >= 0, -0.1, 1.3),  # 斜率为正则星号放在右侧，否则左侧
            size = 7) +
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "none",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    # panel.grid = element_blank()         # 网格线（全部移除）
    panel.grid.major.y = element_line(color = "grey90",linetype = "solid",linewidth = 0.1), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe13_Dfc) 

######04-2 Dfc_phe34   #########

imp_df_34_PF_Dfc <- imp_df_34_PF %>%
  filter(Climate_Class == "Dfc")

# 添加颜色分类
imp_df_34_PF_Dfc <- imp_df_34_PF_Dfc %>%
  mutate(color = ifelse(mk_tau > 0, "Positive", "Negative"))

#显示数据结果：
imp_df_34_PF_Dfc_selected <- imp_df_34_PF_Dfc %>%
  select(Climate_Class, Forest_Type, Variable, mk_tau, mk_p, stars) %>%
  mutate(mk_tau = formatC(mk_tau, format = "f", digits = 2))
print(imp_df_34_PF_Dfc_selected)
# Climate_Class Forest_Type Variable mk_tau       mk_p stars
# 1            Dfc           3      CDD  -0.11 0.71834773      
# 2            Dfc           3      CWD  -0.18 0.52959883      
# 3            Dfc           3      DTR   0.07 0.85802770      
# 4            Dfc           3 Duration   0.42 0.10740471      
# 5            Dfc           3  Rainday   0.24 0.37109327      
# 6            Dfc           3   Rx1day  -0.20 0.47427434      
# 7            Dfc           3     SDII  -0.11 0.72051477      
# 8            Dfc           3    TNlt2   0.15 0.72772360      
# 9            Dfc           3      TNn   0.64 0.01226604     *
# 10           Dfc           3   TXge30   0.48 0.07686102      
# 11           Dfc           3      TXx  -0.02 1.00000000   

# PF-phe34-Dfc绘制趋势条形图
p_phe34_Dfc <- ggplot(imp_df_34_PF_Dfc,aes(x = reorder(Variable, mk_tau),y = mk_tau, fill = color)) +
  geom_col(width = 0.8) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.6, linetype = "dashed") +  # 添加 x=0 的竖线
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#fc8d59", "Negative" = "lightblue")) +
  scale_y_continuous(limits = c(-0.8, 0.8)) +
  labs(title = "Dfc",
       x = "",
       y = "Trend") +
  theme_classic() + # 使用classic主题作为基础
  
  geom_text(aes(label = stars),
            vjust = 0.7,  # 往下移动星号（0.5 为正中，>0.5 往下）
            hjust = ifelse(imp_df_34_PF_Dfc$mk_tau >= 0, -0.1, 1.3),  # 斜率为正则星号放在右侧，否则左侧
            size = 7) +
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "none",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    # panel.grid = element_blank()         # 网格线（全部移除）
    panel.grid.major.y = element_line(color = "grey90",linetype = "solid",linewidth = 0.1), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe34_Dfc) 


######04-3 Dfc_phe46   #########

imp_df_46_PF_Dfc <- imp_df_46_PF %>%
  filter(Climate_Class == "Dfc")

# 添加颜色分类
imp_df_46_PF_Dfc <- imp_df_46_PF_Dfc %>%
  mutate(color = ifelse(mk_tau > 0, "Positive", "Negative"))

#显示数据结果：
imp_df_46_PF_Dfc_selected <- imp_df_46_PF_Dfc %>%
  select(Climate_Class, Forest_Type, Variable, mk_tau, mk_p, stars) %>%
  mutate(mk_tau = formatC(mk_tau, format = "f", digits = 2))
print(imp_df_46_PF_Dfc_selected)
# Climate_Class Forest_Type Variable mk_tau       mk_p stars
# 1            Dfc           3      CDD   0.09 0.78761578      
# 2            Dfc           3      CWD  -0.47 0.07363828      
# 3            Dfc           3      DTR   0.11 0.72051477      
# 4            Dfc           3 Duration   0.16 0.59150505      
# 5            Dfc           3  Rainday  -0.11 0.72051477      
# 6            Dfc           3   Rx1day   0.24 0.37109327      
# 7            Dfc           3     SDII   0.40 0.12684929      
# 8            Dfc           3    TNlt2  -0.18 0.52959883      
# 9            Dfc           3      TNn  -0.42 0.10740466      
# 10           Dfc           3   TXge30  -0.35 0.29626989      
# 11           Dfc           3      TXx   0.11 0.72051477   

# PF-phe46-Dfc绘制趋势条形图
p_phe46_Dfc <- ggplot(imp_df_46_PF_Dfc,aes(x = reorder(Variable, mk_tau),y = mk_tau, fill = color)) +
  geom_col(width = 0.8) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.6, linetype = "dashed") +  # 添加 x=0 的竖线
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#fc8d59", "Negative" = "lightblue")) +
  scale_y_continuous(limits = c(-0.8, 0.8)) +
  labs(title = "Dfc",
       x = "",
       y = "Trend") +
  theme_classic() + # 使用classic主题作为基础
  
  geom_text(aes(label = stars),
            vjust = 0.7,  # 往下移动星号（0.5 为正中，>0.5 往下）
            hjust = ifelse(imp_df_46_PF_Dfc$mk_tau >= 0, -0.1, 1.3),  # 斜率为正则星号放在右侧，否则左侧
            size = 7) +
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "none",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    # panel.grid = element_blank()         # 网格线（全部移除）
    panel.grid.major.y = element_line(color = "grey90",linetype = "solid",linewidth = 0.1), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe46_Dfc) 

####################################### 05 对Dwb区域#####  #########################################
######05-1 Dwb_phe13   #########

imp_df_13_PF_Dwb <- imp_df_13_PF %>%
  filter(Climate_Class == "Dwb")

# 添加颜色分类
imp_df_13_PF_Dwb <- imp_df_13_PF_Dwb %>%
  mutate(color = ifelse(mk_tau > 0, "Positive", "Negative"))

#显示数据结果：
imp_df_13_PF_Dwb_selected <- imp_df_13_PF_Dwb %>%
  select(Climate_Class, Forest_Type, Variable, mk_tau, mk_p, stars) %>%
  mutate(mk_tau = formatC(mk_tau, format = "f", digits = 2))
print(imp_df_13_PF_Dwb_selected)
# Climate_Class Forest_Type Variable mk_tau      mk_p stars
# 1            Dwb           3      CDD   0.18 0.5295988      
# 2            Dwb           3      CWD  -0.09 0.7876158      
# 3            Dwb           3      DTR  -0.24 0.3710933      
# 4            Dwb           3 Duration   0.13 0.6534221      
# 5            Dwb           3  Rainday   0.02 1.0000000      
# 6            Dwb           3   Rx1day   0.11 0.7205148      
# 7            Dwb           3     SDII   0.29 0.2831309      
# 8            Dwb           3    TNlt2   0.09 0.7876158      
# 9            Dwb           3      TNn   0.24 0.3710933      
# 10           Dwb           3   TXge30  -0.26 0.3619763      
# 11           Dwb           3      TXx  -0.24 0.3710933   

# PF-phe13-Dwb绘制趋势条形图
p_phe13_Dwb <- ggplot(imp_df_13_PF_Dwb,aes(x = reorder(Variable, mk_tau),y = mk_tau, fill = color)) +
  geom_col(width = 0.8) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.6, linetype = "dashed") +  # 添加 x=0 的竖线
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#fc8d59", "Negative" = "lightblue")) +
  scale_y_continuous(limits = c(-0.8, 0.8)) +
  labs(title = "Dwb",
       x = "",
       y = "Trend") +
  theme_classic() + # 使用classic主题作为基础
  geom_text(aes(label = stars),
            vjust = 0.7,  # 往下移动星号（0.5 为正中，>0.5 往下）
            hjust = ifelse(imp_df_13_PF_Dwb$mk_tau >= 0, -0.1, 1.3),  # 斜率为正则星号放在右侧，否则左侧
            size = 7) +
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "none",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    # panel.grid = element_blank()         # 网格线（全部移除）
    panel.grid.major.y = element_line(color = "grey90",linetype = "solid",linewidth = 0.1), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe13_Dwb) 

######05-2 Dwb_phe34   #########

imp_df_34_PF_Dwb <- imp_df_34_PF %>%
  filter(Climate_Class == "Dwb")

# 添加颜色分类
imp_df_34_PF_Dwb <- imp_df_34_PF_Dwb %>%
  mutate(color = ifelse(mk_tau > 0, "Positive", "Negative"))

#显示数据结果：
imp_df_34_PF_Dwb_selected <- imp_df_34_PF_Dwb %>%
  select(Climate_Class, Forest_Type, Variable, mk_tau, mk_p, stars) %>%
  mutate(mk_tau = formatC(mk_tau, format = "f", digits = 2))
print(imp_df_34_PF_Dwb_selected)
# Climate_Class Forest_Type Variable mk_tau      mk_p stars
# 1            Dwb           3      CDD   0.20 0.4742744      
# 2            Dwb           3      CWD   0.29 0.2831309      
# 3            Dwb           3      DTR   0.07 0.8580277      
# 4            Dwb           3 Duration   0.24 0.3710933      
# 5            Dwb           3  Rainday   0.16 0.5915051      
# 6            Dwb           3   Rx1day  -0.07 0.8580276      
# 7            Dwb           3     SDII   0.16 0.5915051      
# 8            Dwb           3    TNlt2  -0.05 1.0000000      
# 9            Dwb           3      TNn   0.02 1.0000000      
# 10           Dwb           3   TXge30   0.31 0.2430350      
# 11           Dwb           3      TXx   0.04 0.9284440 

# PF-phe34-Dwb绘制趋势条形图
p_phe34_Dwb <- ggplot(imp_df_34_PF_Dwb,aes(x = reorder(Variable, mk_tau),y = mk_tau, fill = color)) +
  geom_col(width = 0.8) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.6, linetype = "dashed") +  # 添加 x=0 的竖线
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#fc8d59", "Negative" = "lightblue")) +
  scale_y_continuous(limits = c(-0.8, 0.8)) +
  labs(title = "Dwb",
       x = "",
       y = "Trend") +
  theme_classic() + # 使用classic主题作为基础
  
  geom_text(aes(label = stars),
            vjust = 0.7,  # 往下移动星号（0.5 为正中，>0.5 往下）
            hjust = ifelse(imp_df_34_PF_Dwb$mk_tau >= 0, -0.1, 1.3),  # 斜率为正则星号放在右侧，否则左侧
            size = 7) +
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "none",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    # panel.grid = element_blank()         # 网格线（全部移除）
    panel.grid.major.y = element_line(color = "grey90",linetype = "solid",linewidth = 0.1), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe34_Dwb)

######05-3 Dwb_phe46   #########

imp_df_46_PF_Dwb <- imp_df_46_PF %>%
  filter(Climate_Class == "Dwb")

# 添加颜色分类
imp_df_46_PF_Dwb <- imp_df_46_PF_Dwb %>%
  mutate(color = ifelse(mk_tau > 0, "Positive", "Negative"))

#显示数据结果：
imp_df_46_PF_Dwb_selected <- imp_df_46_PF_Dwb %>%
  select(Climate_Class, Forest_Type, Variable, mk_tau, mk_p, stars) %>%
  mutate(mk_tau = formatC(mk_tau, format = "f", digits = 2))
print(imp_df_46_PF_Dwb_selected)
# Climate_Class Forest_Type Variable mk_tau       mk_p stars
# 1            Dwb           3      CDD   0.04 0.92844403      
# 2            Dwb           3      CWD  -0.11 0.72051477      
# 3            Dwb           3      DTR  -0.18 0.52959883      
# 4            Dwb           3 Duration   0.56 0.03182316     *
# 5            Dwb           3  Rainday  -0.07 0.85802764      
# 6            Dwb           3   Rx1day   0.58 0.02476382     *
# 7            Dwb           3     SDII   0.16 0.59150505      
# 8            Dwb           3    TNlt2  -0.11 0.72051477      
# 9            Dwb           3      TNn   0.40 0.12684929      
# 10           Dwb           3   TXge30   0.22 0.50144207      
# 11           Dwb           3      TXx  -0.16 0.59150505 

# PF-phe46-Dwb绘制趋势条形图
p_phe46_Dwb <- ggplot(imp_df_46_PF_Dwb,aes(x = reorder(Variable, mk_tau),y = mk_tau, fill = color)) +
  geom_col(width = 0.8) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.6, linetype = "dashed") +  # 添加 x=0 的竖线
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#fc8d59", "Negative" = "lightblue")) +
  scale_y_continuous(limits = c(-0.8, 0.8)) +
  labs(title = "Dwb",
       x = "",
       y = "Trend") +
  theme_classic() + # 使用classic主题作为基础
  
  geom_text(aes(label = stars),
            vjust = 0.7,  # 往下移动星号（0.5 为正中，>0.5 往下）
            hjust = ifelse(imp_df_46_PF_Dwb$mk_tau >= 0, -0.1, 1.3),  # 斜率为正则星号放在右侧，否则左侧
            size = 7) +
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "none",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    # panel.grid = element_blank()         # 网格线（全部移除）
    panel.grid.major.y = element_line(color = "grey90",linetype = "solid",linewidth = 0.1), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe46_Dwb)

####################################### 06 对Dwc区域#####  #########################################
######06-1 Dwc_phe13   #########

imp_df_13_PF_Dwc <- imp_df_13_PF %>%
  filter(Climate_Class == "Dwc")

# 添加颜色分类
imp_df_13_PF_Dwc <- imp_df_13_PF_Dwc %>%
  mutate(color = ifelse(mk_tau > 0, "Positive", "Negative"))

#显示数据结果：
imp_df_13_PF_Dwc_selected <- imp_df_13_PF_Dwc %>%
  select(Climate_Class, Forest_Type, Variable, mk_tau, mk_p, stars) %>%
  mutate(mk_tau = formatC(mk_tau, format = "f", digits = 2))
print(imp_df_13_PF_Dwc_selected)
# Climate_Class Forest_Type Variable mk_tau       mk_p stars
# 1            Dwc           3      CDD   0.02 1.00000000      
# 2            Dwc           3      CWD   0.09 0.78761578      
# 3            Dwc           3      DTR  -0.04 0.92844403      
# 4            Dwc           3 Duration  -0.54 0.03887941     *
# 5            Dwc           3  Rainday  -0.13 0.65342212      
# 6            Dwc           3   Rx1day  -0.13 0.65342212      
# 7            Dwc           3     SDII  -0.33 0.21049768      
# 8            Dwc           3    TNlt2  -0.33 0.21049768      
# 9            Dwc           3      TNn  -0.24 0.37109333      
# 10           Dwc           3   TXge30  -0.30 0.31631267      
# 11           Dwc           3      TXx  -0.11 0.72051477      

# PF-phe13-Dwc绘制趋势条形图
p_phe13_Dwc <- ggplot(imp_df_13_PF_Dwc,aes(x = reorder(Variable, mk_tau),y = mk_tau, fill = color)) +
  geom_col(width = 0.8) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.6, linetype = "dashed") +  # 添加 x=0 的竖线
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#fc8d59", "Negative" = "lightblue")) +
  scale_y_continuous(limits = c(-0.8, 0.8)) +
  labs(title = "Dwc",
       x = "",
       y = "Trend") +
  theme_classic() + # 使用classic主题作为基础
  geom_text(aes(label = stars),
            vjust = 0.7,  # 往下移动星号（0.5 为正中，>0.5 往下）
            hjust = ifelse(imp_df_13_PF_Dwc$mk_tau >= 0, -0.1, 1.3),  # 斜率为正则星号放在右侧，否则左侧
            size = 7) +
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "none",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    # panel.grid = element_blank()         # 网格线（全部移除）
    panel.grid.major.y = element_line(color = "grey90",linetype = "solid",linewidth = 0.1), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe13_Dwc) 

######06-2 Dwc_phe34   #########

imp_df_34_PF_Dwc <- imp_df_34_PF %>%
  filter(Climate_Class == "Dwc")

# 添加颜色分类
imp_df_34_PF_Dwc <- imp_df_34_PF_Dwc %>%
  mutate(color = ifelse(mk_tau > 0, "Positive", "Negative"))

#显示数据结果：
imp_df_34_PF_Dwc_selected <- imp_df_34_PF_Dwc %>%
  select(Climate_Class, Forest_Type, Variable, mk_tau, mk_p, stars) %>%
  mutate(mk_tau = formatC(mk_tau, format = "f", digits = 2))
print(imp_df_34_PF_Dwc_selected)
# Climate_Class Forest_Type Variable mk_tau      mk_p stars
# 1            Dwc           3      CDD  -0.11 0.7205148      
# 2            Dwc           3      CWD  -0.13 0.6534221      
# 3            Dwc           3      DTR  -0.42 0.1074047      
# 4            Dwc           3 Duration  -0.07 0.8580276      
# 5            Dwc           3  Rainday  -0.20 0.4742743      
# 6            Dwc           3   Rx1day  -0.10 0.7799786      
# 7            Dwc           3     SDII   0.31 0.2430350      
# 8            Dwc           3    TNlt2   0.00 1.0000000      
# 9            Dwc           3      TNn  -0.29 0.2831308      
# 10           Dwc           3   TXge30  -0.15 0.6415175      
# 11           Dwc           3      TXx  -0.16 0.5915051  

# PF-phe34-Dwc绘制趋势条形图
p_phe34_Dwc <- ggplot(imp_df_34_PF_Dwc,aes(x = reorder(Variable, mk_tau),y = mk_tau, fill = color)) +
  geom_col(width = 0.8) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.6, linetype = "dashed") +  # 添加 x=0 的竖线
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#fc8d59", "Negative" = "lightblue")) +
  scale_y_continuous(limits = c(-0.8, 0.8)) +
  labs(title = "Dwc",
       x = "",
       y = "Trend") +
  theme_classic() + # 使用classic主题作为基础
  
  geom_text(aes(label = stars),
            vjust = 0.7,  # 往下移动星号（0.5 为正中，>0.5 往下）
            hjust = ifelse(imp_df_34_PF_Dwc$mk_tau >= 0, -0.1, 1.3),  # 斜率为正则星号放在右侧，否则左侧
            size = 7) +
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "none",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    # panel.grid = element_blank()         # 网格线（全部移除）
    panel.grid.major.y = element_line(color = "grey90",linetype = "solid",linewidth = 0.1), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe34_Dwc)

######06-3 Dwc_phe46   #########

imp_df_46_PF_Dwc <- imp_df_46_PF %>%
  filter(Climate_Class == "Dwc")

# 添加颜色分类
imp_df_46_PF_Dwc <- imp_df_46_PF_Dwc %>%
  mutate(color = ifelse(mk_tau > 0, "Positive", "Negative"))

#显示数据结果：
imp_df_46_PF_Dwc_selected <- imp_df_46_PF_Dwc %>%
  select(Climate_Class, Forest_Type, Variable, mk_tau, mk_p, stars) %>%
  mutate(mk_tau = formatC(mk_tau, format = "f", digits = 2))
print(imp_df_46_PF_Dwc_selected)
# Climate_Class Forest_Type Variable mk_tau      mk_p stars
# 1            Dwc           3      CDD  -0.13 0.6534221      
# 2            Dwc           3      CWD  -0.27 0.3232363      
# 3            Dwc           3      DTR  -0.16 0.5915051      
# 4            Dwc           3 Duration   0.07 0.8580277      
# 5            Dwc           3  Rainday  -0.33 0.2104977      
# 6            Dwc           3   Rx1day   0.36 0.1779674      
# 7            Dwc           3     SDII   0.25 0.3672323      
# 8            Dwc           3    TNlt2  -0.16 0.5915051      
# 9            Dwc           3      TNn   0.11 0.7183478      
# 10           Dwc           3   TXge30   0.00 1.0000000      
# 11           Dwc           3      TXx  -0.20 0.4742743  

# PF-phe46-Dwc绘制趋势条形图
p_phe46_Dwc <- ggplot(imp_df_46_PF_Dwc,aes(x = reorder(Variable, mk_tau),y = mk_tau, fill = color)) +
  geom_col(width = 0.8) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.6, linetype = "dashed") +  # 添加 x=0 的竖线
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#fc8d59", "Negative" = "lightblue")) +
  scale_y_continuous(limits = c(-0.8, 0.8)) +
  labs(title = "Dwc",
       x = "",
       y = "Trend") +
  theme_classic() + # 使用classic主题作为基础
  
  geom_text(aes(label = stars),
            vjust = 0.7,  # 往下移动星号（0.5 为正中，>0.5 往下）
            hjust = ifelse(imp_df_46_PF_Dwc$mk_tau >= 0, -0.1, 1.3),  # 斜率为正则星号放在右侧，否则左侧
            size = 7) +
  theme(
    axis.text.y = element_text(size = 14), # 变量名加粗 #color = "grey30",face = "bold"
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 5)),
    plot.title = element_text(size = 16, hjust = 0.5, color = "grey20"), #face = "bold",
    legend.position = "none",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    panel.background = element_blank(),
    # panel.grid = element_blank()         # 网格线（全部移除）
    panel.grid.major.y = element_line(color = "grey90",linetype = "solid",linewidth = 0.1), # x轴对应的网格线（翻转后为垂直）
    panel.grid.minor.y = element_blank(),  # 移除次要网格线
    panel.grid.major.x = element_blank(),  # 移除y轴网格线（翻转后为水平）
    panel.grid.minor.x = element_blank()) + #    
  coord_flip()

# 显示图形
print(p_phe46_Dwc)


####################################### 7 Combine Plots #####  #########################################


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
  filename = "./0.figure/03 Paper/Fig.15-a.P_10yr_Importance_EOS_plot.tiff",
  plot = combined_plot1, width = 14, height = 3.5, units = "in", dpi = 300)

# 保存第二行图像
ggsave(
  filename = "./0.figure/03 Paper/Fig.15-b.P_10yr_Importance_EOS_plot.tiff",
  plot = combined_plot2, width = 14, height = 3.5, units = "in", dpi = 300)

# 保存第三行图像
ggsave(
  filename = "./0.figure/03 Paper/Fig.15-c.P_10yr_Importance_EOS_plot.tiff",
  plot = combined_plot3, width = 14, height = 3.5, units = "in", dpi = 300)

