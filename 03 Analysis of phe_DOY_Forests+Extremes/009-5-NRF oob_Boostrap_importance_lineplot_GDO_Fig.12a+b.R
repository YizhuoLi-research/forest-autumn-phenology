
library(dplyr)
library(tidyr)
library(ggpubr)
library(ggplot2)


setwd("D:/Graduation_Thesis")

###########################  重要性时间序列作图 Fig.12  ######################

###################################  00.筛选再生林 #####################################

imp_df_13 <- read.csv("./EA+NA_Results/merged_Indices_Importance_result/GDO_10yrTrend_of_Indices_Importance_13.csv", stringsAsFactors = FALSE)
range(imp_df_13$mk_tau, na.rm = TRUE)  # [1] -0.6  0.64

## 原生林-SOS-GMO的指标趋势结果
imp_df_13_PF <- imp_df_13 %>%
  filter(Forest_Type == 1)

imp_df_34 <- read.csv("./EA+NA_Results/merged_Indices_Importance_result/GDO_10yrTrend_of_Indices_Importance_34.csv", stringsAsFactors = FALSE)
range(imp_df_34$mk_tau, na.rm = TRUE)  # [1] -0.6  0.67

## 原生林-GMO-GDO的指标趋势结果
imp_df_34_PF <- imp_df_34 %>%
  filter(Forest_Type == 1)

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
# Climate_Class Forest_Type Variable mk_tau      mk_p stars
# 1            Cfa           1      CDD  -0.11 0.7205148      
# 2            Cfa           1      CWD   0.00 1.0000000      
# 3            Cfa           1      DTR  -0.33 0.2104977      
# 4            Cfa           1 Duration  -0.07 0.8580276      
# 5            Cfa           1  Rainday   0.07 0.8580277      
# 6            Cfa           1   Rx1day  -0.27 0.3232363      
# 7            Cfa           1     SDII  -0.16 0.5915051      
# 8            Cfa           1    TNlt2   0.38 0.1524062      
# 9            Cfa           1      TNn   0.38 0.1524062      
# 10           Cfa           1   TXge30   0.11 0.7205148      
# 11           Cfa           1      TXx  -0.24 0.3710933   

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
# 1            Cfa           1      CDD   0.02 1.0000000      
# 2            Cfa           1      CWD  -0.20 0.4742743      
# 3            Cfa           1      DTR  -0.24 0.3710933      
# 4            Cfa           1 Duration   0.16 0.5915051      
# 5            Cfa           1  Rainday   0.20 0.4742744      
# 6            Cfa           1   Rx1day  -0.38 0.1524063      
# 7            Cfa           1     SDII  -0.42 0.1074047      
# 8            Cfa           1    TNlt2   0.15 0.6952986      
# 9            Cfa           1      TNn  -0.20 0.4742743      
# 10           Cfa           1   TXge30  -0.20 0.4742743      
# 11           Cfa           1      TXx  -0.24 0.3710933 

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
# Climate_Class Forest_Type Variable mk_tau      mk_p stars
# 1            Cfb           1      CDD  -0.29 0.2831308      
# 2            Cfb           1      CWD  -0.29 0.2831308      
# 3            Cfb           1      DTR  -0.33 0.2104977      
# 4            Cfb           1 Duration  -0.29 0.2831308      
# 5            Cfb           1  Rainday  -0.07 0.8580276      
# 6            Cfb           1   Rx1day  -0.11 0.7205148      
# 7            Cfb           1     SDII  -0.07 0.8580276      
# 8            Cfb           1    TNlt2  -0.11 0.7205148      
# 9            Cfb           1      TNn  -0.02 1.0000000      
# 10           Cfb           1   TXge30   0.40 0.1446837      
# 11           Cfb           1      TXx  -0.38 0.1524063 

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
#    Climate_Class Forest_Type Variable mk_tau       mk_p stars
# 1            Cfb           1      CDD   0.11 0.72051477      
# 2            Cfb           1      CWD   0.38 0.15240622      
# 3            Cfb           1      DTR   0.20 0.47427440      
# 4            Cfb           1 Duration   0.29 0.28313088      
# 5            Cfb           1  Rainday  -0.02 1.00000000      
# 6            Cfb           1   Rx1day  -0.07 0.85802764      
# 7            Cfb           1     SDII  -0.11 0.72051477      
# 8            Cfb           1    TNlt2  -0.11 0.76934189      
# 9            Cfb           1      TNn   0.11 0.72051477      
# 10           Cfb           1   TXge30   0.51 0.04909801     *
# 11           Cfb           1      TXx   0.29 0.28313088

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
# Climate_Class Forest_Type Variable mk_tau       mk_p stars
# 1            Dfb           1      CDD  -0.07 0.85802764      
# 2            Dfb           1      CWD  -0.38 0.15240626      
# 3            Dfb           1      DTR   0.42 0.10740471      
# 4            Dfb           1 Duration  -0.11 0.72051477      
# 5            Dfb           1  Rainday  -0.47 0.07363828      
# 6            Dfb           1   Rx1day   0.16 0.59150505      
# 7            Dfb           1     SDII  -0.07 0.85802764      
# 8            Dfb           1    TNlt2  -0.02 1.00000000      
# 9            Dfb           1      TNn   0.51 0.04909801     *
# 10           Dfb           1   TXge30   0.51 0.04909801     *
# 11           Dfb           1      TXx   0.24 0.37109327 

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
# Climate_Class Forest_Type Variable mk_tau       mk_p stars
# 1            Dfb           1      CDD  -0.42 0.10740466      
# 2            Dfb           1      CWD  -0.24 0.37109333      
# 3            Dfb           1      DTR  -0.07 0.85802764      
# 4            Dfb           1 Duration  -0.04 0.92844403      
# 5            Dfb           1  Rainday   0.07 0.85802770      
# 6            Dfb           1   Rx1day  -0.20 0.47427434      
# 7            Dfb           1     SDII  -0.24 0.37109333      
# 8            Dfb           1    TNlt2   0.47 0.09229863      
# 9            Dfb           1      TNn   0.20 0.47427440      
# 10           Dfb           1   TXge30   0.11 0.72051477      
# 11           Dfb           1      TXx   0.02 1.00000000    

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
# 1            Dfc           1      CDD  -0.42 0.10740466      
# 2            Dfc           1      CWD  -0.09 0.78761578      
# 3            Dfc           1      DTR   0.16 0.59150505      
# 4            Dfc           1 Duration  -0.11 0.72051477      
# 5            Dfc           1  Rainday   0.13 0.65342212      
# 6            Dfc           1   Rx1day   0.24 0.37109327      
# 7            Dfc           1     SDII   0.07 0.85802770      
# 8            Dfc           1    TNlt2  -0.33 0.21049768      
# 9            Dfc           1      TNn   0.02 1.00000000      
# 10           Dfc           1   TXge30  -0.45 0.08796146      
# 11           Dfc           1      TXx  -0.29 0.28313082 

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
# Climate_Class Forest_Type Variable mk_tau      mk_p stars
# 1            Dfc           1      CDD   0.16 0.5915051      
# 2            Dfc           1      CWD  -0.38 0.1524063      
# 3            Dfc           1      DTR   0.20 0.4742744      
# 4            Dfc           1 Duration   0.38 0.1524062      
# 5            Dfc           1  Rainday   0.33 0.2104976      
# 6            Dfc           1   Rx1day   0.02 1.0000000      
# 7            Dfc           1     SDII   0.20 0.4742744      
# 8            Dfc           1    TNlt2   0.22 0.4189618      
# 9            Dfc           1      TNn   0.24 0.3710933      
# 10           Dfc           1   TXge30   0.09 0.7876158      
# 11           Dfc           1      TXx  -0.02 1.0000000

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
# 1            Dwb           1      CDD   0.00 1.0000000      
# 2            Dwb           1      CWD   0.11 0.7205148      
# 3            Dwb           1      DTR  -0.07 0.8580276      
# 4            Dwb           1 Duration  -0.02 1.0000000      
# 5            Dwb           1  Rainday   0.16 0.5915051      
# 6            Dwb           1   Rx1day  -0.11 0.7205148      
# 7            Dwb           1     SDII  -0.07 0.8580276      
# 8            Dwb           1    TNlt2  -0.11 0.7205148      
# 9            Dwb           1      TNn   0.02 1.0000000      
# 10           Dwb           1   TXge30  -0.18 0.5295988      
# 11           Dwb           1      TXx   0.02 1.0000000

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
# Climate_Class Forest_Type Variable mk_tau       mk_p stars
# 1            Dwb           1      CDD   0.20 0.47427440      
# 2            Dwb           1      CWD   0.02 1.00000000      
# 3            Dwb           1      DTR   0.02 1.00000000      
# 4            Dwb           1 Duration   0.24 0.37109327      
# 5            Dwb           1  Rainday   0.42 0.10740471      
# 6            Dwb           1   Rx1day  -0.24 0.37109333      
# 7            Dwb           1     SDII  -0.07 0.85802764      
# 8            Dwb           1    TNlt2   0.00 1.00000000      
# 9            Dwb           1      TNn   0.51 0.04909801     *
# 10           Dwb           1   TXge30   0.42 0.10740471      
# 11           Dwb           1      TXx   0.20 0.47427440

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
# Climate_Class Forest_Type Variable mk_tau      mk_p stars
# 1            Dwc           1      CDD  -0.11 0.7205148      
# 2            Dwc           1      CWD   0.29 0.2831309      
# 3            Dwc           1      DTR   0.20 0.4742744      
# 4            Dwc           1 Duration   0.38 0.1524062      
# 5            Dwc           1  Rainday   0.24 0.3710933      
# 6            Dwc           1   Rx1day  -0.07 0.8580276      
# 7            Dwc           1     SDII  -0.40 0.1268493      
# 8            Dwc           1    TNlt2  -0.02 1.0000000      
# 9            Dwc           1      TNn  -0.20 0.4742743      
# 10           Dwc           1   TXge30  -0.29 0.3057069      
# 11           Dwc           1      TXx   0.42 0.1074047 

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
Climate_Class Forest_Type Variable mk_tau      mk_p stars
# 1            Dwc           1      CDD  -0.16 0.5915051      
# 2            Dwc           1      CWD   0.16 0.5915051      
# 3            Dwc           1      DTR   0.20 0.4742744      
# 4            Dwc           1 Duration  -0.02 1.0000000      
# 5            Dwc           1  Rainday   0.02 1.0000000      
# 6            Dwc           1   Rx1day  -0.33 0.2104977      
# 7            Dwc           1     SDII  -0.29 0.2831308      
# 8            Dwc           1    TNlt2  -0.13 0.6534221      
# 9            Dwc           1      TNn   0.16 0.5915051      
# 10           Dwc           1   TXge30  -0.18 0.5295988      
# 11           Dwc           1      TXx  -0.11 0.7205148

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



####################################### 7 Combine Plots #####  #########################################


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
  filename = "./0.figure/03 Paper/Fig.12-a.NRF_10yr_Importance_GDO_plot.tiff",
  plot = combined_plot1, width = 14, height = 3.5, units = "in", dpi = 300)

# 保存第二行图像
ggsave(
  filename = "./0.figure/03 Paper/Fig.12-b.NRF_10yr_Importance_GDO_plot.tiff",
  plot = combined_plot2, width = 14, height = 3.5, units = "in", dpi = 300)

