
library(dplyr)
library(tidyr)
library(ggpubr)
library(ggplot2)


setwd("D:/Graduation_Thesis")

###########################  重要性时间序列作图 Fig.10  ######################

importance_data <- read.csv("./EA+NA_Results/merged_Indices_Importance_result/GDO_10years_Indices_Importance_Results.csv")
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
head(imp_df_13);# str(imp_df_13)
head(imp_df_34);# str(imp_df_34)


####################################### 00  对所有气候区、森林类型的重要性趋势分析  #########################################
library(dplyr)
library(trend)
library(Kendall)  #MK 趋势分析的P值与 Sen's trend计算的p值一致

##  年份从小到大排序后进行趋势分析
# SOS--GMO
trend_result_13 <- imp_df_13 %>%
  arrange(Year) %>%
  group_by(Climate_Class, Forest_Type, Variable) %>%
  summarise(
    res = list({
      x <- na.omit(IncMSE)
      
      if (length(x) < 6 || length(unique(x)) == 1) {
        # 数据太短或常数，返回默认值
        c(sen_slope = 0, sen_p = 1, mk_tau = 0, mk_p = 1)
      } else {
        # 正常分析，使用 tryCatch 保底
        sen <- tryCatch(trend::sens.slope(x), error = function(e) NULL)
        mk <- tryCatch(Kendall::MannKendall(x), error = function(e) NULL)
        
        if (is.null(sen) || is.null(mk)) {
          c(sen_slope = NA, sen_p = NA, mk_tau = NA, mk_p = NA)
        } else {
          c(sen_slope = sen$estimates,
            sen_p = sen$p.value,
            mk_tau = mk$tau,
            mk_p = mk$sl)
        }
      }
    }),
    .groups = "drop"
  ) %>%
  tidyr::unnest_wider(res)

# 添加显著性符号
trend_result_13 <- trend_result_13 %>%
  mutate(
    sen_p = as.numeric(sen_p),
    stars = case_when(
      sen_p <= 0.001 ~ "***",
      sen_p <= 0.01  ~ "**",
      sen_p <= 0.05  ~ "*",
      TRUE ~ ""
    )
  )

# 导出结果
write.csv(trend_result_13, "./EA+NA_Results/merged_Indices_Importance_result/GDO_10yrTrend_of_Indices_Importance_13.csv", row.names = FALSE)


# GMO--GDO
trend_result_34 <- imp_df_34 %>%
  arrange(Year) %>%
  group_by(Climate_Class, Forest_Type, Variable) %>%
  summarise(
    res = list({
      x <- na.omit(IncMSE)
      
      if (length(x) < 6 || length(unique(x)) == 1) {
        # 数据不足或是常数，返回默认值
        c(sen_slope = 0, sen_p = 1, mk_tau = 0, mk_p = 1)
      } else {
        sen <- tryCatch(trend::sens.slope(x), error = function(e) NULL)
        mk  <- tryCatch(Kendall::MannKendall(x), error = function(e) NULL)
        
        if (is.null(sen) || is.null(mk)) {
          c(sen_slope = NA, sen_p = NA, mk_tau = NA, mk_p = NA)
        } else {
          c(sen_slope = sen$estimates,
            sen_p = sen$p.value,
            mk_tau = mk$tau,
            mk_p = mk$sl)
        }
      }
    }),
    .groups = "drop"
  ) %>%
  tidyr::unnest_wider(res)

# 添加显著性符号
trend_result_34 <- trend_result_34 %>%
  mutate(
    sen_p = as.numeric(sen_p),
    stars = case_when(
      sen_p <= 0.001 ~ "***",
      sen_p <= 0.01  ~ "**",
      sen_p <= 0.05  ~ "*",
      TRUE ~ ""
    )
  )

# 导出结果
write.csv(trend_result_34, "./EA+NA_Results/merged_Indices_Importance_result/GDO_10yrTrend_of_Indices_Importance_34.csv",row.names = FALSE)



###################################  00.筛选原生林 #####################################

imp_df_13 <- read.csv("./EA+NA_Results/merged_Indices_Importance_result/GDO_10yrTrend_of_Indices_Importance_13.csv", stringsAsFactors = FALSE)
range(imp_df_13$mk_tau, na.rm = TRUE)  # [1] -0.6  0.64

## 原生林-SOS-GMO的指标趋势结果
imp_df_13_PF <- imp_df_13 %>%
  filter(Forest_Type == 2)

imp_df_34 <- read.csv("./EA+NA_Results/merged_Indices_Importance_result/GDO_10yrTrend_of_Indices_Importance_34.csv", stringsAsFactors = FALSE)
range(imp_df_34$mk_tau, na.rm = TRUE)  # [1] -0.6  0.67

## 原生林-GMO-GDO的指标趋势结果
imp_df_34_PF <- imp_df_34 %>%
  filter(Forest_Type == 2)

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
# 1            Cfa           2      CDD  -0.18 0.52959883      
# 2            Cfa           2      CWD   0.54 0.03887939     *
# 3            Cfa           2      DTR   0.33 0.21049762      
# 4            Cfa           2 Duration  -0.16 0.59150505      
# 5            Cfa           2  Rainday   0.16 0.59150505      
# 6            Cfa           2   Rx1day  -0.18 0.52959883      
# 7            Cfa           2     SDII  -0.02 1.00000000      
# 8            Cfa           2    TNlt2   0.47 0.07363832      
# 9            Cfa           2      TNn   0.64 0.01226604     *
# 10           Cfa           2   TXge30   0.54 0.03887939     *
# 11           Cfa           2      TXx  -0.11 0.72051477      

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
# Climate_Class Forest_Type Variable mk_tau      mk_p stars
# 1            Cfa           2      CDD  -0.07 0.8580276      
# 2            Cfa           2      CWD  -0.11 0.7205148      
# 3            Cfa           2      DTR   0.02 1.0000000      
# 4            Cfa           2 Duration  -0.20 0.4742743      
# 5            Cfa           2  Rainday  -0.24 0.3710933      
# 6            Cfa           2   Rx1day   0.20 0.4742744      
# 7            Cfa           2     SDII  -0.11 0.7205148      
# 8            Cfa           2    TNlt2   0.00 1.0000000      
# 9            Cfa           2      TNn  -0.24 0.3710933      
# 10           Cfa           2   TXge30   0.33 0.2104976      
# 11           Cfa           2      TXx   0.38 0.1524062  

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
# 1            Cfb           2      CDD   0.16 0.5915051      
# 2            Cfb           2      CWD   0.16 0.5915051      
# 3            Cfb           2      DTR  -0.27 0.3232363      
# 4            Cfb           2 Duration   0.24 0.3710933      
# 5            Cfb           2  Rainday   0.11 0.7205148      
# 6            Cfb           2   Rx1day   0.02 1.0000000      
# 7            Cfb           2     SDII  -0.07 0.8580276      
# 8            Cfb           2    TNlt2   0.11 0.7205148      
# 9            Cfb           2      TNn  -0.07 0.8580276      
# 10           Cfb           2   TXge30   0.37 0.2206714      
# 11           Cfb           2      TXx   0.42 0.1074047  

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
# 1            Cfb           2      CDD   0.16 0.5915051      
# 2            Cfb           2      CWD  -0.02 1.0000000      
# 3            Cfb           2      DTR  -0.16 0.5915051      
# 4            Cfb           2 Duration  -0.16 0.5915051      
# 5            Cfb           2  Rainday  -0.36 0.1779674      
# 6            Cfb           2   Rx1day   0.20 0.4742744      
# 7            Cfb           2     SDII   0.07 0.8580277      
# 8            Cfb           2    TNlt2   0.33 0.2632234      
# 9            Cfb           2      TNn   0.02 1.0000000      
# 10           Cfb           2   TXge30   0.02 1.0000000      
# 11           Cfb           2      TXx  -0.33 0.2104977 

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
# 1            Dfb           2      CDD  -0.02 1.00000000      
# 2            Dfb           2      CWD  -0.33 0.21049768      
# 3            Dfb           2      DTR   0.47 0.07363832      
# 4            Dfb           2 Duration   0.11 0.72051477      
# 5            Dfb           2  Rainday   0.07 0.85802770      
# 6            Dfb           2   Rx1day  -0.16 0.59150505      
# 7            Dfb           2     SDII   0.07 0.85802770      
# 8            Dfb           2    TNlt2  -0.20 0.47427434      
# 9            Dfb           2      TNn   0.07 0.85802770      
# 10           Dfb           2   TXge30   0.58 0.02476382     *
# 11           Dfb           2      TXx   0.07 0.85802770   

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
# 1            Dfb           2      CDD  -0.16 0.59150505      
# 2            Dfb           2      CWD   0.07 0.85802770      
# 3            Dfb           2      DTR  -0.11 0.72051477      
# 4            Dfb           2 Duration  -0.33 0.21049768      
# 5            Dfb           2  Rainday  -0.42 0.10740466      
# 6            Dfb           2   Rx1day  -0.16 0.59150505      
# 7            Dfb           2     SDII   0.11 0.72051477      
# 8            Dfb           2    TNlt2   0.49 0.05931437      
# 9            Dfb           2      TNn   0.38 0.15240622      
# 10           Dfb           2   TXge30   0.29 0.28313088      
# 11           Dfb           2      TXx   0.18 0.52959883   

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
# Climate_Class Forest_Type Variable mk_tau      mk_p stars
# 1            Dfc           2      CDD   0.07 0.8580277      
# 2            Dfc           2      CWD  -0.24 0.3710933      
# 3            Dfc           2      DTR   0.16 0.5915051      
# 4            Dfc           2 Duration  -0.20 0.4742743      
# 5            Dfc           2  Rainday  -0.07 0.8580276      
# 6            Dfc           2   Rx1day   0.20 0.4742744      
# 7            Dfc           2     SDII   0.11 0.7205148      
# 8            Dfc           2    TNlt2  -0.07 0.8580276      
# 9            Dfc           2      TNn  -0.07 0.8580276      
# 10           Dfc           2   TXge30  -0.07 0.8580276      
# 11           Dfc           2      TXx   0.07 0.8580277    

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
# 1            Dfc           2      CDD   0.38 0.15240622      
# 2            Dfc           2      CWD  -0.51 0.04909798     *
# 3            Dfc           2      DTR   0.16 0.59150505      
# 4            Dfc           2 Duration   0.29 0.28313088      
# 5            Dfc           2  Rainday  -0.11 0.72051477      
# 6            Dfc           2   Rx1day  -0.20 0.47427434      
# 7            Dfc           2     SDII   0.04 0.92844403      
# 8            Dfc           2    TNlt2   0.22 0.41896176      
# 9            Dfc           2      TNn   0.11 0.72051477      
# 10           Dfc           2   TXge30   0.20 0.47427440      
# 11           Dfc           2      TXx   0.07 0.85802770     

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
# Climate_Class Forest_Type Variable mk_tau       mk_p stars
# 1            Dwb           2      CDD   0.29 0.28313088      
# 2            Dwb           2      CWD   0.16 0.59150505      
# 3            Dwb           2      DTR  -0.04 0.92844403      
# 4            Dwb           2 Duration  -0.29 0.28313082      
# 5            Dwb           2  Rainday  -0.02 1.00000000      
# 6            Dwb           2   Rx1day  -0.11 0.72051477      
# 7            Dwb           2     SDII   0.51 0.04909801     *
# 8            Dwb           2    TNlt2  -0.29 0.28313082      
# 9            Dwb           2      TNn   0.02 1.00000000      
# 10           Dwb           2   TXge30  -0.52 0.04729228     *
# 11           Dwb           2      TXx  -0.20 0.47427434 

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
# Climate_Class Forest_Type Variable mk_tau        mk_p stars
# 1            Dwb           2      CDD   0.16 0.591505051      
# 2            Dwb           2      CWD  -0.02 1.000000000      
# 3            Dwb           2      DTR  -0.22 0.418961763      
# 4            Dwb           2 Duration   0.11 0.720514774      
# 5            Dwb           2  Rainday   0.11 0.720514774      
# 6            Dwb           2   Rx1day  -0.07 0.858027637      
# 7            Dwb           2     SDII  -0.11 0.720514774      
# 8            Dwb           2    TNlt2   0.00 1.000000000      
# 9            Dwb           2      TNn   0.24 0.371093273      
# 10           Dwb           2   TXge30   0.67 0.009206891    **
# 11           Dwb           2      TXx   0.56 0.031823158     *   

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
# Climate_Class Forest_Type Variable mk_tau       mk_p stars
# 1            Dwc           2      CDD   0.29 0.28313088      
# 2            Dwc           2      CWD   0.16 0.59150505      
# 3            Dwc           2      DTR   0.38 0.15240622      
# 4            Dwc           2 Duration  -0.11 0.72051477      
# 5            Dwc           2  Rainday  -0.07 0.85802764      
# 6            Dwc           2   Rx1day  -0.07 0.85802764      
# 7            Dwc           2     SDII  -0.04 0.92844403      
# 8            Dwc           2    TNlt2  -0.56 0.03182312     *
# 9            Dwc           2      TNn  -0.24 0.37109333      
# 10           Dwc           2   TXge30  -0.22 0.47648537      
# 11           Dwc           2      TXx   0.11 0.72051477       

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
# 1            Dwc           2      CDD   0.42 0.1074047      
# 2            Dwc           2      CWD   0.02 1.0000000      
# 3            Dwc           2      DTR   0.02 1.0000000      
# 4            Dwc           2 Duration   0.07 0.8580277      
# 5            Dwc           2  Rainday  -0.02 1.0000000      
# 6            Dwc           2   Rx1day   0.20 0.4742744      
# 7            Dwc           2     SDII  -0.11 0.7205148      
# 8            Dwc           2    TNlt2  -0.04 0.9284440      
# 9            Dwc           2      TNn   0.29 0.2831309      
# 10           Dwc           2   TXge30   0.29 0.2831309      
# 11           Dwc           2      TXx  -0.24 0.3710933    

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
  filename = "./0.figure/03 Paper/Fig.10-a.PF_10yr_Importance_GDO_plot.tiff",
  plot = combined_plot1, width = 14, height = 3.5, units = "in", dpi = 300)

# 保存第二行图像
ggsave(
  filename = "./0.figure/03 Paper/Fig.10-b.PF_10yr_Importance_GDO_plot.tiff",
  plot = combined_plot2, width = 14, height = 3.5, units = "in", dpi = 300)

