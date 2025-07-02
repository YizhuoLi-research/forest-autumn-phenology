rm(list = ls())

######################################   00 加载包  ##################################################
library(terra)
library(tidyverse)
library(raster)
library(ggplot2)
library(ppcor)
library(extrafont)  #字体
font_import()  # 导入系统字体
loadfonts()    # 加载字体
# library(ggpubr)

setwd("D:/Graduation_Thesis")

#############################   01.按照柯本气候类型，将气候区分为6个气候区域   #######################
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

#划定Climate_Class_list中每个类型区域的边界
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

# print(classify_border)   # 查看 classify_border 的整体结构
# summary(classify_border) # 确认 classify_border 是否有几何和属性数据

Climate_Class_list <- list(Cfa,Cfb,Dfb,Dfc,Dwb,Dwc)


#############################   02.按照气候类型类型，计算相关性前做好调取   ####################
#############################   1---再生林 2---原生林 3---人工林


## 1. 读取数据
# 列出文件夹中的所有文件
r2 <-  rast("./EA+NA_Results/EA+NA_GFC_30km.tif")
forest_types <- c(1, 2, 3)  # 1: 自然再生林； 2：原生林；   3：人工林


##################  03 按气候-森林类型 取像元计算偏相关关系（Pearson相关性）计算2--GDO/3--EOS个气候区间  ###########################


# 文件路径和标签列表
# base_dir1 <- "./EA+NA_Results/merged_Clim_Duration_Extremes_years/phe1_3/"
# base_dir2 <- "./EA+NA_Results/merged_Clim_Duration_Extremes_years/phe3_4/"
base_dir <- c(
  "./EA+NA_Results/merged_Clim_Duration_Extremes_years/phe1_3/",
  "./EA+NA_Results/merged_Clim_Duration_Extremes_years/phe3_4/")

phe_GDO_dir <- "./EA+NA_Results/merged_Phe_DOY_years_2_PHE_analysis/merged_phe4_DOY/"

# 定义需要匹配的前缀
prefixes <- c("TXx_phe1_3", "TNn_phe1_3", "DTR_phe1_3", "TXge30_phe1_3", "TNlt2_phe1_3",
              "RainDay_phe1_3", "CDD_phe1_3", "CWD_phe1_3", "Rx1day_phe1_3", "SDII_phe1_3", 
              "merged_Duration_phe1_3",
              "TXx_phe3_4", "TNn_phe3_4", "DTR_phe3_4", "TXge30_phe3_4", "TNlt2_phe3_4",
              "RainDay_phe3_4", "CDD_phe3_4", "CWD_phe3_4", "Rx1day_phe3_4", "SDII_phe3_4", 
              "merged_Duration_phe3_4")

# 读取所有匹配文件，并按变量类型组织取对应指标名称下的10 （年）个文件
file_paths <- unlist(lapply(prefixes, function(prefix) {
  list.files(base_dir, pattern = paste0("^", prefix, ".*\\.tif$"), 
             full.names = TRUE, ignore.case = TRUE) %>%
    sort() %>%
    head(10)  # 取前 10 个文件
}))

# 读取 phe_GDO_DOY 目录下的所有文件
phe_GDO_files <- list.files(phe_GDO_dir, full.names = TRUE)

# 预定义标签
labels <- prefixes


# 筛选文件 # 10年phe1_3 extremes的 SpatRaster
TXx_phe1_3_files <- file_paths[grepl("^TXx_phe1_3", basename(file_paths))]
TNn_phe1_3_files <- file_paths[grepl("^TNn_phe1_3", basename(file_paths))]
DTR_phe1_3_files <- file_paths[grepl("^DTR_phe1_3", basename(file_paths))]
TXge30_phe1_3_files <- file_paths[grepl("^TXge30_phe1_3", basename(file_paths))]
TNlt2_phe1_3_files <- file_paths[grepl("^TNlt2_phe1_3", basename(file_paths))]
RainDay_phe1_3_files <- file_paths[grepl("^RainDay_phe1_3", basename(file_paths))]
CDD_phe1_3_files <- file_paths[grepl("^CDD_phe1_3", basename(file_paths))]
CWD_phe1_3_files <- file_paths[grepl("^CWD_phe1_3", basename(file_paths))]
Rx1day_phe1_3_files <- file_paths[grepl("^Rx1day_phe1_3", basename(file_paths))]
SDII_phe1_3_files <- file_paths[grepl("^SDII_phe1_3", basename(file_paths))]
merged_Duration_phe1_3_files <- file_paths[grepl("^merged_Duration_phe1_3", basename(file_paths))]


# 筛选文件 # 10年phe3_4 extremes的 SpatRaster
TXx_phe3_4_files <- file_paths[grepl("^TXx_phe3_4", basename(file_paths))]
TNn_phe3_4_files <- file_paths[grepl("^TNn_phe3_4", basename(file_paths))]
DTR_phe3_4_files <- file_paths[grepl("^DTR_phe3_4", basename(file_paths))]
TXge30_phe3_4_files <- file_paths[grepl("^TXge30_phe3_4", basename(file_paths))]
TNlt2_phe3_4_files <- file_paths[grepl("^TNlt2_phe3_4", basename(file_paths))]
RainDay_phe3_4_files <- file_paths[grepl("^RainDay_phe3_4", basename(file_paths))]
CDD_phe3_4_files <- file_paths[grepl("^CDD_phe3_4", basename(file_paths))]
CWD_phe3_4_files <- file_paths[grepl("^CWD_phe3_4", basename(file_paths))]
Rx1day_phe3_4_files <- file_paths[grepl("^Rx1day_phe3_4", basename(file_paths))]
SDII_phe3_4_files <- file_paths[grepl("^SDII_phe3_4", basename(file_paths))]
merged_Duration_phe3_4_files <- file_paths[grepl("^merged_Duration_phe3_4", basename(file_paths))]

#11*2组变量
file_groups <- list( TXx_1_3 = TXx_phe1_3_files, TNn_1_3 = TNn_phe1_3_files,  DTR_1_3 = DTR_phe1_3_files, TXge30_1_3 = TXge30_phe1_3_files,
                     TNlt2_1_3 = TNlt2_phe1_3_files, RainDay_1_3 = RainDay_phe1_3_files,CDD_1_3 = CDD_phe1_3_files, CWD_1_3 = CWD_phe1_3_files,
                     Rx1day_1_3 = Rx1day_phe1_3_files, SDII_1_3 = SDII_phe1_3_files, merged_Duration_1_3 = merged_Duration_phe1_3_files,
                     
                     TXx_3_4= TXx_phe3_4_files, TNn_3_4 = TNn_phe3_4_files,  DTR_3_4 = DTR_phe3_4_files, TXge30_3_4 = TXge30_phe3_4_files,
                     TNlt2_3_4 = TNlt2_phe3_4_files, RainDay_3_4 = RainDay_phe3_4_files,CDD_3_4 = CDD_phe3_4_files, CWD_3_4 = CWD_phe3_4_files,
                     Rx1day_3_4 = Rx1day_phe3_4_files, SDII_3_4 = SDII_phe3_4_files, merged_Duration_3_4 = merged_Duration_phe3_4_files)


analyze_correlation <- function(extremes_var, phe_GDO_DOY, classify_border, Climate_Class_list, forest_types, var_type, control_vars) {
  results <- data.frame(Variable = character(),
                        Climate_Class = character(),
                        Forest_Class = numeric(),
                        Partial_Correlation = numeric(),
                        p_value = numeric(),
                        Zero_Variance_Vars = character(),  # 新增列，记录被移除的方差为 0 的变量
                        stringsAsFactors = FALSE)
  
  for (Climate_Class in Climate_Class_list) {
    cat("Processing Climate Class:", Climate_Class, "\n")
    
    # Climate_Class = "Cfa"
    
    order_val <- match(Climate_Class, classify_border$EA_koppen_30km_addClimate)
    if (is.na(order_val)) next  # 跳过不存在的气候类型
    
    selected_border_val <- classify_border[order_val, ]
    
    # 对 该极端指标变量、phe变量 进行掩膜
    extremes_masked <- mask(crop(extremes_var, selected_border_val), selected_border_val)
    phe_masked      <- mask(crop(phe_GDO_DOY, selected_border_val), selected_border_val)
    
    # 处理森林类型
    forest_masked   <- mask(crop(r2, selected_border_val), selected_border_val)
    
    # 对 每个其他极端指标变量（即控制变量）进行掩膜
    control_vars_masked <- lapply(control_vars, function(ctrl_var) {
      mask(crop(ctrl_var, selected_border_val), selected_border_val)
    })
    
    for (forest in forest_types) {
      cat("Processing Forest Type:", forest, "\n")
      # forest = 1
      # 创建森林掩膜
      current_forest_mask <- ifel(forest_masked == forest, 1, NA)
      
      # 进行森林掩膜
      extremes_forest <- mask(extremes_masked, current_forest_mask)
      phe_forest      <- mask(phe_masked, current_forest_mask)
      
      # 控制变量同样进行森林掩膜**
      control_vars_forest <- lapply(control_vars_masked, function(ctrl_var) {
        mask(ctrl_var, current_forest_mask)
      })
      
      # 提取数据
      df1_val <- terra::extract(extremes_forest, selected_border_val)
      df2_val <- terra::extract(phe_forest, selected_border_val)
      
      control_vars_vals <- lapply(control_vars_forest, function(ctrl_var) {
        terra::extract(ctrl_var, selected_border_val)
      })
      
      if (is.null(df1_val) || is.null(df2_val)) next  # 确保数据存在
      
      df_val <- cbind(df1_val[, -1], df2_val[, -1])
      
      colnames(df_val) <- c(paste0("extremes_var", 2013:2022),
                            paste0("phe_GDO_DOY", 2013:2022))
      
      # 提取数值
      extremes_val <- unlist(df_val[, grep("extremes_var", colnames(df_val), value = TRUE)])
      phe_val      <- unlist(df_val[, grep("phe_GDO_DOY", colnames(df_val), value = TRUE)])
      
      # 处理控制变量数据并展开
      control_vals_df <- do.call(cbind, lapply(control_vars_vals, function(ctrl) {
        unlist(ctrl[, -1, drop = FALSE])  # 删除 ID 列，仅保留数值
      }))
      
      # **组合最终数据框**
      new_df_val <- data.frame(extremes_value = extremes_val, phe_value = phe_val, control_vals_df)
      new_df_val <- na.omit(new_df_val)  # 去除 NA 值
      
      # [1] 检查样本量
      # 确保样本数（行数）大于变量数（列数）+2 
      # 主要是为了 保证偏相关分析的统计有效性，避免自由度不足和矩阵求逆失败的问题
      if (nrow(new_df_val) < ncol(new_df_val) + 2) next
      # [2] 检查控制变量的方差
      variances <- apply(new_df_val[, -c(1, 2)], 2, var, na.rm = TRUE)
      zero_var_cols <- names(variances[variances == 0])
        # 如果 zero_var_cols 为空，设置为 NA 或空字符串
        if (length(zero_var_cols) == 0) {
        zero_var_cols <- NA}  # 或者 zero_var_cols <- ""}
        # 移除方差为 0 的控制变量
        if (length(zero_var_cols) > 0 && !is.na(zero_var_cols)) {
        cat("Removing control variables with zero variance:", zero_var_cols, "\n")
        new_df_val <- new_df_val[, !colnames(new_df_val) %in% zero_var_cols]}
      
      # 计算偏相关性
      partial_cor_test <- pcor.test(new_df_val$extremes_value, new_df_val$phe_value, new_df_val[, -c(1,2)])
      partial_cor_test$p.value <- formatC(partial_cor_test$p.value, format = "e", digits = 4) #科学计数法，字符型
      
      result_row <- data.frame(
        Variable = var_type,  
        Climate_Class = Climate_Class, 
        Forest_Class  = forest, 
        Partial_Correlation = partial_cor_test$estimate, 
        p_value = partial_cor_test$p.value,
          zero_var_cols = paste(zero_var_cols, collapse = ", "),  # 将 zero_var_cols 转换为字符串
          stringsAsFactors = FALSE
      )
      
      results <- rbind(results, result_row)
    }
  }
  
  return(results)
 }



perform_analysis <- function(file_groups) {
  list_of_results <- list()
  
  for (var_type in names(file_groups)) {  
    
    # var_type <- "TXx_1_3"
    
    cat("Processing:", var_type, "\n")
    
    file_list_extremes <- file_groups[[var_type]]  # 当前变量的文件
    other_file_groups <- file_groups[names(file_groups) != var_type]  # 除当前变量外的其他变量
    
    # 读取当前极端变量数据
    extremes_var <- rast(file_list_extremes)
    
    # 读取 phe_GDO_DOY 数据
    phe_GDO_DOY <- rast(phe_GDO_files)
    
    # 读取其他变量作为控制变量
    control_vars <- lapply(other_file_groups, function(file_list) rast(file_list)) 
    
    # 调用 analyze_correlation
    list_of_results[[var_type]] <- analyze_correlation(
      extremes_var, 
      phe_GDO_DOY, 
      classify_border, 
      Climate_Class_list, 
      forest_types, 
      var_type, 
      control_vars
    )
    
    cat("Results for", var_type, "computed.\n")
  }
  
  return(list_of_results)
}


results_all <- perform_analysis(file_groups)
results_df <- do.call(rbind, results_all)

results_df$Partial_Cor_value <- round(as.numeric(results_df$Partial_Correlation), 2)
results_df$p_value <- as.numeric(results_df$p_value)    #原科学计数法-字符型转换为数值型
results_df$stars <- ifelse(results_df$p_value <= 0.001, "***",
                           ifelse(results_df$p_value <= 0.01, "**",
                                  ifelse(results_df$p_value <= 0.05, "*",
                                         sprintf("%.2f", results_df$p_value))))

write.csv(results_df, "./0.figure/03 Paper/01Partial_Cor_22var_to_DOY.csv", row.names = FALSE)




##################  04 按气候-森林类型 取像元计算皮尔森（Pearson相关性）计算2--GDO/3--EOS个气候区间  ###########################

results_df <- read.csv("./0.figure/03 Paper/01Partial_Cor_22var_to_DOY.csv", stringsAsFactors = FALSE)

library(dplyr)
library(ggplot2)

# 1.添加 forest_name 列
results_df <- results_df %>%
  mutate(forest_name = case_when(
    Forest_Class == 1 ~ "NRF",
    Forest_Class == 2 ~ "PF",
    Forest_Class == 3 ~ "P",
    TRUE ~ NA_character_  # 如果 Forest_Class 不是 1、2、3，设置为 NA
  ))

# 2.创建纵坐标标签
# results_df$Lab <- paste(results_df$Climate_Class, results_df$forest_name, "GDO", sep = " + ")
results_df$Lab <- paste(results_df$Climate_Class, results_df$forest_name, sep = ", ")
# 变量名称重命名
results_df$Variable <- gsub("^merged_Duration", "Duration", results_df$Variable, 
                            ignore.case = TRUE)  #不区分大小写

# 3.创建显示文本标签：相关性值和显著性标记
results_df$Text <- ifelse(
  is.na(results_df$Partial_Correlation),  # 如果相关性值是 NA
  "",  # 不显示任何内容
  ifelse(
    results_df$stars %in% c("***", "**", "*"),  # 如果显著性标记是 ***、** 或 *
    paste(
      sprintf("%.2f", results_df$Partial_Correlation),  # 相关性值，保留两位小数
      "\n",  # 换行
      results_df$stars,  # 显著性标记
      sep = ""
    ),
    paste(
      sprintf("%.2f", results_df$Partial_Correlation),  # 相关性值，保留两位小数
      "\n ",  # 换行并用空格占位
      sep = ""
    )
  )
)

# 定义气候类型、森林类型的顺序
climate_order <- c("Cfa", "Cfb", "Dfb", "Dfc", "Dwb", "Dwc")
forest_order <- c("PF", "NRF", "P")

# 将 Climate_Class 和 forest_name 转换为因子，并指定顺序
results_df$Climate_Class <- factor(results_df$Climate_Class, levels = climate_order)
results_df$forest_name <- factor(results_df$forest_name, levels = forest_order)

# 按照 Climate_Class 和 forest_name 排序
results_df <- results_df %>%
  arrange(Climate_Class, forest_name)

# 将 Lab 转换为因子，以保持排序后的顺序（不对，要反置）
# results_df$Lab <- factor(results_df$Lab, levels = unique(results_df$Lab))
results_df$Lab <- factor(results_df$Lab, levels = rev(unique(results_df$Lab)))

# 4.按阶段拆分result_df
# 按 Variable 列的结尾筛选出不同的子数据框
result_df_1_3 <- results_df %>% filter(grepl("_1_3$", Variable))
result_df_3_4 <- results_df %>% filter(grepl("_3_4$", Variable))

# 去掉 Variable 列中 "_1_3" 和 "_3_4" 部分，仅保留变量名称
result_df_1_3 <- result_df_1_3 %>% mutate(Variable = sub("_1_3$", "", Variable))
result_df_3_4 <- result_df_3_4 %>% mutate(Variable = sub("_3_4$", "", Variable))

# 将 RainDay 改为 Rainday
result_df_1_3$Variable <- gsub("RainDay", "Rainday", result_df_1_3$Variable)
result_df_3_4$Variable <- gsub("RainDay", "Rainday", result_df_3_4$Variable)

# 设定纵坐标 (Climate_Class & Forest_Class) 的顺序
result_df_1_3$Variable <- factor(result_df_1_3$Variable, levels = c("TXx", "TXge30", "TNn", "TNlt2", "DTR", "Rainday", "CWD", "CDD", "Rx1day", "SDII", "Duration"))
result_df_3_4$Variable <- factor(result_df_3_4$Variable, levels = c("TXx", "TXge30", "TNn", "TNlt2", "DTR", "Rainday", "CWD", "CDD", "Rx1day", "SDII", "Duration"))

# # 查看结果
head(result_df_1_3)
head(result_df_3_4)



######## 绘制相关性图1(SOS-GMO) #######

p1 <- ggplot(result_df_1_3, aes(x = Variable, y = Lab, fill = Partial_Correlation)) +
  geom_tile(color = "grey40") +                                 # 矩形块表示相关性，边框灰
  geom_text(aes(label = Text), color = "black", size = 2.5,
            vjust = 0.75, lineheight = 0.85) +  # vjust = 0.8更下, 控制垂直位置；调整 vjust 和行间距
  scale_fill_gradient2(
    low = "#006699", mid = "white", high = "#990000", midpoint = 0,  
    limits = c(-0.5, 0.5),  
    breaks = c(-0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5),  
    labels = c("<-0.5", "-0.4", "-0.3", "-0.2", "-0.1", "0", "0.1", "0.2", "0.3", "0.4", ">0.5"),  
    name = "Partial correlation coefficients",                                              # 图例标题
    guide = guide_colorbar(title.position = "right", title.hjust = 0.5),# 标题竖直居中
    oob = scales::squish) +    # 超出范围的值压缩到范围内，但仍保留颜色映射   
  theme_minimal() +                                                    # 简洁主题
  theme(                       # 字体大小 字体类型 # 字体颜色 # family = "Arial",
    axis.text.x = element_text( size = 8, angle = 30, hjust = 0.7),              # 旋转横坐标标签
    axis.text.y = element_text( size = 8),
    legend.text = element_text( size = 8),
    legend.title = element_text(size = 9, angle = 90, hjust = 0.5, vjust = 1),  # 标题竖直显示
    legend.position = "right",                                        # 图例放在右侧
    legend.key.height = unit(2.15, "cm"),                              # 增高图例
    # legend.key.height = unit(1, "npc"),                             # 让图例高度等于整个绘图区域
    legend.key.width = unit(0.3, "cm"),                               # 让图例变窄
    panel.grid.major = element_blank(),                               # 去掉网格线
    panel.grid.minor = element_blank()) +
    labs(x = "",  y = "", fill = "Partial correlation coefficients")                         # 图例标题

print(p1)
ggsave(filename = "./0.figure/03 Paper/Fig.6.PartialCor_GDO_phe1_3(a).tiff",
        plot = p1,  width = 5,  height = 5,  units = "in",  dpi = 300)


######## 绘制相关性图2(GMO-GDO) #######


p2 <- ggplot(result_df_3_4, aes(x = Variable, y = Lab, fill = Partial_Correlation)) +
  geom_tile(color = "grey40") +                                 # 矩形块表示相关性，边框灰
  geom_text(aes(label = Text), color = "black", size = 2.5,
            vjust = 0.75, lineheight = 0.85) +  # vjust = 0.8更下, 控制垂直位置；调整 vjust 和行间距
  scale_fill_gradient2(
    low = "#006699", mid = "white", high = "#990000", midpoint = 0,  
    limits = c(-0.5, 0.5),  
    breaks = c(-0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5),  
    labels = c("<-0.5", "-0.4", "-0.3", "-0.2", "-0.1", "0", "0.1", "0.2", "0.3", "0.4", ">0.5"),  
    name = "Partial correlation coefficients",                                               # 图例标题
    guide = guide_colorbar(title.position = "right", title.hjust = 0.5),# 标题竖直居中
    oob = scales::squish) +    # 超出范围的值压缩到范围内，但仍保留颜色映射
  theme_minimal() +                                                     # 简洁主题
  theme(                       # 字体大小 字体类型 # 字体颜色 # family = "Arial",
    axis.text.x = element_text( size = 8, angle = 30, hjust = 0.7),              # 旋转横坐标标签
    axis.text.y = element_text( size = 8),
    legend.text = element_text( size = 8),
    legend.title = element_text(size = 9, angle = 90, hjust = 0.5, vjust = 1),  # 标题竖直显示
    legend.position = "right",                                        # 图例放在右侧
    legend.key.height = unit(2.15, "cm"),                              # 增高图例
    # legend.key.height = unit(1, "npc"),                             # 让图例高度等于整个绘图区域
    legend.key.width = unit(0.3, "cm"),                               # 让图例变窄
    panel.grid.major = element_blank(),                               # 去掉网格线
    panel.grid.minor = element_blank()) +
  labs(x = "",  y = "", fill = "Partial correlation coefficients")                         # 图例标题

print(p2)
ggsave(filename = "./0.figure/03 Paper/Fig.6.PartialCor_GDO_phe1_3(b).tiff",
       plot = p2,  width = 5,  height = 5,  units = "in",  dpi = 300)






