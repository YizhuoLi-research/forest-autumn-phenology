library(terra)
library(raster)
library(randomForest)
library(dplyr)
library(purrr)
library(ggplot2)
setwd("D:/Graduation_Thesis")

#############################   01. 提取极端指标文件和物候文件   #######################

# 定义目录路径
base_dir <- c(
  "./EA+NA_Results/merged_Clim_Duration_Extremes_years/phe1_3/",
  "./EA+NA_Results/merged_Clim_Duration_Extremes_years/phe3_4/",
  "./EA+NA_Results/merged_Clim_Duration_Extremes_years/phe4_6/"
)

phe_EOS_dir <- "./EA+NA_Results/merged_Phe_DOY_years_2_PHE_analysis/merged_phe6_DOY/"

# 定义33个气候指标的前缀
prefixes <- c(
  "TXx_phe1_3", "TNn_phe1_3", "DTR_phe1_3", "TXge30_phe1_3", "TNlt2_phe1_3",
  "RainDay_phe1_3", "CDD_phe1_3", "CWD_phe1_3", "Rx1day_phe1_3", "SDII_phe1_3", 
  "merged_Duration_phe1_3",
  "TXx_phe3_4", "TNn_phe3_4", "DTR_phe3_4", "TXge30_phe3_4", "TNlt2_phe3_4",
  "RainDay_phe3_4", "CDD_phe3_4", "CWD_phe3_4", "Rx1day_phe3_4", "SDII_phe3_4", 
  "merged_Duration_phe3_4",
  "TXx_phe4_6", "TNn_phe4_6", "DTR_phe4_6", "TXge30_phe4_6", "TNlt2_phe4_6",
  "RainDay_phe4_6", "CDD_phe4_6", "CWD_phe4_6", "Rx1day_phe4_6", "SDII_phe4_6", 
  "merged_Duration_phe4_6")

# 读取气候指标数据（22个指标 × 10年）
file_paths <- unlist(lapply(prefixes, function(prefix) {
  list.files(base_dir, pattern = paste0("^", prefix, ".*\\.tif$"), full.names = TRUE) %>%
    sort() %>%
    head(10)
}))
clim_rasters <- rast(file_paths)        #330层

# 读取GDO物候数据（10年）
phe_EOS_files <- list.files(phe_EOS_dir, pattern = "\\.tif$", full.names = TRUE)
phe_EOS_rasters <- rast(phe_EOS_files)  #10层

#############################   02.按照柯本气候类型，将气候区分为6个气候区域   #######################
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

Climate_Class_list <- list(Cfa,Cfb,Dfb,Dfc,Dwb,Dwc)

#############################   03.在气候类型内，筛选三种林地类型   ####################
#############################   1---再生林 2---原生林 3---人工林


## 1. 读取数据
# 列出文件夹中的所有文件
r2 <-  rast("./EA+NA_Results/EA+NA_GFC_30km.tif")
forest_types <- c(1, 2, 3)  # 1: 再生林； 2：原生林；   3：人工林

##################  04. 按气候-森林类型 采用随机森林方法做计算  ###########################


# # 确保所有栅格范围和分辨率一致
# # clim_rasters <- resample(clim_rasters, phe_EOS_rasters)
# # climate_zone <- resample(climate_zone, phe_EOS_rasters)
# # forest_type <- resample(forest_type, phe_EOS_rasters)
# 
# # 合并所有数据为一个多层栅格
# all_data <- c(phe_EOS_rasters, clim_rasters, climate_zone, forest_type)
# names(all_data) <- c(paste0("EOS_", 1:10), prefixes, "climate_zone", "forest_type")
# 
# # 转换为数据框（按像素提取值）
# df <- as.data.frame(all_data, xy = TRUE, na.rm = TRUE)
# 
# # 计算每个像素的GDO多年平均值（或其他统计量）
# df$EOS_mean <- rowMeans(df[, grep("EOS_", names(df))], na.rm = TRUE)


# 初始化空数据框存储所有结果
final_importance_df <- data.frame()


for (Climate_Class in Climate_Class_list) {
  # Climate_Class = "Cfa"
  cat("Processing Climate Class:", Climate_Class, "\n")
  
  # 检查气候类型是否存在
  order_val <- match(Climate_Class, classify_border$EA_koppen_30km_addClimate)
  if (is.na(order_val)) next  # 跳过不存在的气候类型
  
  selected_border_val <- classify_border[order_val, ]
  
  # 对极端指标变量和phe变量进行掩膜
  extremes_masked_zone <- mask(crop(clim_rasters, selected_border_val), selected_border_val)
  phe_masked_zone      <- mask(crop(phe_EOS_rasters, selected_border_val), selected_border_val)
  
  # 处理森林类型--Cfa中的森林类型： 0--空 1-再生林 2-原生林 3-人工林
  forest_masked   <- mask(crop(r2, selected_border_val), selected_border_val)
  
  for (forest in forest_types) {
    # forest = 1
    cat("Processing Forest Type:", forest, "\n")
    
    tryCatch({
    # 创建森林掩膜
    current_forest_mask <- ifel(forest_masked == forest, 1, NA)
    
    # 进行森林掩膜
    extremes_masked_zone_forest <- mask(extremes_masked_zone, current_forest_mask)
    phe_masked_zone_forest      <- mask(phe_masked_zone, current_forest_mask)
    
    # 获取有效像元索引（非NA）
    valid_pixels <- which(!is.na(values(phe_masked_zone_forest[[1]])))  # 以第一层（2013年）为例
    
    # 转换为数据框并添加年份和像元ID
    df_base <- data.frame(
      pixel_id = rep(valid_pixels, times = 10),  # 每个像元重复10次（对应10年）
      year = rep(2013:2022, each = length(valid_pixels)))  # 年份按像元分组
    
    # 提取GDO值
    df_base$GDO <- as.vector(values(phe_masked_zone_forest)[valid_pixels, ])
    
    # 直接使用33个指标名称作为列名提取数据
    for (i in 1:33) {
      idx_start <- (i-1)*10 + 1
      idx_end <- i*10
      df_base[[prefixes[i]]] <- as.vector(values(extremes_masked_zone_forest[[idx_start:idx_end]])[valid_pixels, ])
    }
    # # 查看结果（现在列名已经是实际指标名称）
    # head(df_base)
    
    # 数据预处理
    safe_scale <- function(x) {
      if (sd(x, na.rm = TRUE) == 0) rep(0, length(x)) else as.numeric(scale(x))
    }
    
    df_model <- df_base %>% 
      select(GDO, all_of(prefixes)) %>%  # 显式选择目标列
      na.omit() %>% 
      mutate(across(-GDO, safe_scale))        # 标准化--降水、温度等单位差异大
    
    # # 2. 验证数据,开始建模
    # stopifnot(length(prefixes) == ncol(df_model) - 1)
   
    set.seed(123)
    # 不设种子（每次结果不同）
    # randomForest(GDO ~ ., data = df_clean, ntree=5)$mse
    # # [1] 38.91217 35.91561 32.53037 30.18442 28.55227
    # # 固定种子（结果一致）
    # set.seed(123); randomForest(GDO ~ ., data=df_clean, ntree=5)$mse
    # # [1] 35.85409 36.30271 33.78257 31.68629 29.07817
    # set.seed(123); randomForest(GDO ~ ., data=df_clean, ntree=5)$mse
    # # [1] 35.85409 36.30271 33.78257 31.68629 29.07817
    # set.seed(456); randomForest(GDO ~ ., data=df_clean, ntree=5)$mse
    # [1] 35.85409 36.30271 33.78257 31.68629 29.07817
    # 虽然不同种子的MSE初始值不同（35.85 vs 42.77），但最终都收敛到28~29区间，说明：
    # 随机性主要影响训练过程，对模型最终性能影响有限，足够多的树ntree能抵消单次运行的随机波动
    #"We set random seeds to 123 for all stochastic processes to ensure reproducibility. 
    #Sensitivity tests with multiple seeds confirmed the robustness of our conclusions."

    # 建模
    rf_base <- randomForest(
      GDO ~ .,
      data = df_model,
      importance = TRUE,
      ntree = 1000,
      #原设定1000，通过plot(rf_base, main="OOB Error Convergence")发现500后树已经平稳
      # 500→1000提升0.2%
      mtry = floor(length(prefixes)/3),  # 每棵树使用的变量数
      # 避免过拟合，回归问题通常取总变量数的1/3
      keep.forest = TRUE
    )
    
    # 3. 提取重要性结果
    # print(rf_base)  # 显示模型概要（包括OOB误差）
    # Mean of squared residuals: 18.88     % Var explained: 75.23
    # # 变量重要性排序（基于%IncMSE）
    imp <- importance(rf_base, type = 1, scale = FALSE)
    importance_df <- as.data.frame(imp) # 转换为数据框
    importance_df$`%IncMSE` <- format(importance_df$`%IncMSE`, scientific = FALSE, digits = 4, nsmall = 4)
    
    # 格式化并添加元信息
    temp_df <- data.frame(
      Climate_Class = Climate_Class,
      Forest_Type = forest,
      Variable = rownames(importance_df),
      IncMSE = as.numeric(importance_df$`%IncMSE`),  # 保持数值格式
      stringsAsFactors = FALSE
    )
    
    # 合并到最终结果
    final_importance_df <- rbind(final_importance_df, temp_df)
    
    }, error = function(e) {
      cat("Error in", Climate_Class, "forest type", forest, ":", e$message, "\n")
    })
  }
}

# 格式化最终输出（可选）
final_importance_df$IncMSE <- format(final_importance_df$IncMSE, scientific = FALSE, digits = 4, nsmall = 4)

# 查看结果
head(final_importance_df)

# 保存结果
output_dir <- "./EA+NA_Results/merged_Indices_Importance_result"
if (!dir.exists(output_dir)) {dir.create(output_dir, recursive = TRUE, showWarnings = TRUE) } # 这里用output_dir变量
write.csv(final_importance_df, 
          file.path(output_dir, "EOS_Indices_Importance_Results.csv"), 
          row.names = FALSE)
    
    
    
    
    
    # Cfa\NRF
    # >     print(importance_sorted)
    # %IncMSE
    # merged_Duration_phe3_4 56.9293546
    # TXge30_phe3_4          30.8184056
    # DTR_phe3_4             19.2167517
    # TXx_phe1_3             16.0036183
    # TNn_phe1_3             12.6338825
    # TXx_phe3_4             12.5597578
    # TNn_phe3_4             11.5059658
    # merged_Duration_phe1_3  9.9370085
    # DTR_phe1_3              9.7255310
    # RainDay_phe1_3          9.6600409
    # TNlt2_phe1_3            7.9393509
    # SDII_phe1_3             7.0498527
    # RainDay_phe3_4          6.7939340
    # SDII_phe3_4             6.1056334
    # Rx1day_phe1_3           5.0771526
    # CWD_phe3_4              4.4279232
    # TXge30_phe1_3           4.4264512
    # CDD_phe1_3              3.6224186
    # CDD_phe3_4              3.5208077
    # Rx1day_phe3_4           2.8512161
    # CWD_phe1_3              1.7569897
    # TNlt2_phe3_4            0.0007138

    
    # # 4. 可视化（优化标签显示）
    # varImpPlot(rf_base,
    #            main = "Variable Importance (With Year Interactions)",
    #            type = 1,
    #            cex = 1)
    
#################  （not adopted ）逐年算法 ###############
# # 初始化存储列表
# yearly_importance <- list()
# valid_years <- c()
# 
# # 按年份循环建模
# for (y in 2013:2022) {
#   # 1. 提取当年数据并处理NA
#   # y=2013
#   df_year <- df_base %>% 
#     filter(year == y) %>%
#     select(GDO, all_of(prefixes)) %>%
#     na.omit()
#   
#   # 跳过无效年份
#   if (nrow(df_year) < 10) {
#     message(paste("跳过年份", y, "：有效样本不足"))
#     next
#   }
#   
#   # 2. 标准化解释变量（安全处理常数列）常数列为0（方差0）
#   safe_scale <- function(x) {
#     if (sd(x, na.rm = TRUE) == 0) rep(0, length(x)) else as.numeric(scale(x))
#   }
#   
#   df_scaled <- df_year %>% 
#     mutate(across(-GDO, safe_scale))
#   
#   # 3. 训练模型
#   set.seed(123)
#   rf_year <- suppressWarnings(
#     randomForest(
#       GDO ~ .,
#       data = df_scaled,
#       importance = TRUE,
#       ntree = 1000,
#       mtry =floor(length(prefixes)/3)  
#     )
#   )
#   print(rf_year)  # 显示模型概要（包括OOB误差）
#   
#   
#   # 4. 存储重要性（%IncMSE）
#   imp <- importance(rf_year, type = 1, scale=F)
#   # imp <- importance(rf_year, type = 1, scale=T)
#   
#   # importance_error
#   yearly_importance[[as.character(y)]] <- imp[, "%IncMSE", drop = FALSE]
#   valid_years <- c(valid_years, y)
# }
# 
# # 合并各年结果
# importance_matrix <- do.call(cbind, yearly_importance)
# 
# # 计算平均重要性
# mean_importance <- data.frame(
#   Variable = rownames(importance_matrix),
#   Mean_IncMSE = rowMeans(importance_matrix),
#   SD_IncMSE = apply(importance_matrix, 1, sd)
# ) %>% arrange(desc(Mean_IncMSE))
# 
# # 打印结果
# print("各年度重要性矩阵：")
# print(importance_matrix)
# print("\n平均重要性排序：")
# print(mean_importance)
# 
# # library(ggplot2)
# # ggplot(mean_importance, aes(x = reorder(Variable, Mean_IncMSE), y = Mean_IncMSE)) +
# #   geom_bar(stat = "identity", fill = "steelblue") +
# #   geom_errorbar(aes(ymin = Mean_IncMSE - SD_IncMSE, ymax = Mean_IncMSE + SD_IncMSE), 
# #                 width = 0.2) +
# #   coord_flip() +
# #   labs(
# #     title = "平均变量重要性 (2013-2022)",
# #     x = "气候极端指标",
# #     y = "重要性 (%IncMSE ± SD)"
# #   ) +
# #   theme_minimal()


