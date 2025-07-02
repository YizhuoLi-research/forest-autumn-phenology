library(terra)
library(ppcor)
setwd("D:/Graduation_Thesis")

# 定义目录路径
base_dir <- c(
  "./EA+NA_Results/merged_Clim_Duration_Extremes_years/phe1_3/",
  "./EA+NA_Results/merged_Clim_Duration_Extremes_years/phe3_4/"
)

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

# 读取物候数据文件
phe_GDO_files <- list.files(phe_GDO_dir, pattern = "\\.tif$", full.names = TRUE)

# 读取物候数据
phe_GDO_rasters <- rast(phe_GDO_files)

# 初始化结果存储
results <- list()

# 遍历每个指标
for (i in seq_along(prefixes)) {
  
  cat("Processing:", prefixes[i], "\n")
  
  # 获取当前指标的文件路径
  current_var_files <- file_paths[grepl(paste0("^", prefixes[i]), basename(file_paths))]
  current_var_rasters <- rast(current_var_files)  # 读取当前指标的栅格数据
  
  # 读取其他控制极端指标数据（21组，每组10个文件）
  control_files <- lapply(prefixes[-i], function(prefix) {
    files <- file_paths[grepl(paste0("^", prefix), basename(file_paths))]
  })
  control_files_rasters <- rast(unlist(control_files))  # 21个变量 * 10层
  
  # 对三套文件的共有像元进行处理
  sample1 <- phe_GDO_rasters
  sample1[is.finite(sample1)] <- 1
  
  sample2 <- current_var_rasters
  sample2[is.finite(sample2)] <- 1
  
  sample3 <- control_files_rasters
  sample3[is.finite(sample3)] <- 1
  
  phe_GDO_rasters <- phe_GDO_rasters * sample1[[1]] * sample2[[1]] * sample3[[1]]
  current_var_rasters <- current_var_rasters * sample1[[1]] * sample2[[1]] * sample3[[1]]
  control_files_rasters <- control_files_rasters * sample1[[1]] * sample2[[1]] * sample3[[1]]
  
  # 合并数据
  new_data <- c(phe_GDO_rasters, current_var_rasters, control_files_rasters)
  
  # 定义偏相关分析函数
  p_cor_analysis <- function(pixels) {
    tryCatch({
      # 提取物候事件的 DOY
      phe_value <- pixels[1:10]  # 10年物候事件的 DOY
      current_var <- pixels[11:20]  # 10年的当前研究变量
      
      # 提取控制变量
      control_vars <- lapply(1:21, function(i) {
        start_index <- 21 + (i - 1) * 10
        end_index <- start_index + 9
        pixels[start_index:end_index]
      })
      
      # 检查数据是否存在
      if (is.null(phe_value) || is.null(current_var)) {
        stop("Missing data for phe_value or current_var")
      }
      
      # 将数据转换为数据框
      df_val <- data.frame(
        phe_value = phe_value,
        current_var = current_var,
        do.call(cbind, control_vars)
      )
      
      # 去除 NA 值
      df_val <- na.omit(df_val)
      
      # **异常处理**
      # 1. 如果数据少于 2 行，直接返回 NA
      if (nrow(df_val) < 2) {    return(NA)   }
      
      # # 2.检查并移除方差为 0 的控制变量
      variances <- apply(df_val[, -c(1, 2)], 2, var)
      zero_var_cols <- names(variances[variances == 0])
      if (length(zero_var_cols) > 0) {
        cat("Removing control variables with zero variance:", zero_var_cols, "\n")
        df_val <- df_val[, !colnames(df_val) %in% zero_var_cols]
      }
      
      # # 3. 如果控制变量全部被移除，不计算偏相关
      # if (ncol(df_val) <= 2) { return(c(NA, NA)) }
      
      # 计算偏相关性
      partial_cor_test <- ppcor::pcor.test(
        df_val$current_var, 
        df_val$phe_value, 
        df_val[, -c(1, 2)]  # 控制变量
        # method = "pearson"   # 默认
      )
      
      # 返回偏相关系数
      return(partial_cor_test$estimate)
    }, error = function(e) {
      # 如果出现错误，返回 NA
      return(NA)
    })
  }
  
  # 使用 app() 函数逐像素计算偏相关性
  partial_corr_map <- app(new_data, fun = p_cor_analysis, cores = 10)
  
  # 保存结果
  # 定义输出目录
  output_dir <- "./EA+NA_Results/merged_partial_corr_result/with_GDO"
  
  # 检查目录是否存在，如果不存在则创建
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)  # recursive = TRUE 确保创建所有父目录
  }
  
  writeRaster(partial_corr_map, 
     filename = paste0("./EA+NA_Results/merged_partial_corr_result/with_GDO/partial_corr_", prefixes[i], ".tif"), 
     overwrite = TRUE)
}


r1 <- rast("./EA+NA_Results/merged_partial_corr_result/with_GDO/partial_corr_TXx_phe1_3.tif")
r2 <- rast("./EA+NA_Results/merged_partial_corr_result/with_GDO/partial_corr_TXge30_phe3_4.tif")
r3 <- rast("./EA+NA_Results/merged_partial_corr_result/with_GDO/partial_corr_TNn_phe1_3.tif")
r4 <- rast("./EA+NA_Results/merged_partial_corr_result/with_GDO/partial_corr_TNlt2_phe1_3.tif")
r5 <- rast("./EA+NA_Results/merged_partial_corr_result/with_GDO/partial_corr_DTR_phe1_3.tif")
r6 <- rast("./EA+NA_Results/merged_partial_corr_result/with_GDO/partial_corr_RainDay_phe1_3.tif")
r7 <- rast("./EA+NA_Results/merged_partial_corr_result/with_GDO/partial_corr_CWD_phe1_3.tif")
r8 <- rast("./EA+NA_Results/merged_partial_corr_result/with_GDO/partial_corr_CDD_phe1_3.tif")
r9 <- rast("./EA+NA_Results/merged_partial_corr_result/with_GDO/partial_corr_RX1day_phe1_3.tif")
r10 <- rast("./EA+NA_Results/merged_partial_corr_result/with_GDO/partial_corr_SDII_phe1_3.tif")
r11 <- rast("./EA+NA_Results/merged_partial_corr_result/with_GDO/partial_corr_merged_Duration_phe1_3.tif")

# plot(raster1)
# plot(raster2)
# 计算平均值
mean1 <- mean(values(r1), na.rm = TRUE)
mean2 <- mean(values(r2), na.rm = TRUE)
mean3 <- mean(values(r3), na.rm = TRUE)
mean4 <- mean(values(r4), na.rm = TRUE)
mean5 <- mean(values(r5), na.rm = TRUE)
mean6 <- mean(values(r6), na.rm = TRUE)
mean7 <- mean(values(r7), na.rm = TRUE)
mean8 <- mean(values(r8), na.rm = TRUE)
mean9 <- mean(values(r9), na.rm = TRUE)
mean10 <- mean(values(r10), na.rm = TRUE)
mean11 <- mean(values(r11), na.rm = TRUE)

# 打印平均值
cat("File 1 平均值:", mean1, "\n")
cat("File 2 平均值:", mean2, "\n")
cat("File 3 平均值:", mean3, "\n")
cat("File 4 平均值:", mean4, "\n")
cat("File 5 平均值:", mean5, "\n")
cat("File 6 平均值:", mean6, "\n")
cat("File 7 平均值:", mean7, "\n")
cat("File 8 平均值:", mean8, "\n")
cat("File 9 平均值:", mean9, "\n")
cat("File 10 平均值:", mean10, "\n")
cat("File 11 平均值:", mean11, "\n")

