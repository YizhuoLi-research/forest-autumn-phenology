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
  "./EA+NA_Results/merged_Clim_Duration_Extremes_years/phe3_4/"
)
phe_GDO_dir <- "./EA+NA_Results/merged_Phe_DOY_years_2_PHE_analysis/merged_phe4_DOY/"

# 定义22个气候指标的前缀
prefixes <- c(
  "TXx_phe1_3", "TNn_phe1_3", "DTR_phe1_3", "TXge30_phe1_3", "TNlt2_phe1_3",
  "RainDay_phe1_3", "CDD_phe1_3", "CWD_phe1_3", "Rx1day_phe1_3", "SDII_phe1_3", 
  "merged_Duration_phe1_3",
  "TXx_phe3_4", "TNn_phe3_4", "DTR_phe3_4", "TXge30_phe3_4", "TNlt2_phe3_4",
  "RainDay_phe3_4", "CDD_phe3_4", "CWD_phe3_4", "Rx1day_phe3_4", "SDII_phe3_4",
  "merged_Duration_phe3_4"
)

# 读取气候指标数据（22个指标 × 10年）
file_paths <- unlist(lapply(prefixes, function(prefix) {
  list.files(base_dir, pattern = paste0("^", prefix, ".*\\.tif$"), full.names = TRUE) %>%
    sort() %>%
    head(10)
}))
clim_rasters <- rast(file_paths)        #220层

# 读取GDO物候数据（10年）
phe_GDO_files <- list.files(phe_GDO_dir, pattern = "\\.tif$", full.names = TRUE)
phe_GDO_rasters <- rast(phe_GDO_files)  #10层

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



# 初始化空数据框存储所有结果
final_importance_df <- data.frame()



# 确保这些变量已正确定义：


for (Climate_Class in Climate_Class_list) {
  cat("Processing Climate Class:", Climate_Class, "\n")
  
  # 检查气候类型是否存在
  order_val <- match(Climate_Class, classify_border$EA_koppen_30km_addClimate)
  if (is.na(order_val)) next
  
  selected_border_val <- classify_border[order_val, ]
  
  # 对极端指标变量和phe变量进行掩膜
  extremes_masked_zone <- mask(crop(clim_rasters, selected_border_val), selected_border_val)
  phe_masked_zone <- mask(crop(phe_GDO_rasters, selected_border_val), selected_border_val)
  forest_masked <- mask(crop(r2, selected_border_val), selected_border_val)
  
  for (forest in forest_types) {
    cat("Processing Forest Type:", forest, "\n")
    
    tryCatch({
      # 创建森林掩膜
      current_forest_mask <- ifel(forest_masked == forest, 1, NA)
      
      # 进行森林掩膜
      extremes_masked_zone_forest <- mask(extremes_masked_zone, current_forest_mask)
      phe_masked_zone_forest <- mask(phe_masked_zone, current_forest_mask)
      
      # 获取有效像元索引
      valid_pixels <- which(!is.na(values(phe_masked_zone_forest[[1]])))
      
      # 构建数据框
      df_base <- data.frame(
        pixel_id = rep(valid_pixels, times = 10),
        year = rep(2013:2022, each = length(valid_pixels)),
        GDO = as.vector(values(phe_masked_zone_forest)[valid_pixels, ])
      )
      
      # 提取22个指标数据
      for (i in 1:22) {
        idx_start <- (i-1)*10 + 1
        idx_end <- i*10
        df_base[[prefixes[i]]] <- as.vector(values(extremes_masked_zone_forest[[idx_start:idx_end]])[valid_pixels, ])
      }
      
      yearly_importance <- list()
      valid_years <- c()
      
      # 按年份循环建模
      for (y in 2013:2022) {
        df_year <- df_base %>% 
          filter(year == y) %>%
          select(GDO, all_of(prefixes)) %>%
          na.omit()
        
        if (nrow(df_year) < 10) {
          message(paste("跳过年份", y, "：有效样本不足"))
          next
        }
        
        # 标准化数据
        safe_scale <- function(x) {
          if (sd(x, na.rm = TRUE) == 0) rep(0, length(x)) else as.numeric(scale(x))
        }
        
        df_scaled <- df_year %>% mutate(across(-GDO, safe_scale))
        
        # 训练模型
        set.seed(123)
        rf_year <- randomForest(
          GDO ~ .,
          data = df_scaled,
          importance = TRUE,
          ntree = 1000,
          mtry = floor(length(prefixes)/3),
          keep.forest = TRUE)
        
        
        # 获取%IncMSE（变量重要性）
        imp <- importance(rf_year, type = 1, scale = FALSE)[, 1]  # 取第一列
        
        # 构造该年结果
        year_df <- data.frame(
          Climate_Class = Climate_Class,
          Forest_Type = forest,
          Year = y,
          Variable = names(imp),
          IncMSE = imp,
          stringsAsFactors = FALSE
        )
        
        # 合并到总表
        final_importance_df <- rbind(final_importance_df, year_df)
      }
    }, error = function(e) {
      cat("Error in", Climate_Class, "forest type", forest, ":", e$message, "\n")
    })
  }
}

# 查看结果
head(final_importance_df)





# 格式化最终输出（可选）
final_importance_df$IncMSE <- format(final_importance_df$IncMSE, scientific = FALSE, digits = 4, nsmall = 4)

# 查看结果
head(final_importance_df)

# 保存结果
output_dir <- "./EA+NA_Results/merged_Indices_Importance_result"
if (!dir.exists(output_dir)) {dir.create(output_dir, recursive = TRUE, showWarnings = TRUE) } # 这里用output_dir变量
write.csv(final_importance_df, 
          file.path(output_dir, "GDO_10years_Indices_Importance_Results.csv"), 
          row.names = FALSE)




