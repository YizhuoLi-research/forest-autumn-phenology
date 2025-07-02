##################  00 加载包   ##########################################################################
library(terra)
library(tidyverse)
library(raster)
library(ggplot2)
# library(ggpubr)

setwd("D:/Graduation_Thesis")

#############################   01.按照柯本气候类型，将气候区分为6个气候区域   ####################
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


##################  03 按气候-森林类型 取像元计算皮尔森（Pearson相关性）计算2--GDO/3--EOS个气候区间  ###########################

# 文件路径和标签列表
base_dir <- "./EA+NA_Results/merged_Clim_Duration_Extremes_years/phe1_3/"
phe_GDO_dir <- "./EA+NA_Results/merged_Phe_DOY_years_2_PHE_analysis/merged_phe4_DOY/"

# 定义需要匹配的前缀
prefixes <- c("TXx_phe1_3", "TNn_phe1_3", "DTR_phe1_3", "TXge30_phe1_3", "TNlt2_phe1_3",
              "RainDay_phe1_3", "CDD_phe1_3", "CWD_phe1_3", "Rx1day_phe1_3", "SDII_phe1_3", 
              "merged_Duration_phe1_3")

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


# 筛选文件 # 10年extremes的 SpatRaster
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

file_groups <- list( TXx = TXx_phe1_3_files, TNn = TNn_phe1_3_files,  DTR = DTR_phe1_3_files, TXge30 = TXge30_phe1_3_files,
                     TNlt2 = TNlt2_phe1_3_files, RainDay = RainDay_phe1_3_files,CDD = CDD_phe1_3_files, CWD = CWD_phe1_3_files,
                     Rx1day = Rx1day_phe1_3_files, SDII = SDII_phe1_3_files, merged_Duration = merged_Duration_phe1_3_files)



analyze_correlation <- function(extremes_var, phe_GDO_DOY, classify_border, Climate_Class_list, forest_types, var_type) {
  results <- data.frame(Variable = character(),
                        Climate_Class = character(),
                        Forest_Class = numeric(),
                        Correlation = numeric(),
                        p_value = numeric(),
                        stringsAsFactors = FALSE)
  
  for (Climate_Class in Climate_Class_list) {
    order_val <- match(Climate_Class, classify_border$EA_koppen_30km_addClimate)
    if (is.na(order_val)) next  # 跳过不存在的气候类型
    
    #创建并提取气候区掩膜
    selected_border_val <- classify_border[order_val, ]
    
    extremes_masked <- mask(crop(extremes_var, selected_border_val), selected_border_val)
    phe_masked      <- mask(crop(phe_GDO_DOY, selected_border_val), selected_border_val)
    forest_masked   <- mask(crop(r2, selected_border_val), selected_border_val)
    
    for (forest in forest_types) {
      # 创建森林掩膜--进提取1/2/3
      current_forest_mask <- ifel(forest_masked == forest, 1, NA)
      # 进行森林掩膜
      extremes_forest <- mask(extremes_masked, current_forest_mask)
      phe_forest      <- mask(phe_masked, current_forest_mask)
      
      # 提取数据
      df1_val <- terra::extract(extremes_forest, selected_border_val)
      df2_val <- terra::extract(phe_forest, selected_border_val)
      
      if (is.null(df1_val) || is.null(df2_val)) next  # 确保数据存在
      
      df_val <- cbind(df1_val[, -1], df2_val[, -1])
      
      colnames(df_val) <- c(paste0("extremes_var", 2013:2022),
                            paste0("phe_GDO_DOY", 2013:2022))
      
      # 提取数值
      extremes_val <- unlist(df_val[, grep("extremes_var", colnames(df_val), value = TRUE)])
      phe_val <- unlist(df_val[, grep("phe_GDO_DOY", colnames(df_val), value = TRUE)])
      
      new_df_val <- data.frame(extemes_value = extremes_val, phe_value = phe_val)
      new_df_val <- na.omit(new_df_val)
      
      # if (nrow(new_df_val) >= 10) {   #调整数据量
      correlation_test <- cor.test(new_df_val$extemes_value, new_df_val$phe_value, method = "pearson")
      
      result_row <- data.frame(
        Variable = var_type,  # 动态使用 var_type
        Climate_Class = Climate_Class, 
        Forest_Class  = forest, 
        Correlation = correlation_test$estimate, 
        p_value = correlation_test$p.value)
      
      results <- rbind(results, result_row)  # 逐步添加结果
      # } else { cat("Insufficient data points for filter value:", Climate_Class, "\n") }
    }
  }
  return(results)
}


# 在 perform_analysis 函数中调用 analyze_correlation
perform_analysis <- function(file_groups) {
  list_of_results <- list()
  
  for (var_type in names(file_groups)) {  # 遍历变量类型 (TXx, TNn, DTR ...)
    file_list_extremes <- file_groups[[var_type]]  # 获取该变量的文件
    
    # 读取 extremes 数据
    extremes_var <- rast(c(file_list_extremes))
    # 读取 phe_GDO_DOY 数据
    phe_GDO_DOY <- rast(c(phe_GDO_files))
    
    # 调用 analyze_correlation，并传递 var_type
    list_of_results[[var_type]] <- analyze_correlation(extremes_var, phe_GDO_DOY, classify_border, Climate_Class_list, forest_types, var_type)
    
    cat("Results for", var_type, "\n")
    print(list_of_results[[var_type]])
  }
  
  return(list_of_results)
}


results_all <- perform_analysis(file_groups)



results_df <- do.call(rbind, results_all)

results_df$Cor_value <- round(as.numeric(results_df$Correlation),2)
results_df$stars <- ifelse(results_df$p_value <= 0.001, "***",
                           ifelse(results_df$p_value <= 0.01, "**",
                                  ifelse(results_df$p_value <= 0.05, "*",
                                         sprintf("%.2f", results_df$p_value))))


library(writexl)
write.csv(results_df, "./0.figure/03 Paper/Pearson-cor_phe1_3toDOY.csv", row.names = FALSE)









###########################

###########################  04-1 绘制C-climatetype的k值lineplot图   #####################################################################

k_df_C$Slope <-  as.numeric(k_df_C$Slope)    
k_df_D$Slope <-  as.numeric(k_df_D$Slope)     

p1 <- ggplot(k_df_C, aes(x = PHE, y = Slope, group = legend, color = legend)) +
  geom_line(size = 5) +                       # 增加线条的粗细
  geom_text(data = k_df_C, aes(label = label_text), vjust = -0, size = 20, 
            show.legend = FALSE)+             #不显示字标图例
  # labs(x = "Phenological index", y = "β (℃/℃  )") +  # 坐标轴标签
  labs(x = "Phenological index", y =expression(paste(D[T]~"(℃/℃)")))+ 
  theme_minimal() +                           # 使用简洁主题
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999",size=2) +
  scale_color_manual(
    values = c("#663300", "#996600", "#FFCC00"),
    labels = c("CXa", "CXb", "CXc"),
    name = "Climate type"
  )+
  theme(legend.position = "right") + 
  coord_fixed(ratio = 1/0.5) +
  theme( legend.position = c(0.84, 0.88), 
         axis.text.x  = element_text(size = 36), 
         axis.text.y  = element_text(size = 42), 
         axis.line = element_line(size = 2),  # 调整坐标轴线粗细为 2
         axis.ticks = element_line(size = 2), 
         axis.ticks.length = unit(0.3, "cm"),
         axis.ticks.y = element_line(size = 2),  # 显示坐标轴刻度宽度
         axis.title = element_text(size = 45,margin = margin(t = 10)),
         legend.title = element_text(size = 37),
         legend.text = element_text(size = 37),
         axis.title.x = element_text(margin = margin(t = 20)),# 调整 x 轴标题与 x 轴的距离（例如设置为 20）
         panel.grid = element_line( linetype = "blank"))+
  guides(color = guide_legend(ncol = 1,keywidth = 2),
         fill = guide_legend(byrow = TRUE) ) +
  ylim(-1.0 ,1.0)
p1

ggsave(
  filename = "./0.figure/Fig.S6-lineplot_C-T.tiff",
  plot = p1,  width = 15,  height = 11,  units = "in",  dpi = 300)


###########################  04-2 绘制D-climatetype的k值lineplot图   #####################################################################


p2 <- ggplot(k_df_D, aes(x = PHE, y = Slope, group = legend, color = legend)) +
  geom_line(size = 5) +                       # 增加线条的粗细
  geom_text(data = k_df_D, aes(label = label_text), vjust = -0, size = 20, 
            show.legend = FALSE)+             #不显示字标图例
  # labs(x = "Phenological index", y = "β (℃/℃  )") +  # 坐标轴标签
  labs(x = "Phenological index", y =expression(paste(D[T]~"(℃/℃)")))+ 
  theme_minimal() +                           # 使用简洁主题
  geom_hline(yintercept = 0, linetype = "dashed", color = "#999999",size=2) +
  guides(color = guide_legend(ncol = 1))+
  scale_color_manual(
    values = c("#820082", "#C800C8", "#C89BFA", "#C8C8FF"),
    labels = c("DXa", "DXb", "DXc","DXd"),
    name = "Climate type"
  )+
  theme(legend.position = "right") + 
  coord_fixed(ratio = 1/0.7) +
  theme( legend.position = c(0.84, 0.86), 
         axis.text.x  = element_text(size = 36), 
         axis.text.y  = element_text(size = 42), 
         axis.line = element_line(size = 2),  # 调整坐标轴线粗细为 2
         axis.ticks = element_line(size = 2), 
         axis.ticks.length = unit(0.3, "cm"),
         axis.ticks.y = element_line(size = 2),  # 显示坐标轴刻度宽度
         axis.title = element_text(size = 45,margin = margin(t = 10)),
         legend.title = element_text(size = 37),
         legend.text = element_text(size = 37),
         axis.title.x = element_text(margin = margin(t = 20)),# 调整 x 轴标题与 x 轴的距离（例如设置为 20）
         panel.grid = element_line( linetype = "blank"))+
  guides(color = guide_legend(ncol = 1,keywidth = 2),
         fill = guide_legend(byrow = TRUE) ) +
  # scale_y_continuous(breaks = c(-0.5, 1.0, 1.5)) +  
  # ylim(-0.6 , 1.9)
  scale_y_continuous(breaks = c(-0.5,0,0.5, 1.0, 1.5,2.0), limits = c(-0.6, 2.25))  # 在 scale_y_continuous 中设置 limits



p2

ggsave(
  filename = "./0.figure/Fig.S6-lineplot_D-T.tiff",
  plot = p2,  width = 15,  height = 11,  units = "in",  dpi = 300)
