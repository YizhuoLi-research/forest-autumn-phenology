library(raster)
library(terra)
library(progress)
# setwd("D:/Graduation_Thesis/01 Download/14 SkinTmax_download")

##  依次运行(7次)
# 11 Tmax_download  // 12 Tmin_download  //
# 13 Precipitation_download  
# 14 SkinTmax_download  //  15 SkinTmin_download  
# 16 SoilTmax_download  //  17 SoilTmin_download


folder_path <- "./01 Download/15 SkinTmin_download/EA_SkinTmin/"   #NorthAmerica的数据
file_list <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)

## 对应修改
#  EA_Tmax  // EA_Tmin
#  EA_Precipitaiton
#  EA_SkinTmax  //  EA_SkinTmin
#  EA_SoilTmax  //  EA_SoilTmin

#识别文件夹年份并分类
file_years <- substr(basename(file_list), 20,23)
file_groups <- split(file_list, file_years)
pb <- progress_bar$new(format = "[:bar] :current/:total", total = length(file_groups), clear = FALSE)
for (i in 1:length(file_groups)) {
  # i=9
  year <- names(file_groups)[i]
  files <- file_groups[[year]]
  
  # 读取第一个文件来获取波段数
  first_raster <- rast(files[1])
  band_count <- nlyr(first_raster)
  # # 将第一个文件作为基准栅格
  # base_raster <- first_raster
  
  # 创建一个新的文件夹来存储拼接后的文件
  output_folder <- paste0(folder_path, year, "/")
  # 检查目录是否存在，如果不存在则创建
  if (!file.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  
  
  # 逐个处理每个波段的拼接，并保存为单独的文件
  for (j in 1:band_count) {
    print(j)
    # j=366
    # 为四个纬度带中的每个文件获取当前波段的栅格数据
    band_rasters <- lapply(files, function(file) {
      a <- rast(file)[[j]]
      a[is.nan(a)] = NA
      return(a)
    })
    
    # merged_raster <- do.call(terra::mosaic, band_rasters)                        # 默认取均值
    # merged_raster <- do.call(terra::mosaic, c(band_rasters, list(fun = "mean")))  #一样的
    
    rsrc <- sprc(band_rasters)
    merged_raster <- merge(rsrc, na.rm = TRUE)
    # # 
    # rsrc <- sprc(band_rasters)
    # merged_raster <- terra::mosaic(rsrc)
    
    
    plot(merged_raster)
    # 获取当前波段的名称
    band_name <- names(rast(files[1]))[j]
    band_number <- gsub("\\D", "", substr(band_name, nchar(band_name) - 2, nchar(band_name)))
    
    # 生成当前波段的输出文件名
    output_file <- paste0(output_folder, band_number, ".tif")
    
    # 写入拼接后的文件
    writeRaster(merged_raster, filename = output_file,overwrite=TRUE)
  }
  
  pb$tick()
  cat("\n已生成", year, "年份的拼接文件\n")
}

