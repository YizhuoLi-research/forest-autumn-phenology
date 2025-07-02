###### 0. 加载包 ####
library(terra)
library(tidyverse)
library(raster)

setwd("D:/Graduation_Thesis")

##################  01 欧洲美洲气候类型栅格数据 合并 ##########################################

# 读取两个不同地理范围的栅格数据
r_EA <- rast("./EA_Results/EA_koppen_30km_addClimate.tif")
r_NA <- rast("./NA_Results/NA_koppen_30km_addClimate.tif")
# 确保两个栅格数据的分辨率和坐标系统一致
# 可以使用 resample() 函数来调整分辨率
# 可以使用 project() 函数来调整坐标系统
# 合并栅格数据
r_climate <- merge(r_EA, r_NA)
plot(r_EA)
plot(r_NA)
plot(r_climate)
writeRaster(r_climate, filename = "./0.Results/EA_NA_koppen_30km_addClimate.tif",
            overwrite=TRUE)