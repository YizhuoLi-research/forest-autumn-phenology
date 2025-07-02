###########################################################################################
##
## R source code to read and visualize Köppen-Geiger fields (Version of 19 August 2022)                                                                                    
##
## Climate classification after Kottek et al. (2006), downscaling after Rubel et al. (2017)
##
## Kottek, M., J. Grieser, C. Beck, B. Rudolf, and F. Rubel, 2006: World Map of the  
## Köppen-Geiger climate classification updated. Meteorol. Z., 15, 259-263.   
## (Cited)
##
## Rubel, F., K. Brugger, K. Haslinger, and I. Auer, 2017: The climate of the 
## European Alps: Shift of very high resolution Köppen-Geiger climate zones 1800-2100. 
## Meteorol. Z., DOI 10.1127/metz/2016/0816.    
## (Cited)
##
## (C) Climate Change & Infectious Diseases Group, University of Veterinary Medicine Vienna
##     Vetmeduni Vienna, Austria
##
###########################################################################################

# required packages 
library(raster); library(rasterVis); data(countriesHigh)
library(latticeExtra);library(terra)
setwd("D:/VegetationImpact/01 Download/04 Koppen-Geiger climate map_download")

# Read raster files
r <- raster("./EA_Results/EA_koppen_30km.tif")

# Color palette for climate classification  # A-B-C-D-E
climate.colors=c("#960000", "#FF0000", "#FF6E6E", "#FFCCCC", 
                 "#CC8D14", "#CCAA54", "#FFCC00", "#FFFF64",
                 "#007800", "#005000", "#003200", "#96FF00", "#00D700", "#00AA00", "#BEBE00", "#8C8C00", "#5A5A00",
                 "#550055", "#820082", "#C800C8", "#FF6EFF", "#646464", "#8C8C8C", "#BEBEBE", "#E6E6E6", "#6E28B4", "#B464FA", "#C89BFA", "#C8C8FF",
                 "#6496FF", "#64FFFF")
# Legend must correspond to all climate classes, insert placeholders
r[1:30] <- seq(1,30,1)  
r0 <- r[1:30]
# Converts raster field to categorical data
#将一个名为 r 的 Raster 对象转换为具有属性表的 RasterLayer 对象，这样可以在栅格数据中附加元数据或其他相关信息。
r <- ratify(r)
rat <- levels(r)[[1]]
# Legend in alphabetic order
rat$climate <- c('Af', 'Am', 'As', 'Aw', 'BSh', 'BSk', 'BWh', 'BWk', 'Cfa', 'Cfb','Cfc', 'Csa', 'Csb', 'Csc', 'Cwa','Cwb', 'Cwc', 'Dfa', 'Dfb', 'Dfc','Dfd', 'Dsa', 'Dsb', 'Dsc', 'Dsd','Dwa', 'Dwb', 'Dwc', 'Dwd', 'EF','ET')
# Remove the placeholders
r[1:30] <- r0
#将修改后的属性表重新赋值给Raster对象r。
levels(r) <- rat
levelplot(r)

writeRaster(r, filename = "./EA_Results/EA_koppen_30km_addClimate.tif", 
            format = "GTiff",overwrite=TRUE)
