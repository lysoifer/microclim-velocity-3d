library(terra)
library(foreach)
library(ggplot2)
library(tidyterra)
library(colorspace)


trin = vect("./../Trinidad_microclimates/data/cropping_polygons/NRange.shp")
trin = aggregate(trin)
lai_files_82_90 = list.files(path = "./../Trinidad_microclimates/data/PAI/GIMMS_LAI4g_AVHRR_MODIS_consolidated_1982_1990/GIMMS_LAI4g_AVHRR_MODIS_consolidated_1982_1990/", full.names = T)
lai_files_11_20 = list.files(path = "./../Trinidad_microclimates/data/PAI/GIMMS_LAI4g_AVHRR_MODIS_consolidated_2011_2020/GIMMS_LAI4g_AVHRR_MODIS_consolidated_2011_2020/", full.names = T)

process_lai = function(lai, v) {
    lai1 = foreach(i = 1:length(lai)) %do% {
      r = rast(lai[i])
      r = crop(r, v)
      
      r[[1]][r[[1]]==65535] = NA
      r[[2]][r[[2]]==65535] = NA
      r[[1]] = r[[1]]*0.001
      
      # quality control - remove pixels with cloud or snow cover
      r[[2]][r[[2]]==12] = NA
      r[[2]][r[[2]]==22] = NA
      r[[2]][r[[2]]==32] = NA
      r[[2]][r[[2]]==42] = NA
      r[[2]][r[[2]]==52] = NA
      
      r = mask(r[[1]], r[[2]])
    }
    lai1 = rast(lai1)
    
    # determine month of each raster
    nms = varnames(lai1)
    nms = strsplit(nms, split = "_")
    nms = sapply(nms, "[[", 4)
    
    # take average PAI per month over the time period
    month = as.numeric(substr(nms, 5,6))
    laisumm = tapp(lai1, index = month, fun = "mean", na.rm = T)
    
  return(lai1)
}

lai_82_90 = process_lai(lai_files_82_90, trin)
lai_11_20 = process_lai(lai_files_11_20, trin)

# caculate difference in monthly PAI between the two time periods
laidiff = lai_11_20 - lai_82_90
laidiff_mean = mean(laidiff, na.rm = T)
plot(laidiff_mean)

ggplot() +
  geom_spatraster(data = laidiff_mean) +
  geom_spatvector(data = trin, fill = NA, color = "black") +
  scale_fill_continuous_divergingx("BrBg", na.value = NA, 
                                   guide = guide_colorbar(title = 'mean LAI difference (present - past)',
                                                          position = "bottom")) +
  theme_classic() +
  theme(legend.title.position = "top",
        legend.key.width = unit(15, "mm"),
        legend.title = element_text(hjust = 0.5),
        panel.background = element_rect(color = "black", fill = NA))
