library(terra)
library(tidyterra)
library(dplyr)
library(ggplot2)

tmax = rast("data/microclim_3D/mosaics/meanTmax_pres_02.tif")
tmin = rast("./../CleanedCode/01_microclimate_models_update/mosaics/temp_bio6/pres/temp_bio6_pres_02.tif")
pai = rast("data/PAI/pai_NRange_rmMeters0to2_rmPAIgt10.tif")

pai = extend(pai, tmax)

r = rast(list(tmax, tmin, pai))
names(r) = c("tmax", "tmin", "pai")

r = r %>% 
  as.data.frame(r, xy = T) %>% 
  mutate(tmax = tmax/100, tmin = tmin/100) %>% 
  drop_na() %>% 
  sample_n(size = 5000) 

rlong = r %>% 
  pivot_longer(cols = c(tmax, tmin), names_to = "var", values_to = "temp")

ggplot(rlong, aes(x = pai, y = temp, color = var)) +
  geom_point(pch = 20) +
  scale_x_continuous("PAI") +
  scale_y_continuous("Temperature (\u00b0C)") +
  scale_color_manual(values = c("forestgreen", "skyblue3")) +
  guides(colour = guide_legend(override.aes = list(size=5))) +
  theme_classic() +
  theme(legend.title = element_blank())

cor.test(r$tmax, r$tmin, method = "spearman")


# check change over time
tmax_past = rast("data/microclim_3D/mosaics/meanTmax_past_02.tif")/100
tmin_past = rast("./../CleanedCode/01_microclimate_models_update/mosaics/temp_bio6/past/temp_bio6_past_02.tif")/100

tmax_dif = tmax/100 - tmax_past
tmin_dif = tmin/100 - tmin_past

plot(tmax_dif)
ggplot() +
  geom_spatraster(data = tmax_dif) +
  scale_fill_viridis_c(limits = c(-2.5,2.5))
plot(tmin_dif)

global(tmax_dif, mean, na.rm = T)
global(tmin_dif, mean, na.rm = T)

global(tmax_dif, median, na.rm = T)
global(tmin_dif, median, na.rm = T)
