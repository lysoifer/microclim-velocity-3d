library(tidyverse)
library(ggplot2)
library(patchwork)
library(viridis)
library(colorspace)
library(terra)
library(data.table)
library(Directional)

tmax.macro = rast("scripts/02_climate_velocity/output/macroclimate/mean_monthly_max_temp/vocc.tif", lyrs = c("vocc", "xyAng"))
tmin.macro = rast("scripts/02_climate_velocity/output/macroclimate/temp_bio6/vocc.tif", lyrs = c("vocc", "xyAng"))
macro = c(tmax.macro, tmin.macro)
names(macro) = c("tmax.vocc", "tmax.xyAng", "tmin.vocc", "tmin.xyAng")
macro = as.data.frame(macro, xy = T) %>% 
  drop_na() %>% 
  filter_all(all_vars(!is.infinite(.)))

tmax.meso = rast("scripts/02_climate_velocity/output/mesoclimate/avg_daily_max_temp/vocc.tif", lyrs = c("vocc", "xyAng"))
tmin.meso = rast("scripts/02_climate_velocity/output/mesoclimate/temp_bio6/vocc.tif", lyrs = c("vocc", "xyAng"))
meso = c(tmax.meso, tmin.meso)
names(meso) = c("tmax.vocc", "tmax.xyAng", "tmin.vocc", "tmin.xyAng")
meso = as.data.frame(meso, xy = T) %>% 
  drop_na() %>% 
  filter_all(all_vars(!is.infinite(.)))

tmax.micro2d = rast("scripts/02_climate_velocity/output/2D/avg_daily_maxTemp/vocc_02m.tif", lyrs = c("vocc", "xyAng"))
tmin.micro2d = rast("scripts/02_climate_velocity/output/2D/temp_bio6/vocc_02m.tif", lyrs = c("vocc", "xyAng"))
micro2d = c(tmax.micro2d, tmin.micro2d)
names(micro2d) = c("tmax.vocc", "tmax.xyAng", "tmin.vocc", "tmin.xyAng")
micro2d = as.data.frame(micro2d, xy = T) %>% 
  drop_na() %>% 
  filter_all(all_vars(!is.infinite(.)))


tmax.micro2d.100m = rast("scripts/02_climate_velocity/output/2D/avg_daily_maxTemp/aggregated_100m/vocc.tif", lyrs = c("vocc", "xyAng"))
tmin.micro2d.100m = rast("scripts/02_climate_velocity/output/2D/temp_bio6/aggregated_100m/vocc.tif", lyrs = c("vocc", "xyAng"))
micro2d.100m = c(tmax.micro2d.100m, tmin.micro2d.100m)
names(micro2d.100m) = c("tmax.vocc", "tmax.xyAng", "tmin.vocc", "tmin.xyAng")
micro2d.100m = as.data.frame(micro2d.100m, xy = T) %>% 
  drop_na() %>% 
  filter_all(all_vars(!is.infinite(.)))

tmax.micro2d.1km = rast("scripts/02_climate_velocity/output/2D/avg_daily_maxTemp/aggregated_1km/vocc.tif", lyrs = c("vocc", "xyAng"))
tmin.micro2d.1km = rast("scripts/02_climate_velocity/output/2D/temp_bio6/aggregated_1km/vocc.tif", lyrs = c("vocc", "xyAng"))
micro2d.1km = c(tmax.micro2d.1km, tmin.micro2d.1km)
names(micro2d.1km) = c("tmax.vocc", "tmax.xyAng", "tmin.vocc", "tmin.xyAng")
micro2d.1km = as.data.frame(micro2d.1km, xy = T) %>% 
  drop_na() %>% 
  filter_all(all_vars(!is.infinite(.)))

macro.minmax.cor = circ.cor1(macro$tmax.xyAng, macro$tmin.xyAng, rads = F)
meso.minmax.cor = circ.cor1(meso$tmax.xyAng, meso$tmin.xyAng, rads = F)
micro2d.minmax.cor = circ.cor1(micro2d$tmax.xyAng, micro2d$tmin.xyAng, rads = F)
micro2d.100m.minmax.cor = circ.cor1(micro2d.100m$tmax.xyAng, micro2d.100m$tmin.xyAng, rads = F)
micro2d.1km.minmax.cor = circ.cor1(micro2d.1km$tmax.xyAng, micro2d.1km$tmin.xyAng, rads = F)

minmax.cor = data.frame(scale = c("macro", "meso", "micro2d", "micro2d.100m", "micro2d.1km"),
                        cor = c(round(macro.minmax.cor[1], 2), round(meso.minmax.cor[1], 2),
                                round(micro2d.minmax.cor[1], 2), round(micro2d.100m.minmax.cor[1], 2),
                                round(micro2d.1km.minmax.cor[1], 2)))

# Functions
# calculates difference between two angles ( always positive)
angDiff <- function(degreeA = 0, degreeB = 0) {
  abs((degreeA - degreeB + 180) %% 360 - 180)
}

macro$angdiff = angDiff(macro$tmax.xyAng, macro$tmin.xyAng)
meso$angdiff = angDiff(meso$tmax.xyAng, meso$tmin.xyAng)
micro2d$angdiff = angDiff(micro2d$tmax.xyAng, micro2d$tmin.xyAng)
micro2d.100m$angdiff = angDiff(micro2d.100m$tmax.xyAng, micro2d.100m$tmin.xyAng)
micro2d.1km$angdiff = angDiff(micro2d.1km$tmax.xyAng, micro2d.1km$tmin.xyAng)

macro$scale = "macro"
meso$scale = "meso"
micro2d$scale = "micro2d"
micro2d.100m$scale = "micro2d.100m"
micro2d.1km$scale = "micro2d.1km"

minmax.angdiff = bind_rows(macro, meso, micro2d, micro2d.100m, micro2d.1km)

minmax.angdif.summ = minmax.angdiff %>% 
  mutate(angle = cut(angdiff, breaks = seq(0,180,6), labels = seq(6,180,6),
                      include.lowest = T)) %>% 
  group_by(scale, angle) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n),
         angle = as.numeric(as.character(angle)))

minmax.angdif.plt = ggplot(minmax.angdif.summ, aes(x = angle, y = freq)) +
  geom_bar(stat = "identity") + 
  scale_x_continuous("Angular difference between direction of maximum\nand minimum temperature velocities", breaks = seq(0,180,45)) +
  scale_y_continuous("Proportion") +
  facet_wrap(~scale, nrow = 1) +
  theme_classic()




tmax = fread("scripts/03_analysis/00_dataframes/analysis_dataframe_full_maxtemp.csv")
tmax = tmax %>% 
  dplyr::select(x,y,vocc,xyAng, var, scale, resolution)
colnames(tmax)[1:4] = c("x", "y", "vocc.tmax", "xyAng.tmax")

tmin = fread("scripts/03_analysis/00_dataframes/analysis_dataframe_full_mintemp.csv")
tmin = tmin %>% 
  dplyr::select(x,y,vocc,xyAng, var, scale, resolution)
colnames(tmin)[1:4] = c("x", "y", "vocc.tmin", "xyAng.tmin")

df = inner_join(tmax, tmin, by = c("x", "y", "scale", "resolution"))

