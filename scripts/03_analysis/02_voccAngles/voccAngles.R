library(tidyverse)
library(ggplot2)
library(patchwork)
library(viridis)
library(colorspace)
library(terra)
library(data.table)

# Read in data
macro = rast("scripts/02_climate_velocity/output/macroclimate/mean_monthly_max_temp/vocc.tif", lyrs = "xyAng")
meso = rast("scripts/02_climate_velocity/output/mesoclimate/avg_daily_max_temp/vocc.tif", lyrs = "xyAng")
micro2d = rast("scripts/02_climate_velocity/output/2D/avg_daily_maxTemp/vocc_02m.tif", lyrs = "xyAng")
micro2d.100m = rast("scripts/02_climate_velocity/output/2D/avg_daily_maxTemp/aggregated_100m/vocc.tif", lyrs = "xyAng")
micro2d.1km = rast("scripts/02_climate_velocity/output/2D/avg_daily_maxTemp/aggregated_1km/vocc.tif", lyrs = "xyAng")

dem.micro = rast('data/topography/dem_reproj.tif')
dem.micro = extend(dem.micro, 1)
dem.meso = resample(dem.micro, meso)
dem.macro = resample(dem.micro, macro)
dem.micro2d.1km = resample(dem.micro, micro2d.1km)

aspect = rast("data/topography/apr.tif")
aspect = extend(aspect, 1)
aspect.micro = aspect * 180/pi
aspect.meso = terrain(dem.meso, v = "aspect", unit = "degrees")
aspect.macro = terrain(dem.macro, v = "aspect", unit = "degrees")
aspect.micro2d.1km = terrain(dem.micro2d.1km, v = "aspect", unit = "degrees")

macro.paidir = rast("scripts/03_analysis/02_voccAngles/pai_direction_macro.tif")
macro = c(macro, macro.paidir, aspect.macro)
names(macro) = c("xyAng", "pai.dir", "aspect")
macro = as.data.frame(macro, xy = T) %>% 
  drop_na() %>% 
  filter_all(all_vars(!is.infinite(.)))

# for aggregated microclimate
micro2d.1km.paidir = rast("scripts/03_analysis/02_voccAngles/pai_direction_macro_aggregated.tif")
micro2d.1km = c(micro2d.1km, micro2d.1km.paidir, aspect.micro2d.1km)
names(micro2d.1km) = c("xyAng", "pai.dir", "aspect")
micro2d.1km = as.data.frame(micro2d.1km, xy = T) %>% 
  drop_na() %>% 
  filter_all(all_vars(!is.infinite(.)))

# topo climate
meso.paidir = rast("scripts/03_analysis/02_voccAngles/pai_direction_meso.tif")
meso = c(meso, meso.paidir, aspect.meso)
names(meso) = c("xyAng", "pai.dir", "aspect")
meso = as.data.frame(meso, xy = T) %>% 
  drop_na()%>% 
  filter_all(all_vars(!is.infinite(.)))

# micro2d.100m
micro2d.100m.paidir = rast("scripts/03_analysis/02_voccAngles/pai_direction_meso.tif")
micro2d.100m = crop(micro2d.100m, micro2d.100m.paidir)
micro2d.100m.paidir = crop(micro2d.100m.paidir, micro2d.100m)
dem.micro2d.100m = resample(dem.micro, micro2d.100m)
aspect.micro2d.100m = terrain(dem.micro2d.100m, v = "aspect", unit = "degrees")
micro2d.100m = c(micro2d.100m, micro2d.100m.paidir, aspect.micro2d.100m)
names(micro2d.100m) = c("xyAng", "pai.dir", "aspect")
micro2d.100m = as.data.frame(micro2d.100m, xy = T) %>% 
  drop_na()%>% 
  filter_all(all_vars(!is.infinite(.)))


micro.paidir = rast("scripts/03_analysis/02_voccAngles/pai_direction_micro.tif")
micro.paidir = extend(micro.paidir, 1)
micro2d = c(micro2d, micro.paidir, aspect.micro)
names(micro2d) = c("xyAng", "pai.dir", "aspect")
micro2d = as.data.frame(micro2d, xy = T) %>% 
  drop_na() %>% 
  filter_all(all_vars(!is.infinite(.)))

# micro3d
micro3d = fread("scripts/03_analysis/00_dataframes/micro3d_canopy_dataframe_tmax_20m.csv")
micro.paidir = as.data.frame(micro.paidir, xy = T)
colnames(micro.paidir) = c("x", "y", "pai.dir")
micro3d = left_join(micro3d, micro.paidir, by = c("x", "y")) %>% 
  drop_na() %>% 
  filter_all(all_vars(!is.infinite(.)))  

# micro3d.100m
micro3d.100m = fread("scripts/03_analysis/00_dataframes/micro3d_canopy_dataframe_tmax_100m.csv")
micro3d.paidir.100m = as.data.frame(micro2d.100m.paidir, xy = T)
colnames(micro3d.paidir.100m) = c("x", "y", "pai.dir")
micro3d.100m = left_join(micro3d.100m, micro3d.paidir.100m, by = c("x", "y")) %>% 
  drop_na() %>% 
  filter_all(all_vars(!is.infinite(.)))  

micro3d.1km = fread("scripts/03_analysis/00_dataframes/micro3d_canopy_dataframe_tmax_1km.csv")
micro3d.paidir.1km = as.data.frame(micro2d.1km.paidir, xy = T)
colnames(micro3d.paidir.1km) = c("x", "y", "pai.dir")
micro3d.1km = left_join(micro3d.1km, micro3d.paidir.1km, by = c("x", "y")) %>% 
  drop_na() %>% 
  filter_all(all_vars(!is.infinite(.)))  

# Functions
# calculates difference between two angles ( always positive)
angDiff <- function(degreeA = 0, degreeB = 0) {
  abs((degreeA - degreeB + 180) %% 360 - 180)
}

## ---------------------------------------------------------------------------##
# add upslope direction (i.e., direction you would have to move to go up slope)
# For example, if aspect is 180  (South), you would have to move north (0 deg) to go upslope


# calculate upslope angle difference and pai angle difference
# i.e., dif between moving upslope and moving toward denser vegetation
# dif of zero indicates movement upslope or movement toward denser vegetation


macro = macro %>% 
  mutate(upslope.dir = ifelse(aspect >=180, aspect-180, aspect+180),
         upslope.angdif= angDiff(xyAng, upslope.dir),
         pai.angdif = angDiff(xyAng, pai.dir),
         resolution = "1km",
         scale = "Free-air")
  
meso = meso %>% 
  mutate(upslope.dir = ifelse(aspect >=180, aspect-180, aspect+180),
         upslope.angdif= angDiff(xyAng, upslope.dir),
         pai.angdif = angDiff(xyAng, pai.dir),
         resolution = "100m",
         scale = "Free-air")

micro2d = micro2d %>% 
  mutate(upslope.dir = ifelse(aspect >=180, aspect-180, aspect+180),
         upslope.angdif= angDiff(xyAng, upslope.dir),
         pai.angdif = angDiff(xyAng, pai.dir),
         resolution = "20m",
         scale = "Land surface")

micro2d.100m = micro2d.100m %>% 
  mutate(upslope.dir = ifelse(aspect >=180, aspect-180, aspect+180),
         upslope.angdif= angDiff(xyAng, upslope.dir),
         pai.angdif = angDiff(xyAng, pai.dir),
         resolution = "100m",
         scale = "Land surface")

micro2d.1km = micro2d.1km %>% 
  mutate(upslope.dir = ifelse(aspect >=180, aspect-180, aspect+180),
         upslope.angdif= angDiff(xyAng, upslope.dir),
         pai.angdif = angDiff(xyAng, pai.dir),
         resolution = "1km",
         scale = "Land surface")

micro3d = micro3d %>% 
  mutate(upslope.dir = ifelse(aspect >=180, aspect-180, aspect+180),
         upslope.angdif= angDiff(xyAng, upslope.dir),
         pai.angdif = angDiff(xyAng, pai.dir),
         resolution = "20m",
         scale = "Within-canopy")

micro3d.100m = micro3d.100m %>% 
  mutate(upslope.dir = ifelse(aspect >=180, aspect-180, aspect+180),
         upslope.angdif= angDiff(xyAng, upslope.dir),
         pai.angdif = angDiff(xyAng, pai.dir),
         resolution = "100m",
         scale = "Within-canopy")

micro3d.1km = micro3d.1km %>% 
  mutate(upslope.dir = ifelse(aspect >=180, aspect-180, aspect+180),
         upslope.angdif= angDiff(xyAng, upslope.dir),
         pai.angdif = angDiff(xyAng, pai.dir),
         resolution = "1km",
         scale = "Within-canopy")

# Circular correlation ----------------------------------------------------

library(Directional) # run in r4.4.0

micro2d.pai.cor = circ.cor1(micro2d$xyAng, micro2d$pai.dir, rads = F)
micro2d.upslope.cor = circ.cor1(micro2d$xyAng, micro2d$upslope.dir, rads = F)

micro2d.100m.pai.cor = circ.cor1(micro2d.100m$xyAng, micro2d.100m$pai.dir, rads = F)
micro2d.100m.upslope.cor = circ.cor1(micro2d.100m$xyAng, micro2d.100m$upslope.dir, rads = F)

micro2d.1km.pai.cor = circ.cor1(micro2d.1km$xyAng, micro2d.1km$pai.dir, rads = F)
micro2d.1km.upslope.cor = circ.cor1(micro2d.1km$xyAng, micro2d.1km$upslope.dir, rads = F)

micro3d.pai.cor = circ.cor1(micro3d$xyAng, micro3d$pai.dir, rads = F)
micro3d.upslope.cor = circ.cor1(micro3d$xyAng, micro3d$upslope.dir, rads = F)

micro3d.100m.pai.cor = circ.cor1(micro3d.100m$xyAng, micro3d.100m$pai.dir, rads = F)
micro3d.100m.upslope.cor = circ.cor1(micro3d.100m$xyAng, micro3d.100m$upslope.dir, rads = F)

micro3d.1km.pai.cor = circ.cor1(micro3d.1km$xyAng, micro3d.1km$pai.dir, rads = F)
micro3d.1km.upslope.cor = circ.cor1(micro3d.1km$xyAng, micro3d.1km$upslope.dir, rads = F)

meso.pai.cor = circ.cor1(meso$xyAng, meso$pai.dir, rads = F)
meso.upslope.cor = circ.cor1(meso$xyAng, meso$upslope.dir, rads = F)

macro.pai.cor = circ.cor1(macro$xyAng, macro$pai.dir, rads = F)
macro.upslope.cor = circ.cor1(macro$xyAng, macro$upslope.dir, rads = F)

paicor.df = data.frame(cor = c(macro.pai.cor[1], micro2d.1km.pai.cor[1], micro3d.1km.pai.cor[1],
                               meso.pai.cor[1], micro2d.100m.pai.cor[1], micro3d.100m.pai.cor[1],
                               micro2d.pai.cor[1], micro3d.pai.cor[1]),
                       scale = c("macro", "micro2d.1km", "micro3d.1km",
                                 "meso", "micro2d.100m", "micro3d.100m",
                                 "micro2d.20m", "micro3d.20m")) %>% 
  mutate(cor = round(cor, 3))

upslopecor.df = data.frame(cor = c(macro.upslope.cor[1], micro2d.1km.upslope.cor[1], micro3d.1km.upslope.cor[1],
                                   meso.upslope.cor[1], micro2d.100m.upslope.cor[1], micro3d.100m.upslope.cor[1],
                                   micro2d.upslope.cor[1], micro3d.upslope.cor[1]),
                       scale = c("macro", "micro2d.1km", "micro3d.1km",
                                 "meso", "micro2d.100m", "micro3d.100m",
                                 "micro2d.20m", "micro3d.20m")) %>% 
  mutate(cor = round(cor, 3))

micro3d = micro3d %>% 
  dplyr::select(x, y, xyAng, pai.dir, aspect, upslope.dir, upslope.angdif, pai.angdif, resolution, scale)

micro3d.100m = micro3d.100m %>% 
  dplyr::select(x, y, xyAng, pai.dir, aspect, upslope.dir, upslope.angdif, pai.angdif, resolution, scale)

micro3d.1km = micro3d.1km %>% 
  dplyr::select(x, y, xyAng, pai.dir, aspect, upslope.dir, upslope.angdif, pai.angdif, resolution, scale)

pts.scale = rbind(micro3d, micro3d.100m, micro3d.1km, micro2d, micro2d.100m, micro2d.1km, meso, macro) %>% 
  mutate(resolution = factor(resolution, levels = c("1km", "100m", "20m")),
    scale = factor(scale, levels = c("Free-air", "Land surface", "Within-canopy")))


upslope.angdif.summ = pts.scale %>% 
  #filter(scale == "Macro") %>% 
  #group_by(scale) %>% 
  mutate(ang.up = cut(upslope.angdif, breaks = seq(0,180,6), labels = seq(6,180,6),
                      include.lowest = T)) %>% 
  group_by(scale, ang.up) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n),
         ang.up = as.numeric(as.character(ang.up)))

pai.angdif.summ = pts.scale %>% 
  mutate(ang.pai = cut(pai.angdif, breaks = seq(0,180,6), labels = seq(6,180,6),
                      include.lowest = T)) %>% 
  group_by(scale, ang.pai) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n),
         ang.pai = as.numeric(as.character(ang.pai)))




# z angle -----------------------------------------------------------------

maxtemp.3d = fread("scripts/03_analysis/00_dataframes/analysis_dataframe_3d_full_maxtemp.csv")
mintemp.3d = fread("scripts/03_analysis/00_dataframes/analysis_dataframe_3d_full_mintemp.csv")

maxtemp.3d$var = "maxtemp"
mintemp.3d$var = "mintemp"

maxtemp.3d = maxtemp.3d %>% rename(temp.pres = maxtemp.pres)
mintemp.3d = mintemp.3d %>% rename(temp.pres = mintemp.pres)


df.3d = rbind(maxtemp.3d, mintemp.3d) %>% 
  mutate(xy_length = abs(cos(zAng*pi/180)), 
         z_length = abs(sin(zAng*pi/180)),
         xyz_ratio = z_length/xy_length,
         resolution = factor(resolution, levels = c("1km", "100m", "20m")))

df3d.wumm

maxtemp.3d = maxtemp.3d %>% 
  group_by(resolution) %>%
  mutate(xy_length = abs(cos(zAng*pi/180)), 
         z_length = abs(sin(zAng*pi/180)),
         xyz_ratio = z_length/xy_length)
maxtemp3d.summ = maxtemp.3d %>% reframe(n = n(),
          perc_down_vect = sum(zAng < 1)/n*100) # percentage of downward pointing vectors (zAng < 0)

mintemp.3d = mintemp.3d %>% 
  group_by(resolution) %>%
  mutate(xy_length = abs(cos(zAng*pi/180)), 
         z_length = abs(sin(zAng*pi/180)),
         xyz_ratio = z_length/xy_length)

mintemp3d.summ = mintemp.3d %>% reframe(n = n(),
          perc_down_vect = sum(zAng < 1)/n*100)

ggplot(maxtemp.3d %>% slice_sample(n=5000), aes(x = log(xyz_ratio), y = relhgt)) +
  geom_point(size = 1, alpha = 0.5) +
  #coord_cartesian(xlim = c(10, 10)) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  scale_x_continuous("ln(vertical length/horizontal length)") +
  scale_y_continuous("Relative canopy height") +
  facet_wrap(~resolution) +
  theme_classic()

ggsave("scripts/03_analysis/00_plots/supplemental_figs/vert_to_horiz_ratio.png", width = 4, height = 4)


# micro3d = fread("scripts/03_analysis/00_dataframes/micro3d_canopy_dataframe_tmax_20m.csv")

# z.df = micro3d %>%
#   mutate(xy_length = abs(cos(zAng*pi/180)), 
#          z_length = abs(sin(zAng*pi/180)),
#          xyz_ratio = z_length/xy_length) # anything greater than one means z movement is longer than xy movement
# 
# down_vectors = sum(z.df$zAng < 0)/nrow(z.df)
# 
# z.df.down = z.df %>% 
#   filter(zAng < 0)
# 
# vh_ratio = sum(abs(z.df$xyz_ratio)>1)/nrow(z.df)

# of downward facing vectors, what proportion have vertical movement greater than horizontal movement
# tmax.vh_ratio.down = sum(abs(z.df.down$xyz_ratio)>1)/nrow(z.df.down)


z.df %>% dplyr::filter(log(xyz_ratio) < -20)


# micro3d.100m

micro3d.100m = fread("scripts/03_analysis/00_dataframes/micro3d_canopy_dataframe_tmax_100m.csv")

z.df = micro3d.100m %>%
  mutate(xy_length = abs(cos(zAng*pi/180)), 
         z_length = abs(sin(zAng*pi/180)),
         xyz_ratio = z_length/xy_length) # anything greater than one means z movement is longer than xy movement

down_vectors = sum(z.df$zAng < 0)/nrow(z.df)

z.df.down = z.df %>% 
  filter(zAng < 0)

vh_ratio = sum(abs(z.df$xyz_ratio)>1)/nrow(z.df)

# of downward facing vectors, what proportion have vertical movement greater than horizontal movement
tmax100m.vh_ratio.down = sum(abs(z.df.down$xyz_ratio)>1)/nrow(z.df.down)

ggplot(z.df %>% slice_sample(n=5000), aes(x = log(xyz_ratio), y = relhgt)) +
  geom_point(size = 1, alpha = 0.5) +
  #coord_cartesian(xlim = c(10, 10)) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  scale_x_continuous("ln(vertical length/horizontal length)") +
  scale_y_continuous("Relative canopy height") +
  theme_classic()

ggsave("scripts/03_analysis/00_plots/supplemental_figs/vert_to_horiz_ratio_100m.png", width = 4, height = 4)


# micro3d 1km

micro3d.1km = fread("scripts/03_analysis/00_dataframes/micro3d_canopy_dataframe_tmax_1km.csv")

z.df = micro3d.1km %>%
  mutate(xy_length = abs(cos(zAng*pi/180)), 
         z_length = abs(sin(zAng*pi/180)),
         xyz_ratio = z_length/xy_length) # anything greater than one means z movement is longer than xy movement

down_vectors = sum(z.df$zAng < 0)/nrow(z.df)

z.df.down = z.df %>% 
  filter(zAng < 0)

vh_ratio = sum(abs(z.df$xyz_ratio)>1)/nrow(z.df)

# of downward facing vectors, what proportion have vertical movement greater than horizontal movement
tmax1km.vh_ratio.down = sum(abs(z.df.down$xyz_ratio)>1)/nrow(z.df.down)

ggplot(z.df %>% slice_sample(n=5000), aes(x = log(xyz_ratio), y = relhgt)) +
  geom_point(size = 1, alpha = 0.5) +
  #coord_cartesian(xlim = c(10, 10)) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  scale_x_continuous("ln(vertical length/horizontal length)") +
  scale_y_continuous("Relative canopy height") +
  theme_classic()

ggsave("scripts/03_analysis/00_plots/supplemental_figs/vert_to_horiz_ratio_1km.png", width = 4, height = 4)

# vertical space ----------------------------------------------------------
# how long does it take to run out of vertical space

# calculate distance travelled in x, y, and z directions after 55 years

# micro3d
# micro3d = fread("03_analysis/00_dataframes/micro3d_canopy_dataframe.csv")
# 
# micro3d[, m55 := vocc*55]
# micro3d[, x55 := m55*cos(zAng*pi/180)*sin(xyAng*pi/180)]
# micro3d[, y55 := m55*cos(zAng*pi/180)*cos(xyAng*pi/180)]
# micro3d[, z55 := m55*sin(zAng*pi/180)]
# micro3d = micro3d %>% 
#   drop_na()
# 
# micro3d.thresh = quantile(micro3d$vocc, probs = 0.95)
# micro3d = micro3d[vocc < micro3d.thresh, ]
# 
# micro3d.down = micro3d[z55 < 0, ]
# maxdown = round(abs(min(micro3d.down$z55)))
# mindown = round(abs(max(micro3d.down$z55)))
# mediandown = round(abs(median(micro3d.down$z55)))
# 
# sum(abs(micro3d.down$z55) > micro3d.down$height)/nrow(micro3d.down)
