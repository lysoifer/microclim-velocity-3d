library(tidyverse)
library(ggplot2)
library(patchwork)
library(viridis)
library(colorspace)
library(terra)
library(data.table)

# Read in data
tmin.macro = rast("scripts/02_climate_velocity/output/macroclimate/temp_bio6/vocc.tif", lyrs = "xyAng")
tmin.meso = rast("scripts/02_climate_velocity/output/mesoclimate/temp_bio6/vocc.tif", lyrs = "xyAng")
tmin.micro2d = rast("scripts/02_climate_velocity/output/2D/temp_bio6/vocc_02m.tif", lyrs = "xyAng")
tmin.micro2d.100m = rast("scripts/02_climate_velocity/output/2D/temp_bio6/aggregated_100m/vocc.tif", lyrs = "xyAng")
tmin.micro2d.1km = rast("scripts/02_climate_velocity/output/2D/temp_bio6/aggregated_1km/vocc.tif", lyrs = "xyAng")

dem.micro = rast('data/topography/dem_reproj.tif')
dem.micro = extend(dem.micro, 1)
dem.meso = resample(dem.micro, tmin.meso)
dem.macro = resample(dem.micro, tmin.macro)
dem.micro2d.1km = resample(dem.micro, tmin.micro2d.1km)

aspect = rast("data/topography/apr.tif")
aspect = extend(aspect, 1)
aspect.micro = aspect * 180/pi
aspect.meso = terrain(dem.meso, v = "aspect", unit = "degrees")
aspect.macro = terrain(dem.macro, v = "aspect", unit = "degrees")
aspect.micro2d.1km = terrain(dem.micro2d.1km, v = "aspect", unit = "degrees")

macro.paidir = rast("scripts/03_analysis/02_voccAngles/pai_direction_macro.tif")
tmin.macro = c(tmin.macro, macro.paidir, aspect.macro)
names(tmin.macro) = c("xyAng", "pai.dir", "aspect")
tmin.macro = as.data.frame(tmin.macro, xy = T) %>% 
  drop_na() %>% 
  filter_all(all_vars(!is.infinite(.)))

# for aggregated microclimate
micro2d.1km.paidir = rast("scripts/03_analysis/02_voccAngles/pai_direction_macro_aggregated.tif")
tmin.micro2d.1km = c(tmin.micro2d.1km, micro2d.1km.paidir, aspect.micro2d.1km)
names(tmin.micro2d.1km) = c("xyAng", "pai.dir", "aspect")
tmin.micro2d.1km = as.data.frame(tmin.micro2d.1km, xy = T) %>% 
  drop_na() %>% 
  filter_all(all_vars(!is.infinite(.)))

# topo climate
meso.paidir = rast("scripts/03_analysis/02_voccAngles/pai_direction_meso.tif")
tmin.meso = c(tmin.meso, meso.paidir, aspect.meso)
names(tmin.meso) = c("xyAng", "pai.dir", "aspect")
tmin.meso = as.data.frame(tmin.meso, xy = T) %>% 
  drop_na()%>% 
  filter_all(all_vars(!is.infinite(.)))

# micro2d.100m
micro2d.100m.paidir = rast("scripts/03_analysis/02_voccAngles/pai_direction_meso.tif")
tmin.micro2d.100m = crop(tmin.micro2d.100m, micro2d.100m.paidir)
micro2d.100m.paidir = crop(micro2d.100m.paidir, tmin.micro2d.100m)
dem.micro2d.100m = resample(dem.micro, tmin.micro2d.100m)
aspect.micro2d.100m = terrain(dem.micro2d.100m, v = "aspect", unit = "degrees")
tmin.micro2d.100m = c(tmin.micro2d.100m, micro2d.100m.paidir, aspect.micro2d.100m)
names(tmin.micro2d.100m) = c("xyAng", "pai.dir", "aspect")
tmin.micro2d.100m = as.data.frame(tmin.micro2d.100m, xy = T) %>% 
  drop_na()%>% 
  filter_all(all_vars(!is.infinite(.)))


micro.paidir = rast("scripts/03_analysis/02_voccAngles/pai_direction_micro.tif")
micro.paidir = extend(micro.paidir, 1)
tmin.micro2d = c(tmin.micro2d, micro.paidir, aspect.micro)
names(tmin.micro2d) = c("xyAng", "pai.dir", "aspect")
tmin.micro2d = as.data.frame(tmin.micro2d, xy = T) %>% 
  drop_na() %>% 
  filter_all(all_vars(!is.infinite(.)))

# micro3d
tmin.micro3d = fread("scripts/03_analysis/00_dataframes/micro3d_canopy_dataframe_tmin_20m.csv")
micro.paidir = as.data.frame(micro.paidir, xy = T)
colnames(micro.paidir) = c("x", "y", "pai.dir")
tmin.micro3d = left_join(tmin.micro3d, micro.paidir, by = c("x", "y")) %>% 
  drop_na() %>% 
  filter_all(all_vars(!is.infinite(.)))  

# micro3d.100m
tmin.micro3d.100m = fread("scripts/03_analysis/00_dataframes/micro3d_canopy_dataframe_tmin_100m.csv")
micro3d.paidir.100m = as.data.frame(micro2d.100m.paidir, xy = T)
colnames(micro3d.paidir.100m) = c("x", "y", "pai.dir")
tmin.micro3d.100m = left_join(tmin.micro3d.100m, micro3d.paidir.100m, by = c("x", "y")) %>% 
  drop_na() %>% 
  filter_all(all_vars(!is.infinite(.)))  

tmin.micro3d.1km = fread("scripts/03_analysis/00_dataframes/micro3d_canopy_dataframe_tmin_1km.csv")
micro3d.paidir.1km = as.data.frame(micro2d.1km.paidir, xy = T)
colnames(micro3d.paidir.1km) = c("x", "y", "pai.dir")
tmin.micro3d.1km = left_join(tmin.micro3d.1km, micro3d.paidir.1km, by = c("x", "y")) %>% 
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


tmin.macro = tmin.macro %>% 
  mutate(upslope.dir = ifelse(aspect >=180, aspect-180, aspect+180),
         upslope.angdif= angDiff(xyAng, upslope.dir),
         pai.angdif = angDiff(xyAng, pai.dir),
         resolution = "1km",
         scale = "Free-air")

tmin.meso = tmin.meso %>% 
  mutate(upslope.dir = ifelse(aspect >=180, aspect-180, aspect+180),
         upslope.angdif= angDiff(xyAng, upslope.dir),
         pai.angdif = angDiff(xyAng, pai.dir),
         resolution = "100m",
         scale = "Free-air")

tmin.micro2d = tmin.micro2d %>% 
  mutate(upslope.dir = ifelse(aspect >=180, aspect-180, aspect+180),
         upslope.angdif= angDiff(xyAng, upslope.dir),
         pai.angdif = angDiff(xyAng, pai.dir),
         resolution = "20m",
         scale = "Land surface")

tmin.micro2d.100m = tmin.micro2d.100m %>% 
  mutate(upslope.dir = ifelse(aspect >=180, aspect-180, aspect+180),
         upslope.angdif= angDiff(xyAng, upslope.dir),
         pai.angdif = angDiff(xyAng, pai.dir),
         resolution = "100m",
         scale = "Land surface")

tmin.micro2d.1km = tmin.micro2d.1km %>% 
  mutate(upslope.dir = ifelse(aspect >=180, aspect-180, aspect+180),
         upslope.angdif= angDiff(xyAng, upslope.dir),
         pai.angdif = angDiff(xyAng, pai.dir),
         resolution = "1km",
         scale = "Land surface")

tmin.micro3d = tmin.micro3d %>% 
  mutate(upslope.dir = ifelse(aspect >=180, aspect-180, aspect+180),
         upslope.angdif= angDiff(xyAng, upslope.dir),
         pai.angdif = angDiff(xyAng, pai.dir),
         resolution = "20m",
         scale = "Within-canopy")

tmin.micro3d.100m = tmin.micro3d.100m %>% 
  mutate(upslope.dir = ifelse(aspect >=180, aspect-180, aspect+180),
         upslope.angdif= angDiff(xyAng, upslope.dir),
         pai.angdif = angDiff(xyAng, pai.dir),
         resolution = "100m",
         scale = "Within-canopy")

tmin.micro3d.1km = tmin.micro3d.1km %>% 
  mutate(upslope.dir = ifelse(aspect >=180, aspect-180, aspect+180),
         upslope.angdif= angDiff(xyAng, upslope.dir),
         pai.angdif = angDiff(xyAng, pai.dir),
         resolution = "1km",
         scale = "Within-canopy")

# Circular correlation ----------------------------------------------------

library(Directional) # run in r4.4.0

tmin.micro2d.pai.cor = circ.cor1(tmin.micro2d$xyAng, tmin.micro2d$pai.dir, rads = F)
tmin.micro2d.upslope.cor = circ.cor1(tmin.micro2d$xyAng, tmin.micro2d$upslope.dir, rads = F)

tmin.micro2d.100m.pai.cor = circ.cor1(tmin.micro2d.100m$xyAng, tmin.micro2d.100m$pai.dir, rads = F)
tmin.micro2d.100m.upslope.cor = circ.cor1(tmin.micro2d.100m$xyAng, tmin.micro2d.100m$upslope.dir, rads = F)

tmin.micro2d.1km.pai.cor = circ.cor1(tmin.micro2d.1km$xyAng, tmin.micro2d.1km$pai.dir, rads = F)
tmin.micro2d.1km.upslope.cor = circ.cor1(tmin.micro2d.1km$xyAng, tmin.micro2d.1km$upslope.dir, rads = F)

tmin.micro3d.pai.cor = circ.cor1(tmin.micro3d$xyAng, tmin.micro3d$pai.dir, rads = F)
tmin.micro3d.upslope.cor = circ.cor1(tmin.micro3d$xyAng, tmin.micro3d$upslope.dir, rads = F)

tmin.micro3d.100m.pai.cor = circ.cor1(tmin.micro3d.100m$xyAng, tmin.micro3d.100m$pai.dir, rads = F)
tmin.micro3d.100m.upslope.cor = circ.cor1(tmin.micro3d.100m$xyAng, tmin.micro3d.100m$upslope.dir, rads = F)

tmin.micro3d.1km.pai.cor = circ.cor1(tmin.micro3d.1km$xyAng, tmin.micro3d.1km$pai.dir, rads = F)
tmin.micro3d.1km.upslope.cor = circ.cor1(tmin.micro3d.1km$xyAng, tmin.micro3d.1km$upslope.dir, rads = F)

tmin.meso.pai.cor = circ.cor1(tmin.meso$xyAng, tmin.meso$pai.dir, rads = F)
tmin.meso.upslope.cor = circ.cor1(tmin.meso$xyAng, tmin.meso$upslope.dir, rads = F)

tmin.macro.pai.cor = circ.cor1(tmin.macro$xyAng, tmin.macro$pai.dir, rads = F)
tmin.macro.upslope.cor = circ.cor1(tmin.macro$xyAng, tmin.macro$upslope.dir, rads = F)

tmin.paicor.df = data.frame(cor = c(tmin.macro.pai.cor[1], tmin.micro2d.1km.pai.cor[1], tmin.micro3d.1km.pai.cor[1],
                               tmin.meso.pai.cor[1], tmin.micro2d.100m.pai.cor[1], tmin.micro3d.100m.pai.cor[1],
                               tmin.micro2d.pai.cor[1], tmin.micro3d.pai.cor[1]),
                       scale = c("macro", "micro2d.1km", "micro3d.1km",
                                 "meso", "micro2d.100m", "micro3d.100m",
                                 "micro2d.20m", "micro3d.20m")) %>% 
  mutate(cor = round(cor, 3))

tmin.upslopecor.df = data.frame(cor = c(tmin.macro.upslope.cor[1], tmin.micro2d.1km.upslope.cor[1], tmin.micro3d.1km.upslope.cor[1],
                                   tmin.meso.upslope.cor[1], tmin.micro2d.100m.upslope.cor[1], tmin.micro3d.100m.upslope.cor[1],
                                   tmin.micro2d.upslope.cor[1], tmin.micro3d.upslope.cor[1]),
                           scale = c("macro", "micro2d.1km", "micro3d.1km",
                                     "meso", "micro2d.100m", "micro3d.100m",
                                     "micro2d.20m", "micro3d.20m")) %>% 
  mutate(cor = round(cor, 3))

tmin.micro3d = tmin.micro3d %>% 
  dplyr::select(x, y, xyAng, pai.dir, aspect, upslope.dir, upslope.angdif, pai.angdif, resolution, scale)

tmin.micro3d.100m = tmin.micro3d.100m %>% 
  dplyr::select(x, y, xyAng, pai.dir, aspect, upslope.dir, upslope.angdif, pai.angdif, resolution, scale)

tmin.micro3d.1km = tmin.micro3d.1km %>% 
  dplyr::select(x, y, xyAng, pai.dir, aspect, upslope.dir, upslope.angdif, pai.angdif, resolution, scale)

tmin.pts.scale = rbind(tmin.micro3d, tmin.micro3d.100m, tmin.micro3d.1km, 
                       tmin.micro2d, tmin.micro2d.100m, tmin.micro2d.1km, tmin.meso, tmin.macro) %>% 
  mutate(scale = factor(scale, levels = c("Free-air", "Land surface", "Within-canopy")),
         resolution = factor(resolution, levels = c("1km", "100m", "20m")),
         var = "tmin")


tmin.upslope.angdif.summ = tmin.pts.scale %>% 
  #filter(scale == "Macro") %>% 
  #group_by(scale) %>% 
  mutate(ang.up = cut(upslope.angdif, breaks = seq(0,180,6), labels = seq(6,180,6),
                      include.lowest = T)) %>% 
  group_by(scale, ang.up) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n),
         ang.up = as.numeric(as.character(ang.up)))

tmin.pai.angdif.summ = tmin.pts.scale %>% 
  mutate(ang.pai = cut(pai.angdif, breaks = seq(0,180,6), labels = seq(6,180,6),
                       include.lowest = T)) %>% 
  group_by(scale, ang.pai) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n),
         ang.pai = as.numeric(as.character(ang.pai)))




# z angle -----------------------------------------------------------------

tmin.micro3d = fread("scripts/03_analysis/00_dataframes/micro3d_canopy_dataframe_tmin_20m.csv")

tmin.z.df = tmin.micro3d %>%
  mutate(xy_length = abs(cos(zAng*pi/180)), 
         z_length = abs(sin(zAng*pi/180)),
         xyz_ratio = z_length/xy_length) # anything greater than one means z movement is longer than xy movement

tmin.down_vectors = sum(tmin.z.df$zAng < 0)/nrow(tmin.z.df)

tmin.z.df.down = tmin.z.df %>% 
  filter(zAng < 0)

tmin.vh_ratio = sum(abs(tmin.z.df$xyz_ratio)>1)/nrow(tmin.z.df)

# of downward facing vectors, what proportion have vertical movement greater than horizontal movement
tmin.vh_ratio.down = sum(abs(tmin.z.df.down$xyz_ratio)>1)/nrow(tmin.z.df.down)

ggplot(z.df %>% slice_sample(n=5000), aes(x = log(xyz_ratio), y = relhgt)) +
  geom_point(size = 1, alpha = 0.5) +
  #coord_cartesian(xlim = c(10, 10)) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  scale_x_continuous("ln(vertical length/horizontal length)") +
  scale_y_continuous("Relative canopy height") +
  theme_classic()

ggsave("scripts/03_analysis/00_plots/supplemental_figs/vert_to_horiz_ratio_tmin.png", width = 4, height = 4)

z.df %>% dplyr::filter(log(xyz_ratio) < -20)


# micro3d.100m

tmin.micro3d.100m = fread("scripts/03_analysis/00_dataframes/micro3d_canopy_dataframe_tmin_100m.csv")

tmin.z.df = tmin.micro3d.100m %>%
  mutate(xy_length = abs(cos(zAng*pi/180)), 
         z_length = abs(sin(zAng*pi/180)),
         xyz_ratio = z_length/xy_length) # anything greater than one means z movement is longer than xy movement

tmin.down_vectors = sum(tmin.z.df$zAng < 0)/nrow(tmin.z.df)

tmin.z.df.down = tmin.z.df %>% 
  filter(zAng < 0)

#vh_ratio = sum(abs(z.df$xyz_ratio)>1)/nrow(z.df)

# of downward facing vectors, what proportion have vertical movement greater than horizontal movement
tmin100m.vh_ratio.down = sum(abs(tmin.z.df.down$xyz_ratio)>1)/nrow(tmin.z.df.down)

ggplot(z.df %>% slice_sample(n=5000), aes(x = log(xyz_ratio), y = relhgt)) +
  geom_point(size = 1, alpha = 0.5) +
  #coord_cartesian(xlim = c(10, 10)) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  scale_x_continuous("ln(vertical length/horizontal length)") +
  scale_y_continuous("Relative canopy height") +
  theme_classic()

ggsave("scripts/03_analysis/00_plots/supplemental_figs/vert_to_horiz_ratio_100m_tmin.png", width = 4, height = 4)


# micro3d 1km

tmin.micro3d.1km = fread("scripts/03_analysis/00_dataframes/micro3d_canopy_dataframe_tmin_1km.csv")

tmin.z.df = tmin.micro3d.1km %>%
  mutate(xy_length = abs(cos(zAng*pi/180)), 
         z_length = abs(sin(zAng*pi/180)),
         xyz_ratio = z_length/xy_length) # anything greater than one means z movement is longer than xy movement

tmin.down_vectors = sum(tmin.z.df$zAng < 0)/nrow(tmin.z.df)

tmin.z.df.down = tmin.z.df %>% 
  filter(zAng < 0)

#vh_ratio = sum(abs(z.df$xyz_ratio)>1)/nrow(z.df)

# of downward facing vectors, what proportion have vertical movement greater than horizontal movement
tmin1km.vh_ratio.down = sum(abs(tmin.z.df.down$xyz_ratio)>1)/nrow(tmin.z.df.down)

ggplot(z.df %>% slice_sample(n=5000), aes(x = log(xyz_ratio), y = relhgt)) +
  geom_point(size = 1, alpha = 0.5) +
  #coord_cartesian(xlim = c(10, 10)) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  scale_x_continuous("ln(vertical length/horizontal length)") +
  scale_y_continuous("Relative canopy height") +
  theme_classic()

ggsave("scripts/03_analysis/00_plots/supplemental_figs/vert_to_horiz_ratio_1km_tmin.png", width = 4, height = 4)

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
