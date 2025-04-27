library(rgl)
library(terra)
library(tidyterra)

# micro2d = rast("data/microclim_3D/mosaics/meanTmax_pres_02.tif")
# meso = rast("data/microclima_100m/present/tmax.tif")
# macro = rast("data/chelsa/tasmax2015_meanMonthly.tif")
# micro3d.05 = rast("data/microclim_3D/mosaics/meanTmax_past_05.tif")
# micro3d.10 = rast("data/microclim_3D/mosaics/meanTmax_past_10.tif")
# micro3d.15 = rast("data/microclim_3D/mosaics/meanTmax_past_15.tif")
# micro3d.20 = rast("data/microclim_3D/mosaics/meanTmax_past_20.tif")

micro2d.100m = rast("data/microclim_3D/mosaics/aggregated_100m/meanTmax_pres_02.tif")
airtemp.100m = rast("data/microclima_100m/present/tmax.tif")
micro3d.100m.05 = rast("data/microclim_3D/mosaics/aggregated_100m/meanTmax_pres_05.tif")
micro3d.100m.10 = rast("data/microclim_3D/mosaics/aggregated_100m/meanTmax_pres_10.tif")
micro3d.100m.15 = rast("data/microclim_3D/mosaics/aggregated_100m/meanTmax_pres_15.tif")
micro3d.100m.20 = rast("data/microclim_3D/mosaics/aggregated_100m/meanTmax_pres_20.tif")

e = ext(-61.4,-61.375,10.7,10.725)
# macro.crop = crop(macro, e)
# meso.crop = crop(meso, project(e, from = "epsg:4326", to = "epsg:2067"))
# micro.crop = crop(micro2d, project(e, from = "epsg:4326", to = "epsg:2067"))/100
# micro3d.05.crop = crop(micro3d.05, project(e, from = "epsg:4326", to = "epsg:2067"))/100
# micro3d.10.crop = crop(micro3d.10, project(e, from = "epsg:4326", to = "epsg:2067"))/100
# micro3d.15.crop = crop(micro3d.15, project(e, from = "epsg:4326", to = "epsg:2067"))/100
# micro3d.20.crop = crop(micro3d.20, project(e, from = "epsg:4326", to = "epsg:2067"))/100

airtemp.100m.crop = crop(airtemp.100m, project(e, from = "epsg:4326", to = "epsg:2067"))
micro2d.100m.crop = crop(micro2d.100m, project(e, from = "epsg:4326", to = "epsg:2067"))
micro3d.100m.05.crop = crop(micro3d.100m.05, project(e, from = "epsg:4326", to = "epsg:2067"))
micro3d.100m.10.crop = crop(micro3d.100m.10, project(e, from = "epsg:4326", to = "epsg:2067"))
micro3d.100m.15.crop = crop(micro3d.100m.15, project(e, from = "epsg:4326", to = "epsg:2067"))
micro3d.100m.20.crop = crop(micro3d.100m.20, project(e, from = "epsg:4326", to = "epsg:2067"))


library(ggplot2)
library(tidyterra)
library(colorspace)

## Color scale ##
macro.df = macro.crop %>% 
  project(crs(micro.crop)) %>% 
  resample(micro.crop) %>% 
  as.data.frame()
meso.df = meso.crop %>% 
  resample(micro.crop) %>% 
  as.data.frame()
micro2d.df = micro.crop %>% as.data.frame



v = c(macro.df$mean, meso.df$layer, micro2d.df$max)
q = quantile(v, probs = seq(0,1,0.05))

maps = c(airtemp.100m.crop, micro2d.100m.crop, micro3d.100m.05.crop,
            micro3d.100m.10.crop, micro3d.100m.15.crop, micro3d.100m.20.crop)
names(maps) = c("Free-air", "Land surface", "Within-canopy 5m",
                "Within-canopy 10m", "Within-canopy 15m", "Within-canopy 20m")

p = ggplot() +
  geom_spatraster(data = maps) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  facet_wrap(~lyr) + 
  scale_fill_continuous_sequential("Viridis", rev = F, guide = guide_colorbar("Temp"))+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())
  
# Get number of unique facets
n_facets <- 6

# Loop to save each one
for (i in 1:6) {
  p_i <- p +
    ggforce::facet_wrap_paginate(~lyr, ncol = 1, nrow = 1, page = i)
  
  ggsave(filename = paste0("scripts/04_plot_code/concept_fig/100m_maps/facet_", i, ".svg"), plot = p_i, width = 5, height = 5)
}


# Continent reference --------------------------------------------------------------

world = vect("data/world_continents/World_Continents_-8398826466908339531/World_Continents.shp")
world = world %>% 
  filter(CONTINENT == "South America" | CONTINENT == "North America")
world = project(world, "epsg:4326")
worldcrop = crop(world, ext(-95, -51,0, 22.4))
box = vect(ext(-62, -60.8, 10, 10.9), crs = "epsg:4326")

p = ggplot() +
  geom_spatvector(data = worldcrop, fill = "gray", color = "black") +
  geom_spatvector(data = box, fill = NA, color = "red3", linewidth = 1) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

svg("scripts/04_plot_code/concept_fig/world_inset.svg", width = 5, height = 4)
p
dev.off()


# Trinidad plot -----------------------------------------------------------

trin = vect("data/gadm_trinidad/gadm/gadm41_TTO_0_pk.rds")
trin = crop(trin, ext(-61.93014, -60.8, 10.04292, 10.9))
micro2d.100m = rast("data/microclim_3D/mosaics/aggregated_100m/meanTmax_pres_02.tif")
pai = rast("data/PAI/pai_NRange_rmMeters0to2_rmPAIgt10.tif")

p = ggplot() +
  geom_spatvector(data = trin, fill = "gray", color = NA) +
  geom_spatraster(data = pai) +
  geom_spatvector(data = trin, fill = NA, color = "black") +
  scale_fill_continuous_sequential("Greens", rev = T, guide = guide_colorbar("PAI"), na.value = NA) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.1,0.65),
        legend.key = element_blank(),
        legend.key.height = unit(5, "mm"),
        legend.key.width = unit(2, "mm"),
        legend.background = element_blank())

png("scripts/04_plot_code/concept_fig/trinidad_PAI.png", width = 3, height = 2, units = "in", res = 1000)
p
dev.off()



# macro.crop = macro.crop %>% 
#   as.data.frame(xy = T) %>% 
#   mutate(brks = cut(mean, breaks = q, include.lowest = T),
#          val = cut(mean, breaks = q, labels = F, include.lowest = T)) %>% 
#   rast(crs = crs(macro.crop))
# 
# macro.plot = ggplot() +
#   geom_spatraster(data = macro.crop, aes(fill = val)) +
#   scale_x_continuous(expand = c(0,0)) +
#   scale_y_continuous(expand = c(0,0)) +
#   scale_fill_continuous_sequential("Viridis", rev = F, guide = guide_colorbar("Temp"),
#                                    breaks = c(1,6,11,16,20), 
#                                    limits = c(1,20),
#                                    labels = c(round(q[c(1,6,11, 16)], 1), paste0("\u2265", round(q[20], 1)))) +
#   theme(axis.text = element_blank(),
#         axis.ticks = element_blank())
# 
# png(filename = "scripts/04_plot_code/concept_fig/macro_crop.png", width = 800, height = 800, res = 300)
# macro.plot
# dev.off()

# meso.crop = meso.crop %>% 
#   as.data.frame(xy = T) %>% 
#   mutate(brks = cut(layer, breaks = q, include.lowest = T),
#          val = cut(layer, breaks = q, labels = F, include.lowest = T)) %>% 
#   rast(crs = crs(meso.crop))
# 
# meso.plot = ggplot() +
#   geom_spatraster(data = meso.crop, aes(fill = val)) +
#   scale_x_continuous(expand = c(0,0)) +
#   scale_y_continuous(expand = c(0,0)) +
#   scale_fill_continuous_sequential("Viridis", rev = F, guide = guide_colorbar("Temp"),
#                                    breaks = c(1,6,11,16,20), 
#                                    limits = c(1,20),
#                                    labels = c(round(q[c(1,6,11, 16)], 1), paste0("\u2265", round(q[20], 1)))) +
#   theme(axis.text = element_blank(),
#         axis.ticks = element_blank())
# png(filename = "scripts/04_plot_code/concept_fig/meso_crop.png", res = 300, width = 800, height = 800)
# meso.plot
# dev.off()
# 
# 
# micro.crop = micro.crop %>% 
#   as.data.frame(xy = T) %>% 
#   mutate(brks = cut(max, breaks = q, include.lowest = T),
#          val = cut(max, breaks = q, labels = F, include.lowest = T)) %>% 
#   rast(crs = crs(micro.crop))
# 
# micro = ggplot() +
#   geom_spatraster(data = micro.crop, aes(fill = val)) +
#   scale_x_continuous(expand = c(0,0)) +
#   scale_y_continuous(expand = c(0,0)) +
#   scale_fill_continuous_sequential(
#     "Viridis", rev = F, na.value = NA, 
#     guide = guide_colorbar("Temp"),
#     breaks = c(1,6,11,16,20), 
#     limits = c(1,20),
#     labels = c(round(q[c(1,6,11, 16)], 1), paste0("\u2265", round(q[20], 1)))) +
#   theme(axis.text = element_blank(),
#         axis.ticks = element_blank())
# png(filename = "scripts/04_plot_code/concept_fig/micro2d_crop.png", res = 300, width = 800, height = 800)
# micro
# dev.off()
# 
# 
# micro3d.05.crop = micro3d.05.crop %>% 
#   as.data.frame(xy = T) %>% 
#   mutate(brks = cut(max, breaks = q, include.lowest = T),
#          val = cut(max, breaks = q, labels = F, include.lowest = T)) %>% 
#   rast(crs = crs(micro3d.05.crop))
# 
# micro.05 = ggplot() +
#   geom_spatraster(data = micro3d.05.crop, aes(fill = val)) +
#   scale_x_continuous(expand = c(0,0)) +
#   scale_y_continuous(expand = c(0,0)) +
#   scale_fill_continuous_sequential(
#     "Viridis", rev = F, na.value = NA, 
#     guide = guide_colorbar("Temp"),
#     breaks = c(1,6,11,16,20), 
#     limits = c(1,20),
#     labels = c(round(q[c(1,6,11, 16)], 1), paste0("\u2265", round(q[20], 1)))) +
#   theme(axis.text = element_blank(),
#         axis.ticks = element_blank())
# png(filename = "scripts/04_plot_code/concept_fig/micro3d_05_crop.png", res = 300, width = 800, height = 800)
# micro.05
# dev.off()
# 
# 
# micro3d.10.crop = micro3d.10.crop %>% 
#   as.data.frame(xy = T) %>% 
#   mutate(brks = cut(max, breaks = q, include.lowest = T),
#          val = cut(max, breaks = q, labels = F, include.lowest = T)) %>% 
#   rast(crs = crs(micro3d.10.crop))
# 
# micro.10 = ggplot() +
#   geom_spatraster(data = micro3d.10.crop, aes(fill = val)) +
#   scale_x_continuous(expand = c(0,0)) +
#   scale_y_continuous(expand = c(0,0)) +
#   scale_fill_continuous_sequential(
#     "Viridis", rev = F, na.value = NA, 
#     guide = guide_colorbar("Temp"),
#     breaks = c(1,6,11,16,20), 
#     limits = c(1,20),
#     labels = c(round(q[c(1,6,11, 16)], 1), paste0("\u2265", round(q[20], 1)))) +
#   theme(axis.text = element_blank(),
#         axis.ticks = element_blank())
# png(filename = "scripts/04_plot_code/concept_fig/micro3d_10_crop.png", res = 300, width = 800, height = 800)
# micro.10
# dev.off()
# 
# 
# micro3d.15.crop = micro3d.15.crop %>% 
#   as.data.frame(xy = T) %>% 
#   mutate(brks = cut(max, breaks = q, include.lowest = T),
#          val = cut(max, breaks = q, labels = F, include.lowest = T)) %>% 
#   rast(crs = crs(micro3d.15.crop))
# 
# micro.15 = ggplot() +
#   geom_spatraster(data = micro3d.15.crop, aes(fill = val)) +
#   scale_x_continuous(expand = c(0,0)) +
#   scale_y_continuous(expand = c(0,0)) +
#   scale_fill_continuous_sequential(
#     "Viridis", rev = F, na.value = NA, 
#     guide = guide_colorbar("Temp"),
#     breaks = c(1,6,11,16,20), 
#     limits = c(1,20),
#     labels = c(round(q[c(1,6,11, 16)], 1), paste0("\u2265", round(q[20], 1)))) +
#   theme(axis.text = element_blank(),
#         axis.ticks = element_blank())
# png(filename = "scripts/04_plot_code/concept_fig/micro3d_15_crop.png", res = 300, width = 800, height = 800)
# micro.15
# dev.off()
# 
# 
# micro3d.20.crop = micro3d.20.crop %>% 
#   as.data.frame(xy = T) %>% 
#   mutate(brks = cut(max, breaks = q, include.lowest = T),
#          val = cut(max, breaks = q, labels = F, include.lowest = T)) %>% 
#   rast(crs = crs(micro3d.20.crop))
# 
# micro.20 = ggplot() +
#   geom_spatraster(data = micro3d.20.crop, aes(fill = val)) +
#   scale_x_continuous(expand = c(0,0)) +
#   scale_y_continuous(expand = c(0,0)) +
#   scale_fill_continuous_sequential(
#     "Viridis", rev = F, na.value = NA, 
#     guide = guide_colorbar("Temp"),
#     breaks = c(1,6,11,16,20), 
#     limits = c(1,20),
#     labels = c(round(q[c(1,6,11, 16)], 1), paste0("\u2265", round(q[20], 1)))) +
#   theme(axis.text = element_blank(),
#         axis.ticks = element_blank())
# png(filename = "scripts/04_plot_code/concept_fig/micro3d_20_crop.png", res = 300, width = 800, height = 800)
# micro.20
# dev.off()
# 
