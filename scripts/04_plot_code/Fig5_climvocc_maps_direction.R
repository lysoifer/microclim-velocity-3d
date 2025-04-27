library(circular)
library(tidyverse)
library(terra)
library(tidyterra)
library(colorspace)
library(ggpubr)
library(cowplot)
library(data.table)
library(patchwork)

vocc.df = fread("scripts/03_analysis/00_dataframes/analysis_dataframe_full_maxtemp.csv")

elev = rast("data/topography/dem_reproj.tif")
pai = rast("data/PAI/pai_NRange_rmMeters0to2_rmPAIgt10.tif")

# macro.vocc = rast("02_climate_velocity/output/macroclimate/mean_monthly_max_temp/vocc.tif")
macro.vocc = vocc.df %>% filter(scale == "Macro") %>% 
  dplyr::select(x, y, vocc, EWdir, NSdir) %>% 
  rast(crs = "epsg:2067")

plot(macro.vocc$vocc)

nrange = vect("data/cropping_polygons/NRange.shp")
nrange = project(nrange, macro.vocc)
nrange = aggregate(nrange)

aripo.large = vect("data/cropping_polygons/Aripo_large/Aripo_large.shp")
aripo.small = vect("data/cropping_polygons/Aripo_small/aripo_small.shp")

macro.vocc = crop(macro.vocc, nrange)
macro.vocc = mask(macro.vocc, nrange)

elev.aripo.large = crop(elev, aripo.large)
names(elev.aripo.large) = "elev"

e = ext(687000,692000,1182000,1185500)

elev.aripo.small = crop(elev.aripo.large, e)
plot(elev.aripo.small)

e.topo = c(689900, 692000, 1184000, 1185500)
elev.topo = crop(elev.aripo.small, e.topo)

pai.aripo.large = crop(pai, aripo.large)
names(pai.aripo.large) = "pai"

pai.aripo.small = crop(pai, aripo.small)
names(pai.aripo.small) = "pai"



pai.small = crop(pai, ext(720000,725000, 1190000,1196000))
pai.small = crop(pai, ext(720200,721500, 1192500,1193500))
names(pai.small) = "pai"
plot(pai.small)
pai.small = vect(ext(pai.small), crs = "epsg:2067")

pai.macrocrop = crop(pai, ext(720000, 724000, 1192000, 1195000))
plot(pai.macrocrop)
pai.macrocrop = vect(ext(pai.macrocrop), crs = "epsg:2067")


# make map inset ----------------------------------------------------------

# large.outline = vect(ext(elev.aripo.large))
# crs(large.outline) = crs(elev.aripo.large)
# 
# small.outline = vect(ext(pai.small))
# crs(small.outline) = crs(pai.small)

outline.elev.macro = vect(ext(elev.aripo.small), crs = "epsg:2067")
outline.elev.small = vect(ext(elev.topo), crs = "epsg:2067")

inset.elev = ggplot() +
  #geom_spatraster(data = elev) +
  geom_spatvector(data = nrange, fill = "gray80", color = "black", linewidth = 1) +
  #geom_spatvector(data = outline.elev.macro, color = "red3", fill = NA, linewidth = 1) +
  geom_spatvector(data = outline.elev.small, color = "blue", fill = NA, linewidth = 1) +
  #scale_fill_continuous_sequential("Grays", na.value = NA, limits = c(0,1000)) +
  scale_x_continuous(expand = c(0,0)) +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(color = "black"))

inset.pai = ggplot() +
  #geom_spatraster(data = pai) +
  geom_spatvector(data = nrange, fill = "gray80", color = "black", linewidth = 1) +
  #geom_spatvector(data = pai.macrocrop, color = "red3", fill = NA, linewidth = 1) +
  geom_spatvector(data = pai.small, color = "blue", fill = NA, linewidth = 1) +
  scale_fill_continuous_sequential("Terrain 2", na.value = NA, limits = c(0,10)) +
  scale_x_continuous(expand = c(0,0)) +
  theme_classic() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(color = "black"))


# direction plots ---------------------------------------------------------


# * - macro ---------------------------------------------------------------

macro.vocc.elev = macro.vocc %>% 
  crop(elev.aripo.small) %>% 
  as.data.frame(xy = T) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_spatraster(data = elev.aripo.small, aes(fill = elev)) +
  geom_segment(aes(xend = x + EWdir*10000, yend = y + NSdir*10000), 
               size = 1, arrow = arrow(length = unit(0.2, "cm"))) +
  scale_fill_hypso_c(alpha = 0.5,
                     guide = guide_colorbar(title = "Elevation (m)", 
                                            title.position = "right", 
                                            title.hjust = 0.5, 
                                            direction = "vertical")) +
  # scale_fill_continuous_sequential("Grays", alpha = 0.5, 
  #                                  guide = guide_colorbar(title = "Elevation (m)", 
  #                                                         title.position = "right", 
  #                                                         title.hjust = 0.5, 
  #                                                         direction = "vertical")) +
  #geom_sf(data = aripo.large, color = "black", size = 1, inherit.aes = F, fill = NA) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_sf(crs = crs(macro.vocc)) +
  ggtitle("Macro") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(color = "black"),
        legend.title = element_text(angle = 270),
        legend.position = "none")
  
macro.vocc.pai = macro.vocc %>% 
  crop(pai.macrocrop) %>% 
  as.data.frame(xy = T) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_spatraster(data = crop(pai, pai.macrocrop)) +
  geom_segment(aes(xend = x + EWdir*10000, yend = y + NSdir*10000), 
               size = 1, arrow = arrow(length = unit(0.2, "cm"))) +
  scale_fill_continuous_sequential("Terrain 2", alpha = 0.5, 
                                   guide = guide_colorbar(title = "PAI"),
                                   na.value = NA) +
  #geom_sf(data = aripo.large, color = "black", size = 1, inherit.aes = F, fill = NA) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_sf(crs = crs(macro.vocc)) +
  ggtitle("Macro") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(color = "black"),
        legend.position = "none")



# * - meso (free-air 100 km) ----------------------------------------------------------------


# mesoclimate velocity
meso.vocc = vocc.df %>% filter(scale == "Topo") %>% 
  dplyr::select(x, y, vocc, EWdir, NSdir) %>% 
  rast(crs = "epsg:2067")
#meso.vocc = rast("02_climate_velocity/output/mesoclimate/avg_daily_max_temp/vocc.tif")
meso.vocc$vocc = abs(meso.vocc$vocc)


meso.vocc.elev = meso.vocc %>% 
  crop(elev.topo) %>% 
  #spatSample(size = 200, method = "random", as.df = T, xy = T, values = T) %>% 
  as.data.frame(xy = T, cells = T) %>% 
  filter(x%%3==0 & y%%3 == 0) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_spatraster(data = elev.topo, aes(fill = elev)) +
  geom_segment(aes(xend = x + EWdir*10000, yend = y + NSdir*10000), 
               size = 1, arrow = arrow(length = unit(0.2, "cm"))) +
  scale_fill_hypso_c(alpha = 0.5, guide = guide_colorbar(title = "Elevation (m)")) +
  #geom_sf(data = aripo.large, color = "black", size = 1, inherit.aes = F, fill = NA) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_sf(crs = crs(macro.vocc)) +
  ggtitle("Free-air: 100 m") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(color = "black"),
        legend.position = "none")


meso.vocc.pai = meso.vocc %>% 
  crop(pai.small) %>% 
  #spatSample(size = 150, method = "regular", as.df = T, xy = T, values = T) %>% 
  as.data.frame(xy = T) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_spatraster(data = crop(pai, pai.small)) +
  geom_segment(aes(xend = x + EWdir*100, yend = y + NSdir*100), 
               size = 1, arrow = arrow(length = unit(0.2, "cm"))) +
  scale_fill_continuous_sequential("Terrain 2", alpha = 0.5, 
                                   guide = guide_colorbar(title = "PAI"),
                                   na.value = NA) +
  #geom_sf(data = aripo.large, color = "black", size = 1, inherit.aes = F, fill = NA) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_sf(crs = crs(macro.vocc)) +
  ggtitle("Free-air: 100 m") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(color = "black"),
        legend.position = "none")

# * - micro 2D (100m)------------------------------------------------------------

# land-surface velocity
land100.vocc = vocc.df %>% filter(scale == "Land-surface" & resolution =="100m") %>% 
  dplyr::select(x, y, vocc, EWdir, NSdir) %>% 
  rast(crs = "epsg:2067")
#meso.vocc = rast("02_climate_velocity/output/mesoclimate/avg_daily_max_temp/vocc.tif")
land100.vocc$vocc = abs(land100.vocc$vocc)


land100.vocc.elev = land100.vocc %>% 
  crop(elev.topo) %>% 
  #spatSample(size = 200, method = "random", as.df = T, xy = T, values = T) %>% 
  as.data.frame(xy = T, cells = T) %>% 
  filter(x%%3==0 & y%%3 == 0) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_spatraster(data = elev.topo, aes(fill = elev)) +
  geom_segment(aes(xend = x + EWdir*10000, yend = y + NSdir*10000), 
               size = 1, arrow = arrow(length = unit(0.2, "cm"))) +
  scale_fill_hypso_c(alpha = 0.5, guide = guide_colorbar(title = "Elevation (m)")) +
  #geom_sf(data = aripo.large, color = "black", size = 1, inherit.aes = F, fill = NA) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_sf(crs = crs(macro.vocc)) +
  ggtitle("Land-surface: 100 m") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(color = "black"),
        legend.position = "none")


land100.vocc.pai = land100.vocc %>% 
  crop(pai.small) %>% 
  #spatSample(size = 150, method = "regular", as.df = T, xy = T, values = T) %>% 
  as.data.frame(xy = T) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_spatraster(data = crop(pai, pai.small)) +
  geom_segment(aes(xend = x + EWdir*100, yend = y + NSdir*100), 
               size = 1, arrow = arrow(length = unit(0.2, "cm"))) +
  scale_fill_continuous_sequential("Terrain 2", alpha = 0.5, 
                                   guide = guide_colorbar(title = "PAI"),
                                   na.value = NA) +
  #geom_sf(data = aripo.large, color = "black", size = 1, inherit.aes = F, fill = NA) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_sf(crs = crs(macro.vocc)) +
  ggtitle("Land-surface: 100 m") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(color = "black"),
        legend.position = "none")


# * - micro 3d (100m) ------------------------------------------------------------

# 3d microclim velocity for the top half of the canopy
# meso = rast("scripts/02_climate_velocity/output/mesoclimate/avg_daily_max_temp/vocc.tif")
# dem.micro = rast('data/topography/dem_reproj.tif')
# dem.micro = extend(dem.micro, 1)
# 
# landuse.micro = rast('data/Helmer_2012_Beard_vegetation/201403_Trin_USDA_pWGS84_forest_tree_communities_for_Trinidad_from_Landsat.tif')
# landuse.micro = project(landuse.micro, dem.micro, method = "near")
# landuse.meso = terra::resample(landuse.micro, meso, method = "near")
# landuse.class = read.csv("data/Helmer_2012_Beard_vegetation/Helmer_classification.csv")
# landuse.class = landuse.class %>% dplyr::select(Value, LU_level_1)
# 
# pai.micro = rast('data/PAI/pai_NRange_rmMeters0to2_rmPAIgt10.tif')
# pai.micro = extend(pai.micro, 1)
# pai.meso = resample(pai.micro, meso)

micro3d_100m = fread("scripts/03_analysis/00_dataframes/analysis_dataframe_3d_full_maxtemp.csv")

micro3d_100m = micro3d_100m %>% 
  filter(resolution =="100m") %>% 
  dplyr::select(x, y, vocc, EWdir, NSdir, zAng) %>% 
  group_by(x, y) %>% 
  summarise(vocc = mean(vocc), NSdir = mean(NSdir), EWdir = mean(EWdir), zAng = mean(zAng)) %>% 
  rast(crs = "epsg:2067")

# landuse.meso.df = as.data.frame(landuse.meso, xy = T)
# colnames(landuse.meso.df)[3] = "landuse"
# landuse.meso.df = left_join(landuse.meso.df, landuse.class, by = c("landuse" = "Value"))
# pai.meso.df = as.data.frame(pai.meso, xy = T)
# names(pai.meso.df) = c("x", "y", "pai")
# micro3d_100m = micro3d_100m %>% 
#   left_join(landuse.meso.df, by = c("x", "y")) %>% 
#   left_join(pai.meso.df, by = c("x", "y")) %>% 
#   mutate(vocc = abs(vocc))
# micro3d_100m = micro3d_100m[vocc < quantile(vocc, probs = 0.95, na.rm = T),]
# micro3d_100m = micro3d_100m[!is.na(vocc), ]
# micro3d_100m = micro3d_100m %>% filter(LU_level_1 == "Forest including forest/shrub land")

micro3d_100m %>% 
  ggplot(aes(pai, zAng)) +
  geom_point() +
  geom_smooth()


micro3d.vocc.elev = micro3d_100m %>% 
  crop(elev.topo) %>% 
  as.data.frame(xy = T) %>%  
  filter(x%%3==0 & y%%3 == 0) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_spatraster(data = elev.topo, aes(fill = elev)) +
  geom_segment(aes(xend = x + EWdir*10, yend = y + NSdir*10, color = zAng), 
               size = 1, arrow = arrow(length = unit(0.2, "cm"))) +
  scale_fill_hypso_c(alpha = 0.5, guide = guide_colorbar(title = "Elevation (m)", 
                                                         title.position = "top",
                                                         title.hjust = 0.5,
                                                         direction = "horizontal")) +
  #geom_sf(data = aripo.large, color = "black", size = 1, inherit.aes = F, fill = NA) +
  scale_color_continuous_divergingx("PuOr", rev = T,
                                    guide = guide_colorbar(title = "Vertical angle (\u00b0)", 
                                                           title.position = "top", 
                                                           title.hjust = 0.5, 
                                                           direction = "horizontal"),
                                    limits = c(-90,30)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_sf(crs = crs(macro.vocc)) +
  ggtitle("Within-canopy") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(color = "black"),
        legend.title = element_text(angle = 270),
        legend.position = "none")



micro3d.vocc.pai = micro3d_100m %>% 
  crop(pai.small) %>% 
  #spatSample(size = 150, method = "regular", as.df = T, xy = T, values = T) %>% 
  as.data.frame(xy = T) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_spatraster(data = crop(pai, pai.small)) +
  geom_segment(aes(xend = x + EWdir*10, yend = y + NSdir*10, color = zAng), 
               size = 1, arrow = arrow(length = unit(0.2, "cm"))) +
  scale_fill_continuous_sequential("Terrain 2", alpha = 0.5,
                                   guide = guide_colorbar(title = "PAI", 
                                                          title.position = "top", 
                                                          title.hjust = 0.5, 
                                                          direction = "horizontal",
                                                          order = 2),
                                   na.value = NA) +
  scale_color_continuous_divergingx("PuOr", rev = T,
                                    guide = guide_colorbar(title = "Vertical angle (\u00b0)", 
                                                           title.position = "top", 
                                                           title.hjust = 0.5, 
                                                           direction = "horizontal"),
                                    limits = c(-90,90)) +
  #geom_sf(data = aripo.large, color = "black", size = 1, inherit.aes = F, fill = NA) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_sf(crs = crs(macro.vocc)) +
  ggtitle("Within-canopy") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(color = "black"),
        legend.title = element_text(angle = 270),
        legend.position = "none")

# rest of plot ------------------------------------------------------------

legend = 
  micro3d_100m %>% 
  #spatSample(size = 200, method = "regular", as.df = T, xy = T, values = T) %>% 
  as.data.frame(xy = T) %>% 
  ggplot(aes(x=x, y=y)) +
  geom_spatraster(data = elev.aripo.small, aes(fill = elev)) +
  scale_fill_hypso_c(alpha = 0.5, guide = guide_colorbar(title = "Elevation (m)", 
                                                         title.position = "top",
                                                         title.hjust = 0.5,
                                                         direction = "horizontal",
                                                         order = 1), breaks = seq(0,900,300)) +
  ggnewscale::new_scale_fill() +
  geom_spatraster(data = crop(pai, pai.small)) +
  scale_fill_continuous_sequential("Terrain 2", alpha = 0.5,
                                   guide = guide_colorbar(title = "PAI", 
                                                          title.position = "top", 
                                                          title.hjust = 0.5, 
                                                          direction = "horizontal",
                                                          order = 2),
                                   na.value = NA) +
  geom_segment(aes(xend = x + EWdir*10, yend = y + NSdir*10, color = zAng), 
               size = 1, arrow = arrow(length = unit(0.2, "cm"))) +
  scale_color_continuous_divergingx("PuOr", rev = T,
                                    guide = guide_colorbar(title = "Vertical angle (\u00b0)", 
                                                           title.position = "top", 
                                                           title.hjust = 0.5, 
                                                           direction = "horizontal",
                                                           order = 3),
                                    limits = c(-90,90), breaks = seq(-90,90,45)) +
  theme_classic() +
  theme(legend.title = element_text(angle = 0),
        legend.box = "horizontal")

legend = get_legend(legend)

# png("03_analysis/00_plots/vocc_direction/direction_compare/inset_large.png", width = 1500, height = 500, res = 300)
# inset.large
# dev.off()
# 
# png("03_analysis/00_plots/vocc_direction/direction_compare/inset_small.png", width = 1500, height = 500, res = 300)
# inset.small
# dev.off()
# 
# png("03_analysis/00_plots/vocc_direction/direction_compare/legend.png", width = 1500, height = 500, res = 300)
# plot(legend)
# dev.off()
# 
# png("03_analysis/00_plots/vocc_direction/direction_compare/macro_vocc_elev.png", width = 1500, height = 1100, res = 300)
# macro.vocc.elev
# dev.off()
# 
# png("03_analysis/00_plots/vocc_direction/direction_compare/meso_vocc_elev.png", width = 1500, height = 1100, res = 300)
# meso.vocc.elev
# dev.off()
# 
# png("03_analysis/00_plots/vocc_direction/direction_compare/micro2d_vocc_elev.png", width = 1500, height = 1100, res = 300)
# micro2d.vocc.elev
# dev.off()
# 
# png("03_analysis/00_plots/vocc_direction/direction_compare/micro3d_vocc_elev.png", width = 1500, height = 1100, res = 300)
# micro3d.vocc.elev
# dev.off()
# 
# png("03_analysis/00_plots/vocc_direction/direction_compare/macro_vocc_pai.png", width = 1300, height = 1100, res = 300)
# macro.vocc.pai
# dev.off()
# 
# png("03_analysis/00_plots/vocc_direction/direction_compare/meso_vocc_pai.png", width = 1300, height = 1100, res = 300)
# meso.vocc.pai
# dev.off()
# 
# png("03_analysis/00_plots/vocc_direction/direction_compare/micro2d_vocc_pai.png", width = 1300, height = 1100, res = 300)
# micro2d.vocc.pai
# dev.off()
# 
# png("03_analysis/00_plots/vocc_direction/direction_compare/micro3d_vocc_pai.png", width = 1300, height = 1100, res = 300)
# micro3d.vocc.pai
# dev.off()

library(gggrid)
library(patchwork)

layout = "
AAABBB
CCDDEE
FFGGHH
IIIIII
"

myPlot = inset.elev + inset.pai +
  meso.vocc.elev + land100.vocc.elev + micro3d.vocc.elev +
  meso.vocc.pai + land100.vocc.pai + micro3d.vocc.pai + 
  legend +
  plot_layout(design = layout, heights = c(1,2,2,1), byrow = T) +
  plot_annotation(tag_levels = "a") + theme(plot.tag.position = c(0.2, 0.8))
  

png("scripts/03_analysis/00_plots/new_figs/fig04_direction_compare.png", height = 160, width = 180, res = 300, units = "mm")
myPlot
dev.off()

