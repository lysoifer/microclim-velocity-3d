# plot climate velocity across spatial scales

library(terra)
library(tidyterra)
library(ggplot2)
library(dplyr)
library(scales)
library(classInt)
library(ggnewscale)
library(patchwork)
library(viridis)
library(colorspace)
library(data.table)
library(grid)

plot_climvocc = function(r, var, hill_plot, plt_lab, legend.title, roundto, brk_vect) {
  r.df = as.data.frame(r[[var]], xy = T)
  r.jenksbr = classIntervals(r.df[,var], n=19, style = "quantile", na.rm = T)
  #r.jenksbr = classIntervals(brk_vect, n=999, style = "quantile", na.rm = T)
  jenksbr = cut(r.df[,3], r.jenksbr$brks, labels = F, include.lowest = T, right = F)
  r.df$jenksbr = jenksbr
  
  r = rast(r.df, type = "xyz", crs = crs(r))
  
  plt = ggplot() +
    #ggnewscale::new_scale_fill() +
    geom_spatraster(data = r, aes(fill = jenksbr), alpha = 1) +
    geom_spatvector(data = nrange, color = "black", fill = NA, linewidth = 1) +
    scale_x_continuous(expand = c(0,0)) +
    scale_fill_gradientn(colors = pal_viridis()(20), breaks = c(1,5,10,15,19), 
                         labels = round(c(r.jenksbr$brks[c(1,5,10,15)], max(r.jenksbr$brks)),roundto), 
                         legend.title, na.value = NA) +
    #annotate(geom = "text", x = 636000, y = 1197200, label = plt_lab, fontface = "bold", size = 5, hjust = 0) +
    new_scale_fill() +
    #geom_spatraster(data = shade.meso, alpha = 0.5) +
    #scale_fill_gradientn(colors = pal_greys, na.value = NA, guide = "none") +
    theme_classic() +
    theme(panel.background = element_rect(color = "black"),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.key.height = unit(10, "pt"))
  
  # qn = quantile(vocc.df$vocc, c(0.01, 0.99), na.rm = TRUE)
  # qn01 <- rescale(c(qn, range(vocc.df$vocc))) 
  # 
  # ggplot(r.df, aes(x = x, y = y, fill = vocc)) + 
  #   geom_tile() + 
  #   scale_fill_gradientn (
  #     colours = pal_viridis()(20),
  #     values = c(0, seq(qn01[1], qn01[2], length.out = 18), 1)) +
  #   theme(legend.key.height = unit (4.5, "lines"))
  # 
  # plt = ggplot() +
  #   #ggnewscale::new_scale_fill() +
  #   #geom_spatraster(data = r, aes(fill = jenksbr), alpha = 1) +
  #   geom_spatraster(data = r, aes(fill = vocc), alpha = 1) +
  #   geom_spatvector(data = nrange, color = "black", fill = NA, linewidth = 1) +
  #   scale_x_continuous(expand = c(0,0)) +
  #   scale_fill_gradientn(colors = pal_viridis()(1000), breaks = round(seq(946,999,length.out = 5)), 
  #                        labels = round(c(r.jenksbr$brks[round(seq(946,999,length.out = 5))[1:4]], max(r.jenksbr$brks)),roundto), 
  #                        legend.title, na.value = NA) +
  #   #annotate(geom = "text", x = 636000, y = 1197200, label = plt_lab, fontface = "bold", size = 5, hjust = 0) +
  #   new_scale_fill() +
  #   #geom_spatraster(data = shade.meso, alpha = 0.5) +
  #   #scale_fill_gradientn(colors = pal_greys, na.value = NA, guide = "none") +
  #   theme_classic() +
  #   theme(panel.background = element_rect(color = "black"),
  #         axis.title = element_blank(),
  #         axis.text = element_blank(),
  #         axis.ticks = element_blank(),
  #         legend.key.height = unit(10, "pt"))
  # 
  return(plt)
}

# elev
elev = rast("data/topography/dem_reproj.tif")

# N range polygon
nrange = vect("data/cropping_polygons/NRange.shp")
nrange = project(nrange, elev)
nrange = aggregate(nrange)

# elev
elev = rast("data/topography/dem_reproj.tif")

# meso elev
elev_meso = raster::aggregate(elev, fact = 5) # merge cells of micro DEM to match resolution of macro dem raster
elev_meso = extend(elev_meso, c(1,1)) # still use lidar dem


# hillshade
hill = rast("data/topography/dem_reproj_hillshade.tif")
names(hill) = "shades"
pal_greys = hcl.colors(1000, "Grays")

slope.meso = terrain(elev_meso, unit = "radians", v = "slope")
aspect.meso = terrain(elev_meso, unit = "radians", v = "aspect")
shade.meso = shade(slope = slope.meso, aspect = aspect.meso)

# plot hillshade
hill_plot = ggplot() +
  geom_spatraster(data = shade.meso, alpha = 0.4) +
  scale_fill_gradientn(colors = pal_greys, na.value = NA, guide = "none") +
  theme_classic()

# includes only forested land use
vocc.df = fread("scripts/03_analysis/00_dataframes/analysis_dataframe_full_mintemp.csv")

# air_1km = vocc.df %>% 
#   filter(scale == "Macro" & resolution == "1km") %>% 
#   dplyr::select(x,y,vocc, spatgrad, tempgrad) %>% 
#   mutate(vocc = abs(vocc)) %>% 
#   rast(type = "xyz", crs = "epsg:2067")
# 
# air_100m = vocc.df %>% 
#   filter(scale == "Topo" & resolution == "100m") %>% 
#   dplyr::select(x,y,vocc, spatgrad, tempgrad) %>% 
#   mutate(vocc = abs(vocc)) %>% 
#   rast(type = "xyz", crs = "epsg:2067")
# 
# land_1km = vocc.df %>% 
#   filter(scale == "Land-surface" & resolution == "1km") %>% 
#   dplyr::select(x,y,vocc, spatgrad, tempgrad) %>% 
#   mutate(vocc = abs(vocc)) %>% 
#   rast(type = "xyz", crs = "epsg:2067")
# 
# land_100m = vocc.df %>% 
#   filter(scale == "Land-surface" & resolution == "100m") %>% 
#   dplyr::select(x,y,vocc, spatgrad, tempgrad) %>% 
#   mutate(vocc = abs(vocc)) %>% 
#   rast(type = "xyz", crs = "epsg:2067")
# 
# land_20m = vocc.df %>% 
#   filter(scale == "Land-surface" & resolution == "20m") %>% 
#   dplyr::select(x,y,vocc, spatgrad, tempgrad) %>% 
#   mutate(vocc = abs(vocc)) %>% 
#   rast(type = "xyz", crs = "epsg:2067")
# 
# canopy_1km = vocc.df %>% 
#   filter(scale == "Within-canopy" & resolution == "1km") %>% 
#   dplyr::select(x,y,vocc, spatgrad, tempgrad) %>% 
#   mutate(vocc = abs(vocc)) %>% 
#   group_by(x,y) %>% 
#   summarise(vocc = mean(vocc, na.rm = T),
#             spatgrad = mean(spatgrad, na.rm = T),
#             tempgrad = mean(tempgrad, na.rm = T)) %>%
#   rast(type = "xyz", crs = "epsg:2067")
# 
# canopy_100m = vocc.df %>% 
#   filter(scale == "Within-canopy" & resolution == "100m") %>% 
#   dplyr::select(x,y,vocc, spatgrad, tempgrad) %>% 
#   mutate(vocc = abs(vocc)) %>% 
#   group_by(x,y) %>% 
#   summarise(vocc = mean(vocc, na.rm = T),
#             spatgrad = mean(spatgrad, na.rm = T),
#             tempgrad = mean(tempgrad, na.rm = T)) %>%
#   rast(type = "xyz", crs = "epsg:2067")
# 
# canopy_20m = vocc.df %>% 
#   filter(scale == "Within-canopy" & resolution == "20m") %>% 
#   dplyr::select(x,y,vocc, spatgrad, tempgrad) %>% 
#   mutate(vocc = abs(vocc)) %>% 
#   group_by(x,y) %>% 
#   summarise(vocc = mean(vocc, na.rm = T),
#             spatgrad = mean(spatgrad, na.rm = T),
#             tempgrad = mean(tempgrad, na.rm = T)) %>%
#   rast(type = "xyz", crs = "epsg:2067")

vocc.df = vocc.df %>% 
  select(x,y,vocc,spatgrad,tempgrad, scale, resolution, elev, pai, mintemp.pres) %>% 
  mutate(vocc = abs(vocc)) %>% 
  group_by(x,y,scale,resolution, elev, pai) %>% 
  summarise(vocc = mean(vocc, na.rm = T),
            spatgrad = mean(spatgrad, na.rm = T),
            tempgrad = mean(tempgrad, na.rm = T),
            mintemp.pres = mean(mintemp.pres))%>%
  mutate(scale = case_when(scale == "Macro" ~ "Free-air",
                           scale == "Topo" ~ "Free-air",
                           scale == "Land-surface" ~ "Land\nsurface",
                           scale == "Within-canopy" ~ "Within\ncanopy",
                           .default = scale))

# df %>%   
#   group_by(scale, resolution) %>% 
#   slice_sample(n=5000) %>% 
#   
#   mutate(scale = case_when(scale == "Topo" ~ "Free-air",
#                            scale == "Macro" ~ "Free-air",
#                            .default = scale)) %>% 
#   ggplot(aes(elev, tempgrad*55)) +
#   geom_point() +
#   facet_grid(rows = vars(scale), cols = vars(resolution)) +
#   theme_classic()

p = ggplot() +
  geom_raster(data = vocc.df, aes(x, y, fill = log10(abs(vocc)))) +
  facet_grid(rows = vars(scale), cols = vars(resolution)) +
  geom_spatvector(data = nrange, color = "black", fill = NA, linewidth = 0.5) +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_viridis_c("Maximum temperature Velocity (m/yr)",
                       option = "turbo",
                       breaks = seq(-4,2,1), labels = 10^(seq(-4,2,1))) +
  coord_sf(crs = "epsg:2067") +
  theme_classic() +
  theme(panel.background = element_rect(color = "black", fill = NA),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(20, "mm"),
        legend.key.height = unit(2,"mm"),
        legend.text = element_text(size = 6, angle = 290, vjust = 0, hjust = 0),
        legend.title = element_text(size = 8),
        legend.title.position = "top")

# brk_vect = vocc.df$vocc
# r.jenksbr = classIntervals(brk_vect, n=80, style = "quantile", na.rm = T)
# vocc.df = vocc.df %>% 
#   as.data.frame() %>% 
#   mutate(jenksbr = cut(vocc.df$vocc, r.jenksbr$brks, labels = seq(1,80,1), include.lowest = T, right = F))
# vocc.df$jenksbr = as.numeric(as.character(vocc.df$jenksbr))
# vocc.df = vocc.df %>% 
#   mutate(resolution = factor(resolution, levels = c("1km", "100m", "20m")))
# 
# 
# p = ggplot() +
#   geom_raster(data = vocc.df, aes(x, y, fill = jenksbr)) +
#   facet_grid(rows = vars(scale), cols = vars(resolution)) +
#   geom_spatvector(data = nrange, color = "black", fill = NA, linewidth = 0.5) +
#   scale_x_continuous(expand = c(0,0)) +
#   # scale_color_continuous_diverging(palette = "Blue-Red",
#   #                                  breaks = c(-40,20,0,20,40),
#   #                                  labels = c(r.jenksbr.neg$brks[1], r.jenksbr.neg$brks[21], 0, r.jenksbr.pos$brks[21], r.jenksbr.pos$brks[41]),
#   #                                  legend.title, na.value = NA) +
#   scale_fill_gradientn(colors = sequential_hcl(palette = "Viridis", n = 80), breaks = c(1, seq(5,80,5)),
#                        labels = round(c(r.jenksbr$brks[1], r.jenksbr$brks[6],
#                                         r.jenksbr$brks[11], r.jenksbr$brks[16],
#                                         r.jenksbr$brks[21], r.jenksbr$brks[26],
#                                         r.jenksbr$brks[31], r.jenksbr$brks[36],
#                                         r.jenksbr$brks[41], r.jenksbr$brks[46], 
#                                         r.jenksbr$brks[51], r.jenksbr$brks[56],
#                                         r.jenksbr$brks[61], r.jenksbr$brks[66],
#                                         r.jenksbr$brks[71], r.jenksbr$brks[76],
#                                         r.jenksbr$brks[81]), 3),
#                        "m/yr", na.value = NA) +
#   coord_sf(crs = "epsg:2067") +
#   theme_classic() +
#   theme(panel.background = element_rect(color = "black", fill = NA),
#         axis.title = element_blank(),
#         axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         legend.position = "bottom",
#         legend.key.width = unit(20, "mm"),
#         legend.key.height = unit(2,"mm"),
#         legend.text = element_text(size = 6, angle = 290, vjust = 0, hjust = 0),
#         legend.title = element_text(size = 8))
# 
# 
# air_1km.plt = plot_climvocc(r = air_1km, "vocc", hill_plot = hill_plot, plt_lab = "Free-air", legend.title = "m/yr", roundto = 2)
# air_100m.plt = plot_climvocc(r = air_100m, "vocc", hill_plot = hill_plot, plt_lab = "Free-air", legend.title = "m/yr", roundto = 2)
# land_1km.plt = plot_climvocc(r = land_1km, "vocc", hill_plot = hill_plot, plt_lab = "Land surface", legend.title = "m/yr", roundto = 2)
# land_100m.plt = plot_climvocc(r = land_100m, "vocc", hill_plot = hill_plot, plt_lab = "Land surface", legend.title = "m/yr", roundto = 2)
# land_20m.plt = plot_climvocc(r = land_20m, "vocc", hill_plot = hill_plot, plt_lab = "Land surface", legend.title = "m/yr", roundto = 2)
# canopy_1km.plt = plot_climvocc(r = canopy_1km, "vocc", hill_plot = hill_plot, plt_lab = "Within-canopy", legend.title = "m/yr", roundto = 2)
# canopy_100m.plt = plot_climvocc(r = canopy_100m, "vocc", hill_plot = hill_plot, plt_lab = "Within-canopy", legend.title = "m/yr", roundto = 2)
# canopy_20m.plt = plot_climvocc(r = canopy_20m, "vocc", hill_plot = hill_plot, plt_lab = "Within-canopy", legend.title = "m/yr", roundto = 2)
# 

# meso.vocc.plt = plot_climvocc(r = meso.vocc, hill_plot = hill_plot, plt_lab = "Topo", legend.title = "m/yr", roundto = 2)
# macro.vocc.plt = plot_climvocc(r = macro.vocc, hill_plot = hill_plot, plt_lab = "Macro", legend.title = "m/yr", roundto = 2)
# micro2d.vocc.plt = plot_climvocc(r = micro2d.vocc, hill_plot = hill_plot, 
#                                  plt_lab = "Land Surface", legend.title = "m/yr", roundto = 2)
# micro3d.vocc.plt = plot_climvocc(r = micro3d.vocc, hill_plot = hill_plot, plt_lab = "Within-canopy", legend.title = "m/yr", roundto = 2)

# climvocc.plt = macro.vocc.plt +  micro2d.vocc.plt + meso.vocc.plt + micro3d.vocc.plt + plot_layout(
#   design = "12
#             34"
# )

# col_label_1 <- wrap_elements(panel = textGrob('1km'))
# col_label_2 <- wrap_elements(panel = textGrob('100m'))
# col_label_3 <- wrap_elements(panel = textGrob('20m'))
# 
# row_label_1 = wrap_elements(panel = textGrob('Free-air', rot = 90, gp=gpar(fontsize=8, col="black")))
# row_label_2 = wrap_elements(panel = textGrob('Land surface', rot = 90, gp=gpar(fontsize=8, col="black")))
# row_label_3 = wrap_elements(panel = textGrob('Within-canopy', rot = 90, gp=gpar(fontsize=8, col="black")))
# 
# 
# design = "
# abcd
# efgh
# ijkl
# mnop"
# 
# climvocc.plt = plot_spacer() + col_label_1 + col_label_2 + col_label_3 +
#   row_label_1 + air_1km.plt + air_100m.plt + plot_spacer() + 
#   row_label_2 + land_1km.plt + land_100m.plt + land_20m.plt +
#   row_label_3 + canopy_1km.plt + canopy_100m.plt + canopy_20m.plt + 
#   plot_layout(design = design, heights = c(0.15,1,1,1), widths = c(0.1, 1,1,1)) +
#   plot_annotation(tag_levels = list(c("", "", "", "",
#                                       "a",  "b", "", "c",
#                                       "d", "e", "", "f",
#                                       "g", "h", "", ""))) &
#   theme(plot.tag.position = c(0.06,0.75),
#         legend.text = element_text(size = 6),
#         legend.title = element_text(size = 8),
#         legend.key.width = unit(6, "mm"),
#         legend.key.height = unit(2, "mm"),
#         legend.position = "inside",
#         legend.position.inside = c(0.5,-0.3),
#         legend.direction = "horizontal",
#         legend.background = element_blank())

png("scripts/03_analysis/00_plots/new_figs/maps/climvocc_tmin.png", width = 180, height = 90, res = 300, units = "mm")
p
dev.off()



# SPATIAL RATE OF CLIMATE CHANGE ------------------------------------------

brk_vect = vocc.df$spatgrad
r.jenksbr = classIntervals(brk_vect, n=40, style = "quantile", na.rm = T)
vocc.df = vocc.df %>% 
  as.data.frame() %>% 
  mutate(jenksbr = cut(vocc.df$spatgrad, r.jenksbr$brks, labels = seq(1,40,1), include.lowest = T, right = F))
vocc.df$jenksbr = as.numeric(as.character(vocc.df$jenksbr))
vocc.df = vocc.df %>% 
  mutate(scale = case_when(scale == "Macro" ~ "Free-air",
                           scale == "Topo" ~ "Free-air",
                           scale == "Land-surface" ~ "Land\nsurface",
                           scale == "Within-canopy" ~ "Within\ncanopy",
                           .default = scale))
vocc.df = vocc.df %>% 
  mutate(resolution = factor(resolution, levels = c("1km", "100m", "20m")))

p = ggplot() +
  geom_raster(data = vocc.df, aes(x, y, fill = jenksbr)) +
  facet_grid(rows = vars(scale), cols = vars(resolution)) +
  geom_spatvector(data = nrange, color = "black", fill = NA, linewidth = 0.5) +
  scale_x_continuous(expand = c(0,0)) +
  # scale_color_continuous_diverging(palette = "Blue-Red",
  #                                  breaks = c(-40,20,0,20,40),
  #                                  labels = c(r.jenksbr.neg$brks[1], r.jenksbr.neg$brks[21], 0, r.jenksbr.pos$brks[21], r.jenksbr.pos$brks[41]),
  #                                  legend.title, na.value = NA) +
  scale_fill_gradientn(colors = sequential_hcl(palette = "Viridis", n = 40), breaks = c(1, 10, 20, 30, 40),
                       labels = round(c(r.jenksbr$brks[1], r.jenksbr$brks[11], r.jenksbr$brks[21], 
                                        r.jenksbr$brks[31], r.jenksbr$brks[41]), 3),
                       "\u00b0C/m", na.value = NA) +
  coord_sf(crs = "epsg:2067") +
  theme_classic() +
  theme(panel.background = element_rect(color = "black", fill = NA),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(10, "mm"),
        legend.key.height = unit(2,"mm"),
        legend.text = element_text(size = 6, angle = 290, vjust = 0, hjust = 0),
        legend.title = element_text(size = 8))

# air_1km.spat = plot_climvocc(r = air_1km, "spatgrad", hill_plot = hill_plot, plt_lab = "Free-air", legend.title = "m/yr", roundto = 2)
# air_100m.spat = plot_climvocc(r = air_100m, "spatgrad", hill_plot = hill_plot, plt_lab = "Free-air", legend.title = "m/yr", roundto = 2)
# land_1km.spat = plot_climvocc(r = land_1km, "spatgrad", hill_plot = hill_plot, plt_lab = "Land surface", legend.title = "m/yr", roundto = 2)
# land_100m.spat = plot_climvocc(r = land_100m, "spatgrad", hill_plot = hill_plot, plt_lab = "Land surface", legend.title = "m/yr", roundto = 2)
# land_20m.spat = plot_climvocc(r = land_20m, "spatgrad", hill_plot = hill_plot, plt_lab = "Land surface", legend.title = "m/yr", roundto = 2)
# canopy_1km.spat = plot_climvocc(r = canopy_1km, "spatgrad", hill_plot = hill_plot, plt_lab = "Within-canopy", legend.title = "m/yr", roundto = 2)
# canopy_100m.spat = plot_climvocc(r = canopy_100m, "spatgrad", hill_plot = hill_plot, plt_lab = "Within-canopy", legend.title = "m/yr", roundto = 2)
# canopy_20m.spat = plot_climvocc(r = canopy_20m, "spatgrad", hill_plot = hill_plot, plt_lab = "Within-canopy", legend.title = "m/yr", roundto = 2)
# 
# spatgrad.plt = plot_spacer() + col_label_1 + col_label_2 + col_label_3 +
#   row_label_1 + air_1km.spat + air_100m.spat + plot_spacer() + 
#   row_label_2 + land_1km.spat + land_100m.spat + land_20m.spat +
#   row_label_3 + canopy_1km.spat + canopy_100m.spat + canopy_20m.spat + 
#   plot_layout(design = design, heights = c(0.15,1,1,1), widths = c(0.1, 1,1,1)) +
#   plot_annotation(tag_levels = list(c("", "", "", "",
#                                       "a",  "b", "", "c",
#                                       "d", "e", "", "f",
#                                       "g", "h", "", ""))) &
#   theme(plot.tag.position = c(0.06,0.75),
#         legend.text = element_text(size = 6),
#         legend.title = element_text(size = 8),
#         legend.key.width = unit(6, "mm"),
#         legend.key.height = unit(2, "mm"),
#         legend.position = "inside",
#         legend.position.inside = c(0.5,-0.3),
#         legend.direction = "horizontal",
#         legend.background = element_blank())

png("scripts/03_analysis/00_plots/new_figs/maps/spatgrad_tmin.png", width = 180, height = 90, res = 300, units = "mm")
p
dev.off()


# TEMPORAL GRADIENT -------------------------------------------------------
brk_vect = vocc.df$tempgrad
brk_vect_neg = brk_vect[which(brk_vect < 0)]
brk_vect_pos = brk_vect[which(brk_vect >= 0)]

r.jenksbr.neg = classIntervals(brk_vect_neg, n=40, style = "quantile", na.rm = T)
r.jenksbr.pos = classIntervals(brk_vect_pos, n=40, style = "quantile", na.rm = T)
vocc.df = vocc.df %>% 
  as.data.frame() %>% 
  mutate(jenksbr = case_when(tempgrad < 0 ~ cut(vocc.df$tempgrad, r.jenksbr.neg$brks, labels = seq(-40,-1,1), include.lowest = T, right = F),
                             tempgrad >= 0 ~ cut(vocc.df$tempgrad, r.jenksbr.pos$brks, labels = seq(1,40,1), include.lowest = T, right = F)))
vocc.df$jenksbr = as.numeric(as.character(vocc.df$jenksbr))
vocc.df = vocc.df %>% 
  mutate(scale = case_when(scale == "Macro" ~ "Free-air",
                           scale == "Topo" ~ "Free-air",
                           scale == "Land-surface" ~ "Land\nsurface",
                           scale == "Within-canopy" ~ "Within\ncanopy",
                           .default = scale))
vocc.df = vocc.df %>% 
  mutate(resolution = factor(resolution, levels = c("1km", "100m", "20m")))

p = ggplot() +
  geom_raster(data = vocc.df, aes(x, y, fill = jenksbr)) +
  facet_grid(rows = vars(scale), cols = vars(resolution)) +
  geom_spatvector(data = nrange, color = "black", fill = NA, linewidth = 0.5) +
  scale_x_continuous(expand = c(0,0)) +
  # scale_color_continuous_diverging(palette = "Blue-Red",
  #                                  breaks = c(-40,20,0,20,40),
  #                                  labels = c(r.jenksbr.neg$brks[1], r.jenksbr.neg$brks[21], 0, r.jenksbr.pos$brks[21], r.jenksbr.pos$brks[41]),
  #                                  legend.title, na.value = NA) +
  scale_fill_gradientn(colors = diverging_hcl(palette = "Blue-Red", n = 81), breaks = c(-40, -30, -20,-10, 0, 10, 20, 30, 40),
                       labels = round(c(r.jenksbr.neg$brks[1], r.jenksbr.neg$brks[11],
                                        r.jenksbr.neg$brks[21], r.jenksbr.neg$brks[31],
                                        0, r.jenksbr.pos$brks[11], r.jenksbr.pos$brks[21], 
                                        r.jenksbr.pos$brks[31], r.jenksbr.pos$brks[41]), 4),
                       "\u00b0C/yr", na.value = NA) +
  coord_sf(crs = "epsg:2067") +
  theme_classic() +
  theme(panel.background = element_rect(color = "black", fill = NA),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(10, "mm"),
        legend.key.height = unit(2,"mm"),
        legend.text = element_text(size = 6, angle = 290, vjust = 0, hjust = 0),
        legend.title = element_text(size = 8))

png("scripts/03_analysis/00_plots/new_figs/maps/tempgrad_tmin.png", width = 180, height = 90, res = 300, units = "mm")
p
dev.off()


# air_1km.temp = plot_climvocc(r = air_1km, "tempgrad", hill_plot = hill_plot, plt_lab = "Free-air", legend.title = "m/yr", roundto = 3)
# air_100m.temp = plot_climvocc(r = air_100m, "tempgrad", hill_plot = hill_plot, plt_lab = "Free-air", legend.title = "m/yr", roundto = 3)
# land_1km.temp = plot_climvocc(r = land_1km, "tempgrad", hill_plot = hill_plot, plt_lab = "Land surface", legend.title = "m/yr", roundto = 3)
# land_100m.temp = plot_climvocc(r = land_100m, "tempgrad", hill_plot = hill_plot, plt_lab = "Land surface", legend.title = "m/yr", roundto = 3)
# land_20m.temp = plot_climvocc(r = land_20m, "tempgrad", hill_plot = hill_plot, plt_lab = "Land surface", legend.title = "m/yr", roundto = 3)
# canopy_1km.temp = plot_climvocc(r = canopy_1km, "tempgrad", hill_plot = hill_plot, plt_lab = "Within-canopy", legend.title = "m/yr", roundto = 3)
# canopy_100m.temp = plot_climvocc(r = canopy_100m, "tempgrad", hill_plot = hill_plot, plt_lab = "Within-canopy", legend.title = "m/yr", roundto = 3)
# canopy_20m.temp = plot_climvocc(r = canopy_20m, "tempgrad", hill_plot = hill_plot, plt_lab = "Within-canopy", legend.title = "m/yr", roundto = 3)
# 
# tempgrad.plt = plot_spacer() + col_label_1 + col_label_2 + col_label_3 +
#   row_label_1 + air_1km.temp + air_100m.temp + plot_spacer() + 
#   row_label_2 + land_1km.temp + land_100m.temp + land_20m.temp +
#   row_label_3 + canopy_1km.temp + canopy_100m.temp + canopy_20m.temp + 
#   plot_layout(design = design, heights = c(0.15,1,1,1), widths = c(0.1, 1,1,1)) +
#   plot_annotation(tag_levels = list(c("", "", "", "",
#                                       "a",  "b", "", "c",
#                                       "d", "e", "", "f",
#                                       "g", "h", "", ""))) &
#   theme(plot.tag.position = c(0.06,0.75),
#         legend.text = element_text(size = 6),
#         legend.title = element_text(size = 8),
#         legend.key.width = unit(6, "mm"),
#         legend.key.height = unit(2, "mm"),
#         legend.position = "inside",
#         legend.position.inside = c(0.5,-0.3),
#         legend.direction = "horizontal",
#         legend.background = element_blank())

# png("scripts/03_analysis/00_plots/new_figs/maps/tempgrad_tmin.png", width = 180, height = 90, res = 300, units = "mm")
# tempgrad.plt
# dev.off()


# PLOT MIN TEMP PRES ------------------------------------------------------

vocc.df = vocc.df %>% 
  as.data.frame()

vocc.df = vocc.df %>% 
  mutate(scale = case_when(scale == "Macro" ~ "Free-air",
                           scale == "Topo" ~ "Free-air",
                           scale == "Land-surface" ~ "Land\nsurface",
                           scale == "Within-canopy" ~ "Within\ncanopy",
                           .default = scale))
vocc.df = vocc.df %>% 
  mutate(resolution = factor(resolution, levels = c("1km", "100m", "20m")))


p = ggplot() +
  geom_raster(data = vocc.df, aes(x, y, fill = mintemp.pres)) +
  facet_grid(rows = vars(scale), cols = vars(resolution)) +
  geom_spatvector(data = nrange, color = "black", fill = NA, linewidth = 0.5) +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_viridis_c("Minimum temperature (\u00b0C)", option = "turbo") +
  coord_sf(crs = "epsg:2067") +
  theme_classic() +
  theme(panel.background = element_rect(color = "black", fill = NA),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(20, "mm"),
        legend.key.height = unit(2,"mm"),
        legend.text = element_text(size = 6, angle = 290, vjust = 0, hjust = 0),
        legend.title = element_text(size = 8),
        legend.title.position = "top")

png("scripts/03_analysis/00_plots/supplemental_figs/mintemp_maps.png", width = 180, height = 90, res = 300, units = "mm")
p
dev.off()

