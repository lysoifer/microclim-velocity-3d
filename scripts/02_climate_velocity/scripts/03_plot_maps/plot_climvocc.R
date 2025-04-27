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

plot_climvocc = function(r, hill_plot, plt_lab, legend.title, roundto) {
  r.df = as.data.frame(r, xy = T)
  r.jenksbr = classIntervals(r.df[,3], n=19, style = "quantile", na.rm = T)
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
    annotate(geom = "text", x = 636000, y = 1197200, label = plt_lab, fontface = "bold", size = 5, hjust = 0) +
    new_scale_fill() +
    #geom_spatraster(data = shade.meso, alpha = 0.5) +
    #scale_fill_gradientn(colors = pal_greys, na.value = NA, guide = "none") +
    theme_classic() +
    theme(panel.background = element_rect(color = "black"),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.key.height = unit(10, "pt"))
  
  return(plt)
}

# elev
elev = rast("data/topography/dem_reproj.tif")

# 3d microclim velocity for the 3rd quarter of the canopy
# micro3d.vocc = rast('02_climate_velocity/output/3D/avg_daily_maxTemp/relhgt_convert/tmax_relhgt_vocc.tif')

# DATA FRAMES COMMENTED OUT DUE TO LONGISH RUN TIME - LOAD AS RASTER
# micro3d = read.csv("03_analysis/00_dataframes/micro3d_canopy_dataframe.csv", row.names = "X")
# # already took abs(vocc) when making the dataframe - see micro3d_analysis_dataframe.R
# # summarize by xy coord
# micro3d = micro3d %>% 
#   group_by(x, y) %>% 
#   summarize(vocc = mean(vocc, na.rm = T),
#             spatgrad = mean(spatgrad, na.rm = T),
#             tempgrad = mean(tempgrad, na.rm = T))
# micro3d.r = rast(micro3d, type = "xyz", crs = crs(elev))
# 
# # this is the mean vocc, spatgrad, and tempgrad for the upper half of the canopy
# writeRaster(micro3d.r, "03_analysis/00_dataframes/micro3d_upper_half_canopy.tif")

# micro3d.r = rast("03_analysis/00_dataframes/micro3d_upper_half_canopy.tif")
# 
# # get absolute value
# # micro3d.r = abs(micro3d.r)
# 
# # take average of the top two quarters of the canopy so that 3d vocc represents canopy on average
# # micro3d.vocc = mean(micro3d.vocc$bkmean03, micro3d.vocc$bkmean04, na.rm = T)
# 
# # remove outliers
# global(micro3d.r$vocc, fun=quantile, probs=seq(0.95,1,0.01), na.rm = T)
# 
# micro3d.thresh = global(micro3d.r$vocc, fun=quantile, probs=0.95, na.rm = T)
# micro3d.vocc = micro3d.r$vocc %>% filter(vocc < micro3d.thresh[1,1])
# 
# 
# # 2d microclim velocity for 2m above the ground
# micro2d.vocc = rast("02_climate_velocity/output/2D/avg_daily_maxTemp/vocc_02m.tif")
# micro2d.vocc = abs(micro2d.vocc$vocc)
# global(micro2d.vocc$vocc, fun=quantile, probs=seq(0.95,0.99,0.01), na.rm = T)
# 
# micro2d.thresh = global(micro2d.vocc$vocc, fun=quantile, probs=0.95, na.rm = T)
# micro2d.vocc = micro2d.vocc %>% filter(vocc < micro2d.thresh[1,1])
# 
# # mesoclimate velocity
# meso.vocc = rast("02_climate_velocity/output/mesoclimate/avg_daily_max_temp/vocc.tif")
# meso.vocc = abs(meso.vocc$vocc)
# global(meso.vocc$vocc, fun=quantile, probs=seq(0.95,0.99,0.01), na.rm = T)
# 
# # remove values above 95 percentile due to extreme values caused by very small spatial gradients
# meso.thresh = global(meso.vocc$vocc, fun=quantile, probs=0.95, na.rm = T)
# meso.vocc = meso.vocc %>% tidyterra::filter(vocc < meso.thresh[1,1])
# 
# # macroclimate velocity
# macro.vocc = rast("02_climate_velocity/output/macroclimate/mean_monthly_max_temp/vocc.tif")
# macro.vocc = abs(macro.vocc$vocc)

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
vocc.df = fread("scripts/03_analysis/00_dataframes/analysis_dataframe_full.csv")
meso.vocc = vocc.df %>% 
  filter(scale == "Topo") %>% 
  dplyr::select(x,y,vocc) %>% 
  mutate(vocc = abs(vocc)) %>% 
  rast(type = "xyz", crs = "epsg:2067")

macro.vocc = vocc.df %>% 
  filter(scale == "Macro") %>% 
  dplyr::select(x,y,vocc) %>% 
  mutate(vocc = abs(vocc)) %>% 
  rast(type = "xyz", crs = "epsg:2067")

micro2d.vocc = vocc.df %>% 
  filter(scale == "Land surface") %>% 
  dplyr::select(x,y,vocc) %>% 
  mutate(vocc = abs(vocc)) %>% 
  rast(type = "xyz", crs = "epsg:2067")

micro3d.vocc = vocc.df %>% 
  filter(scale == "Within-canopy") %>% 
  dplyr::select(x,y,vocc) %>% 
  mutate(vocc = abs(vocc)) %>% 
  group_by(x,y) %>% 
  summarise(vocc = mean(vocc, na.rm = T)) %>% 
  rast(type = "xyz", crs = "epsg:2067")


meso.vocc.plt = plot_climvocc(r = meso.vocc, hill_plot = hill_plot, plt_lab = "Topo", legend.title = "m/yr", roundto = 2)
macro.vocc.plt = plot_climvocc(r = macro.vocc, hill_plot = hill_plot, plt_lab = "Macro", legend.title = "m/yr", roundto = 2)
micro2d.vocc.plt = plot_climvocc(r = micro2d.vocc, hill_plot = hill_plot, 
                                 plt_lab = "Land Surface", legend.title = "m/yr", roundto = 2)
micro3d.vocc.plt = plot_climvocc(r = micro3d.vocc, hill_plot = hill_plot, plt_lab = "Within-canopy", legend.title = "m/yr", roundto = 2)

climvocc.plt = macro.vocc.plt +  micro2d.vocc.plt + meso.vocc.plt + micro3d.vocc.plt + plot_layout(
  design = "12
            34"
)



# SPATIAL RATE OF CLIMATE CHANGE ------------------------------------------

meso.spatgrad = vocc.df %>% 
  filter(scale == "Topo") %>% 
  dplyr::select(x,y,spatgrad) %>% 
  rast(type = "xyz", crs = "epsg:2067")

macro.spatgrad = vocc.df %>% 
  filter(scale == "Macro") %>% 
  dplyr::select(x,y,spatgrad) %>% 
  rast(type = "xyz", crs = "epsg:2067")

micro2d.spatgrad = vocc.df %>% 
  filter(scale == "Land surface") %>% 
  dplyr::select(x,y,spatgrad) %>% 
  rast(type = "xyz", crs = "epsg:2067")

micro3d.spatgrad = vocc.df %>% 
  filter(scale == "Within-canopy") %>% 
  dplyr::select(x,y,spatgrad) %>% 
  group_by(x,y) %>% 
  summarise(spatgrad = mean(spatgrad, na.rm = T)) %>% 
  rast(type = "xyz", crs = "epsg:2067")

meso.spatgrad.plt = plot_climvocc(r = meso.spatgrad, hill_plot = hill_plot, plt_lab = "Topo", legend.title = "\u00b0C/m", roundto = 4)
macro.spatgrad.plt = plot_climvocc(r = macro.spatgrad, hill_plot = hill_plot, plt_lab = "Macro", legend.title = "\u00b0C/m", roundto = 4)
micro2d.spatgrad.plt = plot_climvocc(r = micro2d.spatgrad, hill_plot = hill_plot, 
                                 plt_lab = "Land Surface", legend.title = "\u00b0C/m", roundto = 4)
micro3d.spatgrad.plt = plot_climvocc(r = micro3d.spatgrad, hill_plot = hill_plot, plt_lab = "Within-canopy", legend.title = "\u00b0C/m", roundto = 4)

spatgrad.plt = macro.spatgrad.plt +  micro2d.spatgrad.plt + meso.spatgrad.plt + micro3d.spatgrad.plt + plot_layout(
  design = "12
            34"
)


# TEMPORAL GRADIENT -------------------------------------------------------

meso.tempgrad = vocc.df %>% 
  filter(scale == "Topo") %>% 
  dplyr::select(x,y,tempgrad) %>% 
  rast(type = "xyz", crs = "epsg:2067")

macro.tempgrad = vocc.df %>% 
  filter(scale == "Macro") %>% 
  dplyr::select(x,y,tempgrad) %>% 
  rast(type = "xyz", crs = "epsg:2067")

micro2d.tempgrad = vocc.df %>% 
  filter(scale == "Land surface") %>% 
  dplyr::select(x,y,tempgrad) %>% 
  rast(type = "xyz", crs = "epsg:2067")

micro3d.tempgrad = vocc.df %>% 
  filter(scale == "Within-canopy") %>% 
  dplyr::select(x,y,tempgrad) %>% 
  group_by(x,y) %>% 
  summarise(tempgrad = mean(tempgrad, na.rm = T)) %>% 
  rast(type = "xyz", crs = "epsg:2067")

meso.tempgrad.plt = plot_climvocc(r = meso.tempgrad, hill_plot = hill_plot, plt_lab = "Topo", legend.title = "\u00b0C/m", roundto = 4)
macro.tempgrad.plt = plot_climvocc(r = macro.tempgrad, hill_plot = hill_plot, plt_lab = "Macro", legend.title = "\u00b0C/m", roundto = 4)
micro2d.tempgrad.plt = plot_climvocc(r = micro2d.tempgrad, hill_plot = hill_plot, 
                                     plt_lab = "Land Surface", legend.title = "\u00b0C/m", roundto = 2)
micro3d.tempgrad.plt = plot_climvocc(r = micro3d.tempgrad, hill_plot = hill_plot, plt_lab = "Within-canopy", legend.title = "\u00b0C/m", roundto = 2)

tempgrad.plt = macro.tempgrad.plt +  micro2d.tempgrad.plt + meso.spatgrad.plt + micro3d.tempgrad.plt + plot_layout(
  design = "12
            34"
)







# get color palette with uneven color distribution for elevation
grad_hypso = hypso.colors2(10, "dem_poster")

# plot elevation
autoplot(elev) +
  scale_fill_gradientn(colors = grad_hypso, na.value = NA)

minmax = minmax(elev)[,1]

elev.plt = 
  ggplot() +
  #ggnewscale::new_scale_fill() +
  geom_spatraster(data = elev, alpha = 1) +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_hypso_tint_c("dem_poster", limits = c(0,minmax[2]), name = "m") +
  annotate(geom = "text", x = 636000, y = 1197200, label = "Elevation", fontface = "bold", size = 6, hjust = 0) +
  new_scale_fill() +
  geom_spatraster(data = shade.meso, alpha = 0.5) +
  scale_fill_gradientn(colors = pal_greys, na.value = NA, guide = "none") + 
  geom_spatvector(data = nrange, color = "black", fill = NA, linewidth = 1) +
  theme_classic() +
  theme(panel.background = element_rect(color = "black"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.key.height = unit(10, "pt"))

pai = rast("data/PAI/pai_NRange_rmMeters0to2_rmPAIgt10.tif")

pai.plt =  ggplot() +
  geom_spatraster(data = pai, alpha = 1) +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_continuous_sequential("Terrain 2", na.value = NA, name = "PAI") +
  annotate(geom = "text", x = 636000, y = 1197200, label = "PAI", fontface = "bold", size = 6, hjust = 0) +
  new_scale_fill() +
  #geom_spatraster(data = shade.meso, alpha = 0.5) +
  #scale_fill_gradientn(colors = pal_greys, na.value = NA, guide = "none") + 
  geom_spatvector(data = nrange, color = "black", fill = NA, linewidth = 1) +
  theme_classic() +
  theme(panel.background = element_rect(color = "black"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.key.height = unit(10, "pt"))




# test plots --------------------------------------------------------------

vocc_summ.plt = function(r.df, title, roundto) {
  
  r.df = r.df %>%
    mutate(elev_cat = cut(elev, breaks = seq(0,950,50), labels = seq(50,950,50),
                          include.lowest = T),
           elev_cat = as.numeric(as.character(elev_cat)),
           pai_cat = cut(pai, breaks = seq(0,10,0.5), labels = seq(0.5,10,0.5),
                         include.lowest = T),
           pai_cat = as.numeric(as.character(pai_cat))) %>% 
  group_by(elev_cat, pai_cat) %>% 
  summarise(vocc = mean(vocc, na.rm = T)) %>% 
  drop_na()
  
  r.jenksbr = classIntervals(r.df$vocc, n=19, style = "quantile", na.rm = T)
  jenksbr = cut(r.df$vocc, r.jenksbr$brks, labels = F, include.lowest = T, right = F)
  r.df$jenksbr = jenksbr
    
  p = ggplot(r.df) +
    geom_raster(aes(x = pai_cat, y = elev_cat, fill = jenksbr)) +
    #scale_fill_viridis("climate velocity (m/yr)") +
    scale_fill_gradientn("climate velocity (m/yr)", colors = pal_viridis()(20), breaks = c(1,5,10,15,19), 
                         labels = round(c(r.jenksbr$brks[c(1,5,10,15)], max(r.jenksbr$brks)),roundto),
                         na.value = NA) +
    guides(fill = guide_colorbar(title.position = "right", title.vjust = 0.5, title.hjust = 0.5)) +
    scale_x_continuous("PAI", breaks = seq(0,10,2), expand = c(0,0)) +
    scale_y_continuous("Elevation (m)", breaks = seq(0,1000,200), expand = c(0,0)) +
    theme_classic() +
    ggtitle(title) +
    theme(legend.title = element_text(angle = 90, hjust = 0.5),
          legend.title.align = 0.5,
          legend.direction = "vertical",
          legend.box.spacing = unit(0.5, "mm"),
          plot.title = element_text(hjust = 0.5))
  
  return(p)

}


vocc.df = fread("scripts/03_analysis/00_dataframes/analysis_dataframe_full.csv") %>% 
  mutate(scale = factor(scale, levels = c("Macro", "Topo", "Land surface", "Within-canopy")))

macro.vocc = vocc.df %>% 
  filter(scale == "Macro") %>% 
  dplyr::select(x, y, vocc, elev, pai)

meso.vocc = vocc.df %>% 
  filter(scale == "Topo") %>% 
  dplyr::select(x, y, vocc, elev, pai)

micro2d.vocc = vocc.df %>% 
  filter(scale == "Land surface") %>% 
  dplyr::select(x, y, vocc, elev, pai) 

micro3d.vocc = vocc.df %>% 
  filter(scale == "Within-canopy") %>% 
  dplyr::select(x, y, vocc, elev, pai) 

micro3d.summ.plt = vocc_summ.plt(r.df = micro3d.vocc, title = "Within-canopy", 2)
micro2d.summ.plt = vocc_summ.plt(r.df = micro2d.vocc, title = "Land Surface", 0)
meso.summ.plt = vocc_summ.plt(r.df = meso.vocc, title = "Topo", 0)
macro.summ.plt = vocc_summ.plt(r.df = macro.vocc, title = "Macro", 0)


