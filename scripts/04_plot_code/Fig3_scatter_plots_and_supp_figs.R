library(tidyverse)
library(terra)
library(data.table)
library(colorspace)
library(ggplot2)

scale_range <- function(scale, limits = NULL, expand = TRUE) {
  expansion <- if (expand) ggplot2:::expand_default(scale) else c(0, 0)
  
  if (is.null(limits)) {
    scale$dimension(expansion)
  } else {
    limits <- ifelse(is.na(limits), scale$get_limits(), limits)
    range <- range(scale$transform(limits))
    scales::expand_range(range, expansion[1], expansion[2])
  } 
}

assignInNamespace("scale_range", scale_range, ns = "ggplot2")

vocc.df = fread("scripts/03_analysis/00_dataframes/analysis_dataframe_full_maxtemp.csv") %>% 
  mutate(scale = factor(scale, levels = c("Macro", "Topo", "Land-surface", "Within-canopy")))

# plot climvocc by temp, color by PAI

vocc.df %>% 
  group_by(scale, resolution) %>% 
  slice_sample(n=5000) %>% 
  mutate(scale2 = paste(scale, resolution, sep = "_")) %>% 
  ggplot(aes(x = maxtemp.pres, y = abs(vocc), color = pai)) +
  geom_point(size = 0.1) +
  scale_color_continuous_sequential("viridis", limits = c(0,10)) +
  guides(colour = guide_colorbar(title = "PAI")) +
  scale_x_continuous("Average maximum temperature 2015 (\u00b0C)") +
  scale_y_continuous("Climate velocity (m/yr)") +
  facet_wrap(~scale2, scales = "free") +
  theme_classic()

ggsave("scripts/03_analysis/00_plots/vocc_speed/compareScales_vocc_temp_pai1_1.png", height = 4, width = 5)


# SCATTER PLOT IN MAIN FIGS -----------------------------------------------

# spearman rank correlation for fig. 3

cor.df = vocc.df %>% 
  drop_na() %>% 
  pivot_longer(cols = c(elev, pai), names_to = "variable", values_to = "val") %>% 
  group_by(scale, var, resolution, variable) %>% 
  mutate(vocc = abs(vocc)) %>% 
  dplyr::summarise(cor = cor(val, vocc, method = "spearman")) %>%
  mutate(cor = round(cor,2)) 

elev.cor = cor.df %>% filter(var == "elev")
pai.cor = cor.df %>% filter(var == "pai")

vocc.df = vocc.df %>% 
  mutate(elev_cat = cut_width(elev, width = 50, center = 125),
         pai_cat = cut_width(pai, width = 1, center = 0.5))

vocc.elevsumm = vocc.df %>% 
  mutate(vocc = abs(vocc)) %>% 
  group_by(elev_cat, scale) %>% 
  summarise(vocc.mean = mean(vocc), n = n(), vocc.sd = sd(vocc)) %>% 
  filter(elev_cat != "[-50,0]")
vocc.elevsumm$elev = c(rep(seq(25,825,50), each = 4), rep(seq(875,925,50), each = 3))
vocc.elevsumm = vocc.elevsumm %>% filter(n > 1)

vocc.paisumm = vocc.df %>% 
  mutate(vocc = abs(vocc)) %>% 
  group_by(pai_cat, scale) %>% 
  summarise(vocc.mean = mean(vocc), n = n(), vocc.sd = sd(vocc))
vocc.paisumm$pai = c(rep(seq(0.5,7.5,1), each = 4), rep(seq(8.5,9.5,1), each = 3))

rho = "\u03C1"

elev.plt = vocc.df %>% 
  dplyr::select(vocc, elev, maxtemp.pres, scale) %>% 
  group_by(scale) %>% 
  slice_sample(n=10000) %>% 
  ggplot() +
  geom_ribbon(data = vocc.elevsumm, aes(x = elev, ymin = vocc.mean - vocc.sd, ymax = vocc.mean + vocc.sd), alpha = 0.3) +
  geom_point(aes(x = elev, y = abs(vocc)), pch = ".", color = "steelblue2") +
  geom_line(data = vocc.elevsumm, aes(x = elev, y = vocc.mean), linewidth = 1) +
  geom_text(data = elev.cor, aes(x = -Inf, y = Inf, label = paste0(rho, " = ", cor)),
            hjust = -0.1, vjust = 1.5) +
  scale_x_continuous("Elevation (m)") +
  scale_y_continuous("Climate velocity (m/yr)", expand = expansion(mult = c(0,0.2))) +
  coord_cartesian(ylim = c(0,NA)) +
  facet_wrap(~scale, nrow = 1, scales = "free_y") +
  theme_classic()

pai.plt = vocc.df %>% 
  dplyr::select(vocc, pai, maxtemp.pres, scale) %>% 
  group_by(scale) %>% 
  slice_sample(n=10000) %>% 
  ggplot() +
  geom_ribbon(data = vocc.paisumm, aes(x = pai, ymin = vocc.mean - vocc.sd, ymax = vocc.mean + vocc.sd), alpha = 0.3) +
  geom_point(aes(x = pai, y = abs(vocc)), pch = ".", color = "forestgreen") +
  geom_line(data = vocc.paisumm, aes(x = pai, y = vocc.mean), linewidth = 1) +
  geom_text(data = pai.cor, aes(x = -Inf, y = Inf, label = paste0(rho, " = ", cor)), 
            hjust = -0.1, vjust = 1.5) +
  scale_x_continuous("PAI") +
  scale_y_continuous("Climate velocity (m/yr)", expand = expansion(mult = c(0,0.2))) +
  coord_cartesian(ylim = c(0,NA)) +
  facet_wrap(~scale, nrow = 1, scales = "free_y") +
  theme_classic()

elev.plt + pai.plt + plot_layout(nrow = 2) + plot_annotation(tag_levels = "a")
ggsave("scripts/03_analysis/00_plots/vocc_speed/compareScales_vocc_elev_pai_scatter.png", height = 5, width = 8)

vocc.df %>% 
  group_by(scale) %>% 
  slice_sample(n=5000) %>% 
  ggplot(aes(x = elev, y = abs(vocc), color = pai)) +
  geom_point(size = 0.1) +
  scale_color_continuous_sequential("viridis", limits = c(0,10)) +
  guides(colour = guide_colorbar(title = "PAI")) +
  scale_x_continuous("Elelvation (m)") +
  scale_y_continuous("Climate velocity (m/yr)") +
  facet_wrap(~scale, scales = "free") +
  theme_classic()

ggsave("scripts/03_analysis/00_plots/vocc_speed/compareScales_vocc_temp_pai1.png", height = 4, width = 5)


vocc.df %>% 
  group_by(scale) %>% 
  slice_sample(n=5000) %>% 
  mutate(pai_cat = cut(pai, seq(0,10,10/3), include.lowest = T, right = T)) %>% 
  ggplot(aes(x = maxtemp.pres, y = abs(vocc), color = pai_cat)) +
  geom_point(size = 0.1) +
  scale_color_brewer(palette = "Set2") +
  scale_x_continuous("Average maximum temperature 2015 (\u00b0C)") +
  scale_y_continuous("Climate velocity (m/yr)") +
  guides(colour = guide_legend(title = "PAI", override.aes = list(size=2))) +
  facet_wrap(~scale, scales = "free") +
  theme_classic() 

ggsave("scripts/03_analysis/00_plots/vocc_speed/compareScales_vocc_temp_pai2.png", height = 4, width = 5)

vocc.df %>% 
  group_by(scale) %>% 
  slice_sample(n=5000) %>% 
  ggplot(aes(x = pai, y = spatgrad, color = pai)) +
  geom_point(size = 0.1) +
  scale_color_continuous_sequential("viridis") +
  scale_x_continuous("PAI") +
  scale_y_continuous("Spatial gradient (\u00b0C/m)") +
  guides(colour = guide_colorbar(title = "PAI", override.aes = list(size=2))) +
  facet_wrap(~scale, scales = "free") +
  theme_classic() 

vocc.df %>% 
  group_by(scale) %>% 
  slice_sample(n=5000) %>% 
  ggplot(aes(x = pai, y = tempgrad, color = pai)) +
  geom_point(size = 0.1) +
  scale_color_continuous_sequential("viridis") +
  scale_x_continuous("PAI") +
  scale_y_continuous("Temporal gradient (\u00b0C/yr)") +
  guides(colour = guide_colorbar(title = "PAI", override.aes = list(size=2))) +
  facet_wrap(~scale, scales = "free") +
  theme_classic() 

pai.sd = vocc.df %>% 
  filter(scale == "Land surface") %>% 
  dplyr::select(x,y,pai) %>% 
  rast(type = "xyz", crs = "epsg:2067") %>% 
  terra::aggregate(fact = 3, fun = "sd")

pai.df = vocc.df %>% 
  filter(scale == "Land surface") %>% 
  dplyr::select(x,y,pai)
pai.df$pai.sd = terra::extract(pai.sd, pai.df[,c("x", "y")])[,2]

pai.df %>% 
  drop_na() %>% 
  slice_sample(n=10000) %>% 
ggplot(aes(pai, pai.sd)) +
  geom_point() +
  geom_smooth(method = "lm")



temp.sd = vocc.df %>% 
  filter(scale == "Land surface") %>% 
  dplyr::select(x,y,maxtemp.pres) %>% 
  rast(type = "xyz", crs = "epsg:2067") %>% 
  terra::aggregate(fact = 3, fun = "sd")

temp.df = vocc.df %>% 
  filter(scale == "Land surface") %>% 
  dplyr::select(x,y,maxtemp.pres)
temp.df$temp.sd = terra::extract(temp.sd, temp.df[,c("x", "y")])[,2]

temp.df %>% 
  drop_na() %>% 
  slice_sample(n=10000) %>% 
  ggplot(aes(maxtemp.pres, temp.sd)) +
  geom_point() +
  geom_smooth(method = "lm")



elev.sd = vocc.df %>% 
  filter(scale == "Land surface") %>% 
  dplyr::select(x,y,elev) %>% 
  rast(type = "xyz", crs = "epsg:2067") %>% 
  terra::aggregate(fact = 3, fun = "sd")

elev.df = vocc.df %>% 
  filter(scale == "Land surface") %>% 
  dplyr::select(x,y,elev)
elev.df$elev.sd = terra::extract(elev.sd, elev.df[,c("x", "y")])[,2]

elev.df %>% 
  drop_na() %>% 
  slice_sample(n=10000) %>% 
  ggplot(aes(elev, elev.sd)) +
  geom_point() +
  geom_smooth(method = "lm")

tpe.df = inner_join(temp.df, pai.df, by = c("x", "y")) %>% 
  inner_join(elev.df, by = c("x", "y"))
tpe.df %>% 
  drop_na() %>% 
  slice_sample(n=10000) %>% 
  ggplot(aes(pai, temp.sd)) +
  geom_point(color = "gray", size = 0.5) +
  geom_smooth() +
  scale_x_continuous('PAI') +
  scale_y_continuous("SD Maximum Temperature (\u00b0C)") +
  theme_classic()

ggsave("scripts/03_analysis/00_plots/supplemental_figs/tmax_pai.png", height = 5, width = 5)

tpe.df %>% 
  drop_na() %>% 
  slice_sample(n=10000) %>% 
  ggplot(aes(pai, elev.sd)) +
  geom_point() +
  geom_smooth(method = "lm")

# pts = read.csv('03_analysis/00_dataframes/vocc_relhgt_df.csv')

meso = rast("scripts/02_climate_velocity/output/mesoclimate/avg_daily_max_temp/vocc.tif")
macro = rast("scripts/02_climate_velocity/output/macroclimate/mean_monthly_max_temp/vocc.tif")

elev.micro = rast("data/topography/dem_reproj.tif")
elev.meso = terra::resample(elev.micro, meso)
elev.macro = terra::resample(elev.micro, macro)

pai.micro = rast("data/PAI/pai_NRange_rmMeters0to2_rmPAIgt10.tif")
pai.meso = resample(pai.micro, meso)
pai.macro = resample(pai.micro, macro)

micro = c(elev.micro, pai.micro)
names(micro) = c("elev", "pai")
micro = as.data.frame(micro) %>% 
  drop_na() %>% 
  slice_sample(n = 5000) %>% 
  mutate(scale = "Micro")

meso = c(elev.meso, pai.meso)
names(meso) = c("elev", "pai")
meso = as.data.frame(meso) %>% 
  drop_na() %>% 
  slice_sample(n = 5000) %>% 
  mutate(scale = "Topo")

macro = c(elev.macro, pai.macro)
names(macro) = c("elev", "pai")
macro = as.data.frame(macro) %>% 
  drop_na() %>% 
  mutate(scale = "Macro")

pts = bind_rows(micro, meso, macro) %>% 
  mutate(scale = factor(scale, levels = c("Macro", "Topo", "Micro")))


# # TEMPERATURE VS PAI ------------------------------------------------------

maxtemp.pres.micro = rast("data/microclim_3D/mosaics/meanTmax_pres_02.tif")
maxtemp.pres.micro = crop(maxtemp.pres.micro, pai.micro)

temp.pai = c(maxtemp.pres.micro, pai.micro)
names(temp.pai) = c("maxtemp", "pai")
temp.pai = temp.pai %>% 
  as.data.frame() %>% 
  drop_na() %>% 
  slice_sample(n = 5000)

# 
# temp.pai = pts %>% 
#   select(pai.macro, maxtemp.pres.macro,
#          pai.meso, maxtemp.pres.meso,
#          pai.micro, maxtemp.pres.micro)
# 
ggplot(temp.pai, aes(pai, maxtemp/100)) +
  geom_point(size = 1, alpha = 0.5, color = "black") +
  scale_x_continuous('PAI')+
  scale_y_continuous("Average daily maximum temperature (\u00b0C)") +
  theme_classic()

ggsave("scripts/03_analysis/00_plots/supplemental_figs/temp_pai_micro.png", width = 4, height = 4)


# ELEV VS PAI -------------------------------------------------------------

# elev.pai = pts %>% 
#   select(dem.macro, pai.macro,
#          dem.meso, pai.meso, 
#          dem.micro, pai.micro)
# 
# elev.pai.micro = elev.pai %>% 
#   select(dem.micro, pai.micro) %>% 
#   mutate(scale = "micro") %>% 
#   rename(elev = dem.micro, pai = pai.micro)
# 
# elev.pai.meso = elev.pai %>% 
#   select(dem.meso, pai.meso) %>% 
#   mutate(scale = "topo") %>% 
#   rename(elev = dem.meso, pai = pai.meso)
# 
# elev.pai.macro = elev.pai %>% 
#   select(dem.macro, pai.macro) %>% 
#   mutate(scale = "macro") %>% 
#   rename(elev = dem.macro, pai = pai.macro)
# 
# elev.pai = bind_rows(elev.pai.macro, elev.pai.meso, elev.pai.micro) %>% 
#   mutate(scale = factor(scale, levels = c("macro", "topo", "micro")))

ggplot(pts, aes(elev, pai)) +
  geom_point(size = 1, alpha = 0.5) +
  scale_x_continuous("Elevation (m)") +
  scale_y_continuous("PAI") +
  facet_grid(cols = vars(scale)) +
  theme_classic()

ggsave("scripts/03_analysis/00_plots/supplemental_figs/elev_pai.png", width = 9, height = 3)  
