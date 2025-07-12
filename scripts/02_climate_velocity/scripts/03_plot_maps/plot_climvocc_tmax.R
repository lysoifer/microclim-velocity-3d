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
library(ggh4x)


# N range polygon
nrange = vect("data/cropping_polygons/NRange.shp")
nrange = project(nrange, elev)
nrange = aggregate(nrange)

# includes only forested land use
# vocc.df = fread("data/dataframes/analysis_dataframe_full_maxtemp.csv")
vocc.df = fread("data/dataframes/analysis_dataframe_full_tempbio5.csv")


vocc.df = vocc.df %>%
  select(x,y,vocc,spatgrad,tempgrad, scale, resolution, maxtemp.pres) %>%
  mutate(vocc = abs(vocc)) %>%
  group_by(x,y,scale,resolution) %>%
  summarise(vocc = mean(vocc, na.rm = T),
            spatgrad = mean(spatgrad, na.rm = T),
            tempgrad = mean(tempgrad, na.rm = T),
            maxtemp.pres = mean(maxtemp.pres, na.rm = T)) %>%
    mutate(scale = case_when(scale == "Macro" ~ "Free-air",
                             scale == "Topo" ~ "Free-air",
                             scale == "Land-surface" ~ "Land\nsurface",
                             scale == "Within-canopy" ~ "Within\ncanopy",
                             .default = scale))

vocc.df = vocc.df %>% 
  mutate(resolution = factor(resolution, levels = c("1km", "100m", "20m")))

p = ggplot() +
  facet_grid2(rows = vars(scale), cols = vars(resolution), render_empty = F) +
  geom_spatvector(data = nrange, color = NA, fill = "gray80", linewidth = 0.5, inherit.aes = F) +
  geom_raster(data = vocc.df, aes(x, y, fill = log10(abs(vocc)))) +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_viridis_c("Maximum temperature Velocity (m/yr)",
                       option = "turbo",
                       breaks = seq(-4,2,1), labels = 10^(seq(-4,2,1)),
                       na.value = "gray") +
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


png("scripts/03_analysis/00_plots/new_figs/maps/climvocc_bio5.png", width = 180, height = 90, res = 300, units = "mm")
p
dev.off()



# SPATIAL RATE OF CLIMATE CHANGE ------------------------------------------

brk_vect = vocc.df$spatgrad
r.jenksbr = classIntervals(brk_vect, n=40, style = "quantile", na.rm = T)
vocc.df = vocc.df %>% 
  as.data.frame() %>% 
  mutate(jenksbr = cut(vocc.df$spatgrad, r.jenksbr$brks, labels = seq(1,40,1), include.lowest = T, right = F))
vocc.df$jenksbr = as.numeric(as.character(vocc.df$jenksbr))


p = ggplot() +
  facet_grid2(rows = vars(scale), cols = vars(resolution), render_empty = F) +
  geom_spatvector(data = nrange, color = NA, fill = "gray80", linewidth = 0.5) +
  geom_raster(data = vocc.df, aes(x, y, fill = jenksbr)) +
  scale_x_continuous(expand = c(0,0)) +
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




png("scripts/03_analysis/00_plots/new_figs/maps/spatgrad_bio5.png", width = 180, height = 90, res = 300, units = "mm")
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


p = ggplot() +
  facet_grid2(rows = vars(scale), cols = vars(resolution), render_empty = F) +
  geom_spatvector(data = nrange, color = NA, fill = "gray80", linewidth = 0.5) +
  geom_raster(data = vocc.df, aes(x, y, fill = jenksbr)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = diverging_hcl(palette = "Blue-Red3", n = 81), breaks = c(-40, -30, -20,-10, 0, 10, 20, 30, 40),
                       labels = round(c(r.jenksbr.neg$brks[1], r.jenksbr.neg$brks[11],
                                        r.jenksbr.neg$brks[21], r.jenksbr.neg$brks[31],
                                        0, r.jenksbr.pos$brks[11], r.jenksbr.pos$brks[21], 
                                        r.jenksbr.pos$brks[31], r.jenksbr.pos$brks[41]), 3),
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

png("scripts/03_analysis/00_plots/new_figs/maps/tempgrad_bio5.png", width = 180, height = 90, res = 300, units = "mm")
p
dev.off()



# PLOT MAX TEMP PRES ------------------------------------------------------

p = ggplot() +
  facet_grid2(rows = vars(scale), cols = vars(resolution), render_empty = F) +
  geom_spatvector(data = nrange, color = NA, fill = "gray80", linewidth = 0.5) +
  geom_raster(data = vocc.df, aes(x, y, fill = maxtemp.pres)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_viridis_c("Maximum temperature (\u00b0C)", option = "turbo", 
                       limits = c(25,45)) +
  #scale_fill_gradient(limits = c(23,45), oob = scales::squish) +
  coord_sf(crs = "epsg:2067") +
  theme_classic() +
  theme(panel.background = element_rect(color = "black", fill = NA),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(20, "mm"),
        legend.key.height = unit(2,"mm"),
        legend.text = element_text(size = 6, vjust = 0, hjust = 0),
        legend.title = element_text(size = 8),
        legend.title.position = "top")

png("scripts/03_analysis/00_plots/supplemental_figs/maxtemp_maps_bio5.png", width = 180, height = 90, res = 300, units = "mm")
p
dev.off()

