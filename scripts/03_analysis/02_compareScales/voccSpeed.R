library(tidyverse)
library(ggplot2)
library(colorspace)
library(patchwork)
library(tidyterra)
library(terra)
library(data.table)
library(RColorBrewer)
library(ggh4x)

vocc.df = fread("scripts/03_analysis/00_dataframes/analysis_dataframe_full_maxtemp.csv") %>% 
  mutate(scale = case_when(scale == "Land-surface" ~ "Land\nsurface",
                           scale == "Within-canopy" ~ "Within-\ncanopy",
                           .default = scale),
         scale = factor(scale, levels = c("Macro", "Topo", "Land\nsurface", "Within-\ncanopy")))

vocc.tmin = fread("scripts/03_analysis/00_dataframes/analysis_dataframe_full_mintemp.csv") %>% 
  mutate(scale = case_when(scale == "Land-surface" ~ "Land\nsurface",
                           scale == "Within-canopy" ~ "Within-\ncanopy",
                           .default = scale),
         scale = factor(scale, levels = c("Macro", "Topo", "Land\nsurface", "Within-\ncanopy")))

# scale refers to macro (ambient), topo (topographic), or land surface/within canopy (accounting for vegetation in 2D or 3D)
vocc.summ.tmax = vocc.df %>% 
  group_by(scale, resolution) %>% 
  summarise(median_vocc = median(vocc, na.rm = T),
            max_vocc = max(vocc, na.rm = T),
            min_vocc = min(vocc, na.rm = T)) %>% 
  mutate(var = "tmax")

vocc.summ.tmin = vocc.tmin %>% 
  group_by(scale, resolution) %>% 
  summarise(median_vocc = median(vocc, na.rm = T),
            max_vocc = max(vocc, na.rm = T),
            min_vocc = min(vocc, na.rm = T)) %>% 
  mutate(var = "tmin")

vocc.df = vocc.df %>% rename(temp.pres = maxtemp.pres)
vocc.tmin = vocc.tmin %>% rename(temp.pres = mintemp.pres)

vocc.all = rbind(vocc.df, vocc.tmin) %>% 
  mutate(var = case_when(var == "maxtemp" ~ "Maximum temperature",
                         var == "mintemp" ~ "Minimum temperature"))

# Figure 2: Impacts of vegetation regardless of spatial scale -----------------------

scales = list(
  scale_y_continuous(limits = c(0,100)),
  scale_y_continuous(limits = c(0,100)),
  scale_y_continuous(limits = c(0,0.04)),
  scale_y_continuous(limits = c(-0.005,0.015)),
  scale_y_continuous(limits = c(0,0.3)),
  scale_y_continuous(limits = c(0,0.05))
)


vocc.all %>% 
  mutate(scale = case_when(scale == "Macro" ~ "Free-air",
                           scale == "Topo" ~ "Free-air",
                           .default = scale),
         vocc = abs(vocc)) %>% 
  filter(resolution == "1km" | resolution == "100m") %>% 
  dplyr::select(tempgrad, spatgrad, vocc, var, scale, resolution) %>% 
  rename("Spatial rate" = spatgrad, "Temporal rate" = tempgrad, "Climate velocity =\ntemporal/spatial" = vocc) %>% 
  pivot_longer(cols = 1:3, names_to = "facet", values_to = "val") %>% 
  mutate(facet = factor(facet, levels = c("Climate velocity =\ntemporal/spatial", "Temporal rate", "Spatial rate")),
         resolution = factor(resolution, levels = c("1km", "100m"))) %>% 
  ggplot(aes(x = resolution, y=val, fill = scale, color = scale)) +
  #geom_jitter(color = "gray50", alpha = 0.3, shape = 20, size = 0.2) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA, linewidth = 0.25) +
  facet_grid2(vars(facet), vars(var), scales = "free", independent = "y", switch = "y") +
  facetted_pos_scales(y=scales) +
  scale_fill_manual(values = c("royalblue4", "royalblue2", "steelblue1")) +
  scale_color_manual(values = c("royalblue4", "royalblue2", "steelblue1")) +
  theme_classic() +
  theme(legend.position = 'bottom',
        axis.text = element_text(size = 6),
        panel.background = element_rect(color = "black", fill = NA),
        axis.title = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 8),
        legend.title = element_blank(),
        legend.box.background = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 6),
        axis.line = element_blank(),
        legend.margin = margin(0,0,0,0))

ggsave("scripts/03_analysis/00_plots/new_figs/fig_02_voccByProximity.png", height = 100, width = 90, units = "mm", dpi = 300)

  

# Figure 3: Impact of spatial scale on forest velocities ------------------

scales = list(
  scale_y_continuous(limits = c(0,100)),
  scale_y_continuous(limits = c(0,60)),
  scale_y_continuous(limits = c(0,0.04)),
  scale_y_continuous(limits = c(-0.005,0.012)),
  scale_y_continuous(limits = c(0,0.35)),
  scale_y_continuous(limits = c(0,0.06))
)


vocc.all %>% 
  mutate(scale = case_when(scale == "Macro" ~ "Free-air",
                           scale == "Topo" ~ "Free-air",
                           .default = scale),
         vocc = abs(vocc)) %>% 
  filter(scale != "Free-air") %>% 
  dplyr::select(tempgrad, spatgrad, vocc, var, scale, resolution) %>% 
  rename("Spatial rate" = spatgrad, "Temporal rate" = tempgrad, "Climate velocity =\ntemporal/spatial" = vocc) %>% 
  pivot_longer(cols = 1:3, names_to = "facet", values_to = "val") %>% 
  mutate(facet = factor(facet, levels = c("Climate velocity =\ntemporal/spatial", "Temporal rate", "Spatial rate")),
         resolution = factor(resolution, levels = c("1km", "100m", "20m"))) %>% 
  # group_by(scale, resolution, var) %>% 
  # slice_sample(n = 5000) %>% 
  ggplot(aes(x = scale, y=val, fill = resolution, color = resolution)) +
  #geom_jitter(color = "gray50", alpha = 0.3, shape = 20, size = 0.2) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA, linewidth = 0.25) +
  facet_grid2(vars(facet), vars(var), scales = "free", independent = "y", switch = "y") +
  facetted_pos_scales(y=scales) +
  scale_fill_manual(values = c("darkgreen", "springgreen3", "palegreen2")) +
  scale_color_manual(values = c("darkgreen", "springgreen3", "palegreen2")) +
  theme_classic() +
  theme(legend.position = 'bottom',
        axis.text = element_text(size = 6),
        panel.background = element_rect(color = "black", fill = NA),
        axis.title = element_blank(),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 8),
        legend.title = element_blank(),
        legend.box.background = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 6),
        axis.line = element_blank(),
        legend.margin = margin(0,0,0,0))

ggsave("scripts/03_analysis/00_plots/new_figs/fig_03_voccByResolution.png", height = 100, width = 90, units = "mm", dpi = 300)