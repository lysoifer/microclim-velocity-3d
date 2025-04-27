library(tidyverse)
library(data.table)

tmax = fread("data/dataframes/analysis_dataframe_full_maxtemp.csv")
tmin = fread("data/dataframes/analysis_dataframe_full_mintemp.csv")

tmax = tmax %>% 
  group_by(var, scale, resolution) %>% 
  mutate(vocc = abs(vocc)) %>% 
  summarize(vocc = median(vocc, na.rm = T),
            tempgrad = median(tempgrad, na.rm = T),
            spatgrad = median(spatgrad, na.rm = T))
tmax = tmax %>% 
  mutate(scale = case_when(scale == "Macro" ~ "Free-air",
                   scale == "Topo" ~ "Free-air",
                   .default = scale))

tmin = tmin %>% 
  group_by(var, scale, resolution) %>% 
  mutate(vocc = abs(vocc)) %>% 
  summarize(vocc = median(vocc, na.rm = T),
            tempgrad = median(tempgrad, na.rm = T),
            spatgrad = median(spatgrad, na.rm = T))

tmin = tmin %>% 
  mutate(scale = case_when(scale == "Macro" ~ "Free-air",
                           scale == "Topo" ~ "Free-air",
                           .default = scale))


# add directional correlation tables to tmax and tmin tables
source("scripts/03_analysis/02_voccAngles/voccAngles.R")
paicor.df = paicor.df %>% 
  mutate(resolution = case_when(scale == "macro" ~ "1km",
                                scale == "micro2d.1km" ~ "1km",
                                scale == "micro3d.1km" ~ "1km",
                                scale == "meso" ~ "100m",
                                scale == "micro2d.100m" ~ "100m",
                                scale == "micro3d.100m" ~ "100m",
                                scale == "micro2d.20m" ~ "20m",
                                scale == "micro3d.20m" ~ "20m"),
         scale = case_when(scale == "macro" ~ "Free-air",
                           scale == "micro2d.1km" ~ "Land-surface",
                           scale == "micro3d.1km" ~ "Within-canopy",
                           scale == "meso" ~ "Free-air",
                           scale == "micro2d.100m" ~ "Land-surface",
                           scale == "micro3d.100m" ~ "Within-canopy",
                           scale == "micro2d.20m" ~ "Land-surface",
                           scale == "micro3d.20m" ~ "Within-canopy"))
paicor.df = paicor.df %>% rename(paicor = cor)

upslopecor.df = upslopecor.df %>% 
  mutate(resolution = case_when(scale == "macro" ~ "1km",
                                scale == "micro2d.1km" ~ "1km",
                                scale == "micro3d.1km" ~ "1km",
                                scale == "meso" ~ "100m",
                                scale == "micro2d.100m" ~ "100m",
                                scale == "micro3d.100m" ~ "100m",
                                scale == "micro2d.20m" ~ "20m",
                                scale == "micro3d.20m" ~ "20m"),
         scale = case_when(scale == "macro" ~ "Free-air",
                           scale == "micro2d.1km" ~ "Land-surface",
                           scale == "micro3d.1km" ~ "Within-canopy",
                           scale == "meso" ~ "Free-air",
                           scale == "micro2d.100m" ~ "Land-surface",
                           scale == "micro3d.100m" ~ "Within-canopy",
                           scale == "micro2d.20m" ~ "Land-surface",
                           scale == "micro3d.20m" ~ "Within-canopy"))
upslopecor.df = upslopecor.df %>% rename(upslopecor = cor)

tmax = inner_join(tmax, upslopecor.df, by = c("scale", "resolution"))
tmax = inner_join(tmax, paicor.df, by = c("scale", "resolution"))

source("scripts/03_analysis/02_voccAngles/voccAngles_tmin.R")
tmin.paicor.df = tmin.paicor.df %>% 
  mutate(resolution = case_when(scale == "macro" ~ "1km",
                                scale == "micro2d.1km" ~ "1km",
                                scale == "micro3d.1km" ~ "1km",
                                scale == "meso" ~ "100m",
                                scale == "micro2d.100m" ~ "100m",
                                scale == "micro3d.100m" ~ "100m",
                                scale == "micro2d.20m" ~ "20m",
                                scale == "micro3d.20m" ~ "20m"),
         scale = case_when(scale == "macro" ~ "Free-air",
                           scale == "micro2d.1km" ~ "Land-surface",
                           scale == "micro3d.1km" ~ "Within-canopy",
                           scale == "meso" ~ "Free-air",
                           scale == "micro2d.100m" ~ "Land-surface",
                           scale == "micro3d.100m" ~ "Within-canopy",
                           scale == "micro2d.20m" ~ "Land-surface",
                           scale == "micro3d.20m" ~ "Within-canopy"))
tmin.paicor.df = tmin.paicor.df %>% rename(paicor = cor)

tmin.upslopecor.df = tmin.upslopecor.df %>% 
  mutate(resolution = case_when(scale == "macro" ~ "1km",
                                scale == "micro2d.1km" ~ "1km",
                                scale == "micro3d.1km" ~ "1km",
                                scale == "meso" ~ "100m",
                                scale == "micro2d.100m" ~ "100m",
                                scale == "micro3d.100m" ~ "100m",
                                scale == "micro2d.20m" ~ "20m",
                                scale == "micro3d.20m" ~ "20m"),
         scale = case_when(scale == "macro" ~ "Free-air",
                           scale == "micro2d.1km" ~ "Land-surface",
                           scale == "micro3d.1km" ~ "Within-canopy",
                           scale == "meso" ~ "Free-air",
                           scale == "micro2d.100m" ~ "Land-surface",
                           scale == "micro3d.100m" ~ "Within-canopy",
                           scale == "micro2d.20m" ~ "Land-surface",
                           scale == "micro3d.20m" ~ "Within-canopy"))
tmin.upslopecor.df = tmin.upslopecor.df %>% rename(upslopecor = cor)

tmin = inner_join(tmin, tmin.upslopecor.df, by = c("scale", "resolution"))
tmin = inner_join(tmin, tmin.paicor.df, by = c("scale", "resolution"))

extdf1 = rbind(tmax, tmin)

# using coast dist dataframe for max temp - see voccSpeed.R
# vocc.df %>% 
#   group_by(scale, resolution) %>% 
#   summarize(corElev = cor(elev, spatgrad, use = "complete.obs"),
#             corCoast = cor(coast_dist, spatgrad, use = "complete.obs"))
