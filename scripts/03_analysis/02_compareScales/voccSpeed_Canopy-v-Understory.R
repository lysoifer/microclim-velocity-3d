library(dplyr)
library(terra)
library(ggplot2)
library(tidyverse)
# compare 2D climate velocities in the canopy vs in the understory
# here, canopy = the top quarter of the canopy, which was calculated from temp in the top quarter of the canopy
# understory = 2m above the ground

# prep landcover
micro2d = rast("scripts/02_climate_velocity/output/2D/temp_bio5/vocc_02m.tif")
micro2d.100m = rast("scripts/02_climate_velocity/output/2D/temp_bio5/aggregated_100m/vocc.tif")
micro2d.1km = rast("scripts/02_climate_velocity/output/2D/temp_bio5/aggregated_1km/vocc.tif")
meso = rast("scripts/02_climate_velocity/output/mesoclimate/avg_daily_max_temp/vocc.tif") # this is max temp (temp_bio5)
macro = rast("scripts/02_climate_velocity/output/macroclimate/temp_bio5/vocc.tif")

dem.micro = rast('data/topography/dem_reproj.tif')
dem.micro = extend(dem.micro, 1)
dem.meso = resample(dem.micro, meso)
dem.macro = resample(dem.micro, macro)
dem.micro2d.1km = resample(dem.micro, micro2d.1km)

landuse.micro = rast('data/Helmer_2012_Beard_vegetation/201403_Trin_USDA_pWGS84_forest_tree_communities_for_Trinidad_from_Landsat.tif')
landuse.micro = project(landuse.micro, dem.micro, method = "near")
landuse.meso = terra::resample(landuse.micro, meso, method = "near")
landuse.macro = resample(landuse.micro, macro, method = "near")
landuse.micro2d.1km = resample(landuse.micro, dem.micro2d.1km, method = "near")
landuse.class = read.csv("data/Helmer_2012_Beard_vegetation/Helmer_classification.csv")
landuse.class = landuse.class %>% dplyr::select(Value, LU_level_1)

# tmax 20m  ---------------------------------------------------------------
# UPDATE TO REMOVE TOP 5TH QUANTILE AND UPDATE TO TEMP BIO5
vocc_u.tmax.20 = rast("scripts/02_climate_velocity/output/2D/temp_bio5/vocc_02m.tif")[[1]]
vocc_c.tmax.20 = rast("scripts/02_climate_velocity/output/2D/temp_bio5_canopy/vocc_q4m.tif")[[1]]
vocc_u.tmax.20 = crop(vocc_u.tmax.20, vocc_c.tmax.20)
vocc_u.tmax.20 = mask(vocc_u.tmax.20, vocc_c.tmax.20)

spatgrad_u.tmax.20 = rast("scripts/02_climate_velocity/output/2D/temp_bio5/spatialGradient_02m.tif")[[1]]
spatgrad_c.tmax.20 = rast("scripts/02_climate_velocity/output/2D/temp_bio5_canopy/spatialGradient_q4m.tif")[[1]]
spatgrad_u.tmax.20 = crop(spatgrad_u.tmax.20, spatgrad_c.tmax.20)
spatgrad_u.tmax.20 = mask(spatgrad_u.tmax.20, spatgrad_c.tmax.20)

tempgrad_u.tmax.20 = rast("scripts/02_climate_velocity/output/2D/temp_bio5/temporalGradient_02m.tif")[[1]]
tempgrad_c.tmax.20 = rast("scripts/02_climate_velocity/output/2D/temp_bio5_canopy/temporalGradient_q4m.tif")[[1]]
tempgrad_u.tmax.20 = crop(tempgrad_u.tmax.20, tempgrad_c.tmax.20)
tempgrad_u.tmax.20 = mask(tempgrad_u.tmax.20, tempgrad_c.tmax.20)

landuse.micro.c = crop(landuse.micro, vocc_c.tmax.20)
tmax_u.20 = c(vocc_u.tmax.20, spatgrad_u.tmax.20, tempgrad_u.tmax.20, landuse.micro.c)
names(tmax_u.20) = c("vocc", "spatgrad", "tempgrad", "landuse") 
tmax_u.20 = as.data.frame(tmax_u.20, xy = T) %>% 
  mutate(resolution = "20m",
         var = "Maximum temperature",
         height = "Land surface",
         vocc = ifelse(vocc == Inf, NA, vocc),
         vocc = abs(vocc)) %>% 
  filter(vocc < quantile(vocc, probs = 0.99, na.rm = T)) %>% 
  drop_na()

tmax_u.20 = left_join(tmax_u.20, landuse.class, by = c("landuse" = "Value"))
tmax_u.20 = tmax_u.20 %>% 
  filter(LU_level_1 == "Forest including forest/shrub land")

landuse.micro.c = crop(landuse.micro, vocc_c.tmax.20)
tmax_c.20 = c(vocc_c.tmax.20, spatgrad_c.tmax.20, tempgrad_c.tmax.20, landuse.micro.c)
names(tmax_c.20) = c("vocc", "spatgrad", "tempgrad", "landuse") 
tmax_c.20 = as.data.frame(tmax_c.20, xy = T) %>% 
  mutate(resolution = "20m",
         var = "Maximum temperature",
         height = "Canopy",
         vocc = ifelse(vocc == Inf, NA, vocc),
         vocc = abs(vocc)) %>% 
  filter(vocc < quantile(vocc, probs = 0.99, na.rm = T)) %>% 
  drop_na()

tmax_c.20 = left_join(tmax_c.20, landuse.class, by = c("landuse" = "Value"))
tmax_c.20 = tmax_c.20 %>% 
  filter(LU_level_1 == "Forest including forest/shrub land")


tmax.20 = bind_rows(tmax_c.20, tmax_u.20)

tmax.20 %>% 
  filter(!is.infinite(vocc)) %>% 
  drop_na() %>% 
  slice_sample(n=10000) %>% 
  ggplot(aes(height, tempgrad)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0,0.1))


# tmin 20m ----------------------------------------------------------------

vocc_u.tmin.20 = rast("scripts/02_climate_velocity/output/2D/temp_bio6/vocc_02m.tif")[[1]]
vocc_c.tmin.20 = rast("scripts/02_climate_velocity/output/2D/temp_bio6_canopy/vocc_q4m.tif")[[1]]
vocc_u.tmin.20 = crop(vocc_u.tmin.20, vocc_c.tmin.20)
vocc_u.tmin.20 = mask(vocc_u.tmin.20, vocc_c.tmin.20)

spatgrad_u.tmin.20 = rast("scripts/02_climate_velocity/output/2D/temp_bio6/spatialGradient_02m.tif")[[1]]
spatgrad_c.tmin.20 = rast("scripts/02_climate_velocity/output/2D/temp_bio6_canopy/spatialGradient_q4m.tif")[[1]]
spatgrad_u.tmin.20 = crop(spatgrad_u.tmin.20, spatgrad_c.tmin.20)
spatgrad_u.tmin.20 = mask(spatgrad_u.tmin.20, spatgrad_c.tmin.20)

tempgrad_u.tmin.20 = rast("scripts/02_climate_velocity/output/2D/temp_bio6/temporalGradient_02m.tif")[[1]]
tempgrad_c.tmin.20 = rast("scripts/02_climate_velocity/output/2D/temp_bio6_canopy/temporalGradient_q4m.tif")[[1]]
tempgrad_u.tmin.20 = crop(tempgrad_u.tmin.20, tempgrad_c.tmin.20)
tempgrad_u.tmin.20 = mask(tempgrad_u.tmin.20, tempgrad_c.tmin.20)

tmin_u.20 = c(vocc_u.tmin.20, spatgrad_u.tmin.20, tempgrad_u.tmin.20, landuse.micro.c)
names(tmin_u.20) = c("vocc", "spatgrad", "tempgrad", "landuse") 
tmin_u.20 = as.data.frame(tmin_u.20, xy = T) %>% 
  mutate(resolution = "20m",
         var = "Minimum temperature",
         height = "Land surface",
         vocc = ifelse(vocc == Inf, NA, vocc),
         vocc = abs(vocc)) %>% 
  filter(vocc < quantile(vocc, probs = 0.99, na.rm = T)) %>% 
  drop_na()

tmin_u.20 = left_join(tmin_u.20, landuse.class, by = c("landuse" = "Value"))
tmin_u.20 = tmin_u.20 %>% 
  filter(LU_level_1 == "Forest including forest/shrub land")

tmin_c.20 = c(vocc_c.tmin.20, spatgrad_c.tmin.20, tempgrad_c.tmin.20, landuse.micro.c)
names(tmin_c.20) = c("vocc", "spatgrad", "tempgrad", "landuse") 
tmin_c.20 = as.data.frame(tmin_c.20, xy = T) %>% 
  mutate(resolution = "20m",
         var = "Minimum temperature",
         height = "Canopy",
         vocc = ifelse(vocc == Inf, NA, vocc),
         vocc = abs(vocc)) %>% 
  filter(vocc < quantile(vocc, probs = 0.99, na.rm = T)) %>% 
  drop_na()

tmin_c.20 = left_join(tmin_c.20, landuse.class, by = c("landuse" = "Value"))
tmin_c.20 = tmin_c.20 %>% 
  filter(LU_level_1 == "Forest including forest/shrub land")

tmin.20 = bind_rows(tmin_c.20, tmin_u.20)

t.20 = bind_rows(tmin.20, tmax.20)


# tmax 100m ---------------------------------------------------------------
vocc_u.tmax.100 = rast("scripts/02_climate_velocity/output/2D/temp_bio5/aggregated_100m/vocc.tif")[[1]]
vocc_c.tmax.100 = rast("scripts/02_climate_velocity/output/2D/temp_bio5_canopy/aggregated_100m/vocc_q4.tif")[[1]]

spatgrad_u.tmax.100 = rast("scripts/02_climate_velocity/output/2D/temp_bio5/aggregated_100m/spatgrad.tif")[[1]]
spatgrad_c.tmax.100 = rast("scripts/02_climate_velocity/output/2D/temp_bio5_canopy/aggregated_100m/spatgrad_q4.tif")[[1]]

tempgrad_u.tmax.100 = rast("scripts/02_climate_velocity/output/2D/temp_bio5/aggregated_100m/tempgrad.tif")[[1]]
tempgrad_c.tmax.100 = rast("scripts/02_climate_velocity/output/2D/temp_bio5_canopy/aggregated_100m/tempgrad_q4.tif")[[1]]

landuse.meso = extend(landuse.meso, vocc_u.tmax.100)
landuse.meso = crop(landuse.meso, vocc_u.tmax.100)
tmax_u.100 = c(vocc_u.tmax.100, spatgrad_u.tmax.100, tempgrad_u.tmax.100, landuse.meso)
names(tmax_u.100) = c("vocc", "spatgrad", "tempgrad", "landuse") 
tmax_u.100 = as.data.frame(tmax_u.100, xy = T) %>% 
  mutate(resolution = "100m",
         var = "Maximum temperature",
         height = "Land surface",
         vocc = ifelse(vocc == Inf, NA, vocc),
         vocc = abs(vocc)) %>% 
  filter(vocc < quantile(vocc, probs = 0.99, na.rm = T)) %>% 
  drop_na()

tmax_u.100 = left_join(tmax_u.100, landuse.class, by = c("landuse" = "Value"))
tmax_u.100 = tmax_u.100 %>% 
  filter(LU_level_1 == "Forest including forest/shrub land")

landuse.meso.c = crop(landuse.meso, vocc_c.tmax.100)
tmax_c.100 = c(vocc_c.tmax.100, spatgrad_c.tmax.100, tempgrad_c.tmax.100, landuse.meso.c)
names(tmax_c.100) = c("vocc", "spatgrad", "tempgrad", "landuse") 
tmax_c.100 = as.data.frame(tmax_c.100, xy = T) %>% 
  mutate(resolution = "100m",
         var = "Maximum temperature",
         height = "Canopy",
         vocc = ifelse(vocc == Inf, NA, vocc),
         vocc = abs(vocc)) %>% 
  filter(vocc < quantile(vocc, probs = 0.99, na.rm = T)) %>% 
  drop_na()

tmax_c.100 = left_join(tmax_c.100, landuse.class, by = c("landuse" = "Value"))
tmax_c.100 = tmax_c.100 %>% 
  filter(LU_level_1 == "Forest including forest/shrub land")

tmax.100 = bind_rows(tmax_c.100, tmax_u.100)


# tmin 100m ---------------------------------------------------------------
vocc_u.tmin.100 = rast("scripts/02_climate_velocity/output/2D/temp_bio6/aggregated_100m/vocc.tif")[[1]]
vocc_c.tmin.100 = rast("scripts/02_climate_velocity/output/2D/temp_bio6_canopy/aggregated_100m/vocc_q4.tif")[[1]]

spatgrad_u.tmin.100 = rast("scripts/02_climate_velocity/output/2D/temp_bio6/aggregated_100m/spatgrad.tif")[[1]]
spatgrad_c.tmin.100 = rast("scripts/02_climate_velocity/output/2D/temp_bio6_canopy/aggregated_100m/spatgrad_q4.tif")[[1]]

tempgrad_u.tmin.100 = rast("scripts/02_climate_velocity/output/2D/temp_bio6/aggregated_100m/tempgrad.tif")[[1]]
tempgrad_c.tmin.100 = rast("scripts/02_climate_velocity/output/2D/temp_bio6_canopy/aggregated_100m/tempgrad_q4.tif")[[1]]

tmin_u.100 = c(vocc_u.tmin.100, spatgrad_u.tmin.100, tempgrad_u.tmin.100, landuse.meso)
names(tmin_u.100) = c("vocc", "spatgrad", "tempgrad", "landuse") 
tmin_u.100 = as.data.frame(tmin_u.100, xy = T) %>% 
  mutate(resolution = "100m",
         var = "Minimum temperature",
         height = "Land surface",
         vocc = ifelse(vocc == Inf, NA, vocc),
         vocc = abs(vocc)) %>% 
  filter(vocc < quantile(vocc, probs = 0.99, na.rm = T)) %>% 
  drop_na()

tmin_u.100 = left_join(tmin_u.100, landuse.class, by = c("landuse" = "Value"))
tmin_u.100 = tmin_u.100 %>% 
  filter(LU_level_1 == "Forest including forest/shrub land")

tmin_c.100 = c(vocc_c.tmin.100, spatgrad_c.tmin.100, tempgrad_c.tmin.100, landuse.meso.c)
names(tmin_c.100) = c("vocc", "spatgrad", "tempgrad", "landuse") 
tmin_c.100 = as.data.frame(tmin_c.100, xy = T) %>% 
  mutate(resolution = "100m",
         var = "Minimum temperature",
         height = "Canopy",
         vocc = ifelse(vocc == Inf, NA, vocc),
         vocc = abs(vocc)) %>% 
  filter(vocc < quantile(vocc, probs = 0.99, na.rm = T)) %>% 
  drop_na()

tmin_c.100 = left_join(tmin_c.100, landuse.class, by = c("landuse" = "Value"))
tmin_c.100 = tmin_c.100 %>% 
  filter(LU_level_1 == "Forest including forest/shrub land")

tmin.100 = bind_rows(tmin_c.100, tmin_u.100)

t.100 = bind_rows(tmax.100, tmin.100)



# tmax 1km ----------------------------------------------------------------

vocc_u.tmax.1km = rast("scripts/02_climate_velocity/output/2D/temp_bio5/aggregated_1km/vocc.tif")[[1]]
vocc_c.tmax.1km = rast("scripts/02_climate_velocity/output/2D/temp_bio5_canopy/aggregated_1km/vocc_q4.tif")[[1]]

spatgrad_u.tmax.1km = rast("scripts/02_climate_velocity/output/2D/temp_bio5/aggregated_1km/spatgrad.tif")[[1]]
spatgrad_c.tmax.1km = rast("scripts/02_climate_velocity/output/2D/temp_bio5_canopy/aggregated_1km/spatgrad_q4.tif")[[1]]

tempgrad_u.tmax.1km = rast("scripts/02_climate_velocity/output/2D/temp_bio5/aggregated_1km/tempgrad.tif")[[1]]
tempgrad_c.tmax.1km = rast("scripts/02_climate_velocity/output/2D/temp_bio5_canopy/aggregated_1km/tempgrad_q4.tif")[[1]]

tmax_u.1km = c(vocc_u.tmax.1km, spatgrad_u.tmax.1km, tempgrad_u.tmax.1km, landuse.micro2d.1km)
names(tmax_u.1km) = c("vocc", "spatgrad", "tempgrad", "landuse") 
tmax_u.1km = as.data.frame(tmax_u.1km, xy = T) %>% 
  mutate(resolution = "1km",
         var = "Maximum temperature",
         height = "Land surface",
         vocc = ifelse(vocc == Inf, NA, vocc),
         vocc = abs(vocc)) %>% 
  filter(vocc < quantile(vocc, probs = 0.99, na.rm = T)) %>% 
  drop_na()

tmax_u.1km = left_join(tmax_u.1km, landuse.class, by = c("landuse" = "Value"))
tmax_u.1km = tmax_u.1km %>% 
  filter(LU_level_1 == "Forest including forest/shrub land")

landuse.micro2d.1km.c = resample(landuse.micro2d.1km, vocc_c.tmax.1km, method = "near")
tmax_c.1km = c(vocc_c.tmax.1km, spatgrad_c.tmax.1km, tempgrad_c.tmax.1km, landuse.micro2d.1km.c)
names(tmax_c.1km) = c("vocc", "spatgrad", "tempgrad", "landuse") 
tmax_c.1km = as.data.frame(tmax_c.1km, xy = T) %>% 
  mutate(resolution = "1km",
         var = "Maximum temperature",
         height = "Canopy",
         vocc = ifelse(vocc == Inf, NA, vocc),
         vocc = abs(vocc)) %>% 
  filter(vocc < quantile(vocc, probs = 0.99, na.rm = T)) %>% 
  drop_na()

tmax_c.1km = left_join(tmax_c.1km, landuse.class, by = c("landuse" = "Value"))
tmax_c.1km = tmax_c.1km %>% 
  filter(LU_level_1 == "Forest including forest/shrub land")

tmax.1km = bind_rows(tmax_c.1km, tmax_u.1km)




# tmin 1km ----------------------------------------------------------------

vocc_u.tmin.1km = rast("scripts/02_climate_velocity/output/2D/temp_bio6/aggregated_1km/vocc.tif")[[1]]
vocc_c.tmin.1km = rast("scripts/02_climate_velocity/output/2D/temp_bio6_canopy/aggregated_1km/vocc_q4.tif")[[1]]

spatgrad_u.tmin.1km = rast("scripts/02_climate_velocity/output/2D/temp_bio6/aggregated_1km/spatgrad.tif")[[1]]
spatgrad_c.tmin.1km = rast("scripts/02_climate_velocity/output/2D/temp_bio6_canopy/aggregated_1km/spatgrad_q4.tif")[[1]]

tempgrad_u.tmin.1km = rast("scripts/02_climate_velocity/output/2D/temp_bio6/aggregated_1km/tempgrad.tif")[[1]]
tempgrad_c.tmin.1km = rast("scripts/02_climate_velocity/output/2D/temp_bio6_canopy/aggregated_1km/tempgrad_q4.tif")[[1]]

tmin_u.1km = c(vocc_u.tmin.1km, spatgrad_u.tmin.1km, tempgrad_u.tmin.1km, landuse.micro2d.1km)
names(tmin_u.1km) = c("vocc", "spatgrad", "tempgrad", "landuse") 
tmin_u.1km = as.data.frame(tmin_u.1km, xy = T) %>% 
  mutate(resolution = "1km",
         var = "Minimum temperature",
         height = "Land surface",
         vocc = ifelse(vocc == Inf, NA, vocc),
         vocc = abs(vocc)) %>% 
  filter(vocc < quantile(vocc, probs = 0.99, na.rm = T)) %>% 
  drop_na()

tmin_u.1km = left_join(tmin_u.1km, landuse.class, by = c("landuse" = "Value"))
tmin_u.1km = tmin_u.1km %>% 
  filter(LU_level_1 == "Forest including forest/shrub land")


tmin_c.1km = c(vocc_c.tmin.1km, spatgrad_c.tmin.1km, tempgrad_c.tmin.1km, landuse.micro2d.1km.c)
names(tmin_c.1km) = c("vocc", "spatgrad", "tempgrad", "landuse") 
tmin_c.1km = as.data.frame(tmin_c.1km, xy = T) %>% 
  mutate(resolution = "1km",
         var = "Minimum temperature",
         height = "Canopy",
         vocc = ifelse(vocc == Inf, NA, vocc),
         vocc = abs(vocc)) %>% 
  filter(vocc < quantile(vocc, probs = 0.99, na.rm = T)) %>% 
  drop_na()

tmin_c.1km = left_join(tmin_c.1km, landuse.class, by = c("landuse" = "Value"))
tmin_c.1km = tmin_c.1km %>% 
  filter(LU_level_1 == "Forest including forest/shrub land")

tmin.1km = bind_rows(tmin_c.1km, tmin_u.1km)

t.1km = bind_rows(tmin.1km, tmax.1km)

t = bind_rows(t.1km, t.100, t.20)

p = t %>% 
  drop_na() %>% 
  filter(!is.infinite(vocc)) %>% 
  group_by(resolution, var, height) %>% 
  #slice_sample(n = 10000) %>% 
  mutate(resolution = factor(resolution, levels = c("1km", "100m", "20m")),
         var = case_when(var == "Maximum temperature" ~ "Maximum\ntemperature",
                         var == "Minimum temperature" ~ "Minimum\ntemperature")) %>% 
  ggplot(aes(x = var, y = abs(vocc), fill = height, color = height)) +
  geom_boxplot(outliers = F, linewidth = 0.5, alpha = 0.2) +
  #coord_cartesian(ylim = c(0,200)) +
  facet_wrap(vars(resolution)) +
  scale_y_continuous("Velocity (m/yr)") +
  scale_fill_manual(values = c("steelblue1", "royalblue2")) +
  scale_color_manual(values = c("steelblue1", "royalblue2")) +
  theme_classic() + 
  theme(panel.background = element_rect(color = "black", fill = NA),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom")

# png("scripts/03_analysis/00_plots/supplemental_figs/canopy_v_understory.png", width = 180, height = 80, res = 300, units = "mm")
# p
# dev.off()

t %>% 
  drop_na() %>% 
  filter(!is.infinite(vocc)) %>% 
  group_by(resolution, var, height) %>% 
  slice_sample(n = 10000) %>% 
  ggplot(aes(x = var, y = spatgrad, fill = height)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0,0.02)) +
  facet_wrap(vars(resolution))

t %>% 
  drop_na() %>% 
  filter(!is.infinite(vocc)) %>% 
  group_by(resolution, var, height) %>% 
  slice_sample(n = 10000) %>% 
  ggplot(aes(x = var, y = tempgrad, fill = height)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(-0.01,0.05)) +
  facet_wrap(vars(resolution))

tab = t %>% 
  drop_na() %>% 
  filter(!is.infinite(vocc)) %>% 
  group_by(resolution, var, height) %>% 
  mutate(vocc = abs(vocc)) %>% 
  summarise(vocc = median(vocc),
            tempgrad = median(tempgrad),
            spatgrad = median(spatgrad))

