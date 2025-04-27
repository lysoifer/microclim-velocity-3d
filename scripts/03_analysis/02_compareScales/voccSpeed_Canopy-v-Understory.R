library(dplyr)
# compare 2D climate velocities in the canopy vs in the understory
# here, canopy = the top quarter of the canopy, which was calculated from temp in the top quarter of the canopy
# understory = 2m above the ground


# tmax 20m  ---------------------------------------------------------------

vocc_u.tmax.20 = rast("scripts/02_climate_velocity/output/2D/avg_daily_maxTemp/vocc_02m.tif")[[1]]
vocc_c.tmax.20 = rast("scripts/02_climate_velocity/output/2D/avg_daily_maxTemp_canopy/vocc_q4m.tif")[[1]]

spatgrad_u.tmax.20 = rast("scripts/02_climate_velocity/output/2D/avg_daily_maxTemp/spatialGradient_02m.tif")[[1]]
spatgrad_c.tmax.20 = rast("scripts/02_climate_velocity/output/2D/avg_daily_maxTemp_canopy/spatialGradient_q4m.tif")[[1]]

tempgrad_u.tmax.20 = rast("scripts/02_climate_velocity/output/2D/avg_daily_maxTemp/temporalGradient_02m.tif")[[1]]
tempgrad_c.tmax.20 = rast("scripts/02_climate_velocity/output/2D/avg_daily_maxTemp_canopy/temporalGradient_q4m.tif")[[1]]

tmax_u.20 = c(vocc_u.tmax.20, spatgrad_u.tmax.20, tempgrad_u.tmax.20)
names(tmax_u.20) = c("vocc", "spatgrad", "tempgrad") 
tmax_u.20 = as.data.frame(tmax_u.20, xy = T)

tmax_c.20 = c(vocc_c.tmax.20, spatgrad_c.tmax.20, tempgrad_c.tmax.20)
names(tmax_c.20) = c("vocc", "spatgrad", "tempgrad") 
tmax_c.20 = as.data.frame(tmax_c.20, xy = T)

tmax_u.20$resolution = "20m"
tmax_u.20$var = "Maximum temperature"
tmax_u.20$height = "Land surface"

tmax_c.20$resolution = "20m"
tmax_c.20$var = "Maximum temperature"
tmax_c.20$height = "Canopy"

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

spatgrad_u.tmin.20 = rast("scripts/02_climate_velocity/output/2D/temp_bio6/spatialGradient_02m.tif")[[1]]
spatgrad_c.tmin.20 = rast("scripts/02_climate_velocity/output/2D/temp_bio6_canopy/spatialGradient_q4m.tif")[[1]]

tempgrad_u.tmin.20 = rast("scripts/02_climate_velocity/output/2D/temp_bio6/temporalGradient_02m.tif")[[1]]
tempgrad_c.tmin.20 = rast("scripts/02_climate_velocity/output/2D/temp_bio6_canopy/temporalGradient_q4m.tif")[[1]]

tmin_u.20 = c(vocc_u.tmin.20, spatgrad_u.tmin.20, tempgrad_u.tmin.20)
names(tmin_u.20) = c("vocc", "spatgrad", "tempgrad") 
tmin_u.20 = as.data.frame(tmin_u.20, xy = T)

tmin_c.20 = c(vocc_c.tmin.20, spatgrad_c.tmin.20, tempgrad_c.tmin.20)
names(tmin_c.20) = c("vocc", "spatgrad", "tempgrad") 
tmin_c.20 = as.data.frame(tmin_c.20, xy = T)

tmin_u.20$resolution = "20m"
tmin_u.20$var = "Minimum temperature"
tmin_u.20$height = "Land surface"

tmin_c.20$resolution = "20m"
tmin_c.20$var = "Minimum temperature"
tmin_c.20$height = "Canopy"

tmin.20 = bind_rows(tmin_c.20, tmin_u.20)

t.20 = bind_rows(tmin.20, tmax.20)


# tmax 100m ---------------------------------------------------------------
vocc_u.tmax.100 = rast("scripts/02_climate_velocity/output/2D/avg_daily_maxTemp/aggregated_100m/vocc.tif")[[1]]
vocc_c.tmax.100 = rast("scripts/02_climate_velocity/output/2D/avg_daily_maxTemp_canopy/aggregated_100m/vocc_q4.tif")[[1]]

spatgrad_u.tmax.100 = rast("scripts/02_climate_velocity/output/2D/avg_daily_maxTemp/aggregated_100m/spatgrad.tif")[[1]]
spatgrad_c.tmax.100 = rast("scripts/02_climate_velocity/output/2D/avg_daily_maxTemp_canopy/aggregated_100m/spatgrad_q4.tif")[[1]]

tempgrad_u.tmax.100 = rast("scripts/02_climate_velocity/output/2D/avg_daily_maxTemp/aggregated_100m/tempgrad.tif")[[1]]
tempgrad_c.tmax.100 = rast("scripts/02_climate_velocity/output/2D/avg_daily_maxTemp_canopy/aggregated_100m/tempgrad_q4.tif")[[1]]

tmax_u.100 = c(vocc_u.tmax.100, spatgrad_u.tmax.100, tempgrad_u.tmax.100)
names(tmax_u.100) = c("vocc", "spatgrad", "tempgrad") 
tmax_u.100 = as.data.frame(tmax_u.100, xy = T)

tmax_c.100 = c(vocc_c.tmax.100, spatgrad_c.tmax.100, tempgrad_c.tmax.100)
names(tmax_c.100) = c("vocc", "spatgrad", "tempgrad") 
tmax_c.100 = as.data.frame(tmax_c.100, xy = T)

tmax_u.100$resolution = "100m"
tmax_u.100$var = "Maximum temperature"
tmax_u.100$height = "Land surface"

tmax_c.100$resolution = "100m"
tmax_c.100$var = "Maximum temperature"
tmax_c.100$height = "Canopy"

tmax.100 = bind_rows(tmax_c.100, tmax_u.100)


# tmin 100m ---------------------------------------------------------------
vocc_u.tmin.100 = rast("scripts/02_climate_velocity/output/2D/temp_bio6/aggregated_100m/vocc.tif")[[1]]
vocc_c.tmin.100 = rast("scripts/02_climate_velocity/output/2D/temp_bio6_canopy/aggregated_100m/vocc_q4.tif")[[1]]

spatgrad_u.tmin.100 = rast("scripts/02_climate_velocity/output/2D/temp_bio6/aggregated_100m/spatgrad.tif")[[1]]
spatgrad_c.tmin.100 = rast("scripts/02_climate_velocity/output/2D/temp_bio6_canopy/aggregated_100m/spatgrad_q4.tif")[[1]]

tempgrad_u.tmin.100 = rast("scripts/02_climate_velocity/output/2D/temp_bio6/aggregated_100m/tempgrad.tif")[[1]]
tempgrad_c.tmin.100 = rast("scripts/02_climate_velocity/output/2D/temp_bio6_canopy/aggregated_100m/tempgrad_q4.tif")[[1]]

tmin_u.100 = c(vocc_u.tmin.100, spatgrad_u.tmin.100, tempgrad_u.tmin.100)
names(tmin_u.100) = c("vocc", "spatgrad", "tempgrad") 
tmin_u.100 = as.data.frame(tmin_u.100, xy = T)

tmin_c.100 = c(vocc_c.tmin.100, spatgrad_c.tmin.100, tempgrad_c.tmin.100)
names(tmin_c.100) = c("vocc", "spatgrad", "tempgrad") 
tmin_c.100 = as.data.frame(tmin_c.100, xy = T)

tmin_u.100$resolution = "100m"
tmin_u.100$var = "Minimum temperature"
tmin_u.100$height = "Land surface"

tmin_c.100$resolution = "100m"
tmin_c.100$var = "Minimum temperature"
tmin_c.100$height = "Canopy"

tmin.100 = bind_rows(tmin_c.100, tmin_u.100)

t.100 = bind_rows(tmax.100, tmin.100)



# tmax 1km ----------------------------------------------------------------

vocc_u.tmax.1km = rast("scripts/02_climate_velocity/output/2D/avg_daily_maxTemp/aggregated_1km/vocc.tif")[[1]]
vocc_c.tmax.1km = rast("scripts/02_climate_velocity/output/2D/avg_daily_maxTemp_canopy/aggregated_1km/vocc_q4.tif")[[1]]

spatgrad_u.tmax.1km = rast("scripts/02_climate_velocity/output/2D/avg_daily_maxTemp/aggregated_1km/spatgrad.tif")[[1]]
spatgrad_c.tmax.1km = rast("scripts/02_climate_velocity/output/2D/avg_daily_maxTemp_canopy/aggregated_1km/spatgrad_q4.tif")[[1]]

tempgrad_u.tmax.1km = rast("scripts/02_climate_velocity/output/2D/avg_daily_maxTemp/aggregated_1km/tempgrad.tif")[[1]]
tempgrad_c.tmax.1km = rast("scripts/02_climate_velocity/output/2D/avg_daily_maxTemp_canopy/aggregated_1km/tempgrad_q4.tif")[[1]]

tmax_u.1km = c(vocc_u.tmax.1km, spatgrad_u.tmax.1km, tempgrad_u.tmax.1km)
names(tmax_u.1km) = c("vocc", "spatgrad", "tempgrad") 
tmax_u.1km = as.data.frame(tmax_u.1km, xy = T)

tmax_c.1km = c(vocc_c.tmax.1km, spatgrad_c.tmax.1km, tempgrad_c.tmax.1km)
names(tmax_c.1km) = c("vocc", "spatgrad", "tempgrad") 
tmax_c.1km = as.data.frame(tmax_c.1km, xy = T)

tmax_u.1km$resolution = "1km"
tmax_u.1km$var = "Maximum temperature"
tmax_u.1km$height = "Land surface"

tmax_c.1km$resolution = "1km"
tmax_c.1km$var = "Maximum temperature"
tmax_c.1km$height = "Canopy"

tmax.1km = bind_rows(tmax_c.1km, tmax_u.1km)




# tmin 1km ----------------------------------------------------------------

vocc_u.tmin.1km = rast("scripts/02_climate_velocity/output/2D/temp_bio6/aggregated_1km/vocc.tif")[[1]]
vocc_c.tmin.1km = rast("scripts/02_climate_velocity/output/2D/temp_bio6_canopy/aggregated_1km/vocc_q4.tif")[[1]]

spatgrad_u.tmin.1km = rast("scripts/02_climate_velocity/output/2D/temp_bio6/aggregated_1km/spatgrad.tif")[[1]]
spatgrad_c.tmin.1km = rast("scripts/02_climate_velocity/output/2D/temp_bio6_canopy/aggregated_1km/spatgrad_q4.tif")[[1]]

tempgrad_u.tmin.1km = rast("scripts/02_climate_velocity/output/2D/temp_bio6/aggregated_1km/tempgrad.tif")[[1]]
tempgrad_c.tmin.1km = rast("scripts/02_climate_velocity/output/2D/temp_bio6_canopy/aggregated_1km/tempgrad_q4.tif")[[1]]

tmin_u.1km = c(vocc_u.tmin.1km, spatgrad_u.tmin.1km, tempgrad_u.tmin.1km)
names(tmin_u.1km) = c("vocc", "spatgrad", "tempgrad") 
tmin_u.1km = as.data.frame(tmin_u.1km, xy = T)

tmin_c.1km = c(vocc_c.tmin.1km, spatgrad_c.tmin.1km, tempgrad_c.tmin.1km)
names(tmin_c.1km) = c("vocc", "spatgrad", "tempgrad") 
tmin_c.1km = as.data.frame(tmin_c.1km, xy = T)

tmin_u.1km$resolution = "1km"
tmin_u.1km$var = "Minimum temperature"
tmin_u.1km$height = "Land surface"

tmin_c.1km$resolution = "1km"
tmin_c.1km$var = "Minimum temperature"
tmin_c.1km$height = "Canopy"

tmin.1km = bind_rows(tmin_c.1km, tmin_u.1km)

t.1km = bind_rows(tmin.1km, tmax.1km)

t = bind_rows(t.1km, t.100, t.20)

p = t %>% 
  drop_na() %>% 
  filter(!is.infinite(vocc)) %>% 
  group_by(resolution, var, height) %>% 
  slice_sample(n = 10000) %>% 
  mutate(resolution = factor(resolution, levels = c("1km", "100m", "20m")),
         var = case_when(var == "Maximum temperature" ~ "Maximum\ntemperature",
                         var == "Minimum temperature" ~ "Minimum\ntemperature")) %>% 
  ggplot(aes(x = var, y = abs(vocc), fill = height)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0,200)) +
  facet_wrap(vars(resolution)) +
  scale_y_continuous("Velocity (m/yr)") +
  theme_classic() + 
  theme(panel.background = element_rect(color = "black", fill = NA),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom")

png("scripts/03_analysis/00_plots/supplemental_figs/canopy_v_understory.png", width = 180, height = 80, res = 300, units = "mm")
p
dev.off()

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

