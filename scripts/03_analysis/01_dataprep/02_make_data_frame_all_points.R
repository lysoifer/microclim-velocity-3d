library(tidyverse)
library(colorspace)
library(terra)
library(data.table)


micro2d = rast("scripts/02_climate_velocity/output/2D/avg_daily_maxTemp/vocc_02m.tif")
micro2d.100m = rast("scripts/02_climate_velocity/output/2D/avg_daily_maxTemp/aggregated_100m/vocc.tif")
micro2d.1km = rast("scripts/02_climate_velocity/output/2D/avg_daily_maxTemp/aggregated_1km/vocc.tif")
meso = rast("scripts/02_climate_velocity/output/mesoclimate/avg_daily_max_temp/vocc.tif")
macro = rast("scripts/02_climate_velocity/output/macroclimate/mean_monthly_max_temp/vocc.tif")

tempgrad.micro2d = rast("scripts/02_climate_velocity/output/2D/avg_daily_maxTemp/temporalGradient_02m.tif")
tempgrad.micro2d.100m = rast("scripts/02_climate_velocity/output/2D/avg_daily_maxTemp/aggregated_100m/tempgrad.tif")
tempgrad.micro2d.1km = rast("scripts/02_climate_velocity/output/2D/avg_daily_maxTemp/aggregated_1km/tempgrad.tif")
tempgrad.meso = rast("scripts/02_climate_velocity/output/mesoclimate/avg_daily_max_temp/tempgrad.tif")
tempgrad.macro = rast("scripts/02_climate_velocity/output/macroclimate/mean_monthly_max_temp/tempgrad.tif")

dem.micro = rast('data/topography/dem_reproj.tif')
dem.micro = extend(dem.micro, 1)
dem.meso = resample(dem.micro, meso)
dem.macro = resample(dem.micro, macro)

pai.micro = rast('data/PAI/pai_NRange_rmMeters0to2_rmPAIgt10.tif')
pai.micro = extend(pai.micro, 1)
pai.meso = resample(pai.micro, meso)
pai.macro = resample(pai.micro, macro)

landuse.micro = rast('data/Helmer_2012_Beard_vegetation/201403_Trin_USDA_pWGS84_forest_tree_communities_for_Trinidad_from_Landsat.tif')
landuse.micro = project(landuse.micro, dem.micro, method = "near")
landuse.meso = terra::resample(landuse.micro, meso, method = "near")
landuse.macro = resample(landuse.micro, macro, method = "near")
landuse.class = read.csv("data/Helmer_2012_Beard_vegetation/Helmer_classification.csv")
landuse.class = landuse.class %>% dplyr::select(Value, LU_level_1)

#maxtemp.pres.micro = rast("01_microclimate_models_update/mosaics/meanTmax/meanTmax_pres_02.tif")
maxtemp.pres.micro = rast("data/microclim_3D/mosaics/meanTmax_pres_02.tif")
maxtemp.pres.micro = crop(maxtemp.pres.micro, dem.micro)
maxtemp.pres.micro.100m = rast("data/microclim_3D/mosaics/aggregated_100m/meanTmax_pres_02.tif")
maxtemp.pres.micro.1km = rast("data/microclim_3D/mosaics/aggregated_1km/meanTmax_pres_02.tif")
#maxtemp.pres.meso = rast('./../Trinidad_microclimates/output2/present/microclima_100m/tmax.tif')
maxtemp.pres.meso = rast('data/microclima_100m/present/tmax.tif')
#maxtemp.pres.macro = rast('./../Trinidad_microclimates/data/chelsa/tasmax2015/tasmax2015_meanMonthly.tif')
maxtemp.pres.macro = rast('data/chelsa/tasmax2015_meanMonthly.tif')
maxtemp.pres.macro = project(maxtemp.pres.macro, 'epsg:2067')

# combine into raster for each scale
micro2d = c(micro2d$vocc, micro2d$magnitude, micro2d$xyAng, micro2d$NSdir, micro2d$EWdir,
            tempgrad.micro2d, dem.micro, pai.micro, landuse.micro, maxtemp.pres.micro/100)
names(micro2d) = c("vocc", "spatgrad", "xyAng", "NSdir", "EWdir", "tempgrad", "elev", "pai", "landuse", "maxtemp.pres")
micro2d = micro2d %>% 
  as.data.frame(xy = T) %>% 
  mutate(vocc = ifelse(vocc == Inf, NA, vocc),
         vocc = abs(vocc)) %>% 
  filter(vocc < quantile(vocc, probs = 0.95, na.rm = T)) %>% 
  drop_na()

micro2d = left_join(micro2d, landuse.class, by = c("landuse" = "Value"))
micro2d = micro2d %>% 
  filter(LU_level_1 == "Forest including forest/shrub land")
micro2d$var = "maxtemp"
micro2d$scale = "Land-surface"
micro2d$resolution = "20m"

# micro2d.100m
micro2d.100m = crop(micro2d.100m, dem.meso)
micro2d.100m = extend(micro2d.100m, dem.meso)
tempgrad.micro2d.100m = crop(tempgrad.micro2d.100m, dem.meso)
tempgrad.micro2d.100m = extend(tempgrad.micro2d.100m, dem.meso)
maxtemp.pres.micro.100m = crop(maxtemp.pres.micro.100m, micro2d.100m)
maxtemp.pres.micro.100m = extend(maxtemp.pres.micro.100m, micro2d.100m)
micro2d.100m = c(micro2d.100m$vocc, micro2d.100m$magnitude, micro2d.100m$xyAng, micro2d.100m$NSdir, micro2d.100m$EWdir,
            tempgrad.micro2d.100m, dem.meso, pai.meso, landuse.meso, maxtemp.pres.micro.100m)
names(micro2d.100m) = c("vocc", "spatgrad", "xyAng", "NSdir", "EWdir", "tempgrad", "elev", "pai", "landuse", "maxtemp.pres")
micro2d.100m = micro2d.100m %>% 
  as.data.frame(xy = T) %>% 
  mutate(vocc = ifelse(vocc == Inf, NA, vocc),
         vocc = abs(vocc)) %>% 
  filter(vocc < quantile(vocc, probs = 0.95, na.rm = T)) %>% 
  drop_na()

micro2d.100m = left_join(micro2d.100m, landuse.class, by = c("landuse" = "Value"))
micro2d.100m = micro2d.100m %>% 
  filter(LU_level_1 == "Forest including forest/shrub land")
micro2d.100m$var = "maxtemp"
micro2d.100m$scale = "Land-surface"
micro2d.100m$resolution = "100m"

# micro2d.1km
micro2d.1km = crop(micro2d.1km, dem.macro)
micro2d.1km = extend(micro2d.1km, dem.macro)
tempgrad.micro2d.1km = crop(tempgrad.micro2d.1km, dem.macro)
tempgrad.micro2d.1km = extend(tempgrad.micro2d.1km, dem.macro)
maxtemp.pres.micro.1km = crop(maxtemp.pres.micro.1km, micro2d.1km)
maxtemp.pres.micro.1km = extend(maxtemp.pres.micro.1km, micro2d.1km)
dem.macro2 = resample(dem.macro, micro2d.1km)
pai.macro2 = resample(pai.macro, micro2d.1km)
landuse.macro2 = resample(landuse.macro, micro2d.1km, method = "near")
micro2d.1km = c(micro2d.1km$vocc, micro2d.1km$magnitude, micro2d.1km$xyAng, micro2d.1km$NSdir, micro2d.1km$EWdir,
                 tempgrad.micro2d.1km, dem.macro2, pai.macro2, landuse.macro2, maxtemp.pres.micro.1km)
names(micro2d.1km) = c("vocc", "spatgrad", "xyAng", "NSdir", "EWdir", "tempgrad", "elev", "pai", "landuse", "maxtemp.pres")
micro2d.1km = micro2d.1km %>% 
  as.data.frame(xy = T) %>% 
  mutate(vocc = ifelse(vocc == Inf, NA, vocc),
         vocc = abs(vocc)) %>% 
  filter(vocc < quantile(vocc, probs = 0.95, na.rm = T)) %>% 
  drop_na()

micro2d.1km = left_join(micro2d.1km, landuse.class, by = c("landuse" = "Value"))
micro2d.1km = micro2d.1km %>% 
  filter(LU_level_1 == "Forest including forest/shrub land")
micro2d.1km$var = "maxtemp"
micro2d.1km$scale = "Land-surface"
micro2d.1km$resolution = "1km"


meso = c(meso$vocc, meso$magnitude, meso$xyAng, meso$NSdir, meso$EWdir,
         tempgrad.meso, dem.meso, pai.meso, landuse.meso, maxtemp.pres.meso)
names(meso) = c("vocc", "spatgrad", "xyAng", "NSdir", "EWdir", 
                "tempgrad", "elev", "pai", "landuse", "maxtemp.pres")
meso = meso %>% 
  as.data.frame(xy = T) %>% 
  mutate(vocc = ifelse(vocc == Inf, NA, vocc),
         vocc = abs(vocc)) %>% 
  filter(vocc < quantile(vocc, probs = 0.95, na.rm = T)) %>% 
  drop_na() 

meso = left_join(meso, landuse.class, by = c("landuse" = "Value"))
meso = meso %>% 
  filter(LU_level_1 == "Forest including forest/shrub land")
meso$var = "maxtemp"
meso$scale = "Topo"
meso$resolution = "100m"

macro = c(macro$vocc, macro$magnitude, macro$xyAng, macro$NSdir, macro$EWdir,
          tempgrad.macro, dem.macro, pai.macro, landuse.macro, maxtemp.pres.macro)
names(macro) = c("vocc", "spatgrad", "xyAng", "NSdir", "EWdir", "tempgrad", "elev", "pai", "landuse", "maxtemp.pres")
macro = macro %>% 
  as.data.frame(xy = T) %>% 
  mutate(vocc = abs(vocc)) %>% 
  filter(vocc < quantile(vocc, probs = 0.95, na.rm = T)) %>% 
  drop_na() 

macro = left_join(macro, landuse.class, by = c("landuse" = "Value"))
macro = macro %>% 
  filter(LU_level_1 == "Forest including forest/shrub land")
macro$var = "maxtemp"
macro$scale = "Macro"
macro$resolution = "1km"

# add micro3d (20m)
micro3d = fread("scripts/03_analysis/00_dataframes/micro3d_canopy_dataframe_tmax_20m.csv")
landuse.micro.df = as.data.frame(landuse.micro, xy = T)
colnames(landuse.micro.df)[3] = "landuse"
landuse.micro.df = left_join(landuse.micro.df, landuse.class, by = c("landuse" = "Value"))
pai.micro.df = as.data.frame(pai.micro, xy = T)
names(pai.micro.df) = c("x", "y", "pai")
micro3d = micro3d %>% 
  left_join(landuse.micro.df, by = c("x", "y")) %>% 
  left_join(pai.micro.df, by = c("x", "y")) %>% 
  mutate(vocc = abs(vocc))
micro3d = micro3d[vocc < quantile(vocc, probs = 0.95, na.rm = T),]
micro3d = micro3d[!is.na(vocc), ]
micro3d = micro3d %>% filter(LU_level_1 == "Forest including forest/shrub land")

micro3d = micro3d %>% 
  dplyr::select(x, y, vocc, spatgrad, tempgrad, xyAng, EWdir, NSdir, elev, pai, landuse, max_temp_pres, LU_level_1, var, resolution) %>% 
  rename(maxtemp.pres = max_temp_pres) 
micro3d = micro3d %>% 
  mutate(maxtemp.pres = maxtemp.pres/100)

micro3d$var = "maxtemp"
micro3d$scale = "Within-canopy"
micro3d$resolution = "20m"
#micro2d$scale = "Land surface"
#meso$scale = "Topo"
#macro$scale = "Macro"

# ADD 3D 100M RESOULTION --------------------------------------------------
micro3d_100m = fread("scripts/03_analysis/00_dataframes/micro3d_canopy_dataframe_tmax_100m.csv")
landuse.meso.df = as.data.frame(landuse.meso, xy = T)
colnames(landuse.meso.df)[3] = "landuse"
landuse.meso.df = left_join(landuse.meso.df, landuse.class, by = c("landuse" = "Value"))
pai.meso.df = as.data.frame(pai.meso, xy = T)
names(pai.meso.df) = c("x", "y", "pai")
micro3d_100m = micro3d_100m %>% 
  left_join(landuse.meso.df, by = c("x", "y")) %>% 
  left_join(pai.meso.df, by = c("x", "y")) %>% 
  mutate(vocc = abs(vocc))
micro3d_100m = micro3d_100m[vocc < quantile(vocc, probs = 0.95, na.rm = T),]
micro3d_100m = micro3d_100m[!is.na(vocc), ]
micro3d_100m = micro3d_100m %>% filter(LU_level_1 == "Forest including forest/shrub land")

micro3d_100m = micro3d_100m %>% 
  dplyr::select(x, y, vocc, spatgrad, tempgrad, xyAng, EWdir, NSdir, elev, pai, landuse, max_temp_pres, LU_level_1, var, resolution) %>% 
  rename(maxtemp.pres = max_temp_pres) 

micro3d_100m$var = "maxtemp"
micro3d_100m$scale = "Within-canopy"
micro3d_100m$resolution = "100m"


# ADD 3D 1KM RESOLUTION ---------------------------------------------------

micro3d_1km = fread("scripts/03_analysis/00_dataframes/micro3d_canopy_dataframe_tmax_1km.csv")
micro3d_1km_rast = rast("scripts/02_climate_velocity/output/3D/avg_daily_maxTemp/aggregated_1km/vocc_05m.tif")
landuse.macro = resample(landuse.micro, micro3d_1km_rast, method = "near")
landuse.macro.df = as.data.frame(landuse.macro, xy = T)
colnames(landuse.macro.df)[3] = "landuse"
landuse.macro.df = left_join(landuse.macro.df, landuse.class, by = c("landuse" = "Value"))
pai.macro = resample(pai.micro, micro3d_1km_rast)
pai.macro.df = as.data.frame(pai.macro, xy = T)
names(pai.macro.df) = c("x", "y", "pai")
micro3d_1km = micro3d_1km %>% 
  left_join(landuse.macro.df, by = c("x", "y")) %>% 
  left_join(pai.macro.df, by = c("x", "y")) %>% 
  mutate(vocc = abs(vocc))
micro3d_1km = micro3d_1km[vocc < quantile(vocc, probs = 0.95, na.rm = T),]
micro3d_1km = micro3d_1km[!is.na(vocc), ]
micro3d_1km = micro3d_1km %>% filter(LU_level_1 == "Forest including forest/shrub land")

micro3d_1km = micro3d_1km %>% 
  dplyr::select(x, y, vocc, spatgrad, tempgrad, xyAng, EWdir, NSdir, elev, pai, landuse, max_temp_pres, LU_level_1, var, resolution) %>% 
  rename(maxtemp.pres = max_temp_pres) 

micro3d_1km$var = "maxtemp"
micro3d_1km$scale = "Within-canopy"
micro3d_1km$resolution = "1km"



df = bind_rows(macro, meso, micro2d, micro2d.100m, micro2d.1km, micro3d, micro3d_100m, micro3d_1km) %>% 
  mutate(scale = factor(scale, levels = c("Macro", "Topo", "Land-surface", "Within-canopy")),
         resolution = factor(resolution, levels = c("1km", "100m", "20m")))

fwrite(df, "data/dataframes/analysis_dataframe_full_maxtemp.csv", row.names = F)



# MIN TEMP ----------------------------------------------------------------

micro2d = rast("scripts/02_climate_velocity/output/2D/temp_bio6/vocc_02m.tif")
micro2d.100m = rast("scripts/02_climate_velocity/output/2D/temp_bio6/aggregated_100m/vocc.tif")
micro2d.1km = rast("scripts/02_climate_velocity/output/2D/temp_bio6/aggregated_1km/vocc.tif")
meso = rast("scripts/02_climate_velocity/output/mesoclimate/temp_bio6/vocc.tif")
macro = rast("scripts/02_climate_velocity/output/macroclimate/temp_bio6/vocc.tif")

tempgrad.micro2d = rast("scripts/02_climate_velocity/output/2D/temp_bio6/temporalGradient_02m.tif")
tempgrad.micro2d.100m = rast("scripts/02_climate_velocity/output/2D/temp_bio6/aggregated_100m/tempgrad.tif")
tempgrad.micro2d.1km = rast("scripts/02_climate_velocity/output/2D/temp_bio6/aggregated_1km/tempgrad.tif")
tempgrad.meso = rast("scripts/02_climate_velocity/output/mesoclimate/temp_bio6/tempgrad.tif")
tempgrad.macro = rast("scripts/02_climate_velocity/output/macroclimate/temp_bio6/tempgrad.tif")

dem.micro = rast('data/topography/dem_reproj.tif')
dem.micro = extend(dem.micro, 1)
dem.meso = resample(dem.micro, meso)
dem.macro = resample(dem.micro, macro)

pai.micro = rast('data/PAI/pai_NRange_rmMeters0to2_rmPAIgt10.tif')
pai.micro = extend(pai.micro, 1)
pai.meso = resample(pai.micro, meso)
pai.macro = resample(pai.micro, macro)

landuse.micro = rast('data/Helmer_2012_Beard_vegetation/201403_Trin_USDA_pWGS84_forest_tree_communities_for_Trinidad_from_Landsat.tif')
landuse.micro = project(landuse.micro, dem.micro, method = "near")
landuse.meso = terra::resample(landuse.micro, meso, method = "near")
landuse.macro = resample(landuse.micro, macro, method = "near")
landuse.class = read.csv("data/Helmer_2012_Beard_vegetation/Helmer_classification.csv")
landuse.class = landuse.class %>% dplyr::select(Value, LU_level_1)

mintemp.pres.micro = rast("data/microclim_3D/mosaics_temp_bio6/pres/temp_bio6_pres_02.tif")
mintemp.pres.micro = crop(mintemp.pres.micro, dem.micro)
mintemp.pres.micro.100m = rast("data/microclim_3D/mosaics_temp_bio6/pres/aggregated_100m/temp_bio6_pres_02.tif")
mintemp.pres.micro.1km = rast("data/microclim_3D/mosaics_temp_bio6/pres/aggregated_1km/temp_bio6_pres_02.tif")
mintemp.pres.meso = rast('data/microclima_100m/present/tmin.tif')
mintemp.pres.macro = rast('data/chelsa/tasmin2015_bio6.tif')
mintemp.pres.macro = project(mintemp.pres.macro, 'epsg:2067')

# combine into raster for each scale
micro2d = c(micro2d$vocc, micro2d$magnitude, micro2d$xyAng, micro2d$NSdir, micro2d$EWdir,
            tempgrad.micro2d, dem.micro, pai.micro, landuse.micro, mintemp.pres.micro/100)
names(micro2d) = c("vocc", "spatgrad", "xyAng", "NSdir", "EWdir", "tempgrad", "elev", "pai", "landuse", "mintemp.pres")
micro2d = micro2d %>% 
  as.data.frame(xy = T) %>% 
  mutate(vocc = ifelse(vocc == Inf, NA, vocc),
         vocc = abs(vocc)) %>% 
  filter(vocc < quantile(vocc, probs = 0.95, na.rm = T)) %>% 
  drop_na()

micro2d = left_join(micro2d, landuse.class, by = c("landuse" = "Value"))
micro2d = micro2d %>% 
  filter(LU_level_1 == "Forest including forest/shrub land")

micro2d$var = "mintemp"
micro2d$scale = "Land-surface"
micro2d$resolution = "20m"

# micro2d aggregated to 100m
micro2d.100m = crop(micro2d.100m, dem.meso)
micro2d.100m = extend(micro2d.100m, dem.meso)
tempgrad.micro2d.100m = crop(tempgrad.micro2d.100m, dem.meso)
tempgrad.micro2d.100m = extend(tempgrad.micro2d.100m, dem.meso)
mintemp.pres.micro.100m = crop(mintemp.pres.micro.100m, micro2d.100m)
mintemp.pres.micro.100m = extend(mintemp.pres.micro.100m, micro2d.100m)
micro2d.100m = c(micro2d.100m$vocc, micro2d.100m$magnitude, micro2d.100m$xyAng, micro2d.100m$NSdir, micro2d.100m$EWdir,
            tempgrad.micro2d.100m, dem.meso, pai.meso, landuse.meso, mintemp.pres.micro.100m)
names(micro2d.100m) = c("vocc", "spatgrad", "xyAng", "NSdir", "EWdir", "tempgrad", "elev", "pai", "landuse", "mintemp.pres")
micro2d.100m = micro2d.100m %>% 
  as.data.frame(xy = T) %>% 
  mutate(vocc = ifelse(vocc == Inf, NA, vocc),
         vocc = abs(vocc)) %>% 
  filter(vocc < quantile(vocc, probs = 0.95, na.rm = T)) %>% 
  drop_na()

micro2d.100m = left_join(micro2d.100m, landuse.class, by = c("landuse" = "Value"))
micro2d.100m = micro2d.100m %>% 
  filter(LU_level_1 == "Forest including forest/shrub land")

micro2d.100m$var = "mintemp"
micro2d.100m$scale = "Land-surface"
micro2d.100m$resolution = "100m"

# micro2d aggregated to 1km
micro2d.1km = crop(micro2d.1km, dem.macro)
micro2d.1km = extend(micro2d.1km, dem.macro)
tempgrad.micro2d.1km = crop(tempgrad.micro2d.1km, dem.macro)
tempgrad.micro2d.1km = extend(tempgrad.micro2d.1km, dem.macro)
mintemp.pres.micro.1km = crop(mintemp.pres.micro.1km, micro2d.1km)
mintemp.pres.micro.1km = extend(mintemp.pres.micro.1km, micro2d.1km)
dem.macro2 = resample(dem.macro, micro2d.1km)
pai.macro2 = resample(pai.macro, micro2d.1km)
landuse.macro2 = resample(landuse.macro, micro2d.1km, method = "near")
micro2d.1km = c(micro2d.1km$vocc, micro2d.1km$magnitude, micro2d.1km$xyAng, micro2d.1km$NSdir, micro2d.1km$EWdir,
                 tempgrad.micro2d.1km, dem.macro2, pai.macro2, landuse.macro2, mintemp.pres.micro.1km)
names(micro2d.1km) = c("vocc", "spatgrad", "xyAng", "NSdir", "EWdir", "tempgrad", "elev", "pai", "landuse", "mintemp.pres")
micro2d.1km = micro2d.1km %>% 
  as.data.frame(xy = T) %>% 
  mutate(vocc = ifelse(vocc == Inf, NA, vocc),
         vocc = abs(vocc)) %>% 
  filter(vocc < quantile(vocc, probs = 0.95, na.rm = T)) %>% 
  drop_na()

micro2d.1km = left_join(micro2d.1km, landuse.class, by = c("landuse" = "Value"))
micro2d.1km = micro2d.1km %>% 
  filter(LU_level_1 == "Forest including forest/shrub land")

micro2d.1km$var = "mintemp"
micro2d.1km$scale = "Land-surface"
micro2d.1km$resolution = "1km"


meso = c(meso$vocc, meso$magnitude, meso$xyAng, meso$NSdir, meso$EWdir,
         tempgrad.meso, dem.meso, pai.meso, landuse.meso, mintemp.pres.meso)
names(meso) = c("vocc", "spatgrad", "xyAng", "NSdir", "EWdir", 
                "tempgrad", "elev", "pai", "landuse", "mintemp.pres")
meso = meso %>% 
  as.data.frame(xy = T) %>% 
  mutate(vocc = ifelse(vocc == Inf, NA, vocc),
         vocc = abs(vocc)) %>% 
  filter(vocc < quantile(vocc, probs = 0.95, na.rm = T)) %>% 
  drop_na() 

meso = left_join(meso, landuse.class, by = c("landuse" = "Value"))
meso = meso %>% 
  filter(LU_level_1 == "Forest including forest/shrub land")

meso$var = "mintemp"
meso$scale = "Topo"
meso$resolution = "100m"

macro = c(macro$vocc, macro$magnitude, macro$xyAng, macro$NSdir, macro$EWdir,
          tempgrad.macro, dem.macro, pai.macro, landuse.macro, mintemp.pres.macro)
names(macro) = c("vocc", "spatgrad", "xyAng", "NSdir", "EWdir", "tempgrad", "elev", "pai", "landuse", "mintemp.pres")
macro = macro %>% 
  as.data.frame(xy = T) %>% 
  mutate(vocc = abs(vocc)) %>%
  filter(vocc < quantile(vocc, probs = 0.95, na.rm = T)) %>% 
  drop_na() 

macro = left_join(macro, landuse.class, by = c("landuse" = "Value"))
macro = macro %>% 
  filter(LU_level_1 == "Forest including forest/shrub land")

macro$var = "mintemp"
macro$scale = "Macro"
macro$resolution = "1km"

# add micro3d (20m)
micro3d = fread("scripts/03_analysis/00_dataframes/micro3d_canopy_dataframe_tmin_20m.csv")
landuse.micro.df = as.data.frame(landuse.micro, xy = T)
colnames(landuse.micro.df)[3] = "landuse"
landuse.micro.df = left_join(landuse.micro.df, landuse.class, by = c("landuse" = "Value"))
pai.micro.df = as.data.frame(pai.micro, xy = T)
names(pai.micro.df) = c("x", "y", "pai")
micro3d = micro3d %>% 
  left_join(landuse.micro.df, by = c("x", "y")) %>% 
  left_join(pai.micro.df, by = c("x", "y")) %>% 
  mutate(vocc = abs(vocc))
micro3d = micro3d[vocc < quantile(vocc, probs = 0.95, na.rm = T),]
micro3d = micro3d[!is.na(vocc), ]
micro3d = micro3d %>% filter(LU_level_1 == "Forest including forest/shrub land")

micro3d = micro3d %>% 
  dplyr::select(x, y, vocc, spatgrad, tempgrad, xyAng, EWdir, NSdir, elev, pai, landuse, min_temp_pres, LU_level_1, var, resolution) %>% 
  rename(mintemp.pres = min_temp_pres) 
micro3d = micro3d %>% 
  mutate(mintemp.pres = mintemp.pres/100)

micro3d$var = "mintemp"
micro3d$scale = "Within-canopy"
micro3d$resolution = "20m"
# micro2d$scale = "Land surface"
# meso$scale = "Topo"
# macro$scale = "Macro"


# ADD 3D 100M RESOULTION --------------------------------------------------
micro3d_100m = fread("scripts/03_analysis/00_dataframes/micro3d_canopy_dataframe_tmin_100m.csv")
landuse.meso.df = as.data.frame(landuse.meso, xy = T)
colnames(landuse.meso.df)[3] = "landuse"
landuse.meso.df = left_join(landuse.meso.df, landuse.class, by = c("landuse" = "Value"))
pai.meso.df = as.data.frame(pai.meso, xy = T)
names(pai.meso.df) = c("x", "y", "pai")
micro3d_100m = micro3d_100m %>% 
  left_join(landuse.meso.df, by = c("x", "y")) %>% 
  left_join(pai.meso.df, by = c("x", "y")) %>% 
  mutate(vocc = abs(vocc))
micro3d_100m = micro3d_100m[vocc < quantile(vocc, probs = 0.95, na.rm = T),]
micro3d_100m = micro3d_100m[!is.na(vocc), ]
micro3d_100m = micro3d_100m %>% filter(LU_level_1 == "Forest including forest/shrub land")

micro3d_100m = micro3d_100m %>% 
  dplyr::select(x, y, vocc, spatgrad, tempgrad, xyAng, EWdir, NSdir, elev, pai, landuse, min_temp_pres, LU_level_1, var, resolution) %>% 
  rename(mintemp.pres = min_temp_pres) 

micro3d_100m$var = "mintemp"
micro3d_100m$scale = "Within-canopy"
micro3d_100m$resolution = "100m"


# ADD 3D 1KM RESOLUTION ---------------------------------------------------

micro3d_1km = fread("scripts/03_analysis/00_dataframes/micro3d_canopy_dataframe_tmin_1km.csv")
micro3d_1km_rast = rast("scripts/02_climate_velocity/output/3D/temp_bio6/aggregated_1km/vocc_05m.tif")
landuse.macro = resample(landuse.micro, micro3d_1km_rast, method = "near")
landuse.macro.df = as.data.frame(landuse.macro, xy = T)
colnames(landuse.macro.df)[3] = "landuse"
landuse.macro.df = left_join(landuse.macro.df, landuse.class, by = c("landuse" = "Value"))
pai.macro = resample(pai.micro, micro3d_1km_rast)
pai.macro.df = as.data.frame(pai.macro, xy = T)
names(pai.macro.df) = c("x", "y", "pai")
micro3d_1km = micro3d_1km %>% 
  left_join(landuse.macro.df, by = c("x", "y")) %>% 
  left_join(pai.macro.df, by = c("x", "y")) %>% 
  mutate(vocc = abs(vocc))
micro3d_1km = micro3d_1km[vocc < quantile(vocc, probs = 0.95, na.rm = T),]
micro3d_1km = micro3d_1km[!is.na(vocc), ]
micro3d_1km = micro3d_1km %>% filter(LU_level_1 == "Forest including forest/shrub land")

micro3d_1km = micro3d_1km %>% 
  dplyr::select(x, y, vocc, spatgrad, tempgrad, xyAng, EWdir, NSdir, elev, pai, landuse, min_temp_pres, LU_level_1, var, resolution) %>% 
  rename(mintemp.pres = min_temp_pres) 

micro3d_1km$var = "mintemp"
micro3d_1km$scale = "Within-canopy"
micro3d_1km$resolution = "1km"



df = bind_rows(macro, meso, micro2d, micro2d.100m, micro2d.1km, micro3d, micro3d_100m, micro3d_1km) %>% 
  mutate(scale = factor(scale, levels = c("Macro", "Topo", "Land-surface", "Within-canopy")),
         resolution = factor(resolution, levels = c("1km", "100m", "20m")))

fwrite(df, "data/dataframes/analysis_dataframe_full_mintemp.csv", row.names = F)


