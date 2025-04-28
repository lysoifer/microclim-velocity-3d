library(tidyverse)
library(colorspace)
library(terra)
library(data.table)

meso = rast("scripts/02_climate_velocity/output/mesoclimate/avg_daily_max_temp/vocc.tif")
macro = rast("scripts/02_climate_velocity/output/macroclimate/mean_monthly_max_temp/vocc.tif")

dem.micro = rast('data/topography/dem_reproj.tif')
dem.micro = extend(dem.micro, 1)
dem.meso = resample(dem.micro, meso)
dem.macro = resample(dem.micro, macro)

aspect.micro = rast("data/topography/apr.tif")
aspect.micro = extend(aspect.micro, 1)
aspect.micro = aspect.micro * 180/pi
aspect.meso = terrain(dem.meso, v = "aspect", unit = "degrees")
aspect.macro = terrain(dem.macro, v = "aspect", unit = "degrees")

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
#maxtemp.pres.meso = rast('./../Trinidad_microclimates/output2/present/microclima_100m/tmax.tif')
maxtemp.pres.meso = rast('data/microclima_100m/present/tmax.tif')
#maxtemp.pres.macro = rast('./../Trinidad_microclimates/data/chelsa/tasmax2015/tasmax2015_meanMonthly.tif')
maxtemp.pres.macro = rast('data/chelsa/tasmax2015_meanMonthly.tif')
maxtemp.pres.macro = project(maxtemp.pres.macro, 'epsg:2067')

# combine into raster for each scale

# add micro3d (20m)
micro3d = fread("data/dataframes/micro3d_canopy_dataframe_tmax_20m.csv")
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
  dplyr::select(x, y, height, chm, relhgt, vocc, spatgrad, tempgrad, xyAng, EWdir, NSdir, zAng, elev, aspect, pai, landuse, max_temp_pres, LU_level_1, var, resolution) %>% 
  rename(maxtemp.pres = max_temp_pres) 
micro3d = micro3d %>% 
  mutate(maxtemp.pres = maxtemp.pres/100)

micro3d$var = "maxtemp"
micro3d$scale = "Within-canopy"
micro3d$resolution = "20m"

# ADD 3D 100M RESOULTION --------------------------------------------------
micro3d_100m = fread("data/dataframes/micro3d_canopy_dataframe_tmax_100m.csv")
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
  dplyr::select(x, y, height, chm, relhgt, vocc, spatgrad, tempgrad, xyAng, 
                EWdir, NSdir, zAng, elev, aspect, pai, landuse, max_temp_pres,
                LU_level_1, var, resolution) %>% 
  rename(maxtemp.pres = max_temp_pres) 

micro3d_100m$var = "maxtemp"
micro3d_100m$scale = "Within-canopy"
micro3d_100m$resolution = "100m"


# ADD 3D 1KM RESOLUTION ---------------------------------------------------

micro3d_1km = fread("data/dataframes/micro3d_canopy_dataframe_tmax_1km.csv")
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
  dplyr::select(x, y, height, chm, relhgt, vocc, spatgrad, tempgrad, xyAng, 
                EWdir, NSdir, zAng, elev, aspect, pai, landuse, max_temp_pres,
                LU_level_1, var, resolution) %>% 
  rename(maxtemp.pres = max_temp_pres) 

micro3d_1km$var = "maxtemp"
micro3d_1km$scale = "Within-canopy"
micro3d_1km$resolution = "1km"



df = bind_rows(micro3d, micro3d_100m, micro3d_1km) %>% 
  mutate(resolution = factor(resolution, levels = c("1km", "100m", "20m")))

fwrite(df, "data/dataframes/analysis_dataframe_3d_full_maxtemp.csv", row.names = F)



# MIN TEMP ----------------------------------------------------------------

meso = rast("scripts/02_climate_velocity/output/mesoclimate/temp_bio6/vocc.tif")
macro = rast("scripts/02_climate_velocity/output/macroclimate/temp_bio6/vocc.tif")

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
mintemp.pres.meso = rast('data/microclima_100m/present/tmin.tif')
mintemp.pres.macro = rast('data/chelsa/tasmin2015_bio6.tif')
mintemp.pres.macro = project(mintemp.pres.macro, 'epsg:2067')

# combine into raster for each scale

# add micro3d (20m)
micro3d = fread("data/dataframes/micro3d_canopy_dataframe_tmin_20m.csv")
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
  dplyr::select(x, y, height, chm, relhgt, vocc, spatgrad, tempgrad, xyAng, 
                EWdir, NSdir, zAng, elev, aspect, pai, landuse, min_temp_pres,
                LU_level_1, var, resolution) %>% 
  rename(mintemp.pres = min_temp_pres) 
micro3d = micro3d %>% 
  mutate(mintemp.pres = mintemp.pres/100)

micro3d$var = "mintemp"
micro3d$scale = "Within-canopy"
micro3d$resolution = "20m"


# ADD 3D 100M RESOULTION --------------------------------------------------
micro3d_100m = fread("data/dataframes/micro3d_canopy_dataframe_tmin_100m.csv")
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
  dplyr::select(x, y, height, chm, relhgt, vocc, spatgrad, tempgrad, xyAng, 
                EWdir, NSdir, zAng, elev, aspect, pai, landuse, min_temp_pres, 
                LU_level_1, var, resolution) %>% 
  rename(mintemp.pres = min_temp_pres) 

micro3d_100m$var = "mintemp"
micro3d_100m$scale = "Within-canopy"
micro3d_100m$resolution = "100m"


# ADD 3D 1KM RESOLUTION ---------------------------------------------------

micro3d_1km = fread("data/dataframes/micro3d_canopy_dataframe_tmin_1km.csv")
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
  dplyr::select(x, y, height, chm, relhgt, vocc, spatgrad, tempgrad, xyAng,
                EWdir, NSdir, zAng, elev, aspect, pai, landuse, min_temp_pres,
                LU_level_1, var, resolution) %>% 
  rename(mintemp.pres = min_temp_pres) 

micro3d_1km$var = "mintemp"
micro3d_1km$scale = "Within-canopy"
micro3d_1km$resolution = "1km"



df = bind_rows(micro3d, micro3d_100m, micro3d_1km) %>% 
  mutate(resolution = factor(resolution, levels = c("1km", "100m", "20m")))

fwrite(df, "data/dataframes/analysis_dataframe_3d_full_mintemp.csv", row.names = F)


