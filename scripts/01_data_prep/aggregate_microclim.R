# This code aggregates microclimate output to 100 m and 1 km resolutions
# for absolute and relative heights in the canopy
# meanTmax refers to average max temps of each month
# tmax bio5 = max temp of warmest month
# tmin bio6 = min temp of coldest month

library(terra)
library(numform)
source('scripts/02_climate_velocity/scripts/00_functions/relhgt_convert.R')


# meanTmax ----------------------------------------------------------------

tmax = list.files("data/microclim_3D/mosaics/mosaics_meanTmax/mean_Tmax_20m/past/", pattern = ".tif", full.names = T)
nms = list.files("data/microclim_3D/mosaics/mosaics_meanTmax/mean_Tmax_20m/past/", pattern = ".tif")

# aggregate to 100 m resolution
for(i in 1:length(tmax)) {
  r = rast(tmax[i])
  r = r/100
  ragg = aggregate(r, 5, fun = "mean", na.rm = T)
  nm = paste0("data/microclim_3D/mosaics/mosaics_meanTmax/aggregated_100m/past/", nms[i])
  writeRaster(ragg, nm, overwrite = T)
}

# 100 m present
tmax = list.files("data/microclim_3D/mosaics/mosaics_meanTmax/mean_Tmax_20m/pres/", pattern = ".tif", full.names = T)
nms = list.files("data/microclim_3D/mosaics/mosaics_meanTmax/mean_Tmax_20m/pres/", pattern = ".tif")

# aggregate to 100 m resolution
for(i in 1:length(tmax)) {
  r = rast(tmax[i])
  r = r/100
  ragg = aggregate(r, 5, fun = "mean", na.rm = T)
  nm = paste0("data/microclim_3D/mosaics/mosaics_meanTmax/aggregated_100m/pres/", nms[i])
  writeRaster(ragg, nm, overwrite = T)
}

# 1 km past
tmax = list.files("data/microclim_3D/mosaics/mosaics_meanTmax/mean_Tmax_20m/past/", pattern = ".tif", full.names = T)
nms = list.files("data/microclim_3D/mosaics/mosaics_meanTmax/mean_Tmax_20m/past/", pattern = ".tif")

# aggregate to 1 km resolution
for(i in 1:length(tmax)) {
  r = rast(tmax[i])
  r = r/100
  ragg = aggregate(r, 50, fun = "mean", na.rm = T)
  nm = paste0("data/microclim_3D/mosaics/mosaics_meanTmax/aggregated_1km/past/", nms[i])
  writeRaster(ragg, nm, overwrite = T)
}

# 1 km present
tmax = list.files("data/microclim_3D/mosaics/mosaics_meanTmax/mean_Tmax_20m/pres/", pattern = ".tif", full.names = T)
nms = list.files("data/microclim_3D/mosaics/mosaics_meanTmax/mean_Tmax_20m/pres/", pattern = ".tif")

# aggregate to 1 km resolution
for(i in 1:length(tmax)) {
  r = rast(tmax[i])
  r = r/100
  ragg = aggregate(r, 50, fun = "mean", na.rm = T)
  nm = paste0("data/microclim_3D/mosaics/mosaics_meanTmax/aggregated_1km/pres/", nms[i])
  writeRaster(ragg, nm, overwrite = T)
}

# relative height for 100 m resolution

# canopy height
chm = rast('data/topography/chm_reproj.tif')
chm = aggregate(chm, 5, mean, na.rm = T)

hgts = c(2,seq(5,40,5))
brks = seq(0,1,0.25)

# calculate relative height metrics for aggregated data
# 100 m present
tmax = rast(list.files(path = 'data/microclim_3D/mosaics/mosaics_meanTmax/aggregated_100m/pres/', pattern = ".tif", full.names=T))
tmax = crop(tmax, chm)

tmax.relhgt = relhgt_convert2(chm, tmax, hgts, brks)

writeRaster(tmax.relhgt, 
            'data/microclim_3D/mosaics_relhgt/mosaics_relhgt_100m_aggregated/meanTmax/meanTmax_100m_pres.tif',
            overwrite = T)

# 100 m past
tmax = rast(list.files(path = 'data/microclim_3D/mosaics/mosaics_meanTmax/aggregated_100m/past/', pattern = ".tif", full.names=T))
tmax = crop(tmax, chm)

tmax.relhgt = relhgt_convert2(chm, tmax, hgts, brks)

writeRaster(tmax.relhgt,
            'data/microclim_3D/mosaics_relhgt/mosaics_relhgt_100m_aggregated/meanTmax/meanTmax_100m_past.tif',
            overwrite = T)


# TMAX 1km ----------------------------------------------------------------
# canopy height
chm = rast('data/topography/chm_reproj.tif')
chm = aggregate(chm, 50, mean, na.rm = T)

hgts = c(2,seq(5,40,5))
brks = seq(0,1,0.25)

# 1km present
tmax = rast(list.files(path = 'data/microclim_3D/mosaics/mosaics_meanTmax/aggregated_1km/pres/', pattern = ".tif", full.names=T))
tmax = crop(tmax, chm)

tmax.relhgt = relhgt_convert2(chm, tmax, hgts, brks)

writeRaster(tmax.relhgt, 'data/microclim_3D/mosaics_relhgt/mosaics_relhgt_1km_aggregated/meanTmax/meanTmax_1km_pres.tif',
            overwrite = T)

# 1km past
tmax = rast(list.files(path = 'data/microclim_3D/mosaics/mosaics_meanTmax/aggregated_1km/past/', pattern = ".tif", full.names=T))
tmax = crop(tmax, chm)

tmax.relhgt = relhgt_convert2(chm, tmax, hgts, brks)

writeRaster(tmax.relhgt, 'data/microclim_3D/mosaics_relhgt/mosaics_relhgt_1km_aggregated/meanTmax/meanTmax_1km_past.tif',
            overwrite = T)


# TMIN --------------------------------------------------------------------

# past
tmin_past = list.files('data/microclim_3D/mosaics/mosaics_temp_bio6/temp_bio6_20m/past/', pattern = ".tif", full.names = T)
nms = list.files("data/microclim_3D/mosaics/mosaics_temp_bio6/temp_bio6_20m/past/", pattern = ".tif")

# aggregate to 100 m resolution
for(i in 1:length(tmin_past)) {
  r = rast(tmin_past[i])
  r = r/100
  ragg = aggregate(r, 5, fun = "mean", na.rm = T)
  nm = paste0("data/microclim_3D/mosaics/mosaics_temp_bio6/aggregated_100m/past/", nms[i])
  writeRaster(ragg, nm, overwrite = T)
}

# aggregate to 1 km resolution
for(i in 1:length(tmin_past)) {
  r = rast(tmin_past[i])
  r = r/100
  ragg = aggregate(r, 50, fun = "mean", na.rm = T)
  nm = paste0("data/microclim_3D/mosaics/mosaics_temp_bio6/aggregated_1km/past/", nms[i])
  writeRaster(ragg, nm, overwrite = T)
}

# present
tmin_pres = list.files('data/microclim_3D/mosaics/mosaics_temp_bio6/temp_bio6_20m/pres/', pattern = ".tif", full.names = T)
nms = list.files("data/microclim_3D/mosaics/mosaics_temp_bio6/temp_bio6_20m/pres/", pattern = ".tif")

# aggregate to 100 m resolution
for(i in 1:length(tmin_pres)) {
  r = rast(tmin_pres[i])
  r = r/100
  ragg = aggregate(r, 5, fun = "mean", na.rm = T)
  nm = paste0("data/microclim_3D/mosaics/mosaics_temp_bio6/aggregated_100m/pres/", nms[i])
  writeRaster(ragg, nm, overwrite = T)
}

# aggregate to 1 km resolution
for(i in 1:length(tmin_pres)) {
  r = rast(tmin_pres[i])
  r = r/100
  ragg = aggregate(r, 50, fun = "mean", na.rm = T)
  nm = paste0("data/microclim_3D/mosaics/mosaics_temp_bio6/aggregated_1km/pres/", nms[i])
  writeRaster(ragg, nm, overwrite = T)
}

# relative height metrics for aggregated data
chm = rast('data/topography/chm_reproj.tif')
chm = aggregate(chm, 5, mean, na.rm = T)

tmin = rast(list.files(path = 'data/microclim_3D/mosaics/mosaics_temp_bio6/aggregated_100m/pres/', pattern = ".tif", full.names=T))
tmin = crop(tmin, chm)

tmin.relhgt = relhgt_convert2(chm, tmin, hgts, brks)

writeRaster(tmin.relhgt, 
            'data/microclim_3D/mosaics_relhgt/mosaics_relhgt_100m_aggregated/temp_bio6/temp_bio6_100m_pres.tif',
            overwrite = T)

# 100 m past
tmin = rast(list.files(path = 'data/microclim_3D/mosaics/mosaics_temp_bio6/aggregated_100m/past/', pattern = ".tif", full.names=T))
tmin = crop(tmin, chm)

tmin.relhgt = relhgt_convert2(chm, tmin, hgts, brks)

writeRaster(tmin.relhgt, 
            'data/microclim_3D/mosaics_relhgt/mosaics_relhgt_100m_aggregated/temp_bio6/temp_bio6_100m_past.tif',
            overwrite = T)


# TMIN 1km ----------------------------------------------------------------
# canopy height
chm = rast('data/topography/chm_reproj.tif')
chm = aggregate(chm, 50, mean, na.rm = T)

hgts = c(2,seq(5,40,5))
brks = seq(0,1,0.25)

# 1km present
tmin = rast(list.files(path = 'data/microclim_3D/mosaics/mosaics_temp_bio6/aggregated_1km/pres/', pattern = ".tif", full.names=T))
tmin = crop(tmin, chm)

tmin.relhgt = relhgt_convert2(chm, tmin, hgts, brks)

writeRaster(tmin.relhgt, 
            'data/microclim_3D/mosaics_relhgt/mosaics_relhgt_1km_aggregated/temp_bio6/temp_bio6_1km_pres.tif',
            overwrite = T)

# 1km past
tmin = rast(list.files(path = 'data/microclim_3D/mosaics/mosaics_temp_bio6/aggregated_1km/past/', pattern = ".tif", full.names=T))
tmin = crop(tmin, chm)

tmin.relhgt = relhgt_convert2(chm, tmin, hgts, brks)

writeRaster(tmin.relhgt, 
            'data/microclim_3D/mosaics_relhgt/mosaics_relhgt_1km_aggregated/temp_bio6/temp_bio6_1km_past.tif',
            overwrite = T)

# TMAX BIO5 --------------------------------------------------------------------

# past
tmax_past = list.files('data/microclim_3D/mosaics/mosaics_temp_bio5/temp_bio5_20m/past/', pattern = ".tif", full.names = T)
nms = list.files("data/microclim_3D/mosaics/mosaics_temp_bio5/temp_bio5_20m/past/", pattern = ".tif")

# aggregate to 100 m resolution
for(i in 1:length(tmax_past)) {
  r = rast(tmax_past[i])
  r = r/100
  ragg = aggregate(r, 5, fun = "mean", na.rm = T)
  nm = paste0("data/microclim_3D/mosaics/mosaics_temp_bio5/aggregated_100m/past/", nms[i])
  writeRaster(ragg, nm, overwrite = T)
}

# aggregate to 1 km resolution
for(i in 1:length(tmax_past)) {
  r = rast(tmax_past[i])
  r = r/100
  ragg = aggregate(r, 50, fun = "mean", na.rm = T)
  nm = paste0("data/microclim_3D/mosaics/mosaics_temp_bio5/aggregated_1km/past/", nms[i])
  writeRaster(ragg, nm, overwrite = T)
}

# present
tmax_pres = list.files('data/microclim_3D/mosaics/mosaics_temp_bio5/temp_bio5_20m/pres/', pattern = ".tif", full.names = T)
nms = list.files("data/microclim_3D/mosaics/mosaics_temp_bio5/temp_bio5_20m/pres/", pattern = ".tif")

# aggregate to 100 m resolution
for(i in 1:length(tmax_pres)) {
  r = rast(tmax_pres[i])
  r = r/100
  ragg = aggregate(r, 5, fun = "mean", na.rm = T)
  nm = paste0("data/microclim_3D/mosaics/mosaics_temp_bio5/aggregated_100m/pres/", nms[i])
  writeRaster(ragg, nm, overwrite = T)
}

# aggregate to 1 km resolution
for(i in 1:length(tmax_pres)) {
  r = rast(tmax_pres[i])
  r = r/100
  ragg = aggregate(r, 50, fun = "mean", na.rm = T)
  nm = paste0("data/microclim_3D/mosaics/mosaics_temp_bio5/aggregated_1km/pres/", nms[i])
  writeRaster(ragg, nm, overwrite = T)
}

# relative height metrics for aggregated data
chm = rast('data/topography/chm_reproj.tif')
chm = aggregate(chm, 5, mean, na.rm = T)

tmax = rast(list.files(path = 'data/microclim_3D/mosaics/mosaics_temp_bio5/aggregated_100m/pres/', pattern = ".tif", full.names=T))
tmax = crop(tmax, chm)

tmax.relhgt = relhgt_convert2(chm, tmax, hgts, brks)

writeRaster(tmax.relhgt, 
            'data/microclim_3D/mosaics_relhgt/mosaics_relhgt_100m_aggregated/temp_bio5/temp_bio5_100m_pres.tif',
            overwrite = T)

# 100 m past
tmax = rast(list.files(path = 'data/microclim_3D/mosaics/mosaics_temp_bio5/aggregated_100m/past/', pattern = ".tif", full.names=T))
tmax = crop(tmax, chm)

tmax.relhgt = relhgt_convert2(chm, tmax, hgts, brks)

writeRaster(tmax.relhgt, 
            'data/microclim_3D/mosaics_relhgt/mosaics_relhgt_100m_aggregated/temp_bio5/temp_bio5_100m_past.tif',
            overwrite = T)


# TMAX (BIO5) 1km ----------------------------------------------------------------
# canopy height
chm = rast('data/topography/chm_reproj.tif')
chm = aggregate(chm, 50, mean, na.rm = T)

hgts = c(2,seq(5,40,5))
brks = seq(0,1,0.25)

# 1km present
tmax = rast(list.files(path = 'data/microclim_3D/mosaics/mosaics_temp_bio5/aggregated_1km/pres/', pattern = ".tif", full.names=T))
tmax = crop(tmax, chm)

tmax.relhgt = relhgt_convert2(chm, tmax, hgts, brks)

writeRaster(tmax.relhgt, 
            'data/microclim_3D/mosaics_relhgt/mosaics_relhgt_1km_aggregated/temp_bio5/temp_bio5_1km_pres.tif',
            overwrite = T)

# 1km past
tmax = rast(list.files(path = 'data/microclim_3D/mosaics/mosaics_temp_bio5/aggregated_1km/past/', pattern = ".tif", full.names=T))
tmax = crop(tmax, chm)

tmax.relhgt = relhgt_convert2(chm, tmax, hgts, brks)

writeRaster(tmax.relhgt, 
            'data/microclim_3D/mosaics_relhgt/mosaics_relhgt_1km_aggregated/temp_bio5/temp_bio5_1km_past.tif',
            overwrite = T)





