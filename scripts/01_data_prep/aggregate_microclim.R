library(terra)

tmax = list.files("data/microclim_3D/mosaics/", pattern = ".tif", full.names = T)
nms = list.files("data/microclim_3D/mosaics/", pattern = ".tif")

# aggregate to 100 m resolution
for(i in 1:length(tmax)) {
  r = rast(tmax[i])
  r = r/100
  ragg = aggregate(r, 5, fun = "mean", na.rm = T)
  nm = paste0("data/microclim_3D/mosaics/aggregated_100m/", nms[i])
  writeRaster(ragg, nm, overwrite = T)
}

# aggregate to 1 km resolution
for(i in 1:length(tmax)) {
  r = rast(tmax[i])
  r = r/100
  ragg = aggregate(r, 50, fun = "mean", na.rm = T)
  nm = paste0("data/microclim_3D/mosaics/aggregated_1km/", nms[i])
  writeRaster(ragg, nm, overwrite = T)
}

# relative height for 100 m resolution
source('scripts/00_functions/relhgt_convert.R')

# canopy height
chm = rast('data/topography/chm_reproj.tif')
chm = aggregate(chm, 5, mean, na.rm = T)

hgts = c(2,seq(5,40,5))
brks = seq(0,1,0.25)

# calculate relative height metrics for aggregated data
# 100 m present
tmax = rast(list.files(path = 'data/microclim_3D/mosaics/aggregated_100m/', pattern = "Tmax_pres", full.names=T))
tmax = crop(tmax, chm)

tmax.relhgt = relhgt_convert2(chm, tmax, hgts, brks)

writeRaster(tmax.relhgt, 'data/microclim_3D/mosaics_relhgt_100m_aggregated/meanTmax/meanTmax_100m_pres.tif')

# 100 m past
tmax = rast(list.files(path = 'data/microclim_3D/mosaics/aggregated_100m/', pattern = "Tmax_past", full.names=T))
tmax = crop(tmax, chm)

tmax.relhgt = relhgt_convert2(chm, tmax, hgts, brks)

writeRaster(tmax.relhgt, 'data/microclim_3D/mosaics_relhgt_100m_aggregated/meanTmax/meanTmax_100m_past.tif')


# TMAX 1km ----------------------------------------------------------------
# canopy height
chm = rast('data/topography/chm_reproj.tif')
chm = aggregate(chm, 50, mean, na.rm = T)

hgts = c(2,seq(5,40,5))
brks = seq(0,1,0.25)

# 1km present
tmax = rast(list.files(path = 'data/microclim_3D/mosaics/aggregated_1km/', pattern = "Tmax_pres", full.names=T))
tmax = crop(tmax, chm)

tmax.relhgt = relhgt_convert2(chm, tmax, hgts, brks)

writeRaster(tmax.relhgt, 'data/microclim_3D/mosaics_relhgt_1km_aggregated/meanTmax/meanTmax_1km_pres.tif')

# 1km past
tmax = rast(list.files(path = 'data/microclim_3D/mosaics/aggregated_1km/', pattern = "Tmax_past", full.names=T))
tmax = crop(tmax, chm)

tmax.relhgt = relhgt_convert2(chm, tmax, hgts, brks)

writeRaster(tmax.relhgt, 'data/microclim_3D/mosaics_relhgt_1km_aggregated/meanTmax/meanTmax_1km_past.tif')


# TMIN --------------------------------------------------------------------

# past
tmin_past = list.files('data/microclim_3D/mosaics_temp_bio6/past/', pattern = ".tif", full.names = T)
nms = list.files("data/microclim_3D/mosaics_temp_bio6/past", pattern = ".tif")

# aggregate to 100 m resolution
for(i in 1:length(tmin_past)) {
  r = rast(tmin_past[i])
  r = r/100
  ragg = aggregate(r, 5, fun = "mean", na.rm = T)
  nm = paste0("data/microclim_3D/mosaics_temp_bio6/past/aggregated_100m/", nms[i])
  writeRaster(ragg, nm, overwrite = T)
}

# aggregate to 1 km resolution
for(i in 1:length(tmin_past)) {
  r = rast(tmin_past[i])
  r = r/100
  ragg = aggregate(r, 50, fun = "mean", na.rm = T)
  nm = paste0("data/microclim_3D/mosaics_temp_bio6/past/aggregated_1km/", nms[i])
  writeRaster(ragg, nm, overwrite = T)
}

# present
tmin_pres = list.files('data/microclim_3D/mosaics_temp_bio6/pres/', pattern = ".tif", full.names = T)
nms = list.files("data/microclim_3D/mosaics_temp_bio6/pres/", pattern = ".tif")

# aggregate to 100 m resolution
for(i in 1:length(tmin_pres)) {
  r = rast(tmin_pres[i])
  r = r/100
  ragg = aggregate(r, 5, fun = "mean", na.rm = T)
  nm = paste0("data/microclim_3D/mosaics_temp_bio6/pres/aggregated_100m/", nms[i])
  writeRaster(ragg, nm, overwrite = F)
}

# aggregate to 1 km resolution
for(i in 1:length(tmin_pres)) {
  r = rast(tmin_pres[i])
  r = r/100
  ragg = aggregate(r, 50, fun = "mean", na.rm = T)
  nm = paste0("data/microclim_3D/mosaics_temp_bio6/pres/aggregated_1km/", nms[i])
  writeRaster(ragg, nm, overwrite = F)
}

# relative height metrics for aggregated data
chm = rast('data/topography/chm_reproj.tif')
chm = aggregate(chm, 5, mean, na.rm = T)

tmin = rast(list.files(path = 'data/microclim_3D/mosaics_temp_bio6/pres/aggregated_100m/', pattern = ".tif", full.names=T))
tmin = crop(tmin, chm)

tmin.relhgt = relhgt_convert2(chm, tmin, hgts, brks)

writeRaster(tmin.relhgt, 'data/microclim_3D/mosaics_relhgt_100m_aggregated/temp_bio6/temp_bio6_100m_pres.tif')

# 100 m past
tmin = rast(list.files(path = 'data/microclim_3D/mosaics_temp_bio6/past/aggregated_100m/', pattern = ".tif", full.names=T))
tmin = crop(tmin, chm)

tmin.relhgt = relhgt_convert2(chm, tmin, hgts, brks)

writeRaster(tmin.relhgt, 'data/microclim_3D/mosaics_relhgt_100m_aggregated/temp_bio6/temp_bio6_100m_past.tif')


# TMIN 1km ----------------------------------------------------------------
# canopy height
chm = rast('data/topography/chm_reproj.tif')
chm = aggregate(chm, 50, mean, na.rm = T)

hgts = c(2,seq(5,40,5))
brks = seq(0,1,0.25)

# 1km present
tmin = rast(list.files(path = 'data/microclim_3D/mosaics_temp_bio6/pres/aggregated_1km/', pattern = ".tif", full.names=T))
tmin = crop(tmin, chm)

tmin.relhgt = relhgt_convert2(chm, tmin, hgts, brks)

writeRaster(tmin.relhgt, 'data/microclim_3D/mosaics_relhgt_1km_aggregated/temp_bio6/temp_bio6_1km_pres.tif')

# 1km past
tmin = rast(list.files(path = 'data/microclim_3D/mosaics_temp_bio6/past/aggregated_1km/', pattern = ".tif", full.names=T))
tmin = crop(tmin, chm)

tmin.relhgt = relhgt_convert2(chm, tmin, hgts, brks)

writeRaster(tmin.relhgt, 'data/microclim_3D/mosaics_relhgt_1km_aggregated/temp_bio6/temp_bio6_1km_past.tif')





