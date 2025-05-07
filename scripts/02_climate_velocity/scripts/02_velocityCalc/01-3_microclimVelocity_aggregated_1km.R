#library(VoCC)
source('scripts/02_climate_velocity/scripts/00_functions/tempGrad2.R')
source('scripts/02_climate_velocity/scripts/00_functions/spatgrad_micro2D.R')
source('scripts/02_climate_velocity/scripts/00_functions/gVoCC_micro2D.R')
library(terra)
library(data.table)


# MEAN TMAX ---------------------------------------------------------------

# calculate macroclimate velocity for temp

tmax1960 = rast('data/microclim_3D/mosaics/mosaics_meanTmax/aggregated_1km/past/meanTmax_past_02.tif')
tmax2015 = rast('data/microclim_3D/mosaics/mosaics_meanTmax/aggregated_1km/pres/meanTmax_pres_02.tif')


# Calculate mesoclimate velocity using orginal method presented in vocc package
# mesoclimate layers produced using microclima without accounting for effects of vegetation
# (i.e., downscale using only topographic information)


# calculate macroclimate velocity for temp and dry season VPD using Chelsa data between 1960 and 2015

#clim = stack(meso1960, meso2015)

tempgrad = tempGrad2(tmax1960, tmax2015, 1960, 2015)

#meanClim = mean(meso1960, meso2015)
#spatgrad = VoCC::spatGrad(meanClim, projected = T)

spatgrad = spatgrad_2Dmicro(c(tmax1960, tmax2015), projected = T, slope_correct = F, hgt = 0)

#climvocc = VoCC::gVoCC(tempgrad, spatgrad)

climvocc = gVocc_micro2D(tempgrad, spatgrad)

plot(climvocc[[1]]$vocc, range = c(1,100))

fname = paste0('scripts/02_climate_velocity/output/2D/avg_daily_maxTemp/aggregated_1km')
writeRaster(tempgrad, filename = paste0(fname, '/tempgrad.tif'), overwrite = T)
writeRaster(spatgrad[[2]], filename = paste0(fname, '/spatgrad.tif'), overwrite = T)
writeRaster(climvocc[[1]], filename = paste0(fname, '/vocc.tif'), overwrite = T)


# TMIN BIO6 VOCC ----------------------------------------------------------

tmin1960 = rast('data/microclim_3D/mosaics/mosaics_temp_bio6/aggregated_1km/past/temp_bio6_past_02.tif')
tmin2015 = rast('data/microclim_3D/mosaics/mosaics_temp_bio6/aggregated_1km/pres/temp_bio6_pres_02.tif')


# Calculate mesoclimate velocity using orginal method presented in vocc package
# mesoclimate layers produced using microclima without accounting for effects of vegetation
# (i.e., downscale using only topographic information)


# calculate macroclimate velocity for temp and dry season VPD using Chelsa data between 1960 and 2015

#clim = stack(meso1960, meso2015)

tempgrad = tempGrad2(tmin1960, tmin2015, 1960, 2015)

#meanClim = mean(meso1960, meso2015)
#spatgrad = VoCC::spatGrad(meanClim, projected = T)

spatgrad = spatgrad_2Dmicro(c(tmin1960, tmin2015), projected = T, slope_correct = F, hgt = 0)

#climvocc = VoCC::gVoCC(tempgrad, spatgrad)

climvocc = gVocc_micro2D(tempgrad, spatgrad)

plot(climvocc[[1]]$vocc, range = c(1,100))

fname = paste0('scripts/02_climate_velocity/output/2D/temp_bio6/aggregated_1km')
writeRaster(tempgrad, filename = paste0(fname, '/tempgrad.tif'), overwrite = T)
writeRaster(spatgrad[[2]], filename = paste0(fname, '/spatgrad.tif'), overwrite = T)
writeRaster(climvocc[[1]], filename = paste0(fname, '/vocc.tif'), overwrite = T)


# TMAX (BIO5) -------------------------------------------------------------

tmax1960 = rast('data/microclim_3D/mosaics/mosaics_temp_bio5/aggregated_1km/past/temp_bio5_past_02.tif')
tmax2015 = rast('data/microclim_3D/mosaics/mosaics_temp_bio5/aggregated_1km/pres/temp_bio5_pres_02.tif')


# Calculate mesoclimate velocity using orginal method presented in vocc package
# mesoclimate layers produced using microclima without accounting for effects of vegetation
# (i.e., downscale using only topographic information)


# calculate macroclimate velocity for temp and dry season VPD using Chelsa data between 1960 and 2015

#clim = stack(meso1960, meso2015)

tempgrad = tempGrad2(tmax1960, tmax2015, 1960, 2015)

#meanClim = mean(meso1960, meso2015)
#spatgrad = VoCC::spatGrad(meanClim, projected = T)

spatgrad = spatgrad_2Dmicro(c(tmax1960, tmax2015), projected = T, slope_correct = F, hgt = 0)

#climvocc = VoCC::gVoCC(tempgrad, spatgrad)

climvocc = gVocc_micro2D(tempgrad, spatgrad)

plot(climvocc[[1]]$vocc, range = c(1,100))

fname = paste0('scripts/02_climate_velocity/output/2D/temp_bio5/aggregated_1km')
writeRaster(tempgrad, filename = paste0(fname, '/tempgrad.tif'), overwrite = T)
writeRaster(spatgrad[[2]], filename = paste0(fname, '/spatgrad.tif'), overwrite = T)
writeRaster(climvocc[[1]], filename = paste0(fname, '/vocc.tif'), overwrite = T)



# CALCULATE CANOPY VELOCITIES (Q4) ----------------------------------------


# * - MEAN TMAX -----------------------------------------------------------
# calculate macroclimate velocity for temp

tmax1960 = rast('data/microclim_3D/mosaics_relhgt/mosaics_relhgt_1km_aggregated/meanTmax/meanTmax_1km_past.tif')
tmax2015 = rast('data/microclim_3D/mosaics_relhgt/mosaics_relhgt_1km_aggregated/meanTmax/meanTmax_1km_pres.tif')

tmax1960 = tmax1960[[4]]
tmax2015 = tmax2015[[4]]

# Calculate mesoclimate velocity using orginal method presented in vocc package
# mesoclimate layers produced using microclima without accounting for effects of vegetation
# (i.e., downscale using only topographic information)


# calculate macroclimate velocity for temp and dry season VPD using Chelsa data between 1960 and 2015

#clim = stack(meso1960, meso2015)

tempgrad = tempGrad2(tmax1960, tmax2015, 1960, 2015)

#meanClim = mean(meso1960, meso2015)
#spatgrad = VoCC::spatGrad(meanClim, projected = T)

spatgrad = spatgrad_2Dmicro(c(tmax1960, tmax2015), projected = T, slope_correct = F, hgt = 0)

#climvocc = VoCC::gVoCC(tempgrad, spatgrad)

climvocc = gVocc_micro2D(tempgrad, spatgrad)

plot(climvocc[[1]]$vocc, range = c(1,100))

fname = paste0('scripts/02_climate_velocity/output/2D/avg_daily_maxTemp_canopy/aggregated_1km')
writeRaster(tempgrad, filename = paste0(fname, '/tempgrad_q4.tif'), overwrite = T)
writeRaster(spatgrad[[2]], filename = paste0(fname, '/spatgrad_q4.tif'), overwrite = T)
writeRaster(climvocc[[1]], filename = paste0(fname, '/vocc_q4.tif'), overwrite = T)


# *- TEMP BIO6 VOCC ----------------------------------------------------------

tmin1960 = rast("data/microclim_3D/mosaics_relhgt/mosaics_relhgt_1km_aggregated/temp_bio6/temp_bio6_1km_past.tif")
tmin2015 = rast("data/microclim_3D/mosaics_relhgt/mosaics_relhgt_1km_aggregated/temp_bio6/temp_bio6_1km_pres.tif")

tmin1960 = tmin1960[[4]]
tmin2015 = tmin2015[[4]]

# Calculate mesoclimate velocity using orginal method presented in vocc package
# mesoclimate layers produced using microclima without accounting for effects of vegetation
# (i.e., downscale using only topographic information)


# calculate macroclimate velocity for temp and dry season VPD using Chelsa data between 1960 and 2015

#clim = stack(meso1960, meso2015)

tempgrad = tempGrad2(tmin1960, tmin2015, 1960, 2015)

#meanClim = mean(meso1960, meso2015)
#spatgrad = VoCC::spatGrad(meanClim, projected = T)

spatgrad = spatgrad_2Dmicro(c(tmin1960, tmin2015), projected = T, slope_correct = F, hgt = 0)

#climvocc = VoCC::gVoCC(tempgrad, spatgrad)

climvocc = gVocc_micro2D(tempgrad, spatgrad)

plot(climvocc[[1]]$vocc, range = c(1,100))

fname = paste0('scripts/02_climate_velocity/output/2D/temp_bio6_canopy/aggregated_1km')
writeRaster(tempgrad, filename = paste0(fname, '/tempgrad_q4.tif'), overwrite = T)
writeRaster(spatgrad[[2]], filename = paste0(fname, '/spatgrad_q4.tif'), overwrite = T)
writeRaster(climvocc[[1]], filename = paste0(fname, '/vocc_q4.tif'), overwrite = T)


# TMAX (BIO5) CANOPY ------------------------------------------------------

tmax1960 = rast('data/microclim_3D/mosaics_relhgt/mosaics_relhgt_1km_aggregated/temp_bio5/temp_bio5_1km_past.tif')
tmax2015 = rast('data/microclim_3D/mosaics_relhgt/mosaics_relhgt_1km_aggregated/temp_bio5/temp_bio5_1km_pres.tif')

tmax1960 = tmax1960[[4]]
tmax2015 = tmax2015[[4]]

# Calculate mesoclimate velocity using orginal method presented in vocc package
# mesoclimate layers produced using microclima without accounting for effects of vegetation
# (i.e., downscale using only topographic information)


# calculate macroclimate velocity for temp and dry season VPD using Chelsa data between 1960 and 2015

#clim = stack(meso1960, meso2015)

tempgrad = tempGrad2(tmax1960, tmax2015, 1960, 2015)

#meanClim = mean(meso1960, meso2015)
#spatgrad = VoCC::spatGrad(meanClim, projected = T)

spatgrad = spatgrad_2Dmicro(c(tmax1960, tmax2015), projected = T, slope_correct = F, hgt = 0)

#climvocc = VoCC::gVoCC(tempgrad, spatgrad)

climvocc = gVocc_micro2D(tempgrad, spatgrad)

plot(climvocc[[1]]$vocc, range = c(1,1000))

fname = paste0('scripts/02_climate_velocity/output/2D/temp_bio5_canopy/aggregated_1km')
writeRaster(tempgrad, filename = paste0(fname, '/tempgrad_q4.tif'), overwrite = T)
writeRaster(spatgrad[[2]], filename = paste0(fname, '/spatgrad_q4.tif'), overwrite = T)
writeRaster(climvocc[[1]], filename = paste0(fname, '/vocc_q4.tif'), overwrite = T)


