# run in R v 4.1.2
# Calculate climate velocity using microclim layers aggregated to mesoclim scale
# accounts for impacts of vegetation at scale equal to mesoclim scale
# use 2m above the ground

#library(VoCC)
#library(raster)
source('scripts/02_climate_velocity/scripts/00_functions/tempGrad2.R')
source('scripts/02_climate_velocity/scripts/00_functions/spatgrad_micro2D.R')
source('scripts/02_climate_velocity/scripts/00_functions/gVoCC_micro2D.R')
library(terra)
library(data.table)

# calculate mesoclimate velocity for temp and dry season VPD using Chelsa data between 1960 and 2015

meso1960 = rast('data/microclim_3D/mosaics/aggregated_100m/meanTmax_past_02.tif')
meso2015 = rast('data/microclim_3D/mosaics/aggregated_100m/meanTmax_pres_02.tif')

#clim = stack(meso1960, meso2015)

tempgrad = tempGrad2(meso1960, meso2015, 1960, 2015)

#meanClim = mean(meso1960, meso2015)
#spatgrad = VoCC::spatGrad(meanClim, projected = T)

spatgrad = spatgrad_2Dmicro(c(meso1960, meso2015), projected = T, slope_correct = F, hgt = 0)

#climvocc = VoCC::gVoCC(tempgrad, spatgrad)

climvocc = gVocc_micro2D(tempgrad, spatgrad)

plot(climvocc[[1]]$vocc, range = c(0,100))

fname = paste0('scripts/02_climate_velocity/output/2D/avg_daily_maxTemp/aggregated_100m')
writeRaster(tempgrad, filename = paste0(fname, '/tempgrad.tif'), overwrite = T)
writeRaster(spatgrad[[2]], filename = paste0(fname, '/spatgrad.tif'), overwrite = T)
writeRaster(climvocc[[1]], filename = paste0(fname, '/vocc.tif'), overwrite = T)



# TMIN --------------------------------------------------------------------

meso1960 = rast('data/microclim_3D/mosaics_temp_bio6/past/aggregated_100m/temp_bio6_past_02.tif')
meso2015 = rast('data/microclim_3D/mosaics_temp_bio6/pres/aggregated_100m/temp_bio6_pres_02.tif')

#clim = stack(meso1960, meso2015)

tempgrad = tempGrad2(meso1960, meso2015, 1960, 2015)

#meanClim = mean(meso1960, meso2015)
#spatgrad = VoCC::spatGrad(meanClim, projected = T)

spatgrad = spatgrad_2Dmicro(c(meso1960, meso2015), projected = T, slope_correct = F, hgt = 0)

#climvocc = VoCC::gVoCC(tempgrad, spatgrad)

climvocc = gVocc_micro2D(tempgrad, spatgrad)

plot(climvocc[[1]]$vocc, range = c(0,100))

fname = paste0('scripts/02_climate_velocity/output/2D/temp_bio6/aggregated_100m/')
writeRaster(tempgrad, filename = paste0(fname, '/tempgrad.tif'), overwrite = T)
writeRaster(spatgrad[[2]], filename = paste0(fname, '/spatgrad.tif'), overwrite = T)
writeRaster(climvocc[[1]], filename = paste0(fname, '/vocc.tif'), overwrite = T)

# 2D canopy tmax ---------------------------------------------------------------
# calculate mesoclimate velocity for temp and dry season VPD using Chelsa data between 1960 and 2015

meso1960 = rast('data/microclim_3D/mosaics_relhgt_100m_aggregated/meanTmax/meanTmax_100m_past.tif')
meso2015 = rast('data/microclim_3D/mosaics_relhgt_100m_aggregated/meanTmax/meanTmax_100m_pres.tif')

# get top quarter of the canopy
meso1960 = meso1960[[4]]
meso2015 = meso2015[[4]]

#clim = stack(meso1960, meso2015)

tempgrad = tempGrad2(meso1960, meso2015, 1960, 2015)

#meanClim = mean(meso1960, meso2015)
#spatgrad = VoCC::spatGrad(meanClim, projected = T)

spatgrad = spatgrad_2Dmicro(c(meso1960, meso2015), projected = T, slope_correct = F, hgt = 0)

#climvocc = VoCC::gVoCC(tempgrad, spatgrad)

climvocc = gVocc_micro2D(tempgrad, spatgrad)

plot(climvocc[[1]]$vocc, range = c(0,100))

fname = paste0('scripts/02_climate_velocity/output/2D/avg_daily_maxTemp_canopy/aggregated_100m')
writeRaster(tempgrad, filename = paste0(fname, '/tempgrad_q4.tif'), overwrite = T)
writeRaster(spatgrad[[2]], filename = paste0(fname, '/spatgrad_q4.tif'), overwrite = T)
writeRaster(climvocc[[1]], filename = paste0(fname, '/vocc_q4.tif'), overwrite = T)


# CANOPY TMIN --------------------------------------------------------------------

meso1960 = rast('data/microclim_3D/mosaics_relhgt_100m_aggregated/temp_bio6/temp_bio6_100m_past.tif')
meso2015 = rast('data/microclim_3D/mosaics_relhgt_100m_aggregated/temp_bio6/temp_bio6_100m_pres.tif')

# get top quarter of the canopy
meso1960 = meso1960[[4]]
meso2015 = meso2015[[4]]

#clim = stack(meso1960, meso2015)

tempgrad = tempGrad2(meso1960, meso2015, 1960, 2015)

#meanClim = mean(meso1960, meso2015)
#spatgrad = VoCC::spatGrad(meanClim, projected = T)

spatgrad = spatgrad_2Dmicro(c(meso1960, meso2015), projected = T, slope_correct = F, hgt = 0)

#climvocc = VoCC::gVoCC(tempgrad, spatgrad)

climvocc = gVocc_micro2D(tempgrad, spatgrad)

plot(climvocc[[1]]$vocc, range = c(0,100))

fname = paste0('scripts/02_climate_velocity/output/2D/temp_bio6_canopy/aggregated_100m/')
writeRaster(tempgrad, filename = paste0(fname, '/tempgrad_q4.tif'), overwrite = T)
writeRaster(spatgrad[[2]], filename = paste0(fname, '/spatgrad_q4.tif'), overwrite = T)
writeRaster(climvocc[[1]], filename = paste0(fname, '/vocc_q4.tif'), overwrite = T)



