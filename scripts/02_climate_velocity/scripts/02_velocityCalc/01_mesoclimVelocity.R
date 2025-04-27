# run in R v 4.1.2
# Calculate mesoclimate velocity using orginal method presented in vocc package
# mesoclimate layers produced using microclima without accounting for effects of vegetation
# (i.e., downscale using only topographic information)

#library(VoCC)
#library(raster)
source('scripts/02_climate_velocity/scripts/00_functions/tempGrad2.R')
source('scripts/02_climate_velocity/scripts/00_functions/spatgrad_micro2D.R')
source('scripts/02_climate_velocity/scripts/00_functions/gVoCC_micro2D.R')
library(terra)
library(data.table)

# calculate mesoclimate velocity for temp and dry season VPD using Chelsa data between 1960 and 2015

meso1960 = rast('data/microclima_100m/past/tmax.tif')
meso2015 = rast('data/microclima_100m/present/tmax.tif')

#clim = stack(meso1960, meso2015)

tempgrad = tempGrad2(meso1960, meso2015, 1960, 2015)

#meanClim = mean(meso1960, meso2015)
#spatgrad = VoCC::spatGrad(meanClim, projected = T)

spatgrad = spatgrad_2Dmicro(c(meso1960, meso2015), projected = T, slope_correct = F, hgt = 0)

#climvocc = VoCC::gVoCC(tempgrad, spatgrad)

climvocc = gVocc_micro2D(tempgrad, spatgrad)

plot(climvocc[[1]]$vocc, range = c(0,1000))

fname = paste0('scripts/02_climate_velocity/output/mesoclimate/avg_daily_max_temp')
writeRaster(tempgrad, filename = paste0(fname, '/tempgrad.tif'), overwrite = T)
writeRaster(spatgrad[[2]], filename = paste0(fname, '/spatgrad.tif'), overwrite = T)
writeRaster(climvocc[[1]], filename = paste0(fname, '/vocc.tif'), overwrite = T)



# TMIN --------------------------------------------------------------------

meso1960 = rast('data/microclima_100m/past/tmin.tif')
meso2015 = rast('data/microclima_100m/present/tmin.tif')

#clim = stack(meso1960, meso2015)

tempgrad = tempGrad2(meso1960, meso2015, 1960, 2015)

#meanClim = mean(meso1960, meso2015)
#spatgrad = VoCC::spatGrad(meanClim, projected = T)

spatgrad = spatgrad_2Dmicro(c(meso1960, meso2015), projected = T, slope_correct = F, hgt = 0)

#climvocc = VoCC::gVoCC(tempgrad, spatgrad)

climvocc = gVocc_micro2D(tempgrad, spatgrad)

plot(climvocc[[1]]$vocc, range = c(0,1000))

fname = paste0('scripts/02_climate_velocity/output/mesoclimate/temp_bio6/')
writeRaster(tempgrad, filename = paste0(fname, '/tempgrad.tif'), overwrite = F)
writeRaster(spatgrad[[2]], filename = paste0(fname, '/spatgrad.tif'), overwrite = F)
writeRaster(climvocc[[1]], filename = paste0(fname, '/vocc.tif'), overwrite = F)

