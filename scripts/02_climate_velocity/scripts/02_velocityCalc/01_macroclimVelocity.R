# Run in R v.4.1.2
library(VoCC)
source('scripts/02_climate_velocity/scripts/00_functions/tempGrad2.R')
source('scripts/02_climate_velocity/scripts/00_functions/spatgrad_micro2D.R')
source('scripts/02_climate_velocity/scripts/00_functions/gVoCC_micro2D.R')
library(terra)
library(data.table)


# calculate macroclimate velocity for temp and dry season VPD using Chelsa data between 1960 and 2015

# tmax1960 = rast('data/chelsa/chelsa_1960_estimates/tasmax1960_estimate.tif')
# tmax2015 = rast('data/chelsa/tasmax2015_meanMonthly.tif')

# max temp warmest month
tmax1960 = rast('data/chelsa/chelsa_1960_estimates/tasmax1960_bio5_estimate.tif')
tmax2015 = rast('data/chelsa/tasmax2015_bio5_maxMonthly.tif')


tmax1960 = project(tmax1960, 'epsg:2067')
tmax2015 = project(tmax2015, 'epsg:2067')

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

# fname = paste0('scripts/02_climate_velocity/output/macroclimate/mean_monthly_max_temp')
# writeRaster(tempgrad, filename = paste0(fname, '/tempgrad.tif'), overwrite = T)
# writeRaster(spatgrad[[2]], filename = paste0(fname, '/spatgrad.tif'), overwrite = T)
# writeRaster(climvocc[[1]], filename = paste0(fname, '/vocc.tif'), overwrite = T)

# bio5 = max of monthly max temps
fname = paste0('scripts/02_climate_velocity/output/macroclimate/temp_bio5')
writeRaster(tempgrad, filename = paste0(fname, '/tempgrad.tif'), overwrite = F)
writeRaster(spatgrad[[2]], filename = paste0(fname, '/spatgrad.tif'), overwrite = F)
writeRaster(climvocc[[1]], filename = paste0(fname, '/vocc.tif'), overwrite = F)

# TEMP BIO6 VOCC ----------------------------------------------------------

# calculate macroclimate velocity for temp and dry season VPD using Chelsa data between 1960 and 2015

tmin1960 = rast('data/chelsa/tasmin1960_bio6estimate.tif')
tmin2015 = rast('data/chelsa/tasmin2015_bio6.tif')


tmin1960 = project(tmin1960, 'epsg:2067')
tmin2015 = project(tmin2015, 'epsg:2067')

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

fname = paste0('scripts/02_climate_velocity/output/macroclimate/temp_bio6')
writeRaster(tempgrad, filename = paste0(fname, '/tempgrad.tif'), overwrite = T)
writeRaster(spatgrad[[2]], filename = paste0(fname, '/spatgrad.tif'), overwrite = T)
writeRaster(climvocc[[1]], filename = paste0(fname, '/vocc.tif'), overwrite = T)
