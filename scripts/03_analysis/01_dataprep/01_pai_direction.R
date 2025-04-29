# Calculate the direction in which a species would have to move to reach denser vegetation

library(terra)
library(data.table)
library(circular)
source('scripts/02_climate_velocity/scripts/00_functions/spatgrad_micro2D.R')
source('scripts/02_climate_velocity/scripts/00_functions/gVoCC_micro2D.R')
#library(VoCC)

## Micro pai direction----------------------------------------------------------##
# load pai raster
pai = rast('data/PAI/pai_NRange_rmMeters0to2_rmPAIgt10.tif')

dem = rast('data/topography/dem_reproj.tif')

# repeat pai raster as a second layer so that spatgrad function works
# (the function needs to take the mean of the two layers, which in regular velocity calculations is to average climate between time periods)
pai = c(pai, pai)

# calculate spatial gradient for pai
paidir = spatgrad_2Dmicro(r=pai, slope_correct = T, dem = dem, hgt = 2) # hgt argument doesn't matter here - just file naming purpose


# no temporal gradient for pai, so set to 0
pai_tempgrad = pai[[1]]
values(pai_tempgrad) = 0
pai_tempgrad = terra::mask(pai_tempgrad, pai[[1]])

# use climate velocity function to calculate xyAng for pai
pai_direction = gVocc_micro2D(pai_tempgrad, paidir)

# angle from focal in which PAI is densest (i.e. spatial gradient angle calculated using PAI)
writeRaster(pai_direction[[1]][['xyAng']], 'scripts/03_analysis/02_voccAngles/pai_direction_micro.tif')


## Meso pai direction----------------------------------------------------------##
# load meso climvocc as a template for resampling
meso = rast('scripts/02_climate_velocity/output/mesoclimate/avg_daily_max_temp/vocc.tif', lyrs = c('vocc'))

# load pai raster
pai.meso = resample(pai, meso)

# repeat pai raster as a second layer so that spatgrad function works
# (the function needs to take the mean of the two layers, which in regular velocity calculations is to average climate between time periods)
pai.meso = c(pai.meso, pai.meso)

# calculate spatial gradient for pai
pai.sp.meso = spatgrad_2Dmicro(r=pai.meso, slope_correct = F, hgt = 2) # hgt argument doesn't matter here - just file naming purpose

# no temporal gradient for pai, so set to 0
pai.tp.meso = pai.meso[[1]]
values(pai.tp.meso) = 0
pai.tp.meso = terra::mask(pai.tp.meso, pai.meso[[1]])

# use climate velocity function to calculate xyAng for pai
pai.dir.meso = gVocc_micro2D(pai.tp.meso, pai.sp.meso)

# angle from focal in which PAI is densest (i.e. spatial gradient angle calculated using PAI)
writeRaster(pai.dir.meso[[1]][['xyAng']], 'scripts/03_analysis/02_voccAngles/pai_direction_meso.tif')

## ----------------------------------------------------------------------------##

## Macro pai direction----------------------------------------------------------##
# load macro climvocc as a template for resampling
macro = rast('scripts/02_climate_velocity/output/macroclimate/mean_monthly_max_temp/vocc.tif', lyrs = c('vocc'))

# load pai raster and resample to macro resolution
pai.macro = resample(pai, macro)

# repeat pai raster as a second layer so that spatgrad function works
# (the function needs to take the mean of the two layers, which in regular velocity calculations is to average climate between time periods)
pai.macro = c(pai.macro, pai.macro)

# calculate spatial gradient for pai
pai.sp.macro = spatgrad_2Dmicro(r=pai.macro, slope_correct = F, hgt = 2) # hgt argument doesn't matter here - just file naming purpose

# no temporal gradient for pai, so set to 0
pai.tp.macro = pai.macro[[1]]
values(pai.tp.macro) = 0
pai.tp.macro = terra::mask(pai.tp.macro, pai.macro[[1]])

# use climate velocity function to calculate xyAng for pai
pai.dir.macro = gVocc_micro2D(pai.tp.macro, pai.sp.macro)

# angle from focal in which PAI is densest (i.e. spatial gradient angle calculated using PAI)
writeRaster(pai.dir.macro[[1]][['xyAng']], 'scripts/03_analysis/02_voccAngles/pai_direction_macro.tif')

## Macro pai direction resampled to micro aggregated ----------------------------------------------------------##
# load macro climvocc as a template for resampling
macro = rast('scripts/02_climate_velocity/output/2D/avg_daily_maxTemp/aggregated_1km/vocc.tif', lyrs = c('vocc'))

# load pai raster and resample to macro resolution
pai.macro = resample(pai, macro)

# repeat pai raster as a second layer so that spatgrad function works
# (the function needs to take the mean of the two layers, which in regular velocity calculations is to average climate between time periods)
pai.macro = c(pai.macro, pai.macro)

# calculate spatial gradient for pai
pai.sp.macro = spatgrad_2Dmicro(r=pai.macro, slope_correct = F, hgt = 2) # hgt argument doesn't matter here - just file naming purpose

# no temporal gradient for pai, so set to 0
pai.tp.macro = pai.macro[[1]]
values(pai.tp.macro) = 0
pai.tp.macro = terra::mask(pai.tp.macro, pai.macro[[1]])

# use climate velocity function to calculate xyAng for pai
pai.dir.macro = gVocc_micro2D(pai.tp.macro, pai.sp.macro)

# angle from focal in which PAI is densest (i.e. spatial gradient angle calculated using PAI)
# this is slightly different from the pai direction for CHELSA because chelsa is a slighlty less than 1 km resolution
writeRaster(pai.dir.macro[[1]][['xyAng']], 'scripts/03_analysis/02_voccAngles/pai_direction_macro_aggregated.tif')

# check out correlation between pai.macro and macro temp
macrotemp = rast("data/chelsa/tasmax2015_meanMonthly.tif")
macrotemp = project(macrotemp, macro)
macrotemp = resample(macrotemp, macro, method = "bilinear")


s = c(pai.macro, macrotemp)
names(s) = c("pai", "temp")
samp = spatSample(s, 200, na.rm = T)
plot(samp$pai, samp$temp)


