# This code calculates 3D microclimate velocity for mean daily maximum temperatures.
# Run in Rv4.1.2

library(data.table)
library(terra)
library(numform)
library(parallel)
library(doParallel)

source('scripts/02_climate_velocity/scripts/00_functions/spatgrad.R')
source('scripts/02_climate_velocity/scripts/00_functions/tempGrad2.R')


# TMAX AGGREGATED 100M ----------------------------------------------------

past = list.files(path = 'data/microclim_3D/mosaics/aggregated_100m/', pattern = '.*past_.*.tif', full.names = T)
pres = list.files(pat = 'data/microclim_3D/mosaics/aggregated_100m/', pattern = '.*pres_.*.tif', full.names = T)
heights = c(2,5,10,15,20,25,30,35,40)

cl = makeCluster(4)
registerDoParallel(cl)
foreach (i=c(1:length(past)), .packages=c('terra', 'data.table', 'numform')) %dopar% {
  if (i <= length(past)-2) {
    past3 = rast(past[i])$max
    past2 = rast(past[i+1])$max
    past1 = rast(past[i+2])$max
    climpast = c(past1, past2, past3) #stack from highest to lowest in the canopy
    
    
    pres3 = rast(pres[i])$max
    pres2 = rast(pres[i+1])$max
    pres1 = rast(pres[i+2])$max
    climpres = c(pres1, pres2, pres3) #stack from highest to lowest in the canopy
    
    
    hgts = c(heights[i+2], heights[i+1], heights[i])
    
    dem = rast('data/topography/dem_reproj.tif')
    dem = extend(dem, c(1,1))
    dem = resample(dem, climpres)
    
    climpast = crop(climpast, dem)
    climpres = crop(climpres, dem)
    
    sp = spatgrad2(climpast, climpres, hgts, slope_correct = T, dem = dem)
    tg = tempGrad2(climpast[[2]], climpres[[2]], 1960, 2015)
    vocc = gVocc(tg,sp)
    
    mid = f_pad_zero(hgts[2], 2)
    
    fname_sp = paste0('scripts/02_climate_velocity/output/3D/avg_daily_maxTemp/aggregated_100m/spatialGradient_', mid, 'm.tif')
    fname_tg = paste0('scripts/02_climate_velocity/output/3D/avg_daily_maxTemp/aggregated_100m/temporalGradient_', mid, 'm.tif')
    fname_rast = paste0('scripts/02_climate_velocity/output/3D/avg_daily_maxTemp/aggregated_100m/vocc_', mid, 'm.tif')
    fname_csv = paste0('scripts/02_climate_velocity/output/3D/avg_daily_maxTemp/aggregated_100m/vocc_', mid, 'm.csv')
    
    writeRaster(sp[[2]], fname_sp, overwrite = T)
    writeRaster(tg, fname_tg, overwrite = T)
    writeRaster(vocc[[1]], fname_rast, overwrite = T)
    write.csv(vocc[[2]], file = fname_csv)
  }
  
}
stopCluster(cl)



# TMAX AGGREGATED 1KM -----------------------------------------------------

past = list.files(path = 'data/microclim_3D/mosaics/aggregated_1km/', pattern = '.*past_.*.tif', full.names = T)
pres = list.files(pat = 'data/microclim_3D/mosaics/aggregated_1km/', pattern = '.*pres_.*.tif', full.names = T)
heights = c(2,5,10,15,20,25,30,35,40)

cl = makeCluster(4)
registerDoParallel(cl)
foreach (i=c(1:length(past)), .packages=c('terra', 'data.table', 'numform')) %do% {
  if (i <= length(past)-2) {
    past3 = rast(past[i])$max
    past2 = rast(past[i+1])$max
    past1 = rast(past[i+2])$max
    climpast = c(past1, past2, past3) #stack from highest to lowest in the canopy

    
    pres3 = rast(pres[i])$max
    pres2 = rast(pres[i+1])$max
    pres1 = rast(pres[i+2])$max
    climpres = c(pres1, pres2, pres3) #stack from highest to lowest in the canopy

    
    hgts = c(heights[i+2], heights[i+1], heights[i])
    
    dem = rast('data/topography/dem_reproj.tif')
    dem = extend(dem, c(1,1))
    dem = resample(dem, climpres)
    
    climpast = crop(climpast, dem)
    climpres = crop(climpres, dem)
    
    sp = spatgrad2(climpast, climpres, hgts, slope_correct = T, dem = dem)
    tg = tempGrad2(climpast[[2]], climpres[[2]], 1960, 2015)
    vocc = gVocc(tg,sp)
    
    mid = f_pad_zero(hgts[2], 2)
    
    fname_sp = paste0('scripts/02_climate_velocity/output/3D/avg_daily_maxTemp/aggregated_1km/spatialGradient_', mid, 'm.tif')
    fname_tg = paste0('scripts/02_climate_velocity/output/3D/avg_daily_maxTemp/aggregated_1km/temporalGradient_', mid, 'm.tif')
    fname_rast = paste0('scripts/02_climate_velocity/output/3D/avg_daily_maxTemp/aggregated_1km/vocc_', mid, 'm.tif')
    fname_csv = paste0('scripts/02_climate_velocity/output/3D/avg_daily_maxTemp/aggregated_1km/vocc_', mid, 'm.csv')
    
    writeRaster(sp[[2]], fname_sp, overwrite = T)
    writeRaster(tg, fname_tg, overwrite = T)
    writeRaster(vocc[[1]], fname_rast, overwrite = T)
    write.csv(vocc[[2]], file = fname_csv)
  }
  
}
stopCluster(cl)


# TMIN AGGREGATED 100 M ---------------------------------------------------

past = list.files(path = 'data/microclim_3D/mosaics_temp_bio6/past/aggregated_100m/', pattern = '.*past_.*.tif', full.names = T)
pres = list.files(pat = 'data/microclim_3D/mosaics_temp_bio6/pres/aggregated_100m/', pattern = '.*pres_.*.tif', full.names = T)
heights = c(2,5,10,15,20,25,30,35,40)

cl = makeCluster(4)
registerDoParallel(cl)
foreach (i=c(1:length(past)), .packages=c('terra', 'data.table', 'numform')) %dopar% {
  if (i <= length(past)-2) {
    past3 = rast(past[i])[[1]]
    past2 = rast(past[i+1])[[1]]
    past1 = rast(past[i+2])[[1]]
    climpast = c(past1, past2, past3) #stack from highest to lowest in the canopy

    
    pres3 = rast(pres[i])[[1]]
    pres2 = rast(pres[i+1])[[1]]
    pres1 = rast(pres[i+2])[[1]]
    climpres = c(pres1, pres2, pres3) #stack from highest to lowest in the canopy

    
    hgts = c(heights[i+2], heights[i+1], heights[i])
    
    dem = rast('data/topography/dem_reproj.tif')
    dem = extend(dem, c(1,1))
    dem = resample(dem, climpres)
    
    climpast = crop(climpast, dem)
    climpres = crop(climpres, dem)
    
    sp = spatgrad2(climpast, climpres, hgts, slope_correct = T, dem = dem)
    tg = tempGrad2(climpast[[2]], climpres[[2]], 1960, 2015)
    vocc = gVocc(tg,sp)
    
    mid = f_pad_zero(hgts[2], 2)
    
    fname_sp = paste0('scripts/02_climate_velocity/output/3D/temp_bio6/aggregated_100m/spatialGradient_', mid, 'm.tif')
    fname_tg = paste0('scripts/02_climate_velocity/output/3D/temp_bio6/aggregated_100m/temporalGradient_', mid, 'm.tif')
    fname_rast = paste0('scripts/02_climate_velocity/output/3D/temp_bio6/aggregated_100m/vocc_', mid, 'm.tif')
    fname_csv = paste0('scripts/02_climate_velocity/output/3D/temp_bio6/aggregated_100m/vocc_', mid, 'm.csv')
    
    writeRaster(sp[[2]], fname_sp, overwrite = T)
    writeRaster(tg, fname_tg, overwrite = T)
    writeRaster(vocc[[1]], fname_rast, overwrite = T)
    write.csv(vocc[[2]], file = fname_csv)
  }
  
}
stopCluster(cl)



# TMIN AGGREGATED 1KM -----------------------------------------------------

past = list.files(path = 'data/microclim_3D/mosaics_temp_bio6/past/aggregated_1km/', pattern = '.*past_.*.tif', full.names = T)
pres = list.files(pat = 'data/microclim_3D/mosaics_temp_bio6/pres/aggregated_1km/', pattern = '.*pres_.*.tif', full.names = T)
heights = c(2,5,10,15,20,25,30,35,40)

cl = makeCluster(4)
registerDoParallel(cl)
foreach (i=c(1:length(past)), .packages=c('terra', 'data.table', 'numform')) %dopar% {
  if (i <= length(past)-2) {
    past3 = rast(past[i])[[1]]
    past2 = rast(past[i+1])[[1]]
    past1 = rast(past[i+2])[[1]]
    climpast = c(past1, past2, past3) #stack from highest to lowest in the canopy
    
    
    pres3 = rast(pres[i])[[1]]
    pres2 = rast(pres[i+1])[[1]]
    pres1 = rast(pres[i+2])[[1]]
    climpres = c(pres1, pres2, pres3) #stack from highest to lowest in the canopy
    
    
    hgts = c(heights[i+2], heights[i+1], heights[i])
    
    dem = rast('data/topography/dem_reproj.tif')
    dem = extend(dem, c(1,1))
    dem = resample(dem, climpres)
    
    climpast = crop(climpast, dem)
    climpres = crop(climpres, dem)
    
    sp = spatgrad2(climpast, climpres, hgts, slope_correct = T, dem = dem)
    tg = tempGrad2(climpast[[2]], climpres[[2]], 1960, 2015)
    vocc = gVocc(tg,sp)
    
    mid = f_pad_zero(hgts[2], 2)
    
    fname_sp = paste0('scripts/02_climate_velocity/output/3D/temp_bio6/aggregated_1km/spatialGradient_', mid, 'm.tif')
    fname_tg = paste0('scripts/02_climate_velocity/output/3D/temp_bio6/aggregated_1km/temporalGradient_', mid, 'm.tif')
    fname_rast = paste0('scripts/02_climate_velocity/output/3D/temp_bio6/aggregated_1km/vocc_', mid, 'm.tif')
    fname_csv = paste0('scripts/02_climate_velocity/output/3D/temp_bio6/aggregated_1km/vocc_', mid, 'm.csv')
    
    writeRaster(sp[[2]], fname_sp, overwrite = T)
    writeRaster(tg, fname_tg, overwrite = T)
    writeRaster(vocc[[1]], fname_rast, overwrite = T)
    write.csv(vocc[[2]], file = fname_csv)
  }
  
}
stopCluster(cl)


