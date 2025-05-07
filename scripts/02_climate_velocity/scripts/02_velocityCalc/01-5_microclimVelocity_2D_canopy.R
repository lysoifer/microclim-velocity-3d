#setwd('/home/ls907/climvocc/microclimModelUpdate')

source('scripts/02_climate_velocity/scripts/00_functions/spatgrad_micro2D.R')
source('scripts/02_climate_velocity/scripts/00_functions/spatgrad.R')
source('scripts/02_climate_velocity/scripts/00_functions/tempGrad2.R')
source('scripts/02_climate_velocity/scripts/00_functions/gVoCC_micro2D.R')

library(terra)
library(data.table)
library(numform)
library(parallel)
library(doParallel)


# MEAN TMAX ---------------------------------------------------------------

past = rast('data/microclim_3D/mosaics_relhgt/mosaics_relhgt_20m/meanTmax/tmax_relhgt_past.tif')
pres = rast('data/microclim_3D/mosaics_relhgt/mosaics_relhgt_20m/meanTmax/tmax_relhgt_pres.tif')

# get temp at top quarter of the canopy
past = past[[4]]
pres = pres[[4]]
#heights = c(2,5,10,15,20,25,30,35,40)

#cl = makeForkCluster(9)
#registerDoParallel(cl)
foreach (i=c(1:length(past)), .packages=c('terra', 'data.table', 'numform')) %do% {
    #past = rast(past[i])$max
    past = past/100 # convert to degC
    #pres = rast(pres[i])$max
    pres = pres/100 #convert to degC
    
    hgt = "q4"

    dem = rast('data/topography/dem_reproj.tif')
    dem = extend(dem, c(1,1))

    past = crop(past, dem)
    pres = crop(pres, dem)

    sp = spatgrad_2Dmicro(c(past, pres), projected = T, slope_correct = T, dem = dem, hgt = hgt)
    tg = tempGrad2(past, pres, 1960, 2015)
    vocc = gVocc_micro2D(tg,sp)

    fname_sp = paste0('output/2D/avg_daily_maxTemp_canopy/spatialGradient_', f_pad_zero(hgt,2), 'm.tif')
    fname_tg = paste0('output/2D/avg_daily_maxTemp_canopy/temporalGradient_',f_pad_zero(hgt,2), 'm.tif')
    fname_rast = paste0('output/2D/avg_daily_maxTemp_canopy/vocc_', f_pad_zero(hgt,2), 'm.tif')
    fname_csv = paste0('output/2D/avg_daily_maxTemp_canopy/vocc_', f_pad_zero(hgt,2), 'm.csv')

    writeRaster(sp[[2]], fname_sp, overwrite = T)
    writeRaster(tg, fname_tg, overwrite = T)
    writeRaster(vocc[[1]], fname_rast, overwrite = T)
    write.csv(vocc[[2]], file = fname_csv)    
}
#stopCluster(cl)


# TMIN (BIO6) -------------------------------------------------------------

# bio6 temp
past = rast('data/microclim_3D/mosaics_relhgt/mosaics_relhgt_20m/temp_bio6/temp_bio6_past.tif')
pres = rast('data/microclim_3D/mosaics_relhgt/mosaics_relhgt_20m/temp_bio6/temp_bio6_pres.tif')
#heights = c(2,5,10,15,20,25,30,35,40)

# get top quarter of the canopy
past = past[[4]]
pres = pres[[4]]

#cl = makeForkCluster(9)
#registerDoParallel(cl)
foreach (i=c(1:length(past)), .packages=c('terra', 'data.table', 'numform')) %do% {
    #past = rast(past[i])[[1]]
    past = past/100 # convert to degC
    #pres = rast(pres[i])[[1]]
    pres = pres/100 #convert to degC
    
    hgt = "q4"

    dem = rast('data/topography/dem_reproj.tif')
    dem = extend(dem, c(1,1))

    past = crop(past, dem)
    pres = crop(pres, dem)

    sp = spatgrad_2Dmicro(c(past, pres), projected = T, slope_correct = T, dem = dem, hgt = hgt)
    tg = tempGrad2(past, pres, 1960, 2015)
    vocc = gVocc_micro2D(tg,sp)

    fname_sp = paste0('output/2D/temp_bio6_canopy/spatialGradient_', f_pad_zero(hgt,2), 'm.tif')
    fname_tg = paste0('output/2D/temp_bio6_canopy/temporalGradient_',f_pad_zero(hgt,2), 'm.tif')
    fname_rast = paste0('output/2D/temp_bio6_canopy/vocc_', f_pad_zero(hgt,2), 'm.tif')
    fname_csv = paste0('output/2D/temp_bio6_canopy/vocc_', f_pad_zero(hgt,2), 'm.csv')

    writeRaster(sp[[2]], fname_sp, overwrite = T)
    writeRaster(tg, fname_tg, overwrite = T)
    writeRaster(vocc[[1]], fname_rast, overwrite = T)
    write.csv(vocc[[2]], file = fname_csv)    
}
#stopCluster(cl)

# bio5 temp
past = rast('data/microclim_3D/mosaics_relhgt/mosaics_relhgt_20m/temp_bio5/temp_bio5_past.tif')
pres = rast('data/microclim_3D/mosaics_relhgt/mosaics_relhgt_20m/temp_bio5/temp_bio5_pres.tif')
#heights = c(2,5,10,15,20,25,30,35,40)

# get top quarter of the canopy
past = past[[4]]
pres = pres[[4]]

#cl = makeForkCluster(9)
#registerDoParallel(cl)

foreach (i=c(1:length(past)), .packages=c('terra', 'data.table', 'numform')) %do% {
  #past = rast(past[i])[[1]]
  past = past/100 # convert to degC
  #pres = rast(pres[i])[[1]]
  pres = pres/100 #convert to degC
  
  hgt = "q4"
  
  dem = rast('data/topography/dem_reproj.tif')
  dem = extend(dem, c(1,1))
  
  past = crop(past, dem)
  pres = crop(pres, dem)
  
  sp = spatgrad_2Dmicro(c(past, pres), projected = T, slope_correct = T, dem = dem, hgt = hgt)
  tg = tempGrad2(past, pres, 1960, 2015)
  vocc = gVocc_micro2D(tg,sp)
  
  fname_sp = paste0('output/2D/temp_bio5_canopy/spatialGradient_', f_pad_zero(hgt,2), 'm.tif')
  fname_tg = paste0('output/2D/temp_bio5_canopy/temporalGradient_',f_pad_zero(hgt,2), 'm.tif')
  fname_rast = paste0('output/2D/temp_bio5_canopy/vocc_', f_pad_zero(hgt,2), 'm.tif')
  fname_csv = paste0('output/2D/temp_bio5_canopy/vocc_', f_pad_zero(hgt,2), 'm.csv')
  
  writeRaster(sp[[2]], fname_sp, overwrite = T)
  writeRaster(tg, fname_tg, overwrite = T)
  writeRaster(vocc[[1]], fname_rast, overwrite = T)
  write.csv(vocc[[2]], file = fname_csv)    
}
#stopCluster(cl)

