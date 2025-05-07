# Run in R v.4.1.2

source('scripts/02_climate_velocity/scripts/00_functions/spatgrad_micro2D.R')
source('scripts/02_climate_velocity/scripts/00_functions/spatgrad.R')
source('scripts/02_climate_velocity/scripts/00_functions/tempGrad2.R')
source('scripts/02_climate_velocity/scripts/00_functions/gVoCC_micro2D.R')

library(terra)
library(data.table)
library(numform)
library(parallel)
library(doParallel)


past = list.files(path = 'data/microclim_3D/mosaics/mosaics_meanTmax/mean_Tmax_20m/past/', pattern = '.tif', full.names = T)
pres = list.files(pat = 'data/microclim_3D/mosaics/mosaics_meanTmax/mean_Tmax_20m/pres/', pattern = 'pres', full.names = T)
heights = c(2,5,10,15,20,25,30,35,40)

cl = makeForkCluster(9)
registerDoParallel(cl)
foreach (i=c(1:length(past)), .packages=c('terra', 'data.table', 'numform')) %dopar% {
    past = rast(past[i])$max
    pres = rast(pres[i])$max
    
    hgt = heights[i]

    dem = rast('data/topography/dem_reproj.tif')
    dem = extend(dem, c(1,1))

    past = crop(past, dem)
    pres = crop(pres, dem)

    sp = spatgrad_2Dmicro(c(past, pres), projected = T, slope_correct = T, dem = dem, hgt = hgt)
    tg = tempGrad2(past, pres, 1960, 2015)
    vocc = gVocc_micro2D(tg,sp)

    fname_sp = paste0('scripts/02_climate_velocity/output/2D/avg_daily_maxTemp/spatialGradient_', f_pad_zero(hgt,2), 'm.tif')
    fname_tg = paste0('scripts/02_climate_velocity/output/2D/avg_daily_maxTemp/temporalGradient_',f_pad_zero(hgt,2), 'm.tif')
    fname_rast = paste0('scripts/02_climate_velocity/output/2D/avg_daily_maxTemp/vocc_', f_pad_zero(hgt,2), 'm.tif')
    fname_csv = paste0('scripts/02_climate_velocity/output/2D/avg_daily_maxTemp/vocc_', f_pad_zero(hgt,2), 'm.csv')

    writeRaster(sp[[2]], fname_sp, overwrite = T)
    writeRaster(tg, fname_tg, overwrite = T)
    writeRaster(vocc[[1]], fname_rast, overwrite = T)
    write.csv(vocc[[2]], file = fname_csv)    
}
stopCluster(cl)

## make map with absolute value of 2m velocity for mapping
vocc2d = rast("scripts/02_climate_velocity/output/2D/avg_daily_maxTemp/vocc_02m.tif")
vocc2d = classify(vocc2d, matrix(c(Inf, NA), ncol =2, byrow = T))
vocc2d = abs(vocc2d)

vocc2d
summary(vocc2d$vocc)
global(vocc2d$vocc, fun = quantile, probs = 0.9999, na.rm = T)

# remove 0.01% points for plotting purposes (vocc value above 480.58)

vocc2d = classify(vocc2d, matrix(c(480.86, Inf, NA), ncol = 3, byrow = T))

writeRaster(vocc2d[["vocc"]], "scripts/02_climate_velocity/output/2D/avg_daily_maxTemp/ABSvocc_02m.tif", overwrite = T)


# bio6 temp
past = list.files(path = 'data/microclim_3D/mosaics_temp_bio6/temp_bio6_20m/past', pattern = 'past', full.names = T)
pres = list.files(pat = 'data/microclim_3D/mosaics_temp_bio6/temp_bio6_20m/pres', pattern = 'pres', full.names = T)
heights = c(2,5,10,15,20,25,30,35,40)


cl = makeForkCluster(9)
registerDoParallel(cl)
foreach (i=c(1:length(past)), .packages=c('terra', 'data.table', 'numform')) %dopar% {
  past = rast(past[i])[[1]]
  past = past/100 # convert to degC
  pres = rast(pres[i])[[1]]
  pres = pres/100 #convert to degC
  
  hgt = heights[i]
  
  dem = rast('data/topography/dem_reproj.tif')
  dem = extend(dem, c(1,1))
  
  past = crop(past, dem)
  pres = crop(pres, dem)
  
  sp = spatgrad_2Dmicro(c(past, pres), projected = T, slope_correct = T, dem = dem, hgt = hgt)
  tg = tempGrad2(past, pres, 1960, 2015)
  vocc = gVocc_micro2D(tg,sp)
  
  fname_sp = paste0('scripts/02_climate_velocity/output/2D/temp_bio6/spatialGradient_', f_pad_zero(hgt,2), 'm.tif')
  fname_tg = paste0('scripts/02_climate_velocity/output/2D/temp_bio6/temporalGradient_',f_pad_zero(hgt,2), 'm.tif')
  fname_rast = paste0('scripts/02_climate_velocity/output/2D/temp_bio6/vocc_', f_pad_zero(hgt,2), 'm.tif')
  fname_csv = paste0('scripts/02_climate_velocity/output/2D/temp_bio6/vocc_', f_pad_zero(hgt,2), 'm.csv')
  
  writeRaster(sp[[2]], fname_sp, overwrite = T)
  writeRaster(tg, fname_tg, overwrite = T)
  writeRaster(vocc[[1]], fname_rast, overwrite = T)
  write.csv(vocc[[2]], file = fname_csv)    
}
stopCluster(cl)


# bio5 temp
past = list.files(path = 'data/microclim_3D/mosaics/mosaics_temp_bio5/temp_bio5_20m/past', pattern = '.tif', full.names = T)
pres = list.files(pat = 'data/microclim_3D/mosaics/mosaics_temp_bio5/temp_bio5_20m/pres', pattern = '.tif', full.names = T)
heights = c(2,5,10,15,20,25,30,35,40)


cl = makeForkCluster(9)
registerDoParallel(cl)
foreach (i=c(1:length(past)), .packages=c('terra', 'data.table', 'numform')) %dopar% {
  past = rast(past[i])[[1]]
  past = past/100 # convert to degC
  pres = rast(pres[i])[[1]]
  pres = pres/100 #convert to degC
  
  hgt = heights[i]
  
  dem = rast('data/topography/dem_reproj.tif')
  dem = extend(dem, c(1,1))
  
  past = crop(past, dem)
  pres = crop(pres, dem)
  
  sp = spatgrad_2Dmicro(c(past, pres), projected = T, slope_correct = T, dem = dem, hgt = hgt)
  tg = tempGrad2(past, pres, 1960, 2015)
  vocc = gVocc_micro2D(tg,sp)
  
  fname_sp = paste0('output/2D/temp_bio5/spatialGradient_', f_pad_zero(hgt,2), 'm.tif')
  fname_tg = paste0('output/2D/temp_bio5/temporalGradient_',f_pad_zero(hgt,2), 'm.tif')
  fname_rast = paste0('output/2D/temp_bio5/vocc_', f_pad_zero(hgt,2), 'm.tif')
  fname_csv = paste0('output/2D/temp_bio5/vocc_', f_pad_zero(hgt,2), 'm.csv')
  
  writeRaster(sp[[2]], fname_sp, overwrite = T)
  writeRaster(tg, fname_tg, overwrite = T)
  writeRaster(vocc[[1]], fname_rast, overwrite = T)
  write.csv(vocc[[2]], file = fname_csv)    
}
stopCluster(cl)

