# This code calculates 3D microclimate velocity for mean daily maximum temperatures.
# Run in Rv4.1.2

library(data.table)
library(terra)
library(numform)
library(parallel)
library(doParallel)

source('scripts/02_climate_velocity/scripts/00_functions/spatgrad.R')
source('scripts/02_climate_velocity/scripts/00_functions/tempGrad2.R')

past = list.files(path = 'data/microclim3D/mosaics', pattern = '.*past_.*.tif', full.names = T)
pres = list.files(pat = 'data/microclim3D/mosaics', pattern = '.*pres_.*.tif', full.names = T)
heights = c(2,5,10,15,20,25,30,35,40)

cl = makeForkCluster(7)
registerDoParallel(cl)
foreach (i=c(1:length(past)), .packages=c('terra', 'data.table', 'numform')) %dopar% {
    if (i <= length(past)-2) {
        past3 = rast(past[i])$max
        past2 = rast(past[i+1])$max
        past1 = rast(past[i+2])$max
        climpast = c(past1, past2, past3) #stack from highest to lowest in the canopy
        climpast = climpast/100 # convert to degC

        pres3 = rast(pres[i])$max
        pres2 = rast(pres[i+1])$max
        pres1 = rast(pres[i+2])$max
        climpres = c(pres1, pres2, pres3) #stack from highest to lowest in the canopy
        climpres = climpres/100 # convert to degC

        hgts = c(heights[i+2], heights[i+1], heights[i])

        dem = rast('data/topography/dem_reproj.tif')
        dem = extend(dem, c(1,1))

        climpast = crop(climpast, dem)
        climpres = crop(climpres, dem)
        
        sp = spatgrad2(climpast, climpres, hgts, slope_correct = T, dem = dem)
        tg = tempGrad2(climpast[[2]], climpres[[2]], 1960, 2015)
        vocc = gVocc(tg,sp)

        mid = f_pad_zero(hgts[2], 2)

        fname_sp = paste0('scripts/output/3D/avg_daily_maxTemp/spatialGradient_', mid, 'm.tif')
        fname_tg = paste0('scripts/output/3D/avg_daily_maxTemp/temporalGradient_', mid, 'm.tif')
        fname_rast = paste0('scripts/output/3D/avg_daily_maxTemp/vocc_', mid, 'm.tif')
        fname_csv = paste0('scripts/output/3D/avg_daily_maxTemp/vocc_', mid, 'm.csv')

        writeRaster(sp[[2]], fname_sp, overwrite = T)
        writeRaster(tg, fname_tg, overwrite = T)
        writeRaster(vocc[[1]], fname_rast, overwrite = T)
        write.csv(vocc[[2]], file = fname_csv)
    }
    
}
stopCluster(cl)

# min temp (bio6)
past = list.files(path = 'data/microclim_3D/mosaics_temp_bio6/past', pattern = '.*past_.*.tif', full.names = T)
pres = list.files(pat = 'data/microclim_3D/mosaics_temp_bio6/pres', pattern = '.*pres_.*.tif', full.names = T)
heights = c(2,5,10,15,20,25,30,35,40)

cl = makeForkCluster(7)
registerDoParallel(cl)
foreach (i=c(1:length(past)), .packages=c('terra', 'data.table', 'numform')) %dopar% {
  if (i <= length(past)-2) {
    past3 = rast(past[i])[[1]]
    past2 = rast(past[i+1])[[1]]
    past1 = rast(past[i+2])[[1]]
    climpast = c(past1, past2, past3) #stack from highest to lowest in the canopy
    climpast = climpast/100 # convert to degC
    
    pres3 = rast(pres[i])[[1]]
    pres2 = rast(pres[i+1])[[1]]
    pres1 = rast(pres[i+2])[[1]]
    climpres = c(pres1, pres2, pres3) #stack from highest to lowest in the canopy
    climpres = climpres/100 # convert to degC
    
    hgts = c(heights[i+2], heights[i+1], heights[i])
    
    dem = rast('/home/ls907/Trinidad_microclimates/data/topography/dem_reproj.tif')
    dem = extend(dem, c(1,1))
    
    climpast = crop(climpast, dem)
    climpres = crop(climpres, dem)
    
    sp = spatgrad2(climpast, climpres, hgts, slope_correct = T, dem = dem)
    tg = tempGrad2(climpast[[2]], climpres[[2]], 1960, 2015)
    vocc = gVocc(tg,sp)
    
    mid = f_pad_zero(hgts[2], 2)
    
    fname_sp = paste0('scripts/02_climate_velocity/output/3D/temp_bio6/spatialGradient_', mid, 'm.tif')
    fname_tg = paste0('scripts/02_climate_velocity/output/3D/temp_bio6/temporalGradient_', mid, 'm.tif')
    fname_rast = paste0('scripts/02_climate_velocity/output/3D/temp_bio6/vocc_', mid, 'm.tif')
    fname_csv = paste0('scripts/02_climate_velocity/output/3D/temp_bio6/vocc_', mid, 'm.csv')
    
    writeRaster(sp[[2]], fname_sp, overwrite = T)
    writeRaster(tg, fname_tg, overwrite = T)
    writeRaster(vocc[[1]], fname_rast, overwrite = T)
    write.csv(vocc[[2]], file = fname_csv)
  }
  
}
stopCluster(cl)

