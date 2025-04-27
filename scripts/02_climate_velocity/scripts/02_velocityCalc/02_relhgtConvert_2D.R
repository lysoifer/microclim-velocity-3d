# Convert absolute height to relative height in the forest

library(terra)
library(data.table)
library(tictoc)
library(numform)

#setwd('/home/ls907/climvocc/microclimModelUpdate')

source('scripts/00_functions/relhgt_convert.R')

# canopy height
chm = rast('/home/ls907/Trinidad_microclimates/data/topography/chm_reproj.tif')
chm = extend(chm, c(1,1)) # extend to match extent of tmean rasters

hgts = c(2,seq(5,40,5))
brks = seq(0,1,0.25)

tmax = rast(list.files(path = 'output/2D/avg_daily_maxTemp', pattern = "vocc_..m.tif", full.names=T))
tmax_vocc = subset(tmax, seq(1,61,7)) # subset vocc layer
tmax_spatGrad = subset(tmax, seq(4,65,7))
tmax_xyAng = subset(tmax, seq(7,63,7))
tmax_tempGrad = rast(list.files(path = 'output/2D/avg_daily_maxTemp', pattern = "temporalGradient_..m.tif", full.names = T))

tmaxVocc.relhgt = relhgt_convert2(chm, tmax_vocc, hgts, brks)
tmaxSpatGrad.relhgt = relhgt_convert2(chm, tmax_spatGrad, hgts, brks)
tmaxXyAng.relhgt = relhgt_convert2(chm, tmax_xyAng, hgts, brks)
tmaxTempGrad.relhgt = relhgt_convert2(chm, tmax_tempGrad, hgts, brks)

# Note: Magnitude refers to spatial gradient

writeRaster(tmaxVocc.relhgt, 'output/2D/avg_daily_maxTemp/relhgt_convert/tmax_relhgt_vocc.tif')
writeRaster(tmaxSpatGrad.relhgt, 'output/2D/avg_daily_maxTemp/relhgt_convert/tmax_relhgt_spatGrad.tif')
writeRaster(tmaxXyAng.relhgt, 'output/2D/avg_daily_maxTemp/relhgt_convert/tmax_relhgt_xyAng.tif')
writeRaster(tmaxTempGrad.relhgt, 'output/2D/avg_daily_maxTemp/relhgt_convert/tmax_relhgt_tempGrad.tif')
