library(terra)
library(data.table)
library(tictoc)
library(numform)

#setwd('/home/ls907/climvocc/microclimModelUpdate')

source('02_climate_velocity/scripts/00_functions/relhgt_convert.R')

# canopy height
chm = rast("./../Trinidad_microclimates/data/topography/chm_reproj.tif")
chm = extend(chm, c(1,1)) # extend to match extent of tmean rasters

hgts = seq(5,35,5)
brks = seq(0,1,0.25)

tmax = rast(list.files(path = '02_climate_velocity/output/3D/avg_daily_maxTemp', pattern = "vocc_..m.tif", full.names=T))
tmax_vocc = subset(tmax, seq(1,61,10)) # subset vocc layer
tmax_spatGrad = subset(tmax, seq(5,65,10))
tmax_xyAng = subset(tmax, seq(9,69,10))
tmax_zAng = subset(tmax, seq(10,70,10))
tmax_tempGrad = rast(list.files(path = '02_climate_velocity/output/3D/avg_daily_maxTemp', pattern = "temporalGradient_..m.tif", full.names = T))

## test ##
c = vect("./../Trinidad_microclimates/data/cropping_polygons/testcrop/testcrop.shp")
r = crop(tmax_vocc, c)
#r = crop(tmax_vocc, ext(700000,700000+20*101, 1178000, 1178000 + 20*101))
chm = crop(chm,c)

tmaxVocc.relhgt = relhgt_convert2(chm, tmax_vocc, hgts, brks)
tmaxSpatGrad.relhgt = relhgt_convert2(chm, tmax_spatGrad, hgts, brks)
tmaxXyAng.relhgt = relhgt_convert2(chm, tmax_xyAng, hgts, brks)
tmaxZAng.relhgt = relhgt_convert2(chm, tmax_zAng, hgts, brks)
tmaxTempGrad.relht = relhgt_convert2(chm, tmax_tempGrad, hgts, brks)

# Note: Magnitude refers to spatial gradient

writeRaster(tmaxVocc.relhgt, 'output/3D/avg_daily_maxTemp/relhgt_convert/tmax_relhgt_vocc.tif')
writeRaster(tmaxSpatGrad.relhgt, 'output/3D/avg_daily_maxTemp/relhgt_convert/tmax_relhgt_spatGrad.tif')
writeRaster(tmaxXyAng.relhgt, 'output/3D/avg_daily_maxTemp/relhgt_convert/tmax_relhgt_xyAng.tif')
writeRaster(tmaxZAng.relhgt, 'output/3D/avg_daily_maxTemp/relhgt_convert/tmax_relhgt_zAng.tif')
writeRaster(tmaxTempGrad.relht, 'output/3D/avg_daily_maxTemp/relhgt_convert/tmax_relhgt_tempGrad.tif')