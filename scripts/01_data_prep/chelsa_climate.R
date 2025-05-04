library(stringr)
library(terra)
library(mcera5)
library(microclima)
library(mcera5)
library(dplyr)
library(ecmwfr)
library(ncdf4)
library(curl)
library(keyring)
library(abind)
library(lubridate)
library(tidync)
library(NicheMapR) # remotes::install_github("mrke/NicheMapR")
library(microctools) # remotes::install_github("ilyamaclean/microctools")
library(plantecophys)

files = read.delim('data/chelsa/envidatS3paths(1).txt', header = F, sep = '\n')[,1]
files = str_trim(files)
files2015 = grep('2015', files, value = T)
files1980 = grep('1980', files, value = T)

files.tmax = read.delim('data/chelsa/tasmax_1980_2015.txt', header = F, sep = '\n')[,1]
files.tmax = str_trim(files.tmax)
files.tmax2015 = grep('2015', files.tmax, value = T)
files.tmax1980 = grep('1980', files.tmax, value = T)

setwd("./../Trinidad_microclimates/")
files.tmin = read.delim('data/chelsa/tasmin_1980_2015.txt', header = F, sep = '\n')[,1]
files.tmin = str_trim(files.tmin)
files.tmin2015 = grep('2015', files.tmin, value = T)
files.tmin1980 = grep('1980', files.tmin, value = T)

files.vpd = read.delim('data/chelsa/chelsaVPD_1980_2015.txt', header = F, sep = '\n')[,1]
files.vpd = str_trim(files.vpd)
files.vpd2015 = grep('2015', files.vpd, value = T)
files.vpd1980 = grep('1980', files.vpd, value = T)

files.hum = read.delim('data/chelsa/chelsa_hurs_1980_2015.txt', header = F, sep = '\n')[,1]
files.hum = str_trim(files.hum)
files.hum2015 = grep('2015', files.hum, value = T)
files.hum1980 = grep('1980', files.hum, value = T)

# tas = mean daily temp for each month
tasnames2015 = sapply(strsplit(files2015, '/'), '[[', 10)
tasnames1980 = sapply(strsplit(files1980, '/'), '[[', 10)

tasmaxnames2015 = sapply(strsplit(files.tmax2015, '/'), '[[', 10)
tasmaxnames1980 = sapply(strsplit(files.tmax1980, '/'), '[[', 10)

tasminnames2015 = sapply(strsplit(files.tmin2015, '/'), '[[', 8)
tasminnames1980 = sapply(strsplit(files.tmin1980, '/'), '[[', 8)

vpdnames2015 = sapply(strsplit(files.vpd2015, '/'), '[[', 10)
vpdnames1980 = sapply(strsplit(files.vpd1980, '/'), '[[', 10)

humnames2015 = sapply(strsplit(files.hum2015, '/'), '[[', 10)
humnames1980 = sapply(strsplit(files.hum1980, '/'), '[[', 10)

# download chelsa data
getOption('timeout')
options(timeout = 900)
download.file(files2015, destfile = paste0('data/chelsa/tas2015/', tasnames2015), method = 'libcurl', cacheOK = F, mode = 'wb')
download.file(files1980, destfile = paste0('data/chelsa/tas1980/', tasnames1980), method = 'libcurl', cacheOK = F, mode = 'wb')

download.file(files.tmax2015, destfile = paste0('data/chelsa/tasmax2015/', tasmaxnames2015), method = 'libcurl', cacheOK = F, mode = 'wb')
download.file(files.tmax1980, destfile = paste0('data/chelsa/tasmax1980/', tasmaxnames1980), method = 'libcurl', cacheOK = F, mode = 'wb')

download.file(files.tmin2015, destfile = paste0('data/chelsa/tasmin2015/', tasminnames2015), method = 'libcurl', cacheOK = F, mode = 'wb')
download.file(files.tmin1980, destfile = paste0('data/chelsa/tasmin1980/', tasminnames1980), method = 'libcurl', cacheOK = F, mode = 'wb')

download.file(files.vpd2015, destfile = paste0('data/chelsa/vpd2015/', vpdnames2015), method = 'libcurl', cacheOK = F, mode = 'wb')
download.file(files.vpd1980, destfile = paste0('data/chelsa/vpd1980/', vpdnames1980), method = 'libcurl', cacheOK = F, mode = 'wb')

download.file(files.hum2015, destfile = paste0('data/chelsa/hum2015/', humnames2015), method = 'libcurl', cacheOK = F, mode = 'wb')
download.file(files.hum1980, destfile = paste0('data/chelsa/hum1980/', humnames1980), method = 'libcurl', cacheOK = F, mode = 'wb')

# read in chelsa data
tas2015 = rast(list.files(path = 'data/chelsa/tas2015', pattern = 'CHELSA', full.names = T))
tas1980 = rast(list.files(path = 'data/chelsa/tas1980', pattern = 'CHELSA', full.names = T))
plot(tas1980$CHELSA_tas_01_1980_V.2.1)

tasmax2015 = rast(list.files(path = 'data/chelsa/tasmax2015', pattern = 'CHELSA', full.names = T))
tasmax1980 = rast(list.files(path = 'data/chelsa/tasmax1980', pattern = 'CHELSA', full.names = T))

tasmin2015 = rast(list.files(path = 'data/chelsa/tasmin2015', pattern = 'CHELSA', full.names = T))
tasmin1980 = rast(list.files(path = 'data/chelsa/tasmin1980', pattern = 'CHELSA', full.names = T))


vpd2015 = rast(list.files(path = 'data/chelsa/vpd2015', pattern = 'CHELSA', full.names = T))
vpd1980 = rast(list.files(path = 'data/chelsa/vpd1980', pattern = 'CHELSA', full.names = T))

hum2015 = rast(list.files(path = 'data/chelsa/hum2015', pattern = 'CHELSA', full.names = T))
hum1980 = rast(list.files(path = 'data/chelsa/hum1980', pattern = 'CHELSA', full.names = T))

# crop to Northern Range
dem = rast('data/topography/dem_reproj.tif')
dem.reproj = project(dem, 'epsg:4326')

chelsa = list(tas2015, tas1980, vpd2015, vpd1980, hum2015, hum1980)
chelsa_tasmin = list(tasmin2015, tasmin1980)
names(chelsa) = c('tas2015', 'tas1980', 'vpd2015', 'vpd1980', 'hum2015', 'hum1980')
names(chelsa_tasmin) = c("tasmin2015", "tasmin1980")
chelsa = lapply(chelsa, crop, ext(dem.reproj))
chelsa_tasmin = lapply(chelsa_tasmin, crop, ext(dem.reproj))
dem.macro = resample(dem, chelsa[[1]]$CHELSA_tas_01_2015_V.2.1)
dem.macro = resample(dem.reproj, chelsa_tasmin[[1]]$CHELSA_tasmin_01_2015_V.2.1)
chelsa = lapply(chelsa, mask, dem.macro)
chelsa_tasmin = lapply(chelsa_tasmin, mask, dem.macro)

# average of mean daily temps in each month
chel.tas2015 = mean(chelsa$tas2015)
chel.tas1980 = mean(chelsa$tas1980)

# convert to degrees C
chel.tas2015 = chel.tas2015 * 0.1 - 273.15
chel.tas1980 = chel.tas1980 * 0.1 - 273.15

# average of mean daily max temps in each month
tasmax = list(tasmax2015, tasmax1980)
names(tasmax) = c('tasmax2015', 'tasmax1980')
tasmax = lapply(tasmax, crop, ext(dem.reproj))
dem.macro = resample(dem.reproj, tasmax[[1]]$CHELSA_tasmax_01_2015_V.2.1)
tasmax = lapply(tasmax, mask, dem.macro)

# average of mean daily temps in each month
tasmax2015_mean = mean(tasmax$tasmax2015)
tasmax1980_mean = mean(tasmax$tasmax1980)

# convert to degrees C
tasmax2015_mean = tasmax2015_mean * 0.1 - 273.15
tasmax1980_mean = tasmax1980_mean * 0.1 - 273.15

# max monthly temp
tasmax2015_bio5 = max(tasmax$tasmax2015)
tasmax1980_bio5 = max(tasmax$tasmax1980)

# convert to degrees C
tasmax2015_bio5 = tasmax2015_bio5 * 0.1 - 273.15
tasmax1980_bio5 = tasmax1980_bio5 * 0.1 - 273.15


# mean daily min temp of the coldest month
tasmin2015 = min(chelsa_tasmin$tasmin2015)
tasmin1980 = min(chelsa_tasmin$tasmin1980)

# convert to degrees C
tasmin2015 = tasmin2015 * 0.1 - 273.15
tasmin1980 = tasmin1980 * 0.1 - 273.15

# dry season (jan - may) VPD and rescale VPD to kPa
chel.VPDdry.2015 = mean(subset(chelsa$vpd2015, subset = 1:5)) * 0.1/1000
chel.VPDdry.1980 = mean(subset(chelsa$vpd1980, subset = 1:5)) * 0.1/1000

# average of mean daily relative humidity in each month
chel.hum2015 = mean(chelsa$hum2015)
chel.hum1980 = mean(chelsa$hum1980)

# Mean of mean daily temps for each month in the given year
writeRaster(chel.tas1980, filename = 'data/chelsa/tas1980/tas1980_meanMonthly.tif', overwrite = T)
writeRaster(chel.tas2015, filename = 'data/chelsa/tas2015/tas2015_meanMonthly.tif', overwrite = T)

writeRaster(tasmax1980_mean, filename = 'data/chelsa/tasmax1980/tasmax1980_meanMonthly.tif', overwrite = T)
writeRaster(tasmax2015_mean, filename = 'data/chelsa/tasmax2015/tasmax2015_meanMonthly.tif', overwrite = T)

writeRaster(tasmax1980_bio5, filename = "data/chelsa/tasmax1980/tasmax1980_bio5_maxmonthly.tif")
writeRaster(tasmax2015_bio5, filename = "data/chelsa/tasmax2015/tasmax2015_bio5_maxmonthly.tif")

writeRaster(tasmin1980, filename = 'data/chelsa/tasmin1980/tasmin1980_bio6.tif', overwrite = T)
writeRaster(tasmin2015, filename = 'data/chelsa/tasmin2015/tasmin2015_bio6.tif', overwrite = T)

writeRaster(chel.VPDdry.1980, filename = 'data/chelsa/vpd1980/VPDdry1980_meanMonthly.tif')
writeRaster(chel.VPDdry.2015, filename = 'data/chelsa/vpd2015/VPDdry2015_meanMonthly.tif')

writeRaster(chel.hum1980, filename = 'data/chelsa/hum1980/hum1980_meanMonthly.tif')
writeRaster(chel.hum2015, filename = 'data/chelsa/hum2015/hum2015_meanMonthly.tif')


############################################################################
setwd('./../')
setwd('Trinidad_microclimates')

# load CHELSA: Mean daily air temperature in degrees C
chel1980.tas = rast('data/chelsa/tas1980/tas1980_meanMonthly.tif')
chel1980.VPDdry = rast('data/chelsa/vpd1980/VPDdry1980_meanMonthly.tif')
chel1980.hum = rast('data/chelsa/hum1980/hum1980_meanMonthly.tif')
chel1980.tasmax = rast('data/chelsa/tasmax1980/tasmax1980_meanMonthly.tif')

# ERA5 rasters for temp and relative humidity

eraGrid1980 = rast('data/era5/era5_Trinidad_1980_1980.nc', subds = 't2m')
plot(eraGrid1980$t2m_1)
plot(chel1980.tas, add = T)
eraGrid1980.crop = eraGrid1980[2:3, 2:5, drop = F] # crop to cells that overlap with Trinidad Northern Range

eraGrid1960 = rast('data/era5/past/era5_Trinidad_1951_1960_1960.nc', subds = 't2m')
eraGrid1960.crop = eraGrid1960[2:3, 2:5, drop = F] # crop to cells that overlap with Trinidad Northern Range

st_time1980 <- lubridate::ymd("1980:01:01")
en_time1980 <- lubridate::ymd("1980:12:31")

st_time1960 <- lubridate::ymd("1960:01:01")
en_time1960 <- lubridate::ymd("1960:12:31")

#make blank rasters to store hourly values
eraTemp1980 = eraRelhum1980 = rast(eraGrid1980.crop, vals = 0)
eraTemp1960 = eraRelhum1960 = rast(eraGrid1960.crop, vals = 0)

for (i in 1:ncell(eraGrid1980.crop)) { # for each cell
  # get long/lat of cell center
  ll = xyFromCell(eraGrid1980.crop, cell = i)
  
  # extract climate data from 1980 era5
  clim1980 = extract_clim('data/era5/era5_Trinidad_1980_1980.nc',
                          long = ll[1], lat = ll[2],
                          start_time = st_time1980, end_time = en_time1980)
  
  # add climate data to respective cell for temperature and humidity (1980)
  values(eraTemp1980)[i,] = clim1980$temperature
  values(eraRelhum1980)[i,] = humidityconvert(h = clim1980$humidity, intype = 'specific', tc = clim1980$temperature, p = clim1980$pressure)$relative
  
  # extract climate data from 1960 era5
  clim1960 = extract_clim('data/era5/past/era5_Trinidad_1951_1960_1960.nc',
                          long = ll[1], lat = ll[2],
                          start_time = st_time1960, end_time = en_time1960)
  
  # add climate data to respective cell for temperature and humidity (1960)
  values(eraTemp1960)[i,] = clim1960$temperature
  values(eraRelhum1960)[i,] = humidityconvert(h = clim1960$humidity, intype = 'specific', tc = clim1960$temperature, p = clim1960$pressure)$relative
}

#####################################################################################################

# summarize ERA5 rasters - average mean daily
era = list(eraTemp1980, eraRelhum1980, eraTemp1960, eraRelhum1960)

d = c(rep(1, 31), rep(2, 28), rep(3, 31), rep(4,30), rep(5,31), rep(6,30),
      rep(7,31), rep(8,31), rep(9,30), rep(10,31), rep(11,30), rep(12,31))
eralist.summ = vector(mode = 'list', length = 4)
names(eralist.summ) = c('eraTemp1980', 'eraRelhum1980', 'eraTemp1960', 'eraRelhum1960')

eralist.summ.dry = vector(mode = 'list', length = 4)
names(eralist.summ.dry) = c('eraTemp1980', 'eraRelhum1980', 'eraTemp1960', 'eraRelhum1960')

# summarise into mean daily temp in each month and then annual mean across monthly means
for(i in 1:length(era)) {
  r = era[[i]]
  r.dailymean = tapp(r, rep(1:nlyr(r), each = 24), fun = 'mean') # daily mean
  r.monthlymean = tapp(r.dailymean, d, 'mean') # monthly mean of daily mean
  eralist.summ[[i]] = mean(r.monthlymean) # annual mean of daily means
  eralist.summ.dry[[i]] = mean(subset(r.monthlymean, subset = 1:5)) # annual mean of daily means
}

era1980.dry = c(eralist.summ.dry$eraRelhum1980, eralist.summ.dry$eraTemp1980)
names(era1980.dry) = c('RH', 'TdegC')

era1960.dry = c(eralist.summ.dry$eraRelhum1960, eralist.summ.dry$eraTemp1960)
names(era1960.dry) = c('RH', 'TdegC')

era.VPDdry1980 = lapp(era1980.dry, RHtoVPD, usenames = T)
era.VPDdry1960 = lapp(era1960.dry, RHtoVPD, usenames = T)

# resample ERA5 data to extent and resolution of CHELSA data
eralist.summ = lapply(eralist.summ, resample, y = chel1980.tas, method = 'near')
era.VPDdry1980 = resample(era.VPDdry1980, y = chel1980.tas, method = 'near')
era.VPDdry1960 = resample(era.VPDdry1960, y = chel1980.tas, method = 'near')

# calculate difference between ERA5 and CHELSA (so ERA5 - tempoffset = chelsa)
# temperature
temp.offset = eralist.summ$eraTemp1980 - chel1980.tas
chel1960.tas = eralist.summ$eraTemp1960 - temp.offset

writeRaster(chel1960.tas, filename = 'data/chelsa/tas1960_estimate.tif', overwrite = T)

# humidity
hum.offset = eralist.summ$eraRelhum1980 - chel1980.hum
chel1960.hum = eralist.summ$eraRelhum1980 - hum.offset

writeRaster(chel1960.hum, filename = 'data/chelsa/hum1960_estimate.tif', overwrite = T)

# dry season VPD
vpd.offset = era.VPDdry1980 - chel1980.VPDdry
chel1960.VPDdry = era.VPDdry1960 - vpd.offset

writeRaster(chel1960.VPDdry, filename = 'data/chelsa/VPDdry1960_estimate.tif', overwrite = T)




