# Estimate Chelsa climate for 1960 based on offsets with ERA5

library(here)
library(terra)
library(mcera5)

setwd('./../')
setwd('Trinidad_microclimates')

# load CHELSA: Mean daily air temperature in degrees C
chel1980.tasmax = rast('data/chelsa/tasmax1980/tasmax1980_meanMonthly.tif')
chel1980.bio5 = rast("data/chelsa/tasmax1980/tasmax1980_bio5_maxmonthly.tif")
chel1980.tasmin = rast('data/chelsa/tasmin1980/tasmin1980_bio6.tif')

# ERA5 raster 2m temperature
# ERA5 1980
eraGrid1980 = rast('data/era5/era5_Trinidad_1980_1980.nc', subds = 't2m')
plot(eraGrid1980$t2m_1)
plot(chel1980.tasmax, add = T)
eraGrid1980.crop = eraGrid1980[2:3, 2:5, drop = F] # crop to cells that overlap with Trinidad Northern Range

# ERA5 1960
eraGrid1960 = rast('data/era5/past/era5_Trinidad_1951_1960_1960.nc', subds = 't2m')
eraGrid1960.crop = eraGrid1960[2:3, 2:5, drop = F] # crop to cells that overlap with Trinidad Northern Range


st_time1980 <- lubridate::ymd("1980:01:01")
en_time1980 <- lubridate::ymd("1980:12:31")

st_time1960 <- lubridate::ymd("1960:01:01")
en_time1960 <- lubridate::ymd("1960:12:31")


#make blank rasters to store hourly values
eraTmax1980 = rast(eraGrid1980.crop, vals = 0)
eraTmax1960 = rast(eraGrid1960.crop, vals = 0)



for (i in 1:ncell(eraGrid1980.crop)) { # for each cell
  # get long/lat of cell center
  ll = xyFromCell(eraGrid1980.crop, cell = i)
  
  # extract climate data from 1980 era5
  clim1980 = extract_clim('data/era5/era5_Trinidad_1980_1980.nc',
                          long = ll[1], lat = ll[2],
                          start_time = st_time1980, end_time = en_time1980)
  
  # add climate data to respective cell for temperature and humidity (1980)
  values(eraTmax1980)[i,] = clim1980$temperature
  
  # extract climate data from 1960 era5
  clim1960 = extract_clim('data/era5/past/era5_Trinidad_1951_1960_1960.nc',
                          long = ll[1], lat = ll[2],
                          start_time = st_time1960, end_time = en_time1960)
  
  # add climate data to respective cell for temperature and humidity (1960)
  values(eraTmax1960)[i,] = clim1960$temperature
  }

# FOR TMIN (EXTRACT_CLIM VERSION ISSUES)
eraGrid1960.crop = eraGrid1960.crop - 273.15 # kelvin to celsius
eraGrid1980.crop = eraGrid1980.crop - 273.15 # kelvin to celsius

# summarize ERA5 rasters - average max daily

# groups for summarizing by month
era = list(eraTmax1980, eraTmax1960)
eramin = list(eraGrid1960.crop, eraGrid1980.crop)

d = c(rep(1, 31), rep(2, 29), rep(3, 31), rep(4,30), rep(5,31), rep(6,30),
      rep(7,31), rep(8,31), rep(9,30), rep(10,31), rep(11,30), rep(12,31))
eralist.summ = vector(mode = 'list', length = 2)
names(eralist.summ) = c('eraTmax1980', 'eraTmax1960')

eralist.bio5.summ = vector(mode = 'list', length = 2)
names(eralist.bio5.summ) = c('eraBio51980', 'eraBio51960')

eraminlist.summ = vector(mode = 'list', length = 2)
names(eraminlist.summ) = c('eraTmin1980', 'eraTmin1960')

# summarise into max daily temp in each month and then annual mean across monthly means
for(i in 1:length(era)) {
  r = era[[i]]
  r.dailymax = tapp(r, rep(1:366, each = 24), fun = 'max') # daily max
  r.monthlymean = tapp(r.dailymax, d, 'mean') # monthly mean of daily max
  eralist.summ[[i]] = mean(r.monthlymean) # annual mean of daily means
}

for(i in 1:length(era)) {
  r = era[[i]]
  r.dailymax = tapp(r, rep(1:366, each = 24), fun = 'max') # daily max
  r.monthlymean = tapp(r.dailymax, d, 'mean') # monthly mean of daily max
  eralist.bio5.summ[[i]] = max(r.monthlymean) # max of average monthly max
}

# summarise into mean min daily temp in each month and then min across monthly means
for(i in 1:length(eramin)) {
  r = eramin[[i]]
  r.dailymin = tapp(r, rep(1:366, each = 24), fun = 'min') # daily max
  r.monthlymean = tapp(r.dailymin, d, 'mean') # monthly mean of daily max
  eraminlist.summ[[i]] = min(r.monthlymean) # annual min of monthly average mins
}


# resample ERA5 data to extent and resolution of CHELSA data
era.Tmax1980 = resample(eralist.summ$eraTmax1980, y = chel1980.tasmax, method = 'near')
era.Tmax1960 = resample(eralist.summ$eraTmax1960, y = chel1980.tasmax, method = 'near')

era.Tmax1980.bio5 = terra::resample(eralist.bio5.summ$eraBio51980, y = chel1980.bio5, method = 'near')
era.Tmax1960.bio5 = resample(eralist.bio5.summ$eraBio51960, y = chel1980.bio5, method = 'near')

era.Tmin1980 = resample(eraminlist.summ$eraTmin1980, y = chel1980.tasmin, method = 'near')
era.Tmin1960 = resample(eraminlist.summ$eraTmin1960, y = chel1980.tasmin, method = 'near')

# calculate difference between ERA5 and CHELSA (so ERA5 - tempoffset = chelsa)
# temperature
temp.offset = era.Tmax1980 - chel1980.tasmax
chel1960.tasmax = era.Tmax1960 - temp.offset

tempbio5.offset = era.Tmax1980.bio5 - chel1980.bio5
chel1960.bio5 = era.Tmax1960.bio5 - tempbio5.offset

tempmin.offset = era.Tmin1980 - chel1980.tasmin
chel1960.tasmin = era.Tmin1960 - tempmin.offset

writeRaster(chel1960.tasmax, filename = 'data/chelsa/tasmax1960_estimate.tif', overwrite = T)
writeRaster(chel1960.bio5, filename = 'data/chelsa/tasmax1960_bio5_estimate.tif', overwrite = T)
writeRaster(chel1960.tasmin, filename = "data/chelsa/tasmin1960_bio6estimate.tif", overwrite = T)
