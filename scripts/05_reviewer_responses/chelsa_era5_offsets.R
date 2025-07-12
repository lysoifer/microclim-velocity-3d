library(here)
library(terra)

# average of mean daily max temps in each month
chel = list.files(path = "data/chelsa/chelsa_raw/tasmax/", pattern = ".tif", full.names = T)
yrs = seq(1980,2015,5)
dem = rast("data/topography/dem_reproj.tif")
dem = project(dem, "epsg:4326")
for(i in 1:8) {
  c = chel[seq(i,88+i,8)]
  c = rast(c)
  c = crop(c, dem)
  dem = resample(dem, c)
  c = mask(c, dem)
  c = mean(c)
  writeRaster(c, paste0("data/chelsa/tasmax_mean/tasmax_", yrs[i], "_meanmonthly.tif"))
}

chel = rast(list.files("data/chelsa/tasmax_mean/", pattern = ".tif", full.names = T))
names(chel) = yrs

# convert to degrees C
chel = chel * 0.1 - 273.15


# ERA5 raster 2m temperature
yrs = seq(1980, 2015,5)
era = list.files("data/era5/rasters/", pattern = ".rds", full.names = T)
names(era) = yrs
era = lapply(era, readRDS)
era = lapply(era, "[[", 2)
era = lapply(era, unwrap)


d = c(rep(1, 31), rep(2, 28), rep(3, 31), rep(4,30), rep(5,31), rep(6,30),
      rep(7,31), rep(8,31), rep(9,30), rep(10,31), rep(11,30), rep(12,31))

d2 = c(rep(1, 31), rep(2, 29), rep(3, 31), rep(4,30), rep(5,31), rep(6,30),
       rep(7,31), rep(8,31), rep(9,30), rep(10,31), rep(11,30), rep(12,31))

# summarise into max daily temp in each month and then annual mean across monthly means
for(i in 1:length(era)) {
  r = era[[i]]
  if(names(era)[i] == "2000" | names(era)[i] == "1980") { # for leap years
    r.dailymax = tapp(r, rep(1:366, each = 24), fun = 'max') # daily max
    r.monthlymean = tapp(r.dailymax, d2, 'mean') # monthly mean of daily max
  } else {
    r.dailymax = tapp(r, rep(1:365, each = 24), fun = 'max') # daily max
    r.monthlymean = tapp(r.dailymax, d, 'mean') # monthly mean of daily max
  }
  era[[i]] = mean(r.monthlymean) # annual mean of daily means
}

era = lapply(era, resample, chel[[1]][[1]], method = "near")

# calculate offsets between era5 and chelsa
offset = list()
for(i in 1:length(era)) {
  offset[[i]] = era[[i]] - chel[[i]]
}

# calculate differences between offsets
diff = list()
for(i in 1:(length(offset)-1)) {
  diff[[i]] = offset[[(i+1)]] - offset[[i]]
}

offset = rast(offset)
x = seq(0,35, 5)

lm = regress(offset, yrs)
