library(mcera5)
library(terra)
library(dplyr)
library(ecmwfr)
library(ncdf4)
library(curl)
library(keyring)
library(abind)
library(lubridate)
library(tidync)

# download era5 data to compare offsets with chelsa data over time

r = rast("data/topography/dem_reproj.tif")
r = extend(r, 50)
r = project(r, "epsg:4326")

uid = "lysoifer@gmail.com"

ecmwfr::wf_set_key(key = "858e83db-1d03-4b3b-a191-0522c954cea7")


# 1980
req = build_era5_request(xmin = xmin(r),
                         xmax = xmax(r),
                         ymin = ymin(r),
                         ymax = ymax(r),
                         start_time = as.POSIXlt("1980-01-01 00:00:00", tz = "UTC"),
                         end_time = as.POSIXlt("1980-12-31 23:00:00", tz = "UTC"),
                         outfile_name = "era5_trinidad")

request_era5(req, uid = "lysoifer@gmail.com", out_path = "data/era5/", overwrite = T)
test = rast("data/era5/era5_trinidad_1980.nc")


# 1985
req = build_era5_request(xmin = xmin(r),
                         xmax = xmax(r),
                         ymin = ymin(r),
                         ymax = ymax(r),
                         start_time = as.POSIXlt("1985-01-01 00:00:00", tz = "UTC"),
                         end_time = as.POSIXlt("1985-12-31 23:00:00", tz = "UTC"),
                         outfile_name = "era5_trinidad")

request_era5(req, uid = "lysoifer@gmail.com", out_path = "data/era5/", overwrite = T)
test = rast("data/era5/era5_trinidad_1985.nc")

# 1990
req = build_era5_request(xmin = xmin(r),
                         xmax = xmax(r),
                         ymin = ymin(r),
                         ymax = ymax(r),
                         start_time = as.POSIXlt("1990-01-01 00:00:00", tz = "UTC"),
                         end_time = as.POSIXlt("1990-12-31 23:00:00", tz = "UTC"),
                         outfile_name = "era5_trinidad")

request_era5(req, uid = "lysoifer@gmail.com", out_path = "data/era5/", overwrite = T)
test = rast("data/era5/era5_trinidad_1990.nc")

# 1995
req = build_era5_request(xmin = xmin(r),
                         xmax = xmax(r),
                         ymin = ymin(r),
                         ymax = ymax(r),
                         start_time = as.POSIXlt("1995-01-01 00:00:00", tz = "UTC"),
                         end_time = as.POSIXlt("1995-12-31 23:00:00", tz = "UTC"),
                         outfile_name = "era5_trinidad")

request_era5(req, uid = "lysoifer@gmail.com", out_path = "data/era5/", overwrite = T)

# 2000
req = build_era5_request(xmin = xmin(r),
                         xmax = xmax(r),
                         ymin = ymin(r),
                         ymax = ymax(r),
                         start_time = as.POSIXlt("2000-01-01 00:00:00", tz = "UTC"),
                         end_time = as.POSIXlt("2000-12-31 23:00:00", tz = "UTC"),
                         outfile_name = "era5_trinidad")

request_era5(req, uid = "lysoifer@gmail.com", out_path = "data/era5/", overwrite = T)

# 2005
req = build_era5_request(xmin = xmin(r),
                         xmax = xmax(r),
                         ymin = ymin(r),
                         ymax = ymax(r),
                         start_time = as.POSIXlt("2005-01-01 00:00:00", tz = "UTC"),
                         end_time = as.POSIXlt("2005-12-31 23:00:00", tz = "UTC"),
                         outfile_name = "era5_trinidad")

request_era5(req, uid = "lysoifer@gmail.com", out_path = "data/era5/", overwrite = T)

# 2010
req = build_era5_request(xmin = xmin(r),
                         xmax = xmax(r),
                         ymin = ymin(r),
                         ymax = ymax(r),
                         start_time = as.POSIXlt("2010-01-01 00:00:00", tz = "UTC"),
                         end_time = as.POSIXlt("2010-12-31 23:00:00", tz = "UTC"),
                         outfile_name = "era5_trinidad")

request_era5(req, uid = "lysoifer@gmail.com", out_path = "data/era5/", overwrite = T)

# 2015
req = build_era5_request(xmin = xmin(r),
                         xmax = xmax(r),
                         ymin = ymin(r),
                         ymax = ymax(r),
                         start_time = as.POSIXlt("2015-01-01 00:00:00", tz = "UTC"),
                         end_time = as.POSIXlt("2015-12-31 23:00:00", tz = "UTC"),
                         outfile_name = "era5_trinidad")

request_era5(req, uid = "lysoifer@gmail.com", out_path = "data/era5/", overwrite = T)

# extract climate data
clim1980 = extract_clima("data/era5/era5_trinidad_1980.nc",
                         long_min = xmin(r),
                         long_max = xmax(r),
                         lat_min = ymin(r),
                         lat_max = ymax(r),
                         start_time = as.POSIXlt("1980-01-01 00:00:00", tz = "UTC"),
                         end_time = as.POSIXlt("1980-12-31 23:00:00", tz = "UTC"))
saveRDS(clim1980, "data/era5/rasters/trinidad_1980.rds")

clim1985 = extract_clima("data/era5/era5_trinidad_1985.nc",
                         long_min = xmin(r),
                         long_max = xmax(r),
                         lat_min = ymin(r),
                         lat_max = ymax(r),
                         start_time = as.POSIXlt("1985-01-01 00:00:00", tz = "UTC"),
                         end_time = as.POSIXlt("1985-12-31 23:00:00", tz = "UTC"))
saveRDS(clim1985, "data/era5/rasters/trinidad_1985.rds")

clim1990 = extract_clima("data/era5/era5_trinidad_1990.nc",
              long_min = xmin(r),
              long_max = xmax(r),
              lat_min = ymin(r),
              lat_max = ymax(r),
              start_time = as.POSIXlt("1990-01-01 00:00:00", tz = "UTC"),
              end_time = as.POSIXlt("1990-12-31 23:00:00", tz = "UTC"))
saveRDS(clim1990, "data/era5/rasters/trinidad_1990.rds")
readRDS("data/era5/rasters/trinidad_1990.rds")

clim1995 = extract_clima("data/era5/era5_trinidad_1995.nc",
                         long_min = xmin(r),
                         long_max = xmax(r),
                         lat_min = ymin(r),
                         lat_max = ymax(r),
                         start_time = as.POSIXlt("1995-01-01 00:00:00", tz = "UTC"),
                         end_time = as.POSIXlt("1995-12-31 23:00:00", tz = "UTC"))
saveRDS(clim1995, "data/era5/rasters/trinidad_1995.rds")

clim2000 = extract_clima("data/era5/era5_trinidad_2000.nc",
                         long_min = xmin(r),
                         long_max = xmax(r),
                         lat_min = ymin(r),
                         lat_max = ymax(r),
                         start_time = as.POSIXlt("2000-01-01 00:00:00", tz = "UTC"),
                         end_time = as.POSIXlt("2000-12-31 23:00:00", tz = "UTC"))
saveRDS(clim2000, "data/era5/rasters/trinidad_2000.rds")

clim2005 = extract_clima("data/era5/era5_trinidad_2005.nc",
                         long_min = xmin(r),
                         long_max = xmax(r),
                         lat_min = ymin(r),
                         lat_max = ymax(r),
                         start_time = as.POSIXlt("2005-01-01 00:00:00", tz = "UTC"),
                         end_time = as.POSIXlt("2005-12-31 23:00:00", tz = "UTC"))
saveRDS(clim2005, "data/era5/rasters/trinidad_2005.rds")

clim2010 = extract_clima("data/era5/era5_trinidad_2010.nc",
                         long_min = xmin(r),
                         long_max = xmax(r),
                         lat_min = ymin(r),
                         lat_max = ymax(r),
                         start_time = as.POSIXlt("2010-01-01 00:00:00", tz = "UTC"),
                         end_time = as.POSIXlt("2010-12-31 23:00:00", tz = "UTC"))
saveRDS(clim2010, "data/era5/rasters/trinidad_2010.rds")

clim2015 = extract_clima("data/era5/era5_trinidad_2015.nc",
                         long_min = xmin(r),
                         long_max = xmax(r),
                         lat_min = ymin(r),
                         lat_max = ymax(r),
                         start_time = as.POSIXlt("2015-01-01 00:00:00", tz = "UTC"),
                         end_time = as.POSIXlt("2015-12-31 23:00:00", tz = "UTC"))
saveRDS(clim2015, "data/era5/rasters/trinidad_2015.rds")





