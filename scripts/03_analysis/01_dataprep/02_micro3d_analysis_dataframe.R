# Make dataframe for 3D microclimate velocity including all points that fall between 0.5 and 1 relative height in the canopy

library(terra)
library(tidyverse)
library(numform)
library(data.table)

# vocc
# micro.3d = list.files(path = "scripts/02_climate_velocity/output/3D/avg_daily_maxTemp/", pattern = "vocc", full.names = T)
micro.3d = list.files(path = "scripts/02_climate_velocity/output/3D/temp_bio5/", pattern = "vocc", full.names = T)
micro.3d = grep(".tif", micro.3d, value = T)
micro.3d = lapply(micro.3d, rast)

vocc.3d = lapply(micro.3d, "[[", 1)
vocc.3d = rast(vocc.3d)
names(vocc.3d) = paste0(f_pad_zero(seq(5,35,5), 2))
vocc.3d = abs(vocc.3d)

EWdir.3d = lapply(micro.3d, "[[", 6)
EWdir.3d = rast(EWdir.3d)
names(EWdir.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

NSdir.3d= lapply(micro.3d, "[[", 7)
NSdir.3d = rast(NSdir.3d)
names(NSdir.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

zAng.3d = lapply(micro.3d, "[[", 10)
zAng.3d = rast(zAng.3d)
names(zAng.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

xyAng.3d = lapply(micro.3d, "[[", 9)
xyAng.3d = rast(xyAng.3d)
names(xyAng.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

vocc.3d = as.data.frame(vocc.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "vocc") %>% 
  drop_na()

EWdir.3d = as.data.frame(EWdir.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "EWdir") %>% 
  drop_na()

NSdir.3d = as.data.frame(NSdir.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "NSdir") %>% 
  drop_na()

zAng.3d = as.data.frame(zAng.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "zAng") %>% 
  drop_na()

xyAng.3d = as.data.frame(xyAng.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "xyAng") %>% 
  drop_na()

# spatgrad.3d
# spatgrad.3d = list.files(path = "scripts/02_climate_velocity/output/3D/avg_daily_maxTemp/", pattern = "spat", full.names = T)
spatgrad.3d = list.files(path = "scripts/02_climate_velocity/output/3D/temp_bio5/", pattern = "spat", full.names = T)
spatgrad.3d = lapply(spatgrad.3d, rast)
spatgrad.3d = lapply(spatgrad.3d, "[[", 1)
spatgrad.3d = rast(spatgrad.3d)
names(spatgrad.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

spatgrad.3d = as.data.frame(spatgrad.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "spatgrad") %>% 
  drop_na()

# tempgrad.3d
# tempgrad.3d = list.files(path = "scripts/02_climate_velocity/output/3D/avg_daily_maxTemp/", pattern = "temp", full.names = T)
tempgrad.3d = list.files(path = "scripts/02_climate_velocity/output/3D/temp_bio5/", pattern = "temp", full.names = T)
tempgrad.3d = lapply(tempgrad.3d, rast)
tempgrad.3d = rast(tempgrad.3d)
names(tempgrad.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

tempgrad.3d = as.data.frame(tempgrad.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "tempgrad") %>% 
  drop_na()

micro3d.df = inner_join(vocc.3d, spatgrad.3d, by = c("x", "y", "height")) %>% 
  inner_join(tempgrad.3d, by = c("x", "y", "height")) %>% 
  inner_join(xyAng.3d, by = c("x", "y", "height")) %>% 
  inner_join(zAng.3d, by = c("x", "y", "height")) %>% 
  inner_join(EWdir.3d, by = c("x", "y", "height")) %>% 
  inner_join(NSdir.3d, by = c("x", "y", "height"))

# add canopy height
chm = rast("data/topography/chm_reproj.tif")
names(chm) = "chm"
chm = as.data.frame(chm, xy = T)

micro3d.df = micro3d.df %>% inner_join(chm, by = c("x", "y"))

# calculate relative height and filter to in the top half of the canopy as measured from ground to top of canopy
micro3d.df = micro3d.df %>% 
  mutate(height = as.numeric(height),
         relhgt = height/chm) %>% 
  filter(relhgt <= 1 & relhgt >= 0.5)

# add aspect and elevation
aspect = rast("data/topography/apr.tif")
names(aspect) = "aspect"
aspect = as.data.frame(aspect, xy = T)

micro3d.df = micro3d.df %>% 
  left_join(aspect, by = c("x", "y"))

elev = rast("data/topography/dem_reproj.tif")
names(elev) = "elev"
elev = as.data.frame(elev, xy = T)

micro3d.df = micro3d.df %>% 
  left_join(elev, by = c("x", "y"))

# this dataframe contains data for all velocity vectors that fall within relhgt between 0.5 and 1
# write.csv(micro3d.df, "data/dataframes/micro3d_canopy_dataframe_tmax_20m.csv")
write.csv(micro3d.df, "data/dataframes/micro3d_canopy_dataframe_tempbio5_20m.csv")


# add temp to data frame --------------------------------------------------

# micro3d.df = fread("data/dataframes/micro3d_canopy_dataframe_tmax_20m.csv")
micro3d.df = fread("data/dataframes/micro3d_canopy_dataframe_tempbio5_20m.csv")
# maxtemp.pres.micro = list.files(path = "data/microclim_3D/mosaics/", pattern = "pres", full.names = T)
maxtemp.pres.micro = list.files(path = "data/microclim_3D/mosaics/mosaics_temp_bio5/temp_bio5_20m/pres/", pattern = "pres", full.names = T)
maxtemp.pres.micro = lapply(maxtemp.pres.micro, rast)
names(maxtemp.pres.micro) = paste0(f_pad_zero(c(2, seq(5,40,5)), 2))
maxtemp.pres.micro = rast(maxtemp.pres.micro)
maxtemp.pres.micro = as.data.frame(maxtemp.pres.micro, xy = T) %>% 
  pivot_longer(3:11, names_to = "height", values_to = "max_temp_pres") %>% 
  drop_na() %>% 
  mutate(height = as.numeric(height))

micro3d.df = micro3d.df %>% 
  left_join(maxtemp.pres.micro, by = c("x", "y", "height"))

micro3d.df$var = "maxtemp"
micro3d.df$resolution = "20m"

# fwrite(micro3d.df, "data/dataframes/micro3d_canopy_dataframe_tmax_20m.csv", row.names = F)
fwrite(micro3d.df, "data/dataframes/micro3d_canopy_dataframe_tempbio5_20m.csv", row.names = F)



# MAX TEMP AGGREGATED TO 100m ---------------------------------------------

# vocc
#micro.3d = list.files(path = "scripts/02_climate_velocity/output/3D/avg_daily_maxTemp/aggregated_100m/", pattern = "vocc", full.names = T)
micro.3d = list.files(path = "scripts/02_climate_velocity/output/3D/temp_bio5/aggregated_100m/", pattern = "vocc", full.names = T)
micro.3d = grep(".tif", micro.3d, value = T)
micro.3d = lapply(micro.3d, rast)

vocc.3d = lapply(micro.3d, "[[", 1)
vocc.3d = rast(vocc.3d)
names(vocc.3d) = paste0(f_pad_zero(seq(5,35,5), 2))
vocc.3d = abs(vocc.3d)

EWdir.3d = lapply(micro.3d, "[[", 6)
EWdir.3d = rast(EWdir.3d)
names(EWdir.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

NSdir.3d= lapply(micro.3d, "[[", 7)
NSdir.3d = rast(NSdir.3d)
names(NSdir.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

zAng.3d = lapply(micro.3d, "[[", 10)
zAng.3d = rast(zAng.3d)
names(zAng.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

xyAng.3d = lapply(micro.3d, "[[", 9)
xyAng.3d = rast(xyAng.3d)
names(xyAng.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

vocc.3d = as.data.frame(vocc.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "vocc") %>% 
  drop_na()

EWdir.3d = as.data.frame(EWdir.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "EWdir") %>% 
  drop_na()

NSdir.3d = as.data.frame(NSdir.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "NSdir") %>% 
  drop_na()

zAng.3d = as.data.frame(zAng.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "zAng") %>% 
  drop_na()

xyAng.3d = as.data.frame(xyAng.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "xyAng") %>% 
  drop_na()

# spatgrad.3d
#spatgrad.3d = list.files(path = "scripts/02_climate_velocity/output/3D/avg_daily_maxTemp/aggregated_100m/", pattern = "spat", full.names = T)
spatgrad.3d = list.files(path = "scripts/02_climate_velocity/output/3D/temp_bio5/aggregated_100m/", pattern = "spat", full.names = T)
spatgrad.3d = lapply(spatgrad.3d, rast)
spatgrad.3d = lapply(spatgrad.3d, "[[", 1)
spatgrad.3d = rast(spatgrad.3d)
names(spatgrad.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

spatgrad.3d = as.data.frame(spatgrad.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "spatgrad") %>% 
  drop_na()

# tempgrad.3d
# tempgrad.3d = list.files(path = "scripts/02_climate_velocity/output/3D/avg_daily_maxTemp/aggregated_100m/", pattern = "temp", full.names = T)
tempgrad.3d = list.files(path = "scripts/02_climate_velocity/output/3D/temp_bio5/aggregated_100m/", pattern = "temp", full.names = T)
tempgrad.3d = lapply(tempgrad.3d, rast)
tempgrad.3d = rast(tempgrad.3d)
names(tempgrad.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

tempgrad.3d = as.data.frame(tempgrad.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "tempgrad") %>% 
  drop_na()

micro3d.df = inner_join(vocc.3d, spatgrad.3d, by = c("x", "y", "height")) %>% 
  inner_join(tempgrad.3d, by = c("x", "y", "height")) %>% 
  inner_join(xyAng.3d, by = c("x", "y", "height")) %>% 
  inner_join(zAng.3d, by = c("x", "y", "height")) %>% 
  inner_join(EWdir.3d, by = c("x", "y", "height")) %>% 
  inner_join(NSdir.3d, by = c("x", "y", "height"))

# add canopy height
chm = rast("data/topography/chm_reproj.tif")
names(chm) = "chm"
chm = project(chm, micro.3d[[1]])
chm = as.data.frame(chm, xy = T)

micro3d.df = micro3d.df %>% inner_join(chm, by = c("x", "y"))

# calculate relative height and filter to in the top half of the canopy as measured from ground to top of canopy
micro3d.df = micro3d.df %>% 
  mutate(height = as.numeric(height),
         relhgt = height/chm) %>% 
  filter(relhgt <= 1 & relhgt >= 0.5)

# add aspect and elevation
elev = rast("data/topography/dem_reproj.tif")
names(elev) = "elev"
elev = resample(elev, micro.3d[[1]])

aspect = terrain(elev, "aspect", unit = "degrees")

elev = as.data.frame(elev, xy = T)
aspect = as.data.frame(aspect, xy = T)

micro3d.df = micro3d.df %>% 
  left_join(elev, by = c("x", "y"))

micro3d.df = micro3d.df %>% 
  left_join(aspect, by = c("x", "y"))

micro3d.df$var = "maxtemp"
micro3d.df$resolution = "100m"

# this dataframe contains data for all velocity vectors that fall within relhgt between 0.5 and 1
# fwrite(micro3d.df, "data/dataframes/micro3d_canopy_dataframe_tmax_100m.csv", row.names = F)
fwrite(micro3d.df, "data/dataframes/micro3d_canopy_dataframe_tempbio5_100m.csv", row.names = F)


# add temp to data frame --------------------------------------------------

# micro3d.df = fread("scripts/03_analysis/00_dataframes/micro3d_canopy_dataframe_tmax_100m.csv")
micro3d.df = fread("data/dataframes/micro3d_canopy_dataframe_tempbio5_100m.csv")
# maxtemp.pres.micro = list.files(path = "data/microclim_3D/mosaics/aggregated_100m/", pattern = "pres", full.names = T)
maxtemp.pres.micro = list.files(path = "data/microclim_3D/mosaics/mosaics_temp_bio5/aggregated_100m/pres/", pattern = "pres", full.names = T)
maxtemp.pres.micro = lapply(maxtemp.pres.micro, rast)
names(maxtemp.pres.micro) = paste0(f_pad_zero(c(2, seq(5,40,5)), 2))
maxtemp.pres.micro = rast(maxtemp.pres.micro)
maxtemp.pres.micro = as.data.frame(maxtemp.pres.micro, xy = T) %>% 
  pivot_longer(3:11, names_to = "height", values_to = "max_temp_pres") %>% 
  drop_na() %>% 
  mutate(height = as.numeric(height))

micro3d.df = micro3d.df %>% 
  left_join(maxtemp.pres.micro, by = c("x", "y", "height"))

# fwrite(micro3d.df, "data/dataframes/micro3d_canopy_dataframe_tmax_100m.csv", row.names = F)
fwrite(micro3d.df, "data/dataframes/micro3d_canopy_dataframe_tempbio5_100m.csv", row.names = F)



# MAX TEMP AGGREGATED 1km -------------------------------------------------

# vocc
# micro.3d = list.files(path = "scripts/02_climate_velocity/output/3D/avg_daily_maxTemp/aggregated_1km", pattern = "vocc", full.names = T)
micro.3d = list.files(path = "scripts/02_climate_velocity/output/3D/temp_bio5/aggregated_1km", pattern = "vocc", full.names = T)
micro.3d = grep(".tif", micro.3d, value = T)
micro.3d = lapply(micro.3d, rast)

vocc.3d = lapply(micro.3d, "[[", 1)
vocc.3d = rast(vocc.3d)
names(vocc.3d) = paste0(f_pad_zero(seq(5,35,5), 2))
vocc.3d = abs(vocc.3d)

EWdir.3d = lapply(micro.3d, "[[", 6)
EWdir.3d = rast(EWdir.3d)
names(EWdir.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

NSdir.3d= lapply(micro.3d, "[[", 7)
NSdir.3d = rast(NSdir.3d)
names(NSdir.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

zAng.3d = lapply(micro.3d, "[[", 10)
zAng.3d = rast(zAng.3d)
names(zAng.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

xyAng.3d = lapply(micro.3d, "[[", 9)
xyAng.3d = rast(xyAng.3d)
names(xyAng.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

vocc.3d = as.data.frame(vocc.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "vocc") %>% 
  drop_na()

EWdir.3d = as.data.frame(EWdir.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "EWdir") %>% 
  drop_na()

NSdir.3d = as.data.frame(NSdir.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "NSdir") %>% 
  drop_na()

zAng.3d = as.data.frame(zAng.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "zAng") %>% 
  drop_na()

xyAng.3d = as.data.frame(xyAng.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "xyAng") %>% 
  drop_na()

# spatgrad.3d
# spatgrad.3d = list.files(path = "scripts/02_climate_velocity/output/3D/avg_daily_maxTemp/aggregated_1km/", pattern = "spat", full.names = T)
spatgrad.3d = list.files(path = "scripts/02_climate_velocity/output/3D/temp_bio5/aggregated_1km/", pattern = "spat", full.names = T)
spatgrad.3d = lapply(spatgrad.3d, rast)
spatgrad.3d = lapply(spatgrad.3d, "[[", 1)
spatgrad.3d = rast(spatgrad.3d)
names(spatgrad.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

spatgrad.3d = as.data.frame(spatgrad.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "spatgrad") %>% 
  drop_na()

# tempgrad.3d
# tempgrad.3d = list.files(path = "scripts/02_climate_velocity/output/3D/avg_daily_maxTemp/aggregated_1km/", pattern = "temp", full.names = T)
tempgrad.3d = list.files(path = "scripts/02_climate_velocity/output/3D/temp_bio5/aggregated_1km/", pattern = "temp", full.names = T)
tempgrad.3d = lapply(tempgrad.3d, rast)
tempgrad.3d = rast(tempgrad.3d)
names(tempgrad.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

tempgrad.3d = as.data.frame(tempgrad.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "tempgrad") %>% 
  drop_na()

micro3d.df = inner_join(vocc.3d, spatgrad.3d, by = c("x", "y", "height")) %>% 
  inner_join(tempgrad.3d, by = c("x", "y", "height")) %>% 
  inner_join(xyAng.3d, by = c("x", "y", "height")) %>% 
  inner_join(zAng.3d, by = c("x", "y", "height")) %>% 
  inner_join(EWdir.3d, by = c("x", "y", "height")) %>% 
  inner_join(NSdir.3d, by = c("x", "y", "height"))

# add canopy height
chm = rast("data/topography/chm_reproj.tif")
names(chm) = "chm"
chm = project(chm, micro.3d[[1]])
chm = as.data.frame(chm, xy = T)

micro3d.df = micro3d.df %>% inner_join(chm, by = c("x", "y"))

# calculate relative height and filter to in the top half of the canopy as measured from ground to top of canopy
micro3d.df = micro3d.df %>% 
  mutate(height = as.numeric(height),
         relhgt = height/chm) %>% 
  filter(relhgt <= 1 & relhgt >= 0.5)

elev = rast("data/topography/dem_reproj.tif")
names(elev) = "elev"
elev = resample(elev, micro.3d[[1]])

aspect = terrain(elev, "aspect", unit = "degrees")

elev = as.data.frame(elev, xy = T)
aspect = as.data.frame(aspect, xy = T)

micro3d.df = micro3d.df %>% 
  left_join(elev, by = c("x", "y"))

micro3d.df = micro3d.df %>% 
  left_join(aspect, by = c("x", "y"))

micro3d.df$var = "maxtemp"
micro3d.df$resolution = "1km"

# this dataframe contains data for all velocity vectors that fall within relhgt between 0.5 and 1
# fwrite(micro3d.df, "data/dataframes/micro3d_canopy_dataframe_tmax_1km.csv", row.names = F)
fwrite(micro3d.df, "data/dataframes/micro3d_canopy_dataframe_tempbio5_1km.csv", row.names = F)


# add temp to data frame --------------------------------------------------

# micro3d.df = fread("scripts/03_analysis/00_dataframes/micro3d_canopy_dataframe_tmax_1km.csv")
micro3d.df = fread("data/dataframes/micro3d_canopy_dataframe_tempbio5_1km.csv")
# maxtemp.pres.micro = list.files(path = "data/microclim_3D/mosaics/aggregated_1km/", pattern = "pres", full.names = T)
maxtemp.pres.micro = list.files(path = "data/microclim_3D/mosaics/mosaics_temp_bio5/aggregated_1km/pres/", pattern = "pres", full.names = T)
maxtemp.pres.micro = lapply(maxtemp.pres.micro, rast)
names(maxtemp.pres.micro) = paste0(f_pad_zero(c(2, seq(5,40,5)), 2))
maxtemp.pres.micro = rast(maxtemp.pres.micro)
maxtemp.pres.micro = as.data.frame(maxtemp.pres.micro, xy = T) %>% 
  pivot_longer(3:11, names_to = "height", values_to = "max_temp_pres") %>% 
  drop_na() %>% 
  mutate(height = as.numeric(height))

micro3d.df = micro3d.df %>% 
  left_join(maxtemp.pres.micro, by = c("x", "y", "height"))

# fwrite(micro3d.df, "data/dataframes/micro3d_canopy_dataframe_tmax_1km.csv", row.names = F)
fwrite(micro3d.df, "data/dataframes/micro3d_canopy_dataframe_tempbio5_1km.csv", row.names = F)


# MIN TEMP COLDEST MONTH (BIO6) -------------------------------------------

# vocc
micro.3d = list.files(path = "scripts/02_climate_velocity/output/3D/temp_bio6/", pattern = "vocc", full.names = T)
micro.3d = grep(".tif", micro.3d, value = T)
micro.3d = lapply(micro.3d, rast)

vocc.3d = lapply(micro.3d, "[[", 1)
vocc.3d = rast(vocc.3d)
names(vocc.3d) = paste0(f_pad_zero(seq(5,35,5), 2))
vocc.3d = abs(vocc.3d)

EWdir.3d = lapply(micro.3d, "[[", 6)
EWdir.3d = rast(EWdir.3d)
names(EWdir.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

NSdir.3d= lapply(micro.3d, "[[", 7)
NSdir.3d = rast(NSdir.3d)
names(NSdir.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

zAng.3d = lapply(micro.3d, "[[", 10)
zAng.3d = rast(zAng.3d)
names(zAng.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

xyAng.3d = lapply(micro.3d, "[[", 9)
xyAng.3d = rast(xyAng.3d)
names(xyAng.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

vocc.3d = as.data.frame(vocc.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "vocc") %>% 
  drop_na()

EWdir.3d = as.data.frame(EWdir.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "EWdir") %>% 
  drop_na()

NSdir.3d = as.data.frame(NSdir.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "NSdir") %>% 
  drop_na()

zAng.3d = as.data.frame(zAng.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "zAng") %>% 
  drop_na()

xyAng.3d = as.data.frame(xyAng.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "xyAng") %>% 
  drop_na()

# spatgrad.3d
spatgrad.3d = list.files(path = "scripts/02_climate_velocity/output/3D/temp_bio6/", pattern = "spat", full.names = T)
spatgrad.3d = lapply(spatgrad.3d, rast)
spatgrad.3d = lapply(spatgrad.3d, "[[", 1)
spatgrad.3d = rast(spatgrad.3d)
names(spatgrad.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

spatgrad.3d = as.data.frame(spatgrad.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "spatgrad") %>% 
  drop_na()

# tempgrad.3d
tempgrad.3d = list.files(path = "scripts/02_climate_velocity/output/3D/temp_bio6/", pattern = "temp", full.names = T)
tempgrad.3d = lapply(tempgrad.3d, rast)
tempgrad.3d = rast(tempgrad.3d)
names(tempgrad.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

tempgrad.3d = as.data.frame(tempgrad.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "tempgrad") %>% 
  drop_na()

micro3d.df = inner_join(vocc.3d, spatgrad.3d, by = c("x", "y", "height")) %>% 
  inner_join(tempgrad.3d, by = c("x", "y", "height")) %>% 
  inner_join(xyAng.3d, by = c("x", "y", "height")) %>% 
  inner_join(zAng.3d, by = c("x", "y", "height")) %>% 
  inner_join(EWdir.3d, by = c("x", "y", "height")) %>% 
  inner_join(NSdir.3d, by = c("x", "y", "height"))

# add canopy height
chm = rast("data/topography/chm_reproj.tif")
names(chm) = "chm"
chm = as.data.frame(chm, xy = T)

micro3d.df = micro3d.df %>% inner_join(chm, by = c("x", "y"))

# calculate relative height and filter to in the top half of the canopy as measured from ground to top of canopy
micro3d.df = micro3d.df %>% 
  mutate(height = as.numeric(height),
         relhgt = height/chm) %>% 
  filter(relhgt <= 1 & relhgt >= 0.5)

# add aspect and elevation
aspect = rast("data/topography/apr.tif")
names(aspect) = "aspect"
aspect = as.data.frame(aspect, xy = T)

micro3d.df = micro3d.df %>% 
  left_join(aspect, by = c("x", "y"))

elev = rast("data/topography/dem_reproj.tif")
names(elev) = "elev"
elev = as.data.frame(elev, xy = T)

micro3d.df = micro3d.df %>% 
  left_join(elev, by = c("x", "y"))

micro3d.df$var = "temp_bio6"
micro3d.df$resolution = "20m"

# this dataframe contains data for all velocity vectors that fall within relhgt between 0.5 and 1
fwrite(micro3d.df, "data/dataframes/micro3d_canopy_dataframe_tmin_20m.csv", row.names = F)


# add temp to data frame --------------------------------------------------

micro3d.df = fread("scripts/03_analysis/00_dataframes/micro3d_canopy_dataframe_tmin_20m.csv")
mintemp.pres.micro = list.files(path = "data/microclim_3D/mosaics_temp_bio6/pres", pattern = "pres", full.names = T)
mintemp.pres.micro = lapply(mintemp.pres.micro, rast)
names(mintemp.pres.micro) = paste0(f_pad_zero(c(2, seq(5,40,5)), 2))
mintemp.pres.micro = rast(mintemp.pres.micro)
mintemp.pres.micro = as.data.frame(mintemp.pres.micro, xy = T) %>% 
  pivot_longer(3:11, names_to = "height", values_to = "min_temp_pres") %>% 
  drop_na() %>% 
  mutate(height = as.numeric(height))

micro3d.df = micro3d.df %>% 
  left_join(mintemp.pres.micro, by = c("x", "y", "height"))

fwrite(micro3d.df, "data/dataframes/micro3d_canopy_dataframe_tmin_20m.csv", row.names = F)


# MIN TEMP AGGREGATED TO 100m ---------------------------------------------

# vocc
micro.3d = list.files(path = "scripts/02_climate_velocity/output/3D/temp_bio6/aggregated_100m/", pattern = "vocc", full.names = T)
micro.3d = grep(".tif", micro.3d, value = T)
micro.3d = lapply(micro.3d, rast)

vocc.3d = lapply(micro.3d, "[[", 1)
vocc.3d = rast(vocc.3d)
names(vocc.3d) = paste0(f_pad_zero(seq(5,35,5), 2))
vocc.3d = abs(vocc.3d)

EWdir.3d = lapply(micro.3d, "[[", 6)
EWdir.3d = rast(EWdir.3d)
names(EWdir.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

NSdir.3d= lapply(micro.3d, "[[", 7)
NSdir.3d = rast(NSdir.3d)
names(NSdir.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

zAng.3d = lapply(micro.3d, "[[", 10)
zAng.3d = rast(zAng.3d)
names(zAng.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

xyAng.3d = lapply(micro.3d, "[[", 9)
xyAng.3d = rast(xyAng.3d)
names(xyAng.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

vocc.3d = as.data.frame(vocc.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "vocc") %>% 
  drop_na()

EWdir.3d = as.data.frame(EWdir.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "EWdir") %>% 
  drop_na()

NSdir.3d = as.data.frame(NSdir.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "NSdir") %>% 
  drop_na()

zAng.3d = as.data.frame(zAng.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "zAng") %>% 
  drop_na()

xyAng.3d = as.data.frame(xyAng.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "xyAng") %>% 
  drop_na()

# spatgrad.3d
spatgrad.3d = list.files(path = "scripts/02_climate_velocity/output/3D/temp_bio6/aggregated_100m/", pattern = "spat", full.names = T)
spatgrad.3d = lapply(spatgrad.3d, rast)
spatgrad.3d = lapply(spatgrad.3d, "[[", 1)
spatgrad.3d = rast(spatgrad.3d)
names(spatgrad.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

spatgrad.3d = as.data.frame(spatgrad.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "spatgrad") %>% 
  drop_na()

# tempgrad.3d
tempgrad.3d = list.files(path = "scripts/02_climate_velocity/output/3D/temp_bio6/aggregated_100m/", pattern = "temp", full.names = T)
tempgrad.3d = lapply(tempgrad.3d, rast)
tempgrad.3d = rast(tempgrad.3d)
names(tempgrad.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

tempgrad.3d = as.data.frame(tempgrad.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "tempgrad") %>% 
  drop_na()

micro3d.df = inner_join(vocc.3d, spatgrad.3d, by = c("x", "y", "height")) %>% 
  inner_join(tempgrad.3d, by = c("x", "y", "height")) %>% 
  inner_join(xyAng.3d, by = c("x", "y", "height")) %>% 
  inner_join(zAng.3d, by = c("x", "y", "height")) %>% 
  inner_join(EWdir.3d, by = c("x", "y", "height")) %>% 
  inner_join(NSdir.3d, by = c("x", "y", "height"))

# add canopy height
chm = rast("data/topography/chm_reproj.tif")
names(chm) = "chm"
chm = project(chm, micro.3d[[1]])
chm = as.data.frame(chm, xy = T)

micro3d.df = micro3d.df %>% inner_join(chm, by = c("x", "y"))

# calculate relative height and filter to in the top half of the canopy as measured from ground to top of canopy
micro3d.df = micro3d.df %>% 
  mutate(height = as.numeric(height),
         relhgt = height/chm) %>% 
  filter(relhgt <= 1 & relhgt >= 0.5)

# add aspect and elevation
elev = rast("data/topography/dem_reproj.tif")
names(elev) = "elev"
elev = resample(elev, micro.3d[[1]])

aspect = terrain(elev, "aspect", unit = "degrees")

elev = as.data.frame(elev, xy = T)
aspect = as.data.frame(aspect, xy = T)

micro3d.df = micro3d.df %>% 
  left_join(elev, by = c("x", "y"))

micro3d.df = micro3d.df %>% 
  left_join(aspect, by = c("x", "y"))

micro3d.df$var = "temp_bio6"
micro3d.df$resolution = "100m"

# this dataframe contains data for all velocity vectors that fall within relhgt between 0.5 and 1
fwrite(micro3d.df, "data/dataframes/micro3d_canopy_dataframe_tmin_100m.csv", row.names = F)


# add temp to data frame --------------------------------------------------

micro3d.df = fread("scripts/03_analysis/00_dataframes/micro3d_canopy_dataframe_tmin_100m.csv")
mintemp.pres.micro = list.files(path = "data/microclim_3D/mosaics_temp_bio6/pres/aggregated_100m/", pattern = "pres", full.names = T)
mintemp.pres.micro = lapply(mintemp.pres.micro, rast)
names(mintemp.pres.micro) = paste0(f_pad_zero(c(2, seq(5,40,5)), 2))
mintemp.pres.micro = rast(mintemp.pres.micro)
mintemp.pres.micro = as.data.frame(mintemp.pres.micro, xy = T) %>% 
  pivot_longer(3:11, names_to = "height", values_to = "min_temp_pres") %>% 
  drop_na() %>% 
  mutate(height = as.numeric(height))

micro3d.df = micro3d.df %>% 
  left_join(mintemp.pres.micro, by = c("x", "y", "height"))

fwrite(micro3d.df, "data/dataframes/micro3d_canopy_dataframe_tmin_100m.csv", row.names = F)



# MIN TEMP AGGREGATED 1km -------------------------------------------------

# vocc
micro.3d = list.files(path = "scripts/02_climate_velocity/output/3D/temp_bio6/aggregated_1km", pattern = "vocc", full.names = T)
micro.3d = grep(".tif", micro.3d, value = T)
micro.3d = lapply(micro.3d, rast)

vocc.3d = lapply(micro.3d, "[[", 1)
vocc.3d = rast(vocc.3d)
names(vocc.3d) = paste0(f_pad_zero(seq(5,35,5), 2))
vocc.3d = abs(vocc.3d)

EWdir.3d = lapply(micro.3d, "[[", 6)
EWdir.3d = rast(EWdir.3d)
names(EWdir.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

NSdir.3d= lapply(micro.3d, "[[", 7)
NSdir.3d = rast(NSdir.3d)
names(NSdir.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

zAng.3d = lapply(micro.3d, "[[", 10)
zAng.3d = rast(zAng.3d)
names(zAng.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

xyAng.3d = lapply(micro.3d, "[[", 9)
xyAng.3d = rast(xyAng.3d)
names(xyAng.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

vocc.3d = as.data.frame(vocc.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "vocc") %>% 
  drop_na()

EWdir.3d = as.data.frame(EWdir.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "EWdir") %>% 
  drop_na()

NSdir.3d = as.data.frame(NSdir.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "NSdir") %>% 
  drop_na()

zAng.3d = as.data.frame(zAng.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "zAng") %>% 
  drop_na()

xyAng.3d = as.data.frame(xyAng.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "xyAng") %>% 
  drop_na()

# spatgrad.3d
spatgrad.3d = list.files(path = "scripts/02_climate_velocity/output/3D/temp_bio6/aggregated_1km/", pattern = "spat", full.names = T)
spatgrad.3d = lapply(spatgrad.3d, rast)
spatgrad.3d = lapply(spatgrad.3d, "[[", 1)
spatgrad.3d = rast(spatgrad.3d)
names(spatgrad.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

spatgrad.3d = as.data.frame(spatgrad.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "spatgrad") %>% 
  drop_na()

# tempgrad.3d
tempgrad.3d = list.files(path = "scripts/02_climate_velocity/output/3D/temp_bio6/aggregated_1km/", pattern = "temp", full.names = T)
tempgrad.3d = lapply(tempgrad.3d, rast)
tempgrad.3d = rast(tempgrad.3d)
names(tempgrad.3d) = paste0(f_pad_zero(seq(5,35,5), 2))

tempgrad.3d = as.data.frame(tempgrad.3d, xy = T) %>% 
  pivot_longer(cols = 3:9, names_to = "height", values_to = "tempgrad") %>% 
  drop_na()

micro3d.df = inner_join(vocc.3d, spatgrad.3d, by = c("x", "y", "height")) %>% 
  inner_join(tempgrad.3d, by = c("x", "y", "height")) %>% 
  inner_join(xyAng.3d, by = c("x", "y", "height")) %>% 
  inner_join(zAng.3d, by = c("x", "y", "height")) %>% 
  inner_join(EWdir.3d, by = c("x", "y", "height")) %>% 
  inner_join(NSdir.3d, by = c("x", "y", "height"))

# add canopy height
chm = rast("data/topography/chm_reproj.tif")
names(chm) = "chm"
chm = project(chm, micro.3d[[1]])
chm = as.data.frame(chm, xy = T)

micro3d.df = micro3d.df %>% inner_join(chm, by = c("x", "y"))

# calculate relative height and filter to in the top half of the canopy as measured from ground to top of canopy
micro3d.df = micro3d.df %>% 
  mutate(height = as.numeric(height),
         relhgt = height/chm) %>% 
  filter(relhgt <= 1 & relhgt >= 0.5)

elev = rast("data/topography/dem_reproj.tif")
names(elev) = "elev"
elev = resample(elev, micro.3d[[1]])

aspect = terrain(elev, "aspect", unit = "degrees")

elev = as.data.frame(elev, xy = T)
aspect = as.data.frame(aspect, xy = T)

micro3d.df = micro3d.df %>% 
  left_join(elev, by = c("x", "y"))

micro3d.df = micro3d.df %>% 
  left_join(aspect, by = c("x", "y"))

micro3d.df$var = "temp_bio6"
micro3d.df$resolution = "1km"

# this dataframe contains data for all velocity vectors that fall within relhgt between 0.5 and 1
fwrite(micro3d.df, "data/dataframes/micro3d_canopy_dataframe_tmin_1km.csv", row.names = F)


# add temp to data frame --------------------------------------------------

micro3d.df = fread("scripts/03_analysis/00_dataframes/micro3d_canopy_dataframe_tmin_1km.csv")
mintemp.pres.micro = list.files(path = "data/microclim_3D/mosaics_temp_bio6/pres/aggregated_1km/", pattern = "pres", full.names = T)
mintemp.pres.micro = lapply(mintemp.pres.micro, rast)
names(mintemp.pres.micro) = paste0(f_pad_zero(c(2, seq(5,40,5)), 2))
mintemp.pres.micro = rast(mintemp.pres.micro)
mintemp.pres.micro = as.data.frame(mintemp.pres.micro, xy = T) %>% 
  pivot_longer(3:11, names_to = "height", values_to = "min_temp_pres") %>% 
  drop_na() %>% 
  mutate(height = as.numeric(height))

micro3d.df = micro3d.df %>% 
  left_join(mintemp.pres.micro, by = c("x", "y", "height"))

fwrite(micro3d.df, "data/dataframes/micro3d_canopy_dataframe_tmin_1km.csv", row.names = F)





