# get chelsa data
library(numform)

files.tmax = read.delim('data/chelsa/tasmax_1980_2015.txt', header = F, sep = '\n')[,1]
files.tmax = str_trim(files.tmax)
files.tmax2015 = grep('2015', files.tmax, value = T)
files.tmax1980 = grep('1980', files.tmax, value = T)

files.tmin = read.delim('data/chelsa/tasmin_1980_2015.txt', header = F, sep = '\n')[,1]
files.tmin = str_trim(files.tmin)
files.tmin2015 = grep('2015', files.tmin, value = T)
files.tmin1980 = grep('1980', files.tmin, value = T)

"https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/monthly/tasmax/CHELSA_tasmax_12_2015_V.2.1.tif "

files.tmax = paste0("https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/monthly/tasmax/CHELSA_tasmax_", 
                    rep(f_pad_zero(1:12,2), each = 8), "_", seq(1980,2015,5), "_V.2.1.tif")

tasmaxnames = sapply(strsplit(files.tmax, '/'), '[[', 8)

getOption('timeout')
options(timeout = 900)
download.file(files.tmax, destfile = paste0('data/chelsa/chelsa_raw/tasmax/', tasmaxnames), method = 'libcurl', cacheOK = F, mode = 'wb')

download.file('https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/monthly/tasmax/CHELSA_tasmax_06_2005_V.2.1.tif',
              destfile = paste0('data/chelsa/chelsa_raw/tasmax/', "CHELSA_tasmax_06_2005_V.2.1.tif"),
              method = 'libcurl', cacheOK = F, mode = 'wb')

download.file('https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/monthly/tasmax/CHELSA_tasmax_02_1980_V.2.1.tif',
              destfile = paste0('data/chelsa/chelsa_raw/tasmax/', "CHELSA_tasmax_02_1980_V.2.1.tif"),
              method = 'libcurl', cacheOK = F, mode = 'wb')
