# library(terra)
# library(data.table)
#
# setwd('C:/Users/lsoif/OneDrive - University of Cambridge/Lydia Soifer/Trinidad_microclimates/')
# chm = rast('data/topography/chm_reproj.tif')
#
# setwd('C:/Users/lsoif/OneDrive - University of Cambridge/Lydia Soifer/climvocc_vis/')
# tmean05 = rast('climvocc_output/slope_correct_6way/05m_intervals/avg_daily_mean_temp/vocc_05m.tif')
# tmean10 = rast('climvocc_output/slope_correct_6way/05m_intervals/avg_daily_mean_temp/vocc_10m.tif')
# tmean15 = rast('climvocc_output/slope_correct_6way/05m_intervals/avg_daily_mean_temp/vocc_15m.tif')
# tmean20 = rast('climvocc_output/slope_correct_6way/05m_intervals/avg_daily_mean_temp/vocc_20m.tif')
# tmean25 = rast('climvocc_output/slope_correct_6way/05m_intervals/avg_daily_mean_temp/vocc_25m.tif')
# tmean30 = rast('climvocc_output/slope_correct_6way/05m_intervals/avg_daily_mean_temp/vocc_30m.tif')
# tmean35 = rast('climvocc_output/slope_correct_6way/05m_intervals/avg_daily_mean_temp/vocc_35m.tif')
#
# tmean05 = tmean05[500:504,1999:2001,,drop = F]
# tmean10 = tmean10[500:504,1999:2001,,drop = F]
# tmean15 = tmean15[500:504,1999:2001,,drop = F]
# tmean20 = tmean20[500:504,1999:2001,,drop = F]
# tmean25 = tmean25[500:504,1999:2001,,drop = F]
# tmean30 = tmean30[500:504,1999:2001,,drop = F]
# tmean35 = tmean35[500:504,1999:2001,,drop = F]
#
# tmean = list(tmean05, tmean10, tmean15, tmean20, tmean25, tmean30, tmean35)
#
# chm.crop = extend(chm, c(1,1))[500:504,1999:2001,,drop = F]

#' @param chm canopy height model
#' @param r1,r2,...,r6 list of spatRasters. Each element in list is a spatRaster representing a single height in the canopy from 5 to 35 m at 5 m increments
#' @param hgts vector of heights represented by list of spatRasters
#' @param var variable in spatRaster to height normalize. Must be the name of a layer in the spatRaster
#'
#' @description Normalizes data from different heights in the canopy to 4 quarters of the canopy vertical profile.
#'              Takes the mean where multiple values occur within one quartile in a single cell.
#' requires terra and data.table
relhgt_convert = function(chm, r1,r2,r3,r4,r5,r6,r7, var) {
  dt = data.table(cellnum = 1:ncell(r1[[var]]))
  dt[, chm := terra::values(chm)[cellnum]]

  # normalize heights by total canopy height
  dt[, relhgt5 := 5/chm]
  dt[, relhgt10 := 10/chm]
  dt[, relhgt15 := 15/chm]
  dt[, relhgt20 := 20/chm]
  dt[, relhgt25 := 25/chm]
  dt[, relhgt30 := 30/chm]
  dt[, relhgt35 := 35/chm]

  # extract values in corresponding quartile
  dt[, q1_5 := ifelse(relhgt5 <= 0.25, terra::values(r1[[var]])[cellnum], NA)]
  dt[, q1_10 := ifelse(relhgt10 <= 0.25, terra::values(r2[[var]])[cellnum], NA)]
  dt[, q1_15 := ifelse(relhgt15 <= 0.25, terra::values(r3[[var]])[cellnum], NA)]
  dt[, q1_20 := ifelse(relhgt20 <= 0.25, terra::values(r4[[var]])[cellnum], NA)]
  dt[, q1_25 := ifelse(relhgt25 <= 0.25, terra::values(r5[[var]])[cellnum], NA)]
  dt[, q1_30 := ifelse(relhgt30 <= 0.25, terra::values(r6[[var]])[cellnum], NA)]
  dt[, q1_35 := ifelse(relhgt35 <= 0.25, terra::values(r7[[var]])[cellnum], NA)]

  dt[, q2_5 := ifelse(relhgt5 > 0.25 & relhgt5 <= 0.5, terra::values(r1[[var]])[cellnum], NA)]
  dt[, q2_10 := ifelse(relhgt10 > 0.25 & relhgt10 <= 0.5, terra::values(r2[[var]])[cellnum], NA)]
  dt[, q2_15 := ifelse(relhgt15 > 0.25 & relhgt15 <= 0.5, terra::values(r3[[var]])[cellnum], NA)]
  dt[, q2_20 := ifelse(relhgt20 > 0.25 & relhgt20 <= 0.5, terra::values(r4[[var]])[cellnum], NA)]
  dt[, q2_25 := ifelse(relhgt25 > 0.25 & relhgt25 <= 0.5, terra::values(r5[[var]])[cellnum], NA)]
  dt[, q2_30 := ifelse(relhgt30 > 0.25 & relhgt30 <= 0.5, terra::values(r6[[var]])[cellnum], NA)]
  dt[, q2_35 := ifelse(relhgt35 > 0.25 & relhgt35 <= 0.5, terra::values(r7[[var]])[cellnum], NA)]

  dt[, q3_5 := ifelse(relhgt5 > 0.5 & relhgt5 <= 0.75, terra::values(r1[[var]])[cellnum], NA)]
  dt[, q3_10 := ifelse(relhgt10 > 0.5 & relhgt10 <= 0.75, terra::values(r2[[var]])[cellnum], NA)]
  dt[, q3_15 := ifelse(relhgt15 > 0.5 & relhgt15 <= 0.75, terra::values(r3[[var]])[cellnum], NA)]
  dt[, q3_20 := ifelse(relhgt20 > 0.5 & relhgt20 <= 0.75, terra::values(r4[[var]])[cellnum], NA)]
  dt[, q3_25 := ifelse(relhgt25 > 0.5 & relhgt25 <= 0.75, terra::values(r5[[var]])[cellnum], NA)]
  dt[, q3_30 := ifelse(relhgt30 > 0.5 & relhgt30 <= 0.75, terra::values(r6[[var]])[cellnum], NA)]
  dt[, q3_35 := ifelse(relhgt35 > 0.5 & relhgt35 <= 0.75, terra::values(r7[[var]])[cellnum], NA)]

  dt[, q4_5 := ifelse(relhgt5 > 0.75 & relhgt5 <= 1, terra::values(r1[[var]])[cellnum], NA)]
  dt[, q4_10 := ifelse(relhgt10 > 0.75 & relhgt10 <= 1, terra::values(r2[[var]])[cellnum], NA)]
  dt[, q4_15 := ifelse(relhgt15 > 0.75 & relhgt15 <= 1, terra::values(r3[[var]])[cellnum], NA)]
  dt[, q4_20 := ifelse(relhgt20 > 0.75 & relhgt20 <= 1, terra::values(r4[[var]])[cellnum], NA)]
  dt[, q4_25 := ifelse(relhgt25 > 0.75 & relhgt25 <= 1, terra::values(r5[[var]])[cellnum], NA)]
  dt[, q4_30 := ifelse(relhgt30 > 0.75 & relhgt30 <= 1, terra::values(r6[[var]])[cellnum], NA)]
  dt[, q4_35 := ifelse(relhgt35 > 0.75 & relhgt35 <= 1, terra::values(r7[[var]])[cellnum], NA)]

  # calculate mean for each cell in each quartile
  dt[, q1_val := apply(.SD, 1, mean, na.rm = T), .SDcols = 10:16]
  dt[, q2_val := apply(.SD, 1, mean, na.rm = T), .SDcols = 17:23]
  dt[, q3_val := apply(.SD, 1, mean, na.rm = T), .SDcols = 24:30]
  dt[, q4_val := apply(.SD, 1, mean, na.rm = T), .SDcols = 31:37]

  # convert data.table to raster
  q1 = q2 = q3 = q4 = terra::rast(chm)
  values(q1) = dt$q1_val
  values(q2) = dt$q2_val
  values(q3) = dt$q3_val
  values(q4) = dt$q4_val

  # make multilayer spatRaster to return
  vals = c(q1, q2, q3, q4)
  names(vals) = c('q1', 'q2', 'q3', 'q4')

  return(vals)
}


# generalizing relhgt_convert function so it can be used with any number of heights
#' @param chm canopy height model
#' @param r spatRaster; each layer represents a height in the canopy
#' @param hgts vector of heights represented by list of spatRasters
#' @param brks list between 0 and 1 of proportions up canopy height between which relative velocity is calculated
#'
#' @description Normalizes data from different heights in the canopy to 4 quarters of the canopy vertical profile.
#'              Takes the mean where multiple values occur within one quartile in a single cell.
#' requires terra and data.table
relhgt_convert2 = function(chm, r, hgts, brks) {

  dt = data.table(cellnum = 1:ncell(r[[1]]))
  dt[, chm := terra::values(chm)[cellnum]]

  cnames = paste0('relhgt', f_pad_zero(hgts, 2))

  # make list of column names for each division
  cnames.brks = list()
  for(b in 1:(length(brks)-1)) {
    cnames.brks[[b]] = paste0('bk', b, '_', f_pad_zero(hgts, 2))
  }
  
  #calculate relative canopy height
  for (i in 1:length(hgts)){
    dt[, cnames[i] := hgts[i]/chm]
  }
 
  # extract values in corresponding quartile/division for the lowest break (e.g., values that fall between 0 and 25% up the canopy (or other lowest break specified))
  for (h in 1:length(hgts)) {
    dt[, cnames.brks[[1]][h] := ifelse(.SD >= brks[1] & .SD <= brks[2], terra::values(r[[h]])[cellnum], NA), .SDcols = cnames[h]]
  } 

  for (b in 2:length(cnames.brks)) {
    for (h in 1:length(hgts)) {
      dt[, cnames.brks[[b]][h] := ifelse(.SD > brks[b] & .SD <= brks[b+1], terra::values(r[[h]])[cellnum], NA), .SDcols = cnames[h]]
    }
  }

  # make list of column names for each division means
  cnames.means = paste0('bkmean', f_pad_zero(2:length(brks)-1, 2))
  
  # calculate mean for each cell in each quartile

  for (b in 1:length(cnames.means)) {
    lower = 2+length(hgts)*b +1
    upper = lower + (length(hgts)-1)
    dt[, cnames.means[b] := apply(.SD, 1, mean, na.rm = T), .SDcols = lower:upper]
  }
  
  # convert data.table to raster
  rast.relhgt = list()
  for (i in cnames.means) {
    rast.relhgt[[i]] = terra::rast(chm)
    values(rast.relhgt[[i]]) = dt[[i]]
  }

  rast.relhgt = rast(rast.relhgt)

  return(rast.relhgt)
}

