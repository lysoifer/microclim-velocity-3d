angulo <- function(dx, dy){
  d <- cbind(dx, dy)
  angline <- function(rw){
    angle <- ifelse(rw[2] < 0, 180 + CircStats::deg(atan(rw[1]/rw[2])),
                    ifelse(rw[1] < 0, 360 + CircStats::deg(atan(rw[1]/rw[2])), CircStats::deg(atan(rw[1]/rw[2]))))
    return(angle)
  }
  return(apply(d, 1, angline))
}

angFromNorth = function(dx, dy) {
  if (is.na(dx) | is.na(dy) | is.nan(as.numeric(dx)) | is.nan(as.numeric(dy))) {a=NA} else {
    if (dy==0 & dx>0) {a=90} # directly east = 90
    if (dy==0 & dx<0) {a=270} # directly west = 270
    if (dx==0 & dy>0) {a=0} # directly north = 0
    if (dx==0 & dy<0) {a=180} # directly south = 180
    if (dx==0 & dy==0) {a=Inf} # no movement in xyplane = 0
    theta = atan(abs(dx)/abs(dy))*180/pi
    if (dx>0 & dy>0) {a=theta}
    if (dx>0 & dy<0) {a=180-theta}
    if (dx<0 & dy<0) {a=180+theta}
    if (dx<0 & dy>0) {a=360-theta}
    return(a)
  }
}




#' @param r1 3 layer spatraster for earlier time period. Layers correspond to heights in canopy where lyr1 is highest and lyr3 is lowest
#' @param r2 3 layer spatraster fo later time period
#' @param hgts vector length 3 corresponding to  heights in the canopy (must be in order from highest to lowest)
#' @param projected logical T = r1 and r2 have a projected CRS
#' @param slope_correct logical; if TRUE, correct for the flattening of mountains from projecting onto 2D surface. If slope_correct = F, magnitude may be underestimated
#' @param slope spatraster; one layer spatraster giving slope in radians. Must have same cell size, resolution, and extent as r1 and r2

spatgrad = function(r1, r2, hgts, projected = TRUE, slope_correct = T, dem = NA) {

  a = terra::mean(r1[[1]], r2[[1]], na.rm = T)
  b = terra::mean(r1[[2]], r2[[2]], na.rm = T)
  c = terra::mean(r1[[3]], r2[[3]], na.rm = T)

  re = terra::res(a)

  # Create columns for focal and each of its 8 adjacent cells
  ya = data.table(terra::adjacent(a, 1:terra::ncell(a), direction = 8, pairs = TRUE))
  yb = data.table(terra::adjacent(b, 1:terra::ncell(b), direction = 8, pairs = TRUE))
  yc = data.table(terra::adjacent(c, 1:terra::ncell(c), direction = 8, pairs = TRUE))

  ya <- ya[, climFocal := terra::values(a)[from]][order(from, to)]   # Get value for focal cell, order the table by raster sequence and omit NAs (land cells)
  yb <- yb[, climFocal := terra::values(b)[from]][order(from, to)]
  yc <- yc[, climFocal := terra::values(c)[from]][order(from, to)]

  ya[, clim := terra::values(a)[to]] # Insert values for adjacent cells
  yb[, clim := terra::values(b)[to]]
  yc[, clim := terra::values(c)[to]]

  ya[, sy := terra::rowFromCell(a, from)-terra::rowFromCell(a, to)]  # Column to identify rows in the raster (N = 1, mid = 0, S = -1)
  yb[, sy := terra::rowFromCell(b, from)-terra::rowFromCell(b, to)]
  yc[, sy := terra::rowFromCell(c, from)-terra::rowFromCell(c, to)]

  ya[, sx := terra::colFromCell(a, to)-terra::colFromCell(a, from)]  # Same for columns (E = 1, mid = 0, W = -1)
  yb[, sx := terra::colFromCell(b, to)-terra::colFromCell(b, from)]
  yc[, sx := terra::colFromCell(c, to)-terra::colFromCell(c, from)]

  ya[sx > 1, sx := -1]   # Sort out the W-E wrap at the dateline, part I
  yb[sx > 1, sx := -1]
  yc[sx > 1, sx := -1]

  ya[sx < -1, sx := 1]   # Sort out the W-E wrap at the dateline, part II
  yb[sx < -1, sx := 1]
  yc[sx < -1, sx := 1]

  ya[, code := paste0(sx, sy)] # Make a unique code for each of the eight neighbouring cells
  yb[, code := paste0(sx, sy)]
  yc[, code := paste0(sx, sy)]

  # Code cells with positions
  ya[.(code = c("10","-10","-11","-1-1","11","1-1","01","0-1"), to = c("climE","climW","climNW","climSW","climNE","climSE","climN","climS")), on = "code", code := i.to]
  yb[.(code = c("10","-10","-11","-1-1","11","1-1","01","0-1"), to = c("climE","climW","climNW","climSW","climNE","climSE","climN","climS")), on = "code", code := i.to]
  yc[.(code = c("10","-10","-11","-1-1","11","1-1","01","0-1"), to = c("climE","climW","climNW","climSW","climNE","climSE","climN","climS")), on = "code", code := i.to]

  # make elevation datatable
  if (slope_correct==T){
    elev = data.table(ya[,.(from, to, code)])
  }

  ya <- dcast(ya[,c("from","code","clim")], from ~ code, value.var = list("clim"))
  yb <- dcast(yb[,c("from","code","clim")], from ~ code, value.var = "clim")
  yc <- dcast(yc[,c("from","code","clim")], from ~ code, value.var = "clim")

  ya[, climFocal := terra::values(a)[from]]   # Put climFocal back in
  yb[, climFocal := terra::values(b)[from]]
  yc[, climFocal := terra::values(c)[from]]

  ya[, LAT := terra::yFromCell(a, from)]         # Add focal cell latitude
  yb[, LAT := terra::yFromCell(b, from)]
  yc[, LAT := terra::yFromCell(c, from)]

  # correct for unprojected coordinate system if projected == F (correct for latitudinal distoration)
  ifelse(projected == TRUE, d <- 1, d <- 111.325)
  ifelse(projected == TRUE, co <- 0, co <- 1)

  # correct resolution distance for slope
  if (slope_correct==T) {
    elev[, elev := abs(terra::values(dem)[to])]
    elev <- dcast(elev[,c("from","code","elev")], from ~ code, value.var = "elev")

    elev[, climFocal := terra::values(dem)[from]]
    elev[, LAT := terra::yFromCell(dem, from)]

    # incorporates correction for latitudinal distoration
    elev[, distWE1 := sqrt((cos(co*CircStats::rad(LAT+re[2]))*(d*re[1]))^2 + (climN-climNW)^2)]
    elev[, distWE2 := sqrt((cos(co*CircStats::rad(LAT+re[2]))*(d*re[1]))^2 + (climFocal-climW)^2)]
    elev[, distWE3 := sqrt((cos(co*CircStats::rad(LAT+re[2]))*(d*re[1]))^2 + (climS-climSW)^2)]
    elev[, distWE4 := sqrt((cos(co*CircStats::rad(LAT+re[2]))*(d*re[1]))^2 + (climNE-climN)^2)]
    elev[, distWE5 := sqrt((cos(co*CircStats::rad(LAT+re[2]))*(d*re[1]))^2 + (climE-climFocal)^2)]
    elev[, distWE6 := sqrt((cos(co*CircStats::rad(LAT+re[2]))*(d*re[1]))^2 + (climSE-climS)^2)]

    elev[, distNS1 := sqrt((d*re[2])^2 + (climNW-climW)^2)]
    elev[, distNS2 := sqrt((d*re[2])^2 + (climN-climFocal)^2)]
    elev[, distNS3 := sqrt((d*re[2])^2 + (climNE-climE)^2)]
    elev[, distNS4 := sqrt((d*re[2])^2 + (climW-climSW)^2)]
    elev[, distNS5 := sqrt((d*re[2])^2 + (climFocal-climS)^2)]
    elev[, distNS6 := sqrt((d*re[2])^2 + (climE-climSE)^2)]

    # Calculate individual spatial climate gradients (climate unit per spatial unit (i.e. degC/m))
    # WE gradient show difference in climate value divided distance between cells (corrected for increase in elevation and latitudinal distortion if specified)
    # positive values indicate higher values in east than west

    ya[, gradWE1 := (climN-climNW)/elev[,distWE1]]
    ya[, gradWE2 := (climFocal-climW)/elev[,distWE2]]
    ya[, gradWE3 := (climS-climSW)/elev[,distWE3]]
    ya[, gradWE4 := (climNE-climN)/elev[,distWE4]]
    ya[, gradWE5 := (climE-climFocal)/elev[,distWE5]]
    ya[, gradWE6 := (climSE-climS)/elev[,distWE6]]

    yb[, gradWE1 := (climN-climNW)/elev[,distWE1]]
    yb[, gradWE2 := (climFocal-climW)/elev[,distWE2]]
    yb[, gradWE3 := (climS-climSW)/elev[,distWE3]]
    yb[, gradWE4 := (climNE-climN)/elev[,distWE4]]
    yb[, gradWE5 := (climE-climFocal)/elev[,distWE5]]
    yb[, gradWE6 := (climSE-climS)/elev[,distWE6]]

    yc[, gradWE1 := (climN-climNW)/elev[,distWE1]]
    yc[, gradWE2 := (climFocal-climW)/elev[,distWE2]]
    yc[, gradWE3 := (climS-climSW)/elev[,distWE3]]
    yc[, gradWE4 := (climNE-climN)/elev[,distWE4]]
    yc[, gradWE5 := (climE-climFocal)/elev[,distWE5]]
    yc[, gradWE6 := (climSE-climS)/elev[,distWE6]]

    # NS gradient show difference in climate value divided distance between cells (corrected for increase in elevation and latitudinal distortion if specified)
    # positive values indicate higher values in north than south

    ya[, gradNS1 := (climNW-climW)/elev[,distNS1]]
    ya[, gradNS2 := (climN-climFocal)/elev[,distNS2]]
    ya[, gradNS3 := (climNE-climE)/elev[,distNS3]]
    ya[, gradNS4 := (climW-climSW)/elev[,distNS4]]
    ya[, gradNS5 := (climFocal-climS)/elev[,distNS5]]
    ya[, gradNS6 := (climE-climSE)/elev[,distNS6]]

    yb[, gradNS1 := (climNW-climW)/elev[,distNS1]]
    yb[, gradNS2 := (climN-climFocal)/elev[,distNS2]]
    yb[, gradNS3 := (climNE-climE)/elev[,distNS3]]
    yb[, gradNS4 := (climW-climSW)/elev[,distNS4]]
    yb[, gradNS5 := (climFocal-climS)/elev[,distNS5]]
    yb[, gradNS6 := (climE-climSE)/elev[,distNS6]]

    yc[, gradNS1 := (climNW-climW)/elev[,distNS1]]
    yc[, gradNS2 := (climN-climFocal)/elev[,distNS2]]
    yc[, gradNS3 := (climNE-climE)/elev[,distNS3]]
    yc[, gradNS4 := (climW-climSW)/elev[,distNS4]]
    yc[, gradNS5 := (climFocal-climS)/elev[,distNS5]]
    yc[, gradNS6 := (climE-climSE)/elev[,distNS6]]

  } else {
    # Calculate individual spatial climate gradients (climate unit per spatial unit (i.e. degC/m))
    # WE gradient show difference in climate value divided distance between cells (corrected for latitudinal distortion if specified)
    # positive values indicate higher values in east than west
    ya[, gradWE1 := (climN-climNW)/(cos(co*CircStats::rad(LAT+re[2]))*(d*re[1]))]
    ya[, gradWE2 := (climFocal-climW)/(cos(co*CircStats::rad(LAT))*(d*re[1]))]
    ya[, gradWE3 := (climS-climSW)/(cos(co*CircStats::rad(LAT-re[2]))*(d*re[1]))]
    ya[, gradWE4 := (climNE-climN)/(cos(co*CircStats::rad(LAT+re[2]))*(d*re[1]))]
    ya[, gradWE5 := (climE-climFocal)/(cos(co*CircStats::rad(LAT))*(d*re[1]))]
    ya[, gradWE6 := (climSE-climS)/(cos(co*CircStats::rad(LAT-re[2]))*(d*re[1]))]

    yb[, gradWE1 := (climN-climNW)/(cos(co*CircStats::rad(LAT+re[2]))*(d*re[1]))]
    yb[, gradWE2 := (climFocal-climW)/(cos(co*CircStats::rad(LAT))*(d*re[1]))]
    yb[, gradWE3 := (climS-climSW)/(cos(co*CircStats::rad(LAT-re[2]))*(d*re[1]))]
    yb[, gradWE4 := (climNE-climN)/(cos(co*CircStats::rad(LAT+re[2]))*(d*re[1]))]
    yb[, gradWE5 := (climE-climFocal)/(cos(co*CircStats::rad(LAT))*(d*re[1]))]
    yb[, gradWE6 := (climSE-climS)/(cos(co*CircStats::rad(LAT-re[2]))*(d*re[1]))]

    yc[, gradWE1 := (climN-climNW)/(cos(co*CircStats::rad(LAT+re[2]))*(d*re[1]))]
    yc[, gradWE2 := (climFocal-climW)/(cos(co*CircStats::rad(LAT))*(d*re[1]))]
    yc[, gradWE3 := (climS-climSW)/(cos(co*CircStats::rad(LAT-re[2]))*(d*re[1]))]
    yc[, gradWE4 := (climNE-climN)/(cos(co*CircStats::rad(LAT+re[2]))*(d*re[1]))]
    yc[, gradWE5 := (climE-climFocal)/(cos(co*CircStats::rad(LAT))*(d*re[1]))]
    yc[, gradWE6 := (climSE-climS)/(cos(co*CircStats::rad(LAT-re[2]))*(d*re[1]))]

    # NS gradient show difference in climate value divided distance between cells (corrected for latitudinal distortion if specified)
    # positive values indicate higher values in north than south
    ya[, gradNS1 := (climNW-climW)/(d*re[2])]
    ya[, gradNS2 := (climN-climFocal)/(d*re[2])]
    ya[, gradNS3 := (climNE-climE)/(d*re[2])]
    ya[, gradNS4 := (climW-climSW)/(d*re[2])]
    ya[, gradNS5 := (climFocal-climS)/(d*re[2])]
    ya[, gradNS6 := (climE-climSE)/(d*re[2])]

    yb[, gradNS1 := (climNW-climW)/(d*re[2])]
    yb[, gradNS2 := (climN-climFocal)/(d*re[2])]
    yb[, gradNS3 := (climNE-climE)/(d*re[2])]
    yb[, gradNS4 := (climW-climSW)/(d*re[2])]
    yb[, gradNS5 := (climFocal-climS)/(d*re[2])]
    yb[, gradNS6 := (climE-climSE)/(d*re[2])]

    yc[, gradNS1 := (climNW-climW)/(d*re[2])]
    yc[, gradNS2 := (climN-climFocal)/(d*re[2])]
    yc[, gradNS3 := (climNE-climE)/(d*re[2])]
    yc[, gradNS4 := (climW-climSW)/(d*re[2])]
    yc[, gradNS5 := (climFocal-climS)/(d*re[2])]
    yc[, gradNS6 := (climE-climSE)/(d*re[2])]
  }

  # combine WE and NS gradients from all three layers
  ew = cbind(ya[,12:17], yb[,12:17], yc[,12:17])
  ns = cbind(ya[,18:23], yb[,18:23], yc[,18:23])

  # Calculate NS and WE gradients. NOTE: for angles to work (at least using simple positive and negative values on Cartesian axes), S-N & W-E gradients need to be positive)
  ew[, WEgrad := apply(.SD, 1, function(x) stats::weighted.mean(x, c(1,1,1,1,1,1,1,2,1,1,2,1,1,1,1,1,1,1), na.rm = T)), .SDcols = 1:18]
  ns[, NSgrad := apply(.SD, 1, function(x) stats::weighted.mean(x, c(1,1,1,1,1,1,1,2,1,1,2,1,1,1,1,1,1,1), na.rm = T)), .SDcols = 1:18]

  # calculate vertical spatial gradient
  topmid = (a-b)/(hgts[1]-hgts[2])
  midbottom = (b-c)/(hgts[2]-hgts[3])

  tb1 = data.table(terra::adjacent(a, 1:terra::ncell(a), direction = 8, pairs = TRUE, include = T))
  tb2 = data.table(terra::adjacent(a, 1:terra::ncell(a), direction = 8, pairs = TRUE, include = T))

  ft = data.table(1:terra::ncell(a), 1:terra::ncell(a)) # from focal to focal
  names(ft) = c('from', 'to')

  tb1 = rbind(tb1,ft)
  tb2 = rbind(tb2, ft)

  # get top/bottom difference values associated with each adjacent position
  tb1[, clim := terra::values(topmid)[to]]
  tb2[, clim := terra::values(midbottom)[to]]

  tb1 <- tb1[order(from, to)] # order based on from to columns and remove NAs
  tb1[, sy := terra::rowFromCell(a, from)-terra::rowFromCell(a, to)]
  tb1[, sx := terra::colFromCell(a, to)-terra::colFromCell(a, from)]
  tb1[sx > 1, sx := -1]
  tb1[sx < -1, sx := 1]
  tb1[, code := paste0(sx, sy)] # Make a unique code for each of the eight neighbouring cells

  # Code cells with positions
  tb1[.(code = c("00", "10","-10","-11","-1-1","11","1-1","01","0-1"), to = c("climFocal", "climE","climW","climNW","climSW","climNE","climSE","climN","climS")), on = "code", code := i.to]
  tb1 <- dcast(tb1[,c("from","code","clim")], from ~ code, value.var = "clim")

  tb2 <- tb2[order(from, to)] # order based on from to columns and remove NAs
  tb2[, sy := terra::rowFromCell(a, from)-terra::rowFromCell(a, to)]
  tb2[, sx := terra::colFromCell(a, to)-terra::colFromCell(a, from)]
  tb2[sx > 1, sx := -1]
  tb2[sx < -1, sx := 1]
  tb2[, code := paste0(sx, sy)] # Make a unique code for each of the eight neighbouring cells

  # Code cells with positions
  tb2[.(code = c("00", "10","-10","-11","-1-1","11","1-1","01","0-1"), to = c("climFocal", "climE","climW","climNW","climSW","climNE","climSE","climN","climS")), on = "code", code := i.to]
  tb2 <- dcast(tb2[,c("from","code","clim")], from ~ code, value.var = "clim")

  tb = cbind(tb1[,2:10], tb2[,2:10])
  tb[, TBgrad := apply(.SD, 1, function(x) stats::weighted.mean(x, c(1,2,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1), na.rm = T)), .SDcols = 1:18]

  grads = data.table(ew[,WEgrad], ns[,NSgrad], tb[,TBgrad])
  colnames(grads) = c('WEgrad', 'NSgrad', 'TBgrad')
  grads[is.na(WEgrad) & !(is.na(NSgrad) & is.na(TBgrad)), WEgrad := 0L]     # Where NSgrad does not exist, but WEgrad does, make NSgrad 0
  grads[is.na(NSgrad) & !(is.na(WEgrad) & is.na(TBgrad)), NSgrad := 0L]     # i.e. if at least one direction is not NA, change the other NA directions to 0
  grads[is.na(TBgrad) & !(is.na(NSgrad) & is.na(WEgrad)), WEgrad := 0L]

  # Calculate vector magnitude (C/km)
  grads[, mag := sqrt(apply(cbind((grads$WEgrad^2), (grads$NSgrad^2), (grads$TBgrad^2)), 1, sum, na.rm = FALSE))] # na.rm = F because if there is an NA in any direction, magntiude can't be calculated

  from <- data.table(1:terra::ncell(a)) # Make ordered from cells
  grads[, from := from] #cell number
  grads[, xlong := terra::xFromCell(a, from)] # longitude
  grads[, ylat := terra::yFromCell(a, from)] # latitude
  grads[, zhgt := hgts[2]] # height above ground

  rMag = rEWgrad = rNSgrad = rTBgrad = rEWmag = rNSmag = rTBmag = terra::rast(b)
  terra::values(rMag) = grads$mag
  terra::values(rEWgrad) = grads$WEgrad
  terra::values(rNSgrad) = grads$NSgrad
  terra::values(rTBgrad) = grads$TBgrad
  terra::values(rEWmag) = abs(grads$WEgrad)
  terra::values(rNSmag) = abs(grads$NSgrad)
  terra::values(rTBmag) = abs(grads$TBgrad)

  rout = c(rMag, rEWgrad, rNSgrad, rTBgrad, rEWmag, rNSmag, rTBmag)
  names(rout) = c('magnitude', 'WEgrad', 'NSgrad', 'TBgrad', 'EWmag', 'NSmag', 'TBmag')
  out = list(grads, rout)

  return(out)
}



######################################################################################
# spatgrad2 calculates 3D spatial gradient using six cells adjacent to center focal cell

#' @param r1 3 layer spatraster for earlier time period. Layers correspond to heights in canopy where lyr1 is highest and lyr3 is lowest
#' @param r2 3 layer spatraster fo later time period
#' @param hgts vector length 3 corresponding to  heights in the canopy (must be in order from highest to lowest)
#' @param projected logical T = r1 and r2 have a projected CRS
#' @param slope_correct logical; if TRUE, correct for the flattening of mountains from projecting onto 2D surface. If slope_correct = F, magnitude may be underestimated
#' @param dem spatraster; one layer spatraster giving elevation. Must have same cell size, resolution, and extent as r1 and r2

spatgrad2 = function(r1, r2, hgts, projected = TRUE, slope_correct = T, dem = NA) {

  a = terra::mean(r1[[1]], r2[[1]], na.rm = T)
  b = terra::mean(r1[[2]], r2[[2]], na.rm = T)
  c = terra::mean(r1[[3]], r2[[3]], na.rm = T)

  re = terra::res(a)

  # Create columns for focal and each of its 8 adjacent cells
  #ya = data.table(terra::adjacent(a, 1:terra::ncell(a), direction = 8, pairs = TRUE)) # layer above
  yb = data.table(terra::adjacent(b, 1:terra::ncell(b), direction = 8, pairs = TRUE)) # layer 0 (layer with focal cell)
  #yc = data.table(terra::adjacent(c, 1:terra::ncell(c), direction = 8, pairs = TRUE)) # layer below

  #ya <- ya[, climFocal := terra::values(a)[from]][order(from, to)]   # Get value for focal cell, order the table by raster sequence and omit NAs (land cells)
  yb <- yb[, climFocal := terra::values(b)[from]][order(from, to)]
  #yc <- yc[, climFocal := terra::values(c)[from]][order(from, to)]

  #ya[, clim := terra::values(a)[to]] # Insert values for adjacent cells
  yb[, clim := terra::values(b)[to]]
  #yc[, clim := terra::values(c)[to]]

  #ya[, sy := terra::rowFromCell(a, from)-terra::rowFromCell(a, to)]  # Column to identify rows in the raster (N = 1, mid = 0, S = -1)
  yb[, sy := terra::rowFromCell(b, from)-terra::rowFromCell(b, to)]
  #yc[, sy := terra::rowFromCell(c, from)-terra::rowFromCell(c, to)]

  #ya[, sx := terra::colFromCell(a, to)-terra::colFromCell(a, from)]  # Same for columns (E = 1, mid = 0, W = -1)
  yb[, sx := terra::colFromCell(b, to)-terra::colFromCell(b, from)]
  #yc[, sx := terra::colFromCell(c, to)-terra::colFromCell(c, from)]

  #ya[sx > 1, sx := -1]   # Sort out the W-E wrap at the dateline, part I
  yb[sx > 1, sx := -1]
  #yc[sx > 1, sx := -1]

  #ya[sx < -1, sx := 1]   # Sort out the W-E wrap at the dateline, part II
  yb[sx < -1, sx := 1]
  #yc[sx < -1, sx := 1]

  #ya[, code := paste0(sx, sy)] # Make a unique code for each of the eight neighbouring cells
  yb[, code := paste0(sx, sy)]
  #yc[, code := paste0(sx, sy)]

  # Code cells with positions
  #ya[.(code = c("10","-10","-11","-1-1","11","1-1","01","0-1"), to = c("climE","climW","climNW","climSW","climNE","climSE","climN","climS")), on = "code", code := i.to]
  yb[.(code = c("10","-10","-11","-1-1","11","1-1","01","0-1"), to = c("climE","climW","climNW","climSW","climNE","climSE","climN","climS")), on = "code", code := i.to]
  #yc[.(code = c("10","-10","-11","-1-1","11","1-1","01","0-1"), to = c("climE","climW","climNW","climSW","climNE","climSE","climN","climS")), on = "code", code := i.to]

  # make elevation datatable
  if (slope_correct==T){
    elev = data.table(yb[,.(from, to, code)])
  }

  #ya <- dcast(ya[,c("from","code","clim")], from ~ code, value.var = list("clim"))
  yb <- dcast(yb[,c("from","code","clim")], from ~ code, value.var = "clim")
  #yc <- dcast(yc[,c("from","code","clim")], from ~ code, value.var = "clim")

  yb[, climUP := terra::values(a)[from]]   # Put climFocal back in (clim focal is the cell in the center of the cube)
  yb[, climFocal := terra::values(b)[from]]
  yb[, climDOWN := terra::values(c)[from]]

  #ya[, LAT := terra::yFromCell(a, from)]         # Add focal cell latitude
  #yb[, LAT := terra::yFromCell(b, from)]
  #yc[, LAT := terra::yFromCell(c, from)]

  # correct for unprojected coordinate system if projected == F (correct for latitudinal distoration)
  #ifelse(projected == TRUE, d <- 1, d <- 111.325)
  #ifelse(projected == TRUE, co <- 0, co <- 1)

  #diag_dist = sqrt(re[1]^2 + re[2]^2) # distance from focal cell to corner cell in the same layer
  dist_up = hgts[1]-hgts[2]
  dist_down = hgts[2]-hgts[3]

  # correct resolution distance for slope
  if (slope_correct==T) {
    elev[, elev := abs(terra::values(dem)[to])]
    elev <- dcast(elev[,c("from","code","elev")], from ~ code, value.var = "elev")

    elev[, climFocal := terra::values(dem)[from]]
    #elev[, LAT := terra::yFromCell(dem, from)]

    # calculate distance between center of focal cell and cells in each direction to adjust for elevational differences between cells
    # these distances are used in place of cell resolution
    elev[, distE := sqrt(re[1]^2 + (climE-climFocal)^2)] # distance between east and focal (layer 0)
    elev[, distW := sqrt(re[1]^2 + (climFocal-climW)^2)] #distance between focal and west (layer 0)
    elev[, distN := sqrt(re[2]^2 + (climN-climFocal)^2)]
    elev[, distS := sqrt(re[2]^2 + (climFocal-climS)^2)]
    #elev[, distNW := sqrt(diag_dist^2 + (climFocal-climNW)^2)]
    #elev[, distSW := sqrt(diag_dist^2 + (climFocal-climSW)^2)]
    #elev[, distNE := sqrt(diag_dist^2 + (climNE-climFocal)^2)]
    #elev[, distSE := sqrt(diag_dist^2 + (climSE-climFocal)^2)]

    # adjusted distances for above
    #elev[, distUE := sqrt(distE^2 + dist_up^2)]
    #elev[, distUW := sqrt(distW^2 + dist_up^2)]
    #elev[, distUN := sqrt(distN^2 + dist_up^2)]
    #elev[, distUS := sqrt(distS^2 + dist_up^2)]
    #elev[, distUNW := sqrt(distNW^2 + dist_up^2)]
    #elev[, distUSW := sqrt(distSW^2 + dist_up^2)]
    #elev[, distUNE := sqrt(distNE^2 + dist_up^2)]
    #elev[, distUSE := sqrt(distSE^2 + dist_up^2)]

#    if (dist_up != dist_down) {
#      # adjusted distances for layer below
#      elev[, distDE := sqrt(distE^2 + dist_down^2)]
#      elev[, distDW := sqrt(distW^2 + dist_down^2)]
#      elev[, distDN := sqrt(distN^2 + dist_down^2)]
#      elev[, distDS := sqrt(distS^2 + dist_down^2)]
#      elev[, distDNW := sqrt(distNW^2 + dist_down^2)]
#      elev[, distDSW := sqrt(distSW^2 + dist_down^2)]
#      elev[, distDNE := sqrt(distNE^2 + dist_down^2)]
#      elev[, distDSE := sqrt(distSE^2 + dist_down^2)]
#    }

    # Calculate individual spatial climate gradients (climate unit per spatial unit (i.e. degC/m))
    # WE gradient shows difference in climate value divided by distance between cells corrected for elevation and height travelled up or down
    # positive values indicate higher values in east than west

    # calculate gradient between focal cell and each of the surrounding cells
    # divide the diagonals into x,y,z, component vectors

    # gradients between focal cell and directly adjacent cells in all 6 directions
    yb[, gradE := (climE-climFocal)/elev[,distE]]
    yb[, gradW := (climFocal-climW)/elev[,distW]]
    yb[, gradN := (climN-climFocal)/elev[,distN]]
    yb[, gradS := (climFocal-climS)/elev[,distS]]
    yb[, gradUP := (climUP-climFocal)/dist_up]
    yb[, gradDOWN := (climFocal-climDOWN)/dist_down]

  } else {
    # Calculate individual spatial climate gradients (climate unit per spatial unit (i.e. degC/m))
    # WE gradient show difference in climate value divided distance between cells (corrected for latitudinal distortion if specified)
    # positive values indicate higher values in east than west, higher values in north than south, and higher values up than down

    yb[, gradE := (climE-climFocal)/re[1]]
    yb[, gradW := (climFocal-climW)/re[1]]
    yb[, gradN := (climN-climFocal)/re[2]]
    yb[, gradS := (climFocal-climS)/re[2]]
    yb[, gradUP := (yb[,climUP]-climFocal)/dist_up]
    yb[, gradDOWN := (climFocal-yb[,climDOWN])/dist_down]
  }

  yb[, EWgrad := apply(.SD, 1, function(x) mean(x, na.rm=T)), .SDcols = 13:14]
  yb[, NSgrad := apply(.SD, 1, function(x) mean(x, na.rm=T)), .SDcols = 15:16]
  yb[, TBgrad := apply(.SD, 1, function(x) mean(x, na.rm=T)), .SDcols = 17:18]



  grads = data.table(yb[,EWgrad], yb[,NSgrad], yb[,TBgrad])
  colnames(grads) = c('EWgrad', 'NSgrad', 'TBgrad')
  #grads[is.na(EWgrad) & !(is.na(NSgrad) & is.na(TBgrad)), EWgrad := 0L]     # Where NSgrad does not exist, but WEgrad does, make NSgrad 0
  #grads[is.na(NSgrad) & !(is.na(EWgrad) & is.na(TBgrad)), NSgrad := 0L]     # i.e. if at least one direction is not NA, change the other NA directions to 0
  #grads[is.na(TBgrad) & !(is.na(NSgrad) & is.na(EWgrad)), WEgrad := 0L]     # doing this doesn't make sense because NA appear due to random missing data points, so we can't assume an NA = 0

  # Calculate vector magnitude (C/km)
  grads[, mag := sqrt(apply(cbind((grads$EWgrad^2), (grads$NSgrad^2), (grads$TBgrad^2)), 1, sum, na.rm = FALSE))] # na.rm = F because if there is an NA in any direction, magntiude can't be calculated

  from <- data.table(1:terra::ncell(a)) # Make ordered from cells
  grads[, from := from] #cell number
  grads[, xlong := terra::xFromCell(a, from)] # longitude
  grads[, ylat := terra::yFromCell(a, from)] # latitude
  grads[, zhgt := hgts[2]] # height above ground

  rMag = rEWgrad = rNSgrad = rTBgrad = rEWmag = rNSmag = rTBmag = terra::rast(b)
  terra::values(rMag) = grads$mag
  terra::values(rEWgrad) = grads$EWgrad
  terra::values(rNSgrad) = grads$NSgrad
  terra::values(rTBgrad) = grads$TBgrad
  terra::values(rEWmag) = abs(grads$EWgrad)
  terra::values(rNSmag) = abs(grads$NSgrad)
  terra::values(rTBmag) = abs(grads$TBgrad)

  rout = c(rMag, rEWgrad, rNSgrad, rTBgrad, rEWmag, rNSmag, rTBmag)
  names(rout) = c('magnitude', 'EWgrad', 'NSgrad', 'TBgrad', 'EWmag', 'NSmag', 'TBmag')
  out = list(grads, rout)

  return(out)
}







