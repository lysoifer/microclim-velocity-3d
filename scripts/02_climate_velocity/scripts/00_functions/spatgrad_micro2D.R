# calculates the 2D microclimate velocity with an elevational correction

#' @param r spatraster with past and present climate data for a single height
#' @param projected logical T = r1 and r2 have a projected CRS
#' @param slope_correct logical; if TRUE, correct for the flattening of mountains from projecting onto 2D surface. If slope_correct = F, magnitude may be underestimated
#' @param dem spatraster; one layer spatraster giving slope in radians. Must have same cell size, resolution, and extent as r1 and r2
#' @param hgt character or numeric - height above ground to label data frame

spatgrad_2Dmicro = function(r, projected = TRUE, slope_correct = T, dem = NA, hgt) {

  a = terra::mean(r[[1]], r[[2]], na.rm = T)

  re = terra::res(a)

  # Create columns for focal and each of its 8 adjacent cells
  ya = data.table(terra::adjacent(a, 1:terra::ncell(a), direction = 8, pairs = TRUE))

  ya <- ya[, climFocal := terra::values(a)[from]][order(from, to)]   # Get value for focal cell, order the table by raster sequence and omit NAs (land cells)

  ya[, clim := terra::values(a)[to]] # Insert values for adjacent cells

  ya[, sy := terra::rowFromCell(a, from)-terra::rowFromCell(a, to)]  # Column to identify rows in the raster (N = 1, mid = 0, S = -1)

  ya[, sx := terra::colFromCell(a, to)-terra::colFromCell(a, from)]  # Same for columns (E = 1, mid = 0, W = -1)

  ya[sx > 1, sx := -1]   # Sort out the W-E wrap at the dateline, part I

  ya[sx < -1, sx := 1]   # Sort out the W-E wrap at the dateline, part II

  ya[, code := paste0(sx, sy)] # Make a unique code for each of the eight neighbouring cells

  # Code cells with positions
  ya[.(code = c("10","-10","-11","-1-1","11","1-1","01","0-1"), to = c("climE","climW","climNW","climSW","climNE","climSE","climN","climS")), on = "code", code := i.to]

  # make elevation datatable
  if (slope_correct==T){
    elev = data.table(ya[,.(from, to, code)])
  }

  ya <- dcast(ya[,c("from","code","clim")], from ~ code, value.var = list("clim"))

  ya[, climFocal := terra::values(a)[from]]   # Put climFocal back in

  ya[, LAT := terra::yFromCell(a, from)]         # Add focal cell latitude

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
    elev[, distEW1 := sqrt((cos(co*CircStats::rad(LAT+re[2]))*(d*re[1]))^2 + (climN-climNW)^2)]
    elev[, distEW2 := sqrt((cos(co*CircStats::rad(LAT+re[2]))*(d*re[1]))^2 + (climFocal-climW)^2)]
    elev[, distEW3 := sqrt((cos(co*CircStats::rad(LAT+re[2]))*(d*re[1]))^2 + (climS-climSW)^2)]
    elev[, distEW4 := sqrt((cos(co*CircStats::rad(LAT+re[2]))*(d*re[1]))^2 + (climNE-climN)^2)]
    elev[, distEW5 := sqrt((cos(co*CircStats::rad(LAT+re[2]))*(d*re[1]))^2 + (climE-climFocal)^2)]
    elev[, distEW6 := sqrt((cos(co*CircStats::rad(LAT+re[2]))*(d*re[1]))^2 + (climSE-climS)^2)]

    elev[, distNS1 := sqrt((d*re[2])^2 + (climNW-climW)^2)]
    elev[, distNS2 := sqrt((d*re[2])^2 + (climN-climFocal)^2)]
    elev[, distNS3 := sqrt((d*re[2])^2 + (climNE-climE)^2)]
    elev[, distNS4 := sqrt((d*re[2])^2 + (climW-climSW)^2)]
    elev[, distNS5 := sqrt((d*re[2])^2 + (climFocal-climS)^2)]
    elev[, distNS6 := sqrt((d*re[2])^2 + (climE-climSE)^2)]

    # Calculate individual spatial climate gradients (climate unit per spatial unit (i.e. degC/m))
    # WE gradient show difference in climate value divided distance between cells (corrected for increase in elevation and latitudinal distortion if specified)
    # positive values indicate higher values in east than west

    ya[, gradEW1 := (climN-climNW)/elev[,distEW1]]
    ya[, gradEW2 := (climFocal-climW)/elev[,distEW2]]
    ya[, gradEW3 := (climS-climSW)/elev[,distEW3]]
    ya[, gradEW4 := (climNE-climN)/elev[,distEW4]]
    ya[, gradEW5 := (climE-climFocal)/elev[,distEW5]]
    ya[, gradEW6 := (climSE-climS)/elev[,distEW6]]


    # NS gradient show difference in climate value divided distance between cells (corrected for increase in elevation and latitudinal distortion if specified)
    # positive values indicate higher values in north than south

    ya[, gradNS1 := (climNW-climW)/elev[,distNS1]]
    ya[, gradNS2 := (climN-climFocal)/elev[,distNS2]]
    ya[, gradNS3 := (climNE-climE)/elev[,distNS3]]
    ya[, gradNS4 := (climW-climSW)/elev[,distNS4]]
    ya[, gradNS5 := (climFocal-climS)/elev[,distNS5]]
    ya[, gradNS6 := (climE-climSE)/elev[,distNS6]]

  } else {
    # Calculate individual spatial climate gradients (climate unit per spatial unit (i.e. degC/m))
    # WE gradient show difference in climate value divided distance between cells (corrected for latitudinal distortion if specified)
    # positive values indicate higher values in east than west
    ya[, gradEW1 := (climN-climNW)/(cos(co*CircStats::rad(LAT+re[2]))*(d*re[1]))]
    ya[, gradEW2 := (climFocal-climW)/(cos(co*CircStats::rad(LAT))*(d*re[1]))]
    ya[, gradEW3 := (climS-climSW)/(cos(co*CircStats::rad(LAT-re[2]))*(d*re[1]))]
    ya[, gradEW4 := (climNE-climN)/(cos(co*CircStats::rad(LAT+re[2]))*(d*re[1]))]
    ya[, gradEW5 := (climE-climFocal)/(cos(co*CircStats::rad(LAT))*(d*re[1]))]
    ya[, gradEW6 := (climSE-climS)/(cos(co*CircStats::rad(LAT-re[2]))*(d*re[1]))]

    # NS gradient show difference in climate value divided distance between cells (corrected for latitudinal distortion if specified)
    # positive values indicate higher values in north than south
    ya[, gradNS1 := (climNW-climW)/(d*re[2])]
    ya[, gradNS2 := (climN-climFocal)/(d*re[2])]
    ya[, gradNS3 := (climNE-climE)/(d*re[2])]
    ya[, gradNS4 := (climW-climSW)/(d*re[2])]
    ya[, gradNS5 := (climFocal-climS)/(d*re[2])]
    ya[, gradNS6 := (climE-climSE)/(d*re[2])]
   }

  # combine WE and NS gradients from all three layers
  ew = cbind(ya[,12:17])
  ns = cbind(ya[,18:23])

  # Calculate NS and WE gradients. NOTE: for angles to work (at least using simple positive and negative values on Cartesian axes), S-N & W-E gradients need to be positive)
  # weight non-adjacent cells by 1/sqrt(2) because of geometry of 45-45-90 triangle
  wgt = 1/sqrt(2)
  ew[, EWgrad := apply(.SD, 1, function(x) stats::weighted.mean(x, c(wgt, 1, wgt, wgt, 1, wgt), na.rm = T)), .SDcols = 1:6]
  ns[, NSgrad := apply(.SD, 1, function(x) stats::weighted.mean(x, c(wgt, 1, wgt, wgt, 1, wgt), na.rm = T)), .SDcols = 1:6]

  grads = data.table(ew[,EWgrad], ns[,NSgrad])
  colnames(grads) = c('EWgrad', 'NSgrad')

  # Calculate vector magnitude (C/km)
  grads[, mag := sqrt(apply(cbind((grads$EWgrad^2), (grads$NSgrad^2)), 1, sum, na.rm = FALSE))] # na.rm = F because if there is an NA in any direction, magntiude can't be calculated

  from <- data.table(1:terra::ncell(a)) # Make ordered from cells
  grads[, from := from] #cell number
  grads[, xlong := terra::xFromCell(a, from)] # longitude
  grads[, ylat := terra::yFromCell(a, from)] # latitude
  grads[, zhgt := hgt] # height above ground

  #mag = magnitude of spatial gradient
  rMag = rEWgrad = rNSgrad = rEWmag = rNSmag = terra::rast(a)
  terra::values(rMag) = grads$mag
  terra::values(rEWgrad) = grads$EWgrad
  terra::values(rNSgrad) = grads$NSgrad
  terra::values(rEWmag) = abs(grads$EWgrad)
  terra::values(rNSmag) = abs(grads$NSgrad)

  rout = c(rMag, rEWgrad, rNSgrad, rEWmag, rNSmag)
  names(rout) = c('magnitude', 'EWgrad', 'NSgrad', 'EWmag', 'NSmag')
  out = list(grads, rout)

  return(out)
}


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
