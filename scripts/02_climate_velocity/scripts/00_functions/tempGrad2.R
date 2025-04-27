tempGrad2 = function(r1, r2, t1, t2) {
  Tdiff = r2-r1
  tg = Tdiff/(t2-t1)
  return(tg)
}

phi_adjust = function(phi) {
  if (is.na(phi) | is.infinite(phi)) {p=Inf} else {
    if (phi>=0 & phi<=90) {p=90-phi}
    if (phi>90 & phi<=180) {p=450-phi}
    if (phi>=-180 & phi<0) {p=-1*phi+90}
  }
  return(p)
}

gVocc = function(tempgrad, spatgrad) {
  VoCC = tempgrad/spatgrad[[2]]$magnitude
  voccEW = tempgrad/spatgrad[[2]]$EWmag
  voccNS = tempgrad/spatgrad[[2]]$NSmag
  voccTB = tempgrad/spatgrad[[2]]$TBmag

  # add climate velocity to datatable
  tab = spatgrad[[1]]
  tab[, vocc := terra::values(VoCC)]

  # adjust velocity vectors for direction climate is moving
  # when vocc is positive (climate warming) must reverse the vector direction so arrow points from warm to cool along the spatial gradient
  tab[, EW_dir := ifelse(vocc>0, -1*EWgrad, EWgrad)]
  tab[, NS_dir := ifelse(vocc>0, -1*NSgrad, NSgrad)]
  tab[, TB_dir := ifelse(vocc>0, -1*TBgrad, TBgrad)]

  # Calculate the unite vectors to normalize for plotting arrows (unit vector magnitude = 1)
  tab[, EWnorm_dir := EW_dir/mag] # east = positive
  tab[, NSnorm_dir := NS_dir/mag] # north = positive
  tab[, TBnorm_dir := TB_dir/mag] # up = positive

  # calculate directional angles
  tab[, alpha_dir := (acos(EW_dir/mag))*180/pi]
  tab[, beta_dir := (acos(NS_dir/mag))*180/pi]
  tab[, gamma_dir := (acos(TB_dir/mag))*180/pi]
  tab[, xyAng := mapply(angFromNorth, EW_dir, NS_dir)] # calculate angle in xyplane (0=N, 90=E, 180=S, 270=W, Inf = no xy movement)
  tab[, zAng := asin(TBnorm_dir)*180/pi] # calculate z projection angle (angle up or down from xy-plane): 90=straight up canopy, -90=straight down canopy


  rEWdir = rNSdir = rTBdir = rxyAng = rzAng = terra::rast(VoCC)
  terra::values(rEWdir) = tab$EW_dir
  terra::values(rNSdir) = tab$NS_dir
  terra::values(rTBdir) = tab$TB_dir
  terra::values(rxyAng) = tab$xyAng
  terra::values(rzAng) = tab$zAng

  rout = c(VoCC, voccEW, voccNS, voccTB, spatgrad[[2]]$magnitude, rEWdir, rNSdir, rTBdir, rxyAng, rzAng)
  names(rout) = c('vocc', 'voccEW', 'voccNS', 'voccTB', 'magnitude', 'EWdir', 'NSdir', 'TBdir', 'xyAng', 'zAng')

  return(list(rout, tab))
}
