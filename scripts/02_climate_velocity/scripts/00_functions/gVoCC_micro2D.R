# tempgrad = tempGrad2(r[[1]], r[[2]], 1955, 1960)
# vocc = gVocc_micro2D(tempgrad, spatgrad)
# plot(vocc[[1]]$vocc)


gVocc_micro2D = function(tempgrad, spatgrad) {
  VoCC = tempgrad/spatgrad[[2]]$magnitude
  voccEW = tempgrad/spatgrad[[2]]$EWmag
  voccNS = tempgrad/spatgrad[[2]]$NSmag

  # add climate velocity to datatable
  tab = spatgrad[[1]]
  tab[, vocc := terra::values(VoCC)]

  # adjust velocity vectors for direction climate is moving
  # when vocc is positive (climate warming) must reverse the vector direction so arrow points from warm to cool along the spatial gradient
  tab[, EW_dir := ifelse(vocc>0, -1*EWgrad, EWgrad)]
  tab[, NS_dir := ifelse(vocc>0, -1*NSgrad, NSgrad)]

  # Calculate the unite vectors to normalize for plotting arrows (unit vector magnitude = 1)
  tab[, EWnorm_dir := EW_dir/mag] # east = positive
  tab[, NSnorm_dir := NS_dir/mag] # north = positive

  # calculate directional angles
  tab[, alpha_dir := (acos(EW_dir/mag))*180/pi]
  tab[, beta_dir := (acos(NS_dir/mag))*180/pi]
  tab[, xyAng := mapply(angFromNorth, EW_dir, NS_dir)] # calculate angle in xyplane (0=N, 90=E, 180=S, 270=W, Inf = no xy movement)

  rEWdir = rNSdir = rxyAng = terra::rast(VoCC)
  terra::values(rEWdir) = tab$EW_dir
  terra::values(rNSdir) = tab$NS_dir
  terra::values(rxyAng) = tab$xyAng

  rout = c(VoCC, voccEW, voccNS, spatgrad[[2]]$magnitude, rEWdir, rNSdir, rxyAng)
  names(rout) = c('vocc', 'voccEW', 'voccNS', 'magnitude', 'EWdir', 'NSdir', 'xyAng')

  return(list(rout, tab))
}
