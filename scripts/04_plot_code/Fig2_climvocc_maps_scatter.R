## I don't think I use this script any more

source("scripts/03_analysis/02_compareScales/voccSpeed.R")
source("scripts/02_climate_velocity/scripts/03_plot_maps/plot_climvocc.R")

layout = "
AC
BC
DF
EG
"

vocc = vocc + scale_x_discrete("Scale", labels = c("Macro", "Topo", "Land\nsurface", "Within-\ncanopy"))

voccplt =  elev.plt + pai.plt + vocc + macro.vocc.plt + meso.vocc.plt + micro2d.vocc.plt + micro3d.vocc.plt + plot_layout(design = layout) +
  plot_annotation(tag_levels = "a")

png("03_analysis/00_plots/vocc_speed/compareScales_map.png", width = 3000, height = 2000, unit = "px", res = 300)
voccplt
dev.off()

layout = "
AC
BD
"
spatgrad.plt = macro.spatgrad.plt + meso.spatgrad.plt + micro2d.spatgrad.plt + micro3d.spatgrad.plt +
  plot_layout(design = layout) + plot_annotation(tag_levels = "a")

png("03_analysis/00_plots/vocc_speed/compareScales_map_spatgrad.png", width = 3000, height = 1000, unit = "px", res = 300)
spatgrad.plt
dev.off()

tempgrad.plt = macro.tempgrad.plt + meso.tempgrad.plt + micro2d.tempgrad.plt + micro3d.tempgrad.plt +
  plot_layout(design = layout) + plot_annotation(tag_levels = "a")

png("03_analysis/00_plots/vocc_speed/compareScales_map_tempgrad.png", width = 3000, height = 1000, unit = "px", res = 300)
tempgrad.plt
dev.off()

vocc.elev.pai.plt = macro.summ.plt + meso.summ.plt + micro2d.summ.plt + micro3d.summ.plt + 
  plot_annotation(tag_levels = "A")

png("03_analysis/00_plots/vocc_speed/compareScales_vocc_elev_pai.png", width = 2500, height = 2000, unit = "px", res = 300)
vocc.elev.pai.plt
dev.off()
