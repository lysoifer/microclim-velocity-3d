library(terra)
library(ggplot2)
library(tidyterra)
library(colorspace)
library(patchwork)

pai = rast("data/PAI/pai_NRange_rmMeters0to2_rmPAIgt10.tif")
elev = rast("data/topography/dem_reproj.tif")
chm = rast("data/topography/chm_reproj.tif")

# N range polygon
nrange = vect("data/cropping_polygons/NRange.shp")
nrange = project(nrange, elev)
nrange = aggregate(nrange)

paiplt = ggplot() +
  geom_spatvector(data = nrange, color = NA, fill = "gray80") +
  geom_spatraster(data = pai) +
  scale_fill_continuous_sequential(palette = "YlGn", na.value = NA) +
  guides(fill = guide_colorbar(title = "PAI")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        #legend.position = "inside",
        #legend.justification = c(1,0),
        legend.background = element_blank(),
        legend.key.size = unit(4, "mm"),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        plot.tag = element_text(size = 3))

chmplt = ggplot() +
  geom_spatvector(data = nrange, color = NA, fill = "gray80") +
  geom_spatraster(data = chm) +
  scale_fill_continuous_sequential(palette = "Viridis", na.value = NA,
                                   limits = c(0,60),
                                   oob = scales::squish,
                                   breaks = c(0,20,40,60),
                                   labels = c("0", "20", "40", ">60")) +
  guides(fill = guide_colorbar(title = "CHM")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        #legend.position = "inside",
        #legend.justification = c(1,0),
        legend.background = element_blank(),
        legend.key.size = unit(4, "mm"),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        plot.tag = element_text(size = 3))


elevplt = ggplot() +
  geom_spatvector(data = nrange, color = NA, fill = "gray80") +
  geom_spatraster(data = elev) +
  scale_fill_continuous_sequential(palette = "Terrain 2", na.value = NA,
                                   limits = c(0,940),
                                   oob = scales::squish,
                                   rev = F) +
  guides(fill = guide_colorbar(title = "Elevation (m)")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        #legend.position = "inside",
        #legend.justification = c(1,0),
        legend.background = element_blank(),
        legend.key.size = unit(4, "mm"),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 6),
        plot.tag = element_text(size = 3)) 

add_tag <- function(label, x = 0, y = 1, padding.x = -unit(2, "pt"), padding.y = unit(2, "pt"), hjust = 0, vjust = 1, size = 8) {
  annotation_custom(
    grid::textGrob(
      x = unit(x, "npc") - padding.x,
      y = unit(y, "npc") - padding.y,
      hjust = hjust, vjust = vjust,
      label = label, gp = grid::gpar(fontsize = size)
    )
  )
}

plot_list = list(elevplt, paiplt, chmplt)
lapply(
  seq_along(plot_list), \(x) plot_list[[x]] + add_tag(letters[x])
) |>
  wrap_plots(nrow = 3)

ggsave("scripts/03_analysis/00_plots/supplemental_figs/ElevPaiChm_maps.png", width = 120, height = 90, dpi = 300, units = "mm")


