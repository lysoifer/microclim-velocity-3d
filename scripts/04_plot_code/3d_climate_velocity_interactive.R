library(terra)
library(plotly)
library(data.table)
library(numform)

# vocc
micro.3d = list.files(path = "02_climate_velocity/output/3D/avg_daily_maxTemp/", pattern = "vocc", full.names = T)
micro.3d = grep(".tif", micro.3d, value = T)
micro.3d = lapply(micro.3d, rast)

vocc = lapply(micro.3d, "[[", 1)
xyAng = lapply(micro.3d, "[[", 9)
zAng = lapply(micro.3d, "[[", 10)

vocc = rast(vocc)
names(vocc) = paste0(f_pad_zero(seq(5,35,5), 2))

xyAng = rast(xyAng)
names(xyAng) = paste0(f_pad_zero(seq(5,35,5), 2))

zAng = rast(zAng)
names(zAng) = paste0(f_pad_zero(seq(5,35,5), 2))

pai = rast("./../Trinidad_microclimates/data/PAI/pai_NRange_rmMeters0to2_rmPAIgt10.tif")
pai.small = crop(pai, ext(688967, 690704, 1186597,1187941))
names(pai.small) = "pai"

pai.small

# add canopy height
chm = rast("./../Trinidad_microclimates/data/topography/chm_reproj.tif")
names(chm) = "chm"
chm = crop(chm, pai.small)
chm = as.data.frame(chm, xy = T)

# crop to small extent
vocc = crop(vocc, pai.small) %>% 
  as.data.frame(xy = T) %>% 
  pivot_longer(3:9, names_to = "height", values_to = "vocc")

xyAng = crop(xyAng, pai.small) %>% 
  as.data.frame(xy = T)%>% 
  pivot_longer(3:9, names_to = "height", values_to = "xyAng")

zAng = crop(zAng, pai.small) %>% 
  as.data.frame(xy = T)%>% 
  pivot_longer(3:9, names_to = "height", values_to = "zAng")

df = vocc %>% 
  left_join(xyAng, by = c("x", "y", "height")) %>% 
  left_join(zAng, by = c("x", "y", "height")) %>% 
  left_join(chm, by = c("x", "y")) %>% 
  drop_na()

# calculate distance travelled in x, y, and z directions after 55 years
df = df %>% 
  mutate(m55 = vocc*55,
         x55 = m55*cos(zAng*pi/180)*sin(xyAng*pi/180),
         y55 = m55*cos(zAng*pi/180)*cos(xyAng*pi/180),
         z55 = m55*sin(zAng*pi/180),
         height = as.numeric(height),
         relhgt = height/chm) %>% 
  filter(relhgt <= 1) %>% 
  drop_na()

test = df[1:100,]

# color represents meters moved in 55 years
fig = plot_ly(
  type = 'cone',
  x = df$x/10, y = df$y/10, z = df$height,
  u = df$x55, v = df$y55, w = df$z55,
  sizemode = 'absolute', sizeref = 1000,
  anchor = 'tail',
  colorbar = list(x=0, xanchor = 'right', side = 'left')
)

print(fig)

library(htmlwidgets)

saveWidget(fig, "03_analysis/00_plots/vocc3d_interactive.html")

Sys.setenv("plotly_username"="lysoifer")
Sys.setenv("plotly_api_key"="snhhKV6EBaJ8dQa8So98")


api_create(fig, filename = "3d_climate_velocity")





















