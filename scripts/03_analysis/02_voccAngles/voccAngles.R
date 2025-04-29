library(tidyverse)
library(ggplot2)
library(patchwork)
library(viridis)
library(colorspace)
library(terra)
library(data.table)
library(ggh4x)

maxtemp = fread("data/dataframes/analysis_dataframe_full_maxtemp.csv")
mintemp = fread("data/dataframes/analysis_dataframe_full_mintemp.csv")

maxtemp$var = "maxtemp"
mintemp$var = "mintemp"

maxtemp = maxtemp %>% rename(temp.pres = maxtemp.pres)
mintemp = mintemp %>% rename(temp.pres = mintemp.pres)


# Functions
# calculates difference between two angles ( always positive)
angDiff <- function(degreeA = 0, degreeB = 0) {
  abs((degreeA - degreeB + 180) %% 360 - 180)
}

## ---------------------------------------------------------------------------##
# add upslope direction (i.e., direction you would have to move to go up slope)
# For example, if aspect is 180  (South), you would have to move north (0 deg) to go upslope


# calculate upslope angle difference and pai angle difference
# i.e., dif between moving upslope and moving toward denser vegetation
# dif of zero indicates movement upslope or movement toward denser vegetation

maxtemp = maxtemp %>% 
  mutate(upslope.dir = ifelse(aspect >=180, aspect-180, aspect+180),
         upslope.angdif= angDiff(xyAng, upslope.dir),
         pai.angdif = angDiff(xyAng, paidir))

mintemp = mintemp %>% 
  mutate(upslope.dir = ifelse(aspect >=180, aspect-180, aspect+180),
         upslope.angdif= angDiff(xyAng, upslope.dir),
         pai.angdif = angDiff(xyAng, paidir))


# Circular correlation ----------------------------------------------------

library(Directional) # run in r4.4.0

maxtemp.cor = maxtemp %>% 
  drop_na(paidir, xyAng, upslope.dir) %>% 
  group_by(scale, resolution, var) %>% 
  filter_all(all_vars(!is.infinite(.))) %>% 
  summarise(paicor.pval = circ.cor1(xyAng, paidir, rads = F)[2],
            paicor.rho = circ.cor1(xyAng, paidir, rads = F)[1],
            upslopecor.pval = circ.cor1(xyAng, upslope.dir, rads = F)[2],
            upslopecor.rho = circ.cor1(xyAng, upslope.dir, rads = F)[1])

mintemp.cor = mintemp %>% 
  drop_na(paidir, xyAng, upslope.dir) %>% 
  group_by(scale, resolution, var) %>% 
  filter_all(all_vars(!is.infinite(.))) %>% 
  summarise(paicor.pval = circ.cor1(xyAng, paidir, rads = F)[2],
            paicor.rho = circ.cor1(xyAng, paidir, rads = F)[1],
            upslopecor.pval = circ.cor1(xyAng, upslope.dir, rads = F)[2],
            upslopecor.rho = circ.cor1(xyAng, upslope.dir, rads = F)[1])

df = bind_rows(maxtemp, mintemp)

upslope.angdif.summ = df %>% 
  mutate(ang.up = cut(upslope.angdif, breaks = seq(0,180,6), labels = seq(6,180,6),
                      include.lowest = T)) %>% 
  group_by(scale, resolution, var, ang.up) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n),
         ang.up = as.numeric(as.character(ang.up)))

pai.angdif.summ = df %>% 
  mutate(ang.pai = cut(pai.angdif, breaks = seq(0,180,6), labels = seq(6,180,6),
                       include.lowest = T)) %>% 
  group_by(scale, resolution, var, ang.pai) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n),
         ang.pai = as.numeric(as.character(ang.pai)))



# z angle -----------------------------------------------------------------


df.3d = df %>% 
  filter(scale == "Within-canopy")

df.3d = df.3d %>% 
  group_by(resolution, var, scale) %>%
  mutate(xy_length = abs(cos(zAng*pi/180)), 
         z_length = abs(sin(zAng*pi/180)),
         xyz_ratio = z_length/xy_length,
         resolution = factor(resolution, levels = c("1km", "100m", "20m")))
df3d.summ = df.3d %>% 
  reframe(n = n(),
          perc_down_vect = sum(zAng < 1)/n*100) # percentage of downward pointing vectors (zAng < 0)
write.csv(df3d.summ, "data/dataframes/zAngle_summary.csv", row.names = F)


# Takes a long time to plot
# ggplot(df.3d, aes(x = log(xyz_ratio), y = relhgt)) +
#   geom_point(pch = ".", alpha = 0.5) +
#   #coord_cartesian(xlim = c(10, 10)) +
#   geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
#   scale_x_continuous("ln(vertical length/horizontal length)") +
#   scale_y_continuous("Relative canopy height") +
#   facet_grid(cols = vars(resolution), rows = vars(var)) +
#   theme_classic()
# 
# ggsave("scripts/03_analysis/00_plots/supplemental_figs/vert_to_horiz_ratio.png", width = 7, height = 4)


# Figure 4 ----------------------------------------------------------------

upslope.angdif.summ = upslope.angdif.summ %>% 
  mutate(dif = "Higher elevation") %>% 
  rename(ang = ang.up)

pai.angdif.summ = pai.angdif.summ %>% 
  mutate(dif = "Denser vegetation") %>% 
  rename(ang = ang.pai)

df = rbind(upslope.angdif.summ, pai.angdif.summ) %>% 
  mutate(dif = factor(dif, levels = c("Higher elevation", "Denser vegetation")),
         var = case_when(var == "maxtemp" ~ "Maximum temperature",
                         var == "mintemp" ~ "Minimum temperature"),
         scale = case_when(scale == "Macro" ~ "Free-air",
                           scale == "Topo" ~ "Free-air",
                           .default = scale),
         resolution = factor(resolution, levels = c("1km", "100m", "20m")))

p = ggplot(df, aes(x = ang, y = freq, color = scale)) +
  geom_line(stat = "identity", linewidth = 0.75) + 
  scale_x_continuous("Angular difference", breaks = seq(0,180,45)) +
  scale_y_continuous("Proportion") +
  facet_nested(resolution ~ var + dif, scales = "free_y") +
  #scale_color_discrete_qualitative(palette = "Dark 3") +
  scale_color_manual(values = c("royalblue4", "royalblue2", "steelblue1")) +
  theme_classic() +
  theme(panel.background = element_rect(color = "black", fill = NA),
        legend.title = element_blank(),
        legend.position = "bottom")

svg('scripts/03_analysis/00_plots/new_figs/fig04_voccangles.svg', width = 7, height = 5)
p
dev.off()



# vertical space ----------------------------------------------------------
# how long does it take to run out of vertical space

# calculate distance travelled in x, y, and z directions after 55 years

# micro3d
# micro3d = fread("03_analysis/00_dataframes/micro3d_canopy_dataframe.csv")
# 
# micro3d[, m55 := vocc*55]
# micro3d[, x55 := m55*cos(zAng*pi/180)*sin(xyAng*pi/180)]
# micro3d[, y55 := m55*cos(zAng*pi/180)*cos(xyAng*pi/180)]
# micro3d[, z55 := m55*sin(zAng*pi/180)]
# micro3d = micro3d %>% 
#   drop_na()
# 
# micro3d.thresh = quantile(micro3d$vocc, probs = 0.95)
# micro3d = micro3d[vocc < micro3d.thresh, ]
# 
# micro3d.down = micro3d[z55 < 0, ]
# maxdown = round(abs(min(micro3d.down$z55)))
# mindown = round(abs(max(micro3d.down$z55)))
# mediandown = round(abs(median(micro3d.down$z55)))
# 
# sum(abs(micro3d.down$z55) > micro3d.down$height)/nrow(micro3d.down)
