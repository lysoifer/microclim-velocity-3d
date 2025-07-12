library(tidyverse)
library(colorspace)
library(terra)
library(data.table)
library(tidyterra)
library(viridis)

#tmax = fread("data/dataframes/analysis_dataframe_full_maxtemp.csv")
tmax = fread("data/dataframes/analysis_dataframe_full_tempbio5.csv")
tmin = fread("data/dataframes/analysis_dataframe_full_mintemp.csv")


tmax = tmax %>% 
  filter(scale == "Within-canopy") %>% 
  mutate(resolution = factor(resolution, levels = c("1km", "100m", "20m"))) %>% 
  rename(temp.pres = maxtemp.pres)
tmin = tmin %>% 
  filter(scale == "Within-canopy") %>% 
  mutate(resolution = factor(resolution, levels = c("1km", "100m", "20m"))) %>% 
  rename(temp.pres = mintemp.pres)

df = bind_rows(tmax, tmin)
df$relhgt_cat = as.numeric(as.character(cut(df$relhgt, breaks = seq(0.5,1,0.05),
                                            labels = seq(0.5,0.95,0.05))))

# only upward directed vectors
df.up = df %>% filter(zAng > 0)
df.down = df %>% filter(zAng < 0)

#df$dir = "All vectors"
df.up$dir = "Upward vectors"
df.down$dir = "Downward vectors"

df = bind_rows(df.down, df.up)
df$horizontal_dist = abs(df$vocc) * cos(abs(df$zAng) * pi / 180)
df$vertical_dist = abs(df$vocc) * sin(abs(df$zAng) * pi / 180)

tmax %>% 
  group_by(resolution) %>%
  drop_na(zAng) %>% 
  summarise(sum(zAng<0)/n())

tmin %>% 
  group_by(resolution) %>% 
  drop_na(zAng) %>% 
  summarise(n = n(),
            down = sum(zAng < 0),
            prop = sum(zAng<0)/n())


tmax %>% 
  group_by(resolution, var) %>% 
  slice_sample(n=5000) %>% 
  ggplot(aes(x=pai,y=relhgt, color = zAng)) + 
  geom_point(alpha = 0.3) +
  scale_color_viridis() +
  facet_wrap(vars(resolution)) +
  theme_classic()

df %>% 
  group_by(resolution, var) %>% 
  slice_sample(n=10000) %>% 
  ggplot(aes(x=temp.pres,y=relhgt)) + 
  geom_point(alpha = 0.3, pch = ".") +
  geom_smooth(method = "lm") +
  scale_color_viridis() +
  facet_grid(cols = vars(resolution), rows = vars(var)) +
  coord_cartesian(ylim = c(0.5,1)) +
  theme_classic()

# plot vertical temp gradients for all vectors and upward directed vectors
p = df %>% 
  group_by(resolution, relhgt_cat, var, dir) %>% 
  summarise(temp = mean(temp.pres, na.rm = T),
            temp.sd = sd(temp.pres, na.rm = T)) %>% 
  arrange(var, resolution, relhgt_cat) %>% 
  mutate(var = case_when(var == "mintemp" ~ "Minimum temperature",
                         var == "maxtemp" ~ "Maximum temperature")) %>% 
  rename("Temperature (\u00b0C)" = temp) %>% 
  ggplot(aes(y=.data[["Temperature (\u00b0C)"]],x=relhgt_cat, color = var, linetype = dir, pch = dir)) + 
  geom_line(linewidth = 0.7) +
  geom_errorbar(aes(ymin = .data[["Temperature (\u00b0C)"]] - temp.sd, ymax = .data[["Temperature (\u00b0C)"]] + temp.sd), width = 0.01) +
  geom_point(alpha = 1) +
  scale_color_manual(values = c("red3", "deepskyblue1")) +
  guides(color = "none") +
  #geom_smooth(method = "lm") +
  #scale_color_viridis() +
  facet_grid2(cols = vars(resolution), rows = vars(var), scales = "free_x", independent = "x") +
  scale_y_continuous("Temperature (\u00b0C)") +
  scale_x_continuous("Relative height") +
  coord_flip(xlim = c(0.5,1)) +
  theme_classic() + 
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.background = element_rect(color = "black", fill = NA))

p + 
  facetted_pos_scales(y = list(var == "Minimum temperature" ~ scale_y_continuous(limits = c(22.5,24.5), breaks = seq(23,24.5,0.5)),
                          var == "Maximum temperature" ~ scale_y_continuous(limits = c(29.5,34), breaks = seq(30,34,1))))

png("scripts/03_analysis/00_plots/supplemental_figs/vert_temp_gradient_bio5.png", width = 180, height = 140, units = "mm", res = 300)
p
dev.off()

# calculate proportion of vectors with larger vertical than horizontal distance
df %>% 
  filter(dir == "All vectors") %>% 
  group_by(resolution, var) %>% 
  summarise(sum(vertical_dist > horizontal_dist)/n())

tmax %>% 
  filter(zAng > 0) %>% 
  group_by(resolution) %>% 
  slice_sample(n=5000) %>% 
  ggplot(aes(x=maxtemp.pres,y=relhgt)) + 
  geom_point(alpha = 0.3) +
  geom_smooth() +
  scale_color_viridis() +
  facet_wrap(vars(resolution)) +
  theme_classic()


tmax100 = tmax %>%
  filter(resolution == "100m") %>% 
  rast()
crs(tmax100) = "epsg:2067"

tmax1km = tmax %>%
  filter(resolution == "1km") %>% 
  rast()
crs(tmax1km) = "epsg:2067"

tmin1km = tmin %>%
  filter(resolution == "1km") %>% 
  rast()
crs(tmin1km) = "epsg:2067"


tmax20m = tmax %>%
  filter(resolution == "20m") %>% 
  rast()
crs(tmax20m) = "epsg:2067"

ggplot() +
  geom_spatraster(data = tmax100, aes(fill = zAng)) +
  theme_classic()

ggplot() +
  geom_spatraster(data = tmax100, aes(fill = pai)) +
  theme_classic()

ggplot() +
  geom_spatraster(data = tmax1km, aes(fill = zAng)) +
  theme_classic()


ggplot() +
  geom_spatraster(data = tmax100, aes(fill = tempgrad)) +
  theme_classic()


df %>% 
  filter(var == "maxtemp" & resolution == "100m" & dir == "All vectors") %>% 
  ggplot(aes(zAng, pai)) +
  geom_point(alpha = 0.2, pch = ".") +
  #geom_boxplot(aes(zAng<0, pai)) +
  theme_bw() +
  theme(panel.grid = element_blank())

mns = df %>% 
  mutate(updown = ifelse(zAng<0, "Down", "Up"),
         var = case_when(var == "maxtemp" ~ "Maximum\ntemperature",
                         var == "mintemp" ~ "Minimum\ntemperature")) %>% 
  group_by(updown, var, resolution) %>% 
  summarise(pai.mean = mean(pai, na.rm = T))

library(ungeviz)
df %>% 
  mutate(updown = ifelse(zAng<0, "Down", "Up"),
         var = case_when(var == "maxtemp" ~ "Maximum\ntemperature",
                         var == "mintemp" ~ "Minimum\ntemperature")) %>%
  ggplot(aes(updown, pai, fill = var)) +
  #geom_jitter(alpha = 0.2, pch = ".", color = "gray") +
  #geom_boxplot(fill = NA, outliers = F) +
  geom_violin(alpha = 0.5, show.legend = F) +
  geom_hpline(data = mns, aes(updown, pai.mean)) +
  facet_grid(rows = vars(var), cols = vars(resolution)) +
  scale_fill_manual(values = c("red3", "deepskyblue1")) +
  scale_y_continuous("PAI") +
  scale_x_discrete("Vertical direction") +
  theme_bw() +
  theme(panel.grid = element_blank())

ggsave("scripts/03_analysis/00_plots/supplemental_figs/zAng~pai_bio5.png", width = 180, height = 120, units = "mm", dpi = 300)


t.test(pai~zAng<0, data = tmax %>% filter(resolution == "1km"))
t.test(pai~zAng<0, data = tmax %>% filter(resolution == "100m"))
t.test(pai~zAng<0, data = tmax %>% filter(resolution == "20m"))

t.test(pai~zAng<0, data = tmin %>% filter(resolution == "1km"))
t.test(pai~zAng<0, data = tmin %>% filter(resolution == "100m"))
t.test(pai~zAng<0, data = tmin %>% filter(resolution == "20m"))




