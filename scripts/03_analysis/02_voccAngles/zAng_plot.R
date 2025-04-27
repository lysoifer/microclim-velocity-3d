library(tidyverse)
library(colorspace)
library(terra)
library(data.table)
library(tidyterra)
library(viridis)

tmax = fread("scripts/03_analysis/00_dataframes/analysis_dataframe_3d_full_maxtemp.csv")
tmin = fread("scripts/03_analysis/00_dataframes/analysis_dataframe_3d_full_mintemp.csv")

tmax = tmax %>% 
  mutate(resolution = factor(resolution, levels = c("1km", "100m", "20m"))) %>% 
  rename(temp.pres = maxtemp.pres)
tmin = tmin %>% 
  mutate(resolution = factor(resolution, levels = c("1km", "100m", "20m"))) %>% 
  rename(temp.pres = mintemp.pres)

df = bind_rows(tmax, tmin)
df$relhgt_cat = as.numeric(as.character(cut(df$relhgt, breaks = seq(0.5,1,0.05), labels = seq(0.5,0.95,0.05))))

# only upward directed vectors
df.up = df %>% filter(zAng > 0)

df$dir = "All vectors"
df.up$dir = "Upward vectors"

df = bind_rows(df, df.up)
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
  ggplot(aes(y=temp,x=relhgt_cat, color = var, linetype = dir, pch = dir)) + 
  geom_line(linewidth = 0.7) +
  geom_errorbar(aes(ymin = temp - temp.sd, ymax = temp + temp.sd), width = 0.01) +
  geom_point(alpha = 1) +
  #geom_smooth(method = "lm") +
  #scale_color_viridis() +
  facet_wrap(vars(resolution)) +
  scale_y_continuous("Temperature (\u00b0C)", breaks = seq(24,40,2)) +
  scale_x_continuous("Relative height") +
  coord_flip(xlim = c(0.5,1)) +
  theme_classic() + 
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.background = element_rect(color = "black", fill = NA))

png("scripts/03_analysis/00_plots/supplemental_figs/vert_temp_gradient.png", width = 180, height = 80, units = "mm", res = 300)
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

df %>% 
  filter(dir == "All vectors") %>% 
  mutate(updown = ifelse(zAng<0, "Down", "Up"),
         var = case_when(var == "maxtemp" ~ "Maximum\ntemperature",
                         var == "mintemp" ~ "Minimum\ntemperature")) %>%
  ggplot(aes(updown, pai)) +
  geom_jitter(alpha = 0.2, pch = ".", color = "gray") +
  geom_boxplot(fill = NA) +
  facet_grid(rows = vars(var), cols = vars(resolution)) +
  scale_y_continuous("PAI") +
  scale_x_discrete("Vertical direction") +
  theme_bw() +
  theme(panel.grid = element_blank())

ggsave("scripts/03_analysis/00_plots/supplemental_figs/zAng~pai.png", width = 180, height = 120, units = "mm", dpi = 300)


t.test(pai~zAng<0, data = tmax %>% filter(resolution == "1km"))
t.test(pai~zAng<0, data = tmax %>% filter(resolution == "100m"))
t.test(pai~zAng<0, data = tmax %>% filter(resolution == "20m"))

t.test(pai~zAng<0, data = tmin %>% filter(resolution == "1km"))
t.test(pai~zAng<0, data = tmin %>% filter(resolution == "100m"))
t.test(pai~zAng<0, data = tmin %>% filter(resolution == "20m"))





