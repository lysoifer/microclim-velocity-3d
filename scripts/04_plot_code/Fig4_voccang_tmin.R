source("scripts/03_analysis/02_voccAngles/voccAngles_tmin.R")

upslope.angdif.summ = pts.scale %>% 
  #filter(scale == "Macro") %>% 
  #group_by(scale) %>% 
  mutate(ang.up = cut(upslope.angdif, breaks = seq(0,180,6), labels = seq(6,180,6),
                      include.lowest = T)) %>% 
  group_by(scale, ang.up) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n),
         ang.up = as.numeric(as.character(ang.up)))

upslope.angdif.plt = ggplot(upslope.angdif.summ, aes(x = ang.up, y = freq)) +
  geom_bar(stat = "identity") + 
  scale_x_continuous("Angular difference between direction of climate velocity\n and direction towards higher elevation", breaks = seq(0,180,45)) +
  scale_y_continuous("Proportion") +
  facet_wrap(~scale, nrow = 1) +
  theme_classic()

pai.angdif.summ = pts.scale %>% 
  mutate(ang.pai = cut(pai.angdif, breaks = seq(0,180,6), labels = seq(6,180,6),
                       include.lowest = T)) %>% 
  group_by(scale, ang.pai) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n),
         ang.pai = as.numeric(as.character(ang.pai)))

pai.angdif.plt = ggplot(pai.angdif.summ, aes(x = ang.pai, y = freq)) +
  geom_bar(stat = "identity") + 
  scale_x_continuous("Angular difference between direction of climate velocity\n and direction towards denser vegetation", breaks = seq(0,180,45)) +
  scale_y_continuous("Proportion") +
  facet_wrap(~scale, nrow = 1) +
  theme_classic()

p = upslope.angdif.plt + pai.angdif.plt + plot_layout(nrow = 2) + plot_annotation(tag_levels = "a")

ggsave('scripts/03_analysis/00_plots/vocc_direction/vocc_direction_compare3_tmin.png', width = 8, height = 5)

