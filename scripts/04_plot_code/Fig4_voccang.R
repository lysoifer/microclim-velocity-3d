library(ggh4x)
source("scripts/03_analysis/02_voccAngles/voccAngles.R")
source("scripts/03_analysis/02_voccAngles/voccAngles_tmin.R")

pts.scale$var = "tmax"

pts.scale = rbind(pts.scale, tmin.pts.scale)

upslope.angdif.summ = pts.scale %>% 
  #filter(scale == "Macro") %>% 
  #group_by(scale) %>% 
  mutate(ang = cut(upslope.angdif, breaks = seq(0,180,15), labels = seq(15,180,15),
                      include.lowest = T)) %>% 
  group_by(scale, resolution, var, ang) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n),
         ang = as.numeric(as.character(ang)),
         dif = "Higher elevation")

# upslope.angdif.plt = ggplot(upslope.angdif.summ, aes(x = ang.up, ymax = freq, ymin = 0, color = scale, fill = scale)) +
#   geom_ribbon(stat = "identity", alpha = 0.1, linewidth = 1) + 
#   scale_x_continuous("Angular difference between direction of climate velocity\n and direction towards higher elevation", breaks = seq(0,180,45)) +
#   scale_y_continuous("Proportion") +
#   #facet_grid(cols = vars(resolution), rows = vars(var)) +
#   theme_classic()+
#   theme(panel.background = element_rect(color = "black", fill = NA))

pai.angdif.summ = pts.scale %>% 
  mutate(ang = cut(pai.angdif, breaks = seq(0,180,15), labels = seq(15,180,15),
                       include.lowest = T)) %>% 
  group_by(scale, resolution, var, ang) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/sum(n),
         ang = as.numeric(as.character(ang)),
         dif = "Denser vegetation")

# pai.angdif.plt = ggplot(pai.angdif.summ, aes(x = ang.pai, ymax = freq, ymin = 0, color = scale, fill = scale)) +
#   geom_ribbon(stat = "identity", alpha = 0.1, linewidth = 1) + 
#   scale_x_continuous("Angular difference between direction of climate velocity\n and direction towards denser vegetation", breaks = seq(0,180,45)) +
#   scale_y_continuous("Proportion") +
#   facet_wrap(~resolution, nrow = 1) +
#   theme_classic()
# 
# 
# p = upslope.angdif.plt + pai.angdif.plt + plot_layout(nrow = 2) + plot_annotation(tag_levels = "a")
# 
# ggsave('scripts/03_analysis/00_plots/vocc_direction/vocc_direction_compare3.png', width = 8, height = 5)
#   

df = rbind(upslope.angdif.summ, pai.angdif.summ) %>% 
  mutate(dif = factor(dif, levels = c("Higher elevation", "Denser vegetation")),
         var = case_when(var == "tmax" ~ "Maximum temperature",
                         var == "tmin" ~ "Minimum temperature"))

# p = ggplot(df, aes(x = ang, y = freq, color = scale)) +
#   geom_line(stat = "identity", linewidth = 0.75) + 
#   scale_x_continuous("Angular difference", breaks = seq(0,180,45)) +
#   scale_y_continuous("Proportion") +
#   facet_nested(var + dif ~ resolution, scales = "free_y") +
#   #scale_color_discrete_qualitative(palette = "Dark 3") +
#   scale_color_brewer(palette = "Dark2") +
#   theme_classic() +
#   theme(panel.background = element_rect(color = "black", fill = NA),
#         legend.title = element_blank(),
#         legend.position = "bottom")
# png('scripts/03_analysis/00_plots/new_figs/fig03_voccangles.png', width = 180, height = 160, res = 300, units = "mm")
# p
# dev.off()


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

svg('scripts/03_analysis/00_plots/new_figs/fig03_voccangles.svg', width = 7, height = 5)
p
dev.off()

