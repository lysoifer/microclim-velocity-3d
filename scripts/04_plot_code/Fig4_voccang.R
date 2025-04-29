# NOT NEEDED
# CODE INCORPORATED INTO VOCCANGLES

# library(ggh4x)
# source("scripts/03_analysis/02_voccAngles/voccAngles.R")
# 
# upslope.angdif.summ = upslope.angdif.summ %>% 
#   mutate(dif = "Higher elevation") %>% 
#   rename(ang = ang.up)
# 
# pai.angdif.summ = pai.angdif.summ %>% 
#   mutate(dif = "Denser vegetation") %>% 
#   rename(ang = ang.pai)
# 
# df = rbind(upslope.angdif.summ, pai.angdif.summ) %>% 
#   mutate(dif = factor(dif, levels = c("Higher elevation", "Denser vegetation")),
#          var = case_when(var == "maxtemp" ~ "Maximum temperature",
#                          var == "mintemp" ~ "Minimum temperature"),
#          scale = case_when(scale == "Macro" ~ "Free-air",
#                            scale == "Topo" ~ "Free-air",
#                            .default = scale),
#          resolution = factor(resolution, levels = c("1km", "100m", "20m")))
# 
# p = ggplot(df, aes(x = ang, y = freq, color = scale)) +
#   geom_line(stat = "identity", linewidth = 0.75) + 
#   scale_x_continuous("Angular difference", breaks = seq(0,180,45)) +
#   scale_y_continuous("Proportion") +
#   facet_nested(resolution ~ var + dif, scales = "free_y") +
#   #scale_color_discrete_qualitative(palette = "Dark 3") +
#   scale_color_manual(values = c("royalblue4", "royalblue2", "steelblue1")) +
#   theme_classic() +
#   theme(panel.background = element_rect(color = "black", fill = NA),
#         legend.title = element_blank(),
#         legend.position = "bottom")
# 
# svg('scripts/03_analysis/00_plots/new_figs/fig04_voccangles.svg', width = 7, height = 5)
# p
# dev.off()
# 
