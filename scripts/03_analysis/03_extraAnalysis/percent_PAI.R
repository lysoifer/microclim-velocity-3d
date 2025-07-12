# Calculate the percent of PAI above a threshold
# Exploratory analysis of PAI

library(tidyverse)
library(data.table)
library(geoR)
library(segmented)

vocc.df = fread("data/dataframes/analysis_dataframe_full_tempbio5.csv") %>% 
  mutate(scale = case_when(scale == "Land-surface" ~ "Land\nsurface",
                           scale == "Within-canopy" ~ "Within-\ncanopy",
                           .default = scale),
         scale = factor(scale, levels = c("Macro", "Topo", "Land\nsurface", "Within-\ncanopy")))


# Is there a clear threshold for offset of max temps in Trinidad
vocc.df.micro2d = vocc.df %>% 
  filter(resolution == "20m" & scale == "Land\nsurface" & var == "maxtemp")
ggplot(vocc.df.micro2d, aes(x = maxtemp.pres, y= pai)) +
  geom_point(pch = ".") +
  theme_classic()

ggplot(vocc.df.micro2d, aes(x = elev, y= pai)) +
  geom_point(pch = ".") +
  theme_classic()

# piecewise regression to identify cutoff point where adding PAI doesn't increase buffering

# subsample to reduce time
vocc.df.micro2d.sub = vocc.df.micro2d %>% 
  slice_sample(n = 100000)

m1 = lm(maxtemp.pres ~ pai, data = vocc.df.micro2d.sub)
m1.seg = segmented(m1, seg.Z = ~pai, psi = 2.4)
print(m1.seg)
summary(m1.seg)

preddat = data.frame(pai = seq(0,10,0.1))
preddat$pred = predict(m1.seg, newdata = preddat)

ggplot() +
  geom_point(data = vocc.df.micro2d.sub, aes(x = pai, y= maxtemp.pres), pch = ".") +
  geom_line(data = preddat, aes(pai, pred), color = "red2") +
  theme_classic()

resid = resid(m1.seg)
plot(vocc.df.micro2d.sub$pai, resid)
plot(vocc.df.micro2d.sub$maxtemp.pres, resid)

vocc.df.micro2d.sub$log_maxtemp.pres = log(vocc.df.micro2d.sub$maxtemp.pres)
plot(vocc.df.micro2d.sub$pai, vocc.df.micro2d.sub$log_maxtemp.pres)

m1 = glm(maxtemp.pres ~ pai, data = vocc.df.micro2d.sub, family = Gamma(link = "log"))
pred = predict(m1, type = "response")
vocc.df.micro2d.sub$pred = pred
ggplot(vocc.df.micro2d.sub) +
  geom_point(aes(x = pai, y = maxtemp.pres), pch = ".") +
  geom_line(aes(x = pai, y = pred))

# find range of spatial autocorrelation
geoR_pai = as.geodata(vocc.df.micro2d.sub,
                      coords.col = 1:2,
                      data.col = 14)

template = ag

sum(vocc.df.micro2d$pai>2.5)/nrow(vocc.df.micro2d) * 100

pai = rast("data/PAI/pai_NRange_rmMeters0to2_rmPAIgt10.tif")
plot(pai > 3.5)


