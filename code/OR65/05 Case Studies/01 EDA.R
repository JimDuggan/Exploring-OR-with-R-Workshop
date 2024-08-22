library(MASS)
library(ggplot2)
library(dplyr)
library(tidyr)
library(GGally)

glimpse(Boston)

bos <- Boston %>%
  dplyr::as_tibble() %>%
  dplyr::select(chas,rm,age,rad,ptratio,medv,nox) %>%
  dplyr::rename(PTRatio=ptratio,
                ByRiver=chas,
                Rooms=rm,
                Age=age,
                Radial=rad,
                HomesValue=medv,
                Nox=nox) %>%
  dplyr::mutate(ByRiver=as.logical(ByRiver))
bos

p3 <- ggpairs(dplyr::select(bos,-ByRiver),progress = F)+
  theme_light()+
  theme(axis.text.x = element_text(size=6),
        axis.text.y = element_text(size=6))
p3


bos_long <- bos %>%
  tidyr::pivot_longer(names_to = "Indicator",
                      values_to = "Value",
                      -ByRiver)
bos_long

p5 <- ggplot(bos_long,aes(x=ByRiver,y=Value,colour=ByRiver))+
  geom_boxplot()+
  facet_wrap(~Indicator,scales="free")+
  theme(legend.position = "top")
p5

