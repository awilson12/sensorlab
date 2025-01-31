library(readxl)
AER_box <- read_excel("AER_boxplot.xlsx")
View(AER_box)

library(tidyverse)
library(hrbrthemes)
library(viridis)

A<-ggplot(AER_box, aes(x=Season, y=AER, fill=Sensor))+
  geom_boxplot() +
  scale_fill_viridis(discrete=TRUE, alpha=0.6) +
  theme_minimal()+
  theme (
    legend.position="right",
    plot.title= element_text(size=11)
  )+
  ggtitle("Air Exchange rate by season")+
  xlab("Season")+
  ylab("Air Exchange Rate (AER)")
A

ggsave("Air exchange rate_sensor.tiff", units="in", width=5, height=4, dpi=300)

B<-ggplot(AER_box, aes(x=Season, y=AER, fill=Season))+
  geom_boxplot() +
  scale_fill_viridis(discrete=TRUE, alpha=0.6) +
  theme_minimal()+
  theme (
    legend.position="none",
    plot.title= element_text(size=11)
  )+
  ggtitle("Air Exchange rate by season")+
  xlab("Season")+
  ylab("Air Exchange Rate (AER)")
B

ggsave("Air exchange rate_season.tiff", units="in", width=5, height=4, dpi=300)

