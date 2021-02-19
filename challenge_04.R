library(tidyverse)
library(extrafont)
tuesdata <- tidytuesdayR::tt_load('2021-02-16')
freed_slaves <- tuesdata$freed_slaves

d <- data.frame(year=freed_slaves$Year, slaves=freed_slaves$Slave, free=freed_slaves$Free, label=sapply(freed_slaves$Free, function(x) paste(x, "%", sep="")))
p04 <- ggplot(data = d, aes(year)) + 
  geom_ribbon(aes(ymin = rep(0,length(year)), ymax = slaves), fill="black") +
  geom_ribbon(aes(ymin = slaves, ymax = rep(100,length(year))), fill = "springgreen4") +
  geom_segment(aes(x=year, xend = year, y=0, yend = 100), colour=alpha("black", 0.2)) +
  ylim(0,105) + 
  labs(title="PROPORTION OF FREEMEN AND SLAVES AMONG AMERICAN NEGROES .\n\nPROPORTION DES NÈGRES LIBRES ET DES ESCLAVES EN AMÉRIQUE .\n", 
       subtitle="DONE BY ATLANTA UNIVERSITY\n") +
  geom_text(aes(x=year, y=c(slaves[1:length(slaves)-1],slaves[length(slaves)-1])+3, label=label), colour="black", size=4, fontface=2, family="Tw Cen MT Condensed") +
  geom_text(aes(x=year, y=103, label=year), colour="black", fontface=2, size=5, family="Tw Cen MT Condensed") +
  annotate("text", x = 1830, y = 55, label = "SLAVES", colour = "wheat2", size=7, fontface=2, family="Tw Cen MT Condensed") +
  annotate("text", x = 1830, y = 50, label = "ESCLAVES", colour = "wheat2", size=7, fontface=2, family="Tw Cen MT Condensed") +
  annotate("text", x = 1830, y = 96, label = "FREE - LIBRE", colour = "black", size=6, fontface=2, family="Tw Cen MT Condensed") +
  theme(panel.background = element_rect(fill = "wheat2"),
        plot.background = element_rect(fill = "wheat2"),
        legend.background = element_rect(fill = "wheat2"),
        plot.title = element_text(colour = "black", size=12, face="bold", family="Tw Cen MT Condensed", hjust = 0.5),
        plot.subtitle = element_text(colour = "black", size=8, face="bold", hjust = 0.5, family="Tw Cen MT Condensed"),
        plot.margin = unit(c(0.2, 0.2, -0.4, 0.2), "cm"), #top, right, bottom, left
        legend.title = element_blank(),
        legend.position="none",
        legend.key = element_rect(size = 1.2, colour = "#c51b8a"),
        legend.text = element_text(colour="#c51b8a", size=12, family="Tw Cen MT Condensed"),
        axis.title.x= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )
p04
setwd("C:/Users/rennien/OneDrive - Lancaster University/Programming/R/dubois_challenge/images")
ggsave(p04, filename = "challenge_04.jpg", height=6.25, width=5, unit="in")





