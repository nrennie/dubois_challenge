library(tidyverse)
library(ggplot2)
library(extrafont)
tuesdata <- tidytuesdayR::tt_load('2021-02-16')
city_rural <- tuesdata$city_rural


theta <- seq(7,11.25*pi,0.01)
r <- 0.5 + 0.5*theta
df <- data.frame(x=r*cos(theta), y=r*sin(-theta)) # Cartesian coords
p <- ggplot() + geom_point(data=df, aes(x,y), col='red3', size=1.5) +
  labs(title="CITY  AND  RURAL  POPULATION.\n1890.\n\n") +
  xlim(-45,45) + ylim(-50,60) +
  geom_segment(aes(x = df[nrow(df),1], y = df[nrow(df),2], xend = 10, yend = 40), col='red3', size=1.5) +
  geom_segment(aes(x = 10, y = 40, xend = -5, yend = 55), col='gold2', size=1.5) +
  geom_segment(aes(x = -5, y = 55, xend = 0, yend = 60), col='dodgerblue4', size=1.5) +
  geom_segment(aes(x = 0.7, y = 60, xend = -35, yend = 60), col='forestgreen', size=1.5) +
  annotate("text", x=0, y=0, label="734,952", col="black", size=2) +
  annotate("text", x=-22, y=57, label="78,139 NEGROES IN CITIES\nOF OVER 10,000 INHABITANTS", col="black", size=2, hjust=0.5) +
  annotate("text", x=0, y=57, label="8,025", col="black", size=2, hjust=0, vjust=0.5) +
  annotate("text", x=5, y=57, label="NEGROES IN CITIES\nFROM 5,000 TO 10,000", col="black", size=2, hjust=0, vjust=0.5) +
  annotate("text", x=-5, y=45, label="37,699\nNEGROES\nIN CITIES\nFROM\n2,500 TO 5,000", col="black", size=2, hjust=0.5) +
  annotate("text", x=0, y=-20, label="NEGROES LIVING IN THE COUNTRY AND VILLAGES", col="black", size=2, hjust=0.5) +
  theme(panel.background = element_rect(fill = "#d0bba8"),
        plot.background = element_rect(fill = "#d0bba8"),
        legend.background = element_rect(fill = "#d0bba8"),
        plot.title = element_text(colour = "black", size=12, face="bold", hjust = 0.5),
        plot.subtitle = element_text(colour = "black", size=10, face="bold", hjust = 0.5),
        legend.title = element_blank(),
        legend.position="none",
        plot.margin = unit(c(0.6, 0.1, 0, 0.1), "cm"), #top, right, bottom, left
        legend.key = element_rect(colour = "black"),
        legend.spacing.x = unit(0.5,"cm"),
        legend.text = element_text(colour="black", size=9),
        axis.title.x= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )

p

#dev.new(width=4,height=6,unit="in", noRStudioGD = TRUE)
ggsave(p, filename = "challenge_06.jpg",  bg = "transparent", height=6, width=4, unit="in")
