library(tidyverse)
library(ggplot2)
library(extrafont)
tuesdata <- tidytuesdayR::tt_load('2021-02-16')
furniture <- tuesdata$furniture

theta1 <- seq(2.7,4.5*pi,0.01)
r1 <- 0.5 + 0.5*theta1
df1 <- data.frame(x=r1*cos(theta1), y=r1*sin(theta1)) 

theta2 <- seq(4.6,4.5*pi,0.01)
r2 <- 1 + 0.5*theta2
df2 <- data.frame(x=r2*cos(theta2), y=r2*sin(theta2)) 

theta3 <- seq(6.5,4.5*pi,0.01)
r3 <- 1.5 + 0.5*theta3
df3 <- data.frame(x=r3*cos(theta3), y=r3*sin(theta3)) 

theta4 <- seq(10,4.5*pi,0.01)
r4 <- 2 + 0.5*theta4
df4 <- data.frame(x=r4*cos(theta4), y=r4*sin(theta4)) 

theta5 <- seq(11.5,4.5*pi,0.01)
r5 <- 2.5 + 0.5*theta5
df5 <- data.frame(x=r5*cos(theta5), y=r5*sin(theta5)) 

theta6 <- seq(14,4.5*pi,0.01)
r6 <- 3 + 0.5*theta6
df6 <- data.frame(x=r6*cos(theta6), y=r6*sin(theta6)) 

p <- ggplot() + 
  geom_point(data=df1, aes(x,y), col='red3', size=4) +
  geom_point(data=df2, aes(x,y), col='gray75', size=4) +
  geom_point(data=df3, aes(x,y), col='gold3', size=4) +
  geom_point(data=df4, aes(x,y), col='gray60', size=4) +
  geom_point(data=df5, aes(x,y), col='lightskyblue3', size=4) +
  geom_point(data=df6, aes(x,y), col='lightpink1', size=4) +
  labs(title="ASSESSED VALUE OF HOUSEHOLD AND KITCHEN FURNITURE\nOWNED BY GEORGIA NEGORES.\n\n") +
  xlim(-10.5,10.5) + ylim(-10.5,10.5) +
  geom_text(data=data.frame(x=rep(-4.5,6), y=rev(seq(7.6,10.1,0.5))), 
                             label=c("1875", "1880", "1885", "1890", "1895", "1899"), 
             aes(x=x, y=y, label=label), family="Lucida Sans Typewriter", size=2.5, hjust=0, colour="black") + 
  geom_text(data=data.frame(x=rep(-0.4,6), y=rev(seq(7.6,10.1,0.5))), 
            label=c("----- $ 21,186", "---- $ 498,532", "---  $ 736,170", "-- $ 1,173,624", "-- $ 1,322,694", "-- $ 1,434,975"), 
            aes(x=x, y=y, label=label), family="Lucida Sans Typewriter", size=2.5, hjust=1, colour="black") + 
  theme(panel.background = element_rect(fill = "#d0bba8"),
        plot.background = element_rect(fill = "#d0bba8"),
        legend.background = element_rect(fill = "#d0bba8"),
        plot.title = element_text(colour = "black", size=12, face="bold", hjust = 0.5, family="Lucida Sans Typewriter"),
        plot.subtitle = element_text(colour = "black", size=10, face="bold", hjust = 0.5, family="Lucida Sans Typewriter"),
        legend.title = element_blank(),
        legend.position="none",
        plot.margin = unit(c(0.7, 0.1, 2, 0.1), "cm"), #top, right, bottom, left
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

#dev.new(width=6,height=8,unit="in", noRStudioGD = TRUE)
ggsave(p, filename = "challenge_07.jpg",  bg = "transparent", height=8, width=6, unit="in")
