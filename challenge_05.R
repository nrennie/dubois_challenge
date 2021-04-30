library(tidyverse)
library(ggpubr)
library(extrafont)
library(pBrackets)
library(grid)
library(cowplot)

tuesdata <- tidytuesdayR::tt_load('2021-02-16')
income <- tuesdata$income
data_long <- gather(income, type, measurement, Rent:Other, factor_key=TRUE)
data_long$type <- factor(data_long$type, levels=c("Rent", "Food", "Clothes", "Tax", "Other"))
data_long$Class <- factor(data_long$Class, levels=rev(c("$100-200", "$200-300", "$300-400", "$400-500", "$500-750", "$750-1000", "Over $1000")))
data_long$measurement[22] <- 0.1
data_long$measurement[29] <- 9.9

p <- ggplot() +
  geom_bar(data=data_long, aes(x=Class, y=measurement, fill=type), stat="identity", width = 0.5) + 
  scale_y_reverse() +
  scale_fill_manual("", values=c("Rent"="black", "Food"="mediumorchid4", "Clothes"="lightsalmon1", "Tax"="slategray3", "Other"="gray75")) +
  #legend
  geom_segment(aes(x = 8, y = 0, xend = 8, yend = 20), col='gray75', size=6) +
  geom_segment(aes(x = 8, y = 20, xend = 8, yend = 40), col='slategray3', size=6) +
  geom_segment(aes(x = 8, y = 40, xend = 8, yend = 60), col='lightsalmon1', size=6) +
  geom_segment(aes(x = 8, y = 60, xend = 8, yend = 80), col='mediumorchid4', size=6) +
  geom_segment(aes(x = 8, y = 80, xend = 8, yend = 100), col='black', size=6) +
  #between lines
  geom_segment(aes(x = 7.25, y = 9.9, xend = 7.8, yend = 20), col='black', size=0.1) +
  geom_segment(aes(x = 6.25, y = 4, xend = 6.75, yend = 9.9), col='black', size=0.1) +
  geom_segment(aes(x = 5.25, y = 11.5, xend = 5.75, yend = 4), col='black', size=0.1) +
  geom_segment(aes(x = 4.25, y = 24.5, xend = 4.75, yend = 11.5), col='black', size=0.1) +
  geom_segment(aes(x = 3.25, y = 34, xend = 3.75, yend = 24.5), col='black', size=0.1) +
  geom_segment(aes(x = 2.25, y = 36, xend = 2.75, yend = 34), col='black', size=0.1) +
  geom_segment(aes(x = 1.25, y = 50.5, xend = 1.75, yend = 36), col='black', size=0.1) +
  #
  geom_segment(aes(x = 7.25, y = 10, xend = 7.8, yend = 40), col='black', size=0.1) +
  geom_segment(aes(x = 6.25, y = 8, xend = 6.75, yend = 10), col='black', size=0.1) +
  geom_segment(aes(x = 5.25, y = 16, xend = 5.75, yend = 8), col='black', size=0.1) +
  geom_segment(aes(x = 4.25, y = 30, xend = 4.75, yend = 16), col='black', size=0.1) +
  geom_segment(aes(x = 3.25, y = 39, xend = 3.75, yend = 30), col='black', size=0.1) +
  geom_segment(aes(x = 2.25, y = 44, xend = 2.75, yend = 39), col='black', size=0.1) +
  geom_segment(aes(x = 1.25, y = 55, xend = 1.75, yend = 44), col='black', size=0.1) +
  #
  geom_segment(aes(x = 7.25, y = 38, xend = 7.8, yend = 60), col='black', size=0.1) +
  geom_segment(aes(x = 6.25, y = 31, xend = 6.75, yend = 38), col='black', size=0.1) +
  geom_segment(aes(x = 5.25, y = 34, xend = 5.75, yend = 31), col='black', size=0.1) +
  geom_segment(aes(x = 4.25, y = 45, xend = 4.75, yend = 34), col='black', size=0.1) +
  geom_segment(aes(x = 3.25, y = 56, xend = 3.75, yend = 45), col='black', size=0.1) +
  geom_segment(aes(x = 2.25, y = 63, xend = 2.75, yend = 56), col='black', size=0.1) +
  geom_segment(aes(x = 1.25, y = 71, xend = 1.75, yend = 63), col='black', size=0.1) +
  #
  geom_segment(aes(x = 7.25, y = 81, xend = 7.8, yend = 80), col='black', size=0.1) +
  geom_segment(aes(x = 6.25, y = 78, xend = 6.75, yend = 81), col='black', size=0.1) +
  geom_segment(aes(x = 5.25, y = 77, xend = 5.75, yend = 78), col='black', size=0.1) +
  geom_segment(aes(x = 4.25, y = 82, xend = 4.75, yend = 77), col='black', size=0.1) +
  geom_segment(aes(x = 3.25, y = 87, xend = 3.75, yend = 82), col='black', size=0.1) +
  geom_segment(aes(x = 2.25, y = 100, xend = 2.75, yend = 87), col='black', size=0.1) +
  #left arrows
  geom_segment(aes(x = seq(0.5,7.5,1), y = rep(137,8), xend = seq(0.5,7.5,1), yend = rep(98,8)), col='black', size=0.3, arrow=arrow(length = unit(0.03, "npc"))) +
  geom_segment(aes(x = 7.8, y = 103, xend = 7.8, yend = 137), col='black', size=0.3) +
  geom_segment(aes(x = 0.5, y = 137, xend = 7.8, yend = 137), col='black', size=0.3) +
  geom_segment(aes(x = 0.5, y = 119, xend = 7.8, yend = 119), col='black', size=0.3) +
  geom_segment(aes(x = 0.5, y = 103, xend = 7.8, yend = 103), col='black', size=0.3) +
  #add percentages
  #clothes
  geom_text(data=data.frame(x=c(7:1), y=100-c(75, 80, 76, 63, 52, 45, 37), 
                            label=c("28%", "23%", "18%", "15%", "17%", "19%", "16%")), 
            aes(x=x, y=y, label=label), family="Lucida Sans Typewriter", size=3, hjust=0.5, colour="black") + 
  #tax
  geom_text(data=data.frame(x=c(6:1), y=c(6, 14, 27, 36, 40, 52), 
                            label=c("4%", "4.5%", "5.5%", "5%", "8%", "4.5%")), 
            aes(x=x, y=y, label=label), family="Lucida Sans Typewriter", size=3, hjust=0.5, colour="black") + 
  #other
  geom_text(data=data.frame(x=c(7:1), y=c(5, 2, 6, 13, 17, 18, 25), 
                            label=c("9.9%", "4%", "11.5%", "24.5%", "34%", "36%", "50.5%")), 
            aes(x=x, y=y, label=label), family="Lucida Sans Typewriter", size=3, hjust=0.5, colour="black") + 
  #food
  geom_text(data=data.frame(x=c(7:1), y=100-c(40, 42, 44, 39, 33, 20, 15), 
                           label=c("43%", "47%", "43%", "37%", "31%", "37%", "29%")), 
           aes(x=x, y=y, label=label), family="Lucida Sans Typewriter", size=3, hjust=0.5, colour="black") + 
  #rent
  geom_text(data=data.frame(x=c(7:3), y=100-c(10, 13, 12, 9, 7), 
                            label=c("19%", "22%", "23%", "18%", "13%")), 
            aes(x=x, y=y, label=label), family="Lucida Sans Typewriter", size=3, hjust=0.5, colour="white") + 
  #values
  geom_text(data=data.frame(x=c(2:7), y=rep(135,6), 
                            label=rev(c("$100-200", "$200-300", "$300-400", "$400-500", "$500-750", "$750-1000"))), 
            aes(x=x, y=y, label=label), family="Lucida Sans Typewriter", size=3, hjust=0) +
  geom_text(data=data.frame(x=1, y=129, 
                            label=("1,000\nAND OVER")), 
            aes(x=x, y=y, label=label), family="Lucida Sans Typewriter", size=3, hjust=0.5) +
  geom_text(data=data.frame(x=c(1:7), y=rep(117,7), 
                            label=rev(sapply(income$`Actual Average`, function(x) paste("$", x, sep="")))), 
            aes(x=x, y=y, label=label), family="Lucida Sans Typewriter", size=3, hjust=0) +
  labs(title="INCOME AND EXPENDITURE OF 150 NEGRO FAMILIES IN ATLANTA, GA., U.S.A.\n\n\n\n\n\n\n\n", 
       caption="FOR FURTHER STATISTICS RAISE THIS FRAME.\n") +
  #rhs labels
  geom_text(data=data.frame(x=c(1, 2.5, 4.5, 6.5), y=rep(-7,4), 
                            label=rev(c("POOR", "FAR", "CONFORTABLE", "WELL-TO-DO"))), 
            aes(x=x, y=y, label=label), family="Lucida Sans Typewriter", size=2.5, hjust=0.5, angle=90) +
  #table labels
  geom_text(data=data.frame(x=c(7.7,7.7), y=c(111,128), 
                            label=c("ACTUAL AVERAGE", "CLASS")), 
            aes(x=x, y=y, label=label), family="Lucida Sans Typewriter", size=2.5, hjust=0.5) +
  #design
  coord_flip() + 
  theme(panel.background = element_rect(fill = "#d0bba8"),
        plot.background = element_rect(fill = "#d0bba8"),
        legend.background = element_rect(fill = "#d0bba8"),
        plot.title = element_text(colour = "black", size=12, face="bold", hjust = 0.5, family="Lucida Sans Typewriter"),
        plot.subtitle = element_text(colour = "black", size=10, hjust = 0, family="Lucida Sans Typewriter"),
        plot.caption = element_text(colour = "black", size=8, hjust = 0.7, family="Lucida Sans Typewriter"),
        legend.title = element_blank(),
        legend.position="none",
        plot.margin = unit(c(0.5, 0, 0, 0.5), "cm"), #top, right, bottom, left
        legend.key = element_rect(colour = "black"),
        legend.spacing.x = unit(0.5,"cm"),
        legend.text = element_text(colour="black", size=12),
        axis.title.x= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        legend.key.width = unit(1.5,"cm"),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )
p

#grid.locator(unit="native") 
setwd("C:/Users/rennien/OneDrive - Lancaster University/Programming/git/dubois_challenge/data/images")
logo_file <- system.file("extdata", "original_05.jpg", package = "cowplot")
q <- ggdraw() +
  draw_plot(p) +
  draw_image("C:/Users/rennien/OneDrive - Lancaster University/Programming/git/dubois_challenge/data/images/original_05.jpg", 
             x = 0.1, y = 0.3, scale = .6) +
  draw_image("C:/Users/rennien/OneDrive - Lancaster University/Programming/git/dubois_challenge/data/images/original_05b.jpg", 
             x = -0.35, y = 0.3, scale = .2) 
q
grid.brackets(696, 217, 696, 286, lwd=0.5, col=alpha("black",0.7)) 
grid.brackets(696, 308, 696, 379, lwd=0.5, col=alpha("black",0.7)) 
grid.brackets(696, 398, 696, 471, lwd=0.5, col=alpha("black",0.7)) 
grid.brackets(696, 493, 696, 521, lwd=0.5, col=alpha("black",0.7)) 

#dev.new(width=8,height=6,unit="in", noRStudioGD = TRUE)
#ggsave(p, filename = "challenge_05.jpg",  bg = "transparent", height=6, width=8, unit="in")
