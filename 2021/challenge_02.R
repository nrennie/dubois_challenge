library(tidyverse)
library(ggpubr)
library(extrafont)
library(pBrackets)
library(grid)
tuesdata <- tidytuesdayR::tt_load('2021-02-16')
conjugal <- tuesdata$conjugal
data_long <- gather(conjugal, status, measurement, Single:`Divorced and Widowed`, factor_key=TRUE)
data_long$status <- factor(data_long$status, levels=c("Single", "Married", "Divorced and Widowed"))
data_long$Population <- factor(data_long$Population, levels=rev(c("German", "Negroes")))

p1 <- ggplot(data=filter(data_long, Age == "15-40"), aes(x=Population, y=measurement, fill=status)) +
  geom_bar(stat="identity", width = 0.5) + 
  labs(title="\n\n") +
  scale_fill_manual("", values=c("Single"="red3", "Married"="gold2", "Divorced and Widowed"=alpha("darkgreen",0.6))) +
  scale_y_reverse(limits=c(180,0)) +
  geom_text(aes(x=c(2,2,2,1,1,1), y=c(68,20.3,0.6,79,32,3), label=c("62.1%", "37.3%", "0.6%", "41%", "54%", "5%")), family="Lucida Sans Typewriter", colour=alpha("black",0.7), size=3) +
  annotate("text", x = 1, y = 120, label = "NEGROES", colour = alpha("black",0.7), size=4, family="Lucida Sans Typewriter") +
  annotate("text", x = 0.73, y = 120, label = "15-40", colour = alpha("black",0.7), size=2, family="Lucida Sans Typewriter") +
  annotate("text", x = 2, y = 120, label = "GERMANY", colour = alpha("black",0.7), size=4, family="Lucida Sans Typewriter") +
  annotate("text", x = 1.5, y = 160, label = "15-40", colour = alpha("black",0.7), size=4, family="Lucida Sans Typewriter") +
  annotate("text", x = 2, y = 155, label = "AGE", colour = alpha("black",0.7), size=4, family="Lucida Sans Typewriter") +
  guides(fill=guide_legend(ncol=2)) +
  coord_flip() + 
  theme(panel.background = element_rect(fill = "wheat"),
        plot.background = element_rect(fill = "wheat"),
        legend.background = element_rect(fill = "wheat"),
        plot.title = element_text(colour = "black", size=12, face="bold", hjust = 0.5),
        plot.subtitle = element_text(colour = "black", size=10, face="bold", hjust = 0.5),
        legend.title = element_blank(),
        legend.position="none",
        plot.margin = unit(c(1.5, 0.5, 0, 1), "cm"), #top, right, bottom, left
        legend.key = element_rect(colour = "black"),
        legend.spacing.x = unit(0.5,"cm"),
        legend.text = element_text(colour="black", size=12),
        axis.title.x= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )
p1

p2 <- ggplot(data=filter(data_long, Age == "40-60"), aes(x=Population, y=measurement, fill=status)) +
  geom_bar(stat="identity", width = 0.5) + 
  labs(title="\n\n") +
  scale_fill_manual("", values=c("Single"="red3", "Married"="gold2", "Divorced and Widowed"=alpha("darkgreen",0.6))) +
  scale_y_reverse(limits=c(180,0)) +
  geom_text(aes(x=c(2,2,2,1,1,1), y=c(93,48,4.5,94,56,11), label=c("9.6%", "84.8%", "5.6%", "4.5%", "73.5%", "22%")), family="Lucida Sans Typewriter", colour=alpha("black",0.7), size=3) +
  annotate("text", x = 1, y = 120, label = "NEGROES", colour = alpha("black",0.7), size=4, family="Lucida Sans Typewriter") +
  annotate("text", x = 0.75, y = 120, label = "40-60", colour = alpha("black",0.7), size=2, family="Lucida Sans Typewriter") +
  annotate("text", x = 2, y = 120, label = "GERMANY", colour = alpha("black",0.7), size=4, family="Lucida Sans Typewriter") +
  annotate("text", x = 1.5, y = 160, label = "40-60", colour = alpha("black",0.7), size=4, family="Lucida Sans Typewriter") +
  guides(fill=guide_legend(ncol=2)) +
  coord_flip() + 
  theme(panel.background = element_rect(fill = "wheat"),
        plot.background = element_rect(fill = "wheat"),
        legend.background = element_rect(fill = "wheat"),
        plot.title = element_text(colour = "black", size=12, face="bold", hjust = 0.5),
        plot.subtitle = element_text(colour = "black", size=10, face="bold", hjust = 0.5),
        legend.title = element_blank(),
        legend.position="none",
        plot.margin = unit(c(0, 0.5, 1, 1), "cm"), #top, right, bottom, left
        legend.key = element_rect(colour = "black"),
        legend.spacing.x = unit(0.5,"cm"),
        legend.text = element_text(colour="black", size=12),
        axis.title.x= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )
p2

p3 <- ggplot(data=filter(data_long, Age == "60 and over"), aes(x=Population, y=measurement, fill=status)) +
  geom_bar(stat="identity", width = 0.5) + 
  labs(title="\n\n") +
  scale_fill_manual("", values=c("Single"="red3", "Married"="gold2", "Divorced and Widowed"=alpha("darkgreen",0.6))) +
  scale_y_reverse(limits=c(180,0)) +
  geom_text(aes(x=c(2,2,2,1,1,1), y=c(93,60,14,94,68,20), label=c("8.2%", "62.2%", "29.2%", "4.5%", "54.5%", "41%")), family="Lucida Sans Typewriter", colour=alpha("black",0.7), size=3) +
  annotate("text", x = 1, y = 120, label = "NEGROES", colour = alpha("black",0.7), size=4, family="Lucida Sans Typewriter") +
  annotate("text", x = 0.73, y = 120, label = "60 AND OVER", colour = alpha("black",0.7), size=2, family="Lucida Sans Typewriter") +
  annotate("text", x = 2, y = 120, label = "GERMANY", colour = alpha("black",0.7), size=4, family="Lucida Sans Typewriter") +
  annotate("text", x = 1.5, y = 160, label = "60\nAND\nOVER", colour = alpha("black",0.7), size=4, family="Lucida Sans Typewriter") +
  guides(fill=guide_legend(ncol=2)) +
  coord_flip() + 
  theme(panel.background = element_rect(fill = "wheat"),
        plot.background = element_rect(fill = "wheat"),
        legend.background = element_rect(fill = "wheat"),
        plot.title = element_text(colour = "black", size=12, face="bold", hjust = 0.5),
        plot.subtitle = element_text(colour = "black", size=10, face="bold", hjust = 0.5),
        legend.title = element_blank(),
        legend.position="none",
        plot.margin = unit(c(-0.5, 0.5, 2, 1), "cm"), #top, right, bottom, left
        legend.key = element_rect(colour = "black"),
        legend.spacing.x = unit(0.5,"cm"),
        legend.text = element_text(colour="black", size=12),
        axis.title.x= element_blank(),
        axis.title.y= element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  )
p3

#dev.new(width=5,height=6.25,unit="in", noRStudioGD = TRUE)

p <- ggarrange(p1, p2, p3, ncol=1, nrow=3, legend="none")

p

g1 <- grid::circleGrob(gp = grid::gpar(fill = "red3"))
g2 <- grid::circleGrob(gp = grid::gpar(fill = "gold1"))
g3 <- grid::circleGrob(gp = grid::gpar(fill = "darkgreen"))

ggdraw(p) +
  draw_label("      CONJUGAL CONDITION .", x = 0.5, y = 0.95, hjust = 0.5, vjust = 0.5, fontface=2, fontfamily = "Lucida Sans Typewriter", color = alpha("black",0.7), size = 11) +
  draw_grob(g1, x=-0.16, y=0.41, scale = 0.04) +
  draw_grob(g2, x=-0.16, y=0.35, scale = 0.04) +
  draw_grob(g3, x=0.16, y=0.38, scale = 0.04) +
  draw_label("SINGLE", x = 0.38, y = 0.91, hjust = 0, vjust = 0.5, fontfamily = "Lucida Sans Typewriter", color = alpha("black",0.7), size = 7) +
  draw_label("MARRIED", x = 0.38, y = 0.85, hjust = 0, vjust = 0.5, fontfamily = "Lucida Sans Typewriter", color = alpha("black",0.7), size = 7) +
  draw_label("WIDOWED AND DIVORCED", x = 0.80, y = 0.88, hjust = 0.5, vjust = 0.5, fontfamily = "Lucida Sans Typewriter", color = alpha("black",0.7), size = 7) 
  

#grid.locator(unit="native") 

grid.brackets(160, 190, 160, 120, lwd=0.5, col=alpha("black",0.7)) 
grid.brackets(160, 340, 160, 270, lwd=0.5, col=alpha("black",0.7)) 
grid.brackets(160, 510, 160, 440, lwd=0.5, col=alpha("black",0.7)) 




