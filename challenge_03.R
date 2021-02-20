library(tidyverse)
library(extrafont)
library(ggpubr)
library(cowplot)
tuesdata <- tidytuesdayR::tt_load('2021-02-16')
occupation <- tuesdata$occupation

dev.new(width=5,height=6.25,unit="in", noRStudioGD = TRUE)


d1 <- data.frame(rbind(filter(occupation, Group=="Negroes"), c("Negroes", "Dummy_var1",2*sum(filter(occupation, Group=="Negroes")$Percentage))))
d1$Perc <- as.numeric(d1$Percentage)/sum(as.numeric(d1$Percentage))
d1$occup <- factor(d1$Occupation, levels = c("Professions","Trade and Transportation", "Manufacturing and Mechanical Industries", "Domestic and Personal Service", "Agriculture, Fisheries and Mining", "Dummy_var_1"))

k <- rbind(c("Whites", "Dummy_var1",(1.5)*sum(filter(occupation, Group=="Whites")$Percentage)), c("Whites", "Dummy_var2",(0.5)*sum(filter(occupation, Group=="Whites")$Percentage)))
colnames(k) <- c("Group", "Occupation", "Percentage")
d2 <- data.frame(rbind(filter(occupation, Group=="Whites"), k))
d2$Perc <- as.numeric(d2$Percentage)/sum(as.numeric(d2$Percentage))
d2$occup <- factor(d2$Occupation, levels = c("Dummy_var1", "Professions","Trade and Transportation", "Manufacturing and Mechanical Industries", "Domestic and Personal Service", "Agriculture, Fisheries and Mining","Dummy_var2"))

p03 <- ggplot() +
  geom_bar(data=d1, aes(x="", y=Perc, fill=occup), width = 1, stat = "identity") +
  geom_bar(data=d2, aes(x="", y=Perc, fill=occup), width = 1, stat = "identity") +
  scale_fill_manual("", values=c("Agriculture, Fisheries and Mining" = "red3", "Manufacturing and Mechanical Industries" = alpha("dodgerblue3",0.8), "Domestic and Personal Service" = "gold1", "Professions" = alpha("darkgoldenrod4",0.7), "Trade and Transportation" = "cornsilk1", "Dummy_var_1"="white", "Dummy_var_2"="white")) +
  coord_polar("y", start=45, clip = 'off') +
  labs(title="\nOCCUPATIONS OF NEGROES AND WHITES IN GEORGIA") +
  theme(panel.background = element_rect(fill = "seashell2"),
        plot.background = element_rect(fill = "seashell2"),
        legend.background = element_rect(fill = "seashell2"),
        plot.title = element_text(colour = "black", size=11, hjust = 0.5, family="Lucida Sans Typewriter"),
        legend.title = element_blank(),
        legend.position="none",
        legend.justification=c(0.5,0),
        plot.margin = unit(c(0.2, 0, 2, 0), "cm"), #top, right, bottom, left
        legend.key = element_rect(fill = "seashell2"),
        legend.spacing.x = unit(0.1,"cm"),
        legend.text = element_text(colour=alpha("black",0.5), size=6, family="Lucida Sans Typewriter", margin = margin(r = 100, unit = "pt")),
        axis.title.x= element_blank(),
        axis.title.y= element_blank(),
        axis.ticks = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()
  )


p03

g1 <- grid::circleGrob(gp = grid::gpar(fill = "red3"))
g2 <- grid::circleGrob(gp = grid::gpar(fill = alpha("dodgerblue3",0.8)))
g3 <- grid::circleGrob(gp = grid::gpar(fill = "gold1"))
g4 <- grid::circleGrob(gp = grid::gpar(fill = alpha("darkgoldenrod4",0.7)))
g5 <- grid::circleGrob(gp = grid::gpar(fill = "cornsilk1"))


ggdraw(p03) +
  draw_label("WHITES", x = 0.5, y = 0.19, hjust = 0.5, vjust = 0, fontfamily = "Lucida Sans Typewriter", color = alpha("black",0.5), size = 8) +
  draw_label("NEGROES", x = 0.5, y = 0.85, hjust = 0.5, vjust = 0, fontfamily = "Lucida Sans Typewriter", color = alpha("black",0.5), size = 8) +
  draw_label("AGRICULTURE, FISHERIES\n  AND MINING", x = 0.29, y = 0.55, hjust = 0.5, vjust = 0, fontfamily = "Lucida Sans Typewriter", color = alpha("black",0.5), size = 5) +
  draw_label("MANUFACTURING AND\nMECHANICAL INDUSTRIES", x = 0.2, y = 0.49, hjust = 0, vjust = 0, fontfamily = "Lucida Sans Typewriter", color = alpha("black",0.5), size = 5) +
  draw_label("DOMESTIC AND\nPERSONAL SERVICE", x = 0.75, y = 0.59, hjust = 0.5, vjust = 0.5, fontfamily = "Lucida Sans Typewriter", color = alpha("black",0.5), size = 5) +
  draw_label("PROFESSIONS", x = 0.77, y = 0.53, hjust = 0.5, vjust = 0.5, fontfamily = "Lucida Sans Typewriter", color = alpha("black",0.5), size = 5) +
  draw_label("TRADE AND\nTRANSPORTATION", x = 0.76, y = 0.47, hjust = 0.5, vjust = 0.5, fontfamily = "Lucida Sans Typewriter", color = alpha("black",0.5), size = 5) +
  draw_label("62%", x = 0.4, y = 0.75, hjust = 0.5, vjust = 0.5, fontfamily = "Lucida Sans Typewriter", color = alpha("black",0.5), size = 7) +
  draw_label("5%", x = 0.79, y = 0.73, hjust = 0.5, vjust = 0.5, fontfamily = "Lucida Sans Typewriter", color = alpha("black",0.5), size = 7) +
  draw_label("28%", x = 0.68, y = 0.78, hjust = 0.5, vjust = 0.5, fontfamily = "Lucida Sans Typewriter", color = alpha("black",0.5), size = 5) +
  draw_label("0.8%", x = 0.82, y = 0.69, hjust = 0.5, vjust = 0.5, fontfamily = "Lucida Sans Typewriter", color = alpha("black",0.5), size = 2) +
  draw_label("4.5%", x = 0.81, y = 0.70, hjust = 0.5, vjust = 0.5, fontfamily = "Lucida Sans Typewriter", color = alpha("black",0.5), size = 5) +
  draw_label("64%", x = 0.6, y = 0.3, hjust = 0.5, vjust = 0.5, fontfamily = "Lucida Sans Typewriter", color = alpha("black",0.5), size = 7) +
  draw_label("12.5%", x = 0.32, y = 0.28, hjust = 0.5, vjust = 0.5, fontfamily = "Lucida Sans Typewriter", color = alpha("black",0.5), size = 5) +
  draw_label("5.5%", x = 0.38, y = 0.25, hjust = 0.5, vjust = 0.5, fontfamily = "Lucida Sans Typewriter", color = alpha("black",0.5), size = 5) +
  draw_label("4%", x = 0.19, y = 0.36, hjust = 0.5, vjust = 0.5, fontfamily = "Lucida Sans Typewriter", color = alpha("black",0.5), size = 5) +
  draw_label("13%", x = 0.24, y = 0.32, hjust = 0.5, vjust = 0.5, fontfamily = "Lucida Sans Typewriter", color = alpha("black",0.5), size = 5) +
  draw_grob(g1, x=-0.34, y=0.06, scale = 0.04) +
  draw_grob(g2, x=-0.34, y=-0.0, scale = 0.04) +
  draw_grob(g3, x=0.35, y=0.09, scale = 0.04) +
  draw_grob(g4, x=0.35, y=0.03, scale = 0.04) +
  draw_grob(g5, x=0.35, y=-0.03, scale = 0.04) 


