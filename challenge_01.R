library(tidyverse)
library(extrafont)
library(ggpubr)
library(pBrackets)
library(grid)
tuesdata <- tidytuesdayR::tt_load('2021-02-16')
georgia_pop <- tuesdata$georgia_pop

dev.new(width=5,height=6.25,unit="in", noRStudioGD = TRUE)

p01 <- ggplot(data=georgia_pop,aes(x=Year)) +
  geom_line(aes(y=Colored, linetype="Colored")) + 
  geom_line(aes(y=White, linetype="White")) + 
  scale_y_reverse(breaks=seq(0,100,5), labels=seq(0,100,5), expand = c(0, 0)) +
  scale_linetype_manual("", values=c("Colored"="solid", "White"="dashed")) +
  annotate("text", x = 1790, y = 17, label = "\n\n\n\n\n\n\n\n\nWHITE", colour = alpha("black",0.5), size=2, family="Lucida Sans Typewriter") +
  annotate("text", x = 1790, y = 82, label = "\n\n\n\n\n\n\n\n\nCOLORED", colour = alpha("black",0.5), size=2, family="Lucida Sans Typewriter") +
  annotate("text", x = 1790, y = 5, label = "\n\n\n\n\n\n\n\n _ _ _ _", colour = alpha("black",0.5), size=2, family="Lucida Sans Typewriter") +
  annotate("text", x = 1790, y = 95, label = "\n\n\n\n\n\n\n\n_______", colour = alpha("black",0.5), size=2, family="Lucida Sans Typewriter") +
  scale_x_continuous(breaks=georgia_pop$Year, labels=georgia_pop$Year, limits=c(1790,1890), expand = c(0, 0)) +
  labs(title = "\nCOMPARATIVE INCREASE OF WHITE AND COLORED\nPOPULATION OF GEORGIA\n", y="\n\n\nPERCENTS") +
  coord_flip(ylim = c(100,0), clip = 'off') + 
  theme(panel.background = element_rect(fill = "wheat2", color = alpha("black",0.3)),
        plot.background = element_rect(fill = "wheat2"),
        legend.background = element_rect(fill = "wheat2"),
        plot.title = element_text(colour = "black", size=11, hjust = 0.5, family="Lucida Sans Typewriter"),
        legend.title = element_blank(),
        legend.position="none",
        legend.justification=c(0.5,0),
        plot.margin = unit(c(0.2, 2, 1.5, 2), "cm"), #top, right, bottom, left
        legend.key = element_rect(fill = "wheat2"),
        legend.spacing.x = unit(0.1,"cm"),
        legend.text = element_text(colour=alpha("black",0.5), size=6, family="Lucida Sans Typewriter", margin = margin(r = 100, unit = "pt")),
        axis.title.x= element_text(colour=alpha("black",0.5), size=6, family="Lucida Sans Typewriter"),
        axis.title.y= element_blank(),
        axis.ticks = element_blank(),
        axis.text.x=element_text(colour=alpha("black",0.5), size=4, family="Lucida Sans Typewriter"),
        axis.text.y=element_text(colour=alpha("black",0.5), size=6, family="Lucida Sans Typewriter"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour=alpha("red1",0.2))
  )
p01

#grid.locator(unit="native") 
grid.brackets(410, 490, 90, 490, lwd=0.5, col="gray57")

setwd("C:/Users/rennien/OneDrive - Lancaster University/Programming/R/dubois_challenge/images")
ggsave(p01, filename = "challenge_01.jpg", height=6.25, width=5, unit="in")
