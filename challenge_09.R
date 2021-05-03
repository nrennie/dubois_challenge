library(extrafont)
library(maps)
library(ggmap)
library(rgdal)
library(dplyr)
library(cowplot)
library(datasets)

setwd("C:/Users/rennien/OneDrive - Lancaster University/Programming/git/dubois_challenge/data")
birthplace <- read.csv("birthplace.csv")
present <- read.csv("present.csv")

setwd("C:/Users/rennien/OneDrive - Lancaster University/Programming/git/dubois_challenge/data/us_states")
#get historic shape files from https://publications.newberry.org/ahcbp/downloads/united_states.html
us = readOGR(dsn=".", layer="US_HistStateTerr", stringsAsFactors = FALSE)
us$start_year <- substr(us$START_DATE, 1, 4)
us$end_year <- substr(us$END_DATE, 1, 4)
us@data$id = rownames(us@data)
us.points = fortify(us, region="id")
us.df = inner_join(us.points, us@data, by="id")
usYear <- function(df, year) {
  temp2 <- subset(df, end_year > year & start_year < year+1)
}
us1890 <- usYear(us.df, 1890)
us1890_2 <- us1890[which(us1890$NAME != "Alaska District"),]

state_names <- unique(us1890_2$NAME)
state_values <- c("red","medgray","olive","pink","medgray",
                  "pink","lightgray","lightgray","medgray","pink",
                  "blue","medgray","yellow","red","red",
                  "lightgray","blue","brown","blue","brown",
                  "lightgray","yellow","red","brown","olive",
                  "pink","lightgray","blue","brown","yellow",
                  "yellow","blue","lightgray","yellow","black",
                  "lightgray","blue","yellow","pink","olive",
                  "pink","olive","lightgray","pink","medgray",
                  "yellow","yellow","brown","pink","blue")

state <- data.frame(NAME=state_names, state_values, abbrevs)
us1890_col <- inner_join(us1890_2, state, by = "NAME")

#read in coord data
# coord <- read.csv("present_coords.txt", row.names=NULL, fill = TRUE , header = FALSE)
# present$lat <- as.numeric(coord[,1])
# present$lon <- as.numeric(coord[,2])
# present <- present[-which(present$State == "AK"),]

p1 <- ggplot() + 
  geom_polygon(data = us1890_col, aes(x=long,y=lat,group=group, fill=state_values), size = 0.5, color="black") +
  scale_fill_manual("", values = c("red"="red", "pink"="lightpink", "yellow"="gold2", "lightgray"="grey88","medgray"="grey56", "olive"="olivedrab4",
                                   "black"="black", "blue"="dodgerblue2", "brown"="tan4", "antiquewhite3"="antiquewhite3","white" = "white")) +
  coord_cartesian(clip = 'off') +
  labs(title="\n\n", caption = "PRESENT DWELLING PLACE OF NEGROES BORN IN GEORGIA") + 
  theme_nothing() +
  geom_text(data=present, aes(x=lon, y=lat, label=Present.Location), family="Lucida Sans Typewriter", size=2) +
  geom_text(data=filter(present, State=="GA"), aes(x=lon, y=lat, label=Present.Location), family="Lucida Sans Typewriter", colour="white", size=2) +
  theme(panel.background = element_rect(fill = "wheat", colour = "wheat"),
        plot.background = element_rect(fill = "wheat", colour = "wheat"),
        legend.background = element_rect(fill = "wheat"),
        plot.title = element_text(colour = "black", size=12, hjust = 0.5, face="bold", family="Lucida Sans Typewriter"),
        plot.subtitle = element_text(colour = "black", size=10, hjust = 0.5, face="bold", family="Lucida Sans Typewriter"),
        plot.caption = element_text(colour = "black", size=8, hjust = 0.5, family="Lucida Sans Typewriter"),
        legend.title = element_blank(),
        legend.position="none",
        legend.justification=c(0.5,0),
        plot.margin = unit(c(1.5, 1.5, 0.5, 2), "cm"), #top, right, bottom, left
        legend.key = element_rect(fill = "wheat"),
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
#p1


#####################################################################################################################################
#####################################################################################################################################
state_names2 <- unique(us1890_2$NAME)
state_values2 <- c("lightgray","blue","red","brown","lightgray",
                  "medgray","blue","medgray","pink","olive",
                  "yellow","pink","red","lightgray","blue",
                  "red","red","blue","medgray","olive",
                  "yellow","pink","lightgray","blue","pink",
                  "lightgray","brown","blue","medgray","brown",
                  "olive","red","red","lightgray","black",
                  "olive","yellow","red","brown","pink",
                  "brown","olive","yellow","medgray","red",
                  "olive","lightgray","lightgray","yellow","pink")
state2 <- data.frame(NAME=state_names2, state_values2)
us1890_col2 <- inner_join(us1890_2, state2, by = "NAME")

#read in coord data
# coord <- read.csv("birthplace_coords.txt", row.names=NULL, fill = TRUE , header = FALSE)
# birthplace$lat <- as.numeric(coord[,1])
# birthplace$lon <- as.numeric(coord[,2])
# birthplace <- birthplace[-which(birthplace$State == "AK"),]

p2 <- ggplot() + 
  geom_polygon(data = us1890_col2, aes(x=long,y=lat,group=group, fill=state_values2), size = 0.5, color="black") +
  scale_fill_manual("", values = c("red"="red", "pink"="lightpink", "yellow"="gold2", "lightgray"="grey88","medgray"="grey56", "olive"="olivedrab4",
                                   "black"="black", "blue"="dodgerblue2", "brown"="tan4", "antiquewhite3"="antiquewhite3","white" = "white")) +
  coord_cartesian(clip = 'off') +
  theme_nothing() +
  geom_text(data=birthplace, aes(x=lon, y=lat, label=Birthplace), family="Lucida Sans Typewriter", size=2) +
  geom_text(data=filter(birthplace, State=="GA"), aes(x=lon, y=lat, label=Birthplace), family="Lucida Sans Typewriter", colour="white", size=2) +
  labs(title="", caption = "BIRTH PLACE OF NEGROES NOW IN GEORGIA") + 
  theme(panel.background = element_rect(fill = "wheat", color = "wheat"),
        plot.background = element_rect(fill = "wheat", colour = "wheat"),
        legend.background = element_rect(fill = "wheat"),
        plot.title = element_text(colour = "black", size=12, hjust = 0.5, face="bold", family="Lucida Sans Typewriter"),
        plot.subtitle = element_text(colour = "black", size=10, hjust = 0.5, face="bold", family="Lucida Sans Typewriter"),
        plot.caption = element_text(colour = "black", size=8, hjust = 0.5, family="Lucida Sans Typewriter"),
        legend.title = element_blank(),
        legend.position="none",
        legend.justification=c(0.5,0),
        plot.margin = unit(c(1.5, 1.5, 0.5, 2), "cm"), #top, right, bottom, left
        legend.key = element_rect(fill = "wheat"),
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
#p2

gt1 <- ggplot_gtable(ggplot_build(p1))
gt2 <- ggplot_gtable(ggplot_build(p2))
gt1$widths <- gt2$widths
gt1$heights <- gt2$heights

#dev.new(width=6,height=8,unit="in", noRStudioGD = TRUE) 

p02 <- cowplot::plot_grid(gt1, gt2, nrow = 2, align = "h") + 
  theme(plot.background = element_rect(fill = "wheat", colour = "wheat")) +
  draw_label("MIGRATION OF NEGROES.\n1890.", x = 0.5, y = 0.92, hjust = 0.5, vjust = 0, fontface=2, fontfamily = "Lucida Sans Typewriter", color = "black", size = 12) 
p02








