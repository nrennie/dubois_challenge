library(extrafont)
library(ggpubr)
library(maps)
library(ggmap)
library(rgdal)

dev.new(width=5,height=6.25,unit="in", noRStudioGD = TRUE)

states <- map_data("state")
georgia <- subset(states, region %in% c("georgia"))
#historic shapefiles obtained from https://publications.newberry.org/ahcbp/pages/Georgia.html
ga = readOGR(dsn=".", layer="GA_Historical_Counties", stringsAsFactors = FALSE)
ga$start_year <- substr(ga$START_DATE, 1, 4)
ga$end_year <- substr(ga$END_DATE, 1, 4)
ga@data$id = rownames(ga@data)
ga.points = fortify(ga, region="id")
ga.df = inner_join(ga.points, ga@data, by="id")
gaYear <- function(df, year) {
  temp2 <- subset(df, end_year > year & start_year < year+1)
}
ga1890 <- usYear(ga.df, 1890)

#manually add in colour data
county_names <- unique(ga1890$NAME)
county_values <- c("red", "red", "lightpink","tan4","red",
                   "tan4", "red","black","gold2","lightpink",
                   "olivedrab4","olivedrab4","lightpink","red","gold2",
                   "lightpink","red","lightpink","lightpink","red",
                   "antiquewhite3","red","red","red","red", 
                   "lightpink","olivedrab4","olivedrab4","lightpink","lightpink",
                   "gold2","red","red","red","lightpink","gold2",
                   "red","gold2","red","red","olivedrab4",
                   "lightpink","red","tan4","gold2","olivedrab4",
                   "black","lightpink","red","lightpink","antiquewhite3",
                   "olivedrab4","gold2","antiquewhite3","red","red",
                   "red","lightpink","gold2","gold2","antiquewhite3",
                   "lightpink","antiquewhite3","red","red","lightpink",
                   "antiquewhite3","lightpink","gold2","antiquewhite3","gold2",
                   "red","gold2","red","gold2","lightpink",
                   "gold2","olivedrab4","lightpink","lightpink","antiquewhite3",
                   "lightpink","gold2","red","red","red",
                   "antiquewhite3","antiquewhite3","antiquewhite3","red","antiquewhite3",
                   "gold2","gold2","black","gold2","gold2",
                   "red","red","antiquewhite3","black","lightpink",
                   "red","lightpink","gold2","red","antiquewhite3",
                   "lightpink","lightpink","red","olivedrab4","blue4",
                   "red","red","lightpink","olivedrab4","antiquewhite3",
                   "lightpink","lightpink","gold2","lightpink","gold2",
                   "antiquewhite3", "red", "olivedrab4","gold2","lightpink",
                   "lightpink","lightpink","antiquewhite3","antiquewhite3","red",
                   "gold2","olivedrab4","olivedrab4","olivedrab4", "white", "red")
county <- data.frame(NAME=county_names, county_values)
ga1890_col <- inner_join(ga1890, county, by = "NAME")
p10 <- ggplot() + 
  geom_polygon(data=georgia, aes(x=long, y=lat, group=group), color="black", size=0.3, fill="lightblue") +
  geom_polygon(aes(x=long,y=lat,group=group, fill=county_values), size = 0.3, color=alpha("black",0.5), data = ga1890_col) +
  labs(title="\n\nNEGRO POPULATION OF GEORGIA BY COUNTIES.", subtitle="1890.\n\n\n\n") +
  annotate("text", x = -87, y = 29, label = "OVER 30,000 NEGROES\n\n\nBETWEEN 20,000 AND 30,000\n\n\n15,000 TO 20,000\n\n\n10,000 TO 15,000", colour = alpha("black",0.5), size=2.5, hjust = 0, family="Lucida Sans Typewriter") +
  annotate("text", x = -80.5, y = 29, label = "5,000 TO 10,000\n\n\n2,500 TO 5,000\n\n\n1,000 TO 2,500\n\n\nUNDER 1000", colour = alpha("black",0.5), size=2.5, hjust = 1, family="Lucida Sans Typewriter") +
  annotate("point", x = -83, y = 30.05, colour = "black", fill="red", size=7, pch=21) +
  annotate("point", x = -83, y = 29.35, colour = "black", fill="lightpink", size=7, pch=21) +
  annotate("point", x = -83, y = 28.6, colour = "black", fill="gold2", size=7, pch=21) +
  annotate("point", x = -83, y = 27.9, colour = "black", fill="olivedrab4", size=7, pch=21) +
  annotate("point", x = -87.5, y = 30.05, colour = "black", fill="black", size=7, pch=21) +
  annotate("point", x = -87.5, y = 29.35, colour = "black", fill="blue4", size=7, pch=21) +
  annotate("point", x = -87.5, y = 28.6, colour = "black", fill="tan4", size=7, pch=21) +
  annotate("point", x = -87.5, y = 27.9, colour = "black", fill="antiquewhite3", size=7, pch=21) +
  scale_fill_manual("", values = c("red"="red", "lightpink"="lightpink", "gold2"="gold2", "olivedrab4"="olivedrab4",
                                   "black"="black", "blue4"="blue4", "tan4"="tan4", "antiquewhite3"="antiquewhite3","purple"="purple", "white" = "white")) +
  coord_cartesian(clip = 'off') +
  theme(panel.background = element_rect(fill = "antiquewhite2", color = NA),
        plot.background = element_rect(fill = "antiquewhite2"),
        legend.background = element_rect(fill = "antiquewhite2"),
        plot.title = element_text(colour = "black", size=12, hjust = 0.5, face="bold", family="Lucida Sans Typewriter"),
        plot.subtitle = element_text(colour = "black", size=10, hjust = 0.5, face="bold", family="Lucida Sans Typewriter"),
        legend.title = element_blank(),
        legend.position="none",
        legend.justification=c(0.5,0),
        plot.margin = unit(c(0.2, 2, 1, 2), "cm"), #top, right, bottom, left
        legend.key = element_rect(fill = "antiquewhite2"),
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
p10

ggsave(p10, filename = "challenge_10.jpg", height=6.25, width=5, unit="in")



