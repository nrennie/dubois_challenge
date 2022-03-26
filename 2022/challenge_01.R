library(showtext)
library(ggplot2)
library(gridExtra)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggforce)
library(cowplot)
library(rnaturalearthhires)

`%notin%` <- Negate(`%in%`)

# load fonts
font_add_google(name = "Space Mono", family = "space")
showtext_auto()

# data
world <- ne_countries(scale = "small", returnclass = "sf")

usa <- ne_states(iso_a2 = 'us', returnclass = "sf")
brazil <- ne_states(country = 'brazil', returnclass = "sf")
amer <- world[world$region_un %in% c("Americas"),] 
country_col1 <- rep("tan", length(amer$name))
amer$country_col1 <- country_col1
usa_col <- rep("tan", length(usa$name))
usa_col[which(usa$name %in% c("Florida", "Georgia", "Alabama", "Mississippi", "South Carolina"))] <- "black"
usa_col[which(usa$name %in% c("Tennessee", "North Carolina", "Arkansas", "Louisiana"))] <- "chocolate4"

brazil_col <- rep("tan", length(brazil$name))
brazil_col[which(brazil$name %in% c("Ceará", "Bahia", "Paraíba", "Piauí", "Rio Grande do Norte", 
                                    "Pernambuco", "Sergipe", "Alagoas", "Minas Gerais", "Espírito Santo", "Rio de Janeiro"))] <- "black"
brazil_col[which(brazil$name %in% c("Maranhão", "Tocantins", "Goiás", "São Paulo", "Rio de Janeiro", "Distrito Federal"))] <- "chocolate4"




p1 <- ggplot() +
  geom_circle(aes(x0 = 1821000, y0 =3910000, r = 8160900), colour="black", fill="#bda086") + 
  geom_sf(data = amer, fill=country_col1) +
  geom_sf(data = brazil, fill=brazil_col) +
  geom_sf(data = usa, fill=usa_col) +
  geom_point(data=data.frame(x = 2121000, y =5580000), aes(x=x, y=y), pch=19, size=1, col="white") +
  coord_sf(crs = "+proj=laea +lat_0=10 +lon_0=-60 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ",
           xlim=c(-2339981, 12803058)-3400000, ylim=c(-5095286, 9970916)+1500000) +
  theme(panel.background = element_rect(fill = "#d0bba8", colour = "#d0bba8"),
        plot.background = element_rect(fill = "#d0bba8", colour = "#d0bba8"),
        legend.background = element_rect(fill = "#d0bba8", colour = "#d0bba8"),
        plot.title = element_text(colour = "black", size=12, face="bold", hjust = 0.5, family="space"),
        plot.subtitle = element_text(colour = "black", size=10, face="bold", hjust = 0.5, family="space"),
        legend.title = element_blank(),
        legend.position="none",
        plot.margin = unit(c(6, 0, 6, 0), "cm"), #top, right, bottom, left
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

p1


afr_eur <- world[world$name %notin% c("Indonesia", "Australia", "Canada", 
                                      "Greenland", "United States", "Cuba",
                                      "Papua New Guinea", "Fr. S. Antarctic Lands") & world$region_un %notin% c("Americas", "Antarctica"),] 
country_col2 <- rep("tan", length(afr_eur$name))
country_col2[which(afr_eur$region_un == "Africa")] <- "black"
country_col2[which(afr_eur$name == "Lesotho")] <- "chocolate4"
country_col2[which(afr_eur$name == "Madagascar")] <- "tan"
country_col2[which(afr_eur$name %in% c("South Africa", "Algeria", "Tunisia", "Libya", 
                                       "Egypt", "Morocco", "Western Sahara", "W. Sahara", "Mauritania", 
                                       "Mali", "Niger", "Chad", "Sudan"))] <- "chocolate4"
afr_eur$country_col2 <- country_col2 
p2 <- ggplot() +
  geom_circle(aes(x0 = 5221000, y0 =2510000, r = 8160900), colour="black", fill="#bda086") +
  geom_sf(data = afr_eur, fill=country_col2) +
  coord_sf(crs = "+proj=laea +lat_0=40 +lon_0=45 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ",
           xlim=c(-2339981, 12803058), ylim=c(-5095286, 9970916)) +
  theme(panel.background = element_rect(fill = "#d0bba8", colour = "#d0bba8"),
        plot.background = element_rect(fill = "#d0bba8", colour = "#d0bba8"),
        legend.background = element_rect(fill = "#d0bba8", colour = "#d0bba8"),
        plot.title = element_text(colour = "black", size=12, face="bold", hjust = 0.5, family="space"),
        plot.subtitle = element_text(colour = "black", size=10, face="bold", hjust = 0.5, family="space"),
        legend.title = element_blank(),
        legend.position="none",
        plot.margin = unit(c(6, 0, 6, 0), "cm"), #top, right, bottom, left
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

p2


p <- grid.arrange(p1, p2, nrow = 1, ncol=2)
p

g1 <- grid::circleGrob(gp = grid::gpar(fill = "white"))

q <- ggdraw(p) +
  theme(plot.background = element_rect(fill="#d0bba8", color = "#d0bba8")) +
  draw_grob(g1, x=-0.25, y=-0.285, scale = 0.015) +
  draw_label("THE GEORGIA NEGRO .", x = 0.5, y = 0.92, hjust = 0.5, vjust = 0.5, 
             fontfamily = "space", fontface="bold", color = alpha("black",0.9), size = 11) +
  draw_label("A SOCIAL STUDY\nBY\nW.E.BURGHARDT DU BOIS.", x = 0.5, y = 0.86, hjust = 0.5, vjust = 0.5, 
             fontfamily = "space", color = alpha("black",0.9), size = 9) +
  draw_label("ROUTES OF THE AFRICAN SLAVE TRADE", x = 0.3, y = 0.25, hjust = 0, vjust = 0.5, 
             fontfamily = "space", color = alpha("black",0.7), size = 9) +
  draw_label("__", x = 0.24, y = 0.26, hjust = 0, vjust = 0.5, 
             fontfamily = "space", color = alpha("black",0.7), size = 9) +
  draw_label("THE STATE OF GEORGIA", x = 0.3, y = 0.22, hjust = 0, vjust = 0.5, 
             fontfamily = "space", color = alpha("black",0.7), size = 9) +
  draw_label("THIS CASE IS DEVOTED TO A SERIES OF CHARTS., MAPS AND OTHER DEVI-\nCES DESIGNED TO ILLUSTRATE THE DEVELOPMENT OF THE AMERICAN NEGRO IN A\nSINGLE TYPICAL STATE OF THE UNITED STATES.\n\nTHE PROBLEM OF THE 20TH CENTURY IS THE PROBLEM OF THE\nCOLOR LINE.", 
             x = 0.5, y = 0.1, hjust = 0.5, vjust = 0.5, fontfamily = "space", color = alpha("black",0.7), size = 9) +
  draw_line(x=c(0.39, 0.62), y=c(0.43, 0.38), colour="black", size=0.5) +
  draw_line(x=c(0.26, 0.62), y=c(0.53, 0.45), colour="black", size=0.5) +
  draw_line(x=c(0.27, 0.62), y=c(0.51, 0.45), colour="black", size=0.5) +
  draw_line(x=c(0.27, 0.62), y=c(0.54, 0.45), colour="black", size=0.5)

q

ggsave(q, filename = "2022/images/challenge_01.jpg", height = 750, width = 600, unit = "px")




