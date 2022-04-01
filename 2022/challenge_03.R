library(tidyverse)
library(showtext)
library(sf)

# load data
df <- readr::read_csv("2022/data/03.csv")
s_file <- st_read(dsn = "2022/data/s_11au16/s_11au16.shp") 

# load fonts
font_add_google(name = "Space Mono", family = "space")
showtext_auto()

# Join map data
df <- df %>% 
  rename("STATE" = "State")
df[8,2] <- "100,000 - 200,000"
map_data1 <- inner_join(s_file, df, by = "STATE")
map_data1$Population = factor(map_data1$Population, 
                              levels = c("750,000 AND OVER", "100,000 - 200,000",
                                         "600,000 - 750,000", "50,000 - 100,000",
                                         "500,000 - 600,000", "25,000 - 50,000",
                                         "300,000 - 500,000", "10,000 - 25,000", 
                                         "200,000 - 300,000", "UNDER - 10,000"))

# Plot data
p <- ggplot() +
  geom_sf(data=map_data1, 
          mapping = aes(fill = Population), 
          colour="black", size = 0.08) +
  labs(title = "RELATIVE NEGRO POPULATION OF THE STATES OF THE\nUNITED STATES.") +
  scale_fill_manual(values = c("#242220", "#d8bea7", "#938270", "#c3042a", "#dbccbb", 
                               "#e5c2b4", "#795640", "#eeb455", "#2e255f", "#d3d3d3")) +
  guides(fill=guide_legend(ncol=2, byrow = TRUE)) +
  theme(plot.background = element_rect(fill = "#dac8b8", colour="#dac8b8"),
        panel.background = element_rect(fill = "#dac8b8", colour="#dac8b8"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(fill = "#dac8b8", colour="#dac8b8"), 
        legend.text = element_text(family = "space", size = 14, margin = margin(r = 20)),
        legend.spacing.y = unit(0.3, 'cm'),
        legend.key.size = unit(0.3, 'cm'),
        legend.key = element_rect(size = 1),
        plot.title = element_text(family = "space", face = "bold", 
                                  hjust = 0.5, size = 24, 
                                  lineheight = 0.3, 
                                  margin = margin(b = 60)),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.1, 0.8, 0.3, 0.8), "cm")) 

ggsave(p, filename = "2022/images/challenge_03.jpg", height = 5, width = 4, unit = "in")
