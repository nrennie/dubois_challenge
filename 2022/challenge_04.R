library(tidyverse)
library(showtext)
library(scales)

# load data
df <- readr::read_csv("2022/data/04.csv")

# load fonts
font_add_google(name = "Space Mono", family = "space")
showtext_auto()

# prep data
plot_data <- df %>% 
  mutate(lt = as.character(1+as.numeric((Year > 1874 & Year < 1899))))
df_rect <- data.frame(x1 = 1865, x2 = 1870, y1 = 0, y2 = 5000000) 

plot_data1 <- plot_data %>% 
  filter(Year < 1899)

plot_data2 <- plot_data %>% 
  filter(Year >= 1899)

plot_data3 <- plot_data %>% 
  filter(Year > 1874 & Year < 1899)

# plot
p <- ggplot() +
  geom_line(data = plot_data1, 
            mapping = aes(x = Year, y = `Property Valuation`, colour = lt), size = 2) +
  geom_line(data = plot_data2, 
            mapping = aes(x = Year, y = `Property Valuation`, colour = lt), size = 2) +
  geom_line(data = plot_data3, 
            mapping = aes(x = Year, y = `Property Valuation`, colour = lt), size = 0.8) +
  geom_line(data = plot_data, 
            mapping = aes(x = Year, y = `Property Valuation`+20000), size = 0.1) +
  geom_line(data = plot_data, 
            mapping = aes(x = Year, y = `Property Valuation`-20000), size = 0.1) +
  geom_rect(data=df_rect, aes(xmin=x1, xmax=x2,ymin = y1, ymax = y2), 
            fill="#dfd3c2", size=0.5, 
            colour = alpha("black", 0.1)) +
  labs(title = "VALUATION OF TOWN AND CITY PROPERTY OWNED\nBY GEORGIA NEGROES.", 
       x = "", y = "DOLLARS") +
  scale_colour_manual(values = c("1" = "#dfd3c2", "2" = "black")) +
  coord_cartesian(expand = F) +
  scale_x_continuous(limits = c(1860, 1900), 
                     breaks = c(seq(1870, 1900, 5)), 
                     minor_breaks = c(seq(1870, 1900, 1))) +
  scale_y_continuous(limits = c(0, 5000000), 
                     breaks = c(seq(1000000, 4000000, 1000000)),
                     minor_breaks = c(seq(0, 5000000, 100000)), 
                     labels = comma) +
  annotate("text", x = 1877, y = 2300000, label = "POLITICAL\n    UNREST", family = "space", size = 4, lineheight = 0.4, colour = "gray20") +
  annotate("text", x = 1884, y = 4200000, label = "RISE OF\n    THE NEW\n              INDUSTRIALISM", lineheight = 0.4, family = "space", size = 4, colour = "gray20") +
  annotate("text", x = 1896, y = 2400000, label = "DISFRANCHSMENT\nAND\nPROSCRIPTIVE\nLAWS", family = "space", lineheight = 0.4, size = 4, colour = "gray20") +
  annotate("text", x = 1892, y = 1600000, label = "LYNCHING", family = "space", size = 4, colour = "gray20") +
  annotate("text", x = 1872, y = 300000, label = "KU-KLUXISM", angle = 90, hjust = 0, vjust = 0.5, family = "space", size = 4, colour = "gray20") +
  annotate("text", x = 1894, y = 300000, label = "FINANCIAL PANIC", angle = 90, hjust = 0, vjust = 0.5, family = "space", size = 4, colour = "gray20") +
  annotate("text", x = 1861, y = 400000, label = "$", hjust = 0, vjust = 0.5, family = "space", size = 6, colour = "gray20") +
  annotate("text", x = 1861, y = 600000, label = "$", hjust = 0, vjust = 0.5, family = "space", size = 6, colour = "gray20") +
  annotate("text", x = 1861, y = 1400000, label = "$", hjust = 0, vjust = 0.5, family = "space", size = 6, colour = "gray20") +
  annotate("text", x = 1861, y = 1600000, label = "$", hjust = 0, vjust = 0.5, family = "space", size = 6, colour = "gray20") +
  annotate("text", x = 1861, y = 2400000, label = "$", hjust = 0, vjust = 0.5, family = "space", size = 6, colour = "gray20") +
  annotate("text", x = 1861, y = 2600000, label = "$", hjust = 0, vjust = 0.5, family = "space", size = 6, colour = "gray20") +
  annotate("text", x = 1861, y = 3400000, label = "$", hjust = 0, vjust = 0.5, family = "space", size = 6, colour = "gray20") +
  annotate("text", x = 1861, y = 3600000, label = "$", hjust = 0, vjust = 0.5, family = "space", size = 6, colour = "gray20") +
  annotate("text", x = 1861, y = 4400000, label = "$", hjust = 0, vjust = 0.5, family = "space", size = 6, colour = "gray20") +
  annotate("text", x = 1861, y = 4600000, label = "$", hjust = 0, vjust = 0.5, family = "space", size = 6, colour = "gray20") +
  annotate("text", x = 1861, y = 1000000, label = "1,000, 000", hjust = 0, vjust = 0.5, family = "space", size = 4, colour = "gray20") +
  annotate("text", x = 1861, y = 2000000, label = "2,000, 000", hjust = 0, vjust = 0.5, family = "space", size = 4, colour = "gray20") +
  annotate("text", x = 1861, y = 3000000, label = "3,000, 000", hjust = 0, vjust = 0.5, family = "space", size = 4, colour = "gray20") +
  annotate("text", x = 1861, y = 4000000, label = "4,000, 000", hjust = 0, vjust = 0.5, family = "space", size = 4, colour = "gray20") +
  theme_light() +
  theme(plot.background = element_rect(fill = "#dfd3c2", colour="#dfd3c2"),
        panel.background = element_rect(fill = "#dfd3c2", colour="#dfd3c2"),
        legend.position = "none", 
        axis.text.y = element_blank(),
        axis.text.x = element_text(colour = "black", size=12, family="space", vjust = 2),
        axis.ticks = element_blank(),
        axis.title.y = element_text(colour = "black", size=14, hjust = -65, family="space", angle = 0), 
        plot.title = element_text(colour = "black", size=20, face = "bold", hjust = 0.5, lineheight = 0.3, family="space", 
                                  margin = margin(0, 0, 10, 0)), 
        panel.grid.major = element_line(colour = alpha("black", 0.1)),
        panel.grid.minor = element_line(colour = alpha("red", 0.1)),
        plot.margin = unit(c(0.5, 0.5, 0.2, 0.1), "cm"))
p

ggsave(p, filename = "2022/images/challenge_04.jpg", height = 5, width = 4, unit = "in")


