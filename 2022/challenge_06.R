library(tidyverse)
library(showtext)

# load data
df <- readr::read_csv("2022/data/06.csv")

# load fonts
font_add_google(name = "Space Mono", family = "space")
showtext_auto()

# plot
p <- ggplot(data = df, 
       mapping = aes(x = `Iliteracy Rate`, y = Date)) +
  geom_segment(aes(y = 1950, yend = Date, x = `Iliteracy Rate`, xend = `Iliteracy Rate`), 
               size = 2) +
  geom_segment(aes(y = Date, yend = Date, x = 100, xend = `Iliteracy Rate`-1), 
               size = 2, colour = "#dac8b8") +
  # line borders
  geom_segment(aes(y = Date+0.8, yend = Date+0.8, x = 101, xend = `Iliteracy Rate`-1), 
               size = 0.2, colour = "black") +
  geom_segment(aes(y = Date-0.8, yend = Date-0.8, x = 101, xend = `Iliteracy Rate`-1), 
               size = 0.2, colour = "black") +
  geom_segment(aes(y = Date-0.8, yend = Date+0.8, x = `Iliteracy Rate`-1, xend = `Iliteracy Rate`-1), 
               size = 0.2) +
  #scales
  scale_y_reverse(limits = c(1950, 1858), 
                  breaks = c(1950, df$Date), 
                  labels = c("PERCENT OF\nILLITERACY", df$Date[1:4], "(1900?)")) +
  scale_x_reverse(breaks = df$`Iliteracy Rate`, 
                  labels = c(paste0(df$`Iliteracy Rate`[1:4], "%"), "(50%?)")) +
  coord_cartesian(expand = T) +
  labs(x = "", 
       y = "", 
       title = "ILLITERACY.") +
  theme(plot.background = element_rect(fill = "#dac8b8", colour="#dac8b8"),
        panel.background = element_rect(fill = "#dac8b8", colour="#dac8b8"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        plot.title = element_text(family = "space", face = "bold", hjust = 0.3, size = 24),
        axis.title = element_text(family = "space", hjust = 0.5),
        axis.text.x = element_text(margin = margin(t = -10), size = 16),
        axis.text.y = element_text(lineheight = 0.4, size = 16),
        axis.ticks = element_blank(),
        plot.margin = unit(c(1.0, 2.0, 1.2, 2.0), "cm"))

p

ggsave(p, filename = "2022/images/challenge_06.jpg", height = 5, width = 4, unit = "in")

