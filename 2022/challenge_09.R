library(tidyverse)
library(showtext)

# load data
df <- readr::read_csv("2022/data/09.csv")

# load fonts
font_add_google(name = "Space Mono", family = "space")
showtext_auto()

# labels
labels <- df %>% 
  mutate(label = paste(CATEGORY, STUDENTS), 
         y = 8:3, 
         x = -1)

# prep data
df1 <- data.frame(x = seq(1, by = 0.1, length.out = 6), 
                  y = rep(8, 6))

df2 <- data.frame(x = seq(1.7, by = 0.1, length.out = 49), 
                  y = rep(7, 49))

df3 <- data.frame(x = seq(3.5, by = 0.1, length.out = 76), 
                  y = rep(6, 76))

df4 <- data.frame(x = seq(2.0, by = 0.1, length.out = 80), 
                 y = rep(5, 80))

df5 <- data.frame(x = seq(1.3, by = 0.1, length.out = 191), 
                  y = rep(4, 191))

x1 <- seq(2.7, 41, by = 0.1)
y1 <- rep(3, length(x1))
x2 <- seq(-0.5, 41, by = 0.1)
y2 <- rep(2.2, length(x2))
x3 <- seq(-0.5, 18, by = 0.1)
y3 <- rep(1.4, length(x3))

df6 <- data.frame(x = c(x1), 
                  y = c(y1))

df7 <- data.frame(x = c(x2), 
                  y = c(y2))

df8 <- data.frame(x = c(x3), 
                  y = c(y3))


p <- ggplot() +
  # points
  geom_line(data = df1, mapping = aes(x= x, y = y), size = 2, colour = "#976c50") +
  geom_line(data = df2, mapping = aes(x= x, y = y), size = 2, colour = "#976c50") +
  geom_line(data = df3, mapping = aes(x= x, y = y), size = 2, colour = "#976c50") +
  geom_line(data = df4, mapping = aes(x= x, y = y), size = 2, colour = "#976c50") +
  geom_line(data = df5, mapping = aes(x= x, y = y), size = 2, colour = "#976c50") +
  geom_line(data = df6, mapping = aes(x= x, y = y), size = 2, colour = "#976c50") +
  geom_line(data = df7, mapping = aes(x= x, y = y), size = 2, colour = "#976c50") +
  geom_line(data = df8, mapping = aes(x= x, y = y), size = 2, colour = "#976c50") +
  # curves
  geom_curve(aes(x = -0.5, y = 1.4, xend = -0.5, yend = 2.2), 
             data = df6, curvature = -1, size = 2, colour = "#976c50") +
  geom_curve(aes(x = 41, y = 2.2, xend = 41, yend = 3), 
             data = df6, curvature = 1, size = 2, colour = "#976c50") +
  #labels
  geom_text(data = labels, mapping = aes(x = -5, y = y, label = label), 
            hjust = 0, family = "space") +
  labs(x = "", 
       y = "", 
       title = "NUMBER OF NEGRO STUDENTS TAKING\nTHE VARIOUS COURSES OF STUDY\nOFFERED IN GEORGIA SCHOOLS.") +
  scale_y_continuous(limits = c(0.5, 8.2)) +
  scale_x_continuous(limits = c(-5.5, 43)) +
  theme(plot.background = element_rect(fill = "#dac8b8", colour="#dac8b8"),
        panel.background = element_rect(fill = "#dac8b8", colour="#dac8b8"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "none",
        plot.title = element_text(family = "space", 
                                  hjust = 0.5, size = 24, 
                                  lineheight = 0.3, 
                                  margin = margin(b = 10)),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.5, 0.8, 0, 0.8), "cm")) 
p

ggsave(p, filename = "2022/images/challenge_09.jpg", height = 5, width = 4, unit = "in")
