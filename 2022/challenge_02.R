library(tidyverse)
library(ggforce)
library(showtext)

# load data
df <- readr::read_csv("2022/data/02.csv") %>% 
  distinct() %>% 
  mutate(label = paste0("$", format(`Valuation (Dollars)`,big.mark=",", trim=TRUE)))
df

# load fonts
font_add_google(name = "Space Mono", family = "space")
showtext_auto()

# prep data
circles <- tibble(x = rep(0, 6), 
                  y = rep(0, 6), 
                  r = c(20, 19, 18, 11, 7, 6), 
                  fill = as.factor(1:6))

year_labels <- tibble(x = rep(0, 6), 
                      y = -1*c(20, 19, 18, 11, 7, 6)+0.5, 
                      label = rev(df$Year), 
                      colour = c(rep("black", 5), "white"))

# plot
p <- ggplot() +
  # circles
  geom_circle(data = circles, 
              mapping = aes(x0 = x, y0 = y, r = r, fill = fill), 
              colour = "#c39c63") +
  # cutouts
  geom_arc_bar(aes(x0 = -2, y0 = 2, r0 = 16.7, r = 0, start = 5.5, end = 5.8), 
               fill = "#c11332", colour = "#c11332") +
  geom_arc_bar(aes(x0 = 2, y0 = 2, r0 = 15.7, r = 0, start = 0.5, end = 0.8), 
               fill = "#dbc8b0", colour = "#dbc8b0") +
  geom_arc_bar(aes(x0 = 3, y0 = 0, r0 = 13.7, r = 0, start = 1.4, end = 1.7), 
               fill = "#efad00", colour = "#efad00") +
  geom_arc_bar(aes(x0 = 2, y0 = -2, r0 = 7.7, r = 0, start = 2.1, end = 2.7), 
               fill = "#3a5288", colour = "#3a5288") +
  geom_arc_bar(aes(x0 = -2, y0 = -2, r0 = 3.3, r = 0, start = 3.8, end = 4.4), 
               fill = "#b28e73", colour = "#b28e73") +
  # text
  geom_text(data = year_labels, 
            mapping = aes(x = x, y = y, label = label, colour = I(colour)),
            family = "space") +
  geom_text(data = df, 
            mapping = aes(x = 0, y = 0, label = unlist(df[1,3])), 
            colour = "white", family = "space") +
  geom_text(data = df,
            aes(x = -4.6, y = -3.7, label = unlist(df[2,3])),
            colour = "black", family = "space", angle = 35, size = 2) + 
  geom_text(data = df,
            aes(x = 5, y = -6, label = unlist(df[3,3])),
            colour = "white", family = "space", angle = -55) + 
  geom_text(data = df,
            aes(x = 10, y = 0, label = unlist(df[4,3])),
            colour = "black", family = "space", angle = 0) + 
  geom_text(data = df,
            aes(x = 8, y = 10, label = unlist(df[5,3])),
            colour = "black", family = "space", angle = 55) + 
  geom_text(data = df,
            aes(x = -8, y = 10, label = unlist(df[6,3])),
            colour = "black", family = "space", angle = -55) + 
  # theme
  scale_fill_manual(values = c("#c11332", "#dbc8b0", "#efad00", 
                               "#3a5288", "#b28e73", "#131313")) +
  coord_fixed() +
  labs(title = "ASSESSED VALUATION OF ALL TAXABLE PROPERTY\nOWNED BY GEORGIA NEGROES. ") +
  theme(plot.background = element_rect(fill = "#dac8b8", colour="#dac8b8"),
        panel.background = element_rect(fill = "#dac8b8", colour="#dac8b8"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "none",
        plot.title = element_text(family = "space", face = "bold", 
                                  hjust = 0.5, size = 24, 
                                  lineheight = 0.3, 
                                  margin = margin(b = 30)),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.1, 0.8, 1.3, 0.8), "cm")) 
p

ggsave(p, filename = "2022/images/challenge_02.jpg", height = 5, width = 4, unit = "in")
