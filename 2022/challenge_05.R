library(tidyverse)
library(showtext)

# load data
df <- readr::read_csv("2022/data/05.csv")

# load fonts
font_add_google(name = "Space Mono", family = "space")
showtext_auto()

# prep data
df_long <- df %>% 
  mutate(Slave = Slave - 97) %>% 
  pivot_longer(cols = c(Slave, Free), names_to = "Type", values_to = "perc") %>% 
  mutate(Type = factor(Type, levels = c("Slave", "Free")))
df_long[17, 3] <- 0.000001
df_long[18, 3] <- 3 - 0.000001
df_long <- df_long %>% 
  add_row(Year = 1863, Type = "Slave", perc = 0.000001) %>% 
  add_row(Year = 1863, Type = "Free", perc = (3 - 0.000001)) %>% 
  mutate(Type = factor(Type, levels = c("Slave", "Free")))


df_wide <- df %>% 
  mutate(Slave = Slave - 97, 
         label = paste0(df_wide$Free, c("%", rep("", 7), "%")))

# plot
p <- ggplot() +
  geom_area(data = df_long, aes(x = Year, y = perc, fill = Type)) +
  geom_text(data = df_wide, aes(x = Year, y = -0.3, label = label), family="space", size = 5) +
  geom_segment(data = filter(df_long, Year != 1863), aes(x = Year, xend = Year, y = 0, yend = 3), colour = "#ddccbb", size = 0.3) +
  annotate("text", x = 1788, y = c(1, 2, 3), label = paste(1:3, "%", sep = ""), family="space", size = 5) +
  annotate("text", x = 1787, y = -0.3, label = "PERCENT\nOF\nFREE NEGROES", family="space", size = 4, lineheight = 0.3) +
  labs(title = "SLAVES AND FREE NEGROES.", 
       x = "", y = "") +
  scale_fill_manual("", values = c("Slave" = "#101010", "Free" = "#cf334e")) +
  scale_x_reverse(breaks = c(seq(1870,1790,-10)), limits = c(1873, 1787)) +
  scale_y_reverse(position = "right", limits=c(NA,-0.5), breaks = c(1:3), labels = c("", "", "")) +
  coord_flip() +
  theme(plot.background = element_rect(fill = "#ddccbb", colour="#ddccbb"),
        panel.background = element_rect(fill = "#ddccbb", colour="#ddccbb"),
        legend.position = "none", 
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.margin = unit(c(0.5, 2.5, 1.2, 2.5), "cm"),
        plot.title = element_text(colour = "black", size=24, face = "bold", 
                                  hjust = 0.4, family="space", margin = margin(b = -10)), 
        axis.text = element_text(colour = "black", size=16, hjust = 0.5, family="space"), 
        axis.text.x = element_text(margin = margin(c(-80, -80, -80, -80))))
p

ggsave(p, filename = "2022/images/challenge_05.jpg", height = 5, width = 4, unit = "in")
