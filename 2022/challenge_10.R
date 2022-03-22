library(tidyverse)
library(showtext)

# load data
df <- readr::read_csv("2022/data/10.csv")

# load fonts
font_add_google(name = "Space Mono", family = "space")
showtext_auto()

# prep data
plot_df <- df %>%
  mutate(rel_size = c(0.6, 0.7, 1),
         red = `Percent Enrolled`*rel_size,
         black = 100*(1 - 0.01*`Percent Enrolled`)*rel_size, 
         blank = 100 - (red + black)) %>% 
  select(Year, red, black, blank) %>% 
  pivot_longer(cols = c(red, black, blank), 
               values_to = "prop", 
               names_to = "type") %>% 
  mutate(type = factor(type, levels = rev(c("blank", "black", "red"))))

text_df <- data.frame(x = df$Year,
                      y = c(90, 80, 70), 
                      label = paste0(df$`Percent Enrolled`, "%"))

year_df <- data.frame(x = df$Year,
                      y = rep(102, 3), 
                      label = df$Year)

# plot
p <- ggplot() +
  geom_col(data = plot_df, 
           mapping = aes(x = Year, y = prop, fill = type), 
           width = 5) +
  geom_text(data = text_df, 
            mapping = aes(x = x, y = y, label = label), 
            family = "space", fontface = "bold", size = 10) +
  geom_text(data = year_df, 
            mapping = aes(x = x, y = y, label = label), 
            family = "space", fontface = "bold", size = 9) +
  annotate("text", x = 1874, y = 23, label = "PROPORTION\nPROPORTION", hjust = 1, family = "space", lineheight = 0.5) +
  annotate("text", x = 1874, y = 9, label = "PROPORTION\nPROPORTION", hjust = 1, family = "space", lineheight = 0.5) +
  scale_fill_manual(values = c("blank" = "#dfd3c2", "red"="#cc0027", "black"="#0d0e0b"), 
                    breaks = c("red", "blank", "black"), 
                    labels = c("OF CHILDREN ENROLLED\nD'ENFANTS ENREGISTRES", 
                               "", 
                               "OF CHILDREN NOT ENROLLED\nD'ENFANTS NON ENREGISTRES")) +
  scale_x_continuous(limits = c(1870, 1901)) +
  labs(x = "", y = "", 
       title = "PROPORTION OF TOTAL NEGRO CHILDREN OF SCHOOL AGE WHO ARE ENROLLED IN PUBLIC SCHOOLS.\n\nPROPORTION DES ENFANTS NEGRES EN AGE D'ECOLE ENREGISTRES DANS LES ECOLES PUBLIQUES.", 
       subtitle = "\nDONE BY ATLANTA UNIVERSITY.") +
  theme(legend.position = c(0.35, 0.2), 
        legend.background = element_rect(fill = "transparent", colour="transparent"), 
        legend.title = element_blank(),
        legend.text = element_text(family = "space", hjust = 0, size = 12, lineheight = 0.5),
        legend.key = element_rect(fill = "#dfd3c2", colour="#dfd3c2"),
        plot.background = element_rect(fill = "#dfd3c2", colour="#dfd3c2"),
        panel.background = element_rect(fill = "#dfd3c2", colour="#dfd3c2"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        plot.title = element_text(family = "space", face = "bold", hjust = 0.5, lineheight = 0.5, size = 16),
        plot.subtitle = element_text(family = "space", hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(1, 0.5, 1, 0.5), "cm"))
p

ggsave(p, filename = "2022/images/challenge_10.jpg", height = 5, width = 4, unit = "in")

