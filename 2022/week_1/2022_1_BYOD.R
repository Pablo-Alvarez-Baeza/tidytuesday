library(pacman)
p_load(tidyverse, cowplot, janitor, readxl, openxlsx, showtext, shadowtext)

font_add_google("Lato")
showtext_auto()

data_raw <- read_excel("transfermarkt.xlsx")

df <- data_raw |> 
  clean_names() |> 
  drop_na() |> 
  select(jugadores, goles) |> 
  mutate(group = factor(if_else(jugadores != "Lionel Messi", 0, 1)))




plot <- df |> 
  ggplot(aes(goles, fill = group, color = group)) +
  geom_dotplot(binwidth = 6,
               method = "histodot",
               stackratio = 1.2,
               dotsize = .8,
               show.legend = FALSE) +
  scale_x_continuous(limits = c(40, 510),
                     breaks = seq(0, 500, by = 50),
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0.005))

plot <- plot +
  geom_curve(x = 445, y = .5,
             xend = 478, yend = .03,
             color = "grey50",
             size = .6,
             curvature = -.3,
             angle = 90,
             arrow = arrow(length = unit(0.25,"cm"))) +
  geom_curve(x = 275, y = .3,
             xend = 312, yend = .03,
             color = "grey75",
             size = .6,
             curvature = -.3,
             angle = 90,
             arrow = arrow(length = unit(0.25,"cm"))) +
  geom_shadowtext(aes(x = 275, y = .32, label = "Cristiano Ronaldo"),
                  color = "grey65", size = 4, family = "Lato", bg.colour = "white", bg.r = .2) 

# Alternative to shadowtext
#annotate(geom = "text",
         #x = 275, y = .32,
         #color = "grey50",
         #label = "Cristiano Ronaldo",
         #size = 4,
         #family = "Lato")

plot <- plot +
  scale_fill_manual(values = c("grey75", "#A50044")) +
  scale_color_manual(values = c("grey75", "#A50044")) +
  labs(x = "Players total goals scored",
       title = "Messi, La Liga all-time top scorer: 474 goals",
       subtitle = "Number of La Liga top scorers by goals, 1928/29 - 2020/21",
       caption = "Visualization by Pablo Alvarez | Data from Transfermarkt") +
  theme_minimal(base_family = "Lato") +
  theme(plot.margin = margin(10, 12, 10, 12),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey85"),
        panel.grid.major.y = element_blank(),
        panel.background = element_rect(fill = "#f0f0f0", color = "#f0f0f0"),
        plot.background = element_rect(fill = "#f0f0f0", color = "#f0f0f0"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.x = element_line(size = .4, colour = "grey50"),
        axis.ticks.length.x = unit(10, "pt"),
        axis.ticks.x = element_line(size = .4, colour = "grey85"),
        axis.text.x = element_text(margin = margin(t = 5),
                                   color = "grey50", size = 12),
        axis.title.x = element_text(margin = margin(t = 20),
                                   color = "black", face = "bold", size = 14,
                                   family = "Lato Black (900)"),
        plot.title = element_text(
          size = 32,
          margin = margin(t = 15),
          face = "bold",
          family = "Lato Black (900)"
        ),
        plot.subtitle = element_text(
          size = 18,
          margin = margin(t = 20, b = 20)
        ),
        plot.caption = element_text(
          color = "grey50",
          hjust = .5,
          size = 10,
          margin = margin(t = 15, b = 5)
        )
    )


ggdraw(plot) +
  draw_image("messi.png",
             x = 1.50, y = .55,
             hjust = 4.2, vjust = .45,
             width = 0.2, height = 0.2)

ggsave("tidytuesday_2022_w1.png", width = 8, height = 8.5, units = "in", dpi = 320)
