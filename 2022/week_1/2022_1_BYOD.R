library(pacman)
p_load(tidyverse, cowplot, janitor, readxl, openxlsx)

data_raw <- read_excel("transfermarkt.xlsx")

df <- data_raw |> 
  clean_names() |> 
  drop_na() |> 
  select(jugadores, goles) |> 
  mutate(group = factor(if_else(jugadores != "Lionel Messi", 0, 1)))




  df |> 
    ggplot(aes(goles, fill = group, color = group)) +
    geom_dotplot(binwidth = 6,
                 method = "histodot",
                 stackratio = 1.3,
                 dotsize = .7) +
    scale_x_continuous(limits = c(40, 510),
                     breaks = seq(0, 500, by = 50),
                     expand = c(0, 0)) +
    scale_y_continuous(breaks = NULL,
                       expand = c(0, 0.005)) +
    scale_fill_manual(values = c("grey75", "#A50044")) +
    scale_color_manual(values = c("grey75", "#A50044")) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "grey85"),
          panel.background = element_rect(fill = "#f0f0f0", color = "#f0f0f0"),
          plot.background = element_rect(fill = "#f0f0f0", color = "#f0f0f0"),
          axis.line.x = element_line(size = .4, colour = "grey50"),
          axis.ticks.length.x = unit(10, "pt"),
          axis.ticks.x = element_line(size = .4, colour = "grey85"),
          axis.text.x = element_text(margin = margin(t = 5),
                                     color = "grey50")
    )
p  
