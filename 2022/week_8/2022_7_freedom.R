library(pacman)
p_load(tidyverse, janitor, showtext, ggtext, patchwork)


# Fonts -------------------------------------------------------------------
font_add(family = "Lato",
         regular = "Lato-Regular.ttf")
font_add(family = "Lato Bold",
         regular = "Lato-Bold.ttf")
showtext_auto()


# Data --------------------------------------------------------------------
freedom <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv') |> 
  clean_names() |> 
  mutate(status = case_when(status == "F" ~ "Free",
                            status == "PF" ~ "Partially Free",
                            status == "NF" ~ "Not Free"))

freedom_status <- freedom |> 
  group_by(status) |> 
  count(year, status) |> 
  ungroup() 


# Plot 1 ------------------------------------------------------------------
plot1 <- freedom_status |>
  filter(year >= 2005) |> 
  ggplot(aes(year, n, color =  status)) +
  geom_line(show.legend = FALSE) +
  geom_text(data = filter(freedom_status, year == 2020, status == "Free"),
            aes(x = year + .25, y = n + .5, label = paste0(status, "\n", n),
                lineheight = .25, hjust = 0, size = 8), show.legend = FALSE) +
  geom_text(data = filter(freedom_status, year == 2020, status == "Partially Free"),
            aes(x = year + .25, y = n + 1.5, label = paste0(status, "\n", n),
                lineheight = .25, hjust = 0, size = 8), show.legend = FALSE) +
  geom_text(data = filter(freedom_status, year == 2020, status == "Not Free"),
            aes(x = year + .25, y = n + -2.5, label = paste0(status, "\n", n),
                lineheight = .25, hjust = 0, size = 8), show.legend = FALSE) +
  scale_x_continuous(limits = c(2005, 2023)) +
  scale_y_continuous(limits = c(35, 95),
                     breaks = seq(35, 95, by = 10)) +
  coord_cartesian(expand = c(0, 0)) +
  scale_color_manual(values = c("Free" = "grey50",
                                "Partially Free" = "grey50",
                                "Not Free" = "red")) +
  labs(title = "In 2020, the number of <span style='color:red;font-family:Lato-Bold'>**Not Free countries**</span>, countries with low political rights and/or low civil liberties,<br>in the world reached its highest level since the beginning of a 15-year period of global democratic decline") +
  theme_minimal(base_family = "Lato") +
  theme(plot.margin = margin(rep(5, 4)),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(margin = margin(r = -1)),
        plot.background = element_rect(fill = "black", color = "black"),
        panel.background = element_rect(fill = "black", color = "black"),
        plot.title = element_markdown(family = "Lato",
                                      size = 16,
                                      color = "white",
                                      hjust = 0,
                                      lineheight = .5,
                                      margin = margin(b = 15))
  )


# Plot 2 ------------------------------------------------------------------
freedom_countries <- freedom |> 
  filter(status == "Not Free", year == 2020) |> 
  select(country) 

all_names1 <- paste0(freedom_countries$country[1:9], collapse = " · ")
all_names2 <- paste0(freedom_countries$country[10:18], collapse = " · ")
all_names3 <- paste0(freedom_countries$country[19:27], collapse = " · ")
all_names4 <- paste0(freedom_countries$country[28:36], collapse = " · ")
all_names5 <- paste0(freedom_countries$country[37:45], collapse = " · ")
all_names6 <- paste0(freedom_countries$country[46:54], collapse = " · ")


plot2 <- ggplot() +
  geom_textbox(aes(6, factor(6), label = all_names1), size = 4, width = 2, height = 0.1, stat = "unique", box.padding = unit(c(2.5, 2.5, 2.5, 2.5), "pt"), fill = "red", box.colour = "black", family = "Lato Bold", color = "black", halign = 0.5) +
  geom_textbox(aes(6, factor(5), label = all_names2), size = 4, width = 2, height = 0.1, stat = "unique", box.padding = unit(c(2.5, 2.5, 2.5, 2.5), "pt"), fill = "red", box.colour = "black", family = "Lato Bold", color = "black", halign = 0.5) +
  geom_textbox(aes(6, factor(4), label = all_names3), size = 4, width = 2, height = 0.1, stat = "unique", box.padding = unit(c(2.5, 2.5, 2.5, 2.5), "pt"), fill = "red", box.colour = "black", family = "Lato Bold", color = "black", halign = 0.5) +
  geom_textbox(aes(6, factor(3), label = all_names4), size = 4, width = 2, height = 0.1, stat = "unique", box.padding = unit(c(2.5, 2.5, 2.5, 2.5), "pt"), fill = "red", box.colour = "black", family = "Lato Bold", color = "black", halign = 0.5) +
  geom_textbox(aes(6, factor(2), label = all_names5), size = 4, width = 2, height = 0.1, stat = "unique", box.padding = unit(c(2.5, 2.5, 2.5, 2.5), "pt"), fill = "red", box.colour = "black", family = "Lato Bold", color = "black", halign = 0.5) +
  geom_textbox(aes(6, factor(1), label = all_names6), size = 4, width = 2, height = 0.1, stat = "unique", box.padding = unit(c(2.5, 2.5, 2.5, 2.5), "pt"), fill = "red", box.colour = "black", family = "Lato Bold", color = "black", halign = 0.5) +
  labs(caption = "Visualization by Pablo Alvarez | Data from Freedom House and the United Nations") +
  theme_void() +
  theme(plot.margin = margin(rep(5, 4)),
        plot.background = element_rect(fill = "black", color = "black"),
        panel.background = element_rect(fill = "black", color = "black"),,
        plot.caption = element_text(family = "Lato",
                                    size = 9.5,
                                    color = "white",
                                    hjust = .5,
                                    margin = margin(t = 5))
        
  )


# Final plot --------------------------------------------------------------
patchwork <- plot1 / plot2

patchwork + plot_annotation(title = 'Freedom under Siege',
                            theme = theme(plot.title = element_markdown(size = 50,
                                                                    color = "white",
                                                                    family = "Lato Bold",
                                                                    hjust = .5,
                                                                    margin = margin(b = 5)),
                                          plot.margin = margin(5, 5, 0, 5),
                                          plot.background = element_rect(fill = "black", color = "black"),
                                          panel.background = element_rect(fill = "black", color = "black"))) 

ggsave("tidytuesday_2022_w8.png",  width = 1080, height = 1216, units= "px", dpi = 320)

