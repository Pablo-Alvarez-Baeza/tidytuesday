# Load packages -----------------------------------------------------------
library(pacman)
p_load(tidyverse, packcircles, tidytuesdayR, ggtext)

font_add_google("Lato")


# Load data ---------------------------------------------------------------
tt <- tt_load('2021-12-07')
spiders <- tt$spiders


# Data wrangling ----------------------------------------------------------
spiders |>
  filter(family == "Theridiidae") |> 
  group_by(genus) |> 
  count() |>
  ungroup() |> 
  mutate(total = sum(n),
         perc = round(n / total, 3) * 100) |> View()
  
  filter(genus == "Latrodectus")

  summarize(total = sum(n),
            perc = round(n / total, 3))

spiders_genus <- spiders |> 
  select(family, genus) |> 
  mutate(color = if_else(genus == "Latrodectus", "red", "white"),
         position = if_else(genus == "Latrodectus", 1, 0)) 



spider_genus <- spiders_genus |> 
  count(genus, color, position) |> 
  # Arranging rows by position so that Latrodectus is located in row 1.
  # The first value is the one that goes in the middle of the plot.
  arrange(desc(position)) |> 
  rename(value = n,
         label = genus)  

spider_genus |> 
  mutate(total = sum(value),
         perc = round(value / total, 4) * 100) |> View()
  


# Generate the layout. This function return a data frame with one line per bubble. 
# It gives its center (x and y) and its radius, proportional of the value.
packing <- circleProgressiveLayout(spider_genus$value) 


# Multiplying the radius by any number below 0 to decrease bubble size and therefore
# add a bit of space between each bubble
packing$radius <- 0.95 * packing$radius


# Adding packing information to the initial data frame.
data <- cbind(spider_genus, packing) 


# The next step is to go from one center + a radius to the coordinates of a circle that
# is drawn by a multitude of straight lines.
dat.gg <- circleLayoutVertices(packing)


# Basic plot --------------------------------------------------------------
plot <- ggplot(data = dat.gg) +
  # Make the bubbles
  geom_polygon(aes(x, y, group = id, fill = factor(id)), 
               colour = "black",
               show.legend = FALSE) +
  scale_fill_manual(values = data$color) +
  coord_equal() 


# Final plot --------------------------------------------------------------
theme_set(theme_void(base_family = "Lato"))

plot +
  labs(title = "A") +
  theme(
    plot.margin = margin(c(5, 5, 5, 5)),
    plot.background = element_rect(fill = "black", color = "black"),
    panel.background = element_rect(fill = "black", color = "black"),
    # Customize title appearance
    plot.title = element_markdown(
      color = "grey25", 
      size = 38, 
      face = "bold",
      margin = margin(t = 15)
    ),
    # Customize subtitle appearance
    plot.subtitle = element_markdown(
      color = "grey50",
      size = 26,
      lineheight = 1.35,
      margin = margin(t = 15, b = 40)
    ),
    # Title and caption are going to be aligned
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_markdown(
      color = "grey30", 
      size = 13,
      lineheight = 1.2, 
      hjust = 0,
      margin = margin(t = 40) # Large margin on the top of the caption.
    ) 
  )
ggsave("tidytuesday_2021_w50.png", width = 10, height = 15, units = "in", dpi = 320)


