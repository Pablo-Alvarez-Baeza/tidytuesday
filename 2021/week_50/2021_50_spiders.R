# Useful information ------------------------------------------------------
# A scientific article about widow spiders from 2021.
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8553018/

# A useful tutorial teaching how to build a basic circle packing chart with only one level of hierarchy.
# https://www.r-graph-gallery.com/305-basic-circle-packing-with-one-level.html


# Load packages -----------------------------------------------------------
library(pacman)
p_load(tidyverse, packcircles, tidytuesdayR, showtext)

font_add_google("Lato")
showtext_auto()


# Load data ---------------------------------------------------------------
tt <- tt_load('2021-12-07')
spiders <- tt$spiders


# Data wrangling ----------------------------------------------------------

# There're 4232 different spider genus. Latrodectus (widow spiders), 
# represent 1 / 4232 
spiders |> 
  distinct(genus)


spiders_genus <- spiders |> 
  select(family, genus) |> 
  mutate(color = if_else(genus == "Latrodectus", "red", "white"),
         position = if_else(genus == "Latrodectus", 1, 0)) 


spider_genus <- spiders_genus |> 
  count(genus, color, position) |> 
  # Arranging rows by position so that Latrodectus is located in row 1.
  # The first value is the one that goes in the middle of the plot by default.
  arrange(desc(position)) |> 
  rename(value = n,
         label = genus)  

  
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
  labs(title = "<span style='color:red'>Widow spiders</span> make up less than 1% of all spiders",
       subtitle = "yet their venom has transformed this small group into a major symbol of arachnophobia in many cultures worldwide",
       caption= "Visualization by Pablo Alvarez • Data from TidyTuesday | 2021 - Week 50<br>Each bubble represents a different spider genus. A bubble's size represents the number of species in that genus.") +
  theme(
    plot.margin = margin(c(20, 20, 20, 20)),
    plot.background = element_rect(fill = "black", color = "black"),
    panel.background = element_rect(fill = "black", color = "black"),
    # Customize title appearance
    plot.title = element_markdown(
      color = "white", 
      size = 40, 
      face = "bold",
      hjust = 0.5,
      margin = margin(t = 15)
    ),
    # Customize subtitle appearance
    plot.subtitle = element_markdown(
      color = "white",
      size = 20,
      hjust = 0.5,
      margin = margin(t = 10, b = 40)
    ),
    # Title and caption are going to be aligned
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_markdown(
      color = "white", 
      size = 13,
      lineheight = 1.2, 
      hjust = 0.5,
      margin = margin(t = 40) # Large margin on the top of the caption.
    ) 
  )

ggsave("tidytuesday_2021_w50.png", width = 15, height = 15, units = "in", dpi = 320)


