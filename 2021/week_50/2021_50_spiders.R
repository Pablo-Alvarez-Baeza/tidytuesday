# Load packages -----------------------------------------------------------
library(pacman)
p_load(tidyverse, packcircles, tidytuesdayR)


# Load data ---------------------------------------------------------------
tt <- tt_load('2021-12-07')
spiders <- tt$spiders


# Data wrangling ----------------------------------------------------------
spiders_genus <- spiders |> 
  select(label = genus) |> 
  mutate(color = if_else(label == "Latrodectus", "red", "grey"),
         position = if_else(label == "Latrodectus", 1, 0)) 

spider_genus <- spiders_genus |> 
  count(label, color, position) |> 
  # Arranging rows by position so that Latrodectus is located in row 1.
  # The first value is the one that goes in the middle of the plot.
  arrange(desc(position)) |> 
  rename(value = n)  


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

