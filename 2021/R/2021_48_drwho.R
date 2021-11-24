library(pacman)
p_load(tidyverse, tidytuesdayR)

# Get the data
tuesdata <- tidytuesdayR::tt_load('2021-11-23')
tidytuesdayR::readme(tuesdata)

# Data frames
directors <- tuesdata$directors
episodes <- tuesdata$episodes
writers <- tuesdata$writers
imdb <- tuesdata$imdb


