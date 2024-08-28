library(BBmisc)
library(tidyverse)
library(ggpubr)
library(corrplot)
library(GPArotation)
library(scales)
library(data.table)
library(Rcpp)
library(readxl)
library(sf)
library(shiny)
library(viridisLite)
library(ggpmisc)
library(ggrepel)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
### Print PNG ###
printPNG = function(filename, dataset, width = 1920, height = 1080, units = "px", dpi = 300){
  
  png(filename = filename,
      width = width,
      height = height,
      res = dpi)
  dataset
  dev.off()
}


### Print SVG ###
printSVG = function(filename, dataset, width = 1920, height = 1080, units = "px", dpi = 300){
  
  svg(filename = filename,
      width = width,
      height = height,
      res = dpi)
  dataset
  dev.off()
}


raw_ratings <- read.csv("all-ratings_enriched.csv")
average_ratings <- read.csv("average-ratings_enriched.csv")

gholdengo <- raw_ratings %>% filter(PokemonName == "Gholdengo")

  
ggplot(gholdengo, aes(x = Popularity))+ 
  geom_histogram() +
  geom_boxplot()

ghost_averages <- average_ratings %>% filter(Type1 == "ghost" | Type2 == "ghost")

png(filename = "ghost_pop_distribution.png", width = 1024, height = 768)
ggplot(ghost_averages, aes(x = Popularity))+ 
  geom_histogram(fill = "purple", bins=20, color = "black") +
  geom_boxplot(linewidth = 2) +
  labs(title="Distribution of the Average Popularity of Ghost Pokemon Designs")
dev.off()

ggplot(ghost_averages, aes(x = Coolness))+ 
  geom_histogram(fill = "purple", bins=40) +
  geom_boxplot(linewidth = 2)

ice_averages <- average_ratings %>% filter(Type1 == "ice" | Type2 == "ice")

png(filename = "ice_pop_distribution.png", width = 1024, height = 768)
ggplot(ice_averages, aes(x = Popularity))+ 
  geom_histogram(fill = "powderblue", bins = 20, color = "black") +
  geom_boxplot(linewidth = 2)+
  labs(title="Distribution of the Average Popularity of Ice Pokemon Designs")
dev.off()

ggplot(ice_averages, aes(x = Coolness))+ 
  geom_histogram(fill = "powderblue") +
  geom_boxplot(linewidth = 2)



png(filename = filename,
    width = width,
    height = height,
    res = dpi)
dataset
dev.off()


printPNG("ice_pop_distribution.png",ice_graph, width = 1024, height = 768)
printPNG("ghost_pop_distribution.png",ghost_graph, width = 1024, height = 768)

