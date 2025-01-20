
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
library(splitstackshape)

average_ratings <- read.csv("average-ratings_enriched.csv")
raw_ratings <- read.csv("all-ratings_enriched.csv")

##### Most Popular By Generation

TopGen1 <- average_ratings %>% filter(Generation == "1") %>% slice_max(Popularity, n = 10)
TopGen2 <- average_ratings %>% filter(Generation == "2") %>% slice_max(Popularity, n = 10)
TopGen3 <- average_ratings %>% filter(Generation == "3") %>% slice_max(Popularity, n = 10)
TopGen4 <- average_ratings %>% filter(Generation == "4") %>% slice_max(Popularity, n = 10)
TopGen5 <- average_ratings %>% filter(Generation == "5") %>% slice_max(Popularity, n = 10)
TopGen6 <- average_ratings %>% filter(Region == "Kalos") %>% slice_max(Popularity, n = 20)
TopGen7 <- average_ratings %>% filter(Generation == "7") %>% slice_max(Popularity, n = 10)
TopGen8 <- average_ratings %>% filter(Region == "Galar") %>% slice_max(Popularity, n = 10)
TopGen9 <- average_ratings %>% filter(Generation == "9") %>% slice_max(Popularity, n = 10) 

