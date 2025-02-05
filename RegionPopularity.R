
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
TopGen6 <- average_ratings %>% filter(Region == "Kalos") %>% slice_max(Popularity, n = 10)
TopGen7 <- average_ratings %>% filter(Generation == "7") %>% slice_max(Popularity, n = 10)
TopGen8 <- average_ratings %>% filter(Region == "Galar") %>% slice_max(Popularity, n = 10)
TopGen9 <- average_ratings %>% filter(Generation == "9") %>% slice_max(Popularity, n = 10) 

BotGen1 <- average_ratings %>% filter(Generation == "1") %>% slice_min(Popularity, n = 1)
BotGen2 <- average_ratings %>% filter(Generation == "2") %>% slice_min(Popularity, n = 1)
BotGen3 <- average_ratings %>% filter(Generation == "3") %>% slice_min(Popularity, n = 1)
BotGen4 <- average_ratings %>% filter(Generation == "4") %>% slice_min(Popularity, n = 1)
BotGen5 <- average_ratings %>% filter(Generation == "5") %>% slice_min(Popularity, n = 1)
BotGen6 <- average_ratings %>% filter(Generation == "6") %>% slice_min(Popularity, n = 1)
BotGen7 <- average_ratings %>% filter(Generation == "7") %>% slice_min(Popularity, n = 1)
BotGen8 <- average_ratings %>% filter(Generation == "8") %>% slice_min(Popularity, n = 1)
BotGen9 <- average_ratings %>% filter(Generation == "9") %>% slice_min(Popularity, n = 1)

BottomGen <- BotGen1 %>%
  rbind(BotGen2) %>%
  rbind(BotGen3) %>%
  rbind(BotGen4) %>%
  rbind(BotGen5) %>%
  rbind(BotGen6) %>%
  rbind(BotGen7) %>%
  rbind(BotGen8) %>%
  rbind(BotGen9) 
