
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




ugliest30 <- average_ratings %>% slice_min(Beauty, n = 30)
uncute30 <- average_ratings %>% slice_min(Cuteness, n = 30)

UglyUncute <- full_join(ugliest30, uncute30)

ugliest31 <- average_ratings %>% slice_min(Beauty, n = 31)
uncute31 <- average_ratings %>% slice_min(Cuteness, n = 31)

UglyUncute2 <- full_join(ugliest31, uncute31)

UglyUncuteCommon <- inner_join(ugliest31, uncute31)
