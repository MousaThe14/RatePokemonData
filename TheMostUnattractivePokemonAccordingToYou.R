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
globalAverage <- read.csv("GlobalAverages.csv")

# Global Averages
# Popularity  3.535
# Beauty      3.019
# Cuteness    3.019


ugliest30 <- average_ratings %>% slice_min(Beauty, n = 30)
uncute30 <- average_ratings %>% slice_min(Cuteness, n = 30)

UglyUncute <- full_join(ugliest30, uncute30)

ugliest31 <- average_ratings %>% slice_min(Beauty, n = 31)
uncute31 <- average_ratings %>% slice_min(Cuteness, n = 31)

UglyUncute2 <- full_join(ugliest31, uncute31)

UglyUncuteCommon <- inner_join(ugliest31, uncute31)

LeastPopular50 <- average_ratings %>% slice_min(Popularity, n = 50)

UnpopularUnattractive <- inner_join(UglyUncute2, LeastPopular50)


averages_beauty_cute_ratio <- average_ratings %>% mutate(BeautyOverCute = Beauty/Cuteness)






  
ggplot(average_ratings, aes(x = Cuteness, y = Beauty)) +
  geom_point() +
  # geom_smooth(se=FALSE) +
  stat_poly_line() +
  stat_poly_eq()
