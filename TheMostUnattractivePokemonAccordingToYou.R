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
globalAverageTraits <- read.csv("GlobalAverages.csv")
globalAverageGenerations <- read.csv("GlobalAverage-Gen.csv")

globalAverageTypes <- read.csv( "GlobalAverages-Type.csv")
# Global Averages
# Popularity  3.533
# Beauty      3.019
# Ugliest Type    - Poison - 2.669
# Ugliest Gen     -  Gen 8 - 2.878
# Cuteness    3.019
# Least Cute Type - Steel - 2.488
# Least Cute Gen  - 8 - 2.801
# Most Popular Gen - Gen 2 - 3.682
# Least Popular Gen - Gen 9 - 3.356 
# Most Popular Type - Ghost - 3.810
# Least Popular Type - Normal - 3.402


ugliest30 <- average_ratings %>% slice_min(Beauty, n = 30)
uncute30 <- average_ratings %>% slice_min(Cuteness, n = 30)

UglyUncute <- full_join(ugliest30, uncute30)
 
# ugliest31 <- average_ratings %>% slice_min(Beauty, n = 31)
# uncute31 <- average_ratings %>% slice_min(Cuteness, n = 31)
# 
# UglyUncute2 <- full_join(ugliest31, uncute31)

UglyUncuteCommon <- inner_join(ugliest30, uncute30)

LeastPopular50 <- average_ratings %>% slice_min(Popularity, n = 50)

UnpopularUnattractive <- inner_join(UglyUncute, LeastPopular50)

LeastPopLeastCute <- inner_join(LeastPopular50, uncute30)
LeastPopUgly <- inner_join(LeastPopular50, ugliest30)

UncuteUglyUnpopular <- full_join(LeastPopUgly, LeastPopLeastCute)



averages_beauty_cute_ratio <- average_ratings %>% mutate(BeautyOverCute = Beauty/Cuteness)



JynxRatings5 <- raw_ratings %>% filter(PokemonName == "Jynx" & Popularity == 5)
JynxRatings4 <- raw_ratings %>% filter(PokemonName == "Jynx" & Popularity == 4)
nrow(JynxRatings4)
nrow(JynxRatings5)
  
ggplot(average_ratings, aes(x = Cuteness, y = Beauty)) +
  geom_point() +
  # geom_smooth(se=FALSE) +
  stat_poly_line() +
  stat_poly_eq()
