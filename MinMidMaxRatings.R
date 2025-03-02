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
library(GGally)

average_ratings <- read.csv("average-ratings_enriched.csv")
raw_ratings <- read.csv("all-ratings_enriched.csv")
globalAverageTraits <- read.csv("GlobalAverages.csv")

Categories <- c("Complexity",
                "Realism",
                "Artificiality",
                "Fantasy",
                "Humanoid",
                "Cuteness",
                "Coolness",
                "Beauty",
                "Popularity")

#Mathematical functions I use to convert negative SDs into positive SDs
squared <- function(listed){
  answer <- listed^2
  return(answer)
}
rooted  <- function(listed){
  answer <- sqrt(listed)
  return(answer)
}

positivedeviation <- function(listed) {
  answer <- rooted(squared(listed))
  return(answer)
}
# Today I learned sd() is the SAMPLE standard deviation.
##### I'm using the population SD so I need to need to make my own function for it#####
sd.p = function(x){
  sd(x) * sqrt((length(x)-1)/length(x))
} 
### Get the Mode ###
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Standard Deviations measures variation from the mean.
# The bigger the SD, the more different ratings
# Basically the smaller the SD, The closer to average a pokemon is

standardD <- average_ratings %>% 
  summarise(Complexity = sd.p(Complexity),
            Realism = sd.p(Realism),
            Artificiality = sd.p(Artificiality),
            Fantasy = sd.p(Fantasy),
            Humanoid = sd.p(Humanoid),
            Cuteness = sd.p(Cuteness),
            Coolness =  sd.p(Coolness),
            Beauty = sd.p(Beauty),
            Popularity = mean(PopularitySD),
            MeanDesignSD = mean(MeanDesignSD))

# People agree the least on how Humanoid a pokemon is and what they like
# People agree the most on how realistic a Pokemon is

globalAverageRounded <- round(globalAverageTraits, 2)
standardDRounded <- round(standardD, 2)
average_ratingsRounded <- average_ratings %>% mutate(across(all_of(Categories), round, 2))



coolRange <- average_ratingsRounded %>% filter(between(Coolness, globalAverageRounded$Coolness - standardDRounded$Coolness,
                                                globalAverageRounded$Coolness + standardDRounded$Coolness))

cuteRange <-  average_ratingsRounded %>% filter(between(Cuteness, globalAverageRounded$Cuteness - standardDRounded$Cuteness,
                                                 globalAverageRounded$Cuteness + standardDRounded$Cuteness))
beautyRange <- average_ratingsRounded %>% filter(between(Beauty, globalAverageRounded$Beauty - standardDRounded$Beauty,
                                   globalAverageRounded$Beauty + standardDRounded$Beauty))
popularityRange <- average_ratingsRounded %>% filter(between(Popularity, globalAverageRounded$Popularity - standardDRounded$Popularity,
                                                         globalAverageRounded$Popularity + standardDRounded$Popularity))



medianBeauty <- beautyRange %>% filter(Beauty == median(Beauty))
medianCute <- cuteRange %>% filter(Cuteness == median(Cuteness))
medianPopularity <- popularityRange %>% filter(Popularity == median(Popularity))