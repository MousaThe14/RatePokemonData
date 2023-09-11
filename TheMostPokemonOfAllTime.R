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

pokemonratings <- read.csv("average-ratings_w_gens.csv") %>% subset(select = -c(DexNum, X, PokeApiName, RatingCount, Order, Generation, Region))

pokemonSD <- pokemonratings %>% 
  summarise(Complexity = sd(Complexity),
            Realism = sd(Realism),
            Artificiality = sd(Artificiality),
            Fantasy = sd(Fantasy),
            Humanoid = sd(Humanoid),
            Cuteness = sd(Cuteness),
            Coolness =  sd(Coolness),
            Beauty = sd(Beauty),
            Popularity = sd(Popularity))

pokemonMean <- pokemonratings %>%
  summarise(Complexity = mean(Complexity),
            Realism = mean(Realism),
            Artificiality = mean(Artificiality),
            Fantasy = mean(Fantasy),
            Humanoid = mean(Humanoid),
            Cuteness = mean(Cuteness),
            Coolness = mean(Coolness),
            Beauty  = mean(Beauty),
            Popularity = mean(Popularity))

rangeratings <- pokemonratings%>% normalize(method = "range", range = c(-2,2))

MostPopular <- pokemonratings %>% filter(Popularity > mean(pokemonMean$Popularity))

TheMostPokemonOfAllTime <- scaleratings %>% filter(Complexity == 0 | Realism == 0|  Artificiality == 0 | Fantasy == 0 | Humanoid == 0 | Cuteness == 0 | Coolness == 0 | Beauty == 0)

squared <- function(listed){
  answer <- listed^2
  return(answer)
}

rooted  <- function(listed){
  answer <- sqrt(listed)
  return(answer)
}


centerratings <- pokemonratings%>% normalize(method = "center")
centerratings <- centerratings %>% 
  mutate_at(c("Complexity","Realism","Artificiality","Fantasy","Humanoid","Cuteness","Coolness", "Beauty", "Popularity"), squared) %>%
  mutate_at(c("Complexity","Realism","Artificiality","Fantasy","Humanoid","Cuteness","Coolness", "Beauty", "Popularity"), rooted)

MostAverage <- centerratings %>% filter(Complexity < pokemonSD$Complexity | Realism < pokemonSD$Realism|  Artificiality < pokemonSD$Artificiality | Fantasy < pokemonSD$Fantasy | Humanoid < pokemonSD$Humanoid | Cuteness < pokemonSD$Cuteness | Coolness < pokemonSD$Coolness | Beauty < pokemonSD$Beauty | Popularity < pokemonSD$Popularity)

MostAverage2 <- centerratings %>% filter(Complexity < pokemonSD$Complexity & Realism < pokemonSD$Realism &  Artificiality < pokemonSD$Artificiality & Fantasy < pokemonSD$Fantasy & Humanoid < pokemonSD$Humanoid & Cuteness < pokemonSD$Cuteness & Coolness < pokemonSD$Coolness & Beauty < pokemonSD$Beauty & Popularity < pokemonSD$Popularity)

TheMostPokemonOfAllTime2 <- inner_join(MostAverage2, MostPopular, by = "PokemonName")


MostAverageOfAllTime <- left_join(MostAverage2, pokemonratings, by = "PokemonName") %>% na.omit()


scaleratings <- pokemonratings%>% normalize(method = "scale")