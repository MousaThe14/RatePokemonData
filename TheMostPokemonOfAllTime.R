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
library(plotly)

pokemonratings <- read.csv("average-ratings_w_gens.csv") %>% subset(select = -c(DexNum, X, PokeApiName, RatingCount, Order, Generation, Region))

pokemonratings <- pokemonratings %>% 
  mutate(DesignMean = rowMeans(select(pokemonratings, c(Complexity,
                                 Realism,
                                 Artificiality,
                                 Fantasy,
                                 Humanoid,
                                 Cuteness,
                                 Coolness,
                                 Beauty)))) %>%
  mutate(DesignSD = sd(DesignMean))


pokemonSD <- pokemonratings %>% 
  summarise(Complexity = sd(Complexity),
            Realism = sd(Realism),
            Artificiality = sd(Artificiality),
            Fantasy = sd(Fantasy),
            Humanoid = sd(Humanoid),
            Cuteness = sd(Cuteness),
            Coolness =  sd(Coolness),
            Beauty = sd(Beauty),
            Popularity = sd(Popularity),
            DesignMeanSD = sd(DesignMean))


pokemonSD <- pokemonSD %>% mutate(DesignDivisiveness = rowMeans(select(pokemonSD, c(Complexity,
                                                                          Realism,
                                                                          Artificiality,
                                                                          Fantasy,
                                                                          Humanoid,
                                                                          Cuteness,
                                                                          Coolness,
                                                                          Beauty))))

pokemonMean <- pokemonratings %>%
  summarise(Complexity = mean(Complexity),
            Realism = mean(Realism),
            Artificiality = mean(Artificiality),
            Fantasy = mean(Fantasy),
            Humanoid = mean(Humanoid),
            Cuteness = mean(Cuteness),
            Coolness = mean(Coolness),
            Beauty  = mean(Beauty),
            DesignMean = mean(DesignMean),
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


centerratings <- pokemonratings[,-11] %>% normalize(method = "center")
centerratings <- centerratings %>% 
  mutate_at(c("Complexity","Realism","Artificiality","Fantasy","Humanoid","Cuteness","Coolness", "Beauty"), squared) %>%
  mutate_at(c("Complexity","Realism","Artificiality","Fantasy","Humanoid","Cuteness","Coolness", "Beauty"), rooted)



MostAverage <- centerratings %>% filter(Complexity < pokemonSD$Complexity | Realism < pokemonSD$Realism |  Artificiality < pokemonSD$Artificiality | Fantasy < pokemonSD$Fantasy | Humanoid < pokemonSD$Humanoid | Cuteness < pokemonSD$Cuteness | Coolness < pokemonSD$Coolness | Beauty < pokemonSD$Beauty)

MostAverage2 <- centerratings %>% filter(Complexity < pokemonSD$Complexity & Realism < pokemonSD$Realism &  Artificiality < pokemonSD$Artificiality & Fantasy < pokemonSD$Fantasy & Humanoid < pokemonSD$Humanoid & Cuteness < pokemonSD$Cuteness & Coolness < pokemonSD$Coolness & Beauty < pokemonSD$Beauty)

ThePopularMostPokemonOfAllTime <- inner_join(MostAverage2, MostPopular, by = "PokemonName")


MostAverageOfAllTime <- left_join(MostAverage2, pokemonratings, by = "PokemonName") %>% na.omit()

MostAverage2 <- MostAverage2 %>% mutate(DesignSDMean = rowMeans(select(MostAverage2, c(Complexity,
                                                                                       Realism,
                                                                                       Artificiality,
                                                                                       Fantasy,
                                                                                       Humanoid,
                                                                                       Cuteness,
                                                                                       Coolness,
                                                                                       Beauty))))
  slice_min(MostAverage2, n = 20, order_by = DesignSDMean)

# scaleratings <- pokemonratings%>% normalize(method = "scale")


###################### New attempt at finding the most average pokemon of all time


ratingsDesignMean <- pokemonratings[,-2:-10] #%>% normalize(method = "center")
ratingsDesignMeanMinusMean <- ratingsDesignMean %>% mutate(SubtractedMean = DesignMean - pokemonMean$DesignMean)
ratingsDesignSD <- ratingsDesignMeanMinusMean %>% mutate(MeanDistance = rooted(squared(SubtractedMean)))
mostaverage <- ratingsDesignSD %>% filter(MeanDistance < pokemonSD$DesignMeanSD)


ratingsPopDesign <- pokemonratings[,-2:-9] %>%
  mutate(DesignDivisiveness = DesignMean - pokemonMean$DesignMean) %>%
  mutate(PopularityDivisiveness = Popularity - pokemonMean$Popularity) %>%
  mutate(DesignDivisiveness = rooted(squared(DesignDivisiveness))) %>%
  mutate(PopularityDivisiveness = rooted(squared(PopularityDivisiveness)))

slice_min(mostaverage, n = 20, order_by = MeanDistance)

mostpopularlyaverage <- inner_join(mostaverage, MostPopular, by = "PokemonName")

slice_min(mostpopularlyaverage, n = 20, order_by = MeanDistance)

######################
ratingsDesignMean <- pokemonratings[,-2:-10] #%>% normalize(method = "center")
ratingsDesignMeanMinusMean <- ratingsDesignMean %>% mutate(SubtractedMean = DesignMean - pokemonMean$DesignMean)
ratingsDesignSD <- ratingsDesignMeanMinusMean %>% mutate(MeanDistance = rooted(squared(SubtractedMean)))
mostaverage <- ratingsDesignSD %>% filter(MeanDistance < pokemonSD$DesignDivisiveness)

slice_min(mostaverage, n = 20, order_by = MeanDistance)

mostpopularlyaverage <- inner_join(mostaverage, MostPopular, by = "PokemonName")

slice_min(mostpopularlyaverage, n = 20, order_by = MeanDistance)



################## Gonna Combine the two things

theMOSTmostAverage <- inner_join(mostaverage, MostAverageOfAllTime, by = "PokemonName")

slice_min(theMOSTmostAverage, n = 20, order_by = MeanDistance)
slice_min(theMOSTmostAverage, n = 20, order_by = MeanDistance, Complexity.x,  Realism.x, Artificiality.x, Fantasy.x)


###################### let's plot this stupid thing

ratingsPopDesignNormal <- ratingsPopDesign %>% normalize(method = "range", range = c(-2,2))


sdDD <- ggplot(ratingsPopDesign, aes(x = DesignDivisiveness, y = PopularityDivisiveness, label = PokemonName))+
  geom_point()

ggplotly(sdDD)