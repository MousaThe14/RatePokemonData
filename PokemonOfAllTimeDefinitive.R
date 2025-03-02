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


##### Functions #####

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
sd.p= function(x){
  sd(x) * sqrt((length(x)-1)/length(x))
} 

#####

##### Import Data #####

# Create DesignMean Column that creates a mean across the design criteria of every pokemon sans Popularity

meanratings <- read.csv("average-ratings_enriched.csv") 
meanratings <- meanratings %>% 
  subset(select = -c(DexNum,
                     X,
                     PokeApiName,
                     Order,
                     Generation,
                     RatingCount,
                     Region)) %>%
  mutate(DesignMean = rowMeans(select(meanratings,
                                      Complexity:Beauty))) %>%
  na.omit()

MeanDesignmean <- mean(meanratings$DesignMean)


rawratings <-read.csv("all-ratings_enriched.csv")  %>% 
  subset(select = -c(DexNum,
                     X,
                     UserID,
                     Order,
                     Generation,
                     Region)) %>%
  mutate(DesignMean = rowMeans(select(rawratings,
                                      c(Complexity,
                                        Realism,
                                        Artificiality,
                                        Fantasy,
                                        Humanoid,
                                        Cuteness,
                                        Coolness,
                                        Beauty))))%>%
  na.omit()

MeanDesignraw <- mean(rawratings$DesignMean)

#####
 
##### Create tables that give us the SD of each column #####

# Population SD
pokemonSDmean <- meanratings %>% 
  summarise(Complexity = sd.p(Complexity),
            Realism = sd.p(Realism),
            Artificiality = sd.p(Artificiality),
            Fantasy = sd.p(Fantasy),
            Humanoid = sd.p(Humanoid),
            Cuteness = sd.p(Cuteness),
            Coolness =  sd.p(Coolness),
            Beauty = sd.p(Beauty),
            Popularity = sd.p(Popularity),
            DesignMeanSD = sd.p(DesignMean)) %>%  # Find the mean of the Desgin SDs sans Popularity
  mutate(MeanSD = mean(c_across(c(Complexity,
                                  Realism,
                                  Artificiality,
                                  Fantasy,
                                  Humanoid,
                                  Cuteness,
                                  Coolness,
                                  Beauty))))



# Population SD with popularity
pokemonSDmeanWPop <- meanratings %>% 
  summarise(Complexity = sd.p(Complexity),
            Realism = sd.p(Realism),
            Artificiality = sd.p(Artificiality),
            Fantasy = sd.p(Fantasy),
            Humanoid = sd.p(Humanoid),
            Cuteness = sd.p(Cuteness),
            Coolness =  sd.p(Coolness),
            Beauty = sd.p(Beauty),
            Popularity = sd.p(Popularity),
            DesignMeanSD = sd.p(DesignMean)) %>%  # Find the mean of the Desgin SDs sans Popularity
  mutate(MeanSD = mean(c_across(c(Complexity,
                                  Realism,
                                  Artificiality,
                                  Fantasy,
                                  Humanoid,
                                  Cuteness,
                                  Coolness,
                                  Beauty,
                                  Popularity))))


pokemonSDraw <- rawratings %>% 
  group_by(PokemonName) %>%
  summarise(Complexity = sd.p(Complexity),
            Realism = sd.p(Realism),
            Artificiality = sd.p(Artificiality),
            Fantasy = sd.p(Fantasy),
            Humanoid = sd.p(Humanoid),
            Cuteness = sd.p(Cuteness),
            Coolness =  sd.p(Coolness),
            Beauty = sd.p(Beauty),
            Popularity = sd.p(Popularity),
            DesignMeanSD = sd.p(DesignMean),
            MeanSD = mean(c_across(c(Complexity,
                                     Realism,
                                     Artificiality,
                                     Fantasy,
                                     Humanoid,
                                     Cuteness,
                                     Coolness,
                                     Beauty)))) 
  # %>% # Find the mean of the Desgin SDs sans Popularity
  # mutate(MeanSD = mean(c_across(c(Complexity,
  #                                Realism,
  #                                Artificiality,
  #                                Fantasy,
  #                                Humanoid,
  #                                Cuteness,
  #                                Coolness,
  #                                Beauty))))


# Sample SD

pokemonSDmeanSample <- meanratings %>% 
  summarise(Complexity = sd(Complexity),
            Realism = sd(Realism),
            Artificiality = sd(Artificiality),
            Fantasy = sd(Fantasy),
            Humanoid = sd(Humanoid),
            Cuteness = sd(Cuteness),
            Coolness =  sd(Coolness),
            Beauty = sd(Beauty),
            Popularity = sd(Popularity),
            DesignMeanSD = sd(DesignMean)) %>%  # Find the mean of the Desgin SDs sans Popularity
  mutate(MeanSD = mean(c_across(c(Complexity,
                                  Realism,
                                  Artificiality,
                                  Fantasy,
                                  Humanoid,
                                  Cuteness,
                                  Coolness,
                                  Beauty))))


# With Popularity
pokemonSDmeanSampleWPop <- meanratings %>% 
  summarise(Complexity = sd(Complexity),
            Realism = sd(Realism),
            Artificiality = sd(Artificiality),
            Fantasy = sd(Fantasy),
            Humanoid = sd(Humanoid),
            Cuteness = sd(Cuteness),
            Coolness =  sd(Coolness),
            Beauty = sd(Beauty),
            Popularity = sd(Popularity),
            DesignMeanSD = sd(DesignMean)) %>%  # Find the mean of the Desgin SDs sans Popularity
  mutate(MeanSD = mean(c_across(c(Complexity,
                                  Realism,
                                  Artificiality,
                                  Fantasy,
                                  Humanoid,
                                  Cuteness,
                                  Coolness,
                                  Beauty,
                                  Popularity))))


pokemonSDrawSample <- rawratings %>% 
  summarise(Complexity = sd(Complexity),
            Realism = sd(Realism),
            Artificiality = sd(Artificiality),
            Fantasy = sd(Fantasy),
            Humanoid = sd(Humanoid),
            Cuteness = sd(Cuteness),
            Coolness =  sd(Coolness),
            Beauty = sd(Beauty),
            Popularity = sd(Popularity),
            DesignMeanSD = sd(DesignMean)) %>% # Find the mean of the Desgin SDs sans Popularity
  mutate(MeanSD = mean(c_across(c(Complexity,
                                  Realism,
                                  Artificiality,
                                  Fantasy,
                                  Humanoid,
                                  Cuteness,
                                  Coolness,
                                  Beauty))))



#####

##### Create tables that gve us the mean of each columns #####

categorymeans <- meanratings %>%
  summarise(Complexity = mean(Complexity),
            Realism = mean(Realism),
            Artificiality = mean(Artificiality),
            Fantasy = mean(Fantasy),
            Humanoid = mean(Humanoid),
            Cuteness = mean(Cuteness),
            Coolness = mean(Coolness),
            Beauty  = mean(Beauty))

categorymeansraw <- rawratings %>%
  summarise(Complexity = mean(Complexity),
            Realism = mean(Realism),
            Artificiality = mean(Artificiality),
            Fantasy = mean(Fantasy),
            Humanoid = mean(Humanoid),
            Cuteness = mean(Cuteness),
            Coolness = mean(Coolness),
            Beauty  = mean(Beauty))
#####

##### Subtract the Design Mean and select for those under the MeanSD

# Population

meanDistanceFromMean <- meanratings %>% 
  mutate(MeanDistance = positivedeviation((DesignMean - MeanDesignmean)))

meanLowestDistanceP <- meanDistanceFromMean %>%
  filter(MeanDistance < pokemonSDmean$MeanSD) %>%
  arrange(MeanDistance) 

meanLowestDistanceP 


rawDistanceFromMean <- rawratings %>% 
  mutate(MeanDistance = positivedeviation((DesignMean - MeanDesignraw))) 



# Sample
meanLowestDistanceS <- meanDistanceFromMean %>%
  filter(MeanDistance < pokemonSDmeanSample$MeanSD) %>%
  arrange(MeanDistance)

##### Subtracting the mean of each category by itself ####
#Population
meanCategoryDistance <- meanratings %>%
  mutate(Complexity = Complexity - categorymeans$Complexity) %>%
  mutate(Realism = Realism- categorymeans$Realism) %>%
  mutate(Artificiality = Artificiality - categorymeans$Artificiality)%>%
  mutate(Fantasy = Fantasy - categorymeans$Fantasy) %>% 
  mutate(Humanoid = Humanoid - categorymeans$Humanoid) %>%
  mutate(Cuteness = Cuteness - categorymeans$Cuteness) %>%
  mutate(Coolness = Coolness - categorymeans$Coolness) %>%
  mutate(Beauty  = Beauty - categorymeans$Beauty) %>%
  mutate(across(c(Complexity,
                                        Realism,
                                        Artificiality,
                                        Fantasy,
                                        Humanoid,
                                        Cuteness,
                                        Coolness,
                                        Beauty),positivedeviation))

AverageType1 <- meanCategoryDistance %>% filter(Complexity < pokemonSDmean$MeanSD & Realism < pokemonSDmean$MeanSD &  Artificiality < pokemonSDmean$MeanSD & Fantasy < pokemonSDmean$MeanSD & Humanoid < pokemonSDmean$MeanSD & Cuteness < pokemonSDmean$MeanSD & Coolness < pokemonSDmean$MeanSD & Beauty < pokemonSDmean$MeanSD)

AverageType2 <- meanCategoryDistance %>% filter(Complexity < pokemonSDmean$Complexity & Realism < pokemonSDmean$Realism &  Artificiality < pokemonSDmean$Artificiality & Fantasy < pokemonSDmean$Fantasy & Humanoid < pokemonSDmean$Humanoid & Cuteness < pokemonSDmean$Cuteness & Coolness < pokemonSDmean$Coolness & Beauty < pokemonSDmean$Beauty)


########## Let's just get Litten and see what we're working with

Litten <- meanratings %>% filter(PokemonName == "Litten")
GDrednaw <- meanratings %>% filter(PokemonName == "Gigantamax Drednaw")
pokemonSDmean
MeanDesignmean
LittenRaw <- rawratings %>% filter(PokemonName == "Litten")


meanratings %>% filter(DesignMean >= 2.743 & DesignMean <= 2.743136) %>% arrange(order_by = DesignMean)
meanratings %>% filter(DesignMean == MeanDesignmean + pokemonSDmean$MeanSD)

pokemonSDmean$MeanSD
pokemonSDmeanSample$MeanSD
pokemonSDmeanWPop$MeanSD
round(pokemonSDmeanSampleWPop$MeanSD, 2)

(pokemonSDmean$MeanSD + pokemonSDmeanWPop$MeanSD)/2

##### The Design Divisiveness ####
# SO I misunderstood how design Divisiveness work. It's the SD of individual Pokemon, not Pokemon as a whole
# Which is also true for popularity probably. So I'll calculate those so I can add them to the averagesTable

pokemonSDraw <- rawratings %>% 
  group_by(PokemonName) %>%
  summarise(Complexity = sd.p(Complexity),
            Realism = sd.p(Realism),
            Artificiality = sd.p(Artificiality),
            Fantasy = sd.p(Fantasy),
            Humanoid = sd.p(Humanoid),
            Cuteness = sd.p(Cuteness),
            Coolness =  sd.p(Coolness),
            Beauty = sd.p(Beauty),
            PopularitySD = sd.p(Popularity),
            MeanDesignSD = mean(c_across(c(Complexity,
                                     Realism,
                                     Artificiality,
                                     Fantasy,
                                     Humanoid,
                                     Cuteness,
                                     Coolness,
                                     Beauty)))) 


meanratings <- read.csv("average-ratings_w_gens.csv")

write.csv("average-ratings_w_gens.csv")