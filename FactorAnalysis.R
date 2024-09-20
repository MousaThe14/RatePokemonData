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
library(psych)
library(nFactors)
library(reshape2)
library(Hmisc)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

corr_function <- function(data, variables){
  data <- data %>% subset(select = variables)
  corrplot(cor(data), addCoef.col ='black', type = 'lower')
}



Categories <- c("Complexity",
                "Realism",
                "Artificiality",
                "Fantasy",
                "Humanoid",
                "Cuteness",
                "Coolness",
                "Beauty")



pokemonratings <- read.csv("all-ratings_enriched.csv") %>% na.omit()
pokemonaverages <- read.csv("average-ratings_enriched.csv")




byType <-  pokemonratings %>%   
  pivot_longer(cols = c(Type1, Type2), names_to = "TypeSlot", values_to = "Type") %>%
  subset(select = -c(TypeSlot, TypeID1, TypeID2)) %>%
  mutate(TypeID = case_when(Type == "Normal" ~ 1,
                            Type == "Fighting" ~ 2,
                            Type == "Flying" ~ 3,
                            Type == "Poison" ~4,
                            Type =="Ground" ~ 5,
                            Type =="Rock" ~ 6,
                            Type == "Bug" ~ 7,
                            Type == "Ghost" ~ 8, 
                            Type== "Steel" ~ 9,
                            Type == "Fire" ~ 10,
                            Type == "Water" ~ 11,
                            Type == "Grass" ~ 12,
                            Type == "Electric" ~ 13,
                            Type == "Psychic" ~ 14,
                            Type == "Ice" ~ 15,
                            Type == "Dragon" ~ 16,
                            Type == "Dark" ~ 17,
                            Type == "Fairy" ~ 18,
                            Type == "No 2nd Type" ~ 19)) %>%
  filter(Type != "No 2nd Type")


byTypeAverages <-  pokemonaverages %>%   
  pivot_longer(cols = c(Type1, Type2), names_to = "TypeSlot", values_to = "Type") %>%
  subset(select = -c(TypeSlot, TypeID1, TypeID2)) %>%
  mutate(TypeID = case_when(Type == "Normal" ~ 1,
                            Type == "Fighting" ~ 2,
                            Type == "Flying" ~ 3,
                            Type == "Poison" ~4,
                            Type =="Ground" ~ 5,
                            Type =="Rock" ~ 6,
                            Type == "Bug" ~ 7,
                            Type == "Ghost" ~ 8, 
                            Type == "Steel" ~ 9,
                            Type == "Fire" ~ 10,
                            Type == "Water" ~ 11,
                            Type == "Grass" ~ 12,
                            Type == "Electric" ~ 13,
                            Type == "Psychic" ~ 14,
                            Type == "Ice" ~ 15,
                            Type == "Dragon" ~ 16,
                            Type == "Dark" ~ 17,
                            Type == "Fairy" ~ 18,
                            Type == "No 2nd Type" ~ 19)) %>%
  filter(Type != "No 2nd Type")

factors <- Categories <- c("Complexity",
                           "Realism",
                           "Artificiality",
                           "Fantasy",
                           "Humanoid",
                           "Cuteness",
                           "Coolness",
                           "Beauty",
                           "Generation",
                           "TypeID")

factorsSansTG <- c("Complexity",
                   "Realism",
                   "Artificiality",
                   "Fantasy",
                   "Humanoid",
                   "Cuteness",
                   "Coolness",
                   "Beauty")

pokemonratings_reduced <- byType %>% subset(select=factors)
pokemonaverages_reduced <- byTypeAverages %>% subset(select=factors)

noramlizedAverages <- pokemonaverages_reduced %>% normalize(factorsSansTG, method = "range", range = c(0,1))

typeAndGen <- pokemonaverages_reduced %>% subset(select=c("TypeID", "Generation"))
noramlizedAverages <- noramlizedAverages %>% subset(select=c("Complexity",
                                                      "Realism",
                                                      "Artificiality",
                                                      "Fantasy",
                                                      "Humanoid",
                                                      "Cuteness",
                                                      "Coolness",
                                                      "Beauty"))

noramlizedAverages <- noramlizedAverages %>% cbind(typeAndGen)


KMOStat10Factor <- KMO(noramlizedAverages)

ratings10factors <- fa(r = noramlizedAverages, nfactors = 10)
factor10score <- factor.scores(noramlizedAverages, fa(r = noramlizedAverages, nfactors = 10))
describe(factor10score$score)

ratings8factors <- fa(r = noramlizedAverages %>% subset(select=c("Complexity",
                                                                 "Realism",
                                                                 "Artificiality",
                                                                 "Fantasy",
                                                                 "Humanoid",
                                                                 "Cuteness",
                                                                 "Coolness",
                                                                 "Beauty")), 
         nfactors = 8)



KMOStat8Factor <- KMO(noramlizedAverages %>% subset(select=c("Complexity",
                                                             "Realism",
                                                             "Artificiality",
                                                             "Fantasy",
                                                             "Humanoid",
                                                             "Cuteness",
                                                             "Coolness",
                                                             "Beauty")))

factor8score <- factor.scores(noramlizedAverages %>% subset(select=c("Complexity",
                                                                    "Realism",
                                                                    "Artificiality",
                                                                    "Fantasy",
                                                                    "Humanoid",
                                                                    "Cuteness",
                                                                    "Coolness",
                                                                    "Beauty")), ratings8factors)
describe(factor8score$score)





KMOStat9Factor <- KMO(noramlizedAverages %>% subset(select=c("Complexity",
                                                             "Realism",
                                                             "Artificiality",
                                                             "Fantasy",
                                                             "Humanoid",
                                                             "Cuteness",
                                                             "Coolness",
                                                             "Beauty",
                                                             "TypeID")))