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

pokemon_rates <- read.csv("all-ratings.csv")
columnnames <- c("DexNum",
                 "PokemonName",
                 "UserID",
                 "Timestamp",
                 "Complexity",
                 "Realism",
                 "Artificiality",
                 "Fantasy",
                 "Humanoid",
                 "Cuteness",
                 "Coolness",
                 "Beauty",
                 "Popularity")

raw_rates_list <- unique(pokemon_rates$PokemonName)

names(pokemon_rates) <- columnnames

pokemon_rates <- pokemon_rates %>% group_by(PokemonName)

pokemon_averages <- read.csv("average-ratings.csv")

names(pokemon_averages) <- c("DexNum",
                            "PokemonName",
                            "PokeApiName",
                            "Complexity",
                            "Realism",
                            "Artificiality",
                            "Fantasy",
                            "Humanoid",
                            "Cuteness",
                            "Coolness",
                            "Beauty",
                            "Popularity",
                            "RatingCount")

theNA <- pokemon_rates %>% filter(is.na(Coolness)) #This single Furfrou ranking just has, like, no data, which is weird. Let's see if it has other votes
DebFurFrou <- pokemon_rates %>% filter(PokemonName == "Debutante Furfrou")


# generation_fun <- function(dexnum){
#   
#   if(dexnum <= 1010) {gen <- "9"}
#   if(dexnum <= 905) {gen <- "8"}
#   if(dexnum <= 809) {gen <- "7"}
#   if(dexnum <= 721) {gen <- "6"}
#   if(dexnum <= 649) {gen <- "5"}
#   if(dexnum <= 493) {gen <- "4"}
#   if(dexnum <= 386) {gen <- "3"}
#   if(dexnum <= 251) {gen <- "2"}
#   if(dexnum <= 151) {gen <- "1"}
#   
#      return(gen)
# }


pokemon_averages_w_gens <- pokemon_averages %>% mutate(Generation = case_when(DexNum <= 1010 & DexNum > 905 | str_detect(PokeApiName, "-paldea")~ "9",
                                                                             DexNum <= 905 & DexNum > 809 | str_detect(PokeApiName, "-galar") | str_detect(PokeApiName, "-gmax") | str_detect(PokeApiName, "-hisui") | str_detect(PokeApiName, "-white-striped") ~ "8",
                                                                             DexNum <= 809 & DexNum > 721 | str_detect(PokeApiName, "-alola") ~ "7",
                                                                             DexNum <= 721 & DexNum > 649 | str_detect(PokeApiName, "-mega") | str_detect(PokeApiName, "-primal")~ "6",
                                                                             DexNum <= 649 & DexNum > 493 ~ "5",
                                                                             DexNum <= 493 & DexNum > 386 ~ "4",
                                                                             DexNum <= 386 & DexNum > 251 ~ "3",
                                                                             DexNum <= 251 & DexNum > 151 ~ "2",
                                                                             DexNum <= 151 ~ "1"),
                                                    Region = case_when(DexNum <= 1010 & DexNum > 905 | str_detect(PokeApiName, "-paldea") ~ "Paldea",
                                                                       DexNum <= 905 & DexNum > 809 | str_detect(PokeApiName, "-galar")  ~ "Galar",
                                                                       DexNum <= 809 & DexNum > 721 | str_detect(PokeApiName, "-alola") ~ "Alola",
                                                                       str_detect(PokeApiName, "-hisui") | str_detect(PokeApiName, "-white-striped") ~ "Hisui",
                                                                       DexNum <= 721 & DexNum > 649 ~ "Kalos",
                                                                       DexNum <= 649 & DexNum > 493  ~ "Unova",
                                                                       DexNum <= 493 & DexNum > 386 ~ "Sinnoh",
                                                                       DexNum <= 386 & DexNum > 251 ~ "Hoenn",
                                                                       DexNum <= 251 & DexNum > 151 ~ "Johto",
                                                                       DexNum <= 151 ~ "Kanto"))

regionfactor <- factor(levels = c("Kanto", "Johto", "Hoenn", "Sinnoh", "Unova", "Kalos", "Alola", "Galar", "Hisui", "Paldea"))
genfactor <- factor(levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))

pokemon_averages_w_gens$Region <- regionfactor
genfactor <- 
  
  
write.csv(pokemon_averages_w_gens, "average-ratings_w_gens.csv")


pokemon_rates_w_gens <- pokemon_rates %>% mutate(Generation = case_when(DexNum <= 1010 & DexNum > 905 | str_detect(PokemonName, "Palean ")~ "9",
                                                                            DexNum <= 905 & DexNum > 809 | str_detect(PokemonName, "Galarian ") | str_detect(PokemonName, "Gigantamax ") | str_detect(PokemonName, "-hisui") | str_detect(PokemonName, "-white-striped") ~ "8",
                                                                            DexNum <= 809 & DexNum > 721 | str_detect(PokemonName, "Alolan ") ~ "7",
                                                                            DexNum <= 721 & DexNum > 649 | str_detect(PokemonName, "Mega ") | str_detect(PokemonName, "Primal ")~ "6",
                                                                            DexNum <= 649 & DexNum > 493 ~ "5",
                                                                            DexNum <= 493 & DexNum > 386 ~ "4",
                                                                            DexNum <= 386 & DexNum > 251 ~ "3",
                                                                            DexNum <= 251 & DexNum > 151 ~ "2",
                                                                            DexNum <= 151 ~ "1"),
                                                     Region = case_when(DexNum <= 1010 & DexNum > 905 | str_detect(PokemonName, "Palean ") ~ "Paldea",
                                                                        DexNum <= 905 & DexNum > 809 | str_detect(PokemonName, "Galarian ")  ~ "Galar",
                                                                        DexNum <= 809 & DexNum > 721 | str_detect(PokemonName, "Alolan ") ~ "Alola",
                                                                        str_detect(PokemonName, "Hisuian ") | str_detect(PokemonName, "White-Stripe") ~ "Hisui",
                                                                        DexNum <= 721 & DexNum > 649 ~ "Kalos",
                                                                        DexNum <= 649 & DexNum > 493  ~ "Unova",
                                                                        DexNum <= 493 & DexNum > 386 ~ "Sinnoh",
                                                                        DexNum <= 386 & DexNum > 251 ~ "Hoenn",
                                                                        DexNum <= 251 & DexNum > 151 ~ "Johto",
                                                                        DexNum <= 151 ~ "Kanto"))
write.csv(pokemon_rates_w_gens, "all-ratings_w_gens.csv") 

AverageCorrelation <- cor(pokemon_averages_w_gens %>% mutate_at("Generation", as.numeric) %>% subset(select = -c(DexNum, PokemonName, PokeApiName, RatingCount, Region))) 
corrplot(AverageCorrelation)

FullCorrelation <- cor(pokemon_rates_w_gens %>% na.omit() %>% mutate_at("Generation", as.numeric) %>% subset(select = -c(DexNum, Timestamp, PokemonName, UserID, Region))) 
corrplot(FullCorrelation)



average_by_gen <- pokemon_averages_w_gens %>% 
  group_by(Generation) %>%
  summarise(Complexity = mean(Complexity),
            Realism = mean(Realism),
            Artificiality = mean(Artificiality),
            Fantasy = mean(Fantasy),
            Humanoid = mean(Humanoid),
            Cuteness = mean(Cuteness),
            Coolness = mean(Coolness),
            Beauty  = mean(Beauty),
            Popularity = mean(Popularity))

average_by_region <- pokemon_averages_w_gens %>%
  group_by(Region) %>%
  summarise(Complexity = mean(Complexity),
            Realism = mean(Realism),
            Artificiality = mean(Artificiality),
            Fantasy = mean(Fantasy),
            Humanoid = mean(Humanoid),
            Cuteness = mean(Cuteness),
            Coolness = mean(Coolness),
            Beauty  = mean(Beauty),
            Popularity = mean(Popularity))



averages_long <- pokemon_averages_w_gens %>% pivot_longer(cols = c(Complexity,
                      Realism,
                      Artificiality,
                      Fantasy,
                      Humanoid,
                      Cuteness,
                      Coolness,
                      Beauty,
                      Popularity), names_to = "Category", values_to = "AverageRank") 

averages_long_gen <- averages_long %>%
  group_by(Generation,Category)%>%
  summarize(AverageRank = mean(AverageRank))

averages_long_region <- averages_long %>%
  group_by(Region,Category)%>%
  summarize(AverageRank = mean(AverageRank))



##### GRAPHS #####


ggplot(averages_long_region, aes(x = Category, y = AverageRank)) +
  geom_col() +
  facet_wrap(~ Region)


ggplot(pokemon_rates_w_gens, aes(x = Complexity)) +
  geom_histogram() +
  facet_wrap(~ Generation)


gggen <- ggplot(average_by_gen)
ggregion <- ggplot(average_by_region)


gggen +
  geom_col(aes(x = Generation, y = Popularity)) +
  geom_text(aes(x = Generation, y = Popularity, label = Popularity))

ggregion +
  geom_col(aes(x = Region, y = Popularity))
           
  
  
  

# The global average of coolness according to ratepkmn.com is 3.3
# Global popularity average is 3.54

# top_cool <- pokemon_averages %>% filter(coolness >= 3.3)
# top_pop <- pokemon_averages %>% filter(popularity >= 3.54)
# top_cool_pop <- inner_join(top_cool, top_pop)
# top_coolpop <- pokemon_averages %>% filter(coolness >= 3.3 & popularity >= 3.54)
# write.csv(top_cool, "AboveAverageCoolestPokemon.csv")
# write.csv(top_pop, "AboveAverageCoolAndPopularPokemon.csv")
# write.csv(top_cool_pop, "AboveAveragePopularPokemon.csv")




