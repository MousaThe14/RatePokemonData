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
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# R's base sd() function performs a sample mean
# This function performs a population mean
sd.p= function(x){
  sd(x) * sqrt((length(x)-1)/length(x))
} 

#List of Pokemon with their types but types are by type id
types <- read.csv(url("https://raw.githubusercontent.com/PokeAPI/pokeapi/master/data/v2/csv/pokemon_types.csv"))

#Types with their id and corresponding name
typenames <- read.csv(url("https://raw.githubusercontent.com/PokeAPI/pokeapi/master/data/v2/csv/types.csv")) %>%
  subset(select = -c(generation_id, damage_class_id))

# Pokemon with their id and identifier. id will help attach their types while identifier is used for merging with ratepkmn
pkmn_id <- read.csv(url("https://raw.githubusercontent.com/PokeAPI/pokeapi/master/data/v2/csv/pokemon.csv")) %>% 
  subset(select = -c(is_default, order))

# Merging pokemon and types with the name of their types
named_types <- types %>% full_join(typenames, by = c("type_id" = "id"))

# renaming "identifier" to "type"
names(named_types) <- c("pokemon_id", "type_id", "slot", "type")

# Merging types with their name with Pokemon by Pokemon
pkmn_types <- pkmn_id %>% inner_join(named_types, by = c("id" = "pokemon_id"))

# Separates the types into distinct columns and thus merges duplicates
pkmn_split <- pkmn_types %>% pivot_wider(names_from = slot, values_from = c(type, type_id))



#renaming for later use
names(pkmn_split) <- c("id", "PokeApiName", "species_id", "Height", "Weight", "base_experience", "Type1", "Type2", "TypeID1", "TypeID2") 

pkmn_split$Type2 <- replace_na(pkmn_split$Type2, 'No 2nd Type')

pkmn_metadata <- pkmn_split %>% subset(select = -c(base_experience, id, Height,Weight,species_id, TypeID1, TypeID2))

pkmn_metadata$Type2 <- replace_na(pkmn_split$Type2, 'No 2nd Type')
  
pokemon_rates <- read.csv("all-ratings.csv")  %>%
  na.omit()

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

pokemon_rates <- pokemon_rates %>% 
  mutate_at("UserID", as.character)%>%
  subset(select = -Timestamp)%>%
  group_by(PokemonName) 

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

ApiName <- pokemon_averages %>% subset(select = PokeApiName)
ApiNameType <- ApiName %>% left_join(pkmn_metadata, by = "PokeApiName")
ApiNameType %>% filter(is.na(Type1))

# theNA <- pokemon_rates %>% filter(is.na(Coolness)) #This single Furfrou ranking just has, like, no data, which is weird. Let's see if it has other votes
# DebFurFrou <- pokemon_rates %>% filter(PokemonName == "Debutante Furfrou")
# gourgeist <- pokemon_averages %>% filter(str_detect(PokeApiName, "gourgeist"))

standardDeviations <- pokemon_rates %>% 
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

sdOnly <- standardDeviations %>% subset(select = c(PokemonName, PopularitySD, MeanDesignSD))



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



regionfactor <- factor(levels = c("1", "2", "3", "4", "5", "6", "7", "8", "8", "9"), labels = c("Kanto", "Johto", "Hoenn", "Sinnoh", "Unova", "Kalos", "Alola", "Galar", "Hisui", "Paldea"), ordered = TRUE)
genfactor <- factor(levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))

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
                                                                       DexNum <= 151 ~ "Kanto")) %>%
  mutate(Region = factor(Region, levels = c("Kanto",
                                            "Johto",
                                            "Hoenn",
                                            "Sinnoh",
                                            "Unova",
                                            "Kalos",
                                            "Alola",
                                            "Galar",
                                            "Hisui",
                                            "Paldea"), ordered = TRUE)) %>%
  mutate(Order = case_when(Region == "Kanto" ~ 1,
                           Region == "Johto" ~ 2,
                           Region == "Hoenn" ~ 3,
                           Region == "Sinnoh" ~ 4,
                           Region == "Unova" ~ 5,
                           Region == "Kalos" ~ 6,
                           Region == "Alola" ~ 7,
                           Region == "Galar" ~ 8,
                           Region == "Hisui" ~ 9,
                           Region == "Paldea" ~ 10))


pokemon_averages_w_gens <- pokemon_averages_w_gens %>% full_join(sdOnly, by = "PokemonName")

pokemon_averages_w_gens_types <- pokemon_averages_w_gens %>% left_join(pkmn_metadata, by = "PokeApiName")

# Pokemon in the list that are missing types
missingtypes <- pokemon_averages_w_gens_types %>% filter(is.na(Type1))
# PokeApiName <- missingtypes$PokeApiName

Type1 <- c("fighting",
           "electric",
           "psychic", 
           "bug", 
           "bug",
           "bug",
           "bug",
           "grass",
           "water",
           "water",
           "water",
           "water",
           "normal",
           "normal",
           "normal",
           "normal",
           "normal",
           "normal",
           "normal",
           "normal",
           "normal",
           "bug",
           "bug",
           "bug",
           "fairy",
           "fairy",
           "fairy",
           "normal",
           "normal",
           "normal",
           "normal",
           "normal",
           "normal",
           "normal",
           "normal",
           "normal",
           "normal",
           "fairy",
           "dragon",
           "dragon",
           "normal",
           "ghost",
           "ghost",
           "fairy",
           "normal",
           "normal",
           "water",
           "dragon",
           "normal",
           "ghost")
Type2 <- c("dragon",
           "dragon", 
           "No 2nd Type",
           "No 2nd Type",
           "No 2nd Type",
           "No 2nd Type",
           "flying",
           "No 2nd Type",
           "No 2nd Type",
           "No 2nd Type",
           "ground",
           "ground",
           "No 2nd Type",
           "grass",
           "grass",
           "grass", 
           "grass",
           "grass", 
           "grass",
           "grass",
           "grass",
           "No 2nd Type",
           "No 2nd Type",
           "flying",
           "No 2nd Type",
           "No 2nd Type",
           "No 2nd Type",
           "No 2nd Type",
           "No 2nd Type",
           "No 2nd Type",
           "No 2nd Type",
           "No 2nd Type",
           "No 2nd Type",
           "No 2nd Type",
           "No 2nd Type",
           "No 2nd Type",
           "No 2nd Type",
           "No 2nd Type",
           "ground",
           "ground",
           "No 2nd Type",
           "No 2nd Type",
           "No 2nd Type",
           "No 2nd Type",
           "No 2nd Type",
           "flying",
           "No 2nd Type",
           "water",
           "No 2nd Type",
           "No 2nd Type")

missingtypes$Type1 <- Type1
missingtypes$Type2 <- Type2

# missingtypedf <- data.frame(PokeApiName, Type1, Type2)
withtypes <- pokemon_averages_w_gens_types %>% filter(!is.na(Type1))

# pokemon_averages_w_gens_types_final <- pokemon_averages_w_gens_types %>% full_join(missingtypedf, by = c("PokeApiName", "Type1", "Type2"))
# pokemon_averages_w_gens_types_final <- pokemon_averages_w_gens_types_final %>% filter(!is.na(Type1))

pokemon_averages_w_gens_types_final <- withtypes %>% rbind(missingtypes)


write.csv(pokemon_averages_w_gens_types_final, "average-ratings_w_gens.csv")


pokemon_rates_w_gens <- pokemon_rates %>% mutate(Generation = case_when(DexNum <= 1010 & DexNum > 905 | str_detect(PokemonName, "Palean ")~ "9",
                                                                            DexNum <= 905 & DexNum > 809 | str_detect(PokemonName, "Galarian ") | str_detect(PokemonName, "Gigantamax ") | str_detect(PokemonName, "-hisui") | str_detect(PokemonName, "-white-striped") ~ "8",
                                                                            DexNum <= 809 & DexNum > 721 | str_detect(PokemonName, "Alolan ") ~ "7",
                                                                            DexNum <= 721 & DexNum > 649 | str_detect(PokemonName, "Mega ") | str_detect(PokemonName, "Primal ") ~ "6",
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
                                                                        DexNum <= 151 ~ "Kanto")) %>%
  mutate(Region = factor(Region, levels = c("Kanto",
                                            "Johto",
                                            "Hoenn",
                                            "Sinnoh",
                                            "Unova",
                                            "Kalos",
                                            "Alola",
                                            "Galar",
                                            "Hisui",
                                            "Paldea"), ordered = TRUE)) %>%
  mutate(Order = case_when(Region == "Kanto" ~ 1, Region == "Johto" ~ 2, Region == "Hoenn" ~ 3, Region == "Sinnoh" ~ 4, Region == "Unova" ~ 5, Region == "Kalos" ~ 6, Region == "Alola" ~ 7, Region == "Galar" ~ 8, Region == "Hisui" ~ 9, Region == "Paldea" ~ 10))


write.csv(pokemon_rates_w_gens, "all-ratings_w_gens.csv") 


AverageCorrelation <- cor(pokemon_averages_w_gens %>% mutate_at("Generation", as.numeric) %>% subset(select = -c(DexNum, PokemonName, PokeApiName, RatingCount, Region, Order, Generation))) 
corrplot(AverageCorrelation, addCoef.col ='black', type = 'lower', order = "FPC")

FullCorrelation <- cor(pokemon_rates_w_gens %>% na.omit() %>% mutate_at("Generation", as.numeric) %>% subset(select = -c(DexNum, PokemonName, UserID, Region, Order, Generation))) 
corrplot(FullCorrelation, addCoef.col ='black', type = 'lower', order="FPC")

  
  
  

# The global average of coolness according to ratepkmn.com is 3.3
# Global popularity average is 3.54

# top_cool <- pokemon_averages %>% filter(coolness >= 3.3)
# top_pop <- pokemon_averages %>% filter(popularity >= 3.54)
# top_cool_pop <- inner_join(top_cool, top_pop)
# top_coolpop <- pokemon_averages %>% filter(coolness >= 3.3 & popularity >= 3.54)
# write.csv(top_cool, "AboveAverageCoolestPokemon.csv")
# write.csv(top_pop, "AboveAverageCoolAndPopularPokemon.csv")
# write.csv(top_cool_pop, "AboveAveragePopularPokemon.csv")




