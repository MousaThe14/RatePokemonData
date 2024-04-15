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

##### Useful Reminders #####

# As of this moment, there are 1251 Pokemon designs that are being ranked.
# So if, at any point, the number of rows is not 1251 when you've grouped by PokemonName
# Then either you intended for a Pokemon to show up multiple times in the table for what you're doing at the moment
# Or something has gone wrong and you need to figure out what.
# This dataset is valid until Generation 10 when the survey will be rebooted.


##### Useful functions #####

### Get the Mode ###
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# R's base sd() function performs a sample mean
# This function performs a population mean
sd.p= function(x){
  sd(x) * sqrt((length(x)-1)/length(x))
} 
#####

##### Type factor #####

### This is for later ###

typeFactor <- factor(levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19),
                     labels = c("Normal", "Fighting", "Flying", "Poison", "Ground", "Rock", "Bug", "Ghost", "Steel",
                                "Fire", "Water", "Grass", "Electric", "Psychic", "Ice", "Dragon", "Dark", "Fairy", "No 2nd Type"),
                     ordered = TRUE)

typeStringReplace <- c("normal" = "Normal",
                       "fighting" = "Fighting",
                       "flying" = "Flying",
                       "poison" = "Poison",
                       "ground" ="Ground",
                       "rock" ="Rock",
                       "bug" = "Bug",
                       "ghost" = "Ghost", 
                       "steel"="Steel",
                       "fire" = "Fire",
                       "water" ="Water",
                       "grass" = "Grass",
                       "electric" = "Electric",
                       "psychic" = "Psychic",
                       "ice" = "Ice",
                       "dragon" = "Dragon",
                       "dark" = "Dark",
                       "fairy" = "Fairy")
#####


### Importing Data ###

##### Importing and Cleaning Type Data #####

#List of Pokemon with their types but types are by type id
types <- read.csv(url("https://raw.githubusercontent.com/PokeAPI/pokeapi/master/data/v2/csv/pokemon_types.csv"))

#Types with their id and corresponding name
typenames <- read.csv(url("https://raw.githubusercontent.com/PokeAPI/pokeapi/master/data/v2/csv/types.csv")) %>%
  subset(select = -c(generation_id, damage_class_id))

# Pokemon with their id and identifier. id will help attach their types while identifier is used for merging with ratepkmn
pkmn_id <- read.csv(url("https://raw.githubusercontent.com/PokeAPI/pokeapi/master/data/v2/csv/pokemon.csv")) %>% 
  subset(select = -c(is_default, order, height, weight, base_experience))


# Merging pokemon and types with the name of their types
named_types <- types %>% full_join(typenames, by = c("type_id" = "id"))


# renaming "identifier" to "type"
names(named_types) <- c("pokemon_id", "type_id", "slot", "type")


# Merging types with their name with Pokemon by Pokemon
# Creates a table with the following columns
# id   identifier   species_id     type_id     slot     type
pkmn_types <- pkmn_id %>% inner_join(named_types, by = c("id" = "pokemon_id"))
# the resulting table has pokemon listed twice if they're dual types


# Separates the types into distinct columns and thus merges duplicates
# creates a table with the following columns:
#  id   identifier  species_id  type_1   type_2  type_id_1   type_id_2
pkmn_split <- pkmn_types %>% pivot_wider(names_from = slot, values_from = c(type, type_id))


# Renaming Columns to make later stuff easier
names(pkmn_split) <- c("id", "PokeApiName", "species_id", "Type1", "Type2", "TypeID1", "TypeID2") 


# Reducing pkmn_split to the bare minimum needed ot merge with the ratepkmn data tables
pkmn_metadata <- pkmn_split %>% subset(select = -c(id, species_id))


# replacing the NAs in the Type2 column with "No 2nd Type"
pkmn_metadata$Type2 <- replace_na(pkmn_split$Type2, 'No 2nd Type')
pkmn_metadata$TypeID2 <- replace_na(pkmn_split$TypeID2, 19)
  
#####



##### Importing and Renaming RatePkmn  Data #####

### All Ratings ###

# Importing the Rate Pokemon data The url version downloads it directly from the website.
pokemon_rates <- read.csv(url("https://ratepkmn.com/dlAll")) %>%
  na.omit() # Removes rows with missing values (That one Debutante Furfrou) 
 

# Importing the Rate Pokemon data from a the locally downloaded file
# pokemon_rates <- read.csv("all-ratings.csv")  %>%
#  na.omit()


# Renaming columns to make life easier later on.
names(pokemon_rates) <- c("DexNum",
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


pokemon_rates <- pokemon_rates %>% 
  mutate_at("UserID", as.character) %>% # Turns the userID column into strings
  #subset(select = -Timestamp)%>% # Removes the timestamp column
  group_by(PokemonName) #Groups ratings by Pokemon name



### Averages ###

# Importing the averages list
pokemon_averages <- read.csv(url("https://ratepkmn.com/dlAvg"))

# Importing the Rate Pokemon Averages from a the locally downloaded file
# pokemon_averages <- read.csv("average-ratings.csv")

# Renaming columns to make life easier later on.
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

#####

##### Troubleshooting1 #####

### Forms ###

# Something went wrong with merging types or merging names.

ApiName <- pokemon_averages %>% subset(select = PokeApiName)
ApiNameType <- ApiName %>% left_join(pkmn_metadata, by = "PokeApiName")

# You can see in the console here that Pokemon with alternate forms didn't merge correctly so they all come out with NAs
ApiNameType %>% filter(is.na(Type1))
# I fix this later and the method is clunky but it's weird that it happened in the first place

####


### The NA ### 

##### tracking down the na value that's ruining everything.
# theNA <- pokemon_rates %>% filter(is.na(Coolness)) #This single Furfrou ranking just has, like, no data, which is weird. Let's see if it has other votes
# DebFurFrou <- pokemon_rates %>% filter(PokemonName == "Debutante Furfrou")
#
# Thankfully this is the only one so I've used na.omit() when the dataset is imported to remove it.
#####


##### Miscellaneous Needed Calculations #####

### Standard Deviations ###

# Creating standard deviations from the raw ratings data for each individual Pokemon and their design categories

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
            # Popularity gets a special name because it's replicating a specific "Popularity Divisiveness" that I will re-add to the final dataset
            PopularitySD = sd.p(Popularity), 
            # This one's a tad tricky but it's basically supposed to replicated the "Divisiveness Score"
            # Which was a mean of the SD of the design categories
            MeanDesignSD = mean(c_across(c(Complexity,
                                           Realism,
                                           Artificiality,
                                           Fantasy,
                                           Humanoid,
                                           Cuteness,
                                           Coolness,
                                           Beauty))))

# I have taken the 2 Divisveness scores and put them into their own data table to be merged with the final one
sdOnly <- standardDeviations %>% subset(select = c(PokemonName, PopularitySD, MeanDesignSD))



##### The Grand Region/Generation Integration Section #####

# Creating factor vectors for the Region and Generation.
# They will be added to the full datasets to make other forms of analysis easier
regionfactor <- factor(levels = c("1", "2", "3", "4", "5", "6", "7", "8", "8", "9"), labels = c("Kanto", "Johto", "Hoenn", "Sinnoh", "Unova", "Kalos", "Alola", "Galar", "Hisui", "Paldea"), ordered = TRUE)
genfactor <- factor(levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))


##### Merging Averages with Gens/Regions

# So for this part we're doing a big mutate(case_when()) where if a pokemon has a DexNum within a certain range it's given a column with the appropriate region and generation
# For with a regional variants are categorized as part of that Region and their Generation category is for generation that form was introduced
# Generational gimmick forms (Gigantamax/Mega) are counted for the Generation they were introduced in
# However they are not counted towards the Region due to the uncertanty of where a form is "from".
pokemon_averages_w_gens <- pokemon_averages %>% mutate(Generation = case_when(DexNum <= 1017 & DexNum > 905 | str_detect(PokeApiName, "-paldea")~ "9",
                                                                             DexNum <= 905 & DexNum > 809 | str_detect(PokeApiName, "-galar") | str_detect(PokeApiName, "-gmax") | str_detect(PokeApiName, "-hisui") | str_detect(PokeApiName, "-white-striped") ~ "8",
                                                                             DexNum <= 809 & DexNum > 721 | str_detect(PokeApiName, "-alola") ~ "7",
                                                                             DexNum <= 721 & DexNum > 649 | str_detect(PokeApiName, "-mega") | str_detect(PokeApiName, "-primal")~ "6",
                                                                             DexNum <= 649 & DexNum > 493 ~ "5",
                                                                             DexNum <= 493 & DexNum > 386 ~ "4",
                                                                             DexNum <= 386 & DexNum > 251 ~ "3",
                                                                             DexNum <= 251 & DexNum > 151 ~ "2",
                                                                             DexNum <= 151 ~ "1"),
                                                    Region = case_when(DexNum <= 1017 & DexNum > 1010 ~ "Kitakami",
                                                                       DexNum <= 1010 & DexNum > 905 | str_detect(PokeApiName, "-paldea") ~ "Paldea",
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
                                            "Paldea",
                                            "Kitakami"), ordered = TRUE)) %>%
  # Order was introduced for chronological reasons.
  # Factoring the Regions wasn't working as intended so Order was created to compensate for that for graphing purposes
  # This will have to be done for the generations when Gen 10 arrives due the system's tendency to words as "1, 10, 2,etc"
  mutate(Order = case_when(Region == "Kanto" ~ 1, 
                           Region == "Johto" ~ 2,
                           Region == "Hoenn" ~ 3,
                           Region == "Sinnoh" ~ 4,
                           Region == "Unova" ~ 5,
                           Region == "Kalos" ~ 6,
                           Region == "Alola" ~ 7,
                           Region == "Galar" ~ 8,
                           Region == "Hisui" ~ 9,
                           Region == "Paldea" ~ 10,
                           Region == "Kitakami" ~ 11))


##### Merging Types and SDs

### Merging the Divisiveness stats ###
# The resulting columns are:
#  DexNum  PokemonName    PokeApiName   Complexity  Realism  Artificiality   Fantasy   Humanoid  Cuteness Coolness Beauty Popularity RatingCount Generation Region Order PopularitySD MeanDesignSD
averages_gens_sds <- pokemon_averages_w_gens %>% full_join(sdOnly, by = "PokemonName")

### Merging the Types ###
# The resulting columns are:
#  DexNum  PokemonName    PokeApiName   Complexity  Realism  Artificiality   Fantasy   Humanoid  Cuteness Coolness Beauty Popularity RatingCount Generation Region Order PopularitySD MeanDesignSD Type1 Type2
pokemon_averages_w_gens_types <- averages_gens_sds %>% left_join(pkmn_metadata, by = "PokeApiName")


### Aforementioned issue with alternate forms missing types ###

##### The Type Restoration Project


# Pokemon in the list that are missing types
missingtypes <- pokemon_averages_w_gens_types %>% filter(is.na(Type1))
# PokeApiName <- missingtypes$PokeApiName

##### Why yes, I did manually have to type these out in order  
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
##### Twice
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


# Giving back those pokemon with missing types their types back
missingtypes$Type1 <- Type1
missingtypes$Type2 <- Type2


# I basically created a new dataframe with the pokemon that still had their types 
withtypes <- pokemon_averages_w_gens_types %>% filter(!is.na(Type1))

# Then I slapped on the list of pokemon whose types I restored onto the end of it.
pokemon_averages_w_gens_types_final <- withtypes %>% rbind(missingtypes)

# doing the names again purely for aesthetic reasons
pokemon_averages_w_gens_types_final$Type1<- str_replace_all(pokemon_averages_w_gens_types_final$Type1, c("normal" = "Normal",
                                                                                                                "fighting" = "Fighting",
                                                                                                                "flying" = "Flying",
                                                                                                                "poison" = "Poison",
                                                                                                                "ground" ="Ground",
                                                                                                                "rock" ="Rock",
                                                                                                                "bug" = "Bug",
                                                                                                                "ghost" = "Ghost", 
                                                                                                                "steel"="Steel",
                                                                                                                "fire" = "Fire",
                                                                                                                "water" ="Water",
                                                                                                                "grass" = "Grass",
                                                                                                                "electric" = "Electric",
                                                                                                                "psychic" = "Psychic",
                                                                                                                "ice" = "Ice",
                                                                                                                "dragon" = "Dragon",
                                                                                                                "dark" = "Dark",
                                                                                                                "fairy" = "Fairy"))


pokemon_averages_w_gens_types_final$Type2<- str_replace_all(pokemon_averages_w_gens_types_final$Type2, c("normal" = "Normal",
                                                                                                         "fighting" = "Fighting",
                                                                                                         "flying" = "Flying",
                                                                                                         "poison" = "Poison",
                                                                                                         "ground" ="Ground",
                                                                                                         "rock" ="Rock",
                                                                                                         "bug" = "Bug",
                                                                                                         "ghost" = "Ghost", 
                                                                                                         "steel"="Steel",
                                                                                                         "fire" = "Fire",
                                                                                                         "water" ="Water",
                                                                                                         "grass" = "Grass",
                                                                                                         "electric" = "Electric",
                                                                                                         "psychic" = "Psychic",
                                                                                                         "ice" = "Ice",
                                                                                                         "dragon" = "Dragon",
                                                                                                         "dark" = "Dark",
                                                                                                         "fairy" = "Fairy"))

pokemon_averages_w_gens_types_final <- pokemon_averages_w_gens_types_final %>%
  mutate(Type1 =  factor(Type1, levels = c("Normal", "Fighting", "Flying", "Poison", "Ground", "Rock", "Bug", "Ghost", "Steel",
                                             "Fire", "Water", "Grass", "Electric", "Psychic", "Ice", "Dragon", "Dark", "Fairy", "No 2nd Type"),
                                        labels = c("Normal", "Fighting", "Flying", "Poison", "Ground", "Rock", "Bug", "Ghost", "Steel",
                                                   "Fire", "Water", "Grass", "Electric", "Psychic", "Ice", "Dragon", "Dark", "Fairy", "No 2nd Type"),
                                        ordered = TRUE)) %>%
  mutate(Type2 =  factor(Type2, levels = c("Normal", "Fighting", "Flying", "Poison", "Ground", "Rock", "Bug", "Ghost", "Steel",
                                             "Fire", "Water", "Grass", "Electric", "Psychic", "Ice", "Dragon", "Dark", "Fairy", "No 2nd Type"),
                         labels = c("Normal", "Fighting", "Flying", "Poison", "Ground", "Rock", "Bug", "Ghost", "Steel",
                                    "Fire", "Water", "Grass", "Electric", "Psychic", "Ice", "Dragon", "Dark", "Fairy", "No 2nd Type"),
                         ordered = TRUE))


# I also need to give the types their numbers for some correlation analysis

pokemon_averages_final<- pokemon_averages_w_gens_types_final %>% 
  mutate(TypeID1 = case_when(Type1 == "Normal" ~ 1,
                             Type1 == "Fighting" ~ 2,
                              Type1 == "Flying" ~ 3,
                              Type1 == "Poison" ~4,
                              Type1 =="Ground" ~ 5,
                              Type1 =="Rock" ~ 6,
                              Type1 == "Bug" ~ 7,
                              Type1 == "Ghost" ~ 8, 
                              Type1=="Steel" ~ 9,
                              Type1 == "Fire" ~ 10,
                              Type1 =="Water" ~ 11,
                              Type1 == "Grass" ~ 12,
                              Type1 == "Electric" ~ 13,
                              Type1 == "Psychic" ~ 14,
                              Type1 == "Ice" ~ 15,
                               Type1 == "Dragon" ~ 16,
                               Type1 == "Dark" ~ 17,
                               Type1 == "Fairy" ~ 18,
                               Type1 == "No 2nd Type" ~ 19),
         TypeID2 = case_when(Type2 == "Normal" ~ 1,
                             Type2 == "Fighting" ~ 2,
                             Type2 == "Flying" ~ 3,
                             Type2 == "Poison" ~4,
                             Type2 =="Ground" ~ 5,
                             Type2 =="Rock" ~ 6,
                             Type2 == "Bug" ~ 7,
                             Type2 == "Ghost" ~ 8, 
                             Type2=="Steel" ~ 9,
                             Type2 == "Fire" ~ 10,
                             Type2 =="Water" ~ 11,
                             Type2 == "Grass" ~ 12,
                             Type2 == "Electric" ~ 13,
                             Type2 == "Psychic" ~ 14,
                             Type2 == "Ice" ~ 15,
                             Type2 == "Dragon" ~ 16,
                             Type2 == "Dark" ~ 17,
                             Type2 == "Fairy" ~ 18,
                             Type2 == "No 2nd Type" ~ 19))

# And send to print for further use
write.csv(pokemon_averages_w_gens_types_final, "average-ratings_enriched.csv")



##### Merging Raw Ratings with Gens/Regions
# Due to my previous methods using the PokeApiName to do all of the merging I elected to not do it here, didn't seem necessary or worth it.
# I did the Generation and Region thing, 


pokemon_rates_w_gens <- pokemon_rates %>% mutate(Generation = case_when(DexNum <= 1017 & DexNum > 905 | str_detect(PokemonName, "Paldean ")~ "9",
                                                                            DexNum <= 905 & DexNum > 809 | str_detect(PokemonName, "Galarian ") | str_detect(PokemonName, "Gigantamax ") | str_detect(PokemonName, "-hisui") | str_detect(PokemonName, "-white-striped") ~ "8",
                                                                            DexNum <= 809 & DexNum > 721 | str_detect(PokemonName, "Alolan ") ~ "7",
                                                                            DexNum <= 721 & DexNum > 649 | str_detect(PokemonName, "Mega ") | str_detect(PokemonName, "Primal ") ~ "6",
                                                                            DexNum <= 649 & DexNum > 493 ~ "5",
                                                                            DexNum <= 493 & DexNum > 386 ~ "4",
                                                                            DexNum <= 386 & DexNum > 251 ~ "3",
                                                                            DexNum <= 251 & DexNum > 151 ~ "2",
                                                                            DexNum <= 151 ~ "1"),
                                                     Region = case_when(DexNum <= 1017 & DexNum > 1010 ~ "Kitakami",
                                                                        DexNum <= 1010 & DexNum > 905 | str_detect(PokemonName, "Paldean ") ~ "Paldea",
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
                                            "Paldea",
                                            "Kitakami"), ordered = TRUE)) %>%
  mutate(Order = case_when(Region == "Kanto" ~ 1, Region == "Johto" ~ 2, Region == "Hoenn" ~ 3, Region == "Sinnoh" ~ 4, Region == "Unova" ~ 5, Region == "Kalos" ~ 6, Region == "Alola" ~ 7, Region == "Galar" ~ 8, Region == "Hisui" ~ 9, Region == "Paldea" ~ 10, Region == "Kitakami" ~ 11))

# Suddenly realized I might be able to incorporate types after all
# Just gonna extract the names and types columns
names_and_types <- pokemon_averages_w_gens_types_final %>% subset(select = c("PokemonName", "Type1", "Type2"))

# Do a full join and hope that doesn't screw things up
rates_final <- pokemon_rates_w_gens %>% full_join(names_and_types, by = "PokemonName")

# Check to see if any errors exist due to the merge method
rates_final %>% na.omit()

# Okay looks like it didn't screw anything up. Going with this.

# doing the names again purely for aesthetic reasons
rates_final$Type1<- str_replace_all(rates_final$Type1, c("normal" = "Normal",
                                                                                                         "fighting" = "Fighting",
                                                                                                         "flying" = "Flying",
                                                                                                         "poison" = "Poison",
                                                                                                         "ground" ="Ground",
                                                                                                         "rock" ="Rock",
                                                                                                         "bug" = "Bug",
                                                                                                         "ghost" = "Ghost", 
                                                                                                         "steel"="Steel",
                                                                                                         "fire" = "Fire",
                                                                                                         "water" ="Water",
                                                                                                         "grass" = "Grass",
                                                                                                         "electric" = "Electric",
                                                                                                         "psychic" = "Psychic",
                                                                                                         "ice" = "Ice",
                                                                                                         "dragon" = "Dragon",
                                                                                                         "dark" = "Dark",
                                                                                                         "fairy" = "Fairy"))


rates_final$Type2<- str_replace_all(rates_final$Type2, c("normal" = "Normal",
                                                                                                         "fighting" = "Fighting",
                                                                                                         "flying" = "Flying",
                                                                                                         "poison" = "Poison",
                                                                                                         "ground" ="Ground",
                                                                                                         "rock" ="Rock",
                                                                                                         "bug" = "Bug",
                                                                                                         "ghost" = "Ghost", 
                                                                                                         "steel"="Steel",
                                                                                                         "fire" = "Fire",
                                                                                                         "water" ="Water",
                                                                                                         "grass" = "Grass",
                                                                                                         "electric" = "Electric",
                                                                                                         "psychic" = "Psychic",
                                                                                                         "ice" = "Ice",
                                                                                                         "dragon" = "Dragon",
                                                                                                         "dark" = "Dark",
                                                                                                         "fairy" = "Fairy"))



# I also need to give the types their numbers for some correlation analysis

rates_final<- rates_final %>% 
  mutate(Type1Id = case_when(Type1 == "Normal" ~ 1,
                             Type1 == "Fighting" ~ 2,
                             Type1 == "Flying" ~ 3,
                             Type1 == "Poison" ~4,
                             Type1 =="Ground" ~ 5,
                             Type1 =="Rock" ~ 6,
                             Type1 == "Bug" ~ 7,
                             Type1 == "Ghost" ~ 8, 
                             Type1=="Steel" ~ 9,
                             Type1 == "Fire" ~ 10,
                             Type1 =="Water" ~ 11,
                             Type1 == "Grass" ~ 12,
                             Type1 == "Electric" ~ 13,
                             Type1 == "Psychic" ~ 14,
                             Type1 == "Ice" ~ 15,
                             Type1 == "Dragon" ~ 16,
                             Type1 == "Dark" ~ 17,
                             Type1 == "Fairy" ~ 18,
                             Type1 == "No 2nd Type" ~ 19),
         Type2Id = case_when(Type2 == "Normal" ~ 1,
                             Type2 == "Fighting" ~ 2,
                             Type2 == "Flying" ~ 3,
                             Type2 == "Poison" ~4,
                             Type2 =="Ground" ~ 5,
                             Type2 =="Rock" ~ 6,
                             Type2 == "Bug" ~ 7,
                             Type2 == "Ghost" ~ 8, 
                             Type2=="Steel" ~ 9,
                             Type2 == "Fire" ~ 10,
                             Type2 =="Water" ~ 11,
                             Type2 == "Grass" ~ 12,
                             Type2 == "Electric" ~ 13,
                             Type2 == "Psychic" ~ 14,
                             Type2 == "Ice" ~ 15,
                             Type2 == "Dragon" ~ 16,
                             Type2 == "Dark" ~ 17,
                             Type2 == "Fairy" ~ 18,
                             Type2 == "No 2nd Type" ~ 19))

#converting dates from UNIX to readable timestmaps
rates_final <- rates_final %>% 
  mutate(Timestamp = as_datetime(Timestamp)) 



# Send to print
write.csv(rates_final, "all-ratings_enriched.csv") 
#####

##### Global Stats #####

globalAverages <- raw_ratings %>%
  summarise(Complexity = mean(Complexity),
          Realism = mean(Realism),
          Artificiality = mean(Artificiality),
          Fantasy = mean(Fantasy),
          Humanoid = mean(Humanoid),
          Cuteness = mean(Cuteness),
          Coolness =  mean(Coolness),
          Beauty = mean(Beauty),
          Popularity = mean(Popularity))

# round(globalAverages)
# 
# so if we round to whole numbers our averages are
# 
# ````
# > Complexity 3
# > Realism: 2
# > Artificiality 2
# > Fantasy 3
# > Humanoid 2
# > Cuteness 3
# > Coolness 3
# > Beauty 3
# ````
# And while our raw numbers aren't whole numbers, the original scale is'

# Remember those divisveness stats I calculated Earlier? Yeah
# Yeah, me neither, throwing them on to the globalAverages table
globalDivisiveness <- sdOnly %>% 
  summarise(PopularitySD = mean(PopularitySD),
            MeanDesignSD = mean(MeanDesignSD))
globalAverages %>% cbind(globalDivisiveness)

write.csv(globalAverages, "GlobalAverages.csv")

##### By Region and Generation 

### By Region ###
averageByRegion <- rates_final %>%
  group_by(Region) %>%
  summarise(Complexity = mean(Complexity),
            Realism = mean(Realism),
            Artificiality = mean(Artificiality),
            Fantasy = mean(Fantasy),
            Humanoid = mean(Humanoid),
            Cuteness = mean(Cuteness),
            Coolness =  mean(Coolness),
            Beauty = mean(Beauty),
            Popularity = mean(Popularity))
  

regionSD <- rates_final %>%
group_by(Region) %>%
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

regionDivisiveness <- regionSD %>% 
  subset(select = c(Region, PopularitySD, MeanDesignSD))

globalAverageRegion <- averageByRegion %>% full_join(regionDivisiveness, by = "Region") %>% arrange(desc(by = Popularity))
  

write.csv(globalAverageRegion, "GlobalAverage-Region.csv")

##### By Generation 
averageByGen <- rates_final %>%
  group_by(Generation) %>%
  summarise(Complexity = mean(Complexity),
            Realism = mean(Realism),
            Artificiality = mean(Artificiality),
            Fantasy = mean(Fantasy),
            Humanoid = mean(Humanoid),
            Cuteness = mean(Cuteness),
            Coolness =  mean(Coolness),
            Beauty = mean(Beauty),
            Popularity = mean(Popularity))
  
genSD <- rates_final %>%
  group_by(Generation) %>%
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

genDivisiveness <- genSD %>%
  subset(select = c(Generation, PopularitySD, MeanDesignSD))
  
globalAverageGen <- averageByGen %>% full_join(genDivisiveness, by = "Generation") %>% arrange(desc(by = Popularity))

write.csv(globalAverageGen, "GlobalAverage-Gen.csv")

##### By Type

# Okay so we're going to do a pivot, turn those 2 type columns into a single column
# That way a single dual-type pokemon will count towards both types
rates_final_long <- rates_final %>%
  pivot_longer(cols = c(Type1, Type2), names_to = "TypeSlot", values_to = "Type") %>%
  subset(select = -TypeSlot) %>%
  filter(Type != "No 2nd Type")

averageByType <- rates_final_long %>%
  group_by(Type) %>%
  summarise(Complexity = mean(Complexity),
            Realism = mean(Realism),
            Artificiality = mean(Artificiality),
            Fantasy = mean(Fantasy),
            Humanoid = mean(Humanoid),
            Cuteness = mean(Cuteness),
            Coolness =  mean(Coolness),
            Beauty = mean(Beauty),
            Popularity = mean(Popularity))


typeSD <- rates_final_long %>%
  group_by(Type) %>%
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

typeDivisiveness <- typeSD %>% 
  subset(select = c(Type, PopularitySD, MeanDesignSD))

globalAverageType <- averageByType %>% full_join(typeDivisiveness, by = "Type") %>% arrange(desc(by = Popularity))

write.csv(globalAverageType, "GlobalAverages-Type.csv")

######




######
# AverageCorrelation <- cor(pokemon_averages_w_gens %>% mutate_at("Generation", as.numeric) %>% subset(select = -c(DexNum, PokemonName, PokeApiName, RatingCount, Region, Order, Generation)))
# corrplot(AverageCorrelation, addCoef.col ='black', type = 'lower', order = "FPC")

# FullCorrelation <- cor(pokemon_rates_w_gens %>% na.omit() %>% mutate_at("Generation", as.numeric) %>% subset(select = -c(DexNum, PokemonName, UserID, Region, Order, Generation))) 
# corrplot(FullCorrelation, addCoef.col ='black', type = 'lower', order="FPC")



