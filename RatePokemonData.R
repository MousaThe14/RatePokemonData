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
sd.p= function(x){
  sd(x) * sqrt((length(x)-1)/length(x))
} 

types <- read.csv(url("https://raw.githubusercontent.com/PokeAPI/pokeapi/master/data/v2/csv/pokemon_types.csv"))
pkmn_id <- read.csv(url("https://raw.githubusercontent.com/PokeAPI/pokeapi/master/data/v2/csv/pokemon.csv"))

pkmn_types <- pkmn_id %>% inner_join(types, by = c("id" = "pokemon_id"))

pkmn_split <- types %>% pivot_wider(names_from = slot, values_from = type_id)

names(pkmn_split) <- c("pokemon_id", "Type1", "Type2") 
  
  
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

# theNA <- pokemon_rates %>% filter(is.na(Coolness)) #This single Furfrou ranking just has, like, no data, which is weird. Let's see if it has other votes
# DebFurFrou <- pokemon_rates %>% filter(PokemonName == "Debutante Furfrou")
gourgeist <- pokemon_averages %>% filter(str_detect(PokeApiName, "gourgeist"))


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
  
write.csv(pokemon_averages_w_gens, "average-ratings_w_gens.csv")


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

average_by_gen_long <- average_by_gen  %>%
  pivot_longer(cols = c(Complexity,
                        Realism,
                        Artificiality,
                        Fantasy,
                        Humanoid,
                        Cuteness,
                        Coolness,
                        Beauty,
                        Popularity), names_to = "Category", values_to = "MeanRating")

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
            Popularity = mean(Popularity)) %>%
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


average_by_region_long <-  average_by_region %>%
  pivot_longer(cols = c(Complexity,
                        Realism,
                        Artificiality,
                        Fantasy,
                        Humanoid,
                        Cuteness,
                        Coolness,
                        Beauty,
                        Popularity), names_to = "Category", values_to = "MeanRating")


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
  summarize(AverageRank = mean(AverageRank))  %>%
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






pokemonmodes <- pokemon_rates_w_gens %>% group_by(PokemonName, Region, Generation) %>% reframe(Complexity = getmode(Complexity),
                                                                             Realism = getmode(Realism),
                                                                             Artificiality = getmode(Artificiality),
                                                                             Fantasy = getmode(Fantasy),
                                                                             Humanoid = getmode(Humanoid),
                                                                             Cuteness = getmode(Cuteness),
                                                                             Coolness = getmode(Coolness),
                                                                             Beauty  = getmode(Beauty),
                                                                             Popularity = getmode(Popularity))

mode_by_region <- pokemonmodes %>% group_by(Region) %>% summarise(Complexity = getmode(Complexity),
                                                              Realism = getmode(Realism),
                                                              Artificiality = getmode(Artificiality),
                                                              Fantasy = getmode(Fantasy),
                                                              Humanoid = getmode(Humanoid),
                                                              Cuteness = getmode(Cuteness),
                                                              Coolness = getmode(Coolness),
                                                              Beauty  = getmode(Beauty),
                                                              Popularity = getmode(Popularity))
mode_by_gen <- pokemonmodes %>% group_by(Generation) %>% summarise(Complexity = getmode(Complexity),
                                                               Realism = getmode(Realism),
                                                               Artificiality = getmode(Artificiality),
                                                               Fantasy = getmode(Fantasy),
                                                               Humanoid = getmode(Humanoid),
                                                               Cuteness = getmode(Cuteness),
                                                               Coolness = getmode(Coolness),
                                                               Beauty  = getmode(Beauty),
                                                               Popularity = getmode(Popularity))

mode_gen_long <-  mode_by_gen %>% pivot_longer(cols = c(Complexity,
                                                                    Realism,
                                                                    Artificiality,
                                                                    Fantasy,
                                                                    Humanoid,
                                                                    Cuteness,
                                                                    Coolness,
                                                                    Beauty,
                                                                    Popularity), names_to = "Category", values_to = "ModeRank") 

mode_region_long <-  mode_by_region %>% pivot_longer(cols = c(Complexity,
                                                                       Realism,
                                                                       Artificiality,
                                                                       Fantasy,
                                                                       Humanoid,
                                                                       Cuteness,
                                                                       Coolness,
                                                                       Beauty,
                                                                       Popularity), names_to = "Category", values_to = "ModeRank") 
  
  

mean_mode_gen <- mode_gen_long %>% full_join(average_by_gen_long)
mean_mode_region <- mode_region_long %>% full_join(average_by_region_long)
# ##### Mode using using full data was no different from the shortened version. This is Deprecated #####
# overall_modes_region <- pokemon_rates_w_gens %>% group_by(Region) %>% summarise(Complexity = getmode(Complexity),
#                                                                                               Realism = getmode(Realism),
#                                                                                               Artificiality = getmode(Artificiality),
#                                                                                               Fantasy = getmode(Fantasy),
#                                                                                               Humanoid = getmode(Humanoid),
#                                                                                               Cuteness = getmode(Cuteness),
#                                                                                               Coolness = getmode(Coolness),
#                                                                                               Beauty  = getmode(Beauty),
#                                                                                               Popularity = getmode(Popularity)) %>%
# pivot_longer(cols = c(Complexity,
#                             Realism,
#                             Artificiality,
#                             Fantasy,
#                             Humanoid,
#                             Cuteness,
#                             Coolness,
#                             Beauty,
#                             Popularity), names_to = "Category", values_to = "ModeRank")
# 
# overall_modes_generation <- pokemon_rates_w_gens %>% group_by(Generation) %>% summarise(Complexity = getmode(Complexity),
#                                                                          Realism = getmode(Realism),
#                                                                          Artificiality = getmode(Artificiality),
#                                                                          Fantasy = getmode(Fantasy),
#                                                                          Humanoid = getmode(Humanoid),
#                                                                          Cuteness = getmode(Cuteness),
#                                                                          Coolness = getmode(Coolness),
#                                                                          Beauty  = getmode(Beauty),
#                                                                          Popularity = getmode(Popularity)) %>%
#   pivot_longer(cols = c(Complexity,
#                         Realism,
#                         Artificiality,
#                         Fantasy,
#                         Humanoid,
#                         Cuteness,
#                         Coolness,
#                         Beauty,
#                         Popularity), names_to = "Category", values_to = "ModeRank")
#####



##### GRAPHS #####

### Mean and Mode ###

### Mode Only
mode_region_long <- mode_region_long %>%
  mutate(Factor = factor(Region, levels = c("Kanto", "Johto", "Hoenn", "Sinnoh", "Unova", "Kalos", "Alola", "Galar", "Hisui", "Paldea"), ordered = TRUE))
mode_gen_plot <- ggplot(mode_gen_long)
mode_region_plot <- ggplot(mode_region_long)

ggmode_gen <-mode_gen_plot +
  geom_line(aes(x = Generation, y = ModeRank, group = Category, color = Category)) +
  geom_point(aes(x = Generation, y = ModeRank, group = Category, color = Category)) +
  labs(title = "Mode of Pokemon Ratings by Generation", x = "Pokemon Generation", y = "Mode Rating") +
  facet_wrap(~Category)+
  theme(axis.text.x = element_text(angle = 30, hjust = 1.2, vjust = 1.5, size = 12),
        axis.text.y = element_text(size = 11))
ggmode_region <- mode_region_plot +
  geom_line(aes(x = reorder(Region, Order), y = ModeRank, group = Category, color = Category)) +
  geom_point(aes(x = Region, y = ModeRank, group = Category, color = Category)) +
  labs(title = "Mode of Pokemon Ratings by Region", x = "Region in Generation Order", y = "Mode Rating") +
  facet_wrap(~Category) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1.2, vjust = 1.5, size = 12),
        axis.text.y = element_text(size = 11))


png(filename = "ModeRatings.png", width = 1080, height = 500)
ggarrange(ggmode_gen, ggmode_region, common.legend = TRUE)
dev.off()

####

### Mean and Mode ###
mean_mode_gen_plot <- ggplot(mean_mode_gen %>% mutate(MeanRating = round(MeanRating, 2)))
mean_mode_region_plot <- ggplot(mean_mode_region %>% mutate(MeanRating = round(MeanRating, 2)))

ggmeanmode_gen <-mean_mode_gen_plot +
  geom_line(aes(x = Generation, y = ModeRank, group = Category, color = Category), linewidth = 1.25) +
  geom_point(aes(x = Generation, y = ModeRank, group = Category, color = Category)) +
  geom_line(aes(x = Generation, y = MeanRating, group = Category, color = Category), color = "black") +
  geom_text(aes(x = Generation, y = MeanRating, group = Category, label = MeanRating, size = "Mean"), fontface = "bold", size = 4, color = "black", nudge_y = 0.30) +
  labs(title = "Mode of Pokemon Ratings by Generation", x = "Pokemon Generation", y = "Mode Rating", fontface = "bold") +
  facet_wrap(~Category)+
  theme( text = element_text(size = 15),
    axis.text.x = element_text( size = 12),
        axis.text.y = element_text(size = 11))
ggmeanmode_region <- mean_mode_region_plot +
  geom_line(aes(x = reorder(Region, Order), y = ModeRank, group = Category, color = Category), linewidth = 1.25) +
  geom_point(aes(x = reorder(Region, Order), y = ModeRank, group = Category, color = Category)) +
  geom_line(aes(x = reorder(Region, Order), y = MeanRating, group = Category), color = "black") +
  geom_text(aes(x = reorder(Region, Order), y = MeanRating, group = Category, label = MeanRating, size = "Mean"),fontface = "bold", size = 4, color = "black", nudge_y = 0.30) +
  labs(title = "Mode of Pokemon Ratings by Region", x = "Region in Generation Order", y = "Mode Rating", fontface = "bold") +
  facet_wrap(~Category) +
  theme(text = element_text(size = 15),
        axis.text.x = element_text(angle = 30, hjust = 1.2, vjust = 1.5, size = 12),
        axis.text.y = element_text(size = 11))


png(filename = "ModeRatings.png", width = 1200, height = 600)
ggarrange(ggmeanmode_gen, ggmeanmode_region, common.legend = TRUE)
dev.off()



png(filename = "MeanModeGen.png", width = 1400, height = 550)
ggmeanmode_gen
dev.off()

png(filename = "MeanModeRegion.png", width = 800, height = 600)
ggmeanmode_region
dev.off()

####


ggplot(averages_long_region, aes(x = Category, y = AverageRank)) +
  geom_col() +
  facet_wrap(~ Region)





gen_rate_long <- pokemon_rates_w_gens %>% 
  pivot_longer(cols = c(Complexity,
                        Realism,
                        Artificiality,
                        Fantasy,
                        Humanoid,
                        Cuteness,
                        Coolness,
                        Beauty,
                        Popularity), names_to = "Category", values_to = "Rank") %>%
  na.omit() %>%
  mutate_at("Rank", as.factor)

ggplot(gen_rate_long, aes(x = Rank)) +
  geom_histogram(stat= "count") +
  facet_wrap(~ Category)

ggplot(gen_rate_long, aes(x = Rank, alpha = 0.5, group = Category, fill = Category)) +
  geom_density(stat= "count") +
  facet_grid(Category ~ Region)
ggplot(gen_rate_long, aes(x = Rank, alpha = 0.5, group = Category, fill = Category)) +
  geom_density(stat= "count") +
  facet_grid(Region ~ Category)


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




