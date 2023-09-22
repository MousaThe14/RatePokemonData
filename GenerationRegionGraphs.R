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

ggtheme <- theme(axis.text.x = element_text(angle = 30, hjust = 1.2, vjust = 1.5, size = 12),
                 axis.text.y = element_text(size = 11))

average_ratings <- read.csv("average-ratings_w_gens.csv")
raw_ratings <- read.csv("all-ratings_w_gens.csv")

average_ratings$Generation <- as.factor(average_ratings$Generation)
raw_ratings$Generation <- as.factor(raw_ratings$Generation)



average_by_gen <- average_ratings %>% 
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

average_by_region <- average_ratings %>%
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






pokemonmodes <- raw_ratings %>% group_by(PokemonName, Region, Generation) %>% reframe(Complexity = getmode(Complexity),
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



gggen <- ggplot(average_by_gen, aes(x = Generation, y = Popularity, fill = Generation)) +
  geom_col() +
  geom_text(aes(x = Generation, y = Popularity, label = round(Popularity, 2))) +
  scale_fill_viridis_d() +
  labs(title = "Average Popularity By Generation", x = "Pokemon Generation", y = "Mean", fontface = "bold") +
  theme(text = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 13))

ggregion <- ggplot(average_by_region, aes(x = Region, y = Popularity, fill = Region)) +
  geom_col() +
  geom_text(aes(x = Region, y = Popularity, label = round(Popularity, 2))) +
  scale_fill_viridis_d() +
  labs(title = "Average Popularity By Region", x = "Pokemon Region", y = "Mean", fontface = "bold") +
  theme(text = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 13))


ggarrange(gggen, ggregion)
