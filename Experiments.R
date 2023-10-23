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

ratings <- read.csv("average-ratings_enriched.csv")
ratings <- read.csv("all-ratings_enriched.csv")
# Are more average pokemon more or less popular?


ratings_weights <- ratings %>%
  mutate(Weights = Popularity/5)

WeightedCoolnessMean <- weighted.mean(ratings_weights$Coolness ,ratings_weights$Weights)

averagesWeighted <- ratings_weights %>%
  group_by(PokemonName) %>%
   reframe(Complexity = weighted.mean(Complexity,Weights),
          Realism = weighted.mean(Realism,Weights),
          Artificiality = weighted.mean(Artificiality,Weights),
          Fantasy = weighted.mean(Fantasy,Weights),
          Humanoid = weighted.mean(Humanoid,Weights),
          Cuteness = weighted.mean(Cuteness,Weights),
          Coolness =  weighted.mean(Coolness,Weights),
          Beauty = weighted.mean(Beauty,Weights),
          Popularity = mean(Popularity))


averages <- ratings %>%
 group_by(PokemonName) %>%
  reframe(Complexity = mean(Complexity),
            Realism = mean(Realism),
            Artificiality = mean(Artificiality),
            Fantasy = mean(Fantasy),
            Humanoid = mean(Humanoid),
            Cuteness = mean(Cuteness),
            Coolness =  mean(Coolness),
            Beauty = mean(Beauty),
            Popularity = mean(Popularity))

avg_long <- ratings%>% 
  pivot_longer(cols = c(Complexity,
                        Realism,
                        Artificiality,
                        Fantasy,
                        Humanoid,
                        Cuteness,
                        Coolness,
                        Beauty), names_to = "Category", values_to = "Mean")


weighted_long <- averagesWeighted%>% 
  pivot_longer(cols = c(Complexity,
                        Realism,
                        Artificiality,
                        Fantasy,
                        Humanoid,
                        Cuteness,
                        Coolness,
                        Beauty), names_to = "Category", values_to = "Mean")




ggplot(avg_long, aes(x = Popularity, y = Mean, group = Category, fill = Category)) +
  geom_point() +
  #geom_smooth() +
  facet_wrap(~Category)


ggplot(weighted_long, aes(x = Popularity, y = Mean, group = Category, fill = Category)) +
  geom_point() +
  geom_smooth(~Category)





##### Normalization test #####

avg_test <- ratings %>%
  mutate(DesignMean = rowMeans(select(ratings,
                                      Complexity:Beauty))) %>%
   group_by(PokemonName) %>%
  summarise(Complexity = mean(Complexity),
            Realism = mean(Realism),
            Artificiality = mean(Artificiality),
            Fantasy = mean(Fantasy),
            Humanoid = mean(Humanoid),
            Cuteness = mean(Cuteness),
            Coolness =  mean(Coolness),
            Beauty = mean(Beauty),
            PopularitySD = sd.p(Popularity),
          Popularity = mean(Popularity),
          DesignSD = sd.p(DesignMean),
          DesignMean = mean(DesignMean)
          ) %>%
  mutate(DesignDistance = abs(DesignMean - mean(DesignMean)))

normalized <- avg_test %>%
  # mutate(Complexity = normalize(Complexity, method = "range", range = c(1,100))) %>%
  # mutate(Realism = normalize(Realism, method = "range", range = c(1,100))) %>%
  # mutate(Artificiality = normalize(Artificiality, method = "range", range = c(1,100))) %>%
  # mutate(Fantasy = normalize(Fantasy, method = "range", range = c(1,100))) %>%
  # mutate(Humanoid = normalize(Humanoid, method = "range", range = c(1,100))) %>%
  # mutate(Cuteness = normalize(Cuteness, method = "range", range = c(1,100))) %>%
  # mutate(Coolness = normalize(Coolness, method = "range", range = c(1,100))) %>%
  # mutate(Beauty = normalize(Beauty, method = "range", range = c(1,100))) %>%
  mutate(Popularity = normalize(Popularity, method = "range", range = c(1,100))) %>%
  mutate(DesignMean = normalize(DesignMean, method = "range", range = c(1,100))) %>%
#  mutate(DesignDistance = normalize(DesignDistance, method = "range", range = c(1,100)))

normalized_long <- normalized %>% 
  pivot_longer(cols = c(Complexity,
                        Realism,
                        Artificiality,
                        Fantasy,
                        Humanoid,
                        Cuteness,
                        Coolness,
                        Beauty), names_to = "Category", values_to = "Mean")
  


scaleplot <- ggplot(normalized,
                    aes(x = Popularity,
                        y = DesignDistance,
  #                      size = PopularitySD,
#                        group = Category,
#                        color = Category,
                        )) +
  #scale_y_log10()+
  geom_point(alpha = 0.5) +
  geom_point(data = normalized %>% slice_min(DesignDistance),
             aes(x = Popularity,
                 y = DesignDistance,label = PokemonName),
             color = "blue") +
  stat_poly_eq()

  
  # geom_smooth(se = FALSE)

scaleplot <- ggplot(normalized_long,
                    aes(x = Popularity,
                        y = Mean,
                        #                      size = PopularitySD,
                                                group = Category,
                                                color = Category)) +
  #scale_y_log10()+
  geom_smooth(alpha = 0.5, se = FALSE, position = "jitter") +
  geom_point(data = normalized_long %>% group_by(Category) %>% slice_min(Mean),
             aes(x = Popularity,
                 y = Mean,
                 label = PokemonName,
             color = Category)) +
  geom_point(data = normalized_long %>% group_by(Category) %>% slice_max(Mean),
             aes(x = Popularity,
                 y = Mean,
                 label = PokemonName,color = Category)
             ) +
  stat_poly_eq()




scaleplot <- ggplot(normalized,
                    aes(x = Popularity,
                        y = DesignDistance,
                        #                      size = PopularitySD,
                        #                        group = Category,
                        #                        color = Category,
                    )) +
  #scale_y_log10()+
  geom_violin(alpha = 0.5) +
  geom_point(data = normalized %>% slice_min(DesignDistance),
             aes(x = Popularity,
                 y = DesignDistance,label = PokemonName),
             color = "blue")
ggplotly(scaleplot)

