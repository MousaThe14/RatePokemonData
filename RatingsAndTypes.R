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

averages_ratings <- read.csv("average-ratings_w_gens.csv")

types_cats_long <- averages_ratings %>%
  subset(select = -c(X, DexNum,
                     PokemonName,
                     PokeApiName,
                     RatingCount,
                     Generation,
                     Region,
                     Order,
                     PopularitySD,
                     MeanDesignSD,
                     id, Height, Weight)) %>%
  pivot_longer(cols = c(Complexity,
                        Realism,
                        Artificiality,
                        Fantasy,
                        Humanoid,
                        Cuteness,
                        Coolness,
                        Beauty,
                        Popularity), names_to = "Category", values_to = "MeanRating")

view(averages_ratings %>% !na.omit())

types_cats_long2 <- types_cats_long %>%
  pivot_longer(cols = c(Type1, Type2), names_to = "TypeSlot", values_to = "Type") %>%
  subset(select = -TypeSlot)

types_cats_long$Type1 <- as.factor(types_cats_long$Type1)
types_cats_long$Type2 <- as.factor(types_cats_long$Type2)
types_cats_long$Category <- as.factor(types_cats_long$Category)
types_cats_long2$Type <- as.factor(types_cats_long2$Type) 
types_cats_long2$Category <- as.factor(types_cats_long2$Category) 


mean_category_types <- types_cats_long2 %>%
  group_by(Type, Category) %>% 
  summarise(MeanRating = mean(MeanRating)) 

ggplot(types_cats_long2, aes(x = Type, y = MeanRating, fill = Type)) +
  geom_violin() +
  scale_fill_viridis_d() +
  facet_wrap(~Category) +
  ggtheme
  




