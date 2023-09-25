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

ggtheme <- theme(axis.text.x = element_text(angle = 30, hjust = 1.2, vjust = 1.2, size = 18),
                axis.text.y = element_text(size = 18),
                title  = element_text(size = 22),
                plot.subtitle = element_text(size = 18),
                legend.text = element_text(size = 18),
                 strip.background = element_rect(colour = "black", fill = "white"),
                strip.text = element_text(size = 20))

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
                     MeanDesignSD)) %>%
  pivot_longer(cols = c(Complexity,
                        Realism,
                        Artificiality,
                        Fantasy,
                        Humanoid,
                        Cuteness,
                        Coolness,
                        Beauty,
                        Popularity), names_to = "Category", values_to = "MeanRating")

# view(averages_ratings %>% filter(is.na(Type1)))

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

png(filename = "TypeCategoryViolin.png", width = 1000, height = 1400)
ggplot(types_cats_long2 %>% filter(Type != "No 2nd Type"), aes(x = Type, y = MeanRating, fill = Type)) +
  geom_violin() +
  labs(title = "Comparing Mean Design Elements to Type", subtitle = "Violin Graphs That Show The Density of Mean Ratings For Each Type in Each Design Category") +
  xlab("Type") +
  ylab("Mean Rating") +
  scale_fill_viridis_d() +
  coord_flip() +
  facet_wrap(~Category) +
  ggtheme
dev.off()


png(filename = "TypeCategory.png", width = 1000, height = 1400)
ggplot(mean_category_types %>% filter(Type != "No 2nd Type"), aes(x = Type, y = MeanRating, fill = Type)) +
  geom_col() +
  coord_flip()+
  # geom_label(aes(x = Type, y = MeanRating, label = Type, fontface = "bold"),
  #  #          position = position_jitter(height = 0.75),
  #            color = "white") +
  labs(title = "Comparing Mean Design Elements to Type") +
  xlab("Type") +
  ylab("Mean Rating") +
  scale_fill_viridis_d() +
  facet_wrap(~Category) +
  ggtheme
dev.off()


