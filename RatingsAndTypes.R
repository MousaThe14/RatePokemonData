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
### Print PNG ###
printPNG = function(filename, dataset, width = 1920, height = 1080, units = "px", dpi = 300){
  
  png(filename = filename,
      width = width,
      height = height,
      res = dpi)
  dataset
  dev.off()
}


### Print SVG ###
printSVG = function(filename, dataset, width = 1920, height = 1080, units = "px", dpi = 300){
  
  svg(filename = filename,
      width = width,
      height = height,
      res = dpi)
  dataset
  dev.off()
}

typeColorPalette <- c("Grass" = "#3FA129",
                      "Water" = "#2980EF",
                      "Fire" = "#E62829",
                      "Electric" = "#FAC000",
                      "Normal" = "#9FA19F",
                      "Fighting" = "#FF8000",
                      "Flying" = "#81B9EF",
                      "Rock" = "#AFA981",
                      "Ground" = "#915121",
                      "Steel" = "#60A1B8",
                      "Poison" = "#9141CB",
                      "Bug" = "#91A119",
                      "Psychic" = "#EF4179",
                      "Ghost" = "#704170",
                      "Dark" = "#624D4E",
                      "Ice" = "#3DCEF3",
                      "Dragon" = "#5060E1",
                      "Fairy" = "#EF70EF")

categoryColorPalette <- c("Cuteness" = "hotpink",
                          "Coolness" = "orange",
                          "Beauty" = "blue",
                          "Artificiality" = "green",
                          "Realism" = "wheat3",
                          "Humanoid" = "tan3",
                          "Fantasy" = "violet",
                          "Complexity" = "grey71",
                          "Popularity" = "gold1") 

ggtheme <- theme(axis.text.x = element_text(angle = 30, hjust = 1.2, vjust = 1.2, size = 18),
                axis.text.y = element_text(size = 18),
                title  = element_text(size = 22),
                plot.subtitle = element_text(size = 18),
                legend.text = element_text(size = 18),
                 strip.background = element_rect(colour = "black", fill = "white"),
                strip.text = element_text(size = 20))

averages_ratings <- read.csv("average-ratings_enriched.csv")
raw_ratings <- read.csv("all-ratings_enriched.csv")

ratings_w_types <- raw_ratings %>% full_join(averages_ratings %>% subset(select = c(PokemonName, Type1, Type2)), by = "PokemonName")

ratingsTypesLong <- raw_ratings %>%
  pivot_longer(cols = c(Type1, Type2), names_to = "TypeSlot", values_to = "Type") %>%
  subset(select = -TypeSlot)


typeMode <- ratingsTypesLong %>% 
  group_by(Type) %>% 
  filter(Type != "No 2nd Type") %>%
  summarise(Complexity = getmode(Complexity),
              Realism = getmode(Realism),
              Artificiality = getmode(Artificiality),
              Fantasy = getmode(Fantasy),
              Humanoid = getmode(Humanoid),
              Cuteness = getmode(Cuteness),
              Coolness = getmode(Coolness),
              Beauty  = getmode(Beauty),
              Popularity = getmode(Popularity))

typeModeLongCat <- typeMode %>%   
  pivot_longer(cols = c(Complexity,
                        Realism,
                        Artificiality,
                        Fantasy,
                        Humanoid,
                        Cuteness,
                        Coolness,
                        Beauty,
                        Popularity), names_to = "Category", values_to = "Mode")


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

# png(filename = "TypeCategoryViolin.png", width = 1000, height = 1400)
ggplot(types_cats_long2 %>% filter(Type != "No 2nd Type"), aes(x = Type, y = MeanRating, fill = Type)) +
  geom_violin() +
  labs(title = "Comparing Mean Design Elements to Type", subtitle = "Violin Graphs That Show The Density of Mean Ratings For Each Type in Each Design Category") +
  xlab("Type") +
  ylab("Mean Rating") +
  scale_fill_manual(values = typeColorPalette) +
  coord_flip() +
  facet_wrap(~Category) +
  ggtheme
# dev.off()


# png(filename = "TypeCategory.png", width = 1000, height = 1400)
ggplot(mean_category_types %>% filter(Type != "No 2nd Type"), aes(x = Type, y = MeanRating, fill = Type)) +
  geom_col() +
  coord_flip()+
  # geom_label(aes(x = Type, y = MeanRating, label = Type, fontface = "bold"),
  #  #          position = position_jitter(height = 0.75),
  #            color = "white") +
  labs(title = "Comparing Mean Design Elements to Type") +
  xlab("Type") +
  ylab("Mean Rating") +
  scale_fill_manual(values = typeColorPalette) +
  facet_wrap(~Category) +
  ggtheme
# dev.off()


png(filename = "ModeCategoryType.png", width = 1200, height = 1400)
ggplot(typeModeLongCat, aes(x = Category, y = Mode, fill = Type, group = Type)) +
  geom_col(color = "black") +
  geom_label(aes(x = Category, y = 0.7, label = Category), color = "black") +
  scale_fill_manual(values = typeColorPalette) +
  labs(title = "Mode Category for Each Type") +
  xlab("Category") +
  ylab("Mode Rating") +
  coord_flip() +
  facet_wrap(~Type) +
  ggtheme
dev.off()


theme(axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 11),
      title  = element_text(size = 18),
      plot.subtitle = element_text(size = 13),
      legend.text = element_text(size = 18),
      strip.background = element_rect(colour = "black", fill = "white"),
      strip.text = element_text(size = 15))




