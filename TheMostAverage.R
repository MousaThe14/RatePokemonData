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
library(gridExtra)
# R's base sd() function performs a sample mean
# This function performs a population mean
sd.p= function(x){
  sd(x) * sqrt((length(x)-1)/length(x))
} 
averages_ratings <- read.csv("average-ratings_enriched.csv")


# Averages 
averages <- averages_ratings %>% summarise(Complexity = mean(Complexity),
                                       Realism = mean(Realism),
                                       Artificiality = mean(Artificiality),
                                       Fantasy = mean(Fantasy),
                                       Humanoid = mean(Humanoid),
                                       Cuteness = mean(Cuteness),
                                       Coolness =  mean(Coolness),
                                       Beauty = mean(Beauty))


# Standard Deviations
sds <- averages_ratings %>% summarise(Complexity = sd.p(Complexity),
                                           Realism = sd.p(Realism),
                                           Artificiality = sd.p(Artificiality),
                                           Fantasy = sd.p(Fantasy),
                                           Humanoid = sd.p(Humanoid),
                                           Cuteness = sd.p(Cuteness),
                                           Coolness =  sd.p(Coolness),
                                           Beauty = sd.p(Beauty))

meanPop <-  mean(averages_ratings$Popularity) #
popSD <- sd.p(averages_ratings$Popularity)



##### First pass, Within SD #####                           
undermean <- averages - sds
overmean <- averages + sds
                             
withinSD <- averages_ratings %>% filter(Complexity >= undermean$Complexity &
                                            Complexity <= overmean$Complexity &
                                             Realism >= undermean$Realism &
                                             Realism <= overmean$Realism &
                                             Artificiality >= undermean$Artificiality &
                                             Artificiality <= overmean$Artificiality & 
                                             Fantasy >= undermean$Fantasy &
                                             Fantasy <= overmean$Fantasy  & 
                                             Humanoid >= undermean$Humanoid &
                                             Humanoid <= overmean$Humanoid  &
                                             Cuteness >= undermean$Cuteness &
                                             Cuteness <= overmean$Cuteness  &
                                             Coolness >= undermean$Coolness &
                                             Coolness <= overmean$Coolness &
                                             Beauty <= overmean$Beauty  &
                                             Beauty >= undermean$Beauty)
withinSD$Generation <- as.character(withinSD$Generation)
mostaverage_gen <- withinSD %>% count(Generation) %>% arrange(n)
mostaverage_region <- withinSD %>% count(Region) %>% arrange(n)


##### Second pass reducing the sd by 25% #####

undermean2 <- averages - (sds*0.75)
overmean2 <- averages + (sds*0.75)

withinSD2 <- averages_ratings %>% filter(Complexity >= undermean2$Complexity &
                                          Complexity <= overmean2$Complexity &
                                          Realism >= undermean2$Realism &
                                          Realism <= overmean2$Realism &
                                          Artificiality >= undermean2$Artificiality &
                                          Artificiality <= overmean2$Artificiality & 
                                          Fantasy >= undermean2$Fantasy &
                                          Fantasy <= overmean2$Fantasy  & 
                                          Humanoid >= undermean2$Humanoid &
                                          Humanoid <= overmean2$Humanoid  &
                                          Cuteness >= undermean2$Cuteness &
                                          Cuteness <= overmean2$Cuteness  &
                                          Coolness >= undermean2$Coolness &
                                          Coolness <= overmean2$Coolness &
                                          Beauty <= overmean2$Beauty  &
                                          Beauty >= undermean2$Beauty)
withinSD2$Generation <- as.character(withinSD2$Generation)
mostaverage_gen2 <- withinSD2 %>% count(Generation) %>% arrange(n)
mostaverage_region2 <- withinSD2 %>% count(Region) %>% arrange(n)



##### Third pass, reduce sd by 30% #####

undermean3 <- averages - (sds*0.70)
overmean3 <- averages + (sds*0.70)

withinSD3 <- averages_ratings %>% filter(Complexity >= undermean3$Complexity &
                                           Complexity <= overmean3$Complexity &
                                           Realism >= undermean3$Realism &
                                           Realism <= overmean3$Realism &
                                           Artificiality >= undermean3$Artificiality &
                                           Artificiality <= overmean3$Artificiality & 
                                           Fantasy >= undermean3$Fantasy &
                                           Fantasy <= overmean3$Fantasy  & 
                                           Humanoid >= undermean3$Humanoid &
                                           Humanoid <= overmean3$Humanoid  &
                                           Cuteness >= undermean3$Cuteness &
                                           Cuteness <= overmean3$Cuteness  &
                                           Coolness >= undermean3$Coolness &
                                           Coolness <= overmean3$Coolness &
                                           Beauty <= overmean3$Beauty  &
                                           Beauty >= undermean3$Beauty)
withinSD3$Generation <- as.character(withinSD3$Generation)
mostaverage_gen3 <- withinSD3 %>% count(Generation) %>% arrange(n)
mostaverage_region3 <- withinSD3 %>% count(Region) %>% arrange(n)




##### Fourth pass, reduce sd by 35% #####

undermean4 <- averages - (sds*0.65)
overmean4 <- averages + (sds*0.65)

withinSD4 <- averages_ratings %>% filter(Complexity >= undermean4$Complexity &
                                           Complexity <= overmean4$Complexity &
                                           Realism >= undermean4$Realism &
                                           Realism <= overmean4$Realism &
                                           Artificiality >= undermean4$Artificiality &
                                           Artificiality <= overmean4$Artificiality & 
                                           Fantasy >= undermean4$Fantasy &
                                           Fantasy <= overmean4$Fantasy  & 
                                           Humanoid >= undermean4$Humanoid &
                                           Humanoid <= overmean4$Humanoid  &
                                           Cuteness >= undermean4$Cuteness &
                                           Cuteness <= overmean4$Cuteness  &
                                           Coolness >= undermean4$Coolness &
                                           Coolness <= overmean4$Coolness &
                                           Beauty <= overmean4$Beauty  &
                                           Beauty >= undermean4$Beauty)
withinSD4$Generation <- as.character(withinSD4$Generation)
mostaverage_gen4 <- withinSD4 %>% count(Generation) %>% arrange(n)
mostaverage_region4 <- withinSD4 %>% count(Region) %>% arrange(n)




##### Fifth pass, reduce sd by 40% #####

undermean5 <- averages - (sds*0.60)
overmean5 <- averages + (sds*0.60)

withinSD5 <- averages_ratings %>% filter(Complexity >= undermean5$Complexity &
                                           Complexity <= overmean5$Complexity &
                                           Realism >= undermean5$Realism &
                                           Realism <= overmean5$Realism &
                                           Artificiality >= undermean5$Artificiality &
                                           Artificiality <= overmean5$Artificiality & 
                                           Fantasy >= undermean5$Fantasy &
                                           Fantasy <= overmean5$Fantasy  & 
                                           Humanoid >= undermean5$Humanoid &
                                           Humanoid <= overmean5$Humanoid  &
                                           Cuteness >= undermean5$Cuteness &
                                           Cuteness <= overmean5$Cuteness  &
                                           Coolness >= undermean5$Coolness &
                                           Coolness <= overmean5$Coolness &
                                           Beauty <= overmean5$Beauty  &
                                           Beauty >= undermean5$Beauty)
withinSD5$Generation <- as.character(withinSD5$Generation)
mostaverage_gen5 <- withinSD5 %>% count(Generation) %>% arrange(n)
mostaverage_region5 <- withinSD5 %>% count(Region) %>% arrange(n)




##### Sixth pass, reduce sd by 45% #####

undermean6 <- averages - (sds*0.55)
overmean6 <- averages + (sds*0.55)

withinSD6 <- averages_ratings %>% filter(Complexity >= undermean6$Complexity &
                                           Complexity <= overmean6$Complexity &
                                           Realism >= undermean6$Realism &
                                           Realism <= overmean6$Realism &
                                           Artificiality >= undermean6$Artificiality &
                                           Artificiality <= overmean6$Artificiality & 
                                           Fantasy >= undermean6$Fantasy &
                                           Fantasy <= overmean6$Fantasy  & 
                                           Humanoid >= undermean6$Humanoid &
                                           Humanoid <= overmean6$Humanoid  &
                                           Cuteness >= undermean6$Cuteness &
                                           Cuteness <= overmean6$Cuteness  &
                                           Coolness >= undermean6$Coolness &
                                           Coolness <= overmean6$Coolness &
                                           Beauty <= overmean6$Beauty  &
                                           Beauty >= undermean6$Beauty)
withinSD6$Generation <- as.character(withinSD6$Generation)
mostaverage_gen6 <- withinSD6 %>% count(Generation) %>% arrange(n)
mostaverage_region6 <- withinSD6 %>% count(Region) %>% arrange(n)





##### Seventh pass, reduce sd by 48.999999999999% #####

undermean7 <- averages - (sds*0.51)
overmean7 <- averages + (sds*0.51)

withinSD7 <- averages_ratings %>% filter(Complexity >= undermean7$Complexity &
                                           Complexity <= overmean7$Complexity &
                                           Realism >= undermean7$Realism &
                                           Realism <= overmean7$Realism &
                                           Artificiality >= undermean7$Artificiality &
                                           Artificiality <= overmean7$Artificiality & 
                                           Fantasy >= undermean7$Fantasy &
                                           Fantasy <= overmean7$Fantasy  & 
                                           Humanoid >= undermean7$Humanoid &
                                           Humanoid <= overmean7$Humanoid  &
                                           Cuteness >= undermean7$Cuteness &
                                           Cuteness <= overmean7$Cuteness  &
                                           Coolness >= undermean7$Coolness &
                                           Coolness <= overmean7$Coolness &
                                           Beauty <= overmean7$Beauty  &
                                           Beauty >= undermean7$Beauty)
withinSD7$Generation <- as.character(withinSD7$Generation)
mostaverage_gen7 <- withinSD7 %>% count(Generation) %>% arrange(n)
mostaverage_region7 <- withinSD7 %>% count(Region) %>% arrange(n)

# AlolaMiddle <- averages_ratings %>% filter(PokemonName == "Brionne" |
#                                              PokemonName == "Torracat" |
#                                              PokemonName == "Dartrix") 

##### The most Popular average pokemon #####
# First we  filter pokemon by being above average popularity
# This is meanPop
# Then I'll use popSD to increment distance popularity while doing the whole mean thing

# Above Average
aboveMeanPop <- averages_ratings %>% filter(Popularity >= meanPop)

# 1 SD above mean popularity 

PopAboveSD1 <- aboveMeanPop %>% filter(Popularity >= (meanPop + popSD))

# 2 SD above mean popularity 
PopAboveSD2 <- aboveMeanPop %>% filter(Popularity >= (meanPop + popSD*2))

# 2.1 SD above mean popularity 
PopAboveSD2.1 <- aboveMeanPop %>% filter(Popularity >= (meanPop + popSD*2.1))

# 2.2 SD above mean popularity 
PopAboveSD2.2 <- aboveMeanPop %>% filter(Popularity >= (meanPop + popSD*2.2))


##### First Pass of most popular average #####

undermean <- averages - sds
overmean <- averages + sds

PopularAverage1 <- aboveMeanPop %>% filter(Complexity >= undermean$Complexity &
                                          Complexity <= overmean$Complexity &
                                          Realism >= undermean$Realism &
                                          Realism <= overmean$Realism &
                                          Artificiality >= undermean$Artificiality &
                                          Artificiality <= overmean$Artificiality & 
                                          Fantasy >= undermean$Fantasy &
                                          Fantasy <= overmean$Fantasy  & 
                                          Humanoid >= undermean$Humanoid &
                                          Humanoid <= overmean$Humanoid  &
                                          Cuteness >= undermean$Cuteness &
                                          Cuteness <= overmean$Cuteness  &
                                          Coolness >= undermean$Coolness &
                                          Coolness <= overmean$Coolness &
                                          Beauty <= overmean$Beauty  &
                                          Beauty >= undermean$Beauty)

# First Pass but +1 popularity SD

undermean <- averages - sds
overmean <- averages + sds

PopularAverageAboveSD1 <- PopAboveSD1 %>% filter(Complexity >= undermean$Complexity &
                                             Complexity <= overmean$Complexity &
                                             Realism >= undermean$Realism &
                                             Realism <= overmean$Realism &
                                             Artificiality >= undermean$Artificiality &
                                             Artificiality <= overmean$Artificiality & 
                                             Fantasy >= undermean$Fantasy &
                                             Fantasy <= overmean$Fantasy  & 
                                             Humanoid >= undermean$Humanoid &
                                             Humanoid <= overmean$Humanoid  &
                                             Cuteness >= undermean$Cuteness &
                                             Cuteness <= overmean$Cuteness  &
                                             Coolness >= undermean$Coolness &
                                             Coolness <= overmean$Coolness &
                                             Beauty <= overmean$Beauty  &
                                             Beauty >= undermean$Beauty)




##### Second pass reducing the sd by 25% #####

undermean2 <- averages - (sds*0.75)
overmean2 <- averages + (sds*0.75)

PopularAverage2 <- aboveMeanPop %>% filter(Complexity >= undermean2$Complexity &
                                           Complexity <= overmean2$Complexity &
                                           Realism >= undermean2$Realism &
                                           Realism <= overmean2$Realism &
                                           Artificiality >= undermean2$Artificiality &
                                           Artificiality <= overmean2$Artificiality & 
                                           Fantasy >= undermean2$Fantasy &
                                           Fantasy <= overmean2$Fantasy  & 
                                           Humanoid >= undermean2$Humanoid &
                                           Humanoid <= overmean2$Humanoid  &
                                           Cuteness >= undermean2$Cuteness &
                                           Cuteness <= overmean2$Cuteness  &
                                           Coolness >= undermean2$Coolness &
                                           Coolness <= overmean2$Coolness &
                                           Beauty <= overmean2$Beauty  &
                                           Beauty >= undermean2$Beauty)

# Second pass but +1 Popularity SD

undermean2 <- averages - (sds*0.75)
overmean2 <- averages + (sds*0.75)

PopularAverageAboveSD2 <- PopAboveSD1 %>% filter(Complexity >= undermean2$Complexity &
                                             Complexity <= overmean2$Complexity &
                                             Realism >= undermean2$Realism &
                                             Realism <= overmean2$Realism &
                                             Artificiality >= undermean2$Artificiality &
                                             Artificiality <= overmean2$Artificiality & 
                                             Fantasy >= undermean2$Fantasy &
                                             Fantasy <= overmean2$Fantasy  & 
                                             Humanoid >= undermean2$Humanoid &
                                             Humanoid <= overmean2$Humanoid  &
                                             Cuteness >= undermean2$Cuteness &
                                             Cuteness <= overmean2$Cuteness  &
                                             Coolness >= undermean2$Coolness &
                                             Coolness <= overmean2$Coolness &
                                             Beauty <= overmean2$Beauty  &
                                             Beauty >= undermean2$Beauty)
# It seems that the Pokemon above the SD of Popularity are not average in all traits





##### Third pass, reduce sd by 30% #####

undermean3 <- averages - (sds*0.70)
overmean3 <- averages + (sds*0.70)

PopularAverage3 <- aboveMeanPop %>% filter(Complexity >= undermean3$Complexity &
                                           Complexity <= overmean3$Complexity &
                                           Realism >= undermean3$Realism &
                                           Realism <= overmean3$Realism &
                                           Artificiality >= undermean3$Artificiality &
                                           Artificiality <= overmean3$Artificiality & 
                                           Fantasy >= undermean3$Fantasy &
                                           Fantasy <= overmean3$Fantasy  & 
                                           Humanoid >= undermean3$Humanoid &
                                           Humanoid <= overmean3$Humanoid  &
                                           Cuteness >= undermean3$Cuteness &
                                           Cuteness <= overmean3$Cuteness  &
                                           Coolness >= undermean3$Coolness &
                                           Coolness <= overmean3$Coolness &
                                           Beauty <= overmean3$Beauty  &
                                           Beauty >= undermean3$Beauty)


################## Distance from The Average ##################

distanceFromAverage <- averages_ratings %>% mutate(Complexity = abs(averages$Complexity - Complexity),
                                                      Realism = abs(averages$Realism - Realism),
                                                      Artificiality = abs(averages$Artificiality - Artificiality),
                                                      Fantasy = abs(averages$Fantasy - Fantasy),
                                                      Humanoid = abs(averages$Humanoid - Humanoid),
                                                      Cuteness = abs(averages$Cuteness - Cuteness),
                                                      Coolness =  abs(averages$Coolness - Coolness),
                                                      Beauty = abs(averages$Beauty - Beauty))

distanceFromAverageLong <- distanceFromAverage %>% 
  pivot_longer(cols = c(Complexity,
                        Realism,
                        Artificiality,
                        Fantasy,
                        Humanoid,
                        Cuteness,
                        Coolness,
                        Beauty), names_to = "Category", values_to = "Distance")
# 
# sds_long <- sds %>%
#   pivot_longer(cols = c(Complexity,
#                         Realism,
#                         Artificiality,
#                         Fantasy,
#                         Humanoid,
#                         Cuteness,
#                         Coolness,
#                         Beauty,
#                         Popularity), names_to = "Category", values_to = "Distance")

ggplot(distanceFromAverageLong %>% slice_min(Distance, n= 50), aes(x = DexNum, y = Distance, label = PokemonName)) +
  geom_point(position = "jitter") +
  geom_label(aes(x = DexNum, y = Distance, label = PokemonName)) +
  #scale_y_log10() +
        #geom_hline(data = sds, yintercept = , label = "SD") + 
    facet_wrap(~Category)


bottom1 <- distanceFromAverageLong %>% group_by(Category) %>%
  slice_min(Distance, n = 1) %>%
  subset(select = c(DexNum, PokemonName, Popularity, PopularitySD, MeanDesignSD, Category, Distance))



printPNG(filename = "MostAverage.png", dataset = grid.table(bottom1), height = 90*nrow(bottom1), width = 60*nrow(bottom1))

closestCool
closestCute
closestBeauty
closestComplexity
closestArtificality
closestHumanoid
closestRealism



