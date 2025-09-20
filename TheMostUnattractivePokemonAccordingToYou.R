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
library(GGally)


### Get Mode ###
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

### Get Population Mean ###
sd.p =  function(x){
  sd(x) * sqrt((length(x)-1)/length(x))
} 





average_ratings <- read.csv("average-ratings_enriched_151Ratings.csv")
raw_ratings <- read.csv("all-ratings_enriched_151Ratings.csv")
globalAverageTraits <- read.csv("GlobalAverages_151Ratings.csv")
globalAverageGenerations <- read.csv("GlobalAverage-Gen.csv")
globalSDs <- average_ratings %>% summarise(PopularitySD = mean(PopularitySD), MeanDesignSD = mean(MeanDesignSD))

globalAverageTraits <- globalAverageTraits %>% cbind(globalSDs)
globalAverageTypes <- read.csv( "GlobalAverages-Type_151Ratings.csv")



corr_functionP <- function(data, variables){
  data <- data %>% subset(select = variables)
  return(corrplot(cor(data, method = "pearson"), addCoef.col ='black', type = 'lower', number.cex=2, cl.cex = 2, tl.cex = 2))
}
corr_functionK <- function(data, variables){
  data <- data %>% subset(select = variables)
  return(corrplot(cor(data, method = "kendall"), addCoef.col ='black', type = 'lower'))
}
corr_functionS <- function(data, variables){
  data <- data %>% subset(select = variables)
  return(corrplot(cor(data, method = "spearman"), addCoef.col ='black', type = 'lower'))
}


# 1920 x 1080 video resolution, needed for when I print charts


# ttest <- function(){
#   data <- data %>% subset(select = variables)
# cor(data)
#   
# }


# averageComplexity
# averageRealism
# averageBeauty
# averageCuteness
averagePopularity <- average_ratings %>% filter(round(Popularity,2) == round(globalAverageTraits$Popularity,2))



Categories <- c("Complexity",
                "Realism",
                "Artificiality",
                "Fantasy",
                "Humanoid",
                "Cuteness",
                "Coolness",
                "Beauty",
                "Popularity")

# Global Averages
# Popularity  3.533
# Beauty      3.019
# Ugliest Type    - Poison - 2.669
# Ugliest Gen     -  Gen 8 - 2.878
# Cuteness    3.019
# Least Cute Type - Steel - 2.488
# Least Cute Gen  - 8 - 2.801
# Most Popular Gen - Gen 2 - 3.682
# Least Popular Gen - Gen 9 - 3.356 
# Most Popular Type - Ghost - 3.810
# Least Popular Type - Normal - 3.402


categoryCorrelations <- corr_functionP(pokemonaverages, Categories)

png("CorrelationOfCategories.png", width = 1080, height = 1080)
corr_functionP(pokemonaverages, Categories)
dev.off()

# Correlation coefficients

## Beauty - Cuteness
### 0.66967295, 66.97%
## Beauty - Popularity
### 0.66645575
## Cuteness - Popularity
### 0.36194970


ugliest30 <- average_ratings %>% slice_min(Beauty, n = 30)
uncute30 <- average_ratings %>% slice_min(Cuteness, n = 30)

UglyUncute <- full_join(ugliest30, uncute30)
 
 ugliest31 <- average_ratings %>% slice_min(Beauty, n = 31)
 uncute31 <- average_ratings %>% slice_min(Cuteness, n = 31)
# 
 UglyUncute2 <- full_join(ugliest31, uncute31)

UglyUncuteCommon <- inner_join(ugliest30, uncute30)

LeastPopular50 <- average_ratings %>% slice_min(Popularity, n = 150)

LeastPopular50ByGen <- LeastPopular50 %>% group_by(Generation)


UnpopularUnattractive <- inner_join(UglyUncute, LeastPopular50)

LeastPopLeastCute <- inner_join(LeastPopular50, uncute30)
LeastPopUgly <- inner_join(LeastPopular50, ugliest30)

UncuteUglyUnpopular <- full_join(LeastPopUgly, LeastPopLeastCute)

UncuteUglyUnpopular <- UncuteUglyUnpopular %>% slice_min(Popularity, n = 20)

# 
# averages_beauty_cute_ratio <- average_ratings %>% mutate(BeautyOverCute = Beauty/Cuteness)
# 
# 
# modelCutePopular <- lm(Popularity ~ Cuteness, data = average_ratings)
# 
# modelBeautyPopular <- lm(Popularity ~ Beauty, data = average_ratings)
# 
# modelCuteBeauty <- lm(Beauty ~ Cuteness, data = average_ratings)
# modelBeautyCute <- lm(Cuteness ~ Beauty, data = average_ratings)
# 
# modelCoolPopular <- lm(Popularity ~ Coolness, data = average_ratings)
# 
# modelCuteBeautyCool <- lm(Popularity ~ Coolness + Beauty + Cuteness, average_ratings)
# # The R^2 is 0.826, which means that 82.6% of the variation Popularity can be explained by the 3 variables
# average_ratings <- average_ratings %>% mutate(PrettyCuteCool = Cuteness + Beauty + Coolness)


##### The Modal Ratings for each of the "least attractive" pokemon


names(UnpopularUnattractive) <- c("DexNum",
                          "PokemonName",
                          "PokeApiName",
                          "AvgComplexity",
                          "AvgRealism",
                          "AvgArtificiality",
                          "AvgFantasy",
                          "AvgHumanoid",
                          "AvgCuteness",
                          "AvgCoolness",
                          "AvgBeauty",
                          "AvgPopularity")



#unattractiveRatings<- inner_join(UnpopularUnattractive, raw_ratings)
unattractiveNames <- UnpopularUnattractive %>% subset(select = PokemonName)

unatractiveRatings <- inner_join(unattractiveNames,raw_ratings) 

unatractiveRatings_long <-  unatractiveRatings %>%
  pivot_longer(cols = c(Complexity,
                        Realism,
                        Artificiality,
                        Fantasy,
                        Humanoid,
                        Cuteness,
                        Coolness,
                        Beauty,
                        Popularity), names_to = "Category", values_to = "Ratings")






#JynxRatings5 <- raw_ratings %>% filter(PokemonName == "Jynx" & Popularity == 5)
#JynxRatings4 <- raw_ratings %>% filter(PokemonName == "Jynx" & Popularity == 4)
#nrow(JynxRatings4)
#nrow(JynxRatings5)


plot_theme <- theme(axis.line = element_line(colour = "#21386E"),
                    panel.grid.major = element_line(color = "#FFCB05"),
                    panel.grid.minor = element_blank(),
                    panel.border = element_blank(),
                    panel.background = element_blank(),
                    element_text(family = "Pokemon Solid"))

unatractiveRatings_long <- unatractiveRatings_long  %>% filter(Category == "Beauty" | Category ==  "Cuteness" | Category ==  "Popularity")

ggplot(unatractiveRatings_long %>% filter(PokemonName == "Rellor"), aes(x = Ratings)) +
  geom_bar() +
  facet_wrap(~toupper(Category)) +
  plot_theme




#####




  
ggplot(average_ratings, aes(x = Cuteness, y = Beauty)) +
  geom_point() +
  # geom_smooth(se=FALSE) +
  stat_poly_line() +
  stat_poly_eq() +
  geom_abline(intercept = 1.5712, slope = 0.4798) +
  stat_regline_equation(label.x.npc = "center")
# The R^2 is 0.45, which means that 45% of the variation in Beauty can be explained by Cuteness

ggplot(average_ratings, aes(x = Beauty, y = Cuteness)) +
  geom_point() +
  # geom_smooth(se=FALSE) +
  stat_poly_line() +
  stat_poly_eq() +
  geom_abline(intercept = 0.19623, slope = 0.93462) +
  stat_regline_equation(label.x.npc = "center")




ggplot(average_ratings, aes(x = Cuteness, y = Popularity)) +
  geom_point() +
  # geom_smooth(se=FALSE) +
  stat_poly_line() +
  stat_poly_eq() +
  geom_abline(intercept = 2.99157, slope = 0.17933) +
  stat_regline_equation(label.x.npc = "center")
# The R^2 is 0.13 which means that only 13% of the variation in Popularity can be explain by Cuteness

ggplot(average_ratings, aes(x = Beauty, y = Popularity)) +
  geom_point() +
  # geom_smooth(se=FALSE) +
  stat_poly_line() +
  stat_poly_eq() +
  geom_abline(intercept = 2.14137, slope = 0.46083) +
  stat_regline_equation(label.x.npc = "center")
# The R^2 is 0.44 which means that 44% of the variation in Popularity can be explained by Beauty

ggplot(average_ratings, aes(x = Coolness, y = Popularity)) +
  geom_point() +
  # geom_smooth(se=FALSE) +
  stat_poly_line() +
  stat_poly_eq() +
  geom_abline(intercept = 2.00586, slope = 0.46024) +
  stat_regline_equation(label.x.npc = "center")
# The R^2 is 0.48 which means that 48% of the variation in Popularity can be explained by Coolness

ggplot(average_ratings, aes(x = Coolness, y = Popularity)) +
  geom_point() +
  # geom_smooth(se=FALSE) +
  stat_poly_line() +
  stat_poly_eq() +
  geom_abline(intercept = 2.00586, slope = 0.46024) +
  stat_regline_equation(label.x.npc = "center")
# The R^2 is 0.48 which means that 48% of the variation in Popularity can be explained by Coolness




# ggplot() +
#   geom_point(average_ratings, mapping = aes(x = Coolness, y = Popularity),  color = "red") +
#   stat_poly_line(average_ratings, mapping = aes(x = Coolness, y = Popularity), color = "red")  +
#   geom_point(average_ratings, mapping = aes(x = Cuteness, y = Popularity), color = "pink") +
#   stat_poly_line(average_ratings, mapping = aes(x = Cuteness, y = Popularity), color = "pink") +
#   geom_point(average_ratings, mapping = aes(x = Beauty, y = Popularity), color = "blue") +
#   stat_poly_line(average_ratings, mapping = aes(x = Beauty, y = Popularity), color = "blue") +
#   xlab("Average Ratings")


ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

ggplotRegression(lm(Popularity ~ Coolness, average_ratings))

ggpairs(average_ratings)