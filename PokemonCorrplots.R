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

corr_function <- function(data, variables){
  data <- data %>% subset(select = variables)
  corrplot(cor(data), addCoef.col ='black', type = 'lower')
}

Categories <- c("Complexity",
                "Realism",
                "Artificiality",
                "Fantasy",
                "Humanoid",
                "Cuteness",
                "Coolness",
                "Beauty",
                "Popularity")


pokemonratings <- read.csv("all-ratings_enriched.csv") %>% na.omit()
pokemonratings <- read.csv("average-ratings_enriched.csv")


typeFactor <- factor(levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19),
                     labels = c("Normal", "Fighting", "Flying", "Poison", "Ground", "Rock", "Bug", "Ghost", "Steel",
                                "Fire", "Water", "Grass", "Electric", "Psychic", "Ice", "Dragon", "Dark", "Fairy", "No 2nd Type"),
                     ordered = TRUE)


gen1 <- pokemonratings %>% filter(Generation == 1)
gen2 <- pokemonratings %>% filter(Generation == 2)
gen3 <- pokemonratings %>% filter(Generation == 3)
gen4 <- pokemonratings %>% filter(Generation == 4)
gen5 <- pokemonratings %>% filter(Generation == 5)
gen6 <- pokemonratings %>% filter(Generation == 6)
gen7 <- pokemonratings %>% filter(Generation == 7)
gen8 <- pokemonratings %>% filter(Generation == 8)
gen9 <- pokemonratings %>% filter(Generation == 9)

kanto <- pokemonratings %>% filter(Region == "Kanto")
johto <- pokemonratings %>% filter(Region == "Johto")
hoenn <- pokemonratings %>% filter(Region == "Hoenn")
sinnoh <- pokemonratings %>% filter(Region == "Sinnoh")
unova <- pokemonratings %>% filter(Region == "Unova")
kalos <- pokemonratings %>% filter(Region == "Kalos")
alola <- pokemonratings %>% filter(Region == "Alola")
galar <- pokemonratings %>% filter(Region == "Galar")
hisui <- pokemonratings %>% filter(Region == "Hisui")
paldea <- pokemonratings %>% filter(Region == "Paldea")
kitakami <- pokemonratings %>% filter(Region == "Kitakami")


##### Regions #####

png("Gen1Corrplot.png", width = 500, height = 500)
corr_function(gen1, Categories)
dev.off()

png("Gen2Corrplot.png", width = 500, height = 500)
corr_function(gen2, Categories)
dev.off()

png("Gen3Corrplot.png", width = 500, height = 500)
corr_function(gen3, Categories)
dev.off()

png("Gen4Corrplot.png", width = 500, height = 500)
corr_function(gen4, Categories)
dev.off()

png("Gen5Corrplot.png", width = 500, height = 500)
corr_function(gen5, Categories)
dev.off()

png("Gen6Corrplot.png", width = 500, height = 500)
corr_function(gen6, Categories)
dev.off()

png("Gen7Corrplot.png", width = 500, height = 500)
corr_function(gen7, Categories)
dev.off()

png("Gen8Corrplot.png", width = 500, height = 500)
corr_function(gen8, Categories)
dev.off()

png("Gen9Corrplot.png", width = 500, height = 500)
corr_function(gen9, Categories)
dev.off()

png("KantoCorrplot.png", width = 500, height = 500)
corr_function(kanto, Categories)
dev.off()

png("JohtoCorrplot.png", width = 500, height = 500)
corr_function(johto, Categories)
dev.off()

png("HoennCorrplot.png", width = 500, height = 500)
corr_function(hoenn, Categories)
dev.off()

png("SinnohCorrplot.png", width = 500, height = 500)
corr_function(sinnoh, Categories)
dev.off()

png("UnovaCorrplot.png", width = 500, height = 500)
corr_function(unova, Categories)
dev.off()

png("KalosCorrplot.png", width = 500, height = 500)
corr_function(kalos, Categories)
dev.off()

png("AlolaCorrplot.png", width = 500, height = 500)
corr_function(alola, Categories)
dev.off()

png("GalarCorrplot.png", width = 500, height = 500)
corr_function(galar, Categories)
dev.off()

png("HisuiCorrplot.png", width = 500, height = 500)
corr_function(hisui, Categories)
dev.off()

png("PaldeaCorrplot.png", width = 500, height = 500)
corr_function(paldea, Categories)
dev.off()

png("KitakamiCorrplot.png", width = 500, height = 500)
corr_function(kitakami, Categories)
dev.off()

####



##### Generations #####
printPNG("Gen1.png", corr_function(gen1, Categories), width = 1620, height = 1620)

printPNG("Gen2.png", corr_function(gen2, Categories), width = 1620, height = 1620)

printPNG("Gen3.png", corr_function(gen3, Categories), width = 1620, height = 1620)

printPNG("Gen4.png", corr_function(gen4, Categories), width = 1620, height = 1620)

printPNG("Gen5.png", corr_function(gen5, Categories), width = 1620, height = 1620)

printPNG("Gen6.png", corr_function(gen6, Categories), width = 1620, height = 1620)

printPNG("Gen7.png", corr_function(gen7, Categories), width = 1620, height = 1620)

printPNG("Gen8.png", corr_function(gen8, Categories), width = 1620, height = 1620)

printPNG("Gen9.png", corr_function(gen9, Categories), width = 1620, height = 1620)

####


##### Correlate Types and Other Qualities

byType <-  pokemonratings %>%   
  pivot_longer(cols = c(Type1, Type2), names_to = "TypeSlot", values_to = "Type") %>%
  subset(select = -c(TypeSlot, TypeID1, TypeID2)) %>%
  mutate(TypeID = case_when(Type == "Normal" ~ 1,
                             Type == "Fighting" ~ 2,
                             Type == "Flying" ~ 3,
                             Type == "Poison" ~4,
                             Type =="Ground" ~ 5,
                             Type =="Rock" ~ 6,
                             Type == "Bug" ~ 7,
                             Type == "Ghost" ~ 8, 
                             Type=="Steel" ~ 9,
                             Type == "Fire" ~ 10,
                             Type =="Water" ~ 11,
                             Type == "Grass" ~ 12,
                             Type == "Electric" ~ 13,
                             Type == "Psychic" ~ 14,
                             Type == "Ice" ~ 15,
                             Type == "Dragon" ~ 16,
                             Type == "Dark" ~ 17,
                             Type == "Fairy" ~ 18,
                             Type == "No 2nd Type" ~ 19)) %>%
  filter(Type != "No 2nd Type")
  
# levels(byType$Type) <- factor(typeFactor)

typeCats <- append(Categories, "TypeID")



