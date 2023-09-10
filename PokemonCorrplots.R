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

corr_function <- function(data){
  data <- data %>% subset(select = c(Complexity, Realism, Artificiality, Fantasy, Humanoid, Cuteness, Coolness, Beauty, Popularity))
  corrplot(cor(data), addCoef.col ='black', type = 'lower')
}


pokemonratings <- read.csv("all-ratings_w_gens.csv") %>% na.omit()
pokemonratings <- read.csv("average-ratings_w_gens.csv")

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


png("Gen1Corrplot.png", width = 500, height = 500)
corr_function(gen1)
dev.off()

png("Gen2Corrplot.png", width = 500, height = 500)
corr_function(gen2)
dev.off()

png("Gen3Corrplot.png", width = 500, height = 500)
corr_function(gen3)
dev.off()

png("Gen4Corrplot.png", width = 500, height = 500)
corr_function(gen4)
dev.off()

png("Gen5Corrplot.png", width = 500, height = 500)
corr_function(gen5)
dev.off()

png("Gen6Corrplot.png", width = 500, height = 500)
corr_function(gen6)
dev.off()

png("Gen7Corrplot.png", width = 500, height = 500)
corr_function(gen7)
dev.off()

png("Gen8Corrplot.png", width = 500, height = 500)
corr_function(gen8)
dev.off()

png("Gen9Corrplot.png", width = 500, height = 500)
corr_function(gen9)
dev.off()

png("KantoCorrplot.png", width = 500, height = 500)
corr_function(kanto)
dev.off()

png("JohtoCorrplot.png", width = 500, height = 500)
corr_function(johto)
dev.off()

png("HoennCorrplot.png", width = 500, height = 500)
corr_function(hoenn)
dev.off()

png("SinnohCorrplot.png", width = 500, height = 500)
corr_function(sinnoh)
dev.off()

png("UnovaCorrplot.png", width = 500, height = 500)
corr_function(unova)
dev.off()

png("KalosCorrplot.png", width = 500, height = 500)
corr_function(kalos)
dev.off()

png("AlolaCorrplot.png", width = 500, height = 500)
corr_function(alola)
dev.off()

png("GalarCorrplot.png", width = 500, height = 500)
corr_function(galar)
dev.off()

png("HisuiCorrplot.png", width = 500, height = 500)
corr_function(hisui)
dev.off()

png("PaldeaCorrplot.png", width = 500, height = 500)
corr_function(paldea)
dev.off()
