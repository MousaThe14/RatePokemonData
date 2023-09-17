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

rawratings <- read.csv("all-ratings_w_gens.csv") %>% na.omit()
averageratings <- read.csv("average-ratings_w_gens.csv") %>% na.omit()

PopularitySDRegion <- averageratings %>%
  group_by(Region) %>% 
  summarise(MeanPopD = mean(PopularitySD))%>%
  mutate(Order = case_when(Region == "Kanto" ~ 1, Region == "Johto" ~ 2, Region == "Hoenn" ~ 3, Region == "Sinnoh" ~ 4, Region == "Unova" ~ 5, Region == "Kalos" ~ 6, Region == "Alola" ~ 7, Region == "Galar" ~ 8, Region == "Hisui" ~ 9, Region == "Paldea" ~ 10))  %>%
  mutate(Region = factor(Region, levels = c("Kanto",
                                            "Johto",
                                            "Hoenn",
                                            "Sinnoh",
                                            "Unova",
                                            "Kalos",
                                            "Alola",
                                            "Galar",
                                            "Hisui",
                                            "Paldea"), ordered = TRUE))


DesignSDRegion <- averageratings %>%
  group_by(Region) %>%
  summarise(MeanDD = mean(MeanDesignSD)) %>%
  mutate(Order = case_when(Region == "Kanto" ~ 1, Region == "Johto" ~ 2, Region == "Hoenn" ~ 3, Region == "Sinnoh" ~ 4, Region == "Unova" ~ 5, Region == "Kalos" ~ 6, Region == "Alola" ~ 7, Region == "Galar" ~ 8, Region == "Hisui" ~ 9, Region == "Paldea" ~ 10))  %>%
  mutate(Region = factor(Region, levels = c("Kanto",
                                            "Johto",
                                            "Hoenn",
                                            "Sinnoh",
                                            "Unova",
                                            "Kalos",
                                            "Alola",
                                            "Galar",
                                            "Hisui",
                                            "Paldea"), ordered = TRUE))



PDByRegion <- ggplot(PopularitySDRegion, aes(x = reorder(Region, Order), y = MeanPopD, fill = MeanPopD))+
  geom_col() +
  geom_hline(yintercept = mean(PopularitySDRegion$MeanPopD)) +
  scale_fill_viridis_c()

DDByRegion <- ggplot(DesignSDRegion, aes(x = reorder(Region, Order), y = MeanDD, fill = MeanDD))+
  geom_col() +
  geom_hline(yintercept = mean(DesignSDRegion$MeanDD)) +
  scale_fill_viridis_c()




PopularitySDGen <- averageratings %>%
  group_by(Generation) %>%
  summarise(MeanPopD = mean(PopularitySD))

DesignSDGen <- averageratings %>%
  group_by(Generation) %>%
  summarise(MeanDD = mean(MeanDesignSD))



PDByGen <- ggplot(PopularitySDGen, aes(x = Generation, y = MeanPopD, fill = MeanPopD))+
  geom_col() +
  geom_hline(yintercept = mean(PopularitySDGen$MeanPopD)) +
  scale_fill_viridis_c()

DDByGen <- ggplot(DesignSDGen, aes(x = Generation, y = MeanDD, fill = MeanDD))+
  geom_col() +
  geom_text(DesignSDRegion, aes(x = Region, y = MeanDD, label = Region)) +
  geom_hline(yintercept = mean(DesignSDGen$MeanDD)) +
  scale_fill_viridis_c()

ggarrange(DDByGen, DDByRegion)

ggarrange(PDByGen, PDByRegion)
