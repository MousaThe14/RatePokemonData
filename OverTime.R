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
library(devtools)
library(splitstackshape)


ratings <- read.csv("all-ratings_enriched.csv")


rates_dated <- ratings %>% arrange(Timestamp) %>% #Arranges chronologically
  group_by(PokemonName)


rates_counted <- data.frame(getanID(rates_dated, c("PokemonName")))%>%
  rename(VoteCount = .id)

rates_counted_long <- rates_counted %>%
  pivot_longer(cols = c(Complexity,
                        Realism,
                        Artificiality,
                        Fantasy,
                        Humanoid,
                        Cuteness,
                        Coolness,
                        Beauty,
                        Popularity), names_to = "Category", values_to = "Rating")

rates_counted_long <- rates_counted_long %>%
  arrange(Timestamp) %>%
  group_by(PokemonName,Category) %>%
  mutate(RunningSum = cumsum(Rating)) %>%
  mutate(AverageOverTime = RunningSum/VoteCount) 

rates_counted_long_region <- rates_counted_long %>%
  arrange(Timestamp) %>%
  group_by(Region, Category, VoteCount) %>%
  reframe(VoteCount = mean(VoteCount),
          AverageOverTime = mean(AverageOverTime))
  

  

Umbreon <- rates_counted_long %>% filter(PokemonName == "Umbreon")

ggplot(Umbreon, aes(x = VoteCount, y = AverageOverTime, group = Category, color = Category, fill = Category)) +
  geom_line()

ggplot(rates_counted_long_region, aes(x = VoteCount, y = AverageOverTime, group = Region, color = Category, fill = Category)) +
  geom_line() +
  facet_wrap(~Region)

