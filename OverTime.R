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


types_long <- rates_dated %>%
  pivot_longer(cols = c(Type1, Type2),
               names_to = "Slot", values_to = "Type") %>%
  subset(select = -Slot) %>%
  filter(Type != "No 2nd Type") %>%
  getanID(c("Type")) %>%
  rename(VoteCount = .id) %>%
  data.frame() %>%
arrange(Timestamp) %>%
  group_by(Type) %>%
  # reframe(VoteCount = mean(VoteCount),
  #         AverageOverTime = mean(AverageOverTime))
  mutate(RunningComplexity = cumsum(Complexity)) %>%
  mutate(RunningRealism = cumsum(Realism)) %>%
  mutate(RunningArtificiality = cumsum(Artificiality)) %>%
  mutate(RunningFantasy = cumsum(Fantasy)) %>%
  mutate(RunningHumanoid = cumsum(Humanoid)) %>%
  mutate(RunningCuteness = cumsum(Cuteness)) %>%
  mutate(RunningCoolness = cumsum(Coolness)) %>%
  mutate(RunningBeauty = cumsum(Beauty)) %>%
  mutate(RunningPopularity = cumsum(Popularity))%>%
  mutate(AvgComplexityOverTime = RunningComplexity/VoteCount) %>%
  mutate(AvgRealismOverTime = RunningRealism/VoteCount) %>%
  mutate(AvgArtificialityOverTime = RunningArtificiality/VoteCount) %>%
  mutate(AvgFantasyOverTime = RunningFantasy/VoteCount) %>%
  mutate(AvgHumanoidOverTime = RunningHumanoid/VoteCount) %>%
  mutate(AvgCutenessOverTime = RunningCuteness/VoteCount) %>%
  mutate(AvgCoolnessOverTime = RunningCoolness/VoteCount)%>%
  mutate(AvgBeautyOverTime = RunningBeauty/VoteCount) %>%
  mutate(AvgPopularityOverTime = RunningPopularity/VoteCount)
  
  
ggplotly(ggplot(types_long, aes(x = VoteCount, y = AvgComplexityOverTime, group = Type, color = Type)) +
  geom_line(alpha = 0.5, linewidth = 1.2) +
  scale_color_manual(values = typeColorPalette) +
  scale_x_log10() + 
  labs(title = "Average Popularity Over Time by Region") )

###########################


rates_by_region <-rates_dated %>%
  getanID(c("Region")) %>%
  rename(VoteCount = .id) %>%
  data.frame() %>%
  group_by(Region) %>%
  arrange(Timestamp) %>%
  # reframe(VoteCount = mean(VoteCount),
  #         AverageOverTime = mean(AverageOverTime))
  mutate(RunningComplexity = cumsum(Complexity)) %>%
  mutate(RunningRealism = cumsum(Realism)) %>%
  mutate(RunningArtificiality = cumsum(Artificiality)) %>%
  mutate(RunningFantasy = cumsum(Fantasy)) %>%
  mutate(RunningHumanoid = cumsum(Humanoid)) %>%
  mutate(RunningCuteness = cumsum(Cuteness)) %>%
  mutate(RunningCoolness = cumsum(Coolness)) %>%
  mutate(RunningBeauty = cumsum(Beauty)) %>%
  mutate(RunningPopularity = cumsum(Popularity))%>%
  mutate(AvgComplexityOverTime = RunningComplexity/VoteCount) %>%
  mutate(AvgRealismOverTime = RunningRealism/VoteCount) %>%
  mutate(AvgArtificialityOverTime = RunningArtificiality/VoteCount) %>%
  mutate(AvgFantasyOverTime = RunningFantasy/VoteCount) %>%
  mutate(AvgHumanoidOverTime = RunningHumanoid/VoteCount) %>%
  mutate(AvgCutenessOverTime = RunningCuteness/VoteCount) %>%
  mutate(AvgCoolnessOverTime = RunningCoolness/VoteCount)%>%
  mutate(AvgBeautyOverTime = RunningBeauty/VoteCount) %>%
  mutate(AvgPopularityOverTime = RunningPopularity/VoteCount)



ggplotly(ggplot(types_long, aes(x = VoteCount, y = AvgComplexityOverTime, group = Region, color = Region)) +
           geom_line(alpha = 0.5, linewidth = 1.1) +
           #  scale_color_manual(values = typeColorPalette) +
           # scale_x_log10() + 
           labs(title = "Average Popularity Over Time by Region")+
           facet_wrap(~Region)
         ) 


