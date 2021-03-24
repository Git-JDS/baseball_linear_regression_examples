#Scatterplot of the relationship between HRs and wins
library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  ggplot(aes(HR_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)


#Scatterplot of the relationship between stolen bases and wins
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>%
  ggplot(aes(SB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)


#Scatterplot of the relationship between bases on balls and runs
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
  ggplot(aes(BB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)


Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(E_per_game = E/G, W_per_game = W/G) %>%
  ggplot(aes(E_per_game, W_per_game)) + 
  geom_point(alpha = 0.5)


Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(X2B_per_game = X2B/G, X3B_per_game = X3B/G) %>%
  ggplot(aes(X2B_per_game, X3B_per_game)) + 
  geom_point(alpha = 0.5)

#correlation coefficient between number of runs per game and number of at bats per game
rho <- mean(scale(x)*scale(y))
Teams %>% filter(yearID %in% 1961:2001 ) %>% summarize(r = cor(AB/G, R/G)) %>% pull(r)

#correlation coefficient between win rate (number of wins per game) and number of errors per game?
rho <- mean(scale(x)*scale(y))
Teams %>% filter(yearID %in% 1961:2001 ) %>% summarize(r = cor(W/G, E/G)) %>% pull(r)

#correlation coefficient between doubles (X2B) per game and triples (X3B) per game
rho <- mean(scale(x)*scale(y))
Teams %>% filter(yearID %in% 1961:2001 ) %>% summarize(r = cor(X2B/G, X3B/G)) %>% pull(r)


# find regression line for predicting runs from BBs
library(tidyverse)
library(Lahman)
bb_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>% 
  lm(R_per_game ~ BB_per_game, data = .) %>% 
  .$coef %>%
  .[2]
bb_slope

# compute regression line for predicting runs from singles
singles_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>%
  mutate(Singles_per_game = (H-HR-X2B-X3B)/G, R_per_game = R/G) %>%
  lm(R_per_game ~ Singles_per_game, data = .) %>%
  .$coef  %>%
  .[2]
singles_slope

# calculate correlation between HR, BB and singles
Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%  
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB,Singles))

# stratify HR per game to nearest 10, filter out strata with few points
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1), 
         BB_per_game = BB / G,
         R_per_game = R / G) %>%
  filter(HR_strata >= 0.4 & HR_strata <=1.2)

# scatterplot for each HR stratum
dat %>% 
  ggplot(aes(BB_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ HR_strata)

# calculate slope of regression line after stratifying by HR
dat %>%  
  group_by(HR_strata) %>%
  summarize(slope = cor(BB_per_game, R_per_game)*sd(R_per_game)/sd(BB_per_game))

# stratify by BB
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_strata = round(BB/G, 1), 
         HR_per_game = HR / G,
         R_per_game = R / G) %>%
  filter(BB_strata >= 2.8 & BB_strata <=3.9) 

# scatterplot for each BB stratum
dat %>% ggplot(aes(HR_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ BB_strata)

# slope of regression line after stratifying by BB
dat %>%  
  group_by(BB_strata) %>%
  summarize(slope = cor(HR_per_game, R_per_game)*sd(R_per_game)/sd(HR_per_game)) 


#Run a linear model in R predicting the number of runs per game based on 
#both the number of bases on balls per game and the number of home runs per game.
#What is the coefficient for bases on balls?
#????

#We have shown how BB and singles have similar predictive power for scoring runs. Another way to compare the usefulness of these baseball metrics is by assessing how stable they are across the years. 
#Because we have to pick players based on their previous performances, we will prefer metrics that are more stable. In these exercises, we will compare the stability of singles and BBs.
#Before we get started, we want to generate two tables: one for 2002 and another for the average of 1999-2001 seasons. We want to define per plate appearance statistics, keeping only players with more than 100 plate appearances. Here is how we create the 2002 table:

bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)
bat_02
summary(bat_02)

bat_9901 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)
bat_9901aggregate <-aggregate(.~playerID,FUN=mean, data=bat_9901)

#How many players had a single rate mean_singles of greater than 0.2 per plate appearance over 1999-2001?
length( which( bat_9901aggregate$singles > .2) )

#How many players had a BB rate mean_bb of greater than 0.2 per plate appearance over 1999-2001?
length( which( bat_9901aggregate$bb > .2) )

#Use inner_join() to combine the bat_02 table with the table of 1999-2001 rate averages you created in the previous question.
bat_029901agg <- inner_join(bat_02,bat_9901aggregate, by = "playerID")

#What is the correlation between 2002 singles rates and 1999-2001 average singles rates?
rho <- mean(scale(x)*scale(y))
bat_029901agg %>% summarize(r = cor(singles.x, singles.y)) %>% pull


#What is the correlation between 2002 BB rates and 1999-2001 average BB rates?
rho <- mean(scale(x)*scale(y))
bat_029901agg %>% summarize(r = cor(bb.x, bb.y)) %>% pull
#in the code above bb.y and singles.y = the mean values of 1999-2001


#Make scatterplots of mean_singles versus singles and mean_bb versus bb.
bat_029901agg %>%
  ggplot(aes(singles.x, singles.y)) + 
  geom_point(alpha = 0.5)

bat_029901agg %>%
  ggplot(aes(bb.x, bb.y)) + 
  geom_point(alpha = 0.5)

#Fit a linear model to predict 2002 singles given 1999-2001 mean_singles.
model_z <- lm(singles.x ~ singles.y, data=bat_029901agg)
summary(model_z)

#Fit a linear model to predict 2002 bb given 1999-2001 mean_bb.
model_bb <- lm(bb.x ~ bb.y, data=bat_029901agg)
summary(model_bb)





