# Packages ----------------------------------------------------------------

#install.packages("readxl")
library(readxl)
#install.packages("ggplot2")
library(ggplot2)
# install.packages("caret",
#                  repos = "http://cran.r-project.org",
#                  dependencies = c("Depends","Imports","Suggests"))
library(caret)
#install.packages("corrplot")
library(corrplot)
#install.packages("e1071")
library(e1071)
#install.packages("RANN")
library(RANN)
#install.packages("randomForest")
library(randomForest)
#install.packages("skimr")
library(skimr)
#install.packages("dplyr")
library(dplyr)
#install.packages("plotly")
library(plotly)

# Wrangling ---------------------------------------------------------------

source("Scripts/2019/2019_Wrangling.R")

# Preprocessing -----------------------------------------------------------

source("Scripts/2019/2019_Preprocessing.R")

# Random Forest -----------------------------------------------------------

source("Scripts/RandomForest.R")

# Fitting -----------------------------------------------------------------

fit2019 <- predict(rf, player2019)

predictions2019 <- data.frame(Player = df_pred2019$Player,
                              Year = df_pred2019$Year_t1+1,
                              vorp_pred = fit2019) %>% 
  mutate_if(is.factor, as.character)

adv2019 <- read_excel("Data/2019_adv.xlsx") %>% 
  select(Player, Tm)

predictions2019 <- merge(predictions2019, adv2019, by = "Player") 

# Team Records ------------------------------------------------------------

source("Scripts/Team_Records.R")

# Rookies -----------------------------------------------------------------

source("Scripts/Rookies.R")

rookies2019 <- subset(rookies, Year == 2019) 

rook19 <- adv2019[which(!(adv2019$Player %in% predictions2019$Player)),] 

rookies2019 <- rookies2019 %>% 
  dplyr::mutate(vorp_pred = predict(rookie_lm, newdata = rookies2019)) %>% 
  select(Player, Year, Tm, vorp_pred) %>% 
  mutate_if(is.factor, as.character)

rookies2019 <- merge(rookies2019, rook19, by = "Player") %>% 
  select(Player, Year, Tm = Tm.y, vorp_pred)

# Team Predictions --------------------------------------------------------

df2019 <- rbind(rookies2019, predictions2019) %>% 
  dplyr::select(Player, Team = Tm, prediction_vorp = vorp_pred) %>% 
  mutate_if(is.factor, as.character)

df_2019_Tm <- df2019 %>% 
  group_by(Team) %>% 
  dplyr::summarize(vorp = sum(prediction_vorp)) 

df_2019_Tm <- as.data.frame(df_2019_Tm)

df_2019_Tm$Wins_pred_2019 = predict(lm_teams, newdata = df_2019_Tm)

teams2018 <- subset(teams_all, teams_all$Year == 2018) %>% 
  dplyr::select(Team, Wins_t1 = Wins)

df_2019_Tm <- merge(df_2019_Tm, teams2018)

Vegas_2019 <- data.frame(Team = c("PHI", "BOS", "OKC","GSW", "MIA", "MIL", "MEM",
                                  "DET", "IND", "CHO", "ORL", "BRK", "CLE",
                                  "NYK", "TOR", "ATL", "NOP", "HOU", "SAS",
                                  "SAC", "UTA", "LAC", "DEN", "DAL", "WAS",
                                  "CHI", "LAL", "POR"),
                         Wins_Vegas_2019 = c(54, 58, 48.5, 63, 42.5, 47.5, 34.5, 
                                             38.5, 47.5, 35.5, 31, 32, 30.5, 
                                             29.5, 54.5, 23.5, 45, 55.5, 43.5, 
                                             26, 49.5, 37.5, 47.5, 34.5, 45.5,
                                             29.5, 48.5, 42))

Pelton_2019 <- data.frame(Team = c("PHI", "BOS", "OKC","GSW", "MIA", "MIL", "MEM",
                                   "DET", "IND", "CHO", "ORL", "BRK", "CLE",
                                   "NYK", "TOR", "ATL", "NOP", "HOU", "SAS",
                                   "SAC", "UTA", "LAC", "DEN", "DAL", "WAS",
                                   "CHI", "LAL", "POR"),
                          Wins_Pelton_2019 = c(47.8, 53.2, 47.2, 58.6, 44.8, 45.2, 33.1, 
                                               39.4, 45.7, 38.3, 33.6, 36.8, 31, 
                                               30.8, 55.1, 25.9, 44.1, 53, 38.5, 
                                               25.4, 53.4, 35.5, 50.5, 32.1, 43.6,
                                               28, 41.2, 42))

Five38_2019 <- data.frame(Team = c("PHI", "BOS", "OKC","GSW", "MIA", "MIL", "MEM",
                                   "DET", "IND", "CHO", "ORL", "BRK", "CLE",
                                   "NYK", "TOR", "ATL", "NOP", "HOU", "SAS",
                                   "SAC", "UTA", "LAC", "DEN", "DAL", "WAS",
                                   "CHI", "LAL", "POR"),
                          Wins_538_2019 = c(52, 53, 53, 64, 41, 47, 32, 
                                            39, 43, 38, 33, 35, 30, 
                                            25, 55, 28, 46, 54, 40, 
                                            21, 54, 33, 48, 27, 48,
                                            27, 46, 39))

df_2019_Tm <- merge(df_2019_Tm, Vegas_2019, all.x = TRUE) %>% 
  mutate(Wins_Vegas_2019 = if_else(is.na(Wins_Vegas_2019), Wins_pred_2019, Wins_Vegas_2019),
         Error_Pred_Vegas = Wins_pred_2019 - Wins_Vegas_2019)

predictions_2019 <-  merge(df_2019_Tm, Pelton_2019, all.x = TRUE)

predictions_2019 <-  merge(predictions_2019, Five38_2019, all.x = TRUE) %>% 
  mutate(Error_Pelton_Vegas = Wins_Pelton_2019 - Wins_Vegas_2019,
         Error_538_Vegas = Wins_538_2019 - Wins_Vegas_2019)

predictions_2019$Sum_Error = predictions_2019$Error_Pred_Vegas + predictions_2019$Error_538_Vegas +
  predictions_2019$Error_Pelton_Vegas

q <- ggplot(df_2019_Tm, aes(y=Wins_pred_2019, x = Wins_Vegas_2019, text = Team))+
  geom_point(aes(color = Error_Pred_Vegas))+
  # geom_text(aes(label = Team), vjust = -1)+
  geom_abline(linetype = 2)+
  scale_x_continuous(breaks = seq(0,70,5))+
  scale_y_continuous(breaks = seq(0,70,5))+
  scale_color_gradient(low = "red", high = "limegreen")+
  theme_bw()

ggplotly(q)

predictions2019 <- data.frame(Player = df_pred2019$Player,
                              Year = df_pred2019$Year_t1+1,
                              vorp_t1 = df_pred2019$VORP_t1,
                              vorp_pred = round(fit2019,2)) %>% 
  mutate_if(is.factor, as.character) 

predictions2019 <- merge(predictions2019, adv2019, by = "Player") 

p <- ggplot(predictions2019, aes(x = vorp_t1, y = vorp_pred, text = Player)) +
  geom_jitter(aes(color = Tm))+
  # geom_text(aes(label = Team), vjust = -1)+
  geom_abline(linetype = 2)+
  theme_bw()

ggplotly(p)


q <- ggplot(predictions_2019, aes(y=Wins_pred_2019, x = Wins_Vegas_2019, text = Team))+
  geom_point(aes(color = Sum_Error, size = abs(Sum_Error)))+
  # geom_text(aes(label = Team), vjust = -1)+
  geom_abline(linetype = 2)+
  scale_x_continuous(breaks = seq(0,70,5))+
  scale_y_continuous(breaks = seq(0,70,5))+
  scale_color_gradient(low = "red", high = "limegreen")+
  theme_bw()

ggplotly(q)
