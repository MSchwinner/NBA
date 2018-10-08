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

df_2019_Tm$wins_pred = predict(lm_teams, newdata = df_2019_Tm)

teams2018 <- subset(teams_all, teams_all$Year == 2018) %>% 
  dplyr::select(Team, Wins_t1 = Wins)

df_2019_Tm <- merge(df_2019_Tm, teams2018)

Vegas <- data.frame(Team = c("PHI", "BOS", "GSW", "MIA", "MIL", "MEM",
                             "DET", "IND", "CHO", "ORL", "BRK", "CLE",
                             "NYK", "TOR", "ATL", "NOP", "HOU", "SAS",
                             "SAC", "UTA", "LAC", "DEN", "DAL", "WAS",
                             "CHI", "LAL", "POR"),
                    Wins_Vegas = c(54, 58, 63, 41.5, 47.5, 34.5, 38.5, 46.5,
                             35.5, 31, 32, 30.5, 29.5, 54.5, 23.5, 45,
                             55.5, 45.5, 26, 48.5, 35.5, 47.5, 34.5, 44.5,
                             29.5, 48.5, 42))

df_2019_Tm <- merge(df_2019_Tm, Vegas, all.x = TRUE) %>% 
  mutate(Wins_Vegas = if_else(is.na(Wins_Vegas), wins_pred, Wins_Vegas),
         error_vegas = wins_pred - Wins_Vegas)

q <- ggplot(df_2019_Tm, aes(y=wins_pred, x = Wins_Vegas, text = Team))+
  geom_point(aes(color = error_vegas))+
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
                              vorp_pred = fit2019) %>% 
  mutate_if(is.factor, as.character)

predictions2019 <- merge(predictions2019, adv2019, by = "Player") 

p <- ggplot(predictions2019, aes(x = vorp_t1, y = round(vorp_pred,2), text = Player)) +
  geom_jitter(aes(color = Tm))+
  # geom_text(aes(label = Team), vjust = -1)+
  geom_abline(linetype = 2)+
  theme_bw()

ggplotly(p)
