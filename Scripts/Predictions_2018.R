
# Create Predictions ------------------------------------------------------

source("Scripts/Preprocessing_test2018.R")

source("Scripts/RandomForest.R")

predictions <- data.frame(Player = df_pred$Player,
                          Year = df_pred$Year_t1+1,
                          y = y_test,
                          y_rf_mean = rfPred,
                          y_rf_knn = rfPred_knn) %>% 
  mutate(error_rf_mean = y_rf_mean - y,
         error_rf_knn = y_rf_knn - y) %>% 
  mutate_if(is.factor, as.character)

# Merge Data --------------------------------------------------------------

adv2018 <- read_excel("Data/2018_adv.xlsx") %>% 
  subset(Tm != "TOT") %>% 
  group_by(Player) %>% 
  top_n(n=1,G)

df2018 <- merge(predictions, adv2018, by = "Player") %>% 
  dplyr::select(Player, Team = Tm, G, MP, vorp = VORP, prediction_vorp = y_rf_mean)

df_2018_Tm <- df2018 %>% 
  group_by(Team) %>% 
  dplyr::summarize(vorp = sum(prediction_vorp)) 

df_2018_Tm$wins_pred = predict(lm_teams, newdata = df_2018_Tm)

teams2018 <- subset(teams_all, teams_all$Year == 2018) %>% 
  dplyr::select(Team, Wins, VORP = vorp)

df_2018_Tm <- merge(df_2018_Tm, teams2018, by = "Team")

ggplot(df_2018_Tm, aes(x=wins_pred, y = Wins))+
  geom_point()+
  geom_text(aes(label = Team), vjust = -1)+
  geom_abline(linetype = 2)+
  scale_x_continuous(breaks = seq(0,70,5))+
  scale_y_continuous(breaks = seq(0,70,5))+
  theme_bw()
