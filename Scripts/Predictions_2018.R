adv2018 <- read_excel("C:/Users/AT0723302/Desktop/NBA/Data/2018_adv.xlsx") %>% 
  subset(Tm != "TOT") %>% 
  group_by(Player) %>% 
  top_n(n=1,G)

df2018 <- merge(predictions, adv2018, by = "Player") %>% 
  select(Player, Team = Tm, G, MP, prediction_vorp = y_rf_mean)

df_2018_Tm <- df2018 %>% 
  group_by(Team) %>% 
  summarize(vorp = sum(prediction_vorp)) 

df_2018_Tm$wins_pred = predict(lm_teams, newdata = df_2018_Tm)

teams2018 <- subset(teams_all, teams_all$Year == 2018) %>% 
  select(Team, Wins, VORP = vorp)

df_2018_Tm <- merge(df_2018_Tm, teams2018, by = "Team")

ggplot(df_2018_Tm, aes(x=wins_pred, y = Wins))+
  geom_point()+
  geom_text(aes(label = Team), vjust = -1)+
  geom_abline(linetype = 2)+
  theme_bw()
