# Data Import -------------------------------------------------------------

rookies <- read.csv("Data/Rookies.csv")
rookies$Player <- as.character(rookies$Player)
rookies$Year <- as.numeric(rookies$Year) +1

#createing list of dataframes with game data
df_game <- list()
k <- 1

for (i in 2013:2018) {
  nam <- paste("g", i, sep = "")
  df_game[[k]] <- assign(nam, read_excel(paste("Data/",i,"_game.xlsx",sep=""))) %>% 
    group_by(Player) %>%
    top_n(n=1,G)
  
  #colnames(df_game[[k]])[-c(2)] <- paste(colnames(df_game[[k]])[-c(2)],df_game[[k]]$Year[[1]], sep="_")
  
  k <- k+1
}

#createing list of dataframes with advanced data
df_adv <- list()
j <- 1

for (i in 2013:2018) {
  nam <- paste("adv", i, sep = "")
  df_adv[[j]] <- assign(nam, read_excel(paste("Data/",i,"_adv.xlsx",sep=""))) %>% 
    group_by(Player) %>% 
    mutate(vorp_total = VORP) %>% 
    top_n(n=1,G)
  #colnames(df_adv[[j]])[-c(1,2)] <- paste(colnames(df_adv[[j]])[-c(1,2)],df_adv[[j]]$Year[[1]], sep="_")
  j <- j+1
}

df_adv_rbdind <- df_adv[[1]]
for(i in 2:length(df_adv)) {
  df_adv_rbdind <- rbind(df_adv_rbdind, df_adv[[i]])
}


# Merge Data --------------------------------------------------------------

df_rookies <- dplyr::inner_join(rookies, df_adv_rbdind, by = c("Player","Year")) %>% 
  select(Player, Tm = Tm.y, Year, Pk, Pos=Pos.y, College, Age=Age.y, vorp_total)

#dont't forget redshirt rookies!
redshirt <- df_adv_rbdind %>% 
  filter(Player %in% subset(rookies,!(rookies$Player %in% df_rookies$Player))$Player) %>% 
  group_by(Player) %>% 
  filter(Year == min(Year))

redshirt_rookies <- dplyr::inner_join(rookies,redshirt, by = c("Player")) %>% 
  select(Player, Tm = Tm.y, Year = Year.y, Pk, Pos=Pos.x, College, Age=Age.y, vorp_total)

df_rookies <- rbind(df_rookies,redshirt_rookies)


# Model -------------------------------------------------------------------


rookie_lm <- lm(data=df_rookies,vorp_total ~ Pk + I(Pk^2) + Age + I(Age^2) + Pos+Tm)
summary(rookie_lm)

df_rookies$vorp_pred <- round(predict(rookie_lm),2)


