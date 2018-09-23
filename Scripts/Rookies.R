# Data Import -------------------------------------------------------------

rookies <- read.csv("Data/NBA_Draft_1980_2017.csv")
rookies$player <- as.character(rookies$player)
rookies$year <- as.integer(rookies$year)

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

df_adv_rbdind$Player_last <- strsplit(df_adv_rbdind$Player, " ")[[1]][2]

df_rookies <- merge(rookies, df_adv_rbdind, by.x = c("player"),
                    by.y = c("Player_last"))
  select(Player, year, pick, Pos, Age, vorp_total)
