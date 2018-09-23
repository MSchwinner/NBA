# Data Import -------------------------------------------------------------

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


# Wrangling ---------------------------------------------------------------

#creating merged dataframes by Player (t, t-1, t-2, ...) 
for (i in 4:6) {
  nam <- paste("df", i, sep = "")
  x <- merge(data.frame(Player=df_adv[[i]][,2],bpm_total=df_adv[[i]][,28]), df_game[[i-1]], by="Player")
  x <- merge(x, df_game[[i-2]], by = "Player", all.x=TRUE)
  x <- merge(x, df_game[[i-3]], by = "Player", all.x=TRUE)
  x <- merge(x, df_adv[[i-1]][, - c(1,3:7,length(df_adv[[i-1]]))], by = "Player", all.x=TRUE)
  x <- merge(x, df_adv[[i-2]][, - c(1,3:7,length(df_adv[[i-2]]))], by = "Player", all.x=TRUE)
  x <- merge(x, df_adv[[i-3]][, - c(1,3:7,length(df_adv[[i-3]]))], by = "Player", all.x=TRUE)
  assign(nam, data.frame(x))
}

# for (i in 3) {
#   nam <- paste("df", i, sep = "")
#   x <- merge(data.frame(Player=df_adv[[i]][,2],bpm_total=df_adv[[i]][,28]), df_game[[i-1]], by="Player")
#   x <- merge(x, df_game[[i-2]], by = "Player", all.x=TRUE)
#   assign(nam, data.frame(x))
# }

skim(df6)

# bind merged dataframes together 
df_rookies <- bind_rows(df6,df5,df4) %>% 
  subset(is.na(Year.x))