df_adv <- list()
j <- 1
for (i in 2013:2018) {
  nam <- paste("adv", i, sep = "")
  df_adv[[j]] <- assign(nam, read_excel(paste("Data/",i,"_adv.xlsx",sep="")) %>% 
  subset(., Tm != "TOT") %>% 
    group_by(Tm) %>% 
    dplyr::summarize(max_vorp = max(VORP),
              min_vorp = min(VORP),
              mean_vorp = mean(VORP),
              median_vorp = median(VORP),
              vorp = sum(VORP)
              ))
  #colnames(df_adv[[j]])[-c(1,2)] <- paste(colnames(df_adv[[j]])[-c(1,2)],df_adv[[j]]$Year[[1]], sep="_")
  j <- j+1
}

df_adv[[1]]$Tm[which(df_adv[[1]]$Tm == "NOH")] <- "NOP"
df_adv[[1]]$Tm[which(df_adv[[1]]$Tm == "CHA")] <- "CHO"
df_adv[[2]]$Tm[which(df_adv[[2]]$Tm == "CHA")] <- "CHO"

teams <- read_excel("Data/Team_Performance.xlsx", sheet = 2) %>% 
  mutate_if(is.character, as.factor)

skim(teams)

j <- 1
for (i in unique(teams$Year)) {
  nam <- paste("teams",i,sep="")
  assign(nam, merge(teams[which(teams$Year == i),], 
                   df_adv[[j]], by.x = "Team", by.y = "Tm"))
  j <- j+1
}

teams_all <- rbind(teams2013,teams2014,teams2015,teams2016,teams2017,teams2018)

lm_teams <- lm(Wins ~ vorp + Team,teams_all)

remove(teams2013,teams2014,teams2015,teams2016,teams2017,teams2018,teams, df_adv,
       adv2013,adv2014,adv2015,adv2016,adv2017,adv2018)
