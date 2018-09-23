
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

# bind merged dataframes together 
df <- bind_rows(df6,df5,df4) %>% 
  subset(!is.na(Year.x))

df <- dplyr::rename(df, Player = Player, vorp_total = vorp_total, Year_t1 = Year.x, Pos_t1 = Pos.x,
             Age_t1 = Age.x, Tm_t1 = Tm.x, G_t1 = G.x, GS_t1 = GS.x, MP_t1 = MP.x, FG_t1 = FG.x,
             FGA_t1 = FGA.x, FGPer_t1 = FG..x, FG3_t1 = X3P.x, FGA3_t1 = X3PA.x, FG3Per_t1 = X3P..x,
             FG2_t1 = X2P.x, FGA2_t1 = X2PA.x, FG2Per_t1 = X2P..x, eFG_t1 = eFG..x, FT_t1 = FT.x,
             FTA_t1 = FTA.x, FTPer_t1 = FT..x, ORB_t1 = ORB.x, DRB_t1 = DRB.x, TRB_t1 = TRB.x, 
             AST_t1 = AST.x, STL_t1 = STL.x, BLK_t1 = BLK.x, TOV_t1 = TOV.x, PF_t1 = PF.x,
             PSG_t1 = PS.G.x,
             Year_t2 = Year.y, Pos_t2 = Pos.y,
             Age_t2 = Age.y, Tm_t2 = Tm.y, G_t2 = G.y, GS_t2 = GS.y, MP_t2 = MP.y, FG_t2 = FG.y,
             FGA_t2 = FGA.y, FGPer_t2 = FG..y, FG3_t2 = X3P.y, FGA3_t2 = X3PA.y, FG3Per_t2 = X3P..y,
             FG2_t2 = X2P.y, FGA2_t2 = X2PA.y, FG2Per_t2 = X2P..y, eFG_t2 = eFG..y, FT_t2 = FT.y,
             FTA_t2 = FTA.y, FTPer_t2 = FT..y, ORB_t2 = ORB.y, DRB_t2 = DRB.y, TRB_t2 = TRB.y, 
             AST_t2 = AST.y, STL_t2 = STL.y, BLK_t2 = BLK.y, TOV_t2 = TOV.y, PF_t2 = PF.y,
             PSG_t2 = PS.G.y,
             Year_t3 = Year, Pos_t3 = Pos,
             Age_t3 = Age, Tm_t3 = Tm, G_t3 = G, GS_t3 = GS, MP_t3 = MP, FG_t3 = FG,
             FGA_t3 = FGA, FGPer_t3 = FG., FG3_t3 = X3P, FGA3_t3 = X3PA, FG3Per_t3 = X3P.,
             FG2_t3 = X2P, FGA2_t3 = X2PA, FG2Per_t3 = X2P., eFG_t3 = eFG., FT_t3  = FT,
             FTA_t3 = FTA, FTPer_t3 = FT., ORB_t3 = ORB, DRB_t3 = DRB, TRB_t3 = TRB, 
             AST_t3 = AST, STL_t3 = STL, BLK_t3 = BLK, TOV_t3 = TOV, PF_t3 = PF,
             PSG_t3 = PS.G,
             PER_t1 = PER.x, TS_t1 = TS..x, Ar3P_t1 = X3PAr.x, FTr_t1 = FTr.x, ORBPer_t1 = ORB..x,
             DRBPer_t1 = DRB..x, TRBPer_t1 = TRB..x, ASTPer_t1 = AST..x, STLPer_t1 = STL..x, 
             BLKPer_t1 = BLK..x, TOVPer_t1 = TOV..x, USG_t1 = USG..x, OWS_t1 = OWS.x, DWS_t1 = DWS.x,
             WS_t1 = WS.x, WS48_t1 = WS.48.x, OBPM_t1 = OBPM.x, DBPM_t1 = DBPM.x, BPM_t1 = BPM.x, 
             VORP_t1 = VORP.x,
             PER_t2 = PER.y, TS_t2 = TS..y, Ar3P_t2 = X3PAr.y, FTr_t2 = FTr.y, ORBPer_t2 = ORB..y,
             DRBPer_t2 = DRB..y, TRBPer_t2 = TRB..y, ASTPer_t2 = AST..y, STLPer_t2 = STL..y, 
             BLKPer_t2 = BLK..y, TOVPer_t2 = TOV..y, USG_t2 = USG..y, OWS_t2 = OWS.y, DWS_t2 = DWS.y,
             WS_t2 = WS.y, WS48_t2 = WS.48.y, OBPM_t2 = OBPM.y, DBPM_t2 = DBPM.y, BPM_t2 = BPM.y, 
             VORP_t2 = VORP.y,
             PER_t3 = PER, TS_t3 = TS., Ar3P_t3 = X3PAr, FTr_t3 = FTr, ORBPer_t3 = ORB.,
             DRBPer_t3 = DRB., TRBPer_t3 = TRB., ASTPer_t3 = AST., STLPer_t3 = STL., 
             BLKPer_t3 = BLK., TOVPer_t3 = TOV., USG_t3 = USG., OWS_t3 = OWS, DWS_t3 = DWS,
             WS_t3 = WS, WS48_t3 = WS.48, OBPM_t3 = OBPM, DBPM_t3 = DBPM, BPM_t3 = BPM, 
             VORP_t3 = VORP)

# Clean Up ----------------------------------------------------------------
#df <- df[-which(df$bpm_total < -45),]

rm(list=setdiff(ls(), "df")) #remove all objects except for df
gc()
