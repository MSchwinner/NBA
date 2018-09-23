library(readxl)
library(ggplot2)
#install.packages("dplyr")
library(dplyr)
#install.packages("caret")
library(caret)
library(corrplot)

#createing list of dataframes with game data
df_game <- list()
k <- 1

for (i in 2014:2018) {
  nam <- paste("g", i, sep = "")
  df_game[[k]] <- assign(nam, read_excel(paste("C:/Users/AT0723302/Desktop/NBA/",i,"_game.xlsx",sep=""))) %>% 
    group_by(Player) %>%
    top_n(n=1,G)
  
  #colnames(df_game[[k]])[-c(2)] <- paste(colnames(df_game[[k]])[-c(2)],df_game[[k]]$Year[[1]], sep="_")
  
  k <- k+1
}

#createing list of dataframes with advanced data
df_adv <- list()
j <- 1

for (i in 2014:2018) {
  nam <- paste("adv", i, sep = "")
  df_adv[[j]] <- assign(nam, read_excel(paste("C:/Users/AT0723302/Desktop/NBA/",i,"_adv.xlsx",sep="")) %>% 
                          filter(Tm != "TOT")) %>%  
    group_by(Player) %>% 
    mutate(bpm_total = sum(BPM *MP) / sum(MP)) %>% 
    top_n(n=1,G)
  
  #colnames(df_adv[[j]])[-c(1,2)] <- paste(colnames(df_adv[[j]])[-c(1,2)],df_adv[[j]]$Year[[1]], sep="_")
  j <- j+1
}

#creating merged dataframes by Player (t, t-1, t-2, ...) 
for (i in 4:5) {
  nam <- paste("df", i, sep = "")
  x <- merge(data.frame(Player=df_adv[[i]][,2],bpm_total=df_adv[[i]][,28]), df_game[[i-1]], by="Player")
  x <- merge(x, df_game[[i-2]], by = "Player", all.x=TRUE)
  x <- merge(x, df_game[[i-3]], by = "Player", all.x=TRUE)
  assign(nam, data.frame(x))
}

for (i in 3) {
  nam <- paste("df", i, sep = "")
  x <- merge(data.frame(Player=df_adv[[i]][,2],bpm_total=df_adv[[i]][,28]), df_game[[i-1]], by="Player")
  x <- merge(x, df_game[[i-2]], by = "Player", all.x=TRUE)
  assign(nam, data.frame(x))
}

# for (i in 2) {
#   nam <- paste("df", i, sep = "")
#   x <- merge(data.frame(Player=df_adv[[i]][,2],bpm_total=df_adv[[i]][,28]), df_game[[i-1]], by="Player")
#   assign(nam, data.frame(x))
# }

# bind merged dataframes together 
df <- bind_rows(df5,df4,df3) %>% 
  subset(!is.na(Year.x))

# correlations of numeroc variables
df_num <- df[,c(2,5,7:31)]
df_num <- select_if(df, is.numeric)
df_num[is.na(df_num)] <- 0

cor_num <- cor(df_num)
corrplot(cor_num, order = "hclust")

# density bpm
ggplot(df_num)+
  geom_density(aes(x = bpm_total), fill = "black", alpha = 0.4)+
  geom_vline(aes(xintercept = mean(bpm_total)), linetype = "dashed")+
  scale_x_continuous(limits = c(-10,10))+
  theme_bw()

# relationship bpm & age(t-1)
ggplot(df_num, aes(x=Age.x, y=bpm_total))+
  geom_boxplot(aes(group = Age.x))+
  geom_smooth(se=FALSE)+
  scale_y_continuous(limits = c(-10,10))+
  theme_bw()

# relationship bpm & MP(t-1) & Player traded 
df$trade <- ifelse(df$Tm.x == "TOT", "Trade", "NoTrade")
ggplot(df, aes(x = MP.x, y = bpm_total))+
  geom_point(aes(color = trade), size = 3)+
  geom_smooth(se=FALSE, aes(color = trade))+
  scale_y_continuous(limits = c(-10,10))+
  theme_bw()

# relationship bpm & STL(t-1)
ggplot(df, aes(x = STL.x, y = bpm_total))+
  geom_boxplot(aes(group = STL.x))+
  geom_smooth(se=FALSE)+
  scale_y_continuous(limits = c(-10,10))+
  theme_bw()

# relationship bpm & DRB (t-1)
ggplot(df, aes(x = DRB.x, y = bpm_total))+
  geom_point()+
  geom_smooth(se=FALSE)+
  scale_y_continuous(limits = c(-10,10))+
  theme_bw()


############
g18 <- read_excel("C:/Users/AT0723302/Desktop/NBA/2018_game.xlsx") %>% 
  filter(Tm != "TOT") %>% 
  group_by(Player) %>% 
  top_n(n=1)

g17 <- read_excel("C:/Users/AT0723302/Desktop/NBA/2017_game.xlsx") %>% 
  filter(Tm != "TOT")


adv18 <- read_excel("C:/Users/AT0723302/Desktop/NBA/2018_adv.xlsx") %>% 
  filter(Tm != "TOT") %>% 
  group_by(Player) %>% 
  mutate(bpm_total = sum(BPM *MP) / sum(MP))



df2018 <- merge(g17, adv18[,c(2,28)], by = c("Player"))
str(adv18)
test2 <- test2[,-c(1)]

summary(lm(data=test2, bpm_total_2018 ~.))

ggplot(filter(df2018, G > 10), aes(x=MP,y=bpm_total))+
  geom_point(aes(color=factor(Age)), size = 3)+
  geom_smooth()+
  scale_y_continuous(limits = c(-15,15))+
  theme_bw()

ggplot(filter(df2018, G > 10 & Age < 30 & bpm_total > -10), aes(bpm_total))+
         geom_density(aes(fill = factor(Age)), alpha = 0.4)+
         theme_bw()
       