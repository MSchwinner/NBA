
# Seperate Data -----------------------------------------------------------
# Character/Factor Variables
df_fac_2019 <- select_if(df, is.character) %>% 
  select(-contains("Player")) #drop Player ID

df_fac_2019 <- cbind(df_fac_2019, select(df, contains("Year"))) #add Years to Factor Variables

# Numeric Variables
df_num_2019 <- select_if(df, is.numeric) %>%  
  select(-contains("Year")) #drop all year variables

# Factor Variables --------------------------------------------------------

df_fac_2019[df_fac_2019=="NOH"] <- "NOP"  #Name Change New Orleans
df_fac_2019[df_fac_2019 =="CHA"] <- "CHO" #Name Change Charlotte

#df_fac <- data.frame(lapply(df_fac, as.factor)) #transform variables into factors

#unique(df_fac_imputed$Tm)
#no dummy variables for now 

# Missing Values ----------------------------------------------------------

y <- df_num_2019[,c(1)] # seperate target variable
df_num_2019 <- df_num_2019[,-c(1)]

df_num_2019 <- transform(df_num_2019, Age_t2 = ifelse(!is.na(Age_t2), Age_t2, Age_t1 - 1)) # impute age variables
df_num_2019 <- transform(df_num_2019, Age_t3 = ifelse(!is.na(Age_t3), Age_t3, Age_t1 - 2))

## Impute numerical variables
# 1. by knn -> data is centered and scaled in the process
preProcValues <- preProcess(df_num_2019, method = c("knnImpute")) 
df_num_knn_2019 <- predict(preProcValues, newdata= df_num_2019, na.action = na.pass) 

# 2. by mean
NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
df_num_mean_2019 <- lapply(df_num_2019, NA2mean)

df_num_mean_2019 <- data.frame(df_num_mean_2019)

## Impute factor variables

df_fac_imputed_2019 <- df_fac_2019

df_fac_imputed_2019 <- transform(df_fac_imputed_2019, Pos_t2 = ifelse(!is.na(Pos_t2), Pos_t2, Pos_t1))
df_fac_imputed_2019 <- transform(df_fac_imputed_2019, Pos_t3 = ifelse(!is.na(Pos_t3), Pos_t3, Pos_t2))
df_fac_imputed_2019 <- transform(df_fac_imputed_2019, Tm_t2 = ifelse(!is.na(Tm_t2), Tm_t2, Tm_t1))
df_fac_imputed_2019 <- transform(df_fac_imputed_2019, Tm_t3 = ifelse(!is.na(Tm_t3), Tm_t3, Tm_t2))
df_fac_imputed_2019 <- transform(df_fac_imputed_2019, Year_t2 = ifelse(!is.na(Year_t2), Year_t2, as.numeric(Year_t1) - 1))
df_fac_imputed_2019 <- transform(df_fac_imputed_2019, Year_t3 = ifelse(!is.na(Year_t3), Year_t3, as.numeric(Year_t2) - 1))

df_fac_imputed_2019 <- data.frame(lapply(df_fac_imputed_2019, as.factor)) #transform characters into factors

skim(df_num_mean_2019)
skim(df_fac_imputed_2019)


# Correlation -------------------------------------------------------------

# correlations of numeric variables

#cor_num <- cor(df_num_mean)
#corrplot(cor_num, order = "hclust")

#Delete suggested variables with high correlation with other features
#cor_high <- findCorrelation(cor_num, cutoff = .95)  #findCorrelation function of carret to search correlation matrix
#names(df_num_mean[,cor_high]) # deleted variables due to collinearity

# df_num_mean_filtered_2019 <- df_num_mean[, -cor_high]
# df_num_knn_filtered_2019  <- df_num_knn[, -cor_high]

# Skewness ----------------------------------------------------------------

#skewValues <- apply(df_num_mean_filtered, MARGIN=2, skewness) 
#summary(skewValues)
#skewValues

# no transformations for now

# Data Splitting ----------------------------------------------------------

## create dataframes for independent variables (x)
x_mean_2019 <- cbind(df_num_mean_2019, df_fac_imputed_2019)
x_knn_2019 <- cbind(df_num_knn_2019, df_fac_imputed_2019)

#create list of training obs. with createDataPartition function of caret (alternative sample())
row_player2019 <- which(grepl(2018, x_mean_2019$Year_t1))

x_mean_2019 <- x_mean_2019 %>% 
  select(-c(Year_t1,Year_t2,Year_t3))

x_knn_2019 <- x_knn_2019 %>% 
  select(-c(Year_t1,Year_t2,Year_t3))

player2019 <- x_mean_2019[row_player2019, ] 
player2019 <- player2019[,names(x_mean)]

#y <- y[-row_player2019]
# x_mean_2019 <- x_mean_2019[-row_player2019,]
# x_knn_2019 <- x_knn_2019[-row_player2019,]

# set.seed(1234)
# trainingRow <- createDataPartition(y, 
#                                    p=.85, #specifies percentage of obs. that get allocated
#                                    list = FALSE) #stores #obs as a vector, not a list
# 
# #create training dataset
# x_mean_train <- x_mean[trainingRow, ]
# x_knn_train <- x_knn[trainingRow, ]
# y_train <- y[trainingRow]
# 
# #create test dataset
# x_mean_test<- x_mean[-trainingRow, ]
# x_knn_test <- x_knn[-trainingRow, ]
# y_test <- y[-trainingRow]
# 
# #df dataset
# df_pred <- df[-trainingRow, ]
# df_pred2019 <- df[row_player2019, ]
# 
# # Clean up ----------------------------------------------------------------
# 
# # rm(list=setdiff(ls(), c("df", "player2019", "testinggRow", "x_mean_train", "x_knn_train",
# #                         "y_train", "x_mean_test", "x_knn_test", "y_test",
# #                         "df_pred", "df_pred2019", "lm_teams"))) 
# 
