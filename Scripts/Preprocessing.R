#skimr::skim(df)

# Seperate Data -----------------------------------------------------------
# Character/Factor Variables
df_fac <- select_if(df, is.character) %>% 
  select(-contains("Player")) #drop Player ID

df_fac <- cbind(df_fac, select(df, contains("Year"))) #add Years to Factor Variables

# Numeric Variables
df_num <- select_if(df, is.numeric) %>%  
  select(-contains("Year")) #drop all year variables

# Factor Variables --------------------------------------------------------

df_fac[df_fac=="NOH"] <- "NOP"  #Name Change New Orleans
df_fac[df_fac =="CHA"] <- "CHO" #Name Change Charlotte

df_fac <- data.frame(lapply(df_fac, as.factor)) #transform variables into factors

#unique(df_fac_imputed$Tm)
#no dummy variables for now 

# Missing Values ----------------------------------------------------------

y <- df_num[,c(1)] # seperate target variable
df_num <- df_num[,-c(1)]

df_num <- transform(df_num, Age_t2 = ifelse(!is.na(Age_t2), Age_t2, Age_t1 - 1)) # impute age variables
df_num <- transform(df_num, Age_t3 = ifelse(!is.na(Age_t3), Age_t3, Age_t1 - 2))

## Impute numerical variables
# 1. by knn -> data is centered and scaled in the process
preProcValues <- preProcess(df_num, method = c("knnImpute")) 
df_num_knn <- predict(preProcValues, newdata= df_num, na.action = na.pass) 

# 2. by mean
NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
df_num_mean <- lapply(df_num, NA2mean)

df_num_mean <- data.frame(df_num_mean)

## Impute factor variables

df_fac_imputed <- df_fac

df_fac_imputed <- transform(df_fac_imputed, Pos_t2 = ifelse(!is.na(Pos_t2), Pos_t2, Pos_t1))
df_fac_imputed <- transform(df_fac_imputed, Pos_t3 = ifelse(!is.na(Pos_t3), Pos_t3, Pos_t2))
df_fac_imputed <- transform(df_fac_imputed, Tm_t2 = ifelse(!is.na(Tm_t2), Tm_t2, Tm_t1))
df_fac_imputed <- transform(df_fac_imputed, Tm_t3 = ifelse(!is.na(Tm_t3), Tm_t3, Tm_t2))
df_fac_imputed <- transform(df_fac_imputed, Year_t2 = ifelse(!is.na(Year_t2), Year_t2, as.numeric(Year_t1) - 1))
df_fac_imputed <- transform(df_fac_imputed, Year_t3 = ifelse(!is.na(Year_t3), Year_t3, as.numeric(Year_t2) - 1))

df_fac_imputed <- data.frame(lapply(df_fac_imputed, as.factor)) #transform characters into factors

skim(df_num_mean)
skim(df_fac_imputed)


# Correlation -------------------------------------------------------------

# correlations of numeric variables

cor_num <- cor(df_num_mean)
#corrplot(cor_num, order = "hclust")

#Delete suggested variables with high correlation with other features
cor_high <- findCorrelation(cor_num, cutoff = .95)  #findCorrelation function of caret to search correlation matrix
names(df_num_mean[,cor_high]) # deleted variables due to collinearity

df_num_mean_filtered <- df_num_mean[, -cor_high]
df_num_knn_filtered <- df_num_knn[, -cor_high]

# Skewness ----------------------------------------------------------------

#skewValues <- apply(df_num_mean_filtered, MARGIN=2, skewness) 
#summary(skewValues)
#skewValues

# no transformations for now

# Data Splitting ----------------------------------------------------------

## create dataframes for independent variables (x)
x_mean <- cbind(df_num_mean_filtered, df_fac_imputed)
x_knn <- cbind(df_num_knn_filtered, df_fac_imputed)
  
#create list of training obs. with createDataPartition function of caret (alternative sample())
set.seed(1234)
trainingRow <- createDataPartition(y, 
                                   p=.85, #specifies percentage of obs. that get allocated
                                   list = FALSE) #stores #obs as a vector, not a list
#create training dataset
x_mean_train <- x_mean[trainingRow, ]
x_knn_train <- x_knn[trainingRow, ]
y_train <- y[trainingRow]

#create test dataset
x_mean_test<- x_mean[-trainingRow, ]
x_knn_test <- x_knn[-trainingRow, ]
y_test <- y[-trainingRow]

#df dataset
df_pred <- df[-trainingRow, ]

# Clean up ----------------------------------------------------------------

rm(list=setdiff(ls(), c("df", "trainingRow", "x_mean_train", "x_knn_train",
                        "y_train", "x_mean_test", "x_knn_test", "y_test",
                        "df_pred"))) 
gc()


