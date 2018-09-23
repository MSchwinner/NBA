# Random Forest -----------------------------------------------------------

## 1. with mean imputed data

#use mtry = 1/3 of predictors -> default for regressions
rfGrid <- expand.grid(
    mtry = length(x_mean_train)/3, 
    splitrule = "variance",
    min.node.size = 5
  )

set.seed(1234)
rf <- train(x = x_mean_train, y = y_train,
            method = "ranger",
            num.trees = 500,
            tuneGrid = rfGrid,
            trControl = trainControl(method = "cv",
                                     verboseIter = TRUE),
            importance = "permutation")

#plot(rf)


## 2. with knn imputed data

set.seed(1234)
rf_knn <- train(x = x_knn_train, y = y_train,
            method = "ranger",
            num.trees = 500,
            tuneGrid = rfGrid,
            trControl = trainControl(method = "cv",
                                     verboseIter = TRUE),
            importance = "permutation")

## compare rf models
rf #train RMSE 0,890; R2 = 0.616
rf_knn #train RMSE 0.889; R2 = 0.618

rfPred <- predict(rf, x_mean_test) #predict yhat for test set
rfRMSE <- sqrt(mean((y_test-rfPred)^2))  
rfRMSE #test RMSE 0.804

rfPred_knn <- predict(rf_knn, x_knn_test) #predict yhat for test set
rfRMSE_knn <- sqrt(mean((y_test-rfPred_knn)^2))  
rfRMSE_knn #test RMSE 0.794

#testing Mean Absolute Error
(rfMAE_mean <- mean(abs(y_test-rfPred))) 
(rfMAE_knn <- mean(abs(y_test-rfPred_knn))) 

plot(varImp(rf), top = 40)
plot(varImp(rf_knn), top = 40)

# ## linear regression
# set.seed(1234)
# lm <- train(x = x_trans_train, y = y_trans_train,
#             method = "lm",
#             trControl = trainControl(method = "cv",
#                                      verboseIter = TRUE),
#             importance = "permutation")
# 
# lm #train RMSE 3.05; R2 0.32
# summary(lm)
# 
# lmPred <- predict(lm, x_trans_test) #predict yhat for test set
# lmRMSE <- sqrt(mean((y_trans_test-lmPred)^2))  
# lmRMSE #test RMSE 3.0

#plot(varImp(rf, scale = TRUE), top = 20) #plot top20 

# xyplot(y_trans_test ~ rfPred,#plotting predicted vs. observed
#        type = c("p","g"), #"p" points and "g" grid
#        panel = function(x, y) {
#          panel.xyplot(x, y)
#          panel.abline(a=0, b=1)
#        }, #add 45 degree line
#        xlab = "Predicted", ylab = "Observed") 


predictions <- data.frame(Player = df_pred$Player,
                          Year = df_pred$Year_t1+1,
                          y = y_test,
                          y_rf_mean = rfPred,
                          y_rf_knn = rfPred_knn) %>% 
  mutate(error_rf_mean = y_rf_mean - y,
         error_rf_knn = y_rf_knn - y) %>% 
  mutate_if(is.factor, as.character)

predictions_training <- data.frame(Player = df[trainingRow, ]$Player,
                                   Year = df[trainingRow, ]$Year_t1+1,
                                   y = y_train,
                                   y_rf_knn = predict(rf_knn)) %>% 
  mutate(error_rf = y_rf_knn - y)

# EDA ---------------------------------------------------------------------

# compare the two impute strategies
ggplot(predictions, aes(x=y_rf_mean, y = y)) +
  geom_point(aes(color = "impute mean"))+
  geom_point(data = predictions, aes(x=y_rf_knn, y = y, color = "impute knn"))+
  geom_abline(linetype = 2)+
  # geom_text(aes(label = ifelse(error_rf > 1.5, paste(Player, Year, sep = " "), "")), vjust = -1, size = 3, color = "red")+
  # geom_text(aes(label = ifelse(error_rf < -1.5, paste(Player, Year, sep = " "), "")), vjust = -1, size = 3, color = "green")+
  theme_bw()

# visualize errors in test set
ggplot(predictions, aes(x=y_rf_knn, y = y)) +
  geom_jitter()+
  geom_abline(linetype = 2)+
  geom_text(aes(label = ifelse(error_rf_knn > 1.8, paste(Player, Year, sep = " "), "")), vjust = -1, size = 3, color = "red")+
  geom_text(aes(label = ifelse(error_rf_knn < -1.8, paste(Player, Year, sep = " "), "")), vjust = -1, size = 3, color = "darkgreen")+
  theme_bw()

# visualize errors in training set
ggplot(predictions_training, aes(x=y_rf_knn, y = y)) +
  geom_jitter()+
  geom_abline(linetype = 2)+
  geom_text(aes(label = ifelse(error_rf > 1.2, paste(Player, Year, sep = " "), "")), vjust = -1, size = 3, color = "red")+
  geom_text(aes(label = ifelse(error_rf < -1.2, paste(Player, Year, sep = " "), "")), vjust = -1, size = 3, color = "darkgreen")+
  theme_bw()
