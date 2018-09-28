# Random Forest -----------------------------------------------------------

## 1. with mean imputed data

#use mtry = 1/3 of predictors -> default for regressions
rfGrid <- expand.grid(
    mtry = round(length(x_mean_train)/3-5,0),
    splitrule = "variance",
    min.node.size = 5
  )

# rfGrid <- expand.grid(
#   mtry = seq(round(length(x_mean_train)/3-5,0), round(length(x_mean_train)/3+5,0),
#              by = 2),
#   splitrule = "variance",
#   min.node.size = 5
# )

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

# set.seed(1234)
# rf_knn <- train(x = x_knn_train, y = y_train,
#             method = "ranger",
#             num.trees = 500,
#             tuneGrid = rfGrid,
#             trControl = trainControl(method = "cv",
#                                      verboseIter = TRUE),
#             importance = "permutation")

## compare rf models
# rf #train RMSE 0.853; R2 = 0.647
# rf_knn #train RMSE 0.858; R2 = 0.643
# 
rfPred <- predict(rf, x_mean_test) #predict yhat for test set
# rfRMSE <- sqrt(mean((y_test-rfPred)^2))  
# rfRMSE #test RMSE 0.916
# 
# rfPred_knn <- predict(rf_knn, x_knn_test) #predict yhat for test set
# rfRMSE_knn <- sqrt(mean((y_test-rfPred_knn)^2))  
# rfRMSE_knn #test RMSE 0.909
# 
# plot(varImp(rf), top = 40)
# plot(varImp(rf_knn), top = 40)

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

