
# Prepare Data ------------------------------------------------------------

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
  labs(y = "y", x = "y_hat")+
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
