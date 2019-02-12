# KFOLD CROSS VALIDATION ON SWISS DATA with caret package
library(caret)

swiss <- swiss

# Firstly Split the data in Train and Test (Validation Dataset)
index = createDataPartition(swiss$Fertility , p= 0.75 , list = F)
train = swiss[index,]
test = swiss[-index,]

# Creating a Linear Regression model 

model_lm = lm(Fertility~. , data = swiss)
pred_lm = predict(model_lm , test)

#Running the library of Metrics to calculate Rsquare , RMSE and MAE of our model

library(Metrics)
data.frame(Rsquare = R2(pred_lm , test$Fertility),
           Rmse = RMSE(pred_lm , test$Fertility),
           MAE =MAE(pred_lm , test$Fertility))

#Rsquare     Rmse      MAE
# 0.778568 6.978066 5.879149

# Now using Train function from caret package to create models

# Decision tree model
controls = trainControl(method = "cv" , number = 5)
model_dtree = train(Fertility~. , data= swiss , trControl = controls ,method = "rpart")
pred_dtree = predict(model_dtree , test)

data.frame(Rsquare = R2(pred_dtree , test$Fertility),
           Rmse = RMSE(pred_dtree , test$Fertility),
           MAE =MAE(pred_dtree , test$Fertility))

#Rsquare     Rmse      MAE
# 0.3842428 10.36812 9.267532


# Treebag model
controls = trainControl(method = "cv" , number = 5)
model_treebag = train(Fertility~. , data= swiss , trControl = controls ,method = "treebag")
pred_treebag = predict(model_treebag , test)

data.frame(Rsquare = R2(pred_treebag , test$Fertility),
           Rmse = RMSE(pred_treebag , test$Fertility),
           MAE =MAE(pred_treebag , test$Fertility))

#Rsquare     Rmse      MAE
#0.6991781 7.58551 5.576832


# Random forest model
controls = trainControl(method = "cv" , number = 5)
model_rf = train(Fertility~. , data= swiss , trControl = controls ,method = "rf")
pred_rf = predict(model_rf , test)

data.frame(Rsquare = R2(pred_rf , test$Fertility),
           Rmse = RMSE(pred_rf , test$Fertility),
           MAE =MAE(pred_rf , test$Fertility))

# Rsquare     Rmse     MAE
# 0.9534032 3.979613 2.92893


# Cforest model
controls = trainControl(method = "cv" , number = 5)
model_cf = train(Fertility~. , data= swiss , trControl = controls ,method = "cforest")
pred_cf = predict(model_cf , test)

data.frame(Rsquare = R2(pred_cf , test$Fertility),
           Rmse = RMSE(pred_cf , test$Fertility),
           MAE =MAE(pred_cf , test$Fertility))

#Rsquare     Rmse      MAE
# 0.7216128 7.919484 6.094118



# KFOLD CROSS VALIDATION ON MTCARS DATA
mtcars <-  mtcars

index_mt <-  createDataPartition(mtcars$mpg , p= 0.75 ,list = F )
train <-  mtcars[index_mt,]
test <-  mtcars[-index_mt,]

#Linear regression model

control = trainControl( method = "cv" , number = 5)
model_lm = train(mpg~. , data= train , trControl = control , method = "lm")
pred_lm = predict(model_lm , test)

data.frame(Rsquare = R2(pred_lm , test$mpg),
           RMSE = RMSE(pred_lm , test$mpg),
           MAE = MAE(pred_lm , test$mpg))

# Rsquare     RMSE      MAE
# 0.8750577 3.356217 2.762894


#Decision tree model

control = trainControl( method = "cv" , number = 5)
model_dtree = train(mpg~. , data= train , trControl = control , method = "rpart")
pred_dtree = predict(model_dtree , test)

data.frame(Rsquare = R2(pred_dtree , test$mpg),
           RMSE = RMSE(pred_dtree , test$mpg),
           MAE = MAE(pred_dtree , test$mpg))

# Rsquare     RMSE      MAE
#0.6881965 4.718187 4.058571


#Random Forest model

control = trainControl( method = "cv" , number = 5)
model_rf = train(mpg~. , data= train , trControl = control , method = "rf")
pred_rf = predict(model_rf , test)

data.frame(Rsquare = R2(pred_rf , test$mpg),
           RMSE = RMSE(pred_rf , test$mpg),
           MAE = MAE(pred_rf , test$mpg))

# Rsquare     RMSE      MAE
# 0.8783189 3.595736 3.172302


#Treebag model

control = trainControl( method = "cv" , number = 5)
model_bag = train(mpg~. , data= train , trControl = control , method = "treebag")
pred_bag = predict(model_bag , test)

data.frame(Rsquare = R2(pred_bag , test$mpg),
           RMSE = RMSE(pred_bag , test$mpg),
           MAE = MAE(pred_bag , test$mpg))

# Rsquare     RMSE      MAE
#0.8093678 4.493403 3.592849

# Cforest model

control = trainControl( method = "cv" , number = 5)
model_cf = train(mpg~. , data= train , trControl = control , method = "cforest")
pred_cf = predict(model_cf , test)

data.frame(Rsquare = R2(pred_cf , test$mpg),
           RMSE = RMSE(pred_cf , test$mpg),
           MAE = MAE(pred_cf , test$mpg))

# Rsquare     RMSE      MAE
#0.7935725 4.822219 3.798916

# Plot the CForest prediction 
library(animation)
cv.ani(pred_cf , k = 5, col = c("green", "red", "blue"), pch = c(4, 1))























































