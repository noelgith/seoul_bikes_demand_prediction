library(tidyverse)
library(glmnet)
library(leaps)
library(gridExtra)
library(coefplot)

#import dataset
df <- read.csv('E:/fall2021/stat515/final project/SeoulBikeData.csv')

#correlation plot
corrplot(cor(select_if(df, is.numeric)),type='upper', diag=FALSE,, tl.srt=45)
cor(select(df,Rented.Bike.Count,Temperature..C.,Hour,Visibility..10m.,Rainfall.mm.,Snowfall..cm.))

analysis <- select(df,Rented.Bike.Count,Temperature..C.,Hour,Visibility..10m.,Rainfall.mm.,Snowfall..cm.)

set.seed(301)
test <- sample(1:nrow(analysis), size=nrow(analysis)*0.3)
train <- (-test)
# Need train and test response data for later MSE calculations
Rshare.train <- analysis$Rented.Bike.Count[train]
Rshare.test <- analysis$Rented.Bike.Count[test]
# 3. Develop a regression model for Rshare that uses all predictors-----
model.all <- lm(Rented.Bike.Count~., data=analysis[train,])
s.all <- summary(model.all)
names(s.all)

# training-set adjusted R-squared and residual standard error
s.all$adj.r.squared

cat("training-set RMSE: ", s.all$sigma)

# test-set RMSE
fit.test <- predict(model.all, newdata=analysis[test,])
cat("test-set RMSE: ", sqrt(mean((Rshare.test - fit.test)^2)))

# number of estimated coefficients with p_value < 0.05
coeffs <- s.all$coefficients
pvalues <- coeffs[,4]
nrow(coeffs[pvalues < 0.05,])
