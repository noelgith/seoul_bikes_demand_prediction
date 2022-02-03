library(tidyverse)
library(rpart)
library(corrplot)
library(GGally)
library(rpart) 
library(randomForest) 
library(rpart.plot)
library(DMwR2)

seoul_df <- read.csv('E:/fall2021/stat515/final project/SeoulBikeData.csv')
# Regression Tree Example

str(seoul_df)

seoul_df$Holiday <- as.factor(seoul_df$Holiday)
seoul_df$Functioning.Day <- as.factor(seoul_df$Functioning.Day)
seoul_df$Seasons <- as.factor(seoul_df$Seasons)
seoul_df <- seoul_df[-c(1)]

seoul_df$Rented.Bike.Count <- as.numeric(seoul_df$Rented.Bike.Count)

seoul_df_.numeric <- select_if(seoul_df, is.numeric)
corrplot(cor(seoul_df_.numeric),type="upper", method="color", diag=FALSE,
         tl.srt=45, addCoef.col="black")

GGally::ggpairs(seoul_df_.numeric,
                lower=list(continuous='blank',
                           combo='blank', 
                           discrete='blank'),
                upper=list(continuous="points",
                           combo="facethist", discrete="facetbar"),
                switch="y")
set.seed(437)
# grow tree
rpart.seoul = rpart(Rented.Bike.Count~.,data = seoul_df, method="anova", cp=0.0001) 
plotcp(rpart.seoul)
rpart.seoul.1se <- prune(rpart.seoul, cp = 0.0001)
# Create plot of 1se tree
rpart.plot(rpart.seoul.1se , extra=1, roundint=FALSE, 
           digits=3, main="1-se Boston regression
tree")
printcp(rpart.seoul.1se) # display the results

# 2.3. Use a validation set to estimate test-set MSE
set.seed(1)
train = sample(1:nrow(seoul_df), nrow(seoul_df)*0.60)
set.seed(4852)
rpart.seoul.train = rpart(Rented.Bike.Count~., data = seoul_df[train,],
                           method="anova", cp=0.0001) 
rpart.seoul.train = prune(rpart.seoul.train, cp=0.0001)
yhat=predict(rpart.seoul.train, newdata = seoul_df[-train,]) 
seoul.test = seoul_df[-train,"Rented.Bike.Count"]
(MSE = mean((yhat-seoul.test)^2)) 
## [1] 33.64095
sqrt(MSE)
## [1] 5.800082

set.seed(123) 
bag.seoul=randomForest(Rented.Bike.Count~., data=seoul_df, subset=train, 
                        , importance=TRUE) 
bag.seoul

plot(bag.seoul, main="Bagged trees, mtry=13, ntrees=200") 

seoul.rf.test <- (seoul_df[-train,"Rented.Bike.Count"]) 
yhat.bag = predict(bag.seoul,newdata=seoul_df[-train,]) 
cat('RMSE = ',sqrt(mean((yhat.bag-seoul.rf.test)^2))) 

importance(bag.seoul)


ggplot(data.frame(yhat.bag, seoul.rf.test), 
       aes(x=yhat.bag ,y=seoul.rf.test)) + 
  geom_point() + 
  geom_abline(slope=1,intercept=0) + 
  labs(x="predicted_Values",  
       y="test-set_Values", 
       title="Bagged trees, mtry=13") 
varImpPlot(bag.seoul)
