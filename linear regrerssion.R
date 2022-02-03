library(tidyverse)
library(rpart)
library(corrplot)
library(GGally)
library(rpart) 
library(randomForest) 
library(rpart.plot)
library(DMwR2)

sdf <- read.csv('E:/fall2021/stat515/final project/SeoulBikeData.csv')

str(sdf)

sdf <- sdf %>%
  mutate(is_snowing = ifelse(Snowfall..cm.>0, 'Yes', 'NO'))
sdf$is_snowing <- as.factor(sdf$is_snowing)

sdf$day <- weekdays(as.Date(sdf$Date))
sdf$day <- as.factor(sdf$day)

ggplot(data = sdf, aes(x = day, y = Rented.Bike.Count))+
  geom_boxplot(aes(fill = day))+
  scale_x_discrete(limits = c('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday'))+
  labs(x= 'Day of the week', y = 'Hourly Bike Demand', 
       title = "Bike demand v/s Day of the Week")+  theme(legend.position = "none")

#train-test split
set.seed(301)
test <- sample(1:nrow(sdf), size=nrow(sdf)*0.3)
train <- (-test)
# Need train and test response data for later MSE calculations
Bike.train <- sdf$Rented.Bike.Count[train]
Bike.test <- sdf$Rented.Bike.Count[test]

# linear regression
lm.seoul <- lm(Rented.Bike.Count~day*Holiday,data = sdf[train,])
summary(lm.seoul)

# test-set RMSE
fit.test <- predict(lm.seoul, newdata=sdf[test,])
cat("test-set RMSE: ", sqrt(mean((Bike.test - fit.test)^2)))

#plot residuals
par(mfrow = c(2,2))
plot(lm.seoul)

#residuals vs fitted
source(file="E:/fall2021/stat515/final project/rss_regress_funcs_v2.R")
residFit(lm.seoul, 
         title2="Using residFit()")
acf(lm.seoul$residuals)

#decisiom tree
set.seed(1946)
rp.seoul <- rpart(Rented.Bike.Count~+day+Holiday, sdf[train,],cp = 0.0001)
plotcp(rp.seoul)

prn.seoul <- rt.prune(rp.seoul,cp = 0.0024)
printcp(prn.seoul)
