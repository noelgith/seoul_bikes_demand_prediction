library(tidyverse)
library(ggcorrplot)
library(coefplot)
library(leaps) 
library(glmnet)
library(corrplot)
library(caret)

seoul_df <- read.csv('E:/fall2021/stat515/final project/SeoulBikeData.csv')


seoul_df$Holiday <- as.factor(seoul_df$Holiday)
seoul_df$Functioning.Day <- as.factor(seoul_df$Functioning.Day)
seoul_df$Seasons <- as.factor(seoul_df$Seasons)

#define Min-Max normalization function
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

#apply min-max normalization
seoul_df_normalized <- seoul_df %>% mutate(norm_temp = min_max_norm(seoul_df$Temperature..C.)) %>%
  mutate(norm_humidity = min_max_norm(seoul_df$Humidity...)) %>%
  mutate(norm_wind = min_max_norm(seoul_df$Wind.speed..m.s.)) %>%
  mutate(norm_visibility = min_max_norm(seoul_df$Visibility..10m.)) %>%
  mutate(norm_dewpt = min_max_norm(seoul_df$Dew.point.temperature..C.))

seoul_df_normalized <- seoul_df_normalized[c(2,3,9:19)]


#following code is for running linear regression using best-subset approach
#this is the analysis of research question 1

#correlation plot
corrplot(cor(select_if(seoul_df_normalized, is.numeric)),type='upper', diag=FALSE, tl.srt=45)

#cor(select(df,Rented.Bike.Count,Temperature..C.,Hour,Visibility..10m.,Rainfall.mm.,Snowfall..cm.))
#weather_df stores all the relevant weather attributes along with the target variable.
weather_df <- select(seoul_df_normalized,Rented.Bike.Count,norm_temp,Hour,norm_visibility,Rainfall.mm.,Snowfall..cm.,norm_wind)

set.seed(1778)
test <- sample(1:nrow(weather_df), size=nrow(weather_df)*0.25)
train <- (-test)

# Need train and test response data for later MSE calculations

demand.train <- seoul_df_normalized$Rented.Bike.Count[train]
demand.test <- seoul_df_normalized$Rented.Bike.Count[test]

#subset predictors
model.fwd <- regsubsets(Rented.Bike.Count ~ .,
                        nvmax=5, data=weather_df)
res.sum <- summary(model.fwd)
data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic)
)

#the following code is obtained from the STHDA website 
#http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/155-best-subsets-regression-essentials-in-r/
# id: model id
# object: regsubsets object
# data: data used to fit regsubsets
# outcome: outcome variable
get_model_formula <- function(id, object, outcome){
  # get models data
  models <- summary(object)$which[id,-1]
  # Get outcome variable
  #form <- as.formula(object$call[[2]])
  #outcome <- all.vars(form)[1]
  # Get model predictors
  predictors <- names(which(models == TRUE))
  predictors <- paste(predictors, collapse = "+")
  # Build model formula
  as.formula(paste0(outcome, "~", predictors))
}

#5-fold cross validation
get_cv_error <- function(model.formula, data){
  set.seed(1)
  train.control <- trainControl(method = "cv", number = 5)
  cv <- train(model.formula, data = data, method = "lm",
              trControl = train.control)
  cv$results$RMSE
}

# Compute cross-validation error
model.ids <- 1:5
cv.errors <-  map(model.ids, get_model_formula, model.fwd, "Rented.Bike.Count") %>%
  map(get_cv_error, data = weather_df) %>%
  unlist()
cv.errors

#get_model_formula(3, model.fwd, "Rented.Bike.Count")
which.min(cv.errors)

coef(model.fwd,5)

model5.lm <- lm(Rented.Bike.Count~., data = weather_df)
summary(model5.lm)

#hypothesis 3&4
seoul_df$day <- weekdays(as.Date(seoul_df$Date))
seoul_df$day <- as.factor(seoul_df$day)

week_holy <- select(seoul_df,Rented.Bike.Count,Temperature..C.,Hour,
                    Visibility..10m.,Rainfall.mm.,Snowfall..cm., day, Holiday)
model2.lm <- lm(Rented.Bike.Count~.+Holiday, data = week_holy)
summary(model2.lm)

# decision tree 

#Lasso - regression

y <- seoul_df[train,c('Rented.Bike.Count')]
test_Y <- seoul_df[test,c('Rented.Bike.Count')]

x <- data.matrix(seoul_df[train,c(1,3:13)])
test_x <- data.matrix(seoul_df[test,c(1,3:13)])

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_model) 

#find coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

#use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = test_x)

#find SST and SSE
sst <- sum((test_Y - mean(test_Y))^2)
sse <- sum((y_predicted - test_Y)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq

cat("test-set RMSE: ", sqrt(mean((demand.test-y_predicted)^2)))

ggplot(data.frame(demand.test, y_predicted),
       aes(x=y_predicted, y= y_predicted - demand.test)) +
  geom_point() +
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0) +
  labs(x="fitted", y="residual",
       title="Test-set residual vs. fitted for best-lambda lasso")

#####
grid=10^seq(10,-2,length=100)
lasso.mod=glmnet(x,y,alpha=1, lambda=grid)

# Cross validation of the training set determines the lambda minimizing MSE.
# Also, determines lambda for the 1-standard-error rule.
set.seed(409)
cv.out=cv.glmnet(x,y,alpha=1, nfold=10)
plot(cv.out)

(bestlam =cv.out$lambda.min)

(bestlam.1se=cv.out$lambda.1se)

# Plot coefficient paths
par(mfrow=c(1,1))
plot(cv.out$glmnet.fit, xvar="lambda", label=TRUE)
# add in vertical lines for optimal values of lambda
abline(v=log(c(bestlam, bestlam.1se)), lty=2)

# Determine MSE's for lambda minimizing cross-validated training-set MSE
fit.train <- predict(lasso.mod, s=bestlam, newx=x)
cat("training set RMSE: ", sqrt(mean((y-fit.train)^2)))

fit.test <- predict(lasso.mod, s=bestlam, newx=test_x)
cat("test-set RMSE: ", sqrt(mean((test_Y-fit.test)^2)))
