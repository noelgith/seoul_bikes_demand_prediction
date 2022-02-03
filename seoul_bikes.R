library(tidyverse)
library(ggcorrplot)
library(coefplot)
library(leaps) 
library(glmnet)

df <- read.csv('E:/fall2021/stat515/final project/SeoulBikeData.csv')

seoul_df <- na.omit(df)
length(seoul_df)

seoul_df$Holiday <- as.factor(seoul_df$Holiday)
seoul_df$Functioning.Day <- as.factor(seoul_df$Functioning.Day)
seoul_df$Seasons <- as.factor(seoul_df$Seasons)

#normalize independent variables

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

temp_df <- seoul_df_normalized[c(2,3,9:19)]

seoul_df_.numeric <- select_if(temp_df, is.numeric)

# Computing correlation matrix
#correlation_matrix <- round(cor(seoul_df.numeric),1)

# calculate the correlations
r <- cor(seoul_df_.numeric, use="complete.obs")
round(r,2)

ggcorrplot(r,
           type = "lower",
           lab = TRUE)

#linear regression
seoul_bike.lm <- lm(Rented.Bike.Count~norm_temp+Hour,data = temp_df)
summary(seoul_bike.lm)

#plot residuals
par(mfrow = c(2,2))
plot(seoul_bike.lm)

#residuals vs fitted
source(file="E:/fall2021/stat515/final project/rss_regress_funcs_v2.R")
residFit(seoul_bike.lm, 
         title2="Using residFit()")

mean(seoul_bike.lm$residuals)

sigma(seoul_bike.lm)/mean(temp_df$Rented.Bike.Count)

regfit.seoul <- regsubsets(Rented.Bike.Count~., data = temp_df)
summary(regfit.seoul)
