# importing packages
library(psych)
library(caret)
library(RWeka)
library(car)

# Importing the dataset into r
dmml.2.df <- read.csv("E:/Study Material/Data Mining and Machine Learning - 1/Project/Datasets/board_games.csv", header = T, na.strings = c(""), stringsAsFactors = T)

########## PRE PROCESSING ##########
summary(dmml.2.df)
str(dmml.2.df)

# Reducing data frame to have only board games
dmml.2.df <- dmml.2.df[which(dmml.2.df$type == "boardgame"),]

# removing unwanted columns
dmml.2.df <- dmml.2.df[ , !names(dmml.2.df) %in% c("id","name","type","yearpublished","playingtime","bayes_average_rating","total_traders","total_comments","total_weights","average_weight","meta")]

# checking for null values
sapply(dmml.2.df, function(x) sum(is.na(x)))

# removing the NA values as they are very low
dmml.2.df <- dmml.2.df[complete.cases(dmml.2.df), ]

# Removing unused factors from the data frame
dmml.2.df <- droplevels(dmml.2.df)

########## TRANSFORMATION ##########
# converting minplaytime and maxplaytime to avgplaytime
avgply <- function(x,y){
  avgtme <- (x+y)/2
  return(avgtme)
}
dmml.2.df$avgplaytime <- mapply(avgply,dmml.2.df$minplaytime,dmml.2.df$maxplaytime)
dmml.2.df <- dmml.2.df[ , !names(dmml.2.df) %in% c("minplaytime","maxplaytime")]

# converting total_wanters and total_wishers to total_interested
totint <- function(x,y){
  totintp <- x+y
  return(totintp)
}
dmml.2.df$total_interested <- mapply(totint,dmml.2.df$total_wanters,dmml.2.df$total_wishers)
dmml.2.df <- dmml.2.df[ , !names(dmml.2.df) %in% c("total_wanters","total_wishers")]

# removing all records with an average rating of 0 to avoid issues with model assumptions
dmml.2.df$average_rating[dmml.2.df$average_rating == 0] <- NA
dmml.2.df <- dmml.2.df[!is.na(dmml.2.df$average_rating), ]
str(dmml.2.df)

########## ANALYSIS USING MULTIPLE LINEAR REGRESSION ##########

# checking for multicollinearity
#pairs.panels(dmml.2.df)

# users_rated has high correlation with total_owners & total_interested
dmml.2.df <- dmml.2.df[ , !names(dmml.2.df) %in% c("users_rated")]

# reset the row count
rownames(dmml.2.df) <- NULL

#checking for normality of the dependant variable
par(mfrow=c(1,1))
hist(dmml.2.df$average_rating, xlab = "Average Rating", main = "Normality check of Average Rating")

# running the analysis to find outliers and removing the outliers
infl <- 0
while (length(infl) > 0) {
  rownames(dmml.2.df) <- NULL
  mlr_full_model <- lm(average_rating ~ ., data = dmml.2.df)
  
  cd_full_model <- cooks.distance(mlr_full_model)
  cd_full_model[cd_full_model>1]
  infl <- as.numeric(names(cd_full_model[cd_full_model>1]))
  if (length(infl) > 0){
    dmml.2.df <- dmml.2.df[-infl, ]  
  }
}


# creating testing and training datasets
set.seed(123)
train_sample <- sample(nrow(dmml.2.df), round(0.75*nrow(dmml.2.df)))

dmml.2.df_train <- dmml.2.df[train_sample, ]
dmml.2.df_test <- dmml.2.df[-train_sample, ]

# checking for even split of dependant variable
summary(dmml.2.df_train$average_rating)
summary(dmml.2.df_test$average_rating)

# running the analysis
mlr_train_model <- lm(average_rating ~ ., data = dmml.2.df_train)

mlr_train_model
summary(mlr_train_model)

# checking for assumptions of a linear model
par(mfrow=c(2,2))
plot(mlr_train_model)

# running model on test dataset
mlr_test_predict <- predict(mlr_train_model, dmml.2.df_test)

#creating a function to find the MEAN ABSOLUTE ERROR (MAE)
MAE <- function(a,p){
  mean(abs(a-p))
}

# evaluating the model
summary(mlr_test_predict)
summary(dmml.2.df_test$average_rating)

cor(mlr_test_predict, dmml.2.df_test$average_rating)

MAE(dmml.2.df_test$average_rating, mlr_test_predict)

MAE(mean(dmml.2.df_train$average_rating), dmml.2.df_test$average_rating)

########## ANALYSIS USING M5 PRIME ##########
m5p_train_model <- M5P(average_rating ~ ., data = dmml.2.df_train)

m5p_train_model
summary(m5p_train_model)

m5p_test_predict <- predict(m5p_train_model, dmml.2.df_test)

# evaluating the model
summary(m5p_train_model$predictions)
summary(dmml.2.df_test$average_rating)

cor(m5p_test_predict, dmml.2.df_test$average_rating)

MAE(m5p_test_predict, dmml.2.df_test$average_rating)

MAE(mean(dmml.2.df_train$average_rating), dmml.2.df_test$average_rating)

