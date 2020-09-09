# Importing Packages
library(stringr)
library(foreign)
library(caret)
library(pROC)
library(psych)

# Importing the dataset into r
dmml.3.df <- read.csv("E:/Study Material/Data Mining and Machine Learning - 1/Project/Datasets/appstore_strategy_games.csv", header = T, na.strings = c(""), stringsAsFactors = T)

########## PRE PROCESSING ##########
summary(dmml.3.df)
str(dmml.3.df)

# removing unwanted columns
dmml.3.df <- dmml.3.df[ , !names(dmml.3.df) %in% c("Primary.Genre","URL","ID","Name","Subtitle","Icon.URL","Description","Developer","Genres","Original.Release.Date","Current.Version.Release.Date")]

# checking for null values
sapply(dmml.3.df, function(x) sum(is.na(x)))

# removing NA values from Size, Price and Languages
dmml.3.df <- dmml.3.df[!is.na(dmml.3.df$Size),]
dmml.3.df <- dmml.3.df[!is.na(dmml.3.df$Price),]
dmml.3.df <- dmml.3.df[!is.na(dmml.3.df$Languages),]

########## TRANSFORMATION ##########
summary(dmml.3.df)
sapply(dmml.3.df, function(x) sum(is.na(x)))

# Transforming Average User Rating to either be good or bad for logistic regression analysis
dmml.3.df <- dmml.3.df[!is.na(dmml.3.df$Average.User.Rating),]
dmml.3.df$Average.User.Rating <- as.character(dmml.3.df$Average.User.Rating)

unique(dmml.3.df$Average.User.Rating)

dmml.3.df$Average.User.Rating[which(dmml.3.df$Average.User.Rating %in% c("0","0.5","1","1.5","2","2.5","3","3.5","4"))] <- "Bad"
dmml.3.df$Average.User.Rating[which(dmml.3.df$Average.User.Rating %in% c("4.5","5"))] <- "Good"
dmml.3.df$Average.User.Rating <- as.factor(dmml.3.df$Average.User.Rating)

# Converting all NA in In.App.Purchases to 0
dmml.3.df$In.app.Purchases[is.na(dmml.3.df$In.app.Purchases)] <- "0"

# converting column size to MB from Bytes for easier understanding
btomb <- function(x){
  varnum <- x/1048576
  varnum <- as.numeric(format(round(varnum, 2), nsmall = 2))
  return (varnum)
}

dmml.3.df$Size <- sapply(dmml.3.df$Size, function(x) btomb(x))

# converting languages to have only 3 levels
conlang <- function(x){
  if (str_detect(x, "^.*EN.*$", negate = TRUE)) {
    varlan = "No EN"
  }
  else if (x == "EN"){
    varlan = "Only EN"
  }
  else {
    varlan = "EN +"
  }
  return(varlan)
}

dmml.3.df$Languages <- as.factor(sapply(dmml.3.df$Languages, function(x) conlang(x)))

# getting only max in-app purchase amount from vector
maxinapp <- function(x){
  maxval <- unlist(str_split(x, ","))
  maxval <- trimws(maxval)
  nummaxval <- vector()
  for (i in (1:length(maxval))) {
    nummaxval[i] <- as.numeric(maxval[i])
  }
  return (max(nummaxval))
}

dmml.3.df$In.app.Purchases <- sapply(dmml.3.df$In.app.Purchases, function(x) maxinapp(x))

# reset the row count
rownames(dmml.3.df) <- NULL

# checking for multicollinearity
pairs.panels(dmml.3.df)

# creating dummy variables for multicategorical variables
dmy <- dummyVars(" ~ Age.Rating", data = dmml.3.df, fullRank = T)
age_rat <- data.frame(predict(dmy, newdata = dmml.3.df))

dmy <- dummyVars(" ~ Languages", data = dmml.3.df, fullRank = T)
lang <- data.frame(predict(dmy, newdata = dmml.3.df))

# removing categorized variables and adding dummy variables
dmml.3.df <- dmml.3.df[,c(-5,-6)]
dmml.3.df <- data.frame(dmml.3.df, age_rat, lang)

str(dmml.3.df)

# Checking the cooks distance for the model values and removing outliers
blr_train_model <- glm(Average.User.Rating ~ ., data = dmml.3.df, family = binomial)

blr_train_model
summary(blr_train_model)
cd_train_model <- cooks.distance(blr_train_model)
cd_train_model[cd_train_model>1]
infl <- as.numeric(names(cd_train_model[cd_train_model>1]))
# No outliers detected

# creating testing and training datasets
set.seed(123)
train_sample <- sample(nrow(dmml.3.df), round(0.7*nrow(dmml.3.df)))

dmml.3.df_train <- dmml.3.df[train_sample, ]
dmml.3.df_test <- dmml.3.df[-train_sample, ]

# checking for even split of dataset
prop.table(table(dmml.3.df_train$Average.User.Rating))
prop.table(table(dmml.3.df_test$Average.User.Rating))

########## ANALYSIS USING BINOMIAL LOGISTIC REGRESSION ##########
# removing insignificant variables using backward testing from the model
blr_train_model <- glm(Average.User.Rating ~ ., data = dmml.3.df_train, family = "binomial")

blr_train_model
summary(blr_train_model)

blr_train_model <- glm(Average.User.Rating ~ User.Rating.Count+Price+In.app.Purchases+Size+Age.Rating.17.+Age.Rating.4.+Age.Rating.9., data = dmml.3.df_train, family = "binomial")

blr_train_model
summary(blr_train_model)

blr_train_model <- glm(Average.User.Rating ~ User.Rating.Count+In.app.Purchases+Size+Age.Rating.17.+Age.Rating.4.+Age.Rating.9., data = dmml.3.df_train, family = "binomial")

blr_train_model
summary(blr_train_model)

blr_test_predict <- predict(blr_train_model, dmml.3.df_test, type = "response")
summary(blr_test_predict)

blr_test_predict[blr_test_predict<=0.5] <- "Bad"
blr_test_predict[blr_test_predict != "Bad"] <- "Good"
blr_test_predict <- factor(blr_test_predict, levels=c("Bad","Good"))

# evaluating the model
confusionMatrix(blr_test_predict, dmml.3.df_test$Average.User.Rating, positive = "Good")

dmml.3.df_test$Average.User.Rating <- as.ordered(dmml.3.df_test$Average.User.Rating)
blr_test_predict <- as.ordered(blr_test_predict)
blr_ROC <- roc(dmml.3.df_test$Average.User.Rating, blr_test_predict)

blr_ROC

par(mfrow=c(1,1))
plot.roc(blr_ROC, print.auc = TRUE, col = "magenta")

# removing unused objects
rm("age_rat","dmy","lang","train_sample")
