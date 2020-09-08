# Importing packages
library(C50)
library(caret)
library(RWeka)
library(pROC)

# Importing the dataset into r
dmml.1.df <- read.csv("E:/Study Material/Data Mining and Machine Learning - 1/Project/Datasets/steam_games.csv", header = T, na.strings = c(""), stringsAsFactors = T)

########## PRE PROCESSING ##########
str(dmml.1.df)
summary(dmml.1.df)

# checking for null values
sapply(dmml.1.df, function(x) sum(is.na(x)))

# removing unwanted columns
dmml.1.df <- dmml.1.df[ , !names(dmml.1.df) %in% c("appid","name","release_date","developer","publisher","genres","steamspy_tags","median_playtime")]

# conversion of certain columns to factors
dmml.1.df$english <- as.factor(dmml.1.df$english)
dmml.1.df$required_age <- as.factor(dmml.1.df$required_age)

# renaming factor levels
levels(dmml.1.df$english)[levels(dmml.1.df$english) == 0] <- "No"
levels(dmml.1.df$english)[levels(dmml.1.df$english) == 1] <- "Yes"
levels(dmml.1.df$required_age)[levels(dmml.1.df$required_age) == 0] <- "No Age Limit"
levels(dmml.1.df$required_age)[levels(dmml.1.df$required_age) == 3] <- "3+"
levels(dmml.1.df$required_age)[levels(dmml.1.df$required_age) == 7] <- "7+"
levels(dmml.1.df$required_age)[levels(dmml.1.df$required_age) == 12] <- "12+"
levels(dmml.1.df$required_age)[levels(dmml.1.df$required_age) == 16] <- "16+"
levels(dmml.1.df$required_age)[levels(dmml.1.df$required_age) == 18] <- "18+"
levels(dmml.1.df$owners)[levels(dmml.1.df$owners) %in% c("0-20000")] <- "<20K"
levels(dmml.1.df$owners)[levels(dmml.1.df$owners) %in% c("20000-50000","50000-100000","100000-200000","200000-500000")] <- "20K to 500K"
levels(dmml.1.df$owners)[levels(dmml.1.df$owners) %in% c("500000-1000000","1000000-2000000","2000000-5000000","5000000-10000000")] <- "500K to 10M"
levels(dmml.1.df$owners)[levels(dmml.1.df$owners) %in% c("10000000-20000000","20000000-50000000","50000000-100000000","100000000-200000000")] <- "10M to 200M"
levels(dmml.1.df$platforms)[levels(dmml.1.df$platforms) %in% c("mac;linux")] <- "mac linux"
levels(dmml.1.df$platforms)[levels(dmml.1.df$platforms) %in% c("windows;linux")] <- "windows linux"
levels(dmml.1.df$platforms)[levels(dmml.1.df$platforms) %in% c("windows;mac")] <- "windows mac"
levels(dmml.1.df$platforms)[levels(dmml.1.df$platforms) %in% c("windows;mac;linux")] <- "windows mac linux"

levels(dmml.1.df$english)
levels(dmml.1.df$required_age)
levels(dmml.1.df$owners)
levels(dmml.1.df$platforms)

# transforming the column category by extracting certain data
checkcat <- function(x){
  if ((length(grep("Multi-player",x))>0) && (length(grep("Single-player",x))>0)){
     var = "Both"
  }
  else if (length(grep("Multi-player",x))>0){
    var = "Multi-player"
  }
  else if (length(grep("Single-player",x))>0){
    var = "Single-player"
  }
  else {
    var = "Not Mentioned"
  }
  return(var)
}

dmml.1.df$categories <- as.character(dmml.1.df$categories)
dmml.1.df$categories <- as.factor(sapply(dmml.1.df$categories, function(x) checkcat(x)))

levels(dmml.1.df$categories)

# reset the row count
rownames(dmml.1.df) <- NULL
str(dmml.1.df)

########## ANALYSIS USING C5.0 DECISION TREE ##########
# creating training and testing data sets
set.seed(123)
train_sample <- sample(nrow(dmml.1.df), round(0.75*nrow(dmml.1.df)))

dmml.1.df_train <- dmml.1.df[train_sample, ]
dmml.1.df_test <- dmml.1.df[-train_sample, ]

# checking for even split of dependant variable
prop.table(table(dmml.1.df_train$owners))
prop.table(table(dmml.1.df_test$owners))

# running the analysis
C5_train_model <- C5.0(dmml.1.df_train[,-9], dmml.1.df_train$owners)

C5_train_model
summary(C5_train_model)

C5_test_predict <- predict(C5_train_model, dmml.1.df_test)

# evaluating the model
confusionMatrix(C5_test_predict, dmml.1.df_test$owners)
dmml.1.df_test <- droplevels(dmml.1.df_test)

########## ANALYSIS USING RIPPER RULE ##########
JRip_train_model <- JRip(owners ~ ., data = dmml.1.df_train)

JRip_train_model
summary(JRip_train_model)

JRip_test_predict <- predict(JRip_train_model, dmml.1.df_test)

# evaluating the model
confusionMatrix(JRip_test_predict, dmml.1.df_test$owners)

########## COMPARISON OF THE 2 MODELS ###########
C5_test_predict <- as.ordered(C5_test_predict)
JRip_test_predict <- as.ordered(JRip_test_predict)
dmml.1.df_test$owners <- as.ordered(dmml.1.df_test$owners)
C5_ROC <- multiclass.roc(dmml.1.df_test$owners, C5_test_predict)
JRip_ROC <- multiclass.roc(dmml.1.df_test$owners, JRip_test_predict)
C5_ROC
JRip_ROC
par(mfrow=c(1,2))
plot.roc(C5_ROC$rocs[[1]], print.auc = TRUE, col = "green", print.auc.x = 0.6, print.auc.y = 0.7)
plot.roc(C5_ROC$rocs[[2]], print.auc = TRUE, col = "red", add = TRUE, print.auc.x = 0.6, print.auc.y = 0.6)
plot.roc(C5_ROC$rocs[[3]], print.auc = TRUE, col = "blue", add = TRUE, print.auc.x = 0.6, print.auc.y = 0.5)
plot.roc(C5_ROC$rocs[[4]], print.auc = TRUE, col = "yellow", add = TRUE, print.auc.x = 0.6, print.auc.y = 0.4)
plot.roc(C5_ROC$rocs[[5]], print.auc = TRUE, col = "black", add = TRUE, print.auc.x = 0.6, print.auc.y = 0.3)
plot.roc(C5_ROC$rocs[[6]], print.auc = TRUE, col = "orange", add = TRUE, print.auc.x = 0.6, print.auc.y = 0.2)
plot.roc(JRip_ROC$rocs[[1]], print.auc = TRUE, col = "green", print.auc.x = 0.6, print.auc.y = 0.7)
plot.roc(JRip_ROC$rocs[[2]], print.auc = TRUE, col = "red", add = TRUE, print.auc.x = 0.6, print.auc.y = 0.6)
plot.roc(JRip_ROC$rocs[[3]], print.auc = TRUE, col = "blue", add = TRUE, print.auc.x = 0.6, print.auc.y = 0.5)
plot.roc(JRip_ROC$rocs[[4]], print.auc = TRUE, col = "yellow", add = TRUE, print.auc.x = 0.6, print.auc.y = 0.4)
plot.roc(JRip_ROC$rocs[[5]], print.auc = TRUE, col = "black", add = TRUE, print.auc.x = 0.6, print.auc.y = 0.3)
plot.roc(JRip_ROC$rocs[[6]], print.auc = TRUE, col = "orange", add = TRUE, print.auc.x = 0.6, print.auc.y = 0.2)

