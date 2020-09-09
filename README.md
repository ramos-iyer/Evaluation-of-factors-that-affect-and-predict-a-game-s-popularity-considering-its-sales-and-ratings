# Evaluation-of-factors-that-affect-and-predict-a-game-s-popularity-considering-its-sales-and-ratings

# Masters in Data Analytics Project

## Project: Evaluation of factors that affect and predict a game’s popularity considering its sales and ratings

## Table of Contents

- [Overview](#overview)
- [Components](#components)
  - [C5.0 Decision Tree and RIPPER rule on Steam Games](#steam)
  - [Multiple Linear Regression and M5-Prime on Board Games](#board)
  - [Binary Logistic Regression on Apple App Store Strategy Games](#strategy)
- [Running the Code](#running)
- [Screenshots](#screenshots)
- [System Configuration steps](#config)
- [File Descriptions](#files)
- [Credits and Acknowledgements](#credits)

***

<a id='overview'></a>

### Overview
This research paper aims to apply five machine learning algorithms to three different data sets from the  Gaming and Entertainment industry to evaluate the various factors that possibly affect their popularity. These three data sets have been acquired from Kaggle and are based on the Games on Steam, Board Games, Apple App Store Strategy Games. Using the C5.0 Decision Tree and RIPPER Rule on Steam Games, Multiple Linear Regression and Model Tree M5-Prime on Board Games and Binary Logistic Regression on the Apple App Store Strategy Games data set, the popularity of these games has been predicted and various significant factors have been found. The number of owners and the average ratings of these games have been taken into consideration for predicting their popularity. Apart from applying these five machine learning algorithms on these three data sets a comparison was also considered between the two machine learning algorithms applied on each of the data sets except the Apple App Store Strategy Games. A very interesting observation in the Board Games data set was that as per Lantz B., the M5-Prime should perform better than a Multiple Linear Regression model but we found the complete opposite\cite{b1}. Also, the C5.0 Decision tree applied on the Steam games data set grew to a size of 176 branches but the RIPPER Rule only had 37 rules for the same dependent and independent variables. We will look deep into all these factors in the research paper using the KDD methodology of data mining to find answers.

<a id='components'></a>

### Components
There are three components to this project:

<a id='steam'></a>

#### C5.0 Decision Tree and RIPPER rule on Steam Games
File _'DMML\_1.r'_ :

- Loads the `steam games` dataset.
- Performs the necessary transformations on the data to gain knowledge.
- Applies the C5.0 Decision Tree and RIPPER Rule algorithm.
- Generates Evaluation metrics for comparison.

<a id='board'></a>

#### Multiple Linear Regression and M5-Prime on Board Games
File _'DMML\_2.r'_ :

- Loads the `board games` dataset.
- Performs the necessary transformations on the data to gain knowledge.
- Checks for the assumptions for applying a multiple linear regression.
- Applies the Multiple Linear Regression and M5-Prime algorithm.
- Generates Evaluation metrics for comparison.

<a id='strategy'></a>

#### Binary Logistic Regression on Apple App Store Strategy Games
File _'DMML\_3.r'_:

- Loads the `apple app store strategy games` dataset.
- Performs the necessary transformations on the data to gain knowledge.
- Checks for the assumptions for applying a binary logistic regression.
- Applies the Binary Logistic Regression algorithm.
- Generates Evaluation metrics.

<a id='running'></a>

### Running the Code

The codes need to be opened on R Studio and can be run as a whole or run line by line. The code contains comments which provides details on what each chunk of code performs on the data. Also, the datasets have been uploaded as a .zip file and need to be extracted before the code is executed.

<a id='screenshots'></a>

### Screenshots

<a id='config'></a>

### System Configuration Steps

In order to run the code, below are the necessary requirements:

- R and R Studio: As the code is developed in R, you need to install R as well as R Studio in order to open and execute the files.
- Packages: Below is a list of packages that need to be installed before execution of the code.
C50
caret
RWeka
pROC
psych
car
stringr
foreign

<a id='files'></a>

### File Descriptions

There are 2 main folders for this project:

1. Datasets:
- Datasets.zip: This zip file contains all the three datasets used for this project.
- Link to Datasets.txt: This text file contains the link to all the three datasets used in this project.

2. Codes:
- DMML_1.r: Contains the code to apply C5.0 Decision Tree and RIPPER rule on Steam Games
- DMML_2.r: Contains the code to apply Multiple Linear Regression and M5-Prime on Board Games
- DMML_3.r: Contains the code to apply Binary Logistic Regression on Apple App Store Strategy Games

<a id='credits'></a>

### Credits and Acknowledgements

* [Kaggle](https://www.kaggle.com/) for providing all the three datasets used for this project.
* [NCI](https://www.ncirl.ie/) for a challenging project as part of their full-time masters in data analytics course subject 'Database and Analytics Programming'
