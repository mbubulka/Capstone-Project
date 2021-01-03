---
title: "CAPSTONE FINAL"
author: "Michael Bubulka"
date: "1/2/2021"
output: word_document
---

####{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown
. 
####{r problem set up}

######  install all required packages for this project
install.packages("caret")
library(caret)
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# Adding Additional Packages 
library (broom)
library(lubridate)
library(tibble)
install.packages("randomForest")
library(randomForest)
install.packages("matrixStats")
library(matrixStats)
library(purrr)
install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
install.packages("e1071")
library(e1071)
library(readr)
library(readxl)
library(ggplot2)
install.packages("caretEnsemble")
library(caretEnsemble)
install.packages("RANN")
install.packages("arm")
library(arm)
install.packages("penalized")
library(penalized)
install.packages("pls")
library(pls)
install.packages("quantregForest")
library(quantregForest)
library(dplyr)
```

###{r importing the data from kaggle}

##Data was downloaded as a CSV and converted into excel file.   Excel  file will be attached separately. 
dataset <- read_xlsx("dataset_1.xlsx")
 


###{r cleaning the data}
### Cleaning the data and creating the trainset and test set.  
dataset_tidy <- as.data.frame(dataset)
dataset_tidy$Genre <- as.factor(dataset_tidy$Genre)
dataset_tidy$Author <- as.factor(dataset_tidy$Author)
set.seed(1, sample.kind ="Rounding")
y <- dataset_tidy$`User Rating`

test_index <- createDataPartition(y, times = 1, p = 0.7, list = FALSE)

train_set <- dataset_tidy%>% slice(test_index)
test_set <- dataset_tidy %>% slice(-test_index)

###`{r}
##Sumarize the data with Scale
summary(dataset_tidy[,1:6])
#### calculate the pre-process parameters from the dataset
preprocessParams <- preProcess(dataset_tidy[,1:6], method=c("scale"))
preprocessParams



####{r exploring the data preproc functions}
##Sumarize the data with Center and Scale
summary(dataset_tidy[,1:6])
#### calculate the pre-process parameters from the dataset
preprocessParams_1 <- preProcess(dataset_tidy[,1:6], method=c("scale","center"))
preprocessParams_1


###{r, a look at the training and test data}
## Viewing the training and test dataset
str(test_set)
head(test_set)
str(train_set)
head(train_set)



###{r average}
## Looking at the average user
avg_user_rating <-  mean(train_set$`User Rating`)
avg_user_rating




###{r LM w/o caret}
## LM model without using caret
fit_lm <- lm(train_set$`User Rating` ~ Reviews + Year +Price + Genre, data = train_set)
fit_lm$coeff

y_hat <- predict(fit_lm, test_set)

rmse_lm_wo <- RMSE(y_hat, test_set$`User Rating`)
rmse_lm_wo



###{r LM w/o caret various variables}
## a look a LM with out caret and ingoring genre
fit_lm_genre <- lm(`User Rating` ~ Reviews + Year +Price , data = train_set)
fit_lm_genre$coeff
y_hat_genre <- predict(fit_lm_genre, test_set)

rmse_lm_wo_genre <- RMSE(y_hat_genre, test_set$`User Rating`)
rmse_lm_wo_genre






###{r LM REVIEWS and YEAR Only}
### A look at Reviews and Year only on a LM model
fit_lm_genre_price <- lm(`User Rating` ~ Reviews + Year  , data = train_set)
fit_lm_genre_price$coeff
y_hat_genre_price <- predict(fit_lm_genre_price, test_set)

rmse_lm_wo_genre_price <- RMSE(y_hat_genre_price, test_set$`User Rating`)
rmse_lm_wo_genre_price






###{r REVIEWS only}
## LM model of Reviews only
fit_lm_genre_price_year <- lm(`User Rating` ~ Reviews   , data = train_set)
fit_lm_genre_price_year$coeff
y_hat_genre_price_year <- predict(fit_lm_genre_price_year, test_set)

rmse_lm_wo_genre_price_year <- RMSE(y_hat_genre_price_year, test_set$`User Rating`)
rmse_lm_wo_genre_price_year






###{r results of LM models}
## results of LM models
results_lm_wo_caret <- data.frame(Method =c( "Linear Regression with Reviews, Year, Price, Genre", "Linear Regression with Reviews, Year, Price", "Linear Regression with Reviews, Year","Linear Regression with Reviews Only"), RMSE =c(rmse_lm_wo, rmse_lm_wo_genre,rmse_lm_wo_genre_price, rmse_lm_wo_genre_price_year))
results_lm_wo_caret

###{r LM w all variables}
### Using Caret to build the LM models
### Genre, year, price, reviews
set.seed(1, sample.kind = "Rounding")
train_lm <- train(`User Rating` ~ Reviews + Year+ Price + Genre , method = "lm", data = train_set)
y_hat_lm <- predict(train_lm, test_set, type ="raw")
rmse_lm <- RMSE(y_hat_lm, test_set$`User Rating`)
rmse_lm




###{r LM Reviews year price}
## Using Caret, LM Model Year, Price, Reviews
set.seed(1, sample.kind = "Rounding")
train_lm_genre <- train(`User Rating` ~ Reviews + Year+ Price  , method = "lm", data = train_set)
y_hat_lm_genre <- predict(train_lm_genre, test_set, type ="raw")
rmse_lm_genre <- RMSE(y_hat_lm_genre, test_set$`User Rating`)
rmse_lm_genre


###{r LM Reviews, Year}
##Using Caret, LM model  of Reviews, year
set.seed(1, sample.kind = "Rounding")
train_lm_genre_price <- train(`User Rating` ~ Reviews + Year  , method = "lm", data = train_set)
y_hat_lm_genre_price <- predict(train_lm_genre_price, test_set, type ="raw")
rmse_lm_genre_price <- RMSE(y_hat_lm_genre_price, test_set$`User Rating`)
rmse_lm_genre_price



###{r LM Reviews}
## using caret, LM model of Reviews only
train_lm_genre_price_year <- train(`User Rating` ~ Reviews   , method = "lm", data = train_set)
y_hat_lm_genre_price_year <- predict(train_lm_genre_price_year, test_set, type ="raw")
rmse_lm_genre_price_year <- RMSE(y_hat_lm_genre_price_year, test_set$`User Rating`)
rmse_lm_genre_price_year



###{r results from CARET LM}
## results of Caret LM models
results_lm_caret <- data.frame(Method=c("LM w/ Caret and all variables","LM w/ Caret and Reviews,Year, Price","LM w/ Caret and Reviews, Year","LM w/ Caret and Reviews"), RMSE = c(rmse_lm,rmse_lm_genre,rmse_lm_genre_price,rmse_lm_genre_price_year))
results_lm_caret

###{r comparison table of LM}
## comparison of simple model and caret models
comparison_results <- data.frame(results_lm_caret, results_lm_wo_caret)
comparison_results




###{r RANDOM FOREST}

## using caret to explore other models.  
### Randomforest
set.seed(1, sample.kind = "Rounding")

train_rf <- train(`User Rating`~ . , method = "rf", data = train_set, metric ="RMSE")
y_hat_rf <- predict(train_rf, test_set, type = "raw")
rmse_rf <- RMSE(y_hat_rf,test_set$`User Rating`)

                
rmse_rf
plot(train_rf)




###{r KNN }
## using caret to explore KNN 
set.seed(1, sample.kind = "Rounding")
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(`User Rating` ~ ., method= "knn", data = train_set, tuneGrid = data.frame(k=seq(9,71,2)), trControl = control, metric ="RMSE")
train_knn$bestTune
y_hat_knn <- predict(train_knn, test_set, type ="raw")
rmse_knn <- RMSE(y_hat_knn, test_set$`User Rating`)
rmse_knn
ggplot(train_knn, highlight = TRUE)
train_knn$finalModel
   plot(train_knn)


   
##{r Penalized}
## using caret to
train_penalized <- train(`User Rating` ~ ., method= "penalized", data= train_set, metric= "RMSE")
y_hat_pen <- predict(train_penalized, test_set, type ="raw")
rmse_pen <- RMSE(y_hat_pen, test_set$`User Rating`)
rmse_pen



###{r Bayes GLM}
set.seed(1, sample.kind ="Rounding")
train_glm <- train(`User Rating`~., method = "bayesglm", data =train_set, metric ="RMSE")
y_hat_glm <- predict(train_glm, test_set, type ="raw")
rmse_glm <- RMSE(y_hat_glm, test_set$`User Rating`)
rmse_glm



  



###{r Results}
results <- data.frame(Method =c( "Linear Regresion with Caret"," Bayes GLM","KNN","RandomForest", "Penalized"), RMSE =c(rmse_lm, rmse_glm,rmse_knn,rmse_rf, rmse_pen))
results



