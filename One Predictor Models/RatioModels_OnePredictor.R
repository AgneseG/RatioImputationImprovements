# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### In this file, all ratio models with one predictor have been fitted and evaluated based on MAPE and RMSE.
### Results are presented first for the Dutch Structural Business Dataset and then for the Dutch Healthcare Dataset.
### For each target variable, a ratio model with (1) wls; (2) Huber; (3) Tukey estimator has been implemented and compared.
# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
setwd("C:/Users/AGMO/Desktop/Final code Thesis/")

set.seed(42)

library(MASS)
library(Metrics)

#####################################################################################
### Dutch Structural Business Dataset 
#####################################################################################

dat<-read.csv2("Data_wholesalers.csv")
str(dat)
dat <- dat[-c(1,4,5,7)]
str(dat)

dat = dat[-c(38, 263,  271, 103, 24), ] #outliers exclusion
dat <- dat[complete.cases(dat),]

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### Train-test split
# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(121)
idx <- seq(nrow(dat))
idx_train <- sample(idx, size = floor(nrow(dat) * 0.75), replace = FALSE) 
idx_test <- idx[!idx %in% idx_train]

train_data  <- dat[idx_train,]
test_data <- dat[idx_test,] 
# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#### wls ratio models
# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

performance.mean <- function (train_data, test_data, col_index_x, col_index_y) {
  
  set.seed(42)
  
  y <- train_data[,col_index_y]
  
  R = lm(y ~ train_data[,col_index_x] -1,weight=1/(train_data[,col_index_x]+1))[[1]]
  
  yhat<- R*test_data[,col_index_x] #predicted values of yhat
  
  yimp <- c(train_data[,col_index_y], yhat)
  
  MAPE<- mape(test_data[,col_index_y], yhat)
  
  RMSE = rmse(test_data[,col_index_y], yhat)
  
  return(list(MAPE, RMSE))
  
  
}

error_n_employyes <- list(performance.mean(train_data, test_data, 3, 5), # MAPE and RMSE when number of employees is used as predictor variable 
                          performance.mean(train_data, test_data,3,1),
                          performance.mean(train_data, test_data,3,6),
                          performance.mean(train_data, test_data,3,4))

error_n_employyes

error_depreciation <- list(performance.mean(train_data, test_data,2,5), # MAPE and RMSE when number of employees is used as predictor variable 
                           performance.mean(train_data, test_data,2,1), 
                           performance.mean(train_data, test_data,2,6),
                           performance.mean(train_data, test_data,2,4))

error_depreciation

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### Huber ratio models
# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

performance.huber <- function (train_data, test_data, col_index_x, col_index_y) {
  
  set.seed(42)
  
  y <- train_data[,col_index_y]
  
  R = rlm(y ~ train_data[,col_index_x] -1, psi=psi.huber)[[1]]
  
  yhat<- R*test_data[,col_index_x] #predicted values of yhat
  
  yimp <- c(train_data[,col_index_y], yhat)
  
  MAPE<- mape(test_data[,col_index_y], yhat)
  
  RMSE = rmse(test_data[,col_index_y], yhat)
  
  return(list(MAPE, RMSE))
  
  
}

error_n_employyes_huber <- list(performance.huber(train_data, test_data, 3, 5), # MAPE and RMSE when number of employees is used as predictor variable 
                                performance.huber(train_data, test_data,3,1),
                                performance.huber(train_data, test_data,3,6),
                                performance.huber(train_data, test_data,3,4))

error_n_employyes_huber

error_depreciation_huber <- list(performance.huber(train_data, test_data,2,5), # MAPE and RMSE when number of employees is used as predictor variable 
                           performance.huber(train_data, test_data,2,1), 
                           performance.huber(train_data, test_data,2,6),
                           performance.huber(train_data, test_data,2,4))

error_depreciation_huber

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### Tukey ratio models
# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


performance.tukey <- function (train_data, test_data, col_index_x, col_index_y) {
  
  set.seed(42)
  
  y <- train_data[,col_index_y]
  
  R = rlm(y ~ train_data[,col_index_x] -1, psi=psi.bisquare)[[1]]
  
  yhat<- R*test_data[,col_index_x] #predicted values of yhat
  
  yimp <- c(train_data[,col_index_y], yhat)
  
  MAPE<- mape(test_data[,col_index_y], yhat)
  
  RMSE = rmse(test_data[,col_index_y], yhat)
  
  return(list(MAPE, RMSE))
  
  
}

error_n_employyes_tukey <- list(performance.tukey(train_data, test_data, 3, 5), # MAPE and RMSE when number of employees is used as predictor variable 
                          performance.tukey(train_data, test_data,3,1),
                          performance.tukey(train_data, test_data,3,6),
                          performance.tukey(train_data, test_data,3,4))

error_n_employyes_tukey

error_turnover_tukey <- list(performance.tukey(train_data, test_data,2,5), # MAPE and RMSE when turnover is used as predictor variable 
                           performance.tukey(train_data, test_data,2,1), 
                           performance.tukey(train_data, test_data,2,6),
                           performance.tukey(train_data, test_data,2,4))

error_turnover_tukey

#####################################################################################
### Dutch Healthcare Dataset 
#####################################################################################

dat<-read.csv2("healthcare.csv")
dat <- dat[-c(1,4,10,11)] # Selecting relevant variables
dat <- dat[-422,] # Excluding main outlier
str(dat)
dat <- dat[complete.cases(dat),]


set.seed(121)
idx <- seq(nrow(dat))
idx_train <- sample(idx, size = floor(nrow(dat) * 0.75), replace = FALSE) 
idx_test <- idx[!idx %in% idx_train]

train_data  <- dat[idx_train,]
test_data <- dat[idx_test,] 

#### wls 

error_n_turnover <- list(performance.mean(train_data, test_data, 1, 5), # MAPE and RMSE when turnover is used as predictor variable 
                          performance.mean(train_data, test_data,1,3),
                          performance.mean(train_data, test_data,1,6),
                          performance.mean(train_data, test_data,1,4),
                          performance.mean(train_data, test_data,1,7))

error_n_turnover


error_employees <- list(performance.mean(train_data, test_data,2,5), # MAPE and RMSE when number of employees is used as predictor variable 
                           performance.mean(train_data, test_data,2,3), 
                           performance.mean(train_data, test_data,2,6),
                           performance.mean(train_data, test_data,2,4),
                           performance.mean(train_data, test_data,2,7))

error_employees

#####################################################################################################

error_n_turnover_huber <- list(performance.huber(train_data, test_data, 1, 5), # MAPE and RMSE when turnover is used as predictor variable 
                                performance.huber(train_data, test_data,1,3),
                                performance.huber(train_data, test_data,1,6),
                                performance.huber(train_data, test_data,1,4),
                                performance.huber(train_data, test_data,1,7))

error_n_turnover_huber

error_employyes_huber <- list(performance.huber(train_data, test_data,2,5), # MAPE and RMSE when number of employees is used as predictor variable 
                                 performance.huber(train_data, test_data,2,3), 
                                 performance.huber(train_data, test_data,2,6),
                                 performance.huber(train_data, test_data,2,4),
                                 performance.huber(train_data, test_data, 2, 7))

error_employyes_huber

#####################################################################################################

error_n_turnover_tukey <- list(performance.tukey(train_data, test_data, 1, 5), # MAPE and RMSE when turnover is used as predictor variable 
                                performance.tukey(train_data, test_data,1,3),
                                performance.tukey(train_data, test_data,1,6),
                                performance.tukey(train_data, test_data,1,4),
                                performance.tukey(train_data, test_data,1,7))

error_n_turnover_tukey

error_employees_tukey <- list(performance.tukey(train_data, test_data,2,5), # MAPE and RMSE when number of employees is used as predictor variable 
                                 performance.tukey(train_data, test_data,2,3), 
                                 performance.tukey(train_data, test_data,2,6),
                                 performance.tukey(train_data, test_data,2,4),
                                 performance.tukey(train_data, test_data,2,7))

error_employees_tukey


