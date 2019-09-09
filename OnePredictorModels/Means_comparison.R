# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### In this file, all models with one predictors (wls, Huber, Tukey) have been evaluated in the post-imputation mean, compared to the real mean.
### Results are presented first for all the target variables of the Dutch Structural Business Dataset 
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

dat = dat[-c(38, 263,  271, 103, 24), ]
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
### Wls
# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(121)

mean_change_wls <- function (train_data, test_data, col_index_x, col_index_y) {
  
  set.seed(42)
  
  y <- train_data[,col_index_y]
  
  R = lm(y ~ train_data[,col_index_x] -1,weight=1/(train_data[,col_index_x]+1))[[1]]
  
  yhat<- R*test_data[,col_index_x] #predicted values of yhat
  
  yimp <- c(train_data[,col_index_y], yhat)
  
  
  return(list("Mean of imputed values"=mean(yhat), "Mean after imputation"=mean(yimp)))
  
  
}


predictor_turnover <- list(mean_change_wls(train_data, test_data,2,5), # when turnover is used as predictor variable 
                           mean_change_wls(train_data, test_data,2,1), 
                           mean_change_wls(train_data, test_data,2,6),
                           mean_change_wls(train_data, test_data,2,4))

predictor_turnover

predictor_employyes <- list(mean_change_wls(train_data, test_data, 3, 5), # when number of employees is used as predictor variable 
                            mean_change_wls(train_data, test_data,3,1),
                            mean_change_wls(train_data, test_data,3,6),
                            mean_change_wls(train_data, test_data,3,4))

predictor_employyes

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### Huber
# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

set.seed(121)
mean_change_huber <- function (train_data, test_data, col_index_x, col_index_y) {
  
  set.seed(42)
  
  y <- train_data[,col_index_y]
  
  R = rlm(y ~ train_data[,col_index_x] -1, psi=psi.huber)[[1]]
  
  yhat<- R*test_data[,col_index_x] #predicted values of yhat
  
  yimp <- c(train_data[,col_index_y], yhat)
  
  return(list("Mean of imputed values"=mean(yhat), "Mean after imputation"=mean(yimp)))
  
  
  
}

predictor_employyes_huber <- list(mean_change_huber(train_data, test_data, 3, 5), # when number of employees is used as predictor variable 
                                mean_change_huber(train_data, test_data,3,1),
                                mean_change_huber(train_data, test_data,3,6),
                                mean_change_huber(train_data, test_data,3,4))

predictor_employyes_huber

predictor_turnover_huber <- list(mean_change_huber(train_data, test_data,2,5), # when turnover is used as predictor variable 
                                 mean_change_huber(train_data, test_data,2,1), 
                                 mean_change_huber(train_data, test_data,2,6),
                                 mean_change_huber(train_data, test_data,2,4))

predictor_turnover_huber

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### Tukey
# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(121)

mean_change_tukey <- function (train_data, test_data, col_index_x, col_index_y) {
  
  set.seed(42)
  
  y <- train_data[,col_index_y]
  
  R = rlm(y ~ train_data[,col_index_x] -1, psi=psi.bisquare, maxit=20000)[[1]]
  
  yhat<- R*test_data[,col_index_x] #predicted values of yhat
  
  yimp <- c(train_data[,col_index_y], yhat)
  
  return(list("Mean of imputed values"=mean(yhat), "Mean after imputation"=mean(yimp)))
  
  
}

predictor_employyes_tukey <- list(mean_change_tukey(train_data, test_data, 3, 5), # when number of employees is used as predictor variable 
                                  mean_change_tukey(train_data, test_data,3,1),
                                  mean_change_tukey(train_data, test_data,3,6),
                                  mean_change_tukey(train_data, test_data,3,4))

predictor_employyes_tukey

predictor_turnoevr_tukey <- list(mean_change_tukey(train_data, test_data,2,5), # when turnover is used as predictor variable 
                                     mean_change_tukey(train_data, test_data,2,1), 
                                     mean_change_tukey(train_data, test_data,2,6),
                                     mean_change_tukey(train_data, test_data,2,4))

predictor_turnoevr_tukey

#####################################################################################
### Dutch Healthcare Dataset 
#####################################################################################

dat<-read.csv2("healthcare.csv")
dat <- dat[-c(1,4,10,11)] # Selecting relevant variables
dat <- dat[-422,] # Excluding main outlier
str(dat)
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
### Wls
# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(121)

error_n_turnover <- list(performance.mean(train_data, test_data, 1, 5), # when turnover is used as predictor variable 
                         performance.mean(train_data, test_data,1,3),
                         performance.mean(train_data, test_data,1,6),
                         performance.mean(train_data, test_data,1,4),
                         performance.mean(train_data, test_data,1,7))

error_n_turnover


mean_employees <- list(mean_change_wls(train_data, test_data,2,5), # when number of employees is used as predictor variable 
                            mean_change_wls(train_data, test_data,2,1), 
                            mean_change_wls(train_data, test_data,2,6),
                            mean_change_wls(train_data, test_data,2,4),
                            mean_change_wls(train_data, test_data,2,7))

mean_employees 

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### Huber
# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(121)

performance.huber <- mean_change_huber

mean_n_turnover_huber <- list(performance.huber(train_data, test_data, 1, 5), # when turnover is used as predictor variable 
                               performance.huber(train_data, test_data,1,3),
                               performance.huber(train_data, test_data,1,6),
                               performance.huber(train_data, test_data,1,4),
                               performance.huber(train_data, test_data,1,7))

mean_n_turnover_huber

mean_employyes_huber <- list(performance.huber(train_data, test_data,2,5), # when number of employees is used as predictor variable 
                              performance.huber(train_data, test_data,2,3), 
                              performance.huber(train_data, test_data,2,6),
                              performance.huber(train_data, test_data,2,4),
                              performance.huber(train_data, test_data, 2, 7))

mean_employyes_huber

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### Tukey
# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(121)
performance.tukey <- mean_change_tukey 

mean_n_turnover_tukey <- list(performance.tukey(train_data, test_data, 1, 5), # when turnover is used as predictor variable 
                               performance.tukey(train_data, test_data,1,3),
                               performance.tukey(train_data, test_data,1,6),
                               performance.tukey(train_data, test_data,1,4),
                               performance.tukey(train_data, test_data,1,7))

mean_n_turnover_tukey

mean_employees_tukey <- list(performance.tukey(train_data, test_data,2,5), # when number of employees is used as predictor variable 
                              performance.tukey(train_data, test_data,2,3), 
                              performance.tukey(train_data, test_data,2,6),
                              performance.tukey(train_data, test_data,2,4),
                              performance.tukey(train_data, test_data,2,7))

mean_employees_tukey

