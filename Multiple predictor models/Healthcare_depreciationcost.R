# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### In this file, all models (excluding the classical ratio model) for the DV depreciation cost from the Dutch Healthcare are fitted.
##### In particular, 6 different models are considered: boosted GAM, boosted Regression Tree, boosted Huber, boosted Tukey, non-boosted Huber and non-boosted Tukey.
####### Each of these models has been fitted with one, two or all predictors.
######### The order in which they are presented is: All models with 1 predictor, All models with 2 predictors, All models with all predictors.
########## For the boosted-Huber model, variable importance has been assessed with the varimp() function.
########### If follows an accuracy comparison of all fitted models (based on RMSE and MAPE) and a summary graph.
############# The final accuracy table is printed as xtable (xtable package) to ease its convertion in Latex 
# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

### Load packages
## -----------------------------------------------------------------------
library(caret)
library(tidyverse)
library(np)
library(mboost)
library(xtable)
library(reshape2)

### Data reading
## -----------------------------------------------------------------------
setwd("C:/Users/AGMO/Desktop/Final code Thesis/Data")

dat<-read.csv2("healthcare.csv")
dat <- dat[-c(1,4,10,11)] # Selecting relevant variables

dat_no_outliers <- dat[complete.cases(dat),]
attach(dat_no_outliers)

colnames(dat_no_outliers)[colnames(dat_no_outliers)=="Depreciations"] <- "y" # Defining outcome variable

dat_no_outliers = dat_no_outliers[-c(422), ] 
str(dat)


### Train-Test split
## ------------------------------------------------------------------------
set.seed(121)
idx <- seq(nrow(dat_no_outliers))
idx_train <- sample(idx, size = floor(nrow(dat_no_outliers) * 0.75), replace = FALSE) 
idx_test <- idx[!idx %in% idx_train]

train_data  <- dat_no_outliers[idx_train,] 
test_data <- dat_no_outliers[idx_test,] 

### Tukey and Huber ad-hoc functions (pre-tested in the file Boosted Huber and Tukey implementation.R)
## --------------------------------------------------------------------------------------------------------------------

Tukey <- function(d = NULL) {
  
  mc <- match.call()
  
  if (length(mc) == 2)
    
    dtxt <- deparse(mc[[2]])
  
  else
    
    dtxt <- NULL
  
  fit <- 0
  
  Family(ngradient = function(y, f, w = 1) {
    
    if (is.null(d)) d <- 4.685*sd(y - fit)
    
    fit <<- f
    
    ifelse((a <- abs(y - f)) < d, (y-f)*(1-((y-f)^2)/(d^2))^2, 0)
    
  },
  
  loss = function(y,f) {
    
    if (is.null(d)) 
      
      d <- 4.685*sd(y - fit)
    
    ifelse((a <- abs(y - f)) < d, (d^2/6)*(1-(1-((y-f)/d)^2)^3), (d^2)/6)
    
  },
  
  check_y = function(y) {
    
    if (!is.numeric(y) || !is.null(dim(y)))
      
      stop("response is not a numeric vector but ",
           
           sQuote("family = Tukey()"))
    
    y
    
  },
  
  name = paste("Tukey Error",
               
               ifelse(is.null(d), "(with adaptive d)",
                      
                      paste("(with d = ", dtxt, ")", sep = ""))),
  
  response = function(f) f)
  
}


Huber_2 <- function(d = NULL) {
  mc <- match.call()
  if (length(mc) == 2)
    dtxt <- deparse(mc[[2]])
  else
    dtxt <- NULL
  fit <- 0
  Family(ngradient = function(y, f, w = 1) {
    if (is.null(d)) d <- 1.345*sd(y - fit)
    fit <<- f
    ifelse(abs(y - f) < d, y - f, d * sign(y - f))
  },
  loss = function(y, f) {
    if (is.null(d)) d <- 1.345*sd(y - fit)
    ifelse((a <- abs(y - f)) < d, a^2/2, d*(a - d/2))
  },
  check_y = function(y) {
    if (!is.numeric(y) || !is.null(dim(y)))
      stop("response is not a numeric vector but ",
           sQuote("family = Huber()"))
    y
  },
  name = paste("Huber Error",
               ifelse(is.null(d), "(with adaptive d)",
                      paste("(with d = ", dtxt, ")", sep = ""))),
  response = function(f) f)
}


mape <- function(y_true, y_pred){
  mean(abs(y_pred-y_true))/100
}

#####################################################################################
### One predictor models
#####################################################################################

## GAM model
## ------------------------------------------------------------------------
set.seed(121)

grid=data.frame(mstop=100, prune=FALSE)

caret_gamboost_employees <- train(y ~ Employees, data=train_data, trControl=train_control, method = 'gamboost', tuneGrid=grid)

caret_gamboost_employees$results

predictions_caret_gamboost_employees <- predict(caret_gamboost_employees, newdata = test_data)

## Regression Tree
## ------------------------------------------------------------------------
set.seed(121)

grid2=data.frame(mstop=100)


caret_blackboost_employees <- train(y ~ Employees, data=train_data, trControl=train_control, method = 'blackboost')

caret_blackboost_employees$results

predictions_caret_blackboost_employees <- predict(caret_blackboost_employees, newdata = test_data)

## Boosted Huber 
## ------------------------------------------------------------------------

set.seed(121)

caret_huber_employees <- glmboost(y~ Employees, data=train_data, family=Huber_2())

predictions_caret_huber_employees<- predict(caret_huber_employees, newdata = test_data)

## Boosted Tukey
## ------------------------------------------------------------------------

set.seed(121)

caret_tukey_employees <- glmboost(y~ Employees, data=train_data, family=Tukey())

predictions_caret_tukey_employees<- predict(caret_tukey_employees, newdata = test_data)

## Non-boosted Huber
## ------------------------------------------------------------------------
grid = data.frame(psi="psi.huber", intercept=FALSE)

set.seed(121)

caret_huber_rlm1 <- train(y ~ Employees, data=train_data, trControl=train_control, method = 'rlm', tuneGrid=grid)

predictions_caret_huber_rlm1 <- predict(caret_huber_rlm1, newdata = test_data)

## Non-boosted Tukey
## ------------------------------------------------------------------------
grid2 = data.frame(psi="psi.bisquare", intercept=FALSE)

set.seed(121)

caret_tukey_rlm1 <- train(y ~ Employees, data=train_data, trControl=train_control, method = 'rlm', tuneGrid=grid2)

predictions_caret_tukey_rlm1 <- predict(caret_tukey_rlm1, newdata = test_data)


#####################################################################################
### Two predictors models
#####################################################################################

## GAM
## ----------------------------------------------------------------
set.seed(121)

caret_gamboost <- train(y ~ Employees + Turnover, data=train_data,  method = 'gamboost', tuneGrid=grid)

caret_gamboost$results

predictions_caret_gamboost <- predict(caret_gamboost, newdata = test_data)

## Regression Tree
## ------------------------------------------------------------------------
set.seed(121)

caret_blackboost <- train(y ~ Employees + Turnover, data=train_data, trControl=train_control, method = 'blackboost')

caret_blackboost$results

predictions_caret_blackboost <- predict(caret_blackboost, newdata = test_data)

## Boosted Huber
## ------------------------------------------------------------------------

set.seed(121)

caret_huber_2 <- glmboost(y~ Employees+Turnover, data=train_data, family=Huber_2())

predictions_caret_huber_2<- predict(caret_huber_2, newdata = test_data)

## Boosted Tukey
## ------------------------------------------------------------------------

set.seed(121)

caret_tukey_2 <- glmboost(y~ Employees+Turnover, data=train_data, family=Tukey())

predictions_caret_tukey_2<- predict(caret_tukey_2, newdata = test_data)

## Non-boosted Huber
## ------------------------------------------------------------------------
grid = data.frame(psi="psi.huber", intercept=FALSE)

set.seed(121)

caret_huber_rlm2 <- train(y ~ Employees+Turnover, data=train_data, trControl=train_control, method = 'rlm', tuneGrid=grid)

predictions_caret_huber_rlm2 <- predict(caret_huber_rlm2, newdata = test_data)

## Non-boosted Tukey
## ------------------------------------------------------------------------
grid2 = data.frame(psi="psi.bisquare", intercept=FALSE)

set.seed(121)

caret_tukey_rlm2 <- train(y ~ Employees+Turnover, data=train_data, trControl=train_control, method = 'rlm', tuneGrid=grid2)

predictions_caret_tukey_rlm2 <- predict(caret_tukey_rlm2, newdata = test_data)

#####################################################################################
### All predictors models
#####################################################################################

## GAM
## -------------------------------------------------------------------------
set.seed(121)

caret_gamboost2 <- train(y ~. , data=train_data, method = 'gamboost', tuneGrid=grid)

caret_gamboost2$results

predictions_caret_gamboost2 <- predict(caret_gamboost2, newdata = test_data)

## Regression Tree
## ------------------------------------------------------------------------
set.seed(121)

caret_blackboost2 <- train(y ~ ., data=train_data, trControl=train_control, method = 'blackboost')

caret_blackboost2$results

predictions_caret_blackboost2 <- predict(caret_blackboost2, newdata = test_data)

## Boosted-Huber
## -------------------------------------------------------------------------
set.seed(121)

caret_huber_all <- glmboost(y~.-1, data=train_data, family=Huber_2())

predictions_caret_huber_all <- predict(caret_huber_all, newdata = test_data)

plot(varimp(caret_huber_all), xlab="Risk reduction") # Variable Importance plot

## Boosted-Tukey
## ------------------------------------------------------------------------
set.seed(121)

caret_tukey_all <- glmboost(y~., data=train_data, family=Tukey())

predictions_caret_tukey_all <-  predict(caret_tukey_all, newdata = test_data)

## Non-boosted Huber
## ------------------------------------------------------------------------
grid = data.frame(psi="psi.huber", intercept=FALSE)

set.seed(121)

caret_huber_rlm_all <- train(y ~ ., data=train_data, trControl=train_control, method = 'rlm', tuneGrid=grid)

predictions_caret_huber_rlm_all <- predict(caret_huber_rlm_all, newdata = test_data)

## Non-boosted Tukey
## ------------------------------------------------------------------------
grid2 = data.frame(psi="psi.bisquare", intercept=FALSE)

set.seed(121)

caret_tukey_rlm_all <- train(y ~ ., data=train_data, trControl=train_control, method = 'rlm', tuneGrid=grid2)

predictions_caret_tukey_rlm_all <- predict(caret_tukey_rlm_all, newdata = test_data)

mean(c(predictions_caret_tukey_rlm_all, train_data[,"y"])) ### Compute post-imputation mean


## ------------------------------------------------------------------------
## Accuracy measures: One predictor
## ------------------------------------------------------------------------

accuracy_onepredictor <- rbind(postResample(pred = predictions_caret_gamboost_employees, obs = test_data$y), 
                               postResample(pred = predictions_caret_blackboost_employees, obs = test_data$y), 
                               postResample(pred = predictions_caret_glmboost_employees, obs = test_data$y))

accuracy_onepredictor <- accuracy_onepredictor[,-2]
accuracy_onepredictor[,2] <- accuracy_onepredictor[,2]/100
colnames(accuracy_onepredictor) <- c("RMSE", "MAPE")

huber=cbind(rmse(test_data$y, predictions_caret_huber_employees), mape(test_data$y, predictions_caret_huber_employees))
tukey=cbind(rmse(test_data$y, predictions_caret_tukey_employees), mape(test_data$y, predictions_caret_tukey_employees))

accuracy_onepredictor <- rbind(accuracy_onepredictor, huber, tukey)

row.names(accuracy_onepredictor) <- c("Generalized Additive Model", "Regression Tree", "Generalized Linear Model", "Huber", "Tukey")
accuracy_onepredictor

## ------------------------------------------------------------------------
## Accuracy measures: two predictors
## ------------------------------------------------------------------------
accuracy_twopredictors <- rbind(postResample(pred = predictions_caret_gamboost, obs = test_data$y), 
                                postResample(pred = predictions_caret_blackboost, obs = test_data$y), 
                                postResample(pred = predictions_caret_glmboost, obs = test_data$y))

accuracy_twopredictors <- accuracy_twopredictors[,-2]
accuracy_twopredictors[,2] <- accuracy_twopredictors[,2]/100
colnames(accuracy_twopredictors) <- c("RMSE", "MAPE")

huber2=cbind(rmse(test_data$y, predictions_caret_huber_2), mape(test_data$y, predictions_caret_huber_2))
tukey2=cbind(rmse(test_data$y, predictions_caret_tukey_2), mape(test_data$y, predictions_caret_tukey_2))

accuracy_twopredictors <- rbind(accuracy_twopredictors, huber2, tukey2)

row.names(accuracy_twopredictors) <- c("Generalized Additive Model", "Regression Tree", "Generalized Linear Model", "Huber", "Tukey")

## ------------------------------------------------------------------------
## Accuracy measures: all predictors
## ------------------------------------------------------------------------
accuracy <- rbind(postResample(pred = predictions_caret_gamboost2, obs = test_data$y), 
                  postResample(pred = predictions_caret_blackboost2, obs = test_data$y) ,
                  postResample(pred = predictions_caret_glmboost2, obs = test_data$y))
accuracy<- accuracy[,-2]
accuracy[,2] <- accuracy[,2]/100

colnames(accuracy) <- c("RMSE", "MAPE")

huber3=cbind(rmse(test_data$y, predictions_caret_huber_all), mape(test_data$y, predictions_caret_huber_all))
tukey3=cbind(rmse(test_data$y, predictions_caret_tukey_all), mape(test_data$y, predictions_caret_tukey_all))

accuracy <- rbind(accuracy, huber3, tukey3)

row.names(accuracy) <- c("Generalized Additive Model", "Regression Tree", "Generalized Linear Model", "Huber", "Tukey")

## ------------------------------------------------------------------------
## Accuracy measures: non-boosted robus models
## ------------------------------------------------------------------------

accuracy_rlm <- rbind(postResample(pred = predictions_caret_huber_rlm1, obs = test_data$y), 
                      postResample(pred = predictions_caret_tukey_rlm1, obs = test_data$y))

accuracy_rlm<- accuracy_rlm[,-2]
accuracy_rlm[,2] <- accuracy_rlm[,2]/100

colnames(accuracy_rlm) <- c("RMSE", "MAPE")

accuracy_robustness_1 <- rbind(accuracy_rlm, huber, tukey)

accuracy_complete_onepredictor <- rbind(accuracy_onepredictor, accuracy_rlm)

accuracy_rlm2 <- rbind(postResample(pred = predictions_caret_huber_rlm2, obs = test_data$y), 
                       postResample(pred = predictions_caret_tukey_rlm2, obs = test_data$y))

accuracy_rlm2<- accuracy_rlm2[,-2]
accuracy_rlm2[,2] <- accuracy_rlm2[,2]/100

colnames(accuracy_rlm2) <- c("RMSE", "MAPE")

accuracy_robustness_2 <- rbind(accuracy_rlm2, huber2, tukey2)

accuracy_complete_twopredictors <- rbind(accuracy_twopredictors, accuracy_rlm2)                  


accuracy_rlm3 <- rbind(postResample(pred = predictions_caret_huber_rlm_all, obs = test_data$y), postResample(pred = predictions_caret_tukey_rlm_all, obs = test_data$y))

accuracy_rlm3<- accuracy_rlm3[,-2]
accuracy_rlm3[,2] <- accuracy_rlm3[,2]/100

colnames(accuracy_rlm3) <- c("RMSE", "MAPE")

accuracy_robustness_3 <- rbind(accuracy_rlm3, huber3, tukey3)

accuracy_complete_allpredictors <- rbind(accuracy, accuracy_rlm3)    

row.names(accuracy_complete_onepredictor) <- c("Generalized Additive Model*", "Regression Tree*", "Generalized Linear Model*", "Huber*", "Tukey*", "Huber", "Tukey")
row.names(accuracy_complete_twopredictors) <- c("Generalized Additive Model*", "Regression Tree*", "Generalized Linear Model*", "Huber*", "Tukey*", "Huber", "Tukey")
row.names(accuracy_complete_allpredictors) <- c("Generalized Additive Model*", "Regression Tree*", "Generalized Linear Model*", "Huber*", "Tukey*", "Huber", "Tukey")

## ------------------------------------------------------------------------
## Accuracy measures: graph creation
## ------------------------------------------------------------------------
melt.df <- melt(list(accuracy_complete_onepredictor, accuracy_complete_twopredictors, accuracy_complete_allpredictors), id.vars=c("RMSE", "MAPE"))

RMSE_toplot <- subset(melt.df, Var2=="RMSE")

RMSE_toplot$L1 <- as.character(RMSE_toplot$L1)

ggplot(RMSE_toplot, aes(L1, y=value, col=Var1)) + geom_point() + geom_line(aes(group=Var1)) + labs(y="RMSE", x=element_blank(), color="Model") + scale_x_discrete(labels=c("One predictor", "Two predictors", "All variables")) 

MAE_toplot <- subset(melt.df, Var2=="MAPE")

MAE_toplot$L1 <- as.character(MAE_toplot$L1)

ggplot(melt.df, aes(as.character(L1), y=value, col=Var1)) + geom_point() + geom_line(aes(group=Var1))+ scale_x_discrete(labels=c("One predictor", "Two predictors", "All variables")) + facet_wrap(~Var2, scales = "free_y") + labs(x=element_blank(), y=element_blank(), color="Model") + theme_grey() 

ggsave('Healthcare_depreciation.png', plot=last_plot(), width=10, height=5.5)


## ------------------------------------------------------------------------
######## Printing of overall accuracy
## ------------------------------------------------------------------------
accuracy_depr <- cbind(round(accuracy_onepredictor,3), round(accuracy_twopredictors,3), round(accuracy,3))
accuracy_rlm_final <- cbind(round(accuracy_robustness_1, 3), round(accuracy_robustness_2, 3), round(accuracy_robustness_3, 3))

colnames(accuracy_depr) <- c("RMSE_1", "MAPE_1","RMSE_2", "MAPE_2", "RMSE", "MAPE") 
accuracy_depr <- accuracy_depr[,c("RMSE_1", "RMSE_2","RMSE","MAPE_1", "MAPE_2", "MAPE")]

colnames(accuracy_rlm_final) <- c("RMSE_1", "MAPE_1","RMSE_2", "MAPE_2", "RMSE", "MAPE") 
accuracy_rlm_final <- accuracy_rlm_final[,c("RMSE_1", "RMSE_2","RMSE","MAPE_1", "MAPE_2", "MAPE")]

row.names(accuracy_rlm_final) <- c("Huber", "Tukey", "Boosted Huber", "Boosted Tukey")

print(xtable(accuracy_depr, type="latex", file="Healthcare_depreciation.tex")) # boosted models' accuracy
print(xtable(accuracy_rlm_final, type="latex", file="Healthcare_depreciation.tex")) # non-boosted models' accuracy