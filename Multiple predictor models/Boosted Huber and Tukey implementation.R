# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### In this file, boosted Tukey and Huber models have been customized to be fitted through the mboost package.
### Huber is already implemented with as default for the constant d the median.
### Tukey loss function and objective function has been manually implemented and pre-tested with both (1) d=median; (2) d depends on residuals, as suggested in literature.
### Both Tukey and Huber have been tested on both options. The second option resulted to give better results and has therefore been used in the following scripts.
# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

library(mboost)

#####################################################################################
### Dutch Structural Business Dataset 
#####################################################################################

dat<-read.csv2("Data_wholesalers.csv")
dat <- dat[-c(1,4,5,7)]
str(dat)
dat_no_outliers = dat[-c(38, 263,  271, 103, 24), ] 
dat_no_outliers <- dat_no_outliers[complete.cases(dat_no_outliers),]

colnames(dat_no_outliers)[colnames(dat_no_outliers)=="Cost_other"] <- "y"

#######################################################################################
### Train-Test split
#######################################################################################
set.seed(121)
idx <- seq(nrow(dat_no_outliers))
idx_train <- sample(idx, size = floor(nrow(dat_no_outliers) * 0.75), replace = FALSE) 
idx_test <- idx[!idx %in% idx_train]

train_data  <- dat_no_outliers[idx_train,] 
test_data <- dat_no_outliers[idx_test,] 
#######################################################################################


#######################################################################
#### Compare choice of d in Huber 
#######################################################################

### 1: default (median)

boosted_huber <- glmboost(y~ ., data=train_data, family=Huber())
predictions_boosted_huber <- predict(boosted_huber, newdata=test_data)
rmse(test_data$y, predictions_boosted_huber)#2132.5
mape(test_data$y, predictions_boosted_huber)#805

### 2: based on residuals 

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

boosted_huber <- glmboost(y~ ., data=train_data, family=Huber_2())
predictions_boosted_huber <- predict(boosted_huber, newdata=test_data)
rmse(test_data$y, predictions_boosted_huber)#1988.693
mape(test_data$y, predictions_boosted_huber)#877

#######################################################################
#### Compare choice of d in Tukey
#######################################################################

#### 1: default (median)

Tukey_median <- function(d = NULL) {
  mc <- match.call()
  if (length(mc) == 2)
    dtxt <- deparse(mc[[2]])
  else
    dtxt <- NULL
  fit <- 0
  Family(ngradient = function(y, f, w = 1) {
    if (is.null(d)) d <- median(abs(y - fit))
    fit <<- f
    ifelse((a <- abs(y - f)) < d, (y-f)*(1-((y-f)^2)/(d^2))^2, 0)
  },
  loss = function(y,f) { 
    if (is.null(d))  
      d <- median(abs(y - fit))
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

boosted_tukey <- glmboost(y ~ ., data=train_data, family=Tukey_median()) # 1
predictions_boosted_tukey <- predict(boosted_tukey, newdata=test_data)

rmse(test_data$y, predictions_boosted_tukey) 
mape(test_data$y, predictions_boosted_tukey)

### 2: based on residuals 

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

boosted_tukey <- glmboost(y ~ ., data=train_data, family=Tukey()) # 2
predictions_boosted_tukey <- predict(boosted_tukey, newdata=test_data)

rmse(test_data$y, predictions_boosted_tukey) 
mape(test_data$y, predictions_boosted_tukey)