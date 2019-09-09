# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### In this file, the ratio R has been estimated for the wls, Huber and Tukey estimators. 
#### Final estimates can be found in the R_estimates dataframe.

## Subsequentely, the goodness of fit on the final imputed mean, compared with the true observed mean, has been compared for each estimator.
### Final estimates can be found in the GoodnessOfFit dataframe.
# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


setwd("C:/Users/AGMO/Desktop")

set.seed(42)

library(MASS)

dat<-read.csv2("Data_wholesalers.csv")
str(dat)
dat <- dat[-c(1,4,5,7)]
str(dat)

# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### R estimates
# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

ratios.estimates <- function(target.variable, predictor.variable) {
  
  r.means <- mean(target.variable, na.rm=TRUE)/mean(predictor.variable, na.rm=TRUE)
  r.huber <- rlm(target.variable ~ predictor.variable -1,weight=1/(predictor.variable+1), psi=psi.huber)[[1]]
  r.turkey <- rlm(target.variable ~ predictor.variable -1,weight=1/(predictor.variable+1), psi=psi.bisquare)[[1]]
  
  r <- data.frame(r.means, r.median, r.huber, r.turkey)
  colnames(r) <- c("Means", "Huber", "Turkey")
  
  return(r)
  
}

PersonalCosts_Employees <- ratios.estimates(dat[,5], dat[,3])
CostPurchases_Turnover <- ratios.estimates(dat[,1], dat[,2])
CostDepreciations_Turnover <- ratios.estimates(dat[,6], dat[,2])
OtherCosts_Employees <- ratios.estimates(dat[,4], dat[,3])
ratios.estimates(dat_centering[,4], dat_centering[,3])

# Combines all R estimates in a dataframe

R_estimates = data.frame(matrix(data=c(PersonalCosts_Employees, CostPurchases_Turnover, CostDepreciations_Turnover, OtherCosts_Employees), ncol=4, nrow=4 , byrow=TRUE))
colnames(R_estimates) <- c("Means", "Median", "Huber", "Turkey")
rownames(R_estimates) <- c("PersonalCosts/Employees", "CostPurchases/Turnover", "CostDepreciations/Turnover", "OtherCosts/Employees ")

R_estimates
     

# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#### GOF: Goodness of fit at the aggregate level (mean)
# --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

GOF <- function(x, y, R){   
  
  p <- imputed.mean <- rep(0,ncol(R))
  
  for (i in 1:ncol(R)){
    
    imputed.mean[i] <- mean(as.numeric(R[i])*x) # Mean predicted values 
  
  mean <- mean(y, na.rm=TRUE)  # Observed mean 
  
  p[i] <-   round(100*(abs(imputed.mean[i] - mean)/mean), 3) #Relative percentage difference
  
  }
  
  return(p)
  
}

# Combines all estimates in a dataframe
GOF1 <- GOF(dat[,3], dat[,5], PersonalCosts_Employees)
GOF2 <- GOF(dat[,2], dat[,1], CostPurchases_Turnover)
GOF3 <- GOF(dat[,2], dat[,6], CostDepreciations_Turnover )
GOF4 <- GOF(dat[,3], dat[,4], OtherCosts_Employees)

GoodnessOfFit <- data.frame(matrix(data=c(GOF1, 
                            GOF2,
                            GOF3, 
                            GOF4), ncol=4, byrow=TRUE))

colnames(GoodnessOfFit) <- c("Means", "Huber", "Turkey")
rownames(GoodnessOfFit) <- c("PersonalCosts/Employees", "CostPurchases/Turnover", "CostDepreciations/Turnover", "OtherCosts/Employees ")

GoodnessOfFit







  
 