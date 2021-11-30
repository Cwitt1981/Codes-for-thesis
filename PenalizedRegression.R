# We will look at epoch 20 with 90  observations in the interval 350 ms before the
# stimulus onset and apply only the train data set which contains 62 observations.
# xtrain is all the predictors.
# ytrain is all the responses.

# First determine the range of the $\lambda$ grid.
## try lasso in order to determine the range of lambda

Lam_maxLasso<-vector()
Lam_minLasso<-vector()
Lam_hatLasso<-vector()
for (i in 1:59){
  for (j in 1:25){
    lasso <- cv.glmnet(xtrain, ytrain[,i], intercept = T, family = "gaussian",
                       type.measure="mse", alpha = 1, nfolds=5) 
    Lam_maxLasso[i]<-max(lasso$lambda)
    Lam_minLasso[i]<-min(lasso$lambda)
    Lam_hatLasso[i]<-lasso$lambda.min
  }}

# lambda_max
max(Lam_maxLasso)
# lambda_min
min(Lam_minLasso)

## The lambda sequence based on lambda_max and lambda_min for all 59 responses for lasso.
lambda_grid <- 10^{seq(0.9581285, -4.264623, length.out = 200)}

WE will train the lasso model with the chosen $\lambda$ sequence performing 5-fold cross-validation (repeat 10 times in order to reduce the randomness)

# LASSO ###########################################################################
MSEs <- NULL
for (j in 1:59){
  for (i in 1:25){
    set.seed(i+25)
    cv <- cv.glmnet(xtrain,ytrain[,j],intercept = T, family = "gaussian",
                    type.measure="mse", alpha = 1, lambda = lambda_grid, nfolds = 5)  
    MSEs <- cbind(MSEs, cv$cvm)
  }
}

# according to the 5-fold cross-validation the $\lambda=0.3859196$ that 
#minimizes the total MSE across the 59 channels. 

# the optimal lambda
lambda_hat<-0.3859196

#predict for lasso with opt. lambda
Y_hat<-matrix(NA, nrow = 27, ncol = 59 )
Pi_lasso <- matrix(NA, nrow = 59, ncol = 59)
for (i in 1:59){
  lassofit <- glmnet(xtrain,ytrain[,i],intercept = T, family = "gaussian",
                     type.measure="mse", alpha = 1, lambda = lambda_hat)  
  Pi_lasso[,i] <- coef(lassofit)[-1] 
  Y_hat[,i]<-predict(lassofit,s=lambda_hat, xtest)
}

# Calculate the MSE for lasso
# Function for MSE
MSE <- function(pred, y.test) {
  stopifnot(length(pred) == length(y.test))
  mean((pred - y.test)^2)
}

MSE_sum<-rep(0,59)
for (i in 1:59){
  MSE_sum[i]<-MSE(Y_hat[,i],ytest[,i])
  
}
MSE_totalTest<-mean(MSE_sum)
MSE_totalTest

# MSE_train
Y_hatTrain<-matrix(NA, nrow = 62, ncol = 59 )
#Pi_lasso <- matrix(NA, nrow = 59, ncol = 59)
for (i in 1:59){
  lassofit1 <- glmnet(xtrain,ytrain[,i],intercept = T, family = "gaussian",
                      type.measure="mse", alpha = 1, lambda = lambda_hat)  
  
  Y_hatTrain[,i]<-predict(lassofit1,s=lambda_hat, xtrain)
}

MSE_sumTrain<-rep(0,59)
for (i in 1:59){
  MSE_sumTrain[i]<-MSE(Y_hatTrain[,i],ytrain[,i])
  
}
MSE_totalTrain<-mean(MSE_sumTrain)
MSE_totalTrain


#######################################################################################
#                        Ridge                                                        #
#######################################################################################
##  Ridge, try in order to determine the range of lambda
set.seed(10)
Lambda_maxR<-vector()
Lambda_minR<-vector()
Lambda_hatR<-vector()
for (i in 1:59){
  ridge <- cv.glmnet(xtrain, ytrain[,i], intercept = T, family = "gaussian",
                     type.measure="mse", alpha = 0, nfolds=5) 
  Lambda_maxR[i]<-max(ridge$lambda)
  Lambda_minR[i]<-min(ridge$lambda)
  Lambda_hatR[i]<-ridge$lambda.min
}

lambda_gridRidge<-10^seq(3.958129, -3.03537,length.out=200)
MSEsRidge <- NULL
for (j in 1:59){
  for (i in 1:25){
    set.seed(i+25)
    ridgecv <- cv.glmnet(xtrain,ytrain[,j],intercept = T, family = "gaussian",
                         type.measure="mse", alpha = 0, lambda = lambda_gridRidge, nfolds=5)  
    MSEsRidge <- cbind(MSEsRidge, ridgecv$cvm)
  }
}

lambda_hatRidge<-rowMeans(MSEsRidge)
which.min(lambda_hatRidge)

lambda_gridRidge<-10^seq(3.958129, -1.264623,length.out=200)

# predict for the ridge regression on the test data
Y_hatRidge<-matrix(NA, nrow = 27, ncol = 59 )
Pi_ridge<-matrix(NA, nrow = 59, ncol = 59)
for (i in 1:59){
  ridgefit <- glmnet(xtrain,ytrain[,i],intercept = T, family = "gaussian",
                     type.measure="mse", alpha = 0, lambda = 1.279407)  
  Pi_ridge[,i] <- coef(ridgefit)[-1] 
  Y_hatRidge[,i]<-predict(ridgefit,s=1.279407, xtest)
}

# And calculate the MSE for the test data for ridge
MSE_sumRidge<-rep(0,59)
for (i in 1:59){
  MSE_sumRidge[i]<-MSE(Y_hatRidge[,i],ytest[,i])
  
}
MSE_totalTestRidge<-mean(MSE_sumRidge)


#Now for the training data for ridge
Y_hatTrainRidge<-matrix(NA, nrow = 62, ncol = 59 )
#Pi_lasso <- matrix(NA, nrow = 59, ncol = 59)
for (i in 1:59){
  Ridgefit1 <- glmnet(xtrain,ytrain[,i],intercept = F, family = "gaussian",
                      type.measure="mse", alpha = 1, lambda = lambda_optRidge)  
  Pi_RidgeTrain[]<- coef(Ridgefit1)[-1] 
  Y_hatTrainRidge[,i]<-predict(Ridgefit1,s=lambda_optRidge, xtrain)
}

MSE_sumTrainRidge<-rep(0,59)
for (i in 1:59){
  MSE_sumTrainRidge[i]<-MSE(Y_hatTrainRidge[,i],ytrain[,i])
  
}
MSE_totalTrainRidge<-mean(MSE_sumTrainRidge)


##################################################################################
#   Elastic net alpha= 0.5                                                       #
##################################################################################

## try lasso in order to determine the range of lambda
set.seed(10)
Lambda_max_elas<-vector()
Lambda_min_elas<-vector()
Lambda_hat_elas<-vector()
for (i in 1:59){
  elastic <- cv.glmnet(xtrain, ytrain[,i], intercept = T, family = "gaussian",
                       type.measure="mse", alpha = 0.5, nfolds = 5) 
  Lambda_max_elas[i]<-max(elastic$lambda)
  Lambda_min_elas[i]<-min(elastic$lambda)
  Lambda_hat_elas[i]<-elastic$lambda.min
}

print(max(Lambda_max_elas))

lambda_grid_elas<-10^{seq(1.259158, -3.963593, length.out = 200)}

MSEs_elas <- NULL
for (j in 1:59){
  for (i in 1:25){
    set.seed(i+25)
    elasticcv <- cv.glmnet(xtrain,ytrain[,j],intercept = T, family = "gaussian",
                           type.measure="mse", alpha = 0.5, lambda = lambda_grid_elas,
                           nfold=5)  
    MSEs_elas <- cbind(MSEs_elas, elasticcv$cvm)
  }
}

lambda_hatelas<-rowMeans(MSEs_elas)


Y_hatTrain05<-matrix(NA, nrow = 62, ncol = 59 )
lambda_opt_elas<-0.4285182
for (i in 1:59){
  elas05train <- glmnet(xtrain,ytrain[,i],intercept = T, family = "gaussian",
                        type.measure="mse", alpha = 0.5, lambda = lambda_opt_elas)  
  
  Y_hatTrain05[,i]<-predict(elas05train,s=lambda_opt_elas, xtrain)
}

MSE_sumTrainElas05<-rep(0,59)
for (i in 1:59){
  MSE_sumTrainElas05[i]<-MSE(Y_hatTrain05[,i],ytrain[,i])
  
}
MSE_totalTrainElas05<-mean(MSE_sumTrainElas05)
MSE_totalTrainElas05


#For the elastic net we get $\lambda=0.4285182$. Now we can predict
lambda_opt_elas<-0.4285182
Y_hatElas<-matrix(NA, nrow = 27, ncol = 59 )
Pi_Elas<-matrix(NA, nrow = 59, ncol = 59)
for (i in 1:59){
  Elasticfit <- glmnet(xtrain,ytrain[,i],intercept = T, family = "gaussian",
                       type.measure="mse", alpha = 0.5, lambda = lambda_opt_elas)  
  Pi_Elas[,i] <- coef(Elasticfit)[-1] 
  Y_hatElas[,i]<-predict(Elasticfit,s=lambda_opt_elas, xtest)
}

# And calculate the MSE for the test data for elastic net
MSE_sumElastic<-rep(0,59)
for (i in 1:59){
  MSE_sumElastic[i]<-MSE(Y_hatElas[,i],ytest[,i])
}
## total MSE for the elastic net with alpha = 0.5
MSE_totalTestElastic<-mean(MSE_sumElastic)
MSE_totalTestElastic

which.min(lambda_hatelas)

##################################################################################
#   Elastic net alpha= 0.75                                                      #
##################################################################################


## try lasso in order to determine the range of lambda 
set.seed(10)
Lambda_max_elas_075<-vector()
Lambda_min_elas_075<-vector()
Lambda_hat_elas_075<-vector()
for (i in 1:59){
  elastic075 <- cv.glmnet(xtrain, ytrain[,i], intercept = T, family = "gaussian",
                          type.measure="mse", alpha = 0.75) 
  Lambda_max_elas_075[i]<-max(elastic075$lambda)
  Lambda_min_elas_075[i]<-min(elastic075$lambda)
  Lambda_hat_elas_075[i]<-elastic075$lambda.min
}

lambda_grid_elas075<-10^{seq(1.083067, -4.139684, length.out = 200)}
#Then we can perform the CV for $\alpha = 0.75$ elastic net

MSEsElastic075 <- NULL
for (j in 1:59){
  for (i in 1:25){
    set.seed(i+25)
    Elastic075cv <- cv.glmnet(xtrain,ytrain[,j],intercept = T, family = "gaussian",
                              type.measure="mse", alpha = 0.75, lambda = lambda_grid_elas075, nfolds=5)  
    MSEsElastic075 <- cbind(MSEsElastic075, Elastic075cv$cvm)
  }
}

lambda_hatElastic075<-rowMeans(MSEsElastic075)
which.min(lambda_hatElastic075)

# lambda_opt for elastic net alpha= 0.75
lambda_optElas075<-0.3637962

#The prediction for the elastic net $\alpha=0.75$
Y_hatElas075<-matrix(NA, nrow = 27, ncol = 59 )
Pi_Elas075<-matrix(NA, nrow = 59, ncol = 59)
for (i in 1:59){
  Elasticfit075 <- glmnet(xtrain,ytrain[,i],intercept = T, family = "gaussian",
                          type.measure="mse", alpha = 0.75, lambda = lambda_optElas075)  
  Pi_Elas075[,i] <- coef(Elasticfit075)[-1] 
  Y_hatElas075[,i]<-predict(Elasticfit075,s=lambda_optElas075, xtest)
}

# And calculate the MSE for the test data for elastic net
MSE_sumElastic075<-rep(0,59)
for (i in 1:59){
  MSE_sumElastic075[i]<-MSE(Y_hatElas075[,i],ytest[,i])
}
## total MSE for the elastic net with alpha = 0.5
MSE_totalTestElastic075<-mean(MSE_sumElastic075)
MSE_totalTestElastic075

#The the MSE_train for elastic net 0.75 
Y_hatTrain075<-matrix(NA, nrow = 62, ncol = 59 )
for (i in 1:59){
  elas075train <- glmnet(xtrain,ytrain[,i],intercept = T, family = "gaussian",
                         type.measure="mse", alpha = 0.75, lambda = lambda_optElas075)  
  
  Y_hatTrain075[,i]<-predict(elas075train,s=lambda_optElas075, xtrain)
}

MSE_sumTrainElas075<-rep(0,59)
for (i in 1:59){
  MSE_sumTrainElas075[i]<-MSE(Y_hatTrain075[,i],ytrain[,i])
  
}
MSE_totalTrainElas075<-mean(MSE_sumTrainElas075)

############################################################################################
#               Elastic net alpha= 0.25                                                   #
############################################################################################
lambda_grid_elas025<-10^{seq( 1.657099, -3.565652, length.out = 200)}

MSEsElastic025 <- NULL
for (j in 1:59){
  for (i in 1:25){
    set.seed(i+25)
    Elastic025cv <- cv.glmnet(xtrain,ytrain[,j],intercept = T, family = "gaussian",
                              type.measure="mse", alpha = 0.25, lambda = lambda_grid_elas025,
                              nfolds=5)  
    MSEsElastic025 <- cbind(MSEsElastic025, Elastic025cv$cvm)
  }
}

lambda_hatElastic025<-rowMeans(MSEsElastic025)
which.min(lambda_hatElastic025)

#check the test mse for 0.25
lam_opt_elas025<-0.6218783
Y_hatElas025<-matrix(NA, nrow = 27, ncol = 59 )
Pi_Elas025<-matrix(NA, nrow = 59, ncol = 59)
for (i in 1:59){
  Elasticfit025 <- glmnet(xtrain,ytrain[,i],intercept = T, family = "gaussian",
                          type.measure="mse", alpha = 0.25, lambda = lam_opt_elas025)  
  Pi_Elas025[,i] <- coef(Elasticfit025)[-1] 
  Y_hatElas025[,i]<-predict(Elasticfit025,s=lam_opt_elas025, xtest)
}

# And calculate the MSE for the test data for elastic net
MSE_sumElastic025<-rep(0,59)
for (i in 1:59){
  MSE_sumElastic025[i]<-MSE(Y_hatElas025[,i],ytest[,i])
}
## total MSE for the elastic net with alpha = 0.25
MSE_totalTestElastic025<-mean(MSE_sumElastic025)
MSE_totalTestElastic025

# train MSE for elastic net alpha= 0.25
Y_hatElasTrain025<-matrix(NA, nrow = 62, ncol = 59 )
Pi_ElasTrain025<-matrix(NA, nrow = 59, ncol = 59)
for (i in 1:59){
  ElasticfitTrain025 <- glmnet(xtrain,ytrain[,i],intercept = T, family = "gaussian",
                               type.measure="mse", alpha = 0.25, lambda = lam_opt_elas025)  
  Pi_ElasTrain025[,i] <- coef(Elasticfit025)[-1] 
  Y_hatElasTrain025[,i]<-predict(ElasticfitTrain025,s=lam_opt_elas025, xtrain)
}

# And calculate the MSE for the test data for elastic net
MSE_sumElasticTrain025<-rep(0,59)
for (i in 1:59){
  MSE_sumElasticTrain025[i]<-MSE(Y_hatElasTrain025[,i],ytrain[,i])
}
## total MSE Train for the elastic net with alpha = 0.25
MSE_totalTrainElastic025<-mean(MSE_sumElasticTrain025)
MSE_totalTrainElastic025

