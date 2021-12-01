# Bootstrapping
## bootstrapping the lasso

TRAIN<-as.matrix(train)
B<-1000
n<-nrow(TRAIN)
L<-NULL
MSE_boot<-NULL
for (b in 1:B){
  i<-sample(n,n,replace=TRUE)
  for (j in 1:59){
    bootGlmNet<-glmnet(TRAIN[,60:118][i,],TRAIN[i,j],intercept = T, family = "gaussian",
                       type.measure="mse", alpha = 1, lambda = 0.3859196)
    L<- cbind(L,coef(bootGlmNet,s=0.3859196)[2:60])
  }}

#Calculate the average Lasso network
# L is the saved boostapped estimates for Pi_lasso
AveragePI<-matrix(NA,ncol=59,nrow=59)
for (i in 1:59){
  for (j in 1:59){
    AveragePI[i,j]<-mean(L[i,seq(j, ncol(L), 59)])
    
  } 
}

# standard deviation of the bootstrapped PI_lasso
SD<-matrix(NA,ncol=59,nrow=59)
for (i in 1:59){
  for (j in 1:59){
    SD[i,j]<-sd(L[i,seq(j, ncol(L), 59)])
  } 
}

# CI for Lasso @@@@@@@@@@@@@@@@@@@@@@@@@@  "percentile method"
S_Lasso<-matrix(NA, nrow = 59, ncol = 59)
for (i in 1:59){
  for (j in 1:59){
    lower<-quantile(L[i,seq(j, ncol(L), 59)],0.05)
    upper<-quantile(L[i,seq(j, ncol(L), 59)],0.95)
    S_Lasso[i,j]<-between(0, lower, upper)
  }}

################################################################
#   ridge regression                                           #
################################################################

#Now bootstrapping for the ridge regression
TRAIN<-as.matrix(train)
B<-1000
n<-nrow(TRAIN)
R<-NULL
for (b in 1:B){
  i<-sample(n,n,replace=TRUE)
  for (j in 1:59){
    bootRidge<-glmnet(TRAIN[,60:118][i,],TRAIN[i,j],intercept = T, family = "gaussian",
                      type.measure="mse", alpha = 0, lambda = 1.279407)
    R<- cbind(R,coef(bootRidge,s="lambda.min")[2:60])
  }}

AveragePI_Ridge<-matrix(NA,ncol=59,nrow=59)
for (i in 1:59){
  for (j in 1:59){
    AveragePI_Ridge[i,j]<-mean(R[i,seq(j, ncol(R), 59)])
    
  } 
}

## standard deviation for ridge regression
SDRidge<-matrix(NA,ncol=59,nrow=59)
for (i in 1:59){
  for (j in 1:59){
    SDRidge[i,j]<-sd(R[i,seq(j, ncol(R), 59)])
    
  } 
}

# CI for Ridge regression @@@@@@@@@@@@@@@@@@@@@@@@@@  "percentile method"
S_Ridge<-matrix(NA, nrow = 59, ncol = 59)
for (i in 1:59){
  for (j in 1:59){
    lower<-quantile(R[i,seq(j, ncol(R), 59)],0.05)
    upper<-quantile(R[i,seq(j, ncol(R), 59)],0.95)
    S_Ridge[i,j]<-between(0, lower, upper)
  }}


##########################################################################################
#                elastic net alpha=0.5                                                   #
##########################################################################################
#Bootstrapping for the elastic-net with alpha=0.5
TRAIN<-as.matrix(train)
B<-1000
n<-nrow(TRAIN)
E<-NULL
for (b in 1:B){
  i<-sample(n,n,replace=TRUE)
  for (j in 1:59){
    bootElas05<-glmnet(TRAIN[,60:118][i,],TRAIN[i,j],intercept = T, family = "gaussian",
                       type.measure="mse", alpha = 0.5, lambda =0.4285182)
    E<- cbind(PI_bootElas05,coef(E,s=0.4285182)[2:60])
    }}

#Calculate the average  elastic net 0.5 network
AveragePI_Elas05<-matrix(NA,ncol=59,nrow=59)
for (i in 1:59){
  for (j in 1:59){
    AveragePI_Elas05[i,j]<-mean(E[i,seq(j, ncol(E), 59)])
  } 
}

## standard deviation for elastic net (alpha = 0.5) 
SDElas05<-matrix(NA,ncol=59,nrow=59)
for (i in 1:59){
  for (j in 1:59){
    SDElas05[i,j]<-sd(E[i,seq(j, ncol(E), 59)])
    
  } 
}

# CI for elastiic net 0.5 @@@@@@@@@@@@@@@@@@@@@@@@@@  "percentile method"
S_Elas<-matrix(NA, nrow = 59, ncol = 59)
for (i in 1:59){
  for (j in 1:59){
    lower<-quantile(E[i,seq(j, ncol(E), 59)],0.05)
    upper<-quantile(E[i,seq(j, ncol(E), 59)],0.95)
    S_Elas[i,j]<-between(0, lower, upper)
  }}

