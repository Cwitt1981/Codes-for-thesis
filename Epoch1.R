# The primary data for the thesis #####
library(glmnet)
library(ggplot2)
library(reshape2)
library(caret)
library(gridExtra)
library(ipflasso)
library(corrplot)
library(dplyr)
library(tibble)
library(MASS)
library(ggplot2)
library(ggforce)
library(foreign)
library(psych)
library(palmerpenguins)
library(tidyverse)
library(ggh4x)
## Data from epoch 1 and 350 ms prior to stimulus onset
# EEG_1
# there are 90 obs pr. channel (64 channels)

DF_epoch1<-data.frame(EEG_1$eeg,EEG_1$Channel)

# make a loop to reconstruct data set EEG_1
#We only look at channels 3 to 32, A3 to A32

channels<-1:64
NewData<-matrix(data=NA,ncol=64,nrow=90)
for (i in channels){
  NewData[,i]<-subset(DF_epoch1[,1],DF_epoch1$EEG_1.Channel==i)
}
NewData1<-as.data.frame((NewData))


## remove the last observation in Newdata1 and rename to NewdataX
NewDataX<-NewData1[1:89,]

# create the responses (the 64 differenced time series) 

NewDataY<-matrix(data=NA,ncol=64,nrow=89)
for (i in channels){
  NewDataY[,i]<-diff(NewData1[,i])
}
NewDataY<-as.data.frame((NewDataY))

names(NewDataY)<-paste0("DX",1:ncol(NewDataY))



# we remove channel 2, 33, 34, 44 and 64 for both the y data and the x data
NewDataY<-NewDataY[,-c(2,33,34,44,64)]
NewDataX<-NewDataX[,-c(2,33,34,44,64)]

Y1<-as.matrix(NewDataY)
X1<-as.matrix(NewDataX)


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#       Split the data into training and test data
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

set.seed(101)
# Set Seed so that same sample can be reproduced in future also
# Now Selecting 70% of data as sample from total 'n' rows of the data  
df<-data.frame(Y1,X1)
sample <- sample.int(n = nrow(df), size = floor(.70*nrow(df)), replace = F)
train <- df[sample, ]

test  <- df[-sample, ]


# Split the responses and the predictors in the two data sets
ytrain<-as.matrix(train[,c(1:59)])
xtrain<-as.matrix(train[,c(60:118)])
ytest<-as.matrix(test[,c(1:59)])
xtest<-as.matrix(test[,c(60:118)])

