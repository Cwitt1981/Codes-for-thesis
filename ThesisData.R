# This is the source file ##
### Thesis Data #####
setwd("C:/Users/camil/OneDrive/Dokumenter/Documents for Thesis 2021/")

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



patient <- 1
# Loading the dataset with EEG signal
library(R.matlab)
data_file <- paste("S", patient, "_clean.mat", sep = "")
path <- paste("C:/Users/camil/OneDrive/Dokumenter/Documents for Thesis 2021/",
              data_file, sep = "")
pathname <- file.path(path)
data <- readMat(pathname)
# Rearranging the data frame 'data'
EEG.data <- data$EEGdata
EEG.times <- data$EEGtimes
rejected <- as.vector(data$EEGrejEpochs)
chan.labels <- c(paste("A", 1:32, sep = ""), paste("B", 1:32, sep = ""))
n3 <- dim(EEG.data)[3]
EEG <- data.frame(time = rep(EEG.times, n3*64),
                  eeg = matrix(aperm(EEG.data, c(2, 3, 1)), ncol = 1),
                  Epoch = factor(rep(rep(1:n3, each = 640), 64)),
                  Channel = rep(1:64, each = 640*n3))
EEG$Channel.name <- chan.labels[EEG$Channel]

#----------------------------------------------------------------------------------
# extract data EEG for participant 1, using only trial 20 in the 
# interval 300-0 ms before the stimulus onset  
#----------------------------------------------------------------------------------

EEG_20<-subset(EEG, EEG$Epoch == "20" & EEG$time >= -350 & EEG$time<=0)
# there are 90 obs pr. channel (64 channels)

DF<-data.frame(EEG_20$eeg,EEG_20$Channel)


# make a loop to reconstruct data set EEG_20 
#We only look at channels 3 to 32, A3 to A32

channels<-1:64
NewData<-matrix(data=NA,ncol=64,nrow=90)
for (i in channels){
  NewData[,i]<-subset(DF[,1],DF$EEG_20.Channel==i)
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

Y20<-as.matrix(NewDataY)
X20<-as.matrix(NewDataX)


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#       Split the data into training and test data
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

set.seed(101)
# Set Seed so that same sample can be reproduced in future also
# Now Selecting 70% of data as sample from total 'n' rows of the data  
df<-data.frame(Y20,X20)
sample <- sample.int(n = nrow(df), size = floor(.70*nrow(df)), replace = F)
train <- df[sample, ]

test  <- df[-sample, ]


# Split the responses and the predictors in the two data sets
ytrain<-as.matrix(train[,c(1:59)])
xtrain<-as.matrix(train[,c(60:118)])
ytest<-as.matrix(test[,c(1:59)])
xtest<-as.matrix(test[,c(60:118)])

