#load file 
dataPima <- read.csv("Pima.csv")
XPim <- dataPima[,1:7]
zPim <- dataPima[,8]

#/!\--------Estimate parameters---------/!\


data = as.data.frame(dataPima);
mu1 = apply(data[which(data$z == 1), 1:2], 2, mean);
mu2 = apply(data[which(data$z == 2), 1:2], 2, mean);
epsilon1 = cov(data[which(data$z == 1), 1:2]);
epsilon2 = cov(data[which(data$z == 2), 1:2]);
pi1 = dim(data[which(data$z == 1),])[1] / dim(data)[1];
pi2 = dim(data[which(data$z == 2),])[1] / dim(data)[1];
df = data.frame("mu1" = mu1, "mu2" = mu2, "epsilon1" = epsilon1, "epsilon2" = epsilon2, "pi1" = pi1, "pi2" = pi2);
write.table(df, paste("Pima","_analysis.csv", sep=""), sep = ';'); #do not forget "sep" arg in ordre to define columns 


#/!\-------EUCLIDEAN CLASSIFIER ANALYSIS---------/!\

#Divide data
donn.sep <- separ1(XPim, zPim)
Xapp <- donn.sep$Xapp
zapp <- donn.sep$zapp
Xtst <- donn.sep$Xtst
ztst <- donn.sep$ztst

#error matrix
error_train <- matrix(0, 1, 20);
error_test <-  matrix(0, 1, 20);

#start of algorithm 

  for (j in 1:20 ){
    
    donn.sep <- separ1(XPim, zPim) #using separ1.R function 
    
    #parting in learning and test sets 
    Xapp <- donn.sep$Xapp
    zapp <- donn.sep$zapp
    Xtst <- donn.sep$Xtst
    ztst <- donn.sep$ztst
    
    #we estimate parameters on Xapp
    mu <- ceuc.app(Xapp, zapp);
    
    #applying values on individuals of sets 
    train_set <- ceuc.val(Xapp, mu);
    test_set <- ceuc.val(Xtst, mu);
    
    #error rate 
    
    error_train[1,j] <- dim(as.matrix(which(train_set != as.matrix(zapp))))[1] / dim(as.matrix(zapp))[1]; 
    error_test[1,j] <- dim(as.matrix(which(test_set != as.matrix(ztst))))[1] / dim(as.matrix(zapp))[1]; 
    
  }
  
}

#applying mean on rows which represent each dataset

average_error_rate_Pima <- matrix(0, nrow = 1, ncol = 2);

average_error_rate_Pima[,1] = apply(error_train, 1, mean);
average_error_rate_Pima[,2] = apply(error_test, 1, mean);

#Confidence interval 

#we consider with alpha = 0.05
#We use : CI = [p(mean) - fract(0.975*s/sqrt(n)), p(mean) + fract(0.975*s/sqrt(n))]

ptrain <- as.matrix(average_error_rate_Pima[, 1]);
ptest <- as.matrix(average_error_rate_Pima[, 2]);
frac = 1.96;

CI_Pima <- matrix(0, nrow = nrow(average_error_rate_Pima), ncol = 4); #4 because of two values for each interval 
for (i in 1:nrow(average_error_rate_Pima)){
  CI_Pima[i, 1] <- ptrain[i] - frac * sqrt((ptrain[i]*(1-ptrain[i]))/nrow(as.data.frame(donn_list[i]))); #lower bound for CI on train set
  CI_Pima[i, 2] <- ptrain[i] + frac * sqrt((ptrain[i]*(1-ptrain[i]))/nrow(as.data.frame(donn_list[i]))); #upper bound for CI on train set
  CI_Pima[i, 3] <- ptest[i] - frac * sqrt((ptest[i]*(1-ptest[i]))/nrow(as.data.frame(donn_list[i]))); #lower bound for CI on test set
  CI_Pima[i, 4] <- ptest[i] + frac * sqrt((ptest[i]*(1-ptest[i]))/nrow(as.data.frame(donn_list[i]))); #upper bound for CI on test set
  
}




