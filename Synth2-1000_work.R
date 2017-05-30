#AT FIRST USE : load-data.R

#AT FIRST : USE load-data.R file 


#/!\-----EUCLIDEAN CLASSIFIER------/!\
#-------------------------------------

#list of our datasets we want to estimate
donn_list = list(donn1000_2);

#error matrix
error_train <- matrix(0, length(donn_list), 20);
error_test <-  matrix(0, length(donn_list), 20);

#start of algorithm 
for (i in 1:length(donn_list)) {
  data <- as.data.frame(donn_list[i])
  X <- data[,1:2];
  z <- data[,3];
  for (j in 1:20 ){
    
    donn.sep <- separ1(X, z) #using separ1.R function 
    
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
    
    error_train[i,j] <- dim(as.matrix(which(train_set != as.matrix(zapp))))[1] / dim(as.matrix(zapp))[1]; 
    error_test[i,j] <- dim(as.matrix(which(test_set != as.matrix(ztst))))[1] / dim(as.matrix(zapp))[1]; 
    
  }
  
}

#applying mean on rows which represent each dataset

average_error_rate <- matrix(0, nrow = length(donn_list), ncol = 2);

average_error_rate[,1] = apply(error_train, 1, mean);
average_error_rate[,2] = apply(error_test, 1, mean);

#Confidence interval 

#we consider with alpha = 0.05
#We use : CI = [p(mean) - fract(0.975*s/sqrt(n)), p(mean) + fract(0.975*s/sqrt(n))]

ptrain <- as.matrix(average_error_rate[, 1]);
ptest <- as.matrix(average_error_rate[, 2]);
frac = 1.96;

CI_tab <- matrix(0, nrow = nrow(average_error_rate), ncol = 4); #4 because of two values for each interval 
for (i in 1:nrow(average_error_rate)){
  CI_tab[i, 1] <- ptrain[i] - frac * sqrt((ptrain[i]*(1-ptrain[i]))/nrow(as.data.frame(donn_list[i]))); #lower bound for CI on train set
  CI_tab[i, 2] <- ptrain[i] + frac * sqrt((ptrain[i]*(1-ptrain[i]))/nrow(as.data.frame(donn_list[i]))); #upper bound for CI on train set
  CI_tab[i, 3] <- ptest[i] - frac * sqrt((ptest[i]*(1-ptest[i]))/nrow(as.data.frame(donn_list[i]))); #lower bound for CI on test set
  CI_tab[i, 4] <- ptest[i] + frac * sqrt((ptest[i]*(1-ptest[i]))/nrow(as.data.frame(donn_list[i]))); #upper bound for CI on test set
  
}


#/!\-----KPPV CLASSIFIER------/!\
#-------------------------------------

#AT FIRST : USE load-data.R file 

#list of our datasets we want to estimate
donn_list = list(donn1000_2);
opti_neigh = matrix(0, nrow = length(donn_list), ncol = 1)

for (i in 1:length(donn_list)) {
  data <- as.data.frame(donn_list[])
  X <- data[,1:2];
  z <- data[,3];
  
  donn.sep <- separ1(X, z) #using separ1.R function 
  
  #parting in learning and test sets 
  Xapp <- donn.sep$Xapp
  zapp <- donn.sep$zapp
  Xtst <- donn.sep$Xtst
  ztst <- donn.sep$ztst
  
  opti_neigh[i] <- kppv.tune(Xapp, zapp, Xapp, zapp, 2*(1:6)-1)
  
}

#/!\-----Confidence Intervals and error Rate------/!\

#AT FIRST : USE load-data.R file 

#list of our datasets we want to estimate
donn_list = list(donn1000_2);

#error matrix
error_train_kppv <- matrix(0, length(donn_list), 20);
error_test_kppv <-  matrix(0, length(donn_list), 20);

#start of algorithm 
for (i in 1:length(donn_list)) {
  data <- as.data.frame(donn_list[i])
  X <- data[,1:2];
  z <- data[,3];
  for (j in 1:20 ){
    
    donn.sep <- separ2(X, z) #using separ1.R function 
    
    #parting in learning and test sets 
    Xapp <- donn.sep$Xapp
    zapp <- donn.sep$zapp
    Xval <- donn.sep$Xval
    zval <- donn.sep$zval
    Xtst <- donn.sep$Xtst
    ztst <- donn.sep$ztst
    
    #we estimate parameters on Xapp
    Kopt <- kppv.tune(Xapp, zapp, Xval, zval,2*(1:6)-1);
    print(Kopt);
    
    #applying values on individuals of sets 
    zapp_val <- kppv.val(Xapp, zapp, Kopt, Xapp);
    ztst_val <- kppv.val(Xapp, zapp, Kopt, Xtst);

    #error rate 
    
    error_train_kppv[i,j] <- length(zapp_val[which(as.matrix(zapp_val) != as.matrix(zapp))]) / dim(Xapp)[1];
    error_test_kppv[i,j] <- length(ztst_val[which(as.matrix(ztst_val) != as.matrix(ztst))]) / dim(Xtst)[1];
    
  }
  
}



#applying mean on rows which represent each dataset

average_error_rate_kppv <- matrix(0, nrow = length(donn_list), ncol = 2);

average_error_rate_kppv[,1] = apply(error_train_kppv, 1, mean);
average_error_rate_kppv[,2] = apply(error_test_kppv, 1, mean);

#Confidence interval 

#we consider with alpha = 0.05
#We use : CI = [p(mean) - fract(0.975*s/sqrt(n)), p(mean) + fract(0.975*s/sqrt(n))]

ptrain <- as.matrix(average_error_rate_kppv[, 1]);
ptest <- as.matrix(average_error_rate_kppv[, 2]);
frac = 1.96;

CI_tab_kppv <- matrix(0, nrow = nrow(average_error_rate_kppv), ncol = 4); #4 because of two values for each interval 
for (i in 1:nrow(average_error_rate)){
  CI_tab_kppv[i, 1] <- ptrain[i] - frac * sqrt((ptrain[i]*(1-ptrain[i]))/nrow(as.data.frame(donn_list[i]))); #lower bound for CI on train set
  CI_tab_kppv[i, 2] <- ptrain[i] + frac * sqrt((ptrain[i]*(1-ptrain[i]))/nrow(as.data.frame(donn_list[i]))); #upper bound for CI on train set
  CI_tab_kppv[i, 3] <- ptest[i] - frac * sqrt((ptest[i]*(1-ptest[i]))/nrow(as.data.frame(donn_list[i]))); #lower bound for CI on test set
  CI_tab_kppv[i, 4] <- ptest[i] + frac * sqrt((ptest[i]*(1-ptest[i]))/nrow(as.data.frame(donn_list[i]))); #upper bound for CI on test set
  
}


