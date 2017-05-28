#AT FIRST : USE load-data.R file 

#list of our datasets we want to estimate
donn_list = list(donn40, donn100, donn500, donn1000);
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