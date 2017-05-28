#load data 

donn40 <- read.csv("Synth1-40.csv")
X40 <- donn40[,1:2]
z40 <- donn40[,3]

#mean estimate 

mu_40 <- round(mean(donn40$z))

#proportion estimate 

prop_40_c1 <- nrow(donn40[donn40$z == 1,])/nrow(donn40)
prop_40_c2 <- nrow(donn40[donn40$z == 2,])/nrow(donn40)

#variance intraclass estimate

K <- ncol(as.data.frame(table(z40))) #calcule occurences dans table et on d?termine le nombre de classe via un ncol 
VW <- (1/nrow(donn40-2))
X1 <- as.matrix(donn40[donn40$z == 1,]) 
V1 <- 1/nrow(donn40[donn40$z==1,]) * (X1 *- diag(ncol(X1)))