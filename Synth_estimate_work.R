#for each data we ant to estimate parameters such as mean, variance and proportion of each class in the population 

#list of our datasets we want to estimate
donn_list = list(donn40, donn100, donn500, donn1000, donn1000_2);

#for the naming of the result file 
fns = c("1", "1", "1", "1", "2");

individuals_numbers = c(40, 100, 500, 1000, 1000);

#this algorithm write the estimate in a table 

for (i in 1:length(donn_list)) {
  data = as.data.frame(donn_list[i]);
  mu1 = apply(data[which(data$z == 1), 1:2], 2, mean);
  mu2 = apply(data[which(data$z == 2), 1:2], 2, mean);
  epsilon1 = cov(data[which(data$z == 1), 1:2]);
  epsilon2 = cov(data[which(data$z == 2), 1:2]);
  pi1 = dim(data[which(data$z == 1),])[1] / dim(data)[1];
  pi2 = dim(data[which(data$z == 2),])[1] / dim(data)[1];
  df = data.frame("mu1" = mu1, "mu2" = mu2, "epsilon1" = epsilon1, "epsilon2" = epsilon2, "pi1" = pi1, "pi2" = pi2);
  write.table(df, paste("synth", fns[i],"_", individuals_numbers[i], "_analysis.csv", sep=""), sep = ';'); #do not forget "sep" arg in ordre to define columns 
}




