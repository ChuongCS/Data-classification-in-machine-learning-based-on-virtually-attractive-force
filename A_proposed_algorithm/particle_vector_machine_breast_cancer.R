rm(list=ls())
library('reshape2')
library('mlbench')
library('e1071')

iter = 100
delt = 0.1
k_fold = 0.8
no_run = 5
sig_scale = 0.05

for (ii in 1:no_run){
print(ii)
  data(BreastCancer)
  df <- as.data.frame(BreastCancer)
  df <- df[,2:ncol(df)]

    for(i in 1:(ncol(df)-1)){
    ind <- which(is.na(df[,i]))
    df[ind,i] <- round(mean(as.numeric(df[,i]),na.rm = T))
  }
  
  df[,1:(ncol(df)-1)] <- as.numeric(as.matrix(df[,1:(ncol(df)-1)]))
  
  for(i in 1:(ncol(df)-1)){
    df[,i] <- (df[,i]-min(df[,i]))/(max(df[,i])-min(df[,i]))
  }

k_train <- sample(sample(1:nrow(df)),round(k_fold*nrow(df)))
k_test <- setdiff((1:nrow(df)),k_train)

df_train <- df[k_train,]
df_test <- df[k_test,]

d_tr <- df_train[,1:(ncol(df_train)-1)] 
l_tr <- df_train[,ncol(df_train)]

d_ts <- df_test[,1:(ncol(df_test)-1)] 
l_ts <- df_test[,ncol(df_test)]

d_ts_new <- d_ts


sigma = sig_scale*(max(d_tr)-min(d_tr))

l_es <- matrix("Blank",1,length(l_ts))

for(i in 1 : nrow(d_ts)){

  x = as.numeric(d_ts[i,])
  
  for (j in 1:iter){
   
    v=0*x
 
    z=as.matrix(d_tr)-matrix(1,nrow(d_tr),1)%*%as.numeric(x)
    
    v = exp(-rowSums(z^2)/sigma^2) %*% z
    
    x = x+v*delt
    
 #  print(sum(v^2))
    
  }
  
  vec = rowSums(z^2)
  ind <- which(vec==min(vec))
  
  l_es[i] <- as.character(l_tr[ind[1]])
  d_ts_new[i,] <- x
}


com_label <- as.data.frame(cbind(matrix(as.character(l_ts),length(l_ts),1),
                                 matrix(as.character(l_es),length(l_es),1)))
names(com_label) <- c('actual_label','estimated_label')

levels(com_label$actual_label) <- as.character(unique(df[,ncol(df)]))
levels(com_label$estimated_label) <- as.character(unique(df[,ncol(df)]))


com_label_dummy <- expand.grid(as.character(unique(df[,ncol(df)])),
                        as.character(unique(df[,ncol(df)])))

names(com_label_dummy) <- names(com_label)

com_label <- rbind(com_label,
                   com_label_dummy)

confusion_mat<- table(as.character( com_label$actual_label),as.character(com_label$estimated_label))
confusion_mat <- 100*confusion_mat/
  sum(confusion_mat)

accuracy_mat <- table(as.character( com_label$actual_label)==as.character(com_label$estimated_label))
accuracy_mat <- 100*accuracy_mat/sum(accuracy_mat)

if (ii == 1){
  confusion_mat_master <- confusion_mat
  accuracy_mat_master <- accuracy_mat
} else {
  confusion_mat_master <- (1/(ii))*((ii-1)*confusion_mat_master+
                                          confusion_mat)
  accuracy_mat_master <- (1/ii)*((ii-1)*accuracy_mat_master+accuracy_mat)
}

}

confusion_mat_master
accuracy_mat_master

write.csv(confusion_mat_master,'pvm_confusion_mat.csv')
write.csv(accuracy_mat_master,'pvm_accuracy_mat.csv')