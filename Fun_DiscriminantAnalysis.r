#-----------------------------------------------------Discriminant Analysis


DA_FUN<-function(dept_var, indpt_var)   # start EVA
{
  
  classLabel = unique(dept_var);
  nClass = length(classLabel);
  Dim = nClass - 1; # number of disciminant functions
  i=1;
  j=1;
  each_class_X_data=c();
  Mean_each_class_data=c();
  each_X_minus_mean=c();
  
  W=matrix(0, ncol(indpt_var),ncol(indpt_var));
  
  while(j<=nClass)
  {
    while(dept_var[i]==classLabel[j])
    {
      each_class_X_data=rbind(each_class_X_data, indpt_var[i,]);
      i=i+1;
      if( i>=length(dept_var)+1){ break };
    }
    assign(paste("L", j,sep="_"),nrow(each_class_X_data));
    
    each_class_X_mean= apply(each_class_X_data,2,mean); # calculate mean column wise
    
    Data_eval=eval(assign(paste("each_class_X_data", j, sep="_"), each_class_X_data));
    
    Mean_each_class_data=rbind(Mean_each_class_data, apply(Data_eval,2,mean));
    
    Each_class_data_minus_Its_mean=c();
    for( d in 1:ncol(Data_eval))
    {
      Each_class_data_minus_Its_mean=cbind(Each_class_data_minus_Its_mean, Data_eval[,d]-mean(Data_eval[,d]));
    }
    
    assign(paste("each_class_X_means", j, sep="_"), Mean_each_class_data);
    
    assign(paste("Each_class_data_minus_Its_mean", j, sep="_"), Each_class_data_minus_Its_mean);
    ss=ncol(Each_class_data_minus_Its_mean);
    w_ec=matrix(0,ss,ss);
    
    for(w_i in 1:nrow(Each_class_data_minus_Its_mean))
    {
      w_ec=w_ec+((Each_class_data_minus_Its_mean[w_i,]) %*% t(Each_class_data_minus_Its_mean[w_i,]));
    }
    
    assign(paste("W_g", j, sep="_"), w_ec);
    L_min_one=nrow(each_class_X_data)-1
    
    assign(paste("S_g", j, sep="_"), (1/L_min_one)*w_ec);
    
    W=W+w_ec;
    c=1;
    each_class_X_data=c();
    j=j+1;
  } # while loop closed
  
  Mean_Each_feature=c();
  for(f in 1:ncol(indpt_var))
  {
    Mean_Each_feature=c(Mean_Each_feature, mean(indpt_var[,f]));
  }
  
  sc_r=nrow(Mean_each_class_data);
  sc_c=ncol(Mean_each_class_data);
  Xclass_mean_minus_Xf_mean=c();
  B=matrix(0,sc_c,sc_c);
  
  for(k in 1:nrow(Mean_each_class_data))
  {
    Xclass_mean_minus_Xf_mean=Mean_each_class_data[k,]-Mean_Each_feature;
    
    X_t_XT=(Xclass_mean_minus_Xf_mean)%*%t(Xclass_mean_minus_Xf_mean);
    
    L=eval(as.symbol(paste("L", k,sep="_")));
    
    B=B+L*X_t_XT;
  }
  
  D<-eigen(solve(W)%*%B)$values
  U<-eigen(solve(W)%*%B)$vectors
  
  lambda=D; # this is the separation.
  W_div_B_W=t(solve(B+W,W));
  B_div_B_W=(solve(B+W,B));
  
  Wilks_lambda=det(W_div_B_W); # This is Wilks Lambda
  eigenvalue_fun_all=eigen(solve((W),B))$values;
  eigenvalue_fun=eigenvalue_fun_all[1:nClass-1]
  
  Cano_corr_coef=sqrt(det(B_div_B_W));
  
  print(B)
  print(W)
  print(Cano_corr_coef)
  print(eigenvalue_fun)
  print(Wilks_lambda)
  print(U);
  print(D)
  
} # -----------------------------------------Function DA is closed
