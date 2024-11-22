#  This function calculates the discriminat function, the With In class covariance (separation)
#   the with in class covatiance matrix (separation)

data_For_GDA <- read.csv("E:/2014/data_For_GDA.csv");
DependentVariable<-data_For_GDA$JOB;
IndependentVariable<-cbind(data_For_GDA[,1:3], data_For_GDA$JID);

DiscriminantAnalysis(DependentVariable, IndependentVariable); # Call the DiscriminanAnalysis Function
# ------------------- ============ The DA starts

DiscriminantAnalysis<-function(DependentVariable, IndependentVariable)   # start Discriminant Function
{
  
  classLabel = unique(DependentVariable);
  nClass = length(classLabel);
  Dim = nClass - 1; # number of disciminant functions
  WICMean_each_class_data<-WithInCovariance(DependentVariable, IndependentVariable,nClass,classLabel) # call WithInCovariance function
  Mean_each_class_data<-WICMean_each_class_data$`MeanOfEachClassData: `;
  
  LengthEachClass<-WICMean_each_class_data$`LengthOfEachClass:`
  
  BWCCOV<-BetWeenCovariance(IndependentVariable, Mean_each_class_data, LengthEachClass); # Call of BetweenCovariance function
  W<-WICMean_each_class_data$WithInClassCovarianceMatrix;
  B<-BWCCOV$BetWeenClassCovarianceMatrix;
  
  each_class_X_data<- WICMean_each_class_data$`EachClassXData: `;
  Mean_each_class_data<- WICMean_each_class_data$`MeanOfEachClassData: `;
  Each_class_data_minus_Its_mean<- WICMean_each_class_data$`EachClassDataMinusItsMean:`;
  
  Mean_Each_feature<- BWCCOV$`EachFeatureMean: `;
  Xclass_mean_minus_Xf_mean<- BWCCOV$`EachClassMeanMinusEachFeatureMean: `;
  
  D<-eigen(solve(W)%*%B)$values
  U<-eigen(solve(W)%*%B)$vectors
  
  lambda=D; # this is the separation.
  W_div_B_W=t(solve(B+W,W));
  B_div_B_W=(solve(B+W,B));
  
  Wilks_lambda=det(W_div_B_W); # This is Wilks Lambda
  eigenvalue_fun_all=eigen(solve((W),B))$values;
  eigenvalue_fun=eigenvalue_fun_all[1:nClass-1]
  
  Cano_corr_coef=sqrt(abs(det(B_div_B_W)));
  
  return(list("BetweenCalass Covariance: "=B,"WithinClass Covariance: " =W,"Eigenvalues:"= D, 
              "Eigenvalues of functions:"= eigenvalue_fun, "EigenVectors of Functions"=U, 
              "Wilks Lanbda: "=Wilks_lambda, "Canonical corr coeff: "=Cano_corr_coef, 
              "EachClassXData: "=each_class_X_data,"MeanOfEachClassData: " =Mean_each_class_data,
              "EachClassDataMinusItsMean:"= Each_class_data_minus_Its_mean, 
              "EachFeatureMean: "=Mean_Each_feature,
              "EachClassMeanMinusEachFeatureMean: " =Xclass_mean_minus_Xf_mean,
              "LengthOfEachClass:"= LengthEachClass));
  
} # End of Discriminant Analysis Function


# -----=============== WithinCovariance calculation stated

WithInCovariance<-function(DependentVariable, IndependentVariable,nClass,classLabel)
{
  i=1;
  j=1;
  each_class_X_data=c();
  Mean_each_class_data=c();
  each_X_minus_mean=c();
  LengthEachClass<-c();
  
  W=matrix(0, ncol(IndependentVariable),ncol(IndependentVariable));
  
  while(j<=nClass)
  {
    while(DependentVariable[i]==classLabel[j])
    {
      each_class_X_data=rbind(each_class_X_data, IndependentVariable[i,]);
      i=i+1;
      if( i>=length(DependentVariable)+1){ break };
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
    LengthEachClass<-c(LengthEachClass, nrow(each_class_X_data));
    
    W=W+w_ec;
    c=1;
    each_class_X_data=c();
    j=j+1;
  } # while loop closed
  return(list("WithInClassCovarianceMatrix"=W, "EachClassXData: "=each_class_X_data,
              "MeanOfEachClassData: " =Mean_each_class_data,
              "EachClassDataMinusItsMean:"= Each_class_data_minus_Its_mean, 
              "LengthOfEachClass:"= LengthEachClass));
  
} # --=========================== end of WithinclassCovariance function

# BetweenCovariance
BetWeenCovariance<-function(IndependentVariable, Mean_each_class_data, LengthEachClass)
{
  # =====    Mean_each_class_data: calculated in WithInCovariance function
  Mean_Each_feature=c();
  for(f in 1:ncol(IndependentVariable))
  {
    Mean_Each_feature=c(Mean_Each_feature, mean(IndependentVariable[,f]));
  }
  
  sc_r=nrow(Mean_each_class_data);
  sc_c=ncol(Mean_each_class_data);
  Xclass_mean_minus_Xf_mean=c();
  B=matrix(0,sc_c,sc_c);
  
  for(k in 1:nrow(Mean_each_class_data))
  {
    Xclass_mean_minus_Xf_mean=Mean_each_class_data[k,]-Mean_Each_feature;
    
    X_t_XT=(Xclass_mean_minus_Xf_mean)%*%t(Xclass_mean_minus_Xf_mean);
    
    L=LengthEachClass[k];#eval(as.symbol(paste("L", k,sep="_")));
    
    B=B+L*X_t_XT;
  }
  return(list("BetWeenClassCovarianceMatrix"=B, "EachFeatureMean: "=Mean_Each_feature,
              "EachClassMeanMinusEachFeatureMean: " =Xclass_mean_minus_Xf_mean,
              "LengthOfEachClass:"= LengthEachClass));
  
} # --============= end of BetWeenCovariance Matrix function
