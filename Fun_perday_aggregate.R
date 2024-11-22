per_day<-function (n) # --------------- start ------per day function
{
  tm<-strptime(n,"%Y-%m-%d %H:%M:%S");
  Y1<- tm$year + 1900;
  M1<- tm$mon + 1;
  D1<- tm$mday;
  H1<- tm$hour;
  Mn1<- tm$min;
  SS1<- tm$sec;
  
  vector_data<-cbind(Y1, M1, D1, H1, Mn1, SS1)
  
  d2<-D1[1]
  day_n=c();
  day_n_res=c();
  count_days=c();
  
  j=1;
  for(i in 1:length(D1))
  {
    n=0;
    
    if(j==length(H1)+1) { break }
    
    while(D1[j]==d2)
    {
      n=n+1;
      j=j+1;
      
      if(j==length(D1)+1) { break }
    }
    count_days<-c(count_days, n);
    day_n<- c(day_n, d2);
    
    day_n_res<- cbind(day_n, count_days);
    
    d2=d2+1;
    return(count_days)
  }
  
} # -----------------------------------------end of per day 
