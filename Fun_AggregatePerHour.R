#------------------------------------------------perhour

per_hr<- function(td)
{
  
  tm<-strptime(td,"%Y-%m-%d %H:%M:%S");
  Y1<- tm$year + 1900;
  M1<- tm$mon + 1;
  D1<- tm$mday;
  H1<- tm$hour;
  Mn1<- tm$min;
  SS1<- tm$sec;
  
  vector_data<-cbind(Y1, M1, D1, H1, Mn1, SS1)
  
  d<-D1[1]
  j=1;
  i1=1;
  # day_n <- array(0, dim=c(length(D1),0))
  No_attacks_res<-array(0, dim=c(length(D1),0))
  #No_attacks_res<-c()
  
  day_n <- c()
  #No_Dattacks_resd<-c()
  No_attacks_resh<-c()
  No_attacks_resc<-c()
  
 while(d<=Max(D1))
  {
    h=0;
    i=i1;
    
    while(h<=23)
    {
      n=0;
      
      while((D1[i]==d) & (H1[i]==h))
      {
        n=n+1;
        i=i+1;
        
      }
      
      #No_attacks_resd<- c(No_attacks_resd, d)
      No_attacks_resh<- c(No_attacks_resh, h)
      No_attacks_resc<- c(No_attacks_resc, n)
      
      h=h+1;
      j=j+1;
      
    }
    i1=i;
    day_n=cbind(day_n, d);
    
    d=d+1;
    
    
  }
  
 return(list(vector_data, No_attacks_resc));

plot(No_attacks_resc, type="l", ylab="Number of attacks per hours", xlab="Hour")

} #----------------------------------------end of function per hour
