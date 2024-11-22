# ---------------------code severity

code_severity<-function(severity)
{
  new_d<-c();
  d11<-severity;
for(i in 1:length(d11))
{
  if(d11[i]==16)
  {
    new_d<-c(new_d,2);
  }
  
  else if(d11[i]==32)
  {
    new_d<-c(new_d,3);
  }
  else
  {
    new_d<-c(new_d,d11[i]);
  }
}
return(new_d)
} # ------------------------Severity is coded
