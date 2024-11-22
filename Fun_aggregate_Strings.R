#---------------------aggregate strings

code_string<-function(string_vec)
{
  str_sr<-string_vec;
  str_sr_data<-c();
  sorted_names_sr_ind=c();
  
  for(i in 1:length(str_sr))
  {
    str_sr_data=cbind(c(str_sr_data,str_sr[i]));
  }
  
  str_unique=unique(str_sr_data);
  
  sorted_index_sr=order(str_sr_data); 
  
  sorted_names_sr=cbind(str_sr_data[sorted_index_sr]);
  sorted_names_sr_ind[1:length(sorted_names_sr)]<-NA
  sorted_names_ind=cbind(sorted_names_sr, sorted_names_sr_ind)
  
  
  for(s in 1:length(str_unique))
  {
    
    a<-strfind(sorted_names_sr, str_unique[s,1]);
    
    ind<-which(as.character(a)=="1")
    
    sorted_names_ind[ind,2]=as.numeric(s);
  }
  
  AB<-sorted_names_ind;
  Data_withcode<-c()
  
  for(f in 1:nrow(AB))
  {
    indd=which(as.numeric(AB[,2])==f);
    
    Data_withcode<-rbind(Data_withcode, AB[indd,]);  # ---------------Final coding of string data
  }
  
  return(Data_withcode);
  
} # end of aggregate function -------------------------
