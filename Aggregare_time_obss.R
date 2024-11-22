<<<<<<< HEAD
<<<<<<< HEAD
=======
# -------------------- Aggregate timestamp data and apply ML Algorithms
>>>>>>> Updated R codes
=======
>>>>>>> f64debcb10ef6b7b95587d337775eaf0dfed6fb9

library("RODBC")

channel <- odbcConnect("portal");

obss<-sqlQuery(channel," select year(timestamp_date) as year,
               month(timestamp_date) as month,
               day(timestamp_date) as day,
               datepart(hour, timestamp_date) as hour, 
               count(*) as obs
               FROM [portal].[dbo].[D1ATTACKS1]
               group by 
               year(timestamp_date) ,
               month(timestamp_date),
               day(timestamp_date),
               DATEPART(hour, [timestamp_date])
               order by year, month, day, hour");
# View(obss)

num_y=obss;
year_v=c();
month_v<-c();
day_v<-c();
dtv=c();
hrv=c();
i=1;
j1=1;
while(i<=nrow(num_y))
{
  h=0;
  j=j1;
  while( h<=23)
  {
    if(j==nrow(num_y)+1) {break};
    if(h==num_y[j,4])
    {
      dtv=c(dtv, num_y[j,5]);
      hrv=c(hrv, h);
      year_v<-c(year_v,num_y[j,1]);
      month_v<-c(month_v, num_y[j,2]);
      day_v<-c(day_v, num_y[j,3]);
      h=h+1;
      j=j+1;
    } # end of if statement
    else
    {
      dtv=c(dtv, 0);
      hrv=c(hrv, h);
      year_v<-c(year_v,num_y[j,1]);
      month_v<-c(month_v, num_y[j,2]);
      day_v<-c(day_v, num_y[j,3]);
      h=h+1;
      
    } # end of else statement
  } # while hour end
  j1=j;
  i=i+1;
} # while loop end

vec=cbind(year_v, month_v, day_v, hrv, dtv);

# View(vec)
# vec[nrow(vec),4]

end_vec<-c();
end_v<-vec[nrow(vec),4];
if(end_v<23)
{
  while(end_v<=23)
  {
    end_vec<-c(end_vec, end_v);
    end_v=end_v+1;
  }
}
if(end_v==23)
{
  end_vec<-c();
}

vec1<-c();
vec_24<-c();
for(e in 2:length(end_vec))
{
  vec1<-rbind(vec1, c(vec[nrow(vec),1:3],end_vec[e],0))
}
# View(vec1)

vec_24<-rbind(vec,vec1);


# View(vec_24)

# -------------Take 10 highest values of the number of attacks in an hour
sort_value_max<-sort(dtv, decreasing = T);
sort_value_10_max<-sort_value_max[1:10];

index_max=c();
data_vec_10_max<-c();
index_vec_max=c();
for(v in 1:10)
{
  index_max<-eval(assign(paste("ind_max", v, sep="_"), which(dtv==sort_value_10_max[v])));
  
  data_vec_10_max<-rbind(data_vec_10_max, vec_24[index_max,]);
  index_vec_max<-c(index_vec_max,index_max);  
}

data_vec_10_with_max<-matrix(0,1,nrow(vec_24));

for(i in 1:10)
{
  data_vec_10_with_max[,index_vec_max[i]]=data_vec_10_max[i,5];
}
# plot the data 

date_num<-ISOdate(vec_24[,1], vec_24[,2], vec_24[,3]);
x.1 <- as.Date(date_num);
plot(date_num, vec_24[,5], ylab="Number of attacks in an hours", xlab="Hour",col="blue", axes="False");
# plot(dtv, ylab="Number of attacks in an hours", xlab="Hour",col="blue", axes="False");
axis(2, at=vec_24[,5]);
axis(1, at=date_num, labels=format(x.1,"%m/%d/%y"));
box();
lines(date_num, data_vec_10_with_max, col="red");

# equal vector of the data

mat1<-matrix(vec_24[,1], 24, ncol = round(nrow(vec_24)/24));
mat2<-matrix(vec_24[,2], 24, ncol = round(nrow(vec_24)/24));
mat3<-matrix(vec_24[,3], 24, ncol = round(nrow(vec_24)/24));
mat4<-matrix(vec_24[,4], 24, ncol = round(nrow(vec_24)/24));
mat5<-matrix(vec_24[,5], 24, ncol = round(nrow(vec_24)/24));

vector1<-matrix(mat1, 1, ncol(mat1)*24);
vector2<-matrix(mat2, 1, ncol(mat2)*24);
vector3<-matrix(mat3, 1, ncol(mat3)*24);
vector4<-matrix(mat4, 1, ncol(mat4)*24);
vector5<-matrix(mat5, 1, ncol(mat5)*24);

length(vector1)

No_attacks_res<-vec_24;
# No_attacks_res=cbind(t(vector1), t(vector2), t(vector3), t(vector4), t(vector5));


# View(No_attacks_res);

write.table(No_attacks_res, "E:/No_attacks_res.txt");
write.csv(No_attacks_res, "E:/No_attacks_res.csv");
write.table(dtv, "E:/Attacks_per_hr.txt");
write.csv(dtv, "E:/Attacks_per_hr.csv");

value_1<-vec_24[which(dtv==1),];
value_2<-vec_24[which(dtv==2),];
value_3<-vec_24[which(dtv==3),];
value_4<-vec_24[which(dtv==4),];
value_5<-vec_24[which(dtv==5),];
value_6<-vec_24[which(dtv==6),];
value_7<-vec_24[which(dtv==7),];
value_8<-vec_24[which(dtv==8),];
value_9<-vec_24[which(dtv==9),];
value_10<-vec_24[which(dtv==10),];

# --------- ---------relative number of attcks in an hour

relv_mat5<-mat5;
# View(relv_mat5)

Relative_No_attacks_res_v<-c();

for(r in 1:ncol(mat5))
{
  
  if(sum(mat5[,r])==0)
  {
    Relative_No_attacks_res_v=c(Relative_No_attacks_res_v, matrix(0,1,24));
  }
  else
  {
    Relative_No_attacks_res_v=c(Relative_No_attacks_res_v, mat5[,r]/sum(mat5[,r]));
    
  }
  
}

Attack_per_hr_relatv=cbind(No_attacks_res, (Relative_No_attacks_res_v));

Names<-as.character(cbind("year","month","day","hour","No.attack_ph","Relv_No_attacks"));
colnames(Attack_per_hr_relatv) <- Names;

# View(Attack_per_hr_relatv);

<<<<<<< HEAD
<<<<<<< HEAD
=======
# --------- --------- end of Attack_per_hr_relatv

# ------------------------------------Take 10 highest values of Attack_per_hr_relatv
>>>>>>> Updated R codes
=======
>>>>>>> f64debcb10ef6b7b95587d337775eaf0dfed6fb9
relv_ph=Attack_per_hr_relatv[,6];
relv_sort_desc<-sort(relv_ph, decreasing = T);

Relv_10_max<-relv_sort_desc[1:10];

relv_max_10<-c();
relv_All_10_max<-c();
Relv_all_ind_10_max=c();

for(rv in 1:10)
{
  relv_max_10<-eval(assign(paste("ind_max", rv, sep="_"), which(relv_ph==Relv_10_max[rv])));
  relv_All_10_max<-rbind(relv_All_10_max, Attack_per_hr_relatv[relv_max_10,]);
  Relv_all_ind_10_max<-c(Relv_all_ind_10_max,relv_max_10);  
}

Relv_vec_with_10_max<-matrix(0,1,length(relv_ph));

for(i in 1:10)
{
  Relv_vec_with_10_max[,Relv_all_ind_10_max[i]]=relv_All_10_max[i,6];
}

# plot the data 

relv_date_num<-ISOdate(Attack_per_hr_relatv[,1], Attack_per_hr_relatv[,2], Attack_per_hr_relatv[,3]);
x.1_rv <- as.Date(relv_date_num);

plot(relv_date_num, relv_ph, ylab="Relative number of attacks in an hours", xlab="Hour",col="blue", axes="False");
axis(2, at=relv_ph)
axis(1, at=relv_date_num, labels=format(x.1_rv,"%m/%d/%y"))
box();
lines(relv_date_num, Relv_vec_with_10_max, col="red");

# length(relv_ph)

# relv_All_10_max

<<<<<<< HEAD
<<<<<<< HEAD
=======
# ------------------------------------Take 10 lowest values of Attack_per_hr_relatv different from 0
>>>>>>> Updated R codes
=======
>>>>>>> f64debcb10ef6b7b95587d337775eaf0dfed6fb9
data_relv=Attack_per_hr_relatv[,6];
ind_diff_0<-which(data_relv!=0);
rela_diff_0<-data_relv[ind_diff_0]
rela_diff_0_sort<-sort(rela_diff_0)
rela_diff_0_sort_10<-rela_diff_0_sort[1:10];

All_10_relv_min<-c();
ind_10_relv_min<-c();

for (m in 1:10)
{
  ind_10_relv_min<-c(ind_10_relv_min, which(data_relv==rela_diff_0_sort_10[m]));
  All_10_relv_min<-rbind(All_10_relv_min, Attack_per_hr_relatv[ind_10_relv_min[m],]);
  
}


Relv_with_0_data<-c();
Relv_with_0_data=matrix(0,1,nrow(Attack_per_hr_relatv));

for(k in 1:10)
{
  
  Relv_with_0_data[,ind_10_relv_min[k]]<- All_10_relv_min[k,6];
  
}

lines(relv_date_num, Relv_with_0_data, col="green");

# length(relv_date_num)
# length(Relv_with_0_data)

write.table(Attack_per_hr_relatv, "E:/No_attacks_relav_res.txt");
write.csv(Attack_per_hr_relatv, "E:/No_attacks_relav_res.csv");

#read.csv(No_attacks_relav_res.csv, header = TRUE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")

# View(data_relv)
<<<<<<< HEAD
<<<<<<< HEAD
# --------------Peaks Over Threshold method
=======

# -----------------------Peaks Over Threshold method
>>>>>>> Updated R codes
=======
# --------------Peaks Over Threshold method
>>>>>>> f64debcb10ef6b7b95587d337775eaf0dfed6fb9

T_j=quantile(data_relv, probs=seq(0.50,0.99,0.01), na.rm=FALSE, names=TRUE, type=7)

# T_j<-Proposed
#T_jj=data.frame(T_j)

# View(T_jj)
# POT

plot(relv_date_num, Attack_per_hr_relatv[,6], ylab="Relative number of attacks in an hours", xlab="Hour",col="blue", axes="False");
axis(2, at=Attack_per_hr_relatv[,6])
axis(1, at=relv_date_num, labels=format(x.1_rv,"%m/%d/%y"))
box();

plot(relv_date_num, Attack_per_hr_relatv[,6], type="l", ylab="Relative number of attacks in an hours", xlab="Hour",col="blue", axes="False");
axis(2, at=Attack_per_hr_relatv[,6])
axis(1, at=relv_date_num, labels=format(x.1_rv,"%m/%d/%y"))
box();

ind_Tj=c();
Excess_Tj=c();
# L_excess_Tj<-c();
Fit_Excess_Tj<-c();

diff_sigma_xi_Tj=c();
All_paramEsts=c();
pluss_excess_minus_Tj=c();
shape_T_j<-c();
scale_T_j<-c();

shape_times_T_j<-c();
scale_minus_shape_times_T_j<-c();


for(i in 1:length(T_j))
{
  
    # L_excess_Tj<-c(L_excess_Tj, length(ind_Tj));
  abline(h=T_j[i],col="red");
  
  ind_Tj<-which(data_relv>T_j[i], arr.ind=T);
  
  Excess_Tj<-data_relv[ind_Tj];
  assign(paste("Excess_T", i,sep="_"), Excess_Tj)
  
  excess=eval(as.symbol(paste("Excess_T", i,sep="_")));
  
  Fit_Excess_Tj<-cbind(Fit_Excess_Tj, gpd.fit(excess,"amle")); # each shape and each scale
  Fit_df<-data.frame(Fit_Excess_Tj);
  
  shape_T_j<-cbind(shape_T_j, Fit_df[1,i]);
  scale_T_j<-cbind(scale_T_j, Fit_Excess_Tj[2,i]);
  
  shape_times_T_j<-cbind(shape_times_T_j, shape_T_j[i]*T_j[i]);
  scale_minus_shape_times_T_j<- cbind(scale_minus_shape_times_T_j, scale_T_j[i]-shape_times_T_j[i]);
}

# ---- calculate the seq (scale_minus_shape_times_T_j-scale_minus_shape_times_T_j-1);

Seq<-c();

for(s in 2:length(scale_minus_shape_times_T_j))
{
  Seq<-c(Seq, scale_minus_shape_times_T_j[s]-scale_minus_shape_times_T_j[s-1]);
}
<<<<<<< HEAD
<<<<<<< HEAD
=======

>>>>>>> Updated R codes
=======
>>>>>>> f64debcb10ef6b7b95587d337775eaf0dfed6fb9
# -- Test hypothesis that the sequence,S_eq, is normally distributed with mean zero
# Perform normality test 
#library(nortest)

Suitable_Threshold<-c();
p_v_shapiro<-c();
Seq_test=Seq;
n=1;
end_n=length(Seq);
norm_test_lil<-c();
p_v_lil<-c();
for(j in 1:length(Seq)-4)
# while(n<=length(Seq)-4)
{
  Seq_test=Seq[n:end_n];
  
norm_test<-shapiro.test(Seq_test); 
p_v_shapiro<-norm_test$p.value;

norm_test_lil<-lillie.test(Seq_test);
p_v_lil<-norm_test_lil$p.value;

if(p_v_shapiro>=0.05)
#if(p_v_lil>=0.05)
{
  Suitable_Threshold<-T_j[n];
  Seq_test=Seq_test;
}else
{
  n=n+1;
  
}

}

Extremes_above_suitable_Tj=Attack_per_hr_relatv[which(Attack_per_hr_relatv[,6]>Suitable_Threshold),];
Extremes_above_suitable_Tj_with_0<-c();
Extremes_above_suitable_Tj_with_0<-matrix(0,nrow(Attack_per_hr_relatv));

for(e in 1:nrow(Extremes_above_suitable_Tj))
{
  Extremes_above_suitable_Tj_with_0[which(Attack_per_hr_relatv[,6]>Suitable_Threshold)[e]]<-c(Extremes_above_suitable_Tj[e,6])
}

plot(relv_date_num, Attack_per_hr_relatv[,6], type="l", ylab="Relative number of attacks in an hours", xlab="Hour", col="blue", axes=FALSE);
abline(h=Suitable_Threshold,col="red", lwd=6);
text(relv_date_num[nrow(Attack_per_hr_relatv)], Suitable_Threshold, paste("Tj=",Suitable_Threshold), col="white", font=2)
axis(2, at=Attack_per_hr_relatv[,6])
axis(1, at=relv_date_num, labels=format(x.1_rv,"%m/%d/%y"))
box();
xx=Attack_per_hr_relatv[,6];
stdv=sd(Attack_per_hr_relatv[,6]);
abline(h=3*stdv, col="green", lwd=6);
text(relv_date_num[nrow(Attack_per_hr_relatv)],3*stdv, paste(" 3stdv=",3*stdv), col = "red", font=2);


# only outliers using suitable threshold;

plot(relv_date_num, Extremes_above_suitable_Tj_with_0, col="blue",axes=FALSE);
abline(h=Suitable_Threshold,col="red", lwd=6);
text(relv_date_num[nrow(Attack_per_hr_relatv)], Suitable_Threshold, paste(Suitable_Threshold), col="black", font=2);
axis(2, at=Attack_per_hr_relatv[,6]);
axis(1, at=relv_date_num, labels=format(x.1_rv,"%m/%d/%y"));
#lines(relv_date_num, Extremes_above_suitable_Tj_with_0, col="black");

<<<<<<< HEAD
<<<<<<< HEAD
# Modified Z-score
=======
# -------------------------------------end of POT

# -------------------------------------------------Modified Z-score
>>>>>>> Updated R codes
=======
# Modified Z-score
>>>>>>> f64debcb10ef6b7b95587d337775eaf0dfed6fb9

MAD_relv=mad(Attack_per_hr_relatv[,6], constant = 1);
x_med=median(Attack_per_hr_relatv[,6]);

M_i=0.6745*(Attack_per_hr_relatv[,6]-x_med)/(MAD_relv);
Outl_MAD<-Attack_per_hr_relatv[which(abs(M_i)>3.5),]

Outl_MAD_with_0<-c();
Outl_MAD_with_0<-matrix(0,nrow(Attack_per_hr_relatv));

for(f in 1:nrow(Outl_MAD))
{
  Outl_MAD_with_0[which(abs(M_i)>3.5)[f]]<-c(Outl_MAD[f,6])
}

<<<<<<< HEAD
<<<<<<< HEAD
# only outliers using M_i;
=======
# -------------- only outliers using M_i;
>>>>>>> Updated R codes
=======
# only outliers using M_i;
>>>>>>> f64debcb10ef6b7b95587d337775eaf0dfed6fb9

plot(relv_date_num, Outl_MAD_with_0, col="red",axes=FALSE);

abline(h=min(Outl_MAD[,6]),col="yellow", lwd=2);
#text(relv_date_num[nrow(Attack_per_hr_relatv)], min(Outl_MAD[,6]), paste(min(Outl_MAD[,6]), col="yelow", font=2))
text(relv_date_num[length(relv_date_num)/2], min(Outl_MAD[,6]), paste("min_Mi=", min(Outl_MAD[,6])), col="black", font=2)

axis(2, at=Attack_per_hr_relatv[,6]);
axis(1, at=relv_date_num, labels=format(x.1_rv,"%m/%d/%y"));

abline(h=Suitable_Threshold,col="blue", lwd=6);
text(relv_date_num[nrow(Attack_per_hr_relatv)], Suitable_Threshold, paste("Tj=", Suitable_Threshold), col="red", font=2);

<<<<<<< HEAD
<<<<<<< HEAD
# Using box plot every day
=======
# -----------------------Using box plot every day
>>>>>>> Updated R codes
=======
# Using box plot every day
>>>>>>> f64debcb10ef6b7b95587d337775eaf0dfed6fb9
mat<-matrix(Attack_per_hr_relatv[,6],24);
boxplot(t(mat), labels=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)); # we can see in which hours the outliers are occuring.
title(ylab="Relative number of attacks in an hours", xlab="Hour", col="blue")
abline(h=0.4, col="red");


# Select the top 10 outliers

out_10<-c();
out_10_a<-c();

out_all<-Outl_MAD[,6];

for(u in 1:10)
{
  out_10<-max(out_all);
  out_10_a<-rbind(out_10_a, Outl_MAD[which(Outl_MAD[,6]==out_10),]);
  out_all[which(Outl_MAD[,6]==out_10)]<--1;
}

  
# ------------label Attackes data per hour Using M_i and Using Sutable Threshold

outlier_v_ind<-which(Outl_MAD_with_0!=0);
Attack_label<-matrix(0,1,length(Outl_MAD_with_0));

Attack_label[outlier_v_ind]=1;

Attack_per_hr_relatv_label<-cbind(Attack_per_hr_relatv, Outl_MAD_with_0, t(Attack_label));

colnames(Attack_per_hr_relatv_label)<-c("Year", "Month", "Day", "Hour", "No.attacks_per_hr", "Relv_No.attacks_per_hr", "Outlier_Value", "Labels")
# View(Attack_per_hr_relatv_label)

write.table(Attack_per_hr_relatv_label, "E:/2015/Attack_per_hr_relatv_label.txt");
write.csv(Attack_per_hr_relatv_label, "E:/2015/Attack_per_hr_relatv_label.csv");

# -----------   label Attackes data per hour Using Sutable Threshold

ST_ind<-which(Extremes_above_suitable_Tj_with_0!=0);
ST_Attack_label<-matrix(0,1,length(Extremes_above_suitable_Tj_with_0));
ST_Attack_label[ST_ind]<-1;

Attack_per_hr_relatv_label_ST<-cbind(Attack_per_hr_relatv, Outl_MAD_with_0, Extremes_above_suitable_Tj_with_0, t(Attack_label),t(ST_Attack_label));

colnames(Attack_per_hr_relatv_label_ST)<-c("Year", "Month", "Day", "Hour", "No.attacks_per_hr", "Relv_No.attacks_per_hr", "Mi_Out_Value", "EVA_Out_Value", "Mi_Label", "EVA_Label")

#View(Attack_per_hr_relatv_label_ST);

write.table(Attack_per_hr_relatv_label_ST, "E:/2015/Attack_per_hr_relatv_label_ST.txt");
write.csv(Attack_per_hr_relatv_label_ST, "E:/2015/Attack_per_hr_relatv_label_ST.csv");

# ------------------------------------ Using Medcouple

medk<-median(Attack_per_hr_relatv_label_ST[,6]);
xi<-Attack_per_hr_relatv_label_ST[which(Attack_per_hr_relatv_label_ST[,6]<median(Attack_per_hr_relatv_label_ST[,6])),6]
xj<-Attack_per_hr_relatv_label_ST[which(Attack_per_hr_relatv_label_ST[,6]>median(Attack_per_hr_relatv_label_ST[,6])),6]
xj_medk<-xj-medk;
xi_medk<-medk-xi;
xj_xi<-xj-xi

MC=median((xj_medk-xi_medk)/xj_xi)
IQR_x=IQR(Attack_per_hr_relatv_label_ST[,6]);
Q1<-quantile(Attack_per_hr_relatv_label_ST[,6], 1/4);
Q3<-quantile(Attack_per_hr_relatv_label_ST[,6], 3/4);
if(MC>0)
{
L=Q1-1.5*IQR_x*exp(-3.5*MC);
U=Q3+1.5*IQR_x*exp(4*MC);
}else
{
  L=Q1-1.5*IQR_x*exp(-4*MC);
  U=Q3+1.5*IQR_x*exp(3.5*MC);
}
which(Attack_per_hr_relatv_label_ST[,6]<=L)
which(Attack_per_hr_relatv_label_ST[,6]>=U)
plot(Attack_per_hr_relatv_label_ST[,6], col="blue")
abline(h=U, col="green")
abline(h=L, col="red")

NdL_U<-Attack_per_hr_relatv_label_ST[which(Attack_per_hr_relatv_label_ST[,6]>=L & Attack_per_hr_relatv_label_ST[,6]<=U), 6]

NdU<-Attack_per_hr_relatv_label_ST[which(Attack_per_hr_relatv_label_ST[,6]<=U),6]
plot(NdU)
hist(NdU)
ST_Nd<-Attack_per_hr_relatv_label_ST[which(Attack_per_hr_relatv_label_ST[,6]<=Suitable_Threshold), 6]

plot(ST_Nd)
hist(ST_Nd)

library("e1071", lib.loc="~/R/win-library/3.1")
skewness(Attack_per_hr_relatv_label_ST[,6])
# 3.149464

View(cbind((xj),(xi),(xj-xi)))
  # SSSBBB
Q1L=quantile(xi,1/4)
Q3L=quantile(xi,3/4)
IQR_L=Q3L-Q1L
Q1R=quantile(xj,1/4)
Q3R=quantile(xj,3/4)
IQR_R=Q3R-Q1R

<<<<<<< HEAD
<<<<<<< HEAD
# -----------------------Using 3*SD
=======
# -----------------------Using 3*SD (3 standard deviation from the mean)

>>>>>>> Updated R codes
=======
# -----------------------Using 3*SD
>>>>>>> f64debcb10ef6b7b95587d337775eaf0dfed6fb9
sd_3<-3*sd(Attack_per_hr_relatv_label_ST[,6]);
x_3sd<-mean(Attack_per_hr_relatv_label_ST[,6])+sd_3;
x_6sd<-mean(Attack_per_hr_relatv_label_ST[,6])+6*sd(Attack_per_hr_relatv_label_ST[,6]);

plot(relv_date_num, Attack_per_hr_relatv_label_ST[,6], type="l", col="blue", xaxt='n', ann=FALSE)
axis(1, at=relv_date_num, labels=format(x.1_rv,"%m/%d/%y"));  

abline(h=U, col="green", lwd=6)
text(relv_date_num[nrow(Attack_per_hr_relatv)],U, paste("HVBP_U=",U), col = "black", font=2);

abline(h=min(Outl_MAD[,6]),col="yellow", lwd=6);
text(relv_date_num[length(relv_date_num)/2], min(Outl_MAD[,6]), paste("min_Mi=", min(Outl_MAD[,6])), col="red", font=2)

abline(h=Suitable_Threshold,col="black", lwd=6);
text(relv_date_num[nrow(Attack_per_hr_relatv)], Suitable_Threshold, paste("Tj=", Suitable_Threshold), col="red", font=2);

abline(h=x_3sd, col="red", lwd=6);
text(relv_date_num[nrow(Attack_per_hr_relatv)],x_3sd, paste(" x_3sd=",x_3sd), col = "yellow", font=2);
title(ylab="Relative number of attacks in an hours", xlab="Hour", col="blue")

U_out<-matrix(0,length(Attack_per_hr_relatv_label_ST[,6]))

U_out[which(Attack_per_hr_relatv_label_ST[,6]>=U)]<-Attack_per_hr_relatv_label_ST[which(Attack_per_hr_relatv_label_ST[,6]>=U),6]
plot(relv_date_num, U_out, col="black", xaxt='n', ann=FALSE)
axis(1, at=relv_date_num, labels=format(x.1_rv,"%m/%d/%y"));  

abline(h=U, col="green")
text(relv_date_num[nrow(Attack_per_hr_relatv)/2],U, paste("HVBP_U=",U), col = "black", font=2);

abline(h=Suitable_Threshold,col="black");
text(relv_date_num[nrow(Attack_per_hr_relatv)/2], Suitable_Threshold, paste("Tj=", Suitable_Threshold), col="red", font=2);

abline(h=min(Outl_MAD[,6]),col="green");
text(relv_date_num[length(relv_date_num)/2], min(Outl_MAD[,6]), paste("min_Mi=", min(Outl_MAD[,6])), col="red", font=2)

abline(h=x_3sd, col="red", lwd=6);
text(relv_date_num[nrow(Attack_per_hr_relatv)],x_3sd, paste(" x_3sd=",x_3sd), col = "white", font=2);
title(ylab="Relative number of attacks in an hours", xlab="Hour", col="blue")

plot(relv_date_num, Attack_per_hr_relatv_label_ST[,6], col="blue", xaxt='n', ann=FALSE)
abline(h=min(Outlier_relv_gre_3[,2]), lwd=6)
text(relv_date_num[nrow(Attack_per_hr_relatv)/2],min(Outlier_relv_gre_3[,2]), paste(" x_3sd_RM=",min(Outlier_relv_gre_3[,2])), col = "red", font=2);
 
<<<<<<< HEAD
<<<<<<< HEAD
# Using sQUARE OF THE DATA
=======
# ----------------------- Using sQUARE OF THE DATA and take 3 std from the mean
>>>>>>> Updated R codes
=======
# Using sQUARE OF THE DATA
>>>>>>> f64debcb10ef6b7b95587d337775eaf0dfed6fb9

sqr<-Attack_per_hr_relatv_label_ST[,6]*Attack_per_hr_relatv_label_ST[,6];
norm_mean<-c()
norm_mean_sq<-c()
for(i in 1: length(sqr))
{
  norm_mean[i]<-(Attack_per_hr_relatv_label_ST[i,6]-mean(Attack_per_hr_relatv_label_ST[,6]))/sd(Attack_per_hr_relatv_label_ST[,6]);
  norm_mean_sq[i]<-(sqr[i]-mean(sqr))/sd(sqr)
}
plot(norm_mean)

length(which(norm_mean>3))
length(which(norm_mean_sq>3))

max(norm_mean_sq[which(norm_mean_sq>3)])
min(Attack_per_hr_relatv_label_ST[which(norm_mean_sq>3),6])
View(log(Attack_per_hr_relatv_label_ST[,6]))

# -----------   label Attackes data per hour Using 3*sd from the mean threshold

ST_ind_3sd<-which(Attack_per_hr_relatv_label_ST[,6]>=x_3sd);
sd3_out_Label<-matrix(0,length(Attack_per_hr_relatv_label_ST[,6]));
sd3_out_Label[which(Attack_per_hr_relatv_label_ST[,6]>=x_3sd)]<-1;

sd3_out_val<-matrix(0,length(Attack_per_hr_relatv_label_ST[,6]));
sd3_out_val[ST_ind_3sd]<-Attack_per_hr_relatv_label_ST[which(Attack_per_hr_relatv_label_ST[,6]>=x_3sd),6]

Attack_per_hr_relatv_label_ST_3sd<-cbind(Attack_per_hr_relatv, Outl_MAD_with_0, Extremes_above_suitable_Tj_with_0, sd3_out_val, t(Attack_label),t(ST_Attack_label),sd3_out_Label);

colnames(Attack_per_hr_relatv_label_ST_3sd)<-c("Year", "Month", "Day", "Hour", "No.attacks_per_hr", "Relv_No.attacks_per_hr", "Mi_Out_Value", "EVA_Out_Value", "3stdv_Out", "Mi_Label", "EVA_Label", "3stdv_Label")

# View(Attack_per_hr_relatv_label_ST_3sd)

write.table(Attack_per_hr_relatv_label_ST_3sd, "E:/2015/Attack_per_hr_relatv_label_ST_3sd.txt");
write.csv(Attack_per_hr_relatv_label_ST_3sd, "E:/2015/Attack_per_hr_relatv_label_ST_3sd.csv");

# write all attacks data into csv
Attacks_16278463_hr_vec_14 <- read.delim("E:/R_Docs/Attacks_16278463_hr_vec_14.dat");

write.table(Attacks_16278463_hr_vec_14, "E:/2015/Attacks_16278463_hr_vec_14.txt");
write.csv(Attacks_16278463_hr_vec_14, "E:/2015/Attacks_16278463_hr_vec_14.csv");
Attacks_16278463_hr_vec_1048575<-Attacks_16278463_hr_vec_14[1:1048575,]

write.table(Attacks_16278463_hr_vec_1048575, "E:/2015/Attacks_16278463_hr_vec_1048575.txt");
write.csv(Attacks_16278463_hr_vec_1048575, "E:/2015/Attacks_16278463_hr_vec_1048575.csv");

# read.csv("E:/2015/Attack_per_hr_relatv_label_ST_3sd.csv");

Only_Outliers_3stdv <- read.csv("E:/2015/14_01_2015/Only_Outliers_3stdv.csv", sep=";");

table(Only_Outliers_3stdv[,3])
table(Only_Outliers_3stdv[,4])
table(Only_Outliers_3stdv[,5])
table(Only_Outliers_3stdv[,6])

# Outlier only from 3std in RapidMiner
table(Outlier_relv_gre_3[,3])
table(Outlier_relv_gre_3[,4])
table(Outlier_relv_gre_3[,5])
table(Outlier_relv_gre_3[,6])

<<<<<<< HEAD
<<<<<<< HEAD
# 15-01-2015

# ------------------------------- Up to this is correct
=======
# ---------------- code until 15-01-2015

>>>>>>> Updated R codes
=======
# 15-01-2015

# ------------------------------- Up to this is correct
>>>>>>> f64debcb10ef6b7b95587d337775eaf0dfed6fb9
# ---------------------------------------------------------------Logistic Regression 

Attack_per_hr_relatv_label1 <- read.csv("E:/2015/Attack_per_hr_relatv_label.csv");
# read.csv("E:/2015/Attack_per_hr_relatv_label_ST.csv");

<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> f64debcb10ef6b7b95587d337775eaf0dfed6fb9

library(aod)
library(ggplot2)


<<<<<<< HEAD
=======
library(aod)
library(ggplot2)

>>>>>>> Updated R codes
=======
>>>>>>> f64debcb10ef6b7b95587d337775eaf0dfed6fb9
Attack_per_hr_relatv_label<-Attack_per_hr_relatv_label1[,2:9];
mydata<-data.frame(Attack_per_hr_relatv_label);
# View(Attack_per_hr_relatv_label);
sapply(mydata, sd);

mydata$Year <- factor(mydata$Year);
mydata$Month <- factor(mydata$Month);
mydata$Day <- factor(mydata$Day);
mydata$Hour <- factor(mydata$Hour);
mydata$Labels <- factor(mydata$Labels);

## two-way contingency table of categorical outcome and predictors we want to make sure there are not 0 cells
xtabs(~Labels + Year, data = mydata);
xtabs(~Labels + Month, data = mydata);
xtabs(~Labels + Day, data = mydata);
xtabs(~Labels + Hour, data = mydata);


mylogit1 <- glm(Labels ~ Year + Month + Day + Hour, data = mydata, family = "binomial")

summary(mylogit1);

<<<<<<< HEAD
<<<<<<< HEAD
=======
# ---------------------- consider only Month, day and hour variables b/c year is specific variable

mylogit_MDH <- glm(Labels ~ Month + Day + Hour, data = mydata, family = "binomial")

summary(mylogit_MDH);

# ---------------- There is no change on the result of the modeling by excluding year variable
# ----- (Intercept) -3.017e+00  3.135e-01  -9.625  < 2e-16 ***
# ------Month5       3.878e-01  1.899e-01   2.042  0.04112 *  
# ------Month7       4.168e-01  2.106e-01   1.979  0.04784 *  
# ----- Month10     -4.716e-01  2.328e-01  -2.026  0.04277 *  
# ------Hour3       -1.013e+00  3.432e-01  -2.952  0.00316 ** 
# ------Hour4       -7.851e-01  3.180e-01  -2.469  0.01356 *  
# ----- Hour5       -5.980e-01  3.001e-01  -1.993  0.04627 *    
# ------Hour7       -6.568e-01  3.055e-01  -2.150  0.03155 *   
# ------Hour9       -4.894e-01  2.907e-01  -1.684  0.09222 .
# ------Hour17      -4.876e-01  2.907e-01  -1.678  0.09343 . 

>>>>>>> Updated R codes
=======
>>>>>>> f64debcb10ef6b7b95587d337775eaf0dfed6fb9
# Overall Wald for Month
wald.test(b = coef(mylogit1), Sigma = vcov(mylogit1), Terms = 4:14);
    # Wald test:
      #----------
  
      # Chi-squared test:
      # X2 = 25.8, df = 11, P(> X2) = 0.0069

# Overall Wald for Day
wald.test(b = coef(mylogit1), Sigma = vcov(mylogit1), Terms = 15:44);
    #Wald test:
      #----------
  
      #Chi-squared test:
      #X2 = 25.5, df = 30, P(> X2) = 0.7    

# Overall Wald for Hour
wald.test(b = coef(mylogit1), Sigma = vcov(mylogit1), Terms = 45:67);
      #Wald test:
        # ----------
  
        #Chi-squared test:
        #X2 = 37.6, df = 23, P(> X2) = 0.028



## odds ratios only
exp(coef(mylogit1))

## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit1), confint(mylogit1)))

# library(ResourceSelection)
<<<<<<< HEAD
<<<<<<< HEAD
hl <- hoslem.test(mylogit1$Labels, fitted(mylogit1), g=10)
=======
hl <- hoslem.test(as.numeric(mydata$Labels), fitted(mylogit1), g=10) # hl is HL not h1 
# ----- hl
# ------ Hosmer and Lemeshow goodness of fit (GOF) test

# ----- data:  as.numeric(mydata$Labels), fitted(mylogit1)
# ----- X-squared = 545199.2, df = 8, p-value < 2.2e-16

# -----------check the AIC (Akaike criterion) and BIC to check model accuracy
# ---- install.packages("ResourceSelection")
AIC(mylogit1)
#   [1] 5335.327
BIC(mylogit1)
#   [1] 5852.891

>>>>>>> Updated R codes
=======
hl <- hoslem.test(mylogit1$Labels, fitted(mylogit1), g=10)
>>>>>>> f64debcb10ef6b7b95587d337775eaf0dfed6fb9
# To find the difference in deviance for the two models (i.e., the test statistic) we can use the command:
with(mylogit1, null.deviance - deviance)
# The degrees of freedom for the difference between the two models is equal to the number of predictor variables in the mode, and can be obtained using:
with(mylogit1, df.null - df.residual)

<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> f64debcb10ef6b7b95587d337775eaf0dfed6fb9
    # Finally, the p-value can be obtained using:
  
  with(mylogit1, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

# The chi-square of 41.46 with 5 degrees of freedom and an associated p-value of less than 0.001 
# tells us that our model as a whole fits significantly better than an empty model. This is sometimes 
# called a likelihood ratio test (the deviance residual is -2*log likelihood). To see the model's log likelihood, we type:

logLik(mylogit1)

# Note: Dummy 
# sr.f = factor(`d1.attacks_1`[,4])

dummies = model.matrix(~sr.f)
<<<<<<< HEAD
=======
    # Finally, the p-value can be obtained using: P_value = 0.01153191
  
  with(mylogit1, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

# The chi-square of 94.83619 with 66 degrees of freedom and an associated p-value of less than 0.05 
# tells us that our model as a whole fits significantly better than an empty model. This is sometimes 
# called a likelihood ratio test (the deviance residual is -2*log likelihood). 
# To see the model's log likelihood, we type:

logLik(mylogit1)

>>>>>>> Updated R codes
=======
>>>>>>> f64debcb10ef6b7b95587d337775eaf0dfed6fb9

# Cook's Distance

cooks.distance(mylogit1)
r = rstudent(mylogit1)

cook = cooks.distance(mylogit1)
plot(cook,ylab="Cooks distances")

qqnorm(mylogit1$res)
qqline(mylogit1$res)
hist(mylogit1$res)

# ---------up to this --------------- Logistic is Correct 

<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> f64debcb10ef6b7b95587d337775eaf0dfed6fb9
mydata$No.attacks_per_hr <- Attack_per_hr_relatv_label[,6];
mylogit <- glm(Labels ~ Year + Month + Day + Hour + No.attacks_per_hr, data = mydata, family = "binomial")

glm(Labels ~ as.numeric(No.attacks_per_hr), data = mydata, family = "binomial");

summary(mylogit);



install.packages("arules");
July_2010 <- read.csv("E:/Months_Attacks_Data_31_12_2014/July_2010.csv");

tr<-data.frame(July_2010[3]);
inspect(tr)
image(tr)
itemFrequencyPlot(tr, support = 0.1)


# Read from file and calculate the frequency of Source attribute

August_2010 <- read.csv("E:/Months_Attacks_Data_31_12_2014/August_2010.csv");

count_all<-table(July_2010[3]);
sum_all_count<-sum(table(July_2010[3]));

relv_count_src<-c();

for(c in 1: length(table(July_2010[3])))
{
  relv_count_src<-c(relv_count_src, count_all[c]/sum_all_count)
}


ind_src_0p5<-which(relv_count_src*100>=0.5);
Vlue_src_0p5<-relv_count_src[which(relv_count_src*100>=0.5)];
cbind_Vlue_src_0p5<-cbind(count_all[ind_src_0p5], Vlue_src_0p5, Vlue_src_0p5*100);

coded_source_v<-matrix(0, length(relv_count_src),4);

coded_source_v[ind_src_0p5,1:3]=cbind_Vlue_src_0p5;

Percent_src<-c();

for(i in 1:nrow(coded_source_v))
  
{
  if(coded_source_v[i,3]!=0)
  {
    coded_source_v[i,4]<-i;
  }else
  {
    coded_source_v[i,4]<-0;
  }
}

# View(coded_source_v);

All_count_coded<-cbind(count_all,coded_source_v);

rowname_coded<-rownames(All_count_coded);
July_2010_with_src_cod<-matrix(0,nrow(July_2010),ncol(July_2010)+1);
July_2010_with_src_cod[,1:13]=July_2010;
View(July_2010_with_src_cod);


for(j in 1:length(rowname_coded))
{
  index_rowma<-which(July_2010[3]==rowname_coded[j])
  July_2010_with_src_cod[index_rowma,14]<-
}

rownames(All_count_coded)

#  -------------------- Up to this correct 12-12-2014

<<<<<<< HEAD
=======
#-------------------------
>>>>>>> Updated R codes
=======
>>>>>>> f64debcb10ef6b7b95587d337775eaf0dfed6fb9

# ---------change to normally distributed data by logarithms -- read from storage

# No_attacks_relav_res[,7]<-Attack_per_hr_relatv[,6];
# No_attacks_relav_res <- read.csv("E:/No_attacks_relav_res.csv");

loggd<-c();
relv_mean<-c();

for (l in 1:nrow(No_attacks_relav_res))
{
  if(No_attacks_relav_res[l,7]>0)
  {
    loggd<-c(loggd, log(No_attacks_relav_res[l,7])) ;
    relv_mean<-c(relv_mean, No_attacks_relav_res[l,7])
<<<<<<< HEAD
<<<<<<< HEAD
   
=======
    
>>>>>>> Updated R codes
=======
   
>>>>>>> f64debcb10ef6b7b95587d337775eaf0dfed6fb9
  }else 
  {
    loggd<-c(loggd, log(min(No_attacks_relav_res[which(No_attacks_relav_res[,7]!=0),7]))) ;
    relv_mean<-c(relv_mean, min(No_attacks_relav_res[which(No_attacks_relav_res[,7]!=0),7]));
<<<<<<< HEAD
<<<<<<< HEAD
     
=======
    
>>>>>>> Updated R codes
=======
     
>>>>>>> f64debcb10ef6b7b95587d337775eaf0dfed6fb9
  }
  
}

# loggd<-log(No_attacks_relav_res[which(No_attacks_relav_res[,7]!=0),7])

# Numb_attacks_relv_loggd<-cbind(No_attacks_relav_res, loggd)

write.csv(Numb_attacks_relv_loggd, "E:/24_12_2014/Numb_attacks_relv_loggd.csv");

Numb_attacks_relv_log_relv_mean<-cbind(No_attacks_relav_res, loggd, relv_mean)

write.csv(Numb_attacks_relv_log_relv_mean, "E:/24_12_2014/Numb_attacks_relv_log_relv_mean.csv");

hist(relv_mean)

hist(loggd);
plot(loggd, col="blue")

# MAD_relv=mad(loggd, constant = 1);
# x_med=median(loggd);

# M_i=0.6745*(loggd-x_med)/(MAD_relv);
# Outl_MAD<-loggd[which(abs(M_i)>3.5)]
# stdv<--sd(loggd)


plot(-loggd, col="blue");

abline(h=min(Outl_MAD),col="red", lwd=2);
abline(h=max(Outl_MAD),col="red", lwd=2);
abline(h=stdv,col="red", lwd=2);

abline(h=-5*MAD_relv,col="red", lwd=2);

No_attacks_relav_res[which(abs(M_i)>3.5),]


text(relv_date_num[length(relv_date_num)/2], min(Outl_MAD[,6]), paste("min_Mi=", min(Outl_MAD[,6])), col="black", font=2)
# Z-Score

Z_transform<-c();
x_mu<-c();

for (l in 1:nrow(No_attacks_relav_res))
{
  x_mu<-(No_attacks_relav_res[l,7]-mean(No_attacks_relav_res[,7]))/sd(No_attacks_relav_res[,7])    
  Z_transform<-c(Z_transform, x_mu)
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> f64debcb10ef6b7b95587d337775eaf0dfed6fb9
      
}


# 18_12_2014 read dummy

Export_August_04_dummy <- read.csv("E:/RapidMiner_Docs/Export_August_04_dummy.csv", sep=";")
Export_August_04_dummy_r<-write.csv(Export_August_04_dummy, "E:/RapidMiner_Docs/Export_August_04_dummy_r.csv")

<<<<<<< HEAD
=======
  
}

# -------------- end of logarithmic transformation
>>>>>>> Updated R codes
=======
>>>>>>> f64debcb10ef6b7b95587d337775eaf0dfed6fb9

# ----------------------Time series Methods

Num_attack_res.ts<-ts(Attack_per_hr_relatv[,5]);
hist(Num_attack_res.ts);
d1<-diff(Num_attack_res.ts,differences=1)

hist(d1);
x<-diff(ts(xx))

Num_attack_rlv.ts<-ts(Attack_per_hr_relatv[,6]);

plot(relv_date_num, Num_attack_rlv.ts)

plot(ts(Attack_per_hr_relatv[,6],frequency = 24*30*12, start=c(2010,7), end=c(2012,6)))

# ----------------------
<<<<<<< HEAD
<<<<<<< HEAD
=======



# -----------------------------------------------correct aggregated code end 23-02-2015S

# ---------------- coding into dummy variables

# Note: Dummy 
# sr.f = factor(`d1.attacks_1`[,4])

dummies = model.matrix(~sr.f)


# 18_12_2014 read dummy

Export_August_04_dummy <- read.csv("E:/RapidMiner_Docs/Export_August_04_dummy.csv", sep=";")
Export_August_04_dummy_r<-write.csv(Export_August_04_dummy, "E:/RapidMiner_Docs/Export_August_04_dummy_r.csv")


>>>>>>> Updated R codes
=======
>>>>>>> f64debcb10ef6b7b95587d337775eaf0dfed6fb9
# August data multinomial logistic regressions

Attacks_August_08 <- read.csv("E:/RapidMiner_Docs/Attacks_August_08.csv")
rm(list=ls());
require(foreign)
require(nnet)
data_log<-cbind(Attacks_August_08[,3:8],Attacks_August_08[,13])
#summary(Attacks_August_08)
summary(data_log)

m11<-data.frame(data_log);
m11<-as.factor(m11);

names(m11)<-c("source","sport", "dest", "dport", "sensorid", "atype", "hour")


m11$source<- as.factor(m11$source)
m11$sport<- as.factor(m11$sport)
m11$dest<- as.factor(m11$dest)
m11$dport<- as.factor(m11$dport)
m11$sensorid<- as.factor(m11$sensorid)
m11$atype<- as.factor(m11$atype)
m11$hour<- as.factor(m11$hour);

summary(m11)

m11$atype2<- relevel(m11$atype, ref="5");

m2<- multinom(m11$atype2~ m11$source+m11$sport+m11$dest+m11$dport+m11$sensorid+m11$hour, data=m11);

m21<- multinom(m11$atype2~ m11$source+m11$sensorid+m11$hour);

m33=cbind(Dummy_coded_24_series.data[,1:176],Dummy_coded_24_series.data[,178])

multinom(m33$atype2~ m33);

multinom(m11$atype2~ m11$source+m11$dest+m11$dport+m11$sensorid+m11$hour, data=m11);

multinom(m11$atype2~ m11$source+m11$dest+m11$sensorid+m11$hour, data=m11);

Attacks_August_08_spss<-write.csv(Dummy_coded_24_series.data, "E:/RapidMiner_Docs/Series_24dummy_coded.csv")

# --------------------------------- Decision Tree

Attacks_16278463_hr_vec_14[1,1];


source<-factor(Attacks_16278463_hr_vec_14$source)
sport<-factor(Attacks_16278463_hr_vec_14$sport)
dest<-factor(Attacks_16278463_hr_vec_14$dest)
dport<-factor(Attacks_16278463_hr_vec_14$dport)
sensorid<-factor(Attacks_16278463_hr_vec_14$sensorid)
year<-factor(Attacks_16278463_hr_vec_14$year)
month<-factor(Attacks_16278463_hr_vec_14$month)
day<-factor(Attacks_16278463_hr_vec_14$day)
hour<-factor(Attacks_16278463_hr_vec_14$hour)


barplot(table(Attacks_16278463_hr_vec_14$dest), col="blue"); # Destination

title(xlab="Destination IP address", ylab="Count of destination IP", col="blue")

barplot(table(Attacks_16278463_hr_vec_14$source), col="blue"); # Destination

title(xlab="source IP address", ylab="Count of source IP", col="blue")

ind <- sample(2, nrow(Attacks_16278463_hr_vec_14), replace=TRUE, prob=c(0.7, 0.3))
trainData <- Attacks_16278463_hr_vec_14[ind==1,]
testData <- Attacks_16278463_hr_vec_14[ind==2,]

# library(party)
myFormula <- atype ~ source + sport + dest + dport + sensorid + year + month + day + hour;
atype_ctree <- ctree(myFormula, data=trainData)
 # check the prediction
 table(predict(atype_ctree), trainData$atype)

plot(atype_ctree)

Decision_Tree_result_Database <- read.csv("E:/2015/Results/Decision_Tree_result_Database.csv")

table(Decision_Tree_result_Database[2:700,14])
# Decision_Tree_result_Database[2:nrow(Decision_Tree_result_Database)-2,14];


prediction_0<-rbind(Decision_Tree_result_Database[1,], Decision_Tree_result_Database[which(Decision_Tree_result_Database[,14]==0),]);
prediction_3<-rbind(Decision_Tree_result_Database[1,], Decision_Tree_result_Database[which(Decision_Tree_result_Database[,14]==3),]);
prediction_4<-rbind(Decision_Tree_result_Database[1,], Decision_Tree_result_Database[which(Decision_Tree_result_Database[,14]==4),]);
prediction_5<-rbind(Decision_Tree_result_Database[1,], Decision_Tree_result_Database[which(Decision_Tree_result_Database[,14]==5),]);
prediction_7<-rbind(Decision_Tree_result_Database[1,], Decision_Tree_result_Database[which(Decision_Tree_result_Database[,14]==7),]);


write.table(prediction_0, "E:/2015/Results/prediction_0.txt");
write.csv(prediction_0, "E:/2015/Results/prediction_0.csv");

write.table(prediction_3, "E:/2015/Results/prediction_3.txt");
write.csv(prediction_3, "E:/2015/Results/prediction_3.csv");

write.table(prediction_4, "E:/2015/Results/prediction_4.txt");
write.csv(prediction_4, "E:/2015/Results/prediction_4.csv");

write.table(prediction_5, "E:/2015/Results/prediction_5.txt");
write.csv(prediction_5, "E:/2015/Results/prediction_5.csv");

write.table(prediction_7, "E:/2015/Results/prediction_7.txt");
write.csv(prediction_7, "E:/2015/Results/prediction_7.csv");

max=max(Decision_Tree_result_Database[3:700,15]);


#names_dt<-data.frame(Decision_Tree_result_Database[1,]);

#colnames(prediction_0) <- names_dt;
#colnames(prediction_3) <- names_dt;
#colnames(prediction_4) <- names_dt;
#colnames(prediction_5) <- names_dt;
#colnames(prediction_7) <- names_dt


#View(prediction_0)
#View(prediction_3)
#View(prediction_4)
#View(prediction_5)
#View(prediction_7)
vector_rlv<-Attack_per_hr_relatv_label1[, 7];
install.packages("pracma");
library("pracma", lib.loc="~/R/win-library/3.1");
Hour_relv_vec<-Reshape(vector_rlv, 24, length(vector_rlv)/24);
#Hour_relv_vec<-matrix(Hour_relv_vec, 24, ncol = round(length(Hour_relv_vec)/24));

boxplot(t(Hour_relv_vec));

max_vec_hr<-c();

for(i in 1:24)
{
  max_vec_hr<-c(max_vec_hr, max(Hour_relv_vec[i,]));
}
abline(h=0.3, col="blue")
#lines(max_vec_hr, col="red")

lines(1:length(max_vec_hr), max_vec_hr, col="red");
title(ylab="Relative number of attacks in an hours", xlab="Hour", col="blue")

# ----------------------------- Classification Tree with rpart

library(rpart)
<<<<<<< HEAD
<<<<<<< HEAD
=======
DT_data<-cbind(Attacks_16278463_hr_vec_14[,2:8], Attacks_16278463_hr_vec_14[,12:14])

>>>>>>> Updated R codes
=======
>>>>>>> f64debcb10ef6b7b95587d337775eaf0dfed6fb9
severity_fac<- factor(Attacks_16278463_hr_vec_14[,2]);
Source_fac<- factor(Attacks_16278463_hr_vec_14[,3]);
sport_fac<- factor(Attacks_16278463_hr_vec_14[,4]);
dest_fac<- factor(Attacks_16278463_hr_vec_14[,5]);
dport_fac<- factor(Attacks_16278463_hr_vec_14[,6]);
sensorid_fac<- factor(Attacks_16278463_hr_vec_14[,7]);
atype_fac<- factor(Attacks_16278463_hr_vec_14[,8]);
month_fac<- factor(Attacks_16278463_hr_vec_14[,12]);
day_fac<- factor(Attacks_16278463_hr_vec_14[,13]);
hour_fac<- factor(Attacks_16278463_hr_vec_14[,14]);

# grow tree 
<<<<<<< HEAD
<<<<<<< HEAD
fit <- rpart(Kyphosis ~ Age + Number + Start,
             method="class", data=kyphosis)
=======
fit <- rpart(atype_fac ~ severity_fac + Source_fac + sport_fac +dest_fac + dport_fac + sensorid_fac + 
               month_fac + day_fac + hour_fac,
             method="class", data=DT_data);


# ----------------------------------------- Need to be edited

# ----------------- codding Soucrce into numeric

# ------------------------- Frequency of Source attribute

install.packages("arules");

# Read from file and calculate the frequency of Source attribute every month

July_2010 <- read.csv("E:/2015/Months_Attacks_Data_31_12_2014/July_2010.csv");
August_2010 <- read.csv("E:/2015/Months_Attacks_Data_31_12_2014/August_2010.csv");

count_all<-table(July_2010[3]);
sum_all_count<-sum(table(July_2010[3]));

relv_count_src<-c();

for(c in 1: length(table(July_2010[3])))
{
  relv_count_src<-c(relv_count_src, count_all[c]/sum_all_count)
}


ind_src_0p5<-which(relv_count_src*100>=0.5);
Vlue_src_0p5<-relv_count_src[which(relv_count_src*100>=0.5)];
cbind_Vlue_src_0p5<-cbind(count_all[ind_src_0p5], Vlue_src_0p5, Vlue_src_0p5*100);

coded_source_v<-matrix(0, length(relv_count_src),4);

coded_source_v[ind_src_0p5,1:3]=cbind_Vlue_src_0p5;

Percent_src<-c();

for(i in 1:nrow(coded_source_v))
  
{
  if(coded_source_v[i,3]!=0)
  {
    coded_source_v[i,4]<-i;
  }else
  {
    coded_source_v[i,4]<-0;
  }
}

# View(coded_source_v);

All_count_coded<-cbind(count_all,coded_source_v);

rowname_coded<-rownames(All_count_coded);
July_2010_with_src_cod<-matrix(0,nrow(July_2010),ncol(July_2010)+1);
July_2010_with_src_cod[,1:13]=July_2010;
# View(July_2010_with_src_cod);


for(j in 1:length(rowname_coded))
{
  index_rowma<-which(July_2010[3]==rowname_coded[j]);
  July_2010_with_src_cod[index_rowma[1],13]<-j;
}
# -------------------------------------------------



>>>>>>> Updated R codes
=======
fit <- rpart(Kyphosis ~ Age + Number + Start,
             method="class", data=kyphosis)
>>>>>>> f64debcb10ef6b7b95587d337775eaf0dfed6fb9
