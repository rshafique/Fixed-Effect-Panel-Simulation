########Clearing Memory and Setting directory and formats

#packages = sqldf, DataCombine, plm

# install.packages("sqldf")
# install.packages("DataCombine")
# install.packages("plm")
# install.packages("Formula")
# install.packages("mgcv")

library(sqldf)
library(DataCombine)
library(Formula)
library(mgcv)
library(plm)

rm(list=ls())
setwd("C:/Users/azrasel/Desktop/Freebasics")
options(scipen=999)



#########Creating Full Period data for all MSISDNs

full_period<-1:50
final_obs<-1:5000
base_data<-as.data.frame(rep(full_period,length(final_obs)))
names(base_data)<-"week_num"
base_data$unq_id<-rep(sort(final_obs),each=length(full_period))





#########Creating hypothetical parameters
set.seed(20)
alpha=rexp(length(final_obs), rate=1/20000)
alpha<-ifelse(alpha<1000,1000,alpha)
beta=rnorm(length(full_period),mean=0,sd=1000)
gama=2000
optin_week_num=sample(c(rep(0,length(final_obs)*400/20000),(1:length(full_period))),size=length(final_obs),replace=T)
trend<-0.00


#table(optin_week_num)

##hist(optin_week_num)
optin_tab<-as.data.frame(cbind(final_obs,optin_week_num))


library(sqldf)
base_data<-sqldf("select a.*, b.optin_week_num 
                 from base_data a
                 left join optin_tab b
                 on a.unq_id=b.final_obs"
)

#sqldf("select optin_week_num, count(unq_id), count(distinct unq_id) from base_data group by optin_week_num")





#########Creating Dummy for Optin
data4<-base_data
data4$optin_dummy<-ifelse(
  (data4$week_num>=data4$optin_week_num)&(data4$optin_week_num!=0), 1, 0)


#########Creating ARPU
data4$arpu<-0
for(i in 1:length(final_obs)){
  data4$arpu[((i-1)*length(full_period)+1):(i*length(full_period))]<-rep(alpha[i],length(full_period))+
    beta+rnorm(length(full_period),mean=0,sd=alpha[i]*.1)+
    rep(gama,length(full_period))*data4$optin_dummy[((i-1)*length(full_period)+1):(i*length(full_period))]+
    if(min(data4$optin_week_num[((i-1)*length(full_period)+1):(i*length(full_period))])>0){
      abs(alpha[i]*trend)*full_period} else {-abs(alpha[i]*trend)*full_period 
      } 
}





#########clearing memory except final dataset
%data4$arpu<-data4$arpu/100
data_final<-data4
rm(list=setdiff(ls(), c("data_final", "data4")))





#########Creating weekly dummies

for (i in min((data_final$week_num)+1):(max(data_final$week_num))){
  data_final[ncol(data_final)+1]<-ifelse(data_final$week_num==i,1,0)
  names(data_final)[ncol(data_final)]<-paste("weekly_dummy_",i,sep="")
}





#########Creating Delta of variables

library(DataCombine)
#data_final$arpu<-log(data_final$arpu+1)
names(data_final)[5]<-"log_arpu"
data_final<-data_final[-3]
for (i in 3:4){
  dd<-slide(data_final, Var=names(data_final[i]), 
            NewVar=paste(names(data_final[i]),"_lag_1",sep=""),
            GroupVar = "unq_id", slideBy = -1)
  data_final[ncol(data_final)+1]<-data_final[i]-dd[ncol(dd)]
  rm(dd)
  names(data_final)[ncol(data_final)]<-paste("delta",names(data_final[i]),sep="_")
}




#########Creating data for final modeling and clearing memory from unnecessay things

data_plm<-data_final[c(1,2,5:ncol(data_final))]
data_plm<-subset(data_plm, week_num!=1)
rm(list=setdiff(ls(), c("data_plm","data4")))





#########Running model

library(plm)
mod<-plm(delta_log_arpu~delta_optin_dummy +
           weekly_dummy_3            +
           weekly_dummy_4            +
           weekly_dummy_5            +
           weekly_dummy_6            +
           weekly_dummy_7            +
           weekly_dummy_8            +
           weekly_dummy_9            +
           weekly_dummy_10          +
           weekly_dummy_11          +
           weekly_dummy_12          +
           weekly_dummy_13          +
           weekly_dummy_14          +
           weekly_dummy_15          +
           weekly_dummy_16          +
           weekly_dummy_17          +
           weekly_dummy_18          +
           weekly_dummy_19          +
           weekly_dummy_20          +
           weekly_dummy_21          +
           weekly_dummy_22          +
           weekly_dummy_23          +
           weekly_dummy_24          +
           weekly_dummy_25          +
           weekly_dummy_26          +
           weekly_dummy_27          +
           weekly_dummy_28          +
           weekly_dummy_29          +
           weekly_dummy_30          +
           weekly_dummy_31          +
           weekly_dummy_32          +
           weekly_dummy_33          +
           weekly_dummy_34          +
           weekly_dummy_35          +
           weekly_dummy_36          +
           weekly_dummy_37          +
           weekly_dummy_38          +
           weekly_dummy_39          +
           weekly_dummy_40          +
           weekly_dummy_41          +
           weekly_dummy_42          +
           weekly_dummy_43          +
           weekly_dummy_44          +
           weekly_dummy_45          +
           weekly_dummy_46          +
           weekly_dummy_47          +
           weekly_dummy_48          +
           weekly_dummy_49          +
           weekly_dummy_50          
         ,data=data_plm,index=c("unq_id","week_num"),model="within")
summary(mod)




#########Getting intercept term
#mean(fixef(mod))
mean(data4$arpu[data4$optin_week_num==0])

mean(data4$arpu[data4$optin_week_num> 0  & data4$optin_dummy ==0])
mean(data4$arpu[data4$optin_week_num> 0  & data4$optin_dummy ==1])

mean(data4$arpu[data4$optin_week_num> 0  & data4$optin_dummy ==1]) - mean(data4$arpu[data4$optin_week_num> 0  & data4$optin_dummy ==0])


