library(tidyverse)
#3.2.1
na<-which(is.na(total_info$X15岁以上人口受教育年限))
kc<-kmeans(na.omit(total_info$X15岁以上人口受教育年限),4)
cluster<-(kc$cluster)
cluster<-append(cluster,'',after = na-1)
total_info$year<-cluster

total_info$year[which(total_info$year==1)]<-'年限短'
total_info$year[which(total_info$year==2)]<-'年限一般'
total_info$year[which(total_info$year==3)]<-'年限较长'
total_info$year[which(total_info$year==4)]<-'年限长'

 
#3.2.2
total_info$pop[total_info$地区总人口>=8e7]=1
total_info$pop[total_info$地区总人口<8e7]=0
noarmy<-total_info[-28,]

woe_f<-function(df,flag_loc,var_loc){
    n_1t<-table(df[,flag_loc])[2]
    n_0t<-table(df[,flag_loc])[1]
    
    varname<-names(df)[var_loc]
    
    fact_n<-as.vector(unique(df[,var_loc]))
    
    fact<-c()
    woe<-c()
    iv<-c()
    
    for (i in fact_n){
      subsetdf<-paste("subset(df,",varname,"=='",i,"')",sep="")
      df_i<-eval(parse(text=subsetdf))
      n_1i<-table(df_i[,flag_loc])[2]
      n_0i<-table(df_i[,flag_loc])[1]
      
      p_1i<-n_1i/n_1t
      p_0i<-n_0i/n_0t
      
      woe_i<-log(p_1i/p_0i)
      iv_i<-(p_1i-p_0i)*woe_i
      
      fact<-c(fact,i)
      woe<-c(woe,woe_i)
      iv<-c(iv,iv_i)
    }
    
    woeres<-paste("woeres<-data.frame(",varname,"=fact,woe_",varname,"=woe)",sep='')
    eval(parse(text=woeres))
    
    ivres<-sum(iv)
    return(list(woe=woeres,iv=ivres))
  }




WOE_year<-woe_f(noarmy,which(names(noarmy)=='pop'),which(names(noarmy)=='year'))

#3.2.3
wy<-function(){
na<-which(is.na(total_info$X15岁以上人口受教育年限))
kc<-kmeans(na.omit(total_info$X15岁以上人口受教育年限),4)
cluster<-(kc$cluster)
cluster<-append(cluster,'',after = na-1)
total_info$year<-cluster

total_info$year[which(total_info$year==1)]<-'年限短'
total_info$year[which(total_info$year==2)]<-'年限一般'
total_info$year[which(total_info$year==3)]<-'年限较长'
total_info$year[which(total_info$year==4)]<-'年限长'

total_info$pop[total_info$地区总人口>=8e7]=1
total_info$pop[total_info$地区总人口<8e7]=0
noarmy<-total_info[-28,]

woe_f<-function(df,flag_loc,var_loc){
  n_1t<-table(df[,flag_loc])[2]
  n_0t<-table(df[,flag_loc])[1]
  
  varname<-names(df)[var_loc]
  
  fact_n<-as.vector(unique(df[,var_loc]))
  
  fact<-c()
  woe<-c()
  iv<-c()
  
  for (i in fact_n){
    subsetdf<-paste("subset(df,",varname,"=='",i,"')",sep="")
    df_i<-eval(parse(text=subsetdf))
    n_1i<-table(df_i[,flag_loc])[2]
    n_0i<-table(df_i[,flag_loc])[1]
    
    p_1i<-n_1i/n_1t
    p_0i<-n_0i/n_0t
    
    woe_i<-log(p_1i/p_0i)
    iv_i<-(p_1i-p_0i)*woe_i
    
    fact<-c(fact,i)
    woe<-c(woe,woe_i)
    iv<-c(iv,iv_i)
  }
  
  woeres<-paste("woeres<-data.frame(",varname,"=fact,woe_",varname,"=woe)",sep='')
  eval(parse(text=woeres))
  
  ivres<-sum(iv)
  return(list(woe=woeres,iv=ivres))
}




a<-woe_f(noarmy,which(names(noarmy)=='pop'),which(names(noarmy)=='year'))

a<-data.frame(a)

names(a)[1:3]<-c('year','WOE_year','iv')

return(merge(total_info,a[,1:2],by='year',all.x=T))


}
wy<-wy()
    