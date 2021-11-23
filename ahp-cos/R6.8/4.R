library("readxl")
library("dplyr")
library("magrittr")
#4.2.1
#准则层
#C1=“地区总人口
#C2=“性别比”
#C3=“15-59岁人口比重”
#c4=15岁以上人口受教育年限”
#方案层
#Fi
t<-total_info[-28,]
row.names(t)<-c(1:31)
t_c1<-as.double(t$地区总人口)
t_c2<-t$性别比
t_c3<-t$X15.59岁
t_c4<-t$X15岁以上人口受教育年限


a2m<-function(t){
m<-matrix(0,nrow = 31,ncol = 31)
for (i in c(1:31)){
  for(j in c(1:31)){
    m[i,j]=t[i]/t[j]
  }
}
return(m)
}

data_F1<-a2m(t_c1)
data_F2<-a2m(t_c2)
data_F3<-a2m(t_c3)
data_F4<-a2m(t_c4)

#准则层判断矩阵
data_C <- matrix(c(1,5,3,7,1/5,1,1/3,5,1/3,3,1,7,1/7,1/5,1/7,1),nrow =4,dimnames = list(c("C1","C2","C3","C4"),c("C1","C2","C3","C4"))
)
#归一
Weigth_fun <- function(data){
  if(class(data) == 'matrix'){
    data = data     
  } else {
    if ( class(data) == 'data.frame' & nrow(data) == ncol(data) - 1 & is.character(data[,1,drop = TRUE])){
      data = as.matrix(data[,-1])
    } else if (class(data) == 'data.frame' & nrow(data) == ncol(data)) {
      data = as.matrix(data)
    } else {
      stop('please recheck your data structure , you must keep a equal num of the row and col')
    }    
  }
  sum_vector_row    =  data %>% apply(2,sum)
  decide_matrix     =  data %>% apply(1,function(x) x/sum_vector_row) 
  weigth_vector     =  decide_matrix %>% apply(2,sum)
  result = list(decide_matrix = decide_matrix, weigth_vector  = weigth_vector/sum(weigth_vector ))
  return(result)
}

Weigth_fun(data_C)
#特征向量λ
AW_Weight <- function(data){
  if(class(data) == 'matrix'){
    data = data     
  } else {
    if ( class(data) == 'data.frame' & nrow(data) == ncol(data) - 1 & is.character(data[,1,drop = TRUE])){
      data = as.matrix(data[,-1])
    } else if (class(data) == 'data.frame' & nrow(data) == ncol(data)) {
      data = as.matrix(data)
    } else {
      stop('please recheck your data structure , you must keep a equal num of the row and col')
    }    
  }
  AW_Vector = data %*% Weigth_fun(data)$weigth_vector
  λ = (AW_Vector/Weigth_fun(data)$weigth_vector) %>%  sum(.) %>% `/`(length(AW_Vector))
  result = list(
    AW_Vector = AW_Vector,
    `∑AW/W`   = AW_Vector/Weigth_fun(data)$weigth_vector,
    λ         =  λ
  )  
  return(result)
}
AW_Weight(data_C)

#一致性
Consist_Test <- function(λ,n){
  RI_refer =  c(0,0,0.52,0.89,1.12,1.26,1.36,1.41,1.46,1.49,1.52,1.54)
  CI = (λ - n)/(n - 1)
  CR = CI/(RI_refer[n])
  if (CR <= .1){
    cat(" 通过一致性检验！",sep = "\n")
    cat(" Wi: ", round(CR,4), "\n")
  } else {
    cat(" 请调整判断矩阵！","\n")
    }
  return(CR)
}

Consist_Test(AW_Weight(data_C)$λ,4)
#排序
rule_Weigth_C <- Weigth_fun(data_C)$weigth_vector   #准则层特征向量
rule_Weigth_f1 <- Weigth_fun(data_F1)$weigth_vector   #方案层(for f1)特征向量
rule_Weigth_f2 <- Weigth_fun(data_F2)$weigth_vector   #方案层(for f2)特征向量
rule_Weigth_f3 <- Weigth_fun(data_F3)$weigth_vector   #方案层(for f3)特征向量
rule_Weigth_f4 <- Weigth_fun(data_F4)$weigth_vector   #方案层(for f4)特征向量
#层次总排序：
all_matrix <- matrix(c(rule_Weigth_f1,rule_Weigth_f2,rule_Weigth_f3,rule_Weigth_f4),nrow = 31)

scores <- all_matrix %*% rule_Weigth_C
dimnames(scores) <- list(t$province,"score")
view(scores)
#4.2.2
sim_value<-function(prov){
vct<-matrix(data=c(t_c1,t_c2,t_c3,t_c4),ncol = 4,dimnames = list(t$province,c("pop","sexrt","yonung","edu")))
A=as.vector(vct[prov,])
prov_n<-which(row.names(vct)==prov)
options(digits=17)
cosθ<-data.frame(t$province)

for (i in c(1:31)){
  if (i!=prov_n){
  B=as.vector(vct[i,])
  
  cosθ[i,2] <- as.double(sum(A*B)/(sqrt(sum(A^2))*sqrt(sum(B^2))))
  }
  else{next}
}
as.double(cosθ[,2])
colnames(cosθ)[2]<-'Distance'
cosθ<- cosθ[order(cosθ[,2],decreasing = T),]
return(cosθ)
}
options(digits=17)
H<-sim_value('河南')
view(H)
