rm(list = ls())
library(tidyverse)
Sys.setlocale("LC_ALL", 'Chinese')
setwd("C:/Users/aklasim/Desktop/R6,8")

#2.1
sexrat<- read.table('历年性别比.csv', sep = ",",header = T,encoding='UTF-8')
basinfo<-read.csv('各省人口基本信息.csv',sep = ",",skip = 1,header = T,encoding='UTF-8')
names(basinfo)[1]="province"
names(sexrat)[1]="年份"

#2.2
sexrat$男性人口数量<-(sexrat[,3]/(100+sexrat[,3]))*sexrat[,2]
sexrat$女性人口数量<-(100/(100+sexrat[,3]))*sexrat[,2]
#2.3
loc_c<-read_csv('地区分类.csv')
newloc<-gather(loc_c,na.rm = T,key = 'loc',value = "province")
for (i in c('省','市','自治区','维吾尔','回族','壮族')){
newloc$province<-str_replace(newloc$province,i,'')}
basinfo$province=str_remove_all(basinfo$province,' ')
total_info<-merge(basinfo,newloc,by="province",all = T)

