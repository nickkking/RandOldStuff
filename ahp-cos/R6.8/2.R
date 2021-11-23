library(ggplot2)
library(tidyverse)
sex<-rbind(sexrat,sexrat)
sex$男性人口数量[8:14]<-sex$女性人口数量[1:7]
sex$女性人口数量<-rep(c('男','女'),each=7)
names(sex)[4:5]<-c('pnum','c')

#bar
bar<-ggplot(data=sex,aes(x=年份,y=pnum,fill=c))+geom_bar(stat = 'identity',position='dodge')+ylab("人口数量 （万人）") + guides(fill=guide_legend(title=NULL))+theme(panel.grid =element_blank())+ scale_x_continuous(breaks=sex[,1])
bar
#line
line<-ggplot(data=sex,aes(x=年份,y=性别比,fill=c))+geom_line()+ geom_point(colour = "pink", size = 2, shape = 21, fill = "brown")+ scale_x_continuous(breaks=sex[,1])+geom_text(aes(label=性别比,vjust = 1.1, hjust = -0.5),size=3,color='red')+ylab('总人口性别比')
line
#piechart1
total_info$loc[is.na(total_info$loc)] <- '现役军人'

label_value <- paste('(', round(total_info$地区总人口/sum(total_info$地区总人口) * 100, 1), '%)', sep = '')

p1<-ggplot(total_info, aes(x="", y=地区总人口, fill=loc)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+labs(x = '', y = '', title = '')+ guides(fill=guide_legend(title=NULL))+theme(axis.text =element_blank(),axis.line = element_blank())
p1
#piecahrt2
type<-c('0-14岁','15-59岁','60岁以上')
names(total_info)[6:8]<-type

x<-(total_info[,6:8])
u14<-sum(na.omit(x[,1]))
u59<-sum(na.omit(x[,2]))
o60<-sum(na.omit(x[,3]))
all<-sum(na.omit(x))
num<-c(u14,u59,o60)
df<-data.frame(num,type)
p2<-ggplot(data=df,aes(x='', y=num, fill=type)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+labs(x = '', y = '', title = '')+ guides(fill=guide_legend(title=NULL))+theme(axis.text =element_blank())
p2
