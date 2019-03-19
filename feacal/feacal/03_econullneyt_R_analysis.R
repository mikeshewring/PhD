# 03 ecoll nut R analysis

# MIke s 03 2019

#install.packages("econullnetr")
library(econullnetr)
library(dplyr)
library(tidyr)
library(ggplot2)

# data prep - short seq
shortseq<-read.csv('20190225_fecal_shortseq_wide.csv')
summary(shortseq)
shortseq<-shortseq[1:134,]
shortseq_long<-gather(shortseq,key=sample, value = occurence, 12:78)
shortseq_long$X.1=NULL
shortseq_long$X.2=NULL
shortseq_long$X.3=NULL
shortseq_long$group=shortseq_long$X
shortseq_long$X=NULL
shortseq_long$occurence[is.na(shortseq_long$occurence)]<-0 # convert NA's to zeros

log<-read.csv('NJ_feacal_log.csv')
log$sample<-log$Lab.ID

foo<-left_join(shortseq_long,log, by='sample')
foo<-foo[foo$group == "moth",]
foo<-foo%>%select(sample,ID, occurence, Site,Year)%>%spread(key=ID, value=occurence)
foo[is.na(foo)]<-0 
write.csv(foo, "20190317_ENR_shortseq_feacalsampledat.csv")

# data prep - long_seq
longseq<-read.csv('Mike_Long_Sequences.csv')
summary(longseq)
longseq_long<-gather(longseq,key=sample, value = occurence, 13:52)
longseq_long$X.1=NULL
longseq_long$group=longseq_long$X
longseq_long$X=NULL
longseq_long$occurence[is.na(longseq_long$occurence)]<-0 # convert NA's to zeros

log<-read.csv('NJ_feacal_log.csv')
log$sample<-log$Lab.ID

foo<-left_join(longseq_long,log, by='sample')
foo<-foo[foo$group == "moth",]
foo<-foo%>%select(sample,ID, occurence, Site,Year)%>%spread(key=ID, value=occurence)
foo[is.na(foo)]<-0 
write.csv(foo, "20190317_ENR_longseq_feacalsampledat.csv")
names(foo)
