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
shortsp<-unique(shortseq_long$ID)
shortseq_long<-shortseq_long[shortseq_long$group == "moth",]
shortseq_long<-shortseq_long%>%select(sample,ID, occurence)


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
longsp<-unique(longseq_long$ID)
log<-read.csv('NJ_feacal_log.csv')
log$sample<-log$Lab.ID

foo<-left_join(longseq_long,log, by='sample')
foo<-foo[foo$group == "moth",]
foo<-foo%>%select(sample,ID, occurence, Site,Year)%>%spread(key=ID, value=occurence)
foo[is.na(foo)]<-0 
write.csv(foo, "20190317_ENR_longseq_feacalsampledat.csv")
names(foo)
rm(foo)

# moth data wrangle ####
moths<-read.csv("C:/Users/C1751157/Documents/PhD/moths/moths/moths_110119.csv")
summary(moths$Binomial.Sp.)
mothssp<-unique(moths$Binomial.Sp.)

foo<-moths%>%select(Binomial.Sp.,Trap.no, Abundance, Site)%>%spread(key=Binomial.Sp., value=Abundance)
foo$V1=NULL
foo[is.na(foo)]<-0 
write.csv(foo, "20190322_ENR_mothtrap_dat.csv")

mothsp_all<-c(as.character(mothssp),as.character(shortsp),as.character(longsp))
unique(mothsp_all)


#find out what species names are not in relevant dataframes
shortseq_additional<-mothsp_all[!(mothsp_all %in% shortseq_long$ID)]
shortseq_additional<-as.character(shortseq_additional)
ID<-shortseq_additional
sample<-rep("NA",213)
occurence<-rep(0,213)
shortseq_additional<-data.frame(cbind(sample,ID, occurence))

# add missing species to short sequence data
shortseq_long<-rbind(shortseq_long,shortseq_additional)

log<-read.csv('NJ_feacal_log.csv')
log$sample<-log$Lab.ID

foo<-left_join(shortseq_long,log, by='sample')
foo<-distinct(foo) # remove duplicates
foo<-foo%>%select(sample,ID, occurence, Site,Year)%>%spread(key=ID, value=occurence)
foo[is.na(foo)]<-0 
write.csv(foo, "20190323_ENR_shortseq_feacalsampledat.csv")

#find out what species names are not in relevant dataframes
longseq_additional<-mothsp_all[!(mothsp_all %in% longseq_long$ID)]
longseq_additional<-as.character(longseq_additional)
ID<-longseq_additional
sample<-rep("NA",213)
occurence<-rep(0,213)
longseq_additional<-data.frame(cbind(sample,ID, occurence))

# add missing species to short sequence data
longseq_long<-longseq_long%>%select(sample,ID,occurence)
longseq_long<-rbind(longseq_long,longseq_additional)

log<-read.csv('NJ_feacal_log.csv')
log$sample<-log$Lab.ID

foo<-left_join(longseq_long,log, by='sample')
foo<-distinct(foo) # remove duplicates
foo<-foo%>%select(sample,ID, occurence, Site,Year)%>%spread(key=ID, value=occurence)
foo[is.na(foo)]<-0 
write.csv(foo, "20190323_ENR_longseq_feacalsampledat.csv")

# long seq
foo<-foo[2:41,] # remove NA column

rm(Binomial.Sp.)
#moths
moths_additional<-mothsp_all[!(mothsp_all %in% moths$Binomial.Sp.)]
moths_additional<-as.character(moths_additional)
Binomial.Sp.<-moths_additional
Trap.no<-rep("NA",135)
Abundance<-rep(0,135)
moths_additional<-data.frame(cbind(Trap.no,Binomial.Sp.,Abundance))
moths<-moths%>%select(Trap.no,Binomial.Sp.,Abundance)
# add missing species to short sequence data
moths<-rbind(moths,moths_additional)
moths<-unique(moths)

foo2<-moths%>%select(Trap.no,Binomial.Sp.,Abundance)%>%spread(key=Binomial.Sp., value=Abundance)
foo2$V1=NULL
foo2[is.na(foo2)]<-0 
write.csv(foo2, "20190325_ENR_mothtrap_sampledat.csv")

## lets plot and try some data anlysis ####
# short sequence load
foo1<-read.csv("20190323_ENR_shortseq_feacalsampledat.csv",check.names=FALSE)
foo1<-foo1[2:66,]
foo1$X=NULL
foo$V1=NULL
short_seqsum<-data.frame(colSums(foo1[,4:348]))
short_seqsum$sp=as.character(rownames(short_seqsum))
short_seqsum<-short_seqsum[order(rownames(short_seqsum)),]
short_seqsum$sprop<-short_seqsum$colSums.foo1...4.348../65
# long sequence load
foo<-read.csv("20190323_ENR_longseq_feacalsampledat.csv",check.names=FALSE)

long_seqsum<-data.frame(colSums(foo[,4:348]))
long_seqsum$sp=as.character(rownames(long_seqsum))
long_seqsum<-long_seqsum[order(rownames(long_seqsum)),]
long_seqsum$lprop<-long_seqsum$colSums.foo...4.348../40

# moth data load####
foo2<-read.csv("20190325_ENR_mothtrap_sampledat.csv",check.names=FALSE)
foo2$X=NULL
foo2<-foo2[,2:346]
#foo2[foo2>1]<-1
moth_trapsum<-data.frame(colSums(foo2[,2:346]))
moth_trapsum$sp=as.character(rownames(moth_trapsum))
moth_trapsum<-moth_trapsum[order(rownames(moth_trapsum)),]
moth_trapsum$mprop<-moth_trapsum$colSums.foo2...2.346../154

plotdat<-cbind(moth_trapsum,long_seqsum,short_seqsum)
plotdat$prop=NULL
plotdat$colSums.foo2...2.346..=NULL
plotdat$colSums.foo...4.348..=NULL
plotdat$colSums.foo1...4.348..=NULL
plotdat$sp=NULL

(ggplot(data=plotdat)+
  geom_col(aes(x=sp, y=mprop, fill=I("blue")))+
    theme(axis.text.x = element_blank())+
  xlab("moth species")+
  ylab("Proportion in Trap/ Sample")+
  theme_minimal())


(ggplot(data=plotdat)+
    geom_col(aes(x=sp, y=lprop, fill=I("yellow")))+
    geom_col(aes(x=sp, y=sprop,fill=I("green")))+
    theme(axis.text.x = element_blank())+
    scale_y_reverse()+
    xlab("moth species")+
    ylab("Proportion in Trap/ Sample")+
    theme_minimal())

?scale_y_reverse
rm(plotdat)
