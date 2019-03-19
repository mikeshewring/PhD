#01_feacal data explore
library(dplyr)
library(tidyr)
spsum<-read.csv('20190221_sp_summary.csv')
spsum
plot(spsum)

plot(spsum$Count~spsum$Group)

shortseq<-read.csv('20190225_fecal_shortseq_wide.csv')
summary(shortseq)
shortseq<-shortseq[1:134,]
shortseq_long<-gather(shortseq,key=sample, value = occurence, 12:78)
shortseq_long$X.1=NULL
shortseq_long$X.2=NULL
shortseq_long$X.3=NULL
shortseq_long$group=shortseq_long$X
shortseq_long$X=NULL

foo<-filter(shortseq_long, !is.na(shortseq_long$occurence))

log<-read.csv('NJ_feacal_log.csv')
log$sample<-log$Lab.ID

foo<-left_join(foo,log, by='sample')


library(ggplot2)
ggplot(data=foo)+
  geom_bar(aes(x=group, fill = group))+
  theme_minimal()+
  facet_grid(~Site)

ggplot(data=foo)+
  geom_bar(aes(x=Site, fill = Site))+
  theme_minimal()

ggplot(data=log)+
  geom_bar(aes(x=Site, fill = Site))+
  theme_minimal()

## data explore by species ###
shortseq_long$occurence[is.na(shortseq_long$occurence)]<-0
f<-shortseq_long%>%group_by(ID, group)%>%summarise(occurence2= sum(occurence))
f1<-f[f$occurence2 >= 5,]
fmoths<-f[f$group == "moth",]

ggplot(data=f)+
  geom_col(aes(x=group, y=occurence2, fill=group))+
  scale_fill_discrete(h = c(1, 360))+
  ylim(-10,160)+
  coord_polar(start = 0)+
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank())
?scale_fill_discrete
# reorder daat frame to occufence value
fmoths$ID <- factor(fmoths$ID, levels = fmoths$ID[order(fmoths$occurence2)])

#  create label data for circular bar plot
label_data=fmoths
angle
# calculate the ANGLE of the labels
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$occurence2-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
label_data$angle<-ifelse(angle < -90, angle+90, angle)

ggplot(data=fmoths)+
  geom_col(aes(x=ID, y=occurence2,fill=alpha("blue", 0.3)))+
  coord_flip()+
  theme_minimal()
?geom_col
?theme
