##02_feacal_NMDS
## 27 02 2019

library(vegan)
library(dplyr)
library(tidyr)

#data manipulate
dat<-left_join(shortseq_long,log, by='sample') # add in sample ref and site
number_samples_site<-dat%>%group_by(Site, sample)%>%summarise()
number_samples_site<-filter(number_samples_site, !is.na(Site))
number_samples_site<-tally(number_samples_site)
number_samples_site<-number_samples_site[2:7,]

dat<-left_join(dat, number_samples_site, by="Site")
summary(is.na(dat$Site)) # remove samples with unknown site
dat<-filter(dat,!is.na(dat$Site)) # remove NA sites
dat<-dat%>%filter(!Site == "")
dat<-dat%>%filter(!Site == "Borth")
dat[is.na(dat)] <- 0 # nmds does not like NA's so change these to 0's
dat<-dat%>%group_by(Site, ID, n)%>%summarise(occurence2= sum(occurence)) # group data by Site and species ID and provide a sumary of the number of occurences in samples....... Do we need to adjust this for the number of feacal samples per site?
dat$occ_samp<-(dat$occurence2/dat$n)
dat$n=NULL
dat$occurence2=NULL
summary(as.factor(dat$Site))
dat$Site<-as.character(dat$Site)
dat$Site<-as.factor(dat$Site)
summary(as.factor(dat$Site))

t<-spread(dat, key = ID, value = occ_samp)
row.names(t)<-t$Site
t$Site=NULL

# the above is not working as i seem to only have sing,e values for each site i.e. no species was found in more than one site. This is not correct

df=t


# cluster analysis
ord <- vegdist(df, method = "euclidean")
cluster = hclust(ord)
plot(cluster)

# nmds
NMDS=metaMDS(df,distance = "bray", k=2,trymax=1000)
?metaMDS
NMDS #Stress:     
 
stressplot(NMDS)
plot(NMDS)
orditorp(NMDS,type="points", display="sites",cex=1.25,air=0.01)

## ggplot
library(ggplot2)
data.scores <- as.data.frame(scores(NMDS)) #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
head(data.scores)  #look at the data

species.scores <- as.data.frame(scores(NMDS, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data

## ggplot for report ####
library(ggrepel)
(nmds1<-ggplot() +
   geom_point(data=species.scores,aes(x=NMDS1,y=NMDS2),size=1) + # add the species point markers 
   geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,colour=site),size=4) + # add the point markers
   geom_text_repel(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=5,vjust=0,hjust=0) +  # add the site labels
   coord_equal() +
   theme_bw() + 
   theme(axis.text.x = element_blank(),  # remove x-axis text
         axis.text.y = element_blank(), # remove y-axis text
         axis.ticks = element_blank(),  # remove axis ticks
         axis.title.x = element_text(size=18), # remove x-axis labels
         axis.title.y = element_text(size=18), # remove y-axis labels
         panel.background = element_blank(), 
         panel.grid.major = element_blank(),  #remove major-grid labels
         panel.grid.minor = element_blank(),  #remove minor-grid labels
         plot.background = element_blank()))

