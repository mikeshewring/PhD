#03 ecoll nut R analysis

# MIke s 03 2019

#install.packages("econullnetr")
library(econullnetr)
library(dplyr)
library(tidyr)
library(ggplot2)

?econullnetr::generate_null_net
#resource data manipulate ####

resources<-foo2[ , order(names(foo2[,1:345]))]
colnames(resources)
foo2<-resources[,-325]
colnames(foo2)
resources=foo2
resources["tots",]<-colSums(resources)
resources<-rbind(colnames(resources),resources[153,])
resources<-resources[2,]
colnames(resources)
#consumers using long sequence data manipulate ####
foo$Site=NULL
foo$Year=NULL
names(foo)
NJ<-foo$sample
consumers<-foo[ , order(names(foo))]
#consumers<-foo
colnames(consumers)
rm(consumers)
consumers$`Arctia caja.1`=NULL
consumers$sample=NULL
consumers<-cbind(NJ,consumers)
colnames(f)
names(resources)
names(consumers)
write.csv(resources,"20190325_ENR_resources.csv")
write.csv(consumers,"20190325_ENR_consumers.csv")



# model run ####
con<-read.csv("20190325_ENR_consumers.csv", check.names = F)
res<-read.csv("20190325_ENR_resources.csv", check.names = F)
names(res)
names(con)
con$NJ
names(con) %in% names(res)
# run a null model ####
null.1 <- generate_null_net(consumers = con, resources = res, sims = 100,data.type = "counts", summary.type = "sum")


moth.links <- test_interactions(null.1, signif.level = 0.95)

?plot_preferences
plot_preferences(null.1,"NJ", signif.level = 0.95,  type = "counts", xlab = "Number of Occurences", cex.lab = 1.5,p.cex = 2, l.cex = 1, lwd = 2, font = 3, Yaxt="n")
?dotchart

## ggplot attempt #####
w<-data.frame(null.1$rand.data)
w<-w[,3:336]
library(dplyr)
w1<-gather(w, key = "species", value = "Sp.Occ")
w<-w[w$sp %in% foo,]

rm(q)
q<-data.frame(null.1$obs.interactions)
q<-q[,2:335]
q1<-gather(q, key = "species", value = "Sp.Occ")

test<-cbind(q,w)
test<-test[! test$t.w.== 0,]
test<-test[! test$t.q.== 0,]
library(ggplot2)
ggplot()+
  theme_classic()+
  geom_line(data = w1[20001:25000,], aes(y=species,x=Sp.Occ, colour = I("blue")))+
  geom_point(data=q1[200:250,], aes(y=species,x=Sp.Occ, size = I(5), colour=I("red"), alpha=I(0.3)))+
  xlab("Species Occurence")+
  ylab("Species")


foo<-c("Diachrysia chrysitis",  "Dysstroma citrata",  "Epirrhoe alternata", "Falcaria lacertinaria", "Hydriomena furcata")
 
  