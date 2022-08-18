#######Fish (updated July 2022, incorporates info on presence/absence of stations)########
###Use combine tables script to gett "All" table



path_to_images <- "https://hailaschultz.github.io/genomics/Fish/"

library(visNetwork)
library(RCurl)
library(dplyr)
library(reshape2)

###prep allmarker table
StationList2<-StationList1


#######Get Markers
All$Order<- recode(All$Order, Teleostei__o = NA)
all1<-subset(All,Class=="Actinopteri")
All$Order<- recode(All$Order, 'N' = NA)

###Class

all4<- all1 %>%
  group_by(Phylum,Class,Marker,variable) %>%
  summarise(
    value = sum(value))

all5<-dcast(all4, Phylum+Class+variable~Marker, value.var = "value")
all5$COI<-replace(all5$COI, all5$COI>0, "Y")
all5$COI<-replace(all5$COI, all5$COI==0, "N")
all5$MiFish<-replace(all5$MiFish, all5$MiFish>0, "Y")
all5$MiFish<-replace(all5$MiFish, all5$MiFish==0, "N")
all5$GenFish<-replace(all5$GenFish, all5$GenFish>0, "Y")
all5$GenFish<-replace(all5$GenFish, all5$GenFish==0, "N")
all5$Mol16S<-replace(all5$Mol16S, all5$Mol16S>0, "Y")
all5$Mol16S<-replace(all5$Mol16S, all5$Mol16S==0, "N")
all5$Morphology<-replace(all5$Morphology, all5$Morphology>0, "Y")
all5$Morphology<-replace(all5$Morphology, all5$Morphology==0, "N")
all5[is.na(all5)] <- "N"
#delete nnn rows
all5$MarkN <- paste(all5$COI,all5$Mol16S,all5$Morphology,all5$GenFish,all5$MiFish)
all5$MarkN <- gsub(" ", "", all5$MarkN)
all5 <- subset( all5, select = -MarkN )
##combine 2 tables
all6<-full_join(x = all5, y = StationList2, by = "variable")

#combine columns
all6$COI2 <- paste(all6$COI1,all6$COI)
all6$COI2 <- gsub(" ", "", all6$COI2)
all6$Mol16S2 <- paste(all6$Mol16S1,all6$Mol16S)
all6$Mol16S2 <- gsub(" ", "", all6$Mol16S2)
all6$Morphology2 <- paste(all6$Morphology1,all6$Morphology)
all6$Morphology2 <- gsub(" ", "", all6$Morphology2)
all6$GenFish2 <- paste(all6$GenFish1,all6$GenFish)
all6$GenFish2 <- gsub(" ", "", all6$GenFish2)
all6$MiFish2 <- paste(all6$MiFish1,all6$MiFish)
all6$MiFish2 <- gsub(" ", "", all6$MiFish2)
#replace NN=1,YY=2,YN=0
all6<-replace(all6, all6=="NN", 0)
all6<-replace(all6, all6=="YY", 2)
all6<-replace(all6, all6=="YN", 1)

#get max number in each column
all7<- all6 %>%
  group_by(Phylum,Class) %>%
  summarise(
    COI = max(COI2),
    MiFish=max(MiFish2),
    Mol16S= max(Mol16S2),
    Morphology=max(Morphology2),
    GenFish=max(GenFish2))
##combine for code
all7$code <- paste(all7$Morphology,all7$COI,all7$Mol16S,all7$GenFish,all7$MiFish)
all7$code <- gsub(" ", "", all7$code)
#clean up table
all7$lvl1<-all7$Phylum
all7$lvl2<-all7$Class
all7$Code<-all7$code
all8<- all7[ -c(1:7) ]
all8$level<-1
all8$label <-all8$lvl2
all8.1<-all8
all8.1$label <-paste("<b>",all8.1$lvl2,"</b>")


###Order

all4<- all1 %>%
  group_by(Class,Order,Marker,variable) %>%
  summarise(
    value = sum(value))

all5<-dcast(all4, Class+Order+variable~Marker, value.var = "value")
all5$COI<-replace(all5$COI, all5$COI>0, "Y")
all5$COI<-replace(all5$COI, all5$COI==0, "N")
all5$MiFish<-replace(all5$MiFish, all5$MiFish>0, "Y")
all5$MiFish<-replace(all5$MiFish, all5$MiFish==0, "N")
all5$GenFish<-replace(all5$GenFish, all5$GenFish>0, "Y")
all5$GenFish<-replace(all5$GenFish, all5$GenFish==0, "N")
all5$Mol16S<-replace(all5$Mol16S, all5$Mol16S>0, "Y")
all5$Mol16S<-replace(all5$Mol16S, all5$Mol16S==0, "N")
all5$Morphology<-replace(all5$Morphology, all5$Morphology>0, "Y")
all5$Morphology<-replace(all5$Morphology, all5$Morphology==0, "N")
all5[is.na(all5)] <- "N"
#delete nnn rows
all5$MarkN <- paste(all5$COI,all5$MiFish,all5$Mol16S,all5$Morphology,all5$GenFish)
all5$MarkN <- gsub(" ", "", all5$MarkN)
all5 <- subset( all5, select = -MarkN )
##combine 2 tables
all6<-full_join(x = all5, y = StationList2, by = "variable")

#combine columns
all6$COI2 <- paste(all6$COI1,all6$COI)
all6$COI2 <- gsub(" ", "", all6$COI2)
all6$MiFish2 <- paste(all6$MiFish1,all6$MiFish)
all6$MiFish2 <- gsub(" ", "", all6$MiFish2)
all6$Mol16S2 <- paste(all6$Mol16S1,all6$Mol16S)
all6$Mol16S2 <- gsub(" ", "", all6$Mol16S2)
all6$Morphology2 <- paste(all6$Morphology1,all6$Morphology)
all6$Morphology2 <- gsub(" ", "", all6$Morphology2)
all6$GenFish2 <- paste(all6$GenFish1,all6$GenFish)
all6$GenFish2 <- gsub(" ", "", all6$GenFish2)
#replace NN=1,YY=2,YN=0
all6<-replace(all6, all6=="NN", 0)
all6<-replace(all6, all6=="YY", 2)
all6<-replace(all6, all6=="YN", 1)

#get max number in each column
all7<- all6 %>%
  group_by(Class,Order) %>%
  summarise(
    COI = max(COI2),
    MiFish=max(MiFish2),
    Mol16S= max(Mol16S2),
    Morphology=max(Morphology2),
    GenFish=max(GenFish2))
##combine for code
all7$code <- paste(all7$Morphology,all7$COI,all7$Mol16S,all7$GenFish,all7$MiFish)
all7$code <- gsub(" ", "", all7$code)
#clean up table
all7$lvl1<-all7$Class
all7$lvl2<-all7$Order
all7$Code<-all7$code
all8<- all7[ -c(1:7) ]
all8$level<-2
all8$label <-all8$lvl2
all8.2<-all8
all8.2$label <-paste("<b>",all8.2$lvl2,"</b>")


#####Family

all4<- all1 %>%
  group_by(Order,Family,Marker,variable) %>%
  summarise(
    value = sum(value))

all5<-dcast(all4, Order+Family+variable~Marker, value.var = "value")
all5$COI<-replace(all5$COI, all5$COI>0, "Y")
all5$COI<-replace(all5$COI, all5$COI==0, "N")
all5$MiFish<-replace(all5$MiFish, all5$MiFish>0, "Y")
all5$MiFish<-replace(all5$MiFish, all5$MiFish==0, "N")
all5$GenFish<-replace(all5$GenFish, all5$GenFish>0, "Y")
all5$GenFish<-replace(all5$GenFish, all5$GenFish==0, "N")
all5$Mol16S<-replace(all5$Mol16S, all5$Mol16S>0, "Y")
all5$Mol16S<-replace(all5$Mol16S, all5$Mol16S==0, "N")
all5$Morphology<-replace(all5$Morphology, all5$Morphology>0, "Y")
all5$Morphology<-replace(all5$Morphology, all5$Morphology==0, "N")
all5[is.na(all5)] <- "N"
#delete nnn rows
all5$MarkN <- paste(all5$COI,all5$MiFish,all5$Mol16S,all5$Morphology,all5$GenFish)
all5$MarkN <- gsub(" ", "", all5$MarkN)
all5 <- subset( all5, select = -MarkN )
##combine 2 tables
all6<-full_join(x = all5, y = StationList2, by = "variable")

#combine columns
all6$COI2 <- paste(all6$COI1,all6$COI)
all6$COI2 <- gsub(" ", "", all6$COI2)
all6$MiFish2 <- paste(all6$MiFish1,all6$MiFish)
all6$MiFish2 <- gsub(" ", "", all6$MiFish2)
all6$Mol16S2 <- paste(all6$Mol16S1,all6$Mol16S)
all6$Mol16S2 <- gsub(" ", "", all6$Mol16S2)
all6$Morphology2 <- paste(all6$Morphology1,all6$Morphology)
all6$Morphology2 <- gsub(" ", "", all6$Morphology2)
all6$GenFish2 <- paste(all6$GenFish1,all6$GenFish)
all6$GenFish2 <- gsub(" ", "", all6$GenFish2)
#replace NN=1,YY=2,YN=0
all6<-replace(all6, all6=="NN", 0)
all6<-replace(all6, all6=="YY", 2)
all6<-replace(all6, all6=="YN", 1)

#get max number in each column
all7<- all6 %>%
  group_by(Order,Family) %>%
  summarise(
    COI = max(COI2),
    MiFish=max(MiFish2),
    Mol16S= max(Mol16S2),
    Morphology=max(Morphology2),
    GenFish=max(GenFish2))
##combine for code
all7$code <- paste(all7$Morphology,all7$COI,all7$Mol16S,all7$GenFish,all7$MiFish)
all7$code <- gsub(" ", "", all7$code)
#clean up table
all7$lvl1<-all7$Order
all7$lvl2<-all7$Family
all7$Code<-all7$code
all8<- all7[ -c(1:7) ]
all8$level<-3
all8$label <-all8$lvl2
all8.3<-all8
all8.3$label <-paste("<b>",all8.3$lvl2,"</b>")


#####Genus

all4<- all1 %>%
  group_by(Family,Genus,Marker,variable) %>%
  summarise(
    value = sum(value))

all5<-dcast(all4, Family+Genus+variable~Marker, value.var = "value")
all5$COI<-replace(all5$COI, all5$COI>0, "Y")
all5$COI<-replace(all5$COI, all5$COI==0, "N")
all5$MiFish<-replace(all5$MiFish, all5$MiFish>0, "Y")
all5$MiFish<-replace(all5$MiFish, all5$MiFish==0, "N")
all5$GenFish<-replace(all5$GenFish, all5$GenFish>0, "Y")
all5$GenFish<-replace(all5$GenFish, all5$GenFish==0, "N")
all5$Mol16S<-replace(all5$Mol16S, all5$Mol16S>0, "Y")
all5$Mol16S<-replace(all5$Mol16S, all5$Mol16S==0, "N")
all5$Morphology<-replace(all5$Morphology, all5$Morphology>0, "Y")
all5$Morphology<-replace(all5$Morphology, all5$Morphology==0, "N")
all5[is.na(all5)] <- "N"
#delete nnn rows
all5$MarkN <- paste(all5$COI,all5$MiFish,all5$Mol16S,all5$Morphology,all5$GenFish)
all5$MarkN <- gsub(" ", "", all5$MarkN)
all5 <- subset( all5, select = -MarkN )
##combine 2 tables
all6<-full_join(x = all5, y = StationList2, by = "variable")

#combine columns
all6$COI2 <- paste(all6$COI1,all6$COI)
all6$COI2 <- gsub(" ", "", all6$COI2)
all6$MiFish2 <- paste(all6$MiFish1,all6$MiFish)
all6$MiFish2 <- gsub(" ", "", all6$MiFish2)
all6$Mol16S2 <- paste(all6$Mol16S1,all6$Mol16S)
all6$Mol16S2 <- gsub(" ", "", all6$Mol16S2)
all6$Morphology2 <- paste(all6$Morphology1,all6$Morphology)
all6$Morphology2 <- gsub(" ", "", all6$Morphology2)
all6$GenFish2 <- paste(all6$GenFish1,all6$GenFish)
all6$GenFish2 <- gsub(" ", "", all6$GenFish2)
#replace NN=1,YY=2,YN=0
all6<-replace(all6, all6=="NN", 0)
all6<-replace(all6, all6=="YY", 2)
all6<-replace(all6, all6=="YN", 1)

#get max number in each column
all7<- all6 %>%
  group_by(Family,Genus) %>%
  summarise(
    COI = max(COI2),
    MiFish=max(MiFish2),
    Mol16S= max(Mol16S2),
    Morphology=max(Morphology2),
    GenFish=max(GenFish2))
##combine for code
all7$code <- paste(all7$Morphology,all7$COI,all7$Mol16S,all7$GenFish,all7$MiFish)
all7$code <- gsub(" ", "", all7$code)
#clean up table
all7$lvl1<-all7$Family
all7$lvl2<-all7$Genus
all7$Code<-all7$code
all8<- all7[ -c(1:7) ]
all8$level<-4
all8$label <-all8$lvl2
all8.4<-all8
all8.4$label <-paste("<i>",all8.4$lvl2,"</i>")
all8.4$label <-paste("<b><i>",all8.4$lvl2,"</i></b>")

#####Species
all4<- all1 %>%
  group_by(Genus,Species,Marker,variable) %>%
  summarise(
    value = sum(value))

all5<-dcast(all4, Genus+Species+variable~Marker, value.var = "value")
all5$COI<-replace(all5$COI, all5$COI>0, "Y")
all5$COI<-replace(all5$COI, all5$COI==0, "N")
all5$MiFish<-replace(all5$MiFish, all5$MiFish>0, "Y")
all5$MiFish<-replace(all5$MiFish, all5$MiFish==0, "N")
all5$GenFish<-replace(all5$GenFish, all5$GenFish>0, "Y")
all5$GenFish<-replace(all5$GenFish, all5$GenFish==0, "N")
all5$Mol16S<-replace(all5$Mol16S, all5$Mol16S>0, "Y")
all5$Mol16S<-replace(all5$Mol16S, all5$Mol16S==0, "N")
all5$Morphology<-replace(all5$Morphology, all5$Morphology>0, "Y")
all5$Morphology<-replace(all5$Morphology, all5$Morphology==0, "N")
all5[is.na(all5)] <- "N"
#delete nnn rows
all5$MarkN <- paste(all5$COI,all5$Mol16S,all5$Morphology,all5$GenFish,all5$MiFish)
all5$MarkN <- gsub(" ", "", all5$MarkN)
all5 <- subset( all5, select = -MarkN )
##combine 2 tables
all6<-full_join(x = all5, y = StationList2, by = "variable")

#combine columns
all6$COI2 <- paste(all6$COI1,all6$COI)
all6$COI2 <- gsub(" ", "", all6$COI2)
all6$MiFish2 <- paste(all6$MiFish1,all6$MiFish)
all6$MiFish2 <- gsub(" ", "", all6$MiFish2)
all6$Mol16S2 <- paste(all6$Mol16S1,all6$Mol16S)
all6$Mol16S2 <- gsub(" ", "", all6$Mol16S2)
all6$Morphology2 <- paste(all6$Morphology1,all6$Morphology)
all6$Morphology2 <- gsub(" ", "", all6$Morphology2)
all6$GenFish2 <- paste(all6$GenFish1,all6$GenFish)
all6$GenFish2 <- gsub(" ", "", all6$GenFish2)
#replace NN=1,YY=2,YN=0
all6<-replace(all6, all6=="NN", 0)
all6<-replace(all6, all6=="YY", 2)
all6<-replace(all6, all6=="YN", 1)

#get max number in each column
all7<- all6 %>%
  group_by(Genus,Species) %>%
  summarise(
    COI = max(COI2),
    MiFish=max(MiFish2),
    Mol16S= max(Mol16S2),
    Morphology=max(Morphology2),
    GenFish=max(GenFish2))
##combine for code
all7$code <- paste(all7$Morphology,all7$COI,all7$Mol16S,all7$GenFish,all7$MiFish)
all7$code <- gsub(" ", "", all7$code)
#clean up table
all7$lvl1<-all7$Genus
all7$lvl2<-all7$Species
all7$Code<-all7$code
all8<- all7[ -c(1:7) ]
all8$level<-5
all8$label <-all8$lvl2
all8.5<-all8
all8.5$label <-paste("<i>",all8.5$lvl2,"</i>")
all8.5$label <-paste("<b><i>",all8.5$lvl2,"</i></b>")

#combine tables
Fish <- rbind(all8.1,all8.2, all8.3,all8.4,all8.5)
Fish<-Fish[!(Fish$lvl1=="N"),]
Fish<-Fish[!(Fish$lvl2=="N"),]
Fish<-Fish[!(Fish$lvl1=="zzOther"),]
Fish<-Fish[!(Fish$lvl2=="zzOther"),]
Fish<-Fish[complete.cases(Fish), ]
Fish<-Fish[!(Fish$Code=="11111"),]


#######make tree
###########need to write edges code and change images, run app

categories <- unique(Fish$Code) 

categories

links2<-Fish
nodes1 <- subset(links2, select = -c(lvl1) )
nodes1$path<-path_to_images
nodes1$png<-".png"
nodes <- data.frame(nodes1, id = 1:112, 
                    shape = c("image"),
                    image = gsub(" ","",paste(nodes1$path,"",nodes1$Code,"",nodes1$png)),
                    size=220)

nodes<- subset(nodes, select = -c(Code,code,path,png,lvl2) )
nodes <- nodes %>% mutate(font.size = 800)
nodes <- nodes %>% mutate(size = 500)


nodes[nrow(nodes) + 1,] <- c(1, "", 113,"image","https://hailaschultz.github.io/genomics/Fish/MyLegend.png",4000,500)



edges<-subset(Fish, select = -c(code,level) )
edges$from <- nodes$id[match(edges$lvl1, nodes1$lvl2)]
edges$to <- nodes$id[match(edges$lvl2, nodes1$lvl2)]
edges <- subset(edges, select = -c(lvl1) )
edges <- subset(edges, select = -c(lvl2) )
edges <- subset(edges, select = -c(label) )
edges = edges[-1,]


plot<-visNetwork(nodes, edges, height=800,width="100%") %>%
  visHierarchicalLayout(levelSeparation=14000,direction = "LR",nodeSpacing = 2500)%>%
  visNodes(shapeProperties = list(useBorderWithImage = FALSE),font=list(color="black",multi=TRUE,background="white"))%>%
  visPhysics(enabled=FALSE)%>%
  visEdges(smooth=list(enabled=TRUE,type="cubicBezier",forceDirection="horizontal"),color="black",width=40)
plot




