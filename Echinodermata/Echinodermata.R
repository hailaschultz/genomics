########Echinodermata#######



path_to_images <- "https://hailaschultz.github.io/genomics/Decapoda/"

library(visNetwork)
library(RCurl)
library(dplyr)


all1<-subset(All,Phylum=="Echinodermata")
###Phylum

all4<- all1 %>%
  group_by(Kingdom,Phylum,Marker,sum) %>%
  summarise(
    value = sum(sum))

all5<-dcast(all4, Kingdom+Phylum~Marker, value.var = "sum")
all6<-all5 %>% mutate(COI1 =
                        case_when(COI <= 0~ "",
                                  COI>= 0 ~ "COI"))
all7<-all6 %>% mutate(Morph1 =
                        case_when(Morphology <= 0~ "",
                                  Morphology>= 0 ~ "Morph"))
all8<-all7 %>% mutate(Cop16S1 =
                        case_when(Cop16S <= 0~ "",
                                  Cop16S>= 0 ~ "Cop16S"))
all9<-all8 %>% mutate(GenFish1 =
                        case_when(GenFish <= 0~ "",
                                  GenFish >= 0 ~ "GenFish "))
all10<-all9 %>% mutate(Mol16S1 =
                         case_when(Mol16S <= 0~ "",
                                   Mol16S>= 0 ~ "Mol16S"))


all10$Markers <- paste(all10$COI1,all10$Cop16S1,all10$Morph1,all10$GenFish1,all10$Mol16S1)
all11 <- all10[ -c(3,4,5,6,7,8,9,10,11,12) ]
all12<- data.frame(lapply(all11, trimws), stringsAsFactors = FALSE)
all12$Markers <- gsub(" ", "", all12$Markers)
all12$lvl1<-all12$Kingdom
all12$lvl2<-all12$Phylum
all12$Marker<-all12$Markers
all13 <- all12[ -c(1,2,3) ]
all13 <- slice(all13, 1:(n() - 1)) 
all13.11<-all13
all13.11$level<-1
all13.11$label <-all13.11$lvl2

###CLass

all4<- all1 %>%
  group_by(Phylum,Class,Marker,sum) %>%
  summarise(
    value = sum(sum))

all5<-dcast(all4, Phylum+Class~Marker, value.var = "sum")
all6<-all5 %>% mutate(COI1 =
                        case_when(COI <= 0~ "",
                                  COI>= 0 ~ "COI"))
all7<-all6 %>% mutate(Morph1 =
                        case_when(Morphology <= 0~ "",
                                  Morphology>= 0 ~ "Morph"))
all8<-all7 %>% mutate(Cop16S1 =
                        case_when(Cop16S <= 0~ "",
                                  Cop16S>= 0 ~ "Cop16S"))
all9<-all8 %>% mutate(GenFish1 =
                        case_when(GenFish  <= 0~ "",
                                  GenFish >= 0 ~ "GenFish "))
all10<-all9 %>% mutate(Mol16S1 =
                         case_when(Mol16S <= 0~ "",
                                   Mol16S>= 0 ~ "Mol16S"))


all10$Markers <- paste(all10$COI1,all10$Cop16S1,all10$Morph1,all10$GenFish1,all10$Mol16S1)
all11 <- all10[ -c(3,4,5,6,7,8,9,10,11,12) ]
all12<- data.frame(lapply(all11, trimws), stringsAsFactors = FALSE)
all12$Markers <- gsub(" ", "", all12$Markers)
all12$lvl1<-all12$Phylum
all12$lvl2<-all12$Class
all12$Marker<-all12$Markers
all13 <- all12[ -c(1,2,3) ]
all13.1<-all13
all13.1$level<-2
all13.1$label <-all13.1$lvl2

###Order

all4<- all1 %>%
  group_by(Class,Order,Marker,sum) %>%
  summarise(
    value = sum(sum))

all5<-dcast(all4, Class+Order~Marker, value.var = "sum")
all6<-all5 %>% mutate(COI1 =
                        case_when(COI <= 0~ "",
                                  COI>= 0 ~ "COI"))
all7<-all6 %>% mutate(Morph1 =
                        case_when(Morphology <= 0~ "",
                                  Morphology>= 0 ~ "Morph"))
all8<-all7 %>% mutate(Cop16S1 =
                        case_when(Cop16S <= 0~ "",
                                  Cop16S>= 0 ~ "Cop16S"))
all9<-all8 %>% mutate(GenFish1 =
                        case_when(GenFish  <= 0~ "",
                                  GenFish >= 0 ~ "GenFish "))
all10<-all9 %>% mutate(Mol16S1 =
                         case_when(Mol16S <= 0~ "",
                                   Mol16S>= 0 ~ "Mol16S"))


all10$Markers <- paste(all10$COI1,all10$Cop16S1,all10$Morph1,all10$GenFish1,all10$Mol16S1)
all11 <- all10[ -c(3,4,5,6,7,8,9,10,11,12) ]
all12<- data.frame(lapply(all11, trimws), stringsAsFactors = FALSE)
all12$Markers <- gsub(" ", "", all12$Markers)
all12$lvl1<-all12$Class
all12$lvl2<-all12$Order
all12$Marker<-all12$Markers
all13 <- all12[ -c(1,2,3) ]
all13 <- slice(all13, 1:(n() - 1)) 
all13.2<-all13
all13.2$level<-3
all13.2$label <-all13.2$lvl2


#####Family

all4<- all1 %>%
  group_by(Order,Family,Marker,sum) %>%
  summarise(
    value = sum(sum))

all5<-dcast(all4, Order+Family~Marker, value.var = "sum")
all6<-all5 %>% mutate(COI1 =
                        case_when(COI <= 0~ "",
                                  COI>= 0 ~ "COI"))
all7<-all6 %>% mutate(Morph1 =
                        case_when(Morphology <= 0~ "",
                                  Morphology>= 0 ~ "Morph"))
all8<-all7 %>% mutate(Cop16S1 =
                        case_when(Cop16S <= 0~ "",
                                  Cop16S>= 0 ~ "Cop16S"))
all9<-all8 %>% mutate(GenFish1 =
                        case_when(GenFish  <= 0~ "",
                                  GenFish >= 0 ~ "GenFish "))
all10<-all9 %>% mutate(Mol16S1 =
                         case_when(Mol16S <= 0~ "",
                                   Mol16S>= 0 ~ "Mol16S"))

all10$Markers <- paste(all10$COI1,all10$Cop16S1,all10$Morph1,all10$GenFish1,all10$Mol16S1)
all11 <- all10[ -c(3,4,5,6,7,8,9,10,11,12) ]
all12<- data.frame(lapply(all11, trimws), stringsAsFactors = FALSE)
all12$Markers <- gsub(" ", "", all12$Markers)
all12$lvl1<-all12$Order
all12$lvl2<-all12$Family
all12$Marker<-all12$Markers
all13 <- all12[ -c(1,2,3) ]
all13 <- slice(all13, 1:(n() - 1))
all13.3<-all13
all13.3$level<-4
all13.3$label<-all13.3$lvl2

#####Genus

all4<- all1 %>%
  group_by(Family,Genus,Marker,sum) %>%
  summarise(
    value = sum(sum))

all5<-dcast(all4, Family+Genus~Marker, value.var = "sum")
all6<-all5 %>% mutate(COI1 =
                        case_when(COI <= 0~ "",
                                  COI>= 0 ~ "COI"))
all7<-all6 %>% mutate(Morph1 =
                        case_when(Morphology <= 0~ "",
                                  Morphology>= 0 ~ "Morph"))
all8<-all7 %>% mutate(Cop16S1 =
                        case_when(Cop16S <= 0~ "",
                                  Cop16S>= 0 ~ "Cop16S"))
all9<-all8 %>% mutate(GenFish1 =
                        case_when(GenFish  <= 0~ "",
                                  GenFish >= 0 ~ "GenFish "))
all10<-all9 %>% mutate(Mol16S1 =
                         case_when(Mol16S <= 0~ "",
                                   Mol16S>= 0 ~ "Mol16S"))

all10$Markers <- paste(all10$COI1,all10$Cop16S1,all10$Morph1,all10$GenFish1,all10$Mol16S1)
all11 <- all10[ -c(3,4,5,6,7,8,9,10,11,12) ]
all12<- data.frame(lapply(all11, trimws), stringsAsFactors = FALSE)
all12$Markers <- gsub(" ", "", all12$Markers)
all12$lvl1<-all12$Family
all12$lvl2<-all12$Genus
all12$Marker<-all12$Markers
all13 <- all12[ -c(1,2,3) ]
all13 <- slice(all13, 1:(n() - 1))
all13.4<-all13
all13.4$level<-5
all13.4$label <-paste("<i>",all13.4$lvl2,"</i>")


#####Species
all4<- all1 %>%
  group_by(Genus,Species,Marker,sum) %>%
  summarise(
    value = sum(sum))

all5<-dcast(all4, Genus+Species~Marker, value.var = "sum")
all6<-all5 %>% mutate(COI1 =
                        case_when(COI <= 0~ "",
                                  COI>= 0 ~ "COI"))
all7<-all6 %>% mutate(Morph1 =
                        case_when(Morphology <= 0~ "",
                                  Morphology>= 0 ~ "Morph"))
all8<-all7 %>% mutate(Cop16S1 =
                        case_when(Cop16S <= 0~ "",
                                  Cop16S>= 0 ~ "Cop16S"))
all9<-all8 %>% mutate(GenFish1 =
                        case_when(GenFish  <= 0~ "",
                                  GenFish >= 0 ~ "GenFish "))
all10<-all9 %>% mutate(Mol16S1 =
                         case_when(Mol16S <= 0~ "",
                                   Mol16S>= 0 ~ "Mol16S"))

all10$Markers <- paste(all10$COI1,all10$Cop16S1,all10$Morph1,all10$GenFish1,all10$Mol16S1)
all11 <- all10[ -c(3,4,5,6,7,8,9,10,11,12) ]
all12<- data.frame(lapply(all11, trimws), stringsAsFactors = FALSE)
all12$Markers <- gsub(" ", "", all12$Markers)
all12$lvl1<-all12$Genus
all12$lvl2<-all12$Species
all12$Marker<-all12$Markers
all13 <- all12[ -c(1,2,3) ]
all13 <- slice(all13, 1:(n() - 1))
all13.5<-all13
all13.5$level<-6
all13.5$label <-paste("<i>",all13.5$lvl2,"</i>")

#combine tables
Echinodermata<- rbind(all13.11,all13.1,all13.2, all13.3,all13.4,all13.5)
Echinodermata<-Echinodermata[complete.cases(Echinodermata), ]



#######make tree
###########need to write edges code and change images, run app

categories <- unique(Echinodermata$Marker) 
categories

links2<-Echinodermata
nodes1 <- subset(links2, select = -c(lvl1) )
nodes1$path<-path_to_images
nodes1$png<-".png"
nodes <- data.frame(nodes1, id = 1:58, 
                    shape = c("image"),
                    image = gsub(" ","",paste(nodes1$path,"",nodes1$Marker,"",nodes1$png)),
                    size=220)

nodes<- subset(nodes, select = -c(Marker,path,png,lvl2) )
nodes <- nodes %>% mutate(font.size = 400)

categories <- unique(nodes1$Marker) 
categories

nodes[nrow(nodes) + 1,] <- c(1, "", 59,"image","https://hailaschultz.github.io/genomics/Decapoda/DecapodaLegend.png",2000,300)



edges<-subset(Echinodermata, select = -c(Marker,level) )
edges$from <- nodes$id[match(edges$lvl1, nodes1$lvl2)]
edges$to <- nodes$id[match(edges$lvl2, nodes1$lvl2)]
edges <- subset(edges, select = -c(lvl1) )
edges <- subset(edges, select = -c(lvl2) )
edges <- subset(edges, select = -c(label) )
edges = edges[-1,]


plot<-visNetwork(nodes, edges, height=1000,width="100%") %>%
  visHierarchicalLayout(levelSeparation=5000,direction = "LR",nodeSpacing = 1500)%>%
  visNodes(shapeProperties = list(useBorderWithImage = FALSE),
           brokenImage="http://127.0.0.1:3274/",size=800, font=list(align="left",multi=TRUE))%>%
  visPhysics(enabled=FALSE)%>%
  visEdges(smooth=list(enabled=TRUE,type="cubicBezier",forceDirection="horizontal"),color="black",width=40)
plot



visExport(plot, type="png", name="network", background="#fff")

