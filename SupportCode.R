## Code for generating support networks

library(igraph)
library(reshape2)
library(plyr)
library(car)


## Read in the files that include details of each individual and the support network data
indiv <- read.csv("TN_Indiv.csv", header=TRUE, as.is=TRUE,stringsAsFactors=FALSE)
indiv$SUWealth_2017Log <- log(indiv$SUCashValue_2017/10000)
nl <- read.csv ( file =  "TN_Nodelist.csv", na="", header=TRUE, as.is=TRUE)

## Marital status and residence
indiv$natal <- indiv$CurrentUr_2017 == indiv$SondaUr
indiv$evermarried <- indiv$MaritalStatus_2017 != "Unmarried"
indiv$nat_mar <- paste(indiv$natal, indiv$evermarried, sep="nat_em")


###############
### NETWORK ###
###############

## Create edgelists for each network question
names<-unique(nl$Question)

for(i in 1:length(names)){
  nameN<-paste("n",unique(nl$Question),sep="")
  assign(nameN[i],subset(nl, Question == names[i], select = -Question))
  nameM<-paste("m",unique(nl$Question),sep="")
  assign(nameM[i],melt(get(nameN[i]), id.vars = "Ego", na.rm = T, value.name = "Alter"))
  nameE<-paste("e",unique(nl$Question),sep="")
  assign(nameE[i],subset(get(nameM[i]), select = -variable))
}

## For our purposes, we're ignoring #2 and #4 (the double sampled questions), and we're limiting the responses for #5-8 to include only those that are gender appropriate (so, women's answers to the female-specific questions; men's answers to the male-specific questions), and we're ignoring the reputational questions (#13-15)

## limiting the female directed question to include only those reported by females
e5$Gender <- indiv$Gender[match(e5$Ego,indiv$IndivID)]
e5f<-subset(e5,e5$Gender=="Female")

## and the same for males
e6$Gender <- indiv$Gender[match(e6$Ego,indiv$IndivID)]
e6m<-subset(e6,e6$Gender=="Male")

## can now make one unified behavioral edge type, which only has answers for the appropriate gender
elBehavioralU <- rbind(e5f,e6m)
elBehavioralU<-elBehavioralU[,1:2]

e7$Gender <- indiv$Gender [match(e7$Ego,indiv$IndivID)]
e7f<-subset(e7,e7$Gender=="Female")

e8$Gender <- indiv$Gender [match(e8$Ego,indiv$IndivID)]
e8m<-subset(e8,e8$Gender=="Male")

elInformationU <- rbind(e7f,e8m)
elInformationU<-elInformationU[,1:2]

e1$LoanAsk<-1
e3$BorrowAsk<-1
elBehavioralU$BehavHelp<-1
elInformationU$Information<-1
e11$ImpIss<-1
e12$Work<-1


dfSup<-merge(e1,e3,by=c("Ego","Alter"),all.x=TRUE,all.y=TRUE) 
dfSup<-merge(dfSup,elBehavioralU,by=c("Ego","Alter"),all.x=TRUE,all.y=TRUE)
dfSup<-merge(dfSup,elInformationU,by=c("Ego","Alter"),all.x=TRUE,all.y=TRUE)
dfSup<-merge(dfSup,e11,by=c("Ego","Alter"),all.x=TRUE,all.y=TRUE)
dfSup<-merge(dfSup,e12,by=c("Ego","Alter"),all.x=TRUE,all.y=TRUE)

dfSup[is.na(dfSup)] <- 0
dfSup$SupSum<-rowSums(dfSup[,3:8])
dfSup<-subset(dfSup,dfSup$SupSum>0)

## generate a network from the multilayer dataframe
sn <- graph.data.frame(dfSup)
id <- V(sn)$name
id <- data.frame(id)


## append the individual metadata to each individual included in the network
att <- merge(id,indiv,by.x="id",by.y="IndivID",sort=FALSE,all.x=TRUE)

## generate the full network (of support ties, not reputational nominations)
snFull <- graph.data.frame(d = dfSup, vertices = att, directed=TRUE)
snFull1 <- simplify(snFull,remove.loops=TRUE,remove.multiple=FALSE) ## removing self-loops

## reduce down to only those who completed the survey (but still with the villages linked)
snSup <- delete.vertices(snFull,V(snFull)[degree(snFull,mode="out")==0])
snSup1<-simplify(snSup,remove.loops=TRUE,remove.multiple=FALSE) ## removing self-loops

## dividing out for each village
snSupT <- delete.vertices(snFull,V(snFull)$CurrentUr_2017!="Tenpatti")
snSupT1 <- delete.vertices(snSupT,V(snSupT)[degree(snSupT,mode="out")==0 & !V(snSupT)$name %in% c("TN71005","TN74801")]) ## limiting to those who completed the survey; TN71005 and TN74801 both completed the survey, but then only named others who didn't themselves (i.e., people living elsewhere or under 18)

snSupT1<-simplify(snSupT1,remove.loops=TRUE,remove.multiple=FALSE) ## removing self-loops. all self-loops (there are 34) have been checked, and they are 'legit,' mostly people saying (when answering questions that are meant to be for all household members), that they support their household, essentially.


snSupA <- delete.vertices(snFull,V(snFull)$CurrentUr_2017!="Azhagapuram")
snSupA1 <- delete.vertices(snSupA,V(snSupA)[degree(snSupA,mode="out")==0]) ## limiting to those who completed the survey
snSupA1<-simplify(snSupA1,remove.loops=TRUE,remove.multiple=FALSE) ## removing self-loops. 


cent_calc <- function(graph){
  df <- data.frame("IndivID"=V(graph)$name)
  df$outdegree <- degree(graph,mode="out")
  df$outstrength <- graph.strength(graph,mode="out",weights=E(graph)$SupSum)
  df$indegree <- degree(graph,mode="in")
  df$instrength <- graph.strength(graph,mode="in",weights=E(graph)$SupSum)
  df$degree <- degree(graph,mode="all")
  df$strength <- graph.strength(graph,mode="all",weights=E(graph)$SupSum)
  df$wpr <- page_rank(graph,directed=TRUE,weights=E(graph)$SupSum)$vector
  df$trans <- transitivity(graph,type="local")
  df$transw <- transitivity(graph,type="weighted")
  return(df)
}

net_calc <- function(graph){
  netcalc <- c(length(V(graph)),
               length(E(graph)),
               sum(E(graph)$SupSum),
               mean(degree(graph,mode="in")),
               mean(graph.strength(graph,mode="in",weights=E(graph)$SupSum)),
               graph.density(graph),
               reciprocity(graph),
               transitivity(graph,type="global"),
               average.path.length(graph),
               diameter(graph),
               sum((degree(graph,mode="out")==0)),
               sum((degree(graph,mode="in")==0)))
  return(netcalc)
}


supA1centcalc <- cent_calc(snSupA1)
supT1centcalc <- cent_calc(snSupT1)

## Supplementary Table S3
netsummary <- as.data.frame(rbind(net_calc(snSupA1),net_calc(snSupT1)))
colnames(netsummary) <- c("Nodes","Edges","Nominations","Mean Degree","Mean Strength","Density","Reciprocity","Transitivity","Average Path Length","Diameter","Count Out-Degree 0","Count In-Degree 0")



########################
## Household Distance ##
########################

## These two files record the distance (as the bird flies) between every pair of households in the two villages
TenHouseholdDist<-read.csv("TenHouseholdDist.csv",as.is=TRUE)
AlaHouseholdDist<-read.csv("AlaHouseholdDist.csv",as.is=TRUE)

## Extract the household ID for each individual
Teni<-V(snSupT1)$name
Tenh<-V(snSupT1)$HouseID_2017
Tenv<-cbind(Teni,Tenh)
Tenv<-as.data.frame(Tenv)
colnames(Tenv)<-c("IndivID","HouseID")
Tenv<-Tenv[with(Tenv, order(IndivID)), ]

## Make an empty matrix with names as the House ID
Tendistancemat <-matrix(nrow=length(Tenv$IndivID),ncol=length(Tenv$IndivID),dimnames=list(as.vector(Tenv$HouseID),as.vector(Tenv$HouseID)))

## Populate the matrix with the values from the Distance file. This will take a moment.
for(i in 1:nrow(TenHouseholdDist)){Tendistancemat[rownames(Tendistancemat)==TenHouseholdDist[i,1],colnames(Tendistancemat)==TenHouseholdDist[i,2]] <- as.numeric(TenHouseholdDist[i,3])}

## Rename points so that they now correspond properly to IndivID not House ID
dimnames(Tendistancemat)<-list(as.vector(Tenv$IndivID),as.vector(Tenv$IndivID))

## Replace NAs, meaning individuals who are in the same house, with 0s
Tendistancemat[is.na(Tendistancemat)]<-0

Alai<-V(snSupA1)$name
Alah<-V(snSupA1)$HouseID_2017
Alav<-cbind(Alai,Alah)
Alav<-as.data.frame(Alav)
colnames(Alav)<-c("IndivID","HouseID")
Alav<-Alav[with(Alav, order(IndivID)), ]

Aladistancemat <-matrix(nrow=length(Alav$IndivID),ncol=length(Alav$IndivID),dimnames=list(as.vector(Alav$HouseID),as.vector(Alav$HouseID)))
for(i in 1:nrow(AlaHouseholdDist)){Aladistancemat[rownames(Aladistancemat)==AlaHouseholdDist[i,1],colnames(Aladistancemat)==AlaHouseholdDist[i,2]] <- as.numeric(AlaHouseholdDist[i,3])}
dimnames(Aladistancemat)<-list(as.vector(Alav$IndivID),as.vector(Alav$IndivID))
Aladistancemat[is.na(Aladistancemat)]<-0

rm(list=setdiff(ls(),c("Aladistancemat", "Tendistancemat","snFull1","snSupA1","snSupT1","indiv","dfSup","netsummary","supFull1centcalc","supA1centcalc","supT1centcalc")))
