#######################
### ORGANIZING DATA ###
#######################

## Call in code to generate networks
source('./SupportCode.R')

## ID the current residents for each village
indivT1 <- subset(indiv,indiv$CurrentUr_2017=="Tenpatti")
indivA1 <- subset(indiv,indiv$CurrentUr_2017=="Azhagapuram")

indivT <- V(snSupT1)$name
indivA <- V(snSupA1)$name

library(reshape2)
library(plyr)
library(kinship2)

## Call in kinship pedigree data (to calculate relatedness coefs)
kin <- read.csv(file = "EFM.csv", stringsAsFactors=FALSE)
kin$Mother[which(kin$Mother=="0" | kin$Mother=="")] <- NA
kin$Father[which(kin$Father=="0" | kin$Father=="")] <- NA
ped <- pedigree(id=kin$Ego, dadid=kin$Father, momid=kin$Mother, sex = kin$Gender, missid = 0)

#create relatedness network
reladj <- kinship(ped)*2
#reladjs<-sparse.model.matrix(~.-1,data=as.data.frame(reladj))
relnet <- graph.adjacency(reladj, mode="undirected", weighted=TRUE, diag=FALSE)
reledge <- data.frame(get.edgelist(relnet),"r"=E(relnet)$weight,stringsAsFactors=FALSE)
reledge <- as.data.frame(rbind(cbind(reledge$X1,reledge$X2,reledge$r),cbind(reledge$X2,reledge$X1,reledge$r)))
colnames(reledge) <- c("ego","alter","r")
Tenkinmat<-reladj[indivT,indivT]
Alakinmat<-reladj[indivA,indivA]
diag(Alakinmat)<-0
diag(Tenkinmat)<-0

## Call in kinship data with all kin types (for different kin networks)
allkin <- read.table("../KinshipRelationsFullNoDivChildless.txt", sep="\t", header=1, stringsAsFactors=FALSE)
attach(allkin)

#Functions for extracting select relations and generating edgelists from this data

extractrelation <- function(egos, relcol) {
  ego <- MB <- character(0)
  edgelist <- data.frame(ego, MB)
  for (i in 1:length(egos)) {
    if (relcol[i] != "") {
      MBlist <- unlist(strsplit(relcol[i], ","))
      for (uncle in MBlist) {
        edgelist <- rbind(edgelist, c(egos[i], uncle), stringsAsFactors=FALSE)
      }
    }
  }
  return(edgelist)
}

selectedgelist <- function(egos, relationlist) {
  ego <- alter <- relation <- character(0)
  edges <- data.frame(ego, alter, relation)
  for (rel in 1:length(relationlist)) {
    temp <- extractrelation(egos, relationlist[[rel]])
    temp2 <- cbind(temp, names(relationlist)[[rel]])
    colnames(temp2) <- c("ego", "alter", "relation")
    edges <- rbind(edges, temp2, stringsAsFactors=FALSE)
  }
  return(edges)
}


#get edges for immediate family (excluding spouse)
closekinnoSp <- list(M=M, Fa=Fa, fB=fB, hB=hB, fZ=fZ, hZ=hZ, S=S, D=D)
closeedgesnoSp <- selectedgelist(Ego, closekinnoSp)
closeedgesnoSp$weight <- ifelse(grepl("h", closeedgesnoSp$relation), 0.25, 0.5) 
closeedgesnoSpT <- closeedgesnoSp[which(closeedgesnoSp$ego %in% indivT),]
closeedgesnoSpT <- closeedgesnoSpT[which(closeedgesnoSpT$alter %in% indivT),]
closeedgesnoSpA <- closeedgesnoSp[which(closeedgesnoSp$ego %in% indivA),]
closeedgesnoSpA <- closeedgesnoSpA[which(closeedgesnoSpA$alter %in% indivA),]

halfsibs <- rbind.data.frame(subset(closeedgesnoSpA,closeedgesnoSpA$relation=="hB"),subset(closeedgesnoSpA,closeedgesnoSpA$relation=="hZ"),subset(closeedgesnoSpT,closeedgesnoSpT$relation=="hB"),subset(closeedgesnoSpT,closeedgesnoSpT$relation=="hZ"))
mathalfsibs <- halfsibs[c(1,2,9,10,11,12),]
pathalfsibs <- halfsibs[c(3,4,5,6,7,8,13,14),]

closeedgesnoSpA <- closeedgesnoSpA[-which(closeedgesnoSpA$weight==0.25),]
closeedgesnoSpT <- closeedgesnoSpT[-which(closeedgesnoSpT$weight==0.25),]

#get edges for matrilateral kin
matriup <- list(MM=MM, MF=MF, MMF=MMF, MMM=MMM, MFF=MFF, MFM=MFM,
                MMfZ=MMfZ, MMfB=MMfB, MFfZ=MFfZ, MFfB=MFfB, MfZD=MfZD, MfZS=MfZS,
                MfBD=MfBD, MfBS=MfBS, 
                MfB=MfB, MhB=MhB, MfZ=MfZ, MhZ=MhZ)
matriedges <- selectedgelist(Ego, matriup)
matriedges$weight <- as.character(matriedges$relation)
matriedges$weight[matriedges$weight %in% c("MF","MM","MfB","MfZ")] <- 0.25
matriedges$weight[matriedges$weight %in% c("MhB","MhZ","MFF","MFM","MFfB","MFfZ","MMF","MMM","MMfB","MMfZ","MfBD","MfBS","MfZD","MfZS")] <- 0.125
matriedges <- rbind.data.frame(matriedges, mathalfsibs) #add maternal half-sibs
matriedges$weight <- as.numeric(matriedges$weight)
matriedgesT <- matriedges[which(matriedges$ego %in% indivT),]
matriedgesT <- matriedgesT[which(matriedgesT$alter %in% indivT),]
matriedgesA <- matriedges[which(matriedges$ego %in% indivA),]
matriedgesA <- matriedgesA[which(matriedgesA$alter %in% indivA),]


#get edges for patrilateral kin
patriup <- list(FM=FM, FF=FF, FMF=FMF, FMM=FMM, FFF=FFF, FFM=FFM,
                FMfZ=FMfZ, FMfB=FMfB, FFfZ=FFfZ, FFfB=FFfB, FfZD=FfZD, FfZS=FfZS,
                FfBD=FfBD, FfBS=FfBS, 
                FfB=FfB, FhB=FhB, FfZ=FfZ, FhZ=FhZ) 
patriedges <- selectedgelist(Ego, patriup)
patriedges$weight <- as.character(patriedges$relation)
patriedges$weight[patriedges$weight %in% c("FF","FM","FfB","FfZ")] <- 0.25
patriedges$weight[patriedges$weight %in% c("FhB","FhZ","FFF","FFM","FFfB","FFfZ","FMF","FMM","FMfB","FMfZ","FfBD","FfBS","FfZD","FfZS")] <- 0.125
patriedges <- rbind.data.frame(patriedges, pathalfsibs) #add maternal half-sibs
patriedges$weight <- as.numeric(patriedges$weight)
patriedgesT <- patriedges[which(patriedges$ego %in% indivT),]
patriedgesT <- patriedgesT[which(patriedgesT$alter %in% indivT),]
patriedgesA <- patriedges[which(patriedges$ego %in% indivA),]
patriedgesA <- patriedgesA[which(patriedgesA$alter %in% indivA),]

#get edges for parent child relationships only
parchi <- list(M=M, Fa=Fa, S=S, D=D)
parchiedges <- selectedgelist(Ego, parchi)
parchiedgesT <- parchiedges[which(parchiedges$ego %in% indivT),]
parchiedgesT <- parchiedgesT[which(parchiedgesT$alter %in% indivT),]
parchiedgesA <- parchiedges[which(parchiedges$ego %in% indivA),]
parchiedgesA <- parchiedgesA[which(parchiedgesA$alter %in% indivA),]

#get edges for spouses
spouseedges <- selectedgelist(Ego, list(Sp=Sp))
spouseedgesT <- spouseedges[which(spouseedges$ego %in% indivT),]
spouseedgesT <- spouseedgesT[which(spouseedgesT$alter %in% indivT),]
spouseedgesA <- spouseedges[which(spouseedges$ego %in% indivA),]
spouseedgesA <- spouseedgesA[which(spouseedgesA$alter %in% indivA),]

#prep spouse dataframes to deal with multiple spouses (remarriages) later on
spouseedgesSp1<-subset(spouseedges,duplicated(spouseedges$ego)==TRUE)
spouseedges2<-subset(spouseedges,duplicated(spouseedges$ego)==FALSE)
spouseedgesSp2<-subset(spouseedges2,spouseedges2$ego %in% c("Mahaalingam","Mutthupillai","Periyaampillai2","TN01402","TN02701","TN02707","TN04402","TN08801","TN13301","TN17101","TN17201","TN17401","TN22003","TN22601","TN24801","TN61401","TN62401","TN72301","TN76301","TN80803", "TN82001", "TN18302"))
spouseedgesbase<-subset(spouseedges2,!spouseedges2$ego %in% c("Mahaalingam","Mutthupillai","Periyaampillai2","TN01402","TN02701","TN02707","TN04402","TN08801","TN13301","TN17101","TN17201","TN17401","TN22003","TN22601","TN24801","TN61401","TN62401","TN72301","TN76301","TN80803", "TN82001", "TN18302"))
spouseedgesbaseSp1 <-rbind(spouseedgesbase,spouseedgesSp1)
spouseedgesbaseSp2 <-rbind(spouseedgesbase,spouseedgesSp2)

#get edges for parents
spousesfamily <- list(M=M, Fa=Fa, fZ=fZ, hZ=hZ, hB=hB, fB=fB,
                      MM=MM, MF=MF, MMF=MMF, MMM=MMM, MFF=MFF, MFM=MFM,
                      MMfZ=MMfZ, MMfB=MMfB, MFfZ=MFfZ, MFfB=MFfB, MfZD=MfZD, MfZS=MfZS,
                      MfBD=MfBD, MfBS=MfBS, MfB=MfB, MhB=MhB, MfZ=MfZ, MhZ=MhZ,
                      FM=FM, FF=FF, FMF=FMF, FMM=FMM, FFF=FFF, FFM=FFM,
                      FMfZ=FMfZ, FMfB=FMfB, FFfZ=FFfZ, FFfB=FFfB, FfZD=FfZD, FfZS=FfZS,
                      FfBD=FfBD, FfBS=FfBS, FfB=FfB, FhB=FhB, FfZ=FfZ, FhZ=FhZ)
sfedges <- selectedgelist(Ego, spousesfamily)
sfedges$weight <- ifelse(grepl("h", sfedges$relation), 0.25, 0.5) 
sfedges$weight <- ifelse(sfedges$relation %in% c("M", "Fa", "fZ", "fB"), 0.5, 0.125)
sfedges$weight[sfedges$relation %in% c("hZ", "hB", "MfZ", "MfB", "FfZ", "FfB", "MM", "MF", "FM", "FF")] <- 0.25

#replace ego with spouse to get parents-in-law; run twice and merge to get remarriages
spfamedges1 <- sfedges
for (i in 1:length(spfamedges1$ego)) {
  spouse <- spouseedgesbaseSp1$alter[which(spouseedgesbaseSp1$ego==spfamedges1$ego[i])]
  if (length(spouse) == 0) {
    spfamedges1$ego[i] <- NA
  }
  if (length(spouse) == 1) {
    spfamedges1$ego[i] <- spouse
  }
  if (length(spouse) > 1) {
    print(spouse)
  }
}

spfamedges2 <- sfedges
for (i in 1:length(spfamedges2$ego)) {
  spouse <- spouseedgesbaseSp2$alter[which(spouseedgesbaseSp2$ego==spfamedges2$ego[i])]
  if (length(spouse) == 0) {
    spfamedges2$ego[i] <- NA
  }
  if (length(spouse) == 1) {
    spfamedges2$ego[i] <- spouse
  }
  if (length(spouse) > 1) {
    print(spouse)
  }
}

spfamedges3 <- merge(spfamedges1,spfamedges2,by=c("ego","alter","relation","weight"),all.x=TRUE,all.y=TRUE)
spfamedges3a<-subset(spfamedges3,is.na(spfamedges3$ego)==FALSE)
spfamedges3a <- subset(spfamedges3a,duplicated(spfamedges3a)==FALSE) ## these two records get duplicated: "TN02707_Thaipillai" "TN02707_Veerachami" -- because TN02707 married one sister, then the other; so same sets of parents-in-law produced by each run through
spfamedges3a$relation <- paste("Sp",spfamedges3a$relation,sep="")
spfamedges3a <- spfamedges3a[-which(spfamedges3a $ego==spfamedges3a$alter),]

spouseedges$weight <- 1    
sfedgesnosp <- spfamedges3a 
sfedges <- rbind(spfamedges3a, spouseedges) 

sfedgesT <- sfedges[which(sfedges$ego %in% indivT),]
sfedgesT <- sfedgesT[which(sfedgesT$alter %in% indivT),]
sfedgesA <- sfedges[which(sfedges$ego %in% indivA),]
sfedgesA <- sfedgesA[which(sfedgesA$alter %in% indivA),]

sfedgesnospT <- sfedgesnosp[which(sfedgesnosp$ego %in% indivT),]
sfedgesnospT <- sfedgesnospT[which(sfedgesnospT$alter %in% indivT),]
sfedgesnospA <- sfedgesnosp[which(sfedgesnosp$ego %in% indivA),]
sfedgesnospA <- sfedgesnospA[which(sfedgesnospA$alter %in% indivA),]

#getting edges to spouses of matrilateral affinal kin
matri <- list(MMfZ=MMfZ, MMfB=MMfB, MFfZ=MFfZ, MFfB=MFfB, MfZD=MfZD, MfZS=MfZS,
              MfBD=MfBD, MfBS=MfBS, 
              MfB=MfB, MhB=MhB, MfZ=MfZ, MhZ=MhZ)
matriedges <- selectedgelist(Ego, matri)
matriedges <- rbind.data.frame(matriedges, mathalfsibs[,c(1:3)]) #add maternal half-sibs

matriedges1 <- matriedges
for (i in 1:length(matriedges1$ego)) {
  matriaff <- spouseedgesbaseSp1$alter[which(spouseedgesbaseSp1$ego == matriedges1$alter[i])]
  if (length(matriaff) == 0) {
    matriedges1$alter[i] <- NA
  }
  if (length(matriaff) == 1) {
    matriedges1$alter[i] <- matriaff
  }
  if (length(matriaff) > 1) {
    print(matriaff)
    print(matriedges1$ego[i])
  }
}

matriedges2 <- matriedges
for (i in 1:length(matriedges2$ego)) {
  matriaff <- spouseedgesbaseSp2$alter[which(spouseedgesbaseSp2$ego == matriedges2$alter[i])]
  if (length(matriaff) == 0) {
    matriedges2$alter[i] <- NA
  }
  if (length(matriaff) == 1) {
    matriedges2$alter[i] <- matriaff
  }
  if (length(matriaff) > 1) {
    print(matriaff)
    print(matriedges2$ego[i])
  }
}

matriaffedges3 <- merge(matriedges1,matriedges2,by=c("ego","alter","relation"),all.x=TRUE,all.y=TRUE)
matriaffedges3a <- subset(matriaffedges3,is.na(matriaffedges3$alter)==FALSE)

matriaffedges3a$weight <- as.character(matriaffedges3a$relation)
matriaffedges3a$weight[matriaffedges3a$weight %in% c("MfB","MfZ", "hB", "hZ")] <- 0.25
matriaffedges3a$weight[matriaffedges3a$weight %in% c("MhB","MhZ","MFF","MFM","MFfB","MFfZ","MMF","MMM","MMfB","MMfZ","MfBD","MfBS","MfZD","MfZS")] <- 0.125
matriaffedges3a$weight <- as.numeric(matriaffedges3a$weight)

matriaffedges3a$relation <- paste(matriaffedges3a$relation, "Sp", sep="")

matriaffedgesT <- matriaffedges3a[which(matriaffedges3a$ego %in% indivT),]
matriaffedgesT <- matriaffedgesT[which(matriaffedgesT$alter %in% indivT),]
matriaffedgesT <- matriaffedgesT[-which(matriaffedgesT$ego==matriaffedgesT$alter),]

matriaffedgesA <- matriaffedges3a[which(matriaffedges3a$ego %in% indivA),]
matriaffedgesA <- matriaffedgesA[which(matriaffedgesA$alter %in% indivA),]
matriaffedgesA <- matriaffedgesA[-which(matriaffedgesA$ego==matriaffedgesA$alter),]


#getting edges to spouses of patrilateral kin
patri <- list(FMfZ=FMfZ, FMfB=FMfB, FFfZ=FFfZ, FFfB=FFfB, FfZD=FfZD, FfZS=FfZS,
              FfBD=FfBD, FfBS=FfBS, 
              FfB=FfB, FhB=FhB, FfZ=FfZ, FhZ=FhZ) 
patriedges <- selectedgelist(Ego, patri)
patriedges <- rbind.data.frame(patriedges, pathalfsibs[,c(1:3)]) #add paternal half-sibs

patriedges1 <- patriedges
for (i in 1:length(patriedges1$ego)) {
  patriaff <- spouseedgesbaseSp1$alter[which(spouseedgesbaseSp1$ego == patriedges1$alter[i])]
  if (length(patriaff) == 0) {
    patriedges1$alter[i] <- NA
  }
  if (length(patriaff) == 1) {
    patriedges1$alter[i] <- patriaff
  }
  if (length(patriaff) > 1) {
    print(patriaff)
    print(patriedges1$ego[i])
  }
}

patriedges2 <- patriedges
for (i in 1:length(patriedges2$ego)) {
  patriaff <- spouseedgesbaseSp2$alter[which(spouseedgesbaseSp2$ego == patriedges2$alter[i])]
  if (length(patriaff) == 0) {
    patriedges2$alter[i] <- NA
  }
  if (length(patriaff) == 1) {
    patriedges2$alter[i] <- patriaff
  }
  if (length(patriaff) > 1) {
    print(patriaff)
    print(patriedges2$ego[i])
  }
}

patriaffedges3 <- merge(patriedges1,patriedges2,by=c("ego","alter","relation"),all.x=TRUE,all.y=TRUE)
patriaffedges3a <- subset(patriaffedges3,is.na(patriaffedges3$alter)==FALSE)

patriaffedges3a$weight <- as.character(patriaffedges3a$relation)
patriaffedges3a$weight[patriaffedges3a$weight %in% c("FfB","FfZ", "hB", "hZ")] <- 0.25
patriaffedges3a$weight[patriaffedges3a$weight %in% c("FhB","FhZ","FFF","FFM","FFfB","FFfZ","FMF","FMM","FMfB","FMfZ","FfBD","FfBS","FfZD","FfZS")] <- 0.125
patriaffedges3a$weight <- as.numeric(patriaffedges3a$weight)

patriaffedges3a$relation <- paste(patriaffedges3a$relation, "Sp", sep="")

patriaffedgesT <- patriaffedges3a[which(patriaffedges3a$ego %in% indivT),]
patriaffedgesT <- patriaffedgesT[which(patriaffedgesT$alter %in% indivT),]
patriaffedgesT <- patriaffedgesT[-which(patriaffedgesT$ego==patriaffedgesT$alter),]

patriaffedgesA <- patriaffedges3a[which(patriaffedges3a$ego %in% indivA),]
patriaffedgesA <- patriaffedgesA[which(patriaffedgesA$alter %in% indivA),]
patriaffedgesA <- patriaffedgesA[-which(patriaffedgesA$ego==patriaffedgesA$alter),]


#getting edges to siblings' spouses
sibs <- list(fZ=fZ, fB=fB, hZ=hZ, hB=hB)
sibedges <- selectedgelist(Ego, sibs)

sibedges1 <- sibedges
for (i in 1:length(sibedges1$ego)) {
  sibsp <- spouseedgesbaseSp1$alter[which(spouseedgesbaseSp1$ego == sibedges1$alter[i])]
  if (length(sibsp) == 0) {
    sibedges1$alter[i] <- NA
  }
  if (length(sibsp) == 1) {
    sibedges1$alter[i] <- sibsp
  }
  if (length(sibsp) > 1) {
    print(sibsp)
    print(sibedges1$ego[i])
  }
}

sibedges2 <- sibedges
for (i in 1:length(sibedges2$ego)) {
  sibsp <- spouseedgesbaseSp2$alter[which(spouseedgesbaseSp2$ego == sibedges2$alter[i])]
  if (length(sibsp) == 0) {
    sibedges2$alter[i] <- NA
  }
  if (length(sibsp) == 1) {
    sibedges2$alter[i] <- sibsp
  }
  if (length(sibsp) > 1) {
    print(sibsp)
    print(sibedges2$ego[i])
  }
}

sibspedges3 <- merge(sibedges1,sibedges2,by=c("ego","alter","relation"),all.x=TRUE,all.y=TRUE)
sibspedges3a <- subset(sibspedges3,is.na(sibspedges3$alter)==FALSE)

sibspedges3a$relation <- paste(sibspedges3a$relation, "Sp", sep="")

sibspedgesT <- sibspedges3a[which(sibspedges3a$ego %in% indivT),]
sibspedgesT <- sibspedgesT[which(sibspedgesT$alter %in% indivT),]
sibspedgesT$weight <- ifelse(grepl("h", sibspedgesT$relation), 0.25, 0.5) 

sibspedgesA <- sibspedges3a[which(sibspedges3a$ego %in% indivA),]
sibspedgesA <- sibspedgesA[which(sibspedgesA$alter %in% indivA),]
sibspedgesA$weight <- ifelse(grepl("h", sibspedgesA$relation), 0.25, 0.5) 


#get edges to rest of siblings' family
sisfam <- list(fZ=fZ, fZD=fZD, fZS=fZS, hZ=hZ, hZD=hZD, hZS=hZS) 
brofam <- list(fB=fB, fBD=fBD, fBS=fBS, hB=hB, hBD=hBD, hBS=hBS)

sisfamedges <- selectedgelist(Ego, sisfam)
brofamedges <- selectedgelist(Ego, brofam)

sisfamedges$weight <- ifelse(grepl("h", sisfamedges$relation), 0.25, 0.5)
sisfamedges$weight <- ifelse(grepl("D", sisfamedges$relation), sisfamedges$weight/2, sisfamedges$weight) 
sisfamedges$weight <- ifelse(grepl("S", sisfamedges$relation), sisfamedges$weight/2, sisfamedges$weight)

brofamedges$weight <- ifelse(grepl("h", brofamedges$relation), 0.25, 0.5)
brofamedges$weight <- ifelse(grepl("D", brofamedges$relation), brofamedges$weight/2, brofamedges$weight) 
brofamedges$weight <- ifelse(grepl("S", brofamedges$relation), brofamedges$weight/2, brofamedges$weight)

sibspedges3a$weight <- ifelse(grepl("h", sibspedges3a$relation), 0.25, 0.5)
sisfamedges <- rbind(sisfamedges, sibspedges3a[which(sibspedges3a$relation %in% c("fZSp", "hZSp")),])
brofamedges <- rbind(brofamedges, sibspedges3a[which(sibspedges3a$relation %in% c("fBSp", "hBSp")),])

#create edges between siblings and their siblings family, broken down by sibling gender combinations
sisedgesT <- sisfamedges[which(sisfamedges$ego %in% indivT),]
sisedgesT <- sisedgesT[which(sisedgesT$alter %in% indivT),]
sisedgesT1 <- merge(sisedgesT,kin[,c(1,4)],by.x="ego",by.y="Ego",all.x=TRUE)
sisedgesTbro <- subset(sisedgesT1,sisedgesT1$Gender=="1")
sisedgesTsis <- subset(sisedgesT1,sisedgesT1$Gender=="2")
sisedgesTbro$Gender<-NULL
sisedgesTsis$Gender<-NULL

sisedgesA <- sisfamedges[which(sisfamedges$ego %in% indivA),]
sisedgesA <- sisedgesA[which(sisedgesA$alter %in% indivA),]
sisedgesA1 <- merge(sisedgesA,kin[,c(1,4)],by.x="ego",by.y="Ego",all.x=TRUE)
sisedgesAbro <- subset(sisedgesA1,sisedgesA1$Gender=="1")
sisedgesAsis <- subset(sisedgesA1,sisedgesA1$Gender=="2")
sisedgesAbro$Gender<-NULL
sisedgesAsis$Gender<-NULL

broedgesT <- brofamedges[which(brofamedges$ego %in% indivT),]
broedgesT <- broedgesT[which(broedgesT$alter %in% indivT),]
broedgesT1 <- merge(broedgesT,kin[,c(1,4)],by.x="ego",by.y="Ego",all.x=TRUE)
broedgesTbro <- subset(broedgesT1,broedgesT1$Gender=="1")
broedgesTsis <- subset(broedgesT1,broedgesT1$Gender=="2")
broedgesTbro$Gender<-NULL
broedgesTsis$Gender<-NULL

broedgesA <- brofamedges[which(brofamedges$ego %in% indivA),]
broedgesA <- broedgesA[which(broedgesA$alter %in% indivA),]
broedgesA1 <- merge(broedgesA,kin[,c(1,4)],by.x="ego",by.y="Ego",all.x=TRUE)
broedgesAbro <- subset(broedgesA1,broedgesA1$Gender=="1")
broedgesAsis <- subset(broedgesA1,broedgesA1$Gender=="2")
broedgesAbro$Gender<-NULL
broedgesAsis$Gender<-NULL

#get mother's brothers
mothersbrothers <- list(MfB=MfB, MhB=MhB)
mbedges <- selectedgelist(Ego, mothersbrothers)
mbedgesT <- mbedges[which(mbedges$ego %in% indivT),]
mbedgesT <- mbedgesT[which(mbedgesT$alter %in% indivT),]
mbedgesA <- mbedges[which(mbedges$ego %in% indivA),]
mbedgesA <- mbedgesA[which(mbedgesA$alter %in% indivA),]


## Now use all these edgelists to build network objects 

library(intergraph)

emptyT<-make_empty_graph(n=length(indivT)) %>% set_vertex_attr("name",value=indivT)
emptyA<-make_empty_graph(n=length(indivA)) %>% set_vertex_attr("name",value=indivA)

kinnets <- list(matrinetA=matriedgesA, matrinetT=matriedgesT,
                patrinetA=patriedgesA, patrinetT=patriedgesT,
                closenetnoSpA=closeedgesnoSpA, closenetnoSpT=closeedgesnoSpT,
                spousenetA=spouseedgesA, spousenetT=spouseedgesT,
                sfnetA=sfedgesA, sfnetT=sfedgesT,
                sfnetnospA=sfedgesnospA, sfnetnospT=sfedgesnospT,
                parchinetA=parchiedgesA, parchinetT=parchiedgesT,
                sibspnetA=sibspedgesA,sibspnetT=sibspedgesT,
                patriaffnetA=patriaffedgesA,patriaffnetT=patriaffedgesT,
                matriaffnetA=matriaffedgesA,matriaffnetT=matriaffedgesT)

for (i in 1:length(kinnets)){
  edgemat <- kinnets[[i]]
  #this removes any duplicate entries, keeping the closest relationship between 2 people
  if (exists("weight", where=edgemat)) {
    edgemat <- edgemat[order(edgemat[,1], edgemat[,2], -edgemat[,"weight"]),]
    edgemat <- edgemat[!duplicated(edgemat[,1:2]),]
  }
  if (i %%2 == 1) {empnet <- emptyA}
  else {empnet <- emptyT}
  net <- add_edges(empnet, as.matrix(t(edgemat[,1:2]), ncol=1, byrow=TRUE))
  net <- set.edge.attribute(net, name="relation", index=E(net), as.character(edgemat$relation))
  if (exists("weight", where=edgemat)) {
    net <- set.edge.attribute(net, name="weight", index=E(net), edgemat$weight)
  }
  net <- as.undirected(net, mode="collapse", edge.attr.comb="first")
  if (any_multiple(net)==TRUE){ 
    net <- simplify(net,remove.multiple=TRUE)
  }
  kinnets[[i]] <- asNetwork(net)
  rm(net)
}

sibnets <- list(sisfamnetAbro=sisedgesAbro, sisfamnetTbro=sisedgesTbro,sisfamnetAsis=sisedgesAsis, sisfamnetTsis=sisedgesTsis,brofamnetAbro=broedgesAbro, brofamnetTbro=broedgesTbro,brofamnetAsis=broedgesAsis, brofamnetTsis=broedgesTsis)
for (i in 1:length(sibnets)){
  edgemat <- sibnets[[i]]
  if (exists("weight", where=edgemat)) {
    edgemat <- edgemat[order(edgemat[,1], edgemat[,2], -edgemat[,"weight"]),]
    edgemat <- edgemat[!duplicated(edgemat[,1:2]),]
  }
  if (i %%2 == 1) {empnet <- emptyA}
  else {empnet <- emptyT}
  net <- add_edges(empnet, as.matrix(t(edgemat[,1:2]), ncol=1, byrow=TRUE))
  net <- set.edge.attribute(net, name="relation", index=E(net), as.character(edgemat$relation))
  if (exists("weight", where=edgemat)) {
    net <- set.edge.attribute(net, name="weight", index=E(net), edgemat$weight)
  }
  sibnets[[i]] <- asNetwork(net)
  rm(net)
}

#Note, the above erases the following double relations:
#TN80305 _ TN80602 BOTH MfBSp DSpM 
#TN08606 _ TN08502 BOTH fBSp DSpM
#TN13702 _ TN00202 BOTH fBSp DSpM
#TN18302 _ TN02501 BOTH DSp  SSpF

detach(package:igraph)
library(sna)

#support network
Net_snSupT1 <- asNetwork(snSupT1)
Net_snSupA1 <- asNetwork(snSupA1)

#relatives 
Net_relatedA <- as.network(Alakinmat, directed=FALSE, ignore.eval=FALSE, name="R")
Net_relatedT <- as.network(Tenkinmat, directed=FALSE, ignore.eval=FALSE, name="R")


## create matrices from the network objects that can be used as edge terms in ERGMs or to tally counts for regressions

Mat_snSupT1 <- as.matrix(Net_snSupT1)
Mat_snSupA1 <- as.matrix(Net_snSupA1)
Mat_matriT <- as.matrix(kinnets$matrinetT)
Mat_matriA <- as.matrix(kinnets$matrinetA)
Mat_patriT <- as.matrix(kinnets$patrinetT)
Mat_patriA <- as.matrix(kinnets$patrinetA)
Mat_closenetnoSpT <- as.matrix(kinnets$closenetnoSpT)
Mat_closenetnoSpA <- as.matrix(kinnets$closenetnoSpA)
Mat_closenetnoSprelT <- as.matrix(kinnets$closenetnoSpT, attrname="weight")
Mat_closenetnoSprelA <- as.matrix(kinnets$closenetnoSpA, attrname="weight")
Mat_spouseT <- as.matrix(kinnets$spousenetT)
Mat_spouseA <- as.matrix(kinnets$spousenetA)
Mat_sfnetnoSpT <- as.matrix(kinnets$sfnetnospT)
Mat_sfnetnoSpA <- as.matrix(kinnets$sfnetnospA)
Mat_sfnetnoSprelT <- as.matrix(kinnets$sfnetnospT, attrname="weight")
Mat_sfnetnoSprelA <- as.matrix(kinnets$sfnetnospA, attrname="weight")
Mat_parchiT <- as.matrix(kinnets$parchinetT)
Mat_parchiA <- as.matrix(kinnets$parchinetA)
Mat_sibspA <- as.matrix(kinnets$sibspnetA)
Mat_sibspT <- as.matrix(kinnets$sibspnetT)
Mat_patriaffA <- as.matrix(kinnets$patriaffnetA)
Mat_patriaffT <- as.matrix(kinnets$patriaffnetT)
Mat_matriaffA <- as.matrix(kinnets$matriaffnetA)
Mat_matriaffT <- as.matrix(kinnets$matriaffnetT)


#directed binary sibling nets
Mat_brotosisfT <- t(as.matrix(sibnets$sisfamnetTbro))
Mat_sistosisfT <- t(as.matrix(sibnets$sisfamnetTsis))
Mat_brotosisfA <- t(as.matrix(sibnets$sisfamnetAbro))
Mat_sistosisfA <- t(as.matrix(sibnets$sisfamnetAsis))
Mat_brotobrofT <- t(as.matrix(sibnets$brofamnetTbro))
Mat_sistobrofT <- t(as.matrix(sibnets$brofamnetTsis))
Mat_brotobrofA <- t(as.matrix(sibnets$brofamnetAbro))
Mat_sistobrofA <- t(as.matrix(sibnets$brofamnetAsis))


## as some kin will be BOTH mat and pat, using the relationship itself to determine values here; so, if someone is both related through both matri and patri, those values could be different. 
Mat_patrelA<-as.matrix(kinnets$patrinetA, attrname="weight")
Mat_matrelA<-as.matrix(kinnets$matrinetA, attrname="weight")
Mat_patrelT<-as.matrix(kinnets$patrinetT, attrname="weight")
Mat_matrelT<-as.matrix(kinnets$matrinetT, attrname="weight")

Mat_pataffrelA<-as.matrix(kinnets$patriaffnetA, attrname="weight")
Mat_mataffrelA<-as.matrix(kinnets$matriaffnetA, attrname="weight")
Mat_pataffrelT<-as.matrix(kinnets$patriaffnetT, attrname="weight")
Mat_mataffrelT<-as.matrix(kinnets$matriaffnetT, attrname="weight")
Mat_sibsprelA <- as.matrix(kinnets$sibspnetA, attrname="weight")
Mat_sibsprelT <- as.matrix(kinnets$sibspnetT, attrname="weight")


#truncate relatedness to 0.125 and double to improve model estimation
Alakinmattrunc <- Alakinmat
Alakinmattrunc[which(Alakinmattrunc<0.125)] <- 0
Alakinmattrunc2 <- Alakinmattrunc*2

Tenkinmattrunc <- Tenkinmat
Tenkinmattrunc[which(Tenkinmattrunc<0.125)] <- 0
Tenkinmattrunc2 <- Tenkinmattrunc*2

#Break out relatedness matrix by coefficient
Biol5A<-Alakinmattrunc2
Biol5A[!Biol5A>=1]<-0
Biol5A[Biol5A>0]<-1

Biol25A<-Alakinmattrunc2
Biol25A[!(Alakinmattrunc2>=0.5 & Alakinmattrunc2<1)]<-0
Biol25A[Biol25A>0]<-1

Biol125A<-Alakinmattrunc2
Biol125A[!Biol125A<0.5]<-0
Biol125A[Biol125A>0]<-1

Biol5T<-Tenkinmattrunc2
Biol5T[!Biol5T>=1]<-0
Biol5T[Biol5T>0]<-1

Biol25T<-Tenkinmattrunc2
Biol25T[!(Tenkinmattrunc2>=0.5 & Tenkinmattrunc2<1)]<-0
Biol25T[Biol25T>0]<-1

Biol125T<-Tenkinmattrunc2
Biol125T[!Biol125T<0.5]<-0
Biol125T[Biol125T>0]<-1

#Break out spouses and other affines by coefficients
Aff1A <- Mat_spouseA

AffA <- pmax(Mat_sfnetnoSprelA, Mat_mataffrelA, Mat_pataffrelA, Mat_sibsprelA)

Aff5A <- ifelse(AffA==0.5, 1, 0)
Aff25A <- ifelse(AffA==0.25, 1, 0)
Aff125A <- ifelse(AffA==0.125, 1, 0)


Aff1T <- Mat_spouseT

AffT <- pmax(Mat_sfnetnoSprelT, Mat_mataffrelT, Mat_pataffrelT, Mat_sibsprelT)

Aff5T <- ifelse(AffT==0.5, 1, 0)
Aff25T <-ifelse(AffT==0.25, 1, 0)
Aff125T <- ifelse(AffT==0.125, 1, 0)

#break out mat and pat relatives by relatedness coefficients
Mat25T <- ifelse(Mat_matrelT==0.25, 1, 0)
Mat125T <- ifelse(Mat_matrelT==0.125, 1, 0)

Pat25T <- ifelse(Mat_patrelT==0.25, 1, 0)
Pat125T <- ifelse(Mat_patrelT==0.125, 1, 0)

Mat25A <- ifelse(Mat_matrelA==0.25, 1, 0)
Mat125A <- ifelse(Mat_matrelA==0.125, 1, 0)

Pat25A <- ifelse(Mat_patrelA==0.25, 1, 0)
Pat125A <- ifelse(Mat_patrelA==0.125, 1, 0)

#break out affinals by family's spouses vs spouse's family (incl. sibling spouse in latter)
ltaffA <- pmax(Mat_mataffrelA, Mat_pataffrelA)
ltaff25A <- ifelse(ltaffA==0.25, 1, 0)
ltaff125A <- ifelse(ltaffA==0.125, 1, 0)

newaff25A <- ifelse(Mat_sfnetnoSprelA==0.25, 1, 0)
newaff125A <- ifelse(Mat_sfnetnoSprelA==0.125, 1, 0)

ltaffT <- pmax(Mat_mataffrelT, Mat_pataffrelT)
ltaff25T <- ifelse(ltaffT==0.25, 1, 0)
ltaff125T <- ifelse(ltaffT==0.125, 1, 0)

newaff25T <- ifelse(Mat_sfnetnoSprelT==0.25, 1, 0)
newaff125T <- ifelse(Mat_sfnetnoSprelT==0.125, 1, 0)