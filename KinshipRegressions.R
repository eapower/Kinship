###################
### REGRESSIONS ###
###################

source("./KinshipDataMerge.R")

library(dplyr)
library(tidyr)
library(readr)

alltypes=list(Fa=Fa,M=M,fB=fB,hB=hB,fZ=fZ,hZ=hZ,D=D,DD=DD,DS=DS,DSp=DSp,DSpF=DSpF,DSpM=DSpM,
              S=S,SD=SD,SS=SS,SSp=SSp,SSpF=SSpF,SSpM=SSpM,
              DDD=DDD,DDS=DDS,DSD=DSD,DSS=DSS,SDD=SDD,SDS=SDS,SSD=SSD,SSS=SSS,
              fBD=fBD,fBS=fBS,fBDD=fBDD,fBDS=fBDS,fBSD=fBSD,fBSS=fBSS,fZD=fZD,
              fZS=fZS,fZDD=fZDD,fZDS=fZDS,fZSD=fZSD,fZSS=fZSS,hBD=hBD,hBS=hBS,
              hBDD=hBDD,hBDS=hBDS,hBSD=hBSD,hBSS=hBSS,hZD=hZD,hZS=hZS,hZDD=hZDD,
              hZDS=hZDS,fBSp=fBSp,fBDSp=fBDSp,fBSSp=fBSSp,fZSp=fZSp,
              fZDSp=fZDSp,fZSSp=fZSSp,hBSp=hBSp,hBDSp=hBDSp,hBSSp=hBSSp,hZSp=hZSp,hZDSp=hZDSp,
              FF=FF,FM=FM,FfB=FfB,FfZ=FfZ,FhB=FhB,FhZ=FhZ,MF=MF,MM=MM,MfB=MfB,MfZ=MfZ,MhB=MhB,MhZ=MhZ,
              FFF=FFF,FFM=FFM,FFfB=FFfB,FFfZ=FFfZ,FFhB=FFhB,
              FFhZ=FFhZ,FMF=FMF,FMM=FMM,FMfB=FMfB,FMfZ=FMfZ,MMF=MMF,MMM=MMM,
              MMfB=MMfB,MMfZ=MMfZ,MMhB=MMhB,MMhZ=MMhZ,FfBD=FfBD,FfBS=FfBS,FfBSp=FfBSp,
              FfZD=FfZD,FfZS=FfZS,FfZSp=FfZSp,MFF=MFF,MFM=MFM,MFfB=MFfB,MFfZ=MFfZ,
              MFhB=MFhB,MFhZ=MFhZ,FhBD=FhBD,FhBS=FhBS,FhBSp=FhBSp,FhZD=FhZD,
              FhZS=FhZS,FhZSp=FhZSp,MfBD=MfBD,MfBS=MfBS,MfBSp=MfBSp,MfZD=MfZD,
              MfZS=MfZS,MfZSp=MfZSp,MhBD=MhBD,MhBS=MhBS,MhBSp=MhBSp,MhZD=MhZD,
              MhZS=MhZS,MhZSp=MhZSp,Sp=Sp,SpF=SpF,SpM=SpM,SpfB=SpfB,SpfZ=SpfZ,
              SphB=SphB,SphZ=SphZ,MfSibChiChi=MfSibChiChi,FfSibChiChi=FfSibChiChi,
              MhSibChiChi=MhSibChiChi,FhSibChiChi=FhSibChiChi,MParParfSib=MParParfSib,
              FParParfSib=FParParfSib,MParParfSibChi=MParParfSibChi,
              FParParfSibChi=FParParfSibChi,MCos=MCos,FCos=FCos,fZChiChiChi=fZChiChiChi,
              fBChiChiChi=fBChiChiChi,hZChiChiChi=hZChiChiChi,
              hBChiChiChi=hBChiChiChi,MParParhSib=MParParhSib,FParParhSib=FParParhSib,
              MParParhSibChi=MParParhSibChi,FParParhSibChi=FParParhSibChi,
              MCosChi=MCosChi,FCosChi=FCosChi,MParParPar=MParParPar,FParParPar=FParParPar,
              SChiChiChi=SChiChiChi,DChiChiChi=DChiChiChi)
## note: no hZSD, hZSS, hZSSp, FMhB, FMhZ
alledges <- selectedgelist(Ego, alltypes)

## and again with all adult residents who did the survey
allsupedgesT <- alledges[which(alledges$ego %in% indivT),]
allsupedgesT <- allsupedgesT[which(allsupedgesT$alter %in% indivT),]
allsupedgesA <- alledges[which(alledges$ego %in% indivA),]
allsupedgesA <- allsupedgesA[which(allsupedgesA$alter %in% indivA),]

allsuppresA<-spread(count(allsupedgesA, ego, relation), relation, n, fill = 0)
allsuppresT<-spread(count(allsupedgesT, ego, relation), relation, n, fill = 0)
relativecount <- full_join(allsuppresA,allsuppresT)

#Now get "activated" support ties with kin

#note -- the support network here is directed; results would be different if we transposed or symmetrized! 
#spouseactT <- tibble("spouseact"=rowSums(Mat_spouseT*Mat_snSupT1))
#spouseactA <- tibble("spouseact"=rowSums(Mat_spouseA*Mat_snSupA1))
#spouseact <- bind_rows(spouseactA,spouseactT)

closerelactT <- tibble("closerelact"=rowSums(Mat_closenetnoSpT*Mat_snSupT1),"closerelres_SN"=rowSums(Mat_closenetnoSpT))
closerelactA <- tibble("closerelact"=rowSums(Mat_closenetnoSpA*Mat_snSupA1),"closerelres_SN"=rowSums(Mat_closenetnoSpA))
closerelact <- bind_rows(closerelactA,closerelactT)

#note: the matri and patri matrices have had the close relatives that could appear here selected out
matriactT <- tibble("matriact"=rowSums(Mat_matriT*Mat_snSupT1),"matrires_SN"=rowSums(Mat_matriT))
matriactA <- tibble("matriact"=rowSums(Mat_matriA*Mat_snSupA1),"matrires_SN"=rowSums(Mat_matriA))
matriact <- bind_rows(matriactA,matriactT)

patriactT <- tibble("patriact"=rowSums(Mat_patriT*Mat_snSupT1),"patrires_SN"=rowSums(Mat_patriT))
patriactA <- tibble("patriact"=rowSums(Mat_patriA*Mat_snSupA1),"patrires_SN"=rowSums(Mat_patriA))
patriact <- bind_rows(patriactA,patriactT)

#get directed edges for spouses' kin
library(igraph)
#ALL spouses' kin
emptyT<-make_empty_graph(n=length(indivT)) %>% set_vertex_attr("name",value=indivT)
emptyA<-make_empty_graph(n=length(indivA)) %>% set_vertex_attr("name",value=indivA)
netT <- add_edges(emptyT, as.matrix(t(sfedgesnospT[,1:2]), ncol=1, byrow=TRUE))
netA <- add_edges(emptyA, as.matrix(t(sfedgesnospA[,1:2]), ncol=1, byrow=TRUE))
newaffsmatA <- as_adj(netA, sparse=FALSE)
newaffsmatT <- as_adj(netT, sparse=FALSE)
#spouses' kin r > 0.125
sfedgesnospTsub <- sfedgesnospT[-which(sfedgesnospT$weight<=0.125),]
sfedgesnospAsub <- sfedgesnospA[-which(sfedgesnospA$weight<=0.125),]
netTsub <- add_edges(emptyT, as.matrix(t(sfedgesnospTsub[,1:2]), ncol=1, byrow=TRUE))
netAsub <- add_edges(emptyA, as.matrix(t(sfedgesnospAsub[,1:2]), ncol=1, byrow=TRUE))
newaffsmatAsub <- as_adj(netAsub, sparse=FALSE)
newaffsmatTsub <- as_adj(netTsub, sparse=FALSE)
detach(package:igraph)

Mat_affT <- newaffsmatT
Mat_affA <- newaffsmatA 
Mat_affsubT <- newaffsmatTsub 
Mat_affsubA <- newaffsmatAsub

spfamactT <- tibble("spfamact"=rowSums(Mat_affT*Mat_snSupT1),"spfamres_SN"=rowSums(Mat_affT))
spfamactA <- tibble("spfamact"=rowSums(Mat_affA*Mat_snSupA1),"spfamres_SN"=rowSums(Mat_affA))
spfamactsubT <- tibble("spfamactsub"=rowSums(Mat_affsubT*Mat_snSupT1),"spfamressub_SN"=rowSums(Mat_affsubT))
spfamactsubA <- tibble("spfamactsub"=rowSums(Mat_affsubA*Mat_snSupA1),"spfamressub_SN"=rowSums(Mat_affsubA))
spfamact <- bind_rows(spfamactA,spfamactT)
spfamactsub <- bind_rows(spfamactsubA,spfamactsubT)

All_affT <- pmax(Mat_mataffrelT, Mat_pataffrelT, Mat_sibspT, Mat_spouseT, Mat_sfnetnoSpT)
All_affA <- pmax(Mat_mataffrelA, Mat_pataffrelA, Mat_sibspA, Mat_spouseA, Mat_sfnetnoSpA) 
All_affT[All_affT>0] <- 1
All_affA[All_affA>0] <- 1
allaffactT <- tibble("allaffact"=rowSums(All_affT*Mat_snSupT1),"allaffres_SN"=rowSums(All_affT))
allaffactA <- tibble("allaffact"=rowSums(All_affA*Mat_snSupA1),"allaffres_SN"=rowSums(All_affA))
allaffact <- bind_rows(allaffactA,allaffactT)

TenkinmattruncBI <- Tenkinmattrunc
TenkinmattruncBI[TenkinmattruncBI>0] <- 1
AlakinmattruncBI <- Alakinmattrunc
AlakinmattruncBI[AlakinmattruncBI>0] <- 1
relactT <- tibble("relact"=rowSums(TenkinmattruncBI*Mat_snSupT1),"relres_SN"=rowSums(TenkinmattruncBI))
relactA <- tibble("relact"=rowSums(AlakinmattruncBI*Mat_snSupA1),"relres_SN"=rowSums(AlakinmattruncBI))
relact <- bind_rows(relactA,relactT)

#Include all affs here!
allrelT <- TenkinmattruncBI + Mat_matriaffT + Mat_patriaffT + Mat_sfnetnoSpT + Mat_spouseT
allrelT[allrelT > 0 ] <- 1
nonrelT <- Mat_snSupT1-allrelT
nonrelT[nonrelT<0] <- 0
nonrelT <- tibble("nonrel"=rowSums(nonrelT))

allrelA <- AlakinmattruncBI + Mat_matriaffA + Mat_patriaffA + Mat_sfnetnoSpA + Mat_spouseA
allrelA[allrelA > 0 ] <- 1
nonrelA <- Mat_snSupA1-allrelA
nonrelA[nonrelA<0] <- 0
nonrelA <- tibble("nonrel"=rowSums(nonrelA))
nonrel <- bind_rows(nonrelA,nonrelT)

activcounts <- bind_cols("ego" = c(indivA,indivT),#spouseact,
                         closerelact,
                         matriact,patriact,relact,spfamact,spfamactsub,allaffact,nonrel)
relativecount <- full_join(relativecount,activcounts,by="ego")


## COMPARE residents living in natal village to those in new village

indiv1 <- read_csv("TN_Indiv.csv")
indiv1 <- right_join(indiv1,relativecount,by=c("IndivID"="ego"))
indiv1$Natal <- indiv1$SondaUr %in% c("Azhagapuram", "Tenpatti")
indiv1$Natal[indiv1$IndivID %in% c("TN17103","TN63302")] <- FALSE ## these two women are from Azhagapuram and married into Tenpatti
indiv1$Natal[which(indiv1$Natal==TRUE)] <- "Natal"
indiv1$Natal[which(indiv1$Natal==FALSE)] <- "Non-Natal"

Ten <- subset(indiv1,CurrentUr_2017=="Tenpatti")
Ala <- subset(indiv1,CurrentUr_2017=="Azhagapuram")
TenSN <- subset(Ten,IndivID %in% indivT)
AlaSN <- subset(Ala,IndivID %in% indivA)

outdegreeA <- data.frame(IndivID=Net_snSupA1%v%"vertex.names", outdegree=degree(Net_snSupA1, cmode="outdegree"))
AlaSN <- left_join(AlaSN,outdegreeA,by="IndivID")
outdegreeT <- data.frame(IndivID=Net_snSupT1%v%"vertex.names", outdegree=degree(Net_snSupT1, cmode="outdegree"))
TenSN <- left_join(TenSN,outdegreeT,by="IndivID")
vills <- data.frame(rbind(AlaSN, TenSN))

notunmarriedT <- TenSN[which(TenSN$MaritalStatus_2017 != "Unmarried"),]
notunmarriedA <- AlaSN[which(AlaSN$MaritalStatus_2017 != "Unmarried"),]
notunmarriedA <- notunmarriedA[order(notunmarriedA$Natal, notunmarriedA$Gender, notunmarriedA$relres_SN),]
notunmarriedT <- notunmarriedT[order(notunmarriedT$Natal, notunmarriedT$Gender, notunmarriedT$relres_SN),]

#get only people with a coresident spouse
notunmSpT <- notunmarriedT[which(notunmarriedT$spfamres_SN>0),]
notunmSpA <- notunmarriedA[which(notunmarriedA$spfamres_SN>0),]

#get only people with any coresident relatives
notunmConT <- notunmarriedT[which(notunmarriedT$relres_SN>0),]
notunmConA <- notunmarriedA[which(notunmarriedA$relres_SN>0),]


###TESTS

#Do non-natal residents have fewer kin in the village?
famresT <- glm(relres_SN~Natal, data=notunmarriedT, family=poisson())
famresA <- glm(relres_SN~Natal, data=notunmarriedA, family=poisson())

#Fewer matrilateral relatives?
matriresT <- glm(matrires_SN~Natal, data=notunmarriedT, family=poisson())
matriresA <- glm(matrires_SN~Natal, data=notunmarriedA, family=poisson())

#Fewer patrilateral relatives?
patriresT <- glm(patrires_SN~Natal, data=notunmarriedT, family=poisson())
patriresA <- glm(patrires_SN~Natal, data=notunmarriedA, family=poisson())

#Do they fewer affinal kin?
affresT <- glm(spfamres_SN~Natal, data=notunmarriedT, family=poisson())
affresA <- glm(spfamres_SN~Natal, data=notunmarriedA, family=poisson())

#Do they have fewer support ties in the village?
odegA <- glm(outdegree~Natal, data=notunmarriedA, family=poisson())
odegT <- glm(outdegree~Natal, data=notunmarriedT, family=poisson())

#Do non-natal people name more consanguineal kin (if they have them)?
conTBN <- glm(cbind(relact, relres_SN-relact) ~ Natal, data = notunmarriedT, family = binomial)
conABN <- glm(cbind(relact, relres_SN-relact) ~ Natal, data = notunmarriedA, family = binomial)

#Do non-natal people name more patrilateral kin (if they have them)?
patriTBN <- glm(cbind(patriact, patrires_SN-patriact) ~ Natal, data = notunmarriedT, family = binomial)
patriABN <- glm(cbind(patriact, patrires_SN-patriact) ~ Natal, data = notunmarriedA, family = binomial)

#Do non-natal people name more matrilateral kin (if they have them)?
matriTBN <- glm(cbind(matriact, matrires_SN-matriact) ~ Natal, data = notunmarriedT, family = binomial)
matriABN <- glm(cbind(matriact, matrires_SN-matriact) ~ Natal, data = notunmarriedA, family = binomial)

#Do non-natal people name more affinal kin (if they have them)?
spfamTBN <- glm(cbind(spfamact, spfamres_SN-spfamact) ~ Natal, data = notunmarriedT, family = binomial)
spfamABN <- glm(cbind(spfamact, spfamres_SN-spfamact) ~ Natal, data = notunmarriedA, family = binomial)

spfamsubTBN <- glm(cbind(spfamactsub, spfamressub_SN-spfamactsub) ~ Natal, data = notunmarriedT, family = binomial)
spfamsubABN <- glm(cbind(spfamactsub, spfamressub_SN-spfamactsub) ~ Natal, data = notunmarriedA, family = binomial)

#print out results
library(texreg)
texreg(list(famresT, famresA, matriresT,matriresA,patriresT,patriresA,affresT,affresA), digits=3)
texreg(list(odegT, odegA, conTBN,conABN,matriTBN,matriABN,patriTBN,patriABN,spfamTBN,spfamABN,spfamsubTBN,spfamsubABN), digits=3)

## fitted probabilities
matrifp <- predict.glm(matriABN,newdata=data.frame(Natal=c("Natal","Non-Natal")),type="link",se.fit=TRUE)
predict.glm(matriABN,newdata=data.frame(Natal=c("Natal","Non-Natal")),type="response")
exp(matrifp$fit-(1.96*matrifp$se.fit))/(1+exp(matrifp$fit-(1.96*matrifp$se.fit)))
exp(matrifp$fit+(1.96*matrifp$se.fit))/(1+exp(matrifp$fit+(1.96*matrifp$se.fit)))

patrifp <- predict.glm(patriABN,newdata=data.frame(Natal=c("Natal","Non-Natal")),type="link",se.fit=TRUE)
predict.glm(patriABN,newdata=data.frame(Natal=c("Natal","Non-Natal")),type="response")
exp(patrifp$fit-(1.96*patrifp$se.fit))/(1+exp(patrifp$fit-(1.96*patrifp$se.fit)))
exp(patrifp$fit+(1.96*patrifp$se.fit))/(1+exp(patrifp$fit+(1.96*patrifp$se.fit)))



##PLOTTING

library(ggplot2)
library(grid)
library(gridExtra)
library(ggridges)

#organize vectors for gender for dot plots (brute-forcing colour for ggplot)
list <- table(notunmarriedT$relres_SN, notunmarriedT$Gender, notunmarriedT$Natal)
list <- c(as.vector(t(list[,,1])), as.vector(t(list[,,2]))) 
colT1 <- rep(rep(c("palevioletred3", "aquamarine4"), length(list)/2), list)

list <- table(notunmarriedA$relres_SN, notunmarriedA$Gender, notunmarriedA$Natal)
list <- c(as.vector(t(list[,,1])), as.vector(t(list[,,2]))) 
colA1 <- rep(rep(c("palevioletred3", "aquamarine4"), length(list)/2), list)

list <- table(notunmarriedT$outdegree, notunmarriedT$Gender, notunmarriedT$Natal)
list <- c(as.vector(t(list[,,1])), as.vector(t(list[,,2]))) 
colT2 <- rep(rep(c("palevioletred3", "aquamarine4"), length(list)/2), list)

list <- table(notunmarriedA$outdegree, notunmarriedA$Gender, notunmarriedA$Natal)
list <- c(as.vector(t(list[,,1])), as.vector(t(list[,,2]))) 
colA2 <- rep(rep(c("palevioletred3", "aquamarine4"), length(list)/2), list)

list <- table(notunmSpT$spfamact, notunmSpT$Gender, notunmSpT$Natal)
list <- c(as.vector(t(list[,,1])), as.vector(t(list[,,2]))) 
colT3 <- rep(rep(c("palevioletred3", "aquamarine4"), length(list)/2), list)

list <- table(notunmSpA$spfamact, notunmSpA$Gender, notunmSpA$Natal)
list <- c(as.vector(t(list[,,1])), as.vector(t(list[,,2]))) 
colA3 <- rep(rep(c("palevioletred3", "aquamarine4"), length(list)/2), list)

list <- table(notunmarriedT$matrires_SN, notunmarriedT$Gender, notunmarriedT$Natal)
list <- c(as.vector(t(list[,,1])), as.vector(t(list[,,2]))) 
colT4 <- rep(rep(c("palevioletred3", "aquamarine4"), length(list)/2), list)

list <- table(notunmarriedA$matrires_SN, notunmarriedA$Gender, notunmarriedA$Natal)
list <- c(as.vector(t(list[,,1])), as.vector(t(list[,,2]))) 
colA4 <- rep(rep(c("palevioletred3", "aquamarine4"), length(list)/2), list)

list <- table(notunmarriedT$patrires_SN, notunmarriedT$Gender, notunmarriedT$Natal)
list <- c(as.vector(t(list[,,1])), as.vector(t(list[,,2]))) 
colT5 <- rep(rep(c("palevioletred3", "aquamarine4"), length(list)/2), list)

list <- table(notunmarriedA$patrires_SN, notunmarriedA$Gender, notunmarriedA$Natal)
list <- c(as.vector(t(list[,,1])), as.vector(t(list[,,2]))) 
colA5 <- rep(rep(c("palevioletred3", "aquamarine4"), length(list)/2), list)

list <- table(notunmConT$relact, notunmConT$Gender, notunmConT$Natal)
list <- c(as.vector(t(list[,,1])), as.vector(t(list[,,2]))) 
colT6 <- rep(rep(c("palevioletred3", "aquamarine4"), length(list)/2), list)

list <- table(notunmConA$relact, notunmConA$Gender, notunmConA$Natal)
list <- c(as.vector(t(list[,,1])), as.vector(t(list[,,2]))) 
colA6 <- rep(rep(c("palevioletred3", "aquamarine4"), length(list)/2), list)


#dot plots of co-resident relatives for natal vs. non-natal
pdf("natalplots_relres.pdf", height=4, width=12, pointsize=8)
p1 <- ggplot(notunmarriedT, aes(x=Natal, y=relres_SN)) + ylim(0,25) +
  geom_violin(adjust=2, scale="width", color="grey60") +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize=0.5, stackratio = 1,
               binpositions="all", colour=colT1, fill=colT1) +
  theme_bw() + labs(x="Number of consanguineal kin",y="Tenpatti")
p7 <- ggplot(notunmarriedT, aes(x=Natal, y=matrires_SN)) + ylim(0,25) +
  geom_violin(adjust=2, scale="width", color="grey60") +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize=0.6, stackratio = 1,
               binpositions="all", colour=colT4, fill=colT4) +
  theme_bw() + labs(x="Number of matrilateral kin",y="")
p9 <- ggplot(notunmarriedT, aes(x=Natal, y=patrires_SN)) + ylim(0,25) +
  geom_violin(adjust=2, scale="width", color="grey60") +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize=0.6, stackratio = 1,
               binpositions="all", colour=colT5, fill=colT5) +
  theme_bw() + labs(x="Number of patrilateral kin",y="")
p2 <- ggplot(notunmarriedA, aes(x=Natal, y=relres_SN)) + ylim(0,25) +
  geom_violin(adjust=2, scale="width", color="grey60") +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize=0.25, stackratio = 1,
               binpositions="all", colour=colA1, fill=colA1) +
  theme_bw() + labs(x="",y="Alakapuram")
p8 <- ggplot(notunmarriedA, aes(x=Natal, y=matrires_SN)) + ylim(0,25) +
  geom_violin(adjust=2, scale="width", color="grey60") +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize=0.3, stackratio = 1,
               binpositions="all", colour=colA4, fill=colA4) +
  theme_bw() + labs(x="",y="")
p10 <- ggplot(notunmarriedA, aes(x=Natal, y=patrires_SN)) + ylim(0,25) +
  geom_violin(adjust=2, scale="width", color="grey60") +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize=0.3, stackratio = 1,
               binpositions="all", colour=colA5, fill=colA5) +
  theme_bw() + labs(x="",y="")
grid.arrange(p1,p7,p9,p2,p8,p10, nrow=2)
dev.off()

#dot plots of support ties for natal vs. non-natal
pdf("natalplots_support.pdf", height=4, width=12, pointsize=8)
p3 <- ggplot(notunmarriedT, aes(x=Natal, y=outdegree)) + ylim(0,23) +
  geom_violin(adjust=2, scale="width", color="grey60") + 
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize=0.3, stackratio = 1,
               binpositions="all", colour=colT2, fill=colT2) +
  theme_bw() + labs(x="Overall support received",y="Tenpatti")
p11 <- ggplot(notunmConT, aes(x=Natal, y=relact)) + ylim(0,8) +
  geom_violin(adjust=2, scale="width", color="grey60") +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize=0.4, stackratio = 1,
               binpositions="all", colour=colT6, fill=colT6) +
  theme_bw() + labs(x="Support from consanguineal kin",y="")
p5 <- ggplot(notunmSpT, aes(x=Natal, y=spfamact)) + ylim(0,8) +
  geom_violin(adjust=2, scale="width", color="grey60", bw=0.2) +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize=0.6, stackratio = 1,
               binpositions="all", colour=colT3, fill=colT3) +
  theme_bw() + labs(x="Support from affinal kin",y="")
p4 <- ggplot(notunmarriedA, aes(x=Natal, y=outdegree)) + ylim(0,23) +
  geom_violin(adjust=2, scale="width", color="grey60") +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize=0.3, stackratio = 1,
               binpositions="all", colour=colA2, fill=colA2) +
  theme_bw() + labs(x="",y="Alakapuram")
p12 <- ggplot(notunmConA, aes(x=Natal, y=relact)) + ylim(0,8) +
  geom_violin(adjust=2, scale="width", color="grey60") +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize=0.3, stackratio = 1, binpositions="all", colour=colA6, fill=colA6) +
  theme_bw() + labs(x="", y="")
p6 <- ggplot(notunmSpA, aes(x=Natal, y=spfamact)) + ylim(0,8) +
  geom_violin(adjust=2, scale="width", color="grey60") +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize=0.3, stackratio = 1, binpositions="all", colour=colA3, fill=colA3) +
  theme_bw() + labs(x="", y="")
grid.arrange(p3,p11,p5,p4,p12,p6, nrow=2)
dev.off()


#histograms of kin availability
vills$GN<-paste(vills$Gender,vills$Natal)

snrel<-gather(vills[,c("Gender", "CurrentUr_2017", "closerelres_SN", "matrires_SN", "patrires_SN", "relres_SN", "allaffres_SN", "Natal", "GN")], Type, Count, -Natal, -CurrentUr_2017,-Gender,-GN)
actrel<-gather(vills[,c("Gender", "CurrentUr_2017", "closerelact", "matriact", "patriact", "relact", "allaffact", "Natal", "GN")], Type, Count, -Natal, -CurrentUr_2017,-Gender,-GN)

pdf("res_histograms.pdf")
ggplot(snrel,aes(x=Count,fill=GN))+geom_histogram(position="stack",bins=50,binwidth=0.5) + facet_wrap(~Type+CurrentUr_2017,nrow=5)+theme_minimal()+scale_fill_manual(values=c("#e78ac3","#e7298a","#66c2a5","#1b9e77"))
dev.off()

pdf("sup_histograms.pdf")
ggplot(actrel,aes(x=Count,fill=GN))+geom_histogram(position="stack",bins=20,binwidth=0.5) + facet_wrap(~Type+CurrentUr_2017,nrow=5)+theme_minimal()+scale_fill_manual(values=c("#e78ac3","#e7298a","#66c2a5","#1b9e77"))
dev.off()

#odds-ratios with CI for regression results
models <- list(famresT, famresA, matriresT,matriresA,patriresT,patriresA,affresT,affresA,odegT, odegA, conTBN,conABN,matriTBN,matriABN,patriTBN,patriABN,spfamTBN, spfamABN)
dat <- data.frame(pe=0,lb=0,ub=0)
for (model in 1:length(models)){
  pe<-exp(summary(models[[model]])$coefficients[2,1])
  lb<-exp(summary(models[[model]])$coefficients[2,1] - 1.96*summary(models[[model]])$coefficients[2,2])
  ub<-exp(summary(models[[model]])$coefficients[2,1] + 1.96*summary(models[[model]])$coefficients[2,2])
  or <- c(pe,lb,ub)
  dat <-rbind(dat,or)
}
dat<-dat[-1,]
dat$model <- c("Res4 Consan", "Res4 Consan", "Res3 Matri","Res3 Matri","Res2 Patri","Res2 Patri","Res1 Aff","Res1 Aff","Act5 Sup", "Act5 Sup", "Act4 Consan","Act4 Consan","Act3 Matri","Act3 Matri","Act2 Patri","Act2 Patri","Act1 Aff", "Act1 Aff")
dat$TA<-rep(c("T","A"),9)
pdf("RegressionORs.pdf")
ggplot(dat,aes(y=pe,x=model,colour=TA)) + geom_hline(yintercept=1,color="black") + geom_pointrange(aes(ymin = lb, ymax = ub,colour=TA), position = position_dodge2(width = 0.9,preserve="single"),show.legend=TRUE,shape=18,size=0.6) + theme_bw() + coord_flip() + labs(y="Odds Ratio",x="",title="Regressions")+ scale_y_continuous(limits=c(0,10),breaks=seq(0,10,by=2)) + scale_colour_manual(values=c("#1f78b4", "#ff7f00"))
dev.off()


## Purely subjective observation: the percent of possible kinship ties activated roughly aligns with relatedness (with, for affines should be half of the `fictive' relatedness we use, following Hughes, so Affines 1.0 have a 0.5 shared stake in future generations)
table(Biol5A,Biol5A*Mat_snSupA1)/rowSums(table(Biol5A,Biol5A*Mat_snSupA1))
table(Biol25A,Biol25A*Mat_snSupA1)/rowSums(table(Biol25A,Biol25A*Mat_snSupA1))
table(Biol125A,Biol125A*Mat_snSupA1)/rowSums(table(Biol125A,Biol125A*Mat_snSupA1))

table(Aff1A,Aff1A*Mat_snSupA1)/rowSums(table(Aff1A,Aff1A*Mat_snSupA1))
table(Aff5A,Aff5A*Mat_snSupA1)/rowSums(table(Aff5A,Aff5A*Mat_snSupA1))
table(Aff25A,Aff25A*Mat_snSupA1)/rowSums(table(Aff25A,Aff25A*Mat_snSupA1))
table(Aff125A,Aff125A*Mat_snSupA1)/rowSums(table(Aff125A,Aff125A*Mat_snSupA1))

table(Biol5T,Biol5T*Mat_snSupT1)/rowSums(table(Biol5T,Biol5T*Mat_snSupT1))
table(Biol25T,Biol25T*Mat_snSupT1)/rowSums(table(Biol25T,Biol25T*Mat_snSupT1))
table(Biol125T,Biol125T*Mat_snSupT1)/rowSums(table(Biol125T,Biol125T*Mat_snSupT1))

table(Aff1T,Aff1T*Mat_snSupT1)/rowSums(table(Aff1T,Aff1T*Mat_snSupT1))
table(Aff5T,Aff5T*Mat_snSupT1)/rowSums(table(Aff5T,Aff5T*Mat_snSupT1))
table(Aff25T,Aff25T*Mat_snSupT1)/rowSums(table(Aff25T,Aff25T*Mat_snSupT1))
table(Aff125T,Aff125T*Mat_snSupT1)/rowSums(table(Aff125T,Aff125T*Mat_snSupT1))
