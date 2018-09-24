
source('./KinshipDataMerge.R')

#######################
## PLOTTING NETWORKS ##
#######################

castecol = Net_snSupA1 %v% "Caste"
#castecol = Net_snSupT1 %v% "Caste" # if want to plot Tenpatti, comment out previous line, and uncomment this
unique(castecol)
library(RColorBrewer)
colors <- brewer.pal(12,"Paired")
castecol[castecol == "Pallar"] = colors[1]
castecol[castecol == "RC Yaathavar"] = colors[7]
castecol[castecol == "RC Vellalar"] = colors[3]
castecol[castecol == "Hindu Yaathavar"] = colors[8]
castecol[castecol == "Agamudaiyaan"] = colors[5]
castecol[castecol == "Arundhathiyar"] = colors[2]
castecol[castecol == "Aasaari"] = colors[11]
castecol[castecol == "Jaanaan"] = "#666666"
castecol[castecol == "Naayakkar"] = "#666666"
castecol[castecol == "Pillamaar"] = "#666666"
castecol[castecol == "Kallar"] = "#666666"
castecol[castecol == "Maravar"] = colors[12]
castecol[castecol == "Kulaalar"] = colors[4]
castecol[castecol == "Hindu Vellalar"] = "#666666"
castecol[castecol == "Paraiyar"] = colors[9]
castecol[castecol == "Naadaar Panaiyaa"] = "#666666"
castecol[castecol == "Pandaaram"] = "#666666"
castecol[castecol == "Ilavaa"] = "#666666"

allkinT <- kinnets$closenetnoSpT + kinnets$matrinetT + kinnets$patrinetT + kinnets$spousenetT + kinnets$sfnetnospT + kinnets$matriaffnetT + kinnets$patriaffnetT
allkinA <- kinnets$closenetnoSpA + kinnets$matrinetA + kinnets$patrinetA+ kinnets$spousenetA + kinnets$sfnetnospA + kinnets$matriaffnetA + kinnets$patriaffnetA

Akinlay<-network.layout.fruchtermanreingold(allkinA,NULL)
Tkinlay<-network.layout.fruchtermanreingold(allkinT,NULL)
Asuplay<-network.layout.fruchtermanreingold(Net_snSupA1,NULL)
Tsuplay<-network.layout.fruchtermanreingold(Net_snSupT1,NULL)

## plotting the support network
plot(Net_snSupA1,vertex.col=castecol,edge.col=rgb(0,0,0, alpha=50, maxColorValue=255),edge.lwd=Net_snSupA1 %e% "SupSum",coord=Asuplay, arrowhead.cex=0.5,vertex.cex=0.75,usecurve=TRUE,edge.curve=0.0025)
legend("topleft",c("Acari","Akamutaiyar","Aruntatiyar","Hindu Yatavar","Kulalar","Maravar","Pallar","Rare","CSI Paraiyar","RC Vellalar","RC Yatavar"),cex=0.6,col=c(colors[c(11,5,2,8,4,12,1)],"#666666",colors[c(9,3,7)]),pch=19,title="Caste")

## plotting the kinship network
plot(kinnets$sfnetnospA,vertex.col=castecol,edge.col=rgb(253,174,97, alpha=100, maxColorValue=255),edge.lwd=0.5,coord=Akinlay,vertex.cex=0.75)
plot(kinnets$matriaffnetA,vertex.col=castecol,edge.col=rgb(253,174,97, alpha=100, maxColorValue=255),edge.lwd=0.5,coord=Akinlay,vertex.cex=0.75,new=FALSE)
plot(kinnets$patriaffnetA,vertex.col=castecol,edge.col=rgb(253,174,97, alpha=100, maxColorValue=255),edge.lwd=0.5,coord=Akinlay,vertex.cex=0.75,new=FALSE)
plot(kinnets$closenetnoSpA,vertex.col=castecol,edge.col=rgb(0,0,0, alpha=125, maxColorValue=255),edge.lwd=0.5, coord=Akinlay,vertex.cex=0.75,new=FALSE)
plot(kinnets$patrinetA,vertex.col=castecol,edge.col=rgb(44,123,182, alpha=175, maxColorValue=255),edge.lwd=0.5,coord=Akinlay,vertex.cex=0.75, new=FALSE)
plot(kinnets$matrinetA,vertex.col=castecol,edge.col=rgb(215,25,28, alpha=125, maxColorValue=255),edge.lwd=0.5,coord=Akinlay,vertex.cex=0.75, new=FALSE)
plot(kinnets$spousenetA,vertex.col=castecol,edge.col=rgb(253,174,97, alpha=175, maxColorValue=255),edge.lwd=0.5,coord=Akinlay,vertex.cex=0.75, new=FALSE)
legend("topleft",c("Relations", "CloseKin","Matri","Patri","Affines","Caste","Acari","Akamutaiyar","Aruntatiyar","Hindu Yatavar","Kulalar","Maravar","Pallar","Rare","CSI Paraiyar","RC Vellalar","RC Yatavar"),cex=0.6,col=c(NA,rgb(0,0,0, maxColorValue=255),rgb(215,25,28, maxColorValue=255),rgb(44,123,182, maxColorValue=255),rgb(253,174,97, maxColorValue=255),NA,colors[c(11,5,2,8,4,12,1)],"#666666",colors[c(9,3,7)]),lty=c(NA,1,1,1,1,rep(NA,11)),lwd=2,pch=c(rep(NA,6),rep(19,11)))


# Tenpatti: remember to change castecol as noted above!

plot(Net_snSupT1,vertex.col=castecol,edge.col=rgb(0,0,0, alpha=50, maxColorValue=255),edge.lwd=0.75,coord=Tsuplay, arrowhead.cex=0.5,vertex.cex=0.75,usecurve=TRUE,edge.curve=0.0025)
legend("topleft",c("Acari","Akamutaiyar","Aruntatiyar","Hindu Yatavar","Kulalar","Maravar","Pallar","Rare","CSI Paraiyar","RC Vellalar","RC Yatavar"),cex=0.6,col=c(colors[c(11,5,2,8,4,12,1)],"#666666",colors[c(9,3,7)]),pch=19,title="Caste")

plot(kinnets$sfnetnospT,vertex.col=castecol,edge.col=rgb(253,174,97, alpha=100, maxColorValue=255),edge.lwd=0.5,coord=Tkinlay,vertex.cex=0.75)
plot(kinnets$matriaffnetT,vertex.col=castecol,edge.col=rgb(253,174,97, alpha=100, maxColorValue=255),edge.lwd=0.5,coord=Tkinlay,vertex.cex=0.75,new=FALSE)
plot(kinnets$patriaffnetT,vertex.col=castecol,edge.col=rgb(253,174,97, alpha=100, maxColorValue=255),edge.lwd=0.5,coord=Tkinlay,vertex.cex=0.75,new=FALSE)
plot(kinnets$closenetnoSpT,vertex.col=castecol,edge.col=rgb(0,0,0, alpha=125, maxColorValue=255),edge.lwd=0.25, coord=Tkinlay,vertex.cex=0.75,new=FALSE)
plot(kinnets$patrinetT,vertex.col=castecol,edge.col=rgb(44,123,182, alpha=175, maxColorValue=255),edge.lwd=0.25,coord=Tkinlay,vertex.cex=0.75, new=FALSE)
plot(kinnets$matrinetT,vertex.col=castecol,edge.col=rgb(215,25,28, alpha=125, maxColorValue=255),edge.lwd=0.25,coord=Tkinlay,vertex.cex=0.75, new=FALSE)
plot(kinnets$spousenetT,vertex.col=castecol,edge.col=rgb(253,174,97, alpha=175, maxColorValue=255),edge.lwd=0.25,coord=Tkinlay,vertex.cex=0.75, new=FALSE)
legend("topleft",c("Relations", "CloseKin","Matri","Patri","Affines","Caste","Acari","Akamutaiyar","Aruntatiyar","Hindu Yatavar","Kulalar","Maravar","Pallar","Rare","CSI Paraiyar","RC Vellalar","RC Yatavar"),cex=0.6,col=c(NA,rgb(0,0,0, maxColorValue=255),rgb(215,25,28, maxColorValue=255),rgb(44,123,182, maxColorValue=255),rgb(253,174,97, maxColorValue=255),NA,colors[c(11,5,2,8,4,12,1)],"#666666",colors[c(9,3,7)]),lty=c(NA,1,1,1,1,rep(NA,11)),lwd=2,pch=c(rep(NA,6),rep(19,11)))

plot(Net_relatedT,vertex.col=castecol,edge.col=rgb(0,0,0, alpha=50, maxColorValue=255),edge.lwd=(Net_relatedT %e% "R")*20,coord=Tkinlay, arrowhead.cex=0.5,vertex.cex=0.75)
plot(kinnets$sfnetT,vertex.col=castecol,edge.col=rgb(253,174,97, alpha=50, maxColorValue=255),edge.lwd=(kinnets$sfnetT %e% "weight")*10,coord=Tkinlay, arrowhead.cex=0.5,vertex.cex=0.75,new=FALSE)



###############
#### ERGMs ####
###############

library(ergm)
library(texreg)

## for the GWESP, alpha = 0.4 is best in both villages

### ALAKAPURAM ##

# Base = controls
model_baseA <- ergm(Net_snSupA1 ~ edges + nodecov("Age_2017") + nodemix("Gender", base=c(1,4)) + nodematch("Caste") + nodefactor("Caste",base=c(1,3,5,6,10,12,13)) + absdiff("EduYears_2017") + nodeicov("SUWealth_2017Log") + nodeifactor("EverCommMember_2017") + edgecov(log(Aladistancemat+1)) + mutual + gwesp(0.4, fixed=TRUE), control = control.ergm(MCMC.burnin=50000,MCMC.samplesize=100000,MCMC.interval=10000),verbose=FALSE)
summary(model_baseA)
saveRDS(model_baseA,"model_baseA.rds")

## Model 1 = controls + natal/immigrant & marriage terms
model_base_natA <- ergm(Net_snSupA1 ~ edges + nodecov("Age_2017") + nodemix("Gender",base=c(1,4)) + nodematch("Caste") + nodefactor("Caste",base = c(1,3,5,6,10,12,13)) + absdiff("EduYears_2017") + nodeicov("SUWealth_2017Log") + nodeifactor("EverCommMember_2017") + edgecov(log(Aladistancemat+1)) + nodeifactor("nat_mar", base=c(1,3)) + nodeofactor("nat_mar",base =c(1,3)) + mutual + gwesp(0.4, fixed=TRUE), control = control.ergm(MCMC.burnin=50000,MCMC.samplesize=100000,MCMC.interval=10000),verbose=FALSE)
summary(model_base_natA)
saveRDS(model_base_natA,"model_base_natA.rds")

#Model 2 = controls + relatedness 0.5, 0.25, 0.125
model_relA <- ergm(Net_snSupA1 ~ edges + nodecov("Age_2017") + nodemix("Gender", base=c(1,4)) + nodematch("Caste") + nodefactor("Caste",base=c(1,3,5,6,10,12,13)) + absdiff("EduYears_2017") + nodeicov("SUWealth_2017Log") + nodeifactor("EverCommMember_2017") + edgecov(log(Aladistancemat+1)) + edgecov(Biol5A) + edgecov(Biol25A) + edgecov(Biol125A) + mutual + gwesp(0.4, fixed=TRUE), control = control.ergm(MCMC.burnin=50000,MCMC.samplesize=100000,MCMC.interval=10000), verbose=FALSE)
summary(model_relA)
saveRDS(model_relA,"model_relA.rds")

#Model 3 = controls + relatedness 0.5, 0.25, 0.125 + fictive 1, 0.5, 0.25, 0.125
model_relaffA <- ergm(Net_snSupA1 ~ edges + nodecov("Age_2017") + nodemix("Gender", base=c(1,4)) + nodematch("Caste") + nodefactor("Caste",base=c(1,3,5,6,10,12,13)) + absdiff("EduYears_2017") + nodeicov("SUWealth_2017Log") + nodeifactor("EverCommMember_2017") + edgecov(log(Aladistancemat+1)) + edgecov(Biol5A) + edgecov(Biol25A) + edgecov(Biol125A) + edgecov(Aff1A)+ edgecov(Aff5A) + edgecov(Aff25A) + edgecov(Aff125A) + mutual + gwesp(0.4, fixed=TRUE), control = control.ergm(MCMC.burnin=50000,MCMC.samplesize=100000,MCMC.interval=10000), verbose=FALSE)
summary(model_relaffA)
saveRDS(model_relaffA,"model_relaffA.rds")

#Model 4 = controls + relatedness 0.5 + fictive 1, 0.5, 0.25, 0.125 + matri 0.25, 0.125 + patri 0.25, 0.125
model_brokenA <- ergm(Net_snSupA1 ~ edges + nodecov("Age_2017") + nodemix("Gender", base=c(1,4)) + nodematch("Caste") + nodefactor("Caste",base=c(1,3,5,6,10,12,13)) + absdiff("EduYears_2017") + nodeicov("SUWealth_2017Log") + nodeifactor("EverCommMember_2017") + edgecov(log(Aladistancemat+1)) + edgecov(Biol5A) + edgecov(Mat25A) + edgecov(Pat25A) + edgecov(Mat125A) + edgecov(Pat125A) + edgecov(Aff1A)+ edgecov(Aff5A) + edgecov(Aff25A) + edgecov(Aff125A) + mutual + gwesp(0.4, fixed=TRUE), control = control.ergm(MCMC.burnin=50000,MCMC.samplesize=100000,MCMC.interval=10000),verbose=FALSE)
summary(model_brokenA) 
saveRDS(model_brokenA,"model_brokenA.rds")

#Model 5 = controls + relatedness 0.5 + fictive 1, 0.5, 0.25, 0.125 + matri 0.25, 0.125 + patri 0.25, 0.125 + sistosisfam + sistobrofam + brotosisfam + brotobrofam 
model_sibsA <- ergm(Net_snSupA1 ~ edges + nodecov("Age_2017") + nodemix("Gender", base=c(1,4)) + nodematch("Caste") + nodefactor("Caste",base=c(1,3,5,6,10,12,13)) + absdiff("EduYears_2017") + nodeicov("SUWealth_2017Log") + nodeifactor("EverCommMember_2017") + edgecov(log(Aladistancemat+1)) + edgecov(Biol5A) + edgecov(Biol25A) + edgecov(Biol125A) + edgecov(Aff1A)+ edgecov(Aff5A) + edgecov(Aff25A) + edgecov(Aff125A) + edgecov(Mat_sistosisfA) + edgecov(Mat_sistobrofA) + edgecov(Mat_brotosisfA) + edgecov(Mat_brotobrofA) + mutual + gwesp(0.4, fixed=TRUE), control = control.ergm(MCMC.burnin=50000,MCMC.samplesize=100000,MCMC.interval=10000),verbose=FALSE)
summary(model_sibsA)
saveRDS(model_sibsA,"model_sibsA.rds")

### diagnostics

models <- c("model_baseA","model_base_natA", "model_relA", "model_relaffA", "model_brokenA","model_sibsA")
for (modelname in models) {
  sink(paste("model_diagnostics/diags_", modelname, ".txt", sep=""), append=TRUE)
  model <- get(modelname)
  pdf(paste("model_diagnostics/mcmc_", modelname, ".pdf", sep=""))
  mcmc.diagnostics(model)
  dev.off()
  modelgof <- gof(model, GOF = ~odegree + idegree + espartners + dspartners, burnin = 5000, interval = 5000)
  print(modelgof)
  pdf(paste("model_diagnostics/gof_", modelname, ".pdf", sep=""))
  plot(modelgof)
  dev.off()
  sink()
}


## TENPATTI ##

# Base = controls
model_baseT <- ergm(Net_snSupT1 ~ edges + nodecov("Age_2017") + nodemix("Gender", base=c(1,4)) + nodematch("Caste") + nodefactor("Caste",base=c(4,6,7)) + absdiff("EduYears_2017") + nodeicov("SUWealth_2017Log") + nodeifactor("EverCommMember_2017") + edgecov(log(Tendistancemat+1)) + mutual + gwesp(0.4, fixed=TRUE), control = control.ergm(MCMC.burnin=50000,MCMC.samplesize=100000,MCMC.interval=10000),verbose=FALSE)
summary(model_baseT)
saveRDS(model_baseT,"model_baseT.rds")

## Model 1 = controls + natal/immigrant & marriage terms
model_base_natT <- ergm(Net_snSupT1 ~ edges + nodecov("Age_2017") + nodemix("Gender",base=c(1,4)) + nodematch("Caste") + nodefactor("Caste",base = c(4,6,7)) + absdiff("EduYears_2017") + nodeicov("SUWealth_2017Log") + nodeifactor("EverCommMember_2017") + edgecov(log(Tendistancemat+1)) + nodeifactor("nat_mar", base=2) + nodeofactor("nat_mar",base =2) + mutual + gwesp(0.4, fixed=TRUE), control = control.ergm(MCMC.burnin=50000,MCMC.samplesize=100000,MCMC.interval=10000),verbose=FALSE)
summary(model_base_natT)
saveRDS(model_base_natT,"model_base_natT.rds")

#Model 2 = controls + relatedness 0.5, 0.25, 0.125
model_relT <- ergm(Net_snSupT1 ~ edges + nodecov("Age_2017") + nodemix("Gender", base=c(1,4)) + nodematch("Caste") + nodefactor("Caste",base=c(4,6,7)) + absdiff("EduYears_2017") + nodeicov("SUWealth_2017Log") + nodeifactor("EverCommMember_2017") + edgecov(log(Tendistancemat+1)) + edgecov(Biol5T) + edgecov(Biol25T) + edgecov(Biol125T) + mutual + gwesp(0.4, fixed=TRUE), control = control.ergm(MCMC.burnin=50000,MCMC.samplesize=100000,MCMC.interval=10000),verbose=FALSE)
summary(model_relT)
saveRDS(model_relT,"model_relT.rds")

#Model 3 = controls + relatedness 0.5, 0.25, 0.125 + fictive 1, 0.5, 0.25, 0.125
model_relaffT <- ergm(Net_snSupT1 ~ edges + nodecov("Age_2017") + nodemix("Gender", base=c(1,4)) + nodematch("Caste") + nodefactor("Caste",base=c(4,6,7)) + absdiff("EduYears_2017") + nodeicov("SUWealth_2017Log") + nodeifactor("EverCommMember_2017") + edgecov(log(Tendistancemat+1)) + edgecov(Biol5T) + edgecov(Biol25T) + edgecov(Biol125T) + edgecov(Aff1T)+ edgecov(Aff5T) + edgecov(Aff25T) + edgecov(Aff125T) + mutual + gwesp(0.4, fixed=TRUE), control = control.ergm(MCMC.burnin=50000,MCMC.samplesize=100000,MCMC.interval=10000),verbose=FALSE)
summary(model_relaffT)
saveRDS(model_relaffT,"model_relaffT.rds")

#Model 4 = controls + relatedness 0.5 + fictive 1, 0.5, 0.25, 0.125 + matri 0.25, 0.125 + patri 0.25, 0.125
model_brokenT <- ergm(Net_snSupT1 ~ edges + nodecov("Age_2017") + nodemix("Gender", base=c(1,4)) + nodematch("Caste") + nodefactor("Caste",base=c(4,6,7)) + absdiff("EduYears_2017") + nodeicov("SUWealth_2017Log") + nodeifactor("EverCommMember_2017") + edgecov(log(Tendistancemat+1)) + edgecov(Biol5T) + edgecov(Mat25T) + edgecov(Pat25T) + edgecov(Mat125T) + edgecov(Pat125T) + edgecov(Aff1T)+ edgecov(Aff5T) + edgecov(Aff25T) + edgecov(Aff125T) + mutual + gwesp(0.4, fixed=TRUE), control = control.ergm(MCMC.burnin=50000,MCMC.samplesize=100000,MCMC.interval=10000),verbose=FALSE)
summary(model_brokenT)
saveRDS(model_brokenT,"model_brokenT.rds")

#Model 5 = controls + relatedness 0.5 + fictive 1, 0.5, 0.25, 0.125 + matri 0.25, 0.125 + patri 0.25, 0.125 + sistosisfam + sistobrofam + brotosisfam + brotobrofam 
model_sibsT <- ergm(Net_snSupT1 ~ edges + nodecov("Age_2017") + nodemix("Gender", base=c(1,4)) + nodematch("Caste") + nodefactor("Caste",base=c(4,6,7)) + absdiff("EduYears_2017") + nodeicov("SUWealth_2017Log") + nodeifactor("EverCommMember_2017") + edgecov(log(Tendistancemat+1)) + edgecov(Biol5T) + edgecov(Biol25T) + edgecov(Biol125T) + edgecov(Aff1T)+ edgecov(Aff5T) + edgecov(Aff25T) + edgecov(Aff125T) + edgecov(Mat_sistosisfT) + edgecov(Mat_sistobrofT) + edgecov(Mat_brotosisfT) + edgecov(Mat_brotobrofT) + mutual + gwesp(0.4, fixed=TRUE), control = control.ergm(MCMC.burnin=50000,MCMC.samplesize=100000,MCMC.interval=10000),verbose=FALSE)
summary(model_sibsT)
saveRDS(model_sibsT,"model_sibsT.rds")



## diagnostics

models <- c("model_baseT", "model_base_natT", "model_relT","model_relaffT", "model_brokenT","model_sibsT")
for (modelname in models) {
  sink(paste("model_diagnostics/diags_", modelname, ".txt", sep=""), append=TRUE)
  model <- get(modelname)
  pdf(paste("model_diagnostics/mcmc_", modelname, ".pdf", sep=""))
  mcmc.diagnostics(model)
  dev.off()
  modelgof <- gof(model, GOF = ~odegree + idegree + espartners + dspartners, burnin = 5000, interval = 5000)
  print(modelgof)
  pdf(paste("model_diagnostics/gof_", modelname, ".pdf", sep=""))
  plot(modelgof)
  dev.off()
  sink()
}



######################
## PLOTTING RESULTS ##
######################

TenMods <- c("model_sibsT","model_brokenT","model_relaffT","model_relT","model_base_natT")
AlaMods <- c("model_sibsA","model_brokenA","model_relaffA","model_relA","model_base_natA")
coefdf <- data.frame()
for (villname in c("TenMods", "AlaMods")) {
  village <- get(villname)
  for (modelname in village){
    model <- get(modelname)
    tempdf <- data.frame(names(model$coef), summary(model)$coefs[1], summary(model)$coefs[2], rep(modelname,length(model$coef)), rep(villname, length(model$coef)))
    coefdf <- rbind(coefdf, tempdf)
  }
}
colnames(coefdf) <- c("term", "coef", "se", "model", "village")
rownames(coefdf) <- 1:length(coefdf$term)

coefdf$lower95 <- coefdf$coef - (1.96 * coefdf$se)
coefdf$upper95 <- coefdf$coef + (1.96 * coefdf$se)
coefdf$or <- exp(coefdf$coef)
coefdf$orlower <- exp(coefdf$coef - (1.96 * coefdf$se))
coefdf$orupper <- exp(coefdf$coef + (1.96 * coefdf$se))


termsT<- unique(c(names(model_base_natT$coef),names(model_relT$coef),names(model_brokenT$coef),names(model_sibsT$coef)))
termsT <- termsT[c(16:19,24,23,22,32,31,30,29,28,27,26,25,33:36)]


termsA<- unique(c(names(model_base_natA$coef),names(model_relA$coef),names(model_brokenA$coef),names(model_sibsA$coef)))
termsA <- termsA[c(16:19,24,23,22,32,31,30,29,28,27,26,25,33:36)]
termsAT <- substring(as.character(termsA),1,nchar(as.character(termsA))-1)


coefdf<-subset(coefdf,coefdf$term %in% c(termsT,termsA))

coefdfT<-subset(coefdf,coefdf$village=="TenMods")
coefdfA<-subset(coefdf,coefdf$village=="AlaMods")

coefdf$term <- substring(as.character(coefdf[,"term"]),1,nchar(as.character(coefdf[,"term"]))-1)

coefdf_sibs <- subset(coefdf,coefdf$model=="model_sibsT" | coefdf$model=="model_sibsA")
coefdf_broken <- subset(coefdf,coefdf$model=="model_brokenT" | coefdf$model=="model_brokenA")
coefdf_relaff <- subset(coefdf,coefdf$model=="model_relaffT" | coefdf$model=="model_relaffA")
coefdf_rel <- subset(coefdf,coefdf$model=="model_relT" | coefdf$model=="model_relA")
coefdf_base_nat <- subset(coefdf,coefdf$model=="model_base_natT" | coefdf$model=="model_base_natA")


library(gridExtra)
library(ggplot2)

A_or <- ggplot(coefdfA,aes(y=or,x=factor(term,levels=rev(termsA)),group=factor(model),colour=factor(model))) + geom_hline(yintercept=1,color="black") + geom_pointrange(aes(ymin = orlower, ymax = orupper,colour=factor(model)), position = position_dodge2(width = 0.9,preserve="single"),show.legend=TRUE,shape=18,size=0.6) + theme_bw() + coord_flip() + labs(y="Odds Ratio",x="",title="Alakapuram") + scale_colour_brewer(palette="Set2",name="Models",guide = guide_legend(reverse=TRUE),labels = c("Model 5","Model 4","Model 3","Model 2","Model 1")) + scale_y_continuous(limits=c(0,12),breaks=seq(0,12,by=2)) + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())

T_or <- ggplot(coefdfT,aes(y=or,x=factor(term,levels=rev(termsT)),group=factor(model),colour=factor(model))) + geom_hline(yintercept=1,color="black") + geom_pointrange(aes(ymin = orlower, ymax = orupper,colour=factor(model)), position = position_dodge2(width = 0.9,preserve="single"),show.legend = FALSE,shape=18,size=0.6) + theme_bw() + coord_flip() + labs(y="Odds Ratio",x="Terms",title="Tenpatti") + scale_x_discrete(labels=rev(c("Marr-non-nat giving", "Marr-nat giving", "Marr-non-nat asking", "Marr-nat asking", "Consan 0.125", "Consan 0.25", "Consan 0.5", "Aff 0.125", "Aff 0.25", "Aff 0.5", "Aff 1.0", "Patri 0.125","Matri 0.125","Patri 0.25","Matri 0.25", "Sister to sis & fam", "Sister to bro & fam", "Brother to sis & fam", "Brother to bro & fam"))) + scale_colour_brewer(palette="Set2") + scale_y_continuous(limits=c(0,12),breaks=seq(0,12,by=2))
grid.arrange(T_or, A_or, ncol = 2)

## FITTED TIE PROBABILITIES
## The reference is two people of the Hindu Yaathavar caste, with the same gender, a combined age of 70, the same education, the average distance (5 with the transformation; equiv to roughly 150 m) between households, average wealth (3.6 with transformation) for the incoming tie, one shared partner.

estoprob <- function(b) {
  exp(b)/(1+exp(b))
}


## Example in text: Non-natal residents having a lower probability of asking for support from natal residents than natal residents themselves, in both villages. However, the magnitude of this difference is very slight (in both villages, a difference of roughly 0.5% in the probability of a tie). 
## fitted probs for Alakapuram
estoprob(sum(as.numeric(c(1,70,0,0,1,0,2,0,0,0,0,0,3.6,0,5,0,1,1,0,0,1))*model_base_natA$coef))
estoprob(sum(as.numeric(c(1,70,0,0,1,0,2,0,0,0,0,0,3.6,0,5,0,1,0,1,0,1))*model_base_natA$coef))

## for Tenpatti
estoprob(sum(as.numeric(c(1,70,0,0,1,0,0,0,2,0,0,0,3.6,0,5,0,1,1,0,0,1))*model_base_natT$coef))
estoprob(sum(as.numeric(c(1,70,0,0,1,0,0,0,2,0,0,0,3.6,0,5,0,1,0,1,0,1))*model_base_natT$coef))


