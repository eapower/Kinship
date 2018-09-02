
## Code to calculate the household wealth of each sharing unit. 

su<-read.csv("TN_SharingUnit.csv",as.is=TRUE,header=TRUE)
su<-subset(su,su$SharingUnitID!="")


su$PropertyValue <- (su$TableFanGovt_2017*500) + (su$TableFanOwn_2017*1500) + (su$CeilingFan_2017*1800) + (su$Cot_2017*1500) + (su$BedFrameWood_2017*10000) + (su$BedFrameMetal_2017*3000) + (su$Mattress_2017*900) + (su$Bureau_2017*7000) + (su$Sofa_2017*4500) + (su$Table_2017*1000) + (su$ChairPlastic_2017*500) + (su$ChairMetal_2017*700) + (su$TVGovt_2017*750) + (su$TVOwn_2017*11000) + (su$LaptopGovt_2017*7500) + (su$LaptopOwn_2017*17000) + (su$Desktop_2017*17000) + (su$CellBrick_2017*1500) + (su$CellTouch_2017*9000) + (su$MixiGovt_2017*650) + (su$MixiOwn_2017*3500) + (su$GrinderGovt_2017*500) + (su$GrinderOwn_2017*4000) + (su$StoveGovt_2017*1500) + (su$StoveOwn_2017*3000) + (su$Fridge_2017*10000) + (su$SewingM_2017*6000) + (su$WashingM_2017*18500) + (su$AirCon_2017*20000) + (su$UPS_2017*17500)

su$VehicleValue <- (su$CycleGovt_2017*1000) + (su$CycleOwn_2017*4000) + (su$Scooter_2017*65000) + (su$TVS_2017*40000) + (su$Motorcycle_2017*65000) + (su$Car_2017*500000) + (su$Van_2017*300000) + (su$Auto_2017*125000) + (su$Tractor_2017*400000) + (su$Cart_2017*15000)

su$AnimalValue <- (su$Cow_2017*22500) + (su$Bull_2017*25000) + (su$Goat_2017*5000) + (su$Chicken_2017*400) + (su$Duck_2017*400) + (su$Dove_2017*125)

su$LandValue <- (su$AgriAcres_2017*550000) + (su$ForestAcres_2017*300000) + (su$Coconut_2017*400) + (su$Banana_2017*250) + (su$Pump_2017*17000)

su$HouseValue <- su$Rooms_2017*I(su$Floor_2017=="Sand")*1000 + su$Rooms_2017*I(su$Floor_2017=="Cement")*20000 + su$Rooms_2017*I(su$Floor_2017=="Tile")*25000 + su$Rooms_2017*I(su$Floor_2017=="Marble")*35000 + su$Rooms_2017*I(su$Roof_2017=="Thatch")*5000+ su$Rooms_2017*I(su$Roof_2017=="Asbestos")*8000 + su$Rooms_2017*I(su$Roof_2017=="Cement")*25000 + su$Rooms_2017*I(su$Roof_2017=="Tile")*20000 + (su$Electricity_2017*2000) + (su$Toilet_2017*30000) + (su$BoreWell_2017*17000) + (su$Tank_2017*2000) 

# renters should not be have the value of the house included in their wealth
su$HouseValue[su$HouseExpense_2017 == "Rent"] <- 0

# an overall base plus a per-person base is also added. This assumes that even the poorest households have:
# 5 Steel containers, 5 small plastic containers, 2 big plastic containers, dosai pan, 3 pans, 3 pots, 1 small pot (for milk), 1 idli steamer, 2 ladels, 2 flat serving spoons, 4 lids, 1 bucket, and 1 pourer (collectively valued as 3500 Rs)
# Then, for each person in the household, there should be: 1 plate, 1 bowl (kinnam), 1 tumbler, 1 tiffin box, and 2 water jugs (collectively valued at 400 Rs per person)
su$BaseValue <- 3500 + 400 * su$SUCountAllRes_2017

su$CashValue <- su$PropertyValue + su$VehicleValue + su$AnimalValue + su$LandValue + su$HouseValue + su$BaseValue


su2<-subset(su,duplicated(su$SharingUnitID)==FALSE)

## append this information to the dataframe with individual attributes
indiv<-merge(indiv,su2[,c(1,18,19,148)],by.x="SharingUnitID_2017",by.y="SharingUnitID",all.x=TRUE,sort=FALSE)






## count number of residents in each sharing unit, family, etc.
## note -- the outcome of this code has now been incorporated into the main file.
indiv <- read.csv("TN_Indiv_Full.csv", header=TRUE, as.is=TRUE)


TenRes<-subset(indiv,indiv$CurrentUr_2017 == "Tenpatti")
AlaRes<-subset(indiv,indiv$CurrentUr_2017 == "Azhagapuram")

TenRes13<-subset(indiv,indiv$CurrentUr_2013 == "Tenpatti")
TenRes13<-subset(TenRes13,TenRes13$Residing_2013 == "Yes")
AlaRes13<-subset(indiv,indiv$CurrentUr_2013 == "Azhagapuram")
AlaRes13<-subset(AlaRes13,AlaRes13$Residing_2013 == "Yes")

TenResAd<-subset(TenRes,TenRes$Age_2017 > 17)
AlaResAd<-subset(AlaRes,AlaRes$Age_2017 > 17)

TenResAd13<-subset(TenRes13,TenRes13$Age_2013 > 17)
AlaResAd13<-subset(AlaRes13,AlaRes13$Age_2013 > 17)


TSUcount<-as.data.frame(table(TenRes$SharingUnitID_2017))
ASUcount<-as.data.frame(table(AlaRes$SharingUnitID_2017))

TSUcountAd<-as.data.frame(table(TenResAd$SharingUnitID_2017))
ASUcountAd<-as.data.frame(table(AlaResAd$SharingUnitID_2017))

TFamcount<-as.data.frame(table(TenRes$FamID_2017))
AFamcount<-as.data.frame(table(AlaRes$FamID_2017))

TFamcountAd<-as.data.frame(table(TenResAd$FamID_2017))
AFamcountAd<-as.data.frame(table(AlaResAd$FamID_2017))


TFamcount13<-as.data.frame(table(TenRes13$FamID_2013))
TFamcount13<-TFamcount13[-1,]
AFamcount13<-as.data.frame(table(AlaRes13$FamID_2013))

TFamcountAd13<-as.data.frame(table(TenResAd13$FamID_2013))
TFamcountAd13<-TFamcountAd13[-1,]
AFamcountAd13<-as.data.frame(table(AlaResAd13$FamID_2013))

Famcount<-rbind(TFamcount,AFamcount)
FamcountAd<-rbind(TFamcountAd,AFamcountAd)
Famcount13<-rbind(TFamcount13,AFamcount13)
FamcountAd13<-rbind(TFamcountAd13,AFamcountAd13)
SUcount<-rbind(TSUcount,ASUcount)
SUcountAd<-rbind(TSUcountAd,ASUcountAd)

colnames(Famcount) <- c("FamID","FamCountAllRes_2017")
colnames(FamcountAd) <- c("FamID","FamCountAdres_2017")
colnames(Famcount13) <- c("FamID","FamCountAllRes_2013")
colnames(FamcountAd13) <- c("FamID","FamCountAdRes_2013")
colnames(SUcount) <- c("SharingUnitID","SUCountAllRes_2017")
colnames(SUcountAd) <- c("SharingUnitID","SuCountAdRes_2017")


su1<-merge(su,SUcount,by="SharingUnitID",all.x=TRUE,sort=FALSE)
su1<-merge(su1,SUcountAd,by="SharingUnitID",all.x=TRUE,sort=FALSE)
su1<-merge(su1,Famcount,by="FamID",all.x=TRUE,sort=FALSE)
su1<-merge(su1,FamcountAd,by="FamID",all.x=TRUE,sort=FALSE)
su1<-merge(su1,Famcount13,by="FamID",all.x=TRUE,sort=FALSE)
su1<-merge(su1,FamcountAd13,by="FamID",all.x=TRUE,sort=FALSE)

