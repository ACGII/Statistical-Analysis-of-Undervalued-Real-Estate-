
for(i in 1:15){
     print(sprintf("Neighborhood(%s) price($%7.0f) Predicted price($%7.0f) difference($%7.0f)",
          as.character(UU[i,'Neighborhood']),UU[i,'price'],UU[i,'Predict'],UU[i,'Diff']))
}


UU<-matrix(0, nrow = 350, ncol = 5,byrow=TRUE,
dimnames = list(1:350,  c("index","Neighborhood", "price","Predict","Diff")))
UU<-data.frame(UU)
Uun<-un[1,]
k=1
for(i in 1:nrow(un)){
     if(un[i,'Predict']>=un[i,'Lim']){
          #print(i)
          UU[k,'Neighborhood']<-as.character(un[i,'Neighborhood'])
          UU[k,'price']<-un[i,'price'] 
          UU[k,'Predict']<-un[i,'Predict']
          UU[k,'Diff']<-un[i,'Predict'] - un[i,'price']
          UU[k,'index']<-i
          Uun[k,]<-un[i,]
          k<-k+1
     }
}
UU<-UU[1:k,]
UU<-UU[order(-UU[,5]),]

OV<-matrix(0, nrow = 350, ncol = 5,byrow=TRUE,
dimnames = list(1:350,  c("index","Neighborhood", "price","Predict","Diff")))
OV<-data.frame(OV)
Oov<-ov[1,]
k=1
for(i in 1:nrow(ov)){
     if(ov[i,'Predict']<=ov[i,'Lim']){
          #print(i)
          OV[k,'Neighborhood']<-ov[i,'Neighborhood']
          OV[k,'price']<-ov[i,'price'] 
          OV[k,'Predict']<-ov[i,'Predict']
          OV[k,'Diff']<-ov[i,'price'] - ov[i,'Predict']
          OV[k,'index']<-i
          Oov[k,]<-ov[i,]
          k<-k+1
     }
}
OV<-OV[1:k,]
OV<-OV[order(-OV[,5]),]

LSet<-select(ames_train,price,Overall.Qual,Lot.Area,
Year.Built,Sale.Condition,area,MS.Zoning,Year.Remod.Add,Land.Slope,
Exter.Qual,Lot.Shape,Land.Contour,Lot.Config,Street,
Bsmt.Qual,Bsmt.Cond,Bsmt.Exposure,BsmtFin.Type.1,Bldg.Type,
BsmtFin.SF.1,BsmtFin.Type.2,BsmtFin.SF.2,Bsmt.Unf.SF,
Total.Bsmt.SF,Heating.QC,Central.Air,House.Style,
Electrical,X1st.Flr.SF,X2nd.Flr.SF,
Bsmt.Full.Bath,Bsmt.Half.Bath,Full.Bath,Half.Bath,
Bedroom.AbvGr,Kitchen.AbvGr,TotRms.AbvGrd,
Functional,Fireplaces,
Garage.Yr.Blt,Garage.Finish,Garage.Cars,Garage.Area,
Paved.Drive,Wood.Deck.SF,
Open.Porch.SF,Enclosed.Porch,X3Ssn.Porch,Screen.Porch,
Fence,Misc.Val,Mo.Sold,Yr.Sold,Sale.Type)

LSet<-data.frame(LSet)
LSet$logarea<-log(LSet$area+1)
LSet$logLot.Area<-log(LSet$Lot.Area+1)
LSet$logGarage.Area<-log(LSet$Garage.Area+1)
LSet$logX2nd.Flr.SF<-log(LSet$X2nd.Flr.SF+1)
LSet$logBsmtFin.SF.1<-log(LSet$BsmtFin.SF.1+1)
LSet$logBsmtFin.SF.2<-log(LSet$BsmtFin.SF.2+1)
LSet$logBsmt.Unf.SF<-log(LSet$Bsmt.Unf.SF+1)
LSet$logTotal.Bsmt.SF<-log(LSet$Total.Bsmt.SF+1)
LSet$logWood.Deck.SF<-log(LSet$Wood.Deck.SF+1)
LSet$logOpen.Porch.SF<-log(LSet$Open.Porch.SF+1)
LSet$logX1st.Flr.SF<-log(LSet$X1st.Flr.SF+1)

 LSet<-LSet[-c(3,6,20,22,23,24,29,30,43,45,46)]


amSet3<-select(LSet,1,2,44,3,45,48,4,19,5,6,31,54,16,39,30,34,18,29,14,28,13)




vLSet<-select(ames_validation,price,Overall.Qual,Lot.Area,
Year.Built,Sale.Condition,area,MS.Zoning,Year.Remod.Add,Land.Slope,
Exter.Qual,Lot.Shape,Land.Contour,Lot.Config,Street,
Bsmt.Qual,Bsmt.Cond,Bsmt.Exposure,BsmtFin.Type.1,Bldg.Type,
BsmtFin.SF.1,BsmtFin.Type.2,BsmtFin.SF.2,Bsmt.Unf.SF,
Total.Bsmt.SF,Heating.QC,Central.Air,House.Style,
Electrical,X1st.Flr.SF,X2nd.Flr.SF,
Bsmt.Full.Bath,Bsmt.Half.Bath,Full.Bath,Half.Bath,
Bedroom.AbvGr,Kitchen.AbvGr,TotRms.AbvGrd,
Functional,Fireplaces,
Garage.Yr.Blt,Garage.Finish,Garage.Cars,Garage.Area,
Paved.Drive,Wood.Deck.SF,
Open.Porch.SF,Enclosed.Porch,X3Ssn.Porch,Screen.Porch,
Fence,Misc.Val,Mo.Sold,Yr.Sold,Sale.Type)

vLSet<-data.frame(vLSet)
vLSet$logarea<-log(vLSet$area+1)
vLSet$logLot.Area<-log(vLSet$Lot.Area+1)
vLSet$logGarage.Area<-log(vLSet$Garage.Area+1)
vLSet$logX2nd.Flr.SF<-log(vLSet$X2nd.Flr.SF+1)
vLSet$logBsmtFin.SF.1<-log(vLSet$BsmtFin.SF.1+1)
vLSet$logBsmtFin.SF.2<-log(vLSet$BsmtFin.SF.2+1)
vLSet$logBsmt.Unf.SF<-log(vLSet$Bsmt.Unf.SF+1)
vLSet$logTotal.Bsmt.SF<-log(vLSet$Total.Bsmt.SF+1)
vLSet$logWood.Deck.SF<-log(vLSet$Wood.Deck.SF+1)
vLSet$logOpen.Porch.SF<-log(vLSet$Open.Porch.SF+1)
vLSet$logX1st.Flr.SF<-log(vLSet$X1st.Flr.SF+1)

 vLSet<-vLSet[-c(3,6,20,22,23,24,29,30,43,45,46)]


avSet3<-select(vLSet,1,2,44,3,45,48,4,19,5,6,31,54,16,39,30,34,18,29,14,28,13)


##############################OLD ###################################################################################


vLSet<-dplyr::select(ames_validation,price,Overall.Qual,Lot.Area,
Year.Built,Sale.Condition,area,MS.Zoning,Year.Remod.Add,Land.Slope,
Exter.Qual,Lot.Shape,Land.Contour,Lot.Config,Street,
Bsmt.Qual,Bsmt.Cond,Bsmt.Exposure,BsmtFin.Type.1,Bldg.Type,
BsmtFin.SF.1,BsmtFin.Type.2,BsmtFin.SF.2,Bsmt.Unf.SF,
Total.Bsmt.SF,Heating.QC,Central.Air,House.Style,
Electrical,X1st.Flr.SF,X2nd.Flr.SF,
Bsmt.Full.Bath,Bsmt.Half.Bath,Full.Bath,Half.Bath,
Bedroom.AbvGr,Kitchen.AbvGr,TotRms.AbvGrd,
Functional,Fireplaces,
Garage.Yr.Blt,Garage.Finish,Garage.Cars,Garage.Area,
Paved.Drive,Wood.Deck.SF,
Open.Porch.SF,Enclosed.Porch,X3Ssn.Porch,Screen.Porch,
Fence,Misc.Val,Mo.Sold,Yr.Sold,Sale.Type,Neighborhood,Overall.Cond,Garage.Type,Kitchen.Qual,Heating)

vLSet<-data.frame(vLSet)

vLSet$logarea<-log(vLSet$area+1)
vLSet$logLot.Area<-log(vLSet$Lot.Area+1)
vLSet$logGarage.Area<-log(vLSet$Garage.Area+1)
vLSet$logX2nd.Flr.SF<-log(vLSet$X2nd.Flr.SF+1)
vLSet$logBsmtFin.SF.1<-log(vLSet$BsmtFin.SF.1+1)
vLSet$logBsmtFin.SF.2<-log(vLSet$BsmtFin.SF.2+1)
vLSet$logBsmt.Unf.SF<-log(vLSet$Bsmt.Unf.SF+1)
vLSet$logTotal.Bsmt.SF<-log(vLSet$Total.Bsmt.SF+1)
vLSet$logWood.Deck.SF<-log(vLSet$Wood.Deck.SF+1)
vLSet$logOpen.Porch.SF<-log(vLSet$Open.Porch.SF+1)
vLSet$logX1st.Flr.SF<-log(vLSet$X1st.Flr.SF+1)



amLSet<-ames_train
amLSet<-data.frame(amLSet)
amLSet$logarea<-log(amLSet$area+1)
amLSet$logLot.Area<-log(amLSet$Lot.Area+1)
amLSet$logX2nd.Flr.SF<-log(amLSet$X2nd.Flr.SF+1)
amLSet$logBsmtFin.SF.1<-log(amLSet$BsmtFin.SF.1+1)

amSet2<-dplyr::select(amLSet,price,Overall.Qual,Neighborhood,logarea,       
Overall.Cond,Year.Built,logLot.Area,Bsmt.Full.Bath,
Garage.Type,Sale.Condition,logX2nd.Flr.SF,Bldg.Type,     
Heating.QC,logBsmtFin.SF.1,Garage.Cars,MS.Zoning,   
Kitchen.Qual,Heating,Central.Air,   
Fireplaces)

 
avLSet2<-dplyr::select(vLSet,price,Overall.Qual,Neighborhood,logarea,       
Overall.Cond,Year.Built,logLot.Area,Bsmt.Full.Bath,
Garage.Type,Sale.Condition,logX2nd.Flr.SF,Bldg.Type,     
Heating.QC,logBsmtFin.SF.1,Garage.Cars,MS.Zoning,   
Kitchen.Qual,Heating,Central.Air,   
Fireplaces)

vLSet<-vLSet[-c(3,6,20,22,23,24,29,30,43,45,46)]
avLSet<-dplyr::select(vLSet,1,2,44,3,45,48,4,19,5,6,31,54,16,39,30,34,18,29,14,28,13)


#######################VALIBATION FINAL ############################################################

load("ames_validation.Rdata")
nrow(ames_validation)

ames_validation$Overall.Qual<-as.factor(ames_validation$Overall.Qual)
ames_validation$Overall.Cond<-as.factor(ames_validation$Overall.Cond)
av<-ames_validation

vLSet<-dplyr::select(ames_validation,price,Overall.Qual,Lot.Area,
Year.Built,Sale.Condition,area,MS.Zoning,Year.Remod.Add,Land.Slope,
Exter.Qual,Lot.Shape,Land.Contour,Lot.Config,Street,
Bsmt.Qual,Bsmt.Cond,Bsmt.Exposure,BsmtFin.Type.1,Bldg.Type,
BsmtFin.SF.1,BsmtFin.Type.2,BsmtFin.SF.2,Bsmt.Unf.SF,
Total.Bsmt.SF,Heating.QC,Central.Air,House.Style,
Electrical,X1st.Flr.SF,X2nd.Flr.SF,
Bsmt.Full.Bath,Bsmt.Half.Bath,Full.Bath,Half.Bath,
Bedroom.AbvGr,Kitchen.AbvGr,TotRms.AbvGrd,
Functional,Fireplaces,
Garage.Yr.Blt,Garage.Finish,Garage.Cars,Garage.Area,
Paved.Drive,Wood.Deck.SF,
Open.Porch.SF,Enclosed.Porch,X3Ssn.Porch,Screen.Porch,
Fence,Misc.Val,Mo.Sold,Yr.Sold,Sale.Type,Neighborhood,Overall.Cond,Garage.Type,Kitchen.Qual,Heating)

vLSet<-data.frame(vLSet)

vLSet$logarea<-log(vLSet$area+1)
vLSet$logLot.Area<-log(vLSet$Lot.Area+1)
vLSet$logGarage.Area<-log(vLSet$Garage.Area+1)
vLSet$logX2nd.Flr.SF<-log(vLSet$X2nd.Flr.SF+1)
vLSet$logBsmtFin.SF.1<-log(vLSet$BsmtFin.SF.1+1)
vLSet$logBsmtFin.SF.2<-log(vLSet$BsmtFin.SF.2+1)
vLSet$logBsmt.Unf.SF<-log(vLSet$Bsmt.Unf.SF+1)
vLSet$logTotal.Bsmt.SF<-log(vLSet$Total.Bsmt.SF+1)
vLSet$logWood.Deck.SF<-log(vLSet$Wood.Deck.SF+1)
vLSet$logOpen.Porch.SF<-log(vLSet$Open.Porch.SF+1)
vLSet$logX1st.Flr.SF<-log(vLSet$X1st.Flr.SF+1)


amLSet<-ames_train
amLSet<-data.frame(amLSet)
amLSet$logarea<-log(amLSet$area+1)
amLSet$logLot.Area<-log(amLSet$Lot.Area+1)
amLSet$logX2nd.Flr.SF<-log(amLSet$X2nd.Flr.SF+1)
amLSet$logBsmtFin.SF.1<-log(amLSet$BsmtFin.SF.1+1)

amSet2<-dplyr::select(amLSet,price,Overall.Qual,Neighborhood,logarea,       
Overall.Cond,Year.Built,logLot.Area,Bsmt.Full.Bath,
Garage.Type,Sale.Condition,logX2nd.Flr.SF,Bldg.Type,     
Heating.QC,logBsmtFin.SF.1,Garage.Cars,MS.Zoning,   
Kitchen.Qual,Heating,Central.Air,   
Fireplaces)

 
avLSet2<-dplyr::select(vLSet,price,Overall.Qual,Neighborhood,logarea,       
Overall.Cond,Year.Built,logLot.Area,Bsmt.Full.Bath,
Garage.Type,Sale.Condition,logX2nd.Flr.SF,Bldg.Type,     
Heating.QC,logBsmtFin.SF.1,Garage.Cars,MS.Zoning,   
Kitchen.Qual,Heating,Central.Air,   
Fireplaces)

aves<-amSet2
lmf2<-lm(log(price)~.,data=aves)

ave<-avLSet2
nrow(ave)
ave<-avLSet2
ave<-filter(ave,Overall.Qual!='1')
ave<-filter(ave,Overall.Qual!='2')
ave<-filter(ave,Overall.Qual!='3')
ave<-filter(ave,Overall.Qual!='10')
ave<-filter(ave,Heating!='OthW')
nrow(ave)
ave<-filter(ave,MS.Zoning!='C (all)')
ave<-filter(ave,Sale.Condition!='Partial') 
ave<-filter(ave,Heating.QC!='Po')


nrow(ave)
ave<-filter(ave,MS.Zoning!='RH')
nrow(ave)

ave<-data.frame(ave)


aves<-amSet2
nrow(aves)
aves<-filter(aves,Overall.Qual!='1')
aves<-filter(aves,Overall.Qual!='3')
nrow(aves)
aves<-filter(aves,Sale.Condition!='AdjLand')
aves<-filter(aves,Heating!='OthW')
aves<-filter(aves,Sale.Condition!='Partial')
aves<-filter(aves,MS.Zoning!='C (all)')
nrow(aves)
aves<-filter(aves,MS.Zoning!='RH')
aves<-filter(aves,Overall.Qual!='2')
aves<-filter(aves,Overall.Qual!='10')
nrow(aves)
aves<-data.frame(aves)

plmfF<-predict(lmf2,ave,interval='predict')
plmfF<-data.frame(plmfF)
pplmfF<-predict(lmf2,aves,interval='predict')
pplmfF<-data.frame(pplmfF)

# Extract Predictions
predict.lmf2 <- exp(predict(lmf2, ave))
# Extract Residuals
resid.lmf2 <- ave$price - predict.lmf2
# Calculate RMSE
r2<-resid.lmf2
resid.lmf2<-data.frame(resid.lmf2)
resid.lmf2<-as.data.frame(lapply(resid.lmf2, na.omit))
rmse.lmf2<- sqrt(mean(resid.lmf2^2))

pr001<- ggplot(ave, aes(ave$price/1000, log(ave$price)))+
 geom_point()+
 
 geom_line(data=plmfF, aes(y=fit),colour='red')+
 geom_ribbon(data=plmfF,aes(ymin=lwr,ymax=upr),alpha=0.1,colour='black',fill="blue")+
ylab("Natural Log Price")+
    scale_x_discrete(name ="Price (in $1000)"  ,
                limits=c(0,100,200,300,400,500,600)) +
    ggtitle('Fig. XIA Plot Validation Data plmf1 Predicted Natural Log of Price using Log Area Adjustments Model ')
    
pr000<- ggplot(aves, aes(aves$price/1000, log(aves$price)))+
 geom_point()+
 
 geom_line(data=pplmfF, aes(y=fit),colour='blue')+
 geom_ribbon(data=pplmfF,aes(ymin=lwr,ymax=upr),alpha=0.1,colour='black',fill="red")+
ylab("Natural Log Price")+
    scale_x_discrete(name ="Price (in $1000)"  ,
                limits=c(0,100,200,300,400,500,600)) +
    ggtitle('Fig. XI Plot Training Data plmf1 Predicted Natural Log of Price using Log Area Adjustments Model ')
rmse.lmf2
plot(r2, main='Residuals vs Collection Index lmf2')

plot(pr000);plot(001)


############################TEST FINAL######################################################

tLSet<-dplyr::select(ames_test,price,Overall.Qual,Lot.Area,
Year.Built,Sale.Condition,area,MS.Zoning,Year.Remod.Add,Land.Slope,
Exter.Qual,Lot.Shape,Land.Contour,Lot.Config,Street,
Bsmt.Qual,Bsmt.Cond,Bsmt.Exposure,BsmtFin.Type.1,Bldg.Type,
BsmtFin.SF.1,BsmtFin.Type.2,BsmtFin.SF.2,Bsmt.Unf.SF,
Total.Bsmt.SF,Heating.QC,Central.Air,House.Style,
Electrical,X1st.Flr.SF,X2nd.Flr.SF,
Bsmt.Full.Bath,Bsmt.Half.Bath,Full.Bath,Half.Bath,
Bedroom.AbvGr,Kitchen.AbvGr,TotRms.AbvGrd,
Functional,Fireplaces,
Garage.Yr.Blt,Garage.Finish,Garage.Cars,Garage.Area,
Paved.Drive,Wood.Deck.SF,
Open.Porch.SF,Enclosed.Porch,X3Ssn.Porch,Screen.Porch,
Fence,Misc.Val,Mo.Sold,Yr.Sold,Sale.Type,Neighborhood,Overall.Cond,Garage.Type,Kitchen.Qual,Heating)

tLSet<-data.frame(tLSet)

tLSet$logarea<-log(tLSet$area+1)
tLSet$logLot.Area<-log(tLSet$Lot.Area+1)
tLSet$logGarage.Area<-log(tLSet$Garage.Area+1)
tLSet$logX2nd.Flr.SF<-log(tLSet$X2nd.Flr.SF+1)
tLSet$logBsmtFin.SF.1<-log(tLSet$BsmtFin.SF.1+1)
tLSet$logBsmtFin.SF.2<-log(tLSet$BsmtFin.SF.2+1)
tLSet$logBsmt.Unf.SF<-log(tLSet$Bsmt.Unf.SF+1)
tLSet$logTotal.Bsmt.SF<-log(tLSet$Total.Bsmt.SF+1)
tLSet$logWood.Deck.SF<-log(tLSet$Wood.Deck.SF+1)
tLSet$logOpen.Porch.SF<-log(tLSet$Open.Porch.SF+1)
tLSet$logX1st.Flr.SF<-log(tLSet$X1st.Flr.SF+1)


amLSet<-ames_train
amLSet<-data.frame(amLSet)
amLSet$logarea<-log(amLSet$area+1)
amLSet$logLot.Area<-log(amLSet$Lot.Area+1)
amLSet$logX2nd.Flr.SF<-log(amLSet$X2nd.Flr.SF+1)
amLSet$logBsmtFin.SF.1<-log(amLSet$BsmtFin.SF.1+1)

amSet2<-dplyr::select(amLSet,price,Overall.Qual,Neighborhood,logarea,       
Overall.Cond,Year.Built,logLot.Area,Bsmt.Full.Bath,
Garage.Type,Sale.Condition,logX2nd.Flr.SF,Bldg.Type,     
Heating.QC,logBsmtFin.SF.1,Garage.Cars,MS.Zoning,   
Kitchen.Qual,Heating,Central.Air,   
Fireplaces)

 
atLSet2<-dplyr::select(tLSet,price,Overall.Qual,Neighborhood,logarea,       
Overall.Cond,Year.Built,logLot.Area,Bsmt.Full.Bath,
Garage.Type,Sale.Condition,logX2nd.Flr.SF,Bldg.Type,     
Heating.QC,logBsmtFin.SF.1,Garage.Cars,MS.Zoning,   
Kitchen.Qual,Heating,Central.Air,   
Fireplaces)

aves<-amSet2
lmt2<-lm(log(price)~.,data=aves)

ave<-atLSet2
nrow(ave)
ave<-atLSet2
ave<-filter(ave,Overall.Qual!='1')
ave<-filter(ave,Overall.Qual!='2')
ave<-filter(ave,Overall.Qual!='3')
ave<-filter(ave,Overall.Qual!='10')
ave<-filter(ave,Heating!='OthW')
nrow(ave)
ave<-filter(ave,MS.Zoning!='C (all)')
ave<-filter(ave,Sale.Condition!='Partial') 
ave<-filter(ave,Heating.QC!='Po')


nrow(ave)
ave<-filter(ave,MS.Zoning!='RH')
nrow(ave)

ave<-data.frame(ave)


aves<-amSet2
nrow(aves)
aves<-filter(aves,Overall.Qual!='1')
aves<-filter(aves,Overall.Qual!='3')
nrow(aves)
aves<-filter(aves,Sale.Condition!='AdjLand')
aves<-filter(aves,Heating!='OthW')
aves<-filter(aves,Sale.Condition!='Partial')
aves<-filter(aves,MS.Zoning!='C (all)')
nrow(aves)
aves<-filter(aves,MS.Zoning!='RH')
aves<-filter(aves,Overall.Qual!='2')
aves<-filter(aves,Overall.Qual!='10')
nrow(aves)
aves<-data.frame(aves)

plmfF<-predict(lmf2,ave,interval='predict')
plmfF<-data.frame(plmfF)
pplmfF<-predict(lmf2,aves,interval='predict')
pplmfF<-data.frame(pplmfF)

# Extract Predictions
predict.lmf2 <- exp(predict(lmf2, ave))
# Extract Residuals
resid.lmf2 <- ave$price - predict.lmf2
# Calculate RMSE
r2<-resid.lmf2
resid.lmf2<-data.frame(resid.lmf2)
resid.lmf2<-as.data.frame(lapply(resid.lmf2, na.omit))
rmse.lmf2<- sqrt(mean(resid.lmf2^2))

pr001<- ggplot(ave, aes(ave$price/1000, log(ave$price)))+
 geom_point()+
 
 geom_line(data=plmfF, aes(y=fit),colour='red')+
 geom_ribbon(data=plmfF,aes(ymin=lwr,ymax=upr),alpha=0.1,colour='black',fill="blue")+
ylab("Natural Log Price")+
    scale_x_discrete(name ="Price (in $1000)"  ,
                limits=c(0,100,200,300,400,500,600)) +
    ggtitle('Fig. XIA Plot Validation Data plmf1 Predicted Natural Log of Price using Log Area Adjustments Model ')
    
pr000<- ggplot(aves, aes(aves$price/1000, log(aves$price)))+
 geom_point()+
 
 geom_line(data=pplmfF, aes(y=fit),colour='blue')+
 geom_ribbon(data=pplmfF,aes(ymin=lwr,ymax=upr),alpha=0.1,colour='black',fill="red")+
ylab("Natural Log Price")+
    scale_x_discrete(name ="Price (in $1000)"  ,
                limits=c(0,100,200,300,400,500,600)) +
    ggtitle('Fig. XI Plot Training Data plmf1 Predicted Natural Log of Price using Log Area Adjustments Model ')
rmse.lmf2
plot(r2, main='Residuals vs Collection Index lmf2')

plot(pr000);plot(001)

####################################BAS ADJUSTMENTS############################

nrow(ates)
nrow(ave)
ave<-filter(aves,Overall.Qual!='1')
nrow(ave)
ave<-filter(ave,Overall.Cond!='3')
nrow(ave)
ave<-filter(ave,Sale.Condition!='Alloca')
ave<-filter(ave,Sale.Condition!='Family')
ave<-filter(ave,Bldg.Type!='2fmCon')
nrow(ave)
ave<-data.frame(ave)
aves<-filter(aves,Overall.Qual!='1')
nrow(aves)
aves<-filter(aves,Overall.Cond!='3')
nrow(aves)
aves<-filter(aves,Sale.Condition!='AdjLand')
nrow(aves)
aves<-filter(aves,Sale.Condition!='Alloca')
aves<-filter(aves,Sale.Condition!='Family')
aves<-filter(aves,Bldg.Type!='2fmCon')
nrow(aves)
aves<-filter(aves,MS.Zoning!='I (all)')
nrow(aves)
unl<-matrix(
for(i in 1:nrow(un)){
  if(un[i,'Predict']>=un[i,'Lim']) {
      print(i)
  }


aves<-data.frame(aves)
