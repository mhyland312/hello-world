################################BNSF Project: Waybill Data ################################
###########################################################################################

mydataAllyears<-read.csv("C:/Users/Mike/OneDrive/Documents/Ongoing Research Projects/BNSF/AllYearsData3.csv",header=T)


########Econometric Model##################
#mydataAllyears$ln_ton_sq = log(mydataAllyears$Bill_weight*mydataAllyears$Bill_weight)
mydataAllyears$ln_ton = log(mydataAllyears$Bill_weight)
mydataAllyears$ln_Freight_rev = log(mydataAllyears$Freight_rev)
mydataAllyears$ln_ton_miles= log(mydataAllyears$Bill_weight*mydataAllyears$Est_SL_mil)

#mydataAllyears$ln_mi_sq = log(mydataAllyears$Est_SL_mil*mydataAllyears$Est_SL_mil)
mydataAllyears$ln_miles = log(mydataAllyears$Est_SL_mil)
mydataAllyears$ln_Rev_ton_mile = log(mydataAllyears$Rev_ton_mile )

UMW_allYears<-subset(mydataAllyears, mydataAllyears$O_BEA==110|mydataAllyears$O_BEA==111
                     |mydataAllyears$O_BEA==112|mydataAllyears$O_BEA==113)


modela<- lm(ln_Rev_ton_mile ~ ln_ton + I(ln_ton^2)+ ln_ton*ln_miles
            + ln_miles + I(ln_miles^2) + Freight_rev + dummy_Oil + Exact_EF + Theo_EF +
              dummy_Railroad_own
            + dummy_Grain*year2013
            + dummy_Oil*year2013
            + dummy_Grain*year2010
            +dummy_Export
            
            + Car_cap + Haz_boxcar_Indicator + Intermodal  , data=mydataAllyears)
summary(modela)

modelb<- lm(ln_Rev_ton_mile ~ ln_ton + I(ln_ton^2)+ ln_ton*ln_miles
            + ln_miles + I(ln_miles^2)+ Carl_num + I(Carl_num^2) + Freight_rev + dummy_Oil + Exact_EF + Theo_EF +
              dummy_Railroad_own
            + dummy_Grain*year2013
            + dummy_Oil*year2013
            + dummy_Grain*year2010
            + dummy_Grain*year2008
            + dummy_Grain*year2006
            + dummy_Grain*year2004
            +dummy_Export
            
            + Car_cap  , data=UMW_allYears)

summary(modelb)
mean(UMW_allYears$ln_Rev_ton_mile)

model2<- lm(ln_Rev_ton_mile ~ ln_ton + I(ln_ton^2)+ ln_ton*ln_miles
            + ln_miles + I(ln_miles^2)+ Carl_num + Freight_rev + dummy_Oil + Exact_EF + Theo_EF +
              dummy_Railroad_own + dummy_Export + year2013 + year2010 + year2008 + year2006 + year2004
            + dummy_UMW*dummy_Grain*year2013
            + dummy_UMW*dummy_Oil*year2013
            + dummy_UMW*dummy_Grain*year2010
            
            + Car_cap + Haz_boxcar_Indicator + Intermodal  , data=mydataAllyears)



summary(model2)

model3<- lm(ln_Rev_ton_mile ~ ln_ton
            + ln_miles + Carl_num + Freight_rev + dummy_Oil + Exact_EF + Theo_EF +
              dummy_Railroad_own +
              + dummy_UMW*dummy_Grain*year2013*dummy_Export
            + dummy_UMW*dummy_Oil*year2013*dummy_Export
            + dummy_UMW*dummy_Grain*year2010*dummy_Export
            + dummy_UMW*dummy_Grain*year2008*dummy_Export
            
            + Car_cap + Haz_boxcar_Indicator + Intermodal  , data=mydataAllyears)
anova(model2,model3)
coefficients(model2)
anova(model2)
summary(model2)$r.squared

summary(model3)

mean(mydataAllyears$ln_Rev_ton_mile)


plot(lm(mydataAllyears$ln_Rev_ton_mile~ mydataAllyears$Carl_num  )
     
     
     
     
     
     
     
     #####use training set to do predictions on withheld set
     predict(lm.out4, list(temp=new.temps))
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     model2<- lm(ln_Rev_ton_mile ~ ln_ton + ln_ton_sq  + ln_mi_sq 
                 + ln_miles + Carl_num + Freight_rev + dummy_Oil + Exact_EF + Theo_EF +
                   dummy_Grain + dummy_Export + dummy_Railroad_own +
                   year2013 + year2010 + year2008 + year2006 + year2004
                 + dummy_UMW 
                 + dummy_UMW*dummy_Grain*year2013*dummy_Export
                 + dummy_UMW*dummy_Oil*year2013*dummy_Export
                 + dummy_UMW*dummy_Grain*year2010*dummy_Export + 
                   dummy_UMW*dummy_Oil*year2010*dummy_Export +
                   dummy_UMW*dummy_Grain*year2008*dummy_Export + 
                   dummy_UMW*dummy_Oil*year2008*dummy_Export +
                   dummy_UMW*dummy_Grain*year2006*dummy_Export + 
                   dummy_UMW*dummy_Oil*year2006*dummy_Export +
                   + Car_cap + Haz_boxcar_Indicator + Intermodal  , data=mydataAllyears)
     
     model2<- lm(ln_Rev_ton_mile ~ ln_ton + ln_ton_sq  + ln_mi_sq + ln_Misc_charg + ln_Transit_charg
                 + ln_miles + Carl_num + Freight_rev + dummy_Oil + 
                   dummy_Grain + dummy_Export + dummy_Railroad_own +
                   year2013 + year2010 + year2008 + year2006 + year2004 + dummy_UMW + 
                   dummy_UMW*dummy_Grain*year2013*dummy_Export + 
                   dummy_UMW*dummy_Oil*year2013*dummy_Export +
                   dummy_UMW*dummy_Grain*year2010*dummy_Export + 
                   dummy_UMW*dummy_Oil*year2010*dummy_Export +
                   dummy_UMW*dummy_Grain*year2008*dummy_Export + 
                   dummy_UMW*dummy_Oil*year2008*dummy_Export +
                   dummy_UMW*dummy_Grain*year2006*dummy_Export + 
                   dummy_UMW*dummy_Oil*year2006*dummy_Export +
                   Car_cap + Haz_boxcar_Indicator + Intermodal  , data=mydataAllyears)
     
     
     
     
     
     
     
     
     model<- lm((Rev_ton_mile) ~ Bill_weight + log(Bill_weight)  + Est_SL_mil
                + log(Est_SL_mil) + Carl_num + Freight_rev + dummy_Oil + 
                  dummy_Grain + dummy_Export + dummy_Railroad_own +
                  year2013 + year2010 + year2008 + year2006 + year2004 , data=mydataAllyears)
     
     coefficients(model)
     anova(model)
     summary(model)$r.squared
     dim(mydataAllyears)
     
     
     
     
     
     
     