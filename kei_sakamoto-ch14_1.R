load("~/import/practice_wooldridge/1111531048_374626/R data sets for 5e/jtrain.RData")
library(plm)
jtrain<-data
jtrain.p<-pdata.frame(jtrain,index=c("fcode","year"))
regwith<-plm(log(scrap)~d88+d89+grant+grant_1,data=jtrain.p,model="within")
summary(regwith)