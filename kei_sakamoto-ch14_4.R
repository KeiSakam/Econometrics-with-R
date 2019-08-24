load("~/import/practice_wooldridge/1111531048_374626/R data sets for 5e/wagepan.RData")
wagepan<-data
library(plm)
wagepan.p<-pdata.frame(wagepan,index=c("nr","year"))
#pooled OLSでの推定
regpool<-plm(lwage~educ+black+hisp+exper+I(exper^2)+married+union,data=wagepan.p,model="pooling")
#random effect modelでの推定
regrand<-plm(lwage~educ+black+hisp+exper+I(exper^2)+married+union,data=wagepan.p,model="random")
#fixed effect(within transformation)での推定
regwith<-plm(lwage~I(exper^2)+married+union,data=wagepan.p,model="within")
#表にまとめます
install.packages("stargazer")
library(stargazer)
stargazer(regpool,regrand,regwith,type="text",column.labels=c("poolOLS","Random","within"))
