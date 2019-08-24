#example15-1
#Applied Econometrics with R (AER)
library(AER)
load("~/import/practice_wooldridge/1111531048_374626/R data sets for 5e/mroz.RData")
mroz<-data
#wageの429行目以降はNAなので428までのデータを抽出
sample<-head(data,n=428)
#OLS
summary(lm(log(wage)~educ,data=sample))
#IV
summary(ivreg(log(wage)~educ|fatheduc,data=sample))


#example15-2
load("~/import/practice_wooldridge/1111531048_374626/R data sets for 5e/wage2.RData")
wage2<-data
#first stage of TSLS
first<-lm(educ~sibs,data=wage2)
summary(first)
#second stage of TSLS
summary(lm(log(wage)~fitted(first),data=wage2))


#example15-5
install.packages("sem")
library(sem)
load("~/import/practice_wooldridge/1111531048_374626/R data sets for 5e/mroz.RData")
mroz<-data
#wageの429行目以降はNAなので428までのデータを抽出
sample<-head(data,n=428)
#tslsのコマンドでIV
summary(tsls(log(wage)~educ+exper+I(exper^2),instruments=~motheduc+fatheduc+exper+I(exper^2),data=sample))
result<-tsls(log(wage)~educ+exper+I(exper^2),instruments=~motheduc+fatheduc+exper+I(exper^2),data=sample)


#example15-10
library(AER)
library(plm)
load("~/import/practice_wooldridge/1111531048_374626/R data sets for 5e/jtrain.RData")
#1989年のデータを排除
only1987and1988<-subset(data,year<=1988)
#panelデータとして登録
only1987and1988_panel<-pdata.frame(only1987and1988,index=c("fcode","year"))
#FDとIVのコラボ
summary(plm(log(scrap)~hrsemp|grant,model="fd",data=only1987and1988_panel))
