#example 16-5
library(AER)
load("~/import/practice_wooldridge/1111531048_374626/R data sets for 5e/mroz.RData")
#NAのデータ以外を抽出
data2<-subset(data,!is.na(wage))
#同時方程式をそれぞれIV regression
summary(ivreg(hours~log(wage)+educ+age+kidslt6+nwifeinc|educ+age+kidslt6+nwifeinc+exper+I(exper^2),data=data2))
summary(ivreg(log(wage)~hours+educ+exper+I(exper^2)|educ+age+kidslt6+nwifeinc+exper+I(exper^2),data=data2))

#example 16-6
load("~/import/practice_wooldridge/1111531048_374626/R data sets for 5e/openness.RData")
openness<-data
#1st stage
summary(lm(open~log(pcinc)+log(land),data=openness))
#上のregressionに名前をつける
reg<-lm(open~log(pcinc)+log(land),data=openness)
#fitted valueの登録
openhat<-fitted(reg)
#2nd stageのための準備。データフレーム作成
data3<-data.frame(openhat=openhat,inf=openness$inf,pcinc=openness$pcinc,land=openness$land)
#2nd stage
summary(lm(inf~openhat+log(pcinc),data=data3))

#example 16-7
load("~/import/practice_wooldridge/1111531048_374626/R data sets for 5e/consump.RData")
consump<-data
#IV regression
summary(ivreg(gc~gy+r3|gc_1+gy_1 +r3_1,data=consump))
