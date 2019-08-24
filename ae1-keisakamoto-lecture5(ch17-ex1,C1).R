#example17-1
library(stargazer)
load("~/import/practice_wooldridge/1111531048_374626/R data sets for 5e/mroz.RData")
mroz<-data
#LPM
LPM<-lm(inlf~nwifeinc+educ+exper+I(exper^2)+age+kidslt6+kidsge6,data=mroz)
#LogitReg
LogitReg<-glm(inlf~nwifeinc+educ+exper+I(exper^2)+age+kidslt6+kidsge6,family=binomial(link=logit),data=mroz)
#ProbitReg
ProbitReg<-glm(inlf~nwifeinc+educ+exper+I(exper^2)+age+kidslt6+kidsge6,family=binomial(link=probit),data=mroz)
#表にまとめる
stargazer(list(LPM,LogitReg,ProbitReg),type="text",keep.stat=c("n","rsq"))

#Computer Exercise1
load("~/import/practice_wooldridge/1111531048_374626/R data sets for 5e/pntsprd.RData")
pntsprd<-data
#(i)spreadが0の時,β0は0.5ならば選好したチームが勝つ確率は1/2つまりランダムと見れそうだから、選好したチームが勝つ確率を説明するものが何もないということなので、spreadには勝敗に関連する全ての情報が含まれると考えられるから。
#(ii)β0は1%有意。
LPM2<-lm(favwin~spread,data=pntsprd)
coeftest(LPM2,hccm)
#(iii)(ii)から、β1も1%有意。spread＝10の時、選好したチームが勝つ確率は0.5769492+0.0193655×10=約0.771
#(iv)推定値が-0.01059、seが0.10349なのでt-statisticは小さく、β0が0という帰無仮説は棄却できない。
ProbitReg2<-glm(favwin~spread,family=binomial(link=probit),data=pntsprd)
summary(ProbitReg2)
#(v)-0.01059+0.09246×10=0.91401でΦ(0.91401)は0.8196442 なので約0.82
pnorm(0.91401)
#(vi)2(-262.6418-(-263.5622))=1.84がLR-statisticなのに対し、自由度3のカイ二乗分布の10%臨界値が6.25なので、加えた３つの変数は統計的に有意ではない。つまりspreadさえあれば他は要らない。
logLik(ProbitReg2)
ProbitReg3<-glm(favwin~spread+favhome+fav25+und25,family=binomial(link=probit),data=pntsprd)
logLik(ProbitReg3)
