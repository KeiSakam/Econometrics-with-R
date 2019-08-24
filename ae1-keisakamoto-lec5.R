load("~/import/practice_wooldridge/1111531048_374626/R data sets for 5e/mroz.RData")
mroz<-data
install.packages("VGAM")
library(VGAM)
#Type1Tobit model
Type1Tobit<-vglm(hours~nwifeinc+educ+exper+I(exper^2)+age+kidslt6+kidsge6,tobit(Lower=0),data=mroz)
summary(Type1Tobit)
