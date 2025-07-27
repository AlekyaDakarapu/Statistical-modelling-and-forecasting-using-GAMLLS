library(gamlss)
data(grip)
head(grip)
summary(grip)
sum(is.na(grip))

set.seed(1058)
index<-sample(3766, 1000)
mydata<-grip[index, ]


dim(mydata)
head(mydata)

library(ggplot2)
ggplot(mydata, aes(x=age, y=grip))+ geom_point()+ labs(x= "Age", y = "Grip")

#fit LMS model using bccg distribution
lms_model <- gamlss(grip ~ pb(age), sigma.fo = ~pb(age), nu.fo = ~pb(age), data = mydata, family = BCCG) 
#get degress of  freedom for smoothing term
edf(lms_model)
#fit BCT
gbct <- gamlss(grip ~ pb(age), sigma.fo = ~pb(age), nu.fo = ~pb(age), data = mydata, family = BCT, start.from = lms_model)
# get degrees of freedom for smoothing term
edf(gbct)
# fit BCPE
gbcpe <- gamlss(grip ~ pb(age), sigma.fo = ~pb(age), nu.fo = ~pb(age), data = mydata, family = BCPE, start.from = lms_model)
# get degeers of freedom
edf(gbcpe)

GAIC(lms_model, gbct, gbcpe)

fittedPlot(lms_model,gbcpe, gbct, x=mydata$age)

centiles_Lms <- centiles(lms_model, var = mydata$grip, xvalues = seq(20, 100, by = 5))
centiles_gbct <- centiles(gbct, var = mydata$grip, xvalues = seq(20, 100, by = 5))
centiles_gbcpe <- centiles(gbcpe, var = mydata$grip, xvalues = seq(20, 100, by = 5))

#Plotting residuals for lms_model
plot(lms_model)
wp(lms_model)
Q.stats(lms_model)

#Plotting residuals for gbct model
plot(gbct)
wp(gbct)
Q.stats(gbct)

#Plotting residuals for gbcpe model
plot(gbcpe)
wp(gbcpe)
Q.stats(gbcpe)

