library(gamlss)
data(dbbmi)
head(dbbmi)
summary(dbbmi)
old<-20
da<- with(dbbmi, subset(dbbmi, age>old+1))
bmi20<-da$bmi

#plotting BMI data in histogram
hist(bmi20)

library(MASS)
truehist(bmi20, nbins=30)
install.packages("gamlss.ggplots")
library("gamlss.ggplots")
gamlss.ggplots:::y_hist(bmi20)

#Fitting normal distribution
fit_norm <- gamlss(bmi20 ~ 1, family = NO)


#Fitting gamma distribution
fit_gamma <- gamlss(bmi20 ~ 1, family = GA)


#Fitting lognormal distribution
fit_lognormal <- gamlss(log(bmi20) ~ 1, family = NO)
plot(fit_lognormal)


#Fitting Inverse gaussian
fit_inversegaussian <- gamlss(bmi20 ~ 1, family = IG)


#compare the AIC values 
AIC.df <- data.frame(Distribution = c("Normal", "gamma" ,"Log-normal", "Inverse gamma"), AIC = c(AIC(fit_norm), AIC(fit_gamma), AIC(fit_lognormal), AIC(fit_inversegaussian)))
AIC.df

# Select the distribution with the lowest AIC
best.dist <- AIC.df$Distribution[which.min(AIC.df$AIC)]
best.dist


                     