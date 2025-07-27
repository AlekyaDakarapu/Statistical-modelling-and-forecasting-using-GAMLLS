library(gamlss)
library(ggplot2)

# Loading the dataset
df <- read.csv("C:/Users/Alekya/Desktop/Stats Modelling/CleanData .csv")
head(df)
summary(df)
#checking for null values
sum(is.na(df))

# Selecting a sample from the dataset
set.seed(1058)

sample_size <- 20000
mydata <- df[sample(1:nrow(df), sample_size), ]

# Encoding categorical columns
for (col in names(mydata)) {
  if (col != 'Price') {
    mydata[[col]] <- as.numeric(factor(mydata[[col]]))
  }
}
# Histogram of Price
ggplot(df, aes(x = Price)) +
  geom_histogram(binwidth = 100000) +
  labs(x = "Price", y = "Frequency") +
  ggtitle("Histogram of Price")
# Fitting the GAMLSS models
fit.norm <- gamlss(Price ~ ., data = mydata, family = NO())
fit.lnorm <- gamlss(Price ~ ., data = mydata, family = LOGNO())
fit.gamma <- gamlss(Price ~ ., data = mydata, family = GA())
fit.gengamma <- gamlss(Price ~ ., data = mydata, family = "GG")
fit.bct <- gamlss(Price ~ ., data = mydata, family = BCT())

# Compare AIC values for the different distributions
AIC.df <- data.frame(Distribution = c("Normal", "Log-normal", "Gamma", "Generalized gamma", "Box-Cox t"),
                     AIC = c(AIC(fit.norm), AIC(fit.lnorm), AIC(fit.gamma), AIC(fit.gengamma), AIC(fit.bct)))
AIC.df

# Select the distribution with the lowest AIC
best.dist <- AIC.df$Distribution[which.min(AIC.df$AIC)]
best.dist

# Checking model diagnostics
par(mfrow = c(2, 2))
plot(fit.bct, which = 1)  # Residuals vs. Fitted
plot(fit.bct, which = 2)  # Normal Q-Q plot
plot(fit.bct, which = 3)  # Scale-Location plot (Square root of standardized residuals vs. Fitted values)
plot(fit.bct, which = 4)  # Cook's distance plot

new_data <- mydata[1:10, ]
gamlss_predictions <- predict(fit.bct, newdata = new_data, type = "response")
gamlss_predictions
