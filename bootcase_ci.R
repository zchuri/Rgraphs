#######################################################################################################
# Scatter plot with fitted line and bootstrap confidence intervals
#######################################################################################################

# This plot is a generalization of the code of EDi (https://stackoverflow.com/users/511399/edi)
# to instead of create parametric confidence intervals (CI), these are built based in a case 
# bootstrap resampling. This is the link for the parametric CI.
# https://stackoverflow.com/questions/14069629/plotting-confidence-intervals

# Let's get a 'chocolated' data.frame to practise
library(datasets)
# Load Swiss Fertility and Socioeconomic Indicators (1888) Data
# http://stat.ethz.ch/R-manual/R-devel/library/datasets/html/swiss.html
data(swiss)
head(swiss)

# Let's check the linear relationship between fertility and infant mortality variables
# Axiliar dataframe in order to use abreviated names
df <- data.frame(x=swiss$Fertility,y=swiss$Infant.Mortality)
# Apply linear model
fit1 <- lm(y ~ x, data = df)
# First, draw the empty plot
plot(y ~ x, data = df,type="n")

# Generate bootstrap confidence intervals
# Take limits a bit longer than the plotted axis to predict
newx <- data.frame(x=seq(min(df$x)*0.1,max(df$x)*1.1,length.out=100))

# Fit the model B times
B=5000
library(car) # 'car' package is needed to run the case bootstrap for regression models
bootfit1 <- bootCase(fit1, function(x) predict(x, newx),B)
# Take quantiles of predictions from bootstrap replicates.
newx$lci <- apply(bootfit1,2,quantile,0.025)
newx$uci <- apply(bootfit1,2,quantile,0.975)
# Add bootstrapped lines and polygon
library(scales) # load alpha function to set color transparency
polygon(c(rev(newx$x), newx$x), c(rev(newx$uci),newx$lci),col=alpha("gray",0.25), border = NA)
# intervals
lines(newx$x, newx$lci, lty = "dashed", col ="black")
lines(newx$x, newx$uci, lty = "dashed", col ="black")
# model
abline(fit1,lwd=2,col="gray")
# intervals
points(y~x,data=df,col=3,pch=19)

# Want more info for bootstrap in linear models, check this link:
# http://rstudio-pubs-static.s3.amazonaws.com/24365_2803ab8299934e888a60e7b16113f619.html

# Let's paint the parametric CI in order to see the difference
# predicts + interval
parfit1 <- predict(fit1,newdata=data.frame(x=newx$x),interval="confidence")
# Add parametric CI
lines(newx$x, parfit1[,3], lty="dashed",col="red")
lines(newx$x, parfit1[,2], lty="dashed",col="red")

# Add a legend
legend("bottomright",legend=c("Bootstrap","Parametric"),col=c("black","red"),cex=.9,lty=c(1,1))
