#AS1 Applied Econometrics
library(stargazer) #export tables
library(dynlm) #use dynamic linear model
library(ARDL) #find best ARDL model
library(pdfetch)#fetch economic and financial time series data
library(stats) #decomposition
library(TSstudio) #functions of time series analysis and forecasting
library(xts) #use xts-related codes
library(lmtest)#testing linear regression models
library(tseries) #time series analysis and computational finance
library(strucchange)#structural change
library(zoo) #use zoo-related commands
library(car) #companion to applied regression
library(urca)#unit root and Cointegration tests for time series
library(plyr)#tools for splitting, applying and combining data
library(sandwich) #robust covariance matrix estimators
library(ggplot2)#Data Visualisations

fred_both<-pdfetch_FRED(c("HDTGPDPLQ163N", "LRUN64TTPLQ156S"))

colnames(fred_both) [1] <- "DEBT" # not seasonally adjusted
colnames(fred_both) [2] <- "UNEMPLOYMENT" # seasonally adjusted
# the data period for each variable is not the same length of time

fred_both2 <- fred_both[!is.na(fred_both$DEBT)] # removes missing data rows

plot(fred_both2)


#turn into time series before decomposing
fredboth_ts<-ts(fred_both2, frequency = 4)
#decompose
stl <- stl(fredboth_ts [,1], s.window="periodic")#DEBT
plot(stl) 
#visual test for debt shows seasonality
#however no clear trend from the decomposition, although there's a downwards movement
#take first difference

stl1 <- stl(fredboth_ts [,2], s.window="periodic")#Unemployment
plot(stl1) 
#ignore seasonal decomposition as the fred data is already seasonally adjusted for unemp
#the unemployment data shows downwards trend between specific a specific period

#take first difference for both above
diffDEBT<-diff(fred_both2$DEBT) #first difference
plot(diffDEBT, las=2)
diffDEBT2<-na.omit(diffDEBT)#retrieves the missing data for the very first year
plot(diffDEBT2, las=2) # this plot includes the first year now 
#do again for unemployment, then create data frame to add both into one panel table
diffUNEMPLOYMENT<-diff(fred_both2$UNEMPLOYMENT) #first difference
plot(diffUNEMPLOYMENT, las = 2)
diffUNEMPLOYMENT2<-na.omit(diffUNEMPLOYMENT)#retrieves the missing data for the very first year
plot(diffUNEMPLOYMENT2, las=2) # this plot includes the first year now
#we don't need to use any of these as we are using an ARDL model which works for non-stationary too

adf<- ur.df(fred_both2$DEBT)#Debt
summary(adf)
#debt to Gdp for Poland is non-stationary as p>0.05

adf1<- ur.df(fred_both2$UNEMPLOYMENT)#unemployment
summary(adf1)
#UNEMPLOYMENT Rate for Poland is stationary as p<0.05

#checking for structural breaks in household debt series
#transform to zoo data
fred_both.zoo<-zoo(fred_both2)
bp <- breakpoints(fred_both2$DEBT ~ 1)
summary(bp) 
bp$breakpoints#11 & 49 based on smallest BIC value
plot(bp)

#two optimal breaks
bp2<-breakpoints(bp, breaks=2)

plot(fred_both.zoo$DEBT, xlab="", ylab="Ratio", 
     main="Household Debt to GDP Ratio - Poland")
lines(bp2)

plot(fred_both.zoo$DEBT, xlab="", ylab="Ratio", 
     main="Household Debt to GDP Ratio - Poland")
abline(v=as.Date(c("2011-06-30", "2020-12-31")), lty=3)

fred1 = data.frame(fred_both.zoo, t=c(1:60))
fred1$date <- as.Date(index(fred_both2))
#add dummy variable to the table
fred1$y11Q2<-ifelse(fred1$t >= 11, 1, 0)
fred1$y20Q4<-ifelse(fred1$t >= 49, 1, 0)

#ARDL Model will be suitable as the one variable is stationary and the other is non-stationary
#first look for optimal model, then model asked for from the question

ardl_aic<-auto_ardl(DEBT ~ UNEMPLOYMENT, selection="AIC", data = fredboth_ts, max_order = 5)
ardl_aic# 5 5

#bic
ardl_bic<-auto_ardl(DEBT ~ UNEMPLOYMENT, selection="BIC", data = fredboth_ts, max_order = 5)
ardl_bic# 5 5

model1<- dynlm(DEBT ~ L(DEBT) + L(DEBT, 2) + L(DEBT, 3) + L(DEBT, 4) + L(DEBT, 5) + UNEMPLOYMENT + L(UNEMPLOYMENT) + L(UNEMPLOYMENT, 2) + L(UNEMPLOYMENT, 3) + L(UNEMPLOYMENT, 4) + L(UNEMPLOYMENT, 5), data = fred_both.zoo)
model1# debt 5, unemployment 5
text_model <- stargazer(model1, type = "text")

model2<-ardl(DEBT~UNEMPLOYMENT+y11Q2+y20Q4, data=fred1, order=c(5,5,0,0))#use fred1 instead of fred_both.zoo for ARDL
summary(model2)
#dummy 20Q4 significant at 1% significance level or where p<0.01
#dummy 11Q2 is not statistically significant at any level

#test for homoscedasticity and serial correlation
bptest(model2) # heteroscedastic
#model2 is heteroscedastic with the dummy variables included, so use robust standard errors
coeftest(model2, vcov = vcovHC(model2, type="HC0"))
#dummy 20Q2 becomes less significant, as it changes to 5% level
dwtest(formula = model2) # data has no serial correlation, as p>0.1
#no additional lags need to be added
bgtest(model2) # also suggests no serial correlation

#find model from Question 5

model3<- dynlm(DEBT ~ L(DEBT) + UNEMPLOYMENT, data = fred_both.zoo)
model3# debt 1, unemployment 0
text_model <- stargazer(model3, type = "text")

model4<-ardl(DEBT~UNEMPLOYMENT, data=fred_both.zoo, order=c(1,0))
summary(model4)

bptest(model4)# p>0.05 , data is homoscedastic
dwtest(formula = model4) # p>0.05, the data has no autocorrelation
bgtest(model4) # no serial correlation
