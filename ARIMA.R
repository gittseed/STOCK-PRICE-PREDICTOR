library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(xts)


getSymbols('ZYDUSLIFE.NS',from='2021-04-05',to='2023-04-15')
#type of data we pulling in i.e xts



#only col 4. open,high,low,close
SPY_Close_Prices = ZYDUSLIFE.NS[,4]

#plotting data
plot(SPY_Close_Prices)
class(SPY_Close_Prices) #xts/zoo

par(mfrow=c(1,1))#resets screen size for graph

#Graph the ACF and PACF looking for Identificable lags PACF->P ACF->Q for custom arimas
par(mfrow=c(1,2))
Acf(SPY_Close_Prices, main='ACF for Differenced Series')
Pacf(SPY_Close_Prices, main='PACF for Differenced Series')
#clearly you have a lag at 1 on the PCF which translates into a P value of 1 

#Test findings on originalXTS objects
#ADF test for p-value
print(adf.test(SPY_Close_Prices)) 
auto.arima(SPY_Close_Prices,seasonal = FALSE) 



fitA = auto.arima(SPY_Close_Prices,seasonal=FALSE ) #auto arima (3,1,4)
tsdisplay(residuals(fitA), lag.max=40, main='(1,1,1) Model Residuals')
auto.arima(SPY_Close_Prices,seasonal=FALSE ) #AIC/BIC=8843.02/8853.73

fitB = arima(SPY_Close_Prices, order = c(1,2,4)) #custom arima (1,2,4)
tsdisplay(residuals(fitA), lag.max=40, main='(1,2,4) Model Residuals')

fitC = arima(SPY_Close_Prices, order = c(0,2,2)) 
tsdisplay(residuals(fitA), lag.max=40, main='(5,1,4) Model Residuals')

fitD = arima(SPY_Close_Prices, order = c(3,1,4)) 
tsdisplay(residuals(fitA), lag.max=40, main='(3,1,4) Model Residuals')

par(mfrow=c(2,2))
#auto
term<-90
fcast1 <- forecast(fitA, h=term)
plot(fcast1)
#custom
fcast2 <- forecast(fitB, h=term)
plot(fcast2)
fcast3 <- forecast(fitC, h=term)
plot(fcast3)
fcast4 <- forecast(fitD, h=term)
plot(fcast4)




#Mape accuracy subtract from 100
accuracy(fcast1) #99.197 accuracy
accuracy(fcast2) #99.201
accuracy(fcast3) #99.207
accuracy(fcast4) #99.197







# plot actual data
plot(SPY_Close_Prices, main="Actual Data vs ARIMA Prediction")
# add ARIMA prediction to the plot
lines(fcast1$mean, col="red")
# add legend
legend("topleft", legend=c("Actual Data", "ARIMA Prediction"), col=c("black", "red"), lty=1, cex=0.8)

count-na = ts(na.omit(ZYDUSLIFE.NS[,4]))







