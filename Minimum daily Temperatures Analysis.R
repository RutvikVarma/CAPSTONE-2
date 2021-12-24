#loading the data
min <- read.csv("C:/Users/Ruthvik/Desktop/Temperatures.csv")
head(min)
colnames(min)

#creating time series
main <- ts(min[,2],start=c(1981,01,01),freq=365)
head(main)

plot(main)

plot(decompose(main)) #there is seasonality in the data

#differencing first time
d.main <- diff(main)
plot(d.main)
#the seasonality in my data is eliminated

#differencing second time
d2.main <- diff(d.main)
plot(d2.main)

#adf test
adf.test(main)
adf.test(d.main)
adf.test(d2.main)


#acf and pacf 
acf(main)
pacf(main)

acf(d.main)
pacf(d.main)

#arima models
arima(main,order=c(1,0,0))
arima(main,order=c(1,1,1))
arima(main,order=c(1,2,1))
arima(main,order=c(1,3,1))
arima(main,order=c(1,4,1))
arima(main,order=c(4,1,1))#this is having less aic value at 16784.72

#auto arima for best fit
auto.arima(main)#(5,0,0)
auto.arima(d.main)#(4,0,1)


model <- arima(main,order=c(1,1,1))
predict(model,50)

train.main <- ts(min[1:3600,2],start=c(1981,01,01),freq=365)
test.main <- ts(min[3601:3650,2],start=c(19910,11,12),freq=365)

d.train <- diff(train.main)
plot(d.train)

d2.train <- diff(d.train)
plot(d2.train)

adf.test(main)
adf.test(train.main)
adf.test(d.train)
adf.test(d2.train)

acf(train.main)
pacf(train.main,lag=15)

acf(d.train)
pacf(d.train,lag=15)

acf(d2.train)
pacf(d2.train,lag=15)

auto.arima(train.main)
auto.arima(d.train)#aic is less
auto.arima(d2.train)

#fitting models
m1 <- arima(train.main,order=c(5,0,0))
m1

m2 <- arima(train.main,order=c(4,1,1))#aic value is less
m2

m3 <- arima(train.main,order=c(5,2,0))
m3

m4 <- arima(train.main,order=c(1,1,1))
m4
#predicting with the models
fit1 <- predict(m1,50)
fit1

fit2 <- predict(m2,50)
fit2

fit3 <- predict(m3,50)
fit3

fit4 <- predict(m4,50)
fit4

#MAPE
MAPE(min$Temp[3601:3650],fit1$pred)#0.249
MAPE(min$Temp[3601:3650],fit2$pred)#0.151
MAPE(min$Temp[3601:3650],fit3$pred)#2.18
MAPE(min$Temp[3601:3650],fit4$pred)#0.1649

fit5 <- predict(m2,80)
fit5


