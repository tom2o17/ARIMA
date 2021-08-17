rm(list=ls(all=T))
library(quantmod)
library(forecast)
library(gmailr)
library(tableHTML)
library(stringr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

b.date <- as.Date("2013-05-10")

date.current <- as.Date("2020-09-01")
dates.t <- seq(date.current,by="day", length.out=91)

dates.t

weekends <- c(5,6,7,12,13,19,20,26,27,(30+3),(30+4),(30+10),(30+11),(30+17),
              (30+18),(30+24),(30+25),(30+31), (61+1),61+1, 61+7,
              61+8,61+14,61+15,61+21,61+22,61+26, 61+28, 61+29)


t <- "SPY"
H <- 1
d.1.predictions <- matrix(NA,length(dates.t),7)

# Loop generating Arima Predictions 


for (i in 1:length(dates.t)){
  x <- getSymbols(t,src="yahoo", from=b.date, to=(dates.t[i]), 
                  auto.assign = F)
  colnames(x)[4] <- "Close"
  c.p <- x$Close
  model.fit <- auto.arima(c.p,lambda = "auto")
  price.forecast <- forecast(model.fit, h=H)
  pred.mean <-  price.forecast$mean
  pred.high <- price.forecast$upper
  pred.low <- price.forecast$lower
  
  current.date <- dates.t[i]
  pred <- data.frame(current.date)
  pred$new.dates <- dates.t[i+1]
  pred$mean <- pred.mean 
  pred$low.80 <- pred.low[,1]
  pred$low.95 <- pred.low[,2]
  pred$up.80 <- pred.high[,1]
  pred$up.95 <- pred.high[,2]
  
  pred.mat <- as.matrix(pred)
  
  d.1.predictions[i,] <- pred.mat
}
predictions <- d.1.predictions[-weekends,]
predictions <- predictions[-63,]
predictions = predictions[-62,]
predictions = predictions[-61,]
dim(predictions)
colnames(predictions) = colnames(pred)


# Actual close price 
real.v <- vector(length=62)
real <- getSymbols(t,src="yahoo", from=predictions[1,2],
                         to=predictions[60,2],auto.assign = F)
real.close <- real[,4]
real.close  <- str_split_fixed(real.close," ",1)
class(real.close)
real.close <- as.matrix(real.close)
real.close <- round(as.numeric(real.close),2)
real.close <- as.data.frame(real.close)

## 
predictions.mean = prediction


pred <- round(as.numeric(predictions[,4]),2)
real.pred <- cbind(real.close,pred)
dim(real.pred)

delta <- as.matrix((pred-real.close))
delta.abs <- abs(delta)
class(delta.abs)
delta.abs <- as.matrix(delta.abs)
mean(delta.abs)
median(delta.abs)

# Here we can see that the projected value was on average 2% off from the 
# actual close price over this time interval
ptc <- mean(delta.abs)/mean(real.close)
print(ptc)


