rm(list=ls(all=T))
library(quantmod)
library(forecast)
library(gmailr)
library(tableHTML)


date.current <- Sys.Date()
H <- 5
new.dates <- seq(date.current,by ="day",length.out = H+1)
new.dates <- as.Date(new.dates[-1])


as.Date(date.current)

stock.ls <- list()


# Insert Securities you would like to see price forecasts for here
tick.v <- c("AMC", "PLTR", "SPY")


for (i in 1:length(tick.v)){
  x <- getSymbols(tick.v[i],src="yahoo",from="2013-05-10", to=date.current,
                  auto.assign = F)
  c.p <- x[,4]
  model.fit <- auto.arima(c.p,lambda="auto")
  price.forecast <- forecast(model.fit,h=H)
  pred.mean <- price.forecast$mean
  pred.high <- price.forecast$upper
  pred.low <- price.forecast$lower
  
  current.date <- rep(date.current,H)
  pred <- data.frame(current.date)
  pred$new.dates <- new.dates
  pred$mean <- pred.mean
  pred$low.80 <- pred.low[,1]
  pred$low.95 <- pred.low[,2]
  pred$up.80 <- pred.high[,1]
  pred$up.95 <- pred.high[,2]
  
  colnames(pred)[1] <- paste(tick.v[i],"(Current Date)")
  
  stock.ls[[i]] <- pred
  
    
}

print(stock.ls)

