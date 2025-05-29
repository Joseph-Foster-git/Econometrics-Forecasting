library(lgarch)
goose <- read.csv("C:/Users/josep/uvic/Econ345/goose_competition_spring_2024.csv")

#Setting up our lagged variable
goose$cyclists_lag <- NA
goose$cyclists_lag <- glag(goose$cyclists)

m1 <- lm(cyclists ~ 1 + Weekend + cyclists_lag +January+February+March+April+May+June+July+August+September+October+November, data=goose)
sum1 <- summary(m1)
sum1

##We will start our forecasts at the first NA observation in the cyclists column
start <- min(which(is.na(goose$cyclists)))
##We will end our forecasts at the last observation in the dataset
end <- nrow(goose)

int <- seq(start, end, 1)

##Creating a new variable that stores the prediction
goose$bikes_pred <- NA  
forecast <- predict(m1, newdata = goose[start:end, ]) 
goose$cyclists[start:end] <- forecast
goose$bikes_pred[start:end] <- forecast

index <- 2
for (i in start+1:end) {
  goose$cyclists_lag <- glag(goose$cyclists)
  
  m2 <- lm(cyclists ~ 1 + trend + Weekend + cyclists_lag +January+February+March+April+May+June+July+August+September+October+November, data=goose)
  forecast[index] <- predict(m2, newdata = goose[i, ])
  goose$cyclists[i] <- forecast[index]
  goose$bikes_pred[i] <- forecast[index]
  index <- index+1
  if (index ==20) {
    break
  }
}

#Clean up variable
goose$cyclists[start:end] <- NA
#To connect the prediction with data in the graph
goose$bikes_pred[start-1] <- goose$cyclists[start-1]

plot(goose$cyclists,ylab="# of Cyclists",xlab = "Day",xlim = c(500, 1920),ylim = c(0,4000),type="l", col="gray70",main="Cyclists on the Galloping Goose Trail")
lines(fitted(m1), col="darkgoldenrod2")
lines(goose$bikes_pred, col="darkblue")

goose$bikes_pred[1900:end]


