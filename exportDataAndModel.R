library("forecast")
library("tseries")
data <- read.csv("/Users/chuamelia/Google Drive/Forecasting Time Series/citi-bike/ts-realtime-analysis-bike/status_161.csv")
date <- as.POSIXlt(data$last_updated)
time <- 1:length(date)
status <- data$num_bikes_available + 1 

log.status <- log(status)
diff.log.status <- c(NA, diff(log.status))
diff2.log.status <- c(NA, diff(diff.log.status))

diff.log.status.table <- data.frame(date,  diff.log.status)
write.csv(diff.log.status.table, file="/Users/chuamelia/Desktop/ameliachu.github.io/realtime_bike/difflogstatus.csv", row.names = FALSE)

diff2.log.status.table <- data.frame(date,  diff2.log.status)
write.csv(diff2.log.status.table, file="/Users/chuamelia/Desktop/ameliachu.github.io/realtime_bike/diff2logstatus.csv", row.names = FALSE)

log.status.acf <- acf(log.status, na.action = na.pass, plot = FALSE)
#log.status.acf.table <- data.frame(log.status.acf$lag,  log.status.acf$acf)[-1,]
log.status.acf.table <- data.frame( log.status.acf$acf)[-1,]
log.status.acf.table <- as.data.frame(log.status.acf$acf)[-1,]
log.status.acf.table <- as.data.frame(log.status.acf.table)
names(log.status.acf.table) <- c("acf")
write.csv(log.status.acf.table, file="/Users/chuamelia/Desktop/ameliachu.github.io/realtime_bike/logstatusacf.csv", row.names = FALSE)

log.status.pacf <- Pacf(log.status, na.action = na.pass, plot = FALSE)
#log.status.pacf.table <- data.frame(log.status.acf$lag,  log.status.acf$pacf)[-1,]
log.status.pacf.table <- data.frame( log.status.pacf$acf)
names(log.status.pacf.table) <- c("pacf")
write.csv(log.status.pacf.table, file="/Users/chuamelia/Desktop/ameliachu.github.io/realtime_bike/logstatuspacf.csv", row.names = FALSE)


diff2.log.status.acf <- acf(diff2.log.status, na.action = na.pass, plot = FALSE)
#diff2.log.status.acf.table <- data.frame(diff2.log.status.acf$lag,  diff2.log.status.acf$acf)[-1,]
diff2.log.status.acf.table <- as.data.frame(diff2.log.status.acf$acf)[-1,]
diff2.log.status.acf.table <- as.data.frame(diff2.log.status.acf.table)
names(diff2.log.status.acf.table) <- c("acf")
write.csv(diff2.log.status.acf.table, file="/Users/chuamelia/Desktop/ameliachu.github.io/realtime_bike/diff2logstatusacf.csv", row.names = FALSE)

diff2.log.status.pacf <- Pacf(diff2.log.status, na.action = na.pass, plot = FALSE)
#diff2.log.status.pacf.table <- data.frame(diff2.log.status.acf$lag,  diff2.log.status.acf$pacf)[-1,]
diff2.log.status.pacf.table <- data.frame( diff2.log.status.pacf$acf)
names(diff2.log.status.pacf.table) <- c("pacf")
write.csv(diff2.log.status.pacf.table, file="/Users/chuamelia/Desktop/ameliachu.github.io/realtime_bike/diff2logstatuspacf.csv", row.names = FALSE)
