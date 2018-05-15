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

log.status.acf <- Acf(log.status, na.action = na.pass, plot = FALSE)
#log.status.acf.table <- data.frame(log.status.acf$lag,  log.status.acf$acf)[-1,]
log.status.acf.table <- data.frame( log.status.acf$acf)[-1,]
write.csv(log.status.acf.table, file="/Users/chuamelia/Desktop/ameliachu.github.io/realtime_bike/logstatusacf.csv", row.names = FALSE)
