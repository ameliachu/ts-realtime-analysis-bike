#' ---	
#' title: "realtime-analysis-gh"	
#' author: "Amelia Chu"	
#' date: "6/26/2018"	
#' output: rmarkdown::github_document	
#' ---	
#' 	
#' 	
library("forecast")	
library("tseries")	
#' 	
#' 	
#' Loading in Data	
#' 	
data <- read.csv("/Users/chuamelia/Google Drive/Forecasting Time Series/citi-bike/ts-realtime-analysis-bike/status_161.csv")	
date <- as.POSIXlt(data$last_updated)	
time <- 1:length(date)	
status <- data$num_bikes_available + 1 	
#' 	
#' 	
#' Taking logs and differences	
#' 	
log.status <- log(status)	
diff.log.status <- c(NA, diff(log.status))	
diff2.log.status <- c(NA, diff(diff.log.status))	
#' 	
#' 	
#' ### Part 1: Identify Potential p, d, q for an ARIMA(p, d, q) Model	
#' 	
#' **Figure 2. Plotting timeseries of Bikes Available, its log and differenced logs.**	
#' 	
par(mfrow=c(2,2))	
plot(date, status, type="l",	
     xlab="Date", ylab="Bikes Available")	
plot(date, log.status, type="l",	
     xlab="Date", ylab="Log Bikes Available")	
plot(date, diff.log.status, type="l",	
     xlab="Date", ylab="Differenced Log Bikes Available")	
plot(date, diff2.log.status, type="l",	
     xlab="Date", ylab="Differenced2 Log Bikes Available")	
#' 	
#' 	
#' 	
diff.log.status.table <- data.frame(date,  diff.log.status)	
write.csv(diff.log.status.table, file="/Users/chuamelia/Desktop/ameliachu.github.io/realtime_bike/difflogstatus.csv", row.names = FALSE)	
	
diff2.log.status.table <- data.frame(date,  diff2.log.status)	
write.csv(diff2.log.status.table, file="/Users/chuamelia/Desktop/ameliachu.github.io/realtime_bike/diff2logstatus.csv", row.names = FALSE)	
#' 	
#' 	
#' **Figures 2b, 3a, 3b. Timeseries of Log Bikes Available and ACF/PACF**	
#' 	
# Time series plot	
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))	
plot(date, log.status, type="l",	
     xlab="Date", ylab="Log Bikes Available")	
	
# ACF and PACF	
	
Acf(log.status, na.action = na.pass)	
log.status.acf <- Acf(log.status, na.action = na.pass, plot = FALSE)	
pacf(log.status, na.action = na.pass)	
log.status.acf.table <- data.frame(log.status.acf$lag,  log.status.acf$acf)[-1,]	
#' 	
#' 	
#' **Figures 2c, 3c, 3d. Timeseries of Differenced Log Bikes Available and ACF/PACF**	
#' 	
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))	
# Time series plot	
plot(date, diff.log.status, type="l",	
     xlab="Date", ylab="Differenced Log Bikes Available")	
# ACF and PACF	
Acf(diff.log.status, na.action = na.pass)	
pacf(diff.log.status, na.action = na.pass)	
#' 	
#' 	
#' **Figures 2d, 3e, 3f. Timeseries of Twice-Differenced Log Bikes Available and ACF/PACF**	
#' 	
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))	
# Time series plot	
plot(date, diff2.log.status, type="l",	
     xlab="Date", ylab="Differenced2 Log Bikes Available")	
# ACF and PACF	
Acf(diff2.log.status, na.action = na.pass)	
pacf(diff2.log.status, na.action = na.pass)	
#' 	
#' 	
#' 	
#Fig-3a	
log.status.acf <- acf(log.status, na.action = na.pass, plot = FALSE)	
#log.status.acf.table <- data.frame(log.status.acf$lag,  log.status.acf$acf)[-1,]	
log.status.acf.table <- data.frame( log.status.acf$acf)[-1,]	
log.status.acf.table <- as.data.frame(log.status.acf$acf)[-1,]	
log.status.acf.table <- as.data.frame(log.status.acf.table)	
names(log.status.acf.table) <- c("acf")	
write.csv(log.status.acf.table, file="/Users/chuamelia/Desktop/ameliachu.github.io/realtime_bike/logstatusacf.csv", row.names = FALSE)	
#Fig-3b	
log.status.pacf <- Pacf(log.status, na.action = na.pass, plot = FALSE)	
#log.status.pacf.table <- data.frame(log.status.acf$lag,  log.status.acf$pacf)[-1,]	
log.status.pacf.table <- data.frame( log.status.pacf$acf)	
names(log.status.pacf.table) <- c("pacf")	
write.csv(log.status.pacf.table, file="/Users/chuamelia/Desktop/ameliachu.github.io/realtime_bike/logstatuspacf.csv", row.names = FALSE)	
	
#Fig-3d	
diff2.log.status.acf <- acf(diff2.log.status, na.action = na.pass, plot = FALSE)	
#diff2.log.status.acf.table <- data.frame(diff2.log.status.acf$lag,  diff2.log.status.acf$acf)[-1,]	
diff2.log.status.acf.table <- as.data.frame(diff2.log.status.acf$acf)[-1,]	
diff2.log.status.acf.table <- as.data.frame(diff2.log.status.acf.table)	
names(diff2.log.status.acf.table) <- c("acf")	
write.csv(diff2.log.status.acf.table, file="/Users/chuamelia/Desktop/ameliachu.github.io/realtime_bike/diff2logstatusacf.csv", row.names = FALSE)	
	
#Fig-3e	
diff2.log.status.pacf <- Pacf(diff2.log.status, na.action = na.pass, plot = FALSE)	
#diff2.log.status.pacf.table <- data.frame(diff2.log.status.acf$lag,  diff2.log.status.acf$pacf)[-1,]	
diff2.log.status.pacf.table <- data.frame( diff2.log.status.pacf$acf)	
names(diff2.log.status.pacf.table) <- c("pacf")	
write.csv(diff2.log.status.pacf.table, file="/Users/chuamelia/Desktop/ameliachu.github.io/realtime_bike/diff2logstatuspacf.csv", row.names = FALSE)	
#' 	
#' 	
#' ### Part 2: Using AICC to Identify Best p, q for ARIMA(p, 1, q) Model	
#' 	
#' **Figure 4. Computing the AICc values for ARIMA Candidate Models**	
#' 	
d <- 1	
	
# choose p, q with AICc	
for (include.constant in c(FALSE, TRUE)) {	
    for (p in 0:4) {	
        for (q in 0:4) {	
            # work-around bug in R by manually differencing	
            fit <- Arima(diff(log.status), c(p,0,q),	
                         include.constant=include.constant, method="ML")	
             cat("ARIMA",	
                "(", p, ",", d, ",", q, ")",	
                "(constant=", include.constant, ")",	
                " : ", fit$aicc, "\n", sep="")	
             #cat( p, ":", d, ":", q, ":",	
            #     ":", include.constant, "",	
             #    " : ", fit$aicc, "\n", sep="")	
        }	
    }	
}	
	
#' 	
#' 	
#' ** Figure 5. Final Estimates of ARIMA Parameters **	
#' 	
#' Here is code to fit the ARIMA model, then compute residuals and the fitted values:	
#' 	
#' 	
fit.mean <- Arima(log.status, c(3, 1, 3), include.constant=FALSE)	
summary(fit.mean)	
#' 	
#' 	
#' ### Part 3: Residuals of the ARIMA Model	
#' 	
#' **Figure 6. Modified Box-Pierce (Ljung-Box) Diagnostics**	
#' 	
#' 	
Box.test(diff.log.status, lag = 24, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)	
Box.test(diff.log.status, lag = 36, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)	
Box.test(diff.log.status, lag = 48, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)	
#' 	
#' 	
#' Here are the residuals, with the last 10 residuals printed out:	
#' 	
resid <- residuals(fit.mean)	
tail(resid, n=10)	
#' 	
#' 	
#' 	
#' Here are the fitted values, with the last 10 fitted values printed out:	
#' 	
f <- fitted.values(fit.mean)	
tail(f, n=10)	
#' 	
#' 	
#' **Figure 7. ARIMA Residual Plots**	
#' 	
#' Here is a plot of the residuals:	
#' 	
#' 	
plot(date, resid, type="l",	
     xlab="Date", ylab="Bikes Available Residuals")	
#' 	
#' 	
#' 	
arima.resid.ts.table <- data.frame(date,resid)	
write.csv(arima.resid.ts.table, file="/Users/chuamelia/Desktop/ameliachu.github.io/realtime_bike/arima-resid-ts.csv", row.names = FALSE)	
#' 	
#' 	
#' Here are the ACF and PACF of the residuals:	
#' 	
#' 	
# ACF and PACF	
par(mfrow=c(1,2))	
Acf(resid, na.action = na.pass)	
Pacf(resid, na.action = na.pass)	
#' 	
#' 	
#' 	
arima.resid.acf <- Acf(resid, na.action = na.pass, plot = FALSE)	
arima.resid.acf.table <- as.data.frame( arima.resid.acf$acf)[-1,]	
arima.resid.acf.table <- as.data.frame( arima.resid.acf.table)	
names(arima.resid.acf.table) <- c("acf")	
write.csv(arima.resid.acf.table, file="/Users/chuamelia/Desktop/ameliachu.github.io/realtime_bike/arima-resid-acf.csv", row.names = FALSE)	
	
arima.resid.pacf <- Pacf(resid, na.action = na.pass, plot = FALSE)	
arima.resid.pacf.table <- data.frame( arima.resid.pacf$acf)	
names(arima.resid.pacf.table) <- c("pacf")	
write.csv(arima.resid.pacf.table, file="/Users/chuamelia/Desktop/ameliachu.github.io/realtime_bike/arima-resid-pacf.csv", row.names = FALSE)	
#' 	
#' 	
#' Here are the ACF and PACF of the squared-residuals:	
#' 	
#' 	
# ACF, PACF of squared residuals.	
par(mfrow=c(1,2))	
Acf(resid^2, na.action = na.pass)	
Pacf(resid^2, na.action = na.pass)	
#' 	
#' 	
#' 	
arima.resid2.acf <- Acf(resid^2, na.action = na.pass, plot = FALSE)	
arima.resid2.acf.table <- as.data.frame( arima.resid2.acf$acf)[-1,]	
arima.resid2.acf.table <- as.data.frame( arima.resid2.acf.table)	
names(arima.resid2.acf.table) <- c("acf")	
write.csv(arima.resid2.acf.table, file="/Users/chuamelia/Desktop/ameliachu.github.io/realtime_bike/arima-resid2-acf.csv", row.names = FALSE)	
	
arima.resid2.pacf <- Pacf(resid^2, na.action = na.pass, plot = FALSE)	
arima.resid2.pacf.table <- data.frame( arima.resid2.pacf$acf)	
names(arima.resid2.pacf.table) <- c("pacf")	
write.csv(arima.resid2.pacf.table, file="/Users/chuamelia/Desktop/ameliachu.github.io/realtime_bike/arima-resid2-pacf.csv", row.names = FALSE)	
#' 	
#' 	
#' ### Part 4: GARCH Model Selected	
#' **Figure 8. AICc for Candidate ARCH/GARCH Models**	
#' 	
#' Here are the AICc values for the ARCH(q):	
#' 	
#' 	
q <- 0:10	
loglik <- rep(NA, length(q))	
N <- length(resid)	
	
for (i in 1:length(q)) {	
    if (q[i] == 0) {	
        loglik[i] <- -0.5 * N * (1 + log(2 * pi * mean(resid^2)))	
    } else {	
        fit <- garch(resid, c(0,q[i]), trace=FALSE)	
        loglik[i] <- logLik(fit)	
    }	
}	
	
k <- q + 1	
aicc <- -2 * loglik  + 2 * k * N / (N - k - 1)	
	
print(data.frame(q, loglik, aicc))	
	
#' 	
#' 	
#' 	
#' 	
#Exporting the arch aicc table for website:	
arch.aicc.table <- data.frame(q, loglik, aicc)	
write.csv(arch.aicc.table, file="/Users/chuamelia/Desktop/ameliachu.github.io/realtime_bike/arch-aicc.csv", row.names = FALSE)	
#' 	
#' 	
#' Here is the AICc for the GARCH(1,1):	
#' 	
#' 	
fit <- garch(resid, c(1,1), trace=FALSE)	
loglik <- logLik(fit)	
k <- 2	
aicc <- -2 * loglik  + 2 * k * N / (N - k - 1)	
	
print(data.frame(loglik, aicc))	
#' 	
#' 	
#' 	
# Exporting the garch aicc table for website:	
garch.aicc.table <- data.frame(loglik, aicc)	
write.csv(garch.aicc.table, file="/Users/chuamelia/Desktop/ameliachu.github.io/realtime_bike/garch-aicc.csv", row.names = FALSE)	
#' 	
#' 	
#' **Figure 9. Model Output for GARCH**	
#' 	
#' Here are the summary and log likelihood of the selected model:	
#' 	
#' 	
fit.var <- garch(resid, c(1,1), trace=FALSE)	
summary(fit.var)	
logLik(fit.var)	
#' 	
#' 	
#' ### Part 5: Compare One-Step Ahead Forecast Intervals of ARIMA vs. ARIMA-ARCH Model	
#' **Figure 10. Forecast Intervals for ARIMA Model**	
#' Here is the one step ahead forecast and 95% forecast interval:	
#' 	
#' 	
forecast(fit.mean, h=1, level = 95)	
#' 	
#' 	
#' **Figure 10. Forecast Intervals for ARIMA-ARCH Model**	
#' 	
#' 	
a0 <- coefficients(fit.var)["a0"]	
a1 <- coefficients(fit.var)["a1"]	
b1 <- coefficients(fit.var)["b1"]	
f1 <- 1.943208	
	
# conditional variance:	
h1<- a0 + a1*(tail(fit.mean$residuals,1)^2) +b1 * tail(fit.var$fit[,1],1)	
	
	
# Finally, we compute the 95% forecast interval:	
f1 + -1 * 1.96 * sqrt(h1)	
f1 + 1 * 1.96 * sqrt(h1)	
#' 	
#' 	
#' 	
#' ### Part 6: Conditional Variances	
#' 	
#' Here are the conditional variances, with the last 10 values printed out:	
#' 	
ht <- fit.var$fit[,1]^2	
tail(ht, n=10)	
#' 	
#' 	
#' **Figure 11. Conditional Variances**	
#' Here is a plot of the conditional variances:	
#' 	
#' 	
# Add plot of the conditional variances	
plot(date, ht, type="l", col=4)	
#' 	
#' 	
#' 	
# Exporting the conditional variances table for website:	
condition.var.table <- data.frame(date, ht)	
write.csv(condition.var.table, file="/Users/chuamelia/Desktop/ameliachu.github.io/realtime_bike/condition-var.csv", row.names = FALSE)	
#' 	
#' 	
#' ### Part 7 & 9: Forecast Intervals of the ARIMA/ARIMA-ARCH Model	
#' 	
plot(forecast(fit.mean, h=100, level=95), col=2)	
#' 	
#' 	
#' 	
# Exporting the ARIMA forecast intervals for ts chart on website:	
arima.interval <- data.frame(forecast(fit.mean, h=100, level=95))	
log.status.actuals <- c(paste(log.status,";", log.status,";",log.status,sep=""), rep("null;null;null", nrow(arima.interval)))	
log.status.forecasts <- c(rep("null;null;null", length(log.status)), paste(arima.interval$Lo.95,";", arima.interval$Point.Forecast,";", arima.interval$Hi.95,sep=""))	
	
time <- 1:length(log.status.actuals)	
	
arima.forecasts <- data.frame(time, log.status.actuals, log.status.forecasts)	
write.csv(arima.forecasts, file="/Users/chuamelia/Desktop/ameliachu.github.io/realtime_bike/12a_arima-forecast-int.csv", row.names = FALSE)	
#' 	
#' 	
#' Defining the Upper and Lower Bounds of the 95% Forecast Interval	
#' 	
upper <- f + 1.96 * sqrt(ht)	
lower <- f - 1.96 * sqrt(ht)	
#' 	
#' 	
#' **Figure 12b. Log Bikes Available Forecast Intervals**	
#' Here is a time series plot which simultaneously shows the log bikes available,	
#' together with the ARIMA-ARCH one-step-ahead 95% forecast intervals based on	
#' information available in the previous day:	
#' 	
#' 	
plot(date, log.status, type="l")	
lines(date, upper, lty=2, col=2)	
lines(date,  lower, lty=2, col=2)	
#' 	
#' 	
#' 	
# Exporting the ARIMA-ARCH forecast intervals for ts chart on website:	
log.status.intervals <- paste(upper,";", log.status,";", lower,sep="")	
forecast.int <- data.frame(date, log.status.intervals)	
write.csv(forecast.int, file="/Users/chuamelia/Desktop/ameliachu.github.io/realtime_bike/12b_forecast-int.csv", row.names = FALSE)	
#' 	
#' 	
#' ### Part 8: Residuals of the ARIMA-ARCH Model	
#' 	
#' Here is a normal probability plot of the ARCH residuals.	
#' 	
#' 	
library("e1071")  	
# Here we compute the arch residuals:	
	
resid.arch <- resid / sqrt(ht)	
	
plot(date, resid.arch, col=4, type="l")	
#' 	
#' 	
#' Here is the normal probability plot:	
#' 	
qqnorm(resid.arch)	
qqline(resid.arch)	
	
kurtosis(resid.arch, na.rm=TRUE)	
#' 	
#' 	
#' 	
qq.resid.arch <- data.frame(qqnorm(resid.arch, plot.it = FALSE))	
write.csv(qq.resid.arch, file="/Users/chuamelia/Desktop/ameliachu.github.io/realtime_bike/13_qqplot.csv", row.names = FALSE)	
#' 	
#' 	
#' 	
# https://stackoverflow.com/questions/37897274/plot-lm-extracting-numbers-labelled-in-the-diagnostic-q-q-plot	
resid.arch <- na.omit(resid.arch)	
x <- sort(abs(resid.arch), decreasing = TRUE)	
x[1:4]	
#' 	
#' 	
#' Here is a count of how many prediction interval failures there were:	
#' 	
#' 	
# Count the number of times the prediciton	
# interval failed:	
	
sum(abs(resid.arch) >  1.96, na.rm=TRUE)	
#' 	
#' 	
#' The number of prediction intervals is:	
#' 	
sum(!is.na(resid.arch))	
#' 	
