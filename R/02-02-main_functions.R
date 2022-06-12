# Loading packages to facilitate the analysis

installOrLoadPackage(c('httr','XML','tidyquant','forecast','repr'))

# Identification of historical declines

## Get S&P 500 constituents from Wikipedia ##
url <- 'https://en.wikipedia.org/wiki/List_of_S%26P_500_companies'
doc <- get_from_wikipedia(url)
gicsSubIndustryList <- make_gics_sub_industry_list(doc)

print(gicsSubIndustryList)

## Set-up start date and symbols ##
startDateTxt <- '1995-01-01'
gicsSubIndustry <- 'airlines'
symbol <- doc$symbol[tolower(doc$gics_sub_industry) == gicsSubIndustry]

## Get S&P 500 prices ##
prices <- get_prices(symbol, startDateTxt)
prices_df <- get_prices_df(prices)

plot(prices, main=gicsSubIndustry)

# Analysis of historical declines and recoveries

## Find historical declines, i.e. decreasing prices in atleast five of six consecutive months ##

recessions <- get_recessions(prices_df)

print(recessions)

severity <- recessions$severity[!is.na(recessions$severity)]
severity_kde <- density(severity, na.rm=TRUE)
hist(severity, prob=TRUE, main='Severity')
lines(severity_kde)

duration <- recessions$duration[!is.na(recessions$duration)]
duration_kde <- density(duration, na.rm=TRUE)
hist(duration, prob=TRUE, main='Duration')
lines(duration_kde)

half <- recessions$half[!is.na(recessions$half)]
half_kde <- density(half, na.rm=TRUE)
hist(half, prob=TRUE, main='Inflection')
lines(half_kde)

# Scenario building

rdens <- function(n=n, density=density, data=data, kernel="gaussian") {
  width <- density$bw
  rkernel <- function(n) rnorm(n, sd=width)
  sample(data, n, replace=TRUE) + rkernel(n)
}

set.seed(67890)
scenarios <- data.frame(severity = rdens(n=1000, density=severity_kde, data=severity)
                        ,duration = rdens(n=1000, density=duration_kde, data=duration)
                        ,half     = rdens(n=1000, density=half_kde,     data=half))

scenarios$duration <- round(scenarios$duration,0)
scenarios$half <- round(scenarios$half,0)
scenarios <- scenarios[complete.cases(scenarios),]
scenarios <- scenarios[scenarios$severity <  0,]
scenarios <- scenarios[scenarios$duration >= 6,]
scenarios <- scenarios[1 <= scenarios$half & scenarios$half < scenarios$duration,]
scenarios <- scenarios[sample(1:nrow(scenarios), size=100, replace=FALSE),]
head(scenarios, 5)

# Load a hypothetical three-year historical call volume data

startDateTxt <- '2017-01-01'
startDate <- as.Date(startDateTxt)
startYear <- as.numeric(format(startDate, '%Y'))
startMonth <- as.numeric(format(startDate, '%m'))
symbol <- '^GSPC'
call_volume <- tq_get(x = symbol, get = 'stock.prices', from = startDateTxt)
call_volume$sales <- call_volume$volume * call_volume$adjusted
call_volume$month <- format(call_volume$date, '%Y-%m')
volume <- aggregate(volume ~ month, data=call_volume, FUN=sum)
sales <- aggregate(sales ~ month, data=call_volume, FUN=sum)
call_volume <- merge(volume, sales, by='month', all=TRUE)
call_volume$volume <- call_volume$sales/call_volume$volume
call_volume$sales <- NULL
set.seed(54321)
mltplr <- 100*runif(n=1)
seasonalC <- exp(runif(n=12, min=-0.25, max=0.25))
seasonalC <- as.numeric(rep(seasonalC, length.out=nrow(call_volume)))
call_volume$volume <- call_volume$volume * mltplr * seasonalC
call_volume$volume <- round(call_volume$volume,0)
call_volume <- ts(as.numeric(call_volume$volume), frequency=12, start=c(startYear, startMonth))
plot(call_volume)

# Call volume time series decomposition

fit_ets <- ets(call_volume + 1, lambda = 0)
plot(fit_ets)
print(fit_ets)

Infusion of scenarios to call volume time series

scenario <- list(severity = -0.4663777, duration = 67, half = 3)
model <- fit_ets

base_series <- data.frame(date=as.Date(model$states[,'l']), l=as.numeric(model$states[,'l']))
base_series$delta_l <- c(NA, diff(base_series$l, lag=1))

startDate <- max(base_series$date)
for (i in nrow(base_series):2) {
  if (base_series[i-1,3] > 0 & base_series[i,3] < 0) {
    startDate <- base_series[i-1,'date']
    break
  }
}

duration <- scenario$duration
severity <- scenario$severity
half <- scenario$half

startNumeric <- as.numeric(format(startDate, '%Y')) + (as.numeric(format(startDate, '%m'))-1)/12
endNumeric <- startNumeric + duration/12
halfNumeric <- startNumeric + half/12

dates <- seq(from=startDate, by='month', length.out=duration + 1)
dates <- dates[!dates %in% base_series$date]
dates <- dates[order(dates)]

future_states <- data.frame(date = dates)
future_states <- merge(base_series, future_states, by='date', all=TRUE)
future_states$delta_l <- NULL
startIdx <- which(future_states$date == startDate)

lowest_point <- severity + future_states$l[startIdx]
delta_dec <- (lowest_point - future_states$l[startIdx])/half
seq_dec <- seq(from=future_states$l[startIdx], by=delta_dec, length.out=half)

delta_inc <- (future_states$l[startIdx] - lowest_point)/(duration - half)
seq_inc <- seq(from=lowest_point, by=delta_inc, length.out=1 + duration - half)
future_states$l_hat <- NA
future_states$l_hat[startIdx:nrow(future_states)] <- c(seq_dec, seq_inc)

startYear <- as.numeric(format(min(future_states$date), '%Y'))
startMonth <- as.numeric(format(min(future_states$date), '%m'))

l_actuals <- ts(future_states$l, frequency = 12, start=c(startYear, startMonth))
l_fits <- ts(future_states$l_hat, frequency = 12, start=c(startYear, startMonth))

options(repr.plot.width=15, repr.plot.height=7.5)
plot(l_fits, lwd=2, lty=2, col='blue', ylab='level')
lines(l_actuals, lwd=2, lty=1, col='black')

abline(v=startNumeric, lty=2, lwd=2, col='red')
abline(v=endNumeric, lty=2, lwd=2, col='green')
abline(v=halfNumeric, lty=2, lwd=2, col='orange')

# Combination of call volume time series components and scenarios

if (model$components[3] != 'N') { 
  future_states$s <- NA
  future_states$s[1:length(model$states[,'s1'])] <- model$states[,'s1']
  for (i in dates) {
    future_states$s[future_states$date == i] <- future_states$s[future_states$date == (as.Date(i) %m-% months(12))] 
  }
} else {
  future_states$s <- rep(x=0, times=nrow(future_states))
}

if (model$components[2] != 'N') {
  future_states$b <- NA
  future_states$b[1:length(model$states[,'b'])] <- model$states[,'b']
  if (model$components[4] == 'TRUE') { 
    phi <- model$par[['phi']]
  } else { phi <- 1 }
  for (i in dates) {
    future_states$b[future_states$date == i] <- phi * future_states$b[future_states$date == (as.Date(i) %m-% months(1))] 
  }
} else {
  future_states$b <- rep(x=0, times=nrow(future_states))
}

plot(ts(future_states$b, frequency = 12, start=c(startYear, startMonth)), lwd=2, lty=1, col='black', ylab='slope')
abline(v=startNumeric, lty=2, lwd=2, col='red')
abline(v=endNumeric, lty=2, lwd=2, col='green')
abline(v=halfNumeric, lty=2, lwd=2, col='orange')

plot(ts(future_states$s, frequency = 12, start=c(startYear, startMonth)), lwd=2, lty=1, col='black', ylab='season')
abline(v=startNumeric, lty=2, lwd=2, col='red')
abline(v=endNumeric, lty=2, lwd=2, col='green')
abline(v=halfNumeric, lty=2, lwd=2, col='orange')

future_states$fit <- expm1(ifelse(!is.na(future_states$l), future_states$l, future_states$l_hat) + 
                             future_states$b + future_states$s)

plot(ts(future_states$fit, frequency = 12, start=c(startYear, startMonth)), lwd=2, lty=1, col='black', ylab='call_volume')
abline(v=startNumeric, lty=2, lwd=2, col='red')
abline(v=endNumeric, lty=2, lwd=2, col='green')
abline(v=halfNumeric, lty=2, lwd=2, col='orange')

recovery_forecast <- function(scenario, model) {
  
  states_lts <- data.frame(date = as.Date(model$states[,'l']), l = as.numeric(model$states[,'l']))
  
  if (model$components[2] != 'N') {
    states_lts$b <- as.numeric(model$states[,'b' ])
  } else {
    states_lts$b <- rep(x=0, times=nrow(states_lts))
  }
  
  if (model$components[3] != 'N') { 
    states_lts$s <- as.numeric(model$states[,'s1']) 
  } else {
    states_lts$s <- rep(x=0, times=nrow(states_lts))
  }
  
  states_lts$delta_l <- c(NA, diff(states_lts$l, lag=1))
  states_lts$delta_b <- c(NA, diff(states_lts$b, lag=1))
  
  if (all(states_lts$delta_l > 0, na.rm=TRUE) | all(states_lts$delta_l < 0, na.rm=TRUE)) {
    monotone_l <- TRUE
  } else {
    monotone_l <- FALSE
  }
  
  if (all(states_lts$delta_b > 0, na.rm=TRUE) | all(states_lts$delta_b < 0, na.rm=TRUE)) {
    monotone_b <- TRUE
  } else {
    monotone_b <- FALSE
  }
  
  if (!monotone_l) {
    base_series <- states_lts[,c('date','l','delta_l')]
  } else if (monotone_l & !monotone_b) {
    base_series <- states_lts[,c('date','b','delta_b')]
  } else {
    base_series <- states_lts[,c('date','l','delta_l')]
  }
  
  startDate <- max(base_series$date)
  for (i in nrow(base_series):2) {
    if (base_series[i-1,3] > 0 & base_series[i,3] < 0) {
      startDate <- base_series[i-1,'date']
      break
    }
  }
  
  duration <- scenario$duration
  dates <- seq(from=startDate, by='month', length.out=1 + duration)
  dates <- dates[!dates %in% states_lts$date]
  dates <- dates[order(dates)]
  
  future_states <- data.frame(date = dates)
  future_states <- merge(states_lts, future_states, by=intersect(names(states_lts), names(future_states)), all=TRUE)
  future_states$delta_l <- future_states$delta_b <- NULL
  
  if (model$components[3] != 'N') {
    for (i in dates) {
      future_states$s[future_states$date == i] <- future_states$s[future_states$date == (as.Date(i) %m-% months(12))] 
    }
  } else {
    future_states$s <- rep(x=0, times=nrow(future_states))
  }
  
  if (model$components[2] != 'N') {
    if (model$components[4] == 'TRUE') { 
      phi <- model$par[['phi']]
    } else { phi <- 1 }
    for (i in dates) {
      future_states$b[future_states$date == i] <- phi * future_states$b[future_states$date == (as.Date(i) %m-% months(1))] 
    }
  } else {
    future_states$b <- rep(x=0, times=nrow(future_states))
  }
  
  severity <- scenario$severity
  half <- scenario$half
  startIdx <- which(future_states$date == startDate)
  
  lowest_point <- severity + future_states$l[startIdx]
  delta_dec <- (lowest_point - future_states$l[startIdx])/half
  seq_dec <- seq(from=future_states$l[startIdx], by=delta_dec, length.out=half)
  delta_inc <- (future_states$l[startIdx] - lowest_point)/(duration - half)
  seq_inc <- seq(from=lowest_point, by=delta_inc, length.out=1 + duration - half)
  future_states$l_hat <- NA
  future_states$l_hat[startIdx:nrow(future_states)] <- c(seq_dec, seq_inc)
  future_states$l[is.na(future_states$l)] <- future_states$l_hat[is.na(future_states$l)]
  future_states$l_hat <- NULL
  
  future_states$fit <- future_states$l + future_states$b + future_states$s
  
  return(future_states)
  
}

# Forecasting call volume recovery

call_volume_df <- data.frame(date=as.Date(call_volume), volume=as.numeric(call_volume))
for (i in 1:nrow(scenarios)) {
  temp0 <- recovery_forecast(scenario=scenarios[i,], model=fit_ets)
  temp0 <- temp0[,c('date','fit')]
  temp0$fit <- expm1(temp0$fit)
  colName <- paste('fit',formatC(i, width = 3, format = "d", flag = "0"), sep='_') 
  colnames(temp0)[colnames(temp0) == 'fit'] <- colName
  call_volume_df <- merge(call_volume_df, temp0, by='date', all=TRUE)
}
colNames <- colnames(call_volume_df)[grepl(pattern='fit', colnames(call_volume_df))]

custom_quantile <- function(x, prob, na.rm) {
  if (length(x[!is.na(x)]) >= 15) {
    quantile(x=x,probs=prob,na.rm=na.rm)
  } else return(NA)
}
call_volume_df$fit_ll     <- apply(X=call_volume_df[,colNames], MARGIN=1, FUN=custom_quantile, prob=0.15, na.rm=TRUE)
call_volume_df$fit_median <- apply(X=call_volume_df[,colNames], MARGIN=1, FUN=custom_quantile, prob=0.50, na.rm=TRUE)
call_volume_df$fit_ul     <- apply(X=call_volume_df[,colNames], MARGIN=1, FUN=custom_quantile, prob=0.85, na.rm=TRUE)
call_volume_df <- call_volume_df[complete.cases(call_volume_df[,c('fit_ll','fit_median','fit_ul')]),]

startDate  <- min(call_volume_df$date)
startYear  <- as.numeric(format(startDate, '%Y'))
startMonth <- as.numeric(format(startDate, '%m'))

actuals <- ts(call_volume_df$volume,     frequency=12, start=c(startYear, startMonth))
fits    <- ts(call_volume_df$fit_median, frequency=12, start=c(startYear, startMonth))
llimits <- ts(call_volume_df$fit_ll,     frequency=12, start=c(startYear, startMonth))
ulimits <- ts(call_volume_df$fit_ul,     frequency=12, start=c(startYear, startMonth))

endDate <- max(call_volume_df$date)
endYear <- as.numeric(format(endDate, '%Y'))
endMonth <- as.numeric(format(endDate, '%m'))

xlim <- c(startYear + (startMonth - 1)/12, endYear + (endMonth - 1)/12) 
ylim <- c(min(c(actuals,fits,llimits,ulimits), na.rm=TRUE), max(c(actuals,fits,llimits,ulimits), na.rm=TRUE))

options(repr.plot.width=15, repr.plot.height=7.5)
plot(fits, lwd=2, lty=1, col='blue', xlim=xlim, ylim=ylim, ylab='call_volume')
lines(ulimits, lwd=1, lty=2, col='blue')
lines(llimits, lwd=1, lty=2, col='blue')
lines(actuals, lwd=2, lty=1, col='black')

