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


set.seed(67890)
scenarios <- generation_scenarios(
  severity,
  severity_kde,
  duration,
  duration_kde,
  half,
  half_kde
)
head(scenarios, 5)

# Load a hypothetical three-year historical call volume data

startDateTxt <- '2017-01-01'
symbol <- '^GSPC'

call_volume <- generate_call_volume(startDateTxt, symbol)
plot(call_volume)

# Call volume time series decomposition

fit_ets <- ets(call_volume + 1, lambda = 0)
plot(fit_ets)
print(fit_ets)

# Infusion of scenarios to call volume time series

scenario <- list(severity = -0.4663777, duration = 67, half = 3)
model <- fit_ets

infused <- infuse_scenarios(scenario, model)

options(repr.plot.width=15, repr.plot.height=7.5)
plot(infused$l_fits, lwd=2, lty=2, col='blue', ylab='level')
lines(infused$l_actuals, lwd=2, lty=1, col='black')

abline(v=infused$startNumeric, lty=2, lwd=2, col='red')
abline(v=infused$endNumeric, lty=2, lwd=2, col='green')
abline(v=infused$halfNumeric, lty=2, lwd=2, col='orange')

# Combination of call volume time series components and scenarios

future_states <- combine_call_volume_scenarios(model, infused$future_states, infused$dates)

plot(ts(future_states$b, frequency = 12, start=c(infused$startYear, infused$startMonth)), lwd=2, lty=1, col='black', ylab='slope')
abline(v=infused$startNumeric, lty=2, lwd=2, col='red')
abline(v=infused$endNumeric, lty=2, lwd=2, col='green')
abline(v=infused$halfNumeric, lty=2, lwd=2, col='orange')

plot(ts(future_states$s, frequency = 12, start=c(infused$startYear, infused$startMonth)), lwd=2, lty=1, col='black', ylab='season')
abline(v=infused$startNumeric, lty=2, lwd=2, col='red')
abline(v=infused$endNumeric, lty=2, lwd=2, col='green')
abline(v=infused$halfNumeric, lty=2, lwd=2, col='orange')

future_states$fit <- expm1(ifelse(!is.na(future_states$l), future_states$l, future_states$l_hat) + 
                             future_states$b + future_states$s)

plot(ts(future_states$fit, frequency = 12, start=c(infused$startYear, infused$startMonth)), lwd=2, lty=1, col='black', ylab='call_volume')
abline(v=infused$startNumeric, lty=2, lwd=2, col='red')
abline(v=infused$endNumeric, lty=2, lwd=2, col='green')
abline(v=infused$halfNumeric, lty=2, lwd=2, col='orange')

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

