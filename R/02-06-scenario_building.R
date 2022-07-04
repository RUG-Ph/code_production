rdens <- function(n=n, density=density, data=data, kernel="gaussian") {
  width <- density$bw
  rkernel <- function(n) rnorm(n, sd=width)
  sample(data, n, replace=TRUE) + rkernel(n)
}

generate_scenarios <- function(
    severity,
    severity_kde,
    duration,
    duration_kde,
    half,
    half_kde
) {
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
  
  return(scenarios)
}

generate_call_volume <- function(startDateTxt, symbol) {
  startDate <- as.Date(startDateTxt)
  startYear <- as.numeric(format(startDate, '%Y'))
  startMonth <- as.numeric(format(startDate, '%m'))
  
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
}

infuse_scenarios <- function(scenario, model) {
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
  
  return(
    list(
      l_fits = l_fits,
      l_actuals = l_actuals,
      startNumeric = startNumeric,
      startYear = startYear,
      startMonth = startMonth,
      endNumeric = endNumeric,
      halfNumeric = halfNumeric,
      future_states = future_states,
      dates = dates
    )
  )
}

combine_call_volume_scenarios <- function(model, future_states, dates) {
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
  
  return(future_states)
}