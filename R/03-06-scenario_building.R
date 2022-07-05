library(R6)

ScenarioBuilder <- R6Class("ScenarioBuilder",
  private = list(
    scenarios = NULL,
    call_volume = NULL,
    fit_ets = NULL,
    l_fits = NULL,
    l_actuals = NULL,
    startNumeric = NULL,
    startYear = NULL,
    startMonth = NULL,
    endNumeric = NULL,
    halfNumeric = NULL,
    future_states = NULL,
    dates = NULL,
    rdens = function(n=n, density=density, data=data, kernel="gaussian") {
      width <- density$bw
      rkernel <- function(n) rnorm(n, sd=width)
      sample(data, n, replace=TRUE) + rkernel(n)
    },
    generate_scenarios = function(
      severity,
      severity_kde,
      duration,
      duration_kde,
      half,
      half_kde
      ) {
        scenarios <- data.frame(severity = private$rdens(n=1000, density=severity_kde, data=severity)
                                ,duration = private$rdens(n=1000, density=duration_kde, data=duration)
                                ,half     = private$rdens(n=1000, density=half_kde,     data=half))
        
        scenarios$duration <- round(scenarios$duration,0)
        scenarios$half <- round(scenarios$half,0)
        scenarios <- scenarios[complete.cases(scenarios),]
        scenarios <- scenarios[scenarios$severity <  0,]
        scenarios <- scenarios[scenarios$duration >= 6,]
        scenarios <- scenarios[1 <= scenarios$half & scenarios$half < scenarios$duration,]
        scenarios <- scenarios[sample(1:nrow(scenarios), size=100, replace=FALSE),]
        
        private$scenarios <- scenarios
      }
  ),
  public = list(
    initialize = function(severity, severity_kde, duration, duration_kde, half, half_kde) {
      private$generate_scenarios(severity, severity_kde, duration, duration_kde, half, half_kde)
    },
    get_scenarios = function() {
      return(private$scenarios)
    },
    make_call_volume = function(startDateTxt, symbol) {
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
      
      private$call_volume <- call_volume
    },
    plot_call_volume = function() {
      plot(private$call_volume)
    },
    get_call_volume = function() {
      return(private$call_volume)
    },
    make_ets = function(lambda) {
      private$fit_ets <- ets(private$call_volume + 1, lambda = lambda)
    },
    plot_fit_ets = function() {
      plot(private$fit_ets)
    },
    get_fit_ets = function() {
      return(private$fit_ets)
    },
    infuse_scenarios = function(scenario) {
      model <- private$fit_ets
      
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
      
      private$l_fits <- l_fits
      private$l_actuals <- l_actuals
      private$startNumeric <- startNumeric
      private$startYear <- startYear
      private$startMonth <- startMonth
      private$endNumeric <- endNumeric
      private$halfNumeric <- halfNumeric
      private$future_states <- future_states
      private$dates <- dates
    },
    plot_infused_scenarios = function() {
      options(repr.plot.width=15, repr.plot.height=7.5)
      plot(private$l_fits, lwd=2, lty=2, col='blue', ylab='level')
      lines(private$l_actuals, lwd=2, lty=1, col='black')
      
      abline(v=private$startNumeric, lty=2, lwd=2, col='red')
      abline(v=private$endNumeric, lty=2, lwd=2, col='green')
      abline(v=private$halfNumeric, lty=2, lwd=2, col='orange')
    },
    combine_call_volume_scenarios = function() {
      model <- private$fit_ets
      
      if (model$components[3] != 'N') { 
        private$future_states$s <- NA
        private$future_states$s[1:length(model$states[,'s1'])] <- model$states[,'s1']
        for (i in private$dates) {
          private$future_states$s[private$future_states$date == i] <- private$future_states$s[private$future_states$date == (as.Date(i) %m-% months(12))] 
        }
      } else {
        private$future_states$s <- rep(x=0, times=nrow(private$future_states))
      }
      
      if (model$components[2] != 'N') {
        private$future_states$b <- NA
        private$future_states$b[1:length(model$states[,'b'])] <- model$states[,'b']
        if (model$components[4] == 'TRUE') { 
          phi <- model$par[['phi']]
        } else { phi <- 1 }
        for (i in private$dates) {
          private$future_states$b[private$future_states$date == i] <- phi * private$future_states$b[private$future_states$date == (as.Date(i) %m-% months(1))] 
        }
      } else {
        private$future_states$b <- rep(x=0, times=nrow(private$future_states))
      }
      
      private$future_states$fit <- expm1(ifelse(!is.na(private$future_states$l), private$future_states$l, private$future_states$l_hat) + 
                                   private$future_states$b + private$future_states$s)
    },
    plot_future_states_b = function() {
      plot(ts(private$future_states$b, frequency = 12, start=c(private$startYear, private$startMonth)), lwd=2, lty=1, col='black', ylab='slope')
      abline(v=private$startNumeric, lty=2, lwd=2, col='red')
      abline(v=private$endNumeric, lty=2, lwd=2, col='green')
      abline(v=private$halfNumeric, lty=2, lwd=2, col='orange')
    },
    plot_future_states_s = function() {
      plot(ts(private$future_states$s, frequency = 12, start=c(private$startYear, private$startMonth)), lwd=2, lty=1, col='black', ylab='season')
      abline(v=private$startNumeric, lty=2, lwd=2, col='red')
      abline(v=private$endNumeric, lty=2, lwd=2, col='green')
      abline(v=private$halfNumeric, lty=2, lwd=2, col='orange')
    },
    plot_future_states_fit = function() {
      plot(ts(private$future_states$fit, frequency = 12, start=c(private$startYear, private$startMonth)), lwd=2, lty=1, col='black', ylab='call_volume')
      abline(v=private$startNumeric, lty=2, lwd=2, col='red')
      abline(v=private$endNumeric, lty=2, lwd=2, col='green')
      abline(v=private$halfNumeric, lty=2, lwd=2, col='orange')
    }
  )
)



