library(R6)

Recessions <- R6Class("Recessions", 
  private = list(
    recessions = NULL,
    severity = NULL,
    severity_kde = NULL,
    duration = NULL,
    duration_kde = NULL,
    half = NULL,
    half_kde = NULL,
    make_recessions = function(prices_df) {
      starts <- c()
      for (i in 1:nrow(prices_df)) {
        if (sum(prices_df$delta[i:min(i+5, nrow(prices_df))] < 0, na.rm=TRUE) >= 5) {
          if (!is.null(starts)) {
            if (i - starts[length(starts)] > 6) { starts <- unique(c(starts, i)) }
          } else { starts <- i }
        }
      }
      ends <- c()
      for (i in starts) {
        candidate_idx <- which(prices_df$price[i] <= prices_df$price)
        candidate_idx <- candidate_idx[i + 5 < candidate_idx]
        if (length(candidate_idx) > 0) { ends <- c(ends, min(candidate_idx)) }
        else { ends <- c(ends, Inf) }
      }
      
      recession_intervals <- ends - starts
      recession_intervals <- recession_intervals[-Inf < recession_intervals & recession_intervals < Inf]
      
      inflxn_points <- c()
      decreases <- c()
      for (i in 1:length(starts)) {
        if (-Inf < starts[i] & ends[i] < Inf){
          j <- which.min(prices_df$price[starts[i]:ends[i]])
          inflxn_points <- c(inflxn_points, starts[i] + j - 1)
          decreases <- c(decreases, log1p(min(prices_df$price[starts[i]:ends[i]])) - log1p(prices_df$price[starts[i]]))
        } else if (starts[i] <= -Inf) {
          inflxn_points <- c(inflxn_points, -Inf)
        } else {
          inflxn_points <- c(inflxn_points,  Inf)
        }
      }
      inflection_intervals <- inflxn_points - starts
      inflection_intervals <- inflection_intervals[-Inf < inflection_intervals & inflection_intervals < Inf]
      
      recessions <- data.frame(start=prices_df$date[starts])
      recessions$bottom <- recessions$end <- recessions$severity <- recessions$severity <- recessions$duration <- recessions$half <- NA
      recessions$bottom <- prices_df$date[inflxn_points]
      recessions$end <- prices_df$date[ends]
      recessions$severity[1:length(decreases)] <- decreases
      recessions$duration[1:length(recession_intervals)] <- recession_intervals
      recessions$half[1:length(inflection_intervals)] <- inflection_intervals
      
      private$recessions <- recessions
    },
    compute_severity = function() {
      private$severity <- private$recessions$severity[!is.na(private$recessions$severity)]
      private$severity_kde <- density(private$severity, na.rm=TRUE)
    },
    compute_duration = function() {
      private$duration <- private$recessions$duration[!is.na(private$recessions$duration)]
      private$duration_kde <- density(private$duration, na.rm=TRUE)
    },
    compute_half = function() {
      private$half <- private$recessions$half[!is.na(private$recessions$half)]
      private$half_kde <- density(private$half, na.rm=TRUE)
    }
  ),
  public = list(
    initialize = function(prices_df) {
      private$make_recessions(prices_df)
      
      private$compute_severity()
      private$compute_duration()
      private$compute_half()
    },
    get_recessions = function() {
      return(private$recessions)
    },
    plot_severity = function() {
      hist(private$severity, prob=TRUE, main='Severity')
      lines(private$severity_kde)
    },
    plot_duration = function() {
      hist(private$duration, prob=TRUE, main='Duration')
      lines(private$duration_kde)
    },
    plot_half = function() {
      hist(private$half, prob=TRUE, main='Inflection')
      lines(private$half_kde)
    },
    get_severity = function() {
      return(private$severity)
    },
    get_severity_kde = function() {
      return(private$severity_kde)
    },
    get_duration = function() {
      return(private$duration)
    },
    get_duration_kde = function() {
      return(private$duration_kde)
    },
    get_half = function() {
      return(private$half)
    },
    get_half_kde = function() {
      return(private$half_kde)
    }
  )
)
