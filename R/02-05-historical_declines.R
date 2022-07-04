get_recessions <- function(prices_df) {
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
  
  return(recessions)
}