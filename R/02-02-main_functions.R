# Loading packages to facilitate the analysis

source("R/02-03-packages.R")

installOrLoadPackage(c('httr','XML','tidyquant','forecast','repr'))

source("R/02-04-get_data.R")
source("R/02-05-historical_declines.R")
source("R/02-06-scenario_building.R")
source("R/02-07-recovery_forecast.R")

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
scenarios <- generate_scenarios(
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



# Forecasting call volume recovery

forecast <- forecast_call_volume_recovery(call_volume, scenarios)

options(repr.plot.width=15, repr.plot.height=7.5)
plot(forecast$fits, lwd=2, lty=1, col='blue', xlim=forecast$xlim, ylim=forecast$ylim, ylab='call_volume')
lines(forecast$ulimits, lwd=1, lty=2, col='blue')
lines(forecast$llimits, lwd=1, lty=2, col='blue')
lines(forecast$actuals, lwd=2, lty=1, col='black')

