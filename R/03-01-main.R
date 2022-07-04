# Loading packages to facilitate the analysis

source("R/02-03-packages.R")

installOrLoadPackage(c('httr','XML','tidyquant','forecast','repr'))

source("R/03-04-get_data.R")
source("R/03-05-historical_declines.R")
source("R/03-06-scenario_building.R")
source("R/03-07-recovery_forecast.R")

# Identification of historical declines

## Get S&P 500 constituents from Wikipedia ##
url <- 'https://en.wikipedia.org/wiki/List_of_S%26P_500_companies'
wiki_data <- WikiData$new(url)
gicsSubIndustryList <- wiki_data$get_gics_sub_industry_list()

print(gicsSubIndustryList)

## Set-up start date and symbols ##
startDateTxt <- '1995-01-01'
gicsSubIndustry <- 'airlines'

wiki_data$make_prices(startDateTxt, gicsSubIndustry)

prices <- wiki_data$get_prices()
prices_df <- wiki_data$get_prices_df()

wiki_data$plot_prices(gicsSubIndustry)

# Analysis of historical declines and recoveries

## Find historical declines, i.e. decreasing prices in atleast five of six consecutive months ##

recessions <- Recessions$new(prices_df)
print(recessions$get_recessions())

recessions$plot_severity()

recessions$plot_duration()

recessions$plot_half()

# Scenario building
set.seed(67890)

scenario_builder <- ScenarioBuilder$new(
  recessions$get_severity(),
  recessions$get_severity_kde(),
  recessions$get_duration(),
  recessions$get_duration_kde(),
  recessions$get_half(),
  recessions$get_half_kde()
)

# Load a hypothetical three-year historical call volume data

startDateTxt <- '2017-01-01'
symbol <- '^GSPC'
scenario_builder$make_call_volume(startDateTxt, symbol)
scenario_builder$plot_call_volume()

# Call volume time series decomposition

scenario_builder$make_ets(0)
scenario_builder$plot_fit_ets()
print(scenario_builder$get_fit_ets())

# Infusion of scenarios to call volume time series

scenario <- list(severity = -0.4663777, duration = 67, half = 3)

scenario_builder$infuse_scenarios(scenario)

scenario_builder$plot_infused_scenarios()

# Combination of call volume time series components and scenarios

scenario_builder$combine_call_volume_scenarios()

scenario_builder$plot_future_states_b()
scenario_builder$plot_future_states_s()
scenario_builder$plot_future_states_fit()

# Forecasting call volume recovery

forecast <- ForecastCallVolumeRecovery$new(
  scenario_builder$get_call_volume(),
  scenario_builder$get_scenarios(),
  scenario_builder$get_fit_ets()
)

forecast$plot_forecast()
