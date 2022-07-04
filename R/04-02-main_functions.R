# Loading packages to facilitate the analysis

source("R/02-03-packages.R")

installOrLoadPackage(c('httr','XML','tidyquant','forecast','repr'))

source("R/04-04-get_data.R")

# Identification of historical declines

## Get S&P 500 constituents from Wikipedia ##

# OK as expected
url <- 'https://en.wikipedia.org/wiki/List_of_S%26P_500_companies'
doc <- get_from_wikipedia(url)
gicsSubIndustryList <- make_gics_sub_industry_list(doc)

# Invalid URL
url <- 'https:/en.wikipedia.org/wiki/List_of_S%26P_500_companies'
doc <- get_from_wikipedia(url)
gicsSubIndustryList <- make_gics_sub_industry_list(doc)

# Wrong domain
url <- 'https://en.wikipedi.org/wiki/List_of_S%26P_500_companies'
doc <- get_from_wikipedia(url)
gicsSubIndustryList <- make_gics_sub_industry_list(doc)


# OK as expected

## Set-up start date and symbols ##
startDateTxt <- '1995-01-01'
gicsSubIndustry <- 'airlines'
symbol <- doc$symbol[tolower(doc$gics_sub_industry) == gicsSubIndustry]

## Get S&P 500 prices ##
prices <- get_prices(symbol, startDateTxt)

# Invalid date

## Set-up start date and symbols ##
startDateTxt <- '1995-01-32'
gicsSubIndustry <- 'airlines'
symbol <- doc$symbol[tolower(doc$gics_sub_industry) == gicsSubIndustry]

## Get S&P 500 prices ##
prices <- get_prices(symbol, startDateTxt)

# Empty symbol

## Set-up start date and symbols ##
startDateTxt <- '1995-01-01'
gicsSubIndustry <- 'xyz'
symbol <- doc$symbol[tolower(doc$gics_sub_industry) == gicsSubIndustry]

## Get S&P 500 prices ##
prices <- get_prices(symbol, startDateTxt)


