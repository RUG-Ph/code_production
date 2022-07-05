
#' Get S&P 500 Constituents from Wikipedia
#'
#' Accesses Wikipedia and scrapes the table.
#'
#' @param url Wikipedia URL for S&P 500 companies
#'
#' @return Data.frame of the scraped data from Wikipedia 
#' @export
#'
#' @examples
#' \dontrun{
#' url <- 'https://en.wikipedia.org/wiki/List_of_S%26P_500_companies'
#' doc <- get_from_wikipedia(url)
#' }
get_from_wikipedia <- function(url) {
  checkmate::assert_string(url, pattern = "(https?|ftp)://[^\\s/$.?#].[^\\s]*")

  doc <- tryCatch(
    {
      response <- GET(url)
      table <- readHTMLTable(doc=content(response, "text"))
    },
    error = function(e) {
      print(e)
    }
  )

  doc <- doc$constituents
  doc[,colnames(doc)] <- lapply(doc[,colnames(doc)], FUN=as.character)
  doc_col_names <- as.character(doc[1,])
  doc_col_names <- gsub(pattern='[^A-Za-z0-9]', replacement='_', doc_col_names)
  doc_col_names <- gsub(pattern='_{2,}', replacement='_', doc_col_names)
  doc_col_names <- gsub(pattern='^_{1,}|_{1,}$', replacement='', doc_col_names)
  doc_col_names <- tolower(doc_col_names)
  colnames(doc) <- doc_col_names
  doc <- doc[-1,]
  
  return(doc)
}


#' Get the list of GICS Sub Industries
#'
#' Produces a list of unique GICS sub industries.
#'
#' @param doc Data.frame S&P 500 table scraped from Wikipedia
#'
#' @return Character vector of GICS sub industry categories
#' @export
#'
#' @examples
#' \dontrun{
#' url <- 'https://en.wikipedia.org/wiki/List_of_S%26P_500_companies'
#' doc <- get_from_wikipedia(url)
#' gicsSubIndustryList <- make_gics_sub_industry_list(doc)
#' }
make_gics_sub_industry_list <- function(doc) {
  gicsSubIndustryList <- tolower(unique(doc$gics_sub_industry))
  gicsSubIndustryList <- gicsSubIndustryList[order(gicsSubIndustryList)]
  
  return(gicsSubIndustryList)
}

#' Get Stock Prices from the Web
#'
#' Uses tidyquant package to retrieve daily stock prices.
#'
#' @param symbol Three letter company stock symbol
#' @param startDateTxt Character date with yyyy-mm-dd format
#'
#' @return Time series data of monthly prices by year
#' @export
#'
#' @examples
#' \dontrun{
#' url <- 'https://en.wikipedia.org/wiki/List_of_S%26P_500_companies'
#' doc <- get_from_wikipedia(url)
#' 
#' startDateTxt <- '1995-01-01'
#' gicsSubIndustry <- 'airlines'
#' symbol <- doc$symbol[tolower(doc$gics_sub_industry) == gicsSubIndustry]
#' prices <- get_prices(symbol, startDateTxt)
#' }
get_prices <- function(symbol, startDateTxt) {
  checkmate::assert_character(symbol, min.len = 1)
  checkmate::assert_date(as.Date(startDateTxt))
  
  startDate <- as.Date(startDateTxt)
  startYear <- as.numeric(format(startDate, '%Y'))
  startMonth <- as.numeric(format(startDate, '%m'))
  
  prices <- tryCatch(
    {
      tq_get(x = symbol, get = 'stock.prices', from = startDateTxt)
    },
    error = function(e) {
      print(e)
    }
  )
  
  prices$sales <- prices$volume * prices$adjusted
  prices$month <- format(prices$date, '%Y-%m')
  volume <- aggregate(volume ~ month, data=prices, FUN=sum)
  sales <- aggregate(sales ~ month, data=prices, FUN=sum)
  prices <- merge(volume, sales, by='month', all=TRUE)
  prices$price <- prices$sales/prices$volume
  prices$volume <- prices$sales <- NULL
  prices <- ts(prices$price[order(prices$month)], frequency=12, start=c(startYear, startMonth))
  
  return(prices)
}

#' Convert time series data into long format
#'
#' @param prices Time series data of monthly prices by year
#'
#' @return Long format data.frame of time series data
#' @export
#'
#' @examples
#' \dontrun{
#' url <- 'https://en.wikipedia.org/wiki/List_of_S%26P_500_companies'
#' doc <- get_from_wikipedia(url)
#' 
#' startDateTxt <- '1995-01-01'
#' gicsSubIndustry <- 'airlines'
#' symbol <- doc$symbol[tolower(doc$gics_sub_industry) == gicsSubIndustry]
#' prices <- get_prices(symbol, startDateTxt)
#' prices_df <- get_prices_df(prices)
#' }
get_prices_df <- function(prices) {
  prices_df <- data.frame(date=as.Date(prices), price=as.numeric(prices))
  prices_df$delta <- c(NA, diff(prices_df$price, lag=1))
  
  return(prices_df)
}
