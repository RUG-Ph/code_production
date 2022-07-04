library(R6)

WikiData <- R6Class("WikiData",
  private = list(
    doc = NULL,
    gicsSubIndustryList = NULL,
    prices = NULL,
    prices_df = NULL,
    get_from_wikipedia = function(url) {
      doc <- GET(url)
      doc <- readHTMLTable(doc=content(doc, "text"))
      doc <- doc$constituents
      doc[,colnames(doc)] <- lapply(doc[,colnames(doc)], FUN=as.character)
      doc_col_names <- as.character(doc[1,])
      doc_col_names <- gsub(pattern='[^A-Za-z0-9]', replacement='_', doc_col_names)
      doc_col_names <- gsub(pattern='_{2,}', replacement='_', doc_col_names)
      doc_col_names <- gsub(pattern='^_{1,}|_{1,}$', replacement='', doc_col_names)
      doc_col_names <- tolower(doc_col_names)
      colnames(doc) <- doc_col_names
      doc <- doc[-1,]
      
      private$doc <- doc
    },
    make_gics_sub_industry_list = function() {
      gicsSubIndustryList <- tolower(unique(private$doc$gics_sub_industry))
      gicsSubIndustryList <- gicsSubIndustryList[order(gicsSubIndustryList)]
      
      private$gicsSubIndustryList <- gicsSubIndustryList
    },
    compute_prices = function(startDateTxt, gicsSubIndustry) {
      symbol <- private$doc$symbol[tolower(private$doc$gics_sub_industry) == gicsSubIndustry]
      
      startDate <- as.Date(startDateTxt)
      startYear <- as.numeric(format(startDate, '%Y'))
      startMonth <- as.numeric(format(startDate, '%m'))
      
      prices  <- tq_get(x = symbol, get = 'stock.prices', from = startDateTxt)
      prices$sales <- prices$volume * prices$adjusted
      prices$month <- format(prices$date, '%Y-%m')
      volume <- aggregate(volume ~ month, data=prices, FUN=sum)
      sales <- aggregate(sales ~ month, data=prices, FUN=sum)
      prices <- merge(volume, sales, by='month', all=TRUE)
      prices$price <- prices$sales/prices$volume
      prices$volume <- prices$sales <- NULL
      prices <- ts(prices$price[order(prices$month)], frequency=12, start=c(startYear, startMonth))
      
      private$prices <- prices
    },
    make_prices_df = function() {
      prices_df <- data.frame(date=as.Date(prices), price=as.numeric(prices))
      prices_df$delta <- c(NA, diff(prices_df$price, lag=1))
      
      private$prices_df <- prices_df
    }
  ),
  public = list(
    initialize = function(url) {
      private$get_from_wikipedia(url)
      private$make_gics_sub_industry_list()
    },
    get_gics_sub_industry_list = function() {
      return(private$gicsSubIndustryList)
    },
    make_prices = function(startDateTxt, gicsSubIndustry) {
      private$compute_prices(startDateTxt, gicsSubIndustry)
      private$make_prices_df()
    },
    get_prices = function() {
      return(private$prices)
    },
    get_prices_df = function() {
      return(private$prices_df)
    },
    plot_prices = function(gicsSubIndustry) {
      plot(private$prices, main = gicsSubIndustry)
    }
  )
)
