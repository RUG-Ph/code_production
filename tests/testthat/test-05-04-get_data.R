library(httr)
library(tidyquant)

source("../../R/05-04-get_data.R")

describe("data scraping", {
  describe("get_from_wikipedia()", {
    it("accepts a valid URL patter", {
      test_url <- 'https://en.wikipedia.org/wiki/List_of_S%26P_500_companies'
      
      expect_silent(get_from_wikipedia(test_url))
    })
    
    it("returns the correct data.frame format", {
      test_url <- 'https://en.wikipedia.org/wiki/List_of_S%26P_500_companies'
      
      response <- get_from_wikipedia(test_url)

      checkmate::expect_data_frame(
        response,
        ncols = 9,
        min.rows = 1
      )
      
      checkmate::expect_names(
        names(response),
        permutation.of = c(
          "symbol",
          "security",
          "sec_filings",
          "gics_sector",
          "gics_sub_industry",
          "headquarters_location",
          "date_first_added",
          "cik",
          "founded"
        )
      )
    })
    
    it("rejects an invalid URL pattern", {
      test_url <- 'htts://en.wikipedia.org/wiki/List_of_S%26P_500_companies'
      
      expect_error(get_from_wikipedia(test_url))
    })
    
    it("fails on a valid URL but non-existing URL", {
      test_url <- 'https://en.wikipeda.org/wiki/List_of_S%26P_500_companies'
      
      expect_error(get_from_wikipedia(test_url))
    })
  })
  
  
})