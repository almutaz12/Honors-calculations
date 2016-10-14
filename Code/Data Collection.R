# This file is using the honors_stocks code to download stock data

honors_stocks <- function(symbols='F',
                          what=c("prices","daily","weekly", "monthly", "dividends"),
                          start_year=1986,end_year=2016) {
  
  if (! require(RCurl)) stop("Must install RCurl package.")
  if (! require(dplyr)) stop("Must install dplyr package.")
  what <- match.arg(what)
  yahooCode <- switch(what,
                      prices = ,
                      daily = "d",
                      weekly = "w",
                      monthly = "m",
                      dividends = "v",
                      "unknown")
  
  stockURL <- "http://real-chart.finance.yahoo.com/table.csv?s=%s&a=05&b=1&c=%d&d=01&e=25&f=%d&g=%s&ignore=.csv"
  
  output <- NULL # for collecting output
  
  for (symbol in symbols) {
    thisURL <- sprintf(stockURL, symbol, start_year, end_year, yahooCode)
    con <- try(textConnection(getURLContent( thisURL )), silent = TRUE)
    if (inherits(con, what = "try-error")) {
      message(paste("Symbol", symbol, "not found in years",
                    start_year, "to", end_year, "on Yahoo finance."))
    } else {
      res <- read.csv(con)
      res$company <- symbol
      close(con)
      output <- rbind(output, res)
    }
  }
  output <- output%>%
    mutate(date = lubridate::ymd(Date)) %>%
    select(-Date)
  if (yahooCode == 'v') {
    output <-
      output %>%
      rename(dividends = Dividends)
  } else {
    output <-
      output %>%
      rename(open = Open, high=High, low=Low, close=Close,
             volume = Volume, adj_close = Adj.Close)
  }
  
  output
}
# Listed Companies in three indexes: 
# NYSE & NASDAQ source:http://www.nasdaq.com/screening/company-list.aspx
# S&P 500 source:http://data.okfn.org/data/core/s-and-p-500-companies#readme

########################################
#NYSE
#Companies <- read.csv("NYSE_companylist.csv", stringsAsFactors = FALSE, na.strings = "n/a")
#Symbols <- Companies$Symbol[ ! is.na(Companies$Sector)]
########################################

#Data Source: Yahoo Finance 
# f1 <- honors_stocks(symbols = Symbols[1:100]) 
#f2 <- honors_stocks(symbols = Symbols[101:200]) 
#f3 <- honors_stocks(symbols = Symbols[201:600]) 
#f4 <- honors_stocks(symbols = Symbols[601:1200]) 
#f5 <- honors_stocks(symbols = Symbols[1201:1900]) 
#f6 <- honors_stocks(symbols = Symbols[1901:2235]) 
#NYSE <- rbind(f1, f2,f3,f4,f5,f6)
#save(NYSE, file = "NYSE_Stock_Data.Rda")

########################################
#NASDAQ
#Companies <- read.csv("NASDAQ_companylist.csv", stringsAsFactors = FALSE, na.strings = "n/a")
#Symbols <- Companies$Symbol[ ! is.na(Companies$Sector)]
########################################

#NASDAQ<- honors_stocks(symbols = Symbols[1:2785])
#save(NASDAQ, file = "NASDAQ_Stock_Data.Rda")

########################################
#S&P 500
#Companies <- read.csv("S&P 500_companylist.csv", stringsAsFactors = FALSE, na.strings = "n/a")
#Symbols <- Companies$Symbol[ ! is.na(Companies$Sector)]
########################################

#SP_500<- honors_stocks(symbols = Symbols[1:504])
#save(SP_500, file = "SP_500_Stock_Data.Rda")

########################################
#S&P 500 historical companies 
Symbols=SP_marketCap$company
SP_H_Comp<- honors_stocks(symbols = Symbols)
save(SP_H_Comp, file = "SP_H_Data.Rda")