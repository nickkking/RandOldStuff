#################################################
# Financial Analytics II
# David Andexler
# Team 11 - Modeling Volatility
#################################################

library(graphics)
library(quantmod)
library(TTR)
library(dplyr)
library(ks)
library(scales)
library(forecast)
library(aTSA)
library(ccgarch) # Removed from CRAN
library(fGarch)
library(rugarch)
library(rvest)
library(stringr)
library(ggplot2)
library(ggthemes)

#################################################
#
# Retrieving and Posturing Data
#
#################################################


# Reads in Ticker Symbols for DJIA from Wikipedia HTML table
rm(list = ls())
tbl <- read_html('https://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average') %>% html_nodes(xpath = '//*[@id="constituents"]') # XPath obtained from 'Inspect' option in browser
tbl <- tbl[1] %>% html_table() %>% as.data.frame() # Converts the HTML table into a DataFrame
tbl$symbol_simple <-  tbl$Symbol %>% str_remove("NYSE:") # Strips NYSE: from ticker symbols
tbl$symbol_simple <- str_trim(tbl$symbol_simple, side = 'left') # Removes leading space
tickers <- tbl$symbol_simple # Saves as ticker

# For each ticker symbol, gathers historical data from 2011-03-01 through 2020-03-03
for (i in 1:length(tickers)) {
  if (i == 1) { # If the first symbol, initializes the data frame
    getSymbols(tickers[i], src = 'yahoo', from = '2011-03-01', to = '2020-03-04') # Gets historical data
    df <- eval(as.name(paste(tickers[i]))) # Calls the variable form of the ticker symbol based on index
    stocks_close_df <- data.frame(df[,4]) # Pulls off closing price
  }
  else { # Same function as above, just for all other indices
    print(paste("Starting", tickers[i]))
    getSymbols(tickers[i], src = 'yahoo', from = '2011-03-01', to = '2020-03-04')
    df2 <- eval(as.name(paste(tickers[i])))
    stock_df <- df2[,4]
    stocks_close_df <- cbind.xts(stocks_close_df, stock_df, fill = NA) # Appends to previous dataframe
  }

}

# Strange bug in first column, MMM, no time to fix

# Replaces first column with correct closing prices
stocks_close_df[,1] <- MMM[,4]
colnames(stocks_close_df)[1] <- "MMM.Close"

# Calculates daily return for stocks
for (column in colnames(stocks_close_df)) {
  stocks_close_df[,column] <- ROC(stocks_close_df[,column])
}

# Adjusts column names to '{Stock Name}_r'
col_list <- colnames(stocks_close_df)
for (i in 1:length(col_list)) {
  col_list[i] <- paste(str_remove(col_list[i], '.Close'), '_r', sep = "")
}
colnames(stocks_close_df) <- col_list

# Saves CSV of final data frame
setwd('#')
write.csv(stocks_close_df, 'stocks.csv')


stocks <- data.frame(stocks_close_df)
plot(stocks$AAPL_r, col="black", main="AAPL Stock Return (Logarithmic)", xlab="", ylab="Daily Returns", lwd=2, type="l")

#################################################################
# Analysis
#################################################################

# Testing for ARCH effects with Lagrange Multiplier 1 Lag Test
for (column in colnames(stocks)) {
  print(paste('\nStock: ', column))
  arch.test(arima(stocks[, column][-1], order = c(0,0,0)), output = TRUE)
}

# Will need to pull LM test statistic from the above and select the top 5 most significant
# Most significant: WMT, CSCO, WBA, MMM, NKE

sig_stocks <- list('WMT', 'CSCO', 'WBA', 'MMM', 'NKE')

# Pick one of the following models for each of the top 5: GARCH(1,1)- Normal, t-GARCH(1,1), QGARCH(1,1)-Normal, QGARCH(1,1)-t

for (i in 1:length(sig_stocks) ) {
  name <- paste(sig_stocks[i], '_r', sep='')
  print("\n")
  print("GARCH (1, 1) - Normal")
  GARCH.N <- garchFit(formula= ~ garch(1,1), data=stocks[,name][-1],
                      cond.dist="norm", include.mean = FALSE)
  summary(GARCH.N)
  
  print("------------------------------------------------------------------------------")
  print("GARCH (1, 1) - t")
  
  GARCH.t <- garchFit(formula= ~ garch(1,1), data=stocks[,name][-1], 
                      cond.dist="std", include.mean = FALSE)
  summary(GARCH.t)
  
  print("------------------------------------------------------------------------------")
  print("QGARCH (1, 1) - Normal")
  
  Skew.GARCH.N <- garchFit(formula= ~ garch(1,1), data=stocks[,name][-1], 
                           cond.dist="snorm",include.mean = FALSE)
  summary(Skew.GARCH.N)
  
  print("------------------------------------------------------------------------------")
  print("QGARCH (1, 1) - t")
  
  Skew.GARCH.t <- garchFit(formula= ~ garch(1,1), data=stocks[,name][-1], 
                           cond.dist="sstd", include.mean = FALSE)
  summary(Skew.GARCH.t)
  break()
}

# Select based on lowest BIC value


# WMT tGARCH(1,1) -6.44
# CSCO QGARCH(1,1)-t -5.84
# WBA t-GARCH(1,1) -5.66
# MMM QGARCH(1,1)-t -6.27
# NKE t-GARCH(1,1) -5.80

# Predicting 30 day volatlity for WMT
WMT.GARCH.t <- garchFit(formula= ~ garch(1,1), data=stocks$WMT_r[-1], cond.dist="std", include.mean = FALSE)
summary(WMT.GARCH.t)

WMT.preds <- predict(WMT.GARCH.t, 30)
WMT.avg_volatility <- mean(WMT.preds$standardDeviation)


# Predicting 30 day volatlity for WBA
WBA.GARCH.t <- garchFit(formula= ~ garch(1,1), data=stocks$WBA_r[-1], cond.dist="std", include.mean = FALSE)
summary(WBA.GARCH.t)

WBA.preds <- predict(WBA.GARCH.t, 30)
WBA.avg_volatility <- mean(WBA.preds$standardDeviation)


# Predicting 30 day volatlity for NKE
NKE.GARCH.t <- garchFit(formula= ~ garch(1,1), data=stocks$NKE_r[-1], cond.dist="std", include.mean = FALSE)
summary(NKE.GARCH.t)

NKE.preds <- predict(NKE.GARCH.t, 30)
NKE.avg_volatility <- mean(NKE.preds$standardDeviation)


# Predicting 30 day volatlity for CSCO
CSCO.Skew.GARCH.t <- garchFit(formula= ~ garch(1,1), data=stocks$CSCO_r[-1], cond.dist="sstd", include.mean = FALSE)
summary(CSCO.Skew.GARCH.t)

CSCO.preds <- predict(CSCO.Skew.GARCH.t, 30)
CSCO.avg_volatility <- mean(CSCO.preds$standardDeviation)



# Predicting 30 day volatlity for MMM
MMM.Skew.GARCH.t <- garchFit(formula= ~ garch(1,1), data=stocks$MMM_r[-1], cond.dist="sstd", include.mean = FALSE)
summary(MMM.Skew.GARCH.t)

MMM.preds <- predict(MMM.Skew.GARCH.t, 30)
MMM.avg_volatility <- mean(MMM.preds$standardDeviation)

# Rank in order of effect of market shock
# Rank in order of persistence of market shock

# Plot for volatility

preds <- data.frame('day'=1:30,WMT.preds$standardDeviation, WBA.preds$standardDeviation, NKE.preds$standardDeviation, CSCO.preds$standardDeviation, MMM.preds$standardDeviation)
p <- ggplot()+
     geom_line(data = preds, aes(x = day, y = WMT.preds$standardDeviation ), color="#080F0F")+
     geom_line(data = preds, aes(x = day, y = WBA.preds$standardDeviation ), color="#A4BAB7")+
  geom_line(data = preds, aes(x = day, y = NKE.preds$standardDeviation ), color="#EFF2C0")+
  geom_line(data = preds, aes(x = day, y = CSCO.preds$standardDeviation ), color="#BEA57D")+
  geom_line(data = preds, aes(x = day, y = MMM.preds$standardDeviation ), color="#A52422")+
  xlab("Day")+
  ylab("Predicted Volatility")

p