#Packages----
pacman::p_load(tidyverse,dygraphs,lubridate,tidyquant,timetk, ggpubr,plotly)

#Stock 1 Year Comparison Function----
#If cor is entered will return the correlation %
#If dif is entered will return a graph of the difference between the stock prices with mean and st_dev
#Else return a chart of the stock prices
stonk_calculator <- function(tickers,x) { #Get price data for 1 year
  stonks <- tq_get(tickers, get = "stock.prices", from = today() - years(1), to = today())
  closes <- stonks %>% #get close prices side by side
    select(date,symbol,close) %>%
    pivot_wider(names_from = symbol, values_from = close)
  if (x == 'cor') { #find correlation between tickers
    res <- cor.test(closes[[2]], closes[[3]], method = "pearson")
    paste(colnames(closes)[2],colnames(closes)[3],'correlation:',round(res$estimate * 100,3))
  } else if (x == 'dif') {
    closes$dif <- closes[[2]] - closes[[3]] #create difference between prices
    closes$mean <- mean(closes$dif) #create mean
    closes$up_std_dev <- closes$mean + sd(closes$dif) #create upper std_dev
    closes$down_std_dev <- closes$mean - sd(closes$dif) #create lower std_dev
    #Create ticker price difference chart
    dif <- ggplot(data = closes, aes(x = date, y = dif)) +
      geom_point(size = .01, aes(text = paste(colnames(closes)[2],closes[[2]],'/n',colnames(closes)[3],closes[[3]]))) + #add stock prices
      geom_line() +
      geom_hline(yintercept = closes$mean, color = 'red', size = .25) + #add horizontal mean line
      geom_hline(yintercept = closes$up_std_dev, color = 'red', linetype = 'dotted', size = 0.3) + #add horizontal upper std_dev line
      geom_hline(yintercept = closes$down_std_dev, color = 'red', linetype = 'dotted', size = 0.3) + #add horizontal lower std_dev line
      labs(x = '', y = '', title = paste(colnames(closes)[2],'and',colnames(closes)[3],'stock price difference')) + #make title tickers
      theme_bw()
    ggplotly(dif)
  } else {#Create chart of ticker prices for 1 year
    stonks_chart <- stonks %>%
      select(symbol, date, adjusted) %>%
      pivot_wider(names_from = symbol, values_from = adjusted) %>%
      tk_xts(date_var = date)
    dygraph(stonks_chart)
    
  }
}

### Try to Find High Correlated Stock Pair for Pair Trading----
#Correlation must be at least 80%

library(htmlwidgets)
library(webshot)

#Compare Stock Returns for General Motors (GM) and Ford (F) to find a pair
GM_F <- c('GM','F')

stonk_calculator(GM_F,'chart')
stonk_calculator(GM_F,'cor')
stonk_calculator(GM_F,'dif')


#Compare ETF Returns for Gold (GLD) and Silver (SLV) to find a pair
GLD_SLV <- c('GLD','SLV')

stonk_calculator(GLD_SLV,'chart')
stonk_calculator(GLD_SLV,'cor')
stonk_calculator(GLD_SLV,'dif')


#Compare ETF Returns for S & P (SPY) and Nasdaq (QQQ) to find a pair
SPY_QQQ <- c('SPY','QQQ')

stonk_calculator(SPY_QQQ,'chart')
stonk_calculator(SPY_QQQ,'cor')
stonk_calculator(SPY_QQQ,'dif')
