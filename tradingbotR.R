#------------------------------------------------------------------------------#
#
#                                     LIBRARIES
#
#------------------------------------------------------------------------------#

library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(jsonlite)
library(plotly)
library(TTR)


#------------------------------------------------------------------------------#
#
#                                     FUNCTIONS
#
#------------------------------------------------------------------------------#

getHistory_kraken <- function(pair = "ETHCAD"){
  #url = paste0("https://api.kraken.com/0/public/OHLC?pair="ETHCAD&since=startdate&interval=5)
  url = "https://api.kraken.com/0/public/OHLC?pair=ETHCAD&since=1517774700&interval=5"
  data = jsonlite::fromJSON(url)
  data = data.frame(data[2])
  
  # Rename column
  names(data) <- c("time",
                  "open",
                  "high",
                  "low",
                  "close",
                  "vwap",
                  "volume",
                  "count",
                  "last")
  
  # convert time column
  data$time = as_datetime(as.numeric(data$time))
  data$open = as.numeric(data$open)
  data$high = as.numeric(data$high)
  data$low = as.numeric(data$low)
  data$close = as.numeric(data$close)
  data$vwap = as.numeric(data$vwap)
  data$volume = as.numeric(data$volume)
  data$high = as.numeric(data$high)
  

  return(data)
}

getData_Poloniex <- function(start_date,
                                end_date = now(),
                                pair = "USDT_ETH", 
                                period = 300,
                                buffer = 2){
  # Function to retrieve stock data between end (date) - start_date_buffer months
  # Variables:
  #   start_date: when to start data acquisition
  #   end_date (default = now()): when to stop data acquisition
  #   pair (default  = USDT_ETH: stock pair to retrieve e.g. "USDT_ETH
  #   period (default = 300 s): number of seconds between acquisition
  #   buffer: at 300sec, maximum 2 months acquisition
  
  data = data.frame()
  data$date = POSIXct()
  data$high = as.numeric()
  data$low = as.numeric()
  data$open = as.numeric()
  data$close = as.numeric()
  data$volume = as.numeric()
  data$quoteVolume = as.numeric()
  data$weightedAverage = as.numeric()
  
  if (end_date - start_date > months(buffer)){
    date_vec = monthSplit(start_date, end_date)
    
    for(i in 1:(length(date_vec)-1)){
      data = bind_rows(data,getHistory_Poloniex(date_vec[i], date_vec[i+1], pair, period, buffer))
    }
    return(data)
  }
  else{
    data = bind_rows(data,getHistory_Poloniex(start_date, end_date, pair, period, buffer))
    return(data)
  }
}

getHistory_Poloniex <- function(start_date,
                                end_date,
                                pair, 
                                period,
                                buffer){
  # Define start and end date in  unix format to be used in the api url
  start_date = as.numeric(as.POSIXct(start_date, origin = "1970-01-01"))
  end_date = as.numeric(as.POSIXct(end_date, origin = "1970-01-01"))
  
  # build url with api commands from value used in function
  url  = "https://poloniex.com/public?command=returnChartData&currencyPair="
  url = paste0(url,pair,"&start=",start_date,"&end=", end_date,"&period=", period)
  
  # Retrieve json data from url
  data = jsonlite::fromJSON(url)
  
  # Convert unix format to date time format
  data$date = as_datetime(as.numeric(data$date))
  return(data)
}

monthSplit <- function(start_date,end_date,buffer = 2){
  # Function to split in "buffer" months spaces, a duration defined by and start_date and end_date
  #   start_date: when to start retrieving data
  #   end _date: when to stop retrieving data
  #   buffer (default = 2months): number of months between spaced acquisition
  
  date_check = start_date # Define date counter/checker
  time_vector = c(start_date) #intitialize date vector
  
  while (date_check < end_date){ # while the actual date in vector is smaller than the end date
    new_date = time_vector[length(time_vector)] + months(buffer) # create a new date + buffer months from
    # the last value in vector
    if (new_date > end_date){ # if this value is after the end date
      time_vector[length(time_vector)+1] = end_date # asign end date at the end of the vector
    }
    else{ # if the new_date is not after the end date, add it to the vector
      time_vector[length(time_vector)+1] = new_date
    }
    
    date_check = time_vector[length(time_vector)] #reset date_check to last date in vector
  }
  # return time vector to be used in getHistory command
  return(time_vector)
}

strat_52150200 <- function(df){
  # Strategy 52150200 based on ema set, adx and bohlinger bands
  # sig1: EMA5 > EMA21
  # sig2: EMA50 > EMA2=200
  # sig3: BBpct > 75%
  # sig4: ADX > 15
  # sig5: Ultosc > 50
  # sig6: sig1@sig6 = T
  
  close_vec = df$close
  df$ema5 = EMA(close_vec,5)
  df$ema21 = EMA(close_vec,21)
  df$ema50 = EMA(close_vec,50)
  df$ema200 = EMA(close_vec,200)
  
  HLC_data = df %>% select(c(high, low, close))
  df$ADX = TTR::ADX(HLC_data)[,4]
  
  df$bbpct <- TTR::BBands(HLC_data)[,4]
  
  df$ultosc <-  TTR::ultimateOscillator(HLC_data)
  
  # Compute signal (T or F)
  df$sig1 <- df$ema5 > df$ema21
  df$sig2 <- df$ema50 > df$ema200
  df$sig3 <- df$bbpct > 0.75
  df$sig4 <- df$ADX > 15 
  df$sig5 <- df$ultosc > 50
  df$sig6 <- df$sig1 & df$sig2 & df$sig3 & df$sig4 & df$sig5
  return(df)
}

gg_52150200 <- function(df){
  df1 <-  df %>% select(c(date, close)) %>% pivot_longer(c(close))
  df2 <-  df %>% select(c(date, ema5, ema21, ema50, ema200)) %>% pivot_longer(c(ema5, ema21, ema50, ema200))
  df3 <-  df %>% select(c(date, ADX)) %>% pivot_longer(c(ADX))
  df4 <-  df %>% select(c(date, bbpct)) %>% pivot_longer(c(bbpct))
  df5 <-  df %>% select(c(date, ultosc)) %>% pivot_longer(c(ultosc))
  
  gg1 <- ggplot(data = df1, aes(x = date, y=value)) + geom_line() + ggtitle("Close Price") + theme_bw()
  gg2 <- ggplot(data = df2, aes(x = date, y=value, color = name)) + geom_line() + ggtitle("EMA set") + theme_bw()
  gg3 <- ggplot(data = df3, aes(x = date, y=value)) + geom_line() + geom_hline(aes(yintercept = 15)) + ggtitle("ADX") + theme_bw()
  gg4 <- ggplot(data = df4, aes(x = date, y=value)) + geom_line() + geom_hline(aes(yintercept = 0.75)) + ggtitle("BBand %B") + theme_bw()
  gg5 <- ggplot(data = df5, aes(x = date, y=value)) + geom_line() + geom_hline(aes(yintercept = 50)) + ggtitle("Ultimate Oscillator") + theme_bw()
  
  gg1 <- ggplotly(gg1)
  gg2 <- ggplotly(gg2)
  gg3 <- ggplotly(gg3)
  gg4 <- ggplotly(gg4)
  gg5 <- ggplotly(gg5)
  
  return(list(gg1,gg2,gg3,gg4,gg5))
}

strat_WillFracSMMA <- function(df){
  df$sma50 <- TR::SMA(df$close, n=50)
  df$sma100 <- TR::SMA(df$close, n=100)
  df$sma200 <- TR::SMA(df$close, n=200)
}

create_log_file <- function(strategy, initial_balance, pair){
  file <- file("logfile.txt","w")
  date_string = paste0("DATE: ",now())
  writeLines("#-----------------------------------------------------------------#",file)
  writeLines("#                                                                 #",file)
  writeLines("#                          LOG FILE                               #",file)
  writeLines(paste("#                ",date_string,"                      #"),file)
  writeLines("#                                                                 #",file)
  writeLines("#-----------------------------------------------------------------#\n",file)
  writeLines("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$",file)
  writeLines(paste("Strategy: ", strategy),file)
  writeLines(paste("Initial Account balance: ", initial_balance),file)
  writeLines(paste("Pair: ", pair),file)
  writeLines("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$",file)
  return(file)
}

print_buy_signal <- function(file, buy_price, buy_time, balance, worth){
  writeLines(paste("\n**BUY**",buy_time," Price: ",buy_price, " Curr. balance: ", balance, " Curr. Worth: ",worth),file)
}
print_sell_signal <- function(file, sell_price, sell_time, sell_status, balance, worth){
  writeLines(paste(paste0("\n**",sell_status,"**"),sell_time," Price: ",sell_price, " Curr. balance: ", balance, " Curr. Worth: ",worth),file)
}

print_results <- function(file, init_balance, end_balance, n_succesful_trade, n_fail_trade){
  writeLines("\n$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$",file)
  writeLines(paste("Init Balance: ", init_balance),file)
  writeLines(paste("Final Balance: ",end_balance ),file)
  writeLines(paste("Profit Margin: ", end_balance/init_balance),file)
  writeLines(paste("# of succesful trade: ", n_succesful_trade),file)
  writeLines(paste("# of fail trade: ", n_fail_trade),file)
  writeLines(paste("Success rate: ", n_succesful_trade/(n_succesful_trade + n_fail_trade)),file)
  writeLines("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$",file)
}

offline_test <- function(df,buy_sig,file, stock_qty, balance){
  # df: Dataframe containing OHLC and buy sig
  # buy_sig: string telling wich column tells you can buy the stock
  # stock_qty: how much of the traded pair in stock
  
  df = drop_na(df)
  
  transaction_on = F # Boolean to know if there is a current transaction
  n_succesful_trade = 0 # Tracking of succesful trade
  n_fail_trade = 0 # Tracking of unsuccesful trade
  worth = balance
  bought_stock_value = 0
  profit_factor = 1.03 # best = 1.03
  loss_factor = 0.94 # best = 0.94
  init_balance = balance
  
  for (i in 1:nrow(df)){
    current_stock_price = df$close[i]
    current_time = df$date[i]

    
    if (worth <= 0){
      writeLines("\n You're broke dumbass")
      break
    }
    else if (df[[buy_sig]][i] == T && transaction_on == F){ # buy stock
      stock_qty = balance/current_stock_price
      worth = stock_qty*current_stock_price
      balance = balance - worth
      transaction_on = T
      bought_stock_value = current_stock_price
      print_buy_signal(file, current_stock_price, current_time, balance, worth)
    }
    else if (transaction_on == T && current_stock_price >= profit_factor*bought_stock_value){ # sell stock with profit
      balance = stock_qty*current_stock_price
      stock_qty = 0
      worth = balance
      transaction_on = F
      bought_stock_value = 0
      print_sell_signal(file, current_stock_price, current_time, "PROFIT", balance, worth)
      n_succesful_trade = n_succesful_trade + 1
    }
    else if (transaction_on == T && current_stock_price <= loss_factor*bought_stock_value){ # sell stock with loss
      balance = stock_qty*current_stock_price
      stock_qty = 0
      worth = balance
      transaction_on = F
      bought_stock_value = 0
      print_sell_signal(file, current_stock_price, current_time, "LOSS", balance, worth)
      n_fail_trade = n_fail_trade + 1
      
    }
  }
  print_results(file, init_balance, balance, n_succesful_trade, n_fail_trade)
}


#------------------------------------------------------------------------------#
#
#                                   INITIALISATION
#
#------------------------------------------------------------------------------#

balance = 85000 # money in the bank
stock_qty = 0 # current amount of stock


#------------------------------------------------------------------------------#
#
#                                        CODE
#
#------------------------------------------------------------------------------#

data = getData_Poloniex(now()- months(36),now())


data <- strat_52150200(data)

# data <- strat_macpoulet(data)
# 
# data_mc = data %>% drop_na %>%  select(c(date,macd, macd_sig)) %>%  pivot_longer(c(macd, macd_sig))
# 
# ggplotly(ggplot(data = data_mc, aes(x = date, y = value, color = name)) + geom_line())


file = create_log_file("52150200", balance, "USDT/ETH")
offline_test(data, "sig6", file,stock_qty,balance)
close(file)



