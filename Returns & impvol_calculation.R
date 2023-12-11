install.packages('tidyr')

# Load necessary libraries
library(dplyr)
library(lubridate)
library(tidyr)
library(purrr)

df_price <- read.csv("price_data_bbg.csv")
df_2009_raw <- read.csv("WSH_data_2009.csv")
df_2014_raw <- read.csv("WSH_data_2014.csv")
df_2019_raw <- read.csv("WSH_data_2019.csv")

tail(df_price)

# Get unique tickers
unique_tickers <- unique(df_price$Stock)

# Convert to a list
ticker_list <- as.list(unique_tickers)

# Print the length of the list
length(ticker_list)

# Subset the data frames based on the stock symbols present in ticker_list
df_2009 <- df_2009_raw[df_2009_raw$stock_symbol %in% ticker_list, ]
df_2014 <- df_2014_raw[df_2014_raw$stock_symbol %in% ticker_list, ]
df_2019 <- df_2019_raw[df_2019_raw$stock_symbol %in% ticker_list, ]




# Function to calculate log returns for BMO
calc_price_change_BMO <- function(stock, ann_date){
  prev_date <- df_price %>% 
    filter(Stock == stock, Date < ann_date) %>%
    summarise(max_date = max(Date)) %>%
    pull(max_date)
  prev_close <- df_price %>% 
    filter(Stock == stock, Date == prev_date) %>%
    pull(PX_Last)
  
  ann_date1 <- df_price %>% 
    filter(Stock == stock, Date >= ann_date) %>%
    summarise(min_date = min(Date)) %>%
    pull(min_date)
  ann_open <- df_price %>% 
    filter(Stock == stock, Date == ann_date1) %>%
    pull(PX_Open)
  
  log_returns <- log(ann_open / prev_close)
  return(log_returns)
}

# Function to calculate log returns for AMC
calc_price_change_AMC <- function(stock, ann_date){
  ann_date2 <- df_price %>% 
    filter(Stock == stock, Date <= ann_date) %>%
    summarise(max_date = max(Date)) %>%
    pull(max_date)
  ann_close <- df_price %>% 
    filter(Stock == stock, Date == ann_date2) %>%
    pull(PX_Last)
  
  next_date <- df_price %>% 
    filter(Stock == stock, Date > ann_date) %>%
    summarise(min_date = min(Date)) %>%
    pull(min_date)
  next_open <- df_price %>% 
    filter(Stock == stock, Date == next_date) %>%
    pull(PX_Open)
  
  log_returns <- log(next_open / ann_close)
  return(log_returns)
}

# Create a classification with BMO and AMC for 2009
df_2009 <- df_2009 %>%
  mutate(announce_datetime = mdy_hm(announce_datetime)) %>%
  mutate(time_class = ifelse(hour(announce_datetime) <= 9.5, "BMO", 
                             ifelse(hour(announce_datetime) >= 16, "AMC", "Other"))) %>%
  mutate(announce_date = as.Date(announce_datetime))

df_price$Date <- as.Date(df_price$Date, format="%d/%m/%y")

#Change class of the announce_date columns 
class(df_2009$announce_date)
df_2009$announce_date <- as.Date(df_2009$announce_date)
head(df_2009)

# Create a new column to store the price change values
df_2009 <- df_2009 %>% 
  rowwise() %>% 
  mutate(log_returns = ifelse(time_class == 'BMO',
                               calc_price_change_BMO(stock_symbol, announce_date),
                               calc_price_change_AMC(stock_symbol, announce_date)))

print(df_2019$log_returns)

# Create a classification with BMO and AMC for 2014
df_2014 <- df_2014 %>%
  mutate(announce_datetime = mdy_hm(announce_datetime)) %>%
  mutate(time_class = ifelse(hour(announce_datetime) <= 9.5, "BMO", 
                             ifelse(hour(announce_datetime) >= 16, "AMC", "Other"))) %>%
  mutate(announce_date = as.Date(announce_datetime))

#Change class of the announce_date columns 
class(df_2014$announce_date)
df_2014$announce_date <- as.Date(df_2014$announce_date)
head(df_2014)

# Create a new column to store the price change values
df_2014 <- df_2014 %>% 
  rowwise() %>% 
  mutate(log_returns = ifelse(time_class == 'BMO',
                               calc_price_change_BMO(stock_symbol, announce_date),
                               calc_price_change_AMC(stock_symbol, announce_date)))

print(df_2014$log_returns)

# Create a classification with BMO and AMC for 2019
df_2019 <- df_2019 %>%
  mutate(announce_datetime = mdy_hm(announce_datetime)) %>%
  mutate(time_class = ifelse(hour(announce_datetime) <= 9.5, "BMO", 
                             ifelse(hour(announce_datetime) >= 16, "AMC", "Other"))) %>%
  mutate(announce_date = as.Date(announce_datetime))

#Change class of the announce_date columns 
class(df_2019$announce_date)
df_2019$announce_date <- as.Date(df_2019$announce_date)
head(df_2019)

# Create a new column to store the price change values
df_2019 <- df_2019 %>% 
  rowwise() %>% 
  mutate(log_returns = ifelse(time_class == 'BMO',
                               calc_price_change_BMO(stock_symbol, announce_date),
                               calc_price_change_AMC(stock_symbol, announce_date)))

print(df_2019$log_returns)


###################################################

#Now we get the 6-month implied vol at the money and the 1-month implied vol at the money before the announcement date.

#Function to get the implied volatility for BMO
calc_base_vol_BMO <- function(stock, ann_date){
  prev_date <- vol_df %>% 
    filter(Stock == stock, Dates < ann_date) %>%
    summarise(max_date = max(Dates)) %>%
    pull(max_date)
  prev_close <- vol_df %>% 
    filter(Stock == stock, Dates == prev_date) %>%
    pull(imp_vol)
  
  vol_change <- prev_close 
  return(vol_change)
}


#Function to get the implied volatility for AMC
calc_base_vol_AMC <- function(stock, ann_date){
  ann_date1 <- vol_df %>% 
    filter(Stock == stock, Dates <= ann_date) %>%
    summarise(max_date = max(Dates)) %>%
    pull(max_date)
  ann_close <- vol_df %>% 
    filter(Stock == stock, Dates == ann_date1) %>%
    pull(imp_vol)
  
  vol_change <- ann_close 
  return(vol_change)
}


#Getting the 1 month implied vol ATM for all the years
vol_df <- read_csv("ivol_base_1month_melted.csv")
vol_df <- arrange(vol_df, Dates)
vol_df$Dates <- as.Date(vol_df$Dates, format="%d/%m/%y")

df_2009 <- df_2009 %>% 
  rowwise() %>% 
  mutate(impvol_1month_ATM = ifelse(time_class == 'BMO',
                             calc_base_vol_BMO(stock_symbol, announce_date),
                             calc_base_vol_AMC(stock_symbol, announce_date)))

df_2014 <- df_2014 %>% 
  rowwise() %>% 
  mutate(impvol_1month_ATM = ifelse(time_class == 'BMO',
                             calc_base_vol_BMO(stock_symbol, announce_date),
                             calc_base_vol_AMC(stock_symbol, announce_date)))

df_2019 <- df_2019 %>% 
  rowwise() %>% 
  mutate(impvol_1month_ATM = ifelse(time_class == 'BMO',
                             calc_base_vol_BMO(stock_symbol, announce_date),
                             calc_base_vol_AMC(stock_symbol, announce_date)))



#Getting the 6 month implied vol ATM for all the years
vol_df <- read_csv("ivol_base_6month_melted.csv")
vol_df <- arrange(vol_df, Dates)
vol_df$Dates <- as.Date(vol_df$Dates, format="%d/%m/%y")


df_2009 <- df_2009 %>% 
  rowwise() %>% 
  mutate(impvol_6month_ATM = ifelse(time_class == 'BMO',
                                    calc_base_vol_BMO(stock_symbol, announce_date),
                                    calc_base_vol_AMC(stock_symbol, announce_date)))

df_2014 <- df_2014 %>% 
  rowwise() %>% 
  mutate(impvol_6month_ATM = ifelse(time_class == 'BMO',
                                    calc_base_vol_BMO(stock_symbol, announce_date),
                                    calc_base_vol_AMC(stock_symbol, announce_date)))

df_2019 <- df_2019 %>% 
  rowwise() %>% 
  mutate(impvol_6month_ATM = ifelse(time_class == 'BMO',
                                    calc_base_vol_BMO(stock_symbol, announce_date),
                                    calc_base_vol_AMC(stock_symbol, announce_date)))

#Save the table to a csv
#write.csv(df_2009, "2009_FINAL.csv", row.names = FALSE)
#write.csv(df_2014, "2014_FINAL.csv", row.names = FALSE)
#write.csv(df_2019, "2019_FINAL.csv", row.names = FALSE)

