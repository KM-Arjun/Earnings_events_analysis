# Earnings events: The Distribution of Moves

## Introduction 
We analyze the notable price fluctuations of publicly traded U.S. stocks coinciding with the announcement of quarterly earnings. Our focus is on understanding how these actual one-day returns correlate with the changes in option implied volatility. For three distinct years, we examine not only the direct (percentage) returns but also a forecasted scale of these returns. This forecast is based on the anticipated adjustments in implied volatility prior to the earnings announcements.

This documentation explains the code structure to reproduce the results of the above Research Paper. The data used to obtain the results are not attached in this documentation. You can replace the data files with your own raw data to run the code above. 

## Data Sources and Preperation 
The script utilizes two main data sources:
1.	**Price Data from Bloomberg**: `df_price` - This dataset includes historical stock prices. It's crucial for calculating stock returns before and after earnings announcements.
2.	**Earnings Announcement Dates from Wall Street Horizon**: `df_2009_raw`, `df_2014_raw`, `df_2019_raw` - These datasets provide the dates and times of earnings announcements for various stocks across the years 2009, 2014, and 2019.

## Code Structure 
The code consists of multiple components, each serving a specific purpose in the analysis:

1.  `Returns & impvol_calculation.R` : Processes and prepares datasets, classifies earnings events, and calculates stock returns.
2.  `Model_analysis.ipynb` : Conducts statistical analysis and modeling of stock price movements and implied volatility.
3.  `Additional Python Analysis` : Focuses on market capitalization segments, analyzing large-cap, mid-cap, and small-cap stocks.

Each file contains specific functions and comments explaining the process along every step.

