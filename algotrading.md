
## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```r
# Load data from CSV file
amd_df <- read.csv("AMD.csv")
# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)
amd_df <- amd_df[, c("date", "close")]
```

#### Plotting the Data
Plot the closing prices over time to visualize the price movement.
```r
plot(amd_df$date, amd_df$close,'l')
```

### Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```r
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

for (i in 1:nrow(amd_df)) {
current_price <- amd_df$close[i] 
if (previous_price == 0) {
amd_df$trade_type[i] <- "buy"
amd_df$costs_proceeds[i] <- -current_price * share_size
accumulated_shares <- accumulated_shares + share_size 
  }
else if (current_price < previous_price) {
amd_df$trade_type[i] <- "buy"
amd_df$costs_proceeds[i] <- -current_price * share_size
accumulated_shares <- accumulated_shares + share_size
} 
amd_df$accumulated_shares[i] <- accumulated_shares    
previous_price <- current_price  
if (i==nrow(amd_df)) { amd_df$trade_type[i] <- 'sell' 
amd_df$costs_proceeds[i]<- current_price * accumulated_shares 
} 
amd_df$accumulated_shares[i]=0 
```


### Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years 
```r
# Define a trading period
start_date <- as.Date("2019-05-20") end_date <- as.Date("2020-05-20") 
 amd_df <- amd_df[amd_df$date >= start_date & amd_df$date <= end_date] 
```


### Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```r
# Total Profit/Loss Calculation omitting any N/A values.  
total_profit_loss <- sum(amd_df$costs_proceeds, na.rm = TRUE) 
 
# Invested Capital Calculation omitting any N/A values. 
total_invested_capital <- -sum(amd_df$costs_proceeds[amd_df$trade_type == "buy"], na.rm = TRUE) 
 
# ROI Calculation 
ROI <- (total_profit_loss / total_invested_capital) * 100 
 
# Print the results of "Total Profit/Loss", "Total Invested Capital" and 
"ROI" cat("Total Profit/Loss: ", total_profit_loss, "\n") 
## Total Profit/Loss:  196930 cat("Total Invested Capital: ", total_invested_capital, "\n") 
## Total Invested Capital:  428999 cat("ROI: ", ROI, "%\n") 
## ROI:  45.90453 % 
```

### Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.


```r
for (i in 1:nrow(amd_df)) {   current_price <- amd_df$close[i] 
     if (i == 1) {
amd_df$trade_type[i] <- "buy" 
amd_df$costs_proceeds[i] <- -current_price * share_size
accumulated_shares <- share_size 
    total_invested <- -amd_df$costs_proceeds[i]
amd_df$accumulated_shares[i] <- accumulated_shares 
  } else { 
# Calculate average purchase price 
average_purchase_price <- total_invested / accumulated_shares 
    if (!is.na(average_purchase_price) && current_price >= average_purchase_price * 1.2) {
amd_df$trade_type[i] <- "sell_half" 
amd_df$costs_proceeds[i] <- current_price * (accumulated_shares / 2)
accumulated_shares <- accumulated_shares / 2
total_invested <- total_invested / 2 
    }
else if (current_price < amd_df$close[i - 1]) {
amd_df$trade_type[i] <- "buy"
amd_df$costs_proceeds[i] <- -current_price * share_size
accumulated_shares <- accumulated_shares + share_size
total_invested <- total_invested - amd_df$costs_proceeds[i] 
    } 
amd_df$accumulated_shares[i] <- accumulated_shares  }
}
 
# Ensure final sell on the last day
amd_df$trade_type[nrow(amd_df)] <- "sell_all" 
amd_df$costs_proceeds[nrow(amd_df)] <- amd_df$close[nrow(amd_df)] * accumulated_shares amd_df$accumulated_shares[nrow(amd_df)] <- 0 
 
# Total Profit/Loss Calculation omitting any N/A values total_profit_loss <- sum(amd_df$costs_proceeds, na.rm = TRUE) 
 
# Invested Capital Calculation omitting any N/A values total_invested_capital <- -sum(amd_df$costs_proceeds[amd_df$trade_type == "buy"], na.rm = TRUE) 
 
# ROI Calculation 
ROI <- (total_profit_loss / total_invested_capital) * 100 
 
# Print the results of "Total Profit/Loss", "Total Invested Capital" and 
"ROI" cat("Total Profit/Loss: ", total_profit_loss, "\n") 
## Total Profit/Loss:  41569.02 cat("Total Invested Capital: ", total_invested_capital, "\n") 
## Total Invested Capital:  170894 cat("ROI: ", ROI, "%\n") 
## ROI:  24.32445 % 
```


### Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.


```r
# Assigning ROI values with Step 3's strategy as ROI_1 and Step 5's strategy as ROI_2 
ROI_1 <- 45.90453 
ROI_2 <- 24.32445 
 
# Calculating ΔROI 
ΔROI <- ROI_2 - ROI_1 
 
# Assigning PnL values where Step 3's strategy is recorded as PnL_1 and Step 5's strategy as PnL_2 
PnL_1 <- 196930 
PnL_2  <- 41569 
 
# Calculating theΔPnL ΔPnL <- PnL_2 - PnL_1 
 
# Printing the results of "ΔROI" and "ΔPnL" cat("ΔROI:", ΔROI, "\n") ## ΔROI: -21.58008 cat("ΔPnL:", ΔPnL, "\n") 
## ΔPnL: -155361 
# Convert the date column to Date type and Close as numeric to enable plotting of data. 
amd_df$date <- as.Date(amd_df$date) amd_df$close <- as.numeric(amd_df$close) 
 
amd_df <- amd_df[, c("date", "close")] 
plot(amd_df$date, amd_df$close,'l') 
```

Though both strategies generate relatively small profits for AMD due to the impact of Covid-19, step 5’s strategy yields a lower profit compared to the strategy used in step 3. Step 3 achieved a profit of $196,930 with a total invested capital of $428,999, translating to an ROI of 45.90453%. In contrast, the profit-taking strategy in step 5 gained a profit of $41,569 with a total invested capital of $170,894, yielding an ROI of 24.32445%. The lower ROI in step 5 is attributed to the global pandemic known as Covid-19, which limited the increase in purchase price, resulting in difficulties to reach above 20% from the average purchase price. This limitation in purchase prices is attributed to the initial phases of the pandemic where on March 11, 2020, the WHO officially declared COVID-19 a pandemic. This announcement, combined with the rising number of cases and implementation of lockdowns triggered widespread panic. As a result, stock markets around the world plunged leading to market uncertainty and instability. Evidence is seen with the graph where the company AMD’s stock price decreased by 34.45% from $58.90 at 2020-02-19, the peak, to $38.61 on 2020-03-23, the trough. However, AMD’s stock prices increase where the customised trading period subtly reveals this from 2020-03-23 onwards as shown on the graph. This is as restrictions had surged for a demand in technological products and services such as remote work solutions, cloud computing and entertainment. This heightened demand for powerful processors and graphics cards to accompany people’s needs caused AMD to maintain competitive strength in the market. In conclusion, while both trading strategies yielded reduced profit during the customised trading period, step 3 outperformed step 5, achieving a higher ROI due to COVID-19’s constraints on stock price increases. 




