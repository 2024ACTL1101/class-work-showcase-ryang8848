
# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

## Instructions

### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
- `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```r
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```

#### Data Processing 
```r
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:

$$
E(R_i) = R_f + \beta_i (E(R_m) - R_f)
$$

Where:

- $E(R_i)$ is the expected return on the capital asset,
- $R_f$ is the risk-free rate,
- $\beta_i$ is the beta of the security, which represents the systematic risk of the security,
- $E(R_m)$ is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
  
$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$

```{r return}
# Calculate Daily Returns using lag
df <- df %>%
  mutate(daily_return_AMD = (AMD - lag(AMD)) / lag(AMD),
         daily_return_GSPC = (GSPC - lag(GSPC)) / lag(GSPC))

# Check if daily returns have been calculated correctly
summary(df$daily_return_AMD)
summary(df$daily_return_GSPC)
```

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
  
$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```{r riskfree}
# Calculate Daily Risk-Free Rate
df <- df %>%
  mutate(daily_risk_free = (1 + RF / 100)^(1 / 360) - 1)

# Check if daily risk-free rate has been calculated correctly
summary(df$daily_risk_free)
```


- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```{r excess return}
# Calculate Excess Returns
df <- df %>%
  mutate(excess_return_AMD = daily_return_AMD - daily_risk_free,
         excess_return_GSPC = daily_return_GSPC - daily_risk_free)

# Check if excess returns have been calculated correctly
summary(df$excess_return_AMD)
summary(df$excess_return_GSPC)
```


- **Perform Regression Analysis**: Using linear regression, we estimate the beta (\(\beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```{r lm}
# Remove rows with NA values before performing regression
df_clean <- df %>%
  filter(!is.na(excess_return_AMD) & !is.na(excess_return_GSPC))

# Check the cleaned data
summary(df_clean$excess_return_AMD)
summary(df_clean$excess_return_GSPC)

# Ensure there are non-NA cases before regression
if (nrow(df_clean) > 0) {
  # Perform Regression Analysis
  regression_result <- lm(excess_return_AMD ~ excess_return_GSPC, data = df_clean)
  
  # Display the summary of the regression analysis
  summary(regression_result)
} else {
  print("No non-NA cases available for regression analysis.")
}


#### Interpretation

What is your \(\beta\)? Is AMD more volatile or less volatile than the market?

The calculated beta value of 1.5699987 suggests that AMD is more volatile than the market. Beta measures a stock's sensitivity to market movements, acting as a volatility ratio, . With a beta value greater than 1, AMD's stock price is expected to fluctuate by approximately 1.57% for every 1% increase in the market's excess returns. This means AMD's stock will have greater fluctuations than the market. During market upswings, when the market's return increases by 1%, AMD's return is expected to rise by 1.57%, outperforming the market. In contrast, during downturns, when the market's return drops by 1%, AMD's return is expected to fall by about 1.57%, underperforming the market. This higher beta value indicates that investing in AMD carries more risk, with the possibility of both higher returns and greater losses depending on market conditions.



#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```{r plot}
# Plotting the scatter plot and regression line
plot <-ggplot(df_clean, aes(x = excess_return_GSPC, y = excess_return_AMD)) +
  geom_point(color = "black", alpha = 0.5) +
  geom_smooth(method = "lm", color = "darkblue") +
  labs(title = "CAPM Analysis: AMD vs S&P 500",
       x = "Excess Return of S&P 500",
       y = "Excess Return of AMD") +
  theme_minimal()
print(plot)
```

### Step 3: Predictions Interval
Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD's annual expected return.



**Answer:**

```{r pi}
 # Given values
rf_rate <- 5/100
expected_return <- 133/1000
trading_days <- 252

# Assuming Model is a linear regression model of excess returns
Model <- lm(excess_return_AMD ~ excess_return_GSPC, data = df)

# Extract the beta coefficient for AMD from the regression results
beta_amd <- coef(Model)["excess_return_GSPC"]

# Calculate the expected return for AMD using the CAPM formula
expected_return_amd <- rf_rate + beta_amd * (expected_return - rf_rate)

# Mean of S&P excess returns
mean_GSPC <- mean(df$excess_return_GSPC, na.rm = TRUE)

# Sample size
n <- length(df$excess_return_GSPC) - 1

# Standard error of the estimate
se <- sqrt(sum(residuals(Model)^2) / (n - 2))

# Daily excess return on the market
x_f <- (expected_return - rf_rate) / trading_days

# Sum of squares of S&P excess returns
SSX <- sum((df$excess_return_GSPC - mean_GSPC)^2, na.rm = TRUE)

# Calculate the daily standard error of the forecast (sf_daily)
sf_daily <- se * sqrt(1 + 1/n + (x_f - mean_GSPC)^2 / SSX)

# Calculate annual standard error of the forecast
sf_annual <- sf_daily * sqrt(trading_days)

# For the 90% prediction interval, use the t-distribution for prediction intervals
alpha <- 0.1
t_value <- qt(1 - alpha / 2, df = n - 2)

# Calculate the prediction interval 
lower_bound <- expected_return_amd - t_value * sf_annual
upper_bound <- expected_return_amd + t_value * sf_annual

# Print results
cat("90% Prediction Interval for AMD's Annual Expected Return:", lower_bound * 100, "% to", upper_bound * 100, "%\n")
```
