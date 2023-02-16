# Load the necessary libraries
library(rbart)
library(ggplot2)
library(tidyquant)

extract_prices <- function(stock_symbol) {
  # Create the URL for the stock's daily prices
  url_stocks <- paste0("https://finance.yahoo.com/lookup/", stock_symbol,
                       "/history?period1=1&period2=9999999999&interval=1d&filter=history&frequency=1d")
  
  # Read the table of daily prices from the URL
  prices_df <- read_html(url_stocks) %>% 
    html_nodes(xpath = '//*[@id="Col1-1-HistoricalDataTable-Proxy"]/section/div[2]/table') %>% 
    html_table(fill = TRUE) %>% 
    as_tibble() %>% 
    select(Date, Open, High, Low, Close)
  
  # Return the data frame of daily prices
  return(stock_data)
}

# Load the stock data
#stock_data <- read.csv("stock_data.csv")

# Split the data into training and test sets
train_ind <- sample(1:nrow(stock_data), 0.8 * nrow(stock_data))
train_data <- stock_data[train_ind, ]
test_data <- stock_data[-train_ind, ]

# Fit the BART model to the training data
bart_model <- bart(formula = daily_return ~ ., data = train_data, ndpost = 500, seed = 12345)

# Make predictions on the test data
predictions <- predict(bart_model, newdata = test_data)

# Visualize the predictions
ggplot(test_data, aes(x = date, y = daily_return)) +
  geom_point(aes(color = "Actual")) +
  geom_line(aes(y = predictions, color = "Predicted")) +
  labs(title = "Daily Stock Performance",
       x = "Date",
       y = "Return",
       color = "")
