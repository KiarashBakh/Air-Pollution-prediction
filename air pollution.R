library(data.table)
library(DMwR2)
library(dplyr)
library(forecast)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(zoo)


setwd("D:/Data Science/Projects/Kaggel/Air Polution")
data <- read.csv("air_pollution.csv")
str(data)
summary(data)
# years after 2021 are acceptable for all countries (< 50% NA)

train <- data[-smp, ]

missing_values <- colSums(is.na(train)) # missing values count

#group by countries
countries <- aggregate(. ~ train$country, train[, 3:ncol(train)], sum)

# Function to get the count of missing values for each column
get_missing_counts <- function(data) {
  return(colSums(is.na(data)))
}

# # categorizing missing values by country
# Split the data by country and apply the function to each group
missing_counts_by_country <- lapply(
  split(train[, 3:ncol(train)], train$country),
  get_missing_counts
)

# Convert the result to a data frame
country_missing <- data.frame(do.call(rbind, missing_counts_by_country))

# Add a column for country names
country_missing$country <- rownames(country_missing)

# Reorder the columns for clarity
country_missing <- country_missing[
  , c("country", names(train[, 3:ncol(train)]))
]

country_missing <- country_missing %>%
  mutate(total_missing = rowSums(select(., -country)))

data_count_by_country <- as.data.frame(table(train$country))

# Rename the columns for clarity
colnames(data_count_by_country) <- c("country", "DataRowsCount")

# add percent to missing count
country_missing$missing_percent <- round((
  country_missing$total_missing / (data_count_by_country$DataRowsCount[
    match(country_missing$country, data_count_by_country$country)
  ] * 7)
) * 100,
2)

# # categorizing missing values by city
city_missing <- data.frame(
  city = train[, 1],
  missing_count = rowSums(is.na(train))
)

city_missing$missing_percent <- round(city_missing$missing_count / 7 * 100, 2)


### Modeling
# Selecting relevant features and changing X20.. to 20..
features <- seq(from = 2017, to = 2023)
colnames(train)[3:ncol(train)] <- as.character(features)

# change Invalid Number to NA
train[, 3:ncol(train)] <- lapply(train[, 3:ncol(train)], as.character)
train[, 3:ncol(train)][train[, 3:ncol(train)] == "Invalid Number"] <- NA


## outliers handling with IQR (nullify)
outlier_detection <- function(year) {
  column <- as.numeric(train[[year]])
  # Calculate lower and upper bounds
  lowband <- mean(column, na.rm = TRUE) - 3 * sd(column, na.rm = TRUE)
  upperband <- mean(column, na.rm = TRUE) + 3 * sd(column, na.rm = TRUE)
  # Replace outliers with NA
  column[which(column > upperband | column < lowband)] <- NA
  return(column)
}

# Loop through each feature and apply outlier_detection function
for (i in as.character(features)) {
  train[[i]] <- outlier_detection(i)
}
colSums(is.na(train))


# Pivot data for each city separately
pivot_city <- function(city) {
  pivot_table <- pivot_longer(
    city, cols = -c(city, country),
    names_to = "year", values_to = "value"
  )
  pivot_table$year <- as.numeric(pivot_table$year)
  return(pivot_table)
}


# function to remove leading Na for each city
remove_leading_na <- function(table) {
  index <- 1
  # Loop through rows until a non-NA value is found
  while (index <= nrow(table) && is.na(table[index, "value"])) {
    index <- index + 1
  }
  return(table[index:nrow(table), ])
}


# Imputing Missing Values with interpolation
impute_missing <- function(table) {
  zoo_data <- zoo(table$value, order.by = table$year)
  table$interpolated_value <- as.numeric(na.approx(zoo_data))
  return(table)
}

# use stationary techniques for each city
transform_to_stationary <- function(table) {
  # Differencing
  table$differenced_value <- c(
    0,
    diff(as.double(table[["interpolated_value"]]))
  )
  # Log Transformation
  table$log_transformed_value <- log(
    table$interpolated_value + 1
  )  # Adding 1 to handle zero values
  return(table)
}


# do a time series analysis on the city
time_series_analysis <- function(table) {
  ts_data <- ts(table$value, start = c(2017, 1), frequency = 1)
  arima_model <- auto.arima(ts_data)
  forecast_values <- forecast(arima_model, h = nrow(table))
  return(forecast_values)
}

results <- train[1, ] %>%
  pivot_city %>%
  remove_leading_na %>%
  impute_missing %>%
  transform_to_stationary %>%
  time_series_analysis

results