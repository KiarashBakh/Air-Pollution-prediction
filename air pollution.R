library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)

setwd("D:/Data Science/Projects/Kaggel/Air Polution")
data <- read.csv("air_pollution.csv")
str(data)
summary(data)
# years after 2021 are acceptable for all countries (< 50% NA)

# train and test sampling
set.seed(5432)
smp <- sample(1:nrow(data), nrow(data) * 0.3, replace = FALSE)
train <- data[-smp, ]
test <- data[smp, ]

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
# outliers handling with IQR (nullify)
...

# use moving average for every NA
...

# use stationary techniques
...

# Pivot data for each city separately
pivot_city <- function(city) {
  return(pivot_longer(
    city, cols = starts_with("X"),
    names_to = "Year", values_to = "Value"
  )
  )
}

# do a time series analysis on the city
time_series_analysis <- function(table){
  ...
}

# add each result to data
...

# fine tune with 2023 as target
...

# use model to predict 2024
...

# use result to gain insight in Tableau