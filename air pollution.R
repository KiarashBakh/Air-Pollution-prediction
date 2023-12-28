library(data.table)
library(ggplot2)
library(dplyr)

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

# Split the data by country and apply the function to each group
missing_counts_by_column <- lapply(
  split(train[, 3:ncol(train)], train$country),
  get_missing_counts
)

# Convert the result to a data frame
country_missing_count <- data.frame(do.call(rbind, missing_counts_by_column))

# Add a column for country names
country_missing_count$Country <- rownames(country_missing_count)

# Reorder the columns for clarity
country_missing_count <- country_missing_count[
  , c("Country", names(train[, 3:ncol(train)]))
]

country_missing_count <- country_missing_count %>%
  mutate(total_missing_percent = rowSums(select(., -Country)))

