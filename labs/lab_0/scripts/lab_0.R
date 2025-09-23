# Load the tidyverse library
library(tidyverse)

# Read in the car sales data
car_data <- read_csv("data/car_sales_data.csv")

# data structure
glimpse(car_data)

# Variable types
var_types <- sapply(car_data, class)
var_types

# Problematic names
problematic_names <- names(car_data)[!make.names(names(car_data)) == names(car_data)]
problematic_names

# Look at the first few rows
head(car_data)

# Look at the tibble version (what we have)
car_data

# Convert to regular data frame and display
car_df <- as.data.frame(car_data)
car_df

library(dplyr)

# Select just Model and Mileage columns
car_data %>%
  select(Model, Mileage)

# Select Manufacturer, Price, and Fuel type
car_data %>%
  select(Manufacturer, Price, `Fuel type`)

# Challenge: Select all columns EXCEPT Engine size
car_data %>%
  select(-`Engine size`)

# Rename 'Year of manufacture' to year
car_data <- car_data %>%
  rename(year = `Year of manufacture`)

# Check that it worked
names(car_data)

# Create an 'age' column (2025 minus year of manufacture)
# Create a mileage_per_year column
car_data <- car_data %>%
  mutate(
    age = 2025 - year,
    mileage_per_year = Mileage / age
  )

# Look at your new columns
car_data %>%
  select(Model, year, age, Mileage, mileage_per_year) %>%
  head()

# Create a price_category column where if price is < 15000, its is coded as budget, between 15000 and 30000 is midrange and greater than 30000 is mid-range (use case_when)
car_data <- car_data %>%
  mutate(
    price_category = case_when(
      Price < 15000 ~ "budget",
      Price >= 15000 & Price <= 30000 ~ "midrange",
      Price > 30000 ~ "mid-range"
    )
  )

# Check your categories select the new column and show it
car_data %>%
  select(price_category) %>%
  head()

# Find all Toyota cars
toyota_cars <- car_data %>%
  filter(Manufacturer == "Toyota")

# Find cars with mileage less than 30,000
low_mileage_cars <- car_data %>%
  filter(Mileage < 30000)

# Find luxury cars (from price category) with low mileage
luxury_low_mileage <- car_data %>%
  filter(price_category == "mid-range", Mileage < 30000)

head(toyota_cars)
head(low_mileage_cars)
head(luxury_low_mileage)

# Find cars that are EITHER Honda OR Nissan
honda_nissan <- car_data %>%
  filter(Manufacturer %in% c("Honda", "Nissan"))

# Find cars with price between $20,000 and $35,000
mid_price <- car_data %>%
  filter(between(Price, 20000, 35000))

# Find diesel cars less than 10 years old
diesel_recent <- car_data %>%
  filter(`Fuel type` == "Diesel", 2025 - year < 10) %>%
  count()

head(honda_nissan)
head(mid_price)
head(diesel_recent)

# Calculate average price by manufacturer
avg_price_by_brand <- car_data %>%
  group_by(Manufacturer) %>%
  summarize(avg_price = mean(Price, na.rm = TRUE))

avg_price_by_brand

# Calculate average mileage by fuel type
avg_mileage_by_fuel <- car_data %>%
  group_by(`Fuel type`) %>%
  summarize(avg_mileage = mean(Mileage, na.rm = TRUE))

# Count cars by manufacturer
cars_by_manufacturer <- car_data %>%
  count(Manufacturer)

avg_mileage_by_fuel
cars_by_manufacturer

# Frequency table for price categories
price_freq <- car_data %>%
  count(price_category)

price_freq
