# Process of importing and organzing the data a bit



# Set libPaths.
.libPaths("/Users/richardfarr/.exploratory/R/4.0")

# Load required packages.
library(janitor)
library(lubridate)
library(hms)
library(tidyr)
library(stringr)
library(readr)
library(forcats)
library(RcppRoll)
library(dplyr)
library(tibble)
library(bit64)
library(exploratory)

# Steps to produce Survey_data
`Survey_data` <- exploratory::read_delim_file("/Users/richardfarr/Documents/UT Final Project November 2020/data/Marketing Project-Survey data.csv" , ",", quote = "\"", skip = 0 , col_names = TRUE , na = c('','NA') , locale=readr::locale(encoding = "UTF-8", decimal_mark = ".", tz = "America/Los_Angeles", grouping_mark = "," ), trim_ws = TRUE , progress = FALSE) %>%
  readr::type_convert() %>%
  exploratory::clean_data_frame()

# Steps to produce the output

# Import the two datasets
all_falcon_data <- exploratory::read_delim_file("/Users/richardfarr/Documents/UT Final Project November 2020/data/Marketing Project-Flight data.csv" , ",", quote = "\"", skip = 0 , col_names = TRUE , na = c('','NA') , locale=readr::locale(encoding = "UTF-8", decimal_mark = ".", tz = "America/Los_Angeles", grouping_mark = "," ), trim_ws = TRUE , progress = FALSE) %>%
  readr::type_convert() %>%
  exploratory::clean_data_frame() %>%
  
  # Combine into one big dataset
  left_join(Survey_data, by = c("CustomerID" = "CustomerId")) %>%
  
  # Group similar columns together
  reorder_cols(Satisfaction, Gender, CustomerType, TypeTravel, Class, Age, Flight_Distance, DepartureDelayin_Mins, ArrivalDelayin_Mins, Gate_location, Seat_comfort, Departure.Arrival.time_convenient, Food_drink, Inflightwifi_service, Inflight_entertainment, Online_support, Ease_of_Onlinebooking, Onboard_service, Leg_room_service, Baggage_handling, Checkin_service, Cleanliness, Online_boarding, CustomerID) %>%
  
  # Replace Missing Values on future factors with Unknown
  mutate_at(vars(CustomerType, TypeTravel, Departure.Arrival.time_convenient, Food_drink, Onboard_service), funs(str_replace_na(., "Unknown"))) %>%
  
  # Convert columns to factors as needed
  mutate_at(vars(Satisfaction, Gender, CustomerType, TypeTravel, Class, Gate_location, Seat_comfort, Departure.Arrival.time_convenient, Food_drink, Inflightwifi_service, Inflight_entertainment, Online_support, Ease_of_Onlinebooking, Onboard_service, Leg_room_service, Baggage_handling, Checkin_service, Cleanliness, Online_boarding), funs(factor)) %>%
  
  # Relevel the Factors one by one
  mutate(Satisfaction = fct_relevel(Satisfaction, "satisfied","neutral or dissatisfied"), CustomerType = fct_relevel(CustomerType, "Loyal Customer", "disloyal Customer", "Unknown"), Class = fct_relevel(Class, "Business","Eco Plus","Eco")) %>%
  
  # Rename some of the levels so easier to read
  mutate(Class = recode(Class, "Eco Plus" = "Economy Plus", "Eco" = "Economy")) %>%
  
  # Relevel a bunch of simlar factors all at once
  mutate_at(vars(Seat_comfort, Departure.Arrival.time_convenient, Food_drink, Inflightwifi_service, Inflight_entertainment, Online_support, Ease_of_Onlinebooking, Onboard_service, Leg_room_service, Baggage_handling, Checkin_service, Cleanliness, Online_boarding), funs(fct_relevel(., "excellent", "good", "acceptable", "need improvement", "poor", "extremely poor", "Unknown"))) %>%
  
  # Relevel the gate locations into a better order
  mutate(Gate_location = fct_relevel(Gate_location, "very convinient", "Convinient", "manageable", "need improvement", "Inconvinient", "very inconvinient")) %>%
  
  # Recode the satisfaction levels a bit
  mutate(Satisfaction = recode(Satisfaction, "satisfied" = "Satisfied", "neutral or dissatisfied" = "Neutral or Dissatisfied")) %>%
  
  # Create a numeric version of satisfaction for correlation analysis later
  mutate(`Satisfaction (Numeric)` = Satisfaction) %>%
  
  # Satisfied is 1 when its numeric
  mutate(`Satisfaction (Numeric)` = recode(`Satisfaction (Numeric)`, "Satisfied" = "1", "Neutral or Dissatisfied" = "0")) %>%
  
  # Turn satisfaction numeric into a number
  mutate(`Satisfaction (Numeric)` = parse_number(`Satisfaction (Numeric)`))


summary(all_falcon_data)

saveRDS(all_falcon_data, file = "01_Data_Combined.rds")
