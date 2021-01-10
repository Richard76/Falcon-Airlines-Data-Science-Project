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
exploratory::read_delim_file("/Users/richardfarr/Documents/UT Final Project November 2020/Marketing Project-Flight data.csv" , ",", quote = "\"", skip = 0 , col_names = TRUE , na = c('','NA') , locale=readr::locale(encoding = "UTF-8", decimal_mark = ".", tz = "America/Los_Angeles", grouping_mark = "," ), trim_ws = TRUE , progress = FALSE) %>%
  readr::type_convert() %>%
  exploratory::clean_data_frame() %>%
  
  # Combine into one big dataset
  left_join(Survey_data, by = c("CustomerID" = "CustomerId")) %>%
  
  # Step 1 - Create a dummy variable column where its 1 if there is a NA there.
  # 
  # Step 2 - Replace missing values with 0 to deal with the missing values issue.
  mutate(ArrivalDelay_NA = ifelse(is.na(ArrivalDelayin_Mins), 1, 0), ArrivalDelayin_Mins = impute_na(ArrivalDelayin_Mins, type = "value", val = "0")) %>%
  
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
  mutate(`Satisfaction (Numeric)` = Satisfaction, ArrivalDelayin_Mins = parse_number(ArrivalDelayin_Mins)) %>%
  
  # Satisfied is 1 when its numeric
  mutate(`Satisfaction (Numeric)` = recode(`Satisfaction (Numeric)`, "Satisfied" = "1", "Neutral or Dissatisfied" = "0")) %>%
  
  # Turn satisfaction numeric into a number
  mutate(`Satisfaction (Numeric)` = parse_number(`Satisfaction (Numeric)`), `log(DepartureDelayin_Mins)` = log(DepartureDelayin_Mins), `log(ArrivalDelayin_Mins)` = log(ArrivalDelayin_Mins)) %>%
  
  # - A way to turn the arrival and departure info into something useful - 
  # 4 possible combinations based on late and on-time (ot)
  # factors releveled from best cast to worst case
  mutate(delay_summary = case_when(
    DepartureDelayin_Mins == 0 & ArrivalDelayin_Mins == 0 ~ "ot-ot",
    DepartureDelayin_Mins == 0 & ArrivalDelayin_Mins > 0 ~ "ot-late",
    DepartureDelayin_Mins > 0 & ArrivalDelayin_Mins == 0 ~ "late-ot",
    TRUE ~ "late-late"), delay_summary = factor(delay_summary), delay_summary = fct_relevel(delay_summary, "ot-ot","late-ot", "ot-late","late-late")) %>%
  
  # Attempt to categorize people into 5 equal (by count) bins
  mutate(Age_category = as.factor(ggplot2::cut_number(Age, 5))) %>%
  select(-`Satisfaction (Numeric)`, -`log(DepartureDelayin_Mins)`, -`log(ArrivalDelayin_Mins)`) %>%
  rename(Departure_Arrival_time_convenient = Departure.Arrival.time_convenient) %>%
  mutate(Flight_Distance = normalize(Flight_Distance))


summary(all_falcon_data)

saveRDS(all_falcon_data, file = "01_Data_Combined.rds")
