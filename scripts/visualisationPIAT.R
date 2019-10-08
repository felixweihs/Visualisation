library(tidyverse)
BOM_data <- read_csv("data/BOM_data.csv")
BOM_stations <- read_csv("data/BOM_stations.csv")

#Challenge - Puttin it all together
#Prepare datasets from BOM data
BOM_stations_tidy <- BOM_stations %>%  
  gather(Station_name, name, -info) %>% #tidy data intermediate
  filter(info == "state") %>%  #Only select rows with state
  select(info = Station_name, name) %>% #Only retain colums with station names and states
  mutate(info = as.numeric(info)) #Convert station names from character to double

separated_min_max <- separate(BOM_data, col = Temp_min_max, into = c('min', 'max'), sep = "/") #Separate temperatures into min and max
avrg_temp_diff_station <- separated_min_max %>% 
  mutate(temp_diff = as.numeric(max) - as.numeric(min)) %>% #Calculate temperature difference
  filter(temp_diff != 'NA') %>% #Filter out rows without a valid temperature difference output
  group_by(Station_number) %>% #Group by Station numbers
  summarise(avrg_temp_diff = mean(temp_diff)) %>% #Calculate average temperature difference
  rename(info = Station_number) #Rename Station_number for following joining function

BOM_stations_state_temp_diff <- inner_join(BOM_stations_tidy, avrg_temp_diff_station) %>% 
  group_by(name) %>% # Group by station name
  summarise(avrg_temp_diff_state = mean(avrg_temp_diff)) %>% #Calculate average temperature difference by state
  arrange(avrg_temp_diff_state)#Arrange by lowest average daily temp difference per state
write_csv(BOM_stations_state_temp_diff, "results/question3.csv") #Output result into csv file in the results folder

separated_min_max <- separate(BOM_data, col = Temp_min_max, into = c('min', 'max'), sep = "/") #Separate temperatures into min and max
stations_very_long <- BOM_stations %>%  
gather(key = "Station_number", value = "values", -info)  
stations_tidy <- stations_very_long %>%  
spread(key = info, value = values) 
stations_tidy <- mutate(stations_tidy, Station_number = as.numeric(Station_number))
BOM_combined <- left_join(separated_min_max, stations_tidy)
