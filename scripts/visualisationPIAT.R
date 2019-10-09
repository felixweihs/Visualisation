library(tidyverse)
BOM_data <- read_csv("data/BOM_data.csv")
BOM_stations <- read_csv("data/BOM_stations.csv")

#Challenge - Puttin it all together
#Prepare datasets from BOM data
separated_min_max <- separate(BOM_data, col = Temp_min_max, into = c('min', 'max'), sep = "/") #Separate temperatures into min and max
stations_very_long <- BOM_stations %>%  
gather(key = "Station_number", value = "values", -info)  
stations_tidy <- stations_very_long %>%  
spread(key = info, value = values) 
stations_tidy <- mutate(stations_tidy, Station_number = as.numeric(Station_number))
BOM_combined <- left_join(separated_min_max, stations_tidy)

#Challenge 1 - For the Perth station (ID 9225), produce three scatter plots showing the 
#relationship between the maximum temperature and each other measurement recorded 
#(minimum temperature, rainfall and solar exposure)
question_1 <- BOM_combined %>%
  filter(Station_number == "9225") %>% 
  mutate(min = as.numeric(min), max = as.numeric(max), Rainfall = as.numeric(Rainfall), Solar_exposure = as.numeric(Solar_exposure), elev = as.numeric(elev), end = as.numeric(end), lat = as.numeric(lat)) 
question_1_plot1 <- ggplot(
  data = question_1, 
  mapping = aes(y = min, x = max)
) + geom_point(alpha = 0.2, size = 1) + 
  labs(title = "Minimum temperature depending on maximum temperature", 
  x = "Maximum temperature", 
  y = "Minimum temperature")

question_1_plot2 <- ggplot(
  data = question_1, 
  mapping = aes(y = Rainfall, x = max)
) + geom_point(alpha = 0.2, size = 1) + 
  labs(title = "Rainfall depending on maximum temperature", 
       x = "Maximum temperature", 
       y = "Rainfall")

question_1_plot3 <- ggplot(
  data = question_1, 
  mapping = aes(y = Solar_exposure, x = max)
) + geom_point(alpha = 0.2, size = 1) + 
  labs(title = "Solar exposure depending on maximum temperature", 
       x = "Maximum temperature", 
       y = "Solar exposure")

#Challenge 2 - Display these four measurements for the Perth station in a single scatter 
#plot by using additional aesthetic mappings.
#- You may need to try a few different data/aesthetic mappings to find one you like.
question_2_plot <- ggplot(
  data = question_1, 
  mapping = aes(y = min, x = max, size = Rainfall, colour = Solar_exposure)
) + geom_point() + 
  labs(title = "Summary", 
       x = "Maximum temperature", 
       y = "Minimum temperature",
       caption = "Data source: BOM_stations") 


#Challenge 3 - Take the four plots you have produced in Q1 and Q2 and save them as a multi-panel figure.
question_3_plot <- plot_grid(question_1_plot1, question_1_plot2, question_1_plot3, question_2_plot, labels = "AUTO")
ggsave("results/question3.jpg", plot = question_3_plot, width = 20, height = 14, units = "cm")




#Challenge 4 - Using the entire BOM dataset, calculate the average monthly rainfall for each station. 
#Produce a lineplot to visualise this data and the state each station is in.

#Tidy up BOM_data rainfall - take out no measurement rows and mutate Rainfall data into doubles
BOM_data_tidy_rainfall <- BOM_data %>% filter(Rainfall != '-') %>% mutate(Rainfall = as.numeric(Rainfall))

#Calculate average rainfall for each station in each month
BOM_data_avrg_rainfall <- BOM_data_tidy_rainfall %>% 
  group_by(Station_number, Month) %>% 
  summarise(mean_rainfall = mean(Rainfall))

#Use tidy BOM_Stations data
BOM_stations_tidy <- BOM_stations %>%  
  gather(Station_name, name, -info) %>% #tidy data intermediate
  filter(info == "state") %>%  #Only select rows with state
  select(Station_number = Station_name, name) %>% #Only retain colums with station names and states
  mutate(Station_number = as.numeric(Station_number)) %>%  #Convert station names from character to double
  mutate(State = name) #Clean up names

#Merge station rainfall information with state information
BOM_data_rainfall_merged <- inner_join(BOM_stations_tidy, BOM_data_avrg_rainfall)

#Plotting average rainfall per month and station
question_4 <- ggplot(
  data = BOM_data_rainfall_merged, 
  mapping = aes(y = mean_rainfall, x = Month, group = Station_number, colour = State)
) + geom_line(size = 2) + 
      labs(title = "Average rainfall by station and month", 
       x = "Month", 
       y = "Average Rainfall [mm]",
       caption = "Data source: BOM_stations") + 
  theme(legend.position="bottom",
        legend.title = element_text(colour="black", size=14, face="bold"),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"))
# scale_x_continuous(breaks = c("1","2","3","4","5","6","7","8","9","10","11","12"), labels=c("Jun", "Feb","Mar","Jun","Feb","Mar", "Jun", "Feb", "Mar", "Jun", "Feb","Mar"))

ggsave("results/question4.jpg", plot = question_4, width = 14, height = 14, units = "cm")


