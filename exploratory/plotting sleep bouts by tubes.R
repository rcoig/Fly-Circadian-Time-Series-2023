# Load necessary libraries
library(dplyr)
library(lubridate)
library(tidyr)

# Read the tab-delimited file into a data frame
df <- read.delim("data/DAM2/Monitor1.txt", header = FALSE, stringsAsFactors = FALSE)

# Rename columns based on the description
colnames(df) <- c("Index", "Date", "Time", "Monitor_Status", "Extras", "Monitor_Number", "Unused_1", "Data_Type",
                  "Unused_2", "Light_Sensor", paste0("Tube_", 1:32))

# Convert Date and Time to POSIXct format for easy manipulation
df$DateTime <- as.POSIXct(paste(df$Date, df$Time), format = "%d %b %y %H:%M:%S")

# Convert wide format to long format to analyze each tube separately
df_long <- df %>% 
  select(c("DateTime", starts_with("Tube_"))) %>% 
  gather(key = "Tube", value = "Ct_Value", -DateTime)

# Create one-minute summaries
df_summary <- df_long %>%
  group_by(Tube, Minute = floor_date(DateTime, "minute")) %>%
  summarise(Summary_Ct = sum(Ct_Value))

df_summary$Tube<-as.factor(df_summary$Tube)

# Filter the data to include only records between 9 AM on October 5th and 9 AM on October 9th
start_time <- as.POSIXct("2023-10-05 08:55:00")
end_time <- as.POSIXct("2023-10-09 09:00:00")
df_summary <- df_summary %>% filter(Minute >= start_time & Minute <= end_time)

# Initialize an empty data frame to store sleep bouts
df_bouts <- data.frame()

# Loop through each Tube to identify sleep bouts
for(tube in unique(df_summary$Tube)) {
  
  # Filter data for the current Tube
  df_tube <- df_summary %>% filter(Tube == tube) %>% arrange(Minute)
  
  # Initialize variables to keep track of sleep bouts
  bout_count = 0
  bout_length = 0
  tube_bouts = data.frame(Tube = character(), Bout_Start = POSIXct(), Bout_Length = integer())
  
  # Loop through each minute to identify sleep bouts
  for(i in 1:nrow(df_tube)) {
    if(df_tube$Summary_Ct[i] == 0) {
      bout_length = bout_length + 1
      if(bout_length == 5) {
        bout_count = bout_count + 1
        bout_start = df_tube$Minute[i] - minutes(4)
        tube_bouts = rbind(tube_bouts, data.frame(Tube = tube, Bout_Start = bout_start, Bout_Length = bout_length))
      } else if(bout_length > 5) {
        tube_bouts[nrow(tube_bouts), "Bout_Length"] = bout_length
      }
    } else {
      bout_length = 0
    }
  }
  
  # Print debug information
  print(paste("Tube:", tube, "Bout count:", bout_count))
  
  # Add to the overall sleep bouts data frame
  df_bouts = rbind(df_bouts, tube_bouts)
}

# Add Date column to df_bouts
df_bouts$Date <- floor_date(df_bouts$Bout_Start, "day")
df_bouts$Tube<-as.factor(df_bouts$Tube)

# Classify each bout as occurring during the light or dark cycle
df_bouts$Cycle <- ifelse(hour(df_bouts$Bout_Start) >= 9 & hour(df_bouts$Bout_Start) < 21, "Light", "Dark")

# Count sleep bouts per day and cycle for each tube
summary_bouts_per_day_cycle <- df_bouts %>%
  group_by(Tube, Date, Cycle) %>%
  summarise(Total_Bouts = n())


# Create the line plot for sleep bouts per cycle
ggplot(summary_bouts_per_day_cycle, aes(x = Date, y = Total_Bouts, color = Cycle)) +
  geom_line() +
  facet_wrap(~ Tube, scales = "free_y") +
  ggtitle("Total Sleep Bouts by Tube, Day, and Cycle") +
  xlab("Day") +
  ylab("Total Sleep Bouts") +
  scale_color_manual(values = c("Light" = "blue", "Dark" = "black"))

