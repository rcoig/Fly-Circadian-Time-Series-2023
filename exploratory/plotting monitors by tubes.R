# Load necessary libraries
library(dplyr)
library(lubridate)
library(tidyr)

# Read the tab-delimited file into a data frame
df <- read.delim("data/DAM2/Monitor1.txt", header = FALSE, stringsAsFactors = FALSE)

# Rename columns based on the description
colnames(df) <- c("Index", "Date", "Time", "Monitor_Status", "Extras", "Monitor_Number", "Tube_Number", "Data_Type",
                  "Unused_1", "Light_Sensor", paste0("Tube_", 1:32))

# Filter the rows where Data_Type is "Ct"
df <- df %>% filter(Data_Type == "Ct")

# Convert Date and Time to POSIXct format for easy manipulation
df$DateTime <- as.POSIXct(paste(df$Date, df$Time), format = "%d %b %y %H:%M:%S")

# Convert wide format to long format to analyze each tube separately
df_long <- df %>% 
  select(c("DateTime", starts_with("Tube_"))) %>% 
  gather(key = "Tube", value = "Ct_Value", -DateTime)

# Filter the data to include only records between 9 AM on October 5th and 9 AM on October 9th
start_time <- as.POSIXct("2023-10-05 08:55:00")
end_time <- as.POSIXct("2023-10-09 09:00:00")
df_summary <- df_summary %>% filter(Minute >= start_time & Minute <= end_time)



# Create one-minute summaries
df_summary <- df_long %>%
  group_by(Tube, Minute = floor_date(DateTime, "minute")) %>%
  summarise(Summary_Ct = sum(Ct_Value))

# Print the first few rows of df_summary for debugging
print(head(df_summary))

# Create an indicator for sleep (5 mins or more of 0 in the Ct field)
df_sleep <- df_summary %>%
  arrange(Minute) %>%
  group_by(Tube, Date = floor_date(Minute, "day")) %>%
  mutate(Sleep_Ind = ifelse(Summary_Ct == 0, 1, 0),
         Sleep_Flag = lag(Sleep_Ind, default = 0) != Sleep_Ind,
         Sleep_Session = cumsum(Sleep_Flag & Sleep_Ind == 1))

# Identify sleep sessions lasting 5 minutes or more
df_sleep_filtered <- df_sleep %>%
  group_by(Tube, Date, Sleep_Session) %>%
  filter(Sleep_Session > 0) %>%
  summarise(Sleep_Duration = sum(Sleep_Ind)) %>%  
  filter(Sleep_Duration >= 5)

# Summary per day of sleep for each tube
summary_per_day <- df_sleep_filtered %>%
  group_by(Tube, Date) %>%
  summarise(Total_Sleep = sum(Sleep_Duration))

# Create the line plot
ggplot(summary_per_day, aes(x = Date, y = Total_Sleep)) +
  geom_line() +
  facet_wrap(~ Tube, scales = "free_y") +  # Separate plot for each tube
  ggtitle("Total Sleep Time by Tube and Day") +
  xlab("Day") +
  ylab("Total Sleep (minutes)")