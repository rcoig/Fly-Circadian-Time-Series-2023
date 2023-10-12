# Load necessary libraries
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

# List all Monitor files
monitor_files <- list.files(path = "data/DAM2", pattern = "Monitor.*\\.txt", full.names = TRUE)

# Initialize an empty data frame to store all summaries
all_summaries <- data.frame()

# Function to process each Monitor file
process_monitor_file <- function(file_path) {
  # Read the tab-delimited file into a data frame
  df <- read.delim(file_path, header = FALSE, stringsAsFactors = FALSE)
  
  # Rename columns
  colnames(df) <- c("Index", "Date", "Time", "Monitor_Status", "Extras", "Monitor_Number", "Unused_1", "Data_Type",
                    "Unused_2", "Light_Sensor", paste0("Tube_", 1:32))
  
  # Convert Date and Time to POSIXct format
  df$DateTime <- as.POSIXct(paste(df$Date, df$Time), format = "%d %b %y %H:%M:%S")
  
  # Convert wide format to long format
  df_long <- df %>% 
    select(c("DateTime", starts_with("Tube_"))) %>% 
    gather(key = "Tube", value = "Ct_Value", -DateTime)
  
  # Create one-minute summaries
  df_summary <- df_long %>%
    group_by(Tube, Minute = floor_date(DateTime, "minute")) %>%
    summarise(Summary_Ct = sum(Ct_Value))
  
  # Filter data
  start_time <- as.POSIXct("2023-10-05 08:55:00")
  end_time <- as.POSIXct("2023-10-11 09:00:00")
  df_summary <- df_summary %>% filter(Minute >= start_time & Minute <= end_time)
  
  # Initialize an empty data frame to store sleep bouts
  df_bouts <- data.frame()
  
  # Loop through each Tube to identify sleep bouts
  for(tube in unique(df_summary$Tube)) {
    df_tube <- df_summary %>% filter(Tube == tube) %>% arrange(Minute)
    bout_count = 0
    bout_length = 0
    tube_bouts = data.frame(Tube = character(), Bout_Start = POSIXct(), Bout_Length = integer())
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
    df_bouts = rbind(df_bouts, tube_bouts)
  }
  
  # Add Date and Cycle columns
  df_bouts$Date <- floor_date(df_bouts$Bout_Start, "day")
  df_bouts$Cycle <- ifelse(hour(df_bouts$Bout_Start) >= 23 & hour(df_bouts$Bout_Start) < 5, "Dip", "Remainder")
  
  # Create a new grouping variable for the "Dip" period
  df_bouts$Dip_Period <- ifelse(df_bouts$Cycle == "Dip", "Dip", "Non-Dip")
  
  summary_bouts_per_dip_period <- df_bouts %>%
    group_by(Tube, Dip_Period) %>%
    summarise(Total_Bouts = n())
  
  
  return(summary_bouts_per_day_cycle)
}

# Loop through each Monitor file
for(file in monitor_files) {
  print(paste("Processing file:", file))
  summary_for_file <- process_monitor_file(file)
  summary_for_file$Monitor <- basename(file)
  all_summaries <- rbind(all_summaries, summary_for_file)
}

metadata <- read.csv("data/DAM2/DAM2_key.csv")
metadata$Tube <- paste0("Tube_", metadata$Tube)

final_data <- left_join(all_summaries, metadata, by = c("Tube", "Monitor"))
final_data <- final_data[complete.cases(final_data), ]
final_data <- final_data%>%filter(Notes!="offspring")
final_data <- final_data%>%filter(Notes!="dead")
final_data <- final_data%>%filter(Notes!="malformed")

#write.csv(final_data,"output/DAM2_bouts.csv")
# Create the plot
ggplot(final_data, aes(x = Date, y = Total_Bouts, color = Dip_Period, group=interaction(Tube,Monitor))) +
  geom_point() +
  facet_grid( treatAdult ~ geno, scales = "free_y") +
  ggtitle("Total Sleep Bouts by Sex, Treatment, and Genotype") +
  xlab("Day") +
  ylab("Total Sleep Bouts") +
  #scale_color_manual(values = c("Light" = "blue", "Dark" = "black")) +
  theme_minimal()




