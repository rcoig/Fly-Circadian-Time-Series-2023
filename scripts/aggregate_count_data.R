# Function to process all .txt files in a directory and aggregate data
aggregate_counts <- function(directory_path, aggregation_level = "minute") {
  # List all .txt files in the specified directory
  file_list <- list.files(path = directory_path, pattern = "\\.txt$", full.names = TRUE)
  
  # Initialize an empty data frame to store results
  result_df <- data.frame()
  
  # Loop through each file in the directory
  for (file_path in file_list) {
    # Read the tab-delimited file into a data frame
    df <- read.delim(file_path, header = FALSE, stringsAsFactors = FALSE)
    
    # Extract the monitor name from the file_path
    monitor_name <- as.integer(sub(".*/Monitor(\\d+)\\.txt$", "\\1", file_path))
    
    # Rename columns (same as before)
    colnames(df) <- c("Index", "Date", "Time", "Monitor_Status", "Extras", "Monitor_Number", "Unused_1", "Data_Type",
                      "Unused_2", "Light_Sensor", paste0("Tube_", 1:32))
    
    # Convert Date and Time to POSIXct format (same as before)
    df$DateTime <- as.POSIXct(paste(df$Date, df$Time), format = "%d %b %y %H:%M:%S", tz = "UTC")
    
    # Convert wide format to long format (same as before)
    df_long <- df %>%
      dplyr::select(c("DateTime", starts_with("Tube_"))) %>%
      gather(key = "Tube", value = "Ct_Value", -DateTime)
    
    # Extract the integer part from the "Tube" column values
    df_long$Tube <- as.integer(sub("^Tube_", "", df_long$Tube))
    
    # Add the integer monitor name to the "Monitor" column
    df_long$Monitor <- monitor_name
    
    # Depending on the aggregation level specified, aggregate the data
    if (aggregation_level == "minute") {
      # Create one-minute summaries (same as before)
      df_summary <- df_long %>%
        group_by(Monitor, Tube, Minute = floor_date(DateTime, "minute")) %>%
        summarise(Summary_Ct = sum(Ct_Value))
    } else if (aggregation_level == "hour") {
      # Create one-hour summaries
      df_summary <- df_long %>%
        group_by(Monitor, Tube, Hour = floor_date(DateTime, "hour")) %>%
        summarise(Summary_Ct = sum(Ct_Value))
    } else {
      stop("Invalid aggregation level. Please specify 'minute' or 'hour'.")
    }
    
    # Append the results to the overall result data frame
    result_df <- bind_rows(result_df, df_summary)
  }
  
  # Return the aggregated data
  return(result_df)
}