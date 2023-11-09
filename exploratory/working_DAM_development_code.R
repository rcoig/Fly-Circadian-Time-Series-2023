activity_minutes<-

target_datetime <- ymd_hms("2023-11-04 09:00:00")
# Replace the value with 0 at the time when I adjusted them
activity_hours <- activity_hours %>%
  mutate(Summary_Ct = ifelse(Hour == target_datetime, 0, Summary_Ct))

target_datetime <- ymd_hms("2023-11-04 10:00:00")
# Replace the value with 0 at the time when I adjusted them
activity_hours <- activity_hours %>%
  mutate(Summary_Ct = ifelse(Hour == target_datetime, 0, Summary_Ct))

vertical_line_datetime <- ymd_hms("2023-11-05 14:00:00")


# Get unique combinations of Monitor and Tube
combinations <- unique(activity_hours[, c("Monitor", "Tube")])
# Create a directory to save the PNG files
dir.create("plots/tubes", showWarnings = FALSE)  # "plots" is the name of the folder

cutoff_datetime <- ymd_hms("2023-11-04 16:00:00")#nothing moved before now
df_eclosion_by_minute <- activity_minutes %>%
  filter(Minute > cutoff_datetime)  
df_eclosion_by_hour <- activity_hours %>%
  filter(Hour > cutoff_datetime)  

# Loop through each combination and create a plot
for (i in 1:nrow(combinations)) {
  monitor <- combinations[i, "Monitor"]
  tube <- combinations[i, "Tube"]
  
  # Filter the data for the specific Monitor and Tube
  df_eclosion_by_hour_filtered <- df_eclosion_by_hour %>%
    filter(Monitor == monitor, Tube == tube)
  
  # Create a plot for the current combination
  plot <- ggplot(df_eclosion_by_hour_filtered, aes(x = Hour, y = Summary_Ct, group = Tube)) +
    geom_point() +
    labs(x = "Date/Time by Hour", y = "Hourly Activity Counts", title = "DAMdev Experiment") +
    scale_fill_manual(values = c("M" = "purple", "F" = "orange")) +
    theme_classic() +
    geom_line(aes(group = Tube)) +
    # Add a vertical dotted line using annotate
    annotate("segment", x = vertical_line_datetime, xend = vertical_line_datetime,
             y = -Inf, yend = Inf, color = "red", linetype = "dotted")+
    facet_wrap(~Geno + treatLarvae, ncol = 4)
  
  # Save the plot as a PNG file in the "plots" folder
  filename <- paste("plots/tubes/plot_", monitor, "_", tube, ".png", sep = "")
  ggsave(filename, plot, width = 8, height = 6)  # Adjust width and height as needed
}
# Create a directory to save the PNG files
dir.create("plots/tubes/minutes", showWarnings = FALSE)  # "plots" is the name of the folder

# Loop through each combination and create a plot
for (i in 1:nrow(combinations)) {
  monitor <- combinations[i, "Monitor"]
  tube <- combinations[i, "Tube"]
  
  # Filter the data for the specific Monitor and Tube
  df_eclosion_by_minute_filtered <- df_eclosion_by_minute %>%
    filter(Monitor == monitor, Tube == tube)
  
  # Create a plot for the current combination
  plot <- ggplot(df_eclosion_by_minute_filtered, aes(x = Minute, y = Summary_Ct, group = Tube)) +
    geom_point() +
    labs(x = "Date/Time by Minute", y = "Minute Activity Counts", title = "DAMdev Experiment") +
    scale_fill_manual(values = c("M" = "purple", "F" = "orange")) +
    theme_classic() +
    geom_line(aes(group = Tube)) +
    # Add a vertical dotted line using annotate
    annotate("segment", x = vertical_line_datetime, xend = vertical_line_datetime,
             y = -Inf, yend = Inf, color = "red", linetype = "dotted")+
    facet_wrap(~Geno + treatLarvae, ncol = 4)
  
  # Save the plot as a PNG file in the "plots" folder
  filename <- paste("plots/tubes/minutes/plot_", monitor, "_", tube, ".png", sep = "")
  ggsave(filename, plot, width = 8, height = 6)  # Adjust width and height as needed
}





cutoff_datetimea <- ymd_hms("2023-11-04 8:00:00")

df_filtereda <- activity_minutes %>%
  filter(Minute < cutoff_datetimea)  
df_filtereda2 <- activity_hours %>%
  filter(Hour < cutoff_datetimea)  

ggplot(df_filtereda2, aes(x = Hour, y = Summary_Ct, group=Tube)) +
  geom_point() +
  labs(x = "Date/Time by Hour", y = "Hourly Activity Counts", title = "DAMdev Experiment") +
  scale_fill_manual(values = c("M" = "purple", "F" = "orange")) +
  theme_classic() +
  geom_line(aes(group=Tube)) +
  facet_wrap(~Geno+treatLarvae,ncol=4)

ggplot(df_filtered2, aes(x = Hour, y = Summary_Ct, group=Tube)) +
  geom_point() +
  labs(x = "Date/Time by Hour", y = "Hourly Activity Counts", title = "DAMdev Experiment") +
  scale_fill_manual(values = c("M" = "purple", "F" = "orange")) +
  theme_classic() +
  geom_line(aes(group=Tube)) +
  facet_wrap(~treatLarvae+Geno,ncol=2)



ggplot(activity_hours%>%filter(Sex!="FAIL"), aes(x = Hour, y = Summary_Ct, group=Tube, 
                              color=factor(Tube), linetype=factor(Sex))) +
  geom_line() +
  labs(x = "Time", y = "Activity Counts", title = "DAMdev Experiment") +
  #scale_color_manual(values = c("M" = "purple", "F" = "orange")) +
  theme_classic() 

activity_minutes<-activity_minutes%>%filter(Minute>="2023-10-18 00:00:00
")

# Determining the first instance of non-zero Summary_Ct for each Tube and Monitor, sorted by Minute
first_non_zero <- df_eclosion_by_minute %>%
  arrange(Minute) %>% # Sort by Minute
  group_by(Tube, Monitor) %>%
  filter(Summary_Ct != 0) %>%
  slice(1) %>%
  ungroup()

# Determining the first instance of non-zero Summary_Ct for each Tube and Monitor, sorted by Minute
first_non_zero_dev <- df_filtereda %>%
  arrange(Minute) %>% # Sort by Minute
  group_by(Tube, Monitor) %>%
  filter(Summary_Ct != 0) %>%
  slice(1) %>%
  ungroup()

# Create a data frame with all monitor-tube combinations
all_tubes <- expand.grid(Monitor = unique(first_non_zero$Monitor),
                         Tube = 1:32)

# Join with the actual data to find missing tubes
missing_tubes <- all_tubes %>%
  anti_join(first_non_zero, by = c("Monitor", "Tube"))

missing_tubes <- all_tubes %>%
  anti_join(first_non_zero_dev, by = c("Monitor", "Tube"))

# Count the missing tubes for each monitor
missing_tubes_count <- missing_tubes %>%
  group_by(Monitor) %>%
  summarise(Missing_Tube_Count = n()) %>%
  ungroup()

missing_groups<-merge(missing_tubes_count,metadata_df[,c(1,3,5)],by="Monitor")
missing_groups<-distinct(missing_groups)

# Calculate the cumulative sum of individuals by Minute, grouped by Sex
cumulative_counts <- first_non_zero %>%
  arrange(Tube, Monitor, Minute) %>% # Sort by Tube, Monitor, and then by Minute
  group_by(Sex) %>%
  summarise(
    Tube=Tube,
    Monitor=Monitor,
    Sex=Sex,
    Minute=Minute,
    Cumulative_Count = cumsum(Summary_Ct))

cumulative_counts <- first_non_zero_dev %>%
  arrange(Tube, Monitor, Minute) %>% # Sort by Tube, Monitor, and then by Minute
  group_by(Sex) %>%
  summarise(
    Tube=Tube,
    Monitor=Monitor,
    Sex=Sex,
    Minute=Minute,
    Cumulative_Count = cumsum(Summary_Ct))

# Calculate the cumulative sum of individuals by Minute, grouped by Sex
cumulative_counts <- first_non_zero %>%
  group_by(Sex) %>%
  arrange(Minute) %>%
  mutate(Cumulative_Count = row_number()) %>%
  ungroup()

# Calculate the cumulative sum of individuals by Minute, grouped
cumulative_counts <- first_non_zero %>%
  group_by(Sex,Geno,treatLarvae) %>%
  arrange(Minute) %>%
  mutate(Cumulative_Count = row_number()) %>%
  ungroup()

# Calculate the cumulative sum of individuals by Minute, grouped
cumulative_counts <- first_non_zero %>%
  group_by(Geno,treatLarvae) %>%
  arrange(Minute) %>%
  mutate(Cumulative_Count = row_number()) %>%
  ungroup()

# Calculate the total number of each group
total_counts <- cumulative_counts %>%
  group_by(Sex,Geno,treatLarvae) %>%
  summarize(Total_Count = max(Cumulative_Count))

total_counts_by_monitor <- first_non_zero %>%
  group_by(Monitor,Geno,treatLarvae) %>%
  summarize(Total_Count = n())
total_counts_by_monitor$percentage<-total_counts_by_monitor$Total_Count/32
ggplot(total_counts_by_monitor, aes(x = treatLarvae, 
                                    y = percentage,
                                    fill = treatLarvae)) +
  geom_boxplot() +
  geom_point() +
  ylim(0.5,1)+
  labs(x = "", y = "% of flies eclosed (out of 32)") +
  scale_color_manual(values = c("F" = "orange", "M" = "purple")) + # Set colors for each sex
  theme_minimal()+
  facet_wrap(~Geno)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels by 45 degrees





# Calculate the total number of males and females
total_counts <- cumulative_counts %>%
  group_by(Sex) %>%
  summarize(Total_Count = max(Cumulative_Count))
total_counts<-bind_rows(unclass(distinct(total_counts)))
cumulative_counts<-bind_rows(unclass(distinct(cumulative_counts)))


# Merge total counts with cumulative counts
cumulative_counts <- cumulative_counts %>%
  left_join(total_counts, by = c("Sex","Geno","treatLarvae")) %>%
  mutate(Proportion = Cumulative_Count / Total_Count)

cumulative_counts$Total_Count<-64
cumulative_counts$Proportion<-cumulative_counts$Cumulative_Count/cumulative_counts$Total_Count

# Create the line plot
ggplot(cumulative_counts, aes(x = Minute, y = Proportion, color = Sex)) +
  geom_line() +
  geom_point()+
  labs(x = "Minute", y = "Proportion of Individuals") +
  scale_color_manual(values = c("F" = "orange", "M" = "purple")) + # Set colors for each sex
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels by 45 degrees


cumulative_counts<-cumulative_counts%>%filter(Sex=="M"|Sex=="F")
cumulative_counts<-cumulative_counts%>%filter(Notes!="malformed")

# Create the line plot
ggplot(cumulative_counts, aes(x = Minute, y = Proportion, color = Sex,
                              group = interaction(Sex,treatLarvae))) +
  geom_line() +
  geom_point() +
  labs(x = "Minute", y = "Cumulative Proportion of Individuals") +
  theme_minimal()+
  facet_wrap(~Geno+treatLarvae,ncol=4)+
  scale_color_manual(values = c("M" = "purple", "F" = "orange")) +
  ylim(0,1)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels by 45 degrees

ggplot(cumulative_counts, aes(x = Minute, y = Proportion, color = Sex,
                shape=Geno,group = interaction(Sex,Geno,treatLarvae))) +
  geom_line() +
  geom_point() +
  scale_shape_manual(values = c("CS" = 19, "v[1];CS" = 2))+
  labs(x = "Minute", y = "Cumulative Proportion of Individuals") +
  theme_minimal()+
  facet_wrap(~treatLarvae,ncol=4)+
  scale_color_manual(values = c("M" = "purple", "F" = "orange")) +
  
  ylim(0,1)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels by 45 degrees

ggplot(cumulative_counts, aes(x = Minute, y = Proportion, color = treatLarvae,
                              shape=Geno,group = interaction(Geno,treatLarvae))) +
  geom_line() +
  geom_point() +
  scale_shape_manual(values = c("CS" = 19, "v[1];CS" = 2))+
  labs(x = "Minute", y = "Cumulative Proportion of Individuals") +
  theme_minimal()+
  facet_wrap(~Sex+Geno,ncol=2)+
  #scale_color_manual(values = c("M" = "purple", "F" = "orange")) +
  
  ylim(0,1)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels by 45 degrees


