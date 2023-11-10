# Replace the value with 0 at the time when I adjusted them
target_datetime <- ymd_hms("2023-10-29 12:29:00")#no larval activity after this point
activity_minutes <- activity_minutes %>%
  mutate(Summary_Ct = ifelse(Minute == target_datetime, 0, Summary_Ct))
target_datetime <- ymd_hms("2023-10-29 12:30:00")#no larval activity after this point
activity_minutes <- activity_minutes %>%
  mutate(Summary_Ct = ifelse(Minute == target_datetime, 0, Summary_Ct))
target_datetime <- ymd_hms("2023-10-29 12:31:00")#no larval activity after this point
activity_minutes <- activity_minutes %>%
  mutate(Summary_Ct = ifelse(Minute == target_datetime, 0, Summary_Ct))
target_datetime <- ymd_hms("2023-10-29 12:32:00")#no larval activity after this point
activity_minutes <- activity_minutes %>%
  mutate(Summary_Ct = ifelse(Minute == target_datetime, 0, Summary_Ct))

cutoff_datetime <- ymd_hms("2023-11-04 00:00:00")#no larval activity after this point
# Determining the first instance of non-zero Summary_Ct for each Tube and Monitor, sorted by Minute
first_non_zero <- activity_minutes %>%
  arrange(Minute) %>% # Sort by Minute
  group_by(Tube, Monitor) %>%
  filter(Summary_Ct != 0) %>%
  slice(1) %>%
  ungroup()

# Create a data frame with all monitor-tube combinations
all_tubes <- expand.grid(Monitor = unique(first_non_zero$Monitor),
                         Tube = 1:32)

# Calculate the cumulative sum of individuals by Minute, grouped
cumulative_counts <- first_non_zero %>%
  group_by(Sex,Geno,treatLarvae) %>%
  arrange(Minute) %>%
  mutate(Cumulative_Count = row_number()) %>%
  ungroup()

# Calculate the total number of each group
total_counts <- cumulative_counts %>%
  group_by(Sex,Geno,treatLarvae) %>%
  summarize(Total_Count = max(Cumulative_Count))

total_counts<-bind_rows(unclass(distinct(total_counts)))
cumulative_counts <- merge(cumulative_counts, total_counts, by = c("Sex", "Geno", "treatLarvae"))
cumulative_counts$Proportion<-cumulative_counts$Cumulative_Count/cumulative_counts$Total_Count

cumulative_counts<-cumulative_counts%>%filter(Sex=="M"|Sex=="F")
cumulative_counts<-cumulative_counts%>%filter(Notes!="malformed")

# Create the line plot
ggplot(cumulative_counts, aes(x = Minute, y = Proportion, color = Sex,
                              shape = Geno,
                              group = interaction(Sex,Geno,treatLarvae))) +
  geom_line() +
  geom_point() +
  labs(x = "Minute", y = "Cumulative Proportion of Individuals") +
  theme_minimal()+
  facet_wrap(~Geno+treatLarvae,ncol=4)+
  scale_color_manual(values = c("M" = "purple", "F" = "orange")) +
  ylim(0,1)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels by 45 degrees

ggplot(cumulative_counts, aes(x = Minute, y = Proportion, color = Sex,
                shape=Geno,group = interaction(Sex,Geno))) +
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


