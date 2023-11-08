avg_counts_by_variable<-avg_counts_by_variable%>%filter(Geno!="cd[1]")
head(avg_counts_by_variable)

# Round variable_mean to the nearest integer
avg_counts_by_variable <- avg_counts_by_variable %>%
  mutate(variable_mean = round(variable_mean))

Treat<-avg_counts_by_variable%>%filter(Geno=="CS")
# Define the file path for saving the plot
plot_file <- file.path("plots", paste(experiment_name, " CS only.png", sep = ""))

ggplot(Treat, aes(x = Time, y = variable_mean, 
                                   color = Sex, 
                                   group = interaction(treatAdult, Sex))) +
  geom_errorbar(aes(ymin = pmax(variable_mean - variable_sd, 0), ymax = variable_mean + variable_sd), width = 0.2)+
annotate("rect", xmin = "00:00:00", xmax = "09:00:00", ymin = -Inf, ymax = Inf,
           fill = "gray", alpha = 0.3) +
  annotate("rect", xmin = "21:00:00", xmax = "23:00:00", ymin = -Inf, ymax = Inf,
           fill = "gray", alpha = 0.3) +
  annotate("rect", xmin = "01:00:00", xmax = "03:00:00", ymin = -Inf, ymax = Inf,
           fill = "lightblue", alpha = 0.7) +
  annotate("text", x = "02:00:00", y = Inf,
           label = "Midnight Snack", vjust = 13, color = "black", size = 4)+
  annotate("rect", xmin = "07:00:00", xmax = "09:00:00", ymin = -Inf, ymax = Inf,
           fill = "lightpink", alpha = 0.7) +
  annotate("text", x = "08:00:00", y = Inf,
           label = "Morning Anxiety", vjust = 3, color = "black", size = 4)+
  annotate("rect", xmin = "11:00:00", xmax = "13:00:00", ymin = -Inf, ymax = Inf,
           fill = "lightgreen", alpha = 0.7) +
  annotate("text", x = "12:00:00", y = Inf,
           label = "Early Siesta", vjust = 13, color = "black", size = 4)+
  annotate("rect", xmin = "20:00:00", xmax = "22:00:00", ymin = -Inf, ymax = Inf,
           fill = "violet", alpha = 0.7) +
  annotate("text", x = "21:00:00", y = Inf,
           label = "Evening Peak", vjust = 3, color = "black", size = 4)+
  #geom_point(aes(shape = interaction(treatLarvae,treatAdult))) +
  geom_line(aes(linetype = treatAdult)) +
  labs(x = "Hour", 
       y = "Mean Counts per Hour", 
       title = paste(experiment_name, "activity by variable"),
  ) +
  #facet_wrap(~treatAdult,nrow=3)+
  scale_color_manual(values = c("M" = "purple", "F" = "orange")) +
  scale_fill_manual(values = c("M" = "purple", "F" = "orange")) +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))+    
  #ylim(0, 350) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(plot_file, width = 12, height = 6, bg = "white", dpi = 300)
