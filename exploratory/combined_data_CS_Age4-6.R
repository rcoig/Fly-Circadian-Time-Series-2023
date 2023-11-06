ggplot(avg_counts_by_Geno, aes(x = Time, y = mean, color = Sex, fill = Sex,                                group=interaction(Sex,Geno))) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2) +
  annotate("rect", xmin = "00:00", xmax = "09:00", ymin = -Inf, ymax = Inf,
           fill = "darkgray", alpha = 0.5) +
  annotate("rect", xmin = "21:00", xmax = "23:59", ymin = -Inf, ymax = Inf,
           fill = "darkgray", alpha = 0.5) +
  annotate("rect", xmin = "01:00", xmax = "03:00", ymin = -Inf, ymax = Inf,
           fill = "lightblue", alpha = 0.7) +
  annotate("text", x = "02:00", y = Inf,
           label = "Midnight Snack", vjust = 13, color = "black", size = 4)+
  annotate("rect", xmin = "07:00", xmax = "09:00", ymin = -Inf, ymax = Inf,
           fill = "lightpink", alpha = 0.7) +
  annotate("text", x = "08:00", y = Inf,
           label = "Morning Anxiety", vjust = 3, color = "black", size = 4)+
  annotate("rect", xmin = "11:00", xmax = "13:00", ymin = -Inf, ymax = Inf,
           fill = "lightgreen", alpha = 0.7) +
  annotate("text", x = "12:00", y = Inf,
           label = "Early Siesta", vjust = 13, color = "black", size = 4)+
  annotate("rect", xmin = "20:00", xmax = "22:00", ymin = -Inf, ymax = Inf,
           fill = "violet", alpha = 0.7) +
  annotate("text", x = "21:00", y = Inf,
           label = "Evening Peak", vjust = 3, color = "black", size = 4)+
  geom_point() +
  geom_line() +
  labs(
    x = "Hour", y = "Mean Counts per Hour",
    title = element_text(
      paste("Canton S Circadian Activity Ages 4-6 days",  # Concatenate the lines
            "\nAveraged across three experiments"),  # Second line of the title
      lineheight = 0.9  # Adjust the line height here (default is 1)
    )
  ) +
  scale_color_manual(values = c("M" = "purple", "F" = "orange")) +
  scale_fill_manual(values = c("M" = "purple", "F" = "orange")) +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels 45deg