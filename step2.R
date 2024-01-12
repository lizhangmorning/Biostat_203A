setwd("/Users/lizhang/Desktop/203final")



library(ggplot2)

# Assuming Q31_1 to Q31_9 are stored in Q31_1, Q31_2, ..., Q31_9 columns
question_vars <- paste0("Q31_", 1:9)

# Define custom X-axis labels
x_axis_labels <- c(
  "Met at work",
  "Met at school",
  "Met at church",
  "Met via personal ads or dating service via the internet",
  "Met during vacation or business trip",
  "Met at a bar/nightclub/dance club",
  "Met through a social organization",
  "Met at a private party",
  "Other"
)

for (i in seq_along(question_vars)) {
  var <- question_vars[i]
  x_labels <- setNames(rep(x_axis_labels, each = 2), c("0", "1"))
  
  hc <- data.frame(prop.table(table(my_hcmst[[var]], my_hcmst$RELATIONSHIP_QUALITY), 1))
  hc <- hc %>%
    arrange(Var1, desc(Var2)) %>%
    group_by(Var1) %>%
    mutate(perclabs = cumsum(Freq))
  
  # Generate a plot

  subtitle <- paste("Among those who met at", tolower(x_axis_labels[i]), "versus those who did not")
  plot_filename <- paste("Sample_Plot_", var, "_Step_2.png", sep = "")
  
  P <- ggplot(hc, aes(x = Var1, y = Freq, fill = Var2)) +
    geom_bar(stat = "identity", width = 0.7) +
    geom_text(aes(y = perclabs, label = ifelse(perclabs > 0.95, "", scales::percent(perclabs))), vjust = 2) +
    scale_x_discrete(limit = c("0", "1"), labels = x_labels) +
    scale_fill_discrete(limit = c("1", "2", "3", "4", "5"), labels = c("very poor", "poor", "fair", "good", "excellent")) +
    labs(y = "Percentage of Respondents",
         x = x_axis_labels[i],
         title = "Relationship Quality",
         subtitle = subtitle) +
    labs(fill = "Relationship Quality")
  
  print(P)
  ggsave(plot_filename, plot = P, width = 5, height = 4.5, units = "in")
}
