# Load required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(janitor)
library(scales)
library(patchwork)
library(ggthemes)
library(gganimate)
library(ggrepel)
library(plotly)

# Set working directory and read the Excel file
setwd("/Users/sara/Desktop/")
data <- read_excel("FIS Survey (Responses).xlsx", sheet = "Form responses 1")

# Clean column names
data <- data %>% clean_names()

# --- Demographics: Age vs. Income Range ---
demographics_data <- data %>%
  group_by(age, income_range) %>%
  summarise(count = n(), .groups = "drop")

demographics_plot <- ggplot(demographics_data, aes(x = age, y = count, fill = income_range)) +
  geom_col(position = "dodge", color = "white") +
  scale_fill_manual(values = c("#4DB6AC", "#FF8A65", "#7986CB", "#FFD54F")) +
  labs(
    title = "Respondent Demographics: Age and Income Range",
    x = "Age Group",
    y = "Number of Respondents",
    fill = "Income Range"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "top"
  ) +
  ggthemes::theme_fivethirtyeight()

# --- Factors Contributing to Racial Unity ---
racial_unity_factors <- data %>%
  select(in_your_opinion_which_of_the_following_factors_contribute_most_to_racial_unity_among_students_choose_3) %>%
  separate_rows(in_your_opinion_which_of_the_following_factors_contribute_most_to_racial_unity_among_students_choose_3, sep = ", ") %>%
  group_by(factor = in_your_opinion_which_of_the_following_factors_contribute_most_to_racial_unity_among_students_choose_3) %>%
  summarise(count = n(), .groups = "drop")

racial_unity_plot <- ggplot(racial_unity_factors, aes(x = reorder(factor, count), y = count, fill = count)) +
  geom_col() +
  coord_flip() +
  geom_text_repel(aes(label = count), hjust = -0.2) +
  scale_fill_gradient(low = "#81D4FA", high = "#0288D1") +
  labs(
    title = "Factors Contributing to Racial Unity",
    x = "Factors",
    y = "Number of Mentions"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "none"
  )

# --- Effectiveness of Educational Programs ---
effectiveness_data <- data %>%
  group_by(how_effective_do_you_believe_educational_programs_are_in_promoting_understanding_and_unity_among_students_from_different_ethnic_backgrounds) %>%
  summarise(count = n(), .groups = "drop")

effectiveness_plot <- ggplot(effectiveness_data, aes(
  x = factor(how_effective_do_you_believe_educational_programs_are_in_promoting_understanding_and_unity_among_students_from_different_ethnic_backgrounds),
  y = count
)) +
  geom_point(aes(size = count, color = count)) +
  geom_line(aes(group = 1, color = count), size = 1) +
  scale_color_gradient(low = "#F8BBD0", high = "#AD1457") +
  labs(
    title = "Effectiveness of Educational Programs",
    x = "Effectiveness Rating (1-5)",
    y = "Number of Responses",
    size = "Responses"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text = element_text(size = 12)
  )

# --- Satisfaction with Government Policies ---
satisfaction_data <- data %>%
  group_by(how_satisfied_are_you_with_the_governments_current_policies_aimed_at_promoting_racial_unity_in_malaysia) %>%
  summarise(count = n(), .groups = "drop")

waffle_plot <- satisfaction_data %>%
  mutate(percentage = count / sum(count)) %>%
  ggplot(aes(x = "", y = percentage, fill = factor(how_satisfied_are_you_with_the_governments_current_policies_aimed_at_promoting_racial_unity_in_malaysia))) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Spectral") +
  labs(
    title = "Satisfaction with Government Policies",
    fill = "Satisfaction Level"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

# --- Combine Interactive Plots with Plotly ---
combined_plot <- ggplotly(
  demographics_plot / racial_unity_plot / effectiveness_plot / waffle_plot,
  tooltip = c("text")
)

# Save interactive plots as HTML for browser display
htmlwidgets::saveWidget(as_widget(combined_plot), "survey_dashboard.html")

# Print the interactive plot in the viewer
combined_plot
