# Load required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(janitor)
library(scales)
library(ggthemes)
library(gganimate)
library(ggrepel)
library(plotly)
library(htmlwidgets)

# Read the Excel file
data <- read_excel("/Users/sara/Desktop/FIS Survey (Responses).xlsx", sheet = "Form responses 1")

# Inspect the first few rows
head(data)

# Handle missing values
data <- data %>% drop_na()

# --- Respondent Demographics: Age and Income Range ---
demographics_data <- data %>%
  count(Age, `Income range`) 

demographics_plot <- ggplot(demographics_data, 
                            aes(x = Age, y = n, fill = `Income range`, 
                                text = paste("Age:", Age, "<br>Count:", n))) +
  geom_col(position = "dodge", color = "white") +
  scale_fill_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3")) +  # Custom color palette
  labs(
    title = "Respondent Demographics: Age and Income Range",
    x = "Age Groups",
    y = "Number of Respondents",
    fill = "Income Range"
  ) +
  theme_classic() +
  theme(plot.title = element_text(size = 16, face = "bold"))

# --- Effectiveness of Educational Programs ---
edu_effectiveness_data <- data %>%
  count(`How effective do you believe educational programs are in promoting understanding and unity among students from different ethnic backgrounds?`) %>%
  rename(effectiveness = `How effective do you believe educational programs are in promoting understanding and unity among students from different ethnic backgrounds?`)

edu_effectiveness_plot <- ggplot(edu_effectiveness_data, 
                                 aes(x = factor(effectiveness, levels = unique(effectiveness)), y = n, 
                                     fill = factor(effectiveness))) +
  geom_col(color = "white") +
  scale_fill_manual(values = c("#66C2A5", "#FC8D62", "#8DA0CB", "#FFD700", "#E78AC3")) +  # Custom color palette
  labs(
    title = "Effectiveness of Educational Programs",
    x = "Effectiveness Rating",
    y = "Number of Respondents",
    fill = "Effectiveness Rating"
  ) +
  theme_classic() +
  theme(plot.title = element_text(size = 16, face = "bold"))

# --- Government Policy Satisfaction ---
policy_satisfaction_data <- data %>%
  count(`How satisfied are you with the government's current policies aimed at promoting racial unity in Malaysia?`) %>%
  rename(satisfaction = `How satisfied are you with the government's current policies aimed at promoting racial unity in Malaysia?`)

policy_satisfaction_plot <- ggplot(policy_satisfaction_data, 
                                   aes(x = factor(satisfaction, levels = unique(satisfaction)), y = n, 
                                       fill = factor(satisfaction))) +
  geom_col(color = "white") +
  scale_fill_manual(values = c("#F44336", "#FF9800", "#FFEB3B", "#4CAF50")) +  # Custom color palette
  labs(
    title = "Government Policy Satisfaction",
    x = "Satisfaction Level",
    y = "Number of Respondents",
    fill = "Satisfaction Level"
  ) +
  theme_classic() +
  theme(plot.title = element_text(size = 16, face = "bold"))

# Display all plots
print(demographics_plot)
print(edu_effectiveness_plot)
print(policy_satisfaction_plot)
