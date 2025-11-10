library(janitor)
student_performance <- clean_names(student_performance)
performance_fac <- clean_names(performance_fac)
exam_scores <- clean_names(exam_scores)

library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(plotly)

ggplot(performance_fac, aes(x = teacher_quality, y = exam_score))+
  geom_boxplot()

# Comparing categorical variables (comparing family income and school type)
p <- ggplot(performance_fac, aes(x = family_income, fill = school_type)) +
  geom_bar(position = "dodge") +
  labs(title = "Family Income and School Type",
       x = "Family Income",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "pink"))

# Make the plot interactive
p_interactive_FamilyIncome_SchoolType <- ggplotly(p)
p_interactive_FamilyIncome_SchoolType

# Comparing categorical variables (comparing school type and teacher level)
# School type vs teacher quality plot
p2<-ggplot(performance_fac, aes(x = school_type, fill = teacher_quality)) +
  geom_bar(position = "dodge") +
  labs(title = "School Type and Teacher Quality",
       x = "School Type",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "pink", "forestgreen"))

# Make the plot interactive
p_interactive_SchoolType_TeacherQuality <- ggplotly(p2)
p_interactive_SchoolType_TeacherQuality

# Percentage of high, medium, and low-income families sending kids to private vs public schools
income_school_breakdown <- performance_fac %>%
  group_by(family_income, school_type) %>%
  summarise(
    count = n()
  ) %>%
  group_by(family_income) %>%
  mutate(
    total = sum(count),
    percentage = (count / total) * 100
  ) %>%
  ungroup()

# Print result for income vs school type breakdown
print(income_school_breakdown)

# Percentage of teachers with high, medium, or low quality in private vs public schools
teacher_quality_school_breakdown <- performance_fac %>%
  group_by(school_type, teacher_quality) %>%
  summarise(
    count = n()
  ) %>%
  group_by(school_type) %>%
  mutate(
    total = sum(count),
    percentage = (count / total) * 100
  ) %>%
  ungroup()

# Print result for teacher quality by school type
print(teacher_quality_school_breakdown)