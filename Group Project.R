library(janitor)
AcademicPerformance <- clean_names(StudentPerformance)
StudentPerformanceFactors <- clean_names(StudentPerformanceFactors)
ExamScores <- clean_names(ExamScores)

library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(plotly)

ggplot(StudentPerformanceFactors, aes(x = teacher_quality, y = exam_score))+
  geom_boxplot()

# Comparing categorical variables (comparing family income and school type)
p <- ggplot(StudentPerformanceFactors, aes(x = family_income, fill = school_type)) +
  geom_bar(position = "dodge") +
  labs(title = "Family Income and School Type",
       x = "Family Income",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(name = "School Type",values = c("lightblue", "pink"))

# Make the plot interactive
p_interactive_FamilyIncome_SchoolType <- ggplotly(p)
p_interactive_FamilyIncome_SchoolType

# Comparing categorical variables (comparing school type and teacher level)
# School type vs teacher quality plot
p2<-ggplot(StudentPerformanceFactors, aes(x = school_type, fill = teacher_quality)) +
  geom_bar(position = "dodge") +
  labs(title = "School Type and Teacher Quality",
       x = "School Type",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(name = "Teacher Quality", values = c("lightblue", "pink", "forestgreen"))

# Make the plot interactive
p_interactive_SchoolType_TeacherQuality <- ggplotly(p2)
p_interactive_SchoolType_TeacherQuality

# Percentage of high, medium, and low-income families sending kids to private vs public schools
income_school_breakdown <- StudentPerformanceFactors %>%
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
teacher_quality_school_breakdown <- StudentPerformanceFactors %>%
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


# Sleep hours and exam scores
p3 <- ggplot(ExamScores, aes(sleep_hours, exam_score))+
  geom_point(color = "blue",shape = "diamond", alpha = 0.7)+
  geom_smooth(color = "purple",method = "lm")+
  labs(
    title = "Sleep Hours vs Exam Scores",
    y = "Exam Scores",
    x = "Sleep Hours")+
  theme_minimal()

#NOT MUCH OF A RELATIONSHIP
p_interactive_SleepHours_ExamScores <- ggplotly(p3)
p_interactive_SleepHours_ExamScores

# Hours studied and attendance percent
p4 <- ggplot(ExamScores, aes(hours_studied,attendance_percent))+
  geom_point(color = "forestgreen", shape = "square", alpha = 0.7)+
  geom_smooth(color = "cyan", method = "lm")+
  labs(
    title = "Hours Studied vs Attendance Percent",
    y = "Attendence Percent",
    x = "Hours Studied") +
  theme_light()

# NOT MUCH OF A RELATIONSHIP
p_interactive_HoursStudied_AttendancePercent <- ggplotly(p4)
p_interactive_HoursStudied_AttendancePercent