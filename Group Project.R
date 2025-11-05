library(janitor)
student_performance <- clean_names(student_performance)
performance_fac <- clean_names(performance_fac)
exam_scores <- clean_names(exam_scores)


library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)

ggplot(performance_fac, aes(x = teacher_quality, y = exam_score))+
  geom_boxplot()

