library(tidyverse)
library(dplyr)
library(tidyr)
library(ggthemes)
library(scales)
library(ggrepel)
library(patchwork)
library(plotly)
StudentPerformanceFactors <- read.csv("StudentPerformanceFactors.csv")
student_exam_scores <- read.csv("student_exam_scores.csv")
AcademicPerformance <- read.csv("AcademicPerformance.csv")

## Preliminary comparison of amount of time spent studying to exam scores in Student Performance Factors data set (first data set)
ggplot(StudentPerformanceFactors, aes(x=Hours_Studied, y=Exam_Score)) +
  geom_point() +
  geom_smooth()

## Cleaning the Exam_Score variable for outliers (scores less than 0 or greater than 100, because they are impossible in this case)
SPFcleaned <- StudentPerformanceFactors |> 
  mutate(Exam_Score = if_else(Exam_Score < 0 | Exam_Score > 100, NA, Exam_Score))

## Interactive plot comparing hours studied and exam scores using motivation level as shape and color
basicSPFggplot <- ggplot(SPFcleaned, aes(x = Hours_Studied, y = Exam_Score)) +
  geom_point(aes(color = Motivation_Level, shape = Motivation_Level)) +
  geom_smooth(method = "lm") +
  labs(
    title = "The Impact of Hours Spent Studying on Grades",
    subtitle = "Exam Score Increases with Time Spent Studying",
    x = "Hours Studied per Week",
    y = "Exam Score (out of 100)",
    color = "Motivation Level", shape = "Motivation Level",
    caption = "Source: Kaggle via Lai Ng"
  ) +
  scale_color_colorblind()

ggplotly(basicSPFggplot)

## Cleaning the exam_score variable for outliers in the Student Exam Scores data set (scores less than 0 or greater than 55, because the grading scale is only that range)
SES2 <- student_exam_scores |> 
  mutate(exam_score = if_else(exam_score < 0 | exam_score > 55, NA, exam_score))

## Comparing amount of time spent studying to exam scores in Student Exam Scores data set
SES2_HSbyES <- ggplot(SES2, aes(x = hours_studied, y = exam_score)) +
  geom_point() +
  geom_smooth() +
  labs(
    title = "The Impact of Hours Spent Studying on Grades",
    subtitle = "Exam Score Increases with Time Spent Studying",
    x = "Hours Studied per Week",
    y = "Exam Score (out of 55)",
    caption = "Source: Kaggle via Muhammad Shoaib"
  )

ggplotly(SES2_HSbyES)

## Comparing amount of sleep to school attendance in Student Exam Scores data set
SES2_SleepbyAP <- ggplot(student_exam_scores, aes(x = sleep_hours, y = attendance_percent)) +
  geom_point() +
  labs(
    title = "The Impact of Sleep on School Attendance",
    subtitle = "Little to no correlation between amount of sleep and attendance",
    x = "Average Number of Hours Slept per Night",
    y = "Percent of Class Attended",
    caption = "Source: Kaggle via Muhammad Shoaib"
  )

ggplotly(SES2_SleepbyAP)

## Comparing school attendance to exam scores in Student Exam Scores data set
SES2_APbyES <- ggplot(SES2, aes(x = attendance_percent, y = exam_score)) +
  geom_point() +
  geom_smooth() +
  labs(
    title = "The Impact of School Attendance on Exam Scores",
    x = "Percent of Class Attended",
    y = "Exam Score (out of 55)",
    caption = "Source: Kaggle via Muhammad Shoaib"
  )

ggplotly(SES2_APbyES)

# Re-ordered motivation variable to go from low to medium to high
NEW_Motivation_Level <- c("Low", "Medium", "High")
SPFcleaned$Motivation_Level <- factor(SPFcleaned$Motivation_Level, levels = NEW_Motivation_Level)
print(levels(SPFcleaned$Motivation_Level))


## MODIFIED plot comparing hours studied and exam scores using motivation level as shape and color, altering alpha level
basicSPFggplot_alpha <- ggplot(SPFcleaned, aes(x = Hours_Studied, y = Exam_Score)) +
  geom_point(aes(color = Motivation_Level, shape = Motivation_Level),
             alpha = 0.5, size = 2.5) + 
  geom_smooth() +
  labs(
    title = "The Impact of Hours Spent Studying on Grades",
    subtitle = "Exam Score Increases with Time Spent Studying",
    x = "Hours Studied per Week",
    y = "Exam Score (out of 100)",
    color = "Motivation Level", shape = "Motivation Level",
    caption = "Source: Kaggle via Lai Ng"
  ) +
  scale_color_colorblind()

ggplotly(basicSPFggplot_alpha)

## Facet wrap as modification to clarify plot
basicSPFggplot_facet <- ggplot(SPFcleaned, aes(x = Hours_Studied, y = Exam_Score)) +
  geom_point(aes(color = Motivation_Level, shape = Motivation_Level)) +
  geom_smooth() +
  facet_wrap(~ Motivation_Level) +  
  labs(
    title = "The Impact of Hours Spent Studying on Grades",
    subtitle = "Exam Score Increases with Time Spent Studying",
    x = "Hours Studied per Week",
    y = "Exam Score (out of 100)",
    color = "Motivation Level", shape = "Motivation Level",
    caption = "Source: Kaggle via Lai Ng"
  ) +
  scale_color_colorblind()

ggplotly(basicSPFggplot_facet)

## Adjusted column to slightly offset hours studied 
SPFcleaned2 <- SPFcleaned %>%
  mutate(Hours_Studied_Offset = case_when(
    Motivation_Level == "Low" ~ Hours_Studied - 0.25,
    Motivation_Level == "High" ~ Hours_Studied + 0.25,
    TRUE ~ Hours_Studied
  ))

## New plot using the offset while keeping original hours in tooltip
basicSPFggplot_offset <- ggplot(SPFcleaned2, aes(x = Hours_Studied_Offset, y = Exam_Score,
                                                 text = paste(
                                                   "Exam Score: ", Exam_Score, "<br>",
                                                   "Original Hours Studied: ", Hours_Studied, "<br>",
                                                   "Motivation: ", Motivation_Level
                                                 ))
) +
  geom_point(aes(color = Motivation_Level, shape = Motivation_Level), size = 2.5) +
  geom_smooth() +
  labs(
    title = "The Impact of Hours Spent Studying on Grades",
    subtitle = "Offset Hours to Reduce Overlapping Points",
    x = "Hours Studied per Week (Offset for Plotting Only)",
    y = "Exam Score (out of 100)",
    color = "Motivation Level", shape = "Motivation Level",
    caption = "Source: Kaggle via Lai Ng"
  ) +
  scale_color_colorblind()

ggplotly(basicSPFggplot_offset, tooltip = "text")





# Prepare data for plotting test scores by lunch status
plot_data <- AcademicPerformance |>
  select(lunch, math.score, reading.score, writing.score) |>
  pivot_longer(
    cols = c(math.score, reading.score, writing.score),
    names_to = "subject",
    values_to = "score"
  )

#Compare lunch status and test scores (math, reading and writing in one faceted plot.
ggplot(plot_data, aes(x = lunch, y = score, fill = lunch)) +
  geom_boxplot(show.legend = FALSE) +
  facet_wrap(~ subject, nrow = 1,
             labeller = labeller(
               subject = c(
                 "math.score" = "Math",
                 "reading.score" = "Reading",
                 "writing.score" = "Writing"
               ))) +
  labs(
    title = "Test Scores by Lunch Status",
    subtitle = "students with standard lunch tend to have higher score on math, reading, and writing",
    x = "Lunch status",
    y = "Score",
    caption = "Source: Academic Performance dataset"
  ) +
  scale_x_discrete(labels = c("free/reduced" = "Free / Reduced",
                              "standard"    = "Standard")) +
  theme_minimal(base_size = 12)
##Students with standard lunch tend to have higher score on math, reading, and writing.





## Preliminary exploration of teacher quality and exam scores variable in Student Performance Factors data set
ggplot(StudentPerformanceFactors, aes(x = Teacher_Quality, y = Exam_Score))+
  geom_boxplot()

## Comparing categorical variables (comparing family income and school type)
p <- ggplot(StudentPerformanceFactors, aes(x = Family_Income, fill = School_Type)) +
  geom_bar(position = "dodge") +
  labs(title = "Family Income and School Type",
       x = "Family Income",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "pink"))

## Making the plot interactive
p_interactive_FamilyIncome_SchoolType <- ggplotly(p)
p_interactive_FamilyIncome_SchoolType

                   
 
## Bar plot of comparing categorical variables of school type and teacher level
p2<-ggplot(StudentPerformanceFactors, aes(x = School_Type, fill = Teacher_Quality)) +
  geom_bar(position = "dodge") +
  labs(title = "School Type and Teacher Quality",
       x = "School Type",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "pink", "forestgreen"))

# Make the plot interactive
p_interactive_SchoolType_TeacherQuality <- ggplotly(p2)
p_interactive_SchoolType_TeacherQuality

                    
                    
## Percentage of high, medium, and low-income families sending kids to private vs public schools
income_school_breakdown <- StudentPerformanceFactors %>%
  group_by(Family_Income, School_Type) %>%
  summarise(
    count = n()
  ) %>%
  group_by(Family_Income) %>%
  mutate(
    total = sum(count),
    percentage = (count / total) * 100
  ) %>%
  ungroup()

# Print result for income vs school type breakdown
print(income_school_breakdown)
                    
                   

# Percentage of teachers with high, medium, or low quality in private vs public schools
teacher_quality_school_breakdown <- StudentPerformanceFactors %>%
  group_by(School_Type, Teacher_Quality) %>%
  summarise(
    count = n()
  ) %>%
  group_by(School_Type) %>%
  mutate(
    total = sum(count),
    percentage = (count / total) * 100
  ) %>%
  ungroup()
print(teacher_quality_school_breakdown)
                    
# Print result for teacher quality by school type
print(teacher_quality_school_breakdown)
                    
  

## Original exploration of Hours of Sleep vs Hours of Studying with Studen Exam Scores (SES) dataset
SES_SHbyHS <- ggplot(student_exam_scores, aes(x = sleep_hours, y = hours_studied)) +
  geom_jitter() +
  geom_smooth() +
  labs(
    title = "Relationship between Hours of Sleep and Hours of Studying",
    subtitle = "Student Exam Scores Dataset",
    x = "Hours of Sleep",
    y = "Hours of Studying"
  )
ggplotly(SES_SHbyHS)


                    
## Comparison of Hours of Sleep vs Hours of Studying with Studen Exam Scores (SES) dataset
p3 <- ggplot(student_exam_scores, aes(sleep_hours, exam_score))+
  geom_point(color = "blue",shape = "diamond", alpha = 0.7)+
  geom_smooth(color = "purple",method = "lm")+
  labs(
        title = "Sleep Hours vs Exam Scores",
        y = "Exam Scores",
        x = "Sleep Hours")+
        theme_minimal(
          
        )

#NOT MUCH OF A RELATIONSHIP
p_interactive_SleepHours_ExamScores <- ggplotly(p3)
p_interactive_SleepHours_ExamScores


                    
# Hours studied and attendance percent
p4 <- ggplot(student_exam_scores, aes(hours_studied,attendance_percent))+
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
                    
# Average Final Score
AcademicPerformance <- AcademicPerformance |> 
  mutate(
    OverallScore = ((math.score+reading.score+writing.score)/3),
                        LetterGrade = case_when(
                          OverallScore >= 93 ~ "A",
                          OverallScore >= 90 ~ "A-",
                          OverallScore >= 87 ~ "B+",
                          OverallScore >= 83 ~ "B",
                          OverallScore >= 80 ~ "B-",
                          OverallScore >= 77 ~ "C+",
                          OverallScore >= 73 ~ "C",
                          OverallScore >= 70 ~ "C-",
                          OverallScore >= 60 ~ "D",
                          TRUE             ~ "F"
                        )
                      )
                    
# Round Final
AcademicPerformance <- AcademicPerformance |>
                      mutate(
                        OverallScore = round(OverallScore)
                      )
                    
                    





                    
head(student_exam_scores)
                    
## Commparison of Hours of Sleep vs Hours of Studying with Studen Exam Scores (SES) dataset
SES_SHbyHS <- ggplot(student_exam_scores, aes(x = sleep_hours, y = hours_studied)) +
  geom_jitter() +
  geom_smooth() +
  labs(
    title = "Relationship between Hours of Sleep and Hours of Studying",
    subtitle = "Student Exam Scores Dataset",
    x = "Hours of Sleep",
    y = "Hours of Studying"
  )
ggplotly(SES_SHbyHS)
                    

## Commparison of Hours of Sleep vs Hours of Studying with Student Performance Factors (SPF) dataset
SPF_SHbyHS <-ggplot(StudentPerformanceFactors, aes(x = Sleep_Hours, y = Hours_Studied)) +
  geom_jitter() +
  geom_smooth() +
  labs(
    title = "Relationship between Hours of Sleep and Hours of Studying",
    subtitle = "Student Performance Factors Dataset",
    x = "Hours of Sleep",
    y = "Hours of Studying"
  )
ggplotly(SPF_SHbyHS)
                    

## Exploring amount of studying vs amount of Physical Activity and variation by gender with SPF dataset
SPF_SHbyPA <- ggplot(StudentPerformanceFactors, aes(x = Hours_Studied, y = Physical_Activity, color = Gender)) +
  geom_jitter() +
  geom_smooth() +
  labs(
    title = "Relationship between Hours of Studying and Hours of Activity",
    subtitle = "Student Performance Factors Dataset",
    x = "Hours Studied",
    y = "Hours Physical Activity"
  )
ggplotly(SPF_SHbyPA)

#Initial Plot of Hours of Studying vs Exam Score Accounting for Disability with SPF dataset
#Trendline shows us that students without a learning disability have slightly higher exam scores
LD1 <- ggplot(SPFcleaned, aes(x = Hours_Studied, y = Exam_Score, color = Learning_Disabilities)) +
  geom_bin2d() +
  geom_smooth() +
  labs(
    title = "Hours Studied vs Exam Score",
    subtitle = "Student Performance Factors Dataset",
    x = "Hours Studied",
    y = "Exam Score",
    color = "Learning Disability"
  )
ggplotly(LD1) 


#Revised Plot of Hours of Studying vs Exam Score Accounting for Disability w/ SPF dataset
#Trendline shows us that No Learning Disability have slightly higher exam scores
LD1NEW <- ggplot(StudentPerformanceFactors, aes(x = Hours_Studied, y = Exam_Score, color = Learning_Disabilities)) +
                      geom_jitter() +
                      geom_smooth() +
                      labs(
                        title = "Hours Studied vs Exam Score",
                        subtitle = "Student Performance Factors Dataset",
                        x = "Hours Studied",
                        y = "Exam Score",
                        color = "Learning Disability"
                      )
ggplotly(LD1NEW) 
                    

#Previous Score vs Exam Score Accounting for Disability w/ SPF dataset
#Trendline shows us that No Learning Disability have slightly higher exam scores
#Also shows slight positive relationship: Higher Previous Score more likely to have higher exam score
LD2 <- ggplot(SPFcleaned, aes(x = Previous_Scores, y = Exam_Score, color = Learning_Disabilities)) +
  geom_bin2d() +
  geom_smooth() +
  labs(
    title = "Previous Score vs Exam Score",
    subtitle = "Student Performance Factors Dataset",
    x = "Previous Score",
    y = "Exam Score",
    color = "Learning Disability"
  )
ggplotly(LD2)
                    

#Creating new variable that only includes students with learning disabilities                      
learning_disability = StudentPerformanceFactors |> filter(Learning_Disabilities == "Yes")

#Plot for Previous Score vs Exam Score for students with learning disability 
LD3 <- ggplot(learning_disability, aes(x = Previous_Scores, y = Exam_Score)) +
  geom_jitter() +
  geom_smooth() +
  labs(
    title = "Previous Score vs Exam Score",
    subtitle = "Student Performance Factors Dataset",
    x = "Previous Score",
    y = "Exam Score",
  )
ggplotly(LD3)
                    
                    
#Creating new variable that only includes students without learning disabilities            
no_disability = StudentPerformanceFactors |> filter(Learning_Disabilities == "No")

#Plot for Previous Score vs Exam Score for students without learning disability
NLD <- ggplot(no_disability, aes(x = Previous_Scores, y = Exam_Score)) +
  geom_jitter() +
  geom_smooth() +
  labs(
    title = "Previous Score vs Exam Score",
    subtitle = "Student Performance Factors Dataset",
    x = "Previous Score",
    y = "Exam Score",
  )
ggplotly(NLD)
                    
                    
#Previous Score vs Exam Score faceted by learning disability status
ldd = StudentPerformanceFactors |> mutate(ld = case_when(
                      Learning_Disabilities == "Yes" ~ "Has Learning Disability",
                      Learning_Disabilities == "No" ~ "No Learning Disability"
                    ))
LD_vs_NLD <- ggplot(ldd, aes(x = Previous_Scores, y = Exam_Score)) +
                      geom_jitter() +
                      facet_wrap(~ ld) +
                      geom_smooth() +
                      labs(
                        title = "Previous Score vs Exam Score",
                        subtitle = "Student Performance Factors Dataset",
                        x = "Previous Score",
                        y = "Exam Score"
                      )
ggplotly(LD_vs_NLD)
                    


#Extracurricular activity participation vs attendance
#Shows us that participation in extracurricular activities has essentially no impact on class attendance
                    ggplot(StudentPerformanceFactors, aes(x = Extracurricular_Activities, y = Attendance)) +
                      geom_boxplot() +
                      labs(
                        title = "Extracurricular Activity Participation vs Attendance",
                        subtitle = "Student Performance Factors Dataset",
                        x = "Participation in Extracurricular Activities",
                        y = "Class Attendance Percentage"
                      )
                    
                    
#Hours of Sleep vs Motivation Level
motiv = StudentPerformanceFactors |> mutate(Motiv_Level = factor(Motivation_Level, levels = c("Low", "Medium", "High")))
                    ggplot(motiv, aes(x = Sleep_Hours, color = Motiv_Level)) +
                      geom_bar() +
                      labs(
                        title = "Student Motivation Level vs Hours of Sleep",
                        subtitle = "Student Performance Factors Dataset",
                        x = "Student Motivation Level",
                        y = "Hours of Sleep"
                      )
                    
                    
#Math Score vs Reading Score
#Shows us higher math score correlates with higher reading score
                    ggplot(AcademicPerformance, aes(x = reading.score, y = math.score)) +
                      geom_jitter() +
                      geom_smooth() +
                      labs(
                        title = "Reading vs Math Scores",
                        subtitle = "Student Performance Dataset",
                        x = "Reading Score",
                        y = "Math Score"
                      )
                    
                    
#Math Score vs Writing Score
#Shows us higher math score correlates with higher reading score
                    ggplot(AcademicPerformance, aes(x = writing.score, y = math.score)) +
                      geom_jitter() +
                      geom_smooth() +
                      labs(
                        title = "Writing vs Math Scores",
                        subtitle = "Student Performance Dataset",
                        x = "Writing Score",
                        y = "Math Score"
                      )
                    
                    
#Reading Score vs Writing Score
#Shows us higher writing score correlates with higher reading score, a very strong. positive, and linear relationship
                    ggplot(AcademicPerformance, aes(x = writing.score, y = reading.score)) +
                      geom_jitter() +
                      geom_smooth() +
                      labs(
                        title = "Writing vs Reading Scores",
                        subtitle = "Student Performance Dataset",
                        x = "Writing Score",
                        y = "Reading Score"
                      )
           
                                
#Gender Effect on Math Scores
#Males better in math on average
                    genderandmath <- ggplot(AcademicPerformance, aes(x = gender, y = math.score)) +
                      geom_boxplot() +
                      labs(
                        title = "Gender effect on Math Scores",
                        subtitle = "Student Performance Dataset",
                        x = "Gender",
                        y = "Math Score"
                      )
                    ggplotly(genderandmath)
                   

#Gender Effect on Reading Scores
#Females better in reading on average
                    genderandreading <- ggplot(AcademicPerformance, aes(x = gender, y = reading.score)) +
                      geom_boxplot() +
                      labs(
                        title = "Gender effect on Reading Scores",
                        subtitle = "Student Performance Dataset",
                        x = "Gender",
                        y = "Reading Score"
                      )
                    ggplotly(genderandreading)
                    

#Gender Effect on Writing Scores
#Females better in writing on average
                    genderandwriting <- ggplot(AcademicPerformance, aes(x = gender, y = writing.score)) +
                      geom_boxplot() +
                      labs(
                        title = "Gender effect on Writing Scores",
                        subtitle = "Student Performance Dataset",
                        x = "Gender",
                        y = "Writing Score"
                      )
                    ggplotly(genderandwriting)
  
                                      
#Lunch status Effect on Writing Score
#Shows us that students with lower income perform worse
                   lunch_statusandwriting <- ggplot(AcademicPerformance, aes(x = lunch, y = writing.score)) +
                      geom_boxplot() +
                      labs(
                        title = "Lunch status effect on Writing Scores",
                        subtitle = "Student Performance Dataset",
                        x = "Lunch",
                        y = "Writing Score"
                      )
                   ggplotly(lunch_statusandwriting)
          
                             
#Lunch status Effect on Reading Score
#Shows us that students with lower income perform worse
                   lunch_statusandreading <-ggplot(AcademicPerformance, aes(x = lunch, y = reading.score)) +
                      geom_boxplot() +
                      labs(
                        title = "Lunch status effect on Reading Scores",
                        subtitle = "Student Performance Dataset",
                        x = "Lunch",
                        y = "Reading Score"
                      )
                   ggplotly(lunch_statusandreading)

                                       
#Lunch status Effect on Math Score
#Shows us that students with lower income perform worse
                   lunch_statusandmath <- ggplot(AcademicPerformance, aes(x = lunch, y = math.score)) +
                      geom_boxplot() +
                      labs(
                        title = "Lunch status effect on Math Scores",
                        subtitle = "Student Performance Dataset",
                        x = "Lunch",
                        y = "Math Score"
                      )
                   ggplotly(lunch_statusandmath)
                   
                    
#Test Prep Effect on Writing
#Shows us that students who do test prep perform better
                   test_prepandwriting <- ggplot(AcademicPerformance, aes(x = test.preparation.course, y = writing.score)) +
                      geom_boxplot() +
                      labs(
                        title = "Test Preparation effect on Writing Scores",
                        subtitle = "Student Performance Dataset",
                        x = "Test Preparation",
                        y = "Writing Score"
                      )
                   ggplotly(test_prepandwriting)
                  
                   
#Test Prep Effect on Reading
#Shows us that students who do test prep perform better
                   test_prepandreading <- ggplot(AcademicPerformance, aes(x = test.preparation.course, y = reading.score)) +
                      geom_boxplot() +
                      labs(
                        title = "Test Preparation effect on Reading Scores",
                        subtitle = "Student Performance Dataset",
                        x = "Test Preparation",
                        y = "Reading Score"
                      )
                   ggplotly(test_prepandreading)
                   
                    
#Test Prep Effect on Math
#Shows us that students who do test prep perform better
#Seems like the effect of test prep on math score is less than the effect on reading and writing
                   test_prepandmath <- ggplot(AcademicPerformance, aes(x = test.preparation.course, y = math.score)) +
                      geom_boxplot() +
                      labs(
                        title = "Test Preparation effect on Math Scores",
                        subtitle = "Student Performance Dataset",
                        x = "Test Preparation",
                        y = "Math Score"
                      )
                   ggplotly(test_prepandmath)
                    
                   
#Parental Involvement Effect on Exam Score
#Shows us that more parental involvement correlates with higher exam scores
      parent = StudentPerformanceFactors |> mutate(Parental_Involvement = factor(Parental_Involvement, levels = c("Low", "Medium", "High")))
                   
      parent_involvementandscores <- ggplot(parent, aes(x = Parental_Involvement, y = Exam_Score)) +
                      geom_boxplot() +
                      labs(
                        title = r'(Parents' Involvement effect on Exam Scores)',
                        subtitle = "Student Performance Factors Dataset",
                        x = r"(Parents' Involvement)",
                        y = "Exam Score"
                      )
      ggplotly(parent_involvementandscores)
                    
      StudentsPerformance_clean <- StudentsPerformance |>
        mutate(
          parent_edu3 = case_when(
            str_detect(tolower(parental.level.of.education), "high school") ~ "High school or less",
            str_detect(tolower(parental.level.of.education), "some college|associate") ~ "Some college / associate",
            str_detect(tolower(parental.level.of.education), "bachelor|master") ~ "Bachelor or higher",
            TRUE ~ NA_character_
          )
        )
      
      StudentPerformanceFactors_clean <- StudentPerformanceFactors |>
        mutate(
          parent_edu3 = case_when(
            str_detect(tolower(Parental_Education_Level), "high school") ~ "High school or less",
            str_detect(tolower(Parental_Education_Level), "some college|associate") ~ "Some college / associate",
            str_detect(tolower(Parental_Education_Level), "bachelor|master") ~ "Bachelor or higher",
            TRUE ~ NA_character_
          )
        )
      
      scores_small <- StudentsPerformance_clean |>
        select(parent_edu3, math.score, reading.score, writing.score)
      
      factors_small <- StudentPerformanceFactors_clean |>
        select(parent_edu3)
      
      joined_data <- scores_small |>
        left_join(factors_small, by = "parent_edu3")
      
      avg_scores <- joined_data |>
        group_by(parent_edu3) |>
        summarise(
          mean_math    = mean(math.score, na.rm = TRUE),
          mean_reading = mean(reading.score, na.rm = TRUE),
          mean_writing = mean(writing.score, na.rm = TRUE),
          n = n()
        )
      
      avg_scores

      avg_scores_long <- avg_scores |>
        pivot_longer(
          cols = c(mean_math, mean_reading, mean_writing),
          names_to = "subject",
          values_to = "average_score"
        )
      
      ggplot(avg_scores_long, aes(x = parent_edu3, y = average_score, fill = subject)) +
        geom_col(position = "dodge") +
        labs(
          title = "Average Scores by Parental Education Level",
          x = "Parental Education Category",
          y = "Average Score",
          fill = "Subject"
        ) +
        theme_minimal() +
        scale_fill_manual(values = c("lightblue", "pink", "red"))