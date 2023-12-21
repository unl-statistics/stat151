
library(tidyverse)
library(lubridate)

# Create a calendar for your syllabus ----
# Source: http://svmiller.com/blog/2020/08/a-ggplot-calendar-for-your-semester/

# 1) what is the first Monday of the semester?
# Any number of ways to identify dates in R, but we'll use {lubridate} and the ymd() function here.
# Format: YYYYMMDD. In this example, 4 January 2022.

# Weekday(s) of class
class_wdays <- c("Wed")

# Spring Break was 09 March 2024 to 17 March 2024.
not_here_dates <- c(
  # ymd("20220117"),
  # Spring Break
  seq(ymd(20240309),ymd(20240317), by=1))

# You can adjust this as you see fit. Basically: add assignment types (e.g. papers, quizzes).
# My intro class was fairly simple: just exams.
exam_dates <- c(ymd(20240306), ymd(20240307), ymd(20240508), ymd(20240510))

# What are the full dates of the semester? 
# In this case: 22 January to 17 May
semester_dates <- seq(ymd(20240122), ymd(20240510), by=1)

exam_week <- seq(ymd(20240513), ymd(20240518), by = 1)

# Custom function for treating the first day of the month as the first week 
# of the month up until the first Sunday (unless Sunday was the start of the month)
wom <- function(date) {
  first <- wday(as.Date(paste(year(date),month(date),1,sep="-")))
  return((mday(date)+(first-2)) %/% 7+1)
}

# Create a data frame of dates, assign to Cal
Cal <- tibble(date = seq(ymd(20240101), ymd(20240530), by=1))  %>%
  mutate(mon = lubridate::month(date, label=T, abbr=F), # get month label
         wkdy = weekdays(date, abbreviate=T), # get weekday label
         wkdy = fct_relevel(wkdy, "Sun", "Mon", "Tue", "Wed", "Thu","Fri","Sat"), # make sure Sunday comes first
         semester = date %in% semester_dates, # is date part of the semester?
         exams = date %in% exam_dates, # is it an exam?
         not_here = date %in% not_here_dates, # is it a day off?
         exam_wk = date %in% exam_week,
         day = lubridate::mday(date), # get day of month to add later as a label
         # Below: our custom wom() function
         week = wom(date))

# Create a category variable, for filling.
# I can probably make this a case_when(), but this will work.

Cal <- Cal %>%
  mutate(category = case_when(
    exams ~ "Exam",
    not_here ~ "UNL holiday",
    semester & wkdy %in% class_wdays & !not_here & !exam_wk ~ "Class Day",
    semester ~ "Semester",
    TRUE ~ "NA"
  ))

class_cal <- Cal %>% 
  ggplot(.,aes(wkdy, week)) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        legend.position = c(1, 0), legend.justification = c(1,0), legend.direction = "vertical", legend.title = element_blank(),
        axis.title.y = element_blank(), axis.title.x = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  # geom_tile and facet_wrap will do all the heavy lifting
  geom_tile(alpha=0.8, aes(fill=category), color="black", linewidth=.45) +
  facet_wrap(~mon, scales = "free", ncol=3) +
  # fill in tiles to make it look more "calendary" (sic)
  geom_text(aes(label=day, color = semester&(!not_here))) +
  # put your y-axis down, flip it, and reverse it
  scale_y_reverse(breaks=NULL) +
  # manually fill scale colors to something you like...
  scale_color_manual(values = c("FALSE" = "white", "TRUE" = "black"), guide = "none") + 
  scale_fill_manual(values=c("Class Day"="purple", 
                             "Semester"="white",
                             "UNL holiday" = "grey10",
                             "NA" = "white", # I like these whited out...
                             "Exam"="orange"),
                    #... but also suppress a label for a non-class semester day
                    breaks=c("Semester", "UNL holiday", "Class Day","Exam"))
# class_cal

exam_days <- filter(Cal, category == "Exam") %>% 
  mutate(topic = c("Midterm Assigned", "Midterm Due", "Final Assigned", "Final Due"),
         time = c("In class", "6pm", "In class", "6pm"))

class_days <- filter(Cal, category == "Class Day") %>%
  mutate(topic = c(
    "Getting Started", 
    "Scripts & Notebooks", 
    "Intro to Programming", 
    "Data Types", 
    "Data Structures", 
    "Control Structures", 
    "Functions", 
    "Version Control", 
    "Data Visualization", 
    "Data Cleaning", 
    "Strings", 
    "Reshaping Data", 
    "Graphics")) %>%
  bind_rows(exam_days) %>%
  arrange(date)
