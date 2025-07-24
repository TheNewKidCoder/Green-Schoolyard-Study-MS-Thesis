## MS Thesis- GSY Study## 

## Title:: Recess socialization and perceptions of school climate by body composition:  
#Evidence from The Green Schoolyard Study in Little Rock, Arkansas..##
rm(list=ls())
require("haven")
library(haven)
library(forcats)
library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)

#working data path
setwd("C:/Users/3079213/Desktop/ms thesis/COPH-MS Thesis/ms thesis data files/Data files")
GSYData<- read.csv("GSYIRBApprovedDoNotE_DATA_LABELS_2024-02-28_1446.csv") 

##############################################################################
##                        School Climate Survey data                        ##
##############################################################################

ClimateSurvey <- c("Record.ID","Person.ID","Study.ID","School", "Grade", "Gender","Age","Birth.Month_numeric",
                   "Survey.Complete","Child.ID",
                   "At.this.school..boys.and.girls.are.treated.equally.well.",
                   "My.teachers.care.about.me.",
                   "It.is.easy.to.talk.with.teachers.at.this.school.",
                   "Students.at.this.school.get.along.well.with.each.other.",
                   "There.are.lots.of.chances.for.students.at.this.school.to.get.involved.in.sports..clubs..and.other.school.activities.outside.of.class.",
                   "I.am.happy.at.this.school.",
                   "I.feel.like.I.have.friends.at.this.school.",
                   "I.feel.safe.at.this.school.",
                   "I.feel.safe.going.to.and.from.this.school.",
                   "The.school.grounds.and.playgrounds.are.kept.clean.and.in.working.order.",
                   "The.school.grounds.and.playgrounds.have.many.different.things.to.play.on..swings..slides..climbing..",
                   "The.school.grounds.and.playgrounds.have.a.lot.of.balls..soccer.balls.basketballs...jump.ropes..blocks.and.game.equipment.for.us.to.play.with.",
                   "The.school.grounds.and.playgrounds.have.enough.space.for.all.activities.we.would.like.to.play.",
                   "At.school..how.much.do.you.enjoy.being.active.playing.at.recess.",
                   "I.would.have.more.fun.on.the.playground.at.recess.if.my.friends.were.doing.activities.too.",
                   "At.school..how.much.do.you.enjoy.playing.with.sporting.equipment..example..basketball.or.soccer..",
                   "At.school..how.much.do.you.enjoy.playing.with.playground.equipment..example..jungle.gym..slides..swings..",
                   "At.school..how.much.do.you.enjoy.playing.on.the.grassy.areas.with.more.natural.things.such.as.trees..rocks..and.garden.area.",
                   "After.outside.recess..I.feel.more.energized.to.learn.at.school."
)

## Convert the Birth month columns into numbers for the ease of Child BMI calculation 

GSYData$Birth.Month_numeric <- ifelse(GSYData$Birth.Month %in% c("January", "February", "March", "April", "May", "June", "July",
                                                                 "August", "September", "October", "November", "December"),
                                      match(GSYData$Birth.Month, month.name),
                                      NA)

# Print the modified data frame
print(GSYData)
## Fixes for child ID and survey completion status
rows_to_update <- subset(GSYData, Child.ID == 140 & Survey.Complete == "Not applicable")
rows_to_update$Child.ID <- 1040
rows_to_update$Survey.Complete <- "Yes"
GSYData[rownames(GSYData) %in% rownames(rows_to_update), ] <- rows_to_update

## Change the name of the schools to maintain anonymity (where A= Brady, B= McDermott, C= Stephens, and D= Washington)
GSYData <- GSYData %>%
  mutate(School = case_when(
    School == "Brady" ~ "A",
    School == "McDermott" ~ "B",
    School == "Stephens" ~ "C",
    School == "Washington" ~ "D",
    TRUE ~ as.character(School)  
  ))

# Check the unique values in the School column after renaming
unique(GSYData$School)

# Create the ClimateSurvey data frame using the dplyr approach
Survey <- GSYData %>%
  select(all_of(ClimateSurvey)) %>%
  group_by(across(everything())) %>%
  summarize(count = n())

Survey <- as.data.frame(Survey)

##############################################################################################################################
## Convert the Survey response options to numeric value where 1= "Strongly Agree", 2= "Agree", 3="No opinion",               #
# 4="Disagree", 5="Strongly Disagree"                                                                                        #
##############################################################################################################################

Survey <- Survey %>%
  mutate(across(starts_with (c("At.", "My.", "It.", "I.", "The.","There.", "Students.", "After.")), ~case_when(
    . == "Strongly Agree" ~ 1,
    . == "Agree" ~ 2,
    . == "No opinion" ~ 3,
    . == "Disagree" ~ 4,
    . == "Strongly Disagree" ~ 5,
    TRUE ~ NA_real_  # If none of the conditions are met, set to NA
  )))

print(Survey)

#########################################################################
## Filtering out rows that has a completed survey indicating they fall in 
# grade 3to5 (upper grade or Survey takers)
#########################################################################

# Filter rows where Survey.Complete is "Yes"
FilteredData <- Survey %>%
  filter(Survey.Complete == "Yes")

# Create the final ClimateSurvey data frame 
YesSurvey <- FilteredData %>%
  select(all_of(ClimateSurvey)) %>%
  group_by(across(everything())) %>%
  summarize(count = n())

YesSurvey <- as.data.frame(YesSurvey)

#########################################################################
## Filtering out rows that has a incomplete survey or not applicable indicating they fall in 
# grade Kto2 (lower grade or Non-Survey takers)
#########################################################################

# Filter rows where Survey.Complete is "No"
FilteredData2 <- Survey %>%
  filter(Survey.Complete == "No")

# Create the final ClimateSurvey data frame 
NoSurvey <- FilteredData2 %>%
  select(all_of(ClimateSurvey)) %>%
  group_by(across(everything())) %>%
  summarize(count = n())

NoSurvey <- as.data.frame(NoSurvey)

#########################################################################

# Filter rows where Survey.Complete is "Not applicable"
FilteredData3 <- Survey %>%
  filter(Survey.Complete == "Not applicable")

# Create the final ClimateSurvey data frame 
NASurvey <- FilteredData3 %>%
  select(all_of(ClimateSurvey)) %>%
  group_by(across(everything())) %>%
  summarize(count = n())

NASurvey <- as.data.frame(NASurvey)

######################################################################################
#Combine the tables with Survey.Complete = "Yes", "No" and "NA" for final usable table and excluded the line that has no answers

NewSurvey<- rbind(YesSurvey,NoSurvey, NASurvey)
print(NewSurvey$Survey.Complete)

######################################################################################
##########################   Data Visualizations   ###################################
######################################################################################

## Q1 At.this.school..boys.and.girls.are.treated.equally.well. ##

# Calculate Percentage Counts for each Gender and response
percentage_counts_gender <- prop.table(table(NewSurvey$At.this.school..boys.and.girls.are.treated.equally.well., NewSurvey$Gender), margin = 2) * 100

# Convert to data frame and reshape
plot_data_gender <- as.data.frame(as.table(percentage_counts_gender))

# Rename columns for clarity
colnames(plot_data_gender) <- c("response", "gender", "percentage")

# Create the double bar plot using ggplot2

ggplot(plot_data_gender, aes(x = response, y = percentage, fill = factor(gender))) +
  geom_bar(stat = "identity", position = "dodge", color = "white", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_dodge(0.9), vjust = -0.5) +
  labs(title = "At this school, boys and girls are treated equally well",
       x = "G/B Treated equal",
       y = "Percentage") +
  scale_fill_manual(values = c("Male" = "skyblue", "Female" = "pink")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



## Q2 My.teachers.care.about.me.##

# Calculate Percentage Counts for each Gender and response
percentage_counts_teacher_gender <- prop.table(table(NewSurvey$My.teachers.care.about.me., NewSurvey$Gender), margin = 2) * 100

# Convert to data frame and reshape
plot_data_teacher_gender <- as.data.frame(as.table(percentage_counts_teacher_gender))

# Rename columns for clarity
colnames(plot_data_teacher_gender) <- c("response", "gender", "percentage")

# Create the double bar plot using ggplot2

ggplot(plot_data_teacher_gender, aes(x = response, y = percentage, fill = factor(gender))) +
  geom_bar(stat = "identity", position = "dodge", color = "white", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_dodge(0.9), vjust = -0.5) +
  labs(title = "My teachers care about me",
       x = "Teachers Care",
       y = "Percentage") +
  scale_fill_manual(values = c("Male" = "skyblue", "Female" = "pink")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

## Q3 It is easy to talk with teachers at this school##

# Calculate Percentage Counts for each Gender and response
percentage_counts_easy_talk_gender <- prop.table(table(NewSurvey$It.is.easy.to.talk.with.teachers.at.this.school., NewSurvey$Gender), margin = 2) * 100

# Convert to data frame and reshape
plot_data_easy_talk_gender <- as.data.frame(as.table(percentage_counts_teacher_gender))

# Rename columns for clarity
colnames(plot_data_teacher_gender) <- c("response", "gender", "percentage")

# Create the double bar plot using ggplot2

ggplot(plot_data_teacher_gender, aes(x = response, y = percentage, fill = factor(gender))) +
  geom_bar(stat = "identity", position = "dodge", color = "white", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_dodge(0.9), vjust = -0.5) +
  labs(title = "It is easy to talk with teachers at this school",
       x = "Easy to talk to teachers",
       y = "Percentage") +
  scale_fill_manual(values = c("Male" = "skyblue", "Female" = "pink")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

## Q4 Students.at.this.school.get.along.well.with.each.other##

# Calculate Percentage Counts for each Gender and response
percentage_counts_students_gender <- prop.table(table(NewSurvey$Students.at.this.school.get.along.well.with.each.other., NewSurvey$Gender), margin = 2) * 100

# Convert to data frame and reshape
plot_data_students_gender <- as.data.frame(as.table(percentage_counts_students_gender))

# Rename columns for clarity
colnames(plot_data_students_gender) <- c("response", "gender", "percentage")

# Create the double bar plot using ggplot2

ggplot(plot_data_students_gender, aes(x = response, y = percentage, fill = factor(gender))) +
  geom_bar(stat = "identity", position = "dodge", color = "white", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_dodge(0.9), vjust = -0.5) +
  labs(title = "Students at this school get along well with each other",
       x = "Students Getting Along",
       y = "Percentage") +
  scale_fill_manual(values = c("Male" = "skyblue", "Female" = "pink")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

## Q5 There.are.lots.of.chances.for.students.at.this.school.to.get.involved.in.sports..clubs..and.other.school.activities.outside.of.class##

# Calculate Percentage Counts for each Gender and response
percentage_counts_opportunities_gender <- prop.table(table(NewSurvey$There.are.lots.of.chances.for.students.at.this.school.to.get.involved.in.sports..clubs..and.other.school.activities.outside.of.class., NewSurvey$Gender), margin = 2) * 100

# Convert to data frame and reshape
plot_data_opportunities_gender <- as.data.frame(as.table(percentage_counts_opportunities_gender))

# Rename columns for clarity
colnames(plot_data_opportunities_gender) <- c("response", "gender", "percentage")

# Create the double bar plot using ggplot2

ggplot(plot_data_opportunities_gender, aes(x = response, y = percentage, fill = factor(gender))) +
  geom_bar(stat = "identity", position = "dodge", color = "white", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_dodge(0.9), vjust = -0.5) +
  labs(title = "There are lots of chances for students at this school to get involved
in sports, clubs, and other school activities outside of class",
       x = "Opportunities Outside School",
       y = "Percentage") +
  scale_fill_manual(values = c("Male" = "skyblue", "Female" = "pink")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

## Q6 I.am.happy.at.this.school##

# Calculate Percentage Counts for each Gender and response
percentage_counts_happy_gender <- prop.table(table(NewSurvey$I.am.happy.at.this.school., NewSurvey$Gender), margin = 2) * 100

# Convert to data frame and reshape
plot_data_happy_gender <- as.data.frame(as.table(percentage_counts_happy_gender))

# Rename columns for clarity
colnames(plot_data_happy_gender) <- c("response", "gender", "percentage")

# Create the double bar plot using ggplot2

ggplot(plot_data_happy_gender, aes(x = response, y = percentage, fill = factor(gender))) +
  geom_bar(stat = "identity", position = "dodge", color = "white", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_dodge(0.9), vjust = -0.5) +
  labs(title = "I am happy at this school",
       x = "Happy at School",
       y = "Percentage") +
  scale_fill_manual(values = c("Male" = "skyblue", "Female" = "pink")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

## Q7 I.feel.like.I.have.friends.at.this.school.##

# Calculate Percentage Counts for each Gender and response
percentage_counts_friends_gender <- prop.table(table(NewSurvey$I.feel.like.I.have.friends.at.this.school., NewSurvey$Gender), margin = 2) * 100

# Convert to data frame and reshape
plot_data_friends_gender <- as.data.frame(as.table(percentage_counts_friends_gender))

# Rename columns for clarity
colnames(plot_data_friends_gender) <- c("response", "gender", "percentage")

# Create the double bar plot using ggplot2

ggplot(plot_data_friends_gender, aes(x = response, y = percentage, fill = factor(gender))) +
  geom_bar(stat = "identity", position = "dodge", color = "white", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_dodge(0.9), vjust = -0.5) +
  labs(title = "I feel like I have friends at this school",
       x = "Have Friends at School",
       y = "Percentage") +
  scale_fill_manual(values = c("Male" = "skyblue", "Female" = "pink")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

## Q8 I.feel.safe.at.this.school.##

# Calculate Percentage Counts for each Gender and response
percentage_counts_safety_gender <- prop.table(table(NewSurvey$I.feel.safe.at.this.school., NewSurvey$Gender), margin = 2) * 100

# Convert to data frame and reshape
plot_data_safety_gender <- as.data.frame(as.table(percentage_counts_safety_gender))

# Rename columns for clarity
colnames(plot_data_safety_gender) <- c("response", "gender", "percentage")

# Create the double bar plot using ggplot2

ggplot(plot_data_safety_gender, aes(x = response, y = percentage, fill = factor(gender))) +
  geom_bar(stat = "identity", position = "dodge", color = "white", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_dodge(0.9), vjust = -0.5) +
  labs(title = "I feel safe at this school",
       x = "Feel Safe at School",
       y = "Percentage") +
  scale_fill_manual(values = c("Male" = "skyblue", "Female" = "pink")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

## Q9 I.feel.safe.going.to.and.from.this.school.##

# Calculate Percentage Counts for each Gender and response
percentage_counts_safety_travel_gender <- prop.table(table(NewSurvey$I.feel.safe.going.to.and.from.this.school., NewSurvey$Gender), margin = 2) * 100

# Convert to data frame and reshape
plot_data_safety_travel_gender <- as.data.frame(as.table(percentage_counts_safety_travel_gender))

# Rename columns for clarity
colnames(plot_data_safety_travel_gender) <- c("response", "gender", "percentage")

# Create the double bar plot using ggplot2

ggplot(plot_data_safety_travel_gender, aes(x = response, y = percentage, fill = factor(gender))) +
  geom_bar(stat = "identity", position = "dodge", color = "white", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_dodge(0.9), vjust = -0.5) +
  labs(title = "I feel safe going to and from this school",
       x = "Feel Safe Traveling to and from School",
       y = "Percentage") +
  scale_fill_manual(values = c("Male" = "skyblue", "Female" = "pink")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

## Q10 The.school.grounds.and.playgrounds.are.kept.clean.and.in.working.order.##

# Calculate Percentage Counts for each Gender and response
percentage_counts_clean_grounds_gender <- prop.table(table(NewSurvey$The.school.grounds.and.playgrounds.are.kept.clean.and.in.working.order., NewSurvey$Gender), margin = 2) * 100

# Convert to data frame and reshape
plot_data_clean_grounds_gender <- as.data.frame(as.table(percentage_counts_clean_grounds_gender))

# Rename columns for clarity
colnames(plot_data_clean_grounds_gender) <- c("response", "gender", "percentage")

# Create the double bar plot using ggplot2

ggplot(plot_data_clean_grounds_gender, aes(x = response, y = percentage, fill = factor(gender))) +
  geom_bar(stat = "identity", position = "dodge", color = "white", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_dodge(0.9), vjust = -0.5) +
  labs(title = "The school grounds and playgrounds are kept clean 
and in working order",
       x = "Clean and Working Grounds",
       y = "Percentage") +
  scale_fill_manual(values = c("Male" = "skyblue", "Female" = "pink")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

## Q11 The.school.grounds.and.playgrounds.have.many.different.things.to.play.on..swings..slides..climbing..##

# Calculate Percentage Counts for each Gender and response
percentage_counts_playgrounds_gender <- prop.table(table(NewSurvey$The.school.grounds.and.playgrounds.have.many.different.things.to.play.on..swings..slides..climbing.., NewSurvey$Gender), margin = 2) * 100

# Convert to data frame and reshape
plot_data_playgrounds_gender <- as.data.frame(as.table(percentage_counts_playgrounds_gender))

# Rename columns for clarity
colnames(plot_data_playgrounds_gender) <- c("response", "gender", "percentage")

# Create the double bar plot using ggplot2

ggplot(plot_data_playgrounds_gender, aes(x = response, y = percentage, fill = factor(gender))) +
  geom_bar(stat = "identity", position = "dodge", color = "white", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_dodge(0.9), vjust = -0.5) +
  labs(title = "The school grounds and playgrounds have many different things 
to play on (swings, slides, climbing)",
       x = "Variety of Things to Play With",
       y = "Percentage") +
  scale_fill_manual(values = c("Male" = "skyblue", "Female" = "pink")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

## Q12 The.school.grounds.and.playgrounds.have.a.lot.of.balls..soccer.balls.basketballs...jump.ropes..blocks.and.game.equipment.for.us.to.play.with.##
# Calculate Percentage Counts for each Gender and response
percentage_counts_game_equipment_gender <- prop.table(table(NewSurvey$The.school.grounds.and.playgrounds.have.a.lot.of.balls..soccer.balls.basketballs...jump.ropes..blocks.and.game.equipment.for.us.to.play.with., NewSurvey$Gender), margin = 2) * 100

# Convert to data frame and reshape
plot_data_game_equipment_gender <- as.data.frame(as.table(percentage_counts_game_equipment_gender))

# Rename columns for clarity
colnames(plot_data_game_equipment_gender) <- c("response", "gender", "percentage")

# Create the double bar plot using ggplot2

ggplot(plot_data_game_equipment_gender, aes(x = response, y = percentage, fill = factor(gender))) +
  geom_bar(stat = "identity", position = "dodge", color = "white", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_dodge(0.9), vjust = -0.5) +
  labs(title = "The school grounds and playgrounds have a lot of balls (soccer balls/ 
basketballs), jump ropes,blocks and game equipment for us to play with",
       x = "Game Equipments",
       y = "Percentage") +
  scale_fill_manual(values = c("Male" = "skyblue", "Female" = "pink")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


## Q13 The.school.grounds.and.playgrounds.have.enough.space.for.all.activities.we.would.like.to.play.##

# Calculate Percentage Counts for each Gender and response
percentage_counts_space_gender <- prop.table(table(NewSurvey$The.school.grounds.and.playgrounds.have.enough.space.for.all.activities.we.would.like.to.play., NewSurvey$Gender), margin = 2) * 100

# Convert to data frame and reshape
plot_data_space_gender <- as.data.frame(as.table(percentage_counts_space_gender))

# Rename columns for clarity
colnames(plot_data_space_gender) <- c("response", "gender", "percentage")

# Create the double bar plot using ggplot2

ggplot(plot_data_space_gender, aes(x = response, y = percentage, fill = factor(gender))) +
  geom_bar(stat = "identity", position = "dodge", color = "white", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_dodge(0.9), vjust = -0.5) +
  labs(title = "The school grounds and playgrounds have enough space 
for all activities we would like to play ",
       x = "Enough Space",
       y = "Percentage") +
  scale_fill_manual(values = c("Male" = "skyblue", "Female" = "pink")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

## Q14 At.school..how.much.do.you.enjoy.being.active.playing.at.recess.##

# Calculate Percentage Counts for each Gender and response
percentage_counts_recess_gender <- prop.table(table(NewSurvey$At.school..how.much.do.you.enjoy.being.active.playing.at.recess., NewSurvey$Gender), margin = 2) * 100

# Convert to data frame and reshape
plot_data_recess_gender <- as.data.frame(as.table(percentage_counts_recess_gender))

# Rename columns for clarity
colnames(plot_data_recess_gender) <- c("response", "gender", "percentage")

# Create the double bar plot using ggplot2

ggplot(plot_data_recess_gender, aes(x = response, y = percentage, fill = factor(gender))) +
  geom_bar(stat = "identity", position = "dodge", color = "white", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_dodge(0.9), vjust = -0.5) +
  labs(title = "At school, how much do you enjoy being active/playing at recess?",
       x = "Enjoy Recess",
       y = "Percentage") +
  scale_fill_manual(values = c("Male" = "skyblue", "Female" = "pink")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

## Q15 I.would.have.more.fun.on.the.playground.at.recess.if.my.friends.were.doing.activities.too.##

# Calculate Percentage Counts for each Gender and response
percentage_counts_friends_playing_gender <- prop.table(table(NewSurvey$I.would.have.more.fun.on.the.playground.at.recess.if.my.friends.were.doing.activities.too., NewSurvey$Gender), margin = 2) * 100

# Convert to data frame and reshape
plot_data_friends_playing_gender <- as.data.frame(as.table(percentage_counts_friends_playing_gender))

# Rename columns for clarity
colnames(plot_data_friends_playing_gender) <- c("response", "gender", "percentage")

# Create the double bar plot using ggplot2

ggplot(plot_data_friends_playing_gender, aes(x = response, y = percentage, fill = factor(gender))) +
  geom_bar(stat = "identity", position = "dodge", color = "white", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_dodge(0.9), vjust = -0.5) +
  labs(title = "I would have more fun on the playground at recess if 
my friends were doing activities too",
       x = "Friends Playing Along",
       y = "Percentage") +
  scale_fill_manual(values = c("Male" = "skyblue", "Female" = "pink")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

## Q16 At.school..how.much.do.you.enjoy.playing.with.sporting.equipment..example..basketball.or.soccer..##

# Calculate Percentage Counts for each Gender and response
percentage_counts_sporting_equipment_gender <- prop.table(table(NewSurvey$At.school..how.much.do.you.enjoy.playing.with.sporting.equipment..example..basketball.or.soccer.., NewSurvey$Gender), margin = 2) * 100

# Convert to data frame and reshape
plot_data_sporting_equipment_gender <- as.data.frame(as.table(percentage_counts_sporting_equipment_gender))

# Rename columns for clarity
colnames(plot_data_sporting_equipment_gender) <- c("response", "gender", "percentage")

# Create the double bar plot using ggplot2

ggplot(plot_data_sporting_equipment_gender, aes(x = response, y = percentage, fill = factor(gender))) +
  geom_bar(stat = "identity", position = "dodge", color = "white", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_dodge(0.9), vjust = -0.5) +
  labs(title = "At school, how much do you enjoy playing with sporting 
equipment (example: basketball or soccer)?",
       x = "Enjoy Playing with Sporting Equipment",
       y = "Percentage") +
  scale_fill_manual(values = c("Male" = "skyblue", "Female" = "pink")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

## Q17 At.school..how.much.do.you.enjoy.playing.with.playground.equipment..example..jungle.gym..slides..swings..##

# Calculate Percentage Counts for each Gender and response
percentage_counts_playground_equipment_gender <- prop.table(table(NewSurvey$At.school..how.much.do.you.enjoy.playing.with.playground.equipment..example..jungle.gym..slides..swings.., NewSurvey$Gender), margin = 2) * 100

# Convert to data frame and reshape
plot_data_playground_equipment_gender <- as.data.frame(as.table(percentage_counts_playground_equipment_gender))

# Rename columns for clarity
colnames(plot_data_playground_equipment_gender) <- c("response", "gender", "percentage")

# Create the double bar plot using ggplot2

ggplot(plot_data_playground_equipment_gender, aes(x = response, y = percentage, fill = factor(gender))) +
  geom_bar(stat = "identity", position = "dodge", color = "white", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_dodge(0.9), vjust = -0.5) +
  labs(title = "At school, how much do you enjoy playing with playground 
equipment (example: jungle gym, slides, swings)?",
       x = "Enjoy Playing with Playground Equipment",
       y = "Percentage") +
  scale_fill_manual(values = c("Male" = "skyblue", "Female" = "pink")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

## Q18 At.school..how.much.do.you.enjoy.playing.on.the.grassy.areas.with.more.natural.things.such.as.trees..rocks..and.garden.area.##

# Calculate Percentage Counts for each Gender and response
percentage_counts_green_area_gender <- prop.table(table(NewSurvey$At.school..how.much.do.you.enjoy.playing.on.the.grassy.areas.with.more.natural.things.such.as.trees..rocks..and.garden.area., NewSurvey$Gender), margin = 2) * 100

# Convert to data frame and reshape
plot_data_green_area_gender <- as.data.frame(as.table(percentage_counts_green_area_gender))

# Rename columns for clarity
colnames(plot_data_green_area_gender) <- c("response", "gender", "percentage")

# Create the double bar plot using ggplot2

ggplot(plot_data_green_area_gender, aes(x = response, y = percentage, fill = factor(gender))) +
  geom_bar(stat = "identity", position = "dodge", color = "white", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_dodge(0.9), vjust = -0.5) +
  labs(title = "At school, how much do you enjoy playing on the grassy areas 
with more natural things such as trees, rocks, and garden area?",
       x = "Enjoy Green Area",
       y = "Percentage") +
  scale_fill_manual(values = c("Male" = "skyblue", "Female" = "pink")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

## Q19 After.outside.recess..I.feel.more.energized.to.learn.at.school.##

# Calculate Percentage Counts for each Gender and response
percentage_counts_learning_interest_gender <- prop.table(table(NewSurvey$After.outside.recess..I.feel.more.energized.to.learn.at.school., NewSurvey$Gender), margin = 2) * 100

# Convert to data frame and reshape
plot_data_learning_interest_gender <- as.data.frame(as.table(percentage_counts_learning_interest_gender))

# Rename columns for clarity
colnames(plot_data_learning_interest_gender) <- c("response", "gender", "percentage")

# Create the double bar plot using ggplot2

ggplot(plot_data_learning_interest_gender, aes(x = response, y = percentage, fill = factor(gender))) +
  geom_bar(stat = "identity", position = "dodge", color = "white", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_dodge(0.9), vjust = -0.5) +
  labs(title = "After outside recess, I feel more energized to learn at school",
       x = "Learning Interest After Recess",
       y = "Percentage") +
  scale_fill_manual(values = c("Male" = "skyblue", "Female" = "pink")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#######################################################################################



##########################################################################
####            Anthropometric Data cleaning                          ####
##########################################################################

# Check the structure and summary of the data before and after conversion
str(GSYData)
summary(GSYData)

# Check for missing values in 'Height.in.Centimeters' and 'Weight.in.Kilograms'
missing_values <- any(is.na(GSYData$Height.in.Centimeters) | is.na(GSYData$Weight.in.Kilograms))

# Check if there are missing values 
if (missing_values) {
  print("Error: The 'Height.in.Centimeters' or 'Weight.in.Kilograms' columns have missing values.")
} else {
  # Convert 'Height.in.Centimeters' and 'Weight.in.Kilograms' columns to numeric
  GSYData$Height.in.Centimeters <- as.numeric(as.character(GSYData$Height.in.Centimeters))
  GSYData$Weight.in.Kilograms <- as.numeric(as.character(GSYData$Weight.in.Kilograms))
  
  # Check if 'Height.in.Centimeters' and 'Weight.in.Kilograms' columns are now numeric
  if (any(is.na(GSYData$Height.in.Centimeters)) | any(is.na(GSYData$Weight.in.Kilograms))) {
    print("Error: Conversion to numeric failed. Check for non-numeric values in the original data.")
  } else {
    # Calculate BMI and create the 'BMI' column
    GSYData$BMI <- GSYData$Weight.in.Kilograms / ((GSYData$Height.in.Centimeters/ 100)^2)
    
    # Print the updated data frame
    print(GSYData)
  }
}

GSYData$BMI <- GSYData$Weight.in.Kilograms / ((GSYData$Height.in.Centimeters/ 100)^2)
##################################################################################################
##             Creating data frame for BMI calculation                                          ##
##################################################################################################

GSYData$Age <- GSYData$Age * 12 + GSYData$Birth.Month_numeric
GSYData <- GSYData[, !(names(GSYData) %in% c("Birth.Month", "Birth.Month_numeric"))]
GSYData <- GSYData %>%
  mutate(Gender = ifelse(Gender == 'Male', 1, ifelse(Gender == 'Female', 2, Gender))) #uniform the values across (M=1, F=2)
GSYData$Gender <- as.numeric(GSYData$Gender)


##################################################################################################
##                             Study Methods                                                    ##
##################################################################################################

##Check number of male and female in our table

gender_counts<- table(GSYData$Gender)

# Print the counts for males and females
print(paste("Number of Males:", gender_counts["1"]))
print(paste("Number of Females:", gender_counts["2"]))

# Check for missing values in the "Gender" column
missing_gender <- sum(is.na(GSYData$Gender))

# Print or use the result as needed
if (missing_gender > 0) {
  print(paste("Number of missing values in the 'Gender' column:", missing_gender))
} else {
  print("No missing values in the 'Gender' column.")
}


################################################################################
##Get LMS values##
################################################################################
##Column titles appear once again in the middle of this csv file which is why
## nrows and skip arguments are being used.
lms<-rbind(read.csv("https://www.cdc.gov/growthcharts/data/zscore/bmiagerev.csv", nrows=219),
           read.csv("https://www.cdc.gov/growthcharts/data/zscore/bmiagerev.csv", skip=220))

names(lms)[1:2]<-c("Gender","Age") #Rename to match NHANES

## Round down, e.g., 28.5 months becomes 28 months to match NHANES
lms<-lms[lms$Age!=24,]
lms$Age<-lms$Age-0.5

################################################################################
## Compute BMI Z using the LMS method
## See https://www.cdc.gov/nccdphp/dnpao/growthcharts/resources/sas.htm
################################################################################
GSYData<-merge(GSYData,lms,by=c("Gender","Age"))
attach(GSYData)
GSYData$BMI_z_score <- ((BMI / M)^L - 1) / (L * S)
detach(GSYData)

hist(GSYData$BMI,main=NULL, xlab="BMI z-score")

# Create a histogram of BMI vs Frequency using ggplot
ggplot(GSYData, aes(x = BMI)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Distribution of BMI",
       x = "BMI",
       y = "Frequency") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Create a histogram of BMI z-score vs Frequency using ggplot
ggplot(GSYData, aes(x = BMI_z_score)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black", bins= 30) +
  labs(title = "Distribution of BMI z-scores",
       x = "BMI z-score",
       y = "Frequency") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


###############################################################################
#height to waist circumference ratio and read about WHO guidelines 
#(https://iris.who.int/bitstream/handle/10665/44583/9789241501491_eng.pGSYData?sequence=1)

#################################################################################
## Categorize the BMI z scores as per the WHO scale ##
## Link: https://www.cdc.gov/obesity/basics/childhood-defining.html
#################################################################################
GSYData$BMIp95_percent <- ((GSYData$BMI / GSYData$P95) * 100)

#################################################################################

# Define a function to categorize BMI
GSYData$Categorize_BMI<-"NA"
GSYData$Categorize_BMI<-ifelse(GSYData$BMI < GSYData$P5,"Underweight",GSYData$Categorize_BMI)
GSYData$Categorize_BMI<-ifelse(GSYData$BMI>= GSYData$P5 & GSYData$BMI<GSYData$P85,"Healthy Weight",GSYData$Categorize_BMI)
GSYData$Categorize_BMI<-ifelse(GSYData$BMI>=GSYData$P85 & GSYData$BMI<GSYData$P95,"Overweight",GSYData$Categorize_BMI)
GSYData$Categorize_BMI<-ifelse(GSYData$BMIp95_percent < 120 & GSYData$BMI >=  GSYData$P95,"Obesity",GSYData$Categorize_BMI)
GSYData$Categorize_BMI<-ifelse(GSYData$BMIp95_percent >= 120 & GSYData$BMI >=  GSYData$P95,"Severe Obesity",GSYData$Categorize_BMI)

#Check
sum(GSYData$Categorize_BMI=="Underweight")

##################################################################################
# Removing 3 cases abnormal readings falling under the "Underweight" BMI category
##################################################################################

# Filter out rows with BMI category "Underweight"
GSYData <- GSYData[GSYData$Categorize_BMI != "Underweight", ]

# Print the updated data frame
print(GSYData)

################################################################################

##Check number of individuals falling into each BMI category in our table

BMICat_counts<- table(GSYData$Categorize_BMI)

# Print the counts for each individual's BMI category

print(paste("Number of Underweight:", BMICat_counts["Underweight"]))
print(paste("Number of Healthy Weight:", BMICat_counts["Healthy Weight"]))
print(paste("Number of Overweight:", BMICat_counts["Overweight"]))
print(paste("Number of Obesity:", BMICat_counts["Obesity"]))
print(paste("Number of Severe Obesity:", BMICat_counts["Severe Obesity"]))

# Total number of individuals
total_individuals <- sum(BMICat_counts)

# Calculate the percentage for each BMI category
percentage_underweight <- (BMICat_counts["Underweight"] / total_individuals) * 100
percentage_healthy_weight <- (BMICat_counts["Healthy Weight"] / total_individuals) * 100
percentage_overweight <- (BMICat_counts["Overweight"] / total_individuals) * 100
percentage_obesity <- (BMICat_counts["Obesity"] / total_individuals) * 100
percentage_severeobesity <- (BMICat_counts["Severe Obesity"] / total_individuals) * 100


# Print the percentages
print(paste("Percentage of Underweight:", percentage_underweight, "%"))
print(paste("Percentage of Healthy Weight:", percentage_healthy_weight, "%"))
print(paste("Percentage of Overweight:", percentage_overweight, "%"))
print(paste("Percentage of Obesity:", percentage_obesity, "%"))
print(paste("Percentage of Severe Obesity:", percentage_severeobesity, "%"))


# Create a vector of percentages
percentage_vector <- c(percentage_healthy_weight, percentage_overweight, percentage_obesity, percentage_severeobesity)

# Define the categories for the x-axis
categories <- c("Healthy Weight", "Overweight", "Obesity", "Severe Obesity")


# Create a bar plot
bp <- barplot(percentage_vector, names.arg = categories, main = "BMI Category Distribution",
              ylab = "Percentage", col = "skyblue", border = "black", ylim = c(0, 100))

# Add percentage labels on top of each bar
text(x = bp, y = percentage_vector + 2, label = paste0(round(percentage_vector), "%"), pos = 3, cex = 0.8, col = "black")

######################################################################################

# Create a pie chart
pie<-pie(percentage_vector, labels = categories, main = "BMI Category Distribution", col = c("blue", "green", "yellow", "red"))

# Add percentage labels inside each slice
percent_labels <- paste0(categories, "\n", round(percentage_vector), "%")
legend("left", legend = percent_labels, cex = 0.7, fill = c("blue", "green", "yellow", "red"))



########################################################################################
##           Chi Square test and Fisher test on the Body composition data             ##
########################################################################################

# Gender vs. BMI Category
GSYData1 <- data.frame(Gender = GSYData$Gender, BMI_Category = GSYData$Categorize_BMI)
contingency_table_gender <- table(GSYData1$Gender, GSYData1$BMI_Category)
print(contingency_table_gender)
chi_square_result_gender<-chisq.test(contingency_table_gender)
print(chi_square_result_gender)

fisher_result_gender <- fisher.test(contingency_table_gender)
print(fisher_result_gender)

# Gender vs. Feeling safe at this school
GSYData2 <- data.frame(Gender = as.factor(GSYData$Gender), Feel_safe = as.factor(GSYData$I.feel.safe.at.this.school.))
contingency_table_feeling_safe <- table(GSYData2$Gender, GSYData2$Feel_safe)
print(contingency_table_feeling_safe)
chi_square_result_feeling_safe <- chisq.test(contingency_table_feeling_safe)
print(chi_square_result_feeling_safe)

fisher_result_safe <- fisher.test(contingency_table_feeling_safe)
print(fisher_result_safe)

# Gender vs. G/B are treated equally well
GSYData3 <- data.frame(Gender = as.factor(GSYData$Gender), Treated_equal = as.factor(GSYData$At.this.school..boys.and.girls.are.treated.equally.well.))
contingency_table_feeling_equal <- table(GSYData3$Gender, GSYData3$Treated_equal)
print(contingency_table_feeling_equal)
chi_square_result_feeling_equal <- chisq.test(contingency_table_feeling_equal)
print(chi_square_result_feeling_equal)

fisher_result_safe <- fisher.test(contingency_table_feeling_safe)
print(fisher_result_safe)

######################################################################################
##                           School wise data analysis                              ##
######################################################################################

# 1) Total count from each school

# Calculate total count of people from each school
school_total_counts <- GSYData %>%
  filter(Categorize_BMI != "Underweight") %>%  # Exclude Underweight category
  group_by(School) %>%
  summarize(TotalCount = n_distinct(Person.ID))

# Plotting a graph with total count of people from each school
ggplot(school_total_counts, aes(x = School, y = TotalCount)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_text(aes(label = TotalCount),  # Add text labels for total count
            vjust = -0.5, size = 3, color = "black") +  # Adjust text position, size, and color
  labs(title = "Total Count of People from Each School (Excluding Underweight)",
       x = "School",
       y = "Total Count") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# 2) Check number of Boys and Girls in each school

# Filter out underweight population
GSYData_filtered <- GSYData %>%
  filter(Categorize_BMI != "Underweight")

# Calculate number of boys and girls in each school after filtering
school_counts_filtered <- GSYData_filtered %>%
  group_by(School, Gender) %>%
  summarize(Count = n())

# Plotting a graph with counts after filtering
ggplot(school_counts_filtered, aes(x = School, y = Count, fill = factor(Gender))) +
  geom_bar(stat = "identity", position = "dodge") +  # Create a bar chart with dodging
  geom_text(aes(label = Count),  # Add text labels for counts
            position = position_dodge(width = 0.9),  # Position labels dodged
            vjust = -0.5, size = 3) +  # Adjust text position and size
  labs(title = "Number of Boys and Girls in Each School (Excluding Underweight)",  # Set plot title and axis labels
       x = "School",
       y = "Count",
       fill = "Gender") +  # Set legend title
  scale_fill_manual(values = c("blue", "orange"), labels = c("Male", "Female")) +  # Set colors and legend labels
  theme_bw() +  # Set theme to black and white
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank())  # Remove minor grid lines

##
# Filter out underweight population
GSYData_filtered <- GSYData %>%
  filter(Categorize_BMI != "Underweight")

# Calculate percentages for each gender in each school after filtering
school_counts_filtered <- GSYData_filtered %>%
  group_by(School, Gender) %>%
  summarize(Count = n()) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

# Plotting a graph with percentages after filtering
ggplot(school_counts_filtered, aes(x = School, y = Count, fill = factor(Gender))) +
  geom_bar(stat = "identity", position = "dodge") +  # Create a bar chart with dodging
  geom_text(aes(label = paste0(round(Percentage), "%")),  # Add text labels for percentages
            position = position_dodge(width = 0.9),  # Position labels dodged
            vjust = -0.5, size = 3) +  # Adjust text position and size
  labs(title = "Number of Boys and Girls in Each School (Excluding Underweight)",  # Set plot title and axis labels
       x = "School",
       y = "Count",
       fill = "Gender") +  # Set legend title
  scale_fill_manual(values = c("blue", "orange"), labels = c("Male", "Female")) +  # Set colors and legend labels
  theme_bw() +  # Set theme to black and white
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank())  # Remove minor grid lines

# 3) Check BMI Category % by school
# Calculate counts of individuals in each BMI category for each school (excluding Underweight)
bmi_counts <- GSYData %>%
  filter(Categorize_BMI != "Underweight") %>%  # Exclude Underweight category
  group_by(School, Categorize_BMI) %>%
  summarize(Count = n())

# Plotting a graph with BMI category counts by school (excluding Underweight)
ggplot(bmi_counts, aes(x = School, y = Count, fill = Categorize_BMI)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = Count),  # Add text labels for counts
            position = position_stack(vjust = 0.5),  # Position labels in the middle of each bar
            size = 3, color = "black") +  # Adjust text size and color
  labs(title = "BMI Category Counts by School (Excluding Underweight)",
       x = "School",
       y = "Count",
       fill = "BMI Category") +
  scale_fill_manual(values = c("green", "yellow", "orange", "red", "maroon"),  # Set colors for BMI categories
                    labels = c("Healthy Weight", "Overweight", "Obesity", "Severe Obesity")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

##
# Calculate counts of individuals in each BMI category for each school (excluding Underweight)
bmi_counts <- GSYData %>%
  filter(Categorize_BMI != "Underweight") %>%  # Exclude Underweight category
  group_by(School, Categorize_BMI) %>%
  summarize(Count = n())

# Calculate percentages of individuals in each BMI category for each school (excluding Underweight)
bmi_percentages <- bmi_counts %>%
  group_by(School) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

# Plotting a graph with BMI category percentages by school (excluding Underweight)
ggplot(bmi_percentages, aes(x = School, y = Percentage, fill = Categorize_BMI)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(Percentage), "%")),  # Add text labels for percentages
            position = position_stack(vjust = 0.5),  # Position labels in the middle of each bar
            size = 3, color = "black") +  # Adjust text size and color
  labs(title = "BMI Category Percentage by School.",
       x = "School",
       y = "Percentage",
       fill = "BMI Category") +
  scale_fill_manual(values = c("green", "yellow", "orange", "red", "maroon"),  # Set colors for BMI categories
                    labels = c("Healthy Weight", "Overweight", "Obesity", "Severe Obesity")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


######################################################################################  
##                                    SOCARP Data                                   ##
######################################################################################        

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
flist<-list.files("C:\\Users\\3079213\\Desktop\\ms thesis\\RecessData\\",pattern="[0-9].csv",recursive = T)

socarp<-NULL
for(i in 1:length(flist)) {
  print(i)
  print(flist[i])
  temp <- read.csv(paste0("C:\\Users\\3079213\\Desktop\\ms thesis\\RecessData\\", flist[i]), fill = TRUE, header = FALSE, colClasses = 'character')
  date<-substr(substrRight(flist[i],12),1,8)
  temp$date<-date
  print(temp)
  if(length(temp)==9) {
    socarp<-rbind(socarp,temp)
  }
}

######################################################################################
colnames(socarp) <- c("Readings", "Coder.ID", "Time", "Child.ID", "Activity level", "Group size", "Activity type", "Interactions", "Date")

head(socarp)
# table(socarp$V4)
# socarp[nchar(socarp$V4)<4,]


################################################################################
##                          Fix coder initials                                ##
################################################################################
socarp$Coder.ID<-ifelse(socarp$Coder.ID=="Ark","ARK",socarp$Coder.ID)
socarp$Coder.ID<-ifelse(socarp$Coder.ID=="Cmm","CMM",socarp$Coder.ID)
socarp$Coder.ID<-ifelse(socarp$Coder.ID=="Dt","DT",socarp$Coder.ID)
socarp<-socarp[socarp$Coder.ID!="Fghj",]
socarp<-socarp[socarp$Coder.ID!="DEMO",]
socarp<-socarp[socarp$Coder.ID!="TST",]
socarp$Coder.ID<-ifelse(socarp$Coder.ID=="Kw","KW",socarp$Coder.ID)
socarp$Coder.ID<-ifelse(socarp$Coder.ID%in%c("Met","Mrt"),"MRT",socarp$Coder.ID)
socarp$Coder.ID<-ifelse(socarp$Coder.ID%in%c("mjb","Mjb"),"MJB",socarp$Coder.ID)
socarp$Coder.ID<-ifelse(socarp$Coder.ID=="UA mk","MKK",socarp$Coder.ID)
socarp$Coder.ID<-ifelse(socarp$Coder.ID=="Marisa kk","MKK",socarp$Coder.ID)
socarp$Coder.ID<-ifelse(socarp$Coder.ID=="Mk ua","MKK",socarp$Coder.ID)
socarp$Coder.ID<-ifelse(socarp$Coder.ID=="MK","MKK",socarp$Coder.ID)
socarp$Coder.ID<-ifelse(socarp$Coder.ID=="Marisa K K","MKK",socarp$Coder.ID)
socarp$Coder.ID<-ifelse(socarp$Coder.ID=="Uaekh","EKH",socarp$Coder.ID)
socarp$Coder.ID<-ifelse(socarp$Coder.ID=="UALRM","LRM",socarp$Coder.ID)
socarp$Coder.ID<-ifelse(socarp$Coder.ID=="Lw","LW",socarp$Coder.ID)
table(socarp$Coder.ID,useNA="always")
################################################################################

# Example assuming "YYYYMMDD" format for date
socarp$Date <- as.Date(socarp$Date, format = "%Y%m%d")

################################################################################
##                          Fix student ID, posture, sportsmanship            ##
################################################################################
socarp$Child.ID<- ifelse(socarp$Child.ID=="100","1100", socarp$Child.ID)
socarp$Child.ID<- ifelse(socarp$Child.ID=="102","1102", socarp$Child.ID)
socarp$Child.ID<- ifelse(socarp$Child.ID=="106","1106", socarp$Child.ID)
socarp$Child.ID<- ifelse(socarp$Child.ID=="166","1166", socarp$Child.ID)
################################################################################
# socarp$`Activity level`<- ifelse(socarp$`Activity level`=="veryAct", "very active", socarp$`Activity level`)
# socarp$Interactions<-ifelse(socarp$Interactions=="physS", "physical sportsmanship",socarp$Interactions)
# socarp$Interactions<-ifelse(socarp$Interactions=="verbS", "verbal sportsmanship",socarp$Interactions)
# socarp$Interactions<-ifelse(socarp$Interactions=="physC", "physical conflict",socarp$Interactions)
# socarp$Interactions<-ifelse(socarp$Interactions=="physC", "physical conflict",socarp$Interactions)
# socarp$Interactions<-ifelse(socarp$Interactions=="verbC", "verbal conflict",socarp$Interactions)
################################################################################
#Check value in the columns
unique(socarp$Interactions)
table(socarp$`Activity level`,socarp$`Group size`)
table(socarp$`Group size`)
table(socarp$`Activity type`)

Interactions<-c("physS", "none",  "verbS", "physC", "verbC")
Activity_type<- c("sedentary", "games", "locomotion","sports")
Group_size<- c("alone","small","medium" ,"large")
Activity_level<- c("standing", "lying","sitting","veryAct", "walking")

socarp<- socarp[!grepl("Reset", socarp$Readings),]

table(socarp$Interactions)
table(socarp$`Activity type`)
table(socarp$`Group size`)
table(socarp$`Activity level`)

socarp$Interactions<- ifelse(socarp$`Activity type` %in% Interactions, socarp$`Activity type`,socarp$Interactions)
socarp$Interactions<- ifelse(socarp$`Group size` %in% Interactions, socarp$`Group size`,socarp$Interactions)
socarp$Interactions<- ifelse(socarp$`Activity level` %in% Interactions, socarp$`Activity level`,socarp$Interactions)
socarp$`Activity type`<- ifelse(socarp$`Activity type` %in% Interactions, "",socarp$`Activity type`)
socarp$`Group size`    <- ifelse(socarp$`Group size` %in% Interactions, "",socarp$`Group size`)
socarp$`Activity level`<- ifelse(socarp$`Activity level` %in% Interactions, "",socarp$`Activity level`)

socarp$`Activity type`<- ifelse(socarp$`Group size` %in% Activity_type, socarp$`Group size`,socarp$`Activity type`)
socarp$`Activity type`<- ifelse(socarp$`Activity level` %in% Activity_type, socarp$`Activity level`,socarp$`Activity type`)
socarp$`Group size`    <- ifelse(socarp$`Group size` %in% Activity_type, "",socarp$`Group size`)
socarp$`Activity level`<- ifelse(socarp$`Activity level` %in% Activity_type, "",socarp$`Activity level`)


socarp$`Group size`<- ifelse(socarp$`Activity level` %in% Group_size, socarp$`Activity level`,socarp$`Group size`)
socarp$`Activity level`<- ifelse(socarp$`Activity level` %in% Group_size, "",socarp$`Activity level`)


table(socarp$Interactions)
table(socarp$`Activity type`)
table(socarp$`Group size`)
table(socarp$`Activity level`)

###############################################################################################

###############################################################################################
###############################################################################################
##               Merge the SOCARP and survey data for further analysis                       ##
###############################################################################################
###############################################################################################

a<-(merge(socarp,GSYData, by= "Child.ID"))

# Change Gender values back to to "Male" and "Female" for categorical analysis
a <- a %>%
  mutate(Gender = ifelse(Gender == 1, "Male", "Female"))

################################################################################################
## Perform Chi square test on the merged data between SOCARP and survey data to check 
#the significance between two categorical variables of choice. 
################################################################################################

# Chi square test between Group Size and BMI Category
achai<- data.frame(Gender = as.factor(a$`Group size`), Categorize_BMI = as.factor(a$Categorize_BMI))
contingency_table_a <- table(a$`Group size`, a$Categorize_BMI)
print(contingency_table_a)
chi_square_a <- chisq.test(contingency_table_a)
print(chi_square_a )

fisher_test_a <- fisher.test(contingency_table_a, simulate.p.value = TRUE)
print(fisher_test_a)

## Chi square test between Activity level and BMI Category
contingency_table_b <- table(a$`Activity level`, a$Categorize_BMI)
print(contingency_table_b)
chi_square_b <- chisq.test(contingency_table_b)
print(chi_square_b)

fisher_test_b <- fisher.test(contingency_table_b, simulate.p.value = TRUE)
print(fisher_test_b)

## Chi square test between Activity type and BMI Category
contingency_table_c <- table(a$`Activity type`, a$Categorize_BMI)
print(contingency_table_c)
chi_square_c <- chisq.test(contingency_table_c)
print(chi_square_c)

fisher_test_c <- fisher.test(contingency_table_c, simulate.p.value = TRUE)
print(fisher_test_c)

## Chi square test between Interactions and BMI Category
contingency_table_d <- table(a$Interactions, a$Categorize_BMI)
print(contingency_table_d)
chi_square_d <- chisq.test(contingency_table_d)
print(chi_square_d)

fisher_test_d <- fisher.test(contingency_table_d, simulate.p.value = TRUE)
print(fisher_test_d)

##########################
## Chi square test between Activity level and Age
contingency_table_e <- table(a$Interactions, a$Age)
print(contingency_table_e)
chi_square_e <- chisq.test(contingency_table_e)
print(chi_square_e)

fisher_test_e <- fisher.test(contingency_table_e, simulate.p.value = TRUE)
print(fisher_test_e)

## Chi square test between Activity type and Age
contingency_table_g <- table(a$`Activity type`, a$Age)
print(contingency_table_g)
chi_square_g <- chisq.test(contingency_table_g)
print(chi_square_g)

fisher_test_g <- fisher.test(contingency_table_g, simulate.p.value = TRUE)
print(fisher_test_g)


## Chi square test between Interactions and Age
contingency_table_h <- table(a$Interactions, a$Age)
print(contingency_table_h)
chi_square_h <- chisq.test(contingency_table_h)
print(chi_square_h)

fisher_test_h <- fisher.test(contingency_table_h, simulate.p.value = TRUE)
print(fisher_test_h)

##########################
## Chi square test between Activity level and Gender
contingency_table_i <- table(a$`Activity level`, a$Gender)
print(contingency_table_i)
chi_square_i <- chisq.test(contingency_table_i)
print(chi_square_i)

fisher_test_i <- fisher.test(contingency_table_i, simulate.p.value = TRUE)
print(fisher_test_i)

## Chi square test between Group Size and Gender
contingency_table_j <- table(a$`Group size`, a$Gender)
print(contingency_table_j)
chi_square_j <- chisq.test(contingency_table_j)
print(chi_square_j)

fisher_test_j <- fisher.test(contingency_table_j, simulate.p.value = TRUE)
print(fisher_test_j)

## Chi square test between Activity type and Gender
contingency_table_k <- table(a$`Activity type`, a$Gender)
print(contingency_table_k)
chi_square_k <- chisq.test(contingency_table_k)
print(chi_square_k)

fisher_test_k <- fisher.test(contingency_table_k, simulate.p.value = TRUE)
print(fisher_test_k)

## Chi square test between Interactions and Gender
contingency_table_l <- table(a$Interactions, a$Gender)
print(contingency_table_l)
chi_square_l <- chisq.test(contingency_table_l)
print(chi_square_l)

fisher_test_l <- fisher.test(contingency_table_l, simulate.p.value = TRUE)
print(fisher_test_l)

################################################################################
##                     Some data visualizations                               ##
################################################################################

# BMI categories
bmi_categories <- c("Healthy Weight", "Overweight", "Obesity", "Severe Obesity")

# Activity levels
activity_levels <- c("very active", "walking", "sitting", "standing", "lying")

# Create a data frame for the visualization
visualization_data <- data.frame(
  BMI_Category = character(),
  Activity_level = character(),
  Percentage = numeric()
)

# Loop through BMI categories and activity levels
for (bmi_category in bmi_categories) {
  for (activity_level in activity_levels) {
    total_count <- nrow(a)  # Total number of rows in the data frame
    count <- sum(a$Categorize_BMI == bmi_category & a$`Activity level` == activity_level)
    percentage <- (count / total_count) * 100
    
    # Add data to the data frame
    visualization_data <- rbind(visualization_data, data.frame(
      BMI_Category = bmi_category,
      Activity_level = activity_level,
      Percentage = percentage
    ))
  }
}


# Create the barplot
ggplot(visualization_data, aes(x = Activity_level, y = Percentage, fill = BMI_Category)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_text(aes(label = paste0(round(Percentage, 2), "%")),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  geom_text(aes(label = BMI_Category),
            position = position_dodge(width = 0.9), vjust = 1.5, size = 3) +
  labs(title = "BMI Category Distribution by Activity levels",
       x = "Activity level",
       y = "Percentage") +
  scale_fill_manual(values = c("Healthy Weight" = "green", 
                               "Overweight" = "yellow", 
                               "Obesity" = "orange", 
                               "Severe Obesity" = "red")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


#############################################################################################

# Calculate the counts for each BMI category by gender
counts <- table(a$Categorize_BMI, a$Gender)

# Calculate the total counts for each BMI category
total_counts <- rowSums(counts)

# Calculate the percentage for each gender in each BMI category
percentages <- (counts / total_counts) * 100

# Create a data frame for the barplot
barplot_data <- as.data.frame(t(percentages))


# Make sure column names are correct
print(colnames(barplot_data))

# Rename columns
colnames(barplot_data) <- c("Gender", "Categorize_BMI", "Percentage")

# Create the double barplot
ggplot(barplot_data, aes(x = Categorize_BMI, y = Percentage, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_text(aes(label = paste0(round(Percentage, 2), "%")),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "BMI Category Distribution by Gender",
       x = "BMI Category",
       y = "Percentage") +
  scale_fill_manual(values = c("Male" = "skyblue", "Female" = "pink")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

##########################################################################################
