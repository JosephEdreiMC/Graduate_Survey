setwd('C:/Users/j_edr/OneDrive/Escritorio/EstudiosPersonales/BEDU/Proyecto2')
library(dplyr)
library(openxlsx)
library("readxl")
# There was a mistake while creating the 8 original excels
df <- read_excel('GraduateSurvey_2022.xlsx')
colnames(df)
View(df)
colnames(df)<-df[1,]
lapply(df[c(1:10)],unique)
learnings <- df[,197:221]
write.xlsx(learnings, file='learnings.xlsx')

# Working with the correct file
df <- read_excel('learnings.xlsx')
colnames(df)
View(df)
df <- df[-1,]
which(is.na(df[,1]))
df <- df[-2876,]


lapply(df[,1],unique)
# How well preparing for
how_well=c('How well is the degree preparing you for: Collecting/analyzing data',
           'How well is the degree preparing you for: Designing experiments',
           'How well is the degree preparing you for: Writing a paper in a journal',
           'How well is the degree preparing you for: Applying for funding',
           'How well is the degree preparing you for: Finding a satisfying career',
           'How well is the degree preparing you for: Managing complex projects',
           'How well is the degree preparing you for: Developing a business plan',
           'How well is the degree preparing you for: Managing people',
           'How well is the degree preparing you for: Managing a large operational budget')
for (i in 1:9){
  colnames(df)[i] <- how_well[i]
}
old_scale=c('Neither well nor badly','Well',
            'Very well', 'Badly', 'Very badly',
            'Not applicable', 'Unsure/Not applicable', 'Prefer not to say')
new_scale=c('3', '4', '5', '2', '1', '0','0','0')
for (i in 1:9){
  df[df==old_scale[i]] <- new_scale[i]
}
df[,1:9] <- sapply(df[,1:9], as.numeric)
df[df==0]<-NA
# 
colnames(df)[10] <- 'How well is the program preparing me for a research career'
colnames(df)[11] <- 'How well is the program preparing me for a non-research science career'
unique(df[,10])
old_scale=c('Somewhat agree','Strongly disagree',
            'Strongly agree', 'Neither agree nor disagree', 'Somewhat disagree',
            'Unsure')
new_scale=c('4', '1', '5', '3', '2', '0')
for (i in 1:6){
  df[df==old_scale[i]] <- new_scale[i]
}
df[,10:11] <- sapply(df[,10:11], as.numeric)

# ACtivities to advance career
lapply(df[c(12:19)],unique)
advance_career=c('Activity done to advance career: Attended career seminars/workshops',
                 'Activity done to advance career: Attended networking events',
                 'Activity done to advance career: Developed social media profile',
                 'Activity done to advance career: Worked out an individualized development plan',
                 'Activity done to advance career: Discussed future career with supervisor',
                 'Activity done to advance career: Discussed future career with mentor',
                 'Activity done to advance career: Discussed future career with career counsellor',
                 'Activity done to advance career: Other')
for (i in 12:19){
  colnames(df)[i] <- advance_career[i-11]
}
for (i in 12:19){
  df[,i] <- replace(df[,i], is.na(df[,i]), '0')
  
}
for (i in 12:19){
  df[,i] <- replace(df[,i], df[,i]!='0', '1')
}

df[,12:19] <- sapply(df[,12:19], as.numeric)

# Do differently
lapply(df[c(20:25)],unique)
differently_=c('What would you do differently if starting degree over? Change area of study',
               'What would you do differently if starting degree over? Change supervisor',
               'What would you do differently if starting degree over? Change university',
               'What would you do differently if starting degree over? Not pursue degree at all',
               'What would you do differently if starting degree over? Nothing',
               'What would you do differently if starting degree over? Other')
for (i in 20:25){
  colnames(df)[i] <- differently_[i-19]
}
for (i in 20:25){
  df[,i] <- replace(df[,i], is.na(df[,i]), '0')
  
}
for (i in 20:25){
  df[,i] <- replace(df[,i], df[,i]!='0', '1')
}

df[,20:25] <- sapply(df[,20:25], as.numeric)
write.xlsx(df, file='learnings_clean.xlsx')


