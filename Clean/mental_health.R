setwd('C:/Users/j_edr/OneDrive/Escritorio/EstudiosPersonales/BEDU/Proyecto2')
library(dplyr)
library(openxlsx)
library("readxl")
df <- read_excel('mental_health.xlsx')
df <- df[-2876,]
colnames(df)
View(df)
new_names=c('Have you ever received help for anxiety/depression linked to studies?',
            'Sought help for anxiety/depression within your institution?',
            "My university's mental health services are appropiate for graduates",
            'My supervisor knows about support services and signposted me to them if needed',
            'My university has adequate one-to-one mental health support',
            'My university promotes mental health and wellbeing',
            'My university supports good work-life balance',
            'There is a long-hours culture at my university')
for (i in 1:8){
  colnames(df)[i] <- new_names[i]
}
bullying_names=c('Have you experienced bullying during degree?',
                 'Perpetrator: Supervisor',
                 'Perpetrator: Another student',
                 'Perpetrator: Postdoc',
                 'Perpetrator: Other academic staff member',
                 'Perpetrator: Online troll',
                 'Perpetrator: Other',
                 'Perpetrator: Prefer not to say')
lapply(df[c(9:16)],unique)

for (i in 9:16){
  colnames(df)[i] <- bullying_names[i-8]
}
lapply(df[c(19:28)],unique)
names_2=c('Feel free to talk about bullying without repercussions?',
          'Experienced discrimination/harassment during degree?',
          'Experienced: Racial discrimination/harassment',
          "Experienced: Sexual harassment",
          "Experienced: Age discrimination",
          "Experienced: Gender discrimination",
          "Experienced: LGBTQ+ discrimination/harassment",
          "Experienced: Religious discrimination",
          "Experienced: Disability discrimination",
          "Experienced: Discrimination for parent/carer responsibilities",
          "Experienced: Other",
          "Experienced: Prefer not to say"
          )
for (i in 17:28){
  colnames(df)[i] <- names_2[i-16]
}
# Do the one-hot encoding
for (i in 10:16){
  df[,i] <- replace(df[,i], is.na(df[,i]), '0')
  
}
for (i in 10:16){
  df[,i] <- replace(df[,i], df[,i]!='0', '1')
}

for (i in 19:28){
  df[,i] <- replace(df[,i], is.na(df[,i]), '0')
  
}
for (i in 19:28){
  df[,i] <- replace(df[,i], df[,i]!='0', '1')
}
df[,19:28] <- sapply(df[,19:28], as.numeric)
old_scale=c('Strongly disagree','Somewhat disagree',
            'Somewhat agree', 'Strongly agree', 'Neither agree nor disagree',
            'Not applicable', 'Unsure/Not applicable', 'Prefer not to say')
new_scale=c('1', '2', '4', '5', '3', '0','0','0')
for (i in 1:8){
  df[df==old_scale[i]] <- new_scale[i]
}
colnames(df)
df <-df[,-29]
for (i in 19:28){
  df[,i] <- replace(df[,i], is.na(df[,i]), '0')
  
}
df[df=='0'] <- NA

unique(df[,1])

# Change data type per column
df[,1:2] <- sapply(df[,1:2], as.factor)
df[,3:8] <- sapply(df[,3:8], as.numeric)
df[,9] <- sapply(df[,9], as.factor)
df[,10:16] <- sapply(df[,10:16], as.numeric)
df[,17:18] <- sapply(df[,17:18], as.factor)
df[,19:28] <- sapply(df[,19:28], as.numeric)

write.xlsx(df,file='mental_health_clean.xlsx')

