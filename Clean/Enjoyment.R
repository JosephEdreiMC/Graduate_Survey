setwd('C:/Users/j_edr/OneDrive/Escritorio/EstudiosPersonales/BEDU/Proyecto2')
library(dplyr)
library(openxlsx)
library("readxl")
df <- read_excel('enjoyment.xlsx')
View(df)
colnames(df)
lapply(df[c(1:10)],unique)
new_enjoy=c('Enjoyment: Intellectual challenge',
            'Enjoyment: Working with interesting people',
            'Enjoyment: Social life',
            'Enjoyment: University/academic environment',
            'Enjoyment: Chance to continue with academic research job',
            'Enjoyment: Chance for a non-academic research job',
            'Enjoyment: Chance use skills in non-research science job',
            'Enjoyment: Chance to consider professional options',
            'Enjoyment: Oportunity travel overseas',
            'Enjoyment: Other')
for (i in 1:10){
  colnames(df)[i] <- new_enjoy[i]
}
new_other=c('How satisfied with decision to pursue a graduate degree?',
            "Planing to pursue a PhD after Master's",
            'How satisfied with graduate degree experience?',
            'Since the start of degree, your level of satisfaction has:')
for (i in 11:14){
  colnames(df)[i] <- new_other[i-10]
}
new_satisfy=c('Satisfaction: Availability of funding',
              'Satisfaction: Hours worked',
              'Satisfaction: Social environment',
              'Satisfaction: Degree of independence',
              'Satisfaction: Recognition from supervisor',
              'Satisfaction: Relationship with supervisor',
              'Satisfaction: Compensation and benefits',
              'Satisfaction: Vacation time',
              'Satisfaction: Guidance from adviser in lab/research',
              'Satisfaction: Meetings and conferences',
              'Satisfaction: Work-life balance',
              'Satisfaction: Career pathway guidance and advice',
              'Satisfaction: Quality of teaching',
              'Satisfaction: Balance of teaching and practical elements',
              'Comparison initial expectations')
for (i in 15:29){
  colnames(df)[i] <- new_satisfy[i-14]
}
for (i in 1:10){
  df[,i] <- replace(df[,i], is.na(df[,i]), '0')
  
}
for (i in 1:10){
  df[,i] <- replace(df[,i], df[,i]!='0', '1')
}
which(is.na(df[,11]))
which(is.na(df[,13]))

df <- df[-2876,]
View(df)
unique(df[,12])
# Modify satisfaction numbers
unique(df[,14])
sum(is.na(df[,14]))
df[df=='1 = Not at all satisfied'] <- '1'
df[df=='4 = Neither satisfied nor dissatisfied'] <- '4'
df[df=='7 = Extremely satisfied'] <- '7'
df[df=="4 = Neither satisfied not dissatisfied"]<-'4'

df[df=='Significantly worsened'] <- '1'
df[df=='Stayed the same'] <- '3'
df[df=='Improved slightly'] <- '4'
df[df=='Worsened a little'] <- '2'
df[df=='Improved greatly'] <- '5'
# Not aplicable for NA
for (i in 15:28){
  df[,i] <- replace(df[,i], df[,i]=='Not applicable', NA)
}
unique(df[,29])
df[df=='Does not meet original expectations'] <- '1'
df[df=='Meets original expectations'] <- '2'
df[df=='Exceeds original expectations'] <- '3'

df[,1:11] <- sapply(df[,1:11], as.numeric)
df[,13:29] <- sapply(df[,13:29], as.numeric)
df[,12] <- sapply(df[,12], as.factor)

write.xlsx(df,file='enjoyment_clean.xlsx')

