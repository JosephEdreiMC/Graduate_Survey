setwd('C:/Users/j_edr/OneDrive/Escritorio/EstudiosPersonales/BEDU/Proyecto2')
library(dplyr)
library(openxlsx)
library("readxl")
df <- read_excel('concerns.xlsx')
colnames(df)
View(df)
sum(is.na(df[,1]))
which(is.na(df[,1]))
df <- df[-2876,]
sum(is.na(df))
unique(df[,1])
# Change column names
new_names=c('Able to save money alongside studies', 'Worried increasing cost of living',
            'Inflation will impact pursue of further studies',
            'Worried increasing student debt', 'Concern: Difficult getting funding/grants',
            'Concern: Not able finish studies in time', 'Concern: Poor relation with supervisor',
            'Concern: Number available faculty research jobs',
            'Concern: High number of continuing temporary contracts as postdocs',
            'Concern: Maintaining work/life balance', 'Concern: Uncertainty value of graduate degree',
            'Concern: Uncertainty career prospects','Concern: Student debt during degree',
            'Concern: Financial worries after degree (housing, children, retirement)',
            'Concern: Political landscape', 'Concern: Impostor syndrome', 'Concern: Mental health',
            'Concern: Poor support of parenting/elder care responsabilities')
for (i in 1:18){
  colnames(df)[i] <- new_names[i]
}
colnames(df)
old_scale=c('Strongly disagree','Somewhat disagree',
            'Somewhat agree', 'Strongly agree', 'Neither agree nor disagree',
            'Not applicable', 'Prefer not to say ')
new_scale=c('1', '2', '4', '5', '3', '0','0')

for (i in 1:18){
  df[df==old_scale[i]] <- new_scale[i]
  
}

# Concerns columns
unique(df[,6])
old_labels=c('5 = Very concerned','1 = Not at all concerned' )
new_labels=c('5','1')
for (i in 1:2){
  df[df==old_labels[i]] <- new_labels[i]
  
}
View(df)
df[,1:18] <- sapply(df[,1:18], as.numeric)
df[df==0] <- NA
View(df)
write.xlsx(df,file='concerns_clean.xlsx')
