setwd('C:/Users/j_edr/OneDrive/Escritorio/EstudiosPersonales/BEDU/Proyecto2')
library(dplyr)
library(openxlsx)
library("readxl")
df <- read_excel('workload.xlsx')
colnames(df)
View(df)
df <- df[-2876,]
new_names=c('Hours a week spend on degree',
            'One-on-one contact with supervisor each week',
            'My supervisor: Converse with me about career',
            'My supervisor: Is open to the idea of me pursuing a career outside academia',
            'My supervisor: Has useful advice for careers outside academia',
            'My supervisor: Encourages me to attend career training and events')
for (i in 1:6){
  colnames(df)[i] <- new_names[i]
}
unique(df[,3])
df[6,3]
old_scale=c('Strongly disagree','Somewhat disagree',
            'Somewhat agree', 'Strongly agree', 'Neither agree nor disagree',
            'Not applicable', 'Unsure/Not applicable')
new_scale=c('1', '2', '4', '5', '3', '0','0')

for (i in 3:6){
  df[df==old_scale[i]] <- new_scale[i]
}

df[df=='Strongly disagree'] <- '1'
df[df=='Somewhat disagree'] <- '2'
df[df=='Unsure/Not applicable'] <-NA
unique(df[,2])
View(df)
df[,3:6] <- sapply(df[,3:6], as.numeric)
df[,1:2] <- sapply(df[,1:2], as.factor)
write.xlsx(df,file='workload_clean.xlsx')
