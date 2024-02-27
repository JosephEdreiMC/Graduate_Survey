setwd('C:/Users/j_edr/OneDrive/Escritorio/EstudiosPersonales/BEDU/Proyecto2')
library(dplyr)
library(openxlsx)
library("readxl")
df <- read_excel('economic.xlsx')
colnames(df)
View(df)
new_names <- c('Have a job', 'Reason having a job',
               'Expect debt?', 'How much expected debt (dollars)')

for (i in 1:4){
  colnames(df)[i] <- new_names[i]
}
df <- sapply(df, as.factor)
which(is.na(df[,1]))
df <- df[-2876,] # Drop the row that is all NA's
unique(df[,2]) # There are 8 different reasons for
# having a job
unique(df[,4]) # 10 categories of debt
View(df)
sum(is.na(df[,1]))
View(df)
df1 <- df
write.csv(df, file='economics_clean.csv') # NOT WORKING SOMEHOW

