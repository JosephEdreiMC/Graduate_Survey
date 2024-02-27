library(dplyr)
library(openxlsx)
library("readxl")
setwd('/Users/j_edr/OneDrive/Escritorio/EstudiosPersonales/BEDU/Proyecto2')
df <-read_excel('GraduateSurvey_2022.xlsx')
colnames(df)<-df[1,]
df <- df[-1,]
View(df)
colnames(df)
unique(df$"Case completed in Snap Interviewer")
# There are some NAs
sum(is.na(df$`Case completed in Snap Interviewer`))
# There is only one NA in this column, we can simply
# ignore it 
unique(colnames(df))
# We can drop the uninteresting columns
df1 <- df[, c(-1,-2,-162,-222)] 
colnames(df1)
# We can divide the data set into multiple categories (subsets)
# Background dataset
backgroud <- df1[, c(1:34)]
economic <- df1[, c(35:38)]
concerns <- df1[, c(39:56)] # Here, we ignored a Covid factor
enjoyment <- df1[, c(58:86)]
workload <- df1[, c(87:92)]
mental_health <- df1[, c(93:120)]
career <- df1[, c(121:193)]
learnings <- df1[, c(194:218)]

files <- list(backgroud, economic, concerns,
           enjoyment, workload, mental_health,
           career, enjoyment)
write.csv(backgroud, file='backgroud.xlsx')
names<- c('backgroud.xlsx', 'economic.xlsx', 'concerns.xlsx',
           'enjoyment.xlsx', 'workload.xlsx', 'mental_health.xlsx',
           'career.xlsx', 'learnings.xlsx')
setwd('/Users/j_edr/OneDrive/Escritorio/EstudiosPersonales/BEDU/Proyecto2/TryExcels')
write.xlsx(backgroud, path_current)
for (i in 1:8){
  write.xlsx(files[i],file=names[i])
  
}
