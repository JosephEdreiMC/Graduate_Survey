setwd('C:/Users/j_edr/OneDrive/Escritorio/EstudiosPersonales/BEDU/Proyecto2')
library(dplyr)
library(openxlsx)
library("readxl")
library(ggplot2)

df <- read_excel('.xlsx')

# Further filter the data set
unique(df[,1])
df[df=='Dual']<-'Doctorate'
count_countries<-df%>%count(df[,29], sort=TRUE)
count_countries <- count_countries[count_countries$n <10,]

noinclude<-count_countries$countries
for (i in 1:nrow(df)){
  if (df[i,29] %in% noinclude){
    df<-df[-i,]
  }
}
df[df=='More than 7 years'] <- '7 years'