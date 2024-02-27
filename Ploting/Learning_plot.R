setwd('C:/Users/j_edr/OneDrive/Escritorio/EstudiosPersonales/BEDU/Proyecto2')
library(dplyr)
library(openxlsx)
library("readxl")
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(e1071)
df <- read_excel('learnings_clean.xlsx')
View(df)
colnames(df)
#####
# Further cleanance
df[df=='Dual']<-'Doctorate'
count_countries<-df%>%count(df[,5], sort=TRUE)
count_countries <- count_countries[count_countries$n <10,]

noinclude<-count_countries$countries
for (i in 1:nrow(df)){
  if (df[i,5] %in% noinclude){
    df<-df[-i,]
  }
}
df[df=='More than 7 years'] <- '7 years'
df[df=="Less than a year"] <- '<1 years'

View(df)
write.xlsx(df,'learnings_clean.xlsx')

#####
# What'd you do differently?
different <- df%>%select(1,2,25:29)
colnames(different)
different1 <-different%>%select(1,2,7)%>%table()%>%
  as.data.frame()%>%rename("Degree"=1, "Current years"=2,
                           "Nothing"=3)

different11 <- ggplot(different1, aes(x = `Current years`, y = Freq))+
  geom_bar(
    aes(fill = `Nothing`),
    stat = "identity", color = "black"
  )+
  facet_wrap(~`Degree`) + 
  ggtitle('What would you do differently if starting degree over?\n Nothing')+fill_palette("ucscgb")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), plot.title = element_text(hjust = 0.5))
different11
