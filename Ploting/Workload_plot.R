setwd('C:/Users/j_edr/OneDrive/Escritorio/EstudiosPersonales/BEDU/Proyecto2')
library(dplyr)
library(openxlsx)
library("readxl")
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(e1071)
df <- read_excel('workload_clean.xlsx')
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
write.xlsx(df,'concerns_clean.xlsx')


#####
# Hours per week
hoursweek <- df%>%select(c(1:4,6))
View(hoursweek)
colnames(hoursweek)

View(hoursweek)
hours_years <- hoursweek%>%select(c(1,2,5))%>%table()%>%
  as.data.frame()%>%rename("Degree"=1, "Current years"=2,
                           "Hours a\nweek spend\non degree"=3)
unique(concern1_years[,2])


hours11 <- ggplot(hours_years, aes(x = `Current years`, y = Freq))+
  geom_bar(
    aes(fill = `Hours a\nweek spend\non degree`),position = "dodge",
    stat = "identity", color = "black"
  )+
  facet_wrap(~`Degree`) + 
  ggtitle('Hours a week spend on degree')+fill_palette("rickandmorty")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
hours11

hours_job <- hoursweek%>%select(c(1,3,5))%>%table()%>%
  as.data.frame()%>%rename("Degree"=1, "Have a job"=2,
                           "Hours a\nweek spend\non degree"=3)

hours22 <- ggplot(hours_job, aes(x = `Hours a\nweek spend\non degree`, y = Freq))+
  geom_bar(
    aes(fill = `Have a job`),position = "dodge",
    stat = "identity", color = "black"
  )+
  facet_wrap(~`Degree`) + 
fill_palette("simpsons")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
hours22

hours_region <- hoursweek%>%select(c(1,4,5))%>%table()%>%
  as.data.frame()%>%rename("Degree"=1, "Current region"=2,
                           "Hours a\nweek spend\non degree"=3)

hours33 <- ggplot(hours_region, aes(x = `Current region`, y = Freq))+
  geom_bar(
    aes(fill = `Hours a\nweek spend\non degree`),position = "dodge",
    stat = "identity", color = "black"
  )+
  facet_wrap(~`Degree`) + 
  fill_palette("rickandmorty")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
hours33

ggarrange(ggarrange(hours11, hours22, ncol=2), hours33,nrow = 2)


