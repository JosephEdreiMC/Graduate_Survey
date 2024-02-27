setwd('C:/Users/j_edr/OneDrive/Escritorio/EstudiosPersonales/BEDU/Proyecto2')
library(dplyr)
library(openxlsx)
library("readxl")
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(e1071)
df <- read_excel('career_clean.xlsx')
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
write.xlsx(df,'career_clean.xlsx')



#####
# Reasons unlikely
colnames(df)
unlikely <- df%>%select(c(1,2,4,20))
unlikely_region <- unlikely%>%select(c(1,3,4))%>%table()%>%
  as.data.frame()%>%rename("Degree"=1, "Current region"=2,
                           "Too demanding"=3)

unlikely11 <- ggplot(unlikely_region, aes(x = `Current region`, y = Freq))+
  geom_bar(
    aes(fill = `Too demanding`),
    stat = "identity", color = "black"
  )+
  facet_wrap(~`Degree`) + 
  ggtitle('Reason not pursue academic research: Too demanding')+fill_palette("jco")+
  theme_classic2()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
unlikely11





#####
# How likely pursue academy than starting
starting <- df%>%select(1,2,25)%>%table()%>%
  as.data.frame()%>%rename("Degree"=1, "Current years"=2,
                           "Likelihood\npursue academy\nthan when\nstarting degree"=3)

starting11 <- ggplot(starting, aes(x = `Current years`, y = Freq))+
  geom_bar(
    aes(fill = `Likelihood\npursue academy\nthan when\nstarting degree`),
    stat = "identity", color = "black"
  )+
  facet_wrap(~`Degree`) + 
  ggtitle('How much likely pursue academic than when started degree?')+fill_palette("uchicago")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
starting11
