setwd('C:/Users/j_edr/OneDrive/Escritorio/EstudiosPersonales/BEDU/Proyecto2')
library(dplyr)
library(openxlsx)
library("readxl")
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(e1071)
df <- read_excel('enjoyment_clean.xlsx')
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
write.xlsx(df,'enjoyment_clean.xlsx')





#####
# How satisfied with decision
decision_year <- df%>%select(c(1,2,16)) 
decision_year <- decision_year%>%table()%>%
  as.data.frame()%>%rename("Degree"=1, "Current years"=2,
                           "How satisfied\nwith decision\nof pursuing\na degree"=3)
View(decision_year)

decision1 <- ggplot(decision_year, aes(x = `Current years`, y = Freq))+
  geom_bar(
    aes(fill = `How satisfied\nwith decision\nof pursuing\na degree`),position = "dodge",
    stat = "identity", color = "black"
  )+
  facet_wrap(~`Degree`) + 
  ggtitle('How satisfied with decision to pursue a graduate degree?')+fill_palette("simpsons")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
decision1

decision_region <- df%>%select(c(1,4,16)) 

decision_region <- decision_region%>%table()%>%
  as.data.frame()%>%rename("Degree"=1, "Current region"=2,
                           "How satisfied\nwith decision\nof pursuing\nadegree"=3)
View(decision_region)
decision2 <- ggplot(decision_region, aes(x = `Current region`, y = Freq))+
  geom_bar(
    aes(fill = `How satisfied\nwith decision\nof pursuing\nadegree`),position = "dodge",
    stat = "identity", color = "black"
  )+
  facet_wrap(~`Degree`) + 
  ggtitle('How satisfied with decision to pursue a graduate degree?')+fill_palette("simpsons")+
  theme_light()+
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
decision2

ggarrange(decision1, decision2,ncol = 2,
          labels=c(1,2))


#####
# Level satisfaction since the start
start<-df%>%select(c(1,2,4,19))

start_year <-start%>%select(c(1,2,4)) %>%table()%>%
  as.data.frame()%>%rename("Degree"=1, "Current years"=2,
                           "Since the\nstart of\ndegree, the\n level of\nsatisfaction\nhas:"=3)
View(start_year)

start1 <- ggplot(start_year, aes(x = `Current years`, y = Freq))+
  geom_bar(
    aes(fill = `Since the\nstart of\ndegree, the\n level of\nsatisfaction\nhas:`),position = "dodge",
    stat = "identity", color = "black"
  )+
  facet_wrap(~`Degree`) + 
  ggtitle('Since the start of degree, your level of satisfaction has:')+fill_palette("rickandmorty")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
start1

start_region <- start%>%select(c(1,3,4)) %>%table()%>%
  as.data.frame()%>%rename("Degree"=1, "Current region"=2,
                           "Since the\nstart of\ndegree, the\n level of\nsatisfaction\nhas:"=3)
View(start_region)
start2 <- ggplot(start_region, aes(x = `Current region`, y = Freq))+
  geom_bar(
    aes(fill = `Since the\nstart of\ndegree, the\n level of\nsatisfaction\nhas:`),position = "dodge",
    stat = "identity", color = "black"
  )+
  facet_wrap(~`Degree`) + 
  ggtitle('Since the start of degree, your level of satisfaction has:')+fill_palette("rickandmorty")+
  theme_light()+
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
start2

ggarrange(start1, start2,ncol = 2,
          labels=c(1,2))

#####
# Comparison initial expectations
start<-df%>%select(c(1,2,4,34))

start_year <-start%>%select(c(1,2,4)) %>%table()%>%
  as.data.frame()%>%rename("Degree"=1, "Current years"=2,
                           "Comparison\ninitial\nexpectations"=3)
View(start_year)

start1 <- ggplot(start_year, aes(x = `Current years`, y = Freq))+
  geom_bar(
    aes(fill = `Comparison\ninitial\nexpectations`),position = "dodge",
    stat = "identity", color = "black"
  )+
  facet_wrap(~`Degree`) + 
  ggtitle('Since the start of degree, your level of satisfaction has:')+fill_palette("Dark2")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
start1

start_region <- start%>%select(c(1,3,4)) %>%table()%>%
  as.data.frame()%>%rename("Degree"=1, "Current region"=2,
                           "Comparison\ninitial\nexpectations:"=3)
View(start_region)
start2 <- ggplot(start_region, aes(x = `Current region`, y = Freq))+
  geom_bar(
    aes(fill = `Comparison\ninitial\nexpectations:`),position = "dodge",
    stat = "identity", color = "black"
  )+
  facet_wrap(~`Degree`) + 
  ggtitle('Comparison initial expectations')+fill_palette("Dark2")+
  theme_light()+
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
start2

ggarrange(start1, start2,ncol = 2,
          labels=c(1,2))


#####
# Categories, satisfaction
#####
# Availability of funding
available <- df%>%select(1,2,4,20)
enjoy_years <- available%>%select(c(1,2,4))%>%table()%>%
  as.data.frame()%>%rename("Degree"=1, "Current years"=2,
                           "Availability\nof funding"=3)
unique(concern1_years[,2])


enjoy11 <- ggplot(enjoy_years, aes(x = `Current years`, y = Freq))+
  geom_bar(
    aes(fill = `Availability\nof funding`),position = "dodge",
    stat = "identity", color = "black"
  )+
  facet_wrap(~`Degree`) + 
  ggtitle('Availability of funding')+fill_palette("rickandmorty")+
  theme_light()+
  coord_flip()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
enjoy11

enjoy_countries <- available%>%select(c(1,3,4))%>%table()%>%
  as.data.frame()%>%rename("Degree"=1, "Current region"=2,
                           "Availability\nof funding"=3)

enjoy22 <- ggballoonplot(enjoy_countries, x = enjoy_countries$`Current region`,
                           y = enjoy_countries$`Availability\nof funding`,
                           size = "Freq",
                           fill = "Freq", facet.by = "Degree",
                           ggtheme = theme_bw()) +
  scale_fill_viridis_b(option = "C")+
  labs(title='Satisfactions per studying year')+
  ylab("Score")
enjoy2
ggarrange(enjoy22, enjoy11,ncol = 2,
          labels=c(1,2))
#####
# Hours worked
colnames(df)

available <- df%>%select(1,2,4,33)
enjoy_years <- available%>%select(c(1,2,4))%>%table()%>%
  as.data.frame()%>%rename("Degree"=1, "Current years"=2,
                           "Balance of\nhands-on\nand teaching"=3)
unique(concern1_years[,2])


enjoy11 <- ggplot(enjoy_years, aes(x = `Current years`, y = Freq))+
  geom_bar(
    aes(fill = `Balance of\nhands-on\nand teaching`),position = "dodge",
    stat = "identity", color = "black"
  )+
  facet_wrap(~`Degree`) + 
  ggtitle('Balance of teaching and practical elements')+fill_palette("PRGn")+
  theme_light()+
  coord_flip()
enjoy11

enjoy_countries <- available%>%select(c(1,3,4))%>%table()%>%
  as.data.frame()%>%rename("Degree"=1, "Current region"=2,
                           "Balance of\nhands-on\nand teaching"=3)

enjoy22 <- ggballoonplot(enjoy_countries, x = enjoy_countries$`Current region`,
                         y = enjoy_countries$`Balance of\nhands-on\nand teaching`,
                         size = "Freq",
                         fill = "Freq", facet.by = "Degree",
                         ggtheme = theme_bw()) +
  scale_fill_viridis_b(option = "C")+
  labs(title='Satisfaction level per region')+
  ylab("Score")
enjoy2
ggarrange(enjoy22, enjoy11,ncol = 2,
          labels=c(1,2))

