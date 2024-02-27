setwd('C:/Users/j_edr/OneDrive/Escritorio/EstudiosPersonales/BEDU/Proyecto2')
library(dplyr)
library(openxlsx)
library("readxl")
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(e1071)
df <- read_excel('concerns_clean.xlsx')
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
# Difficult getting funding
concerns <- df%>%select(c(1,2,4, 10:23))
View(concerns)
colnames(concerns)

View(concern1_years)
concern1_years <- concerns%>%select(c(1,2,4))%>%table()%>%
  as.data.frame()%>%rename("Degree"=1, "Current years"=2,
                           "Difficulty getting\nfunding"=3)
unique(concern1_years[,2])


concern11 <- ggplot(concern1_years, aes(x = `Current years`, y = Freq))+
  geom_bar(
    aes(fill = `Difficulty getting\nfunding`),position = "dodge",
    stat = "identity", color = "black"
  )+
  facet_wrap(~`Degree`) + 
  ggtitle('Concerns per studying year')+fill_palette("simpsons")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
concern11

concern1_country <- concerns%>%select(c(1,3,4))%>%table()%>%
  as.data.frame()%>%rename("Degree"=1, "Current region"=2,
                           "Difficulty getting\nfunding"=3)
View(concern1_country)
colnames(concern1_country)
concern12 <- ggballoonplot(concern1_country, x = concern1_country$`Current region`,
                           y = concern1_country$`Difficulty getting\nfunding`,
                           size = "Freq",
                           fill = "Freq", facet.by = "Degree",
                           ggtheme = theme_bw()) +
  scale_fill_viridis_c(option = "A")+
  labs(title='Difficulty getting funding (1-5 scale)')+
  ylab("Score")
concern12
ggarrange(concern11, concern12,ncol = 2,
          labels=c(1,2))

#####
# Not able finish in time
View(concerns)
colnames(concerns)

View(concern1_years)
concern1_years <- concerns%>%select(c(1,2,5))%>%table()%>%
  as.data.frame()%>%rename("Degree"=1, "Current years"=2,
                           "Not able\nfinish studies\nin time"=3)
unique(concern1_years[,2])


concern11 <- ggplot(concern1_years, aes(x = `Current years`, y = Freq))+
  geom_bar(
    aes(fill = `Not able\nfinish studies\nin time`),position = "dodge",
    stat = "identity", color = "black"
  )+
  facet_wrap(~`Degree`) + 
  ggtitle('Concerns per studying year')+fill_palette("rickandmorty")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
concern11

concern1_country <- concerns%>%select(c(1,3,5))%>%table()%>%
  as.data.frame()%>%rename("Degree"=1, "Current region"=2,
                           "Not able\nfinish studies\nin time"=3)
View(concern1_country)
colnames(concern1_country)
concern12 <- ggballoonplot(concern1_country, x = concern1_country$`Current region`,
                           y = concern1_country$`Not able\nfinish studies\nin time`,
                           size = "Freq",
                           fill = "Freq", facet.by = "Degree",
                           ggtheme = theme_bw()) +
  scale_fill_viridis_c(option = "C")+
  labs(title='Not able to finish studies in time')+
  ylab("Score")
concern12
ggarrange(concern11, concern12,ncol = 2,
          labels=c(1,2))
#####
# Work-life balance
View(concerns)
colnames(concerns)

View(concern1_years)
concern1_years <- concerns%>%select(c(1,2,9))%>%table()%>%
  as.data.frame()%>%rename("Degree"=1, "Current years"=2,
                           "Uncertainty\ncareer\nprospects"=3)
unique(concern1_years[,2])


concern11 <- ggplot(concern1_years, aes(x = `Current years`, y = Freq))+
  geom_bar(
    aes(fill = `Uncertainty\ncareer\nprospects`),position = "dodge",
    stat = "identity", color = "black"
  )+
  facet_wrap(~`Degree`) + 
  ggtitle('Concerns per studying year')+fill_palette("npg")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
concern11

concern1_country <- concerns%>%select(c(1,3,9))%>%table()%>%
  as.data.frame()%>%rename("Degree"=1, "Current region"=2,
                           "Uncertainty\ncareer\nprospects"=3)
View(concern1_country)
colnames(concern1_country)
concern12 <- ggballoonplot(concern1_country, x = concern1_country$`Current region`,
                           y = concern1_country$`Uncertainty\ncareer\nprospects`,
                           size = "Freq",
                           fill = "Freq", facet.by = "Degree",
                           ggtheme = theme_bw()) +
  scale_fill_viridis_c(option = "F")+
  labs(title='Uncertainty about career prospects')+
  ylab("Score")
concern12
ggarrange(concern11, concern12,ncol = 2,
          labels=c(1,2))

#####
# Career prospects
View(concerns)
colnames(concerns)

View(concern1_years)
concern1_years <- concerns%>%select(c(1,2,11))%>%table()%>%
  as.data.frame()%>%rename("Degree"=1, "Current years"=2,
                           "Uncertainty\ncareer\nprospects"=3)
unique(concern1_years[,2])


concern11 <- ggplot(concern1_years, aes(x = `Current years`, y = Freq))+
  geom_bar(
    aes(fill = `Uncertainty\ncareer\nprospects`),position = "dodge",
    stat = "identity", color = "black"
  )+
  facet_wrap(~`Degree`) + 
  ggtitle('Concerns per studying year')+fill_palette("lancet")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
concern11

concern1_country <- concerns%>%select(c(1,3,11))%>%table()%>%
  as.data.frame()%>%rename("Degree"=1, "Current region"=2,
                           "Uncertainty\ncareer\nprospects"=3)
View(concern1_country)
colnames(concern1_country)
concern12 <- ggballoonplot(concern1_country, x = concern1_country$`Current region`,
                           y = concern1_country$`Uncertainty\ncareer\nprospects`,
                           size = "Freq",
                           fill = "Freq", facet.by = "Degree",
                           ggtheme = theme_bw()) +
  scale_fill_viridis_c(option = "E")+
  labs(title='Uncertainty about career prospects')+
  ylab("Score")
concern12
ggarrange(concern11, concern12,ncol = 2,
          labels=c(1,2))
#####
# Student debt
View(concerns)
colnames(concerns)

View(concern1_years)
concern1_years <- concerns%>%select(c(1,2,13))%>%table()%>%
  as.data.frame()%>%rename("Degree"=1, "Current years"=2,
                           "Financial\nworries\nafter\ndegree"=3)
unique(concern1_years[,2])


concern11 <- ggplot(concern1_years, aes(x = `Current years`, y = Freq))+
  geom_bar(
    aes(fill = `Financial\nworries\nafter\ndegree`),position = "dodge",
    stat = "identity", color = "black"
  )+
  facet_wrap(~`Degree`) + 
  ggtitle('Concerns per studying year')+fill_palette("uchicago")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
concern11

concern1_country <- concerns%>%select(c(1,3,13))%>%table()%>%
  as.data.frame()%>%rename("Degree"=1, "Current region"=2,
                           "Financial\nworries\nafter\ndegree"=3)
View(concern1_country)
colnames(concern1_country)
concern12 <- ggballoonplot(concern1_country, x = concern1_country$`Current region`,
                           y = concern1_country$`Financial\nworries\nafter\ndegree`,
                           size = "Freq",
                           fill = "Freq", facet.by = "Degree",
                           ggtheme = theme_bw()) +
  scale_fill_viridis_c(option = "H")+
  labs(title='Financial worries after degree (housing, children, retirement)')+
  ylab("Score")
concern12
ggarrange(concern11, concern12,ncol = 2,
          labels=c(1,2))



#####
# Model
View(concerns)
colnames(concerns)
model <- concerns%>% select(-c(1,3))
View(model)
set.seed(1)

#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(model), replace=TRUE, prob=c(0.7,0.3))
train  <- model[sample, ] %>% as.data.frame()
train1 <- train[,-1]
test   <- model[!sample, ] %>% as.data.frame()
dim(train)
dim(test)
colnames(train)
View(train)
train[is.na(train)] <-0

train[,1] <- sapply(train[,1], as.factor)

tune.rad = tune(svm, `Current years`~., data = train,
               kernel = "linear",
               ranges = list(
                 cost = c(0.1, 1, 10, 100, 1000),
                 gamma = c(0.01, 0.1, 0.5)
               ))
svmfit = svm(`Current years` ~ ., data = train, 
             kernel = "radial", cost = 10, gamma=0.01,scale = FALSE)



