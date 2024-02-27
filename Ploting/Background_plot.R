setwd('C:/Users/j_edr/OneDrive/Escritorio/EstudiosPersonales/BEDU/Proyecto2')
library(dplyr)
library(openxlsx)
library("readxl")
library(ggplot2)
library(tidyverse)
library(ggpubr)
df <- read_excel('background_clean.xlsx')

# nrow(df[df$countries=='Australia' & df$Degree=='Doctorate',])
colnames(df)
View(df)

#####
# Plot Degree-Job-YearsInto
View(df)
df1 <- data.frame(df$Degree,df$`Have a job`, df$`Current years`)
View(df1)
colnames(df1)[2] <-'Have a job' 
conTable =table(df1)
View(conTable)
df1 <- as.data.frame(conTable)

ggplot(df1, aes(x = df1[,3], y = df1[,4]))+
  geom_bar(
    aes(fill = `Have a job`), stat = "identity", color = "white",
    position = position_dodge(0.9)
  )+
  facet_wrap(~df1[,1]) + 
  fill_palette("jco")+scale_x_discrete(guide = guide_axis(angle = 90))+
  xlab("Number of years into degree")+
  ylab("Frequency")
#######
# Plot countries
# Lollipop horizontal
# Create contingency table of interest
df2 <- data.frame(df$Degree,df$`Countries`)
conTable =table(df2)
View(conTable)
df2 <- as.data.frame(conTable)
View(df2)
colnames(df2)
ggplot(df2, aes(x=df2$df.Countries, y=df2$Freq)) +
  geom_segment( aes(x=df2$df.Countries, xend=df2$df.Countries,
                    y=0, yend=df2$Freq), color="skyblue") +
  geom_point( color="coral1", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  facet_wrap(~`df.Degree`)
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )



  
#####
# Reasons to enroll. First percentage.
colnames(df)

df3 <- df%>%select(c(1,3,6))%>%table()%>%
  as.data.frame()
View(df3)
colnames(df3)[3] <- 'Academic\ncareer'
colnames(df3)[2]<- 'Have a job'
colnames(df3)[2]<- 'Have a job'
colnames(df3)[4] <- 'Frequency'

reasons_enroll1 <- ggplot(df3, aes(x = `Have a job`, y = Frequency))+
  geom_bar(
    aes(fill = `Academic\ncareer`), stat = "identity", color = "white",
    position = position_dodge(0.9)
  )+
  facet_wrap(~`Degree`) + 
  ggtitle('Reason for enrolling')+
  fill_palette("npg")

df3 <- df%>%select(c(1,3,7))%>%table()%>%
  as.data.frame()%>%rename("Non-academic\ncareer"=3, 
                           "Have a job"=2, "Frequency"=4)
reasons_enroll2 <- ggplot(df3, aes(x = `Have a job`, y = Frequency))+
  geom_bar(
    aes(fill = `Non-academic\ncareer`), stat = "identity", color = "white"
  )+
  facet_wrap(~`Degree`) + 
  ggtitle('Reason for enrolling')+
  fill_palette("aaas")
df3 <- df%>%select(c(1,3,8))%>%table()%>%
  as.data.frame()%>%rename("No job\nwithout\ndegree"=3, 
                           "Have a job"=2, "Frequency"=4)


reasons_enroll3 <- ggplot(df3, aes(x = `Have a job`, y = Frequency))+
  geom_bar(
    aes(fill = `No job\nwithout\ndegree`),position = "dodge",
    stat = "identity", color = "gray"
  )+
  facet_wrap(~`Degree`) + 
  ggtitle('Reason for enrolling')+fill_palette("aaas")
df3 <- df%>%select(c(1,3,9))%>%table()%>%
  as.data.frame()%>%rename("Personal\ninterest in\nsubject"=3, 
                           "Have a job"=2, "Frequency"=4)

reasons_enroll4 <- ggplot(df3, aes(x = `Have a job`, y = Frequency))+
  geom_bar(
    aes(fill = `Personal\ninterest in\nsubject`), stat = "identity", color = "white"
  )+
  facet_wrap(~`Degree`) + 
  ggtitle('Reason for enrolling')+
  fill_palette("npg")
reasons_enroll4

ggarrange(reasons_enroll1, reasons_enroll2, reasons_enroll3,
          reasons_enroll4, labels=c(1,2,3,4),ncol=2, nrow=2)

for (i in 7:14){
df5 <- df%>%select(c(1,3,i))%>%table()%>%as.data.frame()%>%    
View()  
df4 <- cbind(df4,df5)
}
View(df3)

ggballoonplot(df3,fill="value")


ggplot(df3, aes(x = Hair, y = Freq))+
  geom_bar(
    aes(fill = Eye), stat = "identity", color = "white",
    position = position_dodge(0.9)
  )+
  facet_wrap(~Sex) + 
  fill_palette("dose")
#####
# Studying in native country
colnames(df)
df1 <- df[,c(5,15)]
df1
df1<-df1%>%as.data.frame()%>%
  table()%>%as.data.frame()%>%rename("Studying in a foreign country"=
                                                   2)
View(df1)
native_1<-ggplot(df1, aes(x=Countries, y=Freq)) +
  geom_segment( aes(x=Countries, xend=Countries, y=0, yend=Freq), color="skyblue") +
  geom_point( color="orange", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  facet_wrap(~`Studying in a foreign country`)+
  ggtitle('Studying in your native country')+
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

native_1
df1 <- df[,c(5,15)]
df1
df1<-df1%>%as.data.frame()%>%
  table()%>%as.data.frame()%>%rename("Studying in a foreign country"=
                                       2)
View(df1)
native_2<-ggplot(df1, aes(x=Countries, y=Freq)) +
  geom_segment( aes(x=Countries, xend=Countries, y=0, yend=Freq), color="skyblue") +
  geom_point( color="orange", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  facet_wrap(~`Studying in a foreign country`)+
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )
#####
# Reasons leaving

colnames(df)
# Specific university
df3 <- df%>%select(c(1,4,16))%>%table()%>%
  as.data.frame()%>%rename("Specific\nUniversity"=3, 
                           "Current\nregion"=2, "Frequency"=4)
reason_leave1 <- ggplot(df3, aes(x = `Current\nregion`, y = Frequency))+
  geom_bar(
    aes(fill = `Specific\nUniversity`), stat = "identity", color = "white"
  )+
  facet_wrap(~`Degree`) + 
  ggtitle('Reason for leaving native country')+
  coord_flip()+
  fill_palette("lancet")
reason_leave1

# No quality programs
df3 <- df%>%select(c(1,4,17))%>%table()%>%
  as.data.frame()%>%rename("No quality\ngraduate\nprograms"=3, 
                           "Current\nregion"=2, "Frequency"=4)
reason_leave2 <- ggplot(df3, aes(x = `Current\nregion`, y = Frequency))+
  geom_bar(
    aes(fill = `No quality\ngraduate\nprograms`), stat = "identity", color = "white",
    position = 'dodge'
  )+
  facet_wrap(~`Degree`) + 
  ggtitle('Reason for leaving native country')+
  coord_flip()+
  fill_palette("ucscgb")
reason_leave2

# No funding
df3 <- df%>%select(c(1,4,18))%>%table()%>%
  as.data.frame()%>%rename("No Funding"=3, 
                           "Current\nregion"=2, "Frequency"=4)
reason_leave3 <- ggplot(df3, aes(x = `Current\nregion`, y = Frequency))+
  geom_bar(
    aes(fill = `No Funding`), stat = "identity", color = "white", position='dodge'
  )+
  facet_wrap(~`Degree`) + 
  ggtitle('Reason for leaving native country')+
  coord_flip()+
  fill_palette("lancet")
reason_leave3

# No program specific subject
df3 <- df%>%select(c(1,4,19))%>%table()%>%
  as.data.frame()%>%rename("No program\nspecific\nsubject"=3, 
                           "Current\nregion"=2, "Frequency"=4)
reason_leave4 <- ggplot(df3, aes(x = `Current\nregion`, y = Frequency))+
  geom_bar(
    aes(fill = `No program\nspecific\nsubject`), stat = "identity", color = "white"
  )+
  facet_wrap(~`Degree`) + 
  ggtitle('Reason for leaving native country')+
  coord_flip()+
  fill_palette("ucscgb")
reason_leave4

# Higher salaries
df3 <- df%>%select(c(1,4,21))%>%table()%>%
  as.data.frame()%>%rename("Higher salaries\npost-studies"=3, 
                           "Current\nregion"=2, "Frequency"=4)
reason_leave5 <- ggplot(df3, aes(x = `Current\nregion`, y = Frequency))+
  geom_bar(
    aes(fill = `Higher salaries\npost-studies`), stat = "identity", color = "white"
  )+
  facet_wrap(~`Degree`) + 
  ggtitle('Reason for leaving native country')+
  coord_flip()+
  fill_palette("uchicago")
reason_leave5

# More job opportunities
df3 <- df%>%select(c(1,4,22))%>%table()%>%
  as.data.frame()%>%rename("More job\nopportunities\npost-studies"=3, 
                           "Current\nregion"=2, "Frequency"=4)
reason_leave6 <- ggplot(df3, aes(x = `Current\nregion`, y = Frequency))+
  geom_bar(
    aes(fill = `More job\nopportunities\npost-studies`),
    stat = "identity", color = "white", position='dodge'
  )+
  facet_wrap(~`Degree`) + 
  ggtitle('Reason for leaving native country')+
  coord_flip()+
  fill_palette("simpsons")
reason_leave6

# Cultural experience
df3 <- df%>%select(c(1,4,26))%>%table()%>%
  as.data.frame()%>%rename("Experience\nanother\nculture"=3, 
                           "Current\nregion"=2, "Frequency"=4)
reason_leave7 <- ggplot(df3, aes(x = `Current\nregion`, y = Frequency))+
  geom_bar(
    aes(fill = `Experience\nanother\nculture`),
    stat = "identity", color = "white"
  )+
  facet_wrap(~`Degree`) + 
  ggtitle('Reason for leaving native country')+
  coord_flip()+
  fill_palette("uchicago")
reason_leave7

ggarrange(ggarrange(reason_leave1, reason_leave2,ncol = 2,
                    labels=c(1,2)),
          ggarrange(reason_leave3,reason_leave4,
                    ncol = 2,
                    labels=c(3,4)),
          ggarrange(reason_leave5, reason_leave6,ncol = 2,
                    labels=c(5,6)),
          reason_leave7, nrow=4,labels=7
          )



for (i in 16:28){
  x=sum(df[,i])
  print(paste("For i=",i, "The number of yes is ", x))
}



ggarrange(reasons_enroll1, reasons_enroll2, reasons_enroll3,
          reasons_enroll4, labels=c(1,2,3,4),ncol=2, nrow=2)








