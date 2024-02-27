setwd('C:/Users/j_edr/OneDrive/Escritorio/EstudiosPersonales/BEDU/Proyecto2')
library(dplyr)
library(openxlsx)
library("readxl")
library(ggplot2)
library(tidyverse)
library(ggpubr)
df <- read_excel('economics_clean.xlsx')
View(df)
#####
# Tidying up

df[df=='Dual']<-'Doctorate'
count_countries<-df%>%count(df[,3], sort=TRUE)
count_countries <- count_countries[count_countries$n <10,]

noinclude<-count_countries$countries
for (i in 1:nrow(df)){
  if (df[i,3] %in% noinclude){
    df<-df[-i,]
  }
}
df[df=='More than 7 years'] <- '7 years'
View(df)
write.xlsx(df,'economics_clean.xlsx')

#####
df <- read_excel('economics_clean.xlsx')
View(df)
colnames(df)
df_job <- df %>% filter(`Have a job`=='Yes')
View(df_job)

# Working students per country. Lollipop
df_job1 <- data.frame(df_job$`Reason having a job`,df_job$`Current region`)
conTable =table(df_job1)
View(conTable)
df_job1 <- as.data.frame(conTable)%>%rename("Reason having a job"=1, "Current region"=2)
View(df_job1)
colnames(df_job1)
 ggplot(df_job1, aes(x=`Current region`, y=Freq)) +
  geom_segment( aes(x=`Current region`, xend=`Current region`,
                    y=0, yend=Freq), color="darkolivegreen") +
  geom_point( color="coral1", size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +
  facet_wrap(~`Reason having a job`)+
  theme(
  panel.grid.major.y = element_blank(),
  panel.border = element_blank(),
  axis.ticks.y = element_blank(),plot.title = element_text(hjust = 0.5)
)+
  ggtitle("Reasons for having a job")

#####
 
 # Dot plot
 
colnames(df)
df1 <- data.frame(df$Degree,df$`Current region`, df$`Expect debt?`)%>%table()%>%
as.data.frame()%>%rename("Degree"=1, "Current region"=2, "Expect debt?"=3)
df1 <- df1 %>% filter(df1$`Expect debt?`!="Other", df1$`Expect debt?` != "Prefer not to say")
View(df1)
ggballoonplot(df1, x = df1$`Current region`, y = df1$`Expect debt?`,
              size = "Freq",
              fill = "Freq", facet.by = "Degree",
              ggtheme = theme_bw()) +
  scale_fill_viridis_c(option = "E")+ggtitle("Do you expect debt?")

#####
# How much debt
colnames(df)
unique(df[,8])

howmuch <- data.frame(df$Degree, df$`Current region`, 
                      df$`How much expected debt (dollars)`)%>%table()%>%
  as.data.frame()%>%
  rename("Degree"=1, "Current region"=2, "How much debt USD?"=3)
howmuch <- howmuch %>%filter(howmuch$`How much debt USD?` != "Prefer not to say")
View(howmuch)

howmuchdebt <- ggplot(howmuch, aes(x = `Current region`, y = Freq))+
  geom_bar(
    aes(fill = `How much debt USD?`), stat = "identity", color = "white",
    position='dodge'
  )+
  facet_wrap(~`Degree`) + 
  ggtitle('How much debt do you expect in USD?')+
  coord_flip()+
  fill_palette("uchicago")  +
  theme(plot.title = element_text(hjust = 0.5))
howmuchdebt  
