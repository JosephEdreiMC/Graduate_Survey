setwd('C:/Users/j_edr/OneDrive/Escritorio/EstudiosPersonales/BEDU/Proyecto2')
library(dplyr)
library(openxlsx)
library("readxl")
df <- read_excel('career.xlsx')
df <- df[-2876,]

View(df)
colnames(df)
unique(df[,1])

# Combine improved aspects columns
academia <- c()
industry <- c()
government <- c()
non_profit<-c()
medical <- c()

for (i in 1:3252){
  for (j in 2:6){
    if (!(is.na(df[i,j]))){
      academia <-append(academia,df[i,j])
    }
  }
}
for (i in 1:3252){
  for (j in 7:11){
    if (!(is.na(df[i,j]))){
      industry <-append(industry,df[i,j])
    }
  }
}
for (i in 1:3252){
  for (j in 12:16){
    if (!(is.na(df[i,j]))){
      government<-append(government,df[i,j])
    }
  }
}
for (i in 1:3252){
  for (j in 17:21){
    if (!(is.na(df[i,j]))){
      non_profit <-append(non_profit,df[i,j])
    }
  }
}
for (i in 1:3252){
  for (j in 22:26){
    if (!(is.na(df[i,j]))){
      medical <-append(medical,df[i,j])
    }
  }
}

academia <- matrix(academia)
industry <- matrix(industry)
government <- matrix(government)
non_profit <- matrix(non_profit)
medical <- matrix(medical)

df <- cbind(df, academia)
df <- cbind(df, industry)
df <- cbind(df, government)
df <- cbind(df, non_profit)
df <- cbind(df, medical)
df <- subset(df, select=-c(2:26)) # Deleting countries columns
# We'll have to reorder columns at the end

# One hot reasons not to pursue academic research

lapply(df[c(2:14)],unique)
not_academic=c('Reason not pursue academic research: Not interested in research career',
               "Reason not pursue academic research: Don't think I have the skills for academia",
               'Reason not pursue academic research: Too competitive/lack of jobs',
               'Reason not pursue academic research: Prefer research outside academia',
               'Reason not pursue academic research: Too low salary',
               'Reason not pursue academic research: Discouraging funding climate',
               'Reason not pursue academic research: Too much administrative work',
               'Reason not pursue academic research: Too demanding',
               'Reason not pursue academic research: Lack work/life balance',
               'Reason not pursue academic research: Political climate is hostile to academia',
               'Reason not pursue academic research: Other')
# There are two 'I don't enjoy research culture in academia'. So we need to mix them up
dont_enjoy_culture <- numeric(3252)
for (i in 1:3252){
  for (j in c(6,9)){
    if (!(is.na(df[i,j]))){
      dont_enjoy_culture[i] <-1
    }
  }
}
View(dont_enjoy_culture)
dont_enjoy_culture <- matrix(dont_enjoy_culture)
df <- cbind(df, dont_enjoy_culture)
df <- subset(df, select=-c(6,9))

for (i in 2:12){
  df[,i] <- replace(df[,i], is.na(df[,i]), '0')
  
}
for (i in 2:12){
  df[,i] <- replace(df[,i], df[,i]!='0', '1')
}

df[,2:12] <- sapply(df[,2:12], as.numeric)
# Rename the reasons for no academic research
for (i in 2:12){
  colnames(df)[i] <- not_academic[i-1]
}
# 
other_names1=c('How long before permanent position after graduation?',
               'How much likely pursue academic than when started degree?')
for (i in 13:14){
  colnames(df)[i] <- other_names1[i-12]
}
df[,13:14] <- sapply(df[,13:14], as.factor)
df[,1] <- sapply(df[,1], as.factor)

lapply(df[c(15:29)],unique)
career_oport=c('How you learn about career oportunities outside academia: I only pursue academic career oportunities',
               "How you learn about career oportunities outside academia: Instution's workshops",
               'How you learn about career oportunities outside academia: Cold-contacting people in interesting jobs',
               'How you learn about career oportunities outside academia: Family',
               'How you learn about career oportunities outside academia: Peers',
               'How you learn about career oportunities outside academia: Professional societies',
               'How you learn about career oportunities outside academia: Nature Careers',
               'How you learn about career oportunities outside academia: Science publications/jobs boards',
               'How you learn about career oportunities outside academia: Journal related to area of speciality',
               'How you learn about career oportunities outside academia: Online resources',
               'How you learn about career oportunities outside academia: LinkedIn and social media',
               'How you learn about career oportunities outside academia: People in my lab',
               'How you learn about career oportunities outside academia: People in my department',
               'How you learn about career oportunities outside academia: Scientific conferences',
               'How you learn about career oportunities outside academia: Other')
for (i in 15:29){
  colnames(df)[i] <- career_oport[i-14]
}
for (i in 15:29){
  df[,i] <- replace(df[,i], is.na(df[,i]), '0')
  
}
for (i in 15:29){
  df[,i] <- replace(df[,i], df[,i]!='0', '1')
}

df[,15:29] <- sapply(df[,15:29], as.numeric)

# Difficulties columns
lapply(df[c(30:37)],unique)
difficult_names=c('Difficulties for grads in studying country: Learning about career posibilities',
                  'Difficulties for grads in studying country: Finding permanent job after graduation',
                  'Difficulties for grads in studying country: Cost of living',
                  'Difficulties for grads in studying country: Lack of affordable housing',
                  'Difficulties for grads in studying country: Work/life balance',
                  'Difficulties for grads in studying country: Future student debt',
                  'Difficulties for grads in studying country: Living as a international student (language barriers, visa issue)',
                  'Difficulties for grads in studying country: Other')
for (i in 30:37){
  colnames(df)[i] <- difficult_names[i-29]
}
for (i in 30:37){
  df[,i] <- replace(df[,i], is.na(df[,i]), '0')
  
}
for (i in 30:37){
  df[,i] <- replace(df[,i], df[,i]!='0', '1')
}

df[,30:37] <- sapply(df[,30:37], as.numeric)

# Resources to establish a satisfying career
lapply(df[c(38:45)],unique)
df <- subset(df, select=-c(46)) # Column 46 is an open question
resources_names=c('Needed resources to establish a satisfying career: Lower competition for grants',
                  'Needed resources to establish a satisfying career: Mentorship with people in my field/department',
                  'Needed resources to establish a satisfying career: Gender-specific mentorship',
                  'Needed resources to establish a satisfying career: Better info about available career oportunities',
                  'Needed resources to establish a satisfying career: Data on career paths of previous graduates from my program',
                  'Needed resources to establish a satisfying career: More jobs in academia',
                  'Needed resources to establish a satisfying career: Grants while transitioning to permanent positions',
                  'Needed resources to establish a satisfying career: Other')
for (i in 38:45){
  colnames(df)[i] <- resources_names[i-37]
}
colnames(df)[46]<-'Academia'

for (i in 38:45){
  df[,i] <- replace(df[,i], is.na(df[,i]), '0')
  
}
for (i in 38:45){
  df[,i] <- replace(df[,i], df[,i]!='0', '1')
}

df[,38:45] <- sapply(df[,38:45], as.numeric)

# Reorder columns we had to modify
df<- df[,c(1,46:51,2:45)]
colnames(df)
colnames(df)[7] <- "Reason not pursue academic research: Don't enjoy research culture in academia"
improve_=c('How much expect the degree to improve prospects in: Academia',
           'How much expect the degree to improve prospects in: Industry',
           'How much expect the degree to improve prospects in: Government',
           'How much expect the degree to improve prospects in: Non-profit',
           'How much expect the degree to improve prospects in: Medical')
for (i in 2:6){
  colnames(df)[i] <- improve_[i-1]
}
df[,2:7] <- sapply(df[,2:7], as.numeric)
write.xlsx(df, 'career_clean.xlsx')
