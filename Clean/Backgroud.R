setwd('C:/Users/j_edr/OneDrive/Escritorio/EstudiosPersonales/BEDU/Proyecto2')
library(dplyr)
library(openxlsx)
library("readxl")
df <- read_excel('background.xlsx')
typeof(df)
write.xlsx(df,file='Try.xlsx')
colnames(df)
View(df)
# Searching for NA's
# Language
unique(df[,1])
sum(is.na(df$`Language ~ ? ~ Idioma ~ Langue ~ Língua`))
which(is.na(df[,1]))
df <- df[-2876,]
colnames(df)[1]<-'Language'
df[df=='Chinese (Simplified, China)'] <- 'Chinese'
unique(df[,2])
df[df=='Doctorate degree (PhD/DPhil/MD)'] <- 'Doctorate'
df[df=="Master's degree (MA/MS/MSc/PSM or other Master’s)"] <- 'Masters'
df[df=="Dual doctorate degree (MD-PhD, PhD-PhD or other combination)"] <- 'Dual'

colnames(df)[2:4]<-c('Degree', 'Duration', 'Current years')
unique(df[,3]) # 9 different categories
sum(df[,3]=='Other') # 15 people answered 'other' in duration
unique(df[,4]) # 9 different categories

###### Change the name of Reasons to enroll columns
lapply(df[c(5:13)],unique)
new_names=c('Reason to enroll: Academic career','Reason to enroll: Non-academic career',
            'Reason to enroll: No job without degree', 'Reason to enroll: Personal interest in subject',
            'Reason to enroll: Continue research',
            'Reason to enroll: Live another country','Reason to enroll: Received scholarship',
            'Reason to enroll: Sponsored by business','Reason to enroll: Other')
for (i in 5:13){
  colnames(df)[i] <- new_names[i-4]
}
# Change the content of Reasons to enroll columns to 0/1, also change type
# to factor
df[df=='I want to pursue an academic career'] <- "1"

for (i in 5:13){
  df[,i] <- replace(df[,i], is.na(df[,i]), '0')
  
}
for (i in 5:13){
  df[,i] <- replace(df[,i], df[,i]!='0', '1')
}

df[,5:13] <- sapply(df[,5:13], as.numeric)
str(df[,5])
View(df)
# Studying abroad
colnames(df)
colnames(df)[14]<- 'Studying in foreign country?'
df[,14] <- sapply(df[,14], as.factor)

# Reasons left country of origin
lapply(df[c(15:27)],unique)
new_names=c('Reason left country: Specific University','Reason left country: No quality graduate programs',
            'Reason left country: No funding', 'Reason left country: No program in specific subject',
            'Reason left country: Pursue specific research',
            'Reason left country: Higher salaries post-study','Reason left country: More job oportunities post-study',
            'Reason left country: Shorter program of study','Reason left country: Lower cost of living',
            'Reason left country: Family reasons','Reason left country: Experience another culture',
            'Reason left country: Political reasons', 'Reason left country: Other')
for (i in 15:27){
  colnames(df)[i] <- new_names[i-14]
}
for (i in 15:27){
  df[,i] <- replace(df[,i], is.na(df[,i]), '0')
  
}
for (i in 15:27){
  df[,i] <- replace(df[,i], df[,i]!='0', '1')
}

df[,15:27] <- sapply(df[,15:27], as.numeric)
View(df)
colnames(df)[28] <- 'Current region'
colnames(df)
View(df)

# Join the country columns into one.
nrow(df)
colnames(df)
for (i in 29:34){
  df[,i] <- replace(df[,i], is.na(df[,i]), '0')
  
}
countries <- c()
for (i in 1:3252){
  for (j in 29:34){
    if (df[i,j] != '0'){
      countries <-append(countries,df[i,j])
    }
    
  }
}
write.xlsx(df,file='background_noCountries.xlsx')

countries <- matrix(countries)
typeof(countries)
df_new <- cbind(df, countries)
View(df_new)
df_new <- subset(df_new, select=-c(29:34)) # Deleting countries columns

# PROBLEM ARISES WHEN ADDING COUNTRIES COLUMN

df$'Country' <- countries
View(df)
colnames(df)
colnames(df)
typeof(df)
df <- as.data.frame(df)
View(df)
write.xlsx(df_new,file='background_clean.xlsx')

