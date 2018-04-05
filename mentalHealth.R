#1. Library and data loading

#loading need libraries
library(readr)
library(stringr)
library(tidyr)
library(dplyr)


#Uploading survey Mental health csv dataset file into r 
mental_set <- read.csv("survey.csv", header = TRUE)
View(mental_set)

#Whats the data row count? 
dim(mental_set)

#whats the distribution of the data?
head(mental_set)

#What types of data do we have?
summary(mental_set)


#2. Data cleaning
#Dealing with missing data
#Let's get rid of the variables "Timestamp","comments", "state" since they don't provide a meaningful information for this investigation.
mental_set$Timestamp <- NULL
mental_set$comments <- NULL
mental_set$state <- NULL

#Checking there's no missing data
sum(is.na(mental_set)) #[1] 282
is.na(mental_set) # It looks like "self_employed" has some missing values
head(mental_set)
sum(is.na(mental_set$self_employed)) 

#Cleaning NaN in column "self_employed". There are only 0.014% of self employed so let's change NaN to NOT self_employed (No)
mental_set$self_employed[is.na(mental_set$self_employed)] <- "No"
#Double Checking
sum(is.na(mental_set$self_employed))

#Checking what other columns have missing values
apply(mental_set, 2, function(x) any(is.na(x)))

#It looks like it is "work_interfere"
sum(is.na(mental_set$work_interfere))

#There are only 0.20% of self work_interfere so let's change NaN to Don't know (Don't Know)
mental_set$work_interfere <- as.character(mental_set$work_interfere)
mental_set$work_interfere[is.na(mental_set$work_interfere)] <- "Don't Know"
mental_set$work_interfere <- as.factor(mental_set$work_interfere)

##Cleaning "Gender" attribute  
#Lower case all columm's elements in "Gender"
levels(mental_set$Gender) <- tolower(levels(mental_set$Gender))

#Select unique elements
survey_gender <- unique(mental_set$Gender)
View(survey_gender)


#Making gender groups
female_str <- c("cis female", "f", "female", "woman",  "femake", "female ","cis-female/femme", "female (cis)", "femail")
male_str <- c("male", "m", "male-ish", "maile", "mal", "male (cis)", "make", "male ", "man","msle", "mail", "malr","cis man")
trans_str <- c("trans-female", "something kinda male?", "queer/she/they", "non-binary","nah", "all", "enby", "fluid", "genderqueer", "androgyne", "agender", "male leaning androgynous", "guy (-ish) ^_^", "trans woman", "neuter", "female (trans)", "queer", "ostensibly male, unsure what that really means" )


for(i in 1:nrow(mental_set))
  {
  for(k in 1:length(str_detect(mental_set$Gender[i],trans_str)))
    if(str_detect(mental_set$Gender[i],trans_str)[k] == TRUE){
      mental_set$Gender[i] <- "trans"
    }
  
  for(m in 1:length(str_detect(mental_set$Gender[i],male_str)))
    if(str_detect(mental_set$Gender[i],male_str)[m] == TRUE){
      mental_set$Gender[i] <- "male"
    }
  
  for(o in 1:length(str_detect(mental_set$Gender[i],female_str)))
    if(str_detect(mental_set$Gender[i],female_str)[o] == TRUE){
      mental_set$Gender[i] <- "female"
    }
}

mental_set %<>% filter(Gender != "a little about you")
mental_set %<>% filter(Gender != "guy (-ish) ^_^")
mental_set %<>% filter(Gender != "p")










NewGender <- replace(
  male_str, 
  mental_set$Gender, 
  c("M")
)













