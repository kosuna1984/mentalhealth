######################LIBRARY AND DATA LOADING
rm(list = ls())
# Function: installing and loading of packages
install_load <- function (packages)  {   
  
  # Start loop to determine if each package is installed
  for(package in packages){
    
    # If package is installed locally, load
    if(package %in% rownames(installed.packages()))
      do.call('library', list(package))
    
    # If package is not installed locally, download, then load
    else {
      install.packages(package, dependencies = TRUE)
      do.call("library", list(package))
    }
  } 
}

# Generic libraries loading
libs <- c("ggplot2", "maps", "plotly", "plyr", "dplyr", "rworldmap","stringr","lubridate", "plotly", "reshape2", "magrittr", "ggthemes", "tidyr", "DT", "lubridate","RColorBrewer","Hmisc")
install_load(libs)

# Specific methods libraries loading
libs.methods <- c("C50", "lattice", "caret", "nnet", "e1071","Matrix", "foreach","glmnet","C50","randomForest","ipred","rpart")
install_load(libs.methods)


#Uploading survey Mental health csv dataset file into r 
mental_set <- read.csv("survey.csv")
View(mental_set)

#############################ANALYZING DATA
#Whats the data row count? 
dim(mental_set)

#whats the distribution of the data?
head(mental_set)

#What types of data do we have?
summary(mental_set)


############################DATA CLEANING
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
#Select unique elements
# survey_gender <- unique(mental_set$Gender)
# View(survey_gender)


# Gender unification.
mental_set$Gender %<>% str_to_lower()

male_str <- c("male", "m", "male-ish", "maile", "mal", "male (cis)", "make", "male ", "man","msle", "mail", "malr","cis man", "cis male")
trans_str <- c("trans-female", "something kinda male?", "queer/she/they", "non-binary","nah", "all", "enby", "fluid", "genderqueer", "androgyne", "agender", "male leaning androgynous", "guy (-ish) ^_^", "trans woman", "neuter", "female (trans)", "queer", "ostensibly male, unsure what that really means" )
female_str <- c("cis female", "f", "female", "woman",  "femake", "female ","cis-female/femme", "female (cis)", "femail")

mental_set$Gender <- sapply(as.vector(mental_set$Gender), function(x) if(x %in% male_str) "male" else x )
mental_set$Gender <- sapply(as.vector(mental_set$Gender), function(x) if(x %in% female_str) "female" else x )
mental_set$Gender <- sapply(as.vector(mental_set$Gender), function(x) if(x %in% trans_str) "trans" else x )
mental_set %<>% filter(Gender != "a little about you")
mental_set %<>% filter(Gender != "guy (-ish) ^_^")
mental_set %<>% filter(Gender != "p")


unique(mental_set$Gender)
# [1] "female" "male"   "trans"



#complete missing age with mean
# Age value is very skewed with ages less than 0 and ages greater than 100. 
# If Age is less than 21 set to NA; If Age is greater than 100 set to NA
mental_set <- mental_set %>% mutate(Age = replace(Age, Age < 21, NA))
mental_set <- mental_set %>% mutate(Age = replace(Age, Age > 65, NA))
summary(mental_set)    #Age Mean   :32.28

# Average value for Age is now 32; Reset values < 21 & > 100 = average age(32)
mental_set <- mental_set %>% mutate(Age = replace(Age, Age < 21, 32))
mental_set <- mental_set %>% mutate(Age = replace(Age, Age > 65, 32))
mental_set$Age[is.na(mental_set$Age)] <- 32
summary(mental_set)

###################################ENCODING FACTOR VARIABLES
#Checking what are factor variables
str(mental_set)
# $ Age                      : num  37 44 32 31 31 33 35 39 42 23 ...
# $ Gender                   : chr  "female" "male" "male" "male" ...

#Converting Gender into a factor
mental_set$Gender <- as.factor(mental_set$Gender)
iconv(mental_set$Gender, from ="", to="UTF-8")


#################COVARIANCE MATRIX. VARIABILITY COMPARISON BETWEEN CATEGORIES OF VARIABLES
#correlation matrix
#mental_set is a data frame 
mental_set[, 2:24] <- sapply(mental_set[, 2:24], as.numeric)
rcorr(as.matrix(mental_set),type ="pearson")



###################### LIBRARY AND DATA LOADING

# Function: installing and loading of packages
install_load <- function (packages)  {   
  
  # Start loop to determine if each package is installed
  for(package in packages){
    
    # If package is installed locally, load
    if(package %in% rownames(installed.packages()))
      do.call('library', list(package))
    
    # If package is not installed locally, download, then load
    else {
      install.packages(package, dependencies = TRUE)
      do.call("library", list(package))
    }
  } 
}

# Generic libraries loading
libs <- c("ggplot2", "maps", "plotly", "plyr", "dplyr", "rworldmap","stringr","lubridate", "plotly", "reshape2", "magrittr", "ggthemes", "tidyr", "DT", "lubridate","RColorBrewer")
install_load(libs)

# Specific methods libraries loading
libs.methods <- c("C50", "lattice", "caret", "nnet", "e1071","Matrix", "foreach","glmnet","C50","randomForest","ipred","rpart")
install_load(libs.methods)


#Uploading survey Mental health csv dataset file into r 
mental_set <- read.csv("survey.csv")
View(mental_set)

############################# ANALYZING DATA
#Whats the data row count? 
dim(mental_set)
#[1] 1259   27

#whats the distribution of the data?
head(mental_set)
#Add this result to document

#What types of data do we have?
summary(mental_set)
#Add this result to document


############################ DATA CLEANING
#Dealing with missing data
#Let's get rid of the variables "Timestamp","comments", "state" since they don't provide a meaningful information for this investigation.
#We should add some code to see this data why is not important for this investigation
mental_set$Timestamp <- NULL
mental_set$comments <- NULL
mental_set$state <- NULL
mental_set$Country <- NULL

#Checking there's no missing data
sum(is.na(mental_set)) #[1] 282
is.na(mental_set) # It looks like "self_employed" has some missing values
head(mental_set)
sum(is.na(mental_set$self_employed)) 

#Cleaning NaN in column "self_employed". There are only 0.014% of self employed so let's change NaN to NOT self_employed (No)
#Calculating that 0.014 % to prove that
mental_set$self_employed[is.na(mental_set$self_employed)] <- "No"
#Double Checking
sum(is.na(mental_set$self_employed))

#Checking what other columns have missing values
apply(mental_set, 2, function(x) any(is.na(x)))

#It looks like it is "work_interfere"
sum(is.na(mental_set$work_interfere))

#Let's change NaN to Don't know (Don't Know) since this column refere to:
#If you have a mental health condition, do you feel that it interferes with your work? 
#If value is NaN, then we assume the individual left that value in blanck because he doesn't have a mental condition 
mental_set$work_interfere <- as.character(mental_set$work_interfere)
mental_set$work_interfere[is.na(mental_set$work_interfere)] <- "Don't Know"
mental_set$work_interfere <- as.factor(mental_set$work_interfere)

##Cleaning "Gender" attribute  
#Select unique elements
# survey_gender <- unique(mental_set$Gender)
# View(survey_gender)


# Gender unification.
mental_set$Gender %<>% str_to_lower()

male_str <- c("male", "m", "male-ish", "maile", "mal", "male (cis)", "make", "male ", "man","msle", "mail", "malr","cis man", "cis male")
trans_str <- c("trans-female", "something kinda male?", "queer/she/they", "non-binary","nah", "all", "enby", "fluid", "genderqueer", "androgyne", "agender", "male leaning androgynous", "guy (-ish) ^_^", "trans woman", "neuter", "female (trans)", "queer", "ostensibly male, unsure what that really means" )
female_str <- c("cis female", "f", "female", "woman",  "femake", "female ","cis-female/femme", "female (cis)", "femail")

mental_set$Gender <- sapply(as.vector(mental_set$Gender), function(x) if(x %in% male_str) "male" else x )
mental_set$Gender <- sapply(as.vector(mental_set$Gender), function(x) if(x %in% female_str) "female" else x )
mental_set$Gender <- sapply(as.vector(mental_set$Gender), function(x) if(x %in% trans_str) "trans" else x )
mental_set %<>% filter(Gender != "a little about you")
mental_set %<>% filter(Gender != "guy (-ish) ^_^")
mental_set %<>% filter(Gender != "p")


unique(mental_set$Gender)
# [1] "female" "male"   "trans"


#complete missing age with mean
# Age value is very skewed with ages less than 0 and ages greater than 100. 
# If Age is less than 21 set to NA; If Age is greater than 100 set to NA
mental_set <- mental_set %>% mutate(Age = replace(Age, Age < 21, NA))
mental_set <- mental_set %>% mutate(Age = replace(Age, Age > 65, NA))
summary(mental_set)    #Age Mean   :32.28

# Average value for Age is now 32; Reset values < 21 & > 100 = average age(32)
mental_set <- mental_set %>% mutate(Age = replace(Age, Age < 21, 32))
mental_set <- mental_set %>% mutate(Age = replace(Age, Age > 65, 32))
mental_set$Age[is.na(mental_set$Age)] <- 32
summary(mental_set)

#Checking what are factor variables
str(mental_set)
# $ Age                      : num  37 44 32 31 31 33 35 39 42 23 ...
# $ Gender                   : chr  "female" "male" "male" "male" ...

#Converting Gender into a factor (character now) to convert dataset factor variables into numeric next
mental_set$Gender <- as.factor(mental_set$Gender)


################# VARIABILITY COMPARISON BETWEEN CATEGORIES OF VARIABLES
#Effect size (strength of association)
library(GoodmanKruskal)
varset1<- c("Gender","self_employed","family_history","treatment","work_interfere","no_employees","remote_work","tech_company","benefits","care_options","wellness_program","seek_help","anonymity","leave","mental_health_consequence","phys_health_consequence","coworkers","supervisor","mental_health_interview","phys_health_interview","mental_vs_physical","obs_consequence")
mentalFrame1<- subset(mental_set, select = varset1)
GKmatrix1<- GKtauDataframe(mentalFrame1)
plot(GKmatrix1, colors = "blue")

### Importance variables: 
#treatment(target), work_interfere, family_history, care_options, benefits, obs_consequence, anonymity, mental_health_interview, wellness_program, seek_help


########################### SOME CHARTS TO SEE DATA RELATIONSHIP
# a <- ggplot(mental_set, aes(x = Age))
# a + geom_histogram(aes(y = ..density..), 
#                    colour="black", fill="white") +
#   geom_density(alpha = 0.2, fill = "#FF6666")
# 
# 
# 
# #Initial data visualization
# #Is there a relationship between treatment (target variable) and gender?
# ggplot(mental_set, aes(Gender, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge")
# #We can notice from this graph that we have a higher number of males, and from males there have been more males not treated than treated
# 
# #Histogram with density plot by Age
# a <- ggplot(mental_set, aes(x = Age))
# a + geom_histogram(aes(y = ..density..), 
#                    colour="black", fill="white") +
#   geom_density(alpha = 0.2, fill = "#FF6666")
# 
# #Distribution and Density by Age Separate by treatment or not
# # # Color by groups
# # a + geom_histogram(aes(y = ..density.., color = treatment), 
# #                    fill = "white",
# #                    position = "identity")+
# #   geom_density(aes(color = treatment), size = 1) +
# #   scale_color_manual(values = c("#868686FF", "#EFC000FF"))



########################### SCALING AND SPLITTING 
#Normalizing or scaling "Age" variable, because is extremely different from the other ones (Min-Max Normalization)
#(X - min(X))/(max(X) - min(X))
#Above could be programmed as the following function in R:
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

mental_set$Age <- as.data.frame(lapply(mental_set["Age"], normalize))
head(mental_set)

#Saving dataset levels list for future reference
mental_set_levels <- lapply(mental_set,levels)

#Converting dataset factor variables into numeric
mental_set[, 2:23] <- sapply(mental_set[, 2:23], as.numeric)

# Selection of variables with higher variability
lastMental<- data.frame(gender= mental_set$Gender,
                   family_history= mental_set$family_history,
                   work_interfere= mental_set$work_interfere,
                   benefits= mental_set$benefits, 
                   care_options= mental_set$care_options,
                   anonymity= mental_set$anonymity,
                   treatment=mental_set$treatment)

# Preparing regression function for the use in other methods
regresion <- treatment~
  gender+
  family_history+
  work_interfere+
  benefits+
  care_options+
  anonymity

# Saving prediction percentage of each method
percent <- data.frame(methods=c("Trees Classifier", "Neuronal Network","Randon Forest","Bagging"), value=c(0,0,0,0))

# Data training and testing
set.seed(101)
n <- nrow(mental_set)
data.index <- sample(1:n , size=round(n*0.7))
train <- mental_set[data.index,]
test <- mental_set[-data.index,]


