######################LIBRARY AND DATA LOADING

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

###################################ENCODING DATA (FACTOR VARIABLES)
#Checking what are factor variables
str(mental_set)
# $ Age                      : num  37 44 32 31 31 33 35 39 42 23 ...
# $ Gender                   : chr  "female" "male" "male" "male" ...

#Converting Gender into a factor
mental_set$Gender <- as.factor(mental_set$Gender)

#Saving dataset levels list
mental_set_levels <- lapply(mental_set,levels)

#Converting dataset factor variables into numeric
mental_set[, 2:24] <- sapply(mental_set[, 2:24], as.numeric)



#################COVARIANCE MATRIX. VARIABILITY COMPARISON BETWEEN CATEGORIES OF VARIABLES
#correlation matrix
corrmat <- cor(mental_set)
corrplot(corrmat, method="color")

# Get rid of Country?

#correlation matrix
# corrmat = train_df.corr()
# f, ax = plt.subplots(figsize=(12, 9))
# sns.heatmap(corrmat, vmax=.8, square=True);
# plt.show()

#"Treatment" variable correlation matrix



































