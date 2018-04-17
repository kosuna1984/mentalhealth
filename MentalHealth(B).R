library(tidyverse)
library(forcats)
library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)

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




# Import Mental Health data set

# Make sure to either 
# A) Make sure the data set is saved in the same folder as your ipython notebook
# B) Set the file path to match where you saved the survey data set on your system

mental_set <- read.csv("survey.csv")

# View head of data set
head(mental_set)

# Check shape/ count of data set
dim(mental_set)

# View summary statistics
summary(mental_set)

# View data's structure
str(mental_set)


ls.str(mental_set)






################################### Data Cleaning ##########################################

#Let's get rid of the variables "Timestamp","comments", "state" since they don't provide a meaningful information for this investigation.
################ *Add code that visualizes the following features and why we drop them
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


################# Consolidating Gender Titles 

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

# View unique gender levels
unique(mental_set$Gender)


############## Handeling outliers in Age variable

# Age value is very skewed with ages less than 0 and ages greater than 100. 
# If Age is less than 21 set to NA; If Age is greater than 100 set to NA

mental_set <- mental_set %>% mutate(Age = replace(Age, Age < 21, NA))
mental_set <- mental_set %>% mutate(Age = replace(Age, Age > 65, NA))
# summary(mental_set)    #Uncomment to view Age distribution

# Average value for Age is now 32; Reset values < 21 & > 100 = average age(32)

mental_set <- mental_set %>% mutate(Age = replace(Age, Age < 21, 32))
mental_set <- mental_set %>% mutate(Age = replace(Age, Age > 65, 32))
mental_set$Age[is.na(mental_set$Age)] <- 32
# summary(mental_set)    #Uncomment to view Age distribution


# Save mental_set data set as mental_set.orig before transforming into type numeric (i.e. save a copy of origional dataset)
mental_set.orig <- mental_set





############################### Visualizations ########################


# Histogran showing distribution of Age (continuous variable)
hist(mental_set$Age, xlab = "Age", main = paste("Histogram of", "Age"), col = "indianred")

# Plotting discrete variables (bar graphs)
### *Note: Graphs need labeling
ggplot(mental_set, aes(Gender)) + geom_bar()
ggplot(mental_set, aes(Gender, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge")

ggplot(mental_set, aes(self_employed)) + geom_bar()
ggplot(mental_set, aes(self_employed, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge")

ggplot(mental_set, aes(family_history)) + geom_bar()
ggplot(mental_set, aes(family_history, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge")

ggplot(mental_set, aes(work_interfere)) + geom_bar()
ggplot(mental_set, aes(work_interfere, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge")

ggplot(mental_set, aes(no_employees)) + geom_bar()
ggplot(mental_set, aes(no_employees, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge")

ggplot(mental_set, aes(remote_work)) + geom_bar()
ggplot(mental_set, aes(remote_work, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge")

ggplot(mental_set, aes(tech_company)) + geom_bar()
ggplot(mental_set, aes(tech_company, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge")

ggplot(mental_set, aes(benefits)) + geom_bar()
ggplot(mental_set, aes(benefits, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge")

ggplot(mental_set, aes(care_options)) + geom_bar()
ggplot(mental_set, aes(care_options, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge")

ggplot(mental_set, aes(wellness_program)) + geom_bar()
ggplot(mental_set, aes(wellness_program, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge")

ggplot(mental_set, aes(seek_help)) + geom_bar()
ggplot(mental_set, aes(seek_help, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge")

ggplot(mental_set, aes(anonymity)) + geom_bar()
ggplot(mental_set, aes(anonymity, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge")

ggplot(mental_set, aes(leave)) + geom_bar()
ggplot(mental_set, aes(leave, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge")

ggplot(mental_set, aes(mental_health_consequence)) + geom_bar()
ggplot(mental_set, aes(mental_health_consequence, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge")

ggplot(mental_set, aes(phys_health_consequence)) + geom_bar()
ggplot(mental_set, aes(phys_health_consequence, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge")

ggplot(mental_set, aes(coworkers)) + geom_bar()
ggplot(mental_set, aes(coworkers, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge")

ggplot(mental_set, aes(supervisor)) + geom_bar()
ggplot(mental_set, aes(supervisor, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge")

ggplot(mental_set, aes(mental_health_interview)) + geom_bar()
ggplot(mental_set, aes(mental_health_interview, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge")

ggplot(mental_set, aes(phys_health_interview)) + geom_bar()
ggplot(mental_set, aes(phys_health_interview, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge")

ggplot(mental_set, aes(obs_consequence)) + geom_bar()
ggplot(mental_set, aes(obs_consequence, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge")







###########################    Correlation    ########################
# (1) Test correlation of variables as categorical variables before transforming into numerical values

# (1)
#########     Test Correlation *************
install.packages("GoodmanKruskal")
library(GoodmanKruskal)

# Resize plot margins
par(mar = c(5,12,12,2) + 0.1) ## default is c(5,4,4,2) + 0.1

varset1<- c("Gender","self_employed","family_history","treatment","work_interfere","no_employees","remote_work","tech_company","benefits","care_options","wellness_program","seek_help","anonymity","leave","mental_health_consequence","phys_health_consequence","coworkers","supervisor","mental_health_interview","phys_health_interview","mental_vs_physical","obs_consequence")
mentalFrame1<- subset(mental_set, select = varset1)
GKmatrix1<- GKtauDataframe(mentalFrame1)
plot(GKmatrix1, corrColors = "blue")
 
# Reset default plot margin values (unblock code below to activate)
# par(mar = c(5,4,4,2) + 0.1)


# Reference only
####  Compute the correlation matrix ____   ****I got the following code from: http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization







######################## Preparing Data for Modeling ########################
# 1.) Convert factor variables into numeric values
# 2.) Standardize & Scale Data

# (1)
##   Convert factor variables into type numeric for modeling

# Saving dataset levels list
# Converting Gender into a factor
mental_set$Gender <- as.factor(mental_set$Gender)


mental_set_levels <- lapply(mental_set,levels)

#Converting dataset factor variables into numeric
mental_set[, 2:23] <- sapply(mental_set[, 2:23], as.numeric)



### Scale the variables Age and Country --------------- *** Scaling function does not work for me; will revisit and correct ***
# (2)
# Scaling data 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

mental_set$Age <- as.numeric(lapply(mental_set["Age"], normalize))
head(mental_set)







#####----- Neural Net Test -----#####

### (1) Convert treatment variable back into factor type
### (2) Create new data set object that inly captures important variables
### (3) Take a Sample of the data
### (4) Create a Train & Test set
### (5) Find the best values for Size & decay
### (6) Create model and predict


# (1)
mental_set$treatment <- factor(mental_set$treatment,labels(c(0, 1)))
levels(mental_set$treatment) <- c("No", "Yes")

# (2)
data <- data.frame(gender= mental_set$Gender,
                   family_history= mental_set$family_history,
                   work_interfere= mental_set$work_interfere,
                   benefits= mental_set$benefits, 
                   care_options= mental_set$care_options,
                   anonymity= mental_set$anonymity,
                   treatment=mental_set$treatment)


### Split data set into train & test sets
smp_siz = floor(0.75*nrow(mental_set))  # creates a value for dividing the data into train and test. In this case the value is defined as 75% of the number of rows in the dataset
smp_siz  # shows the value of the sample size

# Resample data
set.seed(123)   # set seed to ensure you always have same random numbers generated
# (1)
train_ind = sample(seq_len(nrow(data)),size = smp_siz)  # Randomly identifies therows equal to sample size ( defined in previous instruction) from  all the rows of Smarket dataset and stores the row number in train_ind
# (2)
train_X = mental_set[train_ind,] #creates the training dataset with row numbers stored in train_ind
test_X = mental_set[-train_ind,]  # creates the test dataset excluding the row numbers mentioned in train_ind



# (5)
# Calculation of size and decay parameters
# size: number of intermediate hidden unit
# decay: avoiding overfitting
parameter <- train( treatment ~ . , data=train_X, method="nnet", trace=F)
size <- parameter$bestTune$size
decay <- parameter$bestTune$decay
#parameter$bestTune



# (6)
# Neuronal Network model
model <- nnet(treatment ~ ., size=size, decay=decay, trace=F, data=train_X)

# Prediction. Creating a dataframe with the probabilities
predict <- data.frame(predict(model, test_X), treatment=predict(model,test_X, type="class"))

# Confussion matrix
( mc <- table(predict$treatment,test_X$treatment, dnn = c("Fake","Real")) )

# Accuracy
sum(diag(mc)) / nrow(test_X) * 100 









############################################################ Code below is *Work-in-Progress






############################# Data Partitioning #################################
### (1) Take a Sample of the data
### (2) Create a Train & Test set

### Split data set into train & test sets
smp_siz = floor(0.75*nrow(mental_set))  # creates a value for dividing the data into train and test. In this case the value is defined as 75% of the number of rows in the dataset
smp_siz  # shows the value of the sample size

# Resample data
set.seed(123)   # set seed to ensure you always have same random numbers generated
train_ind = sample(seq_len(nrow(mental_set)),size = smp_siz)  # Randomly identifies therows equal to sample size ( defined in previous instruction) from  all the rows of Smarket dataset and stores the row number in train_ind
train_X = mental_set[train_ind,] #creates the training dataset with row numbers stored in train_ind
test_X = mental_set[-train_ind,]  # creates the test dataset excluding the row numbers mentioned in train_ind

# Create train_y & test_y data sets
train_y <- train_X["treatment"]
test_y <- test_X["treatment"]

# Drop the treatment variable and resave data set as "train_x" & "test_X (independent variables)
train_X <- within(train_X, rm(treatment))
test_X <- within(test_X, rm(treatment))

#Converting dataset factor variables into numeric
mental_set$treatment <- sapply(mental_set$treatment, as.numeric)

# Convert treatment factor into factor and add labels
mental_set$treatment <- factor(mental_set$treatment, levels = c("No", "Yes"), labels(c(0, 1)))
mental_set$treatment <- factor(mental_set$treatment, levels = c(0, 1), labels(c("No", "Yes")))

mental_set <- mental_set.orig





######################-------------- Working Section ----------#####################
# *** This is a work-in-progess section....
# ...I am running a correlation model below but I need additional time to interpret and validate results


mental_set[,1:23] <- sapply(mental_set[,1:23], as.numeric)

# Correlation Matrix - Function{cor}
cor(mental_set[,1:23])

corrplot(cor(mental_set[,1:23]), method = "ellipse")




# Fit Linear Model
library(car)
# Fit full model to a Linear Model
mod.lm <- glm(treatment ~ ., family = "binomial", data = mental_set)
summary(mod.lm)


# Examine model's Residual Plots 

mod.lm.df <- fortify(mod.lm)
ggplot(mod.lm.df, aes(x=.fitted, y=.resid)) +
  geom_point() +
  geom_hline(yintercept=0, linetype=2) +
  labs(x="Fitted Values", y="Residuals")


# Check for Normality
qqPlot(mod.lm$residuals, pch=16)
shapiro.test(mod.lm$residuals)


######################-------------- End ----------#####################

