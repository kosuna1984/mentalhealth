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



#Let's get rid of the variables "Timestamp","comments", "state" since they don't provide a meaningful information for this investigation.
mental_set$Timestamp <- NULL
mental_set$comments <- NULL
mental_set$state <- NULL



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




#Checking there's no missing data
sum(is.na(mental_set)) #[1] 282
is.na(mental_set) # It looks like "self_employed" has some missing values
head(mental_set)
sum(is.na(mental_set$self_employed)) 

#Cleaning NaN in column "self_employed". There are only 0.014% of self employed so let's change NaN to NOT self_employed (No)
mental_set$self_employed[is.na(mental_set$self_employed)] <- "No"
#Double Checking
sum(is.na(mental_set$self_employed))


#There are only 0.20% of self work_interfere so let's change NaN to Don't know (Don't Know)
mental_set$work_interfere <- as.character(mental_set$work_interfere)
mental_set$work_interfere[is.na(mental_set$work_interfere)] <- "Don't Know"
mental_set$work_interfere <- as.factor(mental_set$work_interfere)


#Checking what other columns have missing values
apply(mental_set, 2, function(x) any(is.na(x)))





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





# Plotting discrete variables (bar graphs)
### *Note: Graphs need labeling

ggplot(mental_set, aes(Gender, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge")
ggplot(mental_set, aes(Country, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge") #This plot's margins need to be adjusted
ggplot(mental_set, aes(tech_company, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge")
ggplot(mental_set, aes(leave, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge")
ggplot(mental_set, aes(benefits, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge")
ggplot(mental_set, aes(anonymity, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge")
ggplot(mental_set, aes(seek_help, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge")
ggplot(mental_set, aes(care_options, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge")

# Save mental_set data set as mental_set.orig before transforming into type numeric
mental_set.orig <- mental_set
# mental_set <-mental_set.orig




###    Preparing data for correlation    ###

# Convert factor variables into type numeric for modeling
#Saving dataset levels list
#Converting Gender into a factor
mental_set$Gender <- as.factor(mental_set$Gender)


mental_set_levels <- lapply(mental_set,levels)

#Converting dataset factor variables into numeric
mental_set[, 2:24] <- sapply(mental_set[, 2:24], as.numeric)






### Compute the correlation matrix ____   ****I got the following code from: http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
# Correlation matrix can be created using the R function cor() :
cormat <- round(cor(mental_set),2)
head(cormat)

###  Create the correlation heatmap with ggplot2
# The package reshape is required to melt the correlation matrix :
library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

# The function geom_tile()[ggplot2 package] is used to visualize the correlation matrix :
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

### Get the lower and upper triangles of the correlation matrix
## Note that, a correlation matrix has redundant information. We’ll use the functions below to set half of it to NA.
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}



### Finished correlation matrix heatmap
## Melt the correlation data and drop the rows with NA values :
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Heatmap
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


### Reorder the correlation matrix
## Helper function to reorder the correlation matrix :
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}


# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)









####____________ Preparing Data for Modeling ___________________________###


### Scale the variables Age and Country

# Scaling data 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

mental_set$Age <- as.numeric(lapply(mental_set["Age"], normalize))
mental_set$Country <- as.numeric(lapply(mental_set["Country"], normalize))
head(mental_set)




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


# Check shape/ count of data set
dim(mental_set)

# View summary statistics
summary(mental_set)

# View data's structure
str(mental_set)



#Let's get rid of the variables "Timestamp","comments", "state" since they don't provide a meaningful information for this investigation.
mental_set$Timestamp <- NULL
mental_set$comments <- NULL
mental_set$state <- NULL



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




#Checking there's no missing data
sum(is.na(mental_set)) #[1] 282
is.na(mental_set) # It looks like "self_employed" has some missing values
head(mental_set)
sum(is.na(mental_set$self_employed)) 

#Cleaning NaN in column "self_employed". There are only 0.014% of self employed so let's change NaN to NOT self_employed (No)
mental_set$self_employed[is.na(mental_set$self_employed)] <- "No"
#Double Checking
sum(is.na(mental_set$self_employed))


#There are only 0.20% of self work_interfere so let's change NaN to Don't know (Don't Know)
mental_set$work_interfere <- as.character(mental_set$work_interfere)
mental_set$work_interfere[is.na(mental_set$work_interfere)] <- "Don't Know"
mental_set$work_interfere <- as.factor(mental_set$work_interfere)


#Checking what other columns have missing values
apply(mental_set, 2, function(x) any(is.na(x)))





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





# Plotting discrete variables (bar graphs)
### *Note: Graphs need labeling

ggplot(mental_set, aes(Gender, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge")
ggplot(mental_set, aes(Country, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge") #This plot's margins need to be adjusted
ggplot(mental_set, aes(tech_company, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge")
ggplot(mental_set, aes(leave, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge")
ggplot(mental_set, aes(benefits, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge")
ggplot(mental_set, aes(anonymity, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge")
ggplot(mental_set, aes(seek_help, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge")
ggplot(mental_set, aes(care_options, ..count..)) + geom_bar(aes(fill = treatment), position = "dodge")

# Save mental_set data set as mental_set.orig before transforming into type numeric
mental_set.orig <- mental_set





###    Preparing data for modeling    ###

# Convert factor variables into type numeric for modeling
#Saving dataset levels list
mental_set_levels <- lapply(mental_set,levels)

#Converting dataset factor variables into numeric
mental_set[, 2:24] <- sapply(mental_set[, 2:24], as.numeric)





# Split data set into train & test sets
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





