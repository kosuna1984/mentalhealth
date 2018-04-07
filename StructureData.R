# Shree Rai Mijarguttu, Karelys Osuna Esquijarosa, Brandon Harden, Anani Assoutovi 

# Clean Environment:
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
libs <- c("ggplot2", "maps", "MASS", "gdata", "foreign", "devtools")
install_load(libs)

# Specific methods libraries loading
libs.methods <- c("C50", "lattice", "caret", "nnet", "e1071","Matrix", "foreach","glmnet","C50","randomForest","ipred","rpart")
install_load(libs.methods)

get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else {
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  return(tolower(os))
}
setWorkDir <- function(){
  if(get_os() == "windows"){
    setWork <- setwd("C:/Users/aassoutovi/Documents/RStudioFiles/CaseStudies")
  }
  else if(get_os() == "osx"){
    setWork <- setwd("~/Desktop/PRACTICUM_II/")
  }
  else{
    print("I DO NOT RECOGNIZED YOUR OS!!!")
  }
}
setWorkDir()
getwd()

# Real Data
actual.Data <- read.csv("survey.csv", header =T, sep =",")

First.Data <- actual.Data[,1:7]
Second.Data <- actual.Data[,7:13]
Third.Data <- actual.Data[,13:19]
Fourth.Data <- actual.Data[,19:27]

write.csv(First.Data, "FirstData.csv", row.names =T)
write.table(Second.Data, "SecondData.txt", row.names =T)
write.table(Third.Data, "ThirdData.xlx", row.names =T)
write.table(Fourth.Data, "FourthData.sql", row.names =T)

divide.Data <- function(dataset){
  if(!is.null(1:length(dataset))){
    if(dataset[1:length(dataset) %% 2 == 1]){
      
    }
    else if(dataset[1:length(dataset) %% 2 ==0]){
      
    }
    else
      stop("NOT VALID!!!")
  }
}
divide.Data(First.Data)





