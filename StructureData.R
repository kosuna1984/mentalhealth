# Shree Rai Mijarguttu, Karelys Osuna Esquijarosa, Brandon Harden, Anani Assoutovi 

# Clean Environment:
rm(list = ls())
library(MASS)
library(gdata)
library(foreign)
library(devtools)

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
write.table(Third.Data, "ThirdData.xlxs", row.names =T)
write.table(Fourth.Data, "FourthData.sql", row.names =T)

divide.Data <- function(dataset){
  if(1:length(dataset) >= 3){
    if(dataset[1:length(dataset) %% 2 == 1]){
      
    }
    else if(dataset[1:length(dataset) %% 2 ==0]){
      
    }
    else
      stop("NOT VALID!!!")
  }
}
divide.Data(First.Data)









