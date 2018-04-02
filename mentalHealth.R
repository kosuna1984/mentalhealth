#1. Library and data loading

#loading need libraries
library(readr)


#Uploading survey Mental health csv dataset file into r 
mental_set <- read.csv("survey.csv", header = TRUE)
View(mental_set)

#Whats the data row count? 
dim(mental_set)
#[1] 1259   27

#whats the distribution of the data?
head(mental_set)

# Timestamp Age Gender        Country state self_employed family_history treatment work_interfere   no_employees remote_work
# 1 2014-08-27 11:29:31  37 Female  United States    IL          <NA>             No       Yes          Often           6-25          No
# 2 2014-08-27 11:29:37  44      M  United States    IN          <NA>             No        No         Rarely More than 1000          No
# 3 2014-08-27 11:29:44  32   Male         Canada  <NA>          <NA>             No        No         Rarely           6-25          No
# 4 2014-08-27 11:29:46  31   Male United Kingdom  <NA>          <NA>            Yes       Yes          Often         26-100          No
# 5 2014-08-27 11:30:22  31   Male  United States    TX          <NA>             No        No          Never        100-500         Yes
# 6 2014-08-27 11:31:22  33   Male  United States    TN          <NA>            Yes        No      Sometimes           6-25          No
# tech_company   benefits care_options wellness_program  seek_help  anonymity              leave mental_health_consequence
# 1          Yes        Yes     Not sure               No        Yes        Yes      Somewhat easy                        No
# 2           No Don't know           No       Don't know Don't know Don't know         Don't know                     Maybe
# 3          Yes         No           No               No         No Don't know Somewhat difficult                        No
# 4          Yes         No          Yes               No         No         No Somewhat difficult                       Yes
# 5          Yes        Yes           No       Don't know Don't know Don't know         Don't know                        No
# 6          Yes        Yes     Not sure               No Don't know Don't know         Don't know                        No
# phys_health_consequence    coworkers supervisor mental_health_interview phys_health_interview mental_vs_physical obs_consequence comments
# 1                      No Some of them        Yes                      No                 Maybe                Yes              No     <NA>
# 2                      No           No         No                      No                    No         Don't know              No     <NA>
#   3                      No          Yes        Yes                     Yes                   Yes                 No              No     <NA>
#   4                     Yes Some of them         No                   Maybe                 Maybe                 No             Yes     <NA>
#   5                      No Some of them        Yes                     Yes                   Yes         Don't know              No     <NA>
# 6                      No          Yes        Yes                      No                 Maybe         Don't know              No     <NA>


#What types of data do we have?
summary(mental_set)

# Timestamp         Age                 Gender              Country        state     self_employed family_history treatment
# 2014-08-27 12:31:41:   2   Min.   :-1.726e+03   Male   :615   United States :751   CA     :138   No  :1095     No :767        No :622  
# 2014-08-27 12:37:50:   2   1st Qu.: 2.700e+01   male   :206   United Kingdom:185   WA     : 70   Yes : 146     Yes:492        Yes:637  
# 2014-08-27 12:43:28:   2   Median : 3.100e+01   Female :121   Canada        : 72   NY     : 57   NA's:  18                             
#  2014-08-27 12:44:51:   2   Mean   : 7.943e+07   M      :116   Germany       : 45   TN     : 45                                         
#  2014-08-27 12:54:11:   2   3rd Qu.: 3.600e+01   female : 62   Ireland       : 27   TX     : 44                                         
#  2014-08-27 14:22:43:   2   Max.   : 1.000e+11   F      : 38   Netherlands   : 27   (Other):390                                         
#  (Other)            :1247                        (Other):101   (Other)       :152   NA's   :515                                         
# work_interfere         no_employees remote_work tech_company       benefits     care_options   wellness_program      seek_help  
# Never    :213    1-5           :162   No :883     No : 228     Don't know:408   No      :501   Don't know:188     Don't know:363  
#  Often    :144    100-500       :176   Yes:376     Yes:1031     No        :374   Not sure:314   No        :842     No        :646  
#  Rarely   :173    26-100        :289                            Yes       :477   Yes     :444   Yes       :229     Yes       :250  
#  Sometimes:465    500-1000      : 60                                                                                               
#  NA's     :264    6-25          :290                                                                                               
# More than 1000:282                                                                                               
# 
# anonymity                  leave     mental_health_consequence phys_health_consequence        coworkers          supervisor 
# Don't know:819   Don't know        :563   Maybe:477                 Maybe:273               No          :260   No          :393  
# No        : 65   Somewhat difficult:126   No   :490                 No   :925               Some of them:774   Some of them:350  
# Yes       :375   Somewhat easy     :266   Yes  :292                 Yes  : 61               Yes         :225   Yes         :516  
# Very difficult    : 98                                                                                          
# Very easy         :206                                                                                          
# 
# 
# mental_health_interview phys_health_interview  mental_vs_physical obs_consequence
# Maybe: 207              Maybe:557             Don't know:576      No :1075       
#  No   :1008              No   :500             No        :340      Yes: 184       
#  Yes  :  44              Yes  :202             Yes       :343                     


#2. Data cleaning
#Dealing with missing data
#Let's get rid of the variables "Timestamp","comments", "state" since they don't provide a meaningful information for this investigation.

mental_set$Timestamp <- NULL
mental_set$comments <- NULL
mental_set$state <- NULL

#Checking there's no missing data
is.na(mental_set) # It looks like "self_employed" has some missing values
head(mental_set)

# Age Gender        Country self_employed family_history treatment work_interfere   no_employees remote_work tech_company   benefits
# 1  37 Female  United States          <NA>             No       Yes          Often           6-25          No          Yes        Yes
# 2  44      M  United States          <NA>             No        No         Rarely More than 1000          No           No Don't know
# 3  32   Male         Canada          <NA>             No        No         Rarely           6-25          No          Yes         No
# 4  31   Male United Kingdom          <NA>            Yes       Yes          Often         26-100          No          Yes         No
# 5  31   Male  United States          <NA>             No        No          Never        100-500         Yes          Yes        Yes
# 6  33   Male  United States          <NA>            Yes        No      Sometimes           6-25          No          Yes        Yes


