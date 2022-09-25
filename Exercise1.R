###Applied Data Science Assignment 1

#Wickham & Grolemund (2017): Chapters 3 through 6
#Bulut & Dejardins (2019): Chapter 4
install.packages("data.table")
library(data.table)

#data.table(DT) has a general form
#DT[i, j, by]
#where i = on which row; j = what to do; and by = grouped by what

#########################
##Exercise 4.2.1
#########################

#Import Data Set
#the pisa data set is too large so I will use the region6 data instead
pisa <- fread("~/Desktop/Academics/2022 Fall/Applied Data Science/Data Sets/region6.csv", na.strings = "")
class(pisa)
print(object.size(pisa), unit = "GB")


###########################
##Exercise 4.3.1
###########################

##P1. Subset all the Female students (ST004D01T) in Germany

pisa[CNTRYID == "Germany" & ST004D01T == "Female"]

##P2 How many students are there in Germany? 
pisa[CNTRYID == "Germany" & ST004D01T == "Female", 
     .N]

#P3 The .N function returns the length of a vector/ number of rows. 
#Use chaining with the .N function to answer Exercise 2
pisa[CNTRYID == "Germany" & ST004D01T == "Female"
     ][,.N]

##General Learnings
# Using j we can:
#select columns
#summarize variables by performing actions on the variables
#and create new variables. 
pisa[, 
     .(CNTRYID)] #.() is a shorthand for list()

#######################
#Exercise 4.4.1
########################

#First, create a function to convert dichotomous variable to numeric scoring
# x a character vector containing "Yes" and "No" responses
bin.to.num <- function(x){
  if(is.na(x)) NA
  else if (x == "Yes") 1L
  else if (x == "No") 0L #L is to make sure variable is treated as interger
}

#Then use this function to create some variables as well as recoding gender

pisa[, `:=` (female = ifelse(ST004D01T == "Female", 1, 0),
             sex = ST004D01T, #the := adds the variables directly to the DT
             
             # At my house we have ... 
             desk = sapply(ST011Q01TA, bin.to.num), 
             own.room = sapply(ST011Q02TA, bin.to.num), 
             quiet.study = sapply(ST011Q03TA, bin.to.num), 
             computer = sapply(ST011Q04TA, bin.to.num), 
             software = sapply(ST011Q05TA, bin.to.num),
             internet = sapply(ST011Q06TA, bin.to.num), 
             lit = sapply(ST011Q07TA, bin.to.num), 
             poetry = sapply(ST011Q08TA, bin.to.num), 
             art = sapply(ST011Q09TA, bin.to.num), 
             book.sch = sapply(ST011Q10TA, bin.to.num), 
             tech.book = sapply(ST011Q11TA, bin.to.num), 
             dict = sapply(ST011Q12TA, bin.to.num), 
             art.book = sapply(ST011Q16NA, bin.to.num))]

#Then create new variables by combining preexisiting ones
pisa[, `:=` 
     (math = rowMeans(pisa[, c(paste0("PV", 1:10, "MATH"))], na.rm = TRUE), 
       reading = rowMeans(pisa[, c(paste0("PV", 1:10, "READ"))], na.rm = TRUE), 
       science = rowMeans(pisa[, c(paste0("PV", 1:10, "SCIE"))], na.rm = TRUE))]

#P1 The computer and software variables that were created above 
#ask a student whether they had a computer in their home that they can use for school work (computer) 
#and whether they had educational software in their home (software)
#Find the proportion of students in Germany and Uruguay 
#that have a computer in their home or have educational software.

pisa[CNTRYID %in% c("Germany", "Uruguay"),
     table(computer, software)]

#Adding labels/ make it look prettier
pisa[CNTRYID %in% c("Germany", "Uruguay"),
     .(computer = factor(ST011Q04TA, levels = c("No", "Yes")),
       software = factor(ST011Q05TA, levels = c("No", "Yes")))
][,
  table(computer, software)
]
#The total number of students in Germany and Uruguay that 
# have a computer in their home OR have educational software
# is 101 + 5280 + 4669 = 10050
#proportion = 10050/10795 = .93


#P2 For just female students, 
#find the proportion of students who have their own room (own.room) 
#or a quiet place to study (quiet.study).
pisa[sex == "Female",
     table(own.room, quiet.study)]
#The total number of female students who 
# have their own room OR a quiet place to study 
# is 5828 + 10680 + 51172 = 67680
#proportion = 67680/ 72608 = .93


#######################
#Exercise 4.5.1
#########################
#P1 Calculate the proportion of students who have art in their home (art) 
#and the average age (AGE) of the students by gender.

pisa[,
     .(art = mean(art, na.rm = TRUE),
       AGE = mean(AGE, na.rm = TRUE)),
     by = .(sex)]


#P2 Within a by argument you can discretize a variable to create a grouping variable. 
# Perform a median split for age within the by argument 
# and assess whether there are age difference associated with 
# having your own room (own.room) or a desk (desk).

pisa[,
     .(own.room = mean(own.room, na.rm = TRUE),
       desk = mean(desk, na.rm = TRUE)),
     by =.(age = (AGE < median(AGE)))
     ]
#Older students (above median age) are more likely to have their own room and desks

#######################
#4.8 Lab 
########################
## This afternoon when we discuss supervised learning, 
#we’ll ask you to develop some models to predict the response to the question 
#Do you expect your child will go into a ?” (PA032Q03TA).

#P1 Recode this variable so that a "Yes" is 1 and a "No" is a -1
#save the variable as sci_car

pisa[, 
     "sci_car" := sapply(PA032Q03TA,
                   function(x){
                     if(is.na(x)) NA
                     else if (x == "Yes") 1L
                     else if (x == "No") -1L
                   })
     ][,
       table(sci_car)]

#P2 Calculate descriptives for this variable by sex and country. 
#Specifically, the proportion of test takers whose parents said “Yes” or 1.

pisa[,
     .(sci_car = mean(sci_car, na.rm =TRUE)),
     by = .(sex = sex, country = CNTRYID)]

#After you’ve done this, 
#spend some time investigating the following variables Label
#and then do the following using data.table and/or sparklyr

#P3 Means and standard deviations (sd) for the variables 
#that you think will be most predictive of sci_car

#As this is a response by parents, 
#variables that might be the most predictive of sci_car should be observable to parents
#Student expected occupational status (BSMJ), 
#Out of school study time (OUTHOURS)
#Home Educational Resources (HEDRES)
#Family Wealth (WEALTH) 


pisa[,
     .(ExpectedStatus = mean(BSMJ, na.rm = TRUE),
       OutsideStudy = mean(OUTHOURS, na.rm = TRUE),
       HomeRes = mean(HEDRES, na.rm = TRUE),
       Wealth = mean(WEALTH, na.rm = TRUE))
       ]


#P4 Calculate these same descriptives by groups (by sci_car and by sex)

pisa[,
     .(ExpectedStatus = mean(BSMJ, na.rm = TRUE),
       OutsideStudy = mean(OUTHOURS, na.rm = TRUE),
       HomeRes = mean(HEDRES, na.rm = TRUE),
       Wealth = mean(WEALTH, na.rm = TRUE)),
     by = .(sci_car, sex)
]

#P5 Calculate correlations between these variables and sci_car
pisa[,
      .(ExpectedStatus = cor(sci_car, BSMJ, use = "na.or.complete"),
        OutsideStudy = cor(sci_car, OUTHOURS, use = "na.or.complete"),
        HomeRes = cor(sci_car, HEDRES, use = "na.or.complete"),
        Wealth = cor(sci_car, WEALTH, use = "na.or.complete"))]


#P6 Create new variables
# 6a. Discretize the math and reading variables using the OECD means (490 for math and 493) 
# and code them as 1 (at or above the mean) and -1 (below the mean), 
# but do in the data.table way without using the $ operator.

pisa[, 
     "mathfac" := sapply(math,
                         function(x){
                           if(is.na(x)) NA
                           else if (x >= 490) 1L
                           else if (x < 490) -1L
                         })]

pisa[, 
     "readfac" := sapply(reading,
                         function(x){
                           if(is.na(x)) NA
                           else if (x >= 493) 1L
                           else if (x < 493) -1L
                         })]


#6b. Calculate the correlation between these variables and the list of variables above.
pisa[,
     .(ExpectedStatusM = cor(mathfac, BSMJ, use = "na.or.complete"),
       OutsideStudyM = cor(mathfac, OUTHOURS, use = "na.or.complete"),
       HomeResM = cor(mathfac, HEDRES, use = "na.or.complete"),
       WealthM = cor(mathfac, WEALTH, use = "na.or.complete"),
       ExpectedStatusR = cor(readfac, BSMJ, use = "na.or.complete"),
       OutsideStudyR = cor(readfac, OUTHOURS, use = "na.or.complete"),
       HomeResR = cor(readfac, HEDRES, use = "na.or.complete"),
       WealthR = cor(readfac, WEALTH, use = "na.or.complete"))]


#P7 Chain together a set of operations
#For example, create an intermediate variable that is the average of JOYSCIE and INTBRSCI, 
#and then calculate the mean by country by sci_car through chaining

pisa[,
     "lovescience" := ((JOYSCIE + INTBRSCI)/2)
     ][,
       .(sci_car = mean(sci_car, na.rm = TRUE)),
       by = .(CNTRYID)]

#P8 Transform variables
#specifically recode MISCED and FISCED from characters to numeric variables.
ParentEdu = c('MISCED', 'FISCED')
pisa[,
     (ParentEdu) := lapply(.SD, as.numeric), .SDcols = ParentEdu
     ][,
       .(class(MISCED),
         class(FISCED))]


#P9 Examine other variables in the pisa data set 
# that you think might be predictive of PA032Q03TA.


