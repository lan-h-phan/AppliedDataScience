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


