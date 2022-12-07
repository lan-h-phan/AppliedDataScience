###Applied Data Science Assignment 1
install.packages("rmarkdown")
#Wickham & Grolemund (2017): Chapters 3 through 6
#Chapter 5
library(nycflights13)
library(tidyverse)
colnames(flights)


#########################
##Exercise 5.2.4
#########################

#P1 Find all flights that

#1. Had an arrival delay of two or more hours
filter(flights, arr_delay >= 120)
#2 Flew to Houston (IAH or HOU)
filter(flights, dest == "IAH" | dest == "HOU")
#3 Were operated by United, American, or Delta
filter(flights, carrier %in% c("UA", "AA", "DL"))
#4 Departed in summer (July, August, and September)
filter(flights, month %in% 7:9)
#5 Arrived more than two hours late, but didn’t leave late
filter(flights, arr_delay > 120 & dep_delay <= 0)
#6 Were delayed by at least an hour, but made up over 30 minutes in flight
filter(flights, dep_delay >= 60 & (dep_delay - arr_delay > 30))
#7 Departed between midnight and 6am (inclusive)
filter(flights, dep_time >= 0000 & dep_time <= 0600)

#P2 Another useful dplyr filtering helper is between(). What does it do? 
#Between indicates the lower and upper cut of the values we are looking for. 
# For example, we can say x is between 7 and 9 if 7<x<9 
# Can you use it to simplify the code needed to answer the previous challenges?
filter(flights, between(month, 7, 9))

#P3 How many flights have a missing dep_time?
summary(flights$dep_time) #8255 missing
# What other variables are missing
filter(flights, is.na(dep_time))
#arrival time is also missing
#What might these rows represent?
#these are flights that did not happen/ have not taken place

#P4 Why is NA ^ 0 not missing? 
NA^0 #because all number ^ 0 equals 1
# Why is NA | TRUE not missing? 
NA | TRUE #this is a "or" statement, so TRUE can be the value regardless of NA
# Why is FALSE & NA not missing? 
FALSE & NA #this is an "and" statement, anything with FALSE will be false
#Can you figure out the general rule? (NA * 0 is a tricky counterexample!)
#The general rule is that if a rule applies to all numbers, it will apply to NA
#The rules for multiplication by 0 is not applied to infinite numbers


#########################
##Exercise 5.3.1
#########################

#P1 How could you use arrange() to sort all missing values to the start? (Hint: use is.na()).
arrange(flights, desc(is.na(dep_time)), dep_time)

#P2 Sort flights to find the most delayed flights
arrange(flights, desc(dep_delay))
#Find the flights that left earliest
arrange(flights, year, month, day, dep_time)

#P3 Sort flights to find the fastest (highest speed) flights.
arrange(flights, desc(distance/air_time))

#P4 Which flights travelled the farthest? Which travelled the shortest?
arrange(flights, desc(distance))
arrange(flights, distance)


#########################
##Exercise 5.4.1
#########################

#P1 Brainstorm as many ways as possible 
# to select dep_time, dep_delay, arr_time, and arr_delay from flights.
select(flights, dep_time, dep_delay, arr_time, arr_delay)
select(flights, any_of(c("dep_time", "dep_delay", "arr_time", "arr_delay")))
select(flights, starts_with("dep_"), starts_with("arr_"))

#P2 What happens if you include the name of a variable multiple times in a select() call?
#Nothing happens and the select() function ignores the duplicate
select(flights, dep_time, dep_time)

#P3 What does the any_of() function do? Why might it be helpful in conjunction with this vector?
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, any_of(vars)) #select all the variables in the vector

#P4 Does the result of running the following code surprise you? 
# How do the select helpers deal with case by default? How can you change that default?
select(flights, contains("TIME"))
#This is not surprising to ignore uppercase character and makes it easier to search
select(flights, contains("TIME", ignore.case = FALSE))

#########################
##Exercise 5.5.2
#########################

#P1 Currently dep_time and sched_dep_time are convenient to look at, 
# but hard to compute with because they’re not really continuous numbers. 
#Convert them to a more convenient representation of number of minutes since midnight.
flights %>%
  transmute(
       dep_time,
       hour1 = dep_time %/% 100,
       minute1 = dep_time %% 100,
       dep_time = hour1*60 + minute1,
       sched_dep_time,
       hour2 = sched_dep_time %/% 100,
       minute2 = sched_dep_time %% 100,
       sched_dep_time = hour2*60 + minute2)

#P2 Compare air_time with arr_time - dep_time. 
#What do you expect to see? What do you see? What do you need to do to fix it?
flights %>%
  transmute(
    dep_time,
    hour1 = dep_time %/% 100,
    minute1 = dep_time %% 100,
    dep_time = hour1*60 + minute1,
    arr_time,
    hour2 = arr_time %/% 100,
    minute2 = arr_time %% 100,
    arr_time = hour2*60 + minute2,
    air_time,
    diff = arr_time - dep_time)
#air_time and arr_time - dep_time is different even though they should be the same
#this could be a change in time zones or entering a new day
  
#P3 Compare dep_time, sched_dep_time, and dep_delay. 
#How would you expect those three numbers to be related?
flights %>%
  transmute(dep_delay,
         dep_time = (dep_time %/% 100 * 60 + dep_time %% 100),
         sched_dep_time = (sched_dep_time %/% 100 * 60 + sched_dep_time %% 100),
         dep_delay_diff = dep_delay - dep_time + sched_dep_time)
#dep_delay should be equals to dep_time - schedule_dep_time; 
# which is what we see with difference being 0
  
#P4 Find the 10 most delayed flights using a ranking function. 
#How do you want to handle ties? Carefully read the documentation for min_rank().
arrange(flights, desc(dep_delay))
flights %>%
  mutate(
    rank = min_rank(desc(dep_delay))
  )


#P5 What does 1:3 + 1:10 return? Why?
1:3 + 1:10
#it returns a vector that contains values that were the sum of the two vectors
# The equation goes: 1+1, 2+2, 3+3, 1+4, 2+5, 3+6, 1+7, 2+8, 3+9, 1+10
#the length is equal to the length of 1:10 (warning message)
  
#P6 What trigonometric functions does R provide?
#we can find out by ?Trig
#there are cos(), sin(), tan(), 
#acos(), asin(), atan(x), atan2(y, x)
#cospi(), sinpi(), tanpi()

#########################
##Exercise 5.6.7
#########################

#P1 Brainstorm at least 5 different ways to assess 
# the typical delay characteristics of a group of flights. 
# Consider the following scenarios:
#A flight is 15 minutes early 50% of the time, and 15 minutes late 50% of the time
#A flight is always 10 mins late
# A fight is 30 minutes early 50% of the time, and 30 minutes late 50% of the time.
# 99% of the time a flight is on time. 1% of the time it’s 2 hours late.
#What is more important: arrival delay or departure delay?

#If thinking from passengers' perspectives, arrival delay is more important as it affects
#their next plans or even the next connecting flights
#This is probability. A flight might be 99% on time, but that 1% long delay can affect people
#more than frequent 10 minutes.

#P2 Come up with another approach that will give you the same output as not_cancelled %>% count(dest)

flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
  group_by(dest) %>%
  summarise(count = n())

# and not_cancelled %>% count(tailnum, wt = distance) (without using count()).
flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay)) %>%
  group_by(tailnum) %>%
  summarise(count = sum(distance))


#P3 Our definition of cancelled flights (is.na(dep_delay) | is.na(arr_delay) ) is slightly suboptimal. 
#Why? Which is the most important column?

#The flights can be redirected or landed at another airport, which might not show up in arr_delay
# We can look at the actual airtime of the flight (arr_time)
  
#P4 Look at the number of cancelled flights per day. 
flights %>%
  mutate(cancel = is.na(arr_time)) %>%
  group_by(year, month, day) %>%
  summarise(count = n(),
            cancelled = sum(cancel)
            ) %>%
  ggplot() +
  geom_point(aes(x = count, y = cancelled))
#More delays seems to be correlated with more flights

#Is there a pattern? Is the proportion of cancelled flights related to the average delay?
flights %>%
  mutate(cancel = is.na(arr_time)) %>%
  group_by(year, month, day) %>%
  summarise(delay = mean(arr_delay, na.rm = TRUE),
            cancelled = sum(cancel)
  ) %>%
  ggplot() +
  geom_point(aes(x = delay, y = cancelled))
#similarly, there are likely more cancelled flights with higher delay rates

#P5 Which carrier has the worst delays? 

flights%>%
  group_by(carrier) %>%
  summarise(delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(desc(delay))
#F9 has the worst delay

#Challenge: can you disentangle the effects of bad airports vs. bad carriers? 
#Why/why not? (Hint: think about flights %>% group_by(carrier, dest) %>% summarise(n()))
flights %>%
  group_by(carrier, dest) %>%
  summarise(delay = mean(arr_delay, na.rm = TRUE),
            count = n()) %>%
  arrange(desc(delay))


#P6 What does the sort argument to count() do. When might you use it?

#the count() argument sort data in the order of observations(n)


#########################
##Exercise 5.7.1
#########################

#P1 Refer back to the lists of useful mutate and filtering functions. 
# Describe how each operation changes when you combine it with grouping.

#the mutate and filtering functions would be apply to an entire variable/ column
#after grouping, they will be applied to the groups we set

#P2 Which plane (tailnum) has the worst on-time record?
flights %>%
  group_by(tailnum) %>%
  filter(!is.na(arr_delay)) %>%
  summarise(delay = mean(arr_delay)) %>%
  arrange(desc(delay))
#N844MH has the worst record
  
#P3 What time of day should you fly if you want to avoid delays as much as possible?
flights %>%
  group_by(hour) %>%
  filter(!is.na(dep_delay)) %>%
  summarise(delay = mean(dep_delay))
#They should leave early in the morning (5-9 a.m.)
  
#P4 For each destination, compute the total minutes of delay. 
flights %>%
  group_by(dest) %>%
  filter(!is.na(dep_delay)) %>%
  summarise(delay = sum(dep_delay))
#For each flight, compute the proportion of the total delay for its destination.
flights %>%
  group_by(dest) %>%
  filter(!is.na(dep_delay)) %>%
  summarise(delay = sum(dep_delay),
            prop = dep_delay/ delay)

#P5 Delays are typically temporally correlated: 
# even once the problem that caused the initial delay has been resolved, 
#later flights are delayed to allow earlier flights to leave. 
#Using lag(), explore how the delay of a flight is related to the delay of the immediately preceding flight.
flights %>%
  group_by(origin) %>%
  filter(!is.na(dep_delay)) %>%
  mutate(lag = lag(dep_delay, default = 0)) %>%
  ggplot() +
  geom_point(aes(x = dep_delay, y = lag))
# the delay of the immediate preceding flight is positively correlated with the delay of the next flight

#P6 Look at each destination. Can you find flights that are suspiciously fast? 
# (i.e. flights that represent a potential data entry error). 
flights %>% 
  filter(!is.na(air_time)) %>%
  select(dest, air_time, carrier, flight, tailnum, distance) %>%
  group_by(dest) %>%
  mutate(speed = distance/ air_time) %>%
  arrange(speed)

# Compute the air time of a flight relative to the shortest flight to that destination. 
#Which flights were most delayed in the air?
flights %>% 
  filter(!is.na(air_time)) %>%
  select(dest, air_time, carrier, flight, tailnum, distance) %>%
  group_by(dest) %>%
  mutate(speed = distance/ air_time) %>%
  arrange(desc(speed))
  
# P7 Find all destinations that are flown by at least two carriers. 
flights %>%
  filter(!is.na(air_time)) %>%
  group_by(dest) %>%
  summarise(count = n_distinct(carrier, na.rm = TRUE)) %>%
  filter(count > 1)

#Use that information to rank the carriers.
flights %>%
  filter(!is.na(air_time)) %>%
  group_by(carrier) %>%
  summarise(count = n_distinct(dest, na.rm = TRUE)) %>%
  filter(count > 1) %>%
  arrange(desc(count))

#P8 For each plane, count the number of flights before the first delay of greater than 1 hour.
flights %>%
  filter(!is.na(air_time)) %>%
  select(tailnum, dep_delay, year, month, day) %>%
  arrange(year, month, day) %>% 
  group_by(tailnum) %>%
  mutate(delay = cumsum(dep_delay >60)) %>%
  summarise(count = sum(delay))

#***Bulut & Dejardins (2019): Chapter 4***#
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

pisa[CNTRYID == "Germany" & ST004D01T == "Yes"]

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


