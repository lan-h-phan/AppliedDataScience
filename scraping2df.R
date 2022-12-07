#Read reddit json files
# search API using https://api.pushshift.io/reddit/search/submission/?q=AllLivesMatter&subreddit=changemyview&size=500&fields=id,author,body,subreddit,score,created_utc&before=1424d&after=1767d
library(jsonlite)
library(dplyr)
library(readxl)
library(tidyverse)
library(data.table)


#####Mass import 2019 files (already did for 2018)
#make a function to import the files
bindfunc <- function(file){
  df <- fromJSON(file)
  df <- do.call(rbind, df)
}
#Get the path of filenames
filenames <- list.files("/Users/lanphan/Desktop/2019 Data", full.names = TRUE)
#Read them in a list
list_data <- lapply(filenames, bindfunc)
#Name the data frame
names(list_data) <- paste('dat2019', seq_along(filenames), sep = '_')
#Create objects in global environment.
list2env(list_data, .GlobalEnv)

#bind them all into one dataset
meta2019 <- bind_rows(mget(ls(pattern = '^dat2019_\\d+$')))

#removing repeated responses  
meta2019 <- distinct(meta2019, id, .keep_all = TRUE)

#convert time from unix to something more interpretable
library(anytime)
meta2019$date <- anydate(meta2019$created_utc)

#save file
write_csv(meta2019, "meta2019.csv")

#Then I'll take the meta file and run the ids in Python to get the posts from Reddit



#####Mass import 2020 files

#Get the path of filenames
filenames2 <- list.files("/Users/lanphan/Desktop/2020 Data", full.names = TRUE)
#Read them in a list
list_data2 <- lapply(filenames2, bindfunc)
#Name the data frame
names(list_data2) <- paste('dat2020', seq_along(filenames2), sep = '_')
#Create objects in global environment.
list2env(list_data2, .GlobalEnv)

#bind them all into one dataset
meta2020 <- bind_rows(mget(ls(pattern = '^dat2020_\\d+$')))

#removing repeated responses  
meta2020 <- distinct(meta2020, id, .keep_all = TRUE)

#convert time from unix to something more interpretable
library(anytime)
meta2020$date <- anydate(meta2020$created_utc)

#save file
write_csv(meta2020, "meta2020.csv")



#####Mass import 2021 files

#Get the path of filenames
filenames3 <- list.files("/Users/lanphan/Desktop/2021 Data", full.names = TRUE)
#Read them in a list
list_data3 <- lapply(filenames3, bindfunc)
#Name the data frame
names(list_data3) <- paste('dat2021', seq_along(filenames3), sep = '_')
#Create objects in global environment.
list2env(list_data3, .GlobalEnv)

#bind them all into one dataset
meta2021 <- bind_rows(mget(ls(pattern = '^dat2021_\\d+$')))

#removing repeated responses  
meta2021 <- distinct(meta2021, id, .keep_all = TRUE)

#convert time from unix to something more interpretable
library(anytime)
meta2021$date <- anydate(meta2021$created_utc)

#save file
write_csv(meta2021, "meta2021.csv")

#after this, goes to Python to retrieve the Reddit content

###################################
#########Data Set with reddit stuffs
###############################


tablefunc <- function(file){
  df <- fread(file, na.strings = "")
}


#Get the path of filenames for 2018 - 2021 comments
filenames21c <- list.files("/Volumes/Lan's 4TB Drive/Big Data Sets/BLMReddit/2021comments", full.names = TRUE)
#Read them in a list of data.table
list_data21c <- lapply(filenames21c, tablefunc)
#Name the data frame
names(list_data21c) <- paste('c21', seq_along(filenames21c), sep = '_')
#Create objects in global environment.
list2env(list_data21c, .GlobalEnv)

#bind them all into one dataset
reddit21c <- rbindlist(mget(ls(pattern = '^c21_\\d+$')), fill = TRUE)

reddit21c <- reddit21c %>%
  filter(!(comment_author == "AutoModerator" | comment_author == "DeltaBot"))%>%
  select(V1:comment_forest)
  

#save file
fwrite(reddit21c, "reddit2021comments.csv")



#Get the path of filenames for 2018-2021 posts


filenames21p <- list.files("/Volumes/Lan's 4TB Drive/Big Data Sets/BLMReddit/2021posts", full.names = TRUE)
#Read them in a list of data.table
list_data21p <- lapply(filenames21p, tablefunc)
#Name the data frame
names(list_data21p) <- paste('p21', seq_along(filenames21p), sep = '_')
#Create objects in global environment.
list2env(list_data21p, .GlobalEnv)

#bind them all into one dataset
reddit21p <- rbindlist(mget(ls(pattern = '^p21_\\d+$')), fill = TRUE)

View(reddit21p)

reddit21p <- reddit21p %>%
  select(V1:subreddit)

fwrite(reddit21p, "reddit2021posts.csv")



##############
#Twitter
###############


#First have to go through terminal for the twarcing process
#after extracting all the IDs
#read Twitter stuffs
twitterdat <- readLines("/Users/lanphan/Desktop/twitterdat/2018/blm1801.jsonl")
twitterdat <- lapply(twitterdat, fromJSON)
twitterdat <- lapply(twitterdat, unlist)
twitterdf <- bind_rows(twitterdat)
#this is taking a million years; my computer is probably dead tired
#maybe it's not worth it





#####################
#US Census Cleaning
#####################


#race census 2020
racecensus <- read_csv("/Users/lanphan/Desktop/Academics/PhD Research/Resonance/BLM/Racecensus.csv")

#turn statenames to a column
racecensus <- racecensus %>%
  pivot_longer(c('Alabama':'Puerto Rico'), names_to = "STATE", values_to = "count")
#labels into column names
racecensus <- racecensus %>%
  pivot_wider(names_from = `Label (Grouping)`, values_from = count)

racecensus <- racecensus %>%
  rename_at(2, ~ "Total") %>%
  rename_at(4, ~ "White") %>%
  rename_at(5, ~ "Black") %>%
  rename_at(6, ~ "Native") %>%
  rename_at(7, ~ "Asian") %>%
  rename_at(8, ~ "Native Hawaiian/PI") %>%
  rename_at(9, ~ "Other") %>%
  rename_at(10, ~ "Inter-racial")

racecensus <- racecensus %>%
  select(1:11)

#save this smaller data set
write_csv(racecensus, "racecensus2020.csv")

#general number count
racecensus <- read_csv("/Users/lanphan/Desktop/Academics/PhD Research/Resonance/BLM/racecensus2020.csv")

censusraw <- read_csv("/Users/lanphan/Desktop/Academics/PhD Research/Resonance/BLM/Census20202021.csv")

censusraw <- censusraw %>%
  rename_at(7, ~ "PopEstimate") %>%
  select(1:7)

censusclean <- censusraw %>%
  inner_join(racecensus,
            by = c("NAME" = "STATE"))

write_csv(censusclean, "censusclean2020.csv")

#################
#Police Violence
#########################

statetranslation <- read_xlsx("/Users/lanphan/Desktop/Academics/PhD Research/GitHub-R-Codes/AppliedDataScience/StateTranslation.xlsx")

policeviolence <- read_csv("/Users/lanphan/Desktop/Academics/PhD Research/Resonance/BLM/Mapping Police Violence-Grid view.csv")

policeviolence <- policeviolence %>%
  select(age, gender, race, date, city, state, county,
         agency_responsible, cause_of_death,
         circumstances, officer_charged, signs_of_mental_illness,
         allegedly_armed, wapo_armed,
         wapo_flee, wapo_body_camera,
         encounter_type, initial_reason,
         call_for_service,
         pop_white_census_tract, pop_black_census_tract,
         pop_hispanic_census_tract) %>%
  separate(date, into = c("month", "day", "year"), sep = "/") %>%
  left_join(statetranslation,
            by = c("state" = "Abbrev")) 

write_csv(policeviolence, "policeviolence2018-2021.csv")

#########################
#Tweet Count Cleaning
#########################

tweetcount <- read_csv("/Users/lanphan/Desktop/Academics/PhD Research/Resonance/BLM/tweet_counts_per_day.csv")

#just tweet counts from 2018-2021
tweetcount <- tweetcount %>%
  separate(date_str, into = c("year", "month", "date"), sep = "-") %>%
  filter(year %in% c("2018", "2019", "2020", "2021"))

write_csv(tweetcount, "blmtweetcount2018-2021.csv")

#########################
#Hate Crime Dat Cleaning
##########################

hatecrime <- read_csv("/Users/lanphan/Desktop/Academics/PhD Research/Resonance/BLM/hate_crime/hate_crime.csv")
hatecrime <- hatecrime %>%
  filter(DATA_YEAR %in% c("2018", "2019", "2020"))

write_csv(hatecrime, "hatecrime2018-2020.csv")





################################
# Demonstrations Count Cleaning
################################

#Probably should sort into for BlackLivesMatter; against BLM (i.e. AllLivesMatter); anti and pro-police/ BlueLivesMatter

# Thank you to this person who cleaned this up
# https://github.com/nonviolent-action-lab/crowd-counting-consortium

c3compiled <- read_csv("/Users/lanphan/Desktop/Academics/PhD Research/Resonance/BLM/ccc_compiled.csv")

blmkeywords <- c("policing", "racism")

blmkeywords <- str_c(blmkeywords, collapse = "|")


c3compiled <- c3compiled %>%
  separate(date, into = c("year", "month", "date"), sep = "-") %>%
  filter(year %in% c("2018", "2019", "2020", "2021", "2022")) %>%
  filter(grepl("policing|racism", issues, ignore.case = TRUE))

write_csv(c3compiled, "c3clean.csv")

bdidat <- read_csv("/Users/lanphan/Desktop/Academics/PhD Research/Resonance/BLM/bridging-organizations-data-2022-11-21.csv")
#probably not going to filter this 
#addressing one aspects might have a ripple effect on others
#cooperative vs competitive (demonstrations are more focused on race)

