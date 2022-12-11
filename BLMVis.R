library(dplyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(data.table)



###################
#State translation
##################
censusdat <- read_csv("/Users/lanphan/Desktop/Academics/PhD Research/GitHub-R-Codes/AppliedDataScience/censusclean2020.csv")
statetranslation <- read_xlsx("/Users/lanphan/Desktop/Academics/PhD Research/GitHub-R-Codes/AppliedDataScience/StateTranslation.xlsx")


statetranslation <- statetranslation %>%
  left_join(censusdat,
            by = c("Name" = "NAME")) %>%
  select(Name, Abbrev, DIVISION, REGION, STATE)

write_csv(statetranslation, "StateTranslation.csv")
####################
#Choropleth graphs
######################


statetranslation <- read_xlsx("/Users/lanphan/Desktop/Academics/PhD Research/GitHub-R-Codes/AppliedDataScience/StateTranslation.xlsx")
ccc2021 <- read_csv("/Users/lanphan/Desktop/Academics/PhD Research/GitHub-R-Codes/AppliedDataScience/ccc2021.csv")
ccc2021$state <- toupper(ccc2021$state) #make sure all abbreviations are uppercase

bdidat <- read_csv("/Users/lanphan/Desktop/Academics/PhD Research/Resonance/BLM/bridging-organizations-data-2022-11-21.csv")


#count the number of demonstrations
mappingdat <- bdidat %>%
  group_by(state) %>%
  summarize(BDIcount = n())

#adding both full state and abbreviations
mappingdat <- mappingdat %>%
  right_join(statetranslation,
             by = c("state" = "Name"))


#census data to find ratio of population vs. number of demonstrations

censusdat <- read_csv("/Users/lanphan/Desktop/Academics/PhD Research/GitHub-R-Codes/AppliedDataScience/censusclean2020.csv")

ratiodat <- mappingdat %>%
  inner_join(censusdat,
             by = c("state" = "NAME"))

ratiodat <- ratiodat %>%
  mutate(ratio = (bdiCount/PopEstimate)*100)


library(leaflet)
library(sf)

usmap <- read_sf("/Users/lanphan/Desktop/Academics/PhD Research/GitHub-R-Codes/AppliedDataScience/gz_2010_us_040_00_5m.json")


combined_data <- ratiodat %>%
  inner_join(usmap,
             by = c("state" = "NAME"))

#turn back into a sf object for geom_sf to map out data
combined_data <- st_as_sf(combined_data)


### Just number of demonstrations
combined_data %>% 
  ggplot(aes(fill = BDIcount)) + # create a ggplot object and 
  # change its fill colour according to median_age
  geom_sf(colour = NA) +# plot all local authority geometries 
  scale_fill_gradient(low = "#82A3FF", 
                      high = "#8B2E2E", na.value = "white") +
  coord_sf(xlim = c(-180, -60), lims_method = "box") + #just zoom in the U.S.
  #labs(title = "U.S. Demonstration Counts, 2018") + #the title is kinda ugly ngl
  theme_void() #remove background


#### By ratio of the population
combined_data %>% 
  ggplot(aes(fill = ratio2021)) + # create a ggplot object and 
  # change its fill colour according to median_age
  geom_sf(colour = NA) +# plot all local authority geometries 
  scale_fill_gradient(low = "#82A3FF", 
                      high = "#8B2E2E", na.value = "white") +
  coord_sf(xlim = c(-180, -60), lims_method = "box") + #just zoom in the U.S.
  theme_void() #remove background

#finally omg
#so beautiful imma cry
#also work with plotly
#Thank you to this person: https://rdvark.net/2021/12/29/pretty-choropleth-maps-with-sf-and-ggplot2/


##Cleaned Police Data
policeviolence <- read_csv("/Users/lanphan/Desktop/Academics/PhD Research/GitHub-R-Codes/AppliedDataScience/policeviolence2018-2021.csv")

tweetcount <- read_csv("/Users/lanphan/Desktop/Academics/PhD Research/GitHub-R-Codes/AppliedDataScience/blmtweetcount2018-2021.csv")

hatecrime <- read_csv("/Users/lanphan/Desktop/Academics/PhD Research/GitHub-R-Codes/AppliedDataScience/hatecrime2018-2020.csv")


comments2021 <- fread("/Volumes/Lan's 4TB Drive/Big Data Sets/BLMReddit/reddit2021comments.csv", na.string = "")

posts2021 <- fread("/Volumes/Lan's 4TB Drive/Big Data Sets/BLMReddit/reddit2021posts.csv", na.string = "")


###############################################
#Changes in Demonstrations by Year and Regions
################################################

#https://r-graph-gallery.com/web-time-series-and-facetting.html

library(geofacet)
library(ggh4x)

#Prep Data

#a list fo division names
divisions <- data.frame(codenum = c(1,2,3,4,5,6,7,8,9),
                        divName = c("New England", "Middle Atlantic", "East North Central",
                                    "West North Central", "South Atlantic",
                                    "East South Central",
                                    "West South Central",
                                    "Mountain", "Pacific"))


statetranslation <- read_csv("/Users/lanphan/Desktop/Academics/PhD Research/GitHub-R-Codes/AppliedDataScience/StateTranslation.csv")
ccc2021 <- read_csv("/Users/lanphan/Desktop/Academics/PhD Research/GitHub-R-Codes/AppliedDataScience/ccc2021.csv")
ccc2021$state <- toupper(ccc2021$state) #make sure all abbreviations are uppercase
ccc2021s <- ccc2021 %>%
  mutate(StateTerritory = state,
         type = valence) %>%
  separate(date, into = c("year", "month", "date"), sep = "-") %>%
  select(year, month, StateTerritory, type)

ccc2018 <- read_csv("/Users/lanphan/Desktop/Academics/PhD Research/GitHub-R-Codes/AppliedDataScience/ccc2018.csv")
ccc2018$StateTerritory <- toupper(ccc2018$StateTerritory)
ccc2018s <- ccc2018 %>%
  separate(Date, into = c("year", "month", "date"), sep = "-") %>%
  mutate(type = `Pro(2)/Anti(1)`) %>%
  select(year, month, StateTerritory, type)

ccc2019 <- read_csv("/Users/lanphan/Desktop/Academics/PhD Research/GitHub-R-Codes/AppliedDataScience/ccc2019.csv")
ccc2019$StateTerritory <- toupper(ccc2019$StateTerritory)
ccc2019s <- ccc2019 %>%
  separate(Date, into = c("year", "month", "date"), sep = "-") %>%
  mutate(type = `Pro(2)/Anti(1)`) %>%
  select(year, month, StateTerritory, type)

ccc2020 <- read_csv("/Users/lanphan/Desktop/Academics/PhD Research/GitHub-R-Codes/AppliedDataScience/ccc2020.csv")
ccc2020$StateTerritory <- toupper(ccc2020$StateTerritory)
ccc2020s <- ccc2020 %>%
  separate(Date, into = c("year", "month", "date"), sep = "-") %>%
  mutate(type = `Pro(2)/Anti(1)`) %>%
  select(year, month, StateTerritory, type)


c3alls <- rbind(ccc2018s, ccc2019s, ccc2020s, ccc2021s)
c3alls$type[is.na(c3alls$type)] <- 1
c3alls <- c3alls %>%
  mutate(type = if_else(type %in% c(0,1), 1, 2)) %>% #code if the protest is for or against BLM
  left_join(statetranslation,
            by = c("StateTerritory" = "Abbrev")) %>%
  count(DIVISION, year, type) %>%
  filter(!is.na(DIVISION))


#pivot it wider so that each year is a column to calculate percentage change


c3alls <- c3alls %>%
  pivot_wider(names_from = year, values_from = `n`)
c3alls <- c3alls[-19, -c(7,8)]

c3alls[is.na(c3alls)] <- 1

c3alls$DIVISION <- as.numeric(c3alls$DIVISION)

c3alls <- c3alls %>%
  mutate(c2018 = 1,
         c2019 = ((`2019` - `2018`)/`2018`),
         c2020 = ((`2020` - `2019`)/`2019`),
         c2021 = ((`2021` - `2020`)/`2020`)) %>%
  pivot_longer(c(`c2018`, `c2019`, `c2020`, `c2021`), names_to = "year", values_to = "change_percent")%>%
  select(DIVISION, year, type, change_percent) %>%
  pivot_wider(names_from = type, values_from = change_percent) %>%
  left_join(divisions,
            by = c("DIVISION" = "codenum"))
  
c3alls$divName <- as.factor(c3alls$divName)
#now I have to pivot it longer again to bring the year back into the same column


test <- c3alls %>%
  filter(DIVISION == 1)

ggplot(test, aes(x = year, group = 1)) +
  geom_line(aes(y = `1`, color = "forBLM")) +
  geom_line(aes(y = `2`, color = "against")) +
  stat_difference(aes(ymin = `1`, ymax = `2`), alpha = .3) +
  scale_y_continuous(labels = scales::percent) 
  

c3alls %>%
  ggplot(aes(x = year, group = 1)) +
  geom_line(aes(y = `1`, color = "forBLM")) +
  geom_line(aes(y = `2`, color = "against")) +
  stat_difference(aes(ymin = `1`, ymax = `2`), alpha = .3) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(c3alls$divName)






#############
###By state
############

c3allstate <- read_csv("/Users/lanphan/Desktop/Academics/PhD Research/GitHub-R-Codes/AppliedDataScience/c3clean.csv")
c3allstate <- c3allstate %>%
  mutate(type = if_else(valence %in% c(0,1), 1, 2)) %>% #code if the protest is for or against BLM
  count(state, year, type) %>%
  left_join(statetranslation,
            by = c("state" = "Abbrev")) %>%
  filter(!is.na(state))

c3allstate$Name <- as.factor(c3allstate$Name)

c3allstate <- c3allstate %>%
  filter(year %in% c(2018,2019,2020,2021,2022)) %>%
  pivot_wider(names_from = type, values_from = n)

c3allstate$`2`[is.na(c3allstate$`2`)] <- 1



###Font Settings
library(showtext)
font_add_google("Fira Sans", family = "firasans")
font_add_google("Lato", family = "lato")

font_add_google("Fira Sans", family = "firasans")
font_add_google("Lato", family = "lato")

#police violence data
policeviolence <- read_csv("/Users/lanphan/Desktop/Academics/PhD Research/GitHub-R-Codes/AppliedDataScience/policeviolence2018-2021.csv")

policeviolence <- policeviolence %>%
  group_by(Name, year) %>%
  summarise(pvcount = n())

c3allstate <- c3allstate %>%
  left_join(policeviolence, by = c("Name" = "Name",
                                   "year" = "year")) %>%
  filter(year %in% c(2018,2019,2020,2021)) 


stategeofacet <- c3allstate %>%
  ggplot(aes(x = year, group = 1)) +
  geom_line(aes(y = `2`, color = "counter-protest")) +
  geom_line(aes(y = `1`, color = "for BLM")) +
  geom_line(aes(y = `pvcount`, color = "police violence")) +
  #stat_difference(aes(ymin = `1`, ymax = `2`), alpha = .3) +
  facet_geo(vars(state), grid = "us_state_grid1")

stategeofacet <- stategeofacet +
  # Colors for the lines
  scale_color_manual(values = c("#C32E5A", "#3D85F7", "grey60")) +
  #scale_fill_manual(
  # values = c(
  #  colorspace::lighten("#C32E5A"), 
  #  colorspace::lighten("#3D85F7"),
  #  "grey60"),
  # labels = c("more BLM", "more counter-protest")
  #) +
  # Add labels
  labs(
    title = "Demonstrations for and against \n #BlackLivesMatter and Police Violence \nfrom 2018-2021",
    caption = "Source: CrowdCountingConsortium"
  ) +
  # Specify the order for the guides
  guides(
    # Order indicates the order of each legend among multiple guides.
    # The guide for 'color' will be placed before the onde for 'fill'
    color = guide_legend(order = 1)#, 
    #fill = guide_legend(order = 2)
  ) 



stategeofacet <- stategeofacet +
  # A minimalistic theme with no background annotations
  theme_minimal() +
  theme(
    # Top-right position
    legend.pos = c(0.875, 0.2),
    # Elements within a guide are placed one next to the other in the same row
    legend.direction = "horizontal",
    # Different guides are stacked vertically
    legend.box = "vertical",
    # No legend title
    legend.title = element_blank(),
    # Light background color
    plot.background = element_rect(fill = "#F5F4EF", color = NA),
    plot.margin = margin(20, 30, 20, 30),
    # Customize the title. Note the new font family and its larger size.
    plot.title = element_text(
      margin = margin(0, 0, -50, 0), 
      size = 14, 
      face = "bold", 
      vjust = 0, 
      color = "grey25"
    ),
    plot.caption = element_text(size = 11),
    # Remove titles for x and y axes.
    axis.title = element_blank(),
    # Specify color for the tick labels along both axes 
    axis.text = element_text(color = "grey40", size = 6),
    # Specify face and color for the text on top of each panel/facet
    strip.text = element_text(face = "bold", color = "grey20")
  )

stategeofacet
#finally omg


            