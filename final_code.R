library(tidyverse)
library(magrittr)
library(dplyr)

#############################################################

## Load data

strawb <- read.csv("Strawberries.csv")

#############################################################

## Drop the no-info columns

drop_no_info_cols <- function(df){
  cnames = colnames(strawb)
  T = NULL
  for(i in 1:ncol(df)){
    T <- c(T, nrow(unique(df[i])))
    }
  drop_cols <- cnames[which(T == 1)]
  return(select(df, !all_of(drop_cols)))
}

strawb <- drop_no_info_cols(strawb)

#############################################################

## Divide "Data.Item" into 4 groups

strawb %<>% separate(col=Data.Item,
                     into = c("Strawberries", "items", "discription", "units"),
                     sep = ",",
                     fill = "right")

## Data explore: see "Strawberries", "items", "discription", "units" information

distinct(strawb, Strawberries)
distinct(strawb, items)
distinct(strawb, discription)
distinct(strawb, units)

#############################################################

## Separate "Domain" into 2 columns

strawb %<>%  separate(col=Domain,
                      into = c("dname", "type" ), 
                      sep = ",", 
                      fill = "right")

## Data explore

distinct(strawb, dname)
distinct(strawb, type)

#############################################################

## Make a copy of "Domain.Category", called "Chemicals"

strawb %<>% 
  mutate(Chemicals = Domain.Category) %>% 
  relocate(Chemicals, .after = Domain.Category) 

## Vector of logicals for each row with "CHEM" at the start of strawb$Chemicals
## Select "CHEM" from Chemicals"

bb <- strawb$Chemicals %>% str_detect("CHEM")
sum(bb)

## Index 
# dim(x): http://www.stat.umn.edu/macanova/htmlhelp/node95.htm
# dim(x)[1] row, dim(x)[2] column

ind_C <- (!bb)*(1:dim(strawb)[1])

## Drop "0" from "ind_C"

r1 <- ind_C[ind_C > 0]

## Set entries in Chemicals column to " " if they don't start with "CHEM"

strawb$Chemicals[r1] <- " "

#############################################################

## Now we need a list of chemicals
## Divide "Chemicals" into 2 groups

strawb %<>% separate(col = Chemicals,
                     into = c("title", "details"),
                     sep = ":",
                     fill = "right")

# str_trim(): removes Spaces from the beginning and end of the string
# str_squish(): also reduces the duplicate Spaces in the string

strawb %<>% mutate(details = str_extract(str_trim(details) ,"[^(].*[^)]") )
strawb %<>% mutate(type = str_trim(type))

## Data explore

distinct(strawb, details)
distinct(strawb, type)

#############################################################

## Note that the 3021 rows in strawb have been spit into three subsets here
## But, be careful -- the columns are sill not sorted out

strawb_chem <- strawb %>% filter((type=="FUNGICIDE")|
                                   (type=="HERBICIDE")|
                                   (type=="INSECTICIDE"))

strawb_other <- strawb %>% filter(type=="OTHER")

strawb_na <- strawb %>% filter(is.na(type)==TRUE)

#############################################################

## look at herbicides in particular

strawb_herb <- strawb %>% filter(type=="HERBICIDE")

distinct(strawb_herb, details)

## more exploration

distinct(strawb_chem, details)
distinct(strawb_other, details)
distinct(strawb_na, details)

#############################################################

pesticides <- read_csv("Pesticides(1).csv")
pesticides

#Delete "N/A" from "Pesticide"

pesticides <- filter(pesticides, !is.na(Pesticide))

#Delete "N/A" from "details"

strawb_new <- filter(strawb, !is.na(details))

## Divide "details" into 2 groups

strawb_new %<>% separate(col = details, 
                            into = c("chemical_name", "chemical_id"), 
                            sep = "=", 
                            fill = "right")

#Capitalization of the "Pesticide" so that it matches the the dataset "strawb"

pesticides <- mutate(pesticides, Pesticide = toupper(Pesticide))

#trimws: Deletes leading/trailing Spaces

strawb_new <- mutate(strawb_new, chemical_name = trimws(chemical_name))

## join two dataframe

joined <- inner_join(strawb_new, pesticides, 
                     by = c("chemical_name" = "Pesticide"))

## data exploration

distinct(joined,type)
distinct(joined,discription)

## select column

joined_new<-select(joined,Year,State,discription,type,chemical_name,Value, Carcinogen,"Hormone Disruptor",Neurotoxins,"Developmental or Reproductive Toxins","Bee Toxins")

#############################################################

## data exploration of "State"

distinct(strawb_new, State)

## Divide by "Year"(only have 2019\2018\2016),explore every "Year" by "State"

joined_new_2019 <- joined_new[joined_new$Year == 2019, ] 
table(joined_new_2019$State)
joined_new_2018 <- joined_new[joined_new$Year == 2018, ] 
table(joined_new_2018$State)
joined_new_2016 <- joined_new[joined_new$Year == 2016, ] 
table(joined_new_2016$State)

## Divide by region in different "Year"

joined_new_2019_CALIFORNIA <- joined_new_2019[joined_new_2019$State=="CALIFORNIA",]
table(joined_new_2019_CALIFORNIA$"Bee Toxins")
joined_new_2019_FLORIDA <- joined_new_2019[joined_new_2019$State=="FLORIDA",]
table(joined_new_2019_FLORIDA$"Bee Toxins")

joined_new_2018_CALIFORNIA <- joined_new_2018[joined_new_2018$State=="CALIFORNIA",]
table(joined_new_2018_CALIFORNIA$"Bee Toxins")
joined_new_2018_FLORIDA <- joined_new_2018[joined_new_2018$State=="FLORIDA",]
table(joined_new_2018_FLORIDA$"Bee Toxins")

joined_new_2016_CALIFORNIA <- joined_new_2016[joined_new_2016$State=="CALIFORNIA",]
table(joined_new_2016_CALIFORNIA$"Bee Toxins")
joined_new_2016_FLORIDA <- joined_new_2016[joined_new_2016$State=="FLORIDA",]
table(joined_new_2016_FLORIDA$"Bee Toxins")
joined_new_2016_OREGON <- joined_new_2016[joined_new_2016$State=="OREGON",]
table(joined_new_2016_OREGON$"Bee Toxins")
joined_new_2016_WASHINGTON <- joined_new_2016[joined_new_2016$State=="WASHINGTON",]
table(joined_new_2016_WASHINGTON$"Bee Toxins")

## pie plot in CALIFORNIA
par(mfrow = c(1, 3)) # Create a 2 x 2 plotting matrix

freq<-c(30,15,35)
piepercent<- paste(round(100*freq/sum(freq), 2), "%")
pie1=pie(freq,labels = piepercent,main="2016_CALIFORNIA",col = rainbow(length(freq)),edges = 200, radius = 1)

freq<-c(30,15,35)
piepercent<- paste(round(100*freq/sum(freq), 2), "%")
pie1=pie(freq,labels = piepercent,main="2018_CALIFORNIA",col = rainbow(length(freq)),edges = 200, radius = 1)

freq<-c(30,15,35)
piepercent<- paste(round(100*freq/sum(freq), 2), "%")
pie1=pie(freq,labels = piepercent,main="2019_CALIFORNIA",col = rainbow(length(freq)),edges = 200, radius = 1)
legend("bottomleft",c("high","moderate","slight"),cex=1,fill=rainbow(length(freq)))

## pie plot in FLORIDA
par(mfrow = c(1, 3)) # Create a 2 x 2 plotting matrix

freq<-c(15,15,25)
piepercent<- paste(round(100*freq/sum(freq), 2), "%")
pie1=pie(freq,labels = piepercent,main="2016_FLORIDA",col = rainbow(length(freq)),edges = 200, radius = 1)

freq<-c(15,10,15)
piepercent<- paste(round(100*freq/sum(freq), 2), "%")
pie1=pie(freq,labels = piepercent,main="2018_FLORIDA",col = rainbow(length(freq)),edges = 200, radius = 1)

freq<-c(20,10,25)
piepercent<- paste(round(100*freq/sum(freq), 2), "%")
pie1=pie(freq,labels = piepercent,main="2019_FLORIDA",col = rainbow(length(freq)),edges = 200, radius = 1)
legend("bottomleft",c("high","moderate","slight"),cex=1,fill=rainbow(length(freq)))

## pie plot in OREGON & WASHINGTON
par(mfrow = c(1, 2))

freq<-c(10,0,10)
piepercent<- paste(round(100*freq/sum(freq), 2), "%")
pie1=pie(freq,labels = piepercent,main="2016_OREGON",col = rainbow(length(freq)),edges = 200, radius = 1)

freq<-c(10,5,15)
piepercent<- paste(round(100*freq/sum(freq), 2), "%")
pie1=pie(freq,labels = piepercent,main="2016_WASHINGTON",col = rainbow(length(freq)),edges = 200, radius = 1)
legend("bottomright",legend=c("high","moderate","slight"),cex=0.4,fill=rainbow(length(freq)))
       
## map plot

library(leaflet)
a<- leaflet() %>%
  addTiles() %>%
  addMarkers(lng=-118.8, lat=36.7, popup="CALIFORNIA") %>%
  addMarkers(lng=-81.5, lat=27.8, popup="FLORIDA") %>%
  addMarkers(lng=-120.7, lat=44.0, popup="OREGON") %>%
  addMarkers(lng=-120.2, lat=47.3, popup="WASHINGTON")
a

library(usmap)
library(ggplot2)
state<-c("CALIFORNIA","FLORIDA","OREGON","WASHINGTON")
observation<-c(48,30,4,6)
dataframe <- data.frame(state,observation)
b<-plot_usmap(data = dataframe, values = "observation", color = "red") + 
  scale_fill_continuous(name = "observation", label = scales::comma) + 
  theme(legend.position = "right")
b






