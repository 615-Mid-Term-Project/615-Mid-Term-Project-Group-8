
# some libs
library(tidyverse)
library(usmap)
library(magrittr)
library(dplyr)
library(babynames)
#library(shiny)

#----------------------------------------------------------------------------------------------------------
state <- c("California", "Florida", "Oregon", "Washington")
#prepare the function

shi_func <- function(yr){
  
  mm <- shiny_map %>% 
    select(yr)
  
  df_shiny <- cbind(state, mm)
  
  # 给相应的df改列名，并且确保为data frame，确保第二列为double以方便最后下一步作图。
  colnames(df_shiny) <- c("state", "observations")
  df_shiny <- data.frame(df_shiny)
  df_shiny$observations <- as.numeric(df_shiny$observations)
  
  return(df_shiny)
}
#----------------------------------------------------------------------------------------------------------


#------------------------------------------------------
## Load data and wrangling data

strawb <- read.csv("Strawberries.csv")
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

## Divide "Data.Item" into 4 groups

strawb %<>% separate(col=Data.Item,
                     into = c("Strawberries", "items", "discription", "units"),
                     sep = ",",
                     fill = "right")

## Separate "Domain" into 2 columns

strawb %<>%  separate(col=Domain,
                      into = c("dname", "type" ), 
                      sep = ",", 
                      fill = "right")

## Make a copy of "Domain.Category", called "Chemicals"
Sys.setlocale("LC_ALL", "C")

strawb %<>% 
  mutate(Chemicals = Domain.Category) %>% 
  relocate(Chemicals, .after = Domain.Category) 

## Vector of logicals for each row with "CHEM" at the start of strawb$Chemicals
## Select "CHEM" from Chemicals"

bb <- strawb$Chemicals %>% str_detect("CHEM")

## Index 

ind_C <- (!bb)*(1:dim(strawb)[1])

## Drop "0" from "ind_C"

r1 <- ind_C[ind_C > 0]

## Set entries in Chemicals column to " " if they don't start with "CHEM"

strawb$Chemicals[r1] <- " "


## Now we need a list of chemicals
## Divide "Chemicals" into 2 groups

strawb %<>% separate(col = Chemicals,
                     into = c("title", "details"),
                     sep = ":",
                     fill = "right")

strawb %<>% mutate(details = str_extract(str_trim(details) ,"[^(].*[^)]") )
strawb %<>% mutate(type = str_trim(type))


strawb_chem <- strawb %>% filter((type=="FUNGICIDE")|
                                   (type=="HERBICIDE")|
                                   (type=="INSECTICIDE"))

strawb_other <- strawb %>% filter(type=="OTHER")

strawb_na <- strawb %>% filter(is.na(type)==TRUE)


pesticides <- read_csv("Pesticides(1).csv",show_col_types = FALSE)


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

strawb_new <- mutate(strawb_new, chemical_name = str_trim(chemical_name))



## join two dataframe

joined <- inner_join(strawb_new, pesticides, 
                     by = c("chemical_name" = "Pesticide"))


## select column

joined_new<-select(joined,Year,State,discription,type,chemical_name,Value, Carcinogen,"Hormone Disruptor",Neurotoxins,"Developmental or Reproductive Toxins","Bee Toxins")
#------------------------------------------------------------------------------
# now we have "joined_new"



a <- joined_new %>%
  group_by(Year,State) %>%
  summarise(count = n())

c1 <- a[1:4, 3]
c2 <- a[5:6,3]
c2
c2 <- rbind(c2, 0, 0)
c3 <- a[7:8, 3]
c3 <- rbind(c3, 0,0)

shiny_map <- cbind(c1,c2,c3)
colnames(shiny_map) <- c("x2016", "x2018", "x2019")
shiny_map

# Now we have shiny_app for making the map

#------------------------------------------------------------------------------------------------------------

