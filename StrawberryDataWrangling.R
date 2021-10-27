##  for data info see
##  https://quickstats.nass.usda.gov/

##  Note: CV Coefficient of variation. Available for the 2012 Census of Agriculture
##        only. County-level CVs are generalized.

library(tidyverse)
library(magrittr)

## load data

strawb <- read.csv("Strawberries.csv")

##################################################
## Drop the no-info columns
##################################




drop_no_info_cols <- function(df){
  cnames = colnames(strawb)
  T = NULL
  for(i in 1:ncol(df)){T <- c(T, nrow(unique(df[i])))}
  drop_cols <- cnames[which(T == 1)]
  return(select(df, !all_of(drop_cols)))
}



strawb <- drop_no_info_cols(strawb)



##############################################################


strawb %<>% separate(col=Data.Item,
                into = c("Strawberries", "items", "discription", "units"),
                sep = ",",
                fill = "right")

####  explore

distinct(strawb, Strawberries)

distinct(strawb, items)

distinct(strawb, discription)

distinct(strawb, units)
  

################################################

## Separte Domain into 2 columns

strawb %<>%  separate(col=Domain,
                      into = c("dname", "type" ), 
                      sep = ",", 
                      fill = "right")
distinct(strawb, dname)


distinct(strawb, type)


###################################
## make a copy of Domain.Categoy

strawb %<>% 
  mutate(Chemicals = Domain.Category) %>% 
  relocate(Chemicals, .after = Domain.Category) 


## vector of logicals for each row with "CHEM" at 
## the start of strawb$Chemicals

bb <- strawb$Chemicals %>% str_detect("CHEM")

sum(bb)

## index 
ind_C <- (!bb)*(1:dim(strawb)[1])

## 
r1 <- ind_C[ind_C > 0]

## set entries in Chemicals column to " " if they don't start with CHEM

strawb$Chemicals[r1] <- " "

#########################################

## now we need a list of chemicals

strawb %<>% separate(col = Chemicals,
                               into = c("title", "details"),
                               sep = ":",
                               fill = "right")

strawb %<>% mutate(details = str_extract(str_trim(details) ,"[^(].*[^)]") )

strawb %<>% mutate(type = str_trim(type))


distinct(strawb, details)

distinct(strawb, type)

#################################################################
## Note that the 3021 rows in strawb have been spit into 
## three subsets here

## But, be careful -- the columns are sill not sorted out

strawb_chem <- strawb %>% filter((type=="FUNGICIDE")|
                                   (type=="HERBICIDE")|
                                   (type=="INSECTICIDE"))



strawb_other <- strawb %>% filter(type=="OTHER")


strawb_na <- strawb %>% filter(is.na(type)==TRUE)

##################################################################
## look at herbicides in particular

strawb_herb <- strawb %>% filter(type=="HERBICIDE")

distinct(strawb_herb, details)

##################################################################
## more exploration


distinct(strawb_chem, details)

distinct(strawb_other, details)

distinct(strawb_na, details)

#########################################################################
### explord California

strawb_chem <- drop_no_info_cols(strawb_chem)
strawb_other <- drop_no_info_cols(strawb_other)



chem_list <- distinct(strawb_chem, details)

chem_list <- rbind2(chem_list,distinct(strawb_other, details))



strawb_california_2020 <- strawb %>% filter((State == "CALIFORNIA")&(Year==2020))


##  aa <- strawb$Year[4]
## GLUFOSINATE-AMMONIUM = 128850



strawb__2019_Californi_GLUFOSINATE_AMMONIUM <- strawb_herb %>% filter((State=="CALIFORNIA")&
                                                                        (Year==2019)&
                                                                        (details=="GLUFOSINATE-AMMONIUM = 128850"))

### this table is full of repetition and multiple related measurements!!

################################################################

### let's look at the other states


distinct(strawb_chem, State)

######################################################################################
### focus on Florida

strawb_chem_FLORIDA_2020 <- strawb_chem %>% filter((State == "FLORIDA")&(Year==2020))

View(strawb_chem_FLORIDA_2020)

## there's nothing in strawb_chem_FLORIDA_2020 -- California was the same way
## probably drop year 2020

strawb_chem_FLORIDA_2019 <- strawb_chem %>% filter((State == "FLORIDA")&(Year==2019))

View(strawb_chem_FLORIDA_2019)





strawb_herb_FLORIDA_2019  <- strawb_herb %>% filter((State == "FLORIDA")&(Year==2019))

View(strawb_herb_FLORIDA_2019)

## get rid of as much redundant data as possible


strawb_herb_FLORIDA_2019 <- drop_no_info_cols(strawb_herb_FLORIDA_2019)

View(strawb_herb_FLORIDA_2019)


## this is a table for Florida Strawberries Herbicides 2019
## get rid of the redundant Domain.Category column

strawb_herb_FLORIDA_2019 %<>% select(-Domain.Category)



View(strawb_herb_FLORIDA_2019)


strawb_herb_FLORIDA_2019_a <- straw_h %>% pivot_wider()


###  Note that there is still a lot of clean-up left to do -- 
##   which could have been done earlier in the larger table
##   but we'll do it here 
##   also note that there is no data available -- except for the identity
##   of the herbicides used


strawb_herb_FLORIDA_2019 %<>% separate(col = items,
                                      into = c("it", "what"), 
                                      sep = "-",
                                      fill = "right")

### drop column it

strawb_herb_FLORIDA_2019 %<>% select(-it)

## relocate details to the first column

strawb_herb_FLORIDA_2019 %<>%  relocate(details, .before = what)

View(strawb_herb_FLORIDA_2019)

## rename "details column to "herbicides"
strawb_herb_FLORIDA_2019 %<>% rename(herbicides = details)


## Now you can clearly see that this is a small table of 5 herbicides measured in 5 ways

## use pivot to make your table wider.  


## This R script has taken you down a path from the large table about strawberries
## to this table about herbicide application in Florida.
## 
## Now, you're ready to go back to the start and deliver your exploration of the entire dataset.
## You can get tables for insecticides, herbicides, fungicides, and fertilizers for each state.
## 
## You don't know the toxicity for each chemical, but you have some examples of both dangerous
## chemicals and benign chemicals.  
## 
## you also don't have CV.  the NASS system has indicated the CV is only available 
## for the year 2021,  
##
## 
## 

pesticides <- read_csv("Pesticides(1).csv")
pesticides

strawb_select <- select(strawb, Year, State, items, discription, units, dname, type, details, Value)

strawb_select
