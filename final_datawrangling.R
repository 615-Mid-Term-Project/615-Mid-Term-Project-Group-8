library(tidyverse)
library(magrittr)

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
#############################################################

pesticides <- read_csv("Pesticides(1).csv")
pesticides

strawb_select <- select(strawb, Year, State, items, discription, units, dname, type, details, Value)
strawb_select

#去除"Pesticide"和"details"的NA项
pesticides <- filter(pesticides, !is.na(Pesticide))
strawb_select <- filter(strawb_select, !is.na(details))

strawb_select %<>% separate(col = details, 
                            into = c("chemical_name", "chemical_id"), 
                            sep = "=", 
                            fill = "right")

#使"Pesticide"变成大写，这样可以match数据集strawb里的大写
pesticides <- mutate(pesticides, Pesticide = toupper(Pesticide))
#trimws：删除前导/尾随空格
strawb_select <- mutate(strawb_select, chemical_name = trimws(chemical_name))

joined <- inner_join(strawb_select, pesticides, 
                     by = c("chemical_name" = "Pesticide"))

#############################################################








