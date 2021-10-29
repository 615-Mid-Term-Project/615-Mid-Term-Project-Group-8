library(tidyverse)
library(magrittr)
library(dplyr)

strawb <- read.csv("Strawberries.csv",fileEncoding = "latin1")
pest <- read.csv("Pesticides(1).csv",fileEncoding = "latin1")

names(pest)[1] <- "Pesticide"

#column we want
#chemical; chemical type (insecticide, herbicide, fungicide, fertilizer);
#toxicity-human/bee;toxicity-level;state;year;measurement(s)

strawb<-strawb %>% mutate(NEWID = row_number())
pest1 <- subset(pest, Pesticide != "" )

drop_no_info_cols <- function(df){
  cnames = colnames(strawb)
  T = NULL
  for(i in 1:ncol(df)){T <- c(T, nrow(unique(df[i])))}
  drop_cols <- cnames[which(T == 1)]
  return(select(df, !all_of(drop_cols)))
}

strawb <- drop_no_info_cols(strawb)

#Separate the Data.Item
strawb %<>% separate(col=Data.Item,
                     into = c("Strawberries", "items", "discription", "units"),
                     sep = ",",
                     fill = "right")
#Seperate Domain
strawb %<>%  separate(col=Domain,
                      into = c("dname", "chemical type" ), 
                      sep = ",", 
                      fill = "right")
#Copy Domain.Category and then rename it to chemicals
strawb %<>% 
  mutate(Chemicals = Domain.Category) %>% 
  relocate(Chemicals, .after = Domain.Category) 
ind_C <- (!bb)*(1:dim(strawb)[1])

r1 <- ind_C[ind_C > 0]

strawb$Chemicals[r1] <- " "

strawb %<>% separate(col = Chemicals,
                     into = c("title", "details"),
                     sep = ":",
                     fill = "right")
strawb %<>% mutate(details = str_extract(str_trim(details) ,"[^(].*[^)]") )

strawb %<>% mutate(chemicaltype = str_trim("chemical type"))

strawb_chem <- strawb %>% filter(("chemical type"=="FUNGICIDE")|
                                   ("chemical type"=="HERBICIDE")|
                                   ("chemical type"=="INSECTICIDE"))

strawb_other <- strawb %>% filter("chemical type"=="OTHER")
#head(strawb_other)
strawb_na <- strawb %>% filter(is.na("chemical type")==TRUE)

pesticides <- filter(pest, !is.na(Pesticide))
strawb_select <- filter(strawb, !is.na(details))

strawb_select %<>% separate(col = details, 
                            into = c("chemical_name", "chemical_id"), 
                            sep = "=", 
                            fill = "right")

pesticides <- mutate(pesticides, Pesticide = toupper(Pesticide))
strawb_select <- mutate(strawb_select, chemical_name = trimws(chemical_name))

joineddata <- inner_join(strawb_select, pesticides, by = c("chemical_name" = "Pesticide"))

#chemical; chemical type (insecticide, herbicide, fungicide, fertilizer);
#toxicity-human/bee;toxicity-level;state;year;measurement(s)
names(joineddata)[8] <- "Measurment(s)"
finaldata <- select(joineddata,"Year","State","chemical type","Measurment(s)")

view(finaldata)
