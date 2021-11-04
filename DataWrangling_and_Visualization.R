library(usmap)
library(tidyverse)
library(magrittr)
library(plotly)

freq_map <- function(){
state<-c("CALIFORNIA","FLORIDA","OREGON","WASHINGTON")
observation<-c(48,30,4,6)
dataframe <- data.frame(state,observation)
viz <- plot_usmap(data = dataframe, values = "observation", color = "red") + 
  scale_fill_continuous(low = "white", high = "purple",name = "observation", label = scales::comma) + 
  theme(legend.position = "right")
return(viz)
}



####################################################################

## Load data and wrangling data

strawb <- read.csv("Strawberries.csv",fileEncoding = "latin1")

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
                     into = c("Strawberries", "items", "measurement", "units"),
                     sep = ",",
                     fill = "right")

## Separate "Domain" into 2 columns

strawb %<>%  separate(col=Domain,
                      into = c("dname", "type" ), 
                      sep = ",", 
                      fill = "right")

## Make a copy of "Domain.Category", called "Chemicals"

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

strawb_new <- mutate(strawb_new, chemical_name = trimws(chemical_name))

## join two dataframe

joined <- inner_join(strawb_new, pesticides, 
                     by = c("chemical_name" = "Pesticide"))


## select column

joined %<>% select(Year, State, measurement, type, chemical_name,Value, Carcinogen,"Hormone Disruptor",Neurotoxins,"Developmental or Reproductive Toxins","Bee Toxins")


#Make column names unique and add a column for the Value as a numeric
names(joined)<-make.names(names(joined),unique = TRUE)
joined$Value_Num <- as.numeric(gsub(",", "",joined$Value))

###############################################################


#Plots chemicals based on bee toxin levels, stratified by the measurement type
plot_Measurements <- function(df){
  groups <- unique(df$measurement)
  for (group in groups){
    temp <- filter(df, measurement == group)
    print(ggplot(data = temp) + 
            geom_jitter(mapping = aes(x = Bee.Toxins, y=Value_Num, color = Bee.Toxins), size = 3)+
            ggtitle(group))
  }
}

#Plots chemicals based on bee toxin levels, stratified by State, using measurement in pounds
plot_Points <- function(df){
  states <- unique(df$State)
  for (state in states){
    temp <- filter(df, State == state, measurement == " MEASURED IN LB")
    print(ggplot(data = temp) + 
            geom_jitter(mapping = aes(x = Bee.Toxins, y=Value_Num, color = Bee.Toxins), size = 3)+
            ggtitle(state))
  }
}


plot_Cols <- function(df){
  temp <- filter(df, measurement == " MEASURED IN LB", Bee.Toxins != "")
  plot <- ggplot(temp, aes(x = State, y = Value, fill = Bee.Toxins)) +
    geom_col(position = "dodge", colour = "black") +
    scale_fill_brewer(palette = "Pastel1")
  return(plot)
}


###################################################################
# Pie Plots

## Divide by "Year"(only have 2019\2018\2016),explore every "Year" by "State"

plot_Pies <- function(df){
  joined_new_2019 <- df[df$Year == 2019, ] 
  #table(joined_new_2019$State)
  joined_new_2018 <- df[df$Year == 2018, ] 
  #table(joined_new_2018$State)
  joined_new_2016 <- df[df$Year == 2016, ] 
  #table(joined_new_2016$State)
  
  ## Divide by region in different "Year"
  
  joined_new_2019_CALIFORNIA <- joined_new_2019[joined_new_2019$State=="CALIFORNIA",]
  #table(joined_new_2019_CALIFORNIA$"Bee Toxins")
  joined_new_2019_FLORIDA <- joined_new_2019[joined_new_2019$State=="FLORIDA",]
  #table(joined_new_2019_FLORIDA$"Bee Toxins")
  
  joined_new_2018_CALIFORNIA <- joined_new_2018[joined_new_2018$State=="CALIFORNIA",]
  #table(joined_new_2018_CALIFORNIA$"Bee Toxins")
  joined_new_2018_FLORIDA <- joined_new_2018[joined_new_2018$State=="FLORIDA",]
  #table(joined_new_2018_FLORIDA$"Bee Toxins")
  
  joined_new_2016_CALIFORNIA <- joined_new_2016[joined_new_2016$State=="CALIFORNIA",]
  #table(joined_new_2016_CALIFORNIA$"Bee Toxins")
  joined_new_2016_FLORIDA <- joined_new_2016[joined_new_2016$State=="FLORIDA",]
  #table(joined_new_2016_FLORIDA$"Bee Toxins")
  joined_new_2016_OREGON <- joined_new_2016[joined_new_2016$State=="OREGON",]
  #table(joined_new_2016_OREGON$"Bee Toxins")
  joined_new_2016_WASHINGTON <- joined_new_2016[joined_new_2016$State=="WASHINGTON",]
  #table(joined_new_2016_WASHINGTON$"Bee Toxins")
  
  ## pie plot in CALIFORNIA
  par(mfrow = c(1, 3)) # Create a 2 x 2 plotting matrix
  
  freq<-c(30,15,35)
  piepercent<- paste(round(100*freq/sum(freq), 2), "%")
  pie1=pie(freq,labels = piepercent,main="2016_CALIFORNIA",col = terrain.colors(length(freq)),edges = 200, radius = 1)
  
  freq<-c(30,15,35)
  piepercent<- paste(round(100*freq/sum(freq), 2), "%")
  pie1=pie(freq,labels = piepercent,main="2018_CALIFORNIA",col = terrain.colors(length(freq)),edges = 200, radius = 1)
  
  freq<-c(30,15,35)
  piepercent<- paste(round(100*freq/sum(freq), 2), "%")
  pie1=pie(freq,labels = piepercent,main="2019_CALIFORNIA",col = terrain.colors(length(freq)),edges = 200, radius = 1)
  legend("bottomleft",c("high","moderate","slight"),cex=1,fill=terrain.colors(length(freq)))
  
  ## pie plot in FLORIDA
  par(mfrow = c(1, 3)) # Create a 2 x 2 plotting matrix
  
  freq<-c(15,15,25)
  piepercent<- paste(round(100*freq/sum(freq), 2), "%")
  pie1=pie(freq,labels = piepercent,main="2016_FLORIDA",col = terrain.colors(length(freq)),edges = 200, radius = 1)
  
  freq<-c(15,10,15)
  piepercent<- paste(round(100*freq/sum(freq), 2), "%")
  pie1=pie(freq,labels = piepercent,main="2018_FLORIDA",col = terrain.colors(length(freq)),edges = 200, radius = 1)
  
  freq<-c(20,10,25)
  piepercent<- paste(round(100*freq/sum(freq), 2), "%")
  pie1=pie(freq,labels = piepercent,main="2019_FLORIDA",col = terrain.colors(length(freq)),edges = 200, radius = 1)
  legend("bottomleft",c("high","moderate","slight"),cex=1,fill=terrain.colors(length(freq)))
  
  ## pie plot in OREGON & WASHINGTON
  par(mfrow = c(1, 2))
  
  freq<-c(10,0,10)
  piepercent<- paste(round(100*freq/sum(freq), 2), "%")
  pie1=pie(freq,labels = piepercent,main="2016_OREGON",col = terrain.colors(length(freq)),edges = 200, radius = 1)
  
  freq<-c(10,5,15)
  piepercent<- paste(round(100*freq/sum(freq), 2), "%")
  pie1=pie(freq,labels = piepercent,main="2016_WASHINGTON",col = terrain.colors(length(freq)),edges = 200, radius = 1)
  legend("bottomright",legend=c("high","moderate","slight"),cex=0.4,fill=terrain.colors(length(freq)))
}


################################################################

pesticides <- pesticides %>% 
  filter(!is.na(Pesticide))
pesticides <- pesticides %>% 
  mutate(Pesticide = toupper(Pesticide))
pesticides1 <- pesticides %>% 
  rename(chemical_name = Pesticide)

# deal with joined2
# install.packages("prob")
joined2 <- left_join(strawb_new, pesticides1,
                     by = "chemical_name")

# subset by state
california <- joined2 %>% 
  filter(State == "CALIFORNIA")

# subset by chemical type
cali_chem <- california %>% filter((type=="FUNGICIDE")|
                                     (type=="HERBICIDE")|
                                     (type=="INSECTICIDE"))


# chemical data are classified according to year,unit of measurement,chemical type
cali_chem1 <- cali_chem %>% 
  group_by(Year, type, measurement) %>% 
  summarise(count = n())

# value as.numeric
cali_chem$Value <- as.numeric(cali_chem$Value)

# sum
cali_chem1 <- cali_chem %>% 
  group_by(Year, type, measurement) %>% 
  summarise(count = n(), value_sum = sum(Value))

# measured in lb larger than other units of measurement
aa <- !is.na(cali_chem$`Bee Toxins`)
ind_C1 <- (!aa)*(1:dim(cali_chem)[1])
r2 <- ind_C1[ind_C1>0]
cali_chem$`Bee Toxins`[r2] <- "none"

cali_chem_summary <- cali_chem %>% 
  group_by(Year,`Bee Toxins`) %>% 
  summarise(total_usage = sum(Value, na.rm = T))

# change position of slight,none
yy <- factor(as.factor(cali_chem_summary$`Bee Toxins`), levels = c("none", "slight" , "moderate", "high"))

cali_chem_summary$`Bee Toxins` <- yy

cali_chem_summary_ordered <- cali_chem_summary %>% 
  arrange(Year,`Bee Toxins`)

# total usage every year
cali_chem_summary1 <- cali_chem_summary %>% 
  group_by(Year) %>% 
  summarise(useageofyear = sum(total_usage))

# Bee toxicity of high, medium and low without use, percentage
perc_col = matrix(cbind(cali_chem_summary_ordered$total_usage[1:4] / 6046.092 , cali_chem_summary_ordered$total_usage[5:8] / 9177.122, cali_chem_summary_ordered$total_usage[9:12] / 8457.764), nrow = 12)

cali_chem_summary_ordered$percentage <- perc_col

#bar plot
plot_bar <- function(df){
  plot <- ggplot(df, aes(x = Year, y = percentage, fill = `Bee Toxins`)) +
    geom_col(position = "dodge")+ scale_fill_brewer(palette = "Pastel1")
   return(plot)
}
#stacked plot
plot_stacked_bar <- function(df){
  plot <- ggplot(cali_chem_summary_ordered, aes(fill = `Bee Toxins`, y = percentage, x = Year)) + 
    geom_bar(position="stack", stat="identity")+ scale_fill_brewer(palette = "Pastel2")
  return(plot)
}



