library(shiny)
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

#runExample("02_text")
# Define UI for dataset viewer app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Shiny Text"),
    
    # Sidebar layout with a input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Selector for choosing dataset ----
            selectInput(inputId = "joined_new",
                        label = "Our final dataset:",
                        choices = c("Combine strawb and pesticide")),
            
            # # Input: Numeric entry for number of obs to view ----
            # numericInput(inputId = "obs",
            #              label = "Number of observations to view:",
            #              value = 10)
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Verbatim text for data summary ----
            verbatimTextOutput("summary"),
            
            # Output: HTML table with requested number of observations ----
            tableOutput("view")
            
        )
    )
)
# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
    
    # Return the requested dataset ----
    datasetInput <- reactive({
        switch(input$joined_new,
               "Year" = Year,
               "State" = State,
               "type" = type)
    })
    
    # Generate a summary of the dataset ----
    output$summary <- renderPrint({
        dataset <- datasetInput()
        summary(joined_new)
    })
    
    # # Show the first "n" observations ----
    # output$view <- renderTable({
    #     head(datasetInput(), n = input$obs)
    # })
    
}
shinyApp(ui = ui, server = server)
