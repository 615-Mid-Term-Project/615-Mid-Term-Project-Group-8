

# some libs
library(tidyverse)
# install.packages("babynames")
library(babynames)

#
# The most basic shiny skeleton.
library(shiny)

ui <- fluidPage(textInput(inputId = "name", 
                          label = "Name", 
                          value = "",
                          placeholder = "Danny"),
                selectInput(inputId = "sex",
                            label = "Sex:",
                            choices = list(Female = "F", #these values need to be exactly how they show in the dataset.
                                           male = "M")),
                sliderInput(inputId = "year",
                            label = "Year Range:",
                            min = min(babynames$year),
                            max = max(babynames$year),
                            value = c(min(babynames$year),
                                      max(babynames$year)),
                            sep = ""),
                submitButton(text = "Create my plot!"),
                
                plotOutput(outputId = "nameplot")
                )


server <- function(input, output){
  output$nameplot <- renderPlot(
    babynames %>% 
      filter(sex == input$sex,
             name == input$name) %>% 
      ggplot(aes(x = year,
                 y = n)) +
      geom_line() +
      theme_minimal() +
      scale_x_continuous(limits = input$year)
  )

  
}
shinyApp(ui = ui, server = server)
