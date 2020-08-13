
#This is a basic shiny application. It uses a combination of flight data from two sources by merging them.
#The user selects an origin airport, the destination airport, and the month of the year. The application
#outputs a histogram showing the range of arrival delay times for the particular route in that particular
#month, as well as summary statistics such as the min, mean, and max times of the selected route for
#that month or year.
#Users can also select no month, and the application will aggregate the data from all months to display
#the route's data for the whole year. 
#NOTE: some flight connections between certain airports do not exist at certain months of the year


library(shiny)
library(nycflights13)
library(ggplot2)
library(tidyverse)

flights1 <- flights
airports1 <- airports
intermediate <- left_join(flights1, airports1, by = c("dest" = "faa")) %>%
  select(year, month, arr_delay, origin, dest, name)

#The merged dataset that will be used
main1<- intermediate %>% 
  mutate(arr_delay1 = ifelse(is.na(arr_delay), mean(arr_delay, na.rm = TRUE),arr_delay))

destvector <- main1 %>% select(name) %>% unique() #used in the input choices


ui <- fluidPage(
  headerPanel("Arrival Delay"),
  fluidRow(
    column(3,
           selectInput(inputId = "OR",
                       label = "Origin",
                       choices <- c("John F Kennedy Intl" = "JFK",
                                    "Newark Liberty Intl" = "EWR",
                                    "La Guardia" = "LGA"
                       )),
           
           selectInput(inputId = "DE",
                       label = "Destination",
                       choices <- c("-", destvector)),
           
           selectInput(inputId = "m",
                       label = "Month",
                       choices <- c("-" = 13,
                                    "January" = 1,
                                    "February" =2,
                                    "March" = 3,
                                    "April" = 4,
                                    "May" = 5,
                                    "June" = 6,
                                    "July" = 7,
                                    "August" = 8,
                                    "September" = 9,
                                    "October" = 10, 
                                    "November" = 11, 
                                    "December" = 12
                       )),
           
           actionButton(inputId = "go",
                        label = "Update"),
           column(9, tableOutput("Monthly_flights"))
    ),#end of panels with column width = 3
    
    verbatimTextOutput("warning"),
    
    fluidRow(column(8,
                    plotOutput("hist"))
    )),
  
  fluidRow(column(8, offset = 4,
                  verbatimTextOutput("summary"))
  )
)

server <- function(input, output) {
  data1 <- reactiveValues(numbers = NULL, title = NULL, warning = "select a flight combination")
  data2 <- reactiveValues(tally = "count")
  
  #if user inputs a valid month, find the data for the selected origin, destination, and month. 
  #otherwise, skip the filtering by month for an aggregate. Information is updated when user
  #clicks the update button so as to prevent constant rendering as user scrolls through 
  #input selection
  observeEvent(input$go, {
    data1$numbers <- if(input$m != 13){ main1 %>% group_by(dest) %>% filter(month == input$m) %>%
        filter(name == input$DE & origin == input$OR) %>% select(arr_delay1)
    }else{
      main1 %>% group_by(dest) %>%
        filter(name == input$DE & origin == input$OR) %>% select(arr_delay1)
    }
    
    #change the title as new inputs are selected
    data1$title <- if(input$m != 13){paste("Arrival delay from ", input$OR, " to ", input$DE, " in month ", input$m)
    }else{
      paste("Arrival delay from ", input$OR, " to ", input$DE, "in the year 2013")
    }
    
    #if the selected connection does not exist, warn the user
    data1$warning <- if(nrow(data1$numbers)==0){
      data1$warning <- "THE SELECTED FLIGHT COMBINATION DOES NOT EXIST"
    } else{
      data1$warning <- "here is the selected data"
    }
    
    #find the summary statistics for the selected flight connections
    data2$tally <- if(nrow(main1 %>% group_by(dest) %>%filter(month == input$m) %>% filter(name == input$DE & origin == input$OR)) < 1 ){
      data2$tally <- NULL
    } else{
      as.data.frame(c(main1 %>% group_by(dest) %>% filter(name == input$DE & origin == input$OR) %>%
                        group_by(month) %>% summarize(count = n()))
      )
    }
  })
  
  #outputs are updated when user clicks update
  output$hist <- renderPlot({
    ggplot(data1$numbers, aes(x = data1$numbers$arr_delay1)) + 
      geom_histogram(color = "black", fill = "white") +
      ggtitle(data1$title) + xlab("delay(minutes)")
  })
  
  output$summary <- renderPrint({
    summary(data1$numbers$arr_delay1)
  })
  
  output$warning <- renderPrint(data1$warning)
  
  output$Monthly_flights <- renderTable({
    if(is.null(data2$tally))
    {return (NULL)}  #prevent crash if the data does not exist
    else{
      data2$tally
    }
    
  })
}

shinyApp(ui = ui, server = server)