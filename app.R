library(shiny)
library(dplyr)
library(ggplot2)
balanced <- read.csv("balanced.csv")

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Tennessee Promise: Analyzing State Grant Aid and Enrollment by Intitution Type"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   value = 5),
      
      # Input: Selector for choosing type ----
      radioButtons("typeInput", "Choose an Intitution Type:", levels(balanced$TYPE))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Inform the reader about the dataset and Tennessee Promise
      p("The Tennessee Promise is a scholarship program started in 2015 with the goal of 
        providing tuition-free attendance at an in-state commmunity or technical college for all 
        high school graduates in Tennessee. The balanced panel dataset here contains key variables
        (such as enrollment of full-time, first-time undergraduates, state grant aid, federal grant 
        aid, etc.) for all undergraduate institutions from 2010 to 2015. Thus, there are four 
        institution types: four-year public, four-year private, two-year public, and two-year private. 
        By selecting one of the four different institution types to the left, you can explore how 
        relevant variables differ by institution type."),
      
      # Output: HTML table with requested number of observations ----
      tableOutput("view"),
      
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),
      
      #Output: Time series of enrollment
      plotOutput("hist"),
      
      #Output: Time series of enrollment
      plotOutput("timeseries")
      
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  datasetInput <- reactive({
    filtered <- dplyr::filter(balanced, TYPE == input$typeInput)
  })
  
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  output$hist <- renderPlot({
    dataset <- datasetInput()
    ggplot2::ggplot(dataset, aes(GRANT_STATE)) +
      geom_histogram(binwidth = 60000)+
      xlab("State Grant Aid")+
      ylab("Frequency")
  })

  
  output$timeseries <- renderPlot({
    dataset <- datasetInput()
    avg.grant <- aggregate(x=dataset$ENROLL_FTUG,
                           by=list(dataset$YEAR),
                           FUN=sum)
    data.table::setnames(avg.grant, "Group.1", "Year")
    data.table::setnames(avg.grant, "x", "Total.Firsttime.Fulltime.Undergraduates")
    ggplot(avg.grant, aes(Year, y=Total.Firsttime.Fulltime.Undergraduates)) +
      geom_line()+
      geom_point()+
      ylab("Total First-time, Full-time Enrollment")+
    ggtitle("Total Full-time, First-time Student Enrollment for Selected Institution Type")
  })
  
}

# Run app ----
shinyApp(ui, server)