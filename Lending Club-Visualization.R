# Prep -----------------
library(tidyverse)
# library(readr)
# library(dplyr)
library(leaflet)
library(shiny)

listings <- read_csv("afterpredict.csv")


# UI -------------------
# Define UI for application that draws a histogram
ui <- fluidPage(
  title = "Lending Club",
  titlePanel("Lending Club"),
  tabsetPanel(
    tabPanel("Investor", dataTableOutput("table"))
  ),
  hr(),
  fluidRow(
    column(4,
           h3("Filter By Loan Amount"),
           column(6,
                  numericInput("minLoan", label = "Minimum Loan Amount:", value = 1000)
           ),
           column(6,
                  numericInput("maxLoan", label = "Maximum Loan Amount:", value = 35000)
           )
    ),
    column(4,
           h3("Filter By Interest Rate"),
           checkboxGroupInput("gradefilter", label="Return Grade:", 
                              choices = c("A", "B", "C", "D", "E", "F"),
                              selected = c("A", "B", "C", "D", "E", "F"))
    ),
    column(4,
           h3("Filter By Loan Term"),
           checkboxGroupInput("termfilter", label="Loan Term:", 
                              choices = c("36 months"=36, "60 months"=60),
                              selected = c("36 months"=36, "60 months"=60))
    ),
    column(4,
           h3("Filter By Probability of Fully Paid"),
           sliderInput("paidfilter", label = "Pr (Fully Paid):",
                       min = 0, max = 1, value = c(0,1))
    )
  ),
  hr(), #horizontal row
  p("Data from", a("Lending Club", href= "https://www.lendingclub.com/site/home", target = "_blank"))
)

# Server ------------------------
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  df <- reactive({
    df <- listings %>%
      filter((loan_amnt >= input$minLoan & loan_amnt <= input$maxLoan) &
               (prob_loan >= input$paidfilter[1] & loan_status <= input$paidfilter[2]) &
               (grade %in% input$gradefilter)&
               (term %in% input$termfilter)) #within the checkbox numeric values
    return(df)
  })
  
  
  # Data Table --------------------
  output$table <- renderDataTable({df()})
  
}

# Run the app -------------------------
# Run the application 
shinyApp(ui = ui, server = server)
rsconnect::setAccountInfo(name='shiqili',
                          token='96B020622C170A87ED72494BFAA69C74',
                          secret='WWy6cxpmY1tFXRkyWE//TUNtOkf9ZQ9YToCV4zM7')
