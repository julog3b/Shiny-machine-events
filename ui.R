shinyUI(
  fluidPage(
    useShinyjs(),
    titlePanel("Pushdev Error Report Summary for DATA 601"),
    #Setting the font weight and color for the dashboard
    style = "font-weight: 500; color: #4d3a7d;",  
    
    
    tabsetPanel(
      
      tabPanel("Date Selection",
               dashboardPage(skin = "blue",
                             dashboardHeader(disable = TRUE),
                             dashboardSidebar(disable = TRUE),
                             dashboardBody(
                               
                               fluidPage( 
                                 box(background= "blue", 
                                     dateRangeInput(
                                       "daterange", label = h3("Date range:"),
                                       start = Sys.Date() - 7,
                                       end = Sys.Date())),
                                 width = 12,
                                 actionButton(inputId = "Go1",
                                              label = "Load Data", 
                                              icon = icon("play"))  
                               )))),
      
      tabPanel("DATA",
               verbatimTextOutput(outputId = "heading1"), 
               tabsetPanel(
                 
                 tabPanel("Message counts",
                          withSpinner(
                            verbatimTextOutput((outputId = "message_counts"))
                          )
                 ),
                 
                 tabPanel("Serial No, Message, Counts",
                          withSpinner(
                            DT::dataTableOutput(outputId = "counts")
                          ),
                          verbatimTextOutput(outputId = "messagesDTnotes")
                 ),
                 
                 tabPanel("Daily error counts",
                          withSpinner(
                            DT::dataTableOutput(outputId = "date_serialNo")
                          )
                 ), 
                 
                 
                 tabPanel("Raw Data",
                          withSpinner(
                            DT::dataTableOutput(outputId = "datatable1")
                          )
                 ),
                 
                 
                 
                 tabPanel("Data Summary",
                          withSpinner(
                            verbatimTextOutput(outputId = "SummaryA3")
                          )
                 ),
                 
                 
                 
                 
                 
                 
               )
      ),
      tabPanel("PLOTS",
               verbatimTextOutput(outputId = "heading2"),
               tabsetPanel(
                 tabPanel("Daily Error Counts per Machine",
                          selectInput(inputId = "VariablesG4", 
                                      label = "Select Serial Number:",
                                      choices = NULL, multiple = FALSE, 
                                      selected = NULL, selectize = FALSE),            
                          withSpinner(
                            highchartOutput(outputId = "highchartlineSN", height = 800)
                          )
                 ),
                 
                 
                 tabPanel("Messages plot",
                          selectInput(inputId = "VariablesG3", 
                                      label = "Choose Message or type keywords:",
                                      choices = NULL, multiple = FALSE, 
                                      selected = NULL, selectize = FALSE),            
                          withSpinner(
                            highchartOutput(outputId = "highchartlineMsg", height = 800)
                          )
                 ),
                 
                 
                 
                 
                 
                 tabPanel("Serial numbers vs Messages Bar plot",
                          selectInput(inputId = "VariablesG0", 
                                      label = "Choose Message or type keywords:",
                                      choices = NULL, multiple = FALSE, 
                                      selected = NULL, selectize = FALSE),                          
                          
                          withSpinner(
                            highchartOutput(outputId = "newBar", height = 800)
                          )
                 ),
                 
                 
                 tabPanel("Time Series Decomposition",
                          selectInput(inputId = "graph", 
                                      label = "Choose the decomposition:", 
                                      choices = c("machine counts", "messages"), 
                                      selected = "machine counts"),
                          selectInput(inputId = "VariablesG5", 
                                      label = "Choose Error message:",
                                      choices = NULL, multiple = FALSE, 
                                      selected = NULL, selectize = FALSE),
                          selectInput(inputId = "VariablesG6", 
                                      label = "Select Serial Number:",
                                      choices = NULL, multiple = FALSE, 
                                      selected = NULL, selectize = FALSE),      
                          sliderInput(inputId = "window", 
                                      label = "Choose trend window", 
                                      min = 1, max = 30, step = 1, value = 2),
                          
                          mainPanel(
                            withSpinner(
                              plotOutput(outputId = "selected_graph", height = 800)),
                            width = 500,  
                            
                          ),
                          verbatimTextOutput(outputId = "ts_decomposeNOTES")
                 ),
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
               )
      )
    )
  )
)

