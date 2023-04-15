shinyServer(function(input, output) {
  
  react <- reactiveValues(cleanData1=NULL, cleanData2=NULL, 
                          recipe=NULL, model=NULL)
  
  onSessionEnded(function() {
    stopApp()
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  observeEvent(input$Go1,
               {getdata()})
  
  
  
  # -----------------------------------------------------
  # CONNECT TO BIGQUERY
  # ---------------------------------------------------- 
  
  getdata <- reactive({
    req(input$Go1)
    showNotification(id = "Loading", ui = "Data is loading. Please wait...",
                     duration = NULL, type = "error")
    
    
    start_date = input$daterange[1] - hours(13)
    end_date = input$daterange[2] + hours(11)
    
    
    # -------------------------------------------------------------------------
    
        con <- dbConnect(
          bigrquery::bigquery(),
          project = "payments-cloud-test",
          dataset = "live_vending_data_snapshot_20230202",
          billing = "payments-cloud-test"
        )




        sql <- "SELECT DISTINCT
    timestamp, level, message, serialNumber, appVersion,
    (SELECT t.value FROM UNNEST (tags) AS t WHERE t.key = 'siteName')
    as siteName,
    (SELECT t2.value FROM UNNEST (tags) AS t2 WHERE t2.key = 'machineLocation')
    as machineLocation,
    (SELECT t3.value FROM UNNEST (tags) AS t3 WHERE t3.key = 'machineId')
    as machineId

     FROM `payments-cloud-test.live_vending_data_snapshot_20230202.log`
     where level in ('Error','Critical','Fatal')


     and timestamp BETWEEN 'DATE1' AND 'DATE2'
     ORDER BY timestamp DESC"



        sql <- sub("DATE1", start_date,sql);
        sql <- sub("DATE2", end_date,sql)


        error_data24N <- strings2factors(dbGetQuery(con, sql))
    removeNotification(id = "Loading")
    # --------------------------------------------------------------------------
    
    
    
    
    
    
    
    
    
    # --------------------------------------------------------------------------
    #                                 CLEANING THE DATA
    # --------------------------------------------------------------------------
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    # 1. Data Date and Serial Number
    date_serialNumber1 <- table(error_data24N$timestamp, error_data24N$serialNumber)
    new_ds1 <- addmargins(date_serialNumber1, margin = 2)
    
    
    
    # 2. Date and Message
    date_message1 <- table(error_data24N$timestamp, error_data24N$message)
    date_message_withTOT <- addmargins(date_message1, margin = 2)
    
    
    
    
    # 3 Serial Number and Message
    serialNumber_message <- table(error_data24N$message, error_data24N$serialNumber)
    
    
    
    
    
    
    
    
    
    
    
    message_serialNumber <- as.data.frame(error_data24N %>%
                                            group_by(message, serialNumber) %>%
                                            tally())
    
    # 4 Assigning all NAs to 0s in  message_serialNumber for the bar plot
    message_serialNumber[message_serialNumber == 0] <- NA
    
    
    date_serialNo <- as.data.frame(error_data24N %>%
                                     group_by(timestamp, serialNumber) %>%
                                     tally())
    
    date_serialNo$timestamp <- as.Date(date_serialNo$timestamp)
    
    
    
    
    
    
    
    
    
    
    
    
    
    # --------------------------------------------------------------------------
    # 5. Creating datetime var for UTC & NZ an NZ date variable  for plots
    # --------------------------------------------------------------------------
    
    error_data24N$timestampUTC <- as.POSIXct(error_data24N$timestamp, 
                                             origin = '2023-01-01')
    error_data24N$timestampNZ <- format(error_data24N$timestampUTC, 
                                        tz = "Pacific/Auckland", usetz = TRUE)
    error_data24N$timestamp <- as.Date(error_data24N$timestampNZ)
    
    
    
    
    allowedG23 <- unique(message_serialNumber$message[1:nrow(message_serialNumber)])
    updateSelectizeInput(session = getDefaultReactiveDomain(),"VariablesG0", 
                         choices = allowedG23, selected = allowedG23[1])
    updateSelectizeInput(session = getDefaultReactiveDomain(),"VariablesG3", 
                         choices = allowedG23, selected = allowedG23[1])
    updateSelectizeInput(session = getDefaultReactiveDomain(),"VariablesG5", 
                         choices = allowedG23, selected = allowedG23[1])
    
    allowedG24 <- unique(message_serialNumber$serialNumber[1:nrow(message_serialNumber)])
    updateSelectizeInput(session = getDefaultReactiveDomain(),"VariablesG4", 
                         choices = allowedG24, selected = allowedG24[1])
    updateSelectizeInput(session = getDefaultReactiveDomain(),"VariablesG6", 
                         choices = allowedG24, selected = allowedG24[1])
    
    
    showNotification(id = "Done", ui = "Data is loaded. Proceed to visualisations", 
                     duration = 5, type = "message")
    
    error_data24N
    
  })
  
  
  
  
  
  # --------------------------------------------
  # HEADINGS WITH DATES
  # --------------------------------------------
  
  output$heading1 <- renderPrint({
    cat(
      paste0("Summaries for the period ", getdata()$timestamp[nrow(getdata())],
             " to ", getdata()$timestamp [1])
    )
  })
  
  output$heading2 <- renderPrint({
    cat(
      paste0("Visualisations for the period ", getdata()$timestamp[nrow(getdata())],
             " to ", getdata()$timestamp[1])
    )
  })
  
  
  
  
  
  # -----------------------------
  # SUMMARIES AND TABLES
  # ------------------------------
  
  
  
  
  output$datatable1 <- DT::renderDataTable({
    DT::datatable(data = as.data.frame(getdata()), options = list(scrollX = TRUE))
    
  }) 
  
  
  
  
  output$SummaryA3 <- renderPrint({
    str(getdata())
  }) 
  
  
  output$message_counts <- renderPrint({
    messages_freq <- 
      getdata() %>% 
      group_by(message) %>%
      dplyr::summarise(Count = n()) %>%
      arrange(desc(Count))
    messages_freq
    
  })
  
  
  # -----------------------------------------------------
  # Messages, serial Nos data table
  # -----------------------------------------------------
  
  output$counts <- renderDataTable({
    machine_freq <- 
      getdata() %>% 
      
      mutate(message = as.character(message)) %>% 
      rowwise() %>% 
      #Reducing the text length to only 1400 characters
      mutate(message = case_when(nchar(message) > 1400 ~ 
                                   paste(str_sub(message, 1, 1400), "..."),
                                 nchar(message) <= 1400 ~ message))  %>%
      
      group_by(serialNumber,  message, machineId) %>%
      dplyr::summarise(Count = n(), .groups = "drop") %>%
      arrange(desc(Count))
    
    url1 = paste0("https://payments-cloud.web.app/machine/" ,
                  machine_freq$machineId)
    machine_freq$machine_url <- paste0("<a href='",url1,"'>link</a>")
    machine_freq$machineId <- NULL    # Removing the machineId column 
    
    DT::datatable(as.data.frame(machine_freq), escape = FALSE, 
                  extensions = 'Buttons',
                  options = list(
                    scrollX = TRUE,
                    dom = 'Bfrtip',
                    buttons = list('copy', 'csv', 'excel', 'print', list(
                      extend = 'pdf',
                      pageSize = 'A3',             # Choosing A3 page size
                      orientation = 'landscape'
                    ))
                  ))
    
  }, escape = FALSE)
  
  # -----------------------------------------------
  # Notes below the Messages, serial Nos data table
  # -----------------------------------------------
  
  output$messagesDTnotes <- renderPrint({
    cat(paste0(
      "
      The table shows a summary of the errors reported by each machine.
      Here we can sort the messages and select the machines with the highest counts.
      
      Click on the linl to get directed to the machine in payments web app
      
      NOTE: OPEN THE LINK IN A NEW TAB
      "
    ))
  })
  
  
  # -----------------------------------------------------
  # Date, serial Nos data table
  # -----------------------------------------------------
  
  output$date_serialNo <- renderDataTable({
    
    date_serialNo <- as.data.frame(getdata() %>%
                                     group_by(timestamp) %>%
                                     tally())
    d <-  date_serialNo %>% 
      arrange(desc(timestamp))
    daily_totals <- d 
    
    colnames(daily_totals) <- c("DATE", "Number of Errors")
    datatable(as.data.frame(daily_totals), extensions = 'Buttons', 
              options = list(
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = list('copy', 'csv', 'excel', 'print', list(
                  extend = 'pdf',
                  pageSize = 'A4',
                  orientation = 'landscape'
                ))
              ))
    
  })
  
  
  
  
  
  
  
  
  
  
  # --------------------------------
  #          NEW BAR PLOT
  # --------------------------------
  
  output$newBar <- renderHighchart({
    MSGG = input$VariablesG0
    
    
    new1 <- as.data.frame(getdata() %>%
                            group_by(message, serialNumber) %>%
                            filter(message == MSGG) %>%
                            tally())
    hchart(new1,
           "column", hcaes(x = serialNumber, y = n)) %>%  
      hc_title(text = paste0("Number of machines reporting the Error: ",
                             str_trunc(MSGG, 70, "right"), "  =    ", 
                             NROW(unique(new1$serialNumber)))) %>%
      hc_xAxis(title = list(text = "Machine Serial Number")) %>%
      hc_yAxis(title = list(text = "Count"))
    
    
    
    
    
    
  }) 
  
  
  
  
  
  
  
  
  
  
  #-----------------------------------------
  # HIGHCHART MESSAGE LINE GRAPH
  #-----------------------------------------
  output$highchartlineMsg <- renderHighchart({
    MSGG = input$VariablesG3
    
    dateMsg <- getdata() %>% 
      group_by(timestamp, message) %>%
      filter(message == MSGG) %>%
      tally()
    dateMsg <- as.data.frame(dateMsg)
    dateMsg1 <- data.frame(getdata()$timestamp, getdata()$message,  
                           rep(0, nrow(getdata())))
    
    colnames(dateMsg1) <- c("timestamp", "message", "count")
    
    
    dateMsg1 <- cbind("id" = rownames(dateMsg1), dateMsg1)
    dateMsg <- cbind("id" = rownames(dateMsg), dateMsg)
    
    
    mergedateMsg <- merge(dateMsg, dateMsg1, all = TRUE)  %>%
      filter(message == MSGG) %>% 
      group_by(timestamp) %>%
      
      summarise(tot = n())
    
    mydates <- data.frame(seq(as.Date(getdata()$timestamp[nrow(getdata())]), 
                              as.Date(getdata()$timestamp[1]),  "day"))
    
    colnames(mydates) <- "timestamp"
    
    
    
    myNewMerged1 <- merge(mydates, mergedateMsg, all = TRUE)
    myNewMerged1[is.na(myNewMerged1)] <- 0
    
    
    
    hchart(myNewMerged1,
           "line", color = "green", hcaes(x = timestamp,
                                          y = ifelse(tot > 0, tot-1, tot)))  %>% 
      #hcharts y values were 1 above the correct ones
      
      hc_title(text = paste0("Error Message: ", str_trunc(MSGG, 70, "right"))) %>%
      hc_xAxis(title = list(text = "Date")) %>%
      hc_yAxis(title = list(text = "Count"))
    
  }) 
  
  
  
  
  
  #-----------------------------------------
  # HIGHCHART SERIAL NUMBER LINE GRAPH
  #-----------------------------------------
  
  output$highchartlineSN <- renderHighchart({
    SER_NO = input$VariablesG4
    dateSN<- getdata() %>% 
      group_by(timestamp, serialNumber) %>%
      filter(serialNumber == SER_NO) %>%
      tally()
    dateSN<- as.data.frame(dateSN)
    dateSN1 <- data.frame(getdata()$timestamp, getdata()$serialNumber,  
                          rep(0, nrow(getdata())))
    
    colnames(dateSN1) <- c("timestamp", "serialNumber", "count")
    
    
    dateSN1 <- cbind("id" = rownames(dateSN1), dateSN1)
    dateSN <- cbind("id" = rownames(dateSN), dateSN)
    
    
    mergedateSN <- merge(dateSN, dateSN1, all = TRUE)  %>%
      filter(serialNumber == SER_NO) %>% 
      group_by(timestamp) %>%
      
      summarise(tot = n())
    
    
    mydates <- data.frame(seq(as.Date(getdata()$timestamp[nrow(getdata())]),
                              as.Date(getdata()$timestamp[1]),  "day"))
    
    colnames(mydates) <- "timestamp"
    
    
    
    myNewMerged2 <- merge(mydates, mergedateSN, all.x = TRUE)
    myNewMerged2[is.na(myNewMerged2)] <- 0
    
    hchart(myNewMerged2,
           "line", color ="red", hcaes(x = timestamp, 
                                       y = ifelse(tot > 0, tot-1, tot))) %>% 
      # hcharts y values were 1 above the correct ones
      
      hc_title(text = paste0("Error counts for machine: ", SER_NO)) %>%
      hc_xAxis(title = list(text = "Date")) %>%
      hc_yAxis(title = list(text = "Count"))
    
  }) 
  
  
  
  
  
  
  
  
  
  
  
  filedata <- reactive({
    
    infile <- input$file1
    if (is.null(infile)) {
      return(NULL)
    }
    myDF <- fread(infile$datapath)
    return(myDF)
  })
  
  
  
  
  
  # ------------------------------------
  # TIME SERIES DECOMPOSITION
  # -------------------------------------
  
  ts_decompose <- reactive({
    
    MSGG = input$VariablesG5
    
    dateMsg <- getdata() %>% 
      group_by(timestamp, message) %>%
      filter(message == MSGG) %>%
      tally()
    dateMsg <- as.data.frame(dateMsg)
    dateMsg1 <- data.frame(getdata()$timestamp, getdata()$message,  
                           rep(0, nrow(getdata())))
    
    colnames(dateMsg1) <- c("timestamp", "message", "count")
    
    
    dateMsg1 <- cbind("id" = rownames(dateMsg1), dateMsg1)
    dateMsg <- cbind("id" = rownames(dateMsg), dateMsg)
    
    
    mergedateMsg <- merge(dateMsg, dateMsg1, all = TRUE)  %>%
      filter(message == MSGG) %>% 
      group_by(timestamp) %>%
      
      summarise(tot = n())
    
    mydates <- data.frame(seq(as.Date(getdata()$timestamp[nrow(getdata())]), 
                              as.Date(getdata()$timestamp[1]),  "day"))
    
    colnames(mydates) <- "timestamp"
    
    
    
    myNewMerged1 <- merge(mydates, mergedateMsg, all = TRUE)
    myNewMerged1[is.na(myNewMerged1)] <- 0
    
    
    
    
    
    myTS <- ts( myNewMerged1$tot, 
                start =  myNewMerged1$timestamp[1],
                end =  myNewMerged1$timestamp[nrow( myNewMerged1)/2],
                frequency = 2)
    decompose1 <- stl(myTS, s.window = 3, t.window = input$window)
    plot(decompose1, col = "blue", 
         main = paste0("Time series decomposition for error message: ", 
                       str_trunc(MSGG, 70, "right")))
    
  })
  
  
  
  
  
  
  ts_decompose2 <- reactive({
    
    SER_NO = input$VariablesG6
    dateSN <- getdata() %>% 
      group_by(timestamp, serialNumber) %>%
      filter(serialNumber == SER_NO) %>%
      tally()
    dateSN <- as.data.frame(dateSN)
    dateSN1 <- data.frame(getdata()$timestamp, getdata()$serialNumber,  
                          rep(0, nrow(getdata())))
    
    colnames(dateSN1) <- c("timestamp", "serialNumber", "count")
    
    
    dateSN1 <- cbind("id" = rownames(dateSN1), dateSN1)
    dateSN <- cbind("id" = rownames(dateSN), dateSN)
    
    
    mergedateSN <- merge(dateSN, dateSN1, all = TRUE)  %>%
      filter(serialNumber == SER_NO) %>% 
      group_by(timestamp) %>%
      
      summarise(tot = n())
    
    
    mydates <- data.frame(seq(as.Date(getdata()$timestamp[nrow(getdata())]),
                              as.Date(getdata()$timestamp[1]),  "day"))
    
    colnames(mydates) <- "timestamp"
    
    
    
    myNewMerged2 <- merge(mydates, mergedateSN, all.x = TRUE)
    myNewMerged2[is.na(myNewMerged2)] <- 0
    
    myTS2 <- ts( myNewMerged2$tot, 
                 start =  myNewMerged2$timestamp[1],
                 end =  myNewMerged2$timestamp[nrow( myNewMerged2)],
                 frequency = 2)
    decompose22 <- stl(myTS2, s.window = "periodic", t.window = input$window)
    plot(decompose22, col = "blue", 
         main = paste0("Time series decomposition for machine ", SER_NO, 
                       " error counts"))
    
    
    
  })
  
  # Return the requested graph
  graphInput <- reactive({
    switch(input$graph,
           "messages" = ts_decompose(),
           "machine counts" = ts_decompose2()  
    )
    
  })
  
  output$selected_graph <- renderPlot({ 
    graphInput()
  })
  
  
  
  
  
  
  # ----------------------------------------------------------------------------
  # Notes under the Time Series decomposition plot
  # ----------------------------------------------------------------------------
  
  output$ts_decomposeNOTES <- renderPrint({
    MSGG = input$VariablesG3
    cat(paste0(
      "
     The daily sales for the error have been decomposed to show
     the actual plot (data),
     the seasonal component,
     the trend component and
     the remainder (residuals) component.
     
     Use the slider above to smoothen the trend.
     "
    ))
  }) 
  
  
  
  
})

