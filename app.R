library(shiny) #version 1.8.0
library(rxode2) #version 2.1.1
library(foreach) #version 1.5.2
library(doParallel) #version 1.0.14
library(ggplot2) #version 3.4.1
library(lotri) #version 0.4.3
library(datasets) #version 4.2.1
library(tidyverse) #version 2.0.0
library(tidyr) #version 1.3.0
library(data.table) #version 1.14.8
library(shinyWidgets) #version 0.8.1
library(rhandsontable) #version 0.3.8
library(shinydashboard) #version 0.7.2
library(optimx) #version 2023-10.21

# Define user interface
ui <- dashboardPage(
  # Define the header with a title
  dashboardHeader(title = "Tacrolimus MIPD"),
  
  # Define the sidebar with various input fields
  dashboardSidebar(
    width = 300,
    fluidRow(
      # eGFR input field
      column(12, style = "display: flex; align-items: center; margin-bottom: -10px;",
             tags$div(style = "flex: 2; padding-left: 15px;", tags$label("eGFR (mL/min/1.73 m²)", `for` = "egfr")),
             tags$div(style = "flex: 1; margin-left: -10px;", numericInput("egfr", NULL, value = 50, width = "100%"))
      ),
      # Hematocrit input field
      column(12, style = "display: flex; align-items: center; margin-bottom: -10px;",
             tags$div(style = "flex: 2; padding-left: 15px;", tags$label("Hematocrit (%)", `for` = "hct")),
             tags$div(style = "flex: 1; margin-left: -10px;", numericInput("hct", NULL, value = 40, width = "100%"))
      )
    ),
    fluidRow(
      # Haptoglobin input field
      column(12, style = "display: flex; align-items: center; margin-bottom: -10px;",
             tags$div(style = "flex: 2; padding-left: 15px;", tags$label("Haptoglobin (g/L)", `for` = "hp")),
             tags$div(style = "flex: 1; margin-left: -10px;", numericInput("hp", NULL, value = 1.4, width = "100%"))
      ),
      # Total cholesterol input field
      column(12, style = "display: flex; align-items: center; margin-bottom: -10px;",
             tags$div(style = "flex: 2; padding-left: 15px;", tags$label("Total cholesterol (mmol/L)", `for` = "chol")),
             tags$div(style = "flex: 1; margin-left: -10px;", numericInput("chol", NULL, value = 4.5, width = "100%"))
      ),
      # Vitamin B12 input field
      column(12, style = "display: flex; align-items: center; margin-bottom: -10px;",
             tags$div(style = "flex: 2; padding-left: 15px;", tags$label("Vitamin B12 (pmol/L)", `for` = "vb12")),
             tags$div(style = "flex: 1; margin-left: -10px;", numericInput("vb12", NULL, value = 320, width = "100%"))
      )
    ),
    fluidRow(
      # Tacrolimus brand radio buttons
      column(12, style = "margin-bottom: -10px;", radioButtons("brand", "Tacrolimus brand", choices = c("Prograf" = 1, "Envarsus" = 2, "Advagraf" = 3), inline = TRUE)),
      # Bioassay radio buttons
      column(12, style = "display: flex; align-items: center; margin-bottom: -10px;",
             tags$div(style = "flex: 0.47; padding-left: 15px;", tags$label("Bioassay")),
             tags$div(style = "flex: 1.8;", radioButtons("bioassay", NULL, choices = c("LCMS" = 1, "CMIA" = 2, "LSIR" = 3), inline = TRUE))
      ),
      # Fast status radio buttons
      column(12, style = "display: flex; align-items: center; margin-bottom: -10px;",
             tags$div(style = "flex: 0.48; padding-left: 15px;", tags$label("Fast")),
             tags$div(style = "flex: 1.9;", radioButtons("fast", NULL, choices = c("Yes" = 1, "No" = 0), inline = TRUE))
      ),
      # Mycophenolic acid radio buttons
      column(12, style = "display: flex; align-items: center; margin-bottom: -10px;", 
             tags$div(style = "flex: 1; padding-left: 15px; padding-right: -5px;", tags$label("Mycophenolic acid")),
             tags$div(style = "flex: 1.5; padding-left: -20px;", radioButtons("mpa", NULL, choices = c("Use" = 1, "Not use" = 0), inline = TRUE))
      ),
      # Vitamin D radio buttons
      column(12, style = "display: flex; align-items: center; margin-bottom: -10px;",
             tags$div(style = "flex: 1; padding-left: 15px; padding-right: -5px;", tags$label("Vitamin D")),
             tags$div(style = "flex: 1.5; padding-left: -20px;", radioButtons("vd", NULL, choices = c("Use" = 1, "Not use" = 0), inline = TRUE))
      )
    ),
    fluidRow(
      # Therapeutic window input fields
      column(12, style = "margin-bottom: -10px;",
             div(
               style = "display: flex; align-items: center;",
               HTML('<div style="margin-right: 5px; flex-shrink: 0; width: 130px;">'),
               numericInput("therapeuticWindowLow", "Therapeutic", value = 10),
               HTML('</div>'),
               HTML("&ndash;"),
               HTML('<div style="margin-left: 5px; margin-right: 5px; flex-shrink: 0; width: 90px;">'),
               numericInput("therapeuticWindowHigh", "Window", value = 15),
               HTML('</div>'),
               HTML('<div style="flex-shrink: 1;">ng/mL</div>')
             )
      )
    ),
    fluidRow(
      # Date inputs for surgery and dosing times
      column(12, style = "margin-bottom: -10px;", airDatepickerInput(
        inputId = "airSurgery",
        label = "Surgery Date",
        timepicker = FALSE,  
        value = format(Sys.Date() - 1)
      )),
      column(12, style = "margin-bottom: -10px;", airDatepickerInput(
        inputId = "airstarttime",
        label = "Next Dose Start at",
        timepicker = TRUE,
        value = format(Sys.Date(), "%Y-%m-%d 08:00:00")
      )),
      column(12, style = "margin-bottom: -10px;", airDatepickerInput(
        inputId = "airendtime",
        label = "Achieve Window at",
        timepicker = TRUE,
        value = format(Sys.Date() + 5, "%Y-%m-%d 08:00:00")
      )),
      column(12, style = "margin-bottom: -10px;", numericInput("dosingInterval", "Dosing Interval (h)", value = 12)),
      column(12, style = "margin-bottom: -10px;", actionButton("submit", "Submit"))
    ),
    # Add script for handling date input blur events
    tags$head(tags$script(HTML("
      $(document).on('shiny:connected', function(event) {
        $('#airSurgery, #airstarttime, #airendtime').on('blur', function(e) {
          var dateValue = e.target.value;
          Shiny.setInputValue(e.target.id, dateValue);
        });
      });
    ")))
  ),
  
  # Define the main body of the dashboard
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Style for the retractable secondary sidebar */
        .left-second-sidebar {
          position: absolute;
          top: 50px;
          bottom: 0;
          left: 0; /* Align with the main sidebar */
          width: 320px;
          padding: 15px;
          overflow-y: auto;
          background-color: #f5f5f5;
          z-index: 800; /* Lower z-index than the main sidebar */
          transform: translateX(-100%); /* Initially hidden */
          transition: transform 0.5s ease;
        }
        .left-second-sidebar.visible {
          transform: translateX(300px); /* Move when expanded */
        }
        /* Style for the toggle button */
        .toggle-sidebar-btn {
          position: absolute;
          top: 55px;
          left: 300px; /* Between main and secondary sidebar */
          cursor: pointer;
          z-index: 1001; /* Higher z-index to ensure clickable */
        }
        /* Adjust content area margin-left */
        .content-wrapper {
          margin-left: 300px; /* Same as main sidebar width */
          transition: margin-left 0.5s ease;
        }
        .content-wrapper.expanded {
          margin-left: 620px; /* Increase with secondary sidebar width */
        }
      ")),
      # Script for toggle button functionality
      tags$script(HTML("
        $(document).ready(function() {
          $('.toggle-sidebar-btn').on('click', function() {
            $('.left-second-sidebar').toggleClass('visible');
            $('.content-wrapper').toggleClass('expanded');
          });
        });
      "))
    ),
    # Define the retractable secondary sidebar content
    tags$div(
      class = "left-second-sidebar",
      fluidRow(
        column(12, br()),
        column(12, br()),
        column(12, strong("Dose Record (mg)")),
        column(12, rHandsontableOutput("table1")),
        column(4, actionLink("addRow1", "Add row")),
        column(5, actionLink("deleteRow1", "Delete row")),
        column(12, br()),
        column(12, strong("Sample Record (ng/mL)")),
        column(12, rHandsontableOutput("table2")),
        column(4, actionLink("addRow2", "Add row")),
        column(5, actionLink("deleteRow2", "Delete row")),
        column(12, br()),
        column(12, strong("Steady state TDM Record")),
        column(12, rHandsontableOutput("table3")),
        column(4, actionLink("addRow3", "Add row")),
        column(5, actionLink("deleteRow3", "Delete row"))
      )
    ),
    # Toggle button for the secondary sidebar
    tags$div(
      class = "toggle-sidebar-btn",
      actionButton("toggleSidebar", label = "Add Events", icon = icon("bars"))
    ),
    # Main panel content
    fluidRow(
      column(12, 
             wellPanel(
               HTML('<h4>Recommended Dose is:</h4>'),
               textOutput("recommendedose", container = span)
             )
      ),
      column(12, plotOutput("ctCurve")),
      column(12,
             actionButton("doseIncrease", "Dose+"),
             actionButton("doseDecrease", "Dose-"),
             wellPanel(
               HTML('<h4>Current Dose is:</h4>'),
               textOutput("CurrentDose", container = span)
             )),
      column(12,
             p("This is a tacrolimus individualized dosage recommendation platform for adult lung transplant patients (20240723 update)."),
             p("Abbreviations：LCMS, liquid chromatography tandem mass spectrometry; CMIA, chemiluminescent microparticle immunoassay; LSIR, latex-sticky immunity repression."),
             p("The PopPK model we used for calculating initial dose (dose predicted without TDM results) was developed by Koomen et al. (2023). https://pubmed.ncbi.nlm.nih.gov/37306899/."),
             p("The PopPK model we used for calculating adjusted dose (dose predicted with TDM results) was developed by Monchaud et al. (2012). https://pubmed.ncbi.nlm.nih.gov/22339449/."))
    )
  )
)

server <- function(input, output, session) {
 
  # Set a value content for user input data
  values <- reactiveValues(
    data1 = data.frame(Date = as.Date(c()), 
                       Time = c(), 
                       Dose = as.numeric()),
    data2 = data.frame(Date = as.Date(character()), 
                       Time = character(), 
                       Conc = numeric()),
    data3 = data.frame(Dose = as.numeric(), 
                       Interval = as.numeric(), 
                       Conc = as.numeric(),
                       Time = as.numeric())
  )
  
  # User input table1 for inputting dosing information
  output$table1 <- renderRHandsontable({
    rhandsontable(values$data1, colHeaders = c("Date", "Time", "Dose"))
  })
  
  # User input table2 for inputting sampling information
  output$table2 <- renderRHandsontable({
    rhandsontable(values$data2, colHeaders = c("Date", "Time", "Conc"))
  })
  
  # User input table3 for inputting steady state TDM information
  output$table3 <- renderRHandsontable({
    rhandsontable(values$data3, colHeaders = c("Dose", "Interval", "Conc", "Time"))
  })
  
  # If the table content changes, send to server immediately
  observeEvent(input$table1$changes$changes, {
    changes <- input$table1$changes$changes
    if (!is.null(changes)) {
      values$data1 <- hot_to_r(input$table1)
    }
  })
  
  observeEvent(input$table2$changes$changes, {
    changes <- input$table2$changes$changes
    if (!is.null(changes)) {
      values$data2 <- hot_to_r(input$table2)
    }
  })
  
  observeEvent(input$table3$changes$changes, {
    changes <- input$table3$changes$changes
    if (!is.null(changes)) {
      values$data3 <- hot_to_r(input$table3)
    }
  })
  
  # Add a new row to table1
  observeEvent(input$addRow1, {
    if (nrow(values$data1) == 0) {
      values$data1 <- data.frame(Date = as.Date(Sys.Date() - 1), Time = "6:00", Dose = 0.5)
    } else {
      # Find last row
      last_row <- tail(values$data1, n = 1)
      
      # Get the date, time, and dose from the last row
      last_date <- last_row$Date
      last_time <- last_row$Time
      last_dose <- last_row$Dose
      
      # Convert date and time to POSIXct object
      last_datetime <- as.POSIXct(paste(last_date, last_time))
      
      # Get dosing interval value
      dosing_interval_hours <- input$dosingInterval
      
      # Add specified number of hours
      new_datetime <- last_datetime + hours(dosing_interval_hours)
      new_datetime2 <- as.character(new_datetime)
      
      # Add a new row to values$data1
      new_row <- data.frame(Date = as.Date(new_datetime2), Time = format(new_datetime, "%H:%M"), Dose = last_dose)
      values$data1 <- rbind(values$data1, new_row)
    }
  })
  
  # Add a new row to table2
  observeEvent(input$addRow2, {
    if (nrow(values$data2) == 0) {
      if (nrow(values$data1) == 0) {
        values$data2 <- data.frame(Date = as.Date(Sys.Date() - 1), Time = "5:30", Conc = 10)
      } else {
        # Find last row
        last_row <- tail(values$data1, n = 1)
        
        # Get the date and time from the last row
        last_date <- last_row$Date
        last_time <- last_row$Time
        
        # Convert date and time to POSIXct object
        last_datetime <- as.POSIXct(paste(last_date, last_time))
        
        # Get dosing interval value
        dosing_interval_hours <- input$dosingInterval
        
        # Add specified number of hours minus 30 minutes
        new_datetime <- last_datetime + hours(dosing_interval_hours) - minutes(30)
        new_datetime2 <- as.character(new_datetime)
        
        # Add a new row to values$data2
        new_row <- data.frame(Date = as.Date(new_datetime2), Time = format(new_datetime, "%H:%M"), Conc = 10)
        values$data2 <- rbind(values$data2, new_row)
      }
    } else {
      # Find last row
      last_row <- tail(values$data2, n = 1)
      
      # Get the date, time, and concentration from the last row
      last_date <- last_row$Date
      last_time <- last_row$Time
      last_Conc <- last_row$Conc
      
      # Convert date and time to POSIXct object
      last_datetime <- as.POSIXct(paste(last_date, last_time))
      
      # Get dosing interval value
      dosing_interval_hours <- input$dosingInterval * 2
      
      # Add specified number of hours
      new_datetime <- last_datetime + hours(dosing_interval_hours)
      new_datetime2 <- as.character(new_datetime)
      
      # Add a new row to values$data2
      new_row <- data.frame(
        Date = as.Date(new_datetime2), # Extract date part
        Time = format(new_datetime, "%H:%M"), # Extract time part
        Conc = last_Conc
      )
      values$data2 <- rbind(values$data2, new_row)
    }
  })
  
  # Add a new row to table3
  observeEvent(input$addRow3, {
    if (nrow(values$data3) == 0) {
      values$data3 <- data.frame(Dose = 2, 
                                 Interval = 12, 
                                 Conc = 5,
                                 Time = 12)
    } else {
      # Find last row
      last_row <- tail(values$data3, n = 1)
      
      # Add a new row with the same data as the last row
      values$data3 <- rbind(values$data3, last_row)
    }
  })
  
  # Delete the last row from table1
  observeEvent(input$deleteRow1, {
    if (nrow(values$data1) > 0) {
      values$data1 <- values$data1[-nrow(values$data1), ]
    }
  })
  
  # Delete the last row from table2
  observeEvent(input$deleteRow2, {
    if (nrow(values$data2) > 0) {
      values$data2 <- values$data2[-nrow(values$data2), ]
    }
  })
  
  # Delete the last row from table3
  observeEvent(input$deleteRow3, {
    if (nrow(values$data3) > 0) {
      values$data3 <- values$data3[-nrow(values$data3), ]
    }
  })
  # Function to convert time format
  convertTime <- function(time) {
    if (grepl(":", time)) {
      # Time is already in "HH:MM" format
      return(time)
    } else {
      # Convert numeric time to "HH:MM" format
      hours <- floor(as.numeric(time))
      minutes <- round((as.numeric(time) - hours) * 60)
      return(sprintf("%02d:%02d", hours, minutes))
    }
  }
  
  # Reactive expression to get Surgery Time input
  SurgeryTime <- reactive({
    input$airSurgery
  })
  
  # Reactive expression to get Start Time input
  StartTime <- reactive({
    input$airstarttime 
  })
  
  # Reactive expression to get End Time input
  EndTime <- reactive({
    input$airendtime 
  })
  
  # Initialize data frames to collect data
  collectedData1 <- data.frame()
  collectedData2 <- data.frame()
  collectedData3 <- data.frame()
  
  # Initialize reactive values for various parameters
  submitData <- reactiveValues(
    Surgerytime = NULL,
    optimalDose = NULL,
    startTime = NULL,
    endTime = NULL,
    starttime = NULL,
    endtime = NULL,
    eta_CL = NULL,
    eta_V = NULL,
    theta = NULL,
    timeDiff = NULL,
    collectedData1 = NULL,
    collectedData2 = NULL,
    collectedData3 = NULL
  )
  
  observeEvent(input$submit, {

    withProgress(message = 'Calculating...', value = 0, {
      # Get and convert input times and dates
      Surgerytime <- as.Date(input$airSurgery)
      starttime <- as.POSIXct(input$airstarttime, format = "%Y-%m-%d %H:%M")
      endtime <- as.POSIXct(input$airendtime, format = "%Y-%m-%d %H:%M")
      
      # Initialize timeDiff
      timeDiff <- NULL
      
      # Calculate the time difference
      if (!is.na(starttime) && !is.na(endtime)) {
        timeDiff <- as.numeric(difftime(endtime, starttime, units = "hours"))
      }
       
      # Ensure collectedData1 is not empty
      if (nrow(values$data1) > 0) {
        collectedData1 <- values$data1 %>%
          filter(Date != 0, Time != 0, Dose != 0) %>%
          mutate(
            # 应用转换函数到 Time 列
            Time = sapply(Time, convertTime),
            DateTime = as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M")
          )
        # 找出最小的时间作为零时刻
        minDateTime <- min(c(starttime, collectedData1$DateTime), na.rm = TRUE)
        collectedData1 <- collectedData1 %>%
          mutate(
            TimeDifference = as.numeric(difftime(DateTime, minDateTime, units = "hours")),
            pod = as.numeric(as.Date(Date)-Surgerytime)
          )
        startTime = as.numeric(difftime(starttime, minDateTime, units = "hours"))
        endTime = as.numeric(difftime(endtime, minDateTime, units = "hours"))
      } 
      else {
        startTime = 0
        endTime = timeDiff
      }
      if (nrow(values$data2) > 0) {
        collectedData2 <- values$data2 %>%
          filter(Date != 0, Time != 0, Conc != 0) %>%
          mutate(
            Time = sapply(Time, convertTime),
            DateTime = as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M")
          ) %>%
          mutate(
            TimeDifference = as.numeric(difftime(DateTime, minDateTime, units = "hours")),
            pod = as.numeric(as.Date(Date)-Surgerytime)
          )
      }
      
      if (nrow(values$data3) > 0) {
        collectedData3 <- values$data3 
      }
      # Calculate the recommended dose
      recommendedose <- calculaterecommendedose(
        input$egfr, 
        input$hct, 
        input$hp, 
        input$chol, 
        input$vb12, 
        input$brand,
        input$fast, 
        input$mpa, 
        input$vd,
        input$therapeuticWindowLow, 
        input$therapeuticWindowHigh, 
        input$dosingInterval,
        input$bioassay,
        collectedData1,
        collectedData2,
        collectedData3,
        Surgerytime,
        startTime,
        endTime,
        starttime,
        endtime,
        timeDiff
      )

      # Display the recommended dose
      output$recommendedose <- renderText({
        paste0(recommendedose$bestDose, " mg\n", " q", input$dosingInterval, "h")
      })
      
      # Simulate a progress bar for a lengthy calculation
      for (replicate in 1:50) {
        setProgress(replicate / 100)
        Sys.sleep(0.005)  # Simulate calculation delay
      }
      
      # Simulate concentration results
      result_plot <- simulateConc(
        input$egfr, 
        input$hct, 
        input$hp, 
        input$chol, 
        input$vb12, 
        input$brand,
        input$fast, 
        input$mpa, 
        input$vd,
        input$dosingInterval, 
        input$bioassay,
        recommendedose$bestDose,
        collectedData1,
        collectedData2,
        collectedData3,
        Surgerytime,
        startTime,
        endTime,
        starttime,
        endtime,
        recommendedose$eta_CL,
        recommendedose$eta_V,
        recommendedose$theta
      )
      
      # Render the plot
      output$ctCurve <- renderPlot({
        plot_result(
          result_plot,
          input$dosingInterval,
          input$therapeuticWindowLow, 
          input$therapeuticWindowHigh,
          starttime,
          endtime,
          timeDiff,
          collectedData1,
          collectedData2,
          collectedData3
        )
      })
      
      # Continue simulating progress bar
      for (replicate in 51:100) {
        setProgress(replicate / 100)
        Sys.sleep(0.05)  # Simulate calculation delay
      }
      
      # Assign values to reactiveValues
      submitData$Surgerytime <- Surgerytime
      submitData$optimalDose <- recommendedose$bestDose
      submitData$startTime <- startTime
      submitData$endTime <- endTime
      submitData$starttime <- starttime
      submitData$endtime <- endtime
      submitData$eta_CL <- recommendedose$eta_CL
      submitData$eta_V <- recommendedose$eta_V
      submitData$theta <- recommendedose$theta
      submitData$timeDiff <- timeDiff
      submitData$collectedData1 <- collectedData1
      submitData$collectedData2 <- collectedData2
      submitData$collectedData3 <- collectedData3
    })
  })
  
  # Event to handle dose increase
  observeEvent(input$doseIncrease, {
    req(submitData$optimalDose)  # Ensure optimalDose is available
    submitData$optimalDose <- submitData$optimalDose + 0.25
    
    # Simulate concentration with the new dose
    result_plot <- simulateConc(
      input$egfr, 
      input$hct, 
      input$hp, 
      input$chol, 
      input$vb12, 
      input$brand,
      input$fast, 
      input$mpa, 
      input$vd, 
      input$dosingInterval,
      input$bioassay,
      submitData$optimalDose,
      submitData$collectedData1,
      submitData$collectedData2,
      submitData$collectedData3,
      submitData$Surgerytime,
      submitData$startTime,
      submitData$endTime,
      submitData$starttime,
      submitData$endtime,
      submitData$eta_CL,
      submitData$eta_V,
      submitData$theta
    )
    
    # Update current dose display
    output$CurrentDose <- renderText({
      paste0(submitData$optimalDose, " mg\n", " q", input$dosingInterval, "h")
    })
    
    # Render updated concentration plot
    output$ctCurve <- renderPlot({
      plot_result(
        result_plot,
        input$dosingInterval,
        input$therapeuticWindowLow, 
        input$therapeuticWindowHigh,
        submitData$starttime,
        submitData$endtime,
        submitData$timeDiff,
        submitData$collectedData1,
        submitData$collectedData2,
        submitData$collectedData3
      )
    })
  })
  
  # Event to handle dose decrease
  observeEvent(input$doseDecrease, {
    req(submitData$optimalDose)  # Ensure optimalDose is available
    submitData$optimalDose <- submitData$optimalDose - 0.25
    
    # Simulate concentration with the new dose
    result_plot <- simulateConc(
      input$egfr, 
      input$hct, 
      input$hp, 
      input$chol, 
      input$vb12, 
      input$brand,
      input$fast, 
      input$mpa,
      input$vd,
      input$dosingInterval, 
      input$bioassay,
      submitData$optimalDose,
      submitData$collectedData1,
      submitData$collectedData2,
      submitData$collectedData3,
      submitData$Surgerytime,
      submitData$startTime,
      submitData$endTime,
      submitData$starttime,
      submitData$endtime,
      submitData$eta_CL,
      submitData$eta_V,
      submitData$theta
    )
    
    # Update current dose display
    output$CurrentDose <- renderText({
      paste0(submitData$optimalDose, " mg\n", " q", input$dosingInterval, "h")
    })
    
    # Render updated concentration plot
    output$ctCurve <- renderPlot({
      plot_result(
        result_plot,
        input$dosingInterval,
        input$therapeuticWindowLow, 
        input$therapeuticWindowHigh,
        submitData$starttime,
        submitData$endtime,
        submitData$timeDiff,
        submitData$collectedData1,
        submitData$collectedData2,
        submitData$collectedData3
      )
    })
    
  # Render updated concentration plot
  output$ctCurve <- renderPlot({
    plot_result(
      result_plot,
      input$dosingInterval,
      input$therapeuticWindowLow, 
      input$therapeuticWindowHigh,
      submitData$starttime,
      submitData$endtime,
      submitData$timeDiff,
      submitData$collectedData1,
      submitData$collectedData2,
      submitData$collectedData3
    )
  })
})
  
  # Function for calculating recommended dose for patients
  calculaterecommendedose <- function(
    egfr, 
    hct, 
    hp, 
    chol, 
    vb12, 
    brand,
    fast, 
    mpa, 
    vd,
    therapeuticWindowLow, 
    therapeuticWindowHigh, 
    dosingInterval,
    bioassay,
    collectedData1,
    collectedData2,
    collectedData3,
    Surgerytime,
    startTime,
    endTime,
    starttime,
    endtime,
    timeDiff) {
 
    # Define brand-related parameters
    if (brand == 1) {
      brand_F <- -0.64
      brand_fast <- 1
    } 
    if (brand == 2){
      brand_F <- -0.74
      brand_fast <- 0
    }
    if (brand == 3){
      brand_F <- -0.62
      brand_fast <- 0
    }
    
    # Convert inputs to numeric
    fast <- as.numeric(fast)
    mpa  <- as.numeric(mpa)
    vd   <- as.numeric(vd)
    
    # Define initial population parameters and variables
    POPpar <- c(14.7, 636, 4.48)
    CV_CL <- 0.201
    eta_CL <- NULL
    eta_V <- NULL
    dose_times <- seq(startTime, endTime, by = dosingInterval)
    specificTime <- startTime + timeDiff %/% dosingInterval * dosingInterval - 0.001
    therapeuticWindowMid <- (therapeuticWindowLow + therapeuticWindowHigh) / 2
    dosingEvents <- seq(from = starttime, to = endtime, by = paste(dosingInterval, "hours"))
    doseOptions <- seq(0, 12, by = 0.25)
    closestDose <- NULL
    minDifference <- Inf
    Depot <- 0
    Alast <- 0
    Depot_startTime <- 0
    Alast_startTime <- 0
    
    # If steady state sample information is available, process it first
    # Here, model of Monchaud et al. (2012) is used for calculating recommended dose
    if (nrow(collectedData3) != 0) {
      ev <- NULL
      sample_time <- 0
      newdosetime <- endTime - startTime
      
      # Process each row of steady state sample data
      for (i in 1:nrow(collectedData3)) {
        interval <- collectedData3$Interval[i]
        dose_times <- seq(0, 120, by = interval)
        dose_times <- dose_times + sample_time
        # Ensure the length of the amt vector matches the length of the time vector
        amt_vector <- rep(collectedData3$Dose[i], length(dose_times))
        ev <- rbind(ev, et(time = dose_times, amt = amt_vector, cmt = "depot1"))
        sample_time <- tail(dose_times, n = 1) + collectedData3$Time[i]
        # Add sampling time to the event
        ev$add.sampling(sample_time) # Assuming this is the correct method call
      }
      
      # Define the model based on the bioassay type
      if (bioassay == 1) {
        mod1 <- rxode2({
          KA = indKA; 
          CL = indCL; 
          V2 = indV2; 
          V3 = indV3; 
          Q  = indQ;
          F1 = indF1;
          Con = centr / V2
          Conc = Con * 1000
          d/dt(depot1) = -F1 * KA * depot1;
          d/dt(depot2) = F1 * KA * depot1 - KA * depot2;
          d/dt(depot3) = KA * depot2 - KA * depot3;
          d/dt(depot4) = KA * depot3 - KA * depot4;
          d/dt(depot5) = KA * depot4 - KA * depot5;
          d/dt(centr) = KA * depot5 - CL * Con - Q / V2 * centr + Q / V3 * perp;
          d/dt(perp) = Q / V2 * centr - Q / V3 * perp
        })
      }
      if (bioassay == 2) {
        mod1 <- rxode2({
          KA = indKA; 
          CL = indCL; 
          V2 = indV2; 
          V3 = indV3; 
          Q  = indQ;
          F1 = indF1;
          Con = centr / V2
          Conc = Con * 1000 * 1.2 + 0.14
          d/dt(depot1) = -F1 * KA * depot1;
          d/dt(depot2) = F1 * KA * depot1 - KA * depot2;
          d/dt(depot3) = KA * depot2 - KA * depot3;
          d/dt(depot4) = KA * depot3 - KA * depot4;
          d/dt(depot5) = KA * depot4 - KA * depot5;
          d/dt(centr) = KA * depot5 - CL * Con - Q / V2 * centr + Q / V3 * perp;
          d/dt(perp) = Q / V2 * centr - Q / V3 * perp
        })
      }
      if (bioassay == 3) {
        mod1 <- rxode2({
          KA = indKA; 
          CL = indCL; 
          V2 = indV2; 
          V3 = indV3; 
          Q  = indQ;
          F1 = indF1;
          Con = centr / V2
          Conc = (Con * 1000 - 0.206) / 0.872
          d/dt(depot1) = -F1 * KA * depot1;
          d/dt(depot2) = F1 * KA * depot1 - KA * depot2;
          d/dt(depot3) = KA * depot2 - KA * depot3;
          d/dt(depot4) = KA * depot3 - KA * depot4;
          d/dt(depot5) = KA * depot4 - KA * depot5;
          d/dt(centr) = KA * depot5 - CL * Con - Q / V2 * centr + Q / V3 * perp;
          d/dt(perp) = Q / V2 * centr - Q / V3 * perp
        })
      }
      
      # Define the covariance matrix for random effects
      omega <- matrix(c(0.182, 0, 0, 0, 0, 0, 0, 0,
                        0, 0.288, 0, 0, 0, 0, 0, 0,
                        0, 0, 0.168, 0, 0, 0, 0, 0,
                        0, 0, 0, 1.588, 0, 0, 0, 0,
                        0, 0, 0, 0, 0.514, 0, 0, 0,
                        0, 0, 0, 0, 0, 0.210, 0, 0,
                        0, 0, 0, 0, 0, 0, 0.219, 0,
                        0, 0, 0, 0, 0, 0, 0, 0.569), 8, 8)
      
      # Define the typical value parameters and residual error
      TVpar <- c(7.06, 17.5, 136, 529, 41.1, 1)
      sigma <- c(0.005, 1.6)
      init_eta <- c(0, 0, 0, 0, 0, 0, 0, 0)
      
      # Objective function for parameter optimization
      obj.TAC <- function(eta, omega, sigma, TVpar, mod1, ev, collectedData3) {
        theta <- c(indKA = TVpar[1] * exp(eta[1] + eta[6]), 
                   indCL = TVpar[2] * exp(eta[2] + eta[7]), 
                   indV2 = TVpar[3] * exp(eta[3] + eta[8]), 
                   indV3 = TVpar[4] * exp(eta[4]), 
                   indQ = TVpar[5] * exp(eta[5]), 
                   indF1 = TVpar[6]) 
        
        # Apply RxODE for simulation
        sim_data <- rxSolve(mod1, theta, ev)
        ipred_values <- sim_data$Conc
        sig2j_values <- ipred_values^2 * sigma[1] + sigma[2]
        obs_Conc <- collectedData3$Conc
        obs_data <- data.frame(Conc = obs_Conc, ipred = ipred_values, sig2j = sig2j_values)
        
        with(obs_data, sum(log(sig2j) + (ipred - Conc)^2 / sig2j, na.rm = TRUE) + 
               t(eta) %*% solve(omega) %*% eta)
      }
      
      # Function for parameter estimation
      TAC.est <- function(init_eta, collectedData3, mod1, TVpar, ev, omega, sigma) {
        res <- optim(par = init_eta, fn = obj.TAC, collectedData3 = collectedData3,
                     mod1 = mod1, TVpar = TVpar, ev = ev, omega = omega, sigma = sigma,
                     method = "L-BFGS-B")
        
        map.eta <- res$par
        KA <- TVpar[1] * exp(map.eta[1] + map.eta[6])
        CL <- TVpar[2] * exp(map.eta[2] + map.eta[7])
        V2 <- TVpar[3] * exp(map.eta[3] + map.eta[8])
        V3 <- TVpar[4] * exp(map.eta[4])
        Q <- TVpar[5] * exp(map.eta[5])
        F1 <- TVpar[6]
        
        map_est <- c(indKA = KA, indCL = CL, indV2 = V2, indV3 = V3, indQ = Q, indF1 = F1)
        return(map_est)
      }
      
      # Estimate individual parameters
      par_ind <- TAC.est(init_eta, collectedData3, mod1, TVpar, ev, omega, sigma)
      theta <- c(indKA = as.numeric(par_ind[1]), 
                 indCL = as.numeric(par_ind[2]), 
                 indV2 = as.numeric(par_ind[3]), 
                 indV3 = as.numeric(par_ind[4]), 
                 indQ = as.numeric(par_ind[5]), 
                 indF1 = as.numeric(par_ind[6])) 
      dose_times2 <- seq(sample_time, sample_time + newdosetime, by = dosingInterval)
      sample_time <- tail(dose_times2, n = 1) + dosingInterval
      ev$add.sampling(sample_time)
      
      # Find the closest dose to the target concentration
      for (dose in doseOptions) {
        ev2 <- rbind(ev, et(time = dose_times2, amt = dose, cmt = depot1))
        sim1 <- solve(mod1, theta, ev2)
        troughConc <- sim1$Conc[sim1$time == sample_time]
        difference <- abs(troughConc - therapeuticWindowMid)
        
        if (difference < minDifference) {
          minDifference <- difference
          closestDose <- dose
        }
      } 
    } 
    else {
      # Define the calculation logic when TDM result is not available
      # Here, the model of Koomen et al. is used for calculation
      
      # Define the model based on the bioassay type
      if (nrow(collectedData2) == 0) {
        if (bioassay == 1) {
          mod1 <- rxode2({
            CL = 257.6 * (hct / 39)^0.49 * (egfr / 51)^0.07 * 0.78; 
            V2 = 4213.2 * (hp / 1.4)^0.13; 
            KA = 3.5; 
            S2 = V2 / 1000
            F = 0.25 * (DD / 2.5)^brand_F * (1 - 0.11 * fast * brand_fast) * (1 + exp(-0.32 * (pod / 1000) * brand_fast))
            BMAX = 35.7 * (hct / 39)^-0.43 * (chol / 4.6)^-0.18 * (hp / 1.4)^-0.05 * (vb / 320)^-0.04 * (1 + 0.14 * mpa) * 1.1 * (1 * 0.05 + vd)
            KD = 0.24
            Conp = centr / V2
            Concp = Conp * 1000
            Concb = Concp * BMAX / (0.24 + Concp)
            Conc = Concp * (1 - hct / 100) + Concb * hct / 100
            d/dt(depot1) = -F * KA * depot1; 
            d/dt(centr) = F * KA * depot1 - CL * Conp; 
          })
        }
        if (bioassay == 2) {
          mod1 <- rxode2({
            CL = 257.6 * (hct / 39)^0.49 * (egfr / 51)^0.07 * 0.78; 
            V2 = 4213.2 * (hp / 1.4)^0.13; 
            KA = 3.5; 
            S2 = V2 / 1000
            F = 0.25 * (DD / 2.5)^brand_F * (1 - 0.11 * fast * brand_fast) * (1 + exp(-0.32 * (pod / 1000) * brand_fast))
            BMAX = 35.7 * (hct / 39)^-0.43 * (chol / 4.6)^-0.18 * (hp / 1.4)^-0.05 * (vb / 320)^-0.04 * (1 + 0.14 * mpa) * 1.1 * (1 * 0.05 + vd)
            KD = 0.24
            Conp = centr / V2
            Concp = Conp * 1000
            Concb = Concp * BMAX / (0.24 + Concp)
            Conc = (Concp * (1 - hct / 100) + Concb * hct / 100) * 1.2 + 0.14
            d/dt(depot1) = -F * KA * depot1; 
            d/dt(centr) = F * KA * depot1 - CL * Conp; 
          })
        }
        if (bioassay == 3) {
          mod1 <- rxode2({
            CL = 257.6 * (hct / 39)^0.49 * (egfr / 51)^0.07 * 0.78; 
            V2 = 4213.2 * (hp / 1.4)^0.13; 
            KA = 3.5; 
            S2 = V2 / 1000
            F = 0.25 * (DD / 2.5)^brand_F * (1 - 0.11 * fast * brand_fast) * (1 + exp(-0.32 * (pod / 1000) * brand_fast))
            BMAX = 35.7 * (hct / 39)^-0.43 * (chol / 4.6)^-0.18 * (hp / 1.4)^-0.05 * (vb / 320)^-0.04 * (1 + 0.14 * mpa) * 1.1 * (1 * 0.05 + vd)
            KD = 0.24
            Conp = centr / V2
            Concp = Conp * 1000
            Concb = Concp * BMAX / (0.24 + Concp)
            Conc = ((Concp * (1 - hct / 100) + Concb * hct / 100) - 0.206) / 0.872
            d/dt(depot1) = -F * KA * depot1; 
            d/dt(centr) = F * KA * depot1 - CL * Conp; 
          })
        }
        
        closestDose <- NULL
        minDifference <- Inf
        
        # Define the calculation logic when dose have already given to the patients
        if (nrow(collectedData1) > 0) {
       
          collectedData1 <- collectedData1 %>%
            mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S")) %>%
            rowwise() %>%
            mutate(DD = sum(collectedData1$Dose[which(collectedData1$DateTime > (DateTime - hours(24)) & collectedData1$DateTime <= DateTime)])) %>%
            ungroup()
          Alast_startTime <- 0
          Depot_startTime <- 0
          
          for (i in 1:(nrow(collectedData1))) {
            DD <- collectedData1$DD[i]
            pod <- collectedData1$pod[i]
            if (pod < 1) {pod = 1}
            ev_single <- et(time = 0, amt = collectedData1$Dose[i], cmt = "depot1")
            if (i < nrow(collectedData1)) {
              next_time <- collectedData1$TimeDifference[i + 1] - collectedData1$TimeDifference[i]
              ev_single$add.sampling(next_time)
            } else {
              end_sample <- startTime - collectedData1$TimeDifference[i]
              ev_single$add.sampling(end_sample)
            }
            inits <- c(depot1 = Depot_startTime, centr = Alast_startTime)
            theta <- c(DD = DD, 
                       hct = hct,  
                       egfr = egfr,   
                       hp = hp, 
                       chol = chol, 
                       vb = vb12, 
                       brand_F = brand_F,
                       brand_fast = brand_fast,
                       fast = fast, 
                       mpa = mpa, 
                       vd = vd,
                       pod = pod)
            sim_Data <- rxSolve(mod1, theta, ev_single, inits = inits)
            Alast_startTime <- tail(sim_Data$centr, 1)
            Depot_startTime <- tail(sim_Data$depot1, 1)
          }
          
          for (dose in doseOptions) {
            Depot <- Depot_startTime
            Alast <- Alast_startTime
            DD <- dose * 24 / dosingInterval
            for (i in 1:length(dosingEvents)) {
              init <- c(depot1 = Depot, centr = Alast)
              pod <- as.numeric(as.Date(dosingEvents[i]) - as.Date(Surgerytime))
              if (pod < 1) {pod = 1}
              theta <- c(DD = DD, 
                         hct = hct,  
                         egfr = egfr,   
                         hp = hp, 
                         chol = chol, 
                         vb = vb12, 
                         brand_F = brand_F,
                         brand_fast = brand_fast,
                         fast = fast, 
                         vd = vd,
                         mpa = mpa, 
                         pod = pod)
              ev <- et(time = 0, amt = dose, cmt = 1) 
              ev$add.sampling(dosingInterval)
              sim1 <- solve(mod1, theta, ev, init = init)
              Depot <- sim1$depot1
              Alast <- sim1$centr
            }
            
            troughConc <- sim1$Conc
            difference <- abs(troughConc - therapeuticWindowMid)
            
            if (difference < minDifference) {
              minDifference <- difference
              closestDose <- dose
            }
          }
        } 
        # Define the calculation logic when dose have not given to the patients
        else {
          for (dose in doseOptions) {
            Depot <- 0
            Alast <- 0
            DD <- dose * 24 / dosingInterval
            if (DD > 12) {
              break
            }
            for (i in 1:length(dosingEvents)) {
              init <- c(depot1 = Depot, centr = Alast)
              pod <- as.numeric(as.Date(dosingEvents[i]) - as.Date(Surgerytime))
              if (pod < 1) {pod = 1}
              theta <- c(DD = DD, 
                         hct = hct,  
                         egfr = egfr,   
                         hp = hp, 
                         chol = chol, 
                         vb = vb12, 
                         brand_F = brand_F,
                         brand_fast = brand_fast,
                         fast = fast, 
                         mpa = mpa, 
                         vd = vd,
                         pod = pod)
              ev <- et(time = 0, amt = dose, cmt = 1) 
              ev$add.sampling(dosingInterval)
              sim1 <- solve(mod1, theta, ev, init = init)
              Depot <- sim1$depot1
              Alast <- sim1$centr
            }
            
            troughConc <- sim1$Conc
            difference <- abs(troughConc - therapeuticWindowMid)
            
            if (difference < minDifference) {
              minDifference <- difference
              closestDose <- dose
            }
          }
        }
      }
      # Define the calculation logic when TDM result is available for patients
      # Here, model of Monchaud et al. (2012) is used
      else {
        collectedData1 <- collectedData1 %>%
          mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S")) %>%
          rowwise() %>%
          mutate(DD = sum(collectedData1$Dose[which(collectedData1$DateTime > (DateTime - hours(24)) & collectedData1$DateTime <= DateTime)])) %>%
          ungroup()
        
        # Define the model based on the bioassay type
        if (bioassay == 1) {
          mod1 <- rxode2({
            KA = indKA; 
            CL = indCL; 
            V2 = indV2; 
            V3 = indV3; 
            Q  = indQ;
            F1 = indF1;
            Con = centr / V2
            Conc = Con * 1000
            d/dt(depot1) = -F1 * KA * depot1;
            d/dt(depot2) = F1 * KA * depot1 - KA * depot2;
            d/dt(depot3) = KA * depot2 - KA * depot3;
            d/dt(depot4) = KA * depot3 - KA * depot4;
            d/dt(depot5) = KA * depot4 - KA * depot5;
            d/dt(centr) = KA * depot5 - CL * Con - Q / V2 * centr + Q / V3 * perp;
            d/dt(perp) = Q / V2 * centr - Q / V3 * perp
          })
        }
        if (bioassay == 2) {
          mod1 <- rxode2({
            KA = indKA; 
            CL = indCL; 
            V2 = indV2; 
            V3 = indV3; 
            Q  = indQ;
            F1 = indF1;
            Con = centr / V2
            Conc = Con * 1000 * 1.2 + 0.14
            d/dt(depot1) = -F1 * KA * depot1;
            d/dt(depot2) = F1 * KA * depot1 - KA * depot2;
            d/dt(depot3) = KA * depot2 - KA * depot3;
            d/dt(depot4) = KA * depot3 - KA * depot4;
            d/dt(depot5) = KA * depot4 - KA * depot5;
            d/dt(centr) = KA * depot5 - CL * Con - Q / V2 * centr + Q / V3 * perp;
            d/dt(perp) = Q / V2 * centr - Q / V3 * perp
          })
        }
        if (bioassay == 3) {
          mod1 <- rxode2({
            KA = indKA; 
            CL = indCL; 
            V2 = indV2; 
            V3 = indV3; 
            Q  = indQ;
            F1 = indF1;
            Con = centr / V2
            Conc = (Con * 1000 - 0.206) / 0.872
            d/dt(depot1) = -F1 * KA * depot1;
            d/dt(depot2) = F1 * KA * depot1 - KA * depot2;
            d/dt(depot3) = KA * depot2 - KA * depot3;
            d/dt(depot4) = KA * depot3 - KA * depot4;
            d/dt(depot5) = KA * depot4 - KA * depot5;
            d/dt(centr) = KA * depot5 - CL * Con - Q / V2 * centr + Q / V3 * perp;
            d/dt(perp) = Q / V2 * centr - Q / V3 * perp
          })
        }
        
        # Define the covariance matrix for random effects
        omega <- matrix(c(0.182, 0, 0, 0, 0, 0, 0, 0,
                          0, 0.288, 0, 0, 0, 0, 0, 0,
                          0, 0, 0.168, 0, 0, 0, 0, 0,
                          0, 0, 0, 1.588, 0, 0, 0, 0,
                          0, 0, 0, 0, 0.514, 0, 0, 0,
                          0, 0, 0, 0, 0, 0.210, 0, 0,
                          0, 0, 0, 0, 0, 0, 0.219, 0,
                          0, 0, 0, 0, 0, 0, 0, 0.569), 8, 8)
        
        # Define the typical value parameters and residual error
        TVpar <- c(7.06, 17.5, 136, 529, 41.1, 1)
        sigma <- c(0.005, 1.6)
        init_eta <- c(0, 0, 0, 0, 0, 0, 0, 0)
        
        # Create event table for RxODE simulation
        ev <- et(time = collectedData1$TimeDifference, amt = collectedData1$Dose, cmt = "depot1")
        ev$add.sampling(collectedData2$TimeDifference)
        
        # Objective function for parameter optimization
        obj.TAC <- function(eta, omega, sigma, TVpar, mod1, ev, collectedData2) {
          theta <- c(indKA = TVpar[1] * exp(eta[1] + eta[6]), 
                     indCL = TVpar[2] * exp(eta[2] + eta[7]), 
                     indV2 = TVpar[3] * exp(eta[3] + eta[8]), 
                     indV3 = TVpar[4] * exp(eta[4]), 
                     indQ = TVpar[5] * exp(eta[5]), 
                     indF1 = TVpar[6]) 
          
          # Apply RxODE for simulation
          sim_data <- rxSolve(mod1, theta, ev)
          ipred_values <- sim_data$Conc
          sig2j_values <- ipred_values^2 * sigma[1] + sigma[2]
          obs_Conc <- collectedData2$Conc
          obs_data <- data.frame(Conc = obs_Conc, ipred = ipred_values, sig2j = sig2j_values)
          
          with(obs_data, sum(log(sig2j) + (ipred - Conc)^2 / sig2j, na.rm = TRUE) + 
                 t(eta) %*% solve(omega) %*% eta)
        }
        
        # Function for parameter estimation
        TAC.est <- function(init_eta, collectedData2, mod1, TVpar, ev, omega, sigma) {
          res <- optim(par = init_eta, fn = obj.TAC, collectedData2 = collectedData2,
                       mod1 = mod1, TVpar = TVpar, ev = ev, omega = omega, sigma = sigma,
                       method = "L-BFGS-B")
          
          map.eta <- res$par
          KA <- TVpar[1] * exp(map.eta[1] + map.eta[6])
          CL <- TVpar[2] * exp(map.eta[2] + map.eta[7])
          V2 <- TVpar[3] * exp(map.eta[3] + map.eta[8])
          V3 <- TVpar[4] * exp(map.eta[4])
          Q <- TVpar[5] * exp(map.eta[5])
          F1 <- TVpar[6]
          
          map_est <- c(indKA = KA, indCL = CL, indV2 = V2, indV3 = V3, indQ = Q, indF1 = F1)
          return(map_est)
        }
        
        # Estimate individual parameters
        par_ind <- TAC.est(init_eta, collectedData2, mod1, TVpar, ev, omega, sigma)
        theta <- c(indKA = as.numeric(par_ind[1]), 
                   indCL = as.numeric(par_ind[2]), 
                   indV2 = as.numeric(par_ind[3]), 
                   indV3 = as.numeric(par_ind[4]), 
                   indQ = as.numeric(par_ind[5]), 
                   indF1 = as.numeric(par_ind[6])) 
        
        dose_times <- seq(startTime, endTime, by = dosingInterval)
        minDifference <- Inf
        
        # Find the closest dose to the target concentration
        for (dose in doseOptions) {
          DD <- dose * 24 / dosingInterval
          if (DD > 12) {
            break
          }
          
          ev2 <- rbind(ev, et(time = dose_times, amt = dose, cmt = "depot1"))
          ev2$add.sampling(specificTime)
          sim1 <- rxSolve(mod1, theta, ev2)
          troughConc <- sim1$Conc[sim1$time == specificTime]
          difference <- abs(troughConc - therapeuticWindowMid)
          
          if (difference < minDifference) {
            minDifference <- difference
            closestDose <- dose
          }
        }
      }
    }
    return(list(bestDose = closestDose, theta = theta))
  }
  
  # Function to simulate the concentration of Tacrolimus
  simulateConc <- function(egfr, 
                           hct, 
                           hp, 
                           chol, 
                           vb12, 
                           brand,
                           fast, 
                           mpa, 
                           vd,
                           dosingInterval,
                           bioassay,
                           optimalDose,
                           collectedData1,
                           collectedData2,
                           collectedData3,
                           Surgerytime,
                           startTime,
                           endTime,
                           starttime,
                           endtime,
                           eta_CL,
                           eta_V,
                           theta,
                           nSim = 1000) {
    
    # Set brand-specific parameters
    if (brand == 1) {
      brand_F <- -0.64
      brand_fast <- 1
    } 
    if (brand == 2) {
      brand_F <- -0.74
      brand_fast <- 0
    }
    if (brand == 3) {
      brand_F <- -0.62
      brand_fast <- 0
    }
    
    # Convert input values to numeric
    fast <- as.numeric(fast)
    mpa  <- as.numeric(mpa)
    vd   <- as.numeric(vd)
    
    # Define population parameters and covariances
    POPpar <- c(14.7, 636, 4.48)
    CV_CL <- 0.201
    DD <- optimalDose * 24 / dosingInterval
    omega <- lotri(eta1~0.201^2)
    sigma=lotri(exp.err.pk ~ 0)
    
    # Set the simulating logic when steady state TDM sample is available
    if (nrow(collectedData3) != 0) {
      sample_time <- 0
      ev <- NULL
      #Calculate the time interval of first dose and last dose
      newdosetime <- endTime-startTime
      for (i in 1:nrow(collectedData3)) {
        interval <- collectedData3$Interval[i]
        dose_times <- seq(0, 120, by = interval)
        dose_times <- dose_times+sample_time
        amt_vector <- rep(collectedData3$Dose[i], length(dose_times))
        ev <- rbind(ev, et(time = dose_times, amt = amt_vector, cmt = "depot1"))
        sample_time <- tail(dose_times, n=1) + collectedData3$Time[i]
      }
      dose_times <- seq(0, newdosetime, by = dosingInterval)+sample_time
      ev <- rbind(ev, et(time = dose_times, amt = optimalDose, cmt = "depot1"))
      if (bioassay == 1) {
        mod1 <- rxode2({
          KA = indKA; 
          CL = indCL; 
          V2 = indV2; 
          V3 = indV3; 
          Q  = indQ;
          F1 = indF1;
          Con = centr/V2
          Conc = Con*1000
          d/dt(depot1) = -F1*KA*depot1;
          d/dt(centr) = F1*KA*depot1 - CL*Con  - Q/V2*centr + Q/V3*perp;
          d/dt(perp) = Q/V2*centr - Q/V3*perp
        })
      }
      if (bioassay == 2) {
        mod1 <- rxode2({
          KA = indKA; 
          CL = indCL; 
          V2 = indV2; 
          V3 = indV3; 
          Q  = indQ;
          F1 = indF1;
          Con = centr/V2
          Conc = Con*1000*1.2+0.14
          d/dt(depot1) = -F1*KA*depot1;
          d/dt(centr) = F1*KA*depot1 - CL*Con  - Q/V2*centr + Q/V3*perp;
          d/dt(perp) = Q/V2*centr - Q/V3*perp
        })
      }
      if (bioassay == 3) {
        mod1 <- rxode2({
          KA = indKA; 
          CL = indCL; 
          V2 = indV2; 
          V3 = indV3; 
          Q  = indQ;
          F1 = indF1;
          Con = centr/V2
          Conc = (Con*1000-0.206)/0.872
          d/dt(depot1) = -F1*KA*depot1;
          d/dt(centr) = F1*KA*depot1 - CL*Con  - Q/V2*centr + Q/V3*perp;
          d/dt(perp) = Q/V2*centr - Q/V3*perp
        })
      }
      sim <- solve(mod1, theta, ev)
    } else {
      # Set the simulating logic when TDM information is not available and no prior dose has been given 
      if (nrow(collectedData1) == 0) {
        # Initialize an empty data frame to store all simulation data
        sim_Data_All <- data.frame()
        alleta <- NULL
        
        # Define dosing events based on the dosing interval
        dosingEvents <- seq(from = starttime, to = endtime, by = paste(dosingInterval, "hours"))
        
        # Define the model based on the bioassay type
        if (bioassay == 1) {
          mod1 <- rxode2({
            CL = 257.6 * (hct/39)^0.49 * (egfr/51)^0.07 * 0.78; 
            V2 = 4213.2 * (hp/1.4)^0.13; 
            KA = 3.5; 
            S2 = V2/1000;
            F = 0.25 * (DD/2.5)^brand_F * (1 - 0.11 * fast * brand_fast) * (1 + exp(-0.32 * (pod/1000) * brand_fast));
            BMAX = 35.7 * (hct/39)^-0.43 * (chol/4.6)^-0.18 * (hp/1.4)^-0.05 * (vb/320)^-0.04 * (1 + 0.14 * mpa) * 1.1 * (1 * 0.05 + vd);
            KD = 0.24;
            Conp = centr/V2;
            Concp = Conp * 1000;
            Concb = Concp * BMAX / (0.24 + Concp);
            Conc = Concp * (1 - hct/100) + Concb * hct/100;
            d/dt(depot1) = -F * KA * depot1; 
            d/dt(centr) = F * KA * depot1 - CL * Conp; 
          })
        }
        
        if (bioassay == 2) {
          mod1 <- rxode2({
            CL = 257.6 * (hct/39)^0.49 * (egfr/51)^0.07 * 0.78; 
            V2 = 4213.2 * (hp/1.4)^0.13; 
            KA = 3.5; 
            S2 = V2/1000;
            F = 0.25 * (DD/2.5)^brand_F * (1 - 0.11 * fast * brand_fast) * (1 + exp(-0.32 * (pod/1000) * brand_fast));
            BMAX = 35.7 * (hct/39)^-0.43 * (chol/4.6)^-0.18 * (hp/1.4)^-0.05 * (vb/320)^-0.04 * (1 + 0.14 * mpa) * 1.1 * (1 * 0.05 + vd);
            KD = 0.24;
            Conp = centr/V2;
            Concp = Conp * 1000;
            Concb = Concp * BMAX / (0.24 + Concp);
            Conc = (Concp * (1 - hct/100) + Concb * hct/100) * 1.2 + 0.14;
            d/dt(depot1) = -F * KA * depot1; 
            d/dt(centr) = F * KA * depot1 - CL * Conp; 
          })
        }
        
        if (bioassay == 3) {
          mod1 <- rxode2({
            CL = 257.6 * (hct/39)^0.49 * (egfr/51)^0.07 * 0.78; 
            V2 = 4213.2 * (hp/1.4)^0.13; 
            KA = 3.5; 
            S2 = V2/1000;
            F = 0.25 * (DD/2.5)^brand_F * (1 - 0.11 * fast * brand_fast) * (1 + exp(-0.32 * (pod/1000) * brand_fast));
            BMAX = 35.7 * (hct/39)^-0.43 * (chol/4.6)^-0.18 * (hp/1.4)^-0.05 * (vb/320)^-0.04 * (1 + 0.14 * mpa) * 1.1 * (1 * 0.05 + vd);
            KD = 0.24;
            Conp = centr/V2;
            Concp = Conp * 1000;
            Concb = Concp * BMAX / (0.24 + Concp);
            Conc = ((Concp * (1 - hct/100) + Concb * hct/100) - 0.206) / 0.872;
            d/dt(depot1) = -F * KA * depot1; 
            d/dt(centr) = F * KA * depot1 - CL * Conp; 
          })
        }
        
        # Define the sample times
        sampletime <- seq(0, dosingInterval, by = 1)
        
        # Initialize parameters for the simulation
        Alast = 0
        Depot = 0
        
        # Loop through each dosing event to simulate the concentration
        for (i in 1:length(dosingEvents)) {
          init <- c(depot1 = Depot, centr = Alast)
          pod <- as.numeric(as.Date(dosingEvents[i]) - as.Date(Surgerytime))
          
          # Calculate the time difference between the current dosing event and the first dosing event
          dosestarthour <- as.numeric(difftime(dosingEvents[i], dosingEvents[1], units = "hours"))
          if (pod < 1) { pod = 1 }
          
          # Set the parameters for the simulation
          theta <- c(DD = DD, 
                     hct = hct,  
                     egfr = egfr,   
                     hp = hp, 
                     chol = chol, 
                     vb = vb12, 
                     brand_F = brand_F,
                     brand_fast = brand_fast,
                     fast = fast, 
                     mpa = mpa, 
                     vd = vd,
                     pod = pod)
          
          # Define the event table for the simulation
          ev <- et(time = 0, amt = optimalDose, cmt = 1) 
          ev$add.sampling(sampletime)
          
          # Solve the model with the defined parameters and event table
          sim1 <- solve(mod1, theta, ev, init = init)
          
          # Update the initial conditions for the next dosing event
          Depot <- tail(sim1$depot1, 1)
          Alast <- tail(sim1$centr, 1)
          
          # Convert the simulation data to a data frame and adjust the time column
          sim_data <- as.data.frame(sim1[, c("time", "Conc")])
          sim_data$time <- sim_data$time + dosestarthour
          
          # Append the simulation data to the overall data frame
          sim_Data_All <- rbind(sim_Data_All, sim_data)
        }
      }
      else {
        # Set the simulating logic when prior dose has been given 
        # Calculate dosingEvent
        if (nrow(collectedData1) > 0) {
          # Update collectedData1 with necessary calculations
          collectedData1 <- collectedData1 %>%
            mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S")) %>%
            rowwise() %>%
            mutate(DD = sum(collectedData1$Dose[which(collectedData1$DateTime > (DateTime - hours(24)) & collectedData1$DateTime <= DateTime)])) %>%
            ungroup()
          
          # Define dosing events based on the dosing interval
          dosingEvents <- seq(from = starttime, to = endtime, by = paste(dosingInterval, "hours"))
          
          # Set the simulating logic when prior dose has been given but TDM result is not available 
          if (nrow(collectedData2) == 0) {
            # Define the model based on the bioassay type
            if (bioassay == 1) {
              mod1 <- rxode2({
                CL = 257.6 * (hct/39)^0.49 * (egfr/51)^0.07 * 0.78; 
                V2 = 4213.2 * (hp/1.4)^0.13; 
                KA = 3.5; 
                S2 = V2/1000;
                F = 0.25 * (DD/2.5)^brand_F * (1 - 0.11 * fast * brand_fast) * (1 + exp(-0.32 * (pod/1000) * brand_fast));
                BMAX = 35.7 * (hct/39)^-0.43 * (chol/4.6)^-0.18 * (hp/1.4)^-0.05 * (vb/320)^-0.04 * (1 + 0.14 * mpa) * 1.1 * (1 * 0.05 + vd);
                KD = 0.24;
                Conp = centr/V2;
                Concp = Conp * 1000;
                Concb = Concp * BMAX / (0.24 + Concp);
                Conc = Concp * (1 - hct/100) + Concb * hct / 100;
                d/dt(depot1) = -F * KA * depot1; 
                d/dt(centr) = F * KA * depot1 - CL * Conp; 
              })
            }
            
            if (bioassay == 2) {
              mod1 <- rxode2({
                CL = 257.6 * (hct/39)^0.49 * (egfr/51)^0.07 * 0.78; 
                V2 = 4213.2 * (hp/1.4)^0.13; 
                KA = 3.5; 
                S2 = V2/1000;
                F = 0.25 * (DD/2.5)^brand_F * (1 - 0.11 * fast * brand_fast) * (1 + exp(-0.32 * (pod/1000) * brand_fast));
                BMAX = 35.7 * (hct/39)^-0.43 * (chol/4.6)^-0.18 * (hp/1.4)^-0.05 * (vb/320)^-0.04 * (1 + 0.14 * mpa) * 1.1 * (1 * 0.05 + vd);
                KD = 0.24;
                Conp = centr/V2;
                Concp = Conp * 1000;
                Concb = Concp * BMAX / (0.24 + Concp);
                Conc = (Concp * (1 - hct/100) + Concb * hct / 100) * 1.2 + 0.14;
                d/dt(depot1) = -F * KA * depot1; 
                d/dt(centr) = F * KA * depot1 - CL * Conp; 
              })
            }
            
            if (bioassay == 3) {
              mod1 <- rxode2({
                CL = 257.6 * (hct/39)^0.49 * (egfr/51)^0.07 * 0.78; 
                V2 = 4213.2 * (hp/1.4)^0.13; 
                KA = 3.5; 
                S2 = V2/1000;
                F = 0.25 * (DD/2.5)^brand_F * (1 - 0.11 * fast * brand_fast) * (1 + exp(-0.32 * (pod/1000) * brand_fast));
                BMAX = 35.7 * (hct/39)^-0.43 * (chol/4.6)^-0.18 * (hp/1.4)^-0.05 * (vb/320)^-0.04 * (1 + 0.14 * mpa) * 1.1 * (1 * 0.05 + vd);
                KD = 0.24;
                Conp = centr/V2;
                Concp = Conp * 1000;
                Concb = Concp * BMAX / (0.24 + Concp);
                Conc = ((Concp * (1 - hct/100) + Concb * hct / 100) - 0.206) / 0.872;
                d/dt(depot1) = -F * KA * depot1; 
                d/dt(centr) = F * KA * depot1 - CL * Conp; 
              })
            }
            
            # Initialize an empty data frame to store all simulation data
            sim_Data_All <- data.frame()
            
            # Define the sample times
            sampletime <- seq(0, dosingInterval, by = 1)
            
            # Initialize parameters for the simulation
            Alast = 0
            Depot = 0
            
            # Loop through each row in collectedData1 to simulate the concentration
            for (i in 1:(nrow(collectedData1))) {
              DD <- collectedData1$DD[i]
              pod <- collectedData1$pod[i]
              if (pod < 1) { pod <- 1 }
              ev_single <- et(time = 0, amt = collectedData1$Dose[i], cmt = "depot1")
              
              # Define the sampling times based on the next dosing event
              if (i < nrow(collectedData1)) {
                next_time <- collectedData1$TimeDifference[i + 1] - collectedData1$TimeDifference[i]
                ev_single$add.sampling(next_time)
                sampling_times <- seq(0, next_time, by = 1)
                ev_single$add.sampling(sampling_times)
              } else {
                next_time <- Inf
                end_sample <- startTime - collectedData1$TimeDifference[i]
                ev_single$add.sampling(end_sample)
                sampling_times <- seq(0, end_sample, by = 1)
                ev_single$add.sampling(sampling_times)
              }
              
              inits <- c(depot1 = Depot, centr = Alast)
              theta <- c(DD = DD, 
                         hct = hct,  
                         egfr = egfr,   
                         hp = hp, 
                         chol = chol, 
                         vb = vb12, 
                         brand_F = brand_F,
                         brand_fast = brand_fast,
                         fast = fast, 
                         mpa = mpa, 
                         vd = vd,
                         pod = pod)
              
              sim1 <- rxSolve(mod1, theta, ev_single, inits = inits)
              Alast <- tail(sim1$centr, 1)
              Depot <- tail(sim1$depot1, 1)
              sim_data <- as.data.frame(sim1[, c("time", "Conc")])
              sim_data$time <- sim_data$time + collectedData1$TimeDifference[i]
              sim_Data_All <- rbind(sim_Data_All, sim_data)
            }
            
            dosestarthour <- startTime - dosingInterval
            for (i in 1:length(dosingEvents)) {
              init <- c(depot1 = Depot, centr = Alast)
              pod <- as.numeric(as.Date(dosingEvents[i]) - as.Date(Surgerytime))
              dosestarthour <- dosestarthour + dosingInterval
              if (pod < 1) { pod = 1 }
              theta <- c(DD = DD, 
                         hct = hct,  
                         egfr = egfr,   
                         hp = hp, 
                         chol = chol, 
                         vb = vb12, 
                         brand_F = brand_F,
                         brand_fast = brand_fast,
                         fast = fast, 
                         mpa = mpa, 
                         vd = vd,
                         pod = pod)
              
              ev <- et(time = 0, amt = optimalDose, cmt = 1) 
              ev$add.sampling(sampletime)
              sim1 <- solve(mod1, theta, ev, init = init)
              Depot <- tail(sim1$depot1, 1)
              Alast <- tail(sim1$centr, 1)
              sim_data <- as.data.frame(sim1[, c("time", "Conc")])
              sim_data$time <- sim_data$time + dosestarthour
              sim_Data_All <- rbind(sim_Data_All, sim_data)
            }
          }
          
          # Set the simulating logic when prior dose has been given and TDM result is available 
          if (nrow(collectedData2) > 0) {
            # Define the model based on the bioassay type
            if (bioassay == 1) {
              mod1 <- rxode2({
                KA = indKA; 
                CL = indCL; 
                V2 = indV2; 
                V3 = indV3; 
                Q  = indQ;
                F1 = indF1;
                Con = centr/V2;
                Conc = Con * 1000;
                d/dt(depot1) = -F1 * KA * depot1;
                d/dt(centr) = F1 * KA * depot1 - CL * Con - Q/V2 * centr + Q/V3 * perp;
                d/dt(perp) = Q/V2 * centr - Q/V3 * perp;
              })
            }
            if (bioassay == 2) {
              mod1 <- rxode2({
                KA = indKA; 
                CL = indCL; 
                V2 = indV2; 
                V3 = indV3; 
                Q  = indQ;
                F1 = indF1;
                Con = centr/V2;
                Conc = Con * 1000 * 1.2 + 0.14;
                d/dt(depot1) = -F1 * KA * depot1;
                d/dt(centr) = F1 * KA * depot1 - CL * Con - Q/V2 * centr + Q/V3 * perp;
                d/dt(perp) = Q/V2 * centr - Q/V3 * perp;
              })
            }
            if (bioassay == 3) {
              mod1 <- rxode2({
                KA = indKA; 
                CL = indCL; 
                V2 = indV2; 
                V3 = indV3; 
                Q  = indQ;
                F1 = indF1;
                Con = centr/V2;
                Conc = (Con * 1000 - 0.206) / 0.872;
                d/dt(depot1) = -F1 * KA * depot1;
                d/dt(centr) = F1 * KA * depot1 - CL * Con - Q/V2 * centr + Q/V3 * perp;
                d/dt(perp) = Q/V2 * centr - Q/V3 * perp;
              })
            }
            
            ev <- et(time = collectedData1$TimeDifference, amt = collectedData1$Dose, cmt = "depot1")
            dose_times <- seq(startTime, endTime, by = dosingInterval)
            ev2 <- rbind(ev, et(time = dose_times, amt = optimalDose, cmt = "depot1"))
            sim <- rxSolve(mod1, theta, ev2)
          }
        }
      }
    }
    if (nrow(collectedData3) > 0) {
      # If steady-state TDM information is available, filter the simulation results up to the time threshold
      result_plot <- data.frame(sim)
      time_threshold <- tail(dose_times, n = 1) + dosingInterval
      result_plot <- result_plot[result_plot$time <= time_threshold, ]
    } else {
      if (nrow(collectedData2) == 0) {
        # If TDM results and collectedData2 are not available, summarize the simulation data
        result_plot <- sim_Data_All %>%
          group_by(time) %>%
          summarise(
            c_0.05 = quantile(Conc, probs = 0.05),
            c_0.5 = quantile(Conc, probs = 0.5),
            c_0.95 = quantile(Conc, probs = 0.95)
          )
      } else {
        # If TDM results are available, filter the simulation results up to the time threshold
        result_plot <- data.frame(sim)
        time_threshold <- tail(dose_times, n = 1) + dosingInterval
        result_plot <- result_plot[result_plot$time <= time_threshold, ]
      }
    }
    return(result_plot)
  }
    
    # Function for drawing C-T curve
    plot_result <- function(result_plot, dosingInterval, therapeuticWindowLow, therapeuticWindowHigh, starttime,
                            endtime, timeDiff,
                            collectedData1, collectedData2, collectedData3) {
      # Define x-axis limits, scales, and breaks
      if (nrow(collectedData3) > 1) {
        # If steady-state TDM information is available
        TDM_times <- collectedData2$TimeDifference
        start_time <- as.POSIXct(starttime)
        hour <- as.numeric(format(start_time, "%H"))
        xMax <- tail(result_plot$time, 1) + hour
        if (dosingInterval < 24) {
          xMax <- xMax - (24 - dosingInterval)
        }
        Break <- seq(0, xMax, by = 24)
        result_plot$time <- result_plot$time + hour
      } else if (nrow(collectedData1) > 1) {
        # If prior dose information is available
        TDM_times <- collectedData2$TimeDifference
        # Extract hour part from collectedData1$Time[1]
        time_string <- collectedData1$Time[1]
        hour <- as.numeric(sub("^(\\d+):.*$", "\\1", time_string))
        xMax <- tail(result_plot$time, 1) + hour
        Break <- seq(0, xMax, by = 24)
        result_plot$time <- result_plot$time + hour
      } else {
        # If neither steady-state TDM nor prior dose information is available
        TDM_times <- collectedData2$TimeDifference
        start_time <- as.POSIXct(starttime)
        hour <- as.numeric(format(start_time, "%H"))
        xMax <- tail(result_plot$time, 1) + hour
        Break <- seq(0, xMax, by = 24)
        result_plot$time <- result_plot$time + hour
      }
      
      if (nrow(collectedData3) > 0) {
        collectedData3$sample_time <- rep(NA, nrow(collectedData3))
        sample_time <- 0
        for (i in 1:nrow(collectedData3)) {
          interval <- collectedData3$Interval[i]
          dose_times <- seq(0, 120, by = interval)
          dose_times <- dose_times + sample_time
          sample_time <- tail(dose_times, n = 1) + interval 
          collectedData3$sample_time[i] <- sample_time
        }
        Label <- as.Date(endtime) - (xMax - hour) / 24 + Break / 24
      } else {
        if (nrow(collectedData1) > 1) {
          start_date <- collectedData1$Date[1]
        } else {
          start_date <- starttime
        }
        Label <- as.Date(start_date + hours(Break))
      }
      
      if (nrow(collectedData3) > 0) {
        ggplot(result_plot, aes(x = time)) + 
          geom_line(aes(y = Conc), color = "blue") +  # Median concentration curve
          geom_point(data = collectedData3, aes(x = sample_time + hour, y = Conc), color = "red") + 
          labs(x = "Time (days)", y = "Tacrolimus Conc (ng/mL)") + 
          scale_x_continuous(limits = c(0, xMax), breaks = Break, labels = Label) +
          scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70)) +
          geom_hline(yintercept = c(therapeuticWindowLow, therapeuticWindowHigh), linetype = 2, size = 0.6, colour = "skyblue") + 
          theme_bw() +
          theme(
            plot.title = element_text(face = "bold", size = 12, color = "black", hjust = 0),
            axis.title.x = element_text(face = "bold", size = 12, color = "black"),
            axis.title.y = element_text(face = "bold", size = 12, color = "black"),
            axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.3),  # Rotate x-axis labels
            axis.text.y = element_text(face = "bold", size = 10, color = "black"),
            strip.text = element_text(face = "bold", size = 10, color = "black"),
            strip.background = element_rect(fill = "grey"),
            panel.background = element_rect(fill = "white", color = "black"),
            panel.grid.major.y = element_line(color = "white", linetype = 2),
            panel.grid.major.x = element_line(color = "white", linetype = 2),
            panel.grid.minor.y = element_line(color = "white", linetype = 2),
            panel.grid.minor.x = element_blank(),
            plot.margin = unit(c(0.3, 0.3, 0.6, 0.6), "lines")
          )
      } else {
        if (nrow(collectedData2) > 0) {
          ggplot(result_plot, aes(x = time)) + 
            geom_line(aes(y = Conc), color = "blue") +  # Median concentration curve
            geom_point(data = collectedData2, aes(x = TDM_times + hour, y = Conc), color = "red") + 
            labs(x = "Time (days)", y = "Tacrolimus Conc (ng/mL)") + 
            scale_x_continuous(limits = c(0, xMax), breaks = Break, labels = Label) +
            scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70)) +
            geom_hline(yintercept = c(therapeuticWindowLow, therapeuticWindowHigh), linetype = 2, size = 0.6, colour = "skyblue") + 
            theme_bw() +
            theme(
              plot.title = element_text(face = "bold", size = 12, color = "black", hjust = 0),
              axis.title.x = element_text(face = "bold", size = 12, color = "black"),
              axis.title.y = element_text(face = "bold", size = 12, color = "black"),
              axis.text = element_text(face = "bold", size = 10, color = "black"),
              axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.3), 
              strip.text = element_text(face = "bold", size = 10, color = "black"),
              strip.background = element_rect(fill = "grey"),
              panel.background = element_rect(fill = "white", color = "black"),
              panel.grid.major.y = element_line(color = "white", linetype = 2),
              panel.grid.major.x = element_line(color = "white", linetype = 2),
              panel.grid.minor.y = element_line(color = "white", linetype = 2),
              panel.grid.minor.x = element_blank(),
              plot.margin = unit(c(0.3, 0.3, 0.6, 0.6), "lines")
            )
        } else {
          ggplot(result_plot, aes(x = time)) + 
            geom_ribbon(aes(ymin = c_0.05, ymax = c_0.95), fill = "lightblue", alpha = 0.5) +  # 5% to 95% confidence interval
            geom_line(aes(y = c_0.5), color = "blue") +  # Median concentration curve
            labs(x = "Time (days)", y = "Tacrolimus Conc (ng/mL)") + 
            scale_x_continuous(limits = c(0, xMax), breaks = Break, labels = Label) +
            scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70)) +
            geom_hline(yintercept = c(therapeuticWindowLow, therapeuticWindowHigh), linetype = 2, size = 0.6, colour = "skyblue") + 
            theme_bw() +
            theme(
              plot.title = element_text(face = "bold", size = 12, color = "black", hjust = 0),
              axis.title.x = element_text(face = "bold", size = 12, color = "black"),
              axis.title.y = element_text(face = "bold", size = 12, color = "black"),
              axis.text = element_text(face = "bold", size = 10, color = "black"),
              strip.text = element_text(face = "bold", size = 10, color = "black"),
              axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.3),
              strip.background = element_rect(fill = "grey"),
              panel.background = element_rect(fill = "white", color = "black"),
              panel.grid.major.y = element_line(color = "white", linetype = 2),
              panel.grid.major.x = element_line(color = "white", linetype = 2),
              panel.grid.minor.y = element_line(color = "white", linetype = 2),
              panel.grid.minor.x = element_blank(),
              plot.margin = unit(c(0.3, 0.3, 0.6, 0.6), "lines")
            )
        }
      }
    }
}

# run
shinyApp(ui = ui, server = server)
#A demonstration of this application is published in "https://pzaauccal.shinyapps.io/tacrolimu_MIPD_lungtransplant_adult/"