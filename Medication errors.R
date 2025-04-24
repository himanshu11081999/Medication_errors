library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(googlesheets4)
library(shinyjs)
library(lubridate)
library(stats)

errors <- c("",
            #####
            "1. Incorrect drug selection", "2. No/wrong dose", "3. No/wrong unit of measurement", "4. No/wrong frequency", 
            "5. No/wrong route", "6. No/wrong concentration", "7. No/wrong rate of administration", "8. Illegible handwriting", 
            "9. Non-approved abbreviations used", "10. Non-usage of capital letters for drug names", 
            "11. Non-usage of generic names", "12. Non-modification of drug dose keeping in mind drug-drug interaction", 
            "13. Non-modification of time of drug administration/dose/drug keeping in mind food-drug interaction", 
            "14. Wrong formulation transcribed/indented", "15. Wrong drug transcribed/indented", 
            "16. Wrong strength transcribed/indented", "17. Wrong drug dispensed", "18. Wrong dose dispensed", 
            "19. Wrong formulation dispensed", "20. Expired/Near-expiry drugs dispensed", "21. No/wrong labelling", 
            "22. Delay in dispense > defined time", "23. Generic or class substitute done without consultation with the prescribing doctor", 
            "24. Wrong Patient", "25. Dose Omission", "26. Improper Dose", "27. Wrong Drug", "28. Wrong Formulation Administered", 
            "29. Wrong Route of Administration", "30. Wrong Rate", "31. Wrong Duration", "32. Wrong Time*", 
            "33. No documentation of drug administration", "34. Incomplete/Improper documentation by Doctor / nursing staff / pharmacist**", 
            "35. Documentation without administration", "36. Wrong date", 
            "37. Incomplete or wrong start or stop order"
            #####
)

# Define UI
ui <- fluidPage(
  titlePanel("Medication Errors Data Entry"),
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      h3("Enter the New Record here", style = "font-weight: bold; color: black;"),
      textInput("uhid", "UHID:"),
      dateInput("date_of_admission", "Date of Admission:", value = Sys.Date()),
      textInput("primary_consultant", "Primary Consultant:"),
      selectInput("drug_allergies", "Allergies Documented:", choices = c("", "Yes", "No")),
      h5("_____________________________________"),
      selectInput("area", "Select Area:", 
                  choices = c("", "Doctors", "Doctor and/or Nurse", "Pharmacist", "Nurses")),
      selectInput("type", "Select Type:",
                  choices = c("", "Prescription", "Transcription/indenting",
                              "Dispensing", "Administration", "Monitoring")),
      selectInput("error", "Select Error:", choices = errors),
      lapply(1:10, function(i) {
        tagList(
          selectInput(paste0("Drug", i), 
                      paste("Drug", i), 
                      choices = c(NA, "0", "A ", "B ", "C ", 
                                  "D ", "E ", "F ", 
                                  "G ", "H ", "I "))
        )
      }),
      h5("_____________________________________"),
      textInput("auditor", "Auditor:"),
      dateInput("date_of_audit", "Date of Audit:", value = Sys.Date()),
      textInput("location", "Location:"),
      textInput("department", "Department:"),
      actionButton("add", h5("Add Record"),
                   style = "color: yellow; background-color: blue; border-color: yellow;"),
      h5("_____________________________________"),
      downloadButton("download", "Download Data"),
      
      h5("_____________________________________"),
      h5("_____________________________________"),
      passwordInput("auth_key", "Enter Authentication Key to Enable Delete all."),
      actionButton("auth_button", "Authenticate"),
      actionButton("delete_all", "Delete All Records",
                   style = "color: white; background-color: red; border-color: red;",
                   disabled = TRUE)  # New button to delete all records
      
    ),
    
    mainPanel(
      
      # Include Material Icons
      tags$head(
        tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/icon?family=Material+Icons")
      ),
      tags$div(
        style = "display: flex; align-items: center;",  # Align items in a row
        tags$a(
          href = "https://docs.google.com/spreadsheets/d/10CM4IEQHt-aBnEyeuTZjEMOlq9ZIfj4sk-8sAy6Om1I/edit?gid=0",
               "Open Google Sheets", 
               target = "_blank",
               style = "margin-left: auto;"), # Opens in a new tab
        tags$span(class = "material-icons", 
                  style = "font-size: 24px; margin-left: 12px;", 
                  "table_chart")  # Google Sheets icon
      ),
      tags$style(HTML("
  #error_stats {
    font-size: 25px;
    color: darkred;
    font-weight: bold;
    background-color: #f3f3f3;
    padding: 10px;
    border-radius: 5px;
    border: 1px solid #ccc;
  }
")),
      verbatimTextOutput("error_stats"),
      plotOutput("bar_errorwise"),
      h5("_____________________________________________________________________________________"),
      DTOutput("summary_table_area"),
      h5("_____________________________________________________________________________________"),
      plotOutput("bar_chart_area"),
      h5("_____________________________________________________________________________________"),
      DTOutput("summary_table_type"),
      h5("_____________________________________________________________________________________"),
      plotOutput("bar_chart_type"),
      h5("_____________________________________________________________________________________"),
      DTOutput("summary_table_error"),
      h5("_____________________________________________________________________________________"),
      plotOutput("bar_chart_error"),
      h5("_____________________________________________________________________________________"),
      DTOutput("table")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  tryCatch({
    Sys.setenv(GOOGLE_APPLICATION_CREDENTIALS = "himanshucmc-9d06ab2bebfc.json")
    gs4_auth(path = "himanshucmc-9d06ab2bebfc.json",
             scopes = "https://www.googleapis.com/auth/spreadsheets",
             cache = TRUE,
             use_oob = FALSE)
    
  }, error = function(e) {
    print(paste("Error: ", e$message))
    # Optionally, return a user-friendly message or take corrective action
  })
  
  path <- "https://docs.google.com/spreadsheets/d/10CM4IEQHt-aBnEyeuTZjEMOlq9ZIfj4sk-8sAy6Om1I/edit?gid=0#gid=0"
  existing_data <- read_sheet(path, sheet = "Data")
  
  # Reactive value to hold data
  data <- reactiveVal(data.frame())  # Initialize with an empty data frame

  data(existing_data)  # Update reactive value with uploaded data
  
  # Add new record
  observeEvent(input$add, {
    
    new_record <- data.frame(
      S_No = nrow(data()) + 1,
      Timestamp = as.character(now()),
      UHID = input$uhid,
      Date_of_Admission = as.character(input$date_of_admission),
      Primary_Consultant = input$primary_consultant,
      Allergies_documented = input$drug_allergies,
      Areas = input$area,
      Type = input$type,
      Errors = input$error,
      Drug_1 = as.character(input$Drug1),  
      Drug_2 = as.character(input$Drug2),
      Drug_3 = as.character(input$Drug3),
      Drug_4 = as.character(input$Drug4),
      Drug_5 = as.character(input$Drug5),
      Drug_6 = as.character(input$Drug6),
      Drug_7 = as.character(input$Drug7),
      Drug_8 = as.character(input$Drug8),
      Drug_9 = as.character(input$Drug9),
      Drug_10 = as.character(input$Drug10),
      Auditor = input$auditor,
      Date_of_Audit = as.character(input$date_of_audit),
      Location = input$location,
      Department = input$department,
      stringsAsFactors = FALSE
    )
    updated_data <- rbind(data(), new_record)
    data(updated_data)
    
    # Append the new record to Google Sheet
    sheet_append(path, new_record, sheet = "Data")
    })
  
  # Delete all records
  observeEvent(input$delete_all, {
    # Create an empty data frame with the same structure
    empty_data <- data.frame(S_No=integer(),
                             Timestamp = character(),
                             UHID=character(), 
                             Date_of_Admission=character(),
                             Primary_Consultant=character(), 
                             Allergies_documented=character(),
                             Areas=character(), 
                             Type = character(),
                             Errors=character(), 
                             Drug_1=character(),
                             Drug_2=character(),
                             Drug_3=character(),
                             Drug_4=character(),
                             Drug_5=character(),
                             Drug_6=character(),
                             Drug_7=character(),
                             Drug_8=character(),
                             Drug_9=character(),
                             Drug_10=character(),
                             Auditor=character(), 
                             Date_of_Audit=character(), 
                             Location=character(),
                             Department=character(),
                             stringsAsFactors=FALSE)
    
    # Update reactive value and write to CSV
    data(empty_data)
    range_clear(path, sheet = "Data", range = "A2:AZ10000")
    
    # Optionally provide feedback to the user
    showNotification("All records have been deleted.", type="message")
  })
  
  ##### Display data table    #####
  output$table <- renderDT({
    datatable(data(), selection = "multiple")
  })
  
  output$error_stats <- renderPrint({
    df <- data()
    
    if (nrow(df) == 0) {
      cat("No data available.")
      return()
    }
    
    # Extract relevant columns: UHID and Drug_1 to Drug_10
    drug_cols <- paste0("Drug_", 1:10)
    df_long <- df %>%
      select(UHID, all_of(drug_cols)) %>%
      pivot_longer(cols = starts_with("Drug_"), names_to = "Drug_No", values_to = "Grade") %>%
      filter(!is.na(Grade))  # Remove NA entries
    
    # Count non-zero grades (actual errors)
    num_errors <- df_long %>%
      filter(Grade %in% LETTERS[1:9]) %>%
      nrow()
    
    # Count unique UHID and number of non-NA drug entries (opportunities)
    df_opportunities <- df_long %>%
      filter(!is.na(Grade)) %>%
      group_by(UHID) %>%
      summarise(Num_Drugs = sum(!is.na(Grade) & Grade != ""), .groups = "drop")
    
    total_opportunities <- sum(df_opportunities$Num_Drugs) * 37  # 37 error slots per drug
    
    dpmo <- ifelse(total_opportunities > 0, num_errors / total_opportunities, NA)
    
    # Approximate Six Sigma value using standard Z table mapping (no shift)
    p <- 1 - dpmo     # Proportion of success
    z <- qnorm(p)             # Z-score
    sigma <- round(z + 1.5, 2)
    
    cat("Medication Error Summary\n\n")
    cat("Total Errors: ", num_errors, "\n")
    cat("Total Opportunities: ", total_opportunities, "\n")
    cat("Error per opportunity: ", round(dpmo, 11), "\n")
    cat("Error per million opportunity: ", round(dpmo, 11)*(10^6), "\n\n")
    cat("Approximate 6 Sigma Level: ", sigma, "\n")
  })
  
  ##### Summary Table & Bar chart Area #####
  # Error wise bar chart
  output$bar_errorwise <- renderPlot({
    chart_data <- data() %>%
      pivot_longer(cols = starts_with("Drug_"), names_to = "Drug_Col", values_to = "Drug") %>%
      filter(Drug != "NA" & Drug != "0") %>%
      group_by(Drug) %>%
      summarise(Count = n(), .groups = 'drop')
    
    ggplot(chart_data, aes(x = Drug, y = Count, fill = Drug)) +
      expand_limits(y = max(chart_data$Count) * 1.2) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Medication Errors", x = "Category of Errors", y = "Count") +
      geom_text(aes(label = Count),  # Use Total_Count for the label
                position = position_dodge(width = 0.9), vjust = -0.5, size = 6) +
      theme(
        plot.title = element_text(size = 24, face = "bold"),   
        axis.title.x = element_text(size = 20),                
        axis.title.y = element_text(size = 20),               
        axis.text.x = element_text(size = 16),            
        axis.text.y = element_text(size = 16))  
  })
  
  # Summary Table
  output$summary_table_area <- renderDT({
    
    summary_data <- data() %>%
      pivot_longer(cols = starts_with("Drug_"), names_to = "Drug_Col", values_to = "Drug") %>%
      filter(!is.na(Drug) & Drug != "" & Drug != "0") %>%
      group_by(Areas, Drug) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      pivot_wider(names_from = Drug, values_from = Count, values_fill = list(Count = 0)) %>%
      mutate(TOTAL = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>%
      mutate(TOTAL = TOTAL - `NA`)
    
    # Create a total row
    total <- data.frame(Areas = "Total", 
                        matrix(colSums(summary_data[, -1], na.rm = TRUE), nrow = 1))
    
    # Ensure the total_row has the same column names as summary_data
    colnames(total) <- colnames(summary_data)
    
    # Combine the summary data with the total row
    xyz <- rbind(summary_data, total)
    
    # Identify the first and last columns
    first_column <- names(xyz)[1]
    last_column <- names(xyz)[ncol(xyz)]
    
    # Identify the middle columns
    middle_columns <- names(xyz)[2:(ncol(xyz) - 1)]
    
    # Sort the middle columns alphabetically
    sorted_middle_columns <- sort(middle_columns)
    
    # Reorder the data frame
    summary_data_with_total <- xyz[, c(first_column, sorted_middle_columns, last_column)]
    
    sheet_write(summary_data_with_total, ss = path, sheet = "Summary_area")
    
    datatable(summary_data_with_total,
              caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black; font-size:200%;', 
                                                'Summary of Medication Errors by Area'))
  })
  
  # Bar Chart
  output$bar_chart_area <- renderPlot({
    chart_data <- data() %>%
      pivot_longer(cols = starts_with("Drug_"), names_to = "Drug_Col", values_to = "Drug") %>%
      filter(Drug != "NA" & Drug != "0") %>%
      group_by(Areas, Drug) %>%
      summarise(Count = n(), .groups = 'drop')
    
    ggplot(chart_data, aes(x = Areas, y = Count, fill = Drug)) +
      expand_limits(y = max(chart_data$Count) * 1.2) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Medication Errors by Area", x = "Areas", y = "Count") +
      geom_text(aes(label = Count),  # Use Total_Count for the label
                position = position_dodge(width = 0.9), vjust = -0.5, size = 6) +
      theme(
        plot.title = element_text(size = 24, face = "bold"),   
            axis.title.x = element_text(size = 20),                
            axis.title.y = element_text(size = 20),               
            axis.text.x = element_text(size = 16),            
            axis.text.y = element_text(size = 16))  
  })
  
  ##### Summary Table & Bar chart Type #####
  # Summary Table
  output$summary_table_type <- renderDT({
    
    summary_data <- data() %>%
      pivot_longer(cols = starts_with("Drug_"), names_to = "Drug_Col", values_to = "Drug") %>%
      filter(!is.na(Drug) & Drug != "" & Drug != "0") %>%
      group_by(Type, Drug) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      pivot_wider(names_from = Drug, values_from = Count, values_fill = list(Count = 0)) %>%
      mutate(TOTAL = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>%
      mutate(TOTAL = TOTAL - `NA`)
    
    # Create a total row
    total <- data.frame(Type = "Total", 
                        matrix(colSums(summary_data[, -1], na.rm = TRUE), nrow = 1))
    
    # Ensure the total_row has the same column names as summary_data
    colnames(total) <- colnames(summary_data)
    
    # Combine the summary data with the total row
    summary_data_with_total <- rbind(summary_data, total)
    
    datatable(summary_data_with_total,
              caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black; font-size:200%;', 
                                                'Summary of Medication Errors by Type'))
  })
  
  # Bar Chart
  output$bar_chart_type <- renderPlot({
    chart_data <- data() %>%
      pivot_longer(cols = starts_with("Drug_"), names_to = "Drug_Col", values_to = "Drug") %>%
      filter(Drug != "NA" & Drug != "0") %>%
      group_by(Type, Drug) %>%
      summarise(Count = n(), .groups = 'drop')
    
    ggplot(chart_data, aes(x = Type, y = Count, fill = Drug)) +
      expand_limits(y = max(chart_data$Count) * 1.2) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Medication Errors by Type", x = "Type", y = "Count") +
      geom_text(aes(label = Count),  # Use Total_Count for the label
                position = position_dodge(width = 0.9), vjust = -0.5, size = 6) +
      theme(
        plot.title = element_text(size = 24, face = "bold"),   
        axis.title.x = element_text(size = 20),                
        axis.title.y = element_text(size = 20),               
        axis.text.x = element_text(size = 16),            
        axis.text.y = element_text(size = 16))  
  })
  
  ##### Summary Table & Bar chart Errors #####
  # Summary Table
  output$summary_table_error <- renderDT({
    
    summary_data <- data() %>%
      pivot_longer(cols = starts_with("Drug_"), names_to = "Drug_Col", values_to = "Drug") %>%
      filter(!is.na(Drug) & Drug != "" & Drug != "0") %>%
      group_by(Errors, Drug) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      pivot_wider(names_from = Drug, values_from = Count, values_fill = list(Count = 0)) %>%
      mutate(TOTAL = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>%
      mutate(TOTAL = TOTAL - `NA`)
    
    # Extract the numeric part of each error
    summary_data$numeric_parts <- as.integer(gsub("[^0-9]", "", summary_data$Errors))
    
    # Sort the errors based on the numeric part
    summary_data <- summary_data[order(summary_data$numeric_parts),]
    summary_data <- summary_data[,-7]
    # Create a total row
    total <- data.frame(Errors = "Total", 
                        matrix(colSums(summary_data[, -1], na.rm = TRUE), nrow = 1))
    
    # Ensure the total_row has the same column names as summary_data
    colnames(total) <- colnames(summary_data)
    
    # Combine the summary data with the total row
    summary_data_with_total <- rbind(summary_data, total)
    
    datatable(summary_data_with_total,
              caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black; font-size:200%;', 
                                                'Summary of Medication Errors by Errors'))
  })
  
  # Bar Chart
  output$bar_chart_error <- renderPlot({
    
    chart_data <- data() %>%
      pivot_longer(cols = starts_with("Drug_"), names_to = "Drug_Col", values_to = "Drug") %>%
      filter(Drug != "NA" & Drug != "0") %>%
      group_by(Errors, Drug) %>%
      summarise(Count = n(), .groups = 'drop')
    
    # Extract the numeric part of each error
    chart_data$numeric_parts <- as.integer(gsub("[^0-9]", "", chart_data$Errors))
    
    # Sort the errors based on the numeric part
    chart_data <- chart_data[order(chart_data$numeric_parts),]
    cd <- chart_data[,-4]
    
    ggplot(cd, aes(x = Count, y = Errors, fill = Drug)) +
      expand_limits(x = max(chart_data$Count) * 1.2) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Medication Errors by Errors", x = "Count", y = "Errors") +
      geom_text(aes(label = Count),  # Use Total_Count for the label
                position = position_dodge(width = 0.9), vjust = 0, size = 6) +
      theme(
        plot.title = element_text(size = 24, face = "bold"),   
        axis.title.x = element_text(size = 20),                
        axis.title.y = element_text(size = 20),               
        axis.text.x = element_text(size = 16),            
        axis.text.y = element_text(size = 16))  
  })
  
  ##### Download handler  #####
  output$download <- downloadHandler(
    filename = function() { "medication_errors_data.csv" },
    content = function(file) {
      write.csv(data(), file)
    }
  )
  
  # Reactive Value to Track Authentication Status
  auth_status <- reactiveVal(FALSE)
  
  # Authentication Logic
  observeEvent(input$auth_button, {
    if (input$auth_key == "95695") {
      auth_status(TRUE)  # Set Authentication to TRUE
      showModal(modalDialog(
        title = "Authentication Successful",
        "You are now authorized delete entries.",
        easyClose = TRUE
      ))
    } else {
      auth_status(FALSE)  # Reset Authentication to FALSE
      showModal(modalDialog(
        title = "Authentication Failed",
        "Incorrect authentication key. Please try again.",
        easyClose = TRUE
      ))
    }
  })
  
  # Enable/Disable Buttons Based on Authentication
  observe({
    if (auth_status()) {
      shinyjs::enable("delete_all")
    } else {
      shinyjs::disable("delete_all")
    }
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
