library(shiny)
library(rhandsontable)
library(shinydashboard)
library(tidyverse)
library(readxl)

# global ------------------------------------------------------------------

data_hpli <- read_rds("data/processed/data_hpli.RDS")
data_betas <- read_rds("data/processed/data_betas.RDS")
data_example <- read_rds("data/processed/data_example.RDS")

# ui ----------------------------------------------------------------------

ui <- shinydashboard::dashboardPage(
  ###### Header ##################################################################
  shinydashboard::dashboardHeader(title = "ADOPT-IPM online performance assessment tool"),
  
  ###### Sidebar #################################################################
  shinydashboard::dashboardSidebar(
    ### Menu ###
    shinydashboard::sidebarMenu(
      id = "sidebar_menu",
      menuItem("  Pesticide Data", tabName = "pest", icon = icon("bugs")),
      menuItem("  Performance Data", tabName = "perf", icon = icon("leaf"))
    ),
    
    # Pesticide data entry specific sidebar content
    conditionalPanel(
      condition = "input.sidebar_menu == 'pest'",
      br(),
      h4("Table Instructions", style = "padding-left: 15px; color: white;"),
      div(
        style = "padding-left: 15px; color: white; font-size: 12px;",
        p("• Select a compound from the dropdown"),
        p("• Load score will auto-populate"),
        p("• Enter the quantity of compound applied (in consistent units for the entire table)"),
        p("• Compound's risk score will be calculated automatically"),
        p("• System's total risk score is displayed in the summary")
      ),
      br(),
      div(
        style = "padding-left: 15px;",
        actionButton("add_row", "Add Row", class = "btn-primary btn-sm", style = "margin-bottom: 10px;"),
        br(),
        actionButton("remove_row", "Remove Row", class = "btn-warning btn-sm", style = "margin-bottom: 15px;"),
        # br(),
        # numericInput("max_rows", "Max Rows:", value = 5, min = 1, max = 50, width = "150px")
      )
    ),
    
    ### Credit info, ADOPT IPM logo ###
    div(
      style = "position: fixed;
               bottom: 15px;
               left: 15px;
               font-size: 12px;
               color: #888;
               z-index: 1000;",
      # Try different approaches for the image
      # # Option 1: Standard approach (what you have)
      img(
        #src = "adopt-ipm_logo-clean.png",
        src = "test.png",
        height = "50px",
        width = "auto",
        style = "margin-bottom: 5px;",
        onerror = "this.style.display='none'; console.log('Image failed to load');"
      ),
      # #Option 2: Alternative - uncomment if above doesn't work
      # tags$img(
      #   src = "test.png",
      #   height = "50px",
      #   style = "margin-bottom: 5px;"
      # ),
      br(),
      HTML(
        "<a href='https://adopt-ipm.eu/' target='_blank'>adopt-ipm.eu</a><br>
         Nichols and Vandevoorde (2025)<br>
         Last updated: Nov 2025<br>"
      )
    )
  ), #--end of sidebar
  
  
  ###### Body ####################################################################
  shinydashboard::dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
      "))
    ),
    
    tabItems(
      ###### Body: Pesticide table tab ######
      tabItem(
        tabName = "pest",
        # First system
        fluidRow(
          box(
            title = "System #1 - Pesticides applied",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            height = "225px",
            rHandsontableOutput("hot_table1")
          ),
          box(
            title = "System #1 - Insight",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            height = "225px",
            verbatimTextOutput("summary1")
          )
        ),
        fluidRow(
          box(
            title = "System #1 - Summary",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            height = "175px",
            fluidRow(
              column(4, valueBoxOutput("total_risk1", width = 12)),
              column(4, valueBoxOutput("item_count1", width = 12)),
              column(4, valueBoxOutput("filled_rows1", width = 12))
            )
          )
        ),
        
        # Second system
        fluidRow(
          box(
            title = "System #2 - Pesticides applied",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            height = "225px",
            rHandsontableOutput("hot_table2")
          ),
          box(
            title = "System #2 - Insight",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            height = "225px",
            verbatimTextOutput("summary2")
          )
        ),
        fluidRow(
          box(
            title = "System #2 - Summary",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            height = "175px",
            fluidRow(
              column(4, valueBoxOutput("total_risk2", width = 12)),
              column(4, valueBoxOutput("item_count2", width = 12)),
              column(4, valueBoxOutput("filled_rows2", width = 12))
            )
          )
        )
      ), #--end of tab
      
      ###### Body: Performance tab ######
      tabItem(
        tabName = "perf",
        fluidRow(
          box(
            title = "Performance Data",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            p("This is the Performance Data tab.")
          )
        )
      )
    )#--end of dashboard body   
  )
)

# server ------------------------------------------------------------------


server <- function(input, output, session) {
  
  # Initialize reactive values for both tables
  values1 <- reactiveValues()
  values2 <- reactiveValues()
  
  # Initialize both data frames
  observe({
    if (is.null(values1$data)) {
      initial_rows <- 5
      values1$data <- data.frame(
        Compound = rep("", initial_rows),
        Load_Score = rep(0, initial_rows),
        Quantity_Applied = rep(0, initial_rows),
        Risk_Score = rep(0, initial_rows),
        stringsAsFactors = FALSE
      )
    }
    
    if (is.null(values2$data)) {
      initial_rows <- 5
      values2$data <- data.frame(
        Compound = rep("", initial_rows),
        Load_Score = rep(0, initial_rows),
        Quantity_Applied = rep(0, initial_rows),
        Risk_Score = rep(0, initial_rows),
        stringsAsFactors = FALSE
      )
    }
  })
  
  # Add row functionality - affects both tables
  observeEvent(input$add_row, {
    if (nrow(values1$data) < 50) {
      new_row <- data.frame(
        Compound = "",
        Load_Score = 0,
        Quantity_Applied = 0,
        Risk_Score = 0,
        stringsAsFactors = FALSE
      )
      values1$data <- rbind(values1$data, new_row)
      values2$data <- rbind(values2$data, new_row)
    }
  })
  
  # Remove row functionality - affects both tables
  observeEvent(input$remove_row, {
    if (nrow(values1$data) > 1) {
      values1$data <- values1$data[-nrow(values1$data), ]
      values2$data <- values2$data[-nrow(values2$data), ]
    }
  })
  
  # Helper function to update calculations
  update_calculations <- function(data) {
    for (i in 1:nrow(data)) {
      if (data$Compound[i] != "" && !is.na(data$Compound[i])) {
        # Look up load score based on Compound
        matching_row <- data_hpli[data_hpli$compound == data$Compound[i], ]
        if (nrow(matching_row) > 0) {
          data$Load_Score[i] <- matching_row$load_score[1]
          # Calculate Risk_Score
          if (!is.na(data$Quantity_Applied[i]) && data$Quantity_Applied[i] > 0) {
            data$Risk_Score[i] <- data$Load_Score[i] * data$Quantity_Applied[i]
          } else {
            data$Risk_Score[i] <- 0
          }
        }
      } else {
        data$Load_Score[i] <- 0
        data$Risk_Score[i] <- 0
      }
    }
    return(data)
  }
  
  # Render table 1
  output$hot_table1 <- renderRHandsontable({
    if (!is.null(values1$data)) {
      values1$data <- update_calculations(values1$data)
      
      rhandsontable(
        values1$data,
        rowHeaders = TRUE,
        height = 250,
        colWidths = c(180, 100, 140, 100)
      ) %>%
        hot_col(
          "Compound",
          type = "dropdown",
          source = as.character(data_hpli$compound),
          allowInvalid = FALSE
        ) %>%
        hot_col(
          "Load_Score",
          readOnly = TRUE,
          type = "numeric",
          format = "0.000"
        ) %>%
        hot_col("Quantity_Applied", type = "numeric", format = "0.000") %>%
        hot_col("Risk_Score", readOnly = TRUE, format = "0.000") %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    }
  })
  
  # Render table 2
  output$hot_table2 <- renderRHandsontable({
    if (!is.null(values2$data)) {
      values2$data <- update_calculations(values2$data)
      
      rhandsontable(
        values2$data,
        rowHeaders = TRUE,
        height = 250,
        colWidths = c(180, 100, 140, 100)
      ) %>%
        hot_col(
          "Compound",
          type = "dropdown",
          source = as.character(data_hpli$compound),
          allowInvalid = FALSE
        ) %>%
        hot_col(
          "Load_Score",
          readOnly = TRUE,
          type = "numeric",
          format = "0.000"
        ) %>%
        hot_col("Quantity_Applied", type = "numeric", format = "0.000") %>%
        hot_col("Risk_Score", readOnly = TRUE, format = "0.000") %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    }
  })
  
  # Update data when table is edited
  observeEvent(input$hot_table1, {
    if (!is.null(input$hot_table1)) {
      # Get the updated data
      updated_data <- hot_to_r(input$hot_table1)

      # Process each row for auto-population and calculations
      for (i in 1:nrow(updated_data)) {
        if (!is.na(updated_data$Compound[i]) &&
            updated_data$Compound[i] != "") {
          # Auto-populate load score based on Compound
          matching_row <- data_hpli[data_hpli$compound == updated_data$Compound[i], ]
          if (nrow(matching_row) > 0) {
            updated_data$Load_Score[i] <- matching_row$load_score[1]
          }

          # Calculate Risk_Score (Load_Score * Quantity_Applied)
          if (!is.na(updated_data$Quantity_Applied[i]) &&
              updated_data$Quantity_Applied[i] > 0) {
            updated_data$Risk_Score[i] <- updated_data$Load_Score[i] * updated_data$Quantity_Applied[i]
          } else {
            updated_data$Risk_Score[i] <- 0
          }
        } else {
          # Reset values1 if no Compound selected
          updated_data$Load_Score[i] <- 0
          updated_data$Risk_Score[i] <- 0
        }
      }

      values1$data <- updated_data
    }
  })

  # Update data when table is edited
  observeEvent(input$hot_table2, {
    if (!is.null(input$hot_table2)) {
      # Get the updated data
      updated_data <- hot_to_r(input$hot_table2)
      
      # Process each row for auto-population and calculations
      for (i in 1:nrow(updated_data)) {
        if (!is.na(updated_data$Compound[i]) &&
            updated_data$Compound[i] != "") {
          # Auto-populate load score based on Compound
          matching_row <- data_hpli[data_hpli$compound == updated_data$Compound[i], ]
          if (nrow(matching_row) > 0) {
            updated_data$Load_Score[i] <- matching_row$load_score[1]
          }
          
          # Calculate Risk_Score (Load_Score * Quantity_Applied)
          if (!is.na(updated_data$Quantity_Applied[i]) &&
              updated_data$Quantity_Applied[i] > 0) {
            updated_data$Risk_Score[i] <- updated_data$Load_Score[i] * updated_data$Quantity_Applied[i]
          } else {
            updated_data$Risk_Score[i] <- 0
          }
        } else {
          # Reset values2 if no Compound selected
          updated_data$Load_Score[i] <- 0
          updated_data$Risk_Score[i] <- 0
        }
      }
      
      values2$data <- updated_data
    }
  })
  
  # Summary output
  output$summary1 <- renderText({
    if (!is.null(values1$data)) {
      # Filter to only filled rows (compounds that have been selected)
      filled_data <- values1$data[values1$data$Compound != "" &
                                    !is.na(values1$data$Compound), ]

      if (nrow(filled_data) > 0) {
        grand_total <- sum(values1$data$Risk_Score, na.rm = TRUE)

        # Find min and max risk scores among filled rows
        risk_min <- min(filled_data$Risk_Score, na.rm = TRUE)
        risk_max <- max(filled_data$Risk_Score, na.rm = TRUE)

        # Find compounds with min and max risk scores
        min_compound <- filled_data$Compound[which(filled_data$Risk_Score == risk_min)[1]]
        max_compound <- filled_data$Compound[which(filled_data$Risk_Score == risk_max)[1]]

        paste(
          "Lowest Risk Application:",
          "\n",
          min_compound,
          " (",
          format(risk_min, digits = 2, nsmall = 2),
          ")",
          "\n\nHighest Risk Application:",
          "\n",
          max_compound,
          " (",
          format(risk_max, digits = 2, nsmall = 2),
          ")"
        )
      } else {
        "No compounds have been selected yet."
      }
    }
  })

  output$summary2 <- renderText({
    if (!is.null(values2$data)) {
      # Filter to only filled rows (compounds that have been selected)
      filled_data <- values2$data[values2$data$Compound != "" &
                                    !is.na(values2$data$Compound), ]
      
      if (nrow(filled_data) > 0) {
        grand_total <- sum(values2$data$Risk_Score, na.rm = TRUE)
        
        # Find min and max risk scores among filled rows
        risk_min <- min(filled_data$Risk_Score, na.rm = TRUE)
        risk_max <- max(filled_data$Risk_Score, na.rm = TRUE)
        
        # Find compounds with min and max risk scores
        min_compound <- filled_data$Compound[which(filled_data$Risk_Score == risk_min)[1]]
        max_compound <- filled_data$Compound[which(filled_data$Risk_Score == risk_max)[1]]
        
        paste(
          "Lowest Risk Application:",
          "\n",
          min_compound,
          " (",
          format(risk_min, digits = 2, nsmall = 2),
          ")",
          "\n\nHighest Risk Application:",
          "\n",
          max_compound,
          " (",
          format(risk_max, digits = 2, nsmall = 2),
          ")"
        )
      } else {
        "No compounds have been selected yet."
      }
    }
  })
  
  # Value boxes for dashboard display
  output$total_risk1 <- renderValueBox({
    if (!is.null(values1$data)) {
      grand_total <- sum(values1$data$Risk_Score, na.rm = TRUE)
      valueBox(
        value = format(grand_total, digits = 2, nsmall = 0),
        subtitle = "Total Risk Score",
        icon = icon("exclamation-triangle"),
        color = "red"
      )
    }
  })

  output$item_count1 <- renderValueBox({
    if (!is.null(values1$data)) {
      total_items <- sum(values1$data$Quantity_Applied, na.rm = TRUE)
      valueBox(
        value = format(total_items, digits = 2, nsmall = 2),
        subtitle = "Total Quantity of Compounds Applied",
        icon = icon("cubes"),
        #color = "blue"
        color = "red"
      )
    }
  })

  output$filled_rows1 <- renderValueBox({
    if (!is.null(values1$data)) {
      filled_rows <- sum(values1$data$Compound != "", na.rm = TRUE)
      valueBox(
        value = filled_rows,
        subtitle = "Number of Applications Entered",
        icon = icon("list"),
        #color = "yellow"
        color = "red"
      )
    }
  })

  output$total_risk2 <- renderValueBox({
    if (!is.null(values2$data)) {
      grand_total <- sum(values2$data$Risk_Score, na.rm = TRUE)
      valueBox(
        value = format(grand_total, digits = 2, nsmall = 0),
        subtitle = "Total Risk Score",
        icon = icon("exclamation-triangle"),
        color = "yellow"
      )
    }
  })
  
  output$item_count2 <- renderValueBox({
    if (!is.null(values2$data)) {
      total_items <- sum(values2$data$Quantity_Applied, na.rm = TRUE)
      valueBox(
        value = format(total_items, digits = 2, nsmall = 2),
        subtitle = "Total Quantity of Compounds Applied",
        icon = icon("cubes"),
        #color = "blue"
        color = "yellow"
      )
    }
  })
  
  output$filled_rows2 <- renderValueBox({
    if (!is.null(values2$data)) {
      filled_rows <- sum(values2$data$Compound != "", na.rm = TRUE)
      valueBox(
        value = filled_rows,
        subtitle = "Number of Applications Entered",
        icon = icon("list"),
        #color = "yellow"
        color = "yellow"
      )
    }
  })
  

}

# run app -----------------------------------------------------------------
shinyApp(ui = ui, server = server)
