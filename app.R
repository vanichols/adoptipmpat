library(shiny)
library(rhandsontable)
library(shinydashboard)
library(tidyverse)
library(readxl)

# global ------------------------------------------------------------------
# Note: These data files would need to exist in your app directory
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
      id = "sidebar_menu", # Add ID here for conditionalPanel
      menuItem("  Pesticide Data", tabName = "pest", icon = icon("bugs")),
      menuItem("  Performance Data", tabName = "perf", icon = icon("leaf"))
    ),
    
    # Pesticide data entry specific sidebar content
    conditionalPanel(
      condition = "input.sidebar_menu == 'pest'", # This works with the ID
      br(),
      h4("Table Instructions", style = "padding-left: 15px; color: white;"),
      div(
        style = "padding-left: 15px; color: white; font-size: 12px;",
        p("• Select a compound from the dropdown"),
        p("• Load score will auto-populate"),
        p("• Enter the quantity of compound applied (in consistent units for the entire table)"),
        p("• The compound's risk score will be calculated automatically"),
        p("• The total risk score for the pesticide package is displayed at the bottom")
      ),
      br(),
      div(
        style = "padding-left: 15px;",
        actionButton("add_row", "Add Row", class = "btn-primary btn-sm", style = "margin-bottom: 10px;"),
        br(),
        actionButton("remove_row", "Remove Row", class = "btn-warning btn-sm", style = "margin-bottom: 15px;"),
        br(),
        numericInput("max_rows", "Max Rows:", value = 5, min = 1, max = 50, width = "150px")
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
      # Image now correctly referenced from www folder
      img(
        src = "adopt-ipm_logo-clean.png", # Make sure this file is in www/adopt-ipm_logo-clean.png
        height = "50px",
        width = "auto",
        style = "margin-bottom: 5px;"
      ),
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
    tabItems(
      ###### Body: Pesticide table tab ######
      tabItem(
        tabName = "pest",
        # First row: Table and Summary Statistics side by side for both packages
        fluidRow(
          box(
            title = "Editable Table with Calculations",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            height = "500px",
            rHandsontableOutput("hot_table1")
          ),
          box(
            title = "Summary Statistics",
            status = "info",
            solidHeader = TRUE,
            width = 2,
            height = "500px",
            verbatimTextOutput("summary1")
          ),
          column(width = 6)
        ),
        
        # Second row: Data Information spanning full width
        fluidRow(
          box(
            title = "Data Information",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            valueBoxOutput("total_risk1"),
            valueBoxOutput("item_count1"),
            valueBoxOutput("filled_rows1")
          ),
          column(width = 6)
        )
      ) #--end of tab
    )#--end of dashboard body   
  )
)

# server ------------------------------------------------------------------
server <- function(input, output) {
  # Add your server logic here
}

# run app -----------------------------------------------------------------
shinyApp(ui = ui, server = server)
