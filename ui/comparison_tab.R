create_comparison_tab <- function() {
  tabItem(
    tabName = "subcomp",
    create_comparison_selection_row(),
    create_comparison_plots_row()
  )
}

create_comparison_selection_row <- function() {
  fluidRow(
    # First substance selection
    box(
      title = "First substance selection",
      status = "primary",
      solidHeader = TRUE,
      width = 4,
      height = "275px",
      
      selectizeInput(
        "substance_category1",
        label = NULL,
        choices = NULL,
        multiple = TRUE,
        selected = NULL,
        options = list(placeholder = "Filter by category")
      ),
      selectizeInput(
        "substance_origins1",
        label = NULL,
        choices = NULL,
        multiple = TRUE,
        selected = NULL,
        options = list(placeholder = "Filter by origin")
      ),
      selectInput(
        "substance_double1",
        "Select Substance:",
        choices = NULL,
        selected = NULL
      )
    ),
    
    # Second substance selection
    box(
      title = "Second substance selection",
      status = "primary",
      solidHeader = TRUE,
      width = 4,
      height = "275px",
      
      selectizeInput(
        "substance_category2",
        label = NULL,
        choices = NULL,
        multiple = TRUE,
        selected = NULL,
        options = list(placeholder = "Filter by category")
      ),
      selectizeInput(
        "substance_origins2",
        label = NULL,
        choices = NULL,
        multiple = TRUE,
        selected = NULL,
        options = list(placeholder = "Filter by origin")
      ),
      
      selectInput(
        "substance_double2",
        "Select Substance:",
        choices = NULL,
        selected = NULL
      )
    ),
    
    # Placeholder/instructions box
    box(
      title = "Comparison Instructions",
      status = "info",
      solidHeader = TRUE,
      width = 4,
      height = "275px",
      div(
        style = "padding: 20px;",
        h5("How to use this comparison:"),
        tags$ul(
          style = "font-size: 14px; line-height: 1.6;",
          tags$li("Select substances from the dropdowns on the left"),
          tags$li("Use filters to narrow down substance choices"),
          tags$li("View load scores side-by-side in the plots below"),
          tags$li("The rightmost plot shows both substances relative to all substances in the database")
        ),
        br(),
        p("This comparison helps you understand the relative impact of different pesticide substances.",
          style = "font-size: 13px; color: #666; font-style: italic;")
      )
    )
  )
}

create_comparison_plots_row <- function() {
  fluidRow(
    # Rose plot first substance
    box(
      title = "First Substance Load Scores",
      status = "primary",
      solidHeader = TRUE,
      width = 4,
      plotOutput("rose_plot1", height = "500px")
    ),
    
    # Rose plot second substance
    box(
      title = "Second Substance Load Scores",
      status = "primary",
      solidHeader = TRUE,
      width = 4,
      plotOutput("rose_plot2", height = "500px")
    ),
    
    # Distribution plot for both substances
    box(
      title = "Load Score(s) Relative to All Substances",
      status = "primary",
      solidHeader = TRUE,
      width = 4,
      plotOutput("dist_plot_both", height = "500px")
    )
  )
}