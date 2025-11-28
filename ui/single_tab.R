create_single_tab <- function() {
  tabItem(
    tabName = "single",
    create_single_selection_row(),
    create_single_plots_row()
  )
}

create_single_selection_row <- function() {
  fluidRow(
    # Substance selection box
    box(
      title = "Substance Selection",
      status = "primary",
      solidHeader = TRUE,
      width = 4,
      height = "300px",
      
      selectizeInput(
        "substance_category",
        label = NULL,
        choices = NULL,
        multiple = TRUE,
        selected = NULL,
        options = list(placeholder = "Filter by category")
      ),
      selectizeInput(
        "substance_origins",
        label = NULL,
        choices = NULL,
        multiple = TRUE,
        selected = NULL,
        options = list(placeholder = "Filter by origin")
      ),
      
      selectInput(
        "substance_single",
        "Select Substance:",
        choices = NULL,
        selected = NULL
      )
    ),
    
    # Substance information box
    box(
      title = "Substance Information",
      status = "primary",
      solidHeader = TRUE,
      width = 4,
      height = "300px",
      verbatimTextOutput("substance_info")
    ),
    
    # Download Data box
    box(
      title = "Download Load Score Details",
      status = "primary",
      solidHeader = TRUE,
      width = 4,
      height = "300px",
      div(
        style = "text-align: center; padding: 20px;",
        p("Download the detailed load score data for the selected substance:"),
        br(),
        downloadButton(
          "download_data",
          "Download Data (TSV)",
          class = "btn-success btn-lg",
          icon = icon("download"),
          style = "background-color: #ffd74a; border-color: #ffd74a;"
        )
      )
    )
  )
}

create_single_plots_row <- function() {
  fluidRow(
    #--Rose plot box
    box(
      title = "Load Scores by Compartment",
      status = "primary",
      solidHeader = TRUE,
      width = 4,
      plotOutput("rose_plot", height = "500px")
    ),
    #--Distribution box
    box(
      title = "Load Score Relative to All Substances",
      status = "primary",
      solidHeader = TRUE,
      width = 4,
      plotOutput("dist_plot", height = "500px")
    ),
    # Information and links box
    box(
      title = "Additional Resources",
      status = "info",
      solidHeader = TRUE,
      width = 4,
      div(
        style = "padding: 15px;",
        h4("About Load Scores"),
        p("Load scores represent a relative toxicity burden ."),
        p(
          "The visualization shows a substance's load scores for each compartment, as calculated by Vandervoode et al. (in review)"
        ),
        br(),
        h4("Useful Links"),
        tags$ul(tags$li(
          tags$a(
            "Pesticide Properties Database",
            href = "https://sitem.herts.ac.uk/aeru/ppdb/",
            target = "_blank"
          )
        ), tags$li(
          tags$a(
            "PhD manuscript with more details and background",
            href = "https://sytra.be/publication/three-tools-reduction-pesticide-impacts/",
            target = "_blank"
          )
        )),
        br()
      )
    )
  )
}