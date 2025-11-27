create_welcome_tab <- function() {
  tabItem(tabName = "welcome", 
          fluidRow(
            box(
              title = "Welcome to PESTO",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              
              h3("Getting Started"),
              p(
                "Welcome to our dashboard! Below is an overview of the tabs and some useful resources:",
                style = "font-size: 16px; margin-bottom: 20px;"
              ),
              
              create_dashboard_contents(),
              
              hr(style = "margin: 30px 0; border-top: 2px solid #bdc3c7;"),
              
              create_additional_resources(),
              
              create_tldr_section()
            )
          )
  )
}

create_dashboard_contents <- function() {
  tagList(
    h4("Dashboard Contents", style = "color: #2c3e50; margin-top: 25px;"),
    tags$ul(
      style = "line-height: 1.8; font-size: 15px;",
      tags$li(
        tags$strong("Single substance view", style = "color: #eb5e23;"),
        " presents detailed information on the impact of substances used in agricultural settings (as calculated by the ",
        tags$em("Harmonized Pesticide Load Index", style = "color: #8e44ad;"),
        ")"
      ),
      tags$li(
        tags$strong("Substance comparison view", style = "color: #eb5e23;"),
        " allows side-by-side comparison of substance impacts"
      ),
      tags$li(
        tags$strong("Single system insights", style = "color: #f39c12;"),
        " presents a wholistic performance of a management system (based on the ",
        tags$em("Harmonized Pesticide Load Index", style = "color: #8e44ad;"),
        ")"
      ),
      tags$li(
        tags$strong("System comparison", style = "color: #f39c12;"),
        " allows side-by-side comparison of performances"
      ),
      tags$li(
        tags$strong("Example case study", style = "color: #27ae60;"),
        " presents an example comparing field cropping and strip cropping in the Netherlands"
      )
    )
  )
}

create_additional_resources <- function() {
  tagList(
    h4("Additional Resources", style = "color: #2c3e50; margin-bottom: 15px;"),
    tags$ul(
      style = "line-height: 2; font-size: 15px;",
      tags$li(
        "Read the ",
        tags$strong("dissertation", style = "color: #2980b9;"),
        " describing calculation of the ",
        tags$em("Harmonized Pesticide Load Index", style = "color: #8e44ad;"),
        " (publication is in review): ",
        tags$a(
          "Vandevoorde 2025",
          href = "https://sytra.be/publication/three-tools-reduction-pesticide-impacts/",
          target = "_blank",
          style = "color: #eb5e23; text-decoration: none; font-weight: bold;
                    border-bottom: 1px dotted #eb5e23;"
        )
      ),
      tags$li(
        "Read the ",
        tags$strong("EU Horizon 2020 project deliverable", style = "color: #2980b9;"),
        " describing the performance tool this methodology is based on: ",
        tags$a(
          "Benefits of IPM to endusers",
          href = "https://cordis.europa.eu/project/id/633999/results",
          target = "_blank",
          style = "color: #f39c12; text-decoration: none; font-weight: bold;
                    border-bottom: 1px dotted #f39c12;"
        )
      ),
      tags$li(
        "Read the ",
        tags$strong("accompanying publication", style = "color: #2980b9;"),
        " to this dashboard: ",
        tags$a(
          "Publication in progress, here is the project website",
          href = "https://adopt-ipm.eu/",
          target = "_blank",
          style = "color: #27ae60; text-decoration: none; font-weight: bold;
                    border-bottom: 1px dotted #27ae60;"
        )
      )
    )
  )
}

create_tldr_section <- function() {
  div(
    style = "margin-top: 30px; padding: 15px; background-color: #ecf0f1; border-radius: 5px;",
    h5("TL;DR?", style = "color: #2c3e50; margin-bottom: 10px;"),
    p(
      "Navigate through the different tabs using the sidebar to explore all available features.
        Each section provides detailed insights into pesticide impacts and agricultural management systems.",
      style = "margin-bottom: 0; font-size: 14px; color: #34495e;"
    )
  )
}