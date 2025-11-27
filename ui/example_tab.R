
create_example_tab <- function() {
  tabItem(tabName = "example", 
          fluidRow(
            box(
              title = "Example Case Study - Coming Soon",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              div(
                style = "text-align: center; padding: 40px;",
                h3("Netherlands Case Study", style = "color: #2c3e50;"),
                p("An example comparing field cropping and strip cropping in the Netherlands."),
                br(),
                p("This detailed case study will demonstrate the practical application of the Harmonized Pesticide Load Index."),
                br(),
                p("Coming in a future update!", style = "font-style: italic; color: #7f8c8d;")
              )
            )
          )
  )
}