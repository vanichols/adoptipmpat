# Additional helper functions for system tab components
create_systemcomp_tab <- function() {
  tabItem(tabName = "syscomp", 
          fluidRow(
            box(
              title = "System Comparison - Coming Soon",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              div(
                style = "text-align: center; padding: 40px;",
                h3("System Comparison Feature", style = "color: #2c3e50;"),
                p("This feature will allow side-by-side comparison of different agricultural management systems."),
                br(),
                p("Coming in a future update!", style = "font-style: italic; color: #7f8c8d;")
              )
            )
          )
  )
}
