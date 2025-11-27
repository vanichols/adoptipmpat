create_system_tab <- function() {
  tabItem(
    tabName = "sys",
    create_system_input_row(),
    create_system_summary_row()
  )
}

create_system_input_row <- function() {
  fluidRow(
    box(
      title = "Pesticides Applied",
      status = "primary",
      solidHeader = TRUE,
      width = 6,
      height = "225px",
      rHandsontableOutput("pest_hottable")
    ),
    box(
      title = "Pesticides Insight",
      status = "primary",
      solidHeader = TRUE,
      width = 6,
      height = "225px",
      verbatimTextOutput("pest_insight")
    )
  )
}

create_system_summary_row <- function() {
  fluidRow(
    box(
      title = "Pesticides Impact Summary",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      height = "175px",
      fluidRow(
        column(4, valueBoxOutput("pest_totalrisk", width = 12)),
        column(4, valueBoxOutput("pest_itemcount", width = 12)),
        column(4, valueBoxOutput("pest_rows", width = 12))
      )
    )
  )
}

