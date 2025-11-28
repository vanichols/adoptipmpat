create_sidebar <- function() {
  shinydashboard::dashboardSidebar(
    ### Menu ###
    shinydashboard::sidebarMenu(
      id = "sidebar_menu",
      menuItem("  Welcome", tabName = "welcome", icon = icon("campground")),
      menuItem(
        "  Single Substance View",
        tabName = "single",
        icon = icon("flask")
      ),
      menuItem(
        "  Substance Comparison View",
        tabName = "subcomp",
        icon = icon("flask-vial")
      ),
      menuItem(
        "  Single System Insights",
        tabName = "sys",
        icon = icon("bug")
      ),
      menuItem(
        "  System Comparison",
        tabName = "syscomp",
        icon = icon("bugs")
      ),
      menuItem(
        "  Example case study",
        tabName = "example",
        icon = icon("bacon")
      )
    ),
    
    # Conditional panels for different tabs
    create_system_sidebar1(),
    create_system_sidebar2(),
    create_footer()
  )
}

create_system_sidebar1 <- function() {
  conditionalPanel(
    condition = "input.sidebar_menu == 'sys'",
    br(),
    h4("Table Instructions", style = "padding-left: 15px; color: white;"),
    div(
      style = "padding-left: 15px; color: white; font-size: 12px;",
      p("• Select a compound from the dropdown"),
      p("• Load score will auto-populate"),
      p(
        "• Enter the quantity of compound applied (in consistent units for the entire table)"
      ),
      p("• Compound's risk score will be calculated automatically")
    ),
    br(),
    div(
      style = "padding: 15px;",
      actionButton("add_row", "Add Row", class = "btn-primary btn-sm", style = "margin-bottom: 10px;"),
      br(),
      actionButton("remove_row", "Remove Row", class = "btn-warning btn-sm", style = "margin-bottom: 15px;")
    )
    
  )
}

# Conditional panel for "single" tab
create_system_sidebar2 <- function(){
  conditionalPanel(
  condition = "input.sidebar_menu == 'single'",
  h4("Plot Options"),
  p("Choose to see details for each metric"),
  checkboxInput("detailed_view", "Detailed plot view", value = FALSE),
  div(
    style = "padding: 15px; color: white; font-size: 12px;",
    p("• Quality of the data ranges from 1 (low) to 5 (high)"),
    p("• Data may be missing (X, dashed filling) or not reported (NR)"),
    )
  
  )
}

create_footer <- function() {
  div(
    style = "position: fixed;
             bottom: 15px;
             left: 15px;
             font-size: 12px;
             color: #888;
             z-index: 1000;",
    img(
      src = "test.png",
      height = "50px",
      width = "auto",
      style = "margin-bottom: 5px;",
      onerror = "this.style.display='none'; console.log('Image failed to load');"
    ),
    br(),
    HTML(
      "<a href='https://adopt-ipm.eu/' target='_blank'>adopt-ipm.eu</a><br>
       Nichols and Vandevoorde (2025)<br>
       Last updated: Nov 2025<br>"
    )
  )
}