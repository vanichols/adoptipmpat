# Main UI file that combines all UI components
source("ui/sidebar.R")
source("ui/welcome_tab.R")
source("ui/single_tab.R")
source("ui/comparison_tab.R")
source("ui/system_tab.R")
source("ui/systemcomp_tab.R")
source("ui/example_tab.R")

ui <- shinydashboard::dashboardPage(
  ###### Header ##################################################################
  shinydashboard::dashboardHeader(title = "ADOPT-IPM online performance assessment tool"),
  
  ###### Sidebar #################################################################
  create_sidebar(),
  
  ###### Body ####################################################################
  shinydashboard::dashboardBody(
    tags$head(tags$style(
      HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
      ")
    )),
    
    tabItems(
      create_welcome_tab(),
      create_single_tab(),
      create_comparison_tab(),
      create_system_tab(),
      create_systemcomp_tab(),  # Not compelte
      create_example_tab()              # Not complete
    )
  )
)