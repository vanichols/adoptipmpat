library(shiny)
library(rhandsontable)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(ggnewscale)
library(ggrepel)
library(ggpattern)

# global ------------------------------------------------------------------

# Source utility functions (rose plot, distribution plot)
source("R/utils.R")

#--load data
data_hpli <- read_rds("data/processed/data_hpli.RDS")
data_betas <- read_rds("data/processed/data_betas.RDS")
data_example <- read_rds("data/processed/data_example.RDS")

# ui ----------------------------------------------------------------------

source("ui/ui.R")

# server ------------------------------------------------------------------

source("server/server.R")

# run app -----------------------------------------------------------------
shinyApp(ui = ui, server = server)
