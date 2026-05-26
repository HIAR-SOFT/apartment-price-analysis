
# Run with: shiny::runApp("shiny-app")

library(shiny)
library(here)

source(here("shiny-app", "ui.R"))
source(here("shiny-app", "server.R"))

shinyApp(ui = ui, server = server)