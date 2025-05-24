# Load global settings and libraries
source("global.R")

# Load UI and server logic
source("ui00_update.R")
source("server03_update.R")

# Run the Shiny app
shinyApp(ui = ui, server = server)