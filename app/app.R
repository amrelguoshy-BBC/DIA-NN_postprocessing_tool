# Load global settings and libraries
source("global.R")

# Load UI and server logic
source("ui.R")
source("server.R")

# Run the Shiny app
shinyApp(ui = ui, server = server)