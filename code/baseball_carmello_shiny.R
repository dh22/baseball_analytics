#shiny_bbcarmello_ui
library(shiny)
ui <- fluidPage("Hello World")
# *Input() funcitons (inputId=unique name for refernce in coding, label= this is what the user sees)
#  *Output() plotOutput(outputID = unique name to use for output)



server <- function(input,output){}




# output object should be  output$outputID <-  renderPlstuff



shinyApp(ui=ui,server = server)