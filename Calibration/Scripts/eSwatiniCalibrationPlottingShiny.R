library(shiny)

##########################################
#Start Shiny webpage
##########################################
ui <- fluidPage("Hello World puppy dog")
server <- function(input, output){}
shinyApp(ui = ui, server=server)
##########################################

#Input Functions
sliderInput(inputId="num", label="", min = 0, max=1)

#Output Functions
ui <- fluidPage(
  sliderInput(inputId="num", 
              label="Choose a number", 
              value=100,min = 1, max=1000),
  plotOutput("hist")
)  
  
#Render functions
server <- function(input, output){
  output$hist <- renderPlot({
  title <- "n random normal values"
    hist(rnorm(input$num))
  })
}
shinyApp(ui = ui, server=server)





