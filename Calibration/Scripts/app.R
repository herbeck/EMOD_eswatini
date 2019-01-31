library(shiny)
library(ggplot2)

##########################################
#Start Shiny webpage
##########################################

#UI
ui <- fluidPage(
  sliderInput(inputId="number", 
              label="Choose a number", 
              value=20,min = 1, max=20),
  plotOutput("histogram"),
  verbatimTextOutput("stats")
)

#Server
server <- function(input, output){
  data <- reactive({rnorm(input$number)})
  output$histogram <- renderPlot({
  title <- "n random normal values"
    hist(data())
  })
  output$stats <- renderPrint({
    summary(data())
  })
}
shinyApp(ui = ui, server=server)

##########################################
#Plot calibration data in ggplot
##########################################
#set working dir
wd <- "C:\\Users\\aakullian\\Documents\\GitHub\\EMOD_eswatini\\Calibration\\Data"
setwd(wd)
inc.smooth_vals.c <- read.csv("EmodIncidenceSmoothedSims.csv")

#UI#
ui <- fluidPage(
  titlePanel(title=h4("HIV incidence in eSwatini, ages 15-49", align="center")),
  sidebarLayout(  
    sidebarPanel(
      checkboxGroupInput(
        "gender_choose",
        "gender:",
        choices=c("Female"="Women","Male"="Men","Both"="Combined")
        ),
        inline = TRUE,
        selected = "Combined"
      ),
      # sliderInput(
      #   "year", 
      #   "Year:",
      #   min = min(df$year), max = max(df$year),
      #   value=c(min(df$year), min(df$year)),
      #   sep = "_"
      #   )
      # ),
  
      mainPanel(
        plotOutput("ggplot1")
      )
    )
)

#Server#
server <- function(input, output){
   df <- reactive({
     # inc.smooth_vals.c[inc.smooth_vals.c$year >= input$year[1] & inc.smooth_vals.c$year <= input$year[2]
     #                   & inc.smooth_vals.c$gender == input$gender,]
     return(inc.smooth_vals.c[inc.smooth_vals.c$gender %in% input$gender_choose,])
   })
    
  output$ggplot1 <- renderPlot({
    ggplot(data=df(),aes(x=year, y=incidence*100, group=gender, color=gender)) +
      geom_line() +
      xlab("Year")+
      ylab("Incidence (per 100 py)")+
      scale_y_continuous(breaks = seq(0,5,1),limits=c(0,5),expand = c(0,0)) +
      scale_x_continuous(expand = c(0,0)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      theme(strip.background = element_rect(colour="black", fill="white")) +
      theme(legend.position="bottom") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_blank(),
            panel.border = element_rect(colour = "black")) +
      theme_bw(base_size=16) }, height = 500, width = 500)
      #facet_grid(~df$gender)
}
shinyApp(ui = ui, server=server)



