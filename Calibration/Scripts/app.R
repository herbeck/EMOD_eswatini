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
setwd("C:\\Users\\aakullian\\Documents\\GitHub\\EMOD_eswatini\\Calibration\\Data")
inc.smooth_vals.c <- read.csv("EmodIncidenceSmoothedSims.csv")
trajectories_IR_comb <- read.csv("EmodIncidence250Sims.csv")
table(trajectories_IR_comb$Gender)
trajectories_IR_comb$Gender <- as.factor(trajectories_IR_comb$Gender)
class(trajectories_IR_comb$Year2)

#css
alignCenter <- function(el) {
  htmltools::tagAppendAttributes(el,
        style="margin-left:auto;margin-right:auto;"
  )
}

#UI#
ui <- fluidPage(
  titlePanel("HIV incidence in eSwatini, ages 15-49"),
  sidebarLayout(  
    sidebarPanel(
      checkboxGroupInput(
        "Gender",
        "Select strata:",
        choices=c(2,0,1)
        ),
        inline = TRUE,
        selected = c(2,0,1)
      ),
    alignCenter(sliderInput(
        "Year2",
        "Year:",
        min = 1980, max = 2050,
        value=c(1980,2050),
        step = 1,
        sep = "",
        ticks=T
        ))
      ),
  # column(4,
  #        selectInput('facet_row', 'Facet Row', "Gender")
  # ),
  mainPanel(plotOutput("ggplot1"))
      )

#Server#
server <- function(input, output){
   
  dataset <- reactive({
     return(trajectories_IR_comb[trajectories_IR_comb$Gender %in% input$Gender 
                              & trajectories_IR_comb$Year2 >= input$Year2[1] & trajectories_IR_comb$Year2 <= input$Year2[2],])
   })
    
  color.groups <- c("2" = 'purple', "0" = 'blue', "1" = 'red')
  output$ggplot1 <- renderPlot({
  p <- ggplot() +
      #geom_line(data=dataset(),aes(x=Year2, y=incidence*100, color=Gender, group=interaction(Gender,sim.id)), size=1, alpha=0.05) +
      geom_smooth(data=dataset(), aes(x=Year2, y=incidence*100, color=Gender, group=Gender),method="loess", span=0.1, se = T, size=1, linetype=1) +
      xlab("Year")+ ylab("Incidence (per 100 py)")+
      scale_y_continuous(breaks = seq(0,5,1),limits=c(0,5),expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
      scale_color_manual(values = color.groups) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      theme(strip.background = element_rect(colour="black", fill="white")) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            strip.background = element_blank(),  panel.border = element_rect(colour = "black")) + theme_bw(base_size=16) 
  
  # facets <- paste('~', input$facet_row)
  # if (facets != '~ .')
  #   p <- p + facet_grid(facets)
    print(p)
  
  }, height = 500, width = 800)

}
shinyApp(ui = ui, server=server)



