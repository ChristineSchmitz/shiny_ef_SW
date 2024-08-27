#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
orchard_data_table_input<-read.csv2("test_input.csv", colClasses = c("character", "character", "numeric", "character","numeric", "character"), sep = ";", dec = ".")                      

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Apple yield"),
  
  # Sidebar with a slider input  
  sidebarLayout(
    sidebarPanel(
      sliderInput("x",
                  "Number of trees per ha:",
                  min = 1,
                  max = 5000,
                  value = 2571),
      sliderInput("y",
                  "Apples per tree:",
                  min = 1,
                  max = 1000,
                  value = c(80,120))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot",height = "800px")#,
      #plotOutput("distPlot2")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    library(decisionSupport)
    library(tidyverse)
    variable<-c("trees_per_ha", "apples_per_tree", "g_per_apple")
    distribution<-c("const", "posnorm","posnorm")
    lower<-c(input$x,input$y[1],150)
    median<-rep(NA, 3)
    upper<-c(input$x,input$y[2],200)
    unit<-c("trees","apples","g")
    orchard_data_slider_input<-data.frame(variable,distribution,lower,median,upper,unit)
    #orchard_data<-read.csv2("2024_test_apple/test_input.csv", colClasses = c("character", "character", "numeric", "character","numeric", "character"), sep = ";", dec = ".")
    orchard_data<-rbind(orchard_data_table_input, orchard_data_slider_input)
    apple_yield_function<-function(){

      yield<-trees_per_ha*apples_per_tree*g_per_apple/1000
      income<-yield*price_per_kg
      result<-income-production_costs_per_ha

      return(list(yield_at_harvest = yield/1000,
                  orchard_income=income,
                  yearly_revenue = result))
    }

    apple_yield_mc_simulation <- mcSimulation(estimate = as.estimate(orchard_data),
                                              model_function = apple_yield_function,
                                              numberOfModelRuns = 10000,
                                              functionSyntax = "plainNames")
    
    Plot_a<-plot_distributions(mcSimulation_object = apple_yield_mc_simulation, 
                       vars = c("yield_at_harvest"),
                       method = 'hist_simple_overlay', 
                       base_size = 7)+
      theme(axis.text = element_text(colour = "black", size = 10),
            axis.title = element_text(colour = "black", size = 10))+
      xlab("Ertrag [t/ha]")+
      ylab("Häufigkeit")
    
    Plot_b<-plot_distributions(mcSimulation_object = apple_yield_mc_simulation, 
                       vars = c("yearly_revenue"),
                       method = 'hist_simple_overlay', 
                       base_size = 7)+
      theme(axis.text = element_text(colour = "black", size = 10),
            axis.title = element_text(colour = "black", size = 10))+
      xlab("Gewinn [€/ha]")+
      ylab("Häufigkeit")
    
    library(patchwork)
    Plots_combined <- list(Plot_a, Plot_b)
    wrap_plots(Plots_combined, nrow = 2) +
      plot_layout(guides = "keep")

  })
  
  # output$distPlot2 <- renderPlot({
  #   
  #   # trees_per_ha2<-input$x
  #   # library(decisionSupport)
  #   # library(tidyverse)
  #   # variable<-c("trees_per_ha", "apples_per_tree", "g_per_apple")
  #   # distribution<-c("const", "posnorm","posnorm")
  #   # lower<-c(input$x,input$y[1],150)
  #   # median<-rep(NA, 3)
  #   # upper<-c(input$x,input$y[2],200)
  #   # unit<-c("trees","apples","g")
  #   # orchard_data_slider_input<-data.frame(variable,distribution,lower,median,upper,unit)
  #   # #orchard_data<-read.csv2("2024_test_apple/test_input.csv", colClasses = c("character", "character", "numeric", "character","numeric", "character"), sep = ";", dec = ".")                      
  #   # orchard_data<-rbind(orchard_data_table_input, orchard_data_slider_input)
  #   # apple_yield_function<-function(){
  #   #   
  #   #   yield<-trees_per_ha*apples_per_tree*g_per_apple/1000
  #   #   income<-yield*price_per_kg
  #   #   result<-income-production_costs_per_ha
  #   #   
  #   #   return(list(yield_at_harvest = yield/1000,
  #   #               orchard_income=income,
  #   #               yearly_revenue = result))
  #   # }
  #   # 
  #   # apple_yield_mc_simulation <- mcSimulation(estimate = as.estimate(orchard_data),
  #   #                                           model_function = apple_yield_function,
  #   #                                           numberOfModelRuns = 10000,
  #   #                                           functionSyntax = "plainNames")
  # 
  #   test_sim()
  #   plot_distributions(mcSimulation_object = apple_yield_mc_simulation, 
  #                      vars = c("yearly_revenue"),
  #                      method = 'hist_simple_overlay', 
  #                      base_size = 7)+
  #     theme(axis.text = element_text(colour = "black", size = 10),
  #           axis.title = element_text(colour = "black", size = 10))+
  #     xlab("Gewinn [€/ha]")+
  #     ylab("Häufigkeit")
  # })
  # # output$distPlot <- renderPlot({
  # #   # generate bins based on input$bins from ui.R
  # #   x    <- faithful[, 2]
  # #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
  # #   
  # #   # draw the histogram with the specified number of bins
  # #   hist(x, breaks = bins, col = 'darkgray', border = 'white')
  # # })
}

# Run the application 
shinyApp(ui = ui, server = server)
