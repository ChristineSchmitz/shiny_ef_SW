#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(datamods)
library(bslib)
library(reactable)
Apple_input<-read.csv2("apple_quality_input_manuscript.csv", colClasses = c("character", "character", "numeric", "character","numeric", "character","character"), sep = ";", dec = ".")
Apple_estimation<-read.csv2("apple_estimation_manuscript.csv",colClasses = c("character", "character", "numeric", "character","numeric", "character","character"), sep = ";", dec = ".")

Apple_prediction_input_csv<-rbind(Apple_input[1:7], Apple_estimation[1:7])
source("functions_v2.R")
#Management_values<-data.frame(management_measure,value)

#App####

app_theme <- bslib::bs_theme(
  version = 5, 
  #bootswatch = "sketchy", 
  bg = "#C7E9EA", 
  fg = "#0A655E", 
  primary = "#429323",   # 
  secondary = "#FBBA00",
  base_font = "Arial"
)
# Define UI for application that draws a histogram
#UI####
ui <- fluidPage(
  theme = app_theme,
  # Application title
  #titlePanel(),
  navbarPage(title = (span(img(src="Bild1.png", height =100),"Ertrags- und Qualitätsprognose -Apfel-")),
             theme = app_theme,
             header=tags$head(tags$style(type='text/css', ".irs-grid-text { font-size: 10pt; }")),
             tabPanel(title = (span(img(src="roter_Apfel.png", height =80),"4 Wochen vor Ernte")),
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("x",
                                      "Bäume pro ha:",
                                      min = 1,
                                      max = 5000,
                                      value = 2571),
                          sliderInput("y",
                                      "Früchte pro Baum (4 Wochen vor Ernte):",
                                      min = 1,
                                      max = 1000,
                                      value = c(80,120)),
                          sliderInput("z",
                                      "Bereits sichtbar beschädigte Früchte [%]:",
                                      min = 1,
                                      max = 100,
                                      value = c(10,20)),
                          sliderInput("Durchmesser",
                                      "Durchmesser 4 Wochen vor Ernte [cm]:",
                                      min = 0.5,
                                      max = 10,
                                      value = c(5.9,6.5)),
                          sliderInput("runs",
                                      "Anzahl Monte-Carlo Durchläufe:",
                                      min = 1,
                                      max = 15000,
                                      value = 1000),
                          tags$style(HTML(".radio-inline {margin-right: 42px;}")),
                          radioButtons("drop",
                                       "Behandlung gegen Vorerntefruchtfall",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("hailnet",
                                       "Hagelnetz",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("cl_irri",
                                       "klimatisierende Beregnung",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("foliar_fert",
                                       "Blattdüngung",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("summer_pruning",
                                       "Sommerschnitt",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("kaolin",
                                       "Behandlung mit Kaolin",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("removing_leaves",
                                       "Entblättern",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("irrigation",
                                       "Tröpfchenbewässerung",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0)
                          
                          # awesomeCheckboxGroup(
                          #   inputId = "work",
                          #   label = "Wähle die Maßnahmen aus die in der Anlage durchgeführt werden", 
                          #   choices = c("Behandlung gegen Vorerntefruchtfall"="spray_against_pre_harvest_fruit_drop",
                          #               #"manual_thinning_after_june_drop",
                          #               #"chemical_fruit_thinning",
                          #               #"mechanical_flower_thinning",
                          #               #"chemical_flower_thinning",
                          #               #"frost_protection",
                          #               #"pollinator_support",
                          #               "Hagelnetz"="hailnet",
                          #               "klimatisierende Beregnung"="climatizing_ov_irrigation",
                          #               "Blattdüngung"="leaf_fertilization",
                          #               "Sommerschnitt"="summer_pruning",
                          #               "Entblättern"="removing_leaves",
                          #               "Behandlung mit Kaolin"="use_kaolin",
                          #               "Tröpfchenbewässerung"="irrigation"),
                          #   selected = NULL)
                        ),
                        
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("TP4",height = "800px")#,
                          #plotOutput("distPlot2")
                        ))),
             tabPanel(title = (span(img(src="gruener_Apfel.png", height =80),"Nach dem Junifall")),
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("x3",
                                      "Bäume pro ha:",
                                      min = 1,
                                      max = 5000,
                                      value = 200),
                          sliderInput("y3",
                                      "Äpfel pro Baum (nach dem Junifall):",
                                      min = 1,
                                      max = 1000,
                                      value = c(95,130)),
                          sliderInput("z3",
                                      "Bereits sichtbar beschädigte Früchte [%]:",
                                      min = 1,
                                      max = 100,
                                      value = c(7,10)),
                          sliderInput("Durchmesser3",
                                      "Fruchtdurchmesser direkt nach Ende des Junifalls [cm]:",
                                      min = 1,
                                      max = 10,
                                      value = c(2,3.5)),
                          sliderInput("runs3",
                                      "Anzahl Monte-Carlo Durchläufe:",
                                      min = 1,
                                      max = 15000,
                                      value = 1000),
                          
                          tags$style(HTML(".radio-inline {margin-right: 42px;}")),
                          radioButtons("drop3",
                                       "Behandlung gegen Vorerntefruchtfall",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("manual_thinning3",
                                       "Handausdünnung nach dem Junifall",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("hailnet3",
                                       "Hagelnetz",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("cl_irri3",
                                       "klimatisierende Beregnung",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("foliar_fert3",
                                       "Blattdüngung",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("summer_pruning3",
                                       "Sommerschnitt",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("kaolin3",
                                       "Behandlung mit Kaolin",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("removing_leaves3",
                                       "Entblättern",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("irrigation3",
                                       "Tröpfchenbewässerung",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0)

                          # awesomeCheckboxGroup(
                          #   inputId = "work3",
                          #   label = "Management measures",
                          #   choices = c("spray_against_pre_harvest_fruit_drop",
                          #               "manual_thinning_after_june_drop",
                          #               "chemical_fruit_thinning",
                          #               "mechanical_flower_thinning",
                          #               "chemical_flower_thinning",
                          #               "frost_protection",
                          #               "pollinator_support",
                          #               "hailnet",
                          #               "climatizing_ov_irrigation",
                          #               "leaf_fertilization",
                          #               "summer_pruning",
                          #               "removing_leaves",
                          #               "use_kaolin",
                          #               "irrigation"),
                          #   selected = NULL)
                        ),


                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("TP3",height = "800px")#,
                          #plotOutput("distPlot2")
                        ))),
             tabPanel(title = (span(img(src="gruener_Apfel_kl.png", height =80),"Vor der Fruchtausdünnung")),
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("x2",
                                      "Bäume pro ha:",
                                      min = 1,
                                      max = 5000,
                                      value = 200),
                          sliderInput("y2",
                                      "Fruchtansätze pro Baum:",
                                      min = 1,
                                      max = 1000,
                                      value = c(80,400)),
                          sliderInput("z2",
                                      "Bereits sichtbar beschädigte Früchte [%]:",
                                      min = 0.1,
                                      max = 100,
                                      value = c(0.5,5)),
                          sliderInput("Durchmesser2",
                                      "Fruchtdurchmesser vor Fruchtausdünnung [cm]:",
                                      min = 0.1,
                                      max = 5,
                                      value = c(0.8,1.6)),
                          sliderInput("runs2",
                                      "Anzahl Monte-Carlo Durchläufe:",
                                      min = 1,
                                      max = 15000,
                                      value = 1000),
                          
                          tags$style(HTML(".radio-inline {margin-right: 42px;}")),
                          radioButtons("chemical_fruit_thinning2",
                                       "Chemische Fruchtausdünnung",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("pollinator_support2",
                                       "Förderung der Bestäuber",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("drop2",
                                       "Behandlung gegen Vorerntefruchtfall",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("manual_thinning2",
                                       "Handausdünnung nach dem Junifall",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("hailnet2",
                                       "Hagelnetz",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("cl_irri2",
                                       "klimatisierende Beregnung",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("foliar_fert2",
                                       "Blattdüngung",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("summer_pruning2",
                                       "Sommerschnitt",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("kaolin2",
                                       "Behandlung mit Kaolin",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("removing_leaves2",
                                       "Entblättern",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("irrigation2",
                                       "Tröpfchenbewässerung",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0)
                          
                          # awesomeCheckboxGroup(
                          #   inputId = "work3",
                          #   label = "Management measures",
                          #   choices = c("spray_against_pre_harvest_fruit_drop",
                          #               "manual_thinning_after_june_drop",
                          #               "chemical_fruit_thinning",
                          #               "mechanical_flower_thinning",
                          #               "chemical_flower_thinning",
                          #               "frost_protection",
                          #               "pollinator_support",
                          #               "hailnet",
                          #               "climatizing_ov_irrigation",
                          #               "leaf_fertilization",
                          #               "summer_pruning",
                          #               "removing_leaves",
                          #               "use_kaolin",
                          #               "irrigation"),
                          #   selected = NULL)
                        ),
                        
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("TP2",height = "800px")#,
                          #plotOutput("distPlot2")
                        ))),
             tabPanel(title = (span(img(src="Bluete.png", height =80),"Vollblüte")),
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("trees_per_ha1",
                                      "Bäume pro ha:",
                                      min = 1,
                                      max = 5000,
                                      value = 2571),
                          sliderInput("flower_clusters_per_tree1",
                                      "Blütencluster pro Baum:",
                                      min = 1,
                                      max = 1000,
                                      value = c(80,120)),
                          # sliderInput("z",
                          #             "Bereits sichtbar beschädigte Früchte [%]:",
                          #             min = 1,
                          #             max = 100,
                          #             value = c(10,20)),
                          # sliderInput("Durchmesser",
                          #             "FruchtDurchmesser 4 Wochen vor Ernte [g]:",
                          #             min = 1,
                          #             max = 400,
                          #             value = c(100,200)),
                          sliderInput("runs1",
                                      "Anzahl Monte-Carlo Durchläufe:",
                                      min = 1,
                                      max = 15000,
                                      value = 1000),
                          tags$style(HTML(".radio-inline {margin-right: 42px;}")),
                          radioButtons("chemical_flower_thinning1",
                                       "Chemische Blütenausdünnung",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("mechanical_flower_thinning1",
                                       "Mechanische Blütenausdünnung",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("frost_protection1",
                                       "Frostschutz",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("chemical_fruit_thinning1",
                                       "Chemische Fruchtausdünnung",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("pollinator_support1",
                                       "Förderung der Bestäuber",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("drop1",
                                       "Behandlung gegen Vorerntefruchtfall",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("manual_thinning1",
                                       "Handausdünnung nach dem Junifall",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("hailnet1",
                                       "Hagelnetz",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("cl_irri1",
                                       "klimatisierende Beregnung",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("foliar_fert1",
                                       "Blattdüngung",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("summer_pruning1",
                                       "Sommerschnitt",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("kaolin1",
                                       "Behandlung mit Kaolin",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("removing_leaves1",
                                       "Entblättern",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0),
                          radioButtons("irrigation1",
                                       "Tröpfchenbewässerung",
                                       choices = list("Ja "=1, "Nein"=0),
                                       inline = TRUE,
                                       #width = "400px",
                                       selected = 0)
                          
                          # awesomeCheckboxGroup(
                          #   inputId = "work",
                          #   label = "Wähle die Maßnahmen aus die in der Anlage durchgeführt werden", 
                          #   choices = c("Behandlung gegen Vorerntefruchtfall"="spray_against_pre_harvest_fruit_drop",
                          #               #"manual_thinning_after_june_drop",
                          #               #"chemical_fruit_thinning",
                          #               #"mechanical_flower_thinning",
                          #               #"chemical_flower_thinning",
                          #               #"frost_protection",
                          #               #"pollinator_support",
                          #               "Hagelnetz"="hailnet",
                          #               "klimatisierende Beregnung"="climatizing_ov_irrigation",
                          #               "Blattdüngung"="leaf_fertilization",
                          #               "Sommerschnitt"="summer_pruning",
                          #               "Entblättern"="removing_leaves",
                          #               "Behandlung mit Kaolin"="use_kaolin",
                          #               "Tröpfchenbewässerung"="irrigation"),
                          #   selected = NULL)
                        ),
                        
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("TP1",height = "800px")#,
                          #plotOutput("distPlot2")
                        ))),
             tabPanel(title = (span(img(src="data.png", height =80),"Datenübersicht")),
                      
                      
                      # Show a plot of the generated distribution
                      mainPanel(
                        #tags$h2(i18n("Edit data"), align = "center"),
                        edit_data_ui(id = "id")#,
                        #verbatimTextOutput("result")

                        #dataTableOutput("input_datasheet")#,
                        #plotOutput("distPlot2")
                      )),
             tabPanel(title = (span(img(src="Info.png", height =80),"Infos")),
                      
                      mainPanel(
                        #tags$h2(i18n("Edit data"), align = "center"),
                        uiOutput("text")#,
                        #verbatimTextOutput("result")
                        
                        #dataTableOutput("input_datasheet")#,
                        #plotOutput("distPlot2")
                      ))
  #https://stackoverflow.com/questions/30086881/how-can-i-control-the-size-of-the-numbers-on-my-slider-in-shiny
  
  )
)


# Define server logic required to draw a histogram
#Server####
server <- function(input, output) {
  edited_r <- edit_data_server(
    id = "id",
    data_r = reactive(Apple_prediction_input_csv),
    add = FALSE,
    update = TRUE,
    delete = FALSE,
    download_csv = TRUE,
    download_excel = TRUE,
    file_name_export = "datas",
    # var_edit = c("name", "job", "credit_card_provider", "credit_card_security_code"),
    #var_mandatory = c("name", "job"),
    var_labels = list(
      variable = "Variable",
      distribution = "Verteilung",
      lower = "Untere",
      median="Median (NA)",
      upper = "Obere",
      Unit = "Einheit",
      description = "Beschreibung"
    ),
    var_edit=list("lower", "upper"),
    add_default_values = list(
      name = "Please enter your name here",
      date_obtained = Sys.Date()
    ),
    n_column = 2,
    modal_size = "l",
    modal_easy_close = TRUE,
    reactable_options = list(
      defaultColDef = colDef(filterable = TRUE),
      selection = "single",
      columns = list(
        lower = colDef(name = "Untere", style = list(fontWeight = "bold")),
        upper = colDef(name = "Obere", style = list(fontWeight = "bold"))),
      bordered = TRUE,
      compact = TRUE,
      searchable = TRUE,
      highlight = TRUE
    )
  )
  
  outputOptions(output, "id-table", suspendWhenHidden = FALSE)

  output$TP4 <- renderPlot({
    Management_values2<-data.frame(management_measure=c("spray_against_pre_harvest_fruit_drop",
                                                       "hailnet",
                                                       "climatizing_ov_irrigation",
                                                       "leaf_fertilization",
                                                       "summer_pruning",
                                                       "removing_leaves",
                                                       "use_kaolin",
                                                       "irrigation"),value= as.numeric(c(input$drop,
                                                                                         input$hailnet,
                                                                                         input$cl_irri,
                                                                                         input$foliar_fert,
                                                                                         input$summer_pruning,
                                                                                         input$removing_leaves,
                                                                                         input$kaolin,
                                                                                         input$irrigation)))
    write.csv(Management_values2, "test_management_values.csv")
    library(decisionSupport)
    library(tidyverse)

    variable<-c("apples_per_tree_tp4", "already_damaged_tp4", "trees_per_ha_4","diameter_fruit_tp4")
    distribution<-c("posnorm", "tnorm_0_1","const","posnorm")
    lower<-c(input$y[1],input$z[1]/100,input$x, input$Durchmesser[1])
    median<-rep(NA, 4)
    upper<-c(input$y[2],input$z[2]/100,input$x, input$Durchmesser[2])
    unit<-c("apples","%","trees","g")
    Apple_prediction_slider_input<-data.frame(variable,distribution,lower,median,upper,unit)
    #orchard_data<-read.csv2("2024_test_apple/test_input.csv", colClasses = c("character", "character", "numeric", "character","numeric", "character"), sep = ";", dec = ".")
    #Apple_prediction_input_raw<-rbind(Apple_prediction_input_csv, Apple_prediction_slider_input)
    Apple_prediction_input<-rbind(edited_r()[1:6], Apple_prediction_slider_input)
    
    source("management_values.R")

    #Quality and Yield tp4####

    tp_4_quality_and_yield_prediction<-function(){
      n_fruits_per_tree_tp4<-round(apples_per_tree_tp4)
      quality_prediction<-quality_tp4_function(n_fruits_4=n_fruits_per_tree_tp4,
                                               visibly_damaged_4=already_damaged_tp4,
                                               risk_sunburn_4=sunburn_risk_tp4_harvest,
                                               damage_sunburn_4=sunburn_damage_tp4_harvest,
                                               risk_sunburn_hailnet_4=sunburn_risk_hailnet_tp4_harvest,
                                               damage_sunburn_hailnet_4=sunburn_damage_hailnet_tp4_harvest,
                                               risk_sunburn_climatizing_irrigation_4=sunburn_risk_climatizing_irrigation_tp4_harvest,
                                               damage_sunburn_climatizing_irrigation_4=sunburn_damage_climatizing_irrigation_tp4_harvest,
                                               risk_sunburn_hailnet_climatizing_irrigation_4=sunburn_risk_hailnet_climatizing_irrigation_tp4_harvest,
                                               damage_sunburn_hailnet_climatizing_irrigation_4=sunburn_damage_hailnet_climatizing_irrigation_tp4_harvest,
                                               risk_rotting_4=rotting_risk_tp4_harvest,
                                               damage_rotting_4=rotting_damage_tp4_harvest,
                                               #risk_color_problem_no_cold_4=color_problem_no_cold_risk_tp4_harvest,
                                               #damage_color_problem_no_cold_4=color_problem_no_cold_damage_tp4_harvest,
                                               risk_hail_4=hail_risk_tp4_harvest,
                                               damage_hail_4=hail_damage_tp4_harvest,
                                               risk_hail_hailnet_4=hail_risk_hailnet_tp4_harvest,
                                               damage_hail_hailnet_4=hail_damage_hailnet_tp4_harvest,
                                               percentage_of_hail_falling=percentage_of_hail_damaged_falling,
                                               risk_bird_4=bird_risk_tp4_harvest,
                                               damage_bird_4=bird_damage_tp4_harvest,
                                               risk_mechanical_4=mechanical_risk_tp4_harvest,
                                               damage_mechanical_4=mechanical_damage_tp4_harvest,
                                               risk_fruit_scab_4=fruit_scab_risk_tp4_harvest,
                                               damage_fruit_scab_4=fruit_scab_damage_tp4_harvest,
                                               risk_other_fungal_diseases_4=other_fungal_diseases_risk_tp4_harvest,
                                               damage_other_fungal_diseases_4=other_fungal_diseases_damage_tp4_harvest,
                                               risk_codling_moth_4=codling_moth_damage_tp4_harvest,
                                               damage_codling_moth_4=codling_moth_damage_tp4_harvest,
                                               risk_aphids_4=aphids_risk_tp4_harvest,
                                               damage_aphids_4=aphids_damage_tp4_harvest,
                                               risk_other_insects_4=other_insects_risk_tp4_harvest,
                                               damage_other_insects_4=other_insects_damage_tp4_harvest,
                                               risk_bitter_pit_4=bitter_pit_risk_tp4_harvest,
                                               damage_bitter_pit_4=bitter_pit_damage_tp4_harvest,
                                               risk_bitter_pit_leaf_fertilization_4=bitter_pit_risk_leaf_fertilization_tp4_harvest,
                                               damage_bitter_pit_leaf_fertilization_4=bitter_pit_damage_leaf_fertilization_tp4_harvest,
                                               risk_other_physiological_disorders_4=other_physiological_disorders_risk_tp4_harvest,
                                               damage_other_physiological_disorders_4=other_physiological_disorders_damage_tp4_harvest,
                                               risk_other_physiological_disorders_leaf_fertilization_4=other_physiological_disorders_risk_leaf_fertilization_tp4_harvest,
                                               damage_other_physiological_disorders_leaf_fertilization_4=other_physiological_disorders_damage_leaf_fertilization_tp4_harvest,
                                               risk_color_decrease_summer_pruning_4=color_risk_decrease_summer_pruning_tp4_harvest,
                                               risk_color_decrease_removing_leaves_4=color_risk_decrease_removing_leaves_tp4_harvest,
                                               risk_color_increase_hailnet_4=color_risk_increase_hailnet_tp4_harvest,
                                               risk_no_cold_nights_4=no_cold_nights_risk_tp4_harvest,
                                               risk_color_increase_no_cold_4=color_risk_increase_no_cold_nights_tp4_harvest,
                                               damage_color_decrease_summer_pruning_4=color_damage_decrease_summer_pruning_tp4_harvest,
                                               damage_color_decrease_removing_leaves_4=color_damage_decrease_removing_leaves_tp4_harvest,
                                               damage_color_increase_hailnet_4=color_damage_increase_hailnet_tp4_harvest,
                                               risk_color_variety=color_risk_variety,
                                               damage_color_variety=color_damage_variety,
                                               damage_color_increase_no_cold=color_damage_increase_no_cold_nights_tp4_harvest,
                                               risk_dirty_fruits_4=dirty_fruits_risk_tp4_harvest,
                                               damage_dirty_fruits_4=dirty_fruits_damage_tp4_harvest,
                                               add_sunburn_removing_leaves_4=additional_sunburn_removing_leaves_tp4_harvest,
                                               add_sunburn_summer_pruning_4=additional_sunburn_summer_pruning_tp4_harvest,
                                               sunburn_reduce_kaolin_4=sunburn_reduce_kaolin_tp4_harvest,
                                               #risk_excessive_russeting_4=excessive_russeting_risk_tp4_harvest,
                                               #damage_excessive_russeting_4=excessive_russeting_damage_tp4_harvest,
                                               risk_fruit_cracking_4=fruit_cracking_risk_tp4_harvest,
                                               damage_fruit_cracking_4=fruit_cracking_damage_tp4_harvest)
      yield_prediction<-yield_prediction_tp_4_function(fruit_drop_natural_4 = pre_harvest_fruit_drop_natural,
                                                       fruit_drop_managed_4 = pre_harvest_fruit_drop_spraying,
                                                       storm_risk_4 = risk_of_storm_pre_harvest_tp_4,
                                                       drop_rate_storm_4 = additional_pre_harvest_fruit_drop_storm,
                                                       fruits_per_tree_4 = n_fruits_per_tree_tp4,
                                                       add_fruit_losses_4=quality_prediction$n_falling_hail,
                                                       #fruit_weight_4 = g_fruit_tp4,
                                                       #daily_weight_increase_4 = daily_weight_increase_tp4_harvest,
                                                       timespan_4=timespan_tp4_harvest,
                                                       #trees_per_ha_4 = n_trees_per_ha,
                                                       #reducing_factor_weight_increase_drought_4=weight_increase_reducing_factor_drought_tp4_harvest,
                                                       risk_drought_4=drought_risk_tp4_harvest,
                                                       risk_drought_irrigation_4=drought_risk_irrigation_tp4_harvest,
                                                       #mice_risk=tree_losses_mice_risk_tp1_harvest,
                                                       #mice_damage=lost_trees_mice_tp1_harvest,
                                                       reducing_factor_diameter_increase_drought_4=diameter_increase_reducing_factor_drought_tp4_harvest,
                                                       fruit_diameter_4= diameter_fruit_tp4,
                                                       weekly_diameter_increase_4 = weekly_diameter_increase_tp4_harvest,
                                                       fruit_density_at_harvest=apple_fruit_density_at_harvest,
                                                       percentage_tree_losses_4=percentage_tree_losses_tp4_harvest,
                                                       risk_tree_losses_4=risk_tree_losses_tp4_harvest,
                                                       trees_per_ha_4 = trees_per_ha_4)
      
      #quality_yield<-(1-quality_prediction$percentage_damaged_at_harvest)*yield_prediction$yield_at_harvest
      quality_yield_diameter<-(1-quality_prediction$percentage_damaged_at_harvest)*yield_prediction$yield_at_harvest_diameter
      
      return(list(n_damaged_apples_per_tree=quality_prediction$n_damaged_apples,
                  percentage_damaged_at_harvest=quality_prediction$percentage_damaged_at_harvest,
                  fruits_per_tree=yield_prediction$number_of_apple_harvested_per_tree,
                  total_yield_diameter=yield_prediction$yield_at_harvest_diameter/1000,
                  high_quality_yield_diameter=quality_yield_diameter/1000,
                  mean_fruit_diameter_at_harvest=yield_prediction$mean_fruit_diameter_at_harvest,
                  mean_fruit_weight_at_harvest_diameter=yield_prediction$mean_fruit_weight_at_harvest_diameter,
                  percentage_undamaged_at_harvest=1-quality_prediction$percentage_damaged_at_harvest))
    }
    #Monte Carlo####
    # apple_quality_and_yield_mc_simulation_tp4 <- mcSimulation(estimate = as.estimate(Apple_prediction_input_raw),
    #                                                           model_function = tp_4_quality_and_yield_prediction,
    #                                                           numberOfModelRuns = input$runs,
    #                                                           functionSyntax = "plainNames")
    apple_quality_and_yield_mc_simulation_tp4 <- mcSimulation(estimate = as.estimate(Apple_prediction_input),
                                                              model_function = tp_4_quality_and_yield_prediction,
                                                              numberOfModelRuns = input$runs,
                                                              functionSyntax = "plainNames")

    Plot_a<-plot_distributions(mcSimulation_object = apple_quality_and_yield_mc_simulation_tp4,
                       vars = c("high_quality_yield_diameter"),
                       method = 'smooth_simple_overlay',
                       base_size = 7)+
      theme(axis.text = element_text(colour = "black", size = 10),
            axis.title = element_text(colour = "black", size = 10),
            legend.position = "none")+
      xlab("Qualitätsertrag [t/ha]")+
      ylab("Häufigkeit")
    Plot_b<-plot_distributions(mcSimulation_object = apple_quality_and_yield_mc_simulation_tp4,
                       vars = c("total_yield_diameter"),
                       method = 'smooth_simple_overlay',
                       base_size = 7)+
      theme(axis.text = element_text(colour = "black", size = 10),
            axis.title = element_text(colour = "black", size = 10),
            legend.position = "none")+
      xlab("Gesamtertrag [t/ha]")+
      ylab("Häufigkeit")
    Plot_c<-plot_distributions(mcSimulation_object = apple_quality_and_yield_mc_simulation_tp4,
                       vars = c("fruits_per_tree"),
                       method = 'smooth_simple_overlay',
                       base_size = 7)+
      theme(axis.text = element_text(colour = "black", size = 10),
            axis.title = element_text(colour = "black", size = 10),
            legend.position = "none")+
      xlab("Früchte pro Baum")+
      ylab("Häufigkeit")

    library(patchwork)
    Plots_combined <- list(Plot_a, Plot_b,Plot_c)
    wrap_plots(Plots_combined, nrow = 3) +
      plot_layout(guides = "keep")

  })
  
  output$TP3 <- renderPlot({
    Management_values3<-data.frame(management_measure=c("manual_thinning_after_june_drop",
                                                        "spray_against_pre_harvest_fruit_drop",
                                                        "hailnet",
                                                        "climatizing_ov_irrigation",
                                                        "leaf_fertilization",
                                                        "summer_pruning",
                                                        "removing_leaves",
                                                        "use_kaolin",
                                                        "irrigation"),value= as.numeric(c(input$manual_thinning3,
                                                                                          input$drop3,
                                                                                          input$hailnet3,
                                                                                          input$cl_irri3,
                                                                                          input$foliar_fert3,
                                                                                          input$summer_pruning3,
                                                                                          input$removing_leaves3,
                                                                                          input$kaolin3,
                                                                                          input$irrigation3)))
    write.csv(Management_values3, "test_management_values3.csv")
    library(decisionSupport)
    library(tidyverse)

    variable<-c("apples_per_tree_tp3", "already_damaged_tp3", "trees_per_ha_3", "diameter_fruit_tp3")
    distribution<-c("posnorm", "tnorm_0_1","const","posnorm")
    lower<-c(input$y3[1],input$z3[1]/100,input$x3,input$Durchmesser3[1])
    median<-rep(NA, 4)
    upper<-c(input$y3[2],input$z3[2]/100,input$x3,input$Durchmesser3[2])
    unit<-c("apples","%","trees","g")
    Apple_prediction_slider_input<-data.frame(variable,distribution,lower,median,upper,unit)
    #orchard_data<-read.csv2("2024_test_apple/test_input.csv", colClasses = c("character", "character", "numeric", "character","numeric", "character"), sep = ";", dec = ".")
    #Apple_prediction_input<-rbind(Apple_prediction_input_csv, Apple_prediction_slider_input)
    Apple_prediction_input<-rbind(edited_r()[1:6], Apple_prediction_slider_input)
    




    source("management_values3.R")
    

    #Quality and Yield tp3####

    tp_3_quality_and_yield_prediction<-function(){
      quality_tp4_p3<-quality_tp3_function(n_fruits_3=round(apples_per_tree_tp3),
                                           visibly_damaged_3=already_damaged_tp3,
                                           risk_sunburn_3=sunburn_risk_tp3_tp4,
                                           damage_sunburn_3=sunburn_damage_tp3_tp4,
                                           risk_sunburn_hailnet_3=sunburn_risk_hailnet_tp3_tp4,
                                           damage_sunburn_hailnet_3=sunburn_damage_hailnet_tp3_tp4,
                                           risk_sunburn_climatizing_irrigation_3=sunburn_risk_climatizing_irrigation_tp3_tp4,
                                           damage_sunburn_climatizing_irrigation_3=sunburn_damage_climatizing_irrigation_tp3_tp4,
                                           risk_sunburn_hailnet_climatizing_irrigation_3=sunburn_risk_hailnet_climatizing_irrigation_tp3_tp4,
                                           damage_sunburn_hailnet_climatizing_irrigation_3=sunburn_damage_hailnet_climatizing_irrigation_tp3_tp4,
                                           risk_rotting_3=rotting_risk_tp3_tp4,
                                           damage_rotting_3=rotting_damage_tp3_tp4,
                                           risk_hail_3=hail_risk_tp3_tp4,
                                           damage_hail_3=hail_damage_tp3_tp4,
                                           risk_hail_hailnet_3=hail_risk_hailnet_tp3_tp4,
                                           damage_hail_hailnet_3=hail_damage_hailnet_tp3_tp4,
                                           percentage_of_hail_falling=percentage_of_hail_damaged_falling,
                                           risk_bird_3=bird_risk_tp3_tp4,
                                           damage_bird_3=bird_damage_tp3_tp4,
                                           risk_mechanical_3=mechanical_risk_tp3_tp4,
                                           damage_mechanical_3=mechanical_damage_tp3_tp4,
                                           risk_fruit_scab_3=fruit_scab_risk_tp3_tp4,
                                           damage_fruit_scab_3=fruit_scab_damage_tp3_tp4,
                                           risk_other_fungal_diseases_3=other_fungal_diseases_risk_tp3_tp4,
                                           damage_other_fungal_diseases_3=other_fungal_diseases_damage_tp3_tp4,
                                           risk_codling_moth_3=codling_moth_damage_tp3_tp4,
                                           damage_codling_moth_3=codling_moth_damage_tp3_tp4,
                                           risk_aphids_3=aphids_risk_tp3_tp4,
                                           damage_aphids_3=aphids_damage_tp3_tp4,
                                           risk_other_insects_3=other_insects_risk_tp3_tp4,
                                           damage_other_insects_3=other_insects_damage_tp3_tp4,
                                           risk_bitter_pit_3=bitter_pit_risk_tp3_tp4,
                                           damage_bitter_pit_3=bitter_pit_damage_tp3_tp4,
                                           risk_bitter_pit_leaf_fertilization_3=bitter_pit_risk_leaf_fertilization_tp3_tp4,
                                           damage_bitter_pit_leaf_fertilization_3=bitter_pit_damage_leaf_fertilization_tp3_tp4,
                                           risk_other_physiological_disorders_3=other_physiological_disorders_risk_tp3_tp4,
                                           damage_other_physiological_disorders_3=other_physiological_disorders_damage_tp3_tp4,
                                           risk_other_physiological_disorders_leaf_fertilization_3=other_physiological_disorders_risk_leaf_fertilization_tp3_tp4,
                                           damage_other_physiological_disorders_leaf_fertilization_3=other_physiological_disorders_damage_leaf_fertilization_tp3_tp4,
                                           sunburn_reduce_kaolin_3=sunburn_reduce_kaolin_tp3_tp4,
                                           #risk_excessive_russeting_3=excessive_russeting_risk_tp3_tp4,
                                           #damage_excessive_russeting_3=excessive_russeting_damage_tp3_tp4,
                                           risk_fruit_cracking_3=fruit_cracking_risk_tp3_tp4,
                                           damage_fruit_cracking_3=fruit_cracking_damage_tp3_tp4,
                                           damage_remove_manual_thinning_3=damage_remove_manual_thinning_tp3_tp4,
                                           thinning_goal_3=desired_number_of_fruits,
                                           variation_thinning_3=manual_thinning_mistake,
                                           add_sunburn_summer_pruning_3=additional_sunburn_summer_pruning_tp3_tp4)
      yield_tp4_p3<-yield_prediction_tp_3_function(#thinning_goal_3=desired_number_of_fruits,
        #variation_thinning_3=manual_thinning_mistake,
        #fruits_per_tree_3=round(n_fruits_per_tree_tp3),
        add_fruit_losses_3=quality_tp4_p3$n_falling_hail_tp3_tp4,
        lower_opt_n_fruits_3=lower_optimum_fruits_per_tree,
        range_opt_n_fruits_3=width_optimum_bearing_range,
        #daily_weight_increase_3=daily_weight_increase_tp3_tp4,
        #change_weight_increase_overbearing_3 = weight_increase_change_overbearing,
        #change_weight_increase_underbearing_3 =weight_increase_change_underbearing,
        #fruit_weight_3=g_fruit_tp3,
        timespan_3=timespan_tp1_tp3,
        total_timespan = timespan_tp1_harvest,
        timespan_4=timespan_tp4_harvest,
        #reducing_factor_weight_increase_drought_3=weight_increase_reducing_factor_drought_tp3_tp4,
        risk_drought_3=drought_risk_tp3_tp4,
        risk_drought_irrigation_3=drought_risk_irrigation_tp3_tp4,
        n_fruits_per_tree_beginning_tp3=quality_tp4_p3$n_fruits_after_manual_thinning_timepoint,
        weekly_diameter_increase_3=weekly_diameter_increase_tp3_tp4,
        change_diameter_increase_overbearing_3=diameter_increase_change_overbearing,
        change_diameter_increase_underbearing_3=diameter_increase_change_underbearing,
        reducing_factor_diameter_increase_drought_3= diameter_increase_reducing_factor_drought_tp3_tp4,
        fruit_diameter_3=diameter_fruit_tp3,
        percentage_tree_losses_3=percentage_tree_losses_tp3_tp4,
        risk_tree_losses_3=risk_tree_losses_tp3_tp4,
        trees_per_ha_3=trees_per_ha_3)
      quality_prediction<-quality_tp4_function(n_fruits_4=yield_tp4_p3$number_of_apples_at_tp4_predicted_at_tp3,
                                               visibly_damaged_4=quality_tp4_p3$percentage_damaged_at_tp4_p3,
                                               risk_sunburn_4=sunburn_risk_tp4_harvest,
                                               damage_sunburn_4=sunburn_damage_tp4_harvest,
                                               risk_sunburn_hailnet_4=sunburn_risk_hailnet_tp4_harvest,
                                               damage_sunburn_hailnet_4=sunburn_damage_hailnet_tp4_harvest,
                                               risk_sunburn_climatizing_irrigation_4=sunburn_risk_climatizing_irrigation_tp4_harvest,
                                               damage_sunburn_climatizing_irrigation_4=sunburn_damage_climatizing_irrigation_tp4_harvest,
                                               risk_sunburn_hailnet_climatizing_irrigation_4=sunburn_risk_hailnet_climatizing_irrigation_tp4_harvest,
                                               damage_sunburn_hailnet_climatizing_irrigation_4=sunburn_damage_hailnet_climatizing_irrigation_tp4_harvest,
                                               risk_rotting_4=rotting_risk_tp4_harvest,
                                               damage_rotting_4=rotting_damage_tp4_harvest,
                                               #risk_color_problem_no_cold_4=color_problem_no_cold_risk_tp4_harvest,
                                               #damage_color_problem_no_cold_4=color_problem_no_cold_damage_tp4_harvest,
                                               risk_hail_4=hail_risk_tp4_harvest,
                                               damage_hail_4=hail_damage_tp4_harvest,
                                               risk_hail_hailnet_4=hail_risk_hailnet_tp4_harvest,
                                               damage_hail_hailnet_4=hail_damage_hailnet_tp4_harvest,
                                               percentage_of_hail_falling=percentage_of_hail_damaged_falling,
                                               risk_bird_4=bird_risk_tp4_harvest,
                                               damage_bird_4=bird_damage_tp4_harvest,
                                               risk_mechanical_4=mechanical_risk_tp4_harvest,
                                               damage_mechanical_4=mechanical_damage_tp4_harvest,
                                               risk_fruit_scab_4=fruit_scab_risk_tp4_harvest,
                                               damage_fruit_scab_4=fruit_scab_damage_tp4_harvest,
                                               risk_other_fungal_diseases_4=other_fungal_diseases_risk_tp4_harvest,
                                               damage_other_fungal_diseases_4=other_fungal_diseases_damage_tp4_harvest,
                                               risk_codling_moth_4=codling_moth_damage_tp4_harvest,
                                               damage_codling_moth_4=codling_moth_damage_tp4_harvest,
                                               risk_aphids_4=aphids_risk_tp4_harvest,
                                               damage_aphids_4=aphids_damage_tp4_harvest,
                                               risk_other_insects_4=other_insects_risk_tp4_harvest,
                                               damage_other_insects_4=other_insects_damage_tp4_harvest,
                                               risk_bitter_pit_4=bitter_pit_risk_tp4_harvest,
                                               damage_bitter_pit_4=bitter_pit_damage_tp4_harvest,
                                               risk_bitter_pit_leaf_fertilization_4=bitter_pit_risk_leaf_fertilization_tp4_harvest,
                                               damage_bitter_pit_leaf_fertilization_4=bitter_pit_damage_leaf_fertilization_tp4_harvest,
                                               risk_other_physiological_disorders_4=other_physiological_disorders_risk_tp4_harvest,
                                               damage_other_physiological_disorders_4=other_physiological_disorders_damage_tp4_harvest,
                                               risk_other_physiological_disorders_leaf_fertilization_4=other_physiological_disorders_risk_leaf_fertilization_tp4_harvest,
                                               damage_other_physiological_disorders_leaf_fertilization_4=other_physiological_disorders_damage_leaf_fertilization_tp4_harvest,
                                               risk_color_decrease_summer_pruning_4=color_risk_decrease_summer_pruning_tp4_harvest,
                                               risk_color_decrease_removing_leaves_4=color_risk_decrease_removing_leaves_tp4_harvest,
                                               risk_color_increase_hailnet_4=color_risk_increase_hailnet_tp4_harvest,
                                               risk_no_cold_nights_4=no_cold_nights_risk_tp4_harvest,
                                               risk_color_increase_no_cold_4=color_risk_increase_no_cold_nights_tp4_harvest,
                                               damage_color_decrease_summer_pruning_4=color_damage_decrease_summer_pruning_tp4_harvest,
                                               damage_color_decrease_removing_leaves_4=color_damage_decrease_removing_leaves_tp4_harvest,
                                               damage_color_increase_hailnet_4=color_damage_increase_hailnet_tp4_harvest,
                                               risk_color_variety=color_risk_variety,
                                               damage_color_variety=color_damage_variety,
                                               damage_color_increase_no_cold=color_damage_increase_no_cold_nights_tp4_harvest,
                                               risk_dirty_fruits_4=dirty_fruits_risk_tp4_harvest,
                                               damage_dirty_fruits_4=dirty_fruits_damage_tp4_harvest,
                                               add_sunburn_removing_leaves_4=additional_sunburn_removing_leaves_tp4_harvest,
                                               sunburn_reduce_kaolin_4=sunburn_reduce_kaolin_tp4_harvest,
                                               #risk_excessive_russeting_4=excessive_russeting_risk_tp4_harvest,
                                               #damage_excessive_russeting_4=excessive_russeting_damage_tp4_harvest,
                                               risk_fruit_cracking_4=fruit_cracking_risk_tp4_harvest,
                                               damage_fruit_cracking_4=fruit_cracking_damage_tp4_harvest,
                                               add_sunburn_summer_pruning_4=additional_sunburn_summer_pruning_tp4_harvest)
      yield_prediction<-yield_prediction_tp_4_function(fruit_drop_natural_4 = pre_harvest_fruit_drop_natural,
                                                       fruit_drop_managed_4 = pre_harvest_fruit_drop_spraying,
                                                       storm_risk_4 = risk_of_storm_pre_harvest_tp_4,
                                                       drop_rate_storm_4 = additional_pre_harvest_fruit_drop_storm,
                                                       fruits_per_tree_4 = yield_tp4_p3$number_of_apples_at_tp4_predicted_at_tp3,
                                                       add_fruit_losses_4=quality_prediction$n_falling_hail,
                                                       #fruit_weight_4 = yield_tp4_p3$fruit_weight_at_tp4_predicted_at_tp3,
                                                       #daily_weight_increase_4 = daily_weight_increase_tp4_harvest,
                                                       timespan_4=timespan_tp4_harvest,
                                                       #trees_per_ha_4 = n_trees_per_ha,
                                                       #reducing_factor_weight_increase_drought_4=weight_increase_reducing_factor_drought_tp4_harvest,
                                                       risk_drought_4=drought_risk_tp4_harvest,
                                                       risk_drought_irrigation_4=drought_risk_irrigation_tp4_harvest,
                                                       #mice_risk=tree_losses_mice_risk_tp1_harvest,
                                                       #mice_damage=lost_trees_mice_tp1_harvest,
                                                       reducing_factor_diameter_increase_drought_4=diameter_increase_reducing_factor_drought_tp4_harvest,
                                                       fruit_diameter_4= yield_tp4_p3$fruit_diameter_at_tp4_predicted_at_tp3,
                                                       weekly_diameter_increase_4 = weekly_diameter_increase_tp4_harvest,
                                                       fruit_density_at_harvest=apple_fruit_density_at_harvest,
                                                       percentage_tree_losses_4=percentage_tree_losses_tp4_harvest,
                                                       risk_tree_losses_4=risk_tree_losses_tp4_harvest,
                                                       trees_per_ha_4=yield_tp4_p3$n_trees_at_tp4_predicted_at_tp3)
      
      #quality_yield<-(1-quality_prediction$percentage_damaged_at_harvest)*yield_prediction$yield_at_harvest
      quality_yield_diameter<-(1-quality_prediction$percentage_damaged_at_harvest)*yield_prediction$yield_at_harvest_diameter
      
      return(list(n_damaged_apples_per_tree=quality_prediction$n_damaged_apples,
                  percentage_damaged_at_harvest=quality_prediction$percentage_damaged_at_harvest,
                  fruits_per_tree=yield_prediction$number_of_apple_harvested_per_tree,
                  total_yield_diameter=yield_prediction$yield_at_harvest_diameter/1000,
                  high_quality_yield_diameter=quality_yield_diameter/1000,
                  mean_fruit_diameter_at_harvest=yield_prediction$mean_fruit_diameter_at_harvest,
                  mean_fruit_weight_at_harvest_diameter=yield_prediction$mean_fruit_weight_at_harvest_diameter,
                  n_apples_tp_4=yield_tp4_p3$number_of_apples_at_tp4_predicted_at_tp3,
                  percentage_damaged_tp_4=quality_tp4_p3$percentage_damaged_at_tp4_p3,
                  percentage_undamaged_at_harvest=1-quality_prediction$percentage_damaged_at_harvest,
                  mean_fruit_diameter_tp_4=yield_tp4_p3$fruit_diameter_at_tp4_predicted_at_tp3))
    }
    
    #Monte Carlo####
    apple_quality_and_yield_mc_simulation_tp3 <- mcSimulation(estimate = as.estimate(Apple_prediction_input),
                                                              model_function = tp_3_quality_and_yield_prediction,
                                                              numberOfModelRuns = input$runs3,
                                                              functionSyntax = "plainNames")

    Plot_a<-plot_distributions(mcSimulation_object = apple_quality_and_yield_mc_simulation_tp3,
                               vars = c("high_quality_yield_diameter"),
                               method = 'smooth_simple_overlay',
                               base_size = 7)+
      theme(axis.text = element_text(colour = "black", size = 10),
            axis.title = element_text(colour = "black", size = 10),
            legend.position = "none")+
      xlab("Qualitätsertrag [t/ha]")+
      ylab("Häufigkeit")
    Plot_b<-plot_distributions(mcSimulation_object = apple_quality_and_yield_mc_simulation_tp3,
                               vars = c("total_yield_diameter"),
                               method = 'smooth_simple_overlay',
                               base_size = 7)+
      theme(axis.text = element_text(colour = "black", size = 10),
            axis.title = element_text(colour = "black", size = 10),
            legend.position = "none")+
      xlab("Gesamtertrag [t/ha]")+
      ylab("Häufigkeit")
    Plot_c<-plot_distributions(mcSimulation_object = apple_quality_and_yield_mc_simulation_tp3,
                               vars = c("fruits_per_tree"),
                               method = 'smooth_simple_overlay',
                               base_size = 7)+
      theme(axis.text = element_text(colour = "black", size = 10),
            axis.title = element_text(colour = "black", size = 10),
            legend.position = "none")+
      xlab("Früchte pro Baum")+
      ylab("Häufigkeit")

    library(patchwork)
    Plots_combined <- list(Plot_a, Plot_b,Plot_c)
    wrap_plots(Plots_combined, nrow = 3) +
      plot_layout(guides = "keep")

  })

  output$TP2 <- renderPlot({
    Management_valuestp2<-data.frame(management_measure=c("chemical_fruit_thinning",
                                                          "pollinator_support",
      "manual_thinning_after_june_drop",
                                                        "spray_against_pre_harvest_fruit_drop",
                                                        "hailnet",
                                                        "climatizing_ov_irrigation",
                                                        "leaf_fertilization",
                                                        "summer_pruning",
                                                        "removing_leaves",
                                                        "use_kaolin",
                                                        "irrigation"),value= as.numeric(c(input$chemical_fruit_thinning2,
                                                                                          input$pollinator_support2,
                                                                                          input$manual_thinning2,
                                                                                          input$drop2,
                                                                                          input$hailnet2,
                                                                                          input$cl_irri2,
                                                                                          input$foliar_fert2,
                                                                                          input$summer_pruning2,
                                                                                          input$removing_leaves2,
                                                                                          input$kaolin2,
                                                                                          input$irrigation2)))
    write.csv(Management_valuestp2, "test_management_values2.csv")
    library(decisionSupport)
    library(tidyverse)
    
    variable<-c("apples_per_tree_tp2", "already_damaged_tp2", "trees_per_ha_2", "diameter_fruit_tp2")
    distribution<-c("posnorm", "tnorm_0_1","const","posnorm")
    lower<-c(input$y2[1],input$z2[1]/100,input$x2,input$Durchmesser2[1])
    median<-rep(NA, 4)
    upper<-c(input$y2[2],input$z2[2]/100,input$x2,input$Durchmesser2[2])
    unit<-c("apples","%","trees","g")
    Apple_prediction_slider_input<-data.frame(variable,distribution,lower,median,upper,unit)
    #orchard_data<-read.csv2("2024_test_apple/test_input.csv", colClasses = c("character", "character", "numeric", "character","numeric", "character"), sep = ";", dec = ".")
    #Apple_prediction_input<-rbind(Apple_prediction_input_csv, Apple_prediction_slider_input)
    Apple_prediction_input<-rbind(edited_r()[1:6], Apple_prediction_slider_input)
    
    
    
    
    
    source("management_values2.R")
    
    
    
    #Quality and Yield tp 2####
    
    tp_2_quality_and_yield_prediction<-function(){
      
      quality_tp3_p2<-quality_tp2_function(n_fruits_2=round(apples_per_tree_tp2),
                                           visibly_damaged_2=already_damaged_tp2,
                                           bad_seed_structure_risk_2=risk_bad_seed_structure,
                                           reduce_bad_seed_structure_risk_pollinator_support=bad_seed_structure_reduce_pollinator_support,
                                           deformation_bad_seed_structure_2=bad_seed_structure_deformation_tp2,
                                           risk_hail_2=hail_risk_tp2_tp3,
                                           damage_hail_2=hail_damage_tp2_tp3,
                                           risk_hail_hailnet_2=hail_risk_hailnet_tp2_tp3,
                                           damage_hail_hailnet_2=hail_damage_hailnet_tp2_tp3,
                                           percentage_of_hail_falling=percentage_of_hail_damaged_falling,
                                           risk_mechanical_2=mechanical_risk_tp2_tp3,
                                           damage_mechanical_2=mechanical_damage_tp2_tp3,
                                           risk_fruit_scab_2=fruit_scab_risk_tp2_tp3,
                                           damage_fruit_scab_2=fruit_scab_damage_tp2_tp3,
                                           risk_other_fungal_diseases_2=other_fungal_diseases_risk_tp2_tp3,
                                           damage_other_fungal_diseases_2=other_fungal_diseases_damage_tp2_tp3,
                                           risk_codling_moth_2=codling_moth_damage_tp2_tp3,
                                           damage_codling_moth_2=codling_moth_damage_tp2_tp3,
                                           risk_aphids_2=aphids_risk_tp2_tp3,
                                           damage_aphids_2=aphids_damage_tp2_tp3,
                                           risk_other_insects_2=other_insects_risk_tp2_tp3,
                                           damage_other_insects_2=other_insects_damage_tp2_tp3,
                                           risk_excessive_russeting_2=excessive_russeting_risk_tp2_tp3,
                                           damage_excessive_russeting_2=excessive_russeting_damage_tp2_tp3,
                                           risk_fruit_cracking_2=fruit_cracking_risk_tp2_tp3,
                                           damage_fruit_cracking_2=fruit_cracking_damage_tp2_tp3,
                                           risk_rotting_2=rotting_risk_tp2_tp3,
                                           damage_rotting_2=rotting_damage_tp2_tp3)
      yield_tp3_p2<-yield_prediction_tp_2_function(fruits_per_tree_2= round(n_fruits_per_tree_tp2),
                                                   thinner_efficiency_2= thinner_efficiency_chemical_fruit_thinning,
                                                   lower_opt_n_fruits_2=lower_optimum_fruits_per_tree,
                                                   range_opt_n_fruits_2=width_optimum_bearing_range,
                                                   add_fruit_losses_2=quality_tp3_p2$n_falling_hail_tp2_tp3,
                                                   june_drop_change_overbearing_2=june_drop_increase_overbearing,
                                                   occourrence_bad_seed_structure=quality_tp3_p2$bad_seed_structure_occurrence,
                                                   stress_risk_2=risk_stress_and_supply_influence_tp2_tp3,
                                                   stress_risk_irrigation_2=risk_stress_and_supply_influence_irrigation_tp2_tp3,
                                                   june_drop_change_bad_seed_structure_2=june_drop_increase_bad_seed_structure,
                                                   june_drop_change_stress_and_supply_problem_2=june_drop_increase_stress_and_supply_problem,
                                                   drop_rate_2=june_drop_rate,
                                                   #daily_weight_increase_2=daily_weight_increase_tp2_tp3,
                                                   #change_weight_increase_overbearing_2=weight_increase_change_overbearing,
                                                   #change_weight_increase_underbearing_2=weight_increase_change_underbearing,
                                                   #fruit_weight_2=g_fruit_tp2,
                                                   timespan_3=timespan_tp1_tp3,
                                                   timespan_1=timespan_tp1_tp2,
                                                   #reducing_factor_weight_increase_stress_2=weight_increase_reducing_factor_stress_tp2_tp3,
                                                   weekly_diameter_increase_2=weekly_diameter_increase_tp2_tp3,
                                                   change_diameter_increase_overbearing_2=diameter_increase_change_overbearing,
                                                   change_diameter_increase_underbearing_2=diameter_increase_change_underbearing,
                                                   fruit_diameter_2=diameter_fruit_tp2,
                                                   reducing_factor_diameter_increase_stress_2=diameter_increase_reducing_factor_stress_tp2_tp3,
                                                   percentage_tree_losses_2=percentage_tree_losses_tp2_tp3,
                                                   risk_tree_losses_2=risk_tree_losses_tp2_tp3,
                                                   trees_per_ha_2=trees_per_ha_2)
      quality_tp4_p3<-quality_tp3_function(n_fruits_3=yield_tp3_p2$number_of_fruits_at_tp3_predicted_at_tp2,
                                           visibly_damaged_3=quality_tp3_p2$percentage_damaged_at_tp3_p2,
                                           risk_sunburn_3=sunburn_risk_tp3_tp4,
                                           damage_sunburn_3=sunburn_damage_tp3_tp4,
                                           risk_sunburn_hailnet_3=sunburn_risk_hailnet_tp3_tp4,
                                           damage_sunburn_hailnet_3=sunburn_damage_hailnet_tp3_tp4,
                                           risk_sunburn_climatizing_irrigation_3=sunburn_risk_climatizing_irrigation_tp3_tp4,
                                           damage_sunburn_climatizing_irrigation_3=sunburn_damage_climatizing_irrigation_tp3_tp4,
                                           risk_sunburn_hailnet_climatizing_irrigation_3=sunburn_risk_hailnet_climatizing_irrigation_tp3_tp4,
                                           damage_sunburn_hailnet_climatizing_irrigation_3=sunburn_damage_hailnet_climatizing_irrigation_tp3_tp4,
                                           risk_rotting_3=rotting_risk_tp3_tp4,
                                           damage_rotting_3=rotting_damage_tp3_tp4,
                                           risk_hail_3=hail_risk_tp3_tp4,
                                           damage_hail_3=hail_damage_tp3_tp4,
                                           risk_hail_hailnet_3=hail_risk_hailnet_tp3_tp4,
                                           damage_hail_hailnet_3=hail_damage_hailnet_tp3_tp4,
                                           percentage_of_hail_falling=percentage_of_hail_damaged_falling,
                                           risk_bird_3=bird_risk_tp3_tp4,
                                           damage_bird_3=bird_damage_tp3_tp4,
                                           risk_mechanical_3=mechanical_risk_tp3_tp4,
                                           damage_mechanical_3=mechanical_damage_tp3_tp4,
                                           risk_fruit_scab_3=fruit_scab_risk_tp3_tp4,
                                           damage_fruit_scab_3=fruit_scab_damage_tp3_tp4,
                                           risk_other_fungal_diseases_3=other_fungal_diseases_risk_tp3_tp4,
                                           damage_other_fungal_diseases_3=other_fungal_diseases_damage_tp3_tp4,
                                           risk_codling_moth_3=codling_moth_damage_tp3_tp4,
                                           damage_codling_moth_3=codling_moth_damage_tp3_tp4,
                                           risk_aphids_3=aphids_risk_tp3_tp4,
                                           damage_aphids_3=aphids_damage_tp3_tp4,
                                           risk_other_insects_3=other_insects_risk_tp3_tp4,
                                           damage_other_insects_3=other_insects_damage_tp3_tp4,
                                           risk_bitter_pit_3=bitter_pit_risk_tp3_tp4,
                                           damage_bitter_pit_3=bitter_pit_damage_tp3_tp4,
                                           risk_bitter_pit_leaf_fertilization_3=bitter_pit_risk_leaf_fertilization_tp3_tp4,
                                           damage_bitter_pit_leaf_fertilization_3=bitter_pit_damage_leaf_fertilization_tp3_tp4,
                                           risk_other_physiological_disorders_3=other_physiological_disorders_risk_tp3_tp4,
                                           damage_other_physiological_disorders_3=other_physiological_disorders_damage_tp3_tp4,
                                           risk_other_physiological_disorders_leaf_fertilization_3=other_physiological_disorders_risk_leaf_fertilization_tp3_tp4,
                                           damage_other_physiological_disorders_leaf_fertilization_3=other_physiological_disorders_damage_leaf_fertilization_tp3_tp4,
                                           sunburn_reduce_kaolin_3=sunburn_reduce_kaolin_tp3_tp4,
                                           #risk_excessive_russeting_3=excessive_russeting_risk_tp3_tp4,
                                           #damage_excessive_russeting_3=excessive_russeting_damage_tp3_tp4,
                                           risk_fruit_cracking_3=fruit_cracking_risk_tp3_tp4,
                                           damage_fruit_cracking_3=fruit_cracking_damage_tp3_tp4,
                                           damage_remove_manual_thinning_3=damage_remove_manual_thinning_tp3_tp4,
                                           thinning_goal_3=desired_number_of_fruits,
                                           variation_thinning_3=manual_thinning_mistake,
                                           add_sunburn_summer_pruning_3=additional_sunburn_summer_pruning_tp3_tp4)
      yield_tp4_p3<-yield_prediction_tp_3_function(#thinning_goal_3=desired_number_of_fruits,
        #variation_thinning_3=manual_thinning_mistake,
        #fruits_per_tree_3=yield_tp3_p2$number_of_fruits_at_tp3_predicted_at_tp2,
        add_fruit_losses_3=quality_tp4_p3$n_falling_hail_tp3_tp4,
        lower_opt_n_fruits_3=lower_optimum_fruits_per_tree,
        range_opt_n_fruits_3=width_optimum_bearing_range,
        #daily_weight_increase_3=daily_weight_increase_tp3_tp4,
        #change_weight_increase_overbearing_3 = weight_increase_change_overbearing,
        #change_weight_increase_underbearing_3 =weight_increase_change_underbearing,
        #fruit_weight_3=yield_tp3_p2$fruit_weight_at_tp3_predicted_at_tp2,
        timespan_3=timespan_tp1_tp3,
        total_timespan = timespan_tp1_harvest,
        timespan_4=timespan_tp4_harvest,
        #reducing_factor_weight_increase_drought_3=weight_increase_reducing_factor_drought_tp3_tp4,
        risk_drought_3=drought_risk_tp3_tp4,
        risk_drought_irrigation_3=drought_risk_irrigation_tp3_tp4,
        n_fruits_per_tree_beginning_tp3=quality_tp4_p3$n_fruits_after_manual_thinning_timepoint,
        weekly_diameter_increase_3=weekly_diameter_increase_tp3_tp4,
        change_diameter_increase_overbearing_3=diameter_increase_change_overbearing,
        change_diameter_increase_underbearing_3=diameter_increase_change_underbearing,
        reducing_factor_diameter_increase_drought_3= diameter_increase_reducing_factor_drought_tp3_tp4,
        fruit_diameter_3=yield_tp3_p2$fruit_diameter_at_tp3_predicted_at_tp2,
        percentage_tree_losses_3=percentage_tree_losses_tp3_tp4,
        risk_tree_losses_3=risk_tree_losses_tp3_tp4,
        trees_per_ha_3=yield_tp3_p2$n_trees_at_tp3_predicted_at_tp2)
      quality_prediction<-quality_tp4_function(n_fruits_4=yield_tp4_p3$number_of_apples_at_tp4_predicted_at_tp3,
                                               visibly_damaged_4=quality_tp4_p3$percentage_damaged_at_tp4_p3,
                                               risk_sunburn_4=sunburn_risk_tp4_harvest,
                                               damage_sunburn_4=sunburn_damage_tp4_harvest,
                                               risk_sunburn_hailnet_4=sunburn_risk_hailnet_tp4_harvest,
                                               damage_sunburn_hailnet_4=sunburn_damage_hailnet_tp4_harvest,
                                               risk_sunburn_climatizing_irrigation_4=sunburn_risk_climatizing_irrigation_tp4_harvest,
                                               damage_sunburn_climatizing_irrigation_4=sunburn_damage_climatizing_irrigation_tp4_harvest,
                                               risk_sunburn_hailnet_climatizing_irrigation_4=sunburn_risk_hailnet_climatizing_irrigation_tp4_harvest,
                                               damage_sunburn_hailnet_climatizing_irrigation_4=sunburn_damage_hailnet_climatizing_irrigation_tp4_harvest,
                                               risk_rotting_4=rotting_risk_tp4_harvest,
                                               damage_rotting_4=rotting_damage_tp4_harvest,
                                               #risk_color_problem_no_cold_4=color_problem_no_cold_risk_tp4_harvest,
                                               #damage_color_problem_no_cold_4=color_problem_no_cold_damage_tp4_harvest,
                                               risk_hail_4=hail_risk_tp4_harvest,
                                               damage_hail_4=hail_damage_tp4_harvest,
                                               risk_hail_hailnet_4=hail_risk_hailnet_tp4_harvest,
                                               damage_hail_hailnet_4=hail_damage_hailnet_tp4_harvest,
                                               percentage_of_hail_falling=percentage_of_hail_damaged_falling,
                                               risk_bird_4=bird_risk_tp4_harvest,
                                               damage_bird_4=bird_damage_tp4_harvest,
                                               risk_mechanical_4=mechanical_risk_tp4_harvest,
                                               damage_mechanical_4=mechanical_damage_tp4_harvest,
                                               risk_fruit_scab_4=fruit_scab_risk_tp4_harvest,
                                               damage_fruit_scab_4=fruit_scab_damage_tp4_harvest,
                                               risk_other_fungal_diseases_4=other_fungal_diseases_risk_tp4_harvest,
                                               damage_other_fungal_diseases_4=other_fungal_diseases_damage_tp4_harvest,
                                               risk_codling_moth_4=codling_moth_damage_tp4_harvest,
                                               damage_codling_moth_4=codling_moth_damage_tp4_harvest,
                                               risk_aphids_4=aphids_risk_tp4_harvest,
                                               damage_aphids_4=aphids_damage_tp4_harvest,
                                               risk_other_insects_4=other_insects_risk_tp4_harvest,
                                               damage_other_insects_4=other_insects_damage_tp4_harvest,
                                               risk_bitter_pit_4=bitter_pit_risk_tp4_harvest,
                                               damage_bitter_pit_4=bitter_pit_damage_tp4_harvest,
                                               risk_bitter_pit_leaf_fertilization_4=bitter_pit_risk_leaf_fertilization_tp4_harvest,
                                               damage_bitter_pit_leaf_fertilization_4=bitter_pit_damage_leaf_fertilization_tp4_harvest,
                                               risk_other_physiological_disorders_4=other_physiological_disorders_risk_tp4_harvest,
                                               damage_other_physiological_disorders_4=other_physiological_disorders_damage_tp4_harvest,
                                               risk_other_physiological_disorders_leaf_fertilization_4=other_physiological_disorders_risk_leaf_fertilization_tp4_harvest,
                                               damage_other_physiological_disorders_leaf_fertilization_4=other_physiological_disorders_damage_leaf_fertilization_tp4_harvest,
                                               risk_color_decrease_summer_pruning_4=color_risk_decrease_summer_pruning_tp4_harvest,
                                               risk_color_decrease_removing_leaves_4=color_risk_decrease_removing_leaves_tp4_harvest,
                                               risk_color_increase_hailnet_4=color_risk_increase_hailnet_tp4_harvest,
                                               risk_no_cold_nights_4=no_cold_nights_risk_tp4_harvest,
                                               risk_color_increase_no_cold_4=color_risk_increase_no_cold_nights_tp4_harvest,
                                               damage_color_decrease_summer_pruning_4=color_damage_decrease_summer_pruning_tp4_harvest,
                                               damage_color_decrease_removing_leaves_4=color_damage_decrease_removing_leaves_tp4_harvest,
                                               damage_color_increase_hailnet_4=color_damage_increase_hailnet_tp4_harvest,
                                               risk_color_variety=color_risk_variety,
                                               damage_color_variety=color_damage_variety,
                                               damage_color_increase_no_cold=color_damage_increase_no_cold_nights_tp4_harvest,
                                               risk_dirty_fruits_4=dirty_fruits_risk_tp4_harvest,
                                               damage_dirty_fruits_4=dirty_fruits_damage_tp4_harvest,
                                               add_sunburn_removing_leaves_4=additional_sunburn_removing_leaves_tp4_harvest,
                                               sunburn_reduce_kaolin_4=sunburn_reduce_kaolin_tp4_harvest,
                                               #risk_excessive_russeting_4=excessive_russeting_risk_tp4_harvest,
                                               #damage_excessive_russeting_4=excessive_russeting_damage_tp4_harvest,
                                               risk_fruit_cracking_4=fruit_cracking_risk_tp4_harvest,
                                               damage_fruit_cracking_4=fruit_cracking_damage_tp4_harvest,
                                               add_sunburn_summer_pruning_4=additional_sunburn_summer_pruning_tp4_harvest)
      yield_prediction<-yield_prediction_tp_4_function(fruit_drop_natural_4 = pre_harvest_fruit_drop_natural,
                                                       fruit_drop_managed_4 = pre_harvest_fruit_drop_spraying,
                                                       storm_risk_4 = risk_of_storm_pre_harvest_tp_4,
                                                       drop_rate_storm_4 = additional_pre_harvest_fruit_drop_storm,
                                                       fruits_per_tree_4 = yield_tp4_p3$number_of_apples_at_tp4_predicted_at_tp3,
                                                       add_fruit_losses_4=quality_prediction$n_falling_hail,
                                                       #fruit_weight_4 = yield_tp4_p3$fruit_weight_at_tp4_predicted_at_tp3,
                                                       #daily_weight_increase_4 = daily_weight_increase_tp4_harvest,
                                                       timespan_4=timespan_tp4_harvest,
                                                       #reducing_factor_weight_increase_drought_4=weight_increase_reducing_factor_drought_tp4_harvest,
                                                       risk_drought_4=drought_risk_tp4_harvest,
                                                       risk_drought_irrigation_4=drought_risk_irrigation_tp4_harvest,
                                                       #mice_risk=tree_losses_mice_risk_tp1_harvest,
                                                       #mice_damage=lost_trees_mice_tp1_harvest,
                                                       reducing_factor_diameter_increase_drought_4=diameter_increase_reducing_factor_drought_tp4_harvest,
                                                       fruit_diameter_4= yield_tp4_p3$fruit_diameter_at_tp4_predicted_at_tp3,
                                                       weekly_diameter_increase_4 = weekly_diameter_increase_tp4_harvest,
                                                       fruit_density_at_harvest=apple_fruit_density_at_harvest,
                                                       percentage_tree_losses_4=percentage_tree_losses_tp4_harvest,
                                                       risk_tree_losses_4=risk_tree_losses_tp4_harvest,
                                                       trees_per_ha_4=yield_tp4_p3$n_trees_at_tp4_predicted_at_tp3)
      
      #quality_yield<-(1-quality_prediction$percentage_damaged_at_harvest)*yield_prediction$yield_at_harvest
      quality_yield_diameter<-(1-quality_prediction$percentage_damaged_at_harvest)*yield_prediction$yield_at_harvest_diameter
      
      return(list(n_damaged_apples_per_tree=quality_prediction$n_damaged_apples,
                  percentage_damaged_at_harvest=quality_prediction$percentage_damaged_at_harvest,
                  total_yield_diameter=yield_prediction$yield_at_harvest_diameter/1000,
                  fruits_per_tree=yield_prediction$number_of_apple_harvested_per_tree,
                  high_quality_yield_diameter=quality_yield_diameter/1000,
                  mean_fruit_diameter_at_harvest=yield_prediction$mean_fruit_diameter_at_harvest,
                  mean_fruit_weight_at_harvest_diameter=yield_prediction$mean_fruit_weight_at_harvest_diameter,
                  n_damaged_apples_per_tree_tp4_p3=quality_tp4_p3$n_damaged_apples_tp4_p3,
                  percentage_damaged_at_tp4_p3=quality_tp4_p3$percentage_damaged_at_tp4_p3,
                  fruits_per_tree_at_tp4_p3=yield_tp4_p3$number_of_apples_at_tp4_predicted_at_tp3,
                  n_damaged_apples_per_tree_tp3_p2=quality_tp3_p2$n_damaged_apples_tp3_p2,
                  percentage_damaged_at_tp3_p2=quality_tp3_p2$percentage_damaged_at_tp3_p2,
                  fruits_per_tree_at_tp3_p2=yield_tp3_p2$number_of_apples_at_tp3_predicted_at_tp2,
                  n_apples_tp_3=yield_tp3_p2$number_of_fruits_at_tp3_predicted_at_tp2,
                  n_apples_tp_4=yield_tp4_p3$number_of_apples_at_tp4_predicted_at_tp3,
                  percentage_damaged_tp_3=quality_tp3_p2$percentage_damaged_at_tp3_p2,
                  percentage_damaged_tp_4=quality_tp4_p3$percentage_damaged_at_tp4_p3,
                  percentage_undamaged_at_harvest=1-quality_prediction$percentage_damaged_at_harvest,
                  mean_fruit_diameter_tp_4=yield_tp4_p3$fruit_diameter_at_tp4_predicted_at_tp3,
                  mean_fruit_diameter_tp_3=yield_tp3_p2$fruit_diameter_at_tp3_predicted_at_tp2))
    }
    
    
    #Monte Carlo####
    apple_quality_and_yield_mc_simulation_tp2 <- mcSimulation(estimate = as.estimate(Apple_prediction_input),
                                                              model_function = tp_2_quality_and_yield_prediction,
                                                              numberOfModelRuns = input$runs2,
                                                              functionSyntax = "plainNames")
    
    Plot_a<-plot_distributions(mcSimulation_object = apple_quality_and_yield_mc_simulation_tp2,
                               vars = c("high_quality_yield_diameter"),
                               method = 'smooth_simple_overlay',
                               base_size = 7)+
      theme(axis.text = element_text(colour = "black", size = 10),
            axis.title = element_text(colour = "black", size = 10),
            legend.position = "none")+
      xlab("Qualitätsertrag [t/ha]")+
      ylab("Häufigkeit")
    Plot_b<-plot_distributions(mcSimulation_object = apple_quality_and_yield_mc_simulation_tp2,
                               vars = c("total_yield_diameter"),
                               method = 'smooth_simple_overlay',
                               base_size = 7)+
      theme(axis.text = element_text(colour = "black", size = 10),
            axis.title = element_text(colour = "black", size = 10),
            legend.position = "none")+
      xlab("Gesamtertrag [t/ha]")+
      ylab("Häufigkeit")
    Plot_c<-plot_distributions(mcSimulation_object = apple_quality_and_yield_mc_simulation_tp2,
                               vars = c("fruits_per_tree"),
                               method = 'smooth_simple_overlay',
                               base_size = 7)+
      theme(axis.text = element_text(colour = "black", size = 10),
            axis.title = element_text(colour = "black", size = 10),
            legend.position = "none")+
      xlab("Früchte pro Baum")+
      ylab("Häufigkeit")
    
    library(patchwork)
    Plots_combined <- list(Plot_a, Plot_b,Plot_c)
    wrap_plots(Plots_combined, nrow = 3) +
      plot_layout(guides = "keep")
    
  })
  output$TP1 <- renderPlot({
    Management_valuestp1<-data.frame(management_measure=c("mechanical_flower_thinning",
                                                          "chemical_flower_thinning",
                                                          "frost_protection",
                                                          "chemical_fruit_thinning",
                                                          "pollinator_support",
                                                          "manual_thinning_after_june_drop",
                                                          "spray_against_pre_harvest_fruit_drop",
                                                          "hailnet",
                                                          "climatizing_ov_irrigation",
                                                          "leaf_fertilization",
                                                          "summer_pruning",
                                                          "removing_leaves",
                                                          "use_kaolin",
                                                          "irrigation"),value= as.numeric(c(input$mechanical_flower_thinning1,
                                                                                            input$chemical_flower_thinning1,
                                                                                            input$frost_protection1,
                                                                                            input$chemical_fruit_thinning1,
                                                                                            input$pollinator_support1,
                                                                                            input$manual_thinning1,
                                                                                            input$drop1,
                                                                                            input$hailnet1,
                                                                                            input$cl_irri1,
                                                                                            input$foliar_fert1,
                                                                                            input$summer_pruning1,
                                                                                            input$removing_leaves1,
                                                                                            input$kaolin1,
                                                                                            input$irrigation1)))
    write.csv(Management_valuestp1, "test_management_values1.csv")
    library(decisionSupport)
    library(tidyverse)
    
    variable<-c("trees_per_ha1", "flower_clusters_per_tree1")
    distribution<-c("const", "posnorm")
    lower<-c(input$trees_per_ha1,input$flower_clusters_per_tree1[1])
    median<-rep(NA, 2)
    upper<-c(input$trees_per_ha1,input$flower_clusters_per_tree1[2])
    unit<-c("trees","flowers")
    Apple_prediction_slider_input<-data.frame(variable,distribution,lower,median,upper,unit)
    #orchard_data<-read.csv2("2024_test_apple/test_input.csv", colClasses = c("character", "character", "numeric", "character","numeric", "character"), sep = ";", dec = ".")
    #Apple_prediction_input<-rbind(Apple_prediction_input_csv, Apple_prediction_slider_input)
    Apple_prediction_input<-rbind(edited_r()[1:6], Apple_prediction_slider_input)
    
    
    
    
    
    source("management_values1.R")
    
    
    
    #Quality and Yield tp 1####
    
    tp_1_quality_and_yield_prediction<-function(){
      yield_tp2_p1<-yield_prediction_tp_1_function(flower_cluster_1=round(flower_clusters_per_tree1),
                                                   flowers_per_cluster_1=n_flowers_per_cluster,
                                                   risk_frost_1=frost_risk,
                                                   frost_damage_1=percentage_frost_damage,
                                                   frost_protection_eff_1=frost_protection_efficiency,
                                                   eff_mech_flower_thinning_1=effiecency_mechanical_thinning,
                                                   eff_chem_flower_thinning_1=efficiency_chemical_thinning,
                                                   frist_drop_rate_1=drop_rate_frist_drop,
                                                   #exp_fruit_weight_tp2_1=fruit_weight_tp2,
                                                   exp_fruit_diameter_tp2_1=expected_fruit_diameter_tp2,
                                                   insect_damage_risk_tp1_tp2=risk_insect_damage_tp1_tp2,
                                                   loss_insect_damage_tp1_tp2=insect_damage_tp1_tp2,
                                                   unfavorable_weather_during_blossom_risk=risk_unfavorable_weather_tp1,
                                                   additional_fruit_drop_unfavorable_weather_tp1_tp2=additional_fruit_drop_unfavorable_weather_tp1,
                                                   fruit_drop_decrease_pollinator_support_tp1=decrease_fruit_drop_pollinator_support_tp1,
                                                   percentage_tree_losses_1=percentage_tree_losses_tp1_tp2,
                                                   risk_tree_losses_1=risk_tree_losses_tp1_tp2,
                                                   trees_per_ha_1=trees_per_ha1)
      quality_tp2_p1<-quality_tp1_function(flower_cluster_1=round(flower_clusters_per_tree1),
                                           flowers_per_cluster_1=n_flowers_per_cluster,
                                           frost_event=yield_tp2_p1$frost_occurrence,
                                           percentage_quality_reduce_frost=quality_reduce_frost_tp1_tp2,
                                           insect_damage_blossom=yield_tp2_p1$insect_damage_tp1,
                                           percentage_quality_reduce_insects=quality_reduce_insects_tp1_tp2,
                                           risk_aphids_1=aphids_risk_tp1_tp2,
                                           damage_aphids_1=aphids_damage_tp1_tp2,
                                           risk_mechanical_1=mechanical_risk_tp1_tp2,
                                           damage_mechanical_1=mechanical_damage_tp1_tp2,
                                           risk_fruit_scab_1=fruit_scab_risk_tp1_tp2,
                                           damage_fruit_scab_1=fruit_scab_damage_tp1_tp2,
                                           risk_excessive_russeting_1 = excessive_russeting_risk_tp1_tp2,
                                           damage_excessive_russeting_1=excessive_russeting_damage_tp1_tp2,
                                           risk_hail_1=hail_risk_tp1_tp2,
                                           damage_hail_1=hail_damage_tp1_tp2,
                                           risk_hail_hailnet_1=hail_risk_hailnet_tp1_tp2,
                                           damage_hail_hailnet_1=hail_damage_hailnet_tp1_tp2,
                                           percentage_of_hail_falling=percentage_of_hail_damaged_falling,
                                           risk_rotting_1=rotting_risk_tp1_tp2,
                                           damage_rotting_1=rotting_damage_tp1_tp2)
      quality_tp3_p2<-quality_tp2_function(n_fruits_2=yield_tp2_p1$number_of_fruits_at_tp_2_predicted_at_tp_1,
                                           visibly_damaged_2=quality_tp2_p1$percentage_damaged_at_tp2_p1,
                                           bad_seed_structure_risk_2=risk_bad_seed_structure,
                                           reduce_bad_seed_structure_risk_pollinator_support=bad_seed_structure_reduce_pollinator_support,
                                           deformation_bad_seed_structure_2=bad_seed_structure_deformation_tp2,
                                           risk_hail_2=hail_risk_tp2_tp3,
                                           damage_hail_2=hail_damage_tp2_tp3,
                                           risk_hail_hailnet_2=hail_risk_hailnet_tp2_tp3,
                                           damage_hail_hailnet_2=hail_damage_hailnet_tp2_tp3,
                                           percentage_of_hail_falling=percentage_of_hail_damaged_falling,
                                           risk_mechanical_2=mechanical_risk_tp2_tp3,
                                           damage_mechanical_2=mechanical_damage_tp2_tp3,
                                           risk_fruit_scab_2=fruit_scab_risk_tp2_tp3,
                                           damage_fruit_scab_2=fruit_scab_damage_tp2_tp3,
                                           risk_other_fungal_diseases_2=other_fungal_diseases_risk_tp2_tp3,
                                           damage_other_fungal_diseases_2=other_fungal_diseases_damage_tp2_tp3,
                                           risk_codling_moth_2=codling_moth_damage_tp2_tp3,
                                           damage_codling_moth_2=codling_moth_damage_tp2_tp3,
                                           risk_aphids_2=aphids_risk_tp2_tp3,
                                           damage_aphids_2=aphids_damage_tp2_tp3,
                                           risk_other_insects_2=other_insects_risk_tp2_tp3,
                                           damage_other_insects_2=other_insects_damage_tp2_tp3,
                                           risk_excessive_russeting_2=excessive_russeting_risk_tp2_tp3,
                                           damage_excessive_russeting_2=excessive_russeting_damage_tp2_tp3,
                                           risk_fruit_cracking_2=fruit_cracking_risk_tp2_tp3,
                                           damage_fruit_cracking_2=fruit_cracking_damage_tp2_tp3,
                                           risk_rotting_2=rotting_risk_tp2_tp3,
                                           damage_rotting_2=rotting_damage_tp2_tp3)
      yield_tp3_p2<-yield_prediction_tp_2_function(fruits_per_tree_2= yield_tp2_p1$number_of_fruits_at_tp_2_predicted_at_tp_1,
                                                   thinner_efficiency_2= thinner_efficiency_chemical_fruit_thinning,
                                                   lower_opt_n_fruits_2=lower_optimum_fruits_per_tree,
                                                   range_opt_n_fruits_2=width_optimum_bearing_range,
                                                   june_drop_change_overbearing_2=june_drop_increase_overbearing,
                                                   add_fruit_losses_2=quality_tp3_p2$n_falling_hail_tp2_tp3,
                                                   occourrence_bad_seed_structure=quality_tp3_p2$bad_seed_structure_occurrence,
                                                   stress_risk_2=risk_stress_and_supply_influence_tp2_tp3,
                                                   stress_risk_irrigation_2=risk_stress_and_supply_influence_irrigation_tp2_tp3,
                                                   june_drop_change_bad_seed_structure_2=june_drop_increase_bad_seed_structure,
                                                   june_drop_change_stress_and_supply_problem_2=june_drop_increase_stress_and_supply_problem,
                                                   drop_rate_2=june_drop_rate,
                                                   #daily_weight_increase_2=daily_weight_increase_tp2_tp3,
                                                   #change_weight_increase_overbearing_2=weight_increase_change_overbearing,
                                                   #change_weight_increase_underbearing_2=weight_increase_change_underbearing,
                                                   #fruit_weight_2=yield_tp2_p1$fruit_weight_at_tp2_predicted_at_tp_1,
                                                   timespan_3=timespan_tp1_tp3,
                                                   timespan_1=timespan_tp1_tp2,
                                                   #reducing_factor_weight_increase_stress_2=weight_increase_reducing_factor_stress_tp2_tp3,
                                                   weekly_diameter_increase_2=weekly_diameter_increase_tp2_tp3,
                                                   change_diameter_increase_overbearing_2=diameter_increase_change_overbearing,
                                                   change_diameter_increase_underbearing_2=diameter_increase_change_underbearing,
                                                   fruit_diameter_2=yield_tp2_p1$fruit_diameter_at_tp2_predicted_at_tp1,
                                                   reducing_factor_diameter_increase_stress_2=diameter_increase_reducing_factor_stress_tp2_tp3,
                                                   percentage_tree_losses_2=percentage_tree_losses_tp2_tp3,
                                                   risk_tree_losses_2=risk_tree_losses_tp2_tp3,
                                                   trees_per_ha_2=yield_tp2_p1$n_trees_at_tp2_predicted_at_tp1)
      quality_tp4_p3<-quality_tp3_function(n_fruits_3=yield_tp3_p2$number_of_fruits_at_tp3_predicted_at_tp2,
                                           visibly_damaged_3=quality_tp3_p2$percentage_damaged_at_tp3_p2,
                                           risk_sunburn_3=sunburn_risk_tp3_tp4,
                                           damage_sunburn_3=sunburn_damage_tp3_tp4,
                                           risk_sunburn_hailnet_3=sunburn_risk_hailnet_tp3_tp4,
                                           damage_sunburn_hailnet_3=sunburn_damage_hailnet_tp3_tp4,
                                           risk_sunburn_climatizing_irrigation_3=sunburn_risk_climatizing_irrigation_tp3_tp4,
                                           damage_sunburn_climatizing_irrigation_3=sunburn_damage_climatizing_irrigation_tp3_tp4,
                                           risk_sunburn_hailnet_climatizing_irrigation_3=sunburn_risk_hailnet_climatizing_irrigation_tp3_tp4,
                                           damage_sunburn_hailnet_climatizing_irrigation_3=sunburn_damage_hailnet_climatizing_irrigation_tp3_tp4,
                                           risk_rotting_3=rotting_risk_tp3_tp4,
                                           damage_rotting_3=rotting_damage_tp3_tp4,
                                           risk_hail_3=hail_risk_tp3_tp4,
                                           damage_hail_3=hail_damage_tp3_tp4,
                                           risk_hail_hailnet_3=hail_risk_hailnet_tp3_tp4,
                                           damage_hail_hailnet_3=hail_damage_hailnet_tp3_tp4,
                                           percentage_of_hail_falling=percentage_of_hail_damaged_falling,
                                           risk_bird_3=bird_risk_tp3_tp4,
                                           damage_bird_3=bird_damage_tp3_tp4,
                                           risk_mechanical_3=mechanical_risk_tp3_tp4,
                                           damage_mechanical_3=mechanical_damage_tp3_tp4,
                                           risk_fruit_scab_3=fruit_scab_risk_tp3_tp4,
                                           damage_fruit_scab_3=fruit_scab_damage_tp3_tp4,
                                           risk_other_fungal_diseases_3=other_fungal_diseases_risk_tp3_tp4,
                                           damage_other_fungal_diseases_3=other_fungal_diseases_damage_tp3_tp4,
                                           risk_codling_moth_3=codling_moth_damage_tp3_tp4,
                                           damage_codling_moth_3=codling_moth_damage_tp3_tp4,
                                           risk_aphids_3=aphids_risk_tp3_tp4,
                                           damage_aphids_3=aphids_damage_tp3_tp4,
                                           risk_other_insects_3=other_insects_risk_tp3_tp4,
                                           damage_other_insects_3=other_insects_damage_tp3_tp4,
                                           risk_bitter_pit_3=bitter_pit_risk_tp3_tp4,
                                           damage_bitter_pit_3=bitter_pit_damage_tp3_tp4,
                                           risk_bitter_pit_leaf_fertilization_3=bitter_pit_risk_leaf_fertilization_tp3_tp4,
                                           damage_bitter_pit_leaf_fertilization_3=bitter_pit_damage_leaf_fertilization_tp3_tp4,
                                           risk_other_physiological_disorders_3=other_physiological_disorders_risk_tp3_tp4,
                                           damage_other_physiological_disorders_3=other_physiological_disorders_damage_tp3_tp4,
                                           risk_other_physiological_disorders_leaf_fertilization_3=other_physiological_disorders_risk_leaf_fertilization_tp3_tp4,
                                           damage_other_physiological_disorders_leaf_fertilization_3=other_physiological_disorders_damage_leaf_fertilization_tp3_tp4,
                                           sunburn_reduce_kaolin_3=sunburn_reduce_kaolin_tp3_tp4,
                                           #risk_excessive_russeting_3=excessive_russeting_risk_tp3_tp4,
                                           #damage_excessive_russeting_3=excessive_russeting_damage_tp3_tp4,
                                           risk_fruit_cracking_3=fruit_cracking_risk_tp3_tp4,
                                           damage_fruit_cracking_3=fruit_cracking_damage_tp3_tp4,
                                           damage_remove_manual_thinning_3=damage_remove_manual_thinning_tp3_tp4,
                                           thinning_goal_3=desired_number_of_fruits,
                                           variation_thinning_3=manual_thinning_mistake,
                                           add_sunburn_summer_pruning_3=additional_sunburn_summer_pruning_tp3_tp4)
      yield_tp4_p3<-yield_prediction_tp_3_function(#thinning_goal_3=desired_number_of_fruits,
        #variation_thinning_3=manual_thinning_mistake,
        #fruits_per_tree_3=yield_tp3_p2$n_fruits_per_tree_tp3,
        add_fruit_losses_3=quality_tp4_p3$n_falling_hail_tp3_tp4,
        lower_opt_n_fruits_3=lower_optimum_fruits_per_tree,
        range_opt_n_fruits_3=width_optimum_bearing_range,
        #daily_weight_increase_3=daily_weight_increase_tp3_tp4,
        #change_weight_increase_overbearing_3 = weight_increase_change_overbearing,
        #change_weight_increase_underbearing_3 =weight_increase_change_underbearing,
        #fruit_weight_3=yield_tp3_p2$fruit_weight_at_tp3_predicted_at_tp2,
        timespan_3=timespan_tp1_tp3,
        total_timespan = timespan_tp1_harvest,
        timespan_4=timespan_tp4_harvest,
        #reducing_factor_weight_increase_drought_3=weight_increase_reducing_factor_drought_tp3_tp4,
        risk_drought_3=drought_risk_tp3_tp4,
        risk_drought_irrigation_3=drought_risk_irrigation_tp3_tp4,
        n_fruits_per_tree_beginning_tp3=quality_tp4_p3$n_fruits_after_manual_thinning_timepoint,
        weekly_diameter_increase_3=weekly_diameter_increase_tp3_tp4,
        change_diameter_increase_overbearing_3=diameter_increase_change_overbearing,
        change_diameter_increase_underbearing_3=diameter_increase_change_underbearing,
        reducing_factor_diameter_increase_drought_3= diameter_increase_reducing_factor_drought_tp3_tp4,
        fruit_diameter_3=yield_tp3_p2$fruit_diameter_at_tp3_predicted_at_tp2,
        percentage_tree_losses_3=percentage_tree_losses_tp3_tp4,
        risk_tree_losses_3=risk_tree_losses_tp3_tp4,
        trees_per_ha_3=yield_tp3_p2$n_trees_at_tp3_predicted_at_tp2)
      quality_prediction<-quality_tp4_function(n_fruits_4=yield_tp4_p3$number_of_apples_at_tp4_predicted_at_tp3,
                                               visibly_damaged_4=quality_tp4_p3$percentage_damaged_at_tp4_p3,
                                               risk_sunburn_4=sunburn_risk_tp4_harvest,
                                               damage_sunburn_4=sunburn_damage_tp4_harvest,
                                               risk_sunburn_hailnet_4=sunburn_risk_hailnet_tp4_harvest,
                                               damage_sunburn_hailnet_4=sunburn_damage_hailnet_tp4_harvest,
                                               risk_sunburn_climatizing_irrigation_4=sunburn_risk_climatizing_irrigation_tp4_harvest,
                                               damage_sunburn_climatizing_irrigation_4=sunburn_damage_climatizing_irrigation_tp4_harvest,
                                               risk_sunburn_hailnet_climatizing_irrigation_4=sunburn_risk_hailnet_climatizing_irrigation_tp4_harvest,
                                               damage_sunburn_hailnet_climatizing_irrigation_4=sunburn_damage_hailnet_climatizing_irrigation_tp4_harvest,
                                               risk_rotting_4=rotting_risk_tp4_harvest,
                                               damage_rotting_4=rotting_damage_tp4_harvest,
                                               #risk_color_problem_no_cold_4=color_problem_no_cold_risk_tp4_harvest,
                                               #damage_color_problem_no_cold_4=color_problem_no_cold_damage_tp4_harvest,
                                               risk_hail_4=hail_risk_tp4_harvest,
                                               damage_hail_4=hail_damage_tp4_harvest,
                                               risk_hail_hailnet_4=hail_risk_hailnet_tp4_harvest,
                                               damage_hail_hailnet_4=hail_damage_hailnet_tp4_harvest,
                                               percentage_of_hail_falling=percentage_of_hail_damaged_falling,
                                               risk_bird_4=bird_risk_tp4_harvest,
                                               damage_bird_4=bird_damage_tp4_harvest,
                                               risk_mechanical_4=mechanical_risk_tp4_harvest,
                                               damage_mechanical_4=mechanical_damage_tp4_harvest,
                                               risk_fruit_scab_4=fruit_scab_risk_tp4_harvest,
                                               damage_fruit_scab_4=fruit_scab_damage_tp4_harvest,
                                               risk_other_fungal_diseases_4=other_fungal_diseases_risk_tp4_harvest,
                                               damage_other_fungal_diseases_4=other_fungal_diseases_damage_tp4_harvest,
                                               risk_codling_moth_4=codling_moth_damage_tp4_harvest,
                                               damage_codling_moth_4=codling_moth_damage_tp4_harvest,
                                               risk_aphids_4=aphids_risk_tp4_harvest,
                                               damage_aphids_4=aphids_damage_tp4_harvest,
                                               risk_other_insects_4=other_insects_risk_tp4_harvest,
                                               damage_other_insects_4=other_insects_damage_tp4_harvest,
                                               risk_bitter_pit_4=bitter_pit_risk_tp4_harvest,
                                               damage_bitter_pit_4=bitter_pit_damage_tp4_harvest,
                                               risk_bitter_pit_leaf_fertilization_4=bitter_pit_risk_leaf_fertilization_tp4_harvest,
                                               damage_bitter_pit_leaf_fertilization_4=bitter_pit_damage_leaf_fertilization_tp4_harvest,
                                               risk_other_physiological_disorders_4=other_physiological_disorders_risk_tp4_harvest,
                                               damage_other_physiological_disorders_4=other_physiological_disorders_damage_tp4_harvest,
                                               risk_other_physiological_disorders_leaf_fertilization_4=other_physiological_disorders_risk_leaf_fertilization_tp4_harvest,
                                               damage_other_physiological_disorders_leaf_fertilization_4=other_physiological_disorders_damage_leaf_fertilization_tp4_harvest,
                                               risk_color_decrease_summer_pruning_4=color_risk_decrease_summer_pruning_tp4_harvest,
                                               risk_color_decrease_removing_leaves_4=color_risk_decrease_removing_leaves_tp4_harvest,
                                               risk_color_increase_hailnet_4=color_risk_increase_hailnet_tp4_harvest,
                                               risk_no_cold_nights_4=no_cold_nights_risk_tp4_harvest,
                                               risk_color_increase_no_cold_4=color_risk_increase_no_cold_nights_tp4_harvest,
                                               damage_color_decrease_summer_pruning_4=color_damage_decrease_summer_pruning_tp4_harvest,
                                               damage_color_decrease_removing_leaves_4=color_damage_decrease_removing_leaves_tp4_harvest,
                                               damage_color_increase_hailnet_4=color_damage_increase_hailnet_tp4_harvest,
                                               risk_color_variety=color_risk_variety,
                                               damage_color_variety=color_damage_variety,
                                               damage_color_increase_no_cold=color_damage_increase_no_cold_nights_tp4_harvest,
                                               risk_dirty_fruits_4=dirty_fruits_risk_tp4_harvest,
                                               damage_dirty_fruits_4=dirty_fruits_damage_tp4_harvest,
                                               add_sunburn_removing_leaves_4=additional_sunburn_removing_leaves_tp4_harvest,
                                               sunburn_reduce_kaolin_4=sunburn_reduce_kaolin_tp4_harvest,
                                               #risk_excessive_russeting_4=excessive_russeting_risk_tp4_harvest,
                                               #damage_excessive_russeting_4=excessive_russeting_damage_tp4_harvest,
                                               risk_fruit_cracking_4=fruit_cracking_risk_tp4_harvest,
                                               damage_fruit_cracking_4=fruit_cracking_damage_tp4_harvest,
                                               add_sunburn_summer_pruning_4=additional_sunburn_summer_pruning_tp4_harvest)
      yield_prediction<-yield_prediction_tp_4_function(fruit_drop_natural_4 = pre_harvest_fruit_drop_natural,
                                                       fruit_drop_managed_4 = pre_harvest_fruit_drop_spraying,
                                                       storm_risk_4 = risk_of_storm_pre_harvest_tp_4,
                                                       drop_rate_storm_4 = additional_pre_harvest_fruit_drop_storm,
                                                       fruits_per_tree_4 = yield_tp4_p3$number_of_apples_at_tp4_predicted_at_tp3,
                                                       add_fruit_losses_4=quality_prediction$n_falling_hail,
                                                       #fruit_weight_4 = yield_tp4_p3$fruit_weight_at_tp4_predicted_at_tp3,
                                                       #daily_weight_increase_4 = daily_weight_increase_tp4_harvest,
                                                       timespan_4=timespan_tp4_harvest,
                                                       #trees_per_ha_4 = n_trees_per_ha,
                                                       #reducing_factor_weight_increase_drought_4=weight_increase_reducing_factor_drought_tp4_harvest,
                                                       risk_drought_4=drought_risk_tp4_harvest,
                                                       risk_drought_irrigation_4=drought_risk_irrigation_tp4_harvest,
                                                       #mice_risk=tree_losses_mice_risk_tp1_harvest,
                                                       #mice_damage=lost_trees_mice_tp1_harvest,
                                                       reducing_factor_diameter_increase_drought_4=diameter_increase_reducing_factor_drought_tp4_harvest,
                                                       fruit_diameter_4= yield_tp4_p3$fruit_diameter_at_tp4_predicted_at_tp3,
                                                       weekly_diameter_increase_4 = weekly_diameter_increase_tp4_harvest,
                                                       fruit_density_at_harvest=apple_fruit_density_at_harvest,
                                                       percentage_tree_losses_4=percentage_tree_losses_tp4_harvest,
                                                       risk_tree_losses_4=risk_tree_losses_tp4_harvest,
                                                       trees_per_ha_4=yield_tp4_p3$n_trees_at_tp4_predicted_at_tp3)
      
      #quality_yield<-(1-quality_prediction$percentage_damaged_at_harvest)*yield_prediction$yield_at_harvest
      quality_yield_diameter<-(1-quality_prediction$percentage_damaged_at_harvest)*yield_prediction$yield_at_harvest_diameter
      
      return(list(n_damaged_apples_per_tree=quality_prediction$n_damaged_apples,
                  percentage_damaged_at_harvest=quality_prediction$percentage_damaged_at_harvest,
                  total_yield_diameter=yield_prediction$yield_at_harvest_diameter/1000,
                  fruits_per_tree=yield_prediction$number_of_apple_harvested_per_tree,
                  high_quality_yield_diameter=quality_yield_diameter/1000,
                  mean_fruit_diameter_at_harvest=yield_prediction$mean_fruit_diameter_at_harvest,
                  mean_fruit_weight_at_harvest_diameter=yield_prediction$mean_fruit_weight_at_harvest_diameter,
                  n_apples_tp_2=yield_tp2_p1$number_of_fruits_at_tp_2_predicted_at_tp_1,
                  n_apples_tp_3=yield_tp3_p2$number_of_fruits_at_tp3_predicted_at_tp2,
                  n_apples_tp_4=yield_tp4_p3$number_of_apples_at_tp4_predicted_at_tp3,
                  percentage_damaged_tp_2=quality_tp2_p1$percentage_damaged_at_tp2_p1,
                  percentage_damaged_tp_3=quality_tp3_p2$percentage_damaged_at_tp3_p2,
                  percentage_damaged_tp_4=quality_tp4_p3$percentage_damaged_at_tp4_p3,
                  percentage_undamaged_at_harvest=1-quality_prediction$percentage_damaged_at_harvest,
                  mean_fruit_diameter_tp_4=yield_tp4_p3$fruit_diameter_at_tp4_predicted_at_tp3,
                  mean_fruit_diameter_tp_3=yield_tp3_p2$yield_tp3_p2$fruit_diameter_at_tp3_predicted_at_tp2,
                  mean_fruit_diameter_tp_2=yield_tp2_p1$fruit_diameter_at_tp2_predicted_at_tp1))
    }
    
    #Monte Carlo####
    apple_quality_and_yield_mc_simulation_tp1 <- mcSimulation(estimate = as.estimate(Apple_prediction_input),
                                                              model_function = tp_1_quality_and_yield_prediction,
                                                              numberOfModelRuns = input$runs1,
                                                              functionSyntax = "plainNames")
    
    Plot_a<-plot_distributions(mcSimulation_object = apple_quality_and_yield_mc_simulation_tp1,
                               vars = c("high_quality_yield_diameter"),
                               method = 'smooth_simple_overlay',
                               base_size = 7)+
      theme(axis.text = element_text(colour = "black", size = 10),
            axis.title = element_text(colour = "black", size = 10),
            legend.position = "none")+
      xlab("Qualitätsertrag [t/ha]")+
      ylab("Häufigkeit")
    Plot_b<-plot_distributions(mcSimulation_object = apple_quality_and_yield_mc_simulation_tp1,
                               vars = c("total_yield_diameter"),
                               method = 'smooth_simple_overlay',
                               base_size = 7)+
      theme(axis.text = element_text(colour = "black", size = 10),
            axis.title = element_text(colour = "black", size = 10),
            legend.position = "none")+
      xlab("Gesamtertrag [t/ha]")+
      ylab("Häufigkeit")
    Plot_c<-plot_distributions(mcSimulation_object = apple_quality_and_yield_mc_simulation_tp1,
                               vars = c("fruits_per_tree"),
                               method = 'smooth_simple_overlay',
                               base_size = 7)+
      theme(axis.text = element_text(colour = "black", size = 10),
            axis.title = element_text(colour = "black", size = 10),
            legend.position = "none")+
      xlab("Früchte pro Baum")+
      ylab("Häufigkeit")
    
    library(patchwork)
    Plots_combined <- list(Plot_a, Plot_b,Plot_c)
    wrap_plots(Plots_combined, nrow = 3) +
      plot_layout(guides = "keep")
    
  })
  output$input_datasheet<-DT::renderDT(rbind(Apple_input[1:7], Apple_estimation[1:7]))
  
  output$text <- renderUI({
    url <- a("Experimentierfelds Suedwest", href="https://ef-sw.de")
    logo<-img(src="BMEL_BLE.png", height =200)
    HTML(paste("<b>Entwicklung der App:</b> Christine Schmitz<sup>1,2</sup><br>",
               "<b>Entwicklung des Modells:</b> Christine Schmitz<sup>1,2</sup>, Lars Zimmermann<sup>1,2</sup>, Katja Schiffers<sup>2</sup>, Eike Luedeling<sup>2</sup> <br>",
               "<br>",
               "<sup>1</sup>Dienstleistungszentrum Ländlicher Raum Rheinpfalz, Campus Klein-Altendorf 2, 53359 Rheinbach, Germany <br>",
               "<sup>2</sup>INRES – Horticultural Sciences, University of Bonn, Auf dem Hügel 6, 53121 Bonn, Germany.",
               "<br>",
               "Kontakt: christine.schmitz@dlr.rlp.de<br>",
               "<br>",
               "<b>Idee hinter dem Modell:</b><br>",
               "Ziel des Modelles ist die Abschätzung des zu erwartende Gesamtertrags und des Qualitätsertrags von Äpfeln bei der Ernte zu vier entscheidenden Zeitpunkten im Produktionszyklus: (i) zur Vollblüte, (ii) vor dem Ausdünnen der Früchte, (iii) nach dem Junifall und (iv) vier Wochen vor der Ernte. Solche Prognosen können Obstbauern und Vermarktern sowohl bei kurzfristigen betrieblichen Entscheidungen während der Vegetationsperiode als auch bei der langfristigen strategischen Planung helfen.<br>",
               "<br>",
               "<b>Methodik</b><br>",
               "Das Modell wurde mit Hilfe des probabilistischen Modellierungsansatzes, der auf Techniken der Entscheidungsanalyse basiert erstellt. Dies beinhaltet eine Erarbeitung der Modellgrundlage und Inputwerte mit Experten aus der Obstbranche, die Verwendung vom Wertebereichen als Inputvariablen im Rahmen einer Monte-Carlo Simulation.<br>",
               "Auch wenn die Unsicherheit und die natürliche Variabilität der Inputwerte zu breiten Ergebnisverteilungen führen, geben diese Verteilungen ein ehrliches Bild der potenziellen Ernteergebnisse in einer Apfelplantage. <br>",
               "<br>",
               "<b>Förderung</b><br>",
               "Die Entwicklung des Modelles und der App wurde im Rahmen des<br>",
               url,
               " durchgeführt und wurde vom Bundesministerium für Landwirtschaft und Ernährung gefördert (Förderkennzeichen 28DE111B22)<br>",
               logo,
               sep = ""))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

