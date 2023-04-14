# This is a Shiny web application created by SpatialBros.  
# You can run the application by clicking 
# the 'Run App' button above.

library(shiny)
library(shinythemes)
library(sp)
library(sf)
library(rgdal)
library(spNetwork)
library(tmap)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(spatstat)
library(maptools)
library(raster)

sf_childcare <- read_rds("rds/childcare.rds")
sf_business <- read_rds("rds/business_establishments.rds")
sf_drinking_fountain <- read_rds("rds/drinking_fountain.rds")
sf_landmarks <- read_rds("rds/landmarks.rds")
sf_pub_toilets <- read_rds("rds/public_toilets.rds")

melb_lga <- read_rds("rds/melbourne_local_government_areas.rds")
melb_localities <- read_rds("rds/melbourne_localities.rds")

net_ped <- read_rds("rds/pedestrian_network_lines.rds")
net_road <- read_rds("rds/road_network_lines.rds")
net_tram <- read_rds("rds/tram_network_lines.rds")


# Define UI for application
ui <- fluidPage(theme = shinytheme("darkly"),
      navbarPage(                                                
        title = div(a(img(src='logo.png',
                          style="margin-top: -18px;padding-right:10px;padding-bottom:10px",
                          height = 60), href="https://spatialbros.netlify.app/")),
        tags$head(tags$style(
          type="text/css",
          "#childcare_image img {max-width: 100%; width: 100%; height: auto}",
          "#introductionkernel img {max-width: 100%; width: 100%; height: auto}",
          ".navbar {background-color: rgb(48, 48, 48)}",
          " #clark{
                      color: grey;
                      background: rgba(0, 0, 0, 0);
                      border-color: grey;
                      border-style: solid;
                      border-width: 2px;
                    }",
          " #netKDESettings {
                      color: grey;
                      background: rgba(0, 0, 0, 0);
                      border-color: grey;
                      border-style: solid;
                      border-width: 2px;
                    }",
          " #KDESettings {
                      color: grey;
                      background: rgba(0, 0, 0, 0);
                      border-color: grey;
                      border-style: solid;
                      border-width: 2px;
                    }",
          
          
        )),
        tabPanel("Home Page",
                 fluidRow(
                   column(9,
                          h2("Project Motivation"),
                          hr(),
                      column(6,
                         uiOutput("projectMotivation"),                         
                             ),
                      column(6, 
                        imageOutput("childcare_image",)
                             ),
                   ),

                   column(3,
                          h1("Spatial Bros"),
                          uiOutput("myList"),
                   )),
                 fluidRow(
                   column(8,
                          h2("What is Point Pattern Analysis and Network Constrained Point Pattern Analysis?"),
                          hr(),
                          uiOutput("ppancppa")),
                   ),
                 fluidRow(
                   column(8,
                          h2("About Spatial Bros"),
                          hr(),
                          uiOutput("aboutus")),
                   )
                 ),
        # 2nd Tab
        tabPanel("Data Exploration",
                 titlePanel("Data Exploration"),
                 hr(),
                 tabsetPanel(type = "tabs",
                             tabPanel("Introduction",
                                      uiOutput("IntroData"),
                             ),
                             tabPanel("Network",
                                      sidebarLayout(
                                        mainPanel(
                                          tmapOutput("DataExpMapNet", width = "100%", height = "700")
                                        ),
                                        sidebarPanel(
                                          h4("Data Exploration Variable Inputs"),
                                          h5("Network"),
                                          selectInput(inputId = "datanetlocalities",
                                                      label = "Localities",
                                                      choices = list("Entire City of Melbourne" = "Entire City of Melbourne",
                                                                     "Carlton" = "Carlton",
                                                                     "Carlton North" = "Carlton North",
                                                                     "Docklands" = "Docklands",
                                                                     "East Melbourne" = "East Melbourne",
                                                                     "Flemington" = "Flemington",
                                                                     "Kensington" = "Kensington",
                                                                     "Melbourne" = "Melbourne",
                                                                     "North Melbourne" = "North Melbourne",
                                                                     "Parkville" = "Parkville",
                                                                     "Port Melbourne" = "Port Melbourne",
                                                                     "South Wharf" = "South Wharf",
                                                                     "South Yarra" = "South Yarra",
                                                                     "Southbank" = "Southbank",
                                                                     "West Melbourne" = "West Melbourne"
                                                      )),
                                          selectInput(inputId = "datanetnetwork_type",
                                                      label = "Types of Network",
                                                      choices = list("Road Network" = "net_road",
                                                                     "Pedestrian Network" = "net_ped",
                                                                     "Tram Network" = "net_tram")),
                                          
                                          actionButton("DataNet", "Generate Map and Data Table"),
                                          
                                          h5("Please note: The map and data will take a few moments to generate after clicking the button."),
                                          
                                        ),
                                        
                                      ),
                                      
                                      br(),
                                      h3("Data Table"),
                                      hr(),
                                      h5("Results will be shown in the table below if dataset is selected."),
                                      dataTableOutput('DataExpTabNet')
                                      
                                      
                                       
                                      
                                      
                                      
                             ),
                             tabPanel("Spatial Points",
                                      sidebarLayout(
                                        mainPanel(
                                          tmapOutput("DataExpMapStat", width = "100%", height = "700")
                                        ),
                                        sidebarPanel(
                                          shinyjs::useShinyjs(),
                                          h4("Data Exploration Variable Inputs"),
                                          h5("Spatial Points"),
                                          selectInput(inputId = "dataspatlocalities",
                                                      label = "Localities",
                                                      choices = list("Entire City of Melbourne" = "Entire City of Melbourne",
                                                                     "Carlton" = "Carlton",
                                                                     "Carlton North" = "Carlton North",
                                                                     "Docklands" = "Docklands",
                                                                     "East Melbourne" = "East Melbourne",
                                                                     "Flemington" = "Flemington",
                                                                     "Kensington" = "Kensington",
                                                                     "Melbourne" = "Melbourne",
                                                                     "North Melbourne" = "North Melbourne",
                                                                     "Parkville" = "Parkville",
                                                                     "Port Melbourne" = "Port Melbourne",
                                                                     "South Wharf" = "South Wharf",
                                                                     "South Yarra" = "South Yarra",
                                                                     "Southbank" = "Southbank",
                                                                     "West Melbourne" = "West Melbourne"
                                                      )),
                                          selectInput(inputId = "dataspatlocs",
                                                      label = "Location of Interest",
                                                      choices = list("Business Establishments" = "sf_business",
                                                                     "Childcare Centres" = "sf_childcare",
                                                                     "Drinking Fountain" = "sf_drinking_fountain",
                                                                     "Landmarks" = "sf_landmarks",
                                                                     "Public Toilets" = "sf_pub_toilets")),
                                          selectInput(inputId = "dataspatsublocs",
                                                      label = "Specific Themes/Sub-Categories",
                                                      choices = list()),
                                          
                                          actionButton("DataSpat", "Generate Map and Data Table"),
                                          
                                          h5("Please note: The map and data will take a few moments to generate after clicking the button."),
                                          
                                        ),
                                        
                                      ),      
                                      br(),
                                      h3("Data Table"),
                                      hr(),
                                      h5("Results will be shown in the table below if dataset is selected."),
                                      dataTableOutput('DataExpTabSpat')
                                      
                             ),
                             
                 ),
        ),
        
        tabPanel("Spatial Point Pattern Analysis",
                 titlePanel("Kernel Density Estimation"),
                 hr(),
                   tabsetPanel(type = "tabs",
                               tabPanel("Introduction",
                               ),             
                               tabPanel("1st Order Spatial Point Pattern Analysis",
                                        sidebarLayout(
                                          mainPanel(
                                            tmapOutput("mapFOKDE", width = "100%", height = "700")
                                          ),
                                          sidebarPanel(
                                            shinyjs::useShinyjs(),
                                            h4("Kernel Density Estimation Variable Inputs"),
                                            h5("Spatial Points"),
                                            selectInput(inputId = "kde_localities",
                                                        label = "Localities",
                                                        choices = list("Entire City of Melbourne" = "Entire City of Melbourne",
                                                                       "Carlton" = "Carlton",
                                                                       "Carlton North" = "Carlton North",
                                                                       "Docklands" = "Docklands",
                                                                       "East Melbourne" = "East Melbourne",
                                                                       "Flemington" = "Flemington",
                                                                       "Kensington" = "Kensington",
                                                                       "Melbourne" = "Melbourne",
                                                                       "North Melbourne" = "North Melbourne",
                                                                       "Parkville" = "Parkville",
                                                                       "Port Melbourne" = "Port Melbourne",
                                                                       "South Wharf" = "South Wharf",
                                                                       "South Yarra" = "South Yarra",
                                                                       "Southbank" = "Southbank",
                                                                       "West Melbourne" = "West Melbourne"
                                                        )),
                                            selectInput(inputId = "kde_locs",
                                                        label = "Location of Interest",
                                                        choices = list("Business Establishments" = "sf_business",
                                                                       "Childcare Centres" = "sf_childcare",
                                                                       "Drinking Fountain" = "sf_drinking_fountain",
                                                                       "Landmarks" = "sf_landmarks",
                                                                       "Public Toilets" = "sf_pub_toilets")),
                                            selectInput(inputId = "kdesublocs",
                                                        label = "Specific Themes/Sub-Categories",
                                                        choices = list()),
                                            h5("Kernel Density Estimation Methods"),
                                            selectInput(inputId = "kde_bandwidth_type",
                                                        label = "Choose the bandwidth to be used:",
                                                        choices = list("Fixed Bandwidth" = "fixed",
                                                                       "Adaptive Bandwidth" = "adaptive")),
                                            selectInput(inputId = "kde_kernel_name",
                                                        label = "Choose the kernel to be used:",
                                                        choices = list("Quartic" = "quartic",
                                                                       "Epanechnikov" = "epanechnikov",
                                                                       "Gaussian" = "gaussian")),
                                            selectInput(inputId = "kde_method_name",
                                                        label = "Select the Method to be used",
                                                        choices = list("Simple" = "simple",
                                                                       "Discontinuous" = "discontinuous",
                                                                       "Continuous" = "Continuous")),
                                            h5("Clark Evans Test Confidence"),
                                            selectInput(inputId = "kde_confidence",
                                                        label = "Select the Confidence to be used",
                                                        choices = list("95%" = 39,
                                                                       "99%" = 199,
                                                                       "99.9%" = 1999)),
                                            
                                            
                                            actionButton("GenerateKDE", "Generate KDE Map"),
                                            
                                            h5("Please note: The map will take a few minutes to generate after clicking the button."),
                                            
                                          ),
                                          
                                        ),
                                        h3("Clark and Evans Test"),
                                        hr(),
                                        h5("Results will be shown in the block below if analysis has been run."),
                                        verbatimTextOutput("clark"),
                                        
                               ),
                               tabPanel("2nd Order Spatial Point Pattern Analysis",
                                       sidebarLayout(
                                         mainPanel(
                                           plotlyOutput("gplot"),
                                           plotlyOutput("kplot"),
                                         ),
                                         sidebarPanel(
                                           shinyjs::useShinyjs(),
                                           h4("G/K Function Variable Inputs"),
                                           h5("Spatial Points"),
                                           selectInput(inputId = "statSO_localities",
                                                       label = "Localities",
                                                       choices = list("Entire City of Melbourne" = "Entire City of Melbourne",
                                                                      "Carlton" = "Carlton",
                                                                      "Carlton North" = "Carlton North",
                                                                      "Docklands" = "Docklands",
                                                                      "East Melbourne" = "East Melbourne",
                                                                      "Flemington" = "Flemington",
                                                                      "Kensington" = "Kensington",
                                                                      "Melbourne" = "Melbourne",
                                                                      "North Melbourne" = "North Melbourne",
                                                                      "Parkville" = "Parkville",
                                                                      "Port Melbourne" = "Port Melbourne",
                                                                      "South Wharf" = "South Wharf",
                                                                      "South Yarra" = "South Yarra",
                                                                      "Southbank" = "Southbank",
                                                                      "West Melbourne" = "West Melbourne"
                                                       )),
                                           selectInput(inputId = "statSO_locs",
                                                       label = "Location of Interest",
                                                       choices = list(
                                                                      "Business Establishments" = "sf_business",
                                                                      "Childcare Centres" = "sf_childcare",
                                                                      "Drinking Fountain" = "sf_drinking_fountain",
                                                                      "Landmarks" = "sf_landmarks",
                                                                      "Public Toilets" = "sf_pub_toilets")),
                                           selectInput(inputId = "statSO_sublocs",
                                                       label = "Specific Themes/Sub-Categories",
                                                       choices = list()),
                                           h5("G/K Function Parameters"),
                                           selectInput(inputId = "statSO_confidence",
                                                       label = "Select the Confidence to be used",
                                                       choices = list("95%" = "39",
                                                                      "99%" = "199",
                                                                      "99.9%" = "1999")),
                                           
                                           
                                           actionButton("GenerateSO", "Generate Analysis"),
                                           
                                           h5("Please note: The analysis will take a few minutes to generate after clicking the button."),                                         ),
                                         
                                       ),

                                              
                                        
                               ),
                             
                   ),          
                 
                 ),
        
        # 3rd Tab
        tabPanel("Network Constrained Point Pattern Analysis",
              titlePanel("Network Constrained Point Pattern Analysis"),
              hr(),
                 tabsetPanel(type = "tabs",
                  tabPanel("Introduction",
                           fluidRow(
                             column(6,
                                    h2("Welcome to the Network Constrained Point Pattern Analysis Page!"),
                                    hr(),
                                    uiOutput("introductiondescription")                         
                                    
                             ),
                             column(6,
                                    h2(),
                                    imageOutput("introductionKernel")),
                            ),
                           
                           
                           
                  ),             
                  tabPanel("Network Kernel Density Estimation", 
                    sidebarLayout(
                      mainPanel(
                        tmapOutput("mapPlot", width = "100%", height = "700"),
                        br(),
                        verbatimTextOutput("netKDESettings"),
                      ),
                      sidebarPanel(
                        shinyjs::useShinyjs(),
                        h4("Network Kernel Density Estimation Variable Inputs"),
                        h5("Network and Spatial Points"),
                        selectInput(inputId = "localities",
                                    label = "Localities",
                                    choices = list("Entire City of Melbourne" = "Entire City of Melbourne",
                                                   "Carlton" = "Carlton",
                                                   "Carlton North" = "Carlton North",
                                                   "Docklands" = "Docklands",
                                                   "East Melbourne" = "East Melbourne",
                                                   "Flemington" = "Flemington",
                                                   "Kensington" = "Kensington",
                                                   "Melbourne" = "Melbourne",
                                                   "North Melbourne" = "North Melbourne",
                                                   "Parkville" = "Parkville",
                                                   "Port Melbourne" = "Port Melbourne",
                                                   "South Wharf" = "South Wharf",
                                                   "South Yarra" = "South Yarra",
                                                   "Southbank" = "Southbank",
                                                   "West Melbourne" = "West Melbourne"
                                    )),
                        selectInput(inputId = "network_type",
                                    label = "Types of Network",
                                    choices = list("Road Network" = "net_road",
                                                   "Pedestrian Network" = "net_ped",
                                                   "Tram Network" = "net_tram")),
                        selectInput(inputId = "locs",
                                    label = "Location of Interest",
                                    choices = list(
                                                   "Business Establishments" = "sf_business",
                                                   "Childcare Centres" = "sf_childcare",
                                                   "Drinking Fountain" = "sf_drinking_fountain",
                                                   "Landmarks" = "sf_landmarks",
                                                   "Public Toilets" = "sf_pub_toilets")),
                        selectInput(inputId = "netsublocs",
                                    label = "Specific Themes/Sub-Categories",
                                    choices = list()),
                        h5("Lixels"),
                        sliderInput(inputId = "lx_length", "Length of Lixel",
                                    min = 100, max = 1500, value = 700, step = 50),
                        sliderInput(inputId = "lx_length_min", "Min. Lixel Length",
                                    min = 100, max = 1500, value = 350, step = 50),
                        h5("Kernel Density Estimation Methods"),
                        selectInput(inputId = "kernel_name",
                                    label = "Choose the kernel to be used:",
                                    choices = list("Quartic" = "quartic",
                                                   "Triangle" = "triangle",
                                                   "Tricube" = "tricube",
                                                   "Cosine" = "cosine",
                                                   "Triweight" = "triweight",
                                                   "Epanechnikov" = "epanechnikov",
                                                   "Uniform" = "uniform")),
                        selectInput(inputId = "method_name",
                                    label = "Select the Method to be used",
                                    choices = list("Simple" = "simple",
                                                   "Discontinuous" = "discontinuous",
                                                   "Continuous" = "Continuous")),
                                
                      actionButton("netKDEGenerate", "Generate KDE Map"),
                      
                      h5("Please note: The map will take a few minutes to generate after clicking the button."),
                    
                      ),
                    
                    ),
                    uiOutput("netKDEExpaliner"),
                    br(),
                  ),
                tabPanel("Statistical Functions", 
                         sidebarLayout(
                           mainPanel(
                             plotlyOutput("kfun"),
                             plotlyOutput("gfun"),
                           ),
                           sidebarPanel(
                             shinyjs::useShinyjs(),
                             h4("Statistical Function Variable Inputs"),
                             h5("Network and Spatial Points"),
                             selectInput(inputId = "netstatlocalities",
                                         label = "Localities",
                                         choices = list("Entire City of Melbourne" = "Entire City of Melbourne",
                                                        "Carlton" = "Carlton",
                                                        "Carlton North" = "Carlton North",
                                                        "Docklands" = "Docklands",
                                                        "East Melbourne" = "East Melbourne",
                                                        "Flemington" = "Flemington",
                                                        "Kensington" = "Kensington",
                                                        "Melbourne" = "Melbourne",
                                                        "North Melbourne" = "North Melbourne",
                                                        "Parkville" = "Parkville",
                                                        "Port Melbourne" = "Port Melbourne",
                                                        "South Wharf" = "South Wharf",
                                                        "South Yarra" = "South Yarra",
                                                        "Southbank" = "Southbank",
                                                        "West Melbourne" = "West Melbourne"
                                         )),
                             selectInput(inputId = "netstatnetwork_type",
                                         label = "Types of Network",
                                         choices = list("Road Network" = "net_road",
                                                        "Pedestrian Network" = "net_ped",
                                                        "Tram Network" = "net_tram")),
                             selectInput(inputId = "netstatlocs",
                                         label = "Location of Interest",
                                         choices = list(
                                                        "Business Establishments" = "sf_business",
                                                        "Childcare Centres" = "sf_childcare",
                                                        "Drinking Fountain" = "sf_drinking_fountain",
                                                        "Landmarks" = "sf_landmarks",
                                                        "Public Toilets" = "sf_pub_toilets")),
                             selectInput(inputId = "netstatsublocs",
                                         label = "Specific Themes/Sub-Categories",
                                         choices = list()),
                             h5("Other Variables"),
                             sliderInput(inputId = "netstatnet_start", "Start",
                                         min = 0, max = 2000, value = 100, step = 50),
                             sliderInput(inputId = "netstatnet_end", "End",
                                         min = 100, max = 5000, value = 500, step = 50),
                             sliderInput(inputId = "netstatn_sims", "Number of Simulations",
                                         min = 10, max = 300, value = 50, step = 5),
                             sliderInput(inputId = "netstatagg", "Aggregate Value",
                                       min = 0, max = 1000, value = 0, step = 50),
                             
                             actionButton("netKDEGenerateStats", "Generate Statistical Results"),
                             
                             h5("Please note: The graphs will take a few minutes to generate."),
                             
                           ),
                         ),
                         uiOutput("netStatsExplainerp1"),
                         hr(),
                         uiOutput("netStatsExplainerp2"),
                    ),
                 ),
                 

        )
      )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  id <- NULL
  
  output$mapPlot <- renderTmap({
    tm_shape(melb_localities) +
      tm_polygons("LOC_NAME", alpha=0.1) +
    tm_shape(melb_lga) +
      tm_borders(lwd = 2, lty = 5,  col="blue") 
  })
  
  output$DataExpMapNet <- renderTmap({
    tm_shape(melb_localities) +
      tm_polygons("LOC_NAME", alpha=0.1) +
      tm_shape(melb_lga) +
      tm_borders(lwd = 2, lty = 5,  col="blue") 
  })
  
  output$DataExpMapStat <- renderTmap({
    tm_shape(melb_localities) +
      tm_polygons("LOC_NAME", alpha=0.1) +
      tm_shape(melb_lga) +
      tm_borders(lwd = 2, lty = 5,  col="blue") 
  })

  output$mapFOKDE <- renderTmap({
    tm_shape(melb_localities) +
      tm_polygons("LOC_NAME", alpha=0.1) +
      tm_shape(melb_lga) +
      tm_borders(lwd = 2, lty = 5,  col="blue") 
  })
  
  
  observeEvent(input$dataspatlocs,{
    if (input$dataspatlocs == "sf_business"){
      shinyjs::enable('dataspatsublocs')
      options <- sort(unique(sf_business$industry_anzsic4_description))
      updateSelectInput(session, "dataspatsublocs",
                        choices = c("All", options)
                        )
    }
    else if (input$dataspatlocs == "sf_landmarks"){
      shinyjs::enable('dataspatsublocs')
      options <- sort(unique(sf_landmarks$theme))
      updateSelectInput(session, "dataspatsublocs",
                        choices = c("All", options)
                        )
    }
    else{
      shinyjs::disable('dataspatsublocs')
    }
  })
  
  observeEvent(input$netstatlocs,{
    if (input$netstatlocs == "sf_business"){
      shinyjs::enable('netstatsublocs')
      options <- sort(unique(sf_business$industry_anzsic4_description))
      updateSelectInput(session, "netstatsublocs",
                        choices = c("All", options)
      )
    }
    else if (input$netstatlocs == "sf_landmarks"){
      shinyjs::enable('netstatsublocs')
      options <- sort(unique(sf_landmarks$theme))
      updateSelectInput(session, "netstatsublocs",
                        choices = c("All", options)
      )
    }
    else{
      shinyjs::disable('netstatsublocs')
    }
  })
  
  observeEvent(input$locs,{
    if (input$locs == "sf_business"){
      shinyjs::enable('netsublocs')
      options <- sort(unique(sf_business$industry_anzsic4_description))
      updateSelectInput(session, "netsublocs",
                        choices = c("All", options)
      )
    }
    else if (input$locs == "sf_landmarks"){
      shinyjs::enable('netsublocs')
      options <- sort(unique(sf_landmarks$theme))
      updateSelectInput(session, "netsublocs",
                        choices = c("All", options)
      )
    }
    else{
      shinyjs::disable('netsublocs')
    }
  })
  
  observeEvent(input$statSO_locs,{
    if (input$statSO_locs == "sf_business"){
      shinyjs::enable('statSO_sublocs')
      options <- sort(unique(sf_business$industry_anzsic4_description))
      updateSelectInput(session, "statSO_sublocs",
                        choices = c("All", options)
      )
    }
    else if (input$statSO_locs == "sf_landmarks"){
      shinyjs::enable('statSO_sublocs')
      options <- sort(unique(sf_landmarks$theme))
      updateSelectInput(session, "statSO_sublocs",
                        choices = c("All", options)
      )
    }
    else{
      shinyjs::disable('statSO_sublocs')
    }
  })
  
  #DATA EXPLORATION
  
  observeEvent(input$DataNet, {
    id <<- showNotification(paste("Generating Map and Data table..."), duration = 0, type = "message", closeButton=FALSE)

    if (input$datanetlocalities == "Entire City of Melbourne"){
      localities <- melb_localities
      boundary <- melb_lga
      network_type <- get(input$datanetnetwork_type)
    }
    else {
      localities <- melb_localities %>% filter(LOC_NAME == input$datanetlocalities)
      boundary <- localities
      network_type <- st_intersection(get(input$datanetnetwork_type), boundary) %>% st_cast("LINESTRING")
      
    }
    
    output$DataExpMapNet <- renderTmap({
      tm_shape(localities) +
        tm_polygons("LOC_NAME", alpha=0.1) +
        tm_shape(boundary) +
        tm_borders(lwd = 2.5, lty = 5,  col="blue") +
        tm_shape(network_type) +
        tm_lines(lwd = 1.5)
    })
    
    output$DataExpTabNet <- renderDataTable(network_type)
    
    if (!is.null(id))
      removeNotification(id)
    id <<- NULL    
    
  })
  
  observeEvent(input$DataSpat, {
    id <<- showNotification(paste("Generating Map and Data table..."), duration = 0, type = "message", closeButton=FALSE)
    
    listing <- input$dataspatsublocs
    
    if (input$dataspatlocs == "sf_business"){
      if (listing == "All"){
        datalocs <- sf_business
      }
      else{
        datalocs <- sf_business %>% filter(industry_anzsic4_description == listing)
      }
      
      
    }
    else if (input$dataspatlocs == "sf_landmarks"){
      if (listing == "All"){
        datalocs <- sf_landmarks
      }
      else{
        datalocs <- sf_landmarks %>% filter(theme == listing)
      }
    }
    else{
      datalocs <- get(input$dataspatlocs)
    }

    if (input$dataspatlocalities == "Entire City of Melbourne"){
      localities <- melb_localities
      boundary <- melb_lga
      loc_interest <- datalocs
    }
    else {
      localities <- melb_localities %>% filter(LOC_NAME == input$dataspatlocalities)
      boundary <- localities
      loc_interest <- st_intersection(datalocs, boundary)
    }
    
    if (nrow(st_intersection(datalocs, boundary)) == 0){
      if (!is.null(id))
        removeNotification(id)
      id <<- NULL    
      
      showNotification(paste(listing, "in", input$netstatlocalities, "has no spatial points to be analysed. The analysis has been terminated."), duration = 0, type = "warning")
      return()
    }
    
    output$DataExpMapStat <- renderTmap({
      tm_shape(localities) +
        tm_polygons("LOC_NAME", alpha=0.1) +
        tm_shape(boundary) +
        tm_borders(lwd = 2.5, lty = 5,  col="blue") +
        tm_shape(loc_interest)+ 
        tm_dots(size = 0.03, alpha = 0.6) 
    })
    
    output$DataExpTabSpat <- renderDataTable(loc_interest)
    
    if (!is.null(id))
      removeNotification(id)
    id <<- NULL    
    
  })
  
  observeEvent(input$netKDEGenerateStats, {
    id <<- showNotification(paste("Calculating Statisical Results..."), duration = 0, type = "message", closeButton=FALSE)
    
    listing <- input$netstatsublocs
    
    if (input$netstatlocs == "sf_business"){
      if (listing == "All"){
        datalocs <- sf_business
      }
      else{
        datalocs <- sf_business %>% filter(industry_anzsic4_description == listing)
      }
    }
    else if (input$netstatlocs == "sf_landmarks"){
      if (listing == "All"){
        datalocs <- sf_landmarks
      }
      else{
        datalocs <- sf_landmarks %>% filter(theme == listing)
      }
    }
    else{
      datalocs <- get(input$netstatlocs)
    }
    
    if (input$netstatlocalities == "Entire City of Melbourne"){
      localities <- melb_localities
      boundary <- melb_lga
      loc_interest <- datalocs
      network_type <- get(input$netstatnetwork_type)
    }
    else {
      localities <- melb_localities %>% filter(LOC_NAME == input$netstatlocalities)
      boundary <- localities
      loc_interest <- st_intersection(datalocs, boundary)
      network_type <- st_intersection(get(input$netstatnetwork_type), boundary) %>% st_cast("LINESTRING")
    }
  

    if (nrow(st_intersection(datalocs, boundary)) == 0){
      if (!is.null(id))
        removeNotification(id)
      id <<- NULL    
      
      showNotification(paste(listing, "in", input$netstatlocalities, "has no spatial points to be analysed. The analysis has been terminated."), duration = 0, type = "warning")
      return()
    }
    
    if (input$netstatagg == 0){
      agg <- NULL
    }
    else{
      agg <- input$netstatagg
    }

    kfun_output <- kfunctions(network_type,
                              loc_interest,
                               start = input$netstatnet_start, 
                               end = input$netstatnet_end, 
                               step = 50, 
                               width = 50, 
                               nsim = input$netstatn_sims, 
                               resolution = 50,
                               verbose = FALSE,
                               agg = agg,
                               conf_int = 0.05)
    
    output$kfun <- renderPlotly({kfun_output$plotk})
    output$gfun <- renderPlotly({kfun_output$plotg})
    
    if (!is.null(id))
      removeNotification(id)
    id <<- NULL    
  })
  
  observeEvent(input$netKDEGenerate, {
    id <<- showNotification(paste("Generating KDE Map..."), duration = 0, type = "message", closeButton=FALSE)
    
    listing <- input$netsublocs
    
    if (input$locs == "sf_business"){
      if (listing == "All"){
        datalocs <- sf_business
      }
      else{
        datalocs <- sf_business %>% filter(industry_anzsic4_description == listing)
      }
    }
    else if (input$locs == "sf_landmarks"){
      if (listing == "All"){
        datalocs <- sf_landmarks
      }
      else{
        datalocs <- sf_landmarks %>% filter(theme == listing)
      }
    }
    else{
      datalocs <- get(input$locs)
    }
    
    if (input$localities == "Entire City of Melbourne"){
      localities <- melb_localities
      boundary <- melb_lga
      network_type <- get(input$network_type)
      loc_interest <- datalocs
    }
    else {
      localities <- melb_localities %>% filter(LOC_NAME == input$localities)
      boundary <- localities
      loc_interest <- st_intersection(datalocs, boundary)
      network_type <- st_intersection(get(input$network_type), boundary) %>% st_cast("LINESTRING")
    }
    
    if (nrow(loc_interest) == 0){
      if (!is.null(id))
        removeNotification(id)
      id <<- NULL    
      
      showNotification(paste(listing, "in", input$localities, "has no spatial points to be analysed. The analysis has been terminated."), duration = 0, type = "warning")
      return()
    }
    else if (nrow(loc_interest) == 1){
      if (!is.null(id))
        removeNotification(id)
      id <<- NULL    
      
      showNotification(paste(listing, "in", input$localities, "has only one spatial points to be analysed and is insufficient for the analysis. The analysis has been terminated."), duration = 0, type = "warning")
      return()
    }
    
    id2 <<- showNotification(paste("Calculating NetworkKDE Bandwidth..."), duration = 0, type = "default", closeButton=FALSE)
    
    cv_scores <- bw_cv_likelihood_calc(c(100,900),20,
                             network_type, loc_interest,
                             rep(1,nrow(loc_interest)),
                             input$kernel_name, input$method_name, verbose=FALSE, check=TRUE)
    max_index <- which.max(cv_scores$cv_scores)
    max_bandwidth <- cv_scores$bw[max_index]
    
    output$netKDESettings <- renderText(paste("NetKDE Bandwidth Selection:", max_bandwidth))
    
    if (!is.null(id2))
      removeNotification(id2)
    id2 <<- NULL    
    
    id2 <<- showNotification(paste("Calculating Network Kernel Density Estimate..."), duration = 0, type = "default", closeButton=FALSE)
    

        
    road_lixels_cc <- lixelize_lines(network_type, input$lx_length, mindist = input$lx_length_min)
    road_samples_cc <- lines_center(road_lixels_cc)
    road_network_cc_densities <- nkde(network_type,
                                      events = loc_interest,
                                      w = rep(1,nrow(loc_interest)), 
                                      samples = road_samples_cc, 
                                      kernel_name =  noquote(input$kernel_name),
                                      bw = max_bandwidth, 
                                      div= "bw", 
                                      adaptive = FALSE,
                                      trim_bw = NULL,
                                      method = noquote(input$method_name), 
                                      digits = 3, 
                                      tol = 1,
                                      grid_shape = c(1,1), 
                                      max_depth = 8,
                                      agg = 10,
                                      sparse = TRUE,
                                      verbose = FALSE)
    
    road_samples_cc$density <- road_network_cc_densities * 1000
    road_lixels_cc$density <- road_network_cc_densities * 1000
    
    if (!is.null(id2))
      removeNotification(id2)
    id2 <<- NULL    
    
    id2 <<- showNotification(paste("Plotting map..."), duration = 0, type = "default", closeButton=FALSE)
    
    output$mapPlot <- renderTmap({
      tm_shape(localities) +
        tm_polygons("LOC_NAME", alpha=0.1) +
        tm_shape(boundary) +
        tm_borders(lwd = 2.5, lty = 5,  col="blue") +
        tm_shape(road_lixels_cc) +
        tm_lines(lwd = 1.5, col="density")+
        tm_shape(loc_interest)+ 
        tm_dots(size = 0.03, alpha = 0.6) 
    })
    
    if (!is.null(id2))
      removeNotification(id2)
    id2 <<- NULL    
    
    if (!is.null(id))
      removeNotification(id)
    id <<- NULL    
    
  })
  
  #KDE 1st ORDER
  
  #Monitor buttons
  
  observeEvent(input$kde_bandwidth_type, {
    if(input$kde_bandwidth_type == "adaptive") {
      shinyjs::disable('kde_kernel_name') 
      shinyjs::disable('kde_method_name') 
    } else {
      shinyjs::enable('kde_kernel_name')
      shinyjs::enable('kde_method_name')
    }
  }, ignoreNULL = T)
  
  observeEvent(input$kde_locs,{
    if (input$kde_locs == "sf_business"){
      shinyjs::enable('kdesublocs')
      options <- sort(unique(sf_business$industry_anzsic4_description))
      updateSelectInput(session, "kdesublocs",
                        choices = c("All", options)
      )
    }
    else if (input$kde_locs == "sf_landmarks"){
      shinyjs::enable('kdesublocs')
      options <- sort(unique(sf_landmarks$theme))
      updateSelectInput(session, "kdesublocs",
                        choices = c("All", options)
      )
    }
    else{
      shinyjs::disable('kdesublocs')
    }
  })
  
  #Main Func
  
  observeEvent(input$GenerateKDE, {
    id <<- showNotification(paste("Generating KDE Map..."), duration = 0, type = "message", closeButton=FALSE)
    
    listing <- input$kdesublocs
    
    if (input$kde_locs == "sf_business"){
      if (listing == "All"){
        datalocs <- sf_business
      }
      else{
        datalocs <- sf_business %>% filter(industry_anzsic4_description == listing)
      }
    }
    else if (input$kde_locs == "sf_landmarks"){
      if (listing == "All"){
        datalocs <- sf_landmarks
      }
      else{
        datalocs <- sf_landmarks %>% filter(theme == listing)
      }
    }
    else{
      datalocs <- get(input$kde_locs)
    }
    
    if (input$kde_localities == "Entire City of Melbourne"){
      localities <- melb_localities
      boundary <- melb_lga
      loc_interest <- datalocs
    }
    else {
      localities <- melb_localities %>% filter(LOC_NAME == input$kde_localities)
      boundary <- localities
      
      loc_interest <- st_intersection(datalocs, boundary)
    }

        if (nrow(loc_interest) == 0){
      if (!is.null(id))
        removeNotification(id)
      id <<- NULL    
      
      showNotification(paste(listing, "in", input$kde_localities, "has no spatial points to be analysed. The analysis has been terminated."), duration = 0, type = "warning")
      return()
    }
    else if (nrow(loc_interest) == 1){
      if (!is.null(id))
        removeNotification(id)
      id <<- NULL    
      
      showNotification(paste(listing, "in", input$kde_localities, "has only one spatial points to be analysed and is insufficient for the analysis. The analysis has been terminated."), duration = 0, type = "warning")
      return()
    }
    
    # CONVERT TO PPP
    
    loc_interest.gsp <- as_Spatial(loc_interest)
    loc_interest.spc <- as(loc_interest.gsp, "SpatialPoints")
    loc_interest.ppp <- as(loc_interest.spc, "ppp")
    
    # Handling duplicated events
    
    if (any(duplicated(loc_interest.ppp))){
      loc_interest.ppp <- rjitter(loc_interest.ppp,
                                  retry = TRUE,
                                  nsim = 1,
                                  drop = TRUE)
    }
    
    #OWIN
    boundary.spat <- as_Spatial(boundary)
    boundary.sp <- as(boundary.spat, "SpatialPolygons")
    boundary.owin <- as(boundary.sp, "owin")
    
    #Combining OWIN
    
    loc_interest.ppp = loc_interest.ppp[boundary.owin]
    loc_interest_obj.km <- rescale(loc_interest.ppp, 1000, "km")
    
    # KDE
    
    if (input$kde_bandwidth_type == "adaptive"){
      kde <- adaptive.density(loc_interest.ppp, method="kernel")
    }
    else{
      kde <- density(loc_interest.ppp, sigma = bw.diggle, edge = TRUE, kernel = input$kde_kernel_name, main = input$kde_method_name)
    }

    #RASTER
    kde.grid <- as.SpatialGridDataFrame.im(kde)
    kde_raster <- raster(kde.grid)
    crs(kde_raster) <- st_crs(7855)$wkt 
    
    output$mapFOKDE <- renderTmap({
      tm_shape(localities) +
        
      tm_borders(alpha = 0.3) +
        tm_shape(boundary) +
        tm_borders(lwd = 2.5, lty = 5,  col="blue") +
        tm_shape(kde_raster) +
        tm_raster("v", alpha = 0.7)
    })
    
    # CLARK EVANS
    
    output$clark <- renderPrint(clarkevans.test(loc_interest.ppp, correction="none", clipregion = NULL, alternative=c("two.sided"), nsim=input$kde_confidence))
    
    if (!is.null(id))
      removeNotification(id)
    id <<- NULL    
    
  })
  
  
  #Main Func
  
  observeEvent(input$GenerateSO, {
    id <<- showNotification(paste("Generating Second Order Analysis..."), duration = 0, type = "message", closeButton=FALSE)
    
    
    listing <- input$statSO_sublocs
    
    if (input$statSO_locs == "sf_business"){
      if (listing == "All"){
        datalocs <- sf_business
      }
      else{
        datalocs <- sf_business %>% filter(industry_anzsic4_description == listing)
      }
    }
    else if (input$statSO_locs == "sf_landmarks"){
      if (listing == "All"){
        datalocs <- sf_landmarks
      }
      else{
        datalocs <- sf_landmarks %>% filter(theme == listing)
      }
    }
    else{
      datalocs <- get(input$statSO_locs)
    }
    
    if (input$statSO_localities == "Entire City of Melbourne"){
      localities <- melb_localities
      boundary <- melb_lga
      loc_interest <- datalocs
    }
    else {
      localities <- melb_localities %>% filter(LOC_NAME == input$statSO_localities)
      boundary <- localities
      loc_interest <- st_intersection(datalocs, boundary)
    }
    
    if (nrow(loc_interest) == 0){
      if (!is.null(id))
        removeNotification(id)
      id <<- NULL    
      
      showNotification(paste(listing, "in", input$statSO_localities, "has no spatial points to be analysed. The analysis has been terminated."), duration = 0, type = "warning")
      return()
    }
    else if (nrow(loc_interest) == 1){
      if (!is.null(id))
        removeNotification(id)
      id <<- NULL    
      
      showNotification(paste(listing, "in", input$statSO_localities, "has only one spatial points to be analysed and is insufficient for the analysis. The analysis has been terminated."), duration = 0, type = "warning")
      return()
    }
    
    # CONVERT TO PPP
    
    loc_interest.gsp <- as_Spatial(loc_interest)
    loc_interest.spc <- as(loc_interest.gsp, "SpatialPoints")
    loc_interest.ppp <- as(loc_interest.spc, "ppp")
    
    # Handling duplicated events
    
    if (any(duplicated(loc_interest.ppp))){
      loc_interest.ppp <- rjitter(loc_interest.ppp,
                                  retry = TRUE,
                                  nsim = 1,
                                  drop = TRUE)
    }
    
    #OWIN
    boundary.spat <- as_Spatial(boundary)
    boundary.sp <- as(boundary.spat, "SpatialPolygons")
    boundary.owin <- as(boundary.sp, "owin")
    
    #Combining OWIN
    loc_interest.ppp = loc_interest.ppp[boundary.owin]

    # G/K
    id2 <<- showNotification(paste("Running Monte Carlo Simulation for G/K Functions - ", input$statSO_confidence, "Simulations..."), duration = 0, type = "default", closeButton=FALSE)
    
    G <- envelope(loc_interest.ppp, Gest, nsim = as.integer(input$statSO_confidence))
    K <- envelope(loc_interest.ppp, Kest, nsim = as.integer(input$statSO_confidence), rank = 1, glocal = TRUE)
    
    if (!is.null(id2))
      removeNotification(id2)
    id2 <<- NULL    
    
    gplot <- ggplot(G, aes(x = r)) +
      geom_ribbon(aes(ymin = lo, ymax = hi), fill = "darkgray", outline.type = "both", lty = 11) +
      geom_line(aes(y = obs), color = "black") +
      geom_line(aes(y = theo), color = "red", lty = 11) +
      labs(x = "Distance", y = "G Function")

    kplot <- ggplot(K, aes(x = r)) +
      geom_ribbon(aes(ymin = lo, ymax = hi), fill = "darkgray", outline.type = "both", lty = 11) +
      geom_line(aes(y = obs), color = "black") +
      geom_line(aes(y = theo), color = "red", lty = 11) +
      labs(x = "Distance", y = "K Function")
    
    
    output$gplot <- renderPlotly({gplot})
    output$kplot <- renderPlotly({kplot})
    
    
    if (!is.null(id))
      removeNotification(id)
    id <<- NULL    
    
  })
  
    # Images
    output$childcare_image <- renderImage({
      list(src = "www/examplechildcare.png",
           width = "100%",
           height = 300,
           align = 'center')
    },
    deleteFile = F)
    
    output$introductionKernel <- renderImage({
      list(src = "www/NSPPAInfo.png",
           width = "100%",
           height = "auto",
           align = 'center')
    },
    deleteFile = F)
    
    output$empiricalkformula <- renderImage({
      list(src = "www/empiricalkformula.png",
           width = "auto",
           height = "50%",
           align = 'center')
    },
    deleteFile = F)
    
    
    # Home page UI
    output$projectMotivation <- renderUI(HTML("<h4> In today’s technological advancing world, there are many useful and interesting spatial data sources that exist in the forms of Geospatial and Aspatial format. Geographical Geospatial data sets the foundation based on the geographical boundary locations and the Aspatial data are the records of observation that can be further prepared to be used to derive meaningful insights. 
                                              </h4> 
                                              <h4>Despite all the data sources out on the interweb, there are not many people who are knowledgeable and trained to perform such analysis. Without the fundamental knowledge and training involved, any results based on the analysis performed could result in inaccuracies.
                          Our group attempt is to mainly focus on performing analysis and develop a website based geographical spatial tool. R Shiny tool will be used with regards to developing the 1st/2nd Order & Network Constrained Point Pattern Analysis of Melbourne City, Australia.<h4>"))
    
    output$ppancppa <- renderUI(HTML("<h4> Point Pattern Analysis methods helps provide insights about where things occur, how the distribution of incidents or the arrangement of data aligns with other features in the landscape, and what the patterns may reveal about potential connections and correlations. <h4>
                                     <h4> Network constrained Spatial Point Patterns Analysis (NetSPAA) is a collection of spatial point patterns analysis methods special developed for analysing spatial point event occurs on or alongside network. The spatial point event can be locations of childcare centre for example. The network, on the other hand can be a road network or river network. <h4>"))
    
    output$aboutus <- renderUI(HTML("<h4> Spatial Bros is created to assist non technologically savvy users in performing geographical point pattern 
                             analysis. This application aims to assist users in 2 types of analysis, particularly in performing 1st/2nd Order & Network Constrained Spatial Point 
                             Pattern Analysis. For each of the analyses, the application will provide users with statistical functions, kernel density heat map estimation, various mappings and G&K function results. 
                             The application will cover an array of spatial points located in Melbourne City such as childcare centres, business establishments, famous landmarks including places of interest such as schools, theaters, health service, 
                             sports facilities, drinking fountains and public toilets. The spatial points will work in conjunction to cover areas of the city’s road, pedestrian and tram network. From this application, users would be able to perform types 
                             of hypothesis testing that allow them to generate insights towards their conclusion on the distribution along the spatial points along the network. </h4>"))
    
    output$myList <- renderUI(HTML("<h4>A Group Project done by<h4>
                                   <ul>
                                     <li><a href='https://www.linkedin.com/in/kwek-ming-rong-9230a360/'>Kwek Ming Rong</a></li>
                                     <br>
                                     <li><a href='https://sg.linkedin.com/in/renjieteo/'>Teo Ren Jie</a></li>
                                     <br>
                                     <li><a href='https://sg.linkedin.com/in/harith-oh-393032131/'>Harith Oh Yee Choon</a></li>
                                   </ul>
                                   <br>
                                   <p> Access our user guide <a href = 'https://spatialbros.netlify.app/'>here</a></p>
                                   <br><br>
                                   <p> This project is done for IS415 Geospatial Analytics & Application under the guidance of Professor Kam Tin Seong </p>
                                   <br>
                                   <img src = 'smu.png' width = 70%, height = 70%>"))
    
    output$netKDEExpaliner <- renderUI(HTML("
                                            <h3>Network Kernel Density Estimation Map</h3>
                                            <hr>
                                            <p>Network Constrained Spatial Point Pattern Analysis analyses point pattern events that happens alongside a network. We provide several point pattern (such as Business Establishment, Drinking Fountains) and Network (such as Pedestrian or Road) options to explore the effects of spatial point patterns and densities surrounding networks. These could be used to investigate the density of point patterns along networks, such as the amount of drinking fountains along pedestrian rotues to inform the planning and installing of more drinking fountains.</p>
                                            <h3> To begin your analysis, you can start by </h3>
                                            <ol>
                                              <li>Select your choice of locality</li>
                                              <li>Select your choice of network</li>
                                              <li>Select your location of interest and subcategories/theme if required</li>
                                              <li>Select your lixel length - we recommend you to start with 500 metres</li>
                                              <li>Select your minimum lixel length - we recommend you to start with 250 metres</li>
                                              <li>Select your kernel of choice</li>
                                              <li>Select your method of choice</li>
                                              <li>Click on 'Generate KDE Map' and you are ready to go!</li>
                                            </ol>
                                            <hr>
                                            <h3>Interpeting the Results</h3>
                                            <hr>
                                            <img src='nkde_example.png' height='400'>
                                            <br>
                                            <p> A legend will be shown at the top right side of the map. The colour shade intensity of the network will get darker if there is a higher relative density of spatial points specified (location of interest).</p>
                                            <p> On contrary, if the colour shade intensity of the network is lighter, it represents a lower relative density alongside the network</p>
                                            <p> The 'NetKDE Bandwidth Selection' tells us what bandwidth has been selected by the algorithm for bandwidth range between 100 and 900, in steps of 20. The goal is to find the highest possible Cross Validation (CV) score. The larger the bandwidth, the increased amount of smoothing, hence, the CV score has been capped at 900 to reduce the amount of detail lost.
                                            <h3>Key Function FAQ</h3>
                                            <hr>
                                            <h4>Lixels</h4>
                                            <img src='nKDElixels.png' height='300'>
                                            <p>Image credit to <a href='https://jeremygelb.github.io/spNetwork/articles/NKDE.html'>spNetwork</a></p>
                                            <p>Lixels are point samples along existing network lines to calculate the density of points near the region. 'Length of Lixel' defines the typical length between such point and 'Min. Lixel Length' defines the minimum if the typical length cannot be fufilled.</p>
                                            <h4>Kernel Density Estimation Methods</h4>
                                            <p>An inforgraphic has been prepared below:</p>
                                            <img src='KernelDensity.png' height='50%'>
                                                                                  "))
    output$netStatsExplainerp1 <- renderUI(HTML("
    <h3> Statistical Functions - G & K </h3>
    <hr>
    <img src='gkpicture.png' height='300'>
    <p>The K-function is a method used in spatial Point Pattern Analysis (PPA) to inspect the spatial distribution of a set of points. It allows the user to assess if the set of points is more or less clustered that what we could expect from a given distribution. </p>
    <p> Most of the time, the set of point is compared with a random distribution.
    The empirical K-function for a specified radius r is calculated with the following formula listed <a href ='https://cran.r-project.org/web/packages/spNetwork/vignettes/KNetworkFunctions.html'> here </a> </p>
    <p> Basically, the K-function calculates for a radius r the proportion of cells with a value below r in the distance matrix between all the points Dij. In other words, the K-function estimates the average number of neighbours of a typical random point </p>
    <p> A modified version of the K-function is the G-function (Pair Correlation Function). The regular K-function is calculated for subsequent disks with increasing radii and thus is cumulative in nature. The G-function uses rings instead of disks and permits the analysis of the points concentrations at different geographical scales. </p>"))
    
    
    output$netStatsExplainerp2 <- renderUI(HTML("<h3> To begin your analysis, you can start by:</h3>
    <ol>
      <li>Select your choice of network</li>
      <li>Select your location of interest</li>
      <li>Select your start value in (metres). We will recommend you to start with 0 to begin.</li>
      <li>Select your end value in (metres). We will recommend you to end with 500 metres to begin.</li>
      <li>Select your number of simulations. We will recommend you to start with 50 simulations to begin.</li>
      <li>Select your aggregate value. We will recommend you to start with 0 to begin.</li>
      <li>Click on 'Generate Statistical Results' and you are ready to go!</li>
    </ol>
    <h3>Interpreting the Results</h3>
    <hr>
    <img src='g&kexample.png' height='600'>
    <p>Upper plot - K function, lower plot - G function, the plot is interactive, you may mouse over at various points in the graph to inspect the exact values </p>
    <p>Hypothesis:</p>
    <p>H0: The distribution of spatial points are randomly distributed</p>
    <p>H1: The distribution of spatial points are not randomly distributed</p>
    <br>
    <p> The grey area represents the function ‘envelope’. The ‘blue’ line represents the empirical function value </p>
    <p> In the event if the observed value is above the envelope, we can reject the null hypothesis (H0) as the value is statistically significant. We can conclude that the spatial points resemble a <b>clustered distribution<b?</p>
    <p> In the event if the observed value is below the envelope, we can reject the null hypothesis (H0) as the value is statistically significant. We can conclude that the spatial points resemble a <b>dispersed distribution</b> </p>
    <p> On contrary, if the observed value is inside the envelope, we cannot reject the null hypothesis (H1) as the value is not statistically significant. We can conclude the spatial points resemble a <b>random distribution</b></p>
    <p> Note: the distances relates to the distance at which the spatial points exhibits a certain pattern</p>
    <h3>Key Function FAQ</h3>
    <hr>
    <h4>Start/End</h4>
    <p>Distances for statistical analysis to be run and plotted</p>
    <h4>Number of Simulations</h4>
    <p>How many simulations to run the statistical analysis. The more simulations, the more accurate the results will be.</p>
    <h4>Aggregate Value</h4>
    <p>Points within that radius will be aggregated (in metres)</p>
    <p>o - Null (no aggregation) | >0 - Aggregation of points</p>
                                                "))
    
    output$introductiondescription <- renderUI(HTML(
      "<h3> Here are a few easy steps to start your analysis!</h3>
    <ol>
      <li>Select your choice of network</li>
      <li>Select your location of interest</li>
      <li>Select your start value in (metres). We will recommend you to start with 0 to begin.</li>
      <li>Select your end value in (metres). We will recommend you to end with 500 metres to begin.</li>
      <li>Select your number of simulations. We will recommend you to start with 50 simulations to begin.</li>
      <li>Select your aggregate value. We will recommend you to start with 0 to begin.</li>
      <li>Click on 'Generate Statistical Results' and you are ready to go!</li>
    </ol>
    <h3>Interpreting the Results</h3>
    <hr>
    <img src='g&kexample.png' height='600' width='100%'>
    <p>Upper plot - K function, lower plot - G function, the plot is interactive, you may mouse over at various points in the graph to inspect the exact values </p>
    <p>Hypothesis:</p>
    <p>H0: The distribution of spatial points are randomly distributed</p>
    <p>H1: The distribution of spatial points are not randomly distributed</p>
    <br>
    <p> The grey area represents the function ‘envelope’. The ‘blue’ line represents the empirical function value </p>
    <p> In the event if the observed value is above the envelope, we can reject the null hypothesis (H0) as the value is statistically significant. We can conclude that the spatial points resemble a <b>clustered distribution<b?</p>
    <p> In the event if the observed value is below the envelope, we can reject the null hypothesis (H0) as the value is statistically significant. We can conclude that the spatial points resemble a <b>dispersed distribution</b> </p>
    <p> On contrary, if the observed value is inside the envelope, we cannot reject the null hypothesis (H1) as the value is not statistically significant. We can conclude the spatial points resemble a <b>random distribution</b></p>
    <p> Note: the distances relates to the distance at which the spatial points exhibits a certain pattern</p>
    <h3>Key Function FAQ</h3>
    <hr>
    <h4>Start/End</h4>
    <p>Distances for statistical analysis to be run and plotted</p>
    <h4>Number of Simulations</h4>
    <p>How many simulations to run the statistical analysis. The more simulations, the more accurate the results will be.</p>
    <h4>Aggregate Value</h4>
    <p>Points within that radius will be aggregated (in metres)</p>
    <p>o - Null (no aggregation) | >0 - Aggregation of points</p>
                                                "))    
    
    output$IntroData <- renderUI(HTML(
      "<h3> Welcome to Data Exploration</h3>
      <hr>
      <p> Here, you can explore the network and spatial point datasets in the City of Melbourne included in the interaction application.</p>
      <h3>Included datasets</h3>
      <hr>
      <h4>Network</h4>
      <ul>
        <li>Road Network - City of Melbourne</li>
        <li>Pedestrian Network - City of Melbourne</li>
        <li>Tram Network - City of Melbourne</li>
      </ul>
      <h4>Spatial Points</h4>
      <ul>
        <li>Childcare Centres</li>
        <li>Business Establishments - Sub-categories by Industry Name</li>  
        <li>Drinking Fountains</li>
        <li>Landmarks - Themes</li>
        <li>Public Toilets</li>
      </ul>
      <h3> To start off:</h3>
      <hr>
    <ol>
      <li>Select the Network / Spatial Points tab to explore network or spatial points</li>
      <li>Select the localities</li>
      <li>Select the location of interest and any specific themes or sub-categories if necessary</li>
      <li>Click on 'Generate Map and Data Table' and you are ready to go!</li>
    </ol>
                                                "))    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
