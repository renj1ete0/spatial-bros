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
        "Spatial Bros",
        tags$head(tags$style(
          type="text/css",
          "#childcare_image img {max-width: 100%; width: 100%; height: auto}",
          ".navbar {background-color: rgb(48, 48, 48)}",
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
        ),
        
        tabPanel("Spatial Point Pattern Analysis",
                 titlePanel("Kernel Density Estimation"),
                 ),
        
        # 3rd Tab
        tabPanel("Network Constrained Point Pattern Analysis",
              titlePanel("Network Constrained Point Pattern Analysis"),
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
                        tmapOutput("mapPlot", width = "100%", height = "780")
                      ),
                      sidebarPanel(
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
                        selectInput(inputId = "location_of_interest",
                                    label = "Location of Interest",
                                    choices = list("Childcare Centres" = "sf_childcare",
                                                   "Business Establishments" = "sf_business",
                                                   "Drinking Fountain" = "sf_drinking_fountain",
                                                   "Landmarks" = "sf_landmarks",
                                                   "Public Toilets" = "sf_pub_toilets")),
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
                  ),
                tabPanel("Statistical Functions", 
                         sidebarLayout(
                           mainPanel(
                             plotlyOutput("kfun"),
                             plotlyOutput("gfun"),
                           ),
                           sidebarPanel(
                             h4("Statistical Function Variable Inputs"),
                             h5("Network and Spatial Points"),
                             selectInput(inputId = "network_type",
                                         label = "Types of Network",
                                         choices = list("Road Network" = "net_road",
                                                        "Pedestrian Network" = "net_ped",
                                                        "Tram Network" = "net_tram")),
                             selectInput(inputId = "locs",
                                         label = "Location of Interest",
                                         choices = list("Childcare Centres" = "sf_childcare",
                                                        "Business Establishments" = "sf_business",
                                                        "Drinking Fountain" = "sf_drinking_fountain",
                                                        "Landmarks" = "sf_landmarks",
                                                        "Public Toilets" = "sf_pub_toilets")),
                             h5("Other Variables"),
                             sliderInput(inputId = "net_start", "Start",
                                         min = 0, max = 2000, value = 350, step = 50),
                             sliderInput(inputId = "net_end", "End",
                                         min = 100, max = 2000, value = 500, step = 50),
                             sliderInput(inputId = "n_sims", "Number of Simulations",
                                         min = 10, max = 300, value = 50, step = 5),
                             sliderInput(inputId = "agg", "Aggregate Value",
                                       min = 0, max = 1000, value = 0, step = 50),
                             
                             actionButton("netKDEGenerateStats", "Generate Statistical Results"),
                             
                             h5("Please note: The graphs will take a few minutes to generate."),
                             
                           ),
                         ),
                         uiOutput("netStatsExplainer")
                    ),
                 ),
                 

        )
      )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  id <- NULL
  
  output$mapPlot <- renderTmap({
    tm_shape(melb_localities) +
      tm_polygons("LOC_NAME", alpha=0.1) +
    tm_shape(melb_lga) +
      tm_borders(lwd = 2, lty = 5,  col="blue") 
  })
  
  observeEvent(input$netKDEGenerateStats, {
    id <<- showNotification(paste("Calculating Statisical Results..."), duration = 0, type = "message", closeButton=FALSE)
    
    if (input$agg){
      agg <- NULL
    }
    else{
      agg <- input$agg
    }

    kfun_output <- kfunctions(get(input$network_type),
                              get(input$locs),
                               start = input$net_start, 
                               end = input$net_end, 
                               step = 50, 
                               width = 50, 
                               nsim = input$n_sims, 
                               resolution = 50,
                               verbose = FALSE, 
                               conf_int = 0.05)
    
    output$kfun <- renderPlotly({kfun_output$plotk})
    output$gfun <- renderPlotly({kfun_output$plotg})
    
    if (!is.null(id))
      removeNotification(id)
    id <<- NULL    
  })
  
  observeEvent(input$netKDEGenerate, {
    id <<- showNotification(paste("Generating KDE Map..."), duration = 0, type = "message", closeButton=FALSE)
    if (input$localities == "Entire City of Melbourne"){
      localities <- melb_localities
      boundary <- melb_lga
      network_type <- get(input$network_type)
      loc_interest <- get(input$locs)
    }
    else {
      localities <- melb_localities %>% filter(LOC_NAME == input$localities)
      boundary <- localities
      loc_interest <- st_intersection(get(input$locs), boundary)
      network_type <- st_intersection(get(input$network_type), boundary) %>% st_cast("LINESTRING")
      
    }
    
    road_lixels_cc <- lixelize_lines(network_type, input$lx_length, mindist = input$lx_length_min)
    road_samples_cc <- lines_center(road_lixels_cc)
    road_network_cc_densities <- nkde(net_road,
                                      events = loc_interest,
                                      w = rep(1,nrow(loc_interest)), 
                                      samples = road_samples_cc, 
                                      kernel_name =  noquote(input$kernel_name),
                                      bw = 300, 
                                      div= "bw", 
                                      method = noquote(input$method_name), 
                                      digits = 3, 
                                      tol = 1,
                                      grid_shape = c(1,1), 
                                      max_depth = 8,
                                      agg = agg,
                                      sparse = TRUE,
                                      verbose = FALSE)
    road_samples_cc$density <- road_network_cc_densities * 1000
    road_lixels_cc$density <- road_network_cc_densities * 1000
    output$mapPlot <- renderTmap({
      tm_shape(localities) +
        tm_polygons("LOC_NAME", alpha=0.1) +
        tm_shape(boundary) +
        tm_borders(lwd = 2.5, lty = 5,  col="blue") +
        tm_shape(road_lixels_cc) +
        tm_lines(lwd = 1.5, col = "density")+
        tm_shape(loc_interest)+ 
        tm_dots(size = 0.03, alpha = 0.6) 
    })
    
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
      list(src = "www/KernelDensity.png",
           width = "auto",
           height = "auto",
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
                                   <img src = 'smu.png' width = 50%, height = 50%>"))
    
    output$netKDEExpaliner <- renderUI(HTML("
                                            <h3> How to use? </h3>
                                            <ol>
                                              <li>Select your choice of network</li>
                                              <li>Select your location of interest</li>
                                              <li>Select your kernel of choice</li>
                                              <li>Select your method of choice</li>
                                            </ol>
                                            <h3> How do we understand the map output? </h3>
                                            <p> A legend will be shown at the top left of the map. For the location of interest spatial points selected, the colour shade intensity of the network will get darker if there is a higher relative density. On contrary, if the colour shade intensity of the network is lighter, it represents a lower relative density alongside the network</p>

                                                                                  "))
    output$netStatsExplainer <- renderUI(HTML("
    <h2> Welcome to the Network Constrained G&K Function Page </h2>
    The K-function is a method used in spatial Point Pattern Analysis (PPA) to inspect the spatial distribution of a set of points. It allows you the user to assess if the set of points is more or less clustered that what we could expect from a given distribution. Most of the time, the set of point is compared with a random distribution.
    <h3> How to use? </h3>
    <ol>
      <li>Select your choice of network</li>
      <li>Select your location of interest</li>
      <li> Enter number of simulations</li>
    </ol>
    <h3> How do we understand the function output? </h3>
    <p> The grey area represents the function ‘envelope’. The ‘blue’ line represents the empirical function value </p>
    <p> In the event if the observed value is above the envelope, we can reject the null hypothesis as the value is statistically significant. We can conclude that the spatial points resemble a clustered distribution</p>
    <p> In the event if the observed value is outside the envelope, we can reject the null hypothesis as the value is statistically significant. We can conclude that the spatial points resemble a dispersed distribution. </p>
    <p> On contrary, if the observed value is inside the envelope, we cannot reject the null hypothesis as the value is not statistically significant. We can conclude the spatial points resemble a random distribution. </p>

                                                                                  "))
    
    output$introductiondescription <- renderUI(HTML(
    "<h4> You will be able to perform network constrained spatial point patterns analysis methods special developed for analysing spatial point event occurs on or alongside network for City of Melbourne, Australia! </h4>
    <h4> There are 2 types of analysis that you can perform
      <ol> 
        <li> Network Kernel Density Estimation </li>
        <li> G & K Function Analysis </li>
      </ol>
    </h4>
    <h4> For each of the analysis, we offer you the options of selecting 
      <ol> 
        <li> Road Network </li>
        <li> Pedestrian Network </li>
        <li> Tram Network </li>
      </ol>
    </h4>
    <h4> In addition you are allowed to pick your location of interest such as
      <ol> 
        <li> Childcare Centres </li>
        <li> Business Establishments </li>
        <li> Drinking Fountains </li>
        <li> Landmarks </li>
        <li> Public Toilets </li>
      </ol>
    </h4>
    <h2> Benefits of performing Network Constrained Point Pattern Analysis </h2>
    <hr>
    <h4> 
        <ol> 
          <li> Accurate analysis: Network Constrained Point Pattern Analysis provides more accurate results compared to traditional point pattern analysis because it accounts for the underlying transportation network. This is particularly important in areas where the transportation network is dense and complex. </li> <br>
          <li> Better decision-making: Network Constrained Point Pattern Analysis can provide insights into how the network infrastructure affects the spatial distribution of points, which can be valuable for decision-making related to urban planning, transportation planning, and public policy </li> <br>
          <li> Improved resource allocation: Network Constrained Point Pattern Analysis can help optimize the allocation of resources, such as improving the accessibility to more drinking fountains/public toilets, by identifying areas with high concentrations of points and areas that are more accessible by the transportation network. </li>
      </ol>
    </h4>"))
    
}

# Run the application 
shinyApp(ui = ui, server = server)
