# This is a Shiny web application created by SpatialBros.  
# You can run the application by clicking 
# the 'Run App' button above.



library(shiny)
library(shinythemes)

# Define UI for application
ui <- fluidPage(theme = shinytheme("united"),
      navbarPage(
        "Spatial Bros",
        tabPanel("Home Page",
                 fluidRow(
                   column(4,
                          h2("Project Motivation"),
                          hr(),
                          h4("The Spatial Bros is created to assist non technologically savvy users in performing geographical point pattern analysis. 
                             This application aims to assist users in 2 types of analysis, particularly in performing 1st/2nd Order & Network Constrained Spatial Point 
                             Pattern Analysis. For each of the analyses, the application will provide users with statistical functions, kernel density heat map estimation, various mappings and G&K function results. The application will cover an array of spatial points located in Melbourne City such as childcare centres, business establishments, 
                             famous landmarks including places of interest such as schools, theaters, health service, sports facilities, drinking fountains and public toilets. The spatial points will work in conjunction to cover areas of the city’s road, pedestrian and tram network. From this application, 
                             users would be able to perform types of hypothesis testing that allow them to generate insights towards their conclusion on the distribution along the spatial points along the network.")),
                 ),
                 fluidRow(
                   column(4,
                          h2("What is Point Pattern Analysis and Network Constrained Point Pattern Analysis?"),
                          hr(),
                          h4("Point Pattern Analysis methods helps provide insights about where things occur, how the distribution of incidents or the arrangement of data aligns with other features in the landscape, and what the patterns may reveal about potential connections and correlations."),
                          h4("Network constrained Spatial Point Patterns Analysis (NetSPAA) is a collection of spatial point patterns analysis methods special developed for analysing spatial point event occurs on or alongside network. The spatial point event can be locations of childcare centre for example. The network, on the other hand can be a road network or river network.")),
                   )),
        tabPanel("Spatial Point Pattern Analysis"),
        tabPanel("Network Constrained Point Pattern Analysis"))
      )          

#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of binss:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
