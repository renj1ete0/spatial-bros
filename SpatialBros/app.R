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
                          h4("In today’s technological advancing world, there are many useful and interesting spatial data sources that exist in the forms of Geospatial and Aspatial format. Geographical Geospatial data sets the foundation based on the geographical boundary locations and the Aspatial data are the records of observation that can be further prepared to be used to derive meaningful insights."),
                          h4("Despite all the data sources out on the interweb, there are not many people who are knowledgeable and trained to perform such analysis. Without the fundamental knowledge and training involved, any results based on the analysis performed could result in inaccuracies.
                          Our group attempt is to mainly focus on performing analysis and develop a website based geographical spatial tool. R Shiny tool will be used with regards to developing the 1st/2nd Order & Network Constrained Point Pattern Analysis of Melbourne City, Australia.")),
                   column(4,
                          h2("Road network with childcare centres in Punggol, Singapore"),
                          hr(),
                          imageOutput("childcare_image",)
                   ),
                   column(4,
                          h1("Spatial Bros"),
                          uiOutput("myList"),
                   )),
                 fluidRow(
                   column(8,
                          h2("What is Point Pattern Analysis and Network Constrained Point Pattern Analysis?"),
                          hr(),
                          h4("Point Pattern Analysis methods helps provide insights about where things occur, how the distribution of incidents or the arrangement of data aligns with other features in the landscape, and what the patterns may reveal about potential connections and correlations."),
                          h4("Network constrained Spatial Point Patterns Analysis (NetSPAA) is a collection of spatial point patterns analysis methods special developed for analysing spatial point event occurs on or alongside network. The spatial point event can be locations of childcare centre for example. The network, on the other hand can be a road network or river network.")),
                   ),
                 fluidRow(
                   column(8,
                          h2("About Spatial Bros"),
                          hr(),
                          h4("Spatial Bros is created to assist non technologically savvy users in performing geographical point pattern 
                             analysis. This application aims to assist users in 2 types of analysis, particularly in performing 1st/2nd Order & Network Constrained Spatial Point 
                             Pattern Analysis. For each of the analyses, the application will provide users with statistical functions, kernel density heat map estimation, various mappings and G&K function results. 
                             The application will cover an array of spatial points located in Melbourne City such as childcare centres, business establishments, famous landmarks including places of interest such as schools, theaters, health service, 
                             sports facilities, drinking fountains and public toilets. The spatial points will work in conjunction to cover areas of the city’s road, pedestrian and tram network. From this application, users would be able to perform types 
                             of hypothesis testing that allow them to generate insights towards their conclusion on the distribution along the spatial points along the network."),
                          ),
                   )
                 ),
        tabPanel("Spatial Point Pattern Analysis"),
        tabPanel("Network Constrained Point Pattern Analysis"))
      )          


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$childcare_image <- renderImage({
      list(src = "www/examplechildcare.png",
           wdith = "100%",
           height = 300,
           align = 'center')
    },
    deleteFile = F)
    
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
    
                                    
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white',
    #          xlab = 'Waiting time to next eruption (in mins)',
    #          main = 'Histogram of waiting times')
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
