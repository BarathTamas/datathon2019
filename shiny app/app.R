
# packages ------------------------------------------------------------------------------------

library(tidyverse)
library(tidytext)
library(shiny)
library(shinydashboard)
library(shinythemes)
# library(shinytest)
# library(DT)
# library(wordcloud)
# library(ggiraph)
# library(shinyjqui)
library(shinyWidgets)
library(shinycssloaders)
# library(colourpicker)
library(formattable)
library(Cairo)
library(RColorBrewer)
library(pheatmap)

# shiny dashboard -----------------------------------------------------------------------------

options(shiny.usecairo = TRUE)

# input data ----------------------------------------------------------------------------------

words_ts <- read.table("words_ts.csv", header = TRUE, stringsAsFactors = FALSE) %>% mutate(date=as.POSIXct(date)) #date column as POSIXct from the start

trending_words <- read.table("trending words.csv", header = TRUE, stringsAsFactors = FALSE)

word_corrs <- read.table("word_correlations.csv", header = TRUE, stringsAsFactors = FALSE)

session_info <- read.table("session_info.csv", header = TRUE, stringsAsFactors = FALSE)

# ui ------------------------------------------------------------------------------------------

ui <- dashboardPage(
  
  skin = "blue",
  
  #### header ####
  
  dashboardHeader(
    
    title = "UN Debate Web App"
    
  ),
  
  #### sidebar ####
  
  dashboardSidebar(
    
    sidebarMenu(id = "sidebar",
                
                menuItem("Trending Words:", tabName = "dashboard", icon = icon("chart-line", lib = "font-awesome")),
                menuItem("Dashboard 2:", tabName = "dashboard2", icon = icon("chart-line", lib = "font-awesome")),
                menuItem("Dashboard 3:", tabName = "dashboard3", icon = icon("chart-line", lib = "font-awesome")),
                menuItem("About us:", tabName = "aboutus", icon = icon("question", lib = "font-awesome"))
                
    ),
    
    hr(),
    
    #### sliders DB 1 ####
    
    conditionalPanel(condition = "input.sidebar=='dashboard'",
                     
                     pickerInput(
                       inputId = "selectedWords",
                       label = "Which words should be plotted?",
                       choices = trending_words$word,
                       selected = c("arab","israel","arms","middle","force,"),
                       options = list(
                         `actions-box` = TRUE,
                         size = 10,
                         `selected-text-format` = "count > 3"
                       ),
                       multiple = TRUE
                     )
    ),
    
    #### sliders DB 2 ####
    
    conditionalPanel(condition = "input.sidebar=='dashboard2'"
                     
                     
                     
    ),
    
    #### sliders DB 3 ####
    
    conditionalPanel(condition = "input.sidebar=='dashboard3'"
                     
    )
    
  ),
  
  #### body ####
  
  dashboardBody(
    
    #### css layout ####
    
    tags$head(tags$style(HTML('
                              /* logo */
                              .skin-blue .main-header .logo {
                              background-color: #5b92e5;
                              }
                              
                              /* logo when hovered */
                              .skin-blue .main-header .logo:hover {
                              background-color: #5b92e5;
                              }
                              
                              /* navbar (rest of the header) */
                              .skin-blue .main-header .navbar {
                              background-color: #5b92e5;
                              }
                              
                              /* main sidebar */
                              .skin-blue .main-sidebar {
                              background-color: #ffffff;
                              }
                              
                              /* active selected tab in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #5b92e5;
                              color: #ffffff;
                              }
                              
                              /* other links in the sidebarmenu */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #ffffff;
                              color: #000000;
                              }
                              
                              /* other links in the sidebarmenu when hovered */
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #0A366B;
                              color: #ffffff;
                              }
                              /* toggle button when hovered  */
                              .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #0A366B;
                              }
                              .box.box-solid.box-primary>.box-header {
                              color:#fff;
                              }
                              '))),
    
    tabItems(
      
      #### content DB 1 ####
      
      tabItem(tabName = "dashboard",
              
              fluidRow(
                
                box(title = "Correlation Heatmap",
                    status = "info",
                    width = 5,
                    height = "550px",
                    solidHeader = TRUE,
                    
                    
                    withSpinner(plotOutput("corrPlot"))
                    
                ),
                
                box(title = "Words over Time",
                    status = "info",
                    width = 7,
                    height = "550px",
                    solidHeader = TRUE,
                    
                    withSpinner(plotOutput("wordsOverTime", click = "plot1_click"))
                )
              ),
              
              fluidRow(
                
                box(title = "Information Table",
                    status = "info",
                    width = 7,
                    solidHeader = TRUE,
                    
                    withSpinner(dataTableOutput("click_info"))
                    
                ),
                
                box(title = "Text",
                    status = "info",
                    width = 5,
                    solidHeader = TRUE
                    
                    # dataTableOutput("click_info")
                    
                )
                
              )
      ),
      
      #### content DB 2 ####
      
      tabItem(tabName = "dashboard2",
              
              fluidRow(
                
                valueBoxOutput("testInfo1"),
                withSpinner(valueBoxOutput("testInfo2")),
                valueBoxOutput("testInfo3")
                
              ),
              
              fluidRow(
                
                box(title = "Line Plot",
                    status = "info",
                    width = 6,
                    solidHeader = TRUE
                    
                    
                    
                ),
                
                box(title = "Bar Plot",
                    status = "info",
                    width = 3,
                    solidHeader = TRUE
                    
                    
                    
                ),
                
                box(title = "Wordcloud",
                    status = "info",
                    width = 3,
                    solidHeader = TRUE
                    
                    
                    
                )
              )
              
      ),
      
      
      #### about us ####
      
      
      tabItem(tabName = "aboutus",
              
              includeMarkdown("aboutUs.Rmd")
              
      )
    )
    )
    )


# server --------------------------------------------------------------------------------------

server <- function(input, output) {
  
  #### rendered objects DB 1 ####
  
  filteredWords <- reactive({
    words_ts %>% 
      filter(word %in% input$selectedWords)
  })
  
  output$wordsOverTime <- renderPlot({
    ggplot(data = filteredWords(), aes(date, n, color = word)) +
      geom_line(size = 1.3) +
      geom_point(size = 5, alpha = 0.5) +
      scale_x_datetime(breaks = seq(as.POSIXct("1970-01-01"),
                                    as.POSIXct("2015-12-31"), "4 years"),
                       minor_breaks = seq(as.POSIXct("1970-01-01"),
                                          as.POSIXct("2015-12-31"), "1 years"),
                       expand = c(0, 0),
                       limits = c(as.POSIXct("1969-12-01"),
                                  as.POSIXct("2016-01-31"))) +
      scale_color_discrete(name = "Selected Words:") +
      xlab("Year") +
      ylab("Frequency") +
      theme_bw() +
      theme(legend.position = "bottom",
            panel.grid.minor.y = element_blank())
  }, height = 490, width = 735)
  
  output$corrPlot <- renderPlot({
    
    pal <- colorRampPalette(rev(brewer.pal(11, "RdYlBu")))(100)
    pheatmap(word_corrs, color = pal, treeheight_row = 0, treeheight_col = 0)
    
  }, height = 490, width = 520)
  
  output$click_info <- renderDataTable({
    
    session_info %>%
      filter(date==as.POSIXct(nearPoints(filteredWords(), input$plot1_click)[1,"date"]))
    
  }, options = list(searching = FALSE, paging = FALSE))
  
  #### rendered objects DB 2 ####
  
  output$testInfo1 <- renderValueBox({
    valueBox(
      subtitle = "Sentiment Index", 
      value = "Der Gerät wird nie müde", 
      icon = icon("thumbs-up", lib = "glyphicon"),
      color = "navy"
    )
  })
  
  output$testInfo2 <- renderValueBox({
    valueBox(
      subtitle = "Mentiod the most positively by:", 
      value = "Der Gerät schläft nie ein", 
      icon = icon("thumbs-up", lib = "glyphicon"),
      color = "blue"
    )
  })
  
  output$testInfo3 <- renderValueBox({
    valueBox(
      subtitle = "Mentiod the most negatively by:", 
      value = "und ist immer vor chef in geschäft", 
      icon = icon("thumbs-up", lib = "glyphicon"),
      color = "light-blue"
    )
  })
  
}

# app -----------------------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
