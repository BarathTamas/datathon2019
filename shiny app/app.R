
# packages ------------------------------------------------------------------------------------

library(tidyverse)
library(tidytext)
library(shiny)
library(shinydashboard)
library(shinythemes)
# library(shinytest)
library(DT)
# library(ggiraph)
# library(shinyjqui)
library(shinyWidgets)
library(shinycssloaders)
# library(colourpicker)
# library(formattable)
library(Cairo)
library(RColorBrewer)
library(pheatmap)
library(ggwordcloud)

# shiny dashboard -----------------------------------------------------------------------------

options(shiny.usecairo = TRUE)

# input data ----------------------------------------------------------------------------------

# dashboard 1

words_ts <- read.table("words_ts.csv", header = TRUE, stringsAsFactors = FALSE) %>% 
  mutate(date = as.POSIXct(date)) #date column as POSIXct from the start

trending_words <- read.table("trending words.csv", header = TRUE, stringsAsFactors = FALSE)

word_corrs <- read.table("word_correlations.csv", header = TRUE, stringsAsFactors = FALSE)

session_info <- as.data.frame(data.table::fread("session_info.csv"))

date_country_word <- as.data.frame(data.table::fread("date_country_word.csv"))

# session_info <- read.table("session_info.csv", header = TRUE, stringsAsFactors = FALSE)
# 
# date_country_word <- read.table("date_country_word.csv", header = TRUE, stringsAsFactors = FALSE)

# dashboard 2

sec_counc_count <- read.csv("securitycouncil_count.csv", header = TRUE, stringsAsFactors = FALSE)

sec_counc_words <- read.csv("securitycouncil_words.csv", header = TRUE, stringsAsFactors = FALSE)


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
                       options = pickerOptions(
                         actionsBox = T,
                         size = 10,
                         selectedTextFormat = "count > 3",
                         maxOptions = 10
                       ),
                       multiple = TRUE
                     )
    ),
    
    #### sliders DB 2 ####
    
    conditionalPanel(condition = "input.sidebar=='dashboard2'",
                     
                     selectInput(inputId = "country",
                                 label = "Select Country:",
                                 choices = unique(sec_counc_count$country),
                                 selected = sec_counc_count$country[1],
                                 selectize = TRUE),
                     
                     sliderInput(inputId = "year",
                                 label = "Select Year:",
                                 min = 1970,
                                 max = 2015,
                                 value = 2015,
                                 step = 1,
                                 sep = ""),
                     
                     knobInput(inputId = "maxWordsCloud",
                               label = "Select Number of Words in Cloud:",
                               value = 10,
                               min = 3,
                               max = 20,
                               displayPrevious = TRUE, 
                               lineCap = "round",
                               fgColor = "#428BCA",
                               inputColor = "#428BCA"
                     )
                     
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
                
                box(title = "Heatmap Displaying Word Correlations",
                    status = "info",
                    width = 5,
                    height = "550px",
                    solidHeader = TRUE,
                    
                    
                    withSpinner(plotOutput("corrPlot"))
                    
                ),
                
                box(title = "Frequency of Selected Words Over Time (click to receive information)",
                    status = "info",
                    width = 7,
                    height = "550px",
                    solidHeader = TRUE,
                    
                    withSpinner(plotOutput("wordsOverTime", click = "plot1_click"))
                )
              ),
              
              fluidRow(
                
                box(title = "Information about selected session",
                    status = "info",
                    width = 7,
                    solidHeader = TRUE,
                    
                    withSpinner(dataTableOutput("click_info"))
                    
                ),
                
                box(title = "Frequency by Country",
                    status = "info",
                    width = 5,
                    solidHeader = TRUE,
                    
                    withSpinner(dataTableOutput("click_info2"))
                    
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
                    solidHeader = TRUE,
                    
                    withSpinner(plotOutput("linePlot", click = "plot2_click"))
                    
                ),
                
                # box(title = "Bar Plot",
                #     status = "info",
                #     width = 3,
                #     solidHeader = TRUE,
                #     
                #     withSpinner(plotOutput("barPlot"))
                #     
                # ),
                
                box(title = "Wordcloud",
                    status = "info",
                    width = 6,
                    solidHeader = TRUE,
                    
                    withSpinner(plotOutput("wordcloud"))
                    
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
      filter(date == as.POSIXct(nearPoints(filteredWords(), input$plot1_click)[1,"date"]))
    
  }, options = list(searching = FALSE, paging = FALSE))
  
  output$click_info2 <- renderDataTable({
    
    date_country_word %>%
      filter(date == as.POSIXct(nearPoints(filteredWords(), input$plot1_click)[1,"date"])) %>% 
      filter(word == nearPoints(filteredWords(), input$plot1_click)[1,"word"]) %>% 
      top_n(5) %>%
      select(country,n) %>% 
      arrange(desc(n))
    
    
  }, options = list(searching = FALSE, paging = FALSE))  
  
  #### rendered objects DB 2 ####
  
  # sentiment index
  
  output$testInfo1 <- renderValueBox({
    valueBox(
      subtitle = paste0("is the Sentiment Index in ", input$year), 
      value = "Der Gerät wird nie müde", 
      icon = icon("heartbeat", lib = "font-awesome"),
      color = "navy"
    )
  })
  
  # mentiod the most positively by
  
  output$testInfo2 <- renderValueBox({
    valueBox(
      subtitle = paste0("Mentioned ", input$country," the most positively in ", input$year), 
      value = "Der Gerät schläft nie ein",
      icon = icon("thumbs-up", lib = "glyphicon"),
      color = "blue"
    )
  })
  
  # mentiod the most negatively by
  
  output$testInfo3 <- renderValueBox({
    valueBox(
      subtitle = paste0("Mentioned ",  input$country," the most negatively"), 
      value = "und ist immer vor chef in geschäft",
      icon = icon("thumbs-down", lib = "glyphicon"),
      color = "light-blue"
    )
  })
  
  #### line plot ####
  
  sec_counc_count_R <- reactive({sec_counc_count %>% 
      filter(country == input$country)
  })
  
  output$linePlot <- renderPlot({
    ggplot(data = sec_counc_count_R(), aes(year, count)) + 
      geom_line() +
      xlab("Year") +
      ylab("Count") +
      theme_bw() +
      theme(legend.position = "bottom",
            panel.grid.minor.y = element_blank())
  })
  
  # # barplot
  # 
  # sec_counc_words_R1 <- reactive({
  #   sec_counc_words %>%
  #     filter(country == input$country) %>% 
  #     filter(year == input$year) %>%
  #     top_n(10, tf) %>%
  #     mutate(word = reorder(word, tf))
  # })
  # 
  # output$barPlot <- renderPlot({
  #   ggplot(data = sec_counc_words_R1(), aes(word, tf)) +
  #     geom_col(show.legend = FALSE) +
  #     coord_flip() +
  #     xlab("Count") +
  #     ylab("Word") +
  #     theme_bw() +
  #     theme(legend.position = "bottom",
  #           panel.grid.minor.y = element_blank())
  # })
  
  #### wordcloud #####
  
  sec_counc_words_R2 <- reactive({sec_counc_words %>%
      mutate(angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 1, 4, 1, 1))) %>% 
      filter(country == input$country, year == floor(input$year)) %>%
      top_n(n = input$maxWordsCloud, wt = freqword)
  })
  
  output$wordcloud <- renderPlot({
    
    ggplot(sec_counc_words_R2(), aes(label = word, 
                                     size = freqword,
                                     color = freqword,
                                     angle = angle,
                                     replace = TRUE),
           rm_outside = TRUE) +
      scale_color_gradient(low = "darkred", high = "darkblue") +
      scale_size_area(max_size = 20) +
      geom_text_wordcloud(shape = "circle") +
      theme_minimal()
    
  })
}

# app -----------------------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
