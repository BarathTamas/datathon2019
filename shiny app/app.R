
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
library(highcharter)

# shiny dashboard -----------------------------------------------------------------------------

options(shiny.usecairo = TRUE)

# input data ----------------------------------------------------------------------------------

# dashboard 1

words_ts <- read.table("words_ts.csv", header = TRUE, stringsAsFactors = FALSE) %>% 
  mutate(date = as.POSIXct(date)) #date column as POSIXct from the start

trending_words <- read.table("trending words.csv", header = TRUE, stringsAsFactors = FALSE)

word_corrs <- read.table("word_correlations.csv", header = TRUE, stringsAsFactors = FALSE)

# session_info <- as.data.frame(data.table::fread("session_info.csv"))
# 
# date_country_word <- as.data.frame(data.table::fread("date_country_word.csv"))

session_info <- read.table("session_info.csv", header = TRUE, stringsAsFactors = FALSE)

date_country_word <- read.table("date_country_word.csv", header = TRUE, stringsAsFactors = FALSE)

country_codes <- read.table("country_codes.csv", header = TRUE, stringsAsFactors = FALSE)

# dashboard 2

sec_counc_count <- read.csv("securitycouncil_count.csv", header = TRUE, stringsAsFactors = FALSE)

sec_counc_words <- read.csv("securitycouncil_words.csv", header = TRUE, stringsAsFactors = FALSE)

sentiment_data <- read.csv("china_sentiment_wrong.csv", header = TRUE, stringsAsFactors = FALSE)

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
                       label = HTML('<p style="color:black;">Select Words to Plot:</p>'),
                       choices = trending_words$word,
                       selected = c("arab","israel","arms","middle","force,"),
                       options = pickerOptions(
                         actionsBox = T,
                         size = 10,
                         selectedTextFormat = "count > 3",
                         maxOptions = 10
                       ),
                       multiple = TRUE
                     ),
                     actionButton("refreshWords","Update Plot")
    ),
    
    #### sliders DB 2 ####
    
    conditionalPanel(condition = "input.sidebar=='dashboard2'",
                     
                     selectInput(inputId = "country",
                                 label = HTML('<p style="color:black;">Select Country:</p>'),
                                 choices = unique(sec_counc_count$country),
                                 selected = sec_counc_count$country[1],
                                 selectize = TRUE),
                     
                     sliderInput(inputId = "year",
                                 label = HTML('<p style="color:black;">Select Year:</p>'),
                                 min = 1970,
                                 max = 2015,
                                 value = 2015,
                                 step = 1,
                                 sep = ""),
                     
                     knobInput(inputId = "maxWordsCloud",
                               label = HTML('<p style="color:black;">Select Number of Words in Cloud:</p>'),
                               value = 20,
                               min = 3,
                               max = 50,
                               displayPrevious = TRUE, 
                               lineCap = "round",
                               fgColor = "#5b92e5",
                               inputColor = "#5b92e5"
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

                              
                              
                              
                              .box.box-solid.box-primary>.box-header {
                              color:#fff;
                              background:#5b92e5
                              }
                              
                              .box.box-solid.box-primary{
                              border-bottom-color:#5b92e5;
                              border-left-color:#5b92e5;
                              border-right-color:#5b92e5;
                              border-top-color:#5b92e5;
                              }
                              '))),
    
    tabItems(
      
      #### content DB 1 ####
      
      tabItem(tabName = "dashboard",
              
              fluidRow(
                
                box(title = "Heatmap Displaying Clusters of Correlated Word Trends",
                    status = "primary",
                    width = 5,
                    height = "520px",
                    solidHeader = TRUE,
                    
                    withSpinner(plotOutput("corrPlot"))
                    
                ),
                
                box(title = "Frequency of Selected Words Over Time (click points for more information)",
                    status = "primary",
                    width = 7,
                    height = "520px",
                    solidHeader = TRUE,
                    
                    withSpinner(plotOutput("wordsOverTime", click = "plot1_click"))
                    
                )
              ),
              
              fluidRow(
                
                box(title = "Information About Selected Session",
                    status = "primary",
                    width = 7,
                    solidHeader = TRUE,
                    
                    withSpinner(tableOutput("click_info"))
                    
                )
              ),
              
              fluidRow(
                  
                box(title = "Frequency by Country (Top Users)",
                    status = "primary",
                    width = 3,
                    solidHeader = TRUE,
                    
                    withSpinner(tableOutput("click_info2"))
                    
                ),
                
                box(title = "Find in Text",
                    status = "primary",
                    width = 3,
                    solidHeader = TRUE,
                    withSpinner(tableOutput("click_info3")),
                    uiOutput('countries'),
                    actionButton(
                      "lookup", "Lookup"
                    )
                    
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
                    status = "primary",
                    width = 6,
                    solidHeader = TRUE,
                    
                    withSpinner(highchartOutput("linePlot")) #, click = "plot2_click"))
                    
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
                    status = "primary",
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
  
  #### rend. obj. DB 1 ####
  global <- reactiveValues(
    filtereWords=NULL,
    countries = NULL
    )
  
  pp <- eventReactive(c(input$refreshWords),{
    global$filteredWords <- words_ts %>% filter(word %in% input$selectedWords) 
    global$filteredWords %>% 
      ggplot(aes(date, n, color = word)) +
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
  })
  

  output$wordsOverTime <- renderPlot({
    pp()
    }, height = 450, width = 700)
  
  output$corrPlot <- renderPlot({
    
    pal <- colorRampPalette(rev(brewer.pal(11, "RdYlBu")))(100)
    pheatmap(word_corrs, color = pal, treeheight_row = 0, treeheight_col = 0)
    
  }, height = 450, width = 480)
  
  #### click ####
  
  output$click_info <- renderTable({
    
    session_info %>%
      filter(date == as.POSIXct(nearPoints(global$filteredWords, input$plot1_click)[1,"date"])) %>% 
      mutate(date=format(as.POSIXct(date),'%Y %B')) %>% 
      as.data.frame()
    
  }, options = list(searching = FALSE, paging = FALSE))

  
  output$click_info2 <- renderTable({
    
    global$countries <- date_country_word %>%
      filter(date == as.POSIXct(nearPoints(global$filteredWords, input$plot1_click)[1,"date"])) %>% 
      filter(word == nearPoints(global$filteredWords, input$plot1_click)[1,"word"]) %>% 
      top_n(5) %>%
      left_join(country_codes) %>% #despite intuition its faster this way then prejoining 
      arrange(desc(n)) %>% 
      mutate(country=fullname,frequency=n) %>% 
      select(country,frequency) %>% as.data.frame()
    
    
  }, options = list(searching = FALSE, paging = FALSE)) 

  output$click_info3 <- renderTable({
    
    data.frame(
      word = nearPoints(global$filteredWords, input$plot1_click)[1,"word"],
      date = format(as.POSIXct(nearPoints(global$filteredWords, input$plot1_click)[1,"date"]),'%Y %B'),
      stringsAsFactors = FALSE)
    
  })
  
  
  
  output$countries = renderUI({
    selectInput("columns",'Country using the word', global$countries$country)
  })
  
  #### rend. obj. DB 2 ####
  
  # sentiment index
  
  # sentimentScore <- reactive({
  #   
  #   sentiment_data %>%
  #     filter(year == input$year & country == input$country) %>% 
  #     select(proportionsentiment) %>% 
  #     mutate(proportionsentiment = round(proportionsentiment, 2))
  #     
  # })
  
  output$testInfo1 <- renderValueBox({
    valueBox(
      subtitle = paste0("is the Sentiment Index in ", input$year), 
      value = "WIP",
      icon = icon("heartbeat", lib = "font-awesome"),
      color = "navy"
    )
  })
  
  # mentiod the most positively by
  
  sentimentPercPos <- reactive({
    
    sentiment_data %>%
      filter(year == input$year & country == input$country & sentiment == "positive") %>% 
      select(proportionsentiment) %>% 
      mutate(proportionsentiment = round(proportionsentiment, 2))
    
  })
  
  output$testInfo2 <- renderValueBox({
    valueBox(
      subtitle = paste0("Mentioned ", input$country," the most positively in ", input$year), 
      value = paste0(sentimentPercPos()*100, "%"),
      icon = icon("thumbs-up", lib = "glyphicon"),
      color = "blue"
    )
  })
  
  # mentiod the most negatively by
  
  sentimentPercNeg <- reactive({
    
    sentiment_data %>%
      filter(year == input$year & country == input$country & sentiment == "negative") %>% 
      select(proportionsentiment) %>% 
      mutate(proportionsentiment = round(proportionsentiment, 2))
    
  })
  
  output$testInfo3 <- renderValueBox({
    valueBox(
      subtitle = paste0("Mentioned ",  input$country," the most negatively in ", input$year), 
      value = paste0(sentimentPercNeg()*100, "%"),
      icon = icon("thumbs-down", lib = "glyphicon"),
      color = "light-blue"
    )
  })
  
  #### line plot ####
  
  sec_counc_count_R <- reactive({sec_counc_count %>% 
      filter(country == input$country)
  })
  
  output$linePlot <- renderHighchart({
    hchart(sec_counc_count_R()[,1:2], type = "line", hcaes(x=year, y=count), name = "Frequency") %>% 
      hc_add_theme(hc_theme_smpl()) %>% 
      hc_xAxis(title = list(text = "Year")) %>% 
      hc_yAxis(title = list(text = "Frequency"))
  })
  
  # output$linePlot <- renderPlot({
  #   ggplot(data = sec_counc_count_R(), aes(year, count)) + 
  #     geom_line() +
  #     xlab("Year") +
  #     ylab("Count") +
  #     theme_bw() +
  #     theme(legend.position = "bottom",
  #           panel.grid.minor.y = element_blank())
  # })
  
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
                                     color = country,
                                     angle = angle,
                                     replace = TRUE),
           rm_outside = TRUE) +
      geom_text_wordcloud_area(shape = "circle", 
                               eccentricity = 0.35) +
      scale_size_area(max_size = 24) +
      theme_minimal()
    
  })
}

# app -----------------------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
