
# packages ------------------------------------------------------------------------------------

library(tidyverse)
library(tidytext)
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinythemes)
library(DT)
library(shinyWidgets)
library(shinycssloaders)
library(Cairo)
library(RColorBrewer)
library(pheatmap)
library(ggwordcloud)
library(highcharter)
library(htmlTable)
library(feather)

# shiny dashboard -----------------------------------------------------------------------------

options(shiny.usecairo = TRUE)

# input data ----------------------------------------------------------------------------------

# dashboard 1

words_ts <- read.table("words_ts.csv", header = TRUE, stringsAsFactors = FALSE) %>% 
  mutate(date = as.POSIXct(date)) #date column as POSIXct from the start
# path <- "words_ts.feather"
# write_feather(words_ts, path)
# words_ts <- read_feather(path)

trending_words <- read.table("trending words.csv", header = TRUE, stringsAsFactors = FALSE)
path <- "trending_words.feather"
write_feather(trending_words, path)
trending_words <- read_feather(path)

word_corrs_ordered <- read.csv("word_corrs_ordered.csv", header = TRUE, stringsAsFactors = FALSE, row.names = 1)
# path <- "word_corrs_ordered.feather"
# write_feather(word_corrs_ordered, path)
# word_corrs_ordered <- read_feather(path)

session_info <- read.table("session_info.csv", header = TRUE, stringsAsFactors = FALSE)
path <- "session_info.feather"
write_feather(session_info, path)
session_info <- read_feather(path)

date_country_word <- read.table("date_country_word.csv", header = TRUE, stringsAsFactors = FALSE)
path <- "date_country_word.feather"
write_feather(date_country_word, path)
date_country_word <- read_feather(path)

country_codes <- read.table("country_codes.csv", header = TRUE, stringsAsFactors = FALSE)
path <- "country_codes.feather"
write_feather(country_codes, path)
country_codes <- read_feather(path)

# sentences <- read.table("sentences_filtered.csv", header = TRUE, stringsAsFactors = FALSE)
sentences <- as.tibble(data.table::fread("sentences_filtered.csv"))
path <- "sentences.feather"
write_feather(sentences, path)
sentences <- read_feather(path)

# dashboard 2

sec_counc_count <- read.csv("securitycouncil_count.csv", header = TRUE, stringsAsFactors = FALSE)
path <- "sec_counc_count.feather"
write_feather(sec_counc_count, path)
sec_counc_count <- read_feather(path)

sec_counc_words <- read.csv("securitycouncil_words.csv", header = TRUE, stringsAsFactors = FALSE)
path <- "sec_counc_words.feather"
write_feather(sec_counc_words, path)
sec_counc_words <- read_feather(path)

sentiment_data <- read.csv("securitycouncil_sentiment.csv", header = TRUE, stringsAsFactors = FALSE)
path <- "sentiment_data.feather"
write_feather(sentiment_data, path)
sentiment_data <- read_feather(path)

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
                
                menuItem("Trending Words", tabName = "dashboard", icon = icon("chart-line", lib = "font-awesome")),
                menuItem("Country Sentiment", tabName = "dashboard2", icon = icon("hand-holding-heart", lib = "font-awesome")),
                menuItem("About Us", tabName = "aboutus", icon = icon("users", lib = "font-awesome"))
                
    ),
    
    hr(),
    
    #### sliders DB 1 ####
    
    conditionalPanel(condition = "input.sidebar=='dashboard'",
                     
                     pickerInput(
                       inputId = "selectedWords",
                       label = HTML('<p style="color:black;">Select Words to Plot:</p>'),
                       choices = trending_words$word,
                       selected = c("arab","israel","arms"),
                       options = pickerOptions(
                         actionsBox = TRUE,
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
                     
                     selectInput(inputId = "year",
                                 label = HTML('<p style="color:black;">Select Year for Wordcloud:</p>'),
                                 choices = 1970:2015,
                                 selected = 2015,
                                 selectize = TRUE),
                     
                     knobInput(inputId = "maxWordsCloud",
                               label = HTML('<p style="color:black;">Fill Wordcloud:</p>'),
                               value = 35,
                               min = 25,
                               max = 60,
                               width = "100%",
                               height = "90%",
                               displayPrevious = TRUE,
                               immediate = FALSE,
                               lineCap = "round",
                               fgColor = "#5b92e5",
                               inputColor = "#5b92e5"
                     )
    ),
    hr()
    
    #### Nato image ####
    
    # sidebarPanel(width = "170px", height = "113px",
    #   imageOutput("logo_un", width = "180px", height = "120px")
    # )
    
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
                              ')
    )
    ),
    
    tabItems(
      
      #### content DB 1 ####
      
      tabItem(tabName = "dashboard",
              
              fluidRow(
                
                box(title = "Heatmap Displaying Clusters of Correlated Word Trends",
                    status = "primary",
                    width = 5,
                    height = "520px",
                    solidHeader = TRUE,
                    
                    withSpinner(highchartOutput("corrPlot", width = 495, height = 480))
                    
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
                    
                    withSpinner(htmlTableWidgetOutput("click_info",height = "auto"))
                    
                ),
                
                box(title = "Frequency by Country (Top Users)",
                    status = "primary",
                    width = 3,
                    solidHeader = TRUE,
                    
                    withSpinner(htmlTableWidgetOutput("click_info2",height="auto"))
                    
                ),
                
                box(title = "Find in Text",
                    status = "primary",
                    width = 2,
                    solidHeader = TRUE,
                    withSpinner(htmlTableWidgetOutput("click_info3",height="auto")),
                    uiOutput('countries'),
                    actionButton(
                      "lookup", "Find Quote"
                    ),
                    
                    tags$head(tags$style(".modal-dialog{ width:1500px}")),
                    tags$head(tags$style(".modal-body{ min-height:700px}")),
                    
                    bsModal(id = "wordquote",
                            title = "Quote(s) in which the word appeared",
                            trigger = "lookup",
                            withSpinner(htmlTableWidgetOutput("text"))
                    ),
                    tags$head(tags$style("#wordquote .modal-footer{ display:none}"))
                )
              )
      ),
      
      #### content DB 2 ####
      
      tabItem(tabName = "dashboard2",
              
              fluidRow(
                
                withSpinner(valueBoxOutput("testInfo1")),
                withSpinner(valueBoxOutput("testInfo2")),
                withSpinner(valueBoxOutput("testInfo3")),
                withSpinner(valueBoxOutput("testInfo4")),
                withSpinner(valueBoxOutput("testInfo5")),
                withSpinner(valueBoxOutput("testInfo6"))
                
              ),
              
              fluidRow(
                
                box(title = "Number of Mentions of Selected Country over Time",
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
                
                tabBox(title = "Top Associated Words",
                       #status = "primary",
                       width = 6,
                       #solidHeader = TRUE,
                       
                       tabPanel("Wordcloud", withSpinner(plotOutput("wordcloud"))),
                       tabPanel("Bar chart", withSpinner(plotOutput("barPlot")))
                       
                )
              )
              
      ),
      
      
      #### about us ####
      
      
      tabItem(tabName = "aboutus",
              
              includeMarkdown("AboutUs.Rmd")
              
      )
    )
  )
)


# server --------------------------------------------------------------------------------------

server <- function(input, output) {
  
  #### DB 1 ####
  global <- reactiveValues(
    filtereWords=NULL,
    countries = NULL
  )
  
  #### NATO IMAGE ####
# 
#   output$logo_un <- renderImage(
#     expr = list(
#       src = "logo.png",
#       filetype = "image/png",
#       width = 180,
#       height = 120),
#     deleteFile = FALSE
#   
#   )

  
  #### word plot ####
  
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
      xlab("Date") +
      ylab("Word Frequency") +
      theme_bw() +
      theme(legend.position = "bottom",
            panel.grid.minor.y = element_blank())
  })
  
  
  output$wordsOverTime <- renderPlot({
    pp()
  }, height = 450, width = 700)
  
  #### corr plot ####
  
  output$corrPlot <- renderHighchart({
    
    pal <- colorRampPalette(rev(brewer.pal(11, "RdYlBu")))(100)
    #pheatmap(word_corrs, color = pal, treeheight_row = 0, treeheight_col = 0)
    
    hc <- hchart(as.matrix(word_corrs_ordered)[1:49,1:49]) %>%
      hc_size(height = 450, width = 480) %>% 
      hc_xAxis(tickmount = 50) %>%
      hc_yAxis(tickmount = 50)
    
    hc$x$hc_opts$colorAxis$stops <- NULL
    
    hc %>% 
      hc_colorAxis(stops = color_stops(11, colors=pal)) %>%
      hc_xAxis(visible = F) #%>%
      #hc_yAxis(visible = F)
    
  }
  #, height = 450, width = 480
  )
  
  #### info table 1 ####
  
  output$click_info <- renderHtmlTableWidget({
    
    session_info %>%
      filter(date == as.POSIXct(nearPoints(global$filteredWords, input$plot1_click)[1,"date"])) %>% 
      mutate(date=format(as.POSIXct(date),'%Y %B')) %>% 
      mutate(link=paste0("<a href='",link,"'>",link,"</a>")) %>% 
      as.data.frame(rownames= FALSE) %>% 
      htmlTableWidget(number_of_entries = c(1),rnames=F)
    
  })
  
  #### info table 2 ####
  
  output$click_info2 <- renderHtmlTableWidget({
    
    global$countries <- date_country_word %>%
      filter(date == as.POSIXct(nearPoints(global$filteredWords, input$plot1_click)[1,"date"])) %>% 
      filter(word == nearPoints(global$filteredWords, input$plot1_click)[1,"word"]) %>% 
      top_n(5) %>%
      left_join(country_codes) %>% #despite intuition its faster this way then prejoining 
      arrange(desc(n)) %>% 
      mutate(country=fullname,frequency=n) %>% 
      select(country,frequency) %>%
      as.data.frame()
    
    global$countries %>%
      htmlTableWidget(number_of_entries = c(10),rnames=F)
    
    
  }) 
  
  #### info table 3 ####
  
  output$click_info3 <- renderHtmlTableWidget({
    
    data.frame(
      word = nearPoints(global$filteredWords, input$plot1_click)[1,"word"],
      date = format(as.POSIXct(nearPoints(global$filteredWords, input$plot1_click)[1,"date"]),'%Y %B'),
      stringsAsFactors = FALSE) %>% 
      htmlTableWidget(number_of_entries = c(1),rnames=F)
    
  })
  
  
  
  output$countries = renderUI({
    selectInput("userCountry",'Country using the word', global$countries$country)
  })
  
  ### text window ####
  
  buttonText <- eventReactive(c(input$lookup),{
    
    sentences %>%
      left_join(country_codes) %>%
      mutate(country=fullname) %>% 
      filter(date == as.POSIXct(nearPoints(global$filteredWords, input$plot1_click)[1,"date"])) %>%
      filter(country==input$userCountry) %>%
      filter(grepl(nearPoints(global$filteredWords, input$plot1_click)[1,"word"],sentence)) %>%
      select(sentence) %>% 
      as.data.frame()
    
  })
  
  output$text <- renderHtmlTableWidget({
    
    htmlTableWidget(buttonText(),number_of_entries = c(25, 10))
    
    
    
  })
  
  
  #### DB 2 ####
  
  #### info box 1 ####
  
  # sentiment index
  
  sentimentScore <- reactive({
    
    sentiment_data %>%
      filter(year == input$year & country == input$country) %>%
      select(totalscore) %>%
      mutate(totalscore = round(totalscore, 2))
    
  })
  
  output$testInfo1 <- renderValueBox({
    valueBox(
      subtitle = paste0("is the averaged Sentiment Score of ", input$country, " in ", input$year), 
      value = sentimentScore(),
      icon = icon("heartbeat", lib = "font-awesome"),
      color = "light-blue"
    )
  })
  
  #### info box 2 ####
  
  # mentiod the most positively by
  
  sentimentPercPos <- reactive({
    
    sentiment_data %>%
      filter(year == input$year & country == input$country & sentiment == "positive") %>% 
      select(proportion, top_country)
    
  })
  
  output$testInfo2 <- renderValueBox({
    valueBox(
      subtitle = paste0("of words referring to ", input$country," in ", input$year, " were positive"), 
      value = sentimentPercPos()[, 1],
      icon = icon("thumbs-up", lib = "glyphicon"),
      color = "olive"
    )
  })
  
  #### info box 3 ####
  
  # mentiod the most negatively by
  
  sentimentPercNeg <- reactive({
    
    sentiment_data %>%
      filter(year == input$year & country == input$country & sentiment == "negative") %>% 
      select(proportion, top_country)
    
  })
  
  output$testInfo3 <- renderValueBox({
    valueBox(
      subtitle = paste0("of words referring to ", input$country," in ", input$year, " were negative"), 
      value = sentimentPercNeg()[, 1],
      icon = icon("thumbs-down", lib = "glyphicon"),
      color = "red"
    )
  })
  
  #### info box 4 ####
  
  yearCount <- reactive({
    
    sec_counc_count %>%
      filter(year == input$year & country == input$country) %>% 
      select(count)
    
  })
  
  output$testInfo4 <- renderValueBox({
    valueBox(
      subtitle = paste0("times ", input$country, " was mentioned in ", input$year),
      value = yearCount(),
      icon = icon("sort-amount-up", lib = "font-awesome"),
      color = "light-blue"
    )
  })
  
  #### info box 5 ####
  
  output$testInfo5 <- renderValueBox({
    valueBox(
      subtitle = paste0("mentioned ", input$country," the most positive in ", input$year), 
      value = sentimentPercPos()[, 2],
      icon = icon("user-plus", lib = "font-awesome"),
      color = "olive"
    )
  })
  
  #### info box 6 ####
  
  output$testInfo6 <- renderValueBox({
    valueBox(
      subtitle = paste0("mentioned ", input$country," the most negative in ", input$year), 
      value = sentimentPercNeg()[, 2],
      icon = icon("user-minus", lib = "font-awesome"),
      color = "red"
    )
  })
  
  #### line plot ####
  
  sec_counc_count_R <- reactive({sec_counc_count %>% 
      filter(country == input$country)
  })
  
  output$linePlot <- renderHighchart({
    hchart(sec_counc_count_R()[,1:2], type = "line", hcaes(x = year, y = count), name = "Frequency") %>% 
      hc_add_theme(hc_theme_smpl()) %>%
      hc_plotOptions(line = list(color = "deepskyblue")) %>% 
      hc_xAxis(title = list(text = "Year")) %>% 
      hc_yAxis(title = list(text = "Number of Mentions"))
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
  
  #### barplot ####
  
   # barplot
   
   sec_counc_words_R1 <- reactive({
     sec_counc_words %>%
       filter(country == input$country) %>% 
       filter(year == input$year) %>%
       top_n(10, tf) %>%
       mutate(word = reorder(word, tf)) %>% '['(1:10,)
       
   })
   
   output$barPlot <- renderPlot({
       ggplot(data = sec_counc_words_R1(), aes(word, freqword,label=word)) +
       geom_col(fill="#5b92e5",color="#5b92e5") +
       xlab(NULL) +
       ylab(NULL) +
       geom_text(size = 5, position = position_stack(vjust = 0.5),
                 color="white",fontface = "bold") +
       coord_flip() +
       theme_bw() +
       theme(axis.title.y=element_blank(),
             axis.text.y=element_blank(),
             axis.ticks.y=element_blank())
   })
  
  #### wordcloud #####
  
  sec_counc_words_R2 <- reactive({sec_counc_words %>%
      # mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40))) %>% 
      mutate(angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 1, 4, 1, 1))) %>%
      filter(country == input$country, year == input$year) %>%
      top_n(n = input$maxWordsCloud, wt = freqword)
  })
  
  output$wordcloud <- renderPlot({
    
    ggplot(sec_counc_words_R2(), aes(label = word,
                                     size = freqword,
                                     color = freqword,
                                     angle = angle,
                                     replace = TRUE),
           rm_outside = TRUE) +
      scale_color_gradient(low = "lightgray", high = "deepskyblue") +
      geom_text_wordcloud_area(shape = "diamond", 
                               eccentricity = 1,
                               seed = 123,
                               rm_outside = TRUE) +
      scale_size_area(max_size = 28) +
      theme_minimal()
    
  })
}

# app -----------------------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
