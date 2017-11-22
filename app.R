library(shiny)
library(plotly)
library(ggplot2)
library(wordcloud)
library(tidytext)
library(rsconnect)
library(swirl)
library(dplyr)
library(stringr)
library(wordcloud)
library(reshape2)
library(syuzhet)
library(sentimentr)
library(tidyr)
library(wordcloud2)
library(rsconnect)
library(RColorBrewer)

rSquaredValues <- read.table("correlationBySegment.csv", stringsAsFactors = FALSE, sep =",", comment.char='', quote='', header= TRUE)
rateOfReturn <- read.csv("rateOfReturn.csv")

ui <- fluidPage( 
  
  # Application title
  headerPanel("Apple Sentiment Analysis"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Drop down for event ----
      selectInput(inputId = "event",
                  label = "Event",
                  choices = c("All",
                              "Introduction",
                              "General",
                              "AppleWatch",
                              "AppleTV",
                              "iPhone8",
                              "iPhoneX",
                              "Ending")),
      
      # Input: Slider for minute delay ----
      sliderInput(inputId = "minDelay",
                  label = "Minute(s) of Lag for Rate of Return",
                  min = 0, 
                  max = 3, 
                  value = 0)
      
      # Input: Slider for max number of words ----
      # sliderInput(inputId = "numberOfWords",
      #             label = "Maximum Number Of Top Words",
      #             min = 1, 
      #             max = 200, 
      #             value = 100),
      
    ),
    
    # nest everything in a mainPanel
    mainPanel(  
      tabsetPanel(
        tabPanel('Sentiment and Rate of Return', 
                 h3('Average Sentiment Score Per Minute'),
                 plotlyOutput("sentimentPlot"),
                 h3('Average Sentiment Score v.s. Rate of Return'),
                 p('R squared value'),
                 verbatimTextOutput("rSquare"),
                 plotlyOutput("sentimentScoreRateofReturn")),
        tabPanel("Word Cloud",
                 h3('Top words used'),
                 wordcloud2Output("wordCloud"))
      )
    )
  )
)

# input is from slider
server <- function(input, output){
  
  # sentiment per minute chart
  output$sentimentPlot <- renderPlotly({ 
    
    data <- rateOfReturn
    
    if (input$event != "All") {
      data <- data[data$event == input$event,]
    }
    
    # create gg plot
    plot <- ggplot(data, aes(time, sentimentScore)) + 
      geom_bar(stat = "identity", aes(fill= isAboveAverage)) +
      ylim(0.0, 0.43) + 
      # change legend
      scale_fill_manual(values = c('TRUE' = '#02E4C5', 'FALSE' = '#01A48E')) + 
      labs(x = "Time", y = "Average Sentiment Score", 
           subtitle = "An average sentiment score of tweets per minute",
           fill = "Above Average") + 
      theme(axis.text.x = element_blank(), 
            axis.ticks.x = element_blank())
    
    # create plotly plot
    ggplotly(plot)
    
  })
  
  # average sentiment score v.s. rate of return
  output$sentimentScoreRateofReturn <- renderPlotly({ 
    
    data <- rateOfReturn
    
    if (input$event != "All") {
      data <- data[data$event == input$event,]
    }
    
    # retrieve type of rate of return by index (starts from 3rd col)
    index <- 4 + input$minDelay

    # plotly with gg plot
    # p2 <- ggplot(data, aes(x = time)) +
    #   geom_line(stat = "identity", aes(y = sentimentScore, col = 'Sentiment Score', group = 1)) +
    #   geom_line(aes(y = data[,index]*100, col = 'Rate of Return', group = 1)) +
    #   scale_y_continuous(sec.axis = sec_axis(~.*0.01, name = "Rate of Return (%)")) +
    #   labs(x = "Time", y = "Average Sentiment Score", 
    #        subtitle = "An average sentiment score of tweets per minute") + 
    #   scale_color_manual(labels = c('Sentiment Score', 'Rate of Return'),
    #                      values = c('Sentiment Score'='#3F81A4', 'Rate of Return' = '#A46A1F')) +
    #   theme(axis.text.x = element_blank(), 
    #         axis.ticks.x = element_blank())
    
    # secondary y axis
    y2 <- list(
      tickfont = list(color = "black"),
      overlaying = "y",
      side = "right",
      title = 'Rate of Return (%)',
      range = c(-0.45, 0.45)
    )
    
    p <- plot_ly() %>% 
      add_lines(x = ~data$time, y = ~data$sentimentScore, name = 'Sentiment Score') %>% 
      add_lines(x = ~data$time, y = ~100*data[,index], name = 'Rate of Return') %>% 
      layout(
        title = "Average Sentiment Score v.s. Rate of Return",
        yaxis2 = y2,
        xaxis = list(title = "time", line = FALSE),
        yaxis = list(title = "Average Sentiment Score Per Min",range = c(-0.45, 0.45), line = FALSE)
      )
    
    # create plotly plot
    ggplotly(p) 
  })
  
  # word cloud per segment
  output$wordCloud <- renderWordcloud2({ 
    if (input$event == "All") {
      generateWordCloud(all)
    }
    else if (input$event == "Introduction") {
      generateWordCloud(listOfWordCloud[[1]])
    }
    else if (input$event == "General") {
      generateWordCloud(listOfWordCloud[[2]])
    }
    else if (input$event == "AppleWatch") {
      generateWordCloud(listOfWordCloud[[3]])
    }
    else if (input$event == "AppleTV") {
      generateWordCloud(listOfWordCloud[[4]])
    }
    else if (input$event == "iPhone8") {
      generateWordCloud(listOfWordCloud[[5]])
    }
    else if (input$event == "iPhoneX") {
      generateWordCloud(listOfWordCloud[[6]])
    }
    else {
      generateWordCloud(listOfWordCloud[[7]])
    }
  })
  
  output$rSquare<- renderPrint({
    n <- rSquaredValues[rSquaredValues$Segment == input$event,][[input$minDelay + 2]]
    n
  })
}

shinyApp(ui, server)