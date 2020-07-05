library(shiny)
library(shinythemes)
library(shinyjs)
library(wordcloud)
library(plotly)
library(ggplot2)
library(stringr)
library(dplyr)
library(datasets)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(RColorBrewer)
library(gmodels)

#=====================load data
americanSentimentCount <- readRDS("americanSentimentCount.rds")
cleanTweets <- readRDS("cleanTweets.rds")
deltaSentimentCount <- readRDS("deltaSentimentCount.rds")
southwestSentimentCount <- readRDS("southwestSentimentCount.rds")
unitedSentimentCount <- readRDS("unitedSentimentCount.rds")
usairwaysSentimentCount <- readRDS("usairwaysSentimentCount.rds")
virginSentimentCount <- readRDS("virginSentimentCount.rds")
polarity_over_time_actual <- readRDS("polarity_over_time_actual.rds")
polarity_over_time_predicted <- readRDS("polarity_over_time_predicted.rds")
relative_polarity_over_time_actual <- readRDS("relative_polarity_over_time_actual.rds")
relative_polarity_over_time_predicted <- readRDS("relative_polarity_over_time_predicted.rds")
sentimentCount <- readRDS("sentimentCount.rds")
tweet_polarity_date_actual <- readRDS("tweet_polarity_date_actual.rds")
tweet_polarity_date_predicted <- readRDS("tweet_polarity_date_predicted.rds")
wordfrequency <- readRDS("wordfrequency.rds")
cc <- readRDS("cc.rds")
uniquewords <- readRDS("uniquewords.rds")
tweets <- readRDS("tweets.rds")
tweetsbySentimentreasonPlot <- readRDS("tweetsbySentimentreasonPlot.rds")


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage(
                    "Airline Sentiment",id = "inTabset",
                    tabPanel("Analysis", icon = icon("chart-bar"),
                             sidebarPanel(
                                 img(src = "plane.png", height = 40),
                                 h5 ("Our team has carried out sentiment analysis of tweets directed at US airlines. Through the analysis of the tweets we have determined customer sentiment as well as see the reasons that drive users to provide feedback to airlines."),
                                 selectInput(inputId = "airln",
                                             label = "Sentiment Count by Airline:",
                                             choices = c("All Airlines", "American", "Delta", "Southwest", "US Airways", "United", "Virgin America"),
                                             selected = "All Airlines"),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 h5 ("This graph shows top unique words from the tweets."),
                                br(),
                                 sliderInput("numWords", "Words in bar graph:", 
                                             min=1, max= 35, value=10, step = 1),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                br(),
                                 h5 ("This graph shows different reasons for the negative sentiment by the customer."),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br()
                             ), 
                             mainPanel(
                                 h5 (style = "color:#555555; text-align: left;",
                                     "Sentiment Analysis on US Airlines Twitter Feeds"
                                 ),
                                 tags$b("Sentiment Count:"),
                                 plotOutput(outputId = "plot1", height = "300px"),
                                 tags$b("Top 20 Words:"),
                                 plotOutput("barplot"),
                                 tags$b("Negative Sentiment Reason:"),
                                 plotOutput("reasonplot")
                             )
                    ),
                    tabPanel("About", icon = icon("code"),
                             mainPanel(includeHTML("markdown.html"))),
                    tabPanel("WordCloud", icon = icon("cloud"),
                             sidebarPanel(
                                 h5 ("This graph shows most frequently used word in tweets. Select the number of words to display using slider."),
                                 br(),
                                 sliderInput("max",
                                             "Maximum Number of Words:",
                                             min = 1,  max = 100,  value = 50)
                             ),
                             mainPanel(
                                 tags$b("WordCloud:"),
                                 plotOutput("plotcloud", height = "500px", width = "500px")
                             )
                    ),
                    tabPanel("Twitter Feed", icon = icon("twitter"),
                             DT::dataTableOutput('ex1')),
                    tabPanel("Polarity", icon = icon("chart-line"),
                             sidebarPanel(
                                 h5 ("This graph shows polar sentiment of tweets from 16-Feb-2015 to 24-Feb-2015,
                     Polarity is Positive-Negative ,Percent Positive is (Positive/Positive+Negative)*100"),
                                 radioButtons("polar", "Polarity Over time:",
                                              c("Actual Polarity Over Time",
                                                "Predicted Polarity Over Time",
                                                "Actual Percent Positive Over Time",
                                                "Predicted Percent Positive Over Time"))
                                 
                             ),
                             mainPanel(
                                 plotOutput("polarityplot1")
                                 #plotOutput("polarityplot2")
                             )
                             )  
                ) 
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #==============================tweets table
    output$ex1 <- DT::renderDataTable(
        DT::datatable(cleanTweets, options = list(pageLength = 10))
    )
    
    #==============================wordcloud
    output$plotcloud <- renderPlot({
        wordcloud(words =wordfrequency$text,freq=wordfrequency$freq,min.freq = 1,
                  max.words=input$max,random.order = FALSE,rot.per=0.35,colors = brewer.pal(8,"Dark2"))
    })
    
    #==============================bargraph
    output$barplot <- renderPlot({
        uniquewords<-wordfrequency%>%
            arrange(-freq)%>%
            top_n(input$numWords)
        uniqueWordsPlot<-ggplot(uniquewords) +
            geom_col(
                mapping = aes(x = freq, y = text, fill = text), position = "dodge"
            )
        print(uniqueWordsPlot)
    })
    
    #==============================sentiment reason bar graph
    output$reasonplot <- renderPlot({
        tweetsbySentimentreason<-tweets%>%
            filter(sentiment=="negative")%>%
            group_by(sentiment,reason)%>%
            dplyr::summarise(srcount=n())
        tweetsbySentimentreasonPlot<-ggplot(tweetsbySentimentreason) +
            geom_col(
                mapping = aes(x = sentiment, y = srcount, fill = reason), position = "dodge"
            )
        print(tweetsbySentimentreasonPlot)
    })
    
    
    #==============================polarity plot 1
    # output$polarityplot1 <- renderPlot({
    #     polarity_over_time_actual <- tweet_polarity_date_actual %>%
    #         ggplot(aes(created, polarity)) +
    #         geom_col() +
    #         geom_smooth(method = "loess", se = FALSE) +
    #         geom_smooth(method = "lm", se = FALSE,aes(color = "#E69F00")) +
    #         theme_fivethirtyeight()+ theme(plot.title = element_text(size = 11)) +
    #         xlab(NULL) + ylab(NULL) +
    #         ggtitle("Polarity Over Time-Actual")
    #     
    #     relative_polarity_over_time_actual <- tweet_polarity_date_actual %>%
    #         ggplot(aes(created, percent_positive )) +
    #         geom_col() +
    #         geom_smooth(method = "loess", se = FALSE) +
    #         geom_smooth(method = "lm", se = FALSE, aes(color = "#E69F00")) +
    #         theme_fivethirtyeight() + theme(plot.title = element_text(size = 11)) +
    #         xlab(NULL) + ylab(NULL) +
    #         ggtitle("Percent Positive Over Time-Actual")
    # 
    #     print(grid.arrange(polarity_over_time_actual, relative_polarity_over_time_actual, ncol = 2))
        #print(grid.arrange(polarity_over_time_actual, ncol = 1))
    #})
    
    #==============================polarity plot 2
    # output$polarityplot2 <- renderPlot({
    #     polarity_over_time_predicted <- tweet_polarity_date_predicted %>%
    #     ggplot(aes(created, polarity)) +
    #         geom_col() +
    #         geom_smooth(method = "loess", se = FALSE) +
    #         geom_smooth(method = "lm", se = FALSE,aes(color = "#E69F00")) +
    #         theme_fivethirtyeight()+ theme(plot.title = element_text(size = 11)) +
    #         xlab(NULL) + ylab(NULL) +
    #         ggtitle("Polarity Over Time-Predicted")
    # 
    #     relative_polarity_over_time_predicted <- tweet_polarity_date_predicted %>%
    #         ggplot(aes(created, percent_positive )) +
    #         geom_col() +
    #         geom_smooth(method = "loess", se = FALSE) +
    #         geom_smooth(method = "lm", se = FALSE, aes(color = "#E69F00")) +
    #         theme_fivethirtyeight() + theme(plot.title = element_text(size = 11)) +
    #         xlab(NULL) + ylab(NULL) +
    #         ggtitle("Percent Positive Over Time-Predicted")
    # 
    #     print(grid.arrange(polarity_over_time_predicted, relative_polarity_over_time_predicted, ncol = 2))
    # 
    # })
    
    output$polarityplot1 <- reactivePlot(function() {
      
        if (input$polar == "Actual Polarity Over Time") {
            polarity_over_time_actual <- tweet_polarity_date_actual %>%
                ggplot(aes(created, polarity)) +
                geom_col() +
                geom_smooth(method = "loess", se = FALSE) +
                geom_smooth(method = "lm", se = FALSE,aes(color = "#E69F00")) +
                theme_fivethirtyeight()+ theme(plot.title = element_text(size = 11)) +
                xlab(NULL) + ylab(NULL) +
                ggtitle("Polarity Over Time-Actual")
            print(polarity_over_time_actual)
        }
        else if (input$polar == "Predicted Polarity Over Time") {
            polarity_over_time_predicted <- tweet_polarity_date_predicted %>%
                    ggplot(aes(created, polarity)) +
                        geom_col() +
                        geom_smooth(method = "loess", se = FALSE) +
                        geom_smooth(method = "lm", se = FALSE,aes(color = "#E69F00")) +
                        theme_fivethirtyeight()+ theme(plot.title = element_text(size = 11)) +
                        xlab(NULL) + ylab(NULL) +
                        ggtitle("Polarity Over Time-Predicted")
            print(polarity_over_time_predicted)
        }
        else if (input$polar == "Actual Percent Positive Over Time") {
            relative_polarity_over_time_actual <- tweet_polarity_date_actual %>%
                        ggplot(aes(created, percent_positive )) +
                        geom_col() +
                        geom_smooth(method = "loess", se = FALSE) +
                        geom_smooth(method = "lm", se = FALSE, aes(color = "#E69F00")) +
                        theme_fivethirtyeight() + theme(plot.title = element_text(size = 11)) +
                        xlab(NULL) + ylab(NULL) +
                        ggtitle("Percent Positive Over Time-Actual")
            print(relative_polarity_over_time_actual)
        }
        else if (input$polar == "Predicted Percent Positive Over Time") {
            relative_polarity_over_time_predicted <- tweet_polarity_date_predicted %>%
                        ggplot(aes(created, percent_positive )) +
                        geom_col() +
                        geom_smooth(method = "loess", se = FALSE) +
                        geom_smooth(method = "lm", se = FALSE, aes(color = "#E69F00")) +
                        theme_fivethirtyeight() + theme(plot.title = element_text(size = 11)) +
                        xlab(NULL) + ylab(NULL) +
                        ggtitle("Percent Positive Over Time-Predicted")
            print(relative_polarity_over_time_predicted)
        }
          
    })
    
    #==============================main page plot
    output$plot1 <- reactivePlot(function() {
        # check for the input variable
        if (input$airln == "All Airlines") {
            p <- ggplot(sentimentCount, aes(x = sentiment, y = count, fill=sentiment)) + 
                geom_col() +
                scale_fill_brewer(palette="Paired")
        }
        else if (input$airln == "American"){
            p <- ggplot(americanSentimentCount, aes(x = sentiment, y = count, fill=sentiment)) + 
                geom_col() +
                scale_fill_brewer(palette="Paired")
        }
        else if (input$airln == "Delta"){
            p <- ggplot(deltaSentimentCount, aes(x = sentiment, y = count, fill=sentiment)) + 
                geom_col() +
                scale_fill_brewer(palette="Paired")
        }
        else if (input$airln == "Southwest"){
            p <- ggplot(southwestSentimentCount, aes(x = sentiment, y = count, fill=sentiment)) + 
                geom_col() +
                scale_fill_brewer(palette="Paired")
        }
        else if (input$airln == "United"){
            p <- ggplot(unitedSentimentCount, aes(x = sentiment, y = count, fill=sentiment)) + 
                geom_col() +
                scale_fill_brewer(palette="Paired")
        }
        else if (input$airln == "US Airways"){
            p <- ggplot(usairwaysSentimentCount, aes(x = sentiment, y = count, fill=sentiment)) + 
                geom_col() +
                scale_fill_brewer(palette="Paired")
        }
        else if (input$airln == "Virgin America"){
            p <- ggplot(virginSentimentCount, aes(x = sentiment, y = count, fill=sentiment)) + 
                geom_col() +
                scale_fill_brewer(palette="Paired")
        }
        print(p)
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
