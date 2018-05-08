# library

options(warn = -1)
library(shiny)
library(shinydashboard)
library(twitteR)
library(tidyverse)
library(ggplot2)
library(tidytext)
library(stringr)
library(scales)
library(wordcloud2)
library(htmlwidgets)
library(tidyr)
library(dplyr)
library(wordcloud)
data(stop_words)

stop_words <- add_row(stop_words, word = "t.co", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "https", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "http", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "rt", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "0", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "1", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "2", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "3", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "9", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "03", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "6", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "4", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "05", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "2018", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "https", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "http", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "t.co", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "4zzhmmngci", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "1", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "2", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "3", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "de", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "amp", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "NA", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "FALSE", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "twitter", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "android", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "therealjrsmith", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "20", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "10", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "qulsgpq1kd", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "gkcirtnkyy", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "qflfyixfax", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "l5hxyww94b", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "50wkjztare", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "8", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "14", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "oprahside", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "hsqvlvpuxz", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "qflfyixfax", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "v8epreriz8", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "wzugyb5bnd", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "I5hxyww94b", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "sound", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "cloud", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "flapper", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "rapper", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "epwkh87zwp", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "50wkjzfdre", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "xyx3aupidp", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "yahboyahmed", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "jemelehil", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "rkad92hm1z", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "shahbazmkhan", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "jemelehil", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "lakeshowyo", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "zism1j8isk", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "earzbtiydx", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "2ze2a1f9nc", lexicon = "SMART")
stop_words <- add_row(stop_words, word = "zsxdkscj16", lexicon = "SMART")


data_total <- read.csv("cavs_raptors.csv")
data_total <- as.tibble(data_total)
data_total$DATE <- as.Date(data_total$DATE)
daily <- filter(data_total)


LBJ_2017_2018 <- read.csv("LBJ_2017_2018.csv")
LBJ_2017_2018 <- as.tibble(LBJ_2017_2018)


getFrequency <- function(file) {
  text <- readLines(file)
  text_df <- data_frame(line = 1:length(text), text = text )
  text_df <- text_df %>%
    unnest_tokens(word, text) %>% 
    count(word, sort = TRUE) %>% 
    filter(n > 200) %>% 
    anti_join(stop_words, by = "word") %>% 
    mutate(word = reorder(word, n)) %>% 
    ggplot(aes(word, n), col = "blue") +
    geom_col() +
    xlab(NULL) +
    coord_flip()
}

getWordCloud <- function(file, min_freq, max_word) {
  df <- getTidyTibble(file)
  cloud <- df %>% anti_join(stop_words, by = "word") %>%
    count(word, sort = TRUE)
  temp <- wordcloud(words = cloud$word, freq = cloud$n, min.freq = min_freq,
                    max.words=max_word, random.order=FALSE, rot.per=0.35, 
                    colors=brewer.pal(8, "Dark2"))
  return(temp)
}

getTidyTibble <- function(file) {
  text <- readLines(file)
  df <- data_frame(line = 1:length(text), text = text)
  df <- df %>% 
    unnest_tokens(word, text) %>% 
    anti_join(stop_words)
  return(df)
}

ui <- dashboardPage(
  dashboardHeader(title = "NBA Playoffs series: Cleveland Cavaliers v.s. Toranto Raptors"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Cleveland V.S. Raptors history", tabName = "match_up"),
      menuItem("Most frequent words", tabName = "frequent_word"),
      menuItem("World cloud", tabName = "word_cloud"),
      menuItem("Sentiment analysis", tabName = "sentiment")
    )
  ),
  
  dashboardBody(
    tabItems(
    tabItem(tabName = "match_up",
            fluidRow(
              box(selectInput("mode1",
                              "LBJ's contribution when playing against Raptors(during the season): ",
                              choices = list("Cavaliers v.s. Raptors history", "LBJ's score in terms of points", "LBJ's score in terms of percentage", "LBJ's performance during the season")),    
                  plotOutput("plot1"), width = 12)
      ),
      p("From the graph shown above, we can see that LeBron James is undoubly the best basketball player in the world right now. In the last few years, in both regular season and playoffs, Cleveland Cavaliers beat Toranto Raptors most of the time.
        The points he contributed in the game takes a large portion of the total team score. In terms of points, LeBron James has been leading the team.")
    ),
    
    tabItem(tabName = "frequent_word",
            fluidRow(
              box(selectInput("mode2",
                              "Most frequent word in tweets mentioning:",
                              choices = list("Cavaliers before game", "Raptors before game", "Cavaliers after game", "Raptors after game")), 
                  plotOutput("plot2"), width = 12)
         ),
         p("As we can see from the grpah, fans from both teams mentioning the names of two teams the most. It is also interesting to see that fans from both teams mentioning LeBron James' name a lot and this may be caused by the amazing performance LBJ has in this series. In the last 3 games, LBJ made the cluch shot twice. Also, we can see some phrasees like wethenorth and whateverittakes that represent the winning determination. ")
         
      ),
    
    tabItem(tabName = "word_cloud",
            fluidRow(
              box(selectInput("mode3",
                              "World cloud:",
                              choices = list("Cavaliers before game", "Raptors before game", "Cavaliers after game", "Raptors after game")), 
                  plotOutput("plot3"), width = 12)
            ),
            p("In the word cloud, the more frequent words appear bigger. It helps us see the frequency of words in a more visual way. Some of the words such as: lebron, game, winner, and sportscenter are the words appear very oftern. This means that these certain topics are most heating discussed topics. ")
            
    ),
    
    tabItem(tabName = "sentiment",
            fluidRow(
              box(selectInput("mode4",
                              "Sentiment:",
                              choices = list("Positive/negative tweets about Cavaliers before game", "Positive/negative tweets about Cavaliers after game", "Positive/negative tweets about Raptors before game", "Positive/negative tweets about Raptors after game", "More analysis about Cavaliers tweets", "More analysis about Raptors tweets")), 
                  plotOutput("plot4"), width = 12)
            ),
            p("From the positive and negative words sentiment analysis, it is hard to see which side has stronger emotion of winning because fans from both sides could mention their opponent team. Also, some of the words that we typically consider positive or negative do not represent the same meaning when peopel tweet about it. However, an interesting fact that I have found is that positive words tend to appear more oftern than negative words, and the meannings of some negative words like: sorry, crazy, and scary should be determined in the context. Overall, the atmosphere of watching playoffs games on Twitter is very friendly. People always get excited about the game instead of attacking opponent teams.")
    )
    )
  ))


server <- function(input, output) {
  
  
  output$plot1 <- renderPlot({
    
    input1 <- input$mode1
    
    if (input1 =="Cavaliers v.s. Raptors history"){
    graph <- ggplot(daily, aes(x = DATE)) + 
      geom_line(aes(y = CLE.PTS), colour="blue", size = 0.5) + 
      geom_line(aes(y = Raptors.PTS), colour="red", size = 0.5) +
      labs(subtitle="Cavaliers Vs Raptors", 
           y="Score", 
           x="Date", 
           title="Team scores comparison",
           caption = "Source: NBA Stats")
    print(graph)
    }
    
    if(input1 =="LBJ's score in terms of points"){
      graph <- ggplot(daily, aes(x = DATE)) + 
        geom_line(aes(y = LBJ.PTS), colour="red", size = 0.5) +
        labs(y="points", 
             x="Date", 
             title="LeBron James' contribution to the team",
             caption = "Source: NBA Stats")
      print(graph)
    }
    
    if(input1 =="LBJ's score in terms of percentage"){
      graph <- ggplot(daily, aes(x = DATE)) + 
        geom_line(aes(y = LBJ_PCG), colour="red", size = 0.5) +
        labs(y="points", 
             x="Date", 
             title="LeBron James' contribution to the team",
             caption = "Source: NBA Stats")
      print(graph)
    
    }
    
    if(input1 =="LBJ's performance during the season"){
      graph <- ggplot(LBJ_2017_2018, aes(x = MATCH, y = PTS)) + 
        geom_point(colour="red", size = 0.5) +
        labs(y="points", 
             x="MATCH", 
             title="Points LeBron James scores during 2017-2018 season",
             caption = "Source: NBA Stats")
      print(graph)
      
    }
  
  })
  
  output$plot2 <- renderPlot({
    
    input2 <- input$mode2
    
    if(input2 =="Cavaliers before game"){
      print(getFrequency('cavs.txt'))
    }
    
    if(input2 =="Cavaliers after game"){
      graph <- getFrequency('after_cavs.txt')
        print(graph)
    }
    
    if(input2 =="Raptors before game"){
      graph <- getFrequency('raptors.txt')
        print(graph)
    }
    
    if(input2 =="Raptors after game"){
      graph <- getFrequency('after_raptors.txt')
        print(graph)
    }
    
  })
  
  output$plot3 <- renderPlot({
    
    input3 <- input$mode3
    
    if(input3 =="Cavaliers before game"){
      cavs_cloud <- getWordCloud('cavs.txt', 100, 300)
      print(cavs_cloud)
    }
    
    if(input3 =="Cavaliers after game"){
      after_cavs_cloud <- getWordCloud('after_cavs.txt', 100, 300)
      print(after_cavs_cloud)
    }
    
    if(input3 =="Raptors before game"){
      raptors_cloud <- getWordCloud('raptors.txt', 100, 300)
      print(raptors_cloud)
    }
    
    if(input3 =="Raptors after game"){
      after_raptors_cloud <- getWordCloud('after_raptors.txt', 100, 300)
      print(after_raptors_cloud)
    }
    
  })
  
  output$plot4 <- renderPlot({
    
    input4 <- input$mode4
    
    if(input4 =="Positive/negative tweets about Cavaliers before game"){
      cavs_word_counts <- tidy_cavs %>% 
        inner_join(get_sentiments("bing")) %>% 
        count(word, sentiment, sort = TRUE)
      
      graph <- cavs_word_counts %>% 
        group_by(sentiment) %>% 
        top_n(10) %>% 
        ungroup() %>% 
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        labs(y = "Sentiments",
             x = NULL) +
        ggtitle("Tweets about Cavs before May 3rd game") +
        coord_flip()
      print(graph)
    }
    
    if(input4 =="Positive/negative tweets about Cavaliers after game"){
      raptors_word_counts <- tidy_raptors %>% 
        inner_join(get_sentiments("bing")) %>% 
        count(word, sentiment, sort = TRUE)
      
      graph <- raptors_word_counts %>% 
        group_by(sentiment) %>% 
        top_n(10) %>% 
        ungroup() %>% 
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        labs(y = "Sentiments",
             x = NULL) +
        ggtitle("Tweets about Raptors before May 3rd game") +
        coord_flip()
      print(graph)
    }
    
    if(input4 =="Positive/negative tweets about Raptors before game"){
      cavs_word_counts <- tidy_after_cavs %>% 
        inner_join(get_sentiments("bing")) %>% 
        count(word, sentiment, sort = TRUE)
      
      graph <- cavs_word_counts %>% 
        group_by(sentiment) %>% 
        top_n(10) %>% 
        ungroup() %>% 
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        labs(y = "Sentiments",
             x = NULL) +
        ggtitle("Tweets about Cavs after May 3rd game") +
        coord_flip()
      print(graph)
    }
    
    if(input4 =="Positive/negative tweets about Raptors after game"){
      raptors_word_counts <- tidy_after_raptors %>% 
        inner_join(get_sentiments("bing")) %>% 
        count(word, sentiment, sort = TRUE)
      
      graph <- raptors_word_counts %>% 
        group_by(sentiment) %>% 
        top_n(10) %>% 
        ungroup() %>% 
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(word, n, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        labs(y = "Sentiments",
             x = NULL) +
        ggtitle("Tweets about Raptors before May 3rd game") +
        coord_flip()
      print(graph)
    }
    
    if(input4 =="More analysis about Cavaliers tweets"){
      # Sentiment analysis with nrc
      nrc_joy <- get_sentiments("nrc") %>% 
        filter(sentiment == "joy")
      nrc_anger <- get_sentiments("nrc") %>% 
        filter(sentiment == "anger")
      nrc_anticipation <- get_sentiments("nrc") %>% 
        filter(sentiment == "anticipation")
      nrc_sadness <- get_sentiments("nrc") %>% 
        filter(sentiment == "sadness")
      
      # Cavs sentiment analysis with nrc
      cavs_joy <- tidy_cavs %>%
        inner_join(nrc_joy) %>%
        count(word, sort = TRUE) %>%
        filter(n > 10) %>%
        mutate(word = reorder(word, n))
      
      cavs_anger <- tidy_cavs %>%
        inner_join(nrc_anger) %>%
        count(word, sort = TRUE) %>%
        filter(n > 10) %>%
        mutate(word = reorder(word, n))
      
      cavs_anticipation <- tidy_cavs %>%
        inner_join(nrc_anticipation) %>%
        count(word, sort = TRUE) %>%
        filter(n > 10) %>%
        mutate(word = reorder(word, n))
      
      cavs_sadness <- tidy_cavs %>%
        inner_join(nrc_sadness) %>%
        count(word, sort = TRUE) %>%
        filter(n > 10) %>%
        mutate(word = reorder(word, n))
      
      graph <- gridExtra::grid.arrange(
        ggplot(cavs_joy, aes(word, n)) +
          geom_col() +
          xlab(NULL) +
          coord_flip() +
          ggtitle("Cavs Joy"),
        ggplot(cavs_anger, aes(word, n)) +
          geom_col() +
          xlab(NULL) +
          coord_flip() +
          ggtitle("Cavs Anger"),
        ggplot(cavs_anticipation, aes(word, n)) +
          geom_col() +
          xlab(NULL) +
          coord_flip() +
          ggtitle("Cavs Anticipation"),
        ggplot(cavs_sadness, aes(word, n)) +
          geom_col() +
          xlab(NULL) +
          coord_flip() +
          ggtitle("Cavs Sadness"),
        nrow=1
      )
      print(graph)
    }
    
    if(input4 =="More analysis about Raptors tweets"){
      nrc_joy <- get_sentiments("nrc") %>% 
        filter(sentiment == "joy")
      nrc_anger <- get_sentiments("nrc") %>% 
        filter(sentiment == "anger")
      nrc_anticipation <- get_sentiments("nrc") %>% 
        filter(sentiment == "anticipation")
      nrc_sadness <- get_sentiments("nrc") %>% 
        filter(sentiment == "sadness")
      
      # Raptors sentiment analysis with nrc
      raptors_joy <- tidy_raptors %>%
        inner_join(nrc_joy) %>%
        count(word, sort = TRUE) %>%
        filter(n > 10) %>%
        mutate(word = reorder(word, n))
      
      raptors_anger <- tidy_raptors %>%
        inner_join(nrc_anger) %>%
        count(word, sort = TRUE) %>%
        filter(n > 10) %>%
        mutate(word = reorder(word, n))
      
      raptors_anticipation <- tidy_raptors %>%
        inner_join(nrc_anticipation) %>%
        count(word, sort = TRUE) %>%
        filter(n > 10) %>%
        mutate(word = reorder(word, n))
      
      raptors_sadness <- tidy_raptors %>%
        inner_join(nrc_sadness) %>%
        count(word, sort = TRUE) %>%
        filter(n > 10) %>%
        mutate(word = reorder(word, n))
      
      graph <- gridExtra::grid.arrange(
        ggplot(raptors_joy, aes(word, n)) +
          geom_col() +
          xlab(NULL) +
          coord_flip() +
          ggtitle("Raptors Joy"),
        ggplot(raptors_anger, aes(word, n)) +
          geom_col() +
          xlab(NULL) +
          coord_flip() +
          ggtitle("Raptors Anger"),
        ggplot(raptors_anticipation, aes(word, n)) +
          geom_col() +
          xlab(NULL) +
          coord_flip() +
          ggtitle("Raptors Anticipation"),
        ggplot(raptors_sadness, aes(word, n)) +
          geom_col() +
          xlab(NULL) +
          coord_flip() +
          ggtitle("Raptors Sadness"),
        nrow=1
      )
      print(graph)
    }
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
