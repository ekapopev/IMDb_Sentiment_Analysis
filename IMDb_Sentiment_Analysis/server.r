#SERVER.R
# Server is a function used to render the objects created in the User Interface function of the shiny Application
# It takes the input and output as an argument
options(shiny.maxRequestSize=35*1024^2, warn=-1)
server = function(input, output, session)
{
  
  # ## Data Frequency Plot - TM ##
  # Creating  reactive to the input actionButton 'goButton' that was created in the the ui function
  # eventReactive - Responds to "event-like" reactive inputs, values, and expressions.
  data1 = eventReactive(input$goButton, {

      if (input$typeInput == "fileUpload") {
        
        df <- filedata()
        
      } else if (input$typeInput == "typeReview"){
        text = input$reviewInput
        df <- data.frame(text)
        
      }
    
    dtm_df <- create_tdm_df(df)
    
    })

  # ## Rendering Frequency Plot - TM ##
  # renderPlot - Renders a reactive plot that is suitable for assigning to an output slot.
  # In this case the the objects used are plot1
  output$plot1 = renderPlot({

      # ## creating Barplot for emotions ##
        barplot(data1()[1:10,]$freq, las = 2, names.arg = data1()[1:10,]$word,
                cex.names = 0.8,
                las = 1,
                main ="Most frequent words",#col ="lightblue", 
                ylab = "Frequency")},
          width = 700, height = 500)
  
  
  # ## Data Frequency Plot - TFIDF ##
  # Creating  reactive to the input actionButton 'goButton' that was created in the the ui function
  # eventReactive - Responds to "event-like" reactive inputs, values, and expressions.
  data2 = eventReactive(input$goButton, {
    
    if (input$typeInput == "fileUpload") {
      
      df <- filedata()
      
    } else if (input$typeInput == "typeReview"){
      text = input$reviewInput
      df <- data.frame(text)
      
    }
    
    dtm_df <- create_tfidf_df(df)
    
  })
  
  # ## Rendering Frequency Plot - TFIDF ##
  # renderPlot - Renders a reactive plot that is suitable for assigning to an output slot.
  # In this case the the objects used are plot2
  output$plot2 = renderPlot({
    
    # ## creating Barplot for emotions ##
    barplot(data2()[1:10,]$freq, las = 2, names.arg = data2()[1:10,]$word,
            cex.names = 0.8,
            las = 1,
            main ="Most frequent words - TFIDF",#col ="lightblue", 
            ylab = "TFIDF Frequency")},
    width = 700, height = 500)
  
  
  # ## Data Frequency Plot TM - Positive ##
  # Creating  reactive to the input actionButton 'goButton' that was created in the the ui function
  # eventReactive - Responds to "event-like" reactive inputs, values, and expressions.
  data3 = eventReactive(input$goButton, {
    
    if (input$typeInput == "fileUpload") {
      
      df <- filedata2()
      
    } else if (input$typeInput == "typeReview"){
      df <- filedata2()
      
    }
    
    validate(need(
      any(df[,2] %in% "Positive"),"Positive reviews not available, please change Selection!!"))
    
    df <- subset(df, df[,2] == "Positive")
    
    dtm_df <- create_tdm_df(df)
    
  })
  
  # ## Rendering Frequency Plot TM - Positive ##
  # renderPlot - Renders a reactive plot that is suitable for assigning to an output slot.
  # In this case the the objects used are plot3
  output$plot3 = renderPlot({
    
    # ## creating Barplot for emotions ##
    barplot(data3()[1:10,]$freq, las = 2, names.arg = data3()[1:10,]$word,
            cex.names = 0.8,
            las = 1,
            main ="Most frequent words - Positive",#col ="lightblue", 
            ylab = "Frequency")},
    width = 700, height = 500)
  
  
  # ## Data Frequency Plot TM - Negative ##
  # Creating  reactive to the input actionButton 'goButton' that was created in the the ui function
  # eventReactive - Responds to "event-like" reactive inputs, values, and expressions.
  data4 = eventReactive(input$goButton, {
    
    if (input$typeInput == "fileUpload") {
      
      df <- filedata2()
      
    } else if (input$typeInput == "typeReview"){

      df <- filedata2()
      
    }
    
    validate(need(
      any(df[,2] %in% "Negative"),"Negative reviews not available, please change Selection!!"))
    
    df <- subset(df, df[,2] == "Negative")
    
    dtm_df <- create_tdm_df(df)
    
  })
  
  # ## Rendering Frequency Plot TM - Negative ##
  # renderPlot - Renders a reactive plot that is suitable for assigning to an output slot.
  # In this case the the objects used are plot4
  output$plot4 = renderPlot({
    
    # ## creating Barplot for emotions ##
    barplot(data4()[1:10,]$freq, las = 2, names.arg = data4()[1:10,]$word,
            cex.names = 0.8,
            las = 1,
            main ="Most frequent words - Negative",#col ="lightblue", 
            ylab = "Frequency")},
    width = 700, height = 500)
  

  # ## Data Frequency Plot TFIDF - Positive ##
  # Creating  reactive to the input actionButton 'goButton' that was created in the the ui function
  # eventReactive - Responds to "event-like" reactive inputs, values, and expressions.
  data5 = eventReactive(input$goButton, {
    
    if (input$typeInput == "fileUpload") {
      
      df <- filedata2()
      
    } else if (input$typeInput == "typeReview"){
      
      df <- filedata2()
      
    }
    
    validate(need(
      any(df[,2] %in% "Positive"),"Positive reviews not available, please change Selection!!"))
    
    df <- subset(df, df[,2] == "Positive")
    
    dtm_df <- create_tfidf_df(df)
    
  })
  
  # ## Rendering Frequency Plot TFIDF - Positive ##
  # renderPlot - Renders a reactive plot that is suitable for assigning to an output slot.
  # In this case the the objects used are plot5
  output$plot5 = renderPlot({
    
    # ## creating Barplot for emotions ##
    barplot(data5()[1:10,]$freq, las = 2, names.arg = data5()[1:10,]$word,
            cex.names = 0.8,
            las = 1,
            main ="Most frequent words - TFIDF - Positive",#col ="lightblue", 
            ylab = "Frequency")},
    width = 700, height = 500)
  
  
  # ## Data Frequency Plot TFIDF - Negative ##
  # Creating  reactive to the input actionButton 'goButton' that was created in the the ui function
  # eventReactive - Responds to "event-like" reactive inputs, values, and expressions.
  data6 = eventReactive(input$goButton, {
    
    if (input$typeInput == "fileUpload") {
      
      df <- filedata2()
      
    } else if (input$typeInput == "typeReview"){
      
      df <- filedata2()
      
    }
    
    validate(need(
      any(df[,2] %in% "Negative"),"Negative reviews not available, please change Selection!!"))
    
    df <- subset(df, df[,2] == "Negative")
    
    dtm_df <- create_tfidf_df(df)
    
  })
  
  # ## Rendering Frequency Plot TFIDF - Negative ##
  # renderPlot - Renders a reactive plot that is suitable for assigning to an output slot.
  # In this case the the objects used are plot6
  output$plot6 = renderPlot({
    
    # ## creating Barplot for emotions ##
    barplot(data6()[1:10,]$freq, las = 2, names.arg = data6()[1:10,]$word,
            cex.names = 0.8,
            las = 1,
            main ="Most frequent words - TFIDF - Negative",#col ="lightblue", 
            ylab = "Frequency")},
    width = 700, height = 500)
  
  
  # renderWordcloud2 - Renders a reactive word cloud that is suitable for assigning to an output slot.
  # In this case the the object used is wordCloud1 to wordCloud4
  # ## Render Wordcloud TM - Positive ##
  output$wordCloud1 = renderWordcloud2({wordcloud2(data = data3())})
  # ## Render Wordcloud TM - Negative ##
  output$wordCloud2 = renderWordcloud2({wordcloud2(data = data4())})
  # ## Render Wordcloud TFIDF - Positive ##
  output$wordCloud3 = renderWordcloud2({wordcloud2(data = data5())})
  # ## Render Wordcloud TFIDF - Negative ##
  output$wordCloud4 = renderWordcloud2({wordcloud2(data = data6())})
  
  
  # ## Data Words Cooccurences Network ##
  # Creating  reactive to the input actionButton 'goButton' that was created in the the ui function
  # eventReactive - Responds to "event-like" reactive inputs, values, and expressions.
  data7 = eventReactive(input$goButton, {
    
    if (input$typeInput == "fileUpload") {
      
      df <- filedata()
      
    } else if (input$typeInput == "typeReview"){
      text = input$reviewInput
      df <- data.frame(text)
      
    }
    
    comments <- sapply(df$text,function(x) iconv(x, 'utf8', 'ascii',""))
    comments <- VCorpus(VectorSource(comments))
    # transform to lower
    comments <- tm_map(comments,content_transformer(tolower))
    # define stopwords
    forremoval <- c(stopwords('english'),"movie","film","one")
    # remove stop words
    comments <- tm_map(comments, removeWords,forremoval)
    comments <- data.frame(text=unlist(sapply(comments, `[`, "content")), stringsAsFactors=F)
    
    # input list of reviews
    input_text <- tibble(ind=seq.int(nrow(comments)),text=comments$text)
    # library(tidytext) -> unnest words
    input_words <- input_text %>% unnest_tokens(word, text)
    word_cooccurences <- pairwise_count(input_words,word,ind,sort = TRUE,diag=FALSE)
    
  })
  
  # ## Rendering Words Cooccurences Network ##
  # renderPlot - Renders a reactive plot that is suitable for assigning to an output slot.
  # In this case the the objects used are plot7
  output$plot7 = renderPlot({
    
    # Plot the cooccurences graph
    head(data7(), min(300,nrow(data7()))) %>%
      filter(n >= mean(data7()$n)) %>%
      graph_from_data_frame() %>%
      ggraph(layout = "fr") +
      geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "grey") +
      geom_node_point(size = 3) +
      geom_node_text(aes(label = name), repel = TRUE, 
                     point.padding = unit(0.2, "lines"))},
    width = 700, height = 500)
  
  ###### Reading CSV Files ########
  ###### Uploaded Data ########
  filedata <- reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    f=read.csv(inFile$datapath,stringsAsFactors=FALSE)
    
    validate(need(
      any(names(f) %in% "text"),'Error: Reviews column should be named as "text" in the uploaded data'))
    
    return(f)
    
  })
  
  
  filedata_clean <- reactive({
    
    inFile <- input$file2
    
    if (is.null(inFile))
      return(NULL)
    
    f=read.csv(inFile$datapath,stringsAsFactors=FALSE)
    
    return(f)
    
  })
  
  # ## Data Sentiment Class ##
  # Creating  reactive to the input actionButton 'goButton' that was created in the the ui function
  # eventReactive - Responds to "event-like" reactive inputs, values, and expressions.
  # Data frame with Uploaded text and predicted sentiment
  filedata2 = eventReactive(input$goButton, {
    
    wordlist <- reactiveFileReader(1000, session, "./Data/wordListSpelling.RData", LoadToEnvironment)
    tr_model <- reactiveFileReader(1000, session, "./Data/tr_model.Rdata", LoadToEnvironment)
    glmnet_clas <- reactiveFileReader(1000, session, "./Data/glmnet_clas.Rdata", LoadToEnvironment)
    
    if (input$typeInput == "fileUpload") {
      text = filedata()$text
      
      test <- clean_reviews(filedata(),
                            wordlist()[[names(wordlist())[1]]])
      
      Sentiment <- sentiment_prediction(test, tr_model()[[names(tr_model())[1]]],
                                        glmnet_clas()[[names(glmnet_clas())[1]]])
      
      df.imdb = data.frame(text, Sentiment)
    } else if (input$typeInput == "typeReview"){
      text = input$reviewInput
      
      df <- data.frame(text)
      
      test <- clean_reviews(df,
                            wordlist()[[names(wordlist())[1]]])
      
      Sentiment <- sentiment_prediction(test, tr_model()[[names(tr_model())[1]]],
                                        glmnet_clas()[[names(glmnet_clas())[1]]])
      
      df.imdb = data.frame(text, Sentiment)
    } else if (input$typeInput == "cleanUpload") {
      text = filedata_clean()$text
      
      test <- filedata_clean()[,names(filedata_clean()) %in% c("text","elongated_words_freq","rating_words_value",
                                                              "elongated_sentiment","exclamed_sentiment","is_elongated",
                                                              "is_exclaimed","capital_freq","is_capital")]
      
      Sentiment <- sentiment_prediction(test)
      
      df.imdb = data.frame(text, Sentiment)
    } else {}
    
    #text = paste0(substr(filedata()$text, 1, 100),"...")
    
    imdbOutput = df.imdb
  })
  
  # ## Render Sentiment Class ##
  # renderDataTable - Renders a reactive data table that is suitable for assigning to an output slot.
  # In this case the the object used is imdbTable
  output$imdbTable = renderDataTable({filedata2()}, options = list(lengthMenu = c(10, 30, 50), pageLength = 5))
  
  
  # ## Data Overall Sentiment Emotions ##
  # Creating  reactive to the input actionButton 'goButton' that was created in the the ui function
  # eventReactive - Responds to "event-like" reactive inputs, values, and expressions.
  data8 = eventReactive(input$goButton, {
    
    if (input$typeInput == "fileUpload") {
      
      df <- filedata()
      
    } else if (input$typeInput == "typeReview"){
      text = input$reviewInput
      df <- data.frame(text)
      
    }
    
    
  })  
  
  # ## Rendering Overall Sentiment Emotions ##
  # renderPlot - Renders a reactive plot that is suitable for assigning to an output slot.
  # In this case the the objects used are plot7
  output$plot8 = renderPlot({
    
    
    sentiment_result <-getSentiments.TF_IDF.nrc(tdm.TFIDF(data8()))
    
    barplot(
      sort(colSums(prop.table(sentiment_result[, 1:8]))), 
      horiz = TRUE, 
      cex.names = 0.75, 
      las = 1, 
      main = "Emotions in reviews",
      xlab="Percentage",
      xlim = c(0,max(colSums(prop.table(sentiment_result[, 1:8])))+0.05))},
    width = 700, height = 500)
  
  
  # ## Rendering Overall Sentiment Result ##
  # renderPlot - Renders a reactive plot that is suitable for assigning to an output slot.
  # In this case the the objects used are plot7
  output$plot9 = renderPlot({
    
    review_pos <- ifelse(filedata2()$Sentiment == "Positive",1,0 )
    review_neg <- ifelse(filedata2()$Sentiment == "Negative",1,0 )
    sentiment_table <- tibble(Positive=review_pos,Negative=review_neg)
    
    barplot(
      sort(colSums(prop.table(sentiment_table))), 
      horiz = TRUE,
      cex.names = 0.75,
      las = 1,
      main = "Ratio of positive and negative",
      xlab="Percentage",
      xlim = c(0,max(colSums(prop.table(sentiment_table)))+0.05)
    )
  },
  width = 700, height = 500)

}



