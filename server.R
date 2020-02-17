#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    
    dataset <- reactive({
        
        if (is.null(input$file)) {return(NULL)}
        else {
            
            dataset = readLines(input$file$datapath)
            return(dataset)}
        
    })
    
    #num_sents <- reactive({ if (is.null(input$num)) {return(5)} })
    #num_sents <- input$num
    
        require(dplyr)
        require(magrittr)
        require(tidytext)
        
    ## load data into tibble
    article_sentences <- reactive({
            
            article_sentences = tibble(text = dataset()) %>%
            unnest_tokens(sentence, text, token = "sentences", to_lower=FALSE) %>%    # sentence-tokenizing the article   
            mutate(sentence_id = row_number()) %>%    # insert setence_id
            select(sentence_id, sentence)  # drop frivolous stuff

        return(article_sentences)
        })
    
    output$article_sentences <- renderTable({ article_sentences() })
    

    output$output1 <- renderTable({
        
        ## word-tokenize too. for IDing keywords
        article_words = article_sentences() %>%
            unnest_tokens(word, sentence) %>%
            # drop stopwords
            anti_join(stop_words, by = "word")
    
        ## print summary
        article_summary <- textrank_sentences(data = article_sentences(), 
                                              terminology = article_words)
            
#        a0 = data.frame(article_summary$sentences)
#        a1 = order(a0$textrank, decreasing=TRUE)
#        summ_sents = a0$sentence[a1[1:input$num]] # %>% tibble()
        
        summ_sents = article_summary[["sentences"]] %>%
            arrange(desc(textrank)) %>% 
            slice(1:input$num) %>%  # dplyr::slice() chooses rows by their ordinal position in the tbl
            pull(sentence) %>% tibble()
        
        return(summ_sents)
        
    })
    
    output$output1 <-  renderText({ 
        "You have selected this"
    })
        
    
}) # shinyServer func ends
