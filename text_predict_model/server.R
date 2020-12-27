#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(textclean)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    # load dictionary
    dic_tr_samp_vec <- readRDS("dic_tr_samp_vec.rds")
    dic_bi_samp_vec <- readRDS("dic_bi_samp_vec.rds")
    dic_un_samp_vec <- readRDS("dic_un_samp_vec.rds")
    dic_ngram       <- readRDS("dic_ngram.rds")
    
    # clean the input text    
    text_cln <- reactive({
        x <- tolower(input$text)
        x <- str_replace_all(x, "’", "'")
        x <- replace_contraction(x)
        x <- str_replace_all(x, pattern = "\\d", replacement = "")
        x <- str_replace_all(x, pattern = "[:punct:]", replacement = "")
        x <- replace_white(x)
        str_split(as_tibble(x), " ")[[1]]
    })
    
    # check the length of input    
    text_len <- reactive({
        length(text_cln())
    })

    # Search for a different dictionary depending on the length of text entered
    input_tr <- reactive({
        paste(tail(text_cln(), 2), collapse = " ")
    })
    input_bi <- reactive({
        paste(tail(text_cln(), 1), collapse = " ")
    })


    tr <- reactive({
        dic_tr_samp_vec[grep(paste0("^",input_tr()), dic_tr_samp_vec)[1]]
    })
    bi <- reactive({
        dic_bi_samp_vec[grep(paste0("^",input_bi()), dic_bi_samp_vec)[1]]
    })
    un <- reactive({
        dic_un_samp_vec[1]
    })
    
    result <- reactive({
        ifelse(input$text =="", "text",
               ifelse(text_len() >=2, (ifelse(!is.na(tr()), tr(), ifelse(!is.na(bi()),bi(),un()))),
                      ifelse(text_len() %in% c(1,2), (ifelse(!is.na(bi()),bi(),un())), "text"))
              )
    })
    
    # text output
    output$answer <- renderText({
        paste(input$text, tail(str_split(result(), " ")[[1]], 1))
    })
    
    # table output
    output$table <- DT::renderDT({
        dic_ngram %>% group_by(category) %>% 
            filter(category == input$category,
                   number_of_text >= input$n)
    })
    
    #url output
    url <- a("n_gram", href="https://en.wikipedia.org/wiki/N-gram")
    output$url <- renderUI({
        tagList("URL link:", url)
    })
})
