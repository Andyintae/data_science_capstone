library(shiny)
library(tidyverse)
library(textclean)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    #Shiny theme
    theme = shinytheme("united"),
    
    # Application title 
    titlePanel("Make your text automatically"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h3("1. Predict"),
            textInput("text", "Input your text"),
            h5(" "),
            h3("2. Dictionary"),
            checkboxGroupInput("category", "select ngram type", 
                               choices = unique(dic_ngram$category),
                               selected = unique(dic_ngram$category)),
            sliderInput("n", "number of text", 1, 10000, 5),
            
            submitButton("Run"),

        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("1. Predict", 
                         h4("Please wait after click the Run button"  ),
                         textOutput("answer")),
                tabPanel("2. Dictionary",
                         DT::DTOutput("table")),
                tabPanel("How to use",
                         h3("1. Predict :"),
                         h4("This app is built to predict a word that are expected to follow the words or phrase you enter.  
                            Just type some word or phrase in the box left.  
                            And please wait until it is shown in the Predict tab.
                            Please enjoy this app and thank you for your review!"),
                         h3("2. Dictionary :"),
                         h4("This app is built to see the dictionary for prediction. You can find what ngram is in the URL link below:"),
                         uiOutput("url")
                         
                )
            )
            
        )
    )
))