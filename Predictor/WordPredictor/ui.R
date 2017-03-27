#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    
    
    #Application title
    titlePanel("Capstone project"),
    #Explanations in the sidebar Panel
    sidebarPanel(
      h4("Introduction"),
      p("This project is the final project for Data Science Specialization "),
      h4("Text Prediction Model"),
      p("The model used for this text prediction was based on the probablilty of different Ngrams by using Markov chains."),
      h4("How to use it?"),
      p("1. Introduce a text for prediction under the text box headed \"Text for prediction\": Numerical characters, separators,... are neglected"),
      p("2. Select the number of words with the slide box: The minimum is 1 and the maximum 3, the default value is 1"),
      p("3. Click \"Predict\" button once the input string has been introduced."),
      p("4. The result is shown under the label \"Prediction outcome\".")
    ),
    #All the controls are in the main panel
    mainPanel(
      textInput("txtInBox", h5("Text for prediction")),
      
      sliderInput("NumOfPredictingWords",
                  h5("Number of predicting words:"),
                  min = 1,
                  max = 3,
                  value = 1,
                  width = validateCssUnit("300px")),
      
      actionButton("Predict", h5("Predict")),
      
      h5("Prediction outcome: "),
      
      h5(textOutput("txtOutBox"))
    )
    
  )
  
)



