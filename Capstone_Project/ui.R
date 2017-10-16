library(shiny)
fluidPage(
      
      textInput("text", label = h4("Enter your phrase, and click submit to predict the next word"),value = "Enter text ...",width = '100%'),
      submitButton("Submit"),
      hr(),
      verbatimTextOutput("value")
      
)

