library(shiny)
shinyUI(ui = pageWithSidebar(
    headerPanel("Stock Market Prediction in 2010: Model Comparison"),
    
    sidebarPanel(
        radioButtons(inputId = 'model_input', label = 'Choose Prediction Model',
                           choices = c('Logistic Regression', 'LDA', 'QDA', 'KNN'),
                           selected = c('Logistic Regression')),
        checkboxGroupInput(inputId = 'variables', label = 'Choose Predictors',
                           choices = c('Lag1', 'Lag2', 'Lag3', 'Lag4', 'Lag5'),
                           selected = c('Lag1', 'Lag2', 'Lag3', 'Lag4', 'Lag5')),
        sliderInput('threshold', 'Choose Probability Threshold  (LR, LDA and QDA only)',value = 0.5,
                    min = 0.4, max = 0.6, step = 0.01,),
        sliderInput('k',
                    'Choose Number of Nearest Neighbors (KNN only)',value = 5,
                    min = 0, max = 20, step = 1,),
        submitButton('Run Prediction')
        ),
    
    mainPanel(
        p('Pairwise Plot of Selected Predictors'),
        plotOutput('pairs'),
        p('2010 Weekly Prediction Results'), verbatimTextOutput('results'),
        p('Model Summary'), verbatimTextOutput('summary'),
        p('Prediction Model'), textOutput('model_input'),
        p('Predictors'), textOutput('variables'),
        p('Probability Threshold'), textOutput('threshold')
    ))
)
