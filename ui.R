library(shiny)
library(DT)
library(tidyverse)
library(wordcloud2)
library(tidytext)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(width = 4,
                 textInput(inputId = 'q', label = 'Enter Job Title:', 
                           value = 'Data analysis'),
                 textInput(inputId = 'l', label = 'Enter Input Job Location:', 
                           value = ''),
                 sliderInput(inputId = 'mp', label = 'Maximum Pages', min = 1, max = 100, 
                             value = 5, step = 1),
                 actionButton(inputId = 'act', label = 'Run', class = 'btn-success'),
                 br(),br(),br(),
                 downloadButton("downloadData", label = "Download Data As CSV File")
    ),
    mainPanel(width = 8,
              tabsetPanel(
                tabPanel(title = 'Visualization', 
                         plotOutput('p1'),
                         wordcloud2Output('p2')),
                tabPanel(title = 'Table', DTOutput('dt1'))
              )
    )
  )
)


