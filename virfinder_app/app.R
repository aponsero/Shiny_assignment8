library(shiny)
library(ggplot2)
library(dplyr)
dataset1 <- read.csv("ex1_result.csv", stringsAsFactors = FALSE)
ui <- fluidPage(
    titlePanel("Assignment 8 - Viral sequence hunting"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("qvalInput", "Q-Value", min = 0, max = 1, value = 0.5),
            sliderInput("pvalInput", "P-Value", min = 0, max = 1, value = 0.5),
            numericInput("minlengthInput", "Minimum contig length", 500, 
                         min = 500, max = 50000, step = 500)
        ),
        mainPanel(
            plotOutput("coolplot"),
            br(), br(),
            tableOutput("results")
        )
    )
                )
server <- function(input, output) {
    
}

server <- function(input, output) {
    filtered <- reactive({
            dataset1 %>% filter(length >= input$minlengthInput & pvalue >= input$pvalInput
                                & qvalue >= input$qvalInput)
    })
    
    output$coolplot <- renderPlot({
        ggplot(filtered(), aes(length)) +
            geom_histogram()+
            ggtitle("Distribution of the selected contigs length")+
            xlab("length (bp)")+
            ylab("number of contigs")
    })
    
    output$results <- renderTable({
        filtered()
   })
}
shinyApp(ui = ui, server = server)