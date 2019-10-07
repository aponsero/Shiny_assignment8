library(shiny)
library(ggplot2)
library(dplyr)
dataset1 <- read.csv("ex1_result.csv", stringsAsFactors = FALSE)
dataset2 <- read.csv("ex2_data.csv", stringsAsFactors = FALSE)

ui <- navbarPage("Assignment 8",
    tabPanel("exercise 1",
        sidebarLayout(
            sidebarPanel(
                sliderInput("scoreInput", "Score", min = 0, max = 1, value = c(0,1)),
                sliderInput("qvalInput", "Q-Value", min = 0, max = 1, value = c(0,1)),
                sliderInput("pvalInput", "P-Value", min = 0, max = 1, value = c(0,1)),
                numericInput("minlengthInput", "Minimum contig length", 500, 
                             min = 500, max = 50000, step = 500)
            ),
            mainPanel(
                # Output: Tabset w/ plot, summary, and table ----
                tabsetPanel(type = "tabs",
                            tabPanel("Plot", plotOutput("coolplot")),
                            tabPanel("Table", tableOutput("results"))
                )
            )
        )
    ),tabPanel("Exercise 2",
           sidebarLayout(
               sidebarPanel(
                   sliderInput("scoreInput2", "Score", min = 0, max = 1, value = c(0,1)),
                   sliderInput("qvalInput2", "Q-Value", min = 0, max = 1, value = c(0,1)),
                   sliderInput("pvalInput2", "P-Value", min = 0, max = 1, value = c(0,1)),
                   numericInput("minlengthInput2", "Minimum contig length", 500, 
                                min = 500, max = 50000, step = 500)
               ),
               mainPanel(
                   # Output: Tabset w/ plot, summary, and table ----
                   tabsetPanel(type = "tabs",
                               tabPanel("Metrics", tableOutput("metrictable2")),
                               tabPanel("Table", tableOutput("results2"))
                   )
               )
           )
    )
    
)
server <- function(input, output) {
    
}

server <- function(input, output) {
    # server for exercise1
    filtered <- reactive({
            dataset1 %>% filter(length >= input$minlengthInput 
                                & score >= input$scoreInput[1] &  score <= input$scoreInput[2]
                                & pvalue >= input$pvalInput[1] &  pvalue <= input$pvalInput[2]
                                & qvalue >= input$qvalInput[1] & qvalue <= input$qvalInput[2])
    })
    
    output$coolplot <- renderPlot({
        ggplot(filtered(), aes(length)) +
            geom_histogram()+
            ggtitle("Distribution of the selected contigs length")+
            xlab("length (bp)")+
            ylab("number of contigs")+
            xlim(0, 40000)
    })
    
    output$results <- renderTable({
        filtered()
   })
    
    #server for exercise 2
    filtered2 <- reactive({
        dataset2 %>% filter(length >= input$minlengthInput2 
                            & score >= input$scoreInput2[1] &  score <= input$scoreInput2[2]
                            & pvalue >= input$pvalInput2[1] &  pvalue <= input$pvalInput2[2]
                            & qvalue >= input$qvalInput2[1] & qvalue <= input$qvalInput2[2])
    })
    recall_precision <- reactive({
        tot<-nrow(filtered2())
        filtered2() %>% filter(viral == 1) %>% tally() %>% rename(true_positives=n) %>%
            mutate(recall = true_positives/100, precision = true_positives/tot)
    })
    fallout_fdr <- reactive({
        tot<-nrow(filtered2())
        filtered2() %>% filter(viral == 0) %>% tally() %>% rename(false_positives=n) %>%
            mutate(fall_out = false_positives/1900, FDR = false_positives/tot)
    })
    metrics2 <- reactive({
        bind_cols(fallout_fdr(), recall_precision())
    })
    output$metrictable2 <- renderTable({
        metrics2()
    })
    
    output$results2 <- renderTable({
        filtered2()
    })
}
shinyApp(ui = ui, server = server)