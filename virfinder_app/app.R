library(shiny)
library(ggplot2)
library(dplyr)
dataset1 <- read.csv("ex1_data.csv", stringsAsFactors = FALSE)
dataset1b <- read.csv("ex1b_data.csv", stringsAsFactors = FALSE)
dataset2 <- read.csv("ex2_result.csv", stringsAsFactors = FALSE)

ui <- navbarPage("Assignment 9",
    tabPanel("exercise 1",
             sidebarLayout(
                 sidebarPanel(
                     sliderInput("scoreInput", "Score", min = 0, max = 1, value = c(0,1)),
                     selectInput("dataset", "dataset:",
                                 c("50% viral" = "dataset1",
                                   "10% viral" = "dataset1b"))
                 ),
                 mainPanel(
                     # Output: Tabset w/ plot, summary, and table ----
                     tabsetPanel(type = "tabs",
                                 tabPanel("Metrics", tableOutput("metrictable")),
                                 tabPanel("Table", tableOutput("results"))
                     )
                 )
             )
    ),tabPanel("Exercise 2",
               sidebarLayout(
                   sidebarPanel(
                       sliderInput("scoreInput2", "Score", min = 0, max = 1, value = c(0,1)),
                       sliderInput("qvalInput2", "Q-Value", min = 0, max = 1, value = c(0,1)),
                       numericInput("minlengthInput2", "Minimum contig length", 500, 
                                    min = 500, max = 50000, step = 500)
                   ),
                   mainPanel(
                       # Output: Tabset w/ plot, summary, and table ----
                       tabsetPanel(type = "tabs",
                                   tabPanel("Plot", plotOutput("plot2")),
                                   tabPanel("Table", tableOutput("results2"))
                       )
                   )
               )
    )
    
)

server <- function(input, output) {
    # server for exercise1
    filtered <- reactive({
        if (input$dataset == "dataset1") {choice_dataset<-dataset1}
        else{choice_dataset<-dataset1b}
        choice_dataset %>% filter(score >= input$scoreInput[1] &  score <= input$scoreInput[2])
    })
    recall_precision <- reactive({
        if (input$dataset == "dataset1") {nb_pos=1000}
        else{nb_pos=100}
        tot<-nrow(filtered())
        filtered() %>% filter(viral == 1) %>% tally() %>% rename(true_positives=n) %>%
            mutate(recall = true_positives/nb_pos, precision = true_positives/tot)
    })
    fallout_fdr <- reactive({
        if (input$dataset == "dataset1") {nb_neg=1000}
        else{nb_neg=1900}
        tot<-nrow(filtered())
        filtered() %>% filter(viral == 0) %>% tally() %>% rename(false_positives=n) %>%
            mutate(fall_out = false_positives/nb_neg, FDR = false_positives/tot)
    })
    metrics <- reactive({
        bind_cols(fallout_fdr(), recall_precision())
    })
    output$metrictable <- renderTable({
        metrics()
    })
    output$results <- renderTable({
        filtered()
    })
    #server for exercise 2
    filtered2 <- reactive({
        dataset2 %>% filter(length >= input$minlengthInput 
                            & score >= input$scoreInput2[1] &  score <= input$scoreInput2[2]
                            & qvalue >= input$qvalInput2[1] & qvalue <= input$qvalInput2[2])
    })
    
    output$plot2 <- renderPlot({
        ggplot(filtered2(), aes(length)) +
            geom_histogram()+
            ggtitle("Distribution of the selected contigs length")+
            xlab("length (bp)")+
            ylab("number of contigs")+
            xlim(0, 40000)
    })
    
    output$results2 <- renderTable({
        filtered2()
    })
}
shinyApp(ui = ui, server = server)