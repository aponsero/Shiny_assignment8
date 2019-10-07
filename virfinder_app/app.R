library(shiny)
library(ggplot2)
library(dplyr)
dataset1 <- read.csv("ex1_result.csv", stringsAsFactors = FALSE)
dataset2 <- read.csv("Vir1_v1_500pb_2kex_5-95.csv", stringsAsFactors = FALSE)

navbarPage("Navbar!",
           tabPanel("Plot",
                    sidebarLayout(
                        sidebarPanel(
                            radioButtons("plotType", "Plot type",
                                         c("Scatter"="p", "Line"="l")
                            )
                        ),
                        mainPanel(
                            plotOutput("plot")
                        )
                    )
           ),
           tabPanel("Summary",
                    verbatimTextOutput("summary")
           ))




ui <- navbarPage("Assignment 8",
    tabPanel("exercise 1",
        sidebarLayout(
            sidebarPanel(
                sliderInput("qvalInput", "Q-Value", min = 0, max = 1, value = c(0.5,1)),
                sliderInput("pvalInput", "P-Value", min = 0, max = 1, value = c(0.5,1)),
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
                   sliderInput("qvalInput2", "Q-Value", min = 0, max = 1, value = c(0.5,1)),
                   sliderInput("pvalInput2", "P-Value", min = 0, max = 1, value = c(0.5,1)),
                   numericInput("minlengthInput2", "Minimum contig length", 500, 
                                min = 500, max = 50000, step = 500)
               ),
               mainPanel(
                   # Output: Tabset w/ plot, summary, and table ----
                   tabsetPanel(type = "tabs",
                               tabPanel("Plot", plotOutput("coolplot2")),
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
                                & pvalue >= input$pvalInput[1] &  pvalue <= input$pvalInput[2]
                                & qvalue >= input$qvalInput[1] & qvalue <= input$qvalInput[2])
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
    
    #server for exercise 2
    filtered2 <- reactive({
        dataset2 %>% filter(length >= input$minlengthInput2 
                            & pvalue >= input$pvalInput2[1] &  pvalue <= input$pvalInput2[2])
    })
    
    output$coolplot2 <- renderPlot({
        ggplot(filtered2(), aes(length)) +
            geom_histogram()+
            ggtitle("Distribution of the selected contigs length")+
            xlab("length (bp)")+
            ylab("number of contigs")
    })
    
    output$results2 <- renderTable({
        filtered2()
    })
}
shinyApp(ui = ui, server = server)