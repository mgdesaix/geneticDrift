################################################################
################### Genetic Drift in Shiny #####################

library(rsconnect)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Genetic Drift Simulation"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("n1", "Effective Population Size: Pop1", 
                  value = 10, min = 0, 
                  max = 300, step = 10),
      sliderInput("n2", "Effective Population Size: Pop2", 
                  value = 50, min = 0, 
                  max = 300, step = 10),
      sliderInput("n3", "Effective Population Size: Pop3", 
                  value = 250, min = 0, 
                  max = 300, step = 10),
      sliderInput("t", "Generations", 
                  value = 100, min = 0, 
                  max = 500, step = 10),
      actionButton("go", label = "Plot")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Genetic Drift",
                 fluidRow(
                   column(width = 12, plotOutput("geneticdrift"))
                 ))
      )
    )
  )
)

server <- function(input, output){
  
  x <- eventReactive(input$go, {
    allele.mat1 <- matrix(nrow = (input$n1*2), ncol = input$t)
    allele.mat1[,1] <- rep(c(0, 1), input$n1)
    
    allele.mat2 <- matrix(nrow = (input$n2*2), ncol = input$t)
    allele.mat2[,1] <- rep(c(0, 1), input$n2)
    
    allele.mat3 <- matrix(nrow = (input$n3*2), ncol = input$t)
    allele.mat3[,1] <- rep(c(0, 1), input$n3)
    
    for(i in 1:(input$t-1)){
      allele.mat1[,i+1] <- sample(allele.mat1[,i], (input$n1*2), replace = T)
      allele.mat2[,i+1] <- sample(allele.mat2[,i], (input$n2*2), replace = T)
      allele.mat3[,i+1] <- sample(allele.mat3[,i], (input$n3*2), replace = T)
    }
    
    freq1 <- apply(allele.mat1, 2, function(x) sum(x)/(input$n1*2))
    freq2 <- apply(allele.mat2, 2, function(x) sum(x)/(input$n2*2))
    freq3 <- apply(allele.mat3, 2, function(x) sum(x)/(input$n3*2))
    
    df <- data.frame("AlleleFreq" = c(freq1,freq2,freq3),
                     "Generations" = rep(1:input$t, 3),
                     "Population" = rep(c("Pop1", "Pop2", "Pop3"), each = input$t))
  
    })
  
  output$geneticdrift <- renderPlot({
    ggplot(data = x(), aes(x = Generations, y = AlleleFreq)) +
      ylim(c(0,1)) +
      geom_point() +
      geom_line(aes(color = Population)) +
      theme_bw() +
      scale_color_discrete(labels = c(paste(input$n1, "Ne"),
                                      paste(input$n2, "Ne"),
                                      paste(input$n3, "Ne"))) +
      labs(color = "Population Size")
  })
  
}

shinyApp(ui, server)