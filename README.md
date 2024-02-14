# test

library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(scales)
library(data.table)
library(ggExtra)


# sample_size 
# CI & Margin of Error
# Number of sample



ui <- page_sidebar(
  title = "Confidence Interval and Margin of error",
  # theme = bs_theme(bootswatch = "morph"),
  sidebar = sidebar(
    sliderInput("pop_size", "Population Size",
                min = 1000, max = 20000, value = 10000, step = 100),
    numericInput("mean", "Population Mean", value = 35, min = 0, max = 60),
    numericInput("sd", "Population St.d",value = 5, min = 1, max = 100),
    hr(),
    sliderInput("sample_size", "Sample Size", min = 5, max = 500, value = 380, step = 5),
    numericInput("ci", "Confidence Interval", value = 0.95, min = 0.9, max = 0.99)
  ),
  mainPanel(
    tabsetPanel(
      tabPanel(
        title = "Plot",
        fluidRow(plotlyOutput("normal"))
        ),
      tabPanel(
        title = "Population table",
        dataTableOutput("population")
        ),
      tabPanel(
        title = "Samples",
        dataTableOutput("samples")
        )
    )
  )
)

server <- function(input, output, session) {

  pop_df <- reactive({
    data.frame(pop = rnorm(n = input$pop_size,
                           mean = input$mean, sd = input$sd))
    })
  

  pop_stat <- reactive({
    pop_df() %>% 
      group_by() %>% 
      mutate(mean = mean(pop),
             sd = sd(pop))
  })
  
  samples <- reactive({
    pop_df() %>%
    sample_n(size = input$sample_size*30, replace = TRUE) %>%
    mutate(sample = rep(1:30, input$sample_size))
    })
  
  stat <- reactive({
    samples() %>%
      group_by(sample) %>%
      summarize(mean = mean(pop),
                n = n(),
                sd = sd(pop),
                se = sd/sqrt(n),
                prop = qt(p = 1-(1-input$ci)/2, df = n - 1),
                MoE = prop*se,
                LC = mean - prop*se,
                UC = mean + prop*se)
  })
  
  # output$average <- renderPlotly({
  #   q <- 
  #     ggplot()+
  #     stat_density(geom = "area",kernel = "gaussian", n = input$pop_size,
  #                  data = samples(), mapping = aes(x = pop, frame = sample),
  #                  fill = "indianred", alpha = 0.2, position = "identity")+
  #     geom_point(data = samples(),
  #                mapping = aes(x = pop, frame = sample, group = sample,
  #                              y = dnorm(mean(pop), input$mean, input$sd))
  #                )+
  #     geom_vline(xintercept = input$mean, lty = 2, col = "indianred")
  #   ggplotly(q)
  #   
  # })
  
  
  output$normal <- renderPlotly({
    req(pop_df())
    p <- ggplot(data = pop_df()) +
      geom_point(data = stat(), aes(x = mean, y = 0, frame = sample), color = "red")+
      geom_histogram(samples(),
                     mapping = aes(x = pop, y = ..density.., frame = sample),
                     position = "identity",
                     alpha = 0.3,
                     fill = "steelblue") + 
      geom_segment(samples(),
                   mapping = aes(x = input$mean, xend = input$mean,
                                 y = 0, yend = dnorm(mean(pop_df()$pop), mean(pop_df()$pop), sd(pop_df()$pop)),
                                 frame = sample),
                   color = "red", lty = 3
      )+
    
      geom_rect(data = stat(),
                mapping = aes(
                  xmin = LC, xmax = UC,
                  ymin = 0,
                  ymax = dnorm(UC, mean(pop_df()$pop), sd(pop_df()$pop)):dnorm(LC, mean(pop_df()$pop), sd(pop_df()$pop)),
                  frame = sample
                  ),
                fill = "#14418B", alpha = 0.5)+
      
      geom_segment(stat(),
                   mapping = aes(x = mean, xend = mean,
                                 y = 0, yend = dnorm(mean, input$mean, input$sd),
                                 frame = sample),
                   color = "darkblue", lty = 2
      )+
      geom_text(data = stat(),
                aes(x = mean, y = 2*dnorm(mean, mean = mean, sd = sd),
                    label = paste0("Population Mean: ",
                                   round(mean(pop_df()$pop), 3),
                                   "\n","Sample Mean: ",
                                   round(mean,3),
                                   "\n","Margin of Error: ",
                                   MoE
                                   ), frame = sample),
                size = 3
                )+
      
      theme_minimal()
    return(ggplotly(p))
  })
  
  output$population <- renderDataTable({
    
    stat() %>% head(100)
    
  })
  
  output$samples <- renderDataTable({
    
    samples()
    
  })
}

shinyApp(ui, server)
