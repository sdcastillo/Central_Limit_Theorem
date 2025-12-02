library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(cowplot)

ui <- fluidPage(
  titlePanel("Central Limit Theorem Demonstration"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("family", "Distribution Family:", 
                  choices = c("exponential", "uniform", "normal", "poisson", 
                              "cauchy", "binomial", "gamma", "chi-square", "student-t")),
      
      numericInput("r", "Number of Samples (Replications):", value = 1000, min = 100, max = 10000),
      numericInput("m", "Sample Size (n per sample):", value = 30, min = 5, max = 500),
      
      conditionalPanel(
        condition = "input.family == 'exponential'",
        numericInput("para1", "Rate (λ):", value = 1, min = 0.1, step = 0.1)
      ),
      conditionalPanel(
        condition = "input.family == 'uniform'",
        numericInput("para1", "Min:", value = 0),
        numericInput("para2", "Max:", value = 1)
      ),
      conditionalPanel(
        condition = "input.family == 'normal'",
        numericInput("para1", "Mean:", value = 0),
        numericInput("para2", "SD:", value = 1, min = 0.01)
      ),
      conditionalPanel(
        condition = "input.family == 'poisson'",
        numericInput("para1", "Lambda (λ):", value = 5, min = 1)
      ),
      conditionalPanel(
        condition = "input.family == 'cauchy'",
        numericInput("para1", "Location:", value = 0),
        numericInput("para2", "Scale:", value = 1, min = 0.01)
      ),
      conditionalPanel(
        condition = "input.family == 'binomial'",
        numericInput("para1", "Trials (size):", value = 10, min = 1),
        numericInput("para2", "Probability (p):", value = 0.5, min = 0, max = 1, step = 0.01)
      ),
      conditionalPanel(
        condition = "input.family == 'gamma'",
        numericInput("para1", "Shape (α):", value = 2, min = 0.1),
        numericInput("para2", "Rate (β):", value = 1, min = 0.1)
      ),
      conditionalPanel(
        condition = "input.family == 'chi-square'",
        numericInput("para1", "Degrees of Freedom:", value = 4, min = 1)
      ),
      conditionalPanel(
        condition = "input.family == 'student-t'",
        numericInput("para1", "Degrees of Freedom:", value = 3, min = 1)
      ),
      
      actionButton("go", "Redraw Plots")
    ),
    
    mainPanel(
      textOutput("explanation"),
      br(),
      plotOutput("distPlot", height = "700px")
    )
  )
)

server <- function(input, output) {
  
  output$explanation <- renderText({
    "The Central Limit Theorem (CLT) states that the distribution of sample means 
    approaches a normal distribution as the sample size gets larger, regardless of 
    the population's distribution — provided the population has finite variance 
    (note: Cauchy distribution has no mean/variance and does NOT satisfy CLT)."
  })
  
  # Reactive expression to generate data only when button is clicked
  plot_data <- eventReactive(input$go, {
    family <- input$family
    n_pop <- 100000  # Large population to sample from
    m <- input$m     # Sample size
    r <- input$r     # Number of sample means
    
    # Generate large population based on selected family
    u <- switch(family,
                exponential = rexp(n_pop, rate = input$para1),
                uniform     = runif(n_pop, min = input$para1, max = input$para2),
                normal      = rnorm(n_pop, mean = input$para1, sd = input$para2),
                poisson     = rpois(n_pop, lambda = input$para1),
                cauchy      = rcauchy(n_pop, location = input$para1, scale = input$para2),
                binomial    = rbinom(n_pop, size = input$para1, prob = input$para2),
                gamma       = rgamma(n_pop, shape = input$para1, rate = input$para2),
                `chi-square` = rchisq(n_pop, df = input$para1),
                `student-t`  = rt(n_pop, df = input$para1),
                rexp(n_pop, rate = 1)  # default
    )
    
    # Generate r sample means
    sample_means <- replicate(r, mean(sample(u, size = m, replace = TRUE)))
    
    list(population = u, means = sample_means, family = family)
  })
  
  output$distPlot <- renderPlot({
    data <- plot_data()
    u <- data$population
    means <- data$means
    family <- data$family
    
    # Population histogram
    p1 <- ggplot(data.frame(x = u), aes(x)) +
      geom_histogram(bins = 50, fill = "skyblue3", color = "black", alpha = 0.8) +
      labs(title = paste("Population Distribution:", tools::toTitleCase(gsub("-", " ", family))),
           x = "Value", y = "Count") +
      theme_minimal(base_size = 14)
    
    # Sampling distribution of the mean
    mean_val <- mean(means)
    sd_val <- sd(means)
    
    p2 <- ggplot(data.frame(x = means), aes(x)) +
      geom_histogram(aes(y = ..density..), bins = 50, fill = "skyblue3", color = "black", alpha = 0.8) +
      stat_function(fun = dnorm, 
                    args = list(mean = mean_val, sd = sd_val),
                    color = "red", size = 1.2) +
      labs(title = paste("Sampling Distribution of the Mean (n =", input$m, ")"),
           subtitle = paste("Mean =", round(mean_val, 3), "| Theoretical SE =", round(sqrt(var(u)/input$m), 3)),
           x = "Sample Mean", y = "Density") +
      theme_minimal(base_size = 14)
    
    # QQ Plot
    p3 <- ggplot(data.frame(x = means), aes(sample = x)) +
      stat_qq(color = "blue") +
      stat_qq_line(color = "red") +
      labs(title = "Normal QQ Plot of Sample Means") +
      theme_minimal(base_size = 14)
    
    # Combine plots
    grid.arrange(p2, p1, p3, ncol = 2,
                 layout_matrix = rbind(c(1,1), c(2,3)),
                 top = "Central Limit Theorem Illustration")
  })
}

shinyApp(ui = ui, server = server)