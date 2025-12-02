# =================================================
# app.R – CLT Explorer (plots show on launch!)
# =================================================

library(shiny)
library(ggplot2)
library(gridExtra)

ui <- fluidPage(
  titlePanel("Central Limit Theorem Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      selectInput("family", h4("Population Distribution"),
                  choices = c(
                    "Normal"           = "normal",
                    "Exponential"      = "exponential",
                    "Uniform"          = "uniform",
                    "Poisson"          = "poisson",
                    "Cauchy (no CLT)"  = "cauchy",
                    "Binomial"         = "binomial",
                    "Gamma"            = "gamma",
                    "Chi-Square"       = "chi_square",
                    "Student's t"      = "t"
                  ),
                  selected = "exponential"),
      
      hr(),
      
      sliderInput("n", "Sample size (n):", 
                  min = 1, max = 200, value = 30, step = 1),
      
      sliderInput("reps", "Number of sample means:", 
                  min = 100, max = 5000, value = 1000, step = 100),
      
      hr(),
      h4("Parameters"),
      
      conditionalPanel("input.family == 'exponential'",
                       numericInput("exp_rate", "Rate λ:", 1, min = 0.01, step = 0.1)),
      
      conditionalPanel("input.family == 'uniform'",
                       numericInput("unif_min", "Min:", 0),
                       numericInput("unif_max", "Max:", 1)),
      
      conditionalPanel("input.family == 'normal'",
                       numericInput("norm_mean", "Mean μ:", 0),
                       numericInput("norm_sd",   "SD σ:",   1, min = 0.01, step = 0.1)),
      
      conditionalPanel("input.family == 'poisson'",
                       numericInput("pois_lambda", "λ:", 5, min = 0.1, step = 0.5)),
      
      conditionalPanel("input.family == 'cauchy'",
                       numericInput("cauchy_loc",   "Location:", 0),
                       numericInput("cauchy_scale", "Scale:",    1, min = 0.01),
                       helpText("Cauchy has no mean/variance → CLT fails!")),
      
      conditionalPanel("input.family == 'binomial'",
                       numericInput("binom_size", "Trials:", 10, min = 1),
                       numericInput("binom_prob", "Prob p:", 0.5, min = 0, max = 1, step = 0.01)),
      
      conditionalPanel("input.family == 'gamma'",
                       numericInput("gamma_shape", "Shape α:", 2, min = 0.1, step = 0.1),
                       numericInput("gamma_rate",  "Rate β:",  1, min = 0.01, step = 0.1)),
      
      conditionalPanel("input.family == 'chi_square'",
                       numericInput("chisq_df", "df:", 4, min = 1)),
      
      conditionalPanel("input.family == 't'",
                       numericInput("t_df", "df:", 3, min = 1)),
      
      hr(),
      actionButton("go", "Redraw Plots", class = "btn-success btn-lg", width = "100%")
    ),
    
    mainPanel(
      width = 8,
      plotOutput("plots", height = "780px"),
      br(),
      verbatimTextOutput("explanation")
    )
  )
)

server <- function(input, output, session) {
  
  output$explanation <- renderText({
    "Central Limit Theorem (CLT):\nThe sampling distribution of the mean becomes approximately normal 
    as sample size n increases — even for highly non-normal populations (as long as variance exists).\n\n
    Try increasing n with Exponential, Poisson, or Chi-square → watch the top plot turn bell-shaped!"
  })
  
  # This reactive will update on startup AND when button is clicked
  data_react <- reactive({
    # Trigger also when the button is clicked
    input$go
    
    pop_size <- 100000
    
    population <- switch(input$family,
                         normal      = rnorm(pop_size, mean = input$norm_mean,   sd = input$norm_sd),
                         exponential = rexp(pop_size,  rate = input$exp_rate),
                         uniform     = runif(pop_size, min = input$unif_min,    max = input$unif_max),
                         poisson     = rpois(pop_size, lambda = input$pois_lambda),
                         cauchy      = rcauchy(pop_size, location = input$cauchy_loc, scale = input$cauchy_scale),
                         binomial    = rbinom(pop_size, size = input$binom_size, prob = input$binom_prob),
                         gamma       = rgamma(pop_size, shape = input$gamma_shape, rate = input$gamma_rate),
                         chi_square  = rchisq(pop_size, df = input$chisq_df),
                         t           = rt(pop_size, df = input$t_df),
                         rexp(pop_size, 1)  # default
    )
    
    means <- replicate(input$reps, mean(sample(population, input$n, replace = TRUE)))
    
    list(pop = population, means = means)
  })
  
  output$plots <- renderPlot({
    d <- data_react()  # This runs on launch and on every change or button click
    
    p1 <- ggplot(data.frame(x = d$pop), aes(x)) +
      geom_histogram(bins = 60, fill = "#1f77b4", color = "black", alpha = 0.85) +
      labs(title = "Population Distribution") +
      theme_minimal(base_size = 14)
    
    mu <- mean(d$means)
    se <- sd(d$means)
    
    p2 <- ggplot(data.frame(x = d$means), aes(x)) +
      geom_histogram(aes(y = ..density..), bins = 60, fill = "#ff7f0e", color = "black", alpha = 0.85) +
      stat_function(fun = dnorm, args = list(mean = mu, sd = se), color = "red", size = 1.3) +
      labs(title = paste0("Sampling Distribution of the Mean (n = ", input$n, ")"),
           subtitle = paste("Mean ≈", round(mu, 4), "  |  SE ≈", round(se, 4))) +
      theme_minimal(base_size = 14)
    
    p3 <- ggplot(data.frame(x = d$means), aes(sample = x)) +
      stat_qq(color = "#2ca02c") + stat_qq_line(color = "red", linetype = "dashed") +
      labs(title = "Normal QQ-Plot of Sample Means") +
      theme_minimal(base_size = 14)
    
    grid.arrange(p2, p1, p3,
                 ncol = 2,
                 layout_matrix = rbind(c(1,1), c(2,3)),
                 top = "Central Limit Theorem Demonstration")
  })
}

# Run the app
shinyApp(ui = ui, server = server)