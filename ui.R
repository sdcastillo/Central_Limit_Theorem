library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Central Limit Theorem Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      selectInput("family", 
                  label = h4("Population Distribution"),
                  choices = c(
                    "Normal"              = "normal",
                    "Exponential"         = "exponential",
                    "Uniform"             = "uniform",
                    "Poisson"             = "poisson",
                    "Cauchy (no CLT!)"    = "cauchy",
                    "Binomial"            = "binomial",
                    "Gamma"               = "gamma",
                    "Chi-Square"          = "chi-square",
                    "Student's t"         = "student-t"
                  ),
                  selected = "exponential"),
      
      hr(),
      
      sliderInput("m",
                  "Sample Size (n) per replication:",
                  min = 1, max = 200, value = 30, step = 1),
      
      sliderInput("r",
                  "Number of Sample Means (replications):",
                  min = 100, max = 5000, value = 1000, step = 100),
      
      hr(),
      h4("Distribution Parameters"),
      
      # Exponential
      conditionalPanel(
        condition = "input.family == 'exponential'",
        numericInput("para1", "Rate (λ):", value = 1, min = 0.01, step = 0.1)
      ),
      
      # Uniform
      conditionalPanel(
        condition = "input.family == 'uniform'",
        numericInput("para1", "Min:", value = 0),
        numericInput("para2", "Max:", value = 1)
      ),
      
      # Normal
      conditionalPanel(
        condition = "input.family == 'normal'",
        numericInput("para1", "Mean (μ):", value = 0),
        numericInput("para2", "Standard Deviation (σ):", value = 1, min = 0.01, step = 0.1)
      ),
      
      # Poisson
      conditionalPanel(
        condition = "input.family == 'poisson'",
        numericInput("para1", "λ (mean):", value = 5, min = 0.1, step = 0.5)
      ),
      
      # Cauchy
      conditionalPanel(
        condition = "input.family == 'cauchy'",
        numericInput("para1", "Location:", value = 0),
        numericInput("para2", "Scale:", value = 1, min = 0.01, step = 0.1),
        helpText("Cauchy has no mean/variance → CLT does NOT apply!")
      ),
      
      # Binomial
      conditionalPanel(
        condition = "input.family == 'binomial'",
        numericInput("para1", "Number of trials (size):", value = 10, min = 1, step = 1),
        numericInput("para2", "Probability of success (p):", value = 0.5, min = 0, max = 1, step = 0.01)
      ),
      
      # Gamma
      conditionalPanel(
        condition = "input.family == 'gamma'",
        numericInput("para1", "Shape (α):", value = 2, min = 0.1, step = 0.1),
        numericInput("para2", "Rate (β):", value = 1, min = 0.01, step = 0.1)
      ),
      
      # Chi-square
      conditionalPanel(
        condition = "input.family == 'chi-square'",
        numericInput("para1", "Degrees of freedom:", value = 4, min = 1, step = 1)
      ),
      
      # Student's t
      conditionalPanel(
        condition = "input.family == 'student-t'",
        numericInput("para1", "Degrees of freedom:", value = 3, min = 1, step = 1),
        helpText("Low df → heavy tails. CLT still works slowly.")
      ),
      
      hr(),
      actionButton("go", "Redraw Plots", class = "btn-primary btn-lg", width = "100%"),
      br(), br(),
      tags$small("Note: Large populations (100,000+) are simulated behind the scenes for accurate sampling.")
    ),
    
    mainPanel(
      width = 8,
      
      h3("Central Limit Theorem in Action"),
      p("The plots below show:"),
      tags$ul(
        tags$li("Top-left: Distribution of sample means (should → normal as n increases)"),
        tags$li("Bottom-left: Original population distribution"),
        tags$li("Bottom-right: QQ plot checking normality of sample means")
      ),
      
      plotOutput("distPlot", height = "750px"),
      
      br(),
      h4("What is the Central Limit Theorem?"),
      verbatimTextOutput("explanation")
    )
  )
)