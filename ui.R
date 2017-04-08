#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Central Limit Theorem"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("sample_size",
                   "Population Size:",
                   min = 1,
                   max = 10000,
                   value = 500),
       
       sliderInput("m",
                   "Sample Size:",
                   min = 1,
                   max = 500,
                   value = 50),
       
       sliderInput("r",
                   "Number of Replications/Samples",
                   min = 1,
                   max = 5000,
                   value = 100),
       
       selectInput(inputId = "family", 
                   label = "Distribution Family to Sample From: ", 
                   choices = list("exponential", "uniform", "normal", "poisson", 
                                  "cauchy", "binomial", "gamma", "chi-square", "student-t"), 
                   selected = "normal", 
                   multiple = FALSE, 
                   selectize = TRUE),
       conditionalPanel( condition = "output.nrows",
                         checkboxInput("headonly", "Only use first 1000 rows")),
       
       numericInput(inputId = "para1", 
                    label = "First Parameter: ", 
                    value = .5, 
                    min = 0.001, 
                    max = 1,
                    step = 0.01,
                    width = NULL),
       
       numericInput(inputId = "para2", 
                    label = "Second Parameter: ", 
                    value = 1, 
                    min = 1, 
                    max = 5,
                    step = NA,
                    width = NULL)
               
    ),#end sidebarPanel
    
   # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot")
    )
 
  )
))

