
library(shiny)
library(gridExtra)
library(dplyr)
library(cowplot)

shinyServer(function(input, output) {
  
  output$explanation <- renderText({
    "The Central Limit Theorem (CLT) says that the mean of any sampling distribution from a 
     population with finite mean and variance approaches a normal distribution, when the 
     sample size n is sufficiently large.  Intuitively, this means that if you take any random 
     variable, say from an exponential or Poisson distribution, draw enough independent 
     samples, and take the average of each of them, then the CLT says that the collection of 
     all of these averages should be approximately normal.

    Arguements:
    - Population Size: The size of the population for the family to sample from.
    - Sample Size: The number of elements to use in calculating each average.
    - Number of Replications/Samples: The number of averages to take.
    - Distribution Family: The type of random variable which you are sampling from initially.
    - Parameters: These define the shape of the previous sample family.
    "
  })
  
  output$distPlot <- renderPlot({
    
    #define get methods
    get_sample = function(r = 1000, m = 30, data = u){
      mean = c()
      k = 1
      while(k <= r){
        mean[k] = mean(sample(u, replace = T, m))
        k = k+1
      }
      return(mean)
    }
    
    
    get_family = function(family){
      #should always return positive values
      if(family == "exponential") { u = rexp(n = input$sample_size , rate = input$para1)}
      else if(family =="uniform") {u = runif(n = input$sample_size , min = input$para1, input$para2)}
      else if(family == "normal") { u = rnorm( n = input$sample_size , mean = input$para1, sd = input$para2)}
      else if(family == "poisson") { u = rpois(n = input$sample_size, input$para1)}
      else if(family =="cauchy") { u = rcauchy(n = input$sample_size, location = input$para1, scale = input$para2)}
      else if(family == "binomial") { u = rbinom(n = input$sample_size, size = max( as.integer(input$para1, 1)), prob = min(input$para2, 0.1))}
      else if(family == "gamma") { u = rgamma(n = input$sample_size, rate = input$para1, scale = input$para1, shape = 1)}
      else if(family == "chi-square") { u = rchisq(n = input$sample_size , df = input$para1, ncp = input$para2)}
      else if(family == "student-t") { u = rt(n = input$sample_size , df = input$para1, ncp = input$para2 )}
      return(u)
    }
    
    #we will need the sample data by itself for the second hist plot
    
    draw_family = function( family){
      x = get_family(family)
      n        <- length(x)
      mean     <- mean(x)
      sd       <- sqrt(var(x))
      binwidth <- 2
      height <- x
      binwidth = 0.01
      
      g1=  qplot(height, geom = "histogram", 
            breaks = seq(mean - 2*sd, mean + 2*sd  , binwidth),
            colour = I("skyblue3"), 
            fill = I("skyblue3"),
            xlab = "x", 
            ylab = "Count",
            main = paste("Sampling Distribution for", family)) +
        theme_grey()
      
      return(g1)
    }
    
    
    #define histogram function
    hist_function = function(sample_size, m, r, family){
      # n = 1000; m = 30; r = 500
      # family = "normal"
      u = get_family( family)
      #how can x be negative?
      x = get_sample( r, m, data = u)#stoping here
      
      
      
      n        <- length(x)
      mean     <- mean(x)
      sd       <- sqrt(var(x))
      binwidth <- 2
      height <- x
      binwidth = 0.01
      
      qplot(height, geom = "histogram", 
            breaks = seq(mean - 5*sd, mean + 5*sd  , binwidth),
            colour = I("black"), 
            fill = I("skyblue3"),
            xlab = "x", 
            ylab = "Count",
            main = paste("Mean Sampling Distribution for", family)) +
        # Create normal curve, adjusting for number of observations and binwidth
        stat_function( 
          colour = "red",
          fun = function(x, mean, sd, n, bw){ 
            dnorm(x = x, mean = mean, sd = sd ) * n * bw
          }, 
          args = c(mean = mean, sd = sd, n = n, bw = binwidth)) + theme_grey()
    }
    
      
    
      g1 = draw_family(input$family)
     #g1  = hist_function( n = 10000, m = 50, r = 1000, family = "normal")
      g2 = hist_function(sample_size = input$sample_size, m = input$m, r = input$r, family = input$family)
      #g1 = hist_function( sample_size = 1000, m = 40, r = 500, family = "exponential")
     
      u = get_family( family)
      #how can x be negative?
      mydata = get_sample( r, m, data = u) %>%as_data_frame()
      
      
      g3 = ggplot(mydata, aes(sample = value)) + stat_qq() + theme_grey() + ggtitle("QQ Plot for Mean of Sample")
      
      
      grid.arrange(g2, g1, g3, ncol = 2, 
                   layout_matrix = rbind(c(1,1), c(2,3)))
      
      
      
  })
  

  
  
  
})

