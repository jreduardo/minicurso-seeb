library("shiny")
 
shinyUI(pageWithSidebar(
    headerPanel(""),

    sidebarPanel(
      withMathJax(),
      h3(strong("Sampling from  Gamma distribution (target)
                with shape and rate parameter
                based on the proposal normal distribution
                with tuning parameter (sigma)",style="color:black")),
      br(),
      br(),
      h3(strong("Behaviour of the MCMC",style="color:red")),
      sliderInput("Iterations",
                  "Iterations",
                  value = 100,min=100,max=100000, 
                  step=1000,animate=TRUE),
      sliderInput(inputId="burnin", 
                  label="Burnin", 
                  value=1, min = 1, max = 5000,
                  step=100,animate=TRUE),
      sliderInput(inputId="thin", 
                  label="Thin", 
                  value=1, min = 1, max = 1000,
                  step=10,animate=TRUE),
      br(),
     br(),
     h3(strong("Initial Values",style="color:red")),
      numericInput(inputId="theta.inicial.1", 
                   label=HTML("&theta;[1]"), 
                   value=1, min = -5, max = 5,
                   step=0.5),
      numericInput(inputId="theta.inicial.2", 
                   label=HTML("&theta;[2]"), 
                   value=5, min = -5, max = 5,
                   step=0.5),
      numericInput(inputId="theta.inicial.3", 
                   label=HTML("&theta;[3]"), 
                   value=12, min = -5, max = 5,
                   step=0.5),
      numericInput(inputId="tuning.parameter", 
                   label=HTML("&sigma;"), 
                   value=0.5, min = 0.5, max = 10,
                   step=0.5),
     br(),
     br(),
     h3(strong("Target distribution",style="color:red")),
      numericInput(inputId="shape", 
                   label="Shape parameter from Gamma distribution", 
                   value=2, min = 0.1, max = 10,
                   step=0.5),
      numericInput(inputId="rate", 
                   label="Rate parameter from Gamma distribution", 
                   value=5, min = 0.1, max = 10,
                   step=0.5),
     br(),
     br(),
     img(src = "rstudio.png", height = 140, width = 400),
    h3(strong("Reference")),
    code("Siddhartha Chib & Edward Greenberg (2012). 
              Understanding the Metropolis-Hastings 
              Algorithm, The American Statistician, 49:4, 
              327-335.")
      
        ),

    mainPanel(
      h1(strong("Metropolis algorithm (dependence sampling)"), align="center"),
      plotOutput("plot1"),
      plotOutput("plot2"),
      h3(strong("Summary of each chain",style="color:red")),
      verbatimTextOutput("summary1")
          
      )))
