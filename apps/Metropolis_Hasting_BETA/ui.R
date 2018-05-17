library("shiny")
 
shinyUI(pageWithSidebar(
    headerPanel(""),

    sidebarPanel(
      withMathJax(),
      h3(strong("Sampling from Beta distribution (target)
               based on the proposal gamma distribution
               with tuning parameter (rate)",
                style="color:black")),
      br(),
      br(),
      h3(strong("Behaviour of the MCMC ",style="color:red")),
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
      h3(strong("Initial values",style="color:red")),
      numericInput(inputId="theta.inicial.1", 
                   label=HTML("&theta;[1]"), 
                   value=0.1, min = 0, max = 5,
                   step=0.5),
      numericInput(inputId="theta.inicial.2",
                   label=HTML("&theta;[2]"), 
                   value=0.5, min = 0, max = 5,
                   step=0.5),
      numericInput(inputId="theta.inicial.3",
                   label=HTML("&theta;[3]"), 
                   value=0.9, min = 0, max = 5,
                   step=0.5),
      numericInput(inputId="tuning.parameter",
                   label=HTML("&beta;"),
                   value=2, min = 0.5, max = 5,
                   step=0.5),
      br(),
      br(),
      h3(strong("Target distribution Beta(shape1, shape2)",style="color:red")),
      numericInput(inputId="shape1",
                   label="Shape1", 
                   value=2, min = 0.5, max = 10,
                   step=0.5),
      numericInput(inputId="shape2", 
                   label="Shape2", 
                   value=5, min = 0.5, max = 10,
                   step=0.5),
      br(),
      br(),
      img(src = "uspbrasao.png", height = 100, width = 100),
      br(),
      br(),
      img(src = "rstudio.png", height = 100, width = 400),h3(strong("Reference")),
      code("Siddhartha Chib & Edward Greenberg (2012). 
               Understanding the Metropolis-Hastings 
               Algorithm, The American Statistician, 49:4, 
               327-335.")
      
      
        ),

    mainPanel(
       h1(strong("Metropolis Hasting algorithm (dependence sampling)"), align="center"),
       plotOutput("plot1"),
       plotOutput("plot2"),
       h3(strong("Summary of each chain",style="color:red")),
       verbatimTextOutput("summary1")
       )))
