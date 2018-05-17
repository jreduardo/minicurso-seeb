library(shiny)


shinyUI(
  #fluidPage(
 # titlePanel("Conjugancy"),
  navbarPage(title="Conjugate families of distributions",
  tabPanel("Help", includeMarkdown("help.md")),
   
  navbarMenu("Binomial",  
  tabPanel("Learning about Theta",
  sidebarLayout(
    sidebarPanel(
      withMathJax(), 
      h3(strong('Prior Beta distribution'),
         style="color:red"),
      numericInput(inputId="alpha",label=HTML("&alpha;"),
                   value=2,min=0.1, max=20, step=0.1),
      numericInput(inputId="beta",label=HTML("&beta;"),
                   value=5,min=0.1, max=20, step=0.1),
      br(),
      br(),
      h3(strong('Binomial Likelihood function',style="color:red")),
      numericInput(inputId="prob",label=HTML("&theta;"),
                   value=0.8,min=0, max=1, step=0.1),
      numericInput(inputId="n",
                   label="Sample size",
                   value=10,min=1, max=1000, step=1),
      numericInput(inputId="m",
                   label="Number of trials",
                   value=1,min=1, max=100, step=1),
      br(),
      br(),
      h3(strong('Posterior Beta distribution',
                style="color:red")),
      numericInput(inputId="confint",label="Credible Interval level",
                   value=0.95,min=0.01, max=1, step=0.01),
      numericInput(inputId="percentil",label="Percentile",
                   value=0.50,min=0.01, max=1, step=0.01),
      br(),
      br(),
      h3(strong('Posterior Preditive distribution'),
         style="color:red"),
      numericInput(inputId="m.linha",label="m.tilde",
                   value=1,min=1, max=100, step=1),
      br(),
      br(),
      img(src = "uspbrasao.png", height = 100, width = 100),
      br(),
      br(),
      img(src = "rstudio.png", height = 100, width = 400),
      h3(strong("Reference")),
      code("Andrew Gelman, John B. Carlin, Hal S. Stern, 
                    David B. Dunson, Aki Vehtari, Donald B. Rubin (2013). 
           Bayesian Data Analysis, Third Edition. 
           Chapman & Hall/CRC.")
      
      
    ),
    mainPanel(
      h1(strong("Prior, Likelihood, Posterior and Posterior Predictive Distribution"), align="center"),
      plotOutput("distPlot1")
     )
  )
  )
  ),
  
  
  navbarMenu("Poisson",
  tabPanel("Learning about Theta",
  
           sidebarLayout(
             sidebarPanel(
               withMathJax(), 
               h3(strong('Prior Gamma distribution',
                         style="color:red")),
               numericInput(inputId="alpha1",label=HTML("&alpha;"),
                            value=2,min=0.1, max=20, step=0.1),
               numericInput(inputId="beta1",label=HTML("&beta;"),
                            value=3,min=0.1, max=20, step=0.1),
               br(),
               br(),
               h3(strong('Poisson Likelihood function',
                         style="color:red")),
               numericInput(inputId="lambda",label=HTML("&theta;"),
                            value=2,min=0.1, max=10, step=0.1),
               numericInput(inputId="n1",
                            label="sample size",
                            value=10,min=10, max=1000, step=1),
               br(),
               br(),
               h3(strong('Posterior Gamma distribution',
                         style="color:red")),
               numericInput(inputId="confint1",label="Credible Interval level",
                            value=0.95,min=0.05, max=1, step=0.01),
               numericInput(inputId="percentil1",label="Percentile",
                            value=0.50,min=0.01, max=1, step=0.01),
              numericInput(inputId="maximo_Poisson",
                           label="Maximum  valur for x axis",
                           value=5,min=5, max=20, step=1),
                br(),
                br(),
               h3(strong('Posterior Preditive distribution'),
                  style="color:red"),
               numericInput(inputId="m1.linha1",label="m.linha",
                            value=1,min=1, max=100, step=1),
               br(),
               br(),
               img(src = "uspbrasao.png", height = 100, width = 100),
               br(),
               br(),
               img(src = "rstudio.png", height = 100, width = 400),
               h3(strong("Reference")),
               code("Andrew Gelman, John B. Carlin, Hal S. Stern, 
                    David B. Dunson, Aki Vehtari, Donald B. Rubin (2013). 
                    Bayesian Data Analysis, Third Edition. 
                    Chapman & Hall/CRC.")
               
               
               
             ),
             mainPanel(
               h1(strong("Prior, Likelihood, Posterior and Posterior Predictive Distribution"), align="center"),
               plotOutput('distPlot2'))
           )
  )
  )
  
  ))

