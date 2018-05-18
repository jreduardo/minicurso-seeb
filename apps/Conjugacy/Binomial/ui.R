library(shiny)


#shinyUI(
  navbarPage(title="Binomial distribution",

  navbarMenu("Conjugacy",  
 
  #tabPanel("Learning about Theta",
  sidebarLayout(
    sidebarPanel(
      withMathJax(), 
      h3(strong('Prior Beta distribution'),
         style="color:red"),
      h3(numericInput(inputId="alpha",label=HTML("&alpha;"),
                   value=2,min=0.1, max=20, step=0.1)),
      h3(numericInput(inputId="beta",label=HTML("&beta;"),
                   value=5,min=0.1, max=20, step=0.1)),
      h3(strong('Binomial Likelihood function',style="color:red")),
      h3(numericInput(inputId="prob",label=HTML("&theta;"),
                   value=0.8,min=0, max=1, step=0.1)),
      h3(numericInput(inputId="n",
                   label="Sample size",
                   value=20,min=1, max=1000, step=1)),
      h3(numericInput(inputId="m",
                   label="Number of trials",
                   value=3,min=1, max=100, step=1)),
      # h3(strong('Posterior Beta distribution',
      #           style="color:red")),
      # numericInput(inputId="confint",label="Credible Interval level",
      #              value=0.95,min=0.01, max=1, step=0.01),
      # numericInput(inputId="percentil",label="Percentile",
      #              value=0.50,min=0.01, max=1, step=0.01),
      # # br(),
      # br(),
      # h3(strong('Posterior Preditive distribution'),
      #    style="color:red"),
      # numericInput(inputId="m.linha",label="m.tilde",
      #              value=1,min=1, max=100, step=1),
      img(src = "uspbrasao.png", height = 100, width = 100),
      img(src = "rstudio.png", height = 100, width = 400),
      h3(strong("Reference")),
      code("Andrew Gelman, John B. Carlin, Hal S. Stern, 
                    David B. Dunson, Aki Vehtari, Donald B. Rubin (2013). 
           Bayesian Data Analysis, Third Edition. 
           Chapman & Hall/CRC.")
      
      
    ),
    
    mainPanel(
      h1(strong("Likelihood, Prior and Posterior Distribution"), 
         align="center"),
      plotOutput("distPlot1")
      
     )
  )
  #)#tabPanel("Learning about Theta"
  ),
  
  navbarMenu("Predictive Posterior Distribution",
             
             tabPanel("XXXX",
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
                        h1(strong("Prior, Likelihood, Posterior and Posterior Predictive Distribution"), 
                           align="center"),
                        plotOutput("distPlot2")
                        
                      )
                      )
             )),
  
  
  navbarMenu("Posterior Distribution")
  
  
  
  )
  
   
    
  #)
#)


  
