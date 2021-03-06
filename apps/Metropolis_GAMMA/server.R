library("shiny")
library("ggplot2")
library("gridExtra")
library("pander")

shinyServer(function(input, output) {
  
  # message<- renderPrint({
  #   if (input$theta.inicial<=0){
  #     print("Initial theta must be positive, try again!")} 
  # })
  # 
  resultados<- reactive({ 
    
    target<- function(x, shape, rate){dgamma(x, shape=input$shape, rate=input$rate)}
    theta<- matrix(0, nrow = input$Iterations, ncol=3)#input$num.cadeias    
    
    vector.theta.inicial<- c(input$theta.inicial.1, input$theta.inicial.2, input$theta.inicial.3)
    theta[1,]<- vector.theta.inicial[1:3]#input$num.cadeias
      
      #if (input$theta.inicial<=0){ stop} 
      
      Acceptance<- matrix(0, 3, 1)#input$num.cadeias
     
      for (i in 1:3){#input$num.cadeias
        
      for (t in 2:input$Iterations){
        theta.proposto<- rnorm(1, theta[t-1,i], input$tuning.parameter)
        numerador  <- target(theta.proposto, shape = input$shape, rate=input$rate)
        denominador<- target(theta[t-1,i], shape = input$shape, rate=input$rate)
        
       if ( denominador==0){ denominador<- rgamma(1,shape=input$shape, rate=input$rate )}
        
        if (runif(1)<= numerador/denominador){
          theta[t,i]<- theta.proposto
          Acceptance[i,1]<- Acceptance[i,1] + 1
        } else {theta[t,i]<- theta[t-1,i]}
      }
    }
     
      theta<- theta[seq(input$burnin,input$Iterations,input$thin),]
      
      Rate.Acceptance<- Acceptance/input$Iterations
      
      list(theta=theta, Rate.Acceptance= Rate.Acceptance)
     })
   
  output$plot1 <- renderPlot({ 
    theta<- resultados()$theta
    Rate.Acceptance<- resultados()$Rate.Acceptance
    
    def1<- data.frame(iter=1:length(theta[,1]), theta1=theta[,1])
    def2<- data.frame(iter=1:length(theta[,1]), theta2=theta[,2])
    def3<- data.frame(iter=1:length(theta[,1]), theta3=theta[,3])
    ggplot() + geom_line(aes(x=iter,y=theta1), def1, color="green")+
      geom_line(aes(x=iter,y=theta2), def2, color="red")+
      geom_line(aes(x=iter,y=theta3), def3,color="blue")
    
    },height = 400)
  
  output$plot2 <- renderPlot({ 
    theta<- resultados()$theta
    Rate.Acceptance<- resultados()$Rate.Acceptance
    
    p1 <- acf(theta[,1], plot = FALSE)
    p2 <- acf(theta[,2], plot = FALSE)
    p3 <- acf(theta[,3], plot = FALSE)
    aux1 <- with(p1, data.frame(lag, acf))
    aux2 <- with(p2, data.frame(lag, acf))
    aux3 <- with(p3, data.frame(lag, acf))
       
       v1<- ggplot(aux1, aes(x=lag,y=acf))+
         geom_bar(stat="identity", position = "identity", width = 0.1,
                  color="green")
       v2<- ggplot(aux2, aes(x=lag,y=acf))+
         geom_bar(stat="identity", position = "identity", width = 0.1,
                  color="red")
       v3<- ggplot(aux3, aes(x=lag,y=acf))+
         geom_bar(stat="identity", position = "identity", width = 0.1,
                  color="blue")
  
       dados1<- data.frame(theta1=theta[,1])
       dados2<- data.frame(theta2=theta[,2]) 
       dados3<- data.frame(theta3=theta[,3])
       valores.x<- range(dados1, dados2, dados3)
       
       
       
  v4<- ggplot(dados1, aes(theta1))+ 
    geom_histogram(aes(y=..density..),fill="green")+
    stat_function(fun = dgamma, 
                  args = list(shape = input$shape, rate=input$rate), 
                  lwd = 2, 
                  col = "black")+
    xlab(expression(theta))+
    xlim(valores.x)+ylim(0,3)+
    ggtitle(paste("Acceptance rate",round(Rate.Acceptance[1],3)))
  
  v5<- ggplot(dados2, aes(theta2))+ 
    geom_histogram(aes(y=..density..),fill="red")+
    stat_function(fun = dgamma, 
                  args = list(shape = input$shape, rate=input$rate), 
                  lwd = 2, 
                  col = "black")+
    xlab(expression(theta))+
    xlim(valores.x)+ylim(0,3)+
    ggtitle(paste("Acceptance rate",round(Rate.Acceptance[2],3)))
  
  v6<- ggplot(dados3, aes(theta3))+ 
    geom_histogram(aes(y=..density..),fill="blue")+
    stat_function(fun = dgamma, 
                  args = list(shape = input$shape, rate=input$rate), 
                  lwd = 2, 
                  col = "black")+
    xlab(expression(theta))+
    xlim(valores.x)+ylim(0,3)+
    ggtitle(paste("Acceptance rate",round(Rate.Acceptance[3],3)))
  
  grid.arrange(v1,v2,v3,v4,v5,v6,ncol=3)  
  },height = 200*2)
  
  output$summary1<- renderPrint({
    theta<- resultados()$theta
    theta.vector<- c(theta[,1], theta[,2], theta[,3])
    cadeias<- sort(rep(1:3,length(theta[,1])))
    dados<- data.frame(theta=theta.vector, cadeias=cadeias)
    teste<- with(dados,tapply(theta, cadeias, summary))
    for (i in 1:3) {
      set.caption(sub(".", " ", 
                      paste("Summary for Chain",i), 
                      fixed = TRUE))
      pander(teste[[i]])
    }
    
    set.caption(sub(".", " ", 
                    paste("Summary for Sampling"), 
                    fixed = TRUE))
    pander(pander(summary(dados$theta)))
   })
   

})
