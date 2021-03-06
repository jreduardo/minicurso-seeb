library("shiny")
library("ggplot2")
library("gridExtra")
library("pander")

shinyServer(function(input, output) {
  
  
   resultados<- reactive({ 
     
     target<- function(x){dbeta(x,2,5)}
     
     proposal<- function(x, shape, rate){
       dgamma(x, shape, rate)
       }
     
     theta<- matrix(0, nrow = input$Iterations, ncol=3)
     Acceptance<- matrix(0,3,1, ncol=1)
     theta[1,]<- c(input$theta.inicial.1,
                   input$theta.inicial.2,
                   input$theta.inicial.3)
     
     for (i in 1:3){
       
     
     for (t in 2:input$Iterations){
       theta.proposto<-rgamma(1, shape=theta[t-1,i], rate=input$rate) 
       numerador<- target(theta.proposto)*proposal(theta[t-1,i],shape=input$shape, rate=input$rate) 
       denominador<-  target(theta[t-1,i])*proposal(theta.proposto, shape=input$shape, rate=input$rate)
       
       if (runif(1)<= numerador/denominador){
         theta[t,i]<- theta.proposto
         Acceptance[i,1]<- Acceptance[i,1] + 1
       } else {theta[t,i]<- theta[t-1,i]}
       
     }
     }
     theta<- theta[seq(input$burnin,input$Iterations,input$thin),]
     Acceptance.Rate<- Acceptance/input$Iterations
     list(theta=theta, Acceptance.Rate= Acceptance.Rate)
     })
   
  
   output$plot1 <- renderPlot({
     theta<- resultados()$theta
     Acceptance.Rate<- resultados()$Acceptance.Rate

     pepe1<- data.frame(iter=1:length(theta[,1]), theta1=theta[,1])
     pepe2<- data.frame(iter=1:length(theta[,1]), theta2=theta[,2])
     pepe3<- data.frame(iter=1:length(theta[,1]), theta3=theta[,3])
     ggplot() + geom_line(aes(x=iter,y=theta1), pepe1, color="green")+
       geom_line(aes(x=iter,y=theta2), pepe2, color="red")+
       geom_line(aes(x=iter,y=theta3), pepe3,color="blue")

   },height = 200*2)
   
   output$plot2 <- renderPlot({
     theta<- resultados()$theta
     Acceptance.Rate<- resultados()$Acceptance.Rate

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
     
     v4<- ggplot(dados1, aes(theta1))+
       geom_histogram(aes(y=..density..),fill="green")+
       xlim(0,1)+ylim(0,4)+
       stat_function(fun = dbeta,
                     args = list(shape1 = input$shape1, shape2=input$shape2),
                     lwd = 2,
                     col = "black")+
       xlab(expression(theta))+
       ggtitle(paste("Acceptance rate",round(Acceptance.Rate[1],3)))
     
     v5<- ggplot(dados2, aes(theta2))+
       geom_histogram(aes(y=..density..),fill="red")+
       xlim(0,1)+ylim(0,4)+
       stat_function(fun = dbeta,
                     args = list(shape1 = input$shape1, shape2=input$shape2),
                     lwd = 2,
                     col = "black")+
       xlab(expression(theta))+
       ggtitle(paste("Acceptance rate",round(Acceptance.Rate[2],3)))
     
     v6<- ggplot(dados3, aes(theta3))+
       geom_histogram(aes(y=..density..),fill="blue")+
       xlim(0,1)+ylim(0,4)+
       stat_function(fun = dbeta,
                     args = list(shape1 = input$shape1, shape2=input$shape2),
                     lwd = 2,
                     col = "black")+
       xlab(expression(theta))+
       ggtitle(paste("Acceptance rate",round(Acceptance.Rate[3],3)))
     
     grid.arrange(v1,v2,v3, v4,v5,v6, ncol=3)


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
