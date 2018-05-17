library("shiny")
library("ggplot2")
library("gridExtra")
library("pander")

shinyServer(function(input, output) {
  
  
   resultados<- reactive({ 
     
     betaBinomial<-function(x,n,alpha, beta){
       num<- gamma(alpha+beta)*gamma(x+alpha)*gamma(n-x+beta)
       den<- gamma(alpha)*gamma(beta)*gamma(alpha+beta+n)
       choose(n,x)*(num/den)
     }
     
     
     x<- matrix(0, input$Iterations, 3)
     y<- matrix(0, input$Iterations, 3)
     
     y[1,]<- c(input$theta.inicial.1,
               input$theta.inicial.2,
               input$theta.inicial.3)
     
     for (i in 1:3){
       for (t in 2:input$Iterations){
         x[t-1,i]<- rbinom(1,size=input$n, prob = y[t-1,i])
         y[t,i]  <- rbeta(1,
                          shape1 = x[t-1,i]+ input$alpha, 
                          shape2 = input$n+input$beta-x[t-1,i])
       }
       
     }
     
     
     x<- x[seq(input$burnin,input$Iterations,input$thin),]
     y<- y[seq(input$burnin,input$Iterations,input$thin),]
     data.frame(x=x,y=y)
     })
   
  
   output$plot1 <- renderPlot({
     x<- resultados()
     names(x)<- NULL
     
     dados1<- data.frame(iter=1:length(x[,1]), x1=x[,1])
     dados2<- data.frame(iter=1:length(x[,1]), x2=x[,2])
     dados3<- data.frame(iter=1:length(x[,1]), x3=x[,3])
     v1<- ggplot() + geom_line(aes(x=iter,y=x1), dados1, color="green")+
       geom_line(aes(x=iter,y=x2), dados2, color="red")+
       geom_line(aes(x=iter,y=x3), dados3,color="blue")+
       ylab(expression(theta))
     v1
   },height = 200*2)
   
   output$plot2 <- renderPlot({
      x<- resultados()
      names(x)<- NULL
      dados<- data.frame(x1=x[,1],x2=x[,2],x3=x[,3])
      x.valores<- range(dados)
      y.valores<- max(hist(dados$x1,plot=T)$counts,
                      hist(dados$x2,plot=T)$counts,
                      hist(dados$x3,plot=T)$counts)     
     p1 <- with(dados, acf(x1, plot = FALSE))
     p2 <- with(dados, acf(x2, plot = FALSE))
     p3 <- with(dados, acf(x3, plot = FALSE))
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

     v4<- ggplot(dados, aes(x1))+
       geom_bar(fill="green")+
       xlim(x.valores)+ylim(0,y.valores)+
       xlab("x")+
       ylab("f(x)")
     
     v5<- ggplot(dados, aes(x2))+
       geom_bar(fill="red")+
       xlim(x.valores)+ylim(0,y.valores)+
       xlab("x")+
       ylab("f(x)")
       
     v6<- ggplot(dados, aes(x3))+
        geom_bar(fill="blue")+
        xlim(x.valores)+ylim(0,y.valores)+
        xlab("x")+
        ylab("f(x)")
       
     grid.arrange(v1,v2,v3, v4,v5,v6, ncol=3)


   },height = 200*2)

   output$summary1<- renderPrint({
      x<- resultados()
      names(x)<- NULL
      theta<- c(x[,1], x[,2], x[,3])
      cadeias<- sort(rep(1:3,length(x[,1])))
      dados<- data.frame(theta=theta, cadeias=cadeias)
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
