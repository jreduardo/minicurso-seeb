library(shiny)
library(ggplot2)
library(gridExtra)
library(plotly)

shinyServer(function(input, output) {
  
  #-------------------------
  #Binomial
  #-------------------------
  output$distPlot1 <- renderPlot({ 
    
    dados<- rbinom(n=input$n, size=input$m, prob=input$prob)
    theta<- seq(0.01,0.99,length.out=1000)
    
    LI<- qbeta( (1-input$confint)/2,input$alpha + sum(dados), 
               input$beta + input$n*input$m -sum(dados) )
    LS<- qbeta((1+input$confint)/2,input$alpha + sum(dados), 
               input$beta + input$n*input$m - sum(dados))
    Percentil<- qbeta(input$percentil,input$alpha + sum(dados), 
                      input$beta + input$n*input$m - sum(dados))
    
    
    Veross<- function(dados,theta,m){
      n<- length(dados)
      sy <- sum(dados)
      L.theta<- theta^sum(dados)*(1-theta)^(m*n-sum(dados))
      L.theta<- L.theta/max(L.theta)
      #L.theta <- dbeta(theta, sy + 1, m * n - sy + 1)
      return(L.theta)
    }
    
    Priori<- function(theta, alpha, beta){ 
      aux<- dbeta(theta, shape1=alpha, shape2=beta)  
      aux/max(aux)
    }
    
    Posterior<- function(dados, theta, alpha, beta, m){
      n<- length(dados)
      aux<- dbeta(theta,shape1=alpha + sum(dados), 
                  shape2 = beta + m*n -sum(dados))
      aux/max(aux)
    }
    
    preditiva<- function(alpha, beta, dados, m, m.linha){
      yt<- 0:m.linha
      n<- length(dados)
      alpha.linha<- alpha + sum(dados) 
      beta.linha<- beta + n*m -sum(dados)
      #  if (beta.linha<0) cat("beta.linha deve ser maior do que zero")
      aux1<- choose(m.linha,yt)
      aux3<- beta(alpha.linha, beta.linha)
      aux2<- beta(yt+alpha.linha,beta.linha-yt+m.linha)
      (aux1*aux2)/aux3      
    }
    
    
    aux1<- data.frame("Distribution" = "Likelihood",
                      x=theta,y=Veross(dados,theta,#=input$prob
                                       m=input$m))
    aux2<- data.frame("Distribution" = "Prior",
                      x=theta, y=Priori(theta,
                                        alpha=input$alpha, 
                                        beta=input$beta))
    aux3<- data.frame("Distribution" = "Posterior",
                      x=theta, y=Posterior(dados, theta, 
                                           alpha=input$alpha, 
                                           beta=input$beta, 
                                           m=input$m))
    
    
    dados_preditiva= reactive({ 
      data.frame(x=as.factor(0:input$m.linha),
                 y=preditiva(alpha=input$alpha, beta=input$beta, 
                             dados, m=input$m,
                             m.linha = input$m.linha)) })
    
    dados_Binomial = reactive({ rbind(aux1, aux2, aux3) })
    
    dados_Posteriori = reactive({ 
      data.frame("Distribution" = "Posterior",
                 x=theta, y=Posterior(dados, theta, 
                                      alpha=input$alpha, 
                                      beta=input$beta, 
                                      m=input$m), 
                 LI=LI, LS=LS,Percentil=Percentil)})
    
    v1<- ggplot(dados_Binomial(), 
                aes(x=x,y=y,color=Distribution)) + 
      geom_line(size=2,linetype="solid") +
      xlab(expression(theta))+
      ylab("")+
      labs(title = "Conjugate families of distributions" )+
      theme(legend.position="top")+
      theme(
        plot.title = element_text(color="red", size=14, face="bold"),
        axis.title.x = element_text(color="red", size=14, face="bold"),
        axis.title.y = element_text(color="red", size=14, face="bold")
      )
    
    
    v2<- ggplot(data=dados_preditiva(), aes(x, y)) +
      geom_bar(stat="identity",width=0.01, color = "blue")+
      xlab(expression(theta))+
      ylab(expression(pi(y[t]/data)))+
      ylim(0,1)+
      labs(title ="Posterior Predictive Distribution")+
      theme(
        plot.title = element_text(color="red", size=14, face="bold"),
        axis.title.x = element_text(color="red", size=14, face="bold"),
        axis.title.y = element_text(color="red", size=14, face="bold")
      )
    
    #input$beta + input$n*input$m-sum(dados)-0:input$m.linha+input$m.linha
    
    v3<- ggplot(dados_Posteriori(), 
                aes(x=x,y=y)) + 
      geom_line(size=2,colour="blue") +
      xlab(expression(theta))+
      ylab(expression(pi(theta/data)))+
      labs(title = "CI for the posterior distribution") +
      geom_area(mapping = aes(x = ifelse(x>LI & x<LS, x, 2)), fill = "blue") +
      xlim(0,1)    +
      theme(
        plot.title = element_text(color="red", size=14, face="bold"),
        axis.title.x = element_text(color="red", size=14, face="bold"),
        axis.title.y = element_text(color="red", size=14, face="bold")
      )
    
    v4<- ggplot(dados_Posteriori(), 
                aes(x=x,y=y)) + 
      geom_line(size=2,colour="blue") +
      xlab(expression(theta))+
      ylab(expression(pi(theta/data)))+
      labs(title = "Percentile of the posterior distribution") +
      geom_area(mapping = aes(x = ifelse( x>0 & x<Percentil, x, 2)), fill = "blue") +
      xlim(0, 1) +
      theme(
        plot.title = element_text(color="red", size=14, face="bold"),
        axis.title.x = element_text(color="red", size=14, face="bold"),
        axis.title.y = element_text(color="red", size=14, face="bold")
      )
    
    grid.arrange(v1,v2,v3, v4,nrow=2, ncol=2)
    }, height = 300*4)#End Binomial
  
  #-------------------------
  #Poisson
  #-------------------------
  output$distPlot2 <- renderPlot({
    dados<- rpois(n=input$n1, input$lambda)
    theta<- seq(0.01,input$maximo_Poisson,length.out=1000)
    LI_Poisson<- qgamma((1-input$confint1)/2, 
                shape=input$alpha1+input$n1*mean(dados), 
                rate=input$n1+input$beta1)
    LS_Poisson<- qgamma((1+input$confint1)/2, 
                shape=input$alpha1+input$n1*mean(dados), 
                rate=input$n1+input$beta1)
    Percentil_Poisson<- qgamma(input$percentil1, 
                       shape=input$alpha1+input$n1*mean(dados), 
                       rate=input$n1+input$beta1)
    
    Veross<- function(dados, theta){
      n<- length(dados)
      aux<- exp(-n*theta)*theta^(sum(dados))
      aux/max(aux)
    }
    
    Priori<- function(theta,alpha,beta){
      aux<- dgamma(theta, shape=alpha, rate=beta)
      aux/max(aux)
      }
    
    Posterior<- function(dados, theta, alpha, beta){
      n<- length(dados)
      aux<- dgamma(theta, shape=alpha+n*mean(dados), rate=n+beta)
      aux/max(aux)
    }
    
    preditiva_Poisson<- function(dados, alpha, beta, m.linha){
      yp<- 0:m.linha
      n<- length(dados)
      alphap<- n*mean(dados)+alpha
      betap<- beta + n
      (betap)^(alphap)/gamma(alphap)* 1/factorial(yp)*gamma(yp+alphap)/(betap+1)^(yp+alphap)    
    }
    
    aux1<- data.frame("Distribution" = "Likelihood",
                      x=theta,y=Veross(dados,theta))
    aux2<- data.frame("Distribution" = "Prior",
                      x=theta, y=Priori(theta,
                                        alpha=input$alpha1, 
                                        beta=input$beta1))
    aux3<- data.frame("Distribution" = "Posterior",
                      x=theta, y=Posterior(dados, theta, 
                                           alpha=input$alpha1, 
                                           beta=input$beta1))
    dados_Poisson= reactive({ dados.ggplot<- rbind(aux1, aux2, aux3) })
    
    dados_Posterior_Poisson = reactive({ 
      data.frame("Distribution" = "Posterior",
                 x=theta, y=Posterior(dados, theta, 
                                      alpha=input$alpha1, 
                                      beta=input$beta1), 
                 LI_Poisson=LI_Poisson, 
                 LS_Poisson=LS_Poisson,
                 Percentil=Percentil_Poisson)})
    
    dados_preditiva_Poisson= reactive({ 
      data.frame(x=0:input$m1.linha1,#as.factor(
                 y=preditiva_Poisson(dados, 
                                     alpha=input$alpha1, 
                                     beta=input$beta1, 
                                     m.linha = input$m1.linha1)) })
    
    v1<- ggplot(dados_Poisson(), 
                aes(x=x,y=y,color=Distribution)) + 
    geom_line(size=2,linetype="solid") +
    xlab(expression(theta))+
    ylab("")+
    labs(title="Conjugate families of distributions")+
    theme(legend.position="top")+
    xlim(0,input$maximo_Poisson)+ylim(0,1)+
      theme(
        plot.title = element_text(color="red", size=14, face="bold"),
        axis.title.x = element_text(color="red", size=14, face="bold"),
        axis.title.y = element_text(color="red", size=14, face="bold")
      )
    
    v2<- ggplot(data=dados_preditiva_Poisson(), aes(x, y)) +
      geom_bar(stat="identity",width=0.01, color = "blue")+
      xlab(expression(theta))+
      ylab(expression(pi(y[t]/data)))+
      labs(title ="Predictive Distribution")+
      #xlim(-1,input$m1.linha1) +
      ylim(0,1)+
      theme(
        plot.title = element_text(color="red", size=14, face="bold"),
        axis.title.x = element_text(color="red", size=14, face="bold"),
        axis.title.y = element_text(color="red", size=14, face="bold")
      )
    
    
    v3<- ggplot(dados_Posterior_Poisson(), 
                     aes(x=x,y=y)) + 
      geom_line(size=1,colour="blue") +
      xlab(expression(theta))+
      ylab(expression(pi(theta/data)))+
      labs(title = "CI for the posterior distribution") +
      geom_area(mapping = aes(x = ifelse(x>LI_Poisson & x<LS_Poisson, x, 100)), fill = "blue") +
      xlim(0,input$maximo_Poisson) +ylim(0,1)+
      theme(
        plot.title = element_text(color="red", size=14, face="bold"),
        axis.title.x = element_text(color="red", size=14, face="bold"),
        axis.title.y = element_text(color="red", size=14, face="bold")
      )
    
    v4<- ggplot(dados_Posterior_Poisson(), 
                aes(x=x,y=y)) + 
      geom_line(size=1,colour="blue") +
      xlab(expression(theta))+
      ylab(expression(pi(theta/data)))+
      labs(title= "Percentile of the posterior distribution") +
      geom_area(mapping = aes(x = ifelse( x>0.01 & x<Percentil_Poisson, x, 100)), fill = "blue") +
      xlim(0,input$maximo_Poisson)+ylim(0,1)+
      theme(
        plot.title = element_text(color="red", size=14, face="bold"),
        axis.title.x = element_text(color="red", size=14, face="bold"),
        axis.title.y = element_text(color="red", size=14, face="bold")
        )
    
    
    grid.arrange(v1,v2, v3,v4,nrow=2, ncol=2)
    #grid.arrange(v2)
    
  }, height = 300*4)#End Poisson
  
}
)