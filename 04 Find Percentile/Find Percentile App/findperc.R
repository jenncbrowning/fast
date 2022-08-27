# This app is to find the quantile value for a area  
# Namely, we want to find z* with N(0,1) for q quantile
#         or to find t* with t_df for q quantile
#
# Update: May 18, 2022
# Update: July 28, 2022


library(shiny)
library(shinyjs)
library(ggplot2)
library(shinythemes)

qval_ui <- function(){
  fluidPage(
    theme = shinythemes::shinytheme('cerulean'),
    fluidRow(wellPanel(
      h2(strong('Find percentile value')),
      'Given an area under a density curve, find the boundary value that determines this area.'
     ) # end wellPanel
    ), # end fluidRow
    
    fluidRow(
      column(4, wellPanel(
        
        radioButtons('distrn_input', 
                     label=h4('Input target distribution'),
                     selected = 1,
                     choiceNames = list('Normal', 't distribution'),
                     choiceValues = list(1, 2),
                     ),
        br(),
        
        
        conditionalPanel('input.distrn_input == 1',
                         numericInput(inputId='mu',
                                      label=h5('Mean'),
                                      value=0,min=-3, max=3, step=0.1),
                         numericInput(inputId='sd',
                                      label=h5('Standard deviation'),
                                      value=1,min=0, max=5, step=0.1)),
        conditionalPanel('input.distrn_input == 2',
                         numericInput(inputId='df', 
                                      label=h5('Degrees of Freedom (df)'),
                                      value=5, min=1, step=1),
                         'Required: df > 0'),
        hr(),
        
        numericInput('area', 
                     label=h4('Area under the curve (AUC): '),
                     value=0.9,min=0,max=1,step=0.01
        ),
        h5('AUC must be a value between 0 and 1'),
        br(),
        
        selectInput(
          inputId = "area_type",
          label = h5("Input type for AUC:"),
          choices = c(
            "Lower Tail" = "lower",
            "Upper Tail" = "upper",
            "Middle" = "middle"),
          selected = "lower"
         ),
        hr(),
        
      ) # end well panel
      ), # end column 
      
      column(8, wellPanel(
        
        h4(strong('Area type in probability form :')),
        verbatimTextOutput('pform'),
        br(),
        
        h4(strong('Quantile value :')),
        verbatimTextOutput('qval'),
        hr(),
        
        
        h4(strong('Density Curve:')),
        plotOutput('picture', height='300px'),
        br(),
        
        h4(strong('R code to use')),
        verbatimTextOutput('rcode.qval'),
        hr(),
      ) # end wellPanel
      ) # column 
      
    ) # end fluid row
  ) # end fluidpage
}

qval_serv <- function(input, output)
{
  
  distrn <- reactive({
    ifelse(input$distrn_input==1, 1, 2)
  })
  
  mu <- reactive({
    ifelse(input$mu!=0,input$mu, 0)
  })
  
  
  sd <- reactive({
    ifelse(input$sd!=1,input$sd, 1)
  })
  
  df <-reactive({
    ifelse(input$df!=5,input$df, 5)
  })
  
  
  # get df of the t-distribution
  output$distrn <- renderText({
    if(is.na(distrn())) return('____')
    else return(distrn())
  })
  
  output$mu <- renderText({
    if(is.na(mu())) return('____')
    else return(mu())
  })
  
  output$sd <- renderText({
    if(is.na(sd())) return('____')
    else return(sd())
  })
  
  output$df <- renderText({
    if(is.na(df())) return('____')
    else return(df())
  })
  
  # find q-value according to given AUC
  output$pform <- renderText({
    
      if(input$area_type=="lower"){
        return(paste0("P(X < a) = ",input$area, ", want to find a. "))}
      else if (input$area_type=="upper"){
        return(paste0("P(X > a) = ",input$area, ", want to find a. "))}
      else{
        return(paste0("P(a < X < b) = ",input$area, " with equal tail probs, want to find a and b. "))}

  })
  
  
  # find q-value according to given AUC
  output$qval <- renderText({
    
    if(input$distrn_input == 1){
      if(input$area_type=="lower"){
        qval <- qnorm(input$area, input$mu, input$sd, lower.tail=T)
        return(paste0("a = ",round(qval,4)))}
      else if (input$area_type=="upper"){
        qval <- qnorm(input$area, input$mu, input$sd, lower.tail=F)
        return(paste0("a = ",round(qval,4)))}
      else{
        qval <- qnorm((1-input$area)/2, input$mu, input$sd, lower.tail=F)
        return(paste0("a =  ",round(2*input$mu-qval,4),", b = ", round(qval,4)))}
      }
    if(input$distrn_input == 2){
      if(input$area_type=="lower"){
        qval <- qt(input$area, input$df, lower.tail=T)
        return(paste0("a = ",round(qval,4)))}
      else if (input$area_type=="upper"){
        qval <- qt(input$area, input$df, lower.tail=F)
        return(paste0("a = ",round(qval,4)))}
      else{
        qval <- qt((1-input$area)/2, input$df, lower.tail=F)
        return(paste0("a = ",round(-qval,4), ", b = ",round(qval,4)))}
      }
    
  })
  
  
  # output r code to find p-value
  output$rcode.qval<- renderText({
    
    if(input$distrn_input == 1){
      if(input$area_type=="lower")
        text <- paste0('qnorm(', input$area,', mean=',input$mu,", sd=",input$sd, ',lower.tail=TRUE)')
      else if (input$area_type=="upper")
        text <- paste0('qnorm(', input$area,', mean=',input$mu,", sd=",input$sd, ',lower.tail=FALSE)')
      else
        text <- paste0('qnorm(', (1-input$area)/2,', mean=',input$mu,",sd=",input$sd, ',lower.tail=FALSE)')
    }
    if(input$distrn_input == 2){
      if(input$area_type=="lower")
        text <- paste0('qt(', input$area,", df=", input$df, ',lower.tail=TRUE)')
      else if (input$area_type=="upper")
        text <- paste0('qt(', input$area,", df=", input$df, ',lower.tail=FALSE)')
      else
        text <- paste0('qt(', (1-input$area)/2,", df=", input$df, ',lower.tail=FALSE)')
    }
    text
  })
  
  # Output density curve with shaded area
  
  output$picture <- renderPlot({
    
    distrn <- distrn()
    mu <- mu()
    sd <- sd()
    df <- df()
    
    par(mar=c(4, .6, .5, .5), 
        cex.main=2, 
        cex.lab=1.4, 
        cex.axis=1.5)
    
    if(distrn==1){
      if(input$area_type=="lower"){
        qx <- round(qnorm(input$area, mu, sd, lower.tail=T),2)
        x <-seq(mu-4.5*sd,mu+4.5*sd,by=0.1)
        plot(x, dnorm(x, mu,sd), 
             type='l', lwd=1, xlab='Normal density curve', xaxt='n', yaxt='n', ylab='')
        axis(side=1, at=qx)
        x.seq =c(mu-4.5*sd, seq(mu-4.5*sd, qx, by=.01),qx)
        y.seq =c(0, dnorm(seq(mu-4.5*sd, qx, by=.01),mu,sd),0)
        polygon(x.seq,y.seq, col="lightblue")
        lines(c(mu,mu),c(0,dnorm(mu,mu,sd)),lty=2,col="red",lwd=2)
        legend("topright",paste0("P(X < ",qx,")=",input$area),bty="n")
      }
      else if(input$area_type=="upper"){
        qx <- round(qnorm(input$area, input$mu, input$sd, lower.tail=F),2)
        x <-seq(input$mu-4.5*input$sd,input$mu+4.5*input$sd,by=0.1)
        plot(x, dnorm(x, input$mu,input$sd), 
             type='l', lwd=1, xlab='Normal density curve', xaxt='n', yaxt='n', ylab='')
        axis(side=1, at=qx)
        x.seq =c(qx, seq(qx, input$mu+4.5*input$sd, by=.01),input$mu+4.5*input$sd)
        y.seq =c(0, dnorm(seq(qx, input$mu+4.5*input$sd, by=.01),input$mu,input$sd),0)
        polygon(x.seq,y.seq, col="lightblue")
        lines(c(input$mu,input$mu),c(0,dnorm(input$mu,input$mu,input$sd)),lty=2,col="red",lwd=2)
        legend("topright",paste0("P(X > ",qx,")=",input$area),bty="n")
      }
      else if(input$area_type=="middle"){
        qx <- round(qnorm((1-input$area)/2, input$mu, input$sd, lower.tail=F),2)
        x <- seq(input$mu-4.5*input$sd,input$mu+4.5*input$sd,by=0.1)
        plot(x, dnorm(x, input$mu,input$sd), 
             type='l', lwd=1, xlab='Normal density curve', xaxt='n', yaxt='n', ylab='')
        qa = 2*input$mu-qx
        qb = qx 
        axis(side=1, at=c(qa,qb))
        x.seq =c(qa, seq(qa, qb, by=.01),qb)
        y.seq =c(0, dnorm(seq(qa, qb, by=.01),input$mu,input$sd),0)
        polygon(x.seq,y.seq, col="lightblue")
        lines(c(input$mu,input$mu),c(0,dnorm(input$mu,input$mu,input$sd)),lty=2,col="red",lwd=2)
        legend("topright",paste0("P(", round(qa,2), "< X <",qb,")=",input$area),bty="n")
      }
    }
     
    
    if(distrn==2){
      x <- seq(-4.5, 4.5, by=.1)
      if(input$area_type=="lower"){
        qx <- round(qt(input$area, input$df, lower.tail=T),2)
        plot(x, dt(x, input$df), type='l', lwd=1, 
             xlab='t distribution density curve', xaxt='n', yaxt='n', ylab='')
        axis(side=1, at=qx)
        x.seq =c(-4.5, seq(-4.5, qx, by=.01),qx)
        y.seq =c(0,dt(seq(-4.5, qx, by=.01),df=input$df),0)
        polygon(x.seq,y.seq, col="lightblue")
        lines(c(0,0),c(0,dt(0,input$df)),lty=2,col="red",lwd=2)
        legend("topright",paste0("P(X < ",qx,")=",input$area),bty="n")
      }
      else if(input$area_type=="upper"){
        qx <- round(qt(input$area, input$df, lower.tail=F),2)
        plot(x, dt(x, input$df), type='l', lwd=1, 
             xlab='t distribution density curve', xaxt='n', yaxt='n', ylab='')
        axis(side=1, at=qx)
        x.seq =c(qx, seq(qx, 4.5, by=.01),4.5)
        y.seq =c(0,dt(seq(qx, 4.5, by=.01),df=input$df),0)
        polygon(x.seq,y.seq, col="lightblue")
        lines(c(0,0),c(0,dt(0,input$df)),lty=2,col="red",lwd=2)
        legend("topright",paste0("P(X > ",qx,")=",input$area),bty="n")
      }
      else if(input$area_type=="middle"){
        qx <- round(qt((1-input$area)/2, df=input$df, lower.tail=F),2)
        plot(x, dt(x, input$df), type='l', lwd=1, 
             xlab='t distribution density curve', xaxt='n', yaxt='n', ylab='')
        axis(side=1, at=c(-qx,qx))
        x.seq =c(-qx, seq(-qx, qx, by=.01),qx)
        y.seq =c(0,dt(seq(-qx, qx, by=.01),df=input$df),0)
        polygon(x.seq,y.seq, col="lightblue")
        lines(c(0,0),c(0,dt(0,input$df)),lty=2,col="red",lwd=2)
        legend("topright",paste0("P(", -qx, "< X <",qx,")=",input$area),bty="n")
      }
    }
  })
}

shinyApp(qval_ui, qval_serv)

