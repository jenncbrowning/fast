# This app is to find the critical value for a given confidence level
# Namely, we want to find z* with N(0,1) for C level CI
#         or to find t* with t_df for C level CI
#
# Updated July 22, 2022
#added cerulean theme
          

library(shiny)
library(shinyjs)
library(ggplot2)
library(shinythemes)

pval_ui <- function(){
  fluidPage(
    theme = shinythemes::shinytheme('cerulean'),
    fluidRow(wellPanel(
      h2(strong('Find critical value')),
      'To find the critical value, 
      you need to give the confidence level and specify the parameter value of the target distribution'
    ) # end wellPanel
    ), # end fluidRow
    fluidRow(
      column(4, wellPanel(
  
        numericInput('conf', 
                     label=h4('Confidence level, C= '),
                     value=0.95,min=0,max=1,step=0.01
                     ),
        h5('C must a value in [0,1]'),
        hr(),
        
        radioButtons('distrn_input', 
                     label=h4('Input target distribution'),
                     choiceNames = list('Normal', 't distribution'),
                     choiceValues = list(1, 2)),
        hr(),
        
        conditionalPanel('input.distrn_input == 1',
                        numericInput(inputId='norm.mu', 
                                     label=h4('Mean'),
                                     value=0,min=-3, max=3, step=0.1),
                        numericInput(inputId='norm.sd', 
                        label=h4('Standard deviation'),
                        value=1,min=0, max=5, step=0.1)),
        conditionalPanel('input.distrn_input == 2',
                         numericInput(inputId='df', 
                                      label=h4('Degrees of Freedom (df)'),
                                      value=5, min=1, step=1),
                         'Required: df > 0')
      ) # end well panel
      ), # end column 
      
      column(8, wellPanel(
        h4(strong('Critical value :')),
        verbatimTextOutput('cval'),
        hr(),
        
        h4(strong('Alpha = (1-C)/2: ')),
        verbatimTextOutput('alpha'),
        hr(),
        
        h4(strong('What R code to use')),
        verbatimTextOutput('rcode.cval'),
        hr(),
        
        h4(strong('Picture:')),
        plotOutput('picture', height='300px'),
      ) # end wellPanel
      ) # column 
      
    ) # end fluid row
  ) # end fluidpage
}

pval_serv <- function(input, output)
{
  
  distrn <- reactive({
    ifelse(input$distrn_input==1, 1, 2)
  })
  
  # get df of the t-distribution
  output$distrn <- renderText({
    if(is.na(distrn())) return('____')
    else return(distrn())
  })
  
  # find p-value according to Ha
  output$cval <- renderText({
    
    if(input$distrn_input == 1){
      cval <- qnorm((1-input$conf)/2, mean=input$norm.mu,sd=input$norm.sd,lower.tail=F)
      return(paste0("z* = ",round(cval,3)))}
    if(input$distrn_input == 2){
      cval <- qt((1-input$conf)/2, df=input$df, lower.tail=F)
      return( paste0("t* = ",round(cval,3)))}
      
  })
  
  
  output$alpha <- renderText({
      alpha <- (1-input$conf)/2
      round(alpha,3)
  })
  
  # output r code to find p-value
  output$rcode.cval<- renderText({
    
    if(input$distrn_input == 1)
      text <- paste0('qnorm(', (1-input$conf)/2,', mean=',input$norm.mu,", sd=",input$norm.sd, ',lower.tail=FALSE)')
    if(input$distrn_input == 2)
      text <- paste0('qt(',(1-input$conf)/2,", df=", input$df, ',lower.tail=FALSE)')
    text
  })
  
  # Output density curve with shaded area
  
  output$picture <- renderPlot({
    
    x <- seq(-4.5, 4.5, by=.1)
    distrn <- distrn()
    
    par(mar=c(4, .5, .5, .5), 
        cex.main=2, 
        cex.lab=1.7, 
        cex.axis=1.5)
    
    cvalue= ifelse(input$distrn_input==1, 
                   round(qnorm((1-input$conf)/2, mean=input$norm.mu,sd=input$norm.sd,lower.tail=F),3),
                   round(qt((1-input$conf)/2, df=input$df, lower.tail=F),3)
                   )
    
    if(distrn==1){
      plot(x, dnorm(x, input$norm.mu,input$norm.sd), 
           type='l', lwd=1, xlab='X', xaxt='n', yaxt='n', ylab='')
      axis(side=1, at=c(-cvalue, cvalue))
      x.seq =c(-cvalue, seq(-cvalue, cvalue, by=.01),cvalue)
      y.seq =c(0, dnorm(seq(-cvalue, cvalue, by=.01)),0)
      polygon(x.seq,y.seq, col="lightblue")
      legend(-0.5,dnorm(-0.5)/2,
             paste0("C=",input$conf),bty="n")
    }
    
    if(distrn==2){
      plot(x, dt(x, input$df), 
           type='l', lwd=1, xlab='X', xaxt='n', yaxt='n', ylab='')
      axis(side=1, at=c(-cvalue, cvalue))
      x.seq =c(-cvalue, seq(-cvalue, cvalue, by=.01),cvalue)
      y.seq =c(0,dt(seq(-cvalue, cvalue, by=.01),df=input$df),0)
      polygon(x.seq,y.seq, col="lightblue")
      legend(-0.5,dt(-0.5,df=input$df)/2,
             paste0("C=",input$conf),bty="n")
    }
    
  })
}

shinyApp(pval_ui, pval_serv)

