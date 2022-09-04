library(shiny)
library(shinythemes)
library(plotly)
library(ggplot2)



ui <- fluidPage(
  theme = shinythemes::shinytheme('cerulean'),
  # Application title
  fluidRow(wellPanel(
    h2(strong('Simple Linear Regression')),
    'Simple linear regression attemps to model the relationship between two variables, i.e.,  
    every value of X is associated with a value of Y.'
     ) # end wellPanel
  ), # end fluidRow
  
  withMathJax(),
  
  sidebarLayout(
    sidebarPanel(
      
      tags$b("Enter Data:"),
      br(),
      br(),
      textInput("x", "Explanatory variable, X", 
                value = "63,64,66,69,69,71,71,72,73,75",
                placeholder = "Enter values separated by a comma with decimals as points, e.g. 2.1, 5.4, 3,3.3, etc."),
      textInput("y", "Response variable, Y", 
                value = "127,121,142,157,162,156,169,165,181,201", 
                placeholder = "Enter values separated by a comma with decimals as points, e.g. 3.2, 10.4, 5.5, 6.01, etc."),
      textInput("xvar","Enter variable name for X","Height",placeholder="x label"),
      textInput("yvar","Enter variable name for Y","Weight", placeholder="y label"),
      checkboxInput("showdata", "Display the data", FALSE),
      
      hr(),
      
      numericInput("xstar","Find interval at X = ",value = 65),
      
      sliderInput("alpha",
                  "Significance level \\(\\alpha = \\)",
                  value = 0.05,
                  min = 0.01,
                  max = 0.15
      ),    
      hr(),
      HTML('<p>This application is created by Wei (Becky) Lin and Jennifer Browning at SFU.</p>')
    ),
    
    
    mainPanel(
      #h4("Scatter plot of Data "),
      #plotOutput("scatter"),
      #plotlyOutput("plotcipi")
      #
      tabsetPanel(type = "tab",
                  tabPanel("Model Estimation", 
                           DT::dataTableOutput('tbl'),
                           #br(),
                           h4("Scatter plot of Data "),
                           plotOutput("scatter"),
                           #br(),
                           h4("Summary statistics:"),
                           #br(),
                           uiOutput("datasummary"),
                           br(),
                           h4("Find parameters in the fitted equation by hand:"),
                           #br(),
                           uiOutput("by_hand"),
                           br()
                           ), # Data as datatable
                  
                  tabPanel("Slope Inference", 
                           br(),
                           h4("Model summary in R:"),
                           verbatimTextOutput("fitsummary"),
                           br(),
                           h4("Inference about slope:"),
                           uiOutput("inferenceslope"),
                           br()
                           ),
                  
                  tabPanel("Find Interval", 
                           br(),
                           h4("Display fitted line with CI and PI"),
                           plotlyOutput("plotcipi"),
                           br(),
                           h4("Confidence interval and Prediction interval"),
                           uiOutput("result_interval"),
                           br(),
                           br()),
                  
                  tabPanel("Residual Analysis",
                           br(),
                           h5("\\( Residual\\  = observed \\ \\ y - predicted\\ \\ y = y -\\hat y\\)"),
                           h5("A residual is a measure of how far away a point is vertically from the regression line. 
                              Simply, it is the error between the observed actual value and a predicted value."),
                           br(),
                           
                           plotOutput("plotresiduals"),
                           h5("A good residual plot (plot 1) displays no clear pattern, all residual data points
                              are randomly and evenly scattered around the zero horizontal line. Moreover, the normality assumption
                              of residuals can be checked through the histogram (plot 2) and the normal Q-Q plot (plot 3), the assumption
                              is met if the histogram does not diverge far from a bell-shape distribution, or most data points
                              in the Q-Q plot fall close to the straight line."),
                           br(),
                           br()
                           )
                  

                ) # end settabpanel
      ) # end mainpanel
    ) # end sidebarLayout
) 



server <- function(input, output) {
  
  extract <- function(text) {
    text <- gsub(" ", "", text)
    split <- strsplit(text, ",", fixed = FALSE)[[1]]
    as.numeric(split)
  }
  
  #
  # Display data 
  output$tbl = DT::renderDataTable({
    if(input$showdata){
    dta = data.frame(cbind(extract(input$x),extract(input$y)))
    colnames(dta)=c(input$xvar,input$yvar)
    DT::datatable(dta, options = list(lengthChange = FALSE))
    }
  })
  
  
  #===========================================
  # Output summary statistics of dataset
  #===========================================
  #
  output$datasummary <- renderUI({
    y <- extract(input$y)
    x <- extract(input$x)
    if (anyNA(x) | length(x) < 2 | anyNA(y) | length(y) < 2) {
      "Invalid input or not enough observations"
    } else if (length(x) != length(y)) {
      "Number of observations must be equal for x and y"
    } else {
      withMathJax(
        paste0("Sample size \\(n =\\) ", length(x)),
        br(),
        paste0("\\(\\bar{x} =\\) ", round(mean(x), 4),"\\(, \\ \\ s_{x} =\\)", round(sd(x),4), 
               "\\(,\\ \\ \\sum_i(x_i-\\bar x)^2 =\\)", round(sum((x-mean(x))^2),4)),
        br(),
        paste0("\\(\\bar{y} =\\) ", round(mean(y), 4),"\\(, \\ \\ s_{y} =\\)", round(sd(y),4), 
               "\\(,\\ \\ \\sum_i(y_i-\\bar y)^2 =\\)", round(sum((y-mean(y))^2),4)),
        br(),
        br(),
        paste0("\\(S_{XY} =\\sum_i(x_i-\\bar x)(y_i-\\bar y) =\\)", round(sum((x-mean(x))*(y-mean(y))),4) ),
        br(),
        paste0("\\( r = cor(x,y) = \\left( \\frac{1}{n-1} \\right)  \\sum_i \\left(\\frac{x_i-\\bar x}{s_x}\\right)
               \\left( \\frac{y_i-\\bar y}{s_y}\\right)=\\) ", 
               round(cor(x,y), 4) )
        )
    }
  })
  
  #===========================================
  # Output b0, b1 result by hand calculation
  #===========================================
  #
  output$by_hand <- renderUI({
    y <- extract(input$y)
    x <- extract(input$x)
    fit <- lm(y ~ x)
    withMathJax(
      paste0("Population model equation: 
             \\(Y_i = \\beta_0+\\beta_1 X_i+\\epsilon_i, \\ \\  \\epsilon_i\\sim_{IID} N(0,\\sigma^2)\\)"),
      br(),
      paste0("Estimated model equation: \\( \\hat y = \\hat{\\beta}_0 + \\hat{\\beta}_1 x = a +b\\ x = \\) ", 
             round(fit$coef[[1]], 4), " + ", round(fit$coef[[2]], 4), "\\( x \\)"),
      br(),
      br(),
      paste0("\\(b = \\hat{\\beta}_1 = \\dfrac{\\sum_i(x_i-\\bar x)(y_i-\\bar y)}{\\sum^n_{i = 1} (x_i - \\bar{x})^2} 
             =r\\left(\\frac{s_y}{s_x}\\right) = \\) ", round(fit$coef[[2]], 4)),
      br(),
      paste0("\\(a = \\hat{\\beta}_0 =\\bar{y} - b \\bar{x} = \\bar{y} - \\hat{\\beta}_1 \\bar{x} = \\) ", round(fit$coef[[1]], 4)),
      br(),
      br()
      
    )
  })
  
  #===========================================
  # Output lm() fitting summary
  #===========================================
  #
  output$fitsummary <- renderPrint({
    y <- extract(input$y)
    x <- extract(input$x)
    fit <- lm(y ~ x)
    summary(fit)
  })
  
  #===========================================
  # Inference about slope
  #===========================================
  #
  output$inferenceslope <- renderUI({
    y <- extract(input$y)
    x <- extract(input$x)
    fit <- lm(y ~ x)
    bci  =  confint(fit,level=1-input$alpha)[2,] 
    withMathJax(
    
      #paste0("\\( R^2 = \\) ", round(summary(fit)$r.squared, 4),
      #       "\\(\\ \\Rightarrow r=\\sqrt{R^2} = ",round(sqrt(summary(fit)$r.squared),4)),
      # br(),
      #br(),
      #paste0( "\\( \\hat y = \\)", round(fit$coef[[1]], 4)," + ",
      #       round(fit$coef[[2]], 4)," x"),
      tags$b("Hypothesis testing about slope"),
      br(),
      paste0("(1) Hypotheses about slope: \\( H_0: \\beta_1 =0\\) versus \\(H_a: \\beta_1\\ne 0\\)"),
      br(),
      paste0("( For SLR, above hypothesis is equivalent to: \\( H_0: r =0\\) versus \\(H_a:r \\ne 0\\) )"),
      br(),
      paste0("(2) Observed test statistics = ", round(fit$coef[[2]], 4),"/", round(summary(fit)$coef[2,2], 4),
             " = ", round(summary(fit)$coef[2,3], 4), " under \\( H_0\\), it is from t distribution with df = ",length(x)-2),
      br(),
      paste0("(3) P-value = \\( 2\\times P(t_{n-2}> \\)", abs(round(summary(fit)$coef[2,3], 4)), ") = ",round(summary(fit)$coef[2, 4], 6) ),
      br(),
      paste0("(4) Conclusion: compare p-value with specified significance level, p-value is ", 
             ifelse(summary(fit)$coef[2, 4]<input$alpha, "less than ", "greater than "), input$alpha, ", therefore we have ",
             ifelse(summary(fit)$coef[2, 4]<input$alpha, "evidence ", "no evidence "), "that the slope is different from zero."),
      br(),
      br(),
      tags$b("Find confidence interval the slope"),
      br(),
      paste0( (1-input$alpha)*100, "% CI for \\(\\beta_1: \\hat\\beta_1\\pm t_{\\alpha/2, n-2}SE(\\hat\\beta_1) \\)",
              ", where \\( SE(\\hat\\beta_1) = \\frac{s}{\\sqrt{\\sum_i (x_i-\\bar x)^2}}\\) and s = ",round(summary(fit)$sigma,4)),
      br(),
      paste0("Get CI from data: ",round(fit$coef[[2]], 4),"\\(\\pm\\)", round(qt(1-input$alpha/2,length(x)-2),4),"*",
             round(summary(fit)$coef[2,2], 4), " = ( ", round(bci[1],4)," , ", round(bci[2],4)," )"),
      
      br()
    ) 
  })
  
  #===========================================
  # Find CI for E(Y) and PI for Y at X*
  #===========================================
  #
  output$result_interval <- renderUI({
    options(scipen=999)
    # read in data and fit with a SLR model
    y <- extract(input$y)
    x <- extract(input$x)
    fit <- lm(y ~ x)
    xnew = data.frame(x=input$xstar)
    ci4my = predict(fit,xnew,interval="confidence",level=1-input$alpha)
    pi4y = predict(fit,xnew, interval="prediction",level=1-input$alpha)
    
    withMathJax(
      tags$b("Confidence interval for mean response"),
      br(),
      paste0( (1 - input$alpha) * 100, 
              "% CI for mean response at X =  ", 
              "\\( x^*: (\\hat{\\beta}_0+\\hat{\\beta}_1\\ x^*) \\pm 
              t_{\\alpha/2,n-2}\\sqrt{s^2\\left( \\dfrac{1}{n}+\\dfrac{(x^*-\\bar x)^2}{\\sum_i(x_i-\\bar x)^2}\\right)} \\) "),
      br(),
      paste0("(1) Estimate = \\(\\hat y = \\hat{\\beta}_0+\\hat{\\beta}_1\\ x^* \\) =", round(ci4my[1],4)),
      br(),
      paste0("(2) Critical value = t* =  \\(t_{\\alpha/2,n-2} \\) =", round(qt(1-input$alpha/2,length(x)-2),4)),
      br(),
      paste0("(3) SE( \\(\\mu_y\\) )= ", round(summary(fit)$sigma,4),
             "\\(\\times\\)",round( sqrt(1/length(x)+(input$xstar-mean(x))^2/sum( (x-mean(x))^2 )),4),
             " = ",round(summary(fit)$sigma* sqrt(1/length(x)+(input$xstar-mean(x))^2/sum( (x-mean(x))^2 )),4)),
      br(),
      paste0("Combined above steps, ", (1 - input$alpha) * 100, "% CI for mean response at X =",input$xstar,
             ": (",round(ci4my[2],4),",",round(ci4my[3],4),")"),
      br(),
      hr(),
      tags$b("Prediction interval for Y"),
      br(),
      paste0( (1 - input$alpha) * 100, 
              "% CI for Y at X =  ", 
              "\\( x^*: (\\hat{\\beta}_0+\\hat{\\beta}_1\\ x^*) \\pm 
              t_{\\alpha/2,n-2}\\sqrt{s^2\\left( 1+ \\dfrac{1}{n}+\\dfrac{(x^*-\\bar x)^2}{\\sum_i(x_i-\\bar x)^2}\\right)} \\) "),
      br(),
      paste0("(1) Estimate = \\(\\hat y = \\hat{\\beta}_0+\\hat{\\beta}_1\\ x^* \\) =", round(pi4y[1],4)),
      br(),
      paste0("(2) Critical value = t* =  \\(t_{\\alpha/2,n-2} \\) =", round(qt(1-input$alpha/2,length(x)-2),4)),
      br(),
      paste0("(3) SE( Y )= ", round(summary(fit)$sigma,4),
             "\\(\\times\\)",round( 1+sqrt(1/length(x)+(input$xstar-mean(x))^2/sum( (x-mean(x))^2 )),4),
             " = ",round(summary(fit)$sigma* sqrt(1+1/length(x)+(input$xstar-mean(x))^2/sum( (x-mean(x))^2 )),4)),
      br(),
      paste0("Combined above steps, ", (1 - input$alpha) * 100, "% PI for Y at X =",input$xstar,
             ": (",round(pi4y[2],4),",",round(pi4y[3],4),")"),
      br(),
      br()
      )
  })
  
  #===========================================
  # Scatterplot of X and Y
  #===========================================
  #
  output$scatter <- renderPlot({
    X= extract(input$x)
    Y= extract(input$y)
    lmfit = lm(Y~X)
    
   # for CI
   new = data.frame(X=seq(min(X)-0.1, max(X)+0.1, 0.001))
   yhat = predict(lmfit)
   
   #predict(lmfit, new, se.fit = TRUE)
   pred.w.plim <- predict(lmfit, new, 
                          interval = "prediction",
                          level=1-input$alpha)
   pred.w.clim <- predict(lmfit, new, 
                          interval = "confidence",
                          level=1-input$alpha)
   #matplot(new$X, cbind(pred.w.clim, pred.w.plim[,-1]),
   #        lty = c(1,2,2,3,3), col=c("red","blue","blue","purple","purple"),
   #        type = "l", 
   #        xlab=input$xvar, ylab =input$yvar,
   #        main=paste0("y.hat = ",round(lmfit$coef[1],4),"+",round(lmfit$coef[2],4),"x"))
   plot(X,yhat,type = "l", col="red",lty=2,lwd=2,
        xlab=input$xvar, ylab =input$yvar,
        main=paste0("y.hat = ",round(lmfit$coef[1],4),"+",round(lmfit$coef[2],4),"x"))
   points(X,Y,col="blue",pch=19)
   legend("topleft",c("Fitted line","data points"), 
          lty=c(2,NA),pch=c(NA,19),lwd=c(2,1),col=c("red","blue"),bty="n")
  })
  
  #===========================================
  # Plot CI and PI 
  #===========================================
  #
  output$plotcipi<- renderPlotly({
    y <- extract(input$y)
    x <- extract(input$x)
    m = lm(y~x)
    # construct prediction and confidence intervals using predict()
    m_ci <- as.data.frame(predict(m, interval = "confidence",level= 1-input$alpha)) 
    colnames(m_ci) = c("fit", "CI_lwr", "CI_upr")
    m_pi <- as.data.frame(predict(m, interval = "prediction",level=1-input$alpha)) 
    colnames(m_pi) = c("fit", "PI_lwr", "PI_upr")
    
    # merge the interval data frames with the data frame used in the model
    m_data <- merge( merge(model.frame(m), m_ci, by = "row.names"), m_pi)
    
    # make a plot using the merged model data frame
    ggplot(m_data)+ # use m_data in the plot
      aes(x = x)+ # put the 'disp' variable on the x axis
      geom_point(aes(y = y)) +
      # add points, put the 'mpg' variable on the y axis for these
      geom_ribbon(aes(ymin = PI_lwr, ymax = PI_upr), fill = "lightblue", alpha = .4)+
      # add a ribbon for the prediction interval, put the pi_lo/pi_hi values on the y axis for this, 
      # color it lightblue and make it semitransparent
      geom_ribbon(aes(ymin = CI_lwr, ymax = CI_upr), fill = "gray", alpha = .4) + 
      # add a ribbon for the confidence interval, put the ci_lo/ci_hi values on the y axis for this, 
      # color it lightblue and make it semitransparent
      geom_line(aes(y = fit),col="red",lty=3) + 
      # add a line for the fitted values, put the 'fit' values on the y axis
      labs(x=paste0("X = ",input$xvar), y= paste0("Y = ",input$yvar))+
      theme_minimal() # use a white background for the plot
    
  })
  
  
  #===========================================
  # Residual plot 
  #===========================================
  #
  output$plotresiduals <- renderPlot({
    x = extract(input$x)
    y = extract(input$y)
    lmfit = lm(y~x)
    par(mfrow=c(1,3), cex.main=1.7, cex.lab=1.8, cex.axis=2, mar=c(4,5,2,2))
    residuals = summary(lmfit)$residuals
    predicted = predict(lmfit)
    plot(residuals ~ predicted, 
         main="Residuals vs. Fitted Values", xlab="Fitted Values", ylab="Residuals", 
         pch=19, col = "blue")
    abline(h = 0, lty = 2)
    d = density(residuals)$y
    h = hist(residuals, plot = FALSE)
    hist(residuals, main="Histogram of Residuals", xlab="Residuals", 
         col="orange", prob = TRUE, ylim = c(0,max(max(d), max(h$density))))
    lines(density(residuals), col = "gray", lwd = 2)
    qqnorm(residuals, pch=19, col = "blue", main = "Normal Q-Q Plot of Residuals")
    qqline(residuals, col = "red", lwd = 2)
  }) # },height=300)
  
}

# Run the application
shinyApp(ui = ui, server = server)


