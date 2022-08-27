library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  theme = shinythemes::shinytheme('cerulean'),
  # Application title
  fluidRow(wellPanel(
    h2(strong('Inference about population proportion(s)')),
    'Make inference about population proportion(s) based on one-sample or two-sample data'
  ) # end wellPanel
  ), # end fluidRow
  withMathJax(),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput(
        inputId = "propinference",
        label = "Inference for:",
        choices = c("one proportion","two proportions"),
        multiple = FALSE,
        selected = "one proportion"),
      hr(),

      
      ##----------- inference about proportion ------- ##
      conditionalPanel(
        condition = "input.propinference == 'one proportion'",
        tags$b("Sample size, \\(n = \\)"),
        numericInput("n_oneprop", NULL,
                     value = 50, min = 0, step = 1),
        hr(),
        radioButtons(
          inputId = "propx_oneprop",
          label = NULL,
          choices = c(
            "Proportion of successes \\(\\hat{p}\\)" = "prop_true",
            "Number of successes \\(x\\)" = "prop_false")
        ),
        br(),
        conditionalPanel(
          condition = "input.propx_oneprop == 'prop_true'",
          tags$b("Proportion of successes"),
          numericInput("p_oneprop", "\\(\\hat{p} = \\)",
                       value = 0.3, min = 0, max = 1, step = 0.01)
        ),
        conditionalPanel(
          condition = "input.propx_oneprop == 'prop_false'",
          tags$b("Number of successes"),
          numericInput("x_oneprop", "\\(x = \\)",
                       value = 15, min = 0, step = 1)
        )
      ),
      
      ## inference about two props
      conditionalPanel(
        condition = "input.propinference == 'two proportions'",
        tags$b("Sample size 1, \\(n_1 = \\)"),
        numericInput("n1_twoprops", NULL,
                     value = 50, min = 0, step = 1),
        tags$b("Sample size 2,  \\(n_2 = \\)"),
        numericInput("n2_twoprops", NULL,
                     value = 50, min = 0, step = 1),
        br(),
        radioButtons(
          inputId = "propx_twoprops",
          label = NULL,
          choices = c(
            "Proportion of successes \\(\\hat{p}\\)" = "prop_true",
            "Number of successes \\(x\\)" = "prop_false")
        ),
        br(),
        conditionalPanel(
          condition = "input.propx_twoprops == 'prop_true'",
          tags$b("Proportion of successes"),
          numericInput("p1_twoprops", "\\(\\hat{p}_1 = \\)",
                       value = 0.3, min = 0, max = 1, step = 0.01),
          numericInput("p2_twoprops", "\\(\\hat{p}_2 = \\)",
                       value = 0.2, min = 0, max = 1, step = 0.01)
        ),
        conditionalPanel(
          condition = "input.propx_twoprops == 'prop_false'",
          tags$b("Number of successes"),
          numericInput("x1_twoprops", "\\(x_1 = \\)",
                       value = 10, min = 0, step = 1),
          numericInput("x2_twoprops", "\\(x_2 = \\)",
                       value = 12, min = 0, step = 1)
        )
      ),
      hr(),
      ## H0 and Ha for different cases
      #
      conditionalPanel(
        condition = "input.propinference =='one proportion'",
        numericInput("h01",
                     label = paste0("Null proportion value, ","\\( p_0 = \\)"),
                     value = 0.2, step = 0.01)
      ),
      conditionalPanel(
        condition = "input.propinference =='two proportions'",
        numericInput("h02",
                     label = paste0("Null proportions difference value, ","\\( \\delta_0 =p_1-p_2= \\)"),
                     value = 0, min=-1, step = 0.01,max=1)
      ),
      br(),
      
      tags$b("Null hypothesis"),
      
      conditionalPanel(
        condition = "input.propinference == 'one proportion'",
        sprintf("\\( H_0 : p = p_0 \\)")
      ),
      conditionalPanel(
        condition = "input.propinference == 'two proportions'",
        sprintf("\\( H_0 : p_1-p_2 = \\delta_0 \\)")
      ),
      br(),
      
      
      conditionalPanel(
        condition = "input.propinference== 'one proportion'",
        radioButtons(
          inputId = "alternative",
          label = "Alternative hypothesis",
          choices = c(
            "\\(H_a: p \\neq p_0 \\)" = "two.sided",
            "\\(H_a: p > p_0 \\)" = "greater",
            "\\(H_a: p < p_0 \\)" = "less")
        )
      ),
      
      conditionalPanel(
        condition = "input.propinference== 'two proportions'",
        radioButtons(
          inputId = "alternative2p",
          label = "Alternative hypothesis",
          choices = c(
            "\\(H_a: p_1-p_2 \\neq \\delta_0 \\)" = "two.sided",
            "\\(H_a: p_1-p_2 > \\delta_0\\)" = "greater",
            "\\(H_a: p_1-p_2 < \\delta_0\\)" = "less")
        )
      ),
      ## 
      hr(),
      sliderInput("alpha",
                  "Significance level \\(\\alpha = \\)",
                  min = 0.01,
                  max = 0.20,
                  value = 0.05
      ),
      hr(),
      HTML('<p>This application is created by Wei (Becky) Lin and Jennifer Browning and modified from the <a href="https://github.com/AntoineSoetewey/statistics-201"> code </a> 
           by Antoine Soetewey.</p>')
      # code: <a href="https://github.com/AntoineSoetewey/statistics-201">code </a>
      # <a href="https://antoinesoetewey.shinyapps.io/statistics-201/">App </a>
      
      ),
    
    #=============== Main Panel ===============
    mainPanel(
      conditionalPanel(
        condition = "input.propinference == 'one proportion'",
        uiOutput("results_oneprop")
      ),
      conditionalPanel(
        condition = "input.propinference == 'two proportions'",
        uiOutput("results_twoprops")
      ),
      br(),
      #h4("Area under the curve for the P-value (blue)"),
      plotOutput("plotpval"),
      h5("Make conclusion by comparing p-value with significance level: 
         We reject null hypothesis", "\\( (H_0) \\)", " whenever p-value < ","\\(\\alpha\\)", 
         ", otherwise we fail to reject", "\\( H_0 \\)"),
      br(),
      #h4("Region rejection (orange) and the observed test statistic"),
      plotOutput("plotRR"),
      h5("Make conclusion by checking whether the observed test statistic value is in the rejection region: 
         We reject ", "\\( H_0\\)", 
         " if the observed test statistic is within the rejection region, 
         otherwise we fail to reject ","\\( H_0.\\)" ),
      br(),
      br()
      )
  )
)

server <- function(input, output) {
  
  #==================================================
  # output: key test result for prop(s)
  #==================================================
  #
  prop.z.test <- function(x, n, p0 = 0.5, conf.level = 0.95, alternative = "two.sided") {
    
    ts.z <- NULL; cint <- NULL; p.val <- NULL
    phat <- x / n; qhat <- 1 - phat
    SE.phat <- sqrt((phat * qhat) / n)
    ts.z <- (phat - p0) / sqrt(p0*(1-p0)/n)
    # find p-value
    p.val <- if (alternative == "two.sided") {
      2 * pnorm(abs(ts.z), lower.tail = FALSE)
    } else if (alternative == "less") {
      pnorm(ts.z, lower.tail = TRUE)
    } else {
      pnorm(ts.z, lower.tail = FALSE)
    }
    # get CI
    cint <- phat + c(-1,1)*qnorm((1-conf.level)/2,lower.tail=F)*SE.phat
    # output result
    return(list(x = x, n = n, estimate = phat, 
                null.value = p0, stderr = SE.phat, 
                statistic = ts.z, p.value = p.val, conf.int = cint))
  }
  
  prop.z.test2 <- function(x1, x2, n1, n2, p0 = 0, conf.level = 0.95, alternative = "two.sided") {
    
    ts.z <- NULL; cint <- NULL; p.val <- NULL
    phat1 <- x1 / n1; qhat1 <- 1 - phat1
    phat2 <- x2 / n2; qhat2 <- 1 - phat2
    pooled.phat <- ((n1 * phat1) + (n2 * phat2)) / (n1 + n2)
    pooled.qhat <- 1 - pooled.phat
    if (p0 == 0) {
      SE.phat <- sqrt(pooled.phat * pooled.qhat * (1 / n1 + 1 / n2))
    } else {
      SE.phat <- sqrt((phat1 * qhat1) / n1 + (phat2 * qhat2) / n2)
    }
    ts.z <- (phat1 - phat2 - p0) / SE.phat
    # find p-value
    p.val <- if (alternative == "two.sided") {
      2 * pnorm(abs(ts.z), lower.tail = FALSE)
    } else if (alternative == "less") {
      pnorm(ts.z, lower.tail = TRUE)
    } else {
      pnorm(ts.z, lower.tail = FALSE)
    }
    # find CI
    se.ci = sqrt((phat1 * qhat1) / n1 + (phat2 * qhat2) / n2)
    cint <- (phat1 - phat2) + c(-1,1)*qnorm( (1-conf.level)/2,lower.tail=F)*se.ci
    # output result
    return(list(x1 = x1, x2 = x2, n1 = n1, n2 = n2, 
                estimate1 = phat1, estimate2 = phat2, 
                null.value = p0, stderr = SE.phat, 
                statistic = ts.z, p.value = p.val, conf.int = cint))
  }
  
 
  
  
  
  #==================================================
  # output: results of one proportion
  #==================================================
  #
  output$results_oneprop <- renderUI({
    
    options(scipen=999)

    if(input$propinference == "one proportion") {
      
      if(input$propx_oneprop == "prop_true"){
        test= prop.z.test(x = input$n_oneprop * input$p_oneprop, 
                          n = input$n_oneprop, p0 = input$h01, 
                          conf.level = 1 - input$alpha, alternative = input$alternative)
      }else if(input$propx_oneprop == "prop_false"){
        test =  prop.z.test(x = input$x_oneprop, 
                            n = input$n_oneprop, p0 = input$h01, 
                            conf.level = 1 - input$alpha, alternative = input$alternative)
      }
  
      
      withMathJax(
        #
        tags$b("Summary statistics of data"),
        br(),
        paste0("------------------------------------------------------"),
        br(),
        paste0("\\(\\ \\ \\ \\ \\ \\ n \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\hat{p} 
               \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\hat{q}=1-\\hat{p} \\) "),
        br(),
        paste0("------------------------------------------------------"),
        br(),
        paste0("\\(\\ \\ \\ \\ \\ \\)", test$n, 
               "\\(\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)", round(test$estimate,3),
               "\\(\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)",
               1-round(test$estimate,3) ),
        br(),
        paste0("------------------------------------------------------"),
        br(),
        br(),
        paste0("Check number of successes and number failures:"),
        br(),
        paste0("\\(n\\hat{p} = \\)", round(test$n*test$estimate,3), ", ", 
              "\\(n(1-\\hat{p}) = \\)", round(test$n*(1-test$estimate),3)),
        hr(),
        tags$b("Key facts"),
        br(),
        paste0("With larger sample size: \\( \\hat{p}\\sim N\\Big(p,\\sqrt{\\dfrac{p(1-p)}{n}}\\Big)\\), where p = population proportion"),
        br(),
        paste0("The Standard Error of \\( \\hat{p}: SE(\\hat{p}) = \\sqrt{\\dfrac{\\hat{p}(1-\\hat{p})}{n}} \\)"),
        hr(),
        tags$b("Confidence interval"),
        br(),
        paste0( (1 - input$alpha) * 100, 
                "% CI for \\(p: \\hat{p} \\pm z_{\\alpha/2}\\sqrt{\\dfrac{\\hat{p}(1-\\hat{p})}{n}} \\) "),
        br(),
        paste0("CI from data: ", round(test$estimate, 3), "  \\( \\pm \\) ", "\\( ( \\)", 
               round(qnorm(input$alpha / 2, lower.tail = FALSE), 3),
               " * ", round(test$stderr, 3), "\\( ) \\) ", "\\( = \\) ",
               "[", round(test$conf.int[1], 3), "; ", round(test$conf.int[2], 3), "]"
        ),
        br(),
        paste0("* This CI is more appropriate if : ", "\\( n\\hat{p}\\ge 15 \\)", " and ", "\\( n(1-\\hat{p})\\ge 15 \\)" ),
        br(),
        hr(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : p = \\) ", test$null.value,
               " and \\(H_a : p \\) ", 
               ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", 
                      ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), 
               test$null.value),
        br(),
        br(),
        
        paste0(
          "2. Observed test statistic : \\(z_{obs} = \\dfrac{\\hat{p} - p_0}{\\sqrt{\\dfrac{p_0(1-p_0)}{n}}} =\\) ",
          "(", round(test$estimate, 3), 
          ifelse(test$null.value >= 0, 
                 paste0(" - ", test$null.value), 
                 paste0(" + ", abs(test$null.value))),
          ") / ", round( sqrt((test$null.value)*(1-test$null.value)/test$n), 3), " \\( = \\) ",
          round(test$statistic, 3)
        ),
        br(),
        paste0("* This test is appropriate if : ", "\\( n\\hat{p}\\ge 10 \\)", " and ", "\\( n(1-\\hat{p})\\ge 10 \\)"),
        br(),
        paste0("3. Critical value and P-value :", 
               ifelse(input$alternative == "two.sided", 
                      " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", 
                      ifelse(input$alternative == "greater", 
                             " \\( z_{\\alpha} = z(\\)",
                             " \\( -z_{\\alpha} = -z(\\)")
               ),
               ifelse(input$alternative == "two.sided", input$alpha / 2, input$alpha),
               ", ", test$parameter, "\\()\\)", " \\( = \\) ",
               ifelse(input$alternative == "two.sided", 
                      "\\( \\pm \\)", ifelse(input$alternative == "greater", "", " -")),
               ifelse(input$alternative == "two.sided", 
                      round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), 
                      round(qnorm(input$alpha, lower.tail = FALSE), 3)),
               ", and p-value = ", round(test$p.value,5)
        ),
        br(),
        paste0("4. Conclusion : ", 
               ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Fail to reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", 
               ifelse(test$p.value < input$alpha, 
                      "we reject the null hypothesis that the true population proportion is ",
                      "we fail to reject the null hypothesis that the true population porportion is "), 
               test$null.value, " \\((p\\)-value ", 
               ifelse(test$p.value < 0.001, "< 0.001", 
                      paste0("\\(=\\) ", round(test$p.value, 3))),
               ")", ".")
        
        
      )
      
    } 
    
  }) # end-output$results_oneprop
  
  
  #==================================================
  # output: results of one proportion
  #==================================================
  #
  output$results_twoprops <- renderUI({
    
    options(scipen=999)
    
    if(input$propinference == "two proportions") {
      
      if(input$propx_twoprops == "prop_true"){
        test= prop.z.test2(x1 = input$n1_twoprops * input$p1_twoprops,
                           x2 = input$n2_twoprops * input$p2_twoprops,
                           n1= input$n1_twoprops, n2=input$n2_twoprops, 
                           p0 = input$h02, 
                          conf.level = 1 - input$alpha, 
                          alternative = input$alternative)
      }else if(input$propx_twoprops == "prop_false"){
        test =  prop.z.test2(x1 = input$x1_twoprops ,
                             x2 = input$x2_twoprops,
                             n1= input$n1_twoprops, n2=input$n2_twoprops,
                             p0 = input$h02, 
                            conf.level = 1 - input$alpha, alternative = input$alternative)
      }
      
      
      withMathJax(
        #
        tags$b("Summary statistics of data"),
        br(),
        paste0("------------------------------------------------------"),
        br(),
        paste0("\\(\\ \\ \\ \\ \\ \\ n \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\hat{p} 
               \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\hat{q}=1-\\hat{p} \\) "),
        br(),
        paste0("------------------------------------------------------"),
        br(),
        paste0("\\(\\ \\ \\ \\ \\ \\)", test$n1, 
               "\\(\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)", round(test$estimate1,3),
               "\\(\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)",
               1-round(test$estimate1,3) ),
        br(),
        paste0("\\(\\ \\ \\ \\ \\ \\)", test$n2, 
               "\\(\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)", round(test$estimate2,3),
               "\\(\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)",
               1-round(test$estimate2,3) ),
        br(),
        paste0("------------------------------------------------------"),
        br(),
        br(),
        paste0("Check number of successes and number failures:"),
        br(),
        paste0("\\(n_1\\hat{p}_1 = \\)", round(test$n1*test$estimate1,3), ", ", 
               "\\(n_1(1-\\hat{p}_1) = \\)", round(test$n1*(1-test$estimate1),3)),
        br(),
        paste0("\\(n_2\\hat{p}_2 = \\)", round(test$n2*test$estimate2,3), ", ", 
               "\\(n_2(1-\\hat{p}_2) = \\)", round(test$n2*(1-test$estimate2),3)),
        br(),
        paste0("Pooled (combined) sample proportion 
               = \\( \\hat{p} =\\dfrac{n_1\\hat{p}_1+n_2\\hat{p}_2}{n_1+n_2}\\) = ",
               round( (test$n1*test$estimate1+ test$n2*test$estimate2)/(test$n1+test$n2),3) ),
        hr(),
        br(),
        tags$b("Key facts"),
        br(),
        paste0("With larger sample size: ", 
               "\\( \\hat{p}_1 - \\hat{p}_2 \\sim N\\Big(p_1-p_2,\\sqrt{\\dfrac{p_1(1-p_1)}{n_1}+\\dfrac{p_2(1-p_2)}{n_2}}\\Big)\\)"),
        br(), 
        paste0(" where \\( p_1, p_2\\) are population proportions"),
        br(),
        paste0("SE( \\(\\hat{p}_1 - \\hat{p}_2) \\) = ",
               "\\( \\sqrt{\\dfrac{\\hat{p}_1(1-\\hat{p}_1)}{n_1}+\\dfrac{\\hat{p}_2(1-\\hat{p}_2)}{n_2}}\\)"),
        br(),
        hr(),
        
        tags$b("Confidence interval"),
        br(),
        paste0( (1 - input$alpha) * 100, 
                "% CI for \\( (p_1-p_2): (\\hat{p}_1-\\hat{p}_2)\\pm z_{\\alpha/2} \\)",
                "\\(\\sqrt{\\dfrac{\\hat{p}_1(1-\\hat{p}_1)}{n_1}+\\dfrac{\\hat{p}_2(1-\\hat{p}_2)}{n_2}} \\)"
                ),
        br(),
        paste0("CI from data: ( ", round(test$estimate1, 3), " - ",round(test$estimate2, 3)," )",
               "  \\( \\pm \\) ", "\\( ( \\)", 
               round(qnorm(input$alpha / 2, lower.tail = FALSE), 3),
               " * ", round(test$stderr, 3), "\\( ) \\) ", "\\( = \\) ",
               "[", round(test$conf.int[1], 3), "; ", round(test$conf.int[2], 3), "]"),
        br(),
        paste0("* This CI is more appropriate if : ", "\\( n_i\\hat{p}_i\\ge 10 \\)", " and ", "\\( n_i(1-\\hat{p}_i)\\ge 10, i=1, 2\\)" ),
        br(),
        hr(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : p_1-p_2 = \\) ", test$null.value,
               " and \\(H_a : p_1-p_2 \\) ", 
               ifelse(input$alternative2p == "two.sided", "\\( \\neq \\) ", 
                      ifelse(input$alternative2p == "greater", "\\( > \\) ", "\\( < \\) ")), 
               test$null.value),
        br(),
        paste0(
          "2. Observed test statistic : ", 
           ifelse(input$h02==0, 
           paste0("\\(z_{obs} = \\dfrac{(\\hat{p}_1-\\hat{p}_2) -\\delta_0}{\\sqrt{\\hat{p}(1-\\hat{p})\\Big(\\dfrac{1}{n_1}+\\dfrac{1}{n_2}\\Big)}}\\) "),
           paste0("\\(z_{obs} = \\dfrac{(\\hat{p}_1-\\hat{p}_2) -\\delta_0}{\\sqrt{\\dfrac{\\hat{p}_1(1-\\hat{p}_1)}{n_1}+\\dfrac{\\hat{p}_2(1-\\hat{p}_2)}{n_2}}}\\) ")),
           " = (", round(test$estimate1, 3)," - ",   round(test$estimate2, 3),
          ifelse(test$null.value >= 0, 
                 paste0(" - ", test$null.value), 
                 paste0(" + ", abs(test$null.value))),
          ") / ", round(test$stderr, 3), " \\( = \\) ",
          round(test$statistic, 3)
        ),
        br(),
        paste0("3. Critical value and P-value :", 
               ifelse(input$alternative2p == "two.sided", 
                      " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", 
                      ifelse(input$alternative2p == "greater", 
                             " \\( z_{\\alpha} = z(\\)",
                             " \\( -z_{\\alpha} = -z(\\)")
               ),
               ifelse(input$alternative2p == "two.sided", input$alpha / 2, input$alpha),
               ", ", test$parameter, "\\()\\)", " \\( = \\) ",
               ifelse(input$alternative2p == "two.sided", 
                      "\\( \\pm \\)", ifelse(input$alternative2p == "greater", "", " -")),
               ifelse(input$alternative2p == "two.sided", 
                      round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), 
                      round(qnorm(input$alpha, lower.tail = FALSE), 3)),
               ", and p-value = ", round(test$p.value,5)
        ),
        br(),
        paste0("4. Conclusion : ", 
               ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Fail to reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", 
               ifelse(test$p.value < input$alpha, 
                      "we reject the null hypothesis that the true population proportion is ",
                      "we fail to reject the null hypothesis that the true population porportion is "), 
               test$null.value, " \\((p\\)-value ", 
               ifelse(test$p.value < 0.001, "< 0.001", 
                      paste0("\\(=\\) ", round(test$p.value, 3))),
               ")", ".")
        
      )
      
    } 
    
  }) # end-output$results_twoprops
  
  
  ###########################################################
  ## Plot Rejection Region for given alpha
  ###########################################################
  #
  output$plotpval <- renderPlot({
    
    #=================================================
    # Plot P-value area: one proportion
    #=================================================
    if(input$propinference == "one proportion"){
      
      # t.test based on data
      if(input$propx_oneprop == "prop_true"){
        test= prop.z.test(x = input$n_oneprop * input$p_oneprop, 
                          n = input$n_oneprop, p0 = input$h01, 
                          conf.level = 1 - input$alpha, alternative = input$alternative)
      } else if(input$propx_oneprop == "prop_false"){
        test =  prop.z.test(x = input$x_oneprop, 
                            n = input$n_oneprop, p0 = input$h01, 
                            conf.level = 1 - input$alpha, alternative = input$alternative)
      }
     
      # plot range of x and y
      x = seq(-4.5, 4.5, by=.01)
      y = dnorm(x)
      # 
      par(mar=c(2, 0.5, 4, .5), 
          cex.main=1.5, 
          cex.lab=1.7, 
          cex.axis=1.5)
      plot(x, y, type='l', lwd=1, xlab='test statistic t', ylab="",
           xaxt='n', yaxt='n',
           main=paste0("Plot area for the p-value under null distribution : N(0,1)"))
      legend("topleft",paste0("p-value = ", round(test$p.value,5),
                              "\nalpha=",input$alpha),bty="n",cex=1.2,text.col="red")
      
      # find critical region
      if (input$alternative == "two.sided") {
        z.obs <- abs(round(test$statistic,3)) 
        axis(side=1, at=c(-z.obs,z.obs))
        abline(v=round(test$statistic, 3),col="red",lty=2, lwd=2)
        text(test$statistic, dnorm(0.4),"z-observed",
             pos = 2, srt = 90)
        if(z.obs<4.5){
          t.neg <- seq(-4.5, -z.obs, by=.01)
          t.pos <- seq(z.obs, 4.5, by=.01)}
        else{
          t.neg <-seq(-4.5, -4.48, by=.01)
          t.pos <- seq(4.48, 4.5, by=.01)}
        polygon(c(t.neg, -z.obs, -4.5), c(dnorm(t.neg),0,0), col="lightblue")
        polygon(c(t.pos, 4.5, z.obs), c(dnorm(t.pos), 0,0), col="lightblue")
      }
      else if (input$alternative == "greater") {
        z.obs <- round(test$statistic,3) 
        if(z.obs<4.5) t.seq <- seq(z.obs, 4.5, by=.01)
        else t.seq = seq(4.48, 4.5, by=.01)
        polygon( c(t.seq, 4.5, z.obs), c(dnorm(t.seq), 0, 0), col="lightblue")
        axis(side=1, at=z.obs)
        abline(v=round(test$statistic, 3),col="red",lty=2, lwd=2)
        text(test$statistic, dnorm(0.4),"z-observed",
             pos = 2, srt = 90)
      } else if (input$alternative == "less") {
        z.obs <- round(test$statistic,3) 
        if(z.obs>-4.5) t.seq <- seq(-4.5, z.obs, by=.01)
        else t.seq = seq(-4.5, -4.48, by=.01)
        polygon( c(t.seq, z.obs,-4.5), c(dnorm(t.seq), 0, 0), col="lightblue")
        axis(side=1, at=z.obs)
        abline(v=round(test$statistic, 3),col="red",lty=2, lwd=2)
        text(test$statistic, dnorm(0.4),"z-observed",
             pos = 2, srt = 90)
      }
    }
    
    
    #=================================================
    # Plot P-value area: two proportions
    #=================================================
    if(input$propinference == "two proportions"){
      
      # t.test based on data
      if(input$propx_twoprops == "prop_true"){
        test= prop.z.test2(x1 = input$n1_twoprops * input$p1_twoprops,
                           x2 = input$n2_twoprops * input$p2_twoprops,
                           n1= input$n1_twoprops, n2=input$n2_twoprops, 
                           p0 = input$h02, 
                           conf.level = 1 - input$alpha, 
                           alternative = input$alternative)
      }else if(input$propx_twoprops == "prop_false"){
        test =  prop.z.test2(x1 = input$x1_twoprops ,
                             x2 = input$x2_twoprops,
                             n1= input$n1_twoprops, n2=input$n2_twoprops,
                             p0 = input$h02, 
                             conf.level = 1 - input$alpha, alternative = input$alternative)
      }
      
      # plot range of x and y
      x = seq(-4.5, 4.5, by=.01)
      y = dnorm(x)
      # 
      par(mar=c(2, 0.5, 4, .5), 
          cex.main=1.5, 
          cex.lab=1.7, 
          cex.axis=1.5)
      plot(x, y, type='l', lwd=1, xlab='test statistic t', ylab="",
           xaxt='n', yaxt='n',
           main=paste0("Plot area for the p-value under null distribution : N(0,1)"))
      legend("topleft",paste0("p-value = ", round(test$p.value,5),
                              "\nalpha=",input$alpha),bty="n",cex=1.2,text.col="red")
      
      # find critical region
      if (input$alternative2p == "two.sided") {
        z.obs <- abs(round(test$statistic,3)) 
        axis(side=1, at=c(-z.obs,z.obs))
        abline(v=round(test$statistic, 3),col="red",lty=2, lwd=2)
        text(test$statistic, dnorm(0.4),"z-observed",
             pos = 2, srt = 90)
        if(z.obs<4.5){
          t.neg <- seq(-4.5, -z.obs, by=.01)
          t.pos <- seq(z.obs, 4.5, by=.01)}
        else{
          t.neg <-seq(-4.5, -4.48, by=.01)
          t.pos <- seq(4.48, 4.5, by=.01)}
        polygon(c(t.neg, -z.obs, -4.5), c(dnorm(t.neg),0,0), col="lightblue")
        polygon(c(t.pos, 4.5, z.obs), c(dnorm(t.pos), 0,0), col="lightblue")
      }
      else if (input$alternative2p == "greater") {
        z.obs <- round(test$statistic,3) 
        if(z.obs<4.5) t.seq <- seq(z.obs, 4.5, by=.01)
        else t.seq = seq(4.48, 4.5, by=.01)
        polygon( c(t.seq, 4.5, z.obs), c(dnorm(t.seq), 0, 0), col="lightblue")
        axis(side=1, at=z.obs)
        abline(v=round(test$statistic, 3),col="red",lty=2, lwd=2)
        text(test$statistic, dnorm(0.4),"z-observed",
             pos = 2, srt = 90)
      } else if (input$alternative2p == "less") {
        z.obs <- round(test$statistic,3) 
        if(z.obs>-4.5) t.seq <- seq(-4.5, z.obs, by=.01)
        else t.seq = seq(-4.5, -4.48, by=.01)
        polygon( c(t.seq, z.obs,-4.5), c(dnorm(t.seq), 0, 0), col="lightblue")
        axis(side=1, at=z.obs)
        abline(v=round(test$statistic, 3),col="red",lty=2, lwd=2)
        text(test$statistic, dnorm(0.4),"z-observed",
             pos = 2, srt = 90)
      }
    }
    
  })
  
  ###########################################################
  ## Plot Rejection Region for given alpha
  ###########################################################
  #
  output$plotRR <- renderPlot({
    
    #=================================================
    # Plot RR area: one prop
    # case I: given p.haat
    #=================================================
    
    if(input$propinference == "one proportion"){
      
      # t.test based on data
      # t.test based on data
      if(input$propx_oneprop == "prop_true"){
        test= prop.z.test(x = input$n_oneprop * input$p_oneprop, 
                          n = input$n_oneprop, p0 = input$h01, 
                          conf.level = 1 - input$alpha, alternative = input$alternative)
      }else if(input$propx_oneprop == "prop_false"){
        test =  prop.z.test(x = input$x_oneprop, 
                            n = input$n_oneprop, p0 = input$h01, 
                            conf.level = 1 - input$alpha, alternative = input$alternative)
      }
      # plot range of x and y
      x = seq(-4.5, 4.5, by=.01)
      y = dnorm(x)
      # 
      par(mar=c(2, 0.5, 4, .5), 
          cex.main=1.5, 
          cex.lab=1.7, 
          cex.axis=1.5)
      plot(x, y, type='l', lwd=1, xlab='test statistic t', ylab="",
           xaxt='n', yaxt='n',
           main=paste0("Plot area of Rejection Region under null distribution: N(0,1)"))
      # find critical region
      if (input$alternative == "two.sided") {
        z.cri <- qnorm(input$alpha/2, lower.tail = FALSE) 
        axis(side=1, at=c(-round(abs(z.cri),3), round(abs(z.cri),3)))
        if(z.cri<4.5){
          t.neg <- seq(-4.5, -z.cri, by=.01)
          t.pos <- seq(z.cri, 4.5, by=.01)}
        else{
          t.neg <-seq(-4.5, -4.48, by=.01)
          t.pos <- seq(4.48, 4.5, by=.01)}
        polygon(c(t.neg, -z.cri, -4.5), c(dnorm(t.neg),0,0), col="orange")
        polygon(c(t.pos, 4.5, z.cri), c(dnorm(t.pos), 0,0), col="orange")
      }
      else if (input$alternative == "greater") {
        z.cri <- qnorm(input$alpha, lower.tail = FALSE) 
        if(z.cri<4.5) t.seq <- seq(z.cri, 4.5, by=.01)
        else t.seq = seq(4.48, 4.5, by=.01)
        polygon( c(t.seq, 4.5, z.cri), c(dnorm(t.seq), 0, 0), col="orange")
        axis(side=1, at=c(round(abs(z.cri),3)))
      } else if (input$alternative == "less") {
        z.cri = qnorm(input$alpha, lower.tail = TRUE)
        if(z.cri>-4.5) t.seq <-seq(-4.5, z.cri, by=.01)
        else t.seq <- seq(-4.5, -4.48, by=.01)
        polygon(c(t.seq, z.cri, -4.5), c(dnorm(t.seq), 0, 0), col="orange")
        axis(side=1, at=c(-round(abs(z.cri),3) ))
      }
      abline(v=round(test$statistic, 3),col="red",lty=2, lwd=2)
      text(test$statistic, dnorm(0.4),"z-observed",
           pos = 2, srt = 90)
      legend("topleft",paste0("z-observed = ", round(test$stat,3), 
                              ifelse(input$alternative=="two.sided",
                                     paste0("\nRejection Reg. cutoff = ",round(abs(z.cri),3)),
                                     paste0("\nRejection Reg. cutoff = ",round(z.cri,3)))),
             bty="n",cex=1.3, text.col="red")
    }
    
    #=================================================
    # Plot RR area: two props
    #=================================================
    
    if(input$propinference == "two proportions"){
      
      # t.test based on data
      if(input$propx_twoprops == "prop_true"){
        test= prop.z.test2(x1 = input$n1_twoprops * input$p1_twoprops,
                           x2 = input$n2_twoprops * input$p2_twoprops,
                           n1= input$n1_twoprops, n2=input$n2_twoprops, 
                           p0 = input$h02, 
                           conf.level = 1 - input$alpha, 
                           alternative = input$alternative)
      }else if(input$propx_twoprops == "prop_false"){
        test =  prop.z.test2(x1 = input$x1_twoprops ,
                             x2 = input$x2_twoprops,
                             n1= input$n1_twoprops, n2=input$n2_twoprops,
                             p0 = input$h02, 
                             conf.level = 1 - input$alpha, alternative = input$alternative)
      }
  
    # plot range of x and y
    x = seq(-4.5, 4.5, by=.01)
    y = dnorm(x)
    # 
    par(mar=c(2, 0.5, 4, .5), 
        cex.main=1.5, 
        cex.lab=1.7, 
        cex.axis=1.5)
    plot(x, y, type='l', lwd=1, xlab='test statistic t', ylab="",
         xaxt='n', yaxt='n',
         main=paste0("Plot area of Rejection Region under null distribution: N(0,1)"))
    # find critical region
    if (input$alternative2p == "two.sided") {
      z.cri <- qnorm(input$alpha/2, lower.tail = FALSE) 
      axis(side=1, at=c(-round(abs(z.cri),3), round(abs(z.cri),3)))
      if(z.cri<4.5){
        t.neg <- seq(-4.5, -z.cri, by=.01)
        t.pos <- seq(z.cri, 4.5, by=.01)}
      else{
        t.neg <-seq(-4.5, -4.48, by=.01)
        t.pos <- seq(4.48, 4.5, by=.01)}
      polygon(c(t.neg, -z.cri, -4.5), c(dnorm(t.neg),0,0), col="orange")
      polygon(c(t.pos, 4.5, z.cri), c(dnorm(t.pos), 0,0), col="orange")
    }
    else if (input$alternative2p == "greater") {
      z.cri <- qnorm(input$alpha, lower.tail = FALSE) 
      if(z.cri<4.5) t.seq <- seq(z.cri, 4.5, by=.01)
      else t.seq = seq(4.48, 4.5, by=.01)
      polygon( c(t.seq, 4.5, z.cri), c(dnorm(t.seq), 0, 0), col="orange")
      axis(side=1, at=c(round(abs(z.cri),3)))
    } else if (input$alternative2p == "less") {
      z.cri = qnorm(input$alpha, lower.tail = TRUE)
      if(z.cri>-4.5) t.seq <-seq(-4.5, z.cri, by=.01)
      else t.seq <- seq(-4.5, -4.48, by=.01)
      polygon(c(t.seq, z.cri, -4.5), c(dnorm(t.seq), 0, 0), col="orange")
      axis(side=1, at=c(-round(abs(z.cri),3) ))
    }
    abline(v=round(test$statistic, 3),col="red",lty=2, lwd=2)
    text(test$statistic, dnorm(0.4),"z-observed",
         pos = 2, srt = 90)
    legend("topleft",paste0("z-observed = ", round(test$stat,3), 
                            ifelse(input$alternative2p=="two.sided",
                                   paste0("\nRejection Reg. cutoff = ",round(abs(z.cri),3)),
                                   paste0("\nRejection Reg. cutoff = ",round(z.cri,3)))),
           bty="n",cex=1.3, text.col="red")
  }
    
    # =================== end of all cases ==========
    
  }) # end: output$plotRR

} # end server code

# Run the application
shinyApp(ui = ui, server = server)