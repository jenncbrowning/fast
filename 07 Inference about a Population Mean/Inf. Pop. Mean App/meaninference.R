#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
#library(EnvStats)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinythemes::shinytheme('cerulean'),
  # Application title
  fluidRow(wellPanel(
    h2(strong('Inference about population mean(s)')),
    'Make inference about population mean(s) based on one-sample or two-sample data'
  ) # end wellPanel
  ), # end fluidRow
  withMathJax(),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput(
        inputId = "meaninference",
        label = "Inference for:",
        choices = c("one mean", "two means", "two means with paired samples"),
        multiple = FALSE,
        selected = "one mean"),
      hr(),
      
      conditionalPanel(
        condition = "input.meaninference == 'one mean'",
        textInput("sample_onemean", "Sample", 
                  value = "1.2, 0.9, -0.8, 1.3, -0.3, 0.2, 0.8, 1.6",
                  #value = "16,15,18,16,11,11,8,4", 
                  placeholder = "Enter observations separated by a comma with decimals as points, e.g. 3.2, 4.5, 5.7, 6.03, etc."),
        hr(),
        checkboxInput("popsd_onemean", "Known population standard deviation", FALSE),
        conditionalPanel(
          condition = "input.popsd_onemean == 1",
          numericInput("sigma_onemean", "\\(\\sigma = \\)",
                       value = 1, min = 0, step = 1)
        )
      ),
      
      conditionalPanel(
        condition = "input.meaninference == 'two means'",
        textInput("sample1_twomeans", "Sample 1", 
                  value = "80,75,86,100,88,83,87,92,70",
                  placeholder = "Enter observartions separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
        textInput("sample2_twomeans", "Sample 2", 
                  value = "69,65,90,78,80,89,65,80,65",
                  placeholder = "Enter observations separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
        hr(),
        
        conditionalPanel(
          condition = "input.popsd_twomeans == 0",
          radioButtons(
            inputId = "var.equal",
            label = "Assuming",
            choices = c(
              "\\( \\sigma_1 \\neq \\sigma_2 \\)" = FALSE,
              "\\( \\sigma_1 = \\sigma_2 \\)" = TRUE)
            ),
        ),
        checkboxInput("popsd_twomeans", "Known standard deviations of both populations", FALSE),
        conditionalPanel(
          condition = "input.popsd_twomeans == 1",
          numericInput("sigma1_twomeans", "\\(\\sigma_1 = \\)",
                       value = 8, min = 0, step = 0.1),
          numericInput("sigma2_twomeans", "\\(\\sigma_2 = \\)",
                       value = 10, min = 0, step = 0.1)
        )
      ),
      
      conditionalPanel(
        condition = "input.meaninference == 'two means with paired samples'",
        textInput("sample1_twomeanspaired", "Sample 1",
                  #value = "0.9, -0.8, 0.1, -0.3, 0.2",
                  #value = "16,6,23,19,15,20,24,24",
                  value = "80,75,86,100,88,83,87,92,70",
                  placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
        textInput("sample2_twomeanspaired", "Sample 2", 
                  #value = "0.8, -0.9, -0.1, 0.4, 0.1",
                  #value = "0,1,5,3,4,9,16,20", 
                  value = "69,65,90,78,80,89,65,80,65",
                  placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
        hr(),
        checkboxInput("popsd_twomeanspaired", "\\( \\sigma_D \\) is known", FALSE),
        conditionalPanel(
          condition = "input.popsd_twomeanspaired == 1",
          numericInput("sigma_twomeanspaired", "\\(\\sigma_D = \\)",
                       value = 9, min = 0, step = 0.1)
        )
      ),
      hr(),
      br(),
      sliderInput("alpha",
                  "Significance level \\(\\alpha = \\)",
                  min = 0.01,
                  max = 0.20,
                  value = 0.05
      ),
      hr(),
    

      conditionalPanel(
        condition = "input.meaninference == 'one mean'",
        numericInput("h01",
                     label = paste0("Null mean value, ","\\( \\mu_0 = \\)"),
                     value = 0, step = 0.01)
      ),
      conditionalPanel(
        condition = "input.meaninference !='one mean'",
        numericInput("h02",
                     label = paste0("Null mean difference value, ","\\( \\delta_0 = \\)"),
                     value = 0, step = 0.01
        )
      ),
      br(),
      
      tags$b("Null hypothesis"),
      conditionalPanel(
        condition = "input.meaninference == 'one mean'",
        sprintf("\\( H_0 : \\mu = \\mu_0\\)")
      ),
      conditionalPanel(
        condition = "input.meaninference == 'two means'",
        sprintf("\\( H_0 : \\mu_1 - \\mu_2 =\\delta_0 \\)")
      ),
      conditionalPanel(
        condition = "input.meaninference == 'two means with paired samples'",
        sprintf("\\( H_0 : \\mu_D = \\delta_0 \\)")
      ),
      br(),
      
      conditionalPanel(
        condition = "input.meaninference== 'one mean'",
        radioButtons(
          inputId = "alternative",
          label = "Alternative hypothesis",
          choices = c(
            "\\(H_a: \\mu \\neq \\mu_0 \\)" = "two.sided",
            "\\(H_a:  \\mu > \\mu_0\\)" = "greater",
            "\\(H_a: \\mu < \\mu_0\\)" = "less"
          )
        )
      ),
      conditionalPanel(
        condition = "input.meaninference== 'two means'",
        radioButtons(
          inputId = "alternative2m",
          label = "Alternative hypothesis",
          choices = c(
            "\\(H_a: \\mu_1-\\mu_2 \\neq \\delta_0 \\)" = "two.sided",
            "\\(H_a: \\mu_1-\\mu_2 > \\delta_0\\)" = "greater",
            "\\(H_a: \\mu_1-\\mu_2 < \\delta_0\\)" = "less"
          )
        )
      ),
      
      conditionalPanel(
        condition = "input.meaninference== 'two means with paired samples'",
        radioButtons(
          inputId = "alternative2p",
          label = "Alternative hypothesis",
          choices = c(
            "\\(H_a: \\mu_D \\neq \\delta_0 \\)" = "two.sided",
            "\\(H_a: \\mu_D > \\delta_0\\)" = "greater",
            "\\(H_a: \\mu_D < \\delta_0\\)" = "less"
          )
        )
      ),
      hr(),
      HTML('<p>This app is created by created by Wei (Becky) Lin and Jennifer Browning and modified from the <a href="https://antoinesoetewey.shinyapps.io/statistics-201/">App</a> 
          by Antoine Soetewey.</p>')
      # code: <a href="https://github.com/AntoineSoetewey/statistics-201">code </a>
      
    ),
    
    #=============== Main Panel ===============
    mainPanel(
      conditionalPanel(
        condition = "input.meaninference == 'one mean'",
        uiOutput("results_onemean")
      ),
      conditionalPanel(
        condition = "input.meaninference == 'two means'",
        uiOutput("results_twomeans")
      ),
      conditionalPanel(
        condition = "input.meaninference == 'two means with paired samples'",
        uiOutput("results_twomeanspaired")
      ),
      br(),
      #h4("Area under the curve for the P-value (blue)"),
      plotOutput("plotpval"),
      h5("Make conclusion by comparing p-value with significance level: 
         We reject null hypothesis", "\\( (H_0) \\)", " whenever p-value < ","\\(\\alpha\\)", 
         ", otherwise we fail to reject", "\\( H_0 \\)."),
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
  # read in data
  extract <- function(text) {
    text <- gsub(" ", "", text)
    split <- strsplit(text, ",", fixed = FALSE)[[1]]
    as.numeric(split)
  }
  
  
  t.test2 <- function(x, V, m0 = 0, alpha = 0.05, alternative = "two.sided") {
    # one sample inference given the population SD
    M <- mean(x)
    n <- length(x)
    sigma <- sqrt(V)
    s <- sqrt(V / n)
    statistic <- (M - m0) / s
    p <- if (alternative == "two.sided") {
      2 * pnorm(abs(statistic), lower.tail = FALSE)
    } else if (alternative == "less") {
      pnorm(statistic, lower.tail = TRUE)
    } else {
      pnorm(statistic, lower.tail = FALSE)
    }
    # p <- (1 - pnorm((M-m0)/S))
    LCL <- M - qnorm(1 - alpha / 2)*s
    UCL <- M + qnorm(1 - alpha / 2)*s
    value <- list(mean = M, m0 = m0, sigma = sigma, 
                  statistic = statistic, 
                  p.value = p, 
                  LCL = LCL, UCL = UCL, alternative = alternative)
    return(value)
  }
  
  t.test3 <- function(x, y, V1, V2, m0 = 0, alpha = 0.05, alternative = "two.sided") {
    # two sample t-test given variances
    M1 <- mean(x)
    M2 <- mean(y)
    n1 <- length(x)
    n2 <- length(y)
    sigma1 <- sqrt(V1)
    sigma2 <- sqrt(V2)
    S <- sqrt((V1 / n1) + (V2 / n2))
    statistic <- (M1 - M2 - m0) / S
    p <- if (alternative == "two.sided") {
      2 * pnorm(abs(statistic), lower.tail = FALSE)
    } else if (alternative == "less") {
      pnorm(statistic, lower.tail = TRUE)
    } else {
      pnorm(statistic, lower.tail = FALSE)
    }
    # p <- (1 - pnorm((M-m0)/S))
    LCL <- (M1 - M2 - S * qnorm(1 - alpha / 2))
    UCL <- (M1 - M2 + S * qnorm(1 - alpha / 2))
    value <- list(mean1 = M1, mean2 = M2, 
                  m0 = m0, sigma1 = sigma1, sigma2 = sigma2, 
                  S = S, statistic = statistic,
                  p.value = p, LCL = LCL, UCL = UCL, alternative = alternative)
    return(value)
  }
  
  #==================================================
  # output: results of one mean
  #==================================================
  #
  output$results_onemean <- renderUI({
    
    mydata <- extract(input$sample_onemean)
    options(scipen=999)
    
    if (anyNA(mydata) | length(mydata) < 2) {
      "Invalid input or not enough observations"
    } else if (input$meaninference == "one mean" & input$popsd_onemean == FALSE) {
      test_confint <- t.test(x = mydata, mu = input$h01, 
                             alternative = "two.sided", 
                             conf.level = 1 - input$alpha)
      test <- t.test(x = mydata, mu = input$h01, 
                     alternative = input$alternative,
                     conf.level = 1 - input$alpha)
      withMathJax(
        tags$b("Display the data:"),
        br(),
        paste(c(paste(mydata, collapse = ", ")), collapse = " "),
        br(),
        br(),
        tags$b("Summary statistics of data"),
        br(),
        paste0("---------------------------------------"),
        br(),
        paste0("\\(\\ \\ \\ \\ \\ \\ n \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\bar{x} 
               \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ s \\) "),
        br(),
        paste0("---------------------------------------"),
        br(),
        paste0("\\(\\ \\ \\ \\ \\ \\)", length(mydata), 
               "\\(\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)", round(mean(mydata), 3),
               "\\(\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)",
               round(sd(mydata),3) ),
        br(),
        paste0("---------------------------------------"),
        br(),
        hr(),
        
        tags$b("Confidence interval"),
        br(),
        paste0( (1 - input$alpha) * 100, 
                "% CI for \\(\\mu: \\bar{x} \\pm t_{\\alpha/2, n - 1} \\dfrac{s}{\\sqrt{n}} \\) "),
        br(),
        paste0("CI from data: ", round(test_confint$estimate, 3), "  \\( \\pm \\) ", "\\( ( \\)", 
               round(qt(input$alpha / 2, df = test_confint$parameter, lower.tail = FALSE), 3),
               " * ", round(sd(mydata), 3),
               " / ", round(sqrt(length(mydata)), 3), "\\( ) \\) ", "\\( = \\) ",
               "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        hr(),
        
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : \\mu = \\) ", test$null.value,
               " and \\(H_a : \\mu \\) ", 
               ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", 
                      ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), 
               test$null.value),
        br(),
        br(),
        paste0(
          "2. Observed test statistic : \\(t_{obs} = \\dfrac{\\bar{x} - \\mu_0}{s / \\sqrt{n}} = \\) ",
          "(", round(test$estimate, 3), 
          ifelse(test$null.value >= 0, 
                 paste0(" - ", test$null.value), 
                 paste0(" + ", abs(test$null.value))),
          ") / ", round(test$stderr, 3), " \\( = \\) ",
          round(test$statistic, 3)
        ),
        br(),
        paste0("3. Critical value and P-value :", 
               ifelse(input$alternative == "two.sided", 
                      " \\( \\pm t_{\\alpha/2, n - 1} = \\pm t(\\)", 
                      ifelse(input$alternative == "greater", 
                             " \\( t_{\\alpha, n - 1} = t(\\)",
                             " \\( -t_{\\alpha, n - 1} = -t(\\)")
               ),
               ifelse(input$alternative == "two.sided", input$alpha / 2, input$alpha),
               ", ", test$parameter, "\\()\\)", " \\( = \\) ",
               ifelse(input$alternative == "two.sided", 
                      "\\( \\pm \\)", ifelse(input$alternative == "greater", "", " -")),
               ifelse(input$alternative == "two.sided", 
                      round(qt(input$alpha / 2, df = test$parameter, lower.tail = FALSE), 3), 
                      round(qt(input$alpha, df = test$parameter, lower.tail = FALSE), 3)),
               ", and p-value = ", round(test$p.value,7)
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
                      "we reject the null hypothesis that the true mean is ",
                      "we fail to reject the null hypothesis that the true mean is "), 
               test$null.value, " \\((p\\)-value ", 
               ifelse(test$p.value < 0.001, "< 0.001", 
                      paste0("\\(=\\) ", round(test$p.value, 3))),
               ")", ".")
      )
    
    } else if (input$meaninference == "one mean" & input$popsd_onemean==TRUE){
      # fit data
      test <- t.test2(x = mydata, V = (input$sigma_onemean)^2, m0 = input$h01, 
                      alpha = input$alpha, alternative = input$alternative)

        withMathJax(
          tags$b("Display the data:"),
          br(),
          paste(c(paste(mydata, collapse = ", ")), collapse = " "),
          br(),
          br(),
          tags$b("Summary statistics of data"),
          br(),
          paste0("---------------------------------------"),
          br(),
          paste0("\\(\\ \\ \\ \\ \\ \\ n \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\bar{x} 
       \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ s \\) "),
          br(),
          paste0("---------------------------------------"),
          br(),
          paste0("\\(\\ \\ \\ \\ \\ \\)", length(mydata), 
                 "\\(\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)", round(mean(mydata), 3),
                 "\\(\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)",
                 round(sd(mydata),3) ),
          br(),
          paste0("---------------------------------------"),
          br(),
          hr(),
          
          tags$b("Confidence interval"),
          br(),
          paste0( (1 - input$alpha) * 100, 
                  "% CI for \\(\\mu: \\bar{x} \\pm z_{\\alpha/2} \\dfrac{\\sigma}{\\sqrt{n}} \\) "),
          br(),
          paste0("CI from data: ", round(test$mean, 3), "  \\( \\pm \\) ",
                 round(qnorm(input$alpha/2,lower.tail=F),3),"*",  "\\( ( \\)",
                 round(test$sigma,3)," / ", round(sqrt(length(mydata)),3),"\\( ) \\ = \\ \\)",
                 "[ ", round(test$LCL, 3), ", ", round(test$UCL)," ]"
                 ),
          br(),
          hr(),
          
          
          tags$b("Hypothesis test"),
          br(),
          paste0("1. \\(H_0 : \\mu = \\) ", input$h01, " and \\(H_a : \\mu \\) ", 
                 ifelse(input$alternative == "two.sided", "\\( \\neq \\) ", 
                        ifelse(input$alternative == "greater", "\\( > \\) ", "\\( < \\) ")), input$h01),
          br(),
          br(),
          paste0(
            "2. Observed test statistic : \\(z_{obs} = \\dfrac{\\bar{x} - \\mu_0}{\\sigma / \\sqrt{n}} = \\) ",
            "(", round(test$mean, 3), ifelse(input$h01 >= 0, 
                                             paste0(" - ", input$h01), 
                                             paste0(" + ", abs(input$h01))), ") / ", 
            round(test$sigma / sqrt(length(mydata)), 3), " \\( = \\) ",
            round(test$statistic, 3)
          ),
          br(),
          paste0(
            "3. Critical value and P-value :", ifelse(input$alternative == "two.sided", " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", 
                                          ifelse(input$alternative == "greater", " \\( z_{\\alpha} = z(\\)", " \\( -z_{\\alpha} = -z(\\)")),
            ifelse(input$alternative == "two.sided", 
                   input$alpha / 2, input$alpha), 
            "\\()\\)", " \\( = \\) ",
            ifelse(input$alternative == "two.sided",
                   "\\( \\pm \\)", ifelse(input$alternative == "greater", "", " -")),
            ifelse(input$alternative == "two.sided", 
                   round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), 
                   round(qnorm(input$alpha, lower.tail = FALSE), 3)),
            ", and P-value =", round(test$p.value,5)
          ),
          br(),
          paste0("4. Conclusion : ", ifelse(test$p.value < input$alpha, "Reject \\(H_0\\)", "Fail to reject \\(H_0\\)")),
          br(),
          br(),
          tags$b("Interpretation "),
          br(),
          paste0("At the ", input$alpha * 100, "% significance level, ", 
                 ifelse(test$p.value < input$alpha, 
                        "we reject the null hypothesis that the true mean is ", 
                        "we fail to reject the null hypothesis that the true mean is "),
                 input$h01, " \\((P\\)-value ", 
                 ifelse(test$p.value < 0.001,"< 0.001", paste0(" = ", round(test$p.value, 3))), ")."),
          br()
        
        )
      }
    
  }) # end-output$results_onemean
  
  
  #==================================================
  # output: results of two means with paired samples
  #==================================================
  #
  output$results_twomeanspaired <- renderUI({
    dat1 <- extract(input$sample1_twomeanspaired)
    dat2 <- extract(input$sample2_twomeanspaired)
    
    if (anyNA(dat1) | length(dat1) < 2 | anyNA(dat2) | length(dat2) < 2) {
      "Invaid input or not enough observations"
    } else if (length(dat1) != length(dat2)) {
      "Number of observations must be equal in the two samples"
    } else if (input$meaninference == "two means with paired samples" & input$popsd_twomeanspaired == FALSE) {
      test_confint <- t.test(x = dat1, y = dat2, 
                             mu = input$h02, 
                             alternative = "two.sided", 
                             conf.level = 1 - input$alpha, paired = TRUE)
      test <- t.test(x = dat1, y = dat2, 
                     mu = input$h02, 
                     alternative = input$alternative2p, 
                     conf.level = 1 - input$alpha, paired = TRUE)
      withMathJax(
        tags$b("Display sample 1 data:"),
        br(),
        paste(c(paste(dat1, collapse = ", ")), collapse = " "),
        br(),
        br(),
        tags$b("Display sample 2 data:"),
        br(),
        paste(c(paste(dat2, collapse = ", ")), collapse = " "),
        br(),
        br(),
        tags$b("Display difference = sample 1- sample 2:"),
        br(),
        paste(c(paste(dat1-dat2, collapse = ", ")), collapse = " "),
        br(),
        br(),
        tags$b("Summary statistics of data"),
        br(),
        paste0("-----------------------------------------------------------"),
        br(),
        paste0("\\(\\ \\ \\ \\ \\ \\ Sample \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ n
              \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\bar{x} 
               \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ s \\) "),
        br(),
        paste0("-----------------------------------------------------------"),
        br(),
        paste0("\\(\\ \\ \\ \\ \\ \\ Sample 1 \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)", length(dat1), 
               "\\(\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)", round(mean(dat1), 3),
               "\\(\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)",
               round(sd(dat1),3) ),
        br(),
        paste0("\\(\\ \\ \\ \\ \\ \\ Sample 2 \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)", length(dat2), 
               "\\(\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)", round(mean(dat2), 3),
               "\\(\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)",
               round(sd(dat2),3) ),
        br(),
        paste0("\\(\\ \\ \\ \\ \\ \\ Difference \\ \\ \\ \\ \\ \\)", length(dat1-dat2), 
               "\\(\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)", round(mean(dat1-dat2), 3),
               "\\(\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)",
               round(sd(dat1-dat2),3) ),
        br(),
        paste0("-----------------------------------------------------------"),
        br(),
        hr(),
        
        tags$b("Confidence interval"),
        br(),
        paste0( (1 - input$alpha) * 100, 
                "% CI for \\(\\mu_D: \\bar{x}_D \\pm t_{\\alpha/2, n - 1} \\dfrac{s_D}{\\sqrt{n}} \\) "),
        br(),
        paste0("CI from data: ", round(test_confint$estimate, 3), "  \\( \\pm \\) ", 
               round(qt(input$alpha / 2, df = test_confint$parameter, lower.tail = FALSE), 3),
               " * ", "\\( ( \\)",  round(sd(dat1-dat2), 3),
               " / ", round(sqrt(length(dat1)), 3), "\\( ) \\) ", "\\( = \\) ",
               "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        hr(),
        
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : \\mu_D = \\) ", test$null.value,
               " versus \\(H_a : \\mu_D \\) ", 
               ifelse(input$alternative2p == "two.sided", "\\( \\neq \\) ", 
                      ifelse(input$alternative2p == "greater", "\\( > \\) ", "\\( < \\) ")), 
               test$null.value),
        br(),
        paste0(
          "2. Observed test statistic : \\(t_{obs} = \\dfrac{\\bar{x}_D - \\delta_0}{s_D / \\sqrt{n}}= \\) ",
          "(", round(test$estimate, 3), 
          ifelse(test$null.value >= 0, 
                 paste0(" - ", test$null.value), 
                 paste0(" + ", abs(test$null.value))),
          ") / ", round(test$stderr, 3), " \\( = \\) ",
          round(test$statistic, 3)
        ),
        br(),
        paste0("3. Critical value and P-value :", 
               ifelse(input$alternative2p == "two.sided", 
                      " \\( \\pm t_{\\alpha/2, n - 1} = \\pm t(\\)", 
                      ifelse(input$alternative2p == "greater", 
                             " \\( t_{\\alpha, n - 1} = t(\\)",
                             " \\( -t_{\\alpha, n - 1} = -t(\\)")
               ),
               ifelse(input$alternative2p == "two.sided", input$alpha / 2, input$alpha),
               ", ", test$parameter, "\\()\\)", " \\( = \\) ",
               ifelse(input$alternative2p == "two.sided", 
                      "\\( \\pm \\)", ifelse(input$alternative2p == "greater", "", " -")),
               ifelse(input$alternative == "two.sided", 
                      round(qt(input$alpha/2, df = test$parameter, lower.tail = FALSE), 3), 
                      round(qt(input$alpha, df = test$parameter, lower.tail = FALSE), 3)),
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
                      "we reject the null hypothesis that the true mean difference is ",
                      "we fail to reject the null hypothesis that the true mean difference is "), 
               test$null.value, " \\((p\\)-value ", 
               ifelse(test$p.value < 0.001, "< 0.001", 
                      paste0("\\(=\\) ", round(test$p.value, 5))),
               ")", "."),
        br()
      ) # end MathJax
    } else if (input$meaninference == "two means with paired samples" & input$popsd_twomeanspaired == TRUE) {
      test <- t.test2(x = dat1 - dat2, 
                      V = (input$sigma_twomeanspaired)^2, 
                      m0 = input$h02, 
                      alpha = input$alpha, alternative = input$alternative2p)
      options(scipen=999)
      withMathJax(
        tags$b("Display sample 1 data:"),
        br(),
        paste(c(paste(dat1, collapse = ", ")), collapse = " "),
        br(),
        br(),
        tags$b("Display sample 2 data:"),
        br(),
        paste(c(paste(dat2, collapse = ", ")), collapse = " "),
        br(),
        br(),
        tags$b("Display difference = sample 1- sample 2:"),
        br(),
        paste(c(paste(dat1-dat2, collapse = ", ")), collapse = " "),
        br(),
        br(),
        tags$b("Summary statistics of data"),
        br(),
        paste0("-----------------------------------------------------------"),
        br(),
        paste0("\\(\\ \\ \\ \\ \\ \\ Sample \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ n
               \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\bar{x} 
               \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ s \\) "),
        br(),
        paste0("-----------------------------------------------------------"),
        br(),
        paste0("\\(\\ \\ \\ \\ \\ \\ Sample 1 \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)", length(dat1), 
               "\\(\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)", round(mean(dat1), 3),
               "\\(\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)",
               round(sd(dat1),3) ),
        br(),
        paste0("\\(\\ \\ \\ \\ \\ \\ Sample 2 \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)", length(dat2), 
               "\\(\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)", round(mean(dat2), 3),
               "\\(\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)",
               round(sd(dat2),3) ),
        br(),
        paste0("\\(\\ \\ \\ \\ \\ \\ Difference \\ \\ \\ \\ \\ \\)", length(dat1-dat2), 
               "\\(\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)", round(mean(dat1-dat2), 3),
               "\\(\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)",
               round(sd(dat1-dat2),3) ),
        br(),
        paste0("-----------------------------------------------------------"),
        br(),
        hr(),
        
        tags$b("Confidence interval"),
        br(),
        paste0( (1 - input$alpha) * 100, 
                "% CI for \\(\\mu_D: \\bar{x}_D \\pm z_{\\alpha/2} \\dfrac{\\sigma_D}{\\sqrt{n}} \\) "),
        br(),
        paste0("CI from data: ", round(test$mean, 3), "  \\( \\pm \\) ", 
               round(qnorm(input$alpha / 2, lower.tail = FALSE), 3),
               " * ", "\\( ( \\)",  round(test$sigma, 3),
               " / ", round(sqrt(length(dat1)), 3), "\\( ) \\) ", "\\( = \\) ",
               "[", round(test$LCL, 3), "; ", round(test$UCL, 3), "]"
        ),
        br(),
        hr(),
        
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : \\mu_D = \\) ", input$h02, " and \\(H_a : \\mu_D \\) ", 
               ifelse(input$alternative2p == "two.sided", "\\( \\neq \\) ", 
                      ifelse(input$alternative2p == "greater", "\\( > \\) ", "\\( < \\) ")), input$h02),
        br(),
        br(),
        paste0(
          "2. Observed test statistic : \\(z_{obs} = \\dfrac{\\bar{x}_D - \\delta_0}{\\sigma_D / \\sqrt{n}} = \\) ",
          "(", round(test$mean, 3), ifelse(input$h02 >= 0, 
                                           paste0(" - ", input$h02), 
                                           paste0(" + ", abs(input$h02))), ") / ", 
          round(test$sigma / sqrt(length(dat1)), 3), " \\( = \\) ",
          round(test$statistic, 3)
        ),
        br(),
        paste0(
          "3. Critical value and P-value :", ifelse(input$alternative2p == "two.sided", 
                                        " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", 
                                        ifelse(input$alternative2p == "greater", 
                                               " \\( z_{\\alpha} = z(\\)", " \\( -z_{\\alpha} = -z(\\)")),
          ifelse(input$alternative2p == "two.sided", 
                 input$alpha / 2, input$alpha), 
          "\\()\\)", " \\( = \\) ",
          ifelse(input$alternative2p == "two.sided",
                 "\\( \\pm \\)", ifelse(input$alternative2p == "greater", "", " -")),
          ifelse(input$alternative2p == "two.sided", 
                 round(qnorm(input$alpha/2, lower.tail = FALSE), 3), 
                 round(qnorm(input$alpha, lower.tail = FALSE), 3)),
          ", and P-value =", round(test$p.value,5)
        ),
        br(),
        paste0("4. Conclusion : ", ifelse(test$p.value < input$alpha, 
                                          "Reject \\(H_0\\)", 
                                          "Fail to reject \\(H_0\\)")),
        br(),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", 
               ifelse(test$p.value < input$alpha, 
                      "we reject the null hypothesis that the true mean is ", 
                      "we fail to reject the null hypothesis that the true mean is "),
               input$h01, " \\((P\\)-value ", 
               ifelse(test$p.value < 0.001,"< 0.001", paste0(" = ", round(test$p.value, 4))), ")."),
        br()
        
      ) # end MathJax
    } 
  })# end: output$results_paired2mean
  
  
  #==================================================
  # output: results of two means with indep. samples
  #==================================================
  #
  output$results_twomeans <- renderUI({
    dat1 <- extract(input$sample1_twomeans)
    dat2 <- extract(input$sample2_twomeans)
      
    if (anyNA(dat1) | length(dat1) < 2 | anyNA(dat2) | length(dat2) < 2) {
      "Invalid input or not enough observations"
    } else if (input$meaninference == "two means" & input$popsd_twomeans == FALSE & input$var.equal == TRUE) {
      
      test_confint <- t.test(x = dat1, y = dat2, 
                             mu = input$h02, 
                             alternative = "two.sided", 
                             conf.level = 1 - input$alpha, 
                             paired = FALSE, var.equal = TRUE
      )
      test <- t.test(x = dat1, y = dat2, 
                     mu = input$h02, 
                     alternative = input$alternative2m, 
                     conf.level = 1 - input$alpha,
                     paired = FALSE, var.equal = TRUE)
      
      sp_top = (length(dat1) - 1) * var(dat1) + (length(dat2) - 1) * var(dat2)
      sp_bottom = length(dat1)+length(dat2)-2
      s_p <- sqrt(sp_top/sp_bottom)
      
      withMathJax(
        tags$b("Display sample 1 data:"),
        br(),
        paste(c(paste(dat1, collapse = ", ")), collapse = " "),
        br(),
        br(),
        tags$b("Display sample 2 data:"),
        br(),
        paste(c(paste(dat2, collapse = ", ")), collapse = " "),
        br(),
        br(),
        tags$b("Summary statistics of data"),
        br(),
        paste0("-----------------------------------------------------------"),
        br(),
        paste0("\\(\\ \\ \\ \\ \\ \\ Sample \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ n
               \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\bar{x} 
               \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ s \\) "),
        br(),
        paste0("-----------------------------------------------------------"),
        br(),
        paste0("\\(\\ \\ \\ \\ \\ \\ Sample 1 \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)", length(dat1), 
               "\\(\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)", round(mean(dat1), 3),
               "\\(\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)",
               round(sd(dat1),3) ),
        br(),
        paste0("\\(\\ \\ \\ \\ \\ \\ Sample 2 \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)", length(dat2), 
               "\\(\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)", round(mean(dat2), 3),
               "\\(\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)",
               round(sd(dat2),3) ),
        br(),
        paste0("-----------------------------------------------------------"),
        br(),
        hr(),
        ################ CI ####################
        tags$b("Confidence interval"),
        br(),
        paste0((1 - input$alpha) * 100, 
               "% CI for \\(\\mu_1 - \\mu_2 = \\ (\\bar{x}_1 - \\bar{x}_2) \\pm t_{\\alpha/2, n_1 + n_2 - 2} 
               (s_p) \\sqrt{1/n_1 + 1/n_2}\\)"),
        br(),
        paste0("where ", 
               "\\( s_p = \\sqrt{\\dfrac{(n_1 - 1)s^2_1 + (n_2 - 1)s^2_2}{n_1 + n_2 - 2}} = \\) ", 
               round(s_p, 3)),
        br(),
        paste0(
          "From data : ", (1 - input$alpha) * 100, "% CI for \\(\\mu_1 - \\mu_2 = \\)","(", 
          round(test_confint$estimate[1], 3), 
          ifelse(test_confint$estimate[2] >= 0, 
                 paste0(" - ", round(test_confint$estimate[2], 3),")"), 
                 paste0(" + ", round(abs(test_confint$estimate[2]), 3),")")), 
          " \\( \\pm \\) ", "\\( (\\)", 
          round(qt(input$alpha / 2, df = test_confint$parameter, lower.tail = FALSE), 3),
          " * ", round(s_p, 3), " * ", round(sqrt(1 / length(dat1) + 1 / length(dat2)), 3), 
          "\\( ) \\) ", "\\( = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        hr(),
        ################ Testing ####################
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : \\mu_1 - \\mu_2 = \\) ", test$null.value, 
               " and \\(H_1 : \\mu_1 - \\mu_2 \\) ", 
               ifelse(input$alternative2m == "two.sided", "\\( \\neq \\) ",
                      ifelse(input$alternative2m == "greater", "\\( > \\) ", "\\( < \\) ")), 
               test$null.value),
        br(),
        paste0(
          "2. Observed test statistic :\n 
          \\(t_{obs} = \\dfrac{(\\bar{x}_1 - \\bar{x}_2) - (\\mu_1 - \\mu_2)}{s_p \\sqrt{\\dfrac{1}{n_1} + \\dfrac{1}{n_2}}}  \\) "),
        br(),
        paste0("\\(\\ \\ \\ \\ \\)From data, \\(t_{obs}\\)=",
          "(", round(test$estimate[1], 3), 
          ifelse(test$estimate[2] >= 0, 
                 paste0(" - ", round(test$estimate[2], 3)), 
                 paste0(" + ", round(abs(test$estimate[2]), 3))),
          ifelse(test$null.value >= 0, paste0(" - ", test$null.value), paste0(" + ", abs(test$null.value))), 
          ") / (", round(s_p, 3), " * ", round(sqrt((1 / length(dat1)) + (1 / length(dat2))), 3), ") \\( = \\) ",
          round(test$statistic, 3)
        ),
        br(),
        paste0(
          "3. Critical value and P-value :", 
          ifelse(input$alternative2m == "two.sided", 
                 " \\( \\pm t_{\\alpha/2, n_1 + n_2 - 2} = \\pm t(\\)", 
                 ifelse(input$alternative2m == "greater", 
                        " \\( t_{\\alpha, n_1 + n_2 - 2} = t(\\)", 
                        " \\( -t_{\\alpha, n_1 + n_2 - 2} = -t(\\)")),
          ifelse(input$alternative2m == "two.sided", input$alpha / 2, input$alpha), 
          ", ", test$parameter, "\\()\\)", " \\( = \\) ",
          ifelse(input$alternative2m == "two.sided", 
                 "\\( \\pm \\)", ifelse(input$alternative2m == "greater", "", " -")),
          ifelse(input$alternative2m == "two.sided", 
                 round(qt(input$alpha / 2, df = test$parameter, lower.tail = FALSE), 3), 
                 round(qt(input$alpha, df = test$parameter, lower.tail = FALSE), 3)),
          " and p-value = ", round(test$p.value,5)
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
                      "we reject the null hypothesis that the true difference in means is ", 
                      "we fail to reject the null hypothesis that the true difference in means is "), 
               test$null.value, " \\((p\\)-value ", 
               ifelse(test$p.value < 0.001, "< 0.001", 
                      paste0("\\(=\\) ", round(test$p.value, 5))), ")", ".")
      )
    } else if (input$meaninference == "two means" & input$popsd_twomeans == FALSE & input$var.equal == FALSE) {
      test_confint <- t.test(x = dat1, y = dat2, 
                             mu = input$h02, 
                             alternative = "two.sided", 
                             conf.level = 1 - input$alpha, 
                             paired = FALSE, var.equal = FALSE
      )
      test <- t.test(x = dat1, y = dat2, 
                     mu = input$h02, 
                     alternative = input$alternative2m, 
                     conf.level = 1 - input$alpha,
                     paired = FALSE, var.equal = FALSE)
      
      #df_top = (var(dat1)/length(dat1)+var(dat2)/lenggh(dat2))^2
      #df_bottom = (var(dat1)/length(dat1))^2/(length(dat1)-1)+(var(dat2)/length(dat2))^2/(length(dat2)-1)
      #nu = df_top/df_buttom
      
      withMathJax(
        tags$b("Display sample 1 data:"),
        br(),
        paste(c(paste(dat1, collapse = ", ")), collapse = " "),
        br(),
        br(),
        tags$b("Display sample 2 data:"),
        br(),
        paste(c(paste(dat2, collapse = ", ")), collapse = " "),
        br(),
        br(),
        tags$b("Summary statistics of data"),
        br(),
        paste0("-----------------------------------------------------------"),
        br(),
        paste0("\\(\\ \\ \\ \\ \\ \\ Sample \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ n
               \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\bar{x} 
               \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ s \\) "),
        br(),
        paste0("-----------------------------------------------------------"),
        br(),
        paste0("\\(\\ \\ \\ \\ \\ \\ Sample 1 \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)", length(dat1), 
               "\\(\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)", round(mean(dat1), 3),
               "\\(\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)",
               round(sd(dat1),3) ),
        br(),
        paste0("\\(\\ \\ \\ \\ \\ \\ Sample 2 \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)", length(dat2), 
               "\\(\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)", round(mean(dat2), 3),
               "\\(\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)",
               round(sd(dat2),3) ),
        br(),
        paste0("-----------------------------------------------------------"),
        br(),
        hr(),
        ################ CI ####################
        tags$b("Confidence interval"),
        br(),
        paste0((1 - input$alpha) * 100, 
               "% CI for \\(\\mu_1 - \\mu_2 = \\ (\\bar{x}_1 - \\bar{x}_2) \\pm t_{\\alpha/2,\\nu} 
               \\sqrt{s_1^2/n_1 + s_2^2/n_2}\\)"),
        br(),
        paste0("where ", 
               "\\( \\nu = \\dfrac{\\Bigg(\\dfrac{s^2_1}{n_1} + \\dfrac{s^2_2}{n_2}\\Bigg)^2}{\\dfrac{\\Bigg(\\dfrac{s^2_1}{n_1}\\Bigg)^2}{n_1-1} + \\dfrac{\\Bigg(\\dfrac{s^2_2}{n_2}\\Bigg)^2}{n_2-1}} = \\) ", 
               round(test$parameter, 3)),
        br(),
        paste0(
          "From data : ", (1 - input$alpha) * 100, "% CI for \\(\\mu_1 - \\mu_2 = \\)","(", 
          round(test_confint$estimate[1], 3), 
          ifelse(test_confint$estimate[2] >= 0, 
                 paste0(" - ", round(test_confint$estimate[2], 3),")"), 
                 paste0(" + ", round(abs(test_confint$estimate[2]), 3),")")), 
          " \\( \\pm \\) ", "\\( (\\)", 
          round(qt(input$alpha / 2, df = round(test_confint$parameter,3), lower.tail = FALSE), 3),
          " * ", round(sqrt(round(var(dat1),5)/ length(dat1) + round(var(dat2),5)/ length(dat2)), 3), 
          "\\( ) \\) ", "\\( = \\) ",
          "[", round(test_confint$conf.int[1], 3), "; ", round(test_confint$conf.int[2], 3), "]"
        ),
        br(),
        hr(),
        
        ################ Testing ####################
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : \\mu_1 - \\mu_2 = \\) ", test$null.value, 
               " and \\(H_1 : \\mu_1 - \\mu_2 \\) ", 
               ifelse(input$alternative2m == "two.sided", "\\( \\neq \\) ",
                      ifelse(input$alternative2m == "greater", "\\( > \\) ", "\\( < \\) ")), 
               test$null.value),
        br(),
        paste0(
          "2. Observed test statistic :\n 
          \\(t_{obs} = \\dfrac{(\\bar{x}_1 - \\bar{x}_2) - (\\mu_1 - \\mu_2)}{ \\sqrt{\\dfrac{s_1^2}{n_1} + \\dfrac{s_2^2}{n_2}}}  \\) "),
        br(),
        paste0("\\(\\ \\ \\ \\ \\)From data, \\(t_{obs}\\)=",
               "(", round(test$estimate[1], 3), 
               ifelse(test$estimate[2] >= 0, 
                      paste0(" - ", round(test$estimate[2], 3)), 
                      paste0(" + ", round(abs(test$estimate[2]), 3))),
               ifelse(test$null.value >= 0, 
                      paste0(" - ", test$null.value), paste0(" + ", abs(test$null.value))), 
               ") / (", 
               round(sqrt(( var(dat1)/ length(dat1)) + (var(dat2) / length(dat2))), 3), ") \\( = \\) ",
               round(test$statistic, 3)
        ),
        br(),
        paste0(
          "3. Critical value and P-value :", 
          ifelse(input$alternative2m == "two.sided", 
                 " \\( \\pm t_{\\alpha/2, \\nu} = \\pm t(\\)", 
                 ifelse(input$alternative2m == "greater", 
                        " \\( t_{\\alpha, \\nu} = t(\\)", 
                        " \\( -t_{\\alpha, \\nu} = -t(\\)")),
          ifelse(input$alternative2m == "two.sided", input$alpha / 2, input$alpha), 
          ", ", round(test$parameter,3), "\\()\\)", " \\( = \\) ",
          ifelse(input$alternative2m == "two.sided", 
                 "\\( \\pm \\)", ifelse(input$alternative2m == "greater", "", " -")),
          ifelse(input$alternative2m == "two.sided", 
                 round(qt(input$alpha / 2, df = test$parameter, lower.tail = FALSE), 3), 
                 round(qt(input$alpha, df = test$parameter, lower.tail = FALSE), 3)),
          " and p-value = ", round(test$p.value,5)
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
                      "we reject the null hypothesis that the true difference in means is ", 
                      "we fail to reject the null hypothesis that the true difference in means is "), 
               test$null.value, " \\((p\\)-value ", 
               ifelse(test$p.value < 0.001, "< 0.001", 
                      paste0("\\(=\\) ", round(test$p.value, 5))), ")", ".")
        
        )
    } else if (input$meaninference == "two means" & input$popsd_twomeans == TRUE) {
      
      # fit data with model
      test <- t.test3(x = dat1, y = dat2, V1 = (input$sigma1_twomeans)^2, 
                      V2 = (input$sigma2_twomeans)^2, m0 = input$h02, 
                      alpha = input$alpha, alternative = input$alternative2m)
      
      withMathJax(
        tags$b("Display sample 1 data:"),
        br(),
        paste(c(paste(dat1, collapse = ", ")), collapse = " "),
        br(),
        br(),
        tags$b("Display sample 2 data:"),
        br(),
        paste(c(paste(dat2, collapse = ", ")), collapse = " "),
        br(),
        br(),
        tags$b("Summary statistics of data"),
        br(),
        paste0("-----------------------------------------------------------"),
        br(),
        paste0("\\(\\ \\ \\ \\ \\ \\ Sample \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ n
               \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\bar{x} 
               \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ s \\) "),
        br(),
        paste0("-----------------------------------------------------------"),
        br(),
        paste0("\\(\\ \\ \\ \\ \\ \\ Sample 1 \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)", length(dat1), 
               "\\(\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)", round(mean(dat1), 3),
               "\\(\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)",
               round(sd(dat1),3) ),
        br(),
        paste0("\\(\\ \\ \\ \\ \\ \\ Sample 2 \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)", length(dat2), 
               "\\(\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)", round(mean(dat2), 3),
               "\\(\\ \\ \\ \\ \\ \\ \\ \\ \\ \\ \\)",
               round(sd(dat2),3) ),
        br(),
        paste0("-----------------------------------------------------------"),
        br(),
        hr(),
        ################ CI ####################
        tags$b("Confidence interval"),
        br(),
        paste0((1 - input$alpha) * 100, 
               "% CI for 
               \\(\\mu_1 - \\mu_2 = 
               \\ (\\bar{x}_1 - \\bar{x}_2) 
               \\pm z_{\\alpha/2} \\sqrt{\\dfrac{\\sigma^2_1}{n_1} + \\dfrac{\\sigma^2_2}{n_2}} = \\) "),
        br(),
        paste0(
          (1 - input$alpha) * 100,"% CI from data = (",
          round(test$mean1, 3), ifelse(test$mean2 >= 0, 
                                       paste0(" - ", round(test$mean2, 3)), 
                                       paste0(" + ", round(abs(test$mean2), 3))), ")",
          "  \\( \\pm \\)", " \\( ( \\)", round(qnorm(input$alpha / 2, lower.tail = FALSE), 3),
          " * ", round(test$S, 3), "\\( ) \\) ", "\\( = \\) ",
          "[", round(test$LCL, 3), "; ", round(test$UCL, 3), "]"
        ),
        br(),
        hr(),
        
        ################ Testing ####################
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : \\mu_1 - \\mu_2 = \\) ", input$h02, 
               " and \\(H_1 : \\mu_1 - \\mu_2 \\) ", 
               ifelse(input$alternative2m == "two.sided", "\\( \\neq \\) ",
                      ifelse(input$alternative2m == "greater", "\\( > \\) ", "\\( < \\) ")), 
               input$h02),
        br(),
        paste0(
          "2. Observed test statistic :\n 
          \\(z_{obs} = \\dfrac{(\\bar{x}_1 - \\bar{x}_2) - (\\mu_1 - \\mu_2)}{ \\sqrt{\\dfrac{\\sigma_1^2}{n_1} + \\dfrac{\\sigma_2^2}{n_2}}}  \\) "),
        br(),
        paste0("\\(\\ \\ \\ \\ \\)From data, \\(z_{obs}\\)=",
               "(", round(test$mean1, 3), 
               ifelse(test$mean2 >= 0, 
                      paste0(" - ", round(test$mean2, 3)), 
                      paste0(" + ", round(abs(test$mean2), 3))), 
               ifelse(input$h0 >= 0, paste0(" - ", input$h02), 
                      paste0(" + ", abs(input$h02))), ") / ", round(test$S, 3), " \\( = \\) ",
               round(test$statistic, 3)
        ),
        br(),
        paste0(
          "3. Critical value and P-value :", 
          ifelse(input$alternative2m == "two.sided", 
                 " \\( \\pm z_{\\alpha/2} = \\pm z(\\)", 
                 ifelse(input$alternative2m == "greater", 
                        " \\( z_{\\alpha} = z(\\)", 
                        " \\( -z_{\\alpha} = -z(\\)")),
          ifelse(input$alternative2m == "two.sided", input$alpha / 2, input$alpha), 
           "\\()\\)", " \\( = \\) ",
          ifelse(input$alternative2m == "two.sided", 
                 "\\( \\pm \\)", ifelse(input$alternative2m == "greater", "", " -")),
          ifelse(input$alternative2m == "two.sided", 
                 round(qnorm(input$alpha / 2, lower.tail = FALSE), 3), 
                 round(qnorm(input$alpha, lower.tail = FALSE), 3)),
          " and p-value = ", round(test$p.value,5)
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
                      "we reject the null hypothesis that the true difference in means is ", 
                      "we fail to reject the null hypothesis that the true difference in means is "), 
               test$null.value, " \\((p\\)-value ", 
               ifelse(test$p.value < 0.001, "< 0.001", 
                      paste0("\\(=\\) ", round(test$p.value, 5))), ")", ".")
      )
    }
    
  }) ## end: output$results_twomean
  
  
  ###########################################################
  ## Plot Rejection Region for given alpha
  ###########################################################
  #
  output$plotpval <- renderPlot({
    
    #=================================================
    # Plot P-value area: one mean
    # case I: don't known population SD
    #=================================================
    if(input$meaninference =="one mean" & input$popsd_onemean==FALSE){
      
      # t.test based on data
      dat <- extract(input$sample_onemean)
      test <- t.test(x = dat, mu = input$h01, 
                     alternative = input$alternative, conf.level = 1 - input$alpha)
      # plot range of x and y
      x = seq(-4.5, 4.5, by=.01)
      y = dt(x, df=test$parameter)
      # 
      par(mar=c(2, 0.5, 4, .5), 
          cex.main=1.5, 
          cex.lab=1.7, 
          cex.axis=1.5)
      plot(x, y, type='l', lwd=1, xlab='test statistic t', ylab="",
           xaxt='n', yaxt='n',
           main=paste0("Plot area for the p-value under null distribution: t(", test$parameter,")"))
      legend("topleft",paste0("p-value = ", round(test$p.value,5),
                              "\nalpha=",input$alpha),bty="n",cex=1.2,text.col="red")
      
      # find critical region
      if (input$alternative == "two.sided") {
        t.obs <- abs(round(test$statistic,3))
        axis(side=1, at=c(-t.obs, t.obs))
        abline(v=c(-t.obs,t.obs),col="red",lty=2, lwd=2)
        text(test$statistic, dt(0.4,df=test$parameter),"t-observed",
             pos = 2, srt = 90)
        if(t.obs<4.5){
          t.neg <- seq(-4.5, -t.obs, by=.01)
          t.pos <- seq(t.obs, 4.5, by=.01)}
        else{
          t.neg <-seq(-4.5, -4.48, by=.01)
          t.pos <- seq(4.48, 4.5, by=.01)}
        polygon(c(t.neg, -t.obs, -4.5), c(dt(t.neg, df=test$parameter),0,0), col="lightblue")
        polygon(c(t.pos, 4.5, t.obs), c(dt(t.pos, df=test$parameter), 0,0), col="lightblue")
      }
      else if (input$alternative == "greater") {
        t.obs <- round(test$statistic,3)
        if(t.obs<4.5) t.seq <- seq(t.obs, 4.5, by=.01)
        else t.seq = seq(4.48, 4.5, by=.01)
        polygon( c(t.seq, 4.5, t.obs), c(dt(t.seq, df=test$parameter), 0, 0), col="lightblue")
        axis(side=1, at=t.obs)
        abline(v=round(test$statistic, 3),col="red",lty=2, lwd=2)
        text(test$statistic, dt(0.4,df=test$parameter),"t-observed",
             pos = 2, srt = 90)
      } else if (input$alternative == "less") {
        t.obs = test$statistic
        if(t.obs>-4.5) t.seq <-seq(-4.5, t.obs, by=.01)
        else t.seq <- seq(-4.5, -4.48, by=.01)
        polygon(c(t.seq, t.obs, -4.5), c(dt(t.seq, df=test$parameter), 0, 0), col="lightblue")
        axis(side=1, at=c(-round(abs(t.obs),3) ))
        abline(v=round(test$statistic, 3),col="red",lty=2, lwd=2)
        text(test$statistic, dt(0.4,df=test$parameter),"t-observed",
             pos = 2, srt = 90)
      }
    }
    
    #=================================================
    # Plot P-value area: one mean
    # case II: Given population SD
    #=================================================
    if(input$meaninference =="one mean" & input$popsd_onemean==TRUE){
      
      # t.test based on data
      mydata <- extract(input$sample_onemean)
      test <- t.test2(x = mydata, V = (input$sigma_onemean)^2, m0 = input$h01, 
                      alpha = input$alpha, alternative = input$alternative)
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
    # Plot P-value area: two means
    # case 1: unknown and equal SD1 and SD2
    #=================================================
    #
    if(input$meaninference == "two means" & input$popsd_twomeans == FALSE & input$var.equal == TRUE){
      
      # load in data
      dat1 <- extract(input$sample1_twomeanspaired)
      dat2 <- extract(input$sample2_twomeanspaired)
      # fit data with t test
      test <- t.test(x = dat1, y = dat2, 
                     mu = input$h02, 
                     alternative = input$alternative2m, 
                     conf.level = 1 - input$alpha,
                     paired = FALSE, var.equal = TRUE)
      options(scipen=999)
      # plot range of x and y
      x = seq(-4.5, 4.5, by=.01)
      y = dt(x, df=test$parameter)
      # 
      par(mar=c(2, 0.5, 4, .5), 
          cex.main=1.5, 
          cex.lab=1.7, 
          cex.axis=1.5)
      plot(x, y, type='l', lwd=1, xlab='test statistic t', ylab="",
           xaxt='n', yaxt='n',
           main=paste0("Plot area for the p-value under null distribution: t(", test$parameter,")"))
      legend("topleft",paste0("p-value = ", round(test$p.value,5),
                              "\nalpha=",input$alpha),bty="n",cex=1.2,text.col="red")
      
      # find critical region
      if (input$alternative2m == "two.sided") {
        t.obs <- abs(round(test$statistic,3))
        axis(side=1, at=c(-t.obs, t.obs))
        abline(v=c(-t.obs,t.obs),col="red",lty=2, lwd=2)
        text(test$statistic, dt(0.4,df=test$parameter),"t-observed",
             pos = 2, srt = 90)
        if(t.obs<4.5){
          t.neg <- seq(-4.5, -t.obs, by=.01)
          t.pos <- seq(t.obs, 4.5, by=.01)}
        else{
          t.neg <-seq(-4.5, -4.48, by=.01)
          t.pos <- seq(4.48, 4.5, by=.01)}
        polygon(c(t.neg, -t.obs, -4.5), c(dt(t.neg, df=test$parameter),0,0), col="lightblue")
        polygon(c(t.pos, 4.5, t.obs), c(dt(t.pos, df=test$parameter), 0,0), col="lightblue")
      }
      else if (input$alternative2m == "greater") {
        t.obs <- round(test$statistic,3)
        if(t.obs<4.5) t.seq <- seq(t.obs, 4.5, by=.01)
        else t.seq = seq(4.48, 4.5, by=.01)
        polygon( c(t.seq, 4.5, t.obs), c(dt(t.seq, df=test$parameter), 0, 0), col="lightblue")
        axis(side=1, at=t.obs)
        abline(v=round(test$statistic, 3),col="red",lty=2, lwd=2)
        text(test$statistic, dt(0.4,df=test$parameter),"t-observed",
             pos = 2, srt = 90)
      } else if (input$alternative2m == "less") {
        t.obs = test$statistic
        if(t.obs>-4.5) t.seq <-seq(-4.5, t.obs, by=.01)
        else t.seq <- seq(-4.5, -4.48, by=.01)
        polygon(c(t.seq, t.obs, -4.5), c(dt(t.seq, df=test$parameter), 0, 0), col="lightblue")
        axis(side=1, at=t.obs)
        abline(v=round(test$statistic, 3),col="red",lty=2, lwd=2)
        text(test$statistic, dt(0.4,df=test$parameter),"t-observed",
             pos = 2, srt = 90)
      }
    }
    
    
    #=================================================
    # Plot P-value area: two means
    # case 2: unknown and unequal SD1 and SD2
    #=================================================
    #
    if(input$meaninference == "two means" & input$popsd_twomeans == FALSE & input$var.equal == FALSE){
      
      # t.test based on data
      dat1 <- extract(input$sample1_twomeanspaired)
      dat2 <- extract(input$sample2_twomeanspaired)
      test <- t.test(x = dat1, y = dat2, 
                     mu = input$h02, 
                     alternative = input$alternative2m, 
                     conf.level = 1 - input$alpha,
                     paired = FALSE, var.equal = FALSE)
      options(scipen=999)
      # plot range of x and y
      x = seq(-4.5, 4.5, by=.01)
      y = dt(x, df=test$parameter)
      # 
      par(mar=c(2, 0.5, 4, .5), 
          cex.main=1.5, 
          cex.lab=1.7, 
          cex.axis=1.5)
      plot(x, y, type='l', lwd=1, xlab='test statistic t', ylab="",
           xaxt='n', yaxt='n',
           main=paste0("Plot area for the p-value under null distribution: t(", 
                       round(test$parameter,3),")"))
      legend("topleft",paste0("p-value = ", round(test$p.value,5),
                              "\nalpha=",input$alpha),bty="n",cex=1.2,text.col="red")
      
      # find critical region
      if (input$alternative2m == "two.sided") {
        t.obs <- abs(round(test$statistic,3))
        axis(side=1, at=c(-t.obs, t.obs))
        abline(v=c(-t.obs,t.obs),col="red",lty=2, lwd=2)
        text(test$statistic, dt(0.4,df=test$parameter),"t-observed",
             pos = 2, srt = 90)
        if(t.obs<4.5){
          t.neg <- seq(-4.5, -t.obs, by=.01)
          t.pos <- seq(t.obs, 4.5, by=.01)}
        else{
          t.neg <-seq(-4.5, -4.48, by=.01)
          t.pos <- seq(4.48, 4.5, by=.01)}
        polygon(c(t.neg, -t.obs, -4.5), c(dt(t.neg, df=test$parameter),0,0), col="lightblue")
        polygon(c(t.pos, 4.5, t.obs), c(dt(t.pos, df=test$parameter), 0,0), col="lightblue")
      }
      else if (input$alternative2m == "greater") {
        t.obs <- round(test$statistic,3)
        if(t.obs<4.5) t.seq <- seq(t.obs, 4.5, by=.01)
        else t.seq = seq(4.48, 4.5, by=.01)
        polygon( c(t.seq, 4.5, t.obs), c(dt(t.seq, df=test$parameter), 0, 0), col="lightblue")
        axis(side=1, at=t.obs)
        abline(v=round(test$statistic, 3),col="red",lty=2, lwd=2)
        text(test$statistic, dt(0.4,df=test$parameter),"t-observed",
             pos = 2, srt = 90)
      } else if (input$alternative2m == "less") {
        t.obs = test$statistic
        if(t.obs>-4.5) t.seq <-seq(-4.5, t.obs, by=.01)
        else t.seq <- seq(-4.5, -4.48, by=.01)
        polygon(c(t.seq, t.obs, -4.5), c(dt(t.seq, df=test$parameter), 0, 0), col="lightblue")
        axis(side=1, at=t.obs)
        abline(v=round(test$statistic, 3),col="red",lty=2, lwd=2)
        text(test$statistic, dt(0.4,df=test$parameter),"t-observed",
             pos = 2, srt = 90)
      }
    }
    
    
    #=================================================
    # Plot P-value area: two means
    # case 3: given both SD1 and SD2
    #=================================================
    if(input$meaninference == "two means" & input$popsd_twomeans == TRUE){
      
      # t.test based on data
      dat1 <- extract(input$sample1_twomeanspaired)
      dat2 <- extract(input$sample2_twomeanspaired)
      test <- t.test3(x = dat1, y = dat2, V1 = (input$sigma1_twomeans)^2, 
                      V2 = (input$sigma2_twomeans)^2, m0 = input$h02, 
                      alpha = input$alpha, alternative = input$alternative2m)
      options(scipen=999)
      # plot range of x and y
      x = seq(-4.5, 4.5, by=.01)
      y = dnorm(x)
      # 
      par(mar=c(2, 0.5, 4, .5), 
          cex.main=1.5, 
          cex.lab=1.7, 
          cex.axis=1.5)
      plot(x, y, type='l', lwd=1, xlab='test statistic z', ylab="",
           xaxt='n', yaxt='n',
           main=paste0("Plot area for the p-value under null distribution: N (0,1)"))
      legend("topleft",paste0("p-value = ", round(test$p.value,5),
                              "\nalpha=",input$alpha),bty="n",cex=1.2,text.col="red")
      
      # find critical region
      if (input$alternative2m == "two.sided") {
        t.obs <- abs(round(test$statistic,3))
        axis(side=1, at=c(-t.obs, t.obs))
        abline(v=c(-t.obs,t.obs),col="red",lty=2, lwd=2)
        text(test$statistic, dnorm(0.4),"z-observed",
             pos = 2, srt = 90)
        if(t.obs<4.5){
          t.neg <- seq(-4.5, -t.obs, by=.01)
          t.pos <- seq(t.obs, 4.5, by=.01)}
        else{
          t.neg <-seq(-4.5, -4.48, by=.01)
          t.pos <- seq(4.48, 4.5, by=.01)}
        polygon(c(t.neg, -t.obs, -4.5), c(dnorm(t.neg),0,0), col="lightblue")
        polygon(c(t.pos, 4.5, t.obs), c(dnorm(t.pos), 0,0), col="lightblue")
      }
      else if (input$alternative2m == "greater") {
        t.obs <- round(test$statistic,3)
        if(t.obs<4.5) t.seq <- seq(t.obs, 4.5, by=.01)
        else t.seq = seq(4.48, 4.5, by=.01)
        polygon( c(t.seq, 4.5, t.obs), c(dnorm(t.seq), 0, 0), col="lightblue")
        axis(side=1, at=t.obs)
        abline(v=round(test$statistic, 3),col="red",lty=2, lwd=2)
        text(test$statistic, dnorm(0.4),"z-observed",
             pos = 2, srt = 90)
      } else if (input$alternative2m == "less") {
        t.obs = test$statistic
        if(t.obs>-4.5) t.seq <-seq(-4.5, t.obs, by=.01)
        else t.seq <- seq(-4.5, -4.48, by=.01)
        polygon(c(t.seq, t.obs, -4.5), c(dnorm(t.seq), 0, 0), col="lightblue")
        axis(side=1, at=t.obs)
        abline(v=round(test$statistic, 3),col="red",lty=2, lwd=2)
        text(test$statistic, dnorm(0.4),"z-observed",
             pos = 2, srt = 90)
      }
    }
    
    #=================================================
    # Plot P-value area: two means with paired samples
    # case : unknown sigma_D 
    #=================================================
    if(input$meaninference =="two means with paired samples" & input$popsd_twomeanspaired==FALSE){
      
      # t.test based on data
      dat1 <- extract(input$sample1_twomeanspaired)
      dat2 <- extract(input$sample2_twomeanspaired)
      test <- t.test(x = dat1, y = dat2, 
                     mu = input$h02, 
                     alternative = input$alternative2p, 
                     conf.level = 1 - input$alpha, paired = TRUE)
      options(scipen=999)
      # plot range of x and y
      x = seq(-4.5, 4.5, by=.01)
      y = dt(x, df=test$parameter)
      # 
      par(mar=c(2, 0.5, 4, .5), 
          cex.main=1.5, 
          cex.lab=1.7, 
          cex.axis=1.5)
      plot(x, y, type='l', lwd=1, xlab='test statistic t', ylab="",
           xaxt='n', yaxt='n',
           main=paste0("Plot area for the p-value under null distribution: t(", test$parameter,")"))
      legend("topleft",paste0("p-value = ", round(test$p.value,5),
                              "\nalpha=",input$alpha),bty="n",cex=1.2,text.col="red")
      
      # find critical region
      if (input$alternative2p == "two.sided") {
        t.obs <- abs(round(test$statistic,3))
        axis(side=1, at=c(-t.obs, t.obs))
        abline(v=c(-t.obs,t.obs),col="red",lty=2, lwd=2)
        text(test$statistic, dt(0.4,df=test$parameter),"t-observed",
             pos = 2, srt = 90)
        if(t.obs<4.5){
          t.neg <- seq(-4.5, -t.obs, by=.01)
          t.pos <- seq(t.obs, 4.5, by=.01)}
        else{
          t.neg <-seq(-4.5, -4.48, by=.01)
          t.pos <- seq(4.48, 4.5, by=.01)}
        polygon(c(t.neg, -t.obs, -4.5), c(dt(t.neg, df=test$parameter),0,0), col="lightblue")
        polygon(c(t.pos, 4.5, t.obs), c(dt(t.pos, df=test$parameter), 0,0), col="lightblue")
      }
      else if (input$alternative2p == "greater") {
        t.obs <- round(test$statistic,3)
        if(t.obs<4.5) t.seq <- seq(t.obs, 4.5, by=.01)
        else t.seq = seq(4.48, 4.5, by=.01)
        polygon( c(t.seq, 4.5, t.obs), c(dt(t.seq, df=test$parameter), 0, 0), col="lightblue")
        axis(side=1, at=t.obs)
        abline(v=round(test$statistic, 3),col="red",lty=2, lwd=2)
        text(test$statistic, dt(0.4,df=test$parameter),"t-observed",
             pos = 2, srt = 90)
      } else if (input$alternative2p == "less") {
        t.obs = test$statistic
        if(t.obs>-4.5) t.seq <-seq(-4.5, t.obs, by=.01)
        else t.seq <- seq(-4.5, -4.48, by=.01)
        polygon(c(t.seq, t.obs, -4.5), c(dt(t.seq, df=test$parameter), 0, 0), col="lightblue")
        axis(side=1, at=t.obs)
        abline(v=round(test$statistic, 3),col="red",lty=2, lwd=2)
        text(test$statistic, dt(0.4,df=test$parameter),"t-observed",
             pos = 2, srt = 90)
      }
    }
    
    
    #=================================================
    # Plot P-value area: two means with paired samples
    # case : given sigma_D 
    #=================================================
    if(input$meaninference =="two means with paired samples" & input$popsd_twomeanspaired==TRUE){
      
      # t.test based on data
      options(scipen=999)
      dat1 <- extract(input$sample1_twomeanspaired)
      dat2 <- extract(input$sample2_twomeanspaired)
      test <- t.test2(x = dat1 - dat2, 
                      V = (input$sigma_twomeanspaired)^2, 
                      m0 = input$h02, 
                      alpha = input$alpha, alternative = input$alternative2p)
      
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
  }) # end: output$plotpval
  
  ###########################################################
  ## Plot Rejection Region for given alpha
  ###########################################################
  #
  output$plotRR <- renderPlot({
    
    #=================================================
    # Plot RR area: one mean
    # case I: don't known population SD
    #=================================================
    if(input$meaninference =="one mean" & input$popsd_onemean==FALSE){
      
      # t.test based on data
      dat <- extract(input$sample_onemean)
      test <- t.test(x = dat, mu = input$h01, 
                     alternative = input$alternative, conf.level = 1 - input$alpha)
      # plot range of x and y
      x = seq(-4.5, 4.5, by=.01)
      y = dt(x, df=test$parameter)
      # 
      par(mar=c(2, 0.5, 4, .5), 
          cex.main=1.5, 
          cex.lab=1.7, 
          cex.axis=1.5)
      plot(x, y, type='l', lwd=1, xlab='test statistic t', ylab="",
           xaxt='n', yaxt='n',
           main=paste0("Plot area of Rejection Region under null distribution: t(", test$parameter,")"))
      # find critical region
      if (input$alternative == "two.sided") {
        t.cri <- qt(input$alpha/2, df = test$parameter, lower.tail = FALSE) 
        axis(side=1, at=c(-round(abs(t.cri),3), round(abs(t.cri),3)))
        if(t.cri<4.5){
          t.neg <- seq(-4.5, -t.cri, by=.01)
          t.pos <- seq(t.cri, 4.5, by=.01)}
        else{
          t.neg <-seq(-4.5, -4.48, by=.01)
          t.pos <- seq(4.48, 4.5, by=.01)}
        polygon(c(t.neg, -t.cri, -4.5), c(dt(t.neg, df=test$parameter),0,0), col="orange")
        polygon(c(t.pos, 4.5, t.cri), c(dt(t.pos, df=test$parameter), 0,0), col="orange")
        }
      else if (input$alternative == "greater") {
        t.cri <- qt(input$alpha, df = test$parameter, lower.tail = FALSE) 
        if(t.cri<4.5) t.seq <- seq(t.cri, 4.5, by=.01)
        else t.seq = seq(4.48, 4.5, by=.01)
        polygon( c(t.seq, 4.5, t.cri), c(dt(t.seq, df=test$parameter), 0, 0), col="orange")
        axis(side=1, at=c(round(abs(t.cri),3)))
        } else if (input$alternative == "less") {
        t.cri = qt(input$alpha, df = test$parameter, lower.tail = TRUE)
        if(t.cri>-4.5) t.seq <-seq(-4.5, t.cri, by=.01)
        else t.seq <- seq(-4.5, -4.48, by=.01)
        polygon(c(t.seq, t.cri, -4.5), c(dt(t.seq, df=test$parameter), 0, 0), col="orange")
        axis(side=1, at=c(-round(abs(t.cri),3) ))
        }
       abline(v=round(test$statistic, 3),col="red",lty=2, lwd=2)
       text(test$statistic, dt(0.4,df=test$parameter),"t-observed",
            pos = 2, srt = 90)
       legend("topleft",paste0("t-observed = ", round(test$stat,3), 
                              ifelse(input$alternative=="two.sided",
                                     paste0("\nRejection Reg. cutoff = ",round(abs(t.cri),3)),
                                     paste0("\nRejection Reg. cutoff = ",round(t.cri,3)))),
             bty="n",cex=1.2,text.col="red")
      
    }
    
    #=================================================
    # Plot RR area: one mean
    # case I: given population SD
    #=================================================
    if(input$meaninference =="one mean" & input$popsd_onemean==TRUE){
      
      # t.test based on data
      mydata <- extract(input$sample_onemean)
      test <- t.test2(x = mydata, V = (input$sigma_onemean)^2, m0 = input$h01, 
                      alpha = input$alpha, alternative = input$alternative)
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
    # Plot RR area: two means with paired samples
    # case A: unknown difference SD
    #=================================================
    if(input$meaninference =="two means with paired samples" & input$popsd_twomeanspaired==FALSE){
      
      # t.test based on data
      dat1 <- extract(input$sample1_twomeanspaired)
      dat2 <- extract(input$sample2_twomeanspaired)
      test <- t.test(x = dat1, y = dat2, 
                     mu = input$h02, 
                     alternative = input$alternative2p, 
                     conf.level = 1 - input$alpha, paired = TRUE)
      options(scipen=999)
      # plot range of x and y
      x = seq(-4.5, 4.5, by=.01)
      y = dt(x, df=test$parameter)
      # 
      par(mar=c(2, 0.5, 4, .5), 
          cex.main=1.5, 
          cex.lab=1.7, 
          cex.axis=1.5)
      plot(x, y, type='l', lwd=1, xlab='test statistic t', ylab="",
           xaxt='n', yaxt='n',
           main=paste0("Plot area of Rejection Region under null distribution: t(", test$parameter,")"))
      # find critical region
      if (input$alternative2p == "two.sided") {
        t.cri <- qt(input$alpha/2, df = test$parameter, lower.tail = FALSE) 
        axis(side=1, at=c(-round(abs(t.cri),3), round(abs(t.cri),3)))
        #
        if(t.cri<4.5){
          t.neg <- seq(-4.5, -t.cri, by=.01)
          t.pos <- seq(t.cri, 4.5, by=.01)}
        else{
          t.neg <-seq(-4.5, -4.48, by=.01)
          t.pos <- seq(4.48, 4.5, by=.01)}
        polygon(c(t.neg, -t.cri, -4.5), c(dt(t.neg, df=test$parameter),0,0), col="orange")
        polygon(c(t.pos, 4.5, t.cri), c(dt(t.pos, df=test$parameter), 0,0), col="orange")
      }
      else if (input$alternative2p == "greater") {
        t.cri <- qt(input$alpha, df = test$parameter, lower.tail = FALSE) 
        if(t.cri<4.5) t.seq <- seq(t.cri, 4.5, by=.01)
        else t.seq = seq(4.48, 4.5, by=.01)
        polygon( c(t.seq, 4.5, t.cri), c(dt(t.seq, df=test$parameter), 0, 0), col="orange")
        axis(side=1, at=c(round(abs(t.cri),3)))
      } else if (input$alternative2p == "less") {
        t.cri = qt(input$alpha, df = test$parameter, lower.tail = TRUE)
        if(t.cri>-4.5) t.seq <-seq(-4.5, t.cri, by=.01)
        else t.seq <- seq(-4.5, -4.48, by=.01)
        polygon(c(t.seq, t.cri, -4.5), c(dt(t.seq, df=test$parameter), 0, 0), col="orange")
        axis(side=1, at=c(-round(abs(t.cri),3) ))
      }
      abline(v=round(test$statistic, 3),col="red",lty=2, lwd=2)
      text(test$statistic, dt(0.4,df=test$parameter),"t-observed",
           pos = 2, srt = 90)
      legend("topleft",paste0("t-observed = ", round(test$stat,3), 
                              ifelse(input$alternative2p=="two.sided",
                                     paste0("\nRejection Reg. cutoff = ",round(abs(t.cri),3)),
                                     paste0("\nRejection Reg. cutoff = ",round(t.cri,3)))),
             bty="n",cex=1.2,text.col="red")
      
    }
    
    
    #=================================================
    # Plot RR area: two means with paired samples
    # case B: given difference SD
    #=================================================
    if(input$meaninference =="two means with paired samples" & input$popsd_twomeanspaired==TRUE){
      
      # t.test based on data
      options(scipen=999)
      dat1 <- extract(input$sample1_twomeanspaired)
      dat2 <- extract(input$sample2_twomeanspaired)
      test <- t.test2(x = dat1 - dat2, 
                      V = (input$sigma_twomeanspaired)^2, 
                      m0 = input$h02, 
                      alpha = input$alpha, alternative = input$alternative2p)
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
    
    #=================================================
    # Plot RR area: two means
    # case 1: unknown and equal SD1 and SD2
    #=================================================
    #
    if(input$meaninference == "two means" & input$popsd_twomeans == FALSE & input$var.equal == TRUE){
      
      # t.test based on data
      dat1 <- extract(input$sample1_twomeanspaired)
      dat2 <- extract(input$sample2_twomeanspaired)
      test <- t.test(x = dat1, y = dat2, 
                     mu = input$h02, 
                     alternative = input$alternative2m, 
                     conf.level = 1 - input$alpha,
                     paired = FALSE, var.equal = TRUE)
      options(scipen=999)
      # plot range of x and y
      # plot range of x and y
      x = seq(-4.5, 4.5, by=.01)
      y = dt(x, df=test$parameter)
      # 
      par(mar=c(2, 0.5, 4, .5), 
          cex.main=1.5, 
          cex.lab=1.7, 
          cex.axis=1.5)
      plot(x, y, type='l', lwd=1, xlab='test statistic t', ylab="",
           xaxt='n', yaxt='n',
           main=paste0("Plot area of Rejection Region under null distribution: t(", test$parameter,")"))
      # find critical region
      if (input$alternative2m == "two.sided") {
        t.cri <- qt(input$alpha/2, df = test$parameter, lower.tail = FALSE) 
        axis(side=1, at=c(-round(abs(t.cri),3), round(abs(t.cri),3)))
        #
        if(t.cri<4.5){
          t.neg <- seq(-4.5, -t.cri, by=.01)
          t.pos <- seq(t.cri, 4.5, by=.01)}
        else{
          t.neg <-seq(-4.5, -4.48, by=.01)
          t.pos <- seq(4.48, 4.5, by=.01)}
        polygon(c(t.neg, -t.cri, -4.5), c(dt(t.neg, df=test$parameter),0,0), col="orange")
        polygon(c(t.pos, 4.5, t.cri), c(dt(t.pos, df=test$parameter), 0,0), col="orange")
      }
      else if (input$alternative2m == "greater") {
        t.cri <- qt(input$alpha, df = test$parameter, lower.tail = FALSE) 
        if(t.cri<4.5) t.seq <- seq(t.cri, 4.5, by=.01)
        else t.seq = seq(4.48, 4.5, by=.01)
        polygon( c(t.seq, 4.5, t.cri), c(dt(t.seq, df=test$parameter), 0, 0), col="orange")
        axis(side=1, at=c(round(abs(t.cri),3)))
      } else if (input$alternative2m == "less") {
        t.cri = qt(input$alpha, df = test$parameter, lower.tail = TRUE)
        if(t.cri>-4.5) t.seq <-seq(-4.5, t.cri, by=.01)
        else t.seq <- seq(-4.5, -4.48, by=.01)
        polygon(c(t.seq, t.cri, -4.5), c(dt(t.seq, df=test$parameter), 0, 0), col="orange")
        axis(side=1, at=c(-round(abs(t.cri),3) ))
      }
      abline(v=round(test$statistic, 3),col="red",lty=2, lwd=2)
      text(test$statistic, dt(0.4,df=test$parameter),"t-observed",
           pos = 2, srt = 90)
      legend("topleft",paste0("t-observed = ", round(test$stat,3), 
                              ifelse(input$alternative2m=="two.sided",
                                     paste0("\nRejection Reg. cutoff = ",round(abs(t.cri),3)),
                                     paste0("\nRejection Reg. cutoff = ",round(t.cri,3)))),
             bty="n",cex=1.2,text.col="red")
      
    }
    
    #=================================================
    # Plot RR area: two means
    # case 2: unknown and unequal SD1 and SD2
    #=================================================
    if(input$meaninference == "two means" & input$popsd_twomeans == FALSE & input$var.equal == FALSE){
      
      # t.test based on data
      dat1 <- extract(input$sample1_twomeanspaired)
      dat2 <- extract(input$sample2_twomeanspaired)
      test <- t.test(x = dat1, y = dat2, 
                     mu = input$h02, 
                     alternative = input$alternative2m, 
                     conf.level = 1 - input$alpha, 
                     paired = FALSE,var.equal=FALSE)
      options(scipen=999)
      # plot range of x and y
      # plot range of x and y
      x = seq(-4.5, 4.5, by=.01)
      y = dt(x, df=test$parameter)
      # 
      par(mar=c(2, 0.5, 4, .5), 
          cex.main=1.5, 
          cex.lab=1.7, 
          cex.axis=1.5)
      plot(x, y, type='l', lwd=1, xlab='test statistic t', ylab="",
           xaxt='n', yaxt='n',
           main=paste0("Plot area of Rejection Region under null distribution: t(", 
                       round(test$parameter,3),")"))
      # find critical region
      if (input$alternative2m == "two.sided") {
        t.cri <- qt(input$alpha/2, df = test$parameter, lower.tail = FALSE) 
        axis(side=1, at=c(-round(abs(t.cri),3), round(abs(t.cri),3)))
        #
        if(t.cri<4.5){
          t.neg <- seq(-4.5, -t.cri, by=.01)
          t.pos <- seq(t.cri, 4.5, by=.01)}
        else{
          t.neg <-seq(-4.5, -4.48, by=.01)
          t.pos <- seq(4.48, 4.5, by=.01)}
        polygon(c(t.neg, -t.cri, -4.5), c(dt(t.neg, df=test$parameter),0,0), col="orange")
        polygon(c(t.pos, 4.5, t.cri), c(dt(t.pos, df=test$parameter), 0,0), col="orange")
      }
      else if (input$alternative2m == "greater") {
        t.cri <- qt(input$alpha, df = test$parameter, lower.tail = FALSE) 
        if(t.cri<4.5) t.seq <- seq(t.cri, 4.5, by=.01)
        else t.seq = seq(4.48, 4.5, by=.01)
        polygon( c(t.seq, 4.5, t.cri), c(dt(t.seq, df=test$parameter), 0, 0), col="orange")
        axis(side=1, at=c(round(abs(t.cri),3)))
      } else if (input$alternative2m == "less") {
        t.cri = qt(input$alpha, df = test$parameter, lower.tail = TRUE)
        if(t.cri>-4.5) t.seq <-seq(-4.5, t.cri, by=.01)
        else t.seq <- seq(-4.5, -4.48, by=.01)
        polygon(c(t.seq, t.cri, -4.5), c(dt(t.seq, df=test$parameter), 0, 0), col="orange")
        axis(side=1, at=c(-round(abs(t.cri),3) ))
      }
      abline(v=round(test$statistic, 3),col="red",lty=2, lwd=2)
      text(test$statistic, dt(0.4,df=test$parameter),"t-observed",
           pos = 2, srt = 90)
      legend("topleft",paste0("t-observed = ", round(test$stat,3), 
                              ifelse(input$alternative2m=="two.sided",
                                     paste0("\nRejection Reg. cutoff = ",round(abs(t.cri),3)),
                                     paste0("\nRejection Reg. cutoff = ",round(t.cri,3)))),
             bty="n",cex=1.2,text.col="red")
      
    }
    
    #=================================================
    # Plot RR area: two means
    # case 3: Given both SD1 and SD2
    #=================================================
    if(input$meaninference == "two means" & input$popsd_twomeans == TRUE){
      # t.test based on data
      dat1 <- extract(input$sample1_twomeanspaired)
      dat2 <- extract(input$sample2_twomeanspaired)
      test <- t.test3(x = dat1, y = dat2, V1 = (input$sigma1_twomeans)^2, 
                      V2 = (input$sigma2_twomeans)^2, m0 = input$h02, 
                      alpha = input$alpha, alternative = input$alternative2m)
      options(scipen=999)
      # plot range of x and y
      x = seq(-4.5, 4.5, by=.01)
      y = dnorm(x)
      # 
      par(mar=c(2, 0.5, 4, .5), 
          cex.main=1.5, 
          cex.lab=1.7, 
          cex.axis=1.5)
      plot(x, y, type='l', lwd=1, xlab='test statistic z', ylab="",
           xaxt='n', yaxt='n',
           main=paste0("Plot area of Rejection Region under null distribution: N(0,1)"))
      # find critical region
      if (input$alternative2m == "two.sided") {
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
      else if (input$alternative2m == "greater") {
        z.cri <- qnorm(input$alpha, lower.tail = FALSE) 
        if(z.cri<4.5) t.seq <- seq(z.cri, 4.5, by=.01)
        else t.seq = seq(4.48, 4.5, by=.01)
        polygon( c(t.seq, 4.5, z.cri), c(dnorm(t.seq), 0, 0), col="orange")
        axis(side=1, at=c(round(abs(z.cri),3)))
      } else if (input$alternative2m == "less") {
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
                              ifelse(input$alternative2m=="two.sided",
                                     paste0("\nRejection Reg. cutoff = ",round(abs(z.cri),3)),
                                     paste0("\nRejection Reg. cutoff = ",round(z.cri,3)))),
             bty="n",cex=1.3, text.col="red")
    }
  
    # =================== end of all cases ==========
    
  }) # end: output$plotRR
  
} # end server code

# Run the application
shinyApp(ui = ui, server = server)