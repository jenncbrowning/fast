library(shiny)
library(shinyjs)
library(shinythemes)
library(rmarkdown)
library(knitr)
library(plotly)
library(pander)
library(RColorBrewer)

ui <- fluidPage(
 theme = shinythemes::shinytheme('cerulean'),
  titlePanel('Descriptive Analysis of a Numerical Variable'),
  sidebarLayout(
    sidebarPanel(
    textAreaInput('data', 
          value = "20,35,8,5,15,25,70,30,40,35,10,12,40,15,20",
          'Enter data here: e.g., 1,2,3,...'),
   radioButtons('hist_type', label='Histogram Type', 
                choiceNames= list('Frequency', 'Relative frequency'),
                choiceValues = list(1,2), 
                choices=NULL),
   radioButtons('box_type', label='Boxplot Type', 
                choiceNames= list('Vertical bars', 'Horizontal Bars'),
                choiceValues = list(1,2), 
                choices=NULL),
   radioButtons('box_addmean', label='Add mean in Boxplot', 
                choiceNames= list('Yes', 'No'),
                choiceValues = list(1,2), 
                choices=NULL)
),


mainPanel(
  tabsetPanel(
    tabPanel("Results",
             h3('Summary Statistics'),
             verbatimTextOutput('SumStats'),
             h3('Histogram'),
             plotlyOutput('histplot'),
             h3('Boxplot'),
             plotlyOutput('bplot')),
    tabPanel("Download Dataset",
    DT::dataTableOutput("dtable"))
  ))
)
)

server <- function(input, output) {  
  
  getdata <- function(text) {
    text <- gsub(" ", "", text)
    split <- strsplit(text, ",", fixed = FALSE)[[1]]
    as.numeric(split)
  }
  
  # Display Data
  output$dtable <- DT::renderDataTable({
    x <- getdata(input$data)
    DT::datatable(data.frame(x),
                  extensions = "Buttons",
                  options = list(
                    lengthChange = TRUE,
                    dom = "Blfrtip",
                    buttons = c("copy", "csv", "excel", "pdf", "print")
                  )
    )
  })
  # Summary Statistics
  output$SumStats <- renderPrint({
    x <- getdata(input$data)
    Min=fivenum(x)[1]
    Q1 = fivenum(x)[2]
    Median = fivenum(x)[3]
    Mean = mean(x)
    SD = sd(x)
    Q3 = fivenum(x)[4]
    Max = fivenum(x)[5]
    #xlab <- c("Min", "Q1", "Median","Q3","Max",Mean","SD")
    #x5sum <- c(fivenum(x)[1:3],mean(x),fivenum(x)[4:5])
    df=data.frame(Min, Q1, Median,Q3, Max, Mean, SD)
    pander::pander(df)
  })
  
  # Histogram
  output$histplot <- renderPlotly({
   
    data <- getdata(input$data)
    dat <- data.frame(data)
    plot_ly(x = ~data,
            alpha = 0.7, breaks="FD",
            type = "histogram",
            histnorm = ifelse(input$hist_type==1,"None","probability"),
            marker = list(color = "89ccd6",
                          line = list(color = "darkgray",
                                      width = 2)))%>%
      layout(title = '', xaxis=list(title = 'X Axis'), yaxis = list(title = "Y Axis"))
  })
  
  # Boxplot
  output$bplot <- renderPlotly({
    
    data <- getdata(input$data)
    dat <- data.frame(data)
    if(input$box_type==1){
      plot_ly(data=dat,
              y=~data,
              type = "box",
              boxmean = (input$box_addmean==1),
              boxpoints="all")%>%
        layout(title = '', xaxis=list(title = 'X Axis'), yaxis = list(title = "Y Axis"))
    }
    else{
      plot_ly(data=dat,
              x=~data,
              type = "box",
              boxmean = (input$box_addmean==1),
              boxpoints="all")%>%
        layout(title = '', xaxis=list(title = 'X Axis'), yaxis = list(title = "Y Axis"))
      
      }
  })
  
}

shinyApp(ui,server)


