library(shiny)
library(shinyjs)
library(shinythemes)
library(rmarkdown)
library(knitr)
library(plotly)
library(pander)
library(shinyMatrix)
library(RColorBrewer)

ui <- fluidPage(
 theme = shinythemes::shinytheme('cerulean'),
  titlePanel('Descriptive analysis of a categorical variable'),
  sidebarLayout(
    sidebarPanel(
      radioButtons('input_type', 
                   label=h4('Data input methods '),
                   selected = 1,
                   choiceNames = list('Individual Observations', 'Frequency Table'),
                   choiceValues = list(1, 2)),
      br(),
      conditionalPanel('input.input_type == 1',
                       textAreaInput('data', 
                                 value = "Female, Male,Female,Male, Female,Female,Female, Male, Female, Male,Male,Male,Female",
                                 'Enter data here: e.g., F,M,F,F,...')),
      conditionalPanel('input.input_type == 2',
                       matrixInput(
                         inputId = "dmatrix",
                         label = h5("Enter category and counts:"),
                         value = matrix(c(10,7),nrow=2,ncol=1,
                                        dimnames=list(c("Apple","Banana"), "Counts")),
                         class = "numeric",
                         cols = list(names = TRUE,editableNames = FALSE),
                         rows = list(extend = TRUE,names = TRUE,editableNames = TRUE, delete = TRUE),
                       )),
      hr(),
      h4("Bar graph options"),

   radioButtons('bar_type', label='Bar Type', 
                choiceNames= list('Show count', 'Show percent'),
                choiceValues = list(1,2), 
                choices=NULL),
   radioButtons('bar_hv', label='Bar alignment', 
                choiceNames= list('Vertical', 'Horizontal'),
                choiceValues = list(1,2), 
                choices=NULL)
),
mainPanel(
  tabsetPanel(
    tabPanel("Results",
  h3('Summary Statistics'),
  verbatimTextOutput('SumStats'),
  hr(), 
  h3('Pie Chart'),
  plotlyOutput('pieplot'),
  h3('Bar Graph'),
  plotlyOutput('barplot')),
  tabPanel("Download Dataset",
        DT::dataTableOutput("dtable")),
  )
)
)
)
server <- function(input, output) {  
  
  getdata <- function(text) {
    text <- gsub(" ", "", text)
    split <- strsplit(text, ",", fixed = FALSE)[[1]]
    as.character(split)
  }
  
  # Display Data
  output$dtable <- DT::renderDataTable({
    if (input$input_type==1){
    x <- getdata(input$data)
    DT::datatable(data.frame(x),
                  extensions = "Buttons",
                  options = list(
                    lengthChange = TRUE,
                    dom = "Blfrtip",
                    buttons = c("copy", "csv", "excel", "pdf", "print")
                  ))
    }
    else{
      DT::datatable(data.frame(input$dmatrix),
                    extensions = "Buttons",
                    options = list(
                      lengthChange = FALSE,
                      dom = "Blfrtip",
                      buttons = c("copy", "csv", "excel", "pdf", "print")
                    ))
    }
  })
  
  # Summary Statistics
  output$SumStats <- renderPrint({
    if(input$input_type==1){
      x <- getdata(input$data)
      df <- data.frame(table(x))
      df$RelativeFreq <-round(df$Freq/sum(df$Freq),4)
      colnames(df) = c("Category","Count","Percent")
    }else{
      df=data.frame(input$dmatrix)
      df$Percent =round(df$Counts/sum(df$Counts),4)
      df$category = rownames(input$dmatrix)
      colnames(df)=c("Count", "Percent","Category")
      df= df[,c("Category","Count","Percent")]
      }

    
    #xlab <- c("Min", "Q1", "Median", "Mean","Q3","Max")
    #x5sum <- c(fivenum(x)[1:3],mean(x),fivenum(x)[4:5])
    print(df,row.names = F)
    #pander::pander(df)
  })
  
  # Pie Chart
  cols <- RColorBrewer::brewer.pal(20,"Set3")[seq_along(labels)]
  output$pieplot <- renderPlotly({
    if(input$input_type==1){
      category <- getdata(input$data)
      df <- data.frame(table(category))
      df$Percent <-round(df$Freq/sum(df$Freq),4)
      colnames(df)=c("Category","Count","Percent")}
    else{
      df=data.frame(input$dmatrix)
      df$Percent =round(df$Counts/sum(df$Counts),4)
      df$category = rownames(input$dmatrix)
      colnames(df)=c("Count", "Percent","Category")
      df= df[,c("Category","Count","Percent")]
    }
    plot_ly(df,labels=~Category, values=~Count, marker = list(colors = cols), type = "pie")
  })
  # Bar plot
  output$barplot <- renderPlotly({
    if(input$input_type==1){
      category <- getdata(input$data)
      df <- data.frame(table(category))
      df$Percent <-round(df$Freq/sum(df$Freq),4)
      colnames(df)=c("Category","Count","Percent")}
    else{
      df=data.frame(input$dmatrix)
      df$Percent = round(df$Counts/sum(df$Counts),4)
      df$category = rownames(input$dmatrix)
      colnames(df)=c("Count", "Percent","Category")
      df= df[,c("Category","Count","Percent")]
    }
    if(input$bar_hv==1){
      plot_ly(df, x = ~Category, y = if(input$bar_type==1) ~Count else ~Percent, 
              type = 'bar',
              marker = list(color = cols,
                            line = list(color = 'rgb(8,48,107)', 
                                        width = 1.5)))
    }
    else{
      plot_ly(df, y = ~Category, x = if(input$bar_type==1) ~Count else ~Percent, 
              type = 'bar',
              marker = list(color = cols,
                            line = list(color = 'rgb(8,48,107)', width = 1.5)))
     
    }
    
  })
  
}

shinyApp(ui,server)
