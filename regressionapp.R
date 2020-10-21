#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
  
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("type","select type",choices = c("simple linear regression","multiple linear regression")), 
        fileInput("file","Choose csv:",multiple="false"),
         uiOutput("subselect1"),
         conditionalPanel(condition = "input.type=='simple linear regression'",
         uiOutput("subselect2")),
         conditionalPanel(condition = "input.type=='multiple linear regression'",
                          uiOutput("multiplesubselect"))
         
      ),

      # Show a plot of the generated distribution
      mainPanel(
         h1("Regression application"),
         conditionalPanel(condition = "input.type=='simple linear regression'",
                          h3("simple linear regression"),
                          h4("Pearson's product-moment correlation Coefficient"),
                          h3("scatter plot"),
                          plotOutput("scatter"),
                          plotOutput("multiplot"),
                          h3("Applying simple linear regression"),
                          verbatimTextOutput("SLR"),
                          h3("summary of regression model"),
                          verbatimTextOutput("summary"),
                          h3("Regression Line"),
                          plotOutput("regressionline"),
                          p("R-squared is a statistical measure of how close the data are to the fitted regression line. It is also known as the coefficient of determination, or the coefficient of multiple determination for multiple regression. In general, the higher the R-squared, the better the model fits your data."),
                          uiOutput("number"),
                          verbatimTextOutput("prediction"),
                          h3("Goodness of fit"),
                          verbatimTextOutput("goodness")
                        
                          
                        ),
         conditionalPanel(condition = "input.type=='multiple linear regression'",
                          h3("multiple linear regression"),
                          h4("Pearson's product-moment correlation Coefficient"),
                          verbatimTextOutput("cor"),
                          h3("Scatter plot"),
                          plotOutput("multipleregplot"),
                          h3("Applying multiple linear regression"),
                          verbatimTextOutput("MLR"),
                          h3("summary of multiple linear regression model"),
                          verbatimTextOutput("multiplesummary"),
                          h3("Goodness of Fit"),
                          verbatimTextOutput("goodnessmlr"),
                          uiOutput("displaypredict"),
                          verbatimTextOutput("multiplepredict"),
                          uiOutput("display"),
                          verbatimTextOutput("datadisplay")
                          
         )
         
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$subselect1 <- renderUI({
    x <- read.csv(input$file$datapath)
    data <- data.frame(x)
    selectInput("sub1","Independent variable",choices=names(data))
    
   })
  output$subselect2 <- renderUI({
    x <- read.csv(input$file$datapath)
    data <- data.frame(x)
    selectInput("sub2","Dependent variable",choices=names(data)[!names(data)==input$sub1])
    
    
  })
  
  output$multiplesubselect <- renderUI({
    x <- read.csv(input$file$datapath)
    data <- data.frame(x)
    selectInput("sub3","Dependent variable",choices=names(data)[!names(data)==input$sub1], multiple = TRUE)
    
    
  })
  output$number<-renderUI({
    numericInput("number","Enter value to Predict","value=1","1","10000","1")
  })
  output$scatter<-renderPlot({
    x <- read.csv(input$file$datapath)
    data <- data.frame(x)
    par(fig=c(0,0.8,0,0.8), new=TRUE)
    plot(data[,input$sub1],data[,input$sub2])
    par(fig=c(0,0.8,0.55,1), new=T)
    boxplot(data[,input$sub1],horizontal = T , axes=T)
    par(fig=c(0.7,1,0,0.8), new=T)
    boxplot(data[,input$sub2], axes=T)
    
  })
  output$multiplot<-renderPlot({
    x <- read.csv(input$file$datapath)
    data <- data.frame(x)
    a<-data[,input$sub1]
    b<-data[,input$sub2]
    df<-data.frame(a,b)
    names(df)[1]=input$sub1
    names(df)[2]=input$sub2
    library(psych)
    pairs.panels(df,
                 method="pearson",
                 density=TRUE,
                 ellipses=F)
                 
    

})
  
  output$SLR<-renderPrint({
    x <- read.csv(input$file$datapath)
    data <- data.frame(x)
    a<-data[,input$sub1]
    b<-data[,input$sub2]
    model=lm(a~b)
    model
    
  })
  output$summary<-renderPrint({
    x <- read.csv(input$file$datapath)
    data <- data.frame(x)
    a<-data[,input$sub1]
    b<-data[,input$sub2]
    model=lm(a~b)
    model
    summary(model)
  })
  output$regressionline<-renderPlot({
    x <- read.csv(input$file$datapath)
    data <- data.frame(x)
    a<-data[,input$sub1]
    b<-data[,input$sub2]
    model=lm(a~b)
    model
    plot(b,a)
    abline(model)
    
  })
  
   output$prediction<-renderPrint({
     x <- read.csv(input$file$datapath)
     data <- data.frame(x)
     a<-data[,input$sub1]
     b<-data[,input$sub2]
     df = data.frame(b,a)
     predictor<-input$number
     model=lm(a~b, data=df)
     predict(model,data.frame(b= as.numeric(predictor)), interval="prediction",
             level=0.95)
   })
   output$goodness<-renderPrint({
     x <- read.csv(input$file$datapath)
     data <- data.frame(x)
     a<-data[,input$sub1]
     b<-data[,input$sub2]
     model=lm(a~b)
     model
     result<-summary(model)
     cat(result$r.squared*100)
     })
   output$multipleregplot<-renderPlot({
     x <- read.csv(input$file$datapath)
     data <- data.frame(x)

     library(psych)
     pairs.panels(data)
   })
   
   output$MLR<-renderPrint({
     x <- read.csv(input$file$datapath)
     data <- data.frame(x)
     a<-data[,(input$sub1)]
     b<-data[,(input$sub3)]
     newdf<-data.frame(a,b)
     model1<-lm(a~.,data = newdf)
     model1
   })
   output$multiplesummary<-renderPrint({
     x <- read.csv(input$file$datapath)
     data <- data.frame(x)
     a<-data[,(input$sub1)]
     b<-data[,(input$sub3)]
     newdf<-data.frame(a,b)
     model1<-lm(a~.,data = newdf)
     summary(model1)
   })
   output$goodnessmlr<-renderPrint({
     x <- read.csv(input$file$datapath)
     data <- data.frame(x)
     a<-data[,(input$sub1)]
     b<-data[,(input$sub3)]
     newdf<-data.frame(a,b)
     model1<-lm(a~.,data = newdf)
     result<-summary(model1)
     cat(result$r.squared*100)
   })
   
   output$cor<-renderPrint({
     x <- read.csv(input$file$datapath)
     data <- data.frame(x)
     a<-data[!is.na(rowMeans(data[,(input$sub3)])),(input$sub1)]
     b<-data[!is.na(rowMeans(data[,(input$sub3)])),(input$sub3)]
     newdf<-data.frame(a,b)
     cor(newdf)
   })
   output$display <- renderUI({
     textInput("n", "Enter the number of data to display")
   })   
   output$datadisplay<-renderPrint({
     x <- read.csv(input$file$datapath)
     data <- data.frame(x)
     a<-data[,(input$sub1)]
     b<-data[,(input$sub3)]
     n<-input$n
     newdf<-data.frame(a,b)
     head(newdf,n)
   })
     output$displaypredict <- renderUI({
       textInput("n1", "Enter the marks to predict for:")
     
   })
   output$multiplepredict<-renderPrint({
     x <- read.csv(input$file$datapath)
     data <- data.frame(x)
     selectedsubject1<-data[,(input$sub1)]
     newsubj1<-selectedsubject1[!is.na(rowMeans(selectedsubject1))]
     selectedsubject2<-data[,(input$sub3)]
     newsubj2<-selectedsubject2[!is.na(rowMeans(selectedsubject2))]
     newdf<-data.frame(newsubj1,newsubj2)
     model<-lm(newsubj1~.,data=newdf)
     n1<-input$n1
     v1<-as.numeric(strsplit(input$n1,",")[[1]])
     z<-as.data.frame(matrix(v1,1,length(v1)))
     names(z)<-(input$x)
     cat("predicted value:",predict(model,z)[1])
     })
}
# Run the application 
shinyApp(ui = ui, server = server)

