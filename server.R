#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
        
        output$Variables = renderUI(
          {
            if (!is.null(input$Dataset))
            {
              D_name=input$Dataset
              data(D_name)
              wellPanel(
              selectInput("Y_var",'Response variable', choices=colnames(get(D_name))), 
              selectInput("X_var",'Explanatory variable', choices=colnames(get(D_name))))
            }
        })

        output$Variables2 = renderUI(
          {
            if (!is.null(input$Import))
            {
              inFile <- input$Import
              file <- inFile$datapath
              DATA=read.table(file,header=TRUE)
              wellPanel(
                selectInput("Y_var2",'Response variable', choices=colnames(DATA)), 
                selectInput("X_var2",'Explanatory variable', choices=colnames(DATA)))
            }
          })
        
        values <- reactiveValues(x=rnorm(30),e=rnorm(30))
        
        observeEvent({input$X_law
                      input$n},
           {
           n=input$n
           if (input$X_law=='N(0,1)') {values$x=rnorm(n)}
           if (input$X_law=='Exp(1)') {values$x=rexp(n)}
           if (input$X_law=='U(0,1)') {values$x=runif(n)}
           values$e=rnorm(input$n)
           })
      
       output$Scatterplot <- renderPlot({
    # generate y values based on input$a, input$b and input$s from ui.R
    x=values$x
    e=values$e
    y=input$a*x+input$b+input$s*e
    res=lm(y~x)
    x0=input$x0
    est=predict(res,data.frame(x=x0))
    plot(x,y,main=paste('cor=',cor(x,y),' Estimation: ',est, sep=''))
        abline(res,col='red')
        segments(x0,min(y),x0,est,col='green',lty=3)
        segments(min(x),est,x0,est,col='blue',lty=3)
        
    
  })
       
       
       output$Scatterplot2 <- renderPlot({
         # generate y values based on input$a, input$b and input$s from ui.R
         if (!is.null(input$Dataset))
         {
         attach(get(input$Dataset))
           if (!is.null(input$X_var)){x=get(input$X_var)}
           if (!is.null(input$Y_var)){y=get(input$Y_var)}
         plot(x,y,main=paste('cor=',cor(x,y),sep=''), xlab=input$X_var, ylab=input$Y_var)
         res=lm(y~x)
         abline(res,col='red')
         }
       })
       
       output$Scatterplot3 <- renderPlot({
         # generate y values based on input$a, input$b and input$s from ui.R
         if (!is.null(input$Import))
           {
           inFile <- input$Import
           file <- inFile$datapath
           DATA=read.table(file,header=TRUE)
           attach(DATA)
           x=get(input$X_var2)
         y=get(input$Y_var2)
         plot(x,y,main=paste('cor=',cor(x,y),sep=''), xlab=input$X_var2, ylab=input$Y_var2)
         res=lm(y~x)
         abline(res,col='red')
         }
       })
       
       
       output$report <- downloadHandler(
         filename = "Modeles_lineaires.docx",
         content = function(file) {
           # Copy the report file to a temporary directory before processing it, in
           # case we don't have write permissions to the current working dir (which
           # can happen when deployed).
           tempReport <- file.path(tempdir(), "Modeles_lineaires.Rmd")
           file.copy("Modeles_lineaires.Rmd", tempReport, overwrite = TRUE)
           # Set up parameters to pass to Rmd document
           params <- list(n=input$n,
                          X_law=input$X_law,  
                          x0=input$x0,
                          a=input$a,
                          b=input$b,
                          s=input$s,
                          x=values$x,
                          e=values$e)
           
           # Knit the document, passing in the `params` list, and eval it in a
           # child of the global environment (this isolates the code in the document
           # from the code in this app).
           rmarkdown::render(tempReport, output_file = file,
                             params = params,
                             envir = new.env(parent = globalenv())
           )
         }
       )
  
})
