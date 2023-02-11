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
generate_random_weight <- function(number_weight_to_generate, lower_bound, upper_bound) {
  return (sample(lower_bound : upper_bound, size = number_weight_to_generate, replace = TRUE))
}
matrix_product <- function(data, weights) {
  as.matrix(data, nrow = 1) %*% as.matrix(weights, ncol = 1)
}
add_biais_column <- function(data_set) {
  data_amount <- nrow(data_set)
  data_set$biais <- rep(1, times = data_amount)
  data_set <- data_set[, c(c(ncol(data_set)), seq(from = 1, to = ncol(data_set) - 1, by = 1))]
  return(data_set)
}
normalize_class_column <- function(data_set) {
  default_value <- data_set[1, ncol(data_set)]
  data_set[ncol(data_set)] <- ifelse(data_set[ncol(data_set)] == default_value, 0, 1)
  return(data_set)
}
perceptron <- function(data_set, max_learning_rate) {
  data_dimension <- ncol(data_set) - 1
  data_amount <- nrow(data_set)
  weights <- generate_random_weight(data_dimension, -1, 1)
  output_guess <- data_set[, data_dimension + 1]
  is_stabilize <- FALSE
  
  for (learning_index in 1:max_learning_rate) {
    for (data_line in 1:data_amount) {
      classe <- data_set[data_line, data_dimension + 1]
      output_guess[data_line] <- ifelse(matrix_product(data_set[data_line, 1:data_dimension], weights) > 0, 1, 0)
      
      for(i in 1:data_dimension) {
        weights[i] <- weights[i] + (classe - output_guess[data_line]) * data_set[data_line, i]
      }
    }
    
    if (is_stabilize <- all(output_guess == data_set[, data_dimension + 1])) {
      break
    }
  }
  
  if (!is_stabilize) {
    stop(cat("le jeu de données n'est pas sécable en 2 avec :", max_learning_rate, " itérations \n"), call. = FALSE)
  }
  
  return(weights)
}
plot_data <- function(data_set){
  normalized <- normalize_class_column(data_set)
  normalized <- normalized[order(as.vector(t((normalized[3])))),]     
  negativ <- normalized[normalized[,3]<1,]
  positiv <- normalized[normalized[,3]>0,]
  length(c(rep(2,nrow(negativ)),rep(3,nrow(positiv))))
  plot(as.vector(t(data_set[1])),as.vector(t(data_set[2])), pch=c(rep(3,nrow(negativ)),rep(1,nrow(positiv))),col = c(rep("green",nrow(negativ)),rep("red",nrow(positiv))),lwd = 3)
  
}
validate_axis <- function(a,b,data_set){
  normalized <- normalize_class_column(data_set)
  normalized <- normalized[order(as.vector(t((normalized[3])))),]     
  negativ <- normalized[normalized[,3]<1,]
  positiv <- normalized[normalized[,3]>0,]
  negativRes<- as.vector(t((negativ[2]))) - as.vector(t((negativ[1])))* a - b
  positivRes<- as.vector(t((positiv[2]))) - as.vector(t((positiv[1])))* a - b 
  return (Reduce(function(u,v) u&&v ,(!(c(negativRes >0)))==(c(positivRes>0))))
}

shinyServer(function(input, output) {
  
  output$formuleNeurone <- renderUI({
    withMathJax(paste("$$w_1x_1+\\ldots+w_mx_m=\\sum_{j=1}^m w_j x_j.$$"))
  })
  
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
       data_set <- read.table("./database/iris.txt", header = TRUE)
       
       output$handPlot <- renderPlot({
         # generate y values based on input$a, input$b and input$s from ui.R
         set.seed(1)
         x <- rnorm(5)
         y <- x + rnorm(5)
         plot_data(data_set)
         abline(input$b_hand,input$a_hand,col='red')
       })
       output$autoPlot <- renderPlot({
         # generate y values based on input$a, input$b and input$s from ui.R
         set.seed(1)
         x <- rnorm(5)
         y <- x + rnorm(5)
         plot_data(data_set)
         abline(input$b_auto,input$a_auto,col='red')
       })
       output$value<-({
         reactive(validate_axis(input$a_auto,input$b_auto,data_set))
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
