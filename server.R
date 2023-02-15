#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
# Define server logic required to draw a histogram
generate_random_weight <- function(number_weight_to_generate, lower_bound, upper_bound) {
  #return (sample(lower_bound : upper_bound, size = number_weight_to_generate, replace = TRUE))
  return(runif(number_weight_to_generate,-1,1))
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
perceptron <- function(data_set, max_learning_iter) {
  data_dimension <- ncol(data_set) - 1
  data_amount <- nrow(data_set)
  weights <- generate_random_weight(data_dimension, -1, 1)
  output_guess <- data_set[, data_dimension + 1]
  is_stabilize <- FALSE
  
  for (learning_index in 1:max_learning_iter) {
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
    return ("Le jeu de données n'est pas linéairement séparable")
  }
  
  return(weights)
}
plot_data <- function(data_set){
  normalized <- normalize_class_column(data_set)
  normalized <- normalized[order(as.vector(t((normalized[3])))),]     
  negativ <- normalized[normalized[,3]<1,]
  positiv <- normalized[normalized[,3]>0,]
  length(c(rep(2,nrow(negativ)),rep(3,nrow(positiv))))
  plot(as.vector(t(normalized[1])),as.vector(t(normalized[2])), pch=c(rep(3,nrow(negativ)),rep(1,nrow(positiv))),col = c(rep("green",nrow(negativ)),rep("red",nrow(positiv))),lwd = 3)
  
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





shinyServer(function(input, output,session) {
  shinyjs::disable("a_auto")
  shinyjs::disable("b_auto")
  shinyjs::disable("launchPerceptronReport")
  
  output$formuleNeurone <- renderUI({
    withMathJax(paste("$$w_1x_1+\\ldots+w_mx_m=\\sum_{j=1}^m w_j x_j.$$"))
  })
  
      
        
        values <- reactiveValues(x=rnorm(30),e=rnorm(30),handy="RIEN",data_set_choosen=read.table("./database/xor.txt", header = TRUE),outputPerceptronValue=" ",b_auto=1,a_auto=1)
        
        observeEvent({input$X_law
                      input$n},
           {
           n=input$n
           if (input$X_law=='N(0,1)') {values$x=rnorm(n)}
           if (input$X_law=='Exp(1)') {values$x=rexp(n)}
           if (input$X_law=='U(0,1)') {values$x=runif(n)}
           values$e=rnorm(input$n)
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
         plot_data(values$data_set_choosen)
         abline(values$b_auto,values$a_auto,col='red')
       })
       
       observeEvent({input$a_auto
         input$b_auto},
         {
           values$a_auto = input$a_auto
           values$b_auto = input$b_auto
         })
       
       #Print the true/false text in hand mode
       output$value<-({
         reactive(values$handy)
       })
       #Update the graph when a/b ar updated
       observeEvent({input$a_hand
         input$b_hand},
         {
           values$handy=validate_axis(input$a_hand,input$b_hand,data_set)
           if(values$handy){
             shinyjs::enable("handtrainNext")
           }else{
             shinyjs::disable("handtrainNext")
           }
         })
       observeEvent({input$importAuto},{
                    if (!is.null(input$importAuto))
                    {
                      inFile <- input$importAuto
                      file <- inFile$datapath
                      values$data_set_choosen=read.table(file,header=TRUE)
                    }
         })
       #update the dataset used for the perceptron
       observeEvent({input$auto_dataset},
         {
          
           if (input$auto_dataset=='Exemple 1') {values$data_set_choosen=read.table("./database/b.txt", header = TRUE)}
           if (input$auto_dataset=='Exemple 2') {values$data_set_choosen=read.table("./database/data.txt", header = TRUE)}
           if (input$auto_dataset=='AND') {values$data_set_choosen=read.table("./database/and.txt", header = TRUE)}
           if (input$auto_dataset=='XOR') {values$data_set_choosen=read.table("./database/xor.txt", header = TRUE)}
           if (input$auto_dataset=='Iris') {values$data_set_choosen=read.table("./database/iris.txt", header = TRUE)}
         })
       observeEvent({input$handtrainNext},{
         updateTabsetPanel(session, "mainNavBar",
                           selected = "autotrain")
       })
       observeEvent({input$cours1},{
         updateTabsetPanel(session, "mainNavBar",
                           selected = "perceptron")
       })
       observeEvent({input$goToHandtrain},{
         updateTabsetPanel(session, "mainNavBar",
                           selected = "handtrain")
       })
       observeEvent({input$launchPerceptron},{
         print("launch perceptron")
         data_set <- add_biais_column(values$data_set_choosen)
         data_set <- normalize_class_column(data_set)
         
         values$outputPerceptronValue <- perceptron(data_set, 10000)
         if(values$outputPerceptronValue != "Le jeu de données n'est pas linéairement séparable" ){
         print(-1.0*values$outputPerceptronValue[1]/values$outputPerceptronValue[3])
         print(-1.0*values$outputPerceptronValue[2]/values$outputPerceptronValue[3])
         values$a_auto <- -1.0*values$outputPerceptronValue[2]/values$outputPerceptronValue[3]
         values$b_auto <- -1.0*values$outputPerceptronValue[1]/values$outputPerceptronValue[3]
         shinyjs::enable("launchPerceptronReport")
         } 
         
         #values$outputPerceptronValue="hey"
       })
       output$outputPerceptron<-({
         reactive(values$outputPerceptronValue)
       })
       
       
       
       output$launchPerceptronReport <- downloadHandler(
         filename = "Perceptron.pdf",
         content = function(file) {
           # Copy the report file to a temporary directory before processing it, in
           # case we don't have write permissions to the current working dir (which
           # can happen when deployed).
           tempReport <- file.path(tempdir(), "perceptron.Rmd")
           file.copy("perceptron.Rmd", tempReport, overwrite = TRUE)
           # Set up parameters to pass to Rmd document
           params <- list(
             data_set=values$data_set_choosen,
             result=values$outputPerceptronValue,
             a=values$a_auto,
             b=values$b_auto
           )
           
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
