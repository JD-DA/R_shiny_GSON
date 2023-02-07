
library(shiny)

# Define UI for application that draws a histogram
shinyUI(navbarPage("Linear regression with R",
                   tabPanel("Menu",
                            value = "menu",
                            #menu with two big buttons
                            fluidRow(
                              column(6,wellPanel(
                                h3("1. C\'est quoi un réseau de neurones ?"),
                                #description
                                p("Réseau de neuronnes, deep learning, machine learning, 
                perceptrons... Vous saurez tout sur ces concepts et bien
                 plus encore !"),
                                p("Un réseau de neurones est un modèle mathématique qui 
                permet de modéliser des processus complexes. Il est 
                constitué de neurones artificiels, qui sont des fonctions
                 mathématiques simples, mais qui peuvent être combinées 
                 de manière complexe pour modéliser des processus complexes. 
                 Les réseaux de neurones sont utilisés dans de nombreux 
                 domaines, comme la reconnaissance d\'images, la traduction 
                 automatique, la prédiction de séries temporelles, etc."),
                                #image
                                img(src="./neuron.png",width="50%"),
                                actionButton('cours1','Je veux savoir !')
                              )),
                              column(6,wellPanel(
                                h3('Entrainer un réseau de neurones'),
                                actionButton('train','Je veux entrainer un réseau !')
                              ))
                            )
                            
                            
                            
                   ),
                   tabPanel('Simulations',
                            # Application title
                            # Sidebar with a slider input for number of bins 
                            sidebarLayout(
                              sidebarPanel(
                                selectInput(inputId='X_law', label='Choose X\'s distribution',
                                            choices=c('N(0,1)','Exp(1)','U(0,1)') ,multiple = FALSE, selected='N(0,1)'),
                                sliderInput("n","Sample size:",min = 1,max = 100,value = 30),
                                sliderInput("a","A:",min = -10,max = 10,value = 3),
                                sliderInput("b","B:",min = -10,max = 10,value = -3),
                                sliderInput("s","S:",min =0,max = 10,value =1,step= .01),
                                sliderInput("x0","Eval.point :",min =-2,max = 2,value =1,step= .01)
                                
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                plotOutput("Scatterplot"),
                                downloadButton("report", "Generate report")
                                
                              )
                              
                            )
                            
                   ),
                   tabPanel('Data sets',
                            sidebarLayout(
                              sidebarPanel(
                                selectInput('Dataset','Select a data set from R',
                                            choices=(data()[[3]])[,3],selected='trees'),
                                uiOutput("Variables")),
                              mainPanel(
                                plotOutput("Scatterplot2")
                              )
                              
                            )
                   ),
                   tabPanel('Import data',
                            sidebarLayout(
                              sidebarPanel(
                                fileInput('Import','Select a txt file to import'),
                                uiOutput("Variables2")),
                              # Show a plot of the generated distribution
                              mainPanel(
                                plotOutput("Scatterplot3")
                              )
                              
                            )
                   )
))
