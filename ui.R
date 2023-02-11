
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
                   navbarMenu("Cours",
                              tabPanel("Perceptron",
                                       value="perceptron",
                                       fluidRow(
                                         column(10,wellPanel(
                                           h3("1. Perceptron"),
                                           p("La brique de base d'un réseau de neuronne est... le neurone."),
                                           p("Comme souvent en Intelligence Artificielle on cherche à mimer le fonctionnement du raisonnement humain. C'est donc tout naturellement que l'on cherche à moddéliser la brique la plus élémentaire: le neurone"),
                                           img(src="./neuronOrga.jpg",width="20%",style="display: block; margin-left: auto; margin-right: auto;"),
                                           p("Un neurone est composé d'un noyau auxquels sont reliés des dentrites. Ceux ci vont recevoir des signaux éléctrochimique, les transmettre au noyau qui va a son tour envoyer des signaux."),
                                           p("Un neurone en informatique est donc une simple modélisation de cette agrégation de signaux d'entrée et d'un calcul d'un sortie. Il suit cette formule :"),
                                           uiOutput("formuleNeurone"),
                                           img(src="./neuron.png",width="20%",style="display: block; margin-left: auto; margin-right: auto;"),
                                           p("On peut utiliser ce neurone, seul, en qualité de perceptron. Un perceptron est un algorithme d'apprentissage supervisé de clasifeur binaire. Il permet de séparer des données linérairement séparables. En prenant en entrée chaque dimension il va chercher un hyperplan qui va séparer les deux classes de nos données dans la perspectives de pouvoir classifier les prochains exemples à leurs tour."),
                                           actionButton('handtrain','Séparer des données')
                                         ))
                                       )
                              ),
                              tabPanel("Entrainer à la main",
                                       value = "handtrain",
                                       sidebarLayout(
                                         sidebarPanel(
                                           
                                           sliderInput("a_hand","A:",min = -1,max = 2,value = -1,step=0.1),
                                           sliderInput("b_hand","B:",min = -10,max = 10,value = 8,step=0.1),
                                           verbatimTextOutput("value")
                                         ),
                                         
                                         # Show a plot of the generated distribution
                                         mainPanel(
                                           plotOutput("handPlot"),
                                           

                                         )
                                         
                                       )
                                       ),
                              tabPanel("Auto-entrainement",
                                       value = "autotrain",
                                       sidebarLayout(
                                         sidebarPanel(
                                           selectInput(inputId='hand_dataset', label='Choose a dataset',
                                                       choices=c('N(0,1)','Exp(1)','U(0,1)') ,multiple = FALSE, selected='N(0,1)'),
                                           sliderInput("a_auto","A:",min = -10,max = 10,value = 3),
                                           sliderInput("b_auto","B:",min = -10,max = 10,value = -3),
                                         ),
                                         
                                         # Show a plot of the generated distribution
                                         mainPanel(
                                           plotOutput("autoPlot"),
                                         )
                                       )
                                      ),
                              tabPanel("Réseaux de neuronnes")),
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
